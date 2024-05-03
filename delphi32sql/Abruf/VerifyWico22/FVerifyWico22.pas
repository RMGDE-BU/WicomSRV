{------------------------------------------------------------------------------}
{ Überwachung für WICO22 mit Redundanzüberwachung                              }
{                                                                              }
{ 21.09.2007  GD  Vs. 1.0  Neu                                                 }
{ 06.09.2007  GD  Vs. 1.01 Diverse Kleinigkeiten (Logs, Sicherungszeiten, ...) }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2007                                      }
{------------------------------------------------------------------------------}
unit FVerifyWico22;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, WDialogs, Menus, ExtCtrls, Buttons, ComCtrls, StdCtrls,
  GD_Utils, FComBasis, Startwin, ActnList,
  FDefaultTab, VerifyWico22Ini, T_Zeit, VerifyFlags,
  FTabVerifyData, FTabVerifyModules, FTabVerifyTakeOver, COM_Utils;

type
  TFormVerifyWico22 = class(TFormComBasis)
    pnClient: TPanel;
    PageControl: TPageControl;
    ActionList: TActionList;
    aReboot: TAction;
    sbtnReboot: TSpeedButton;
    pnStatus: TPanel;
    aShowJounal: TAction;
    pnSystemStates: TPanel;
    shMyState: TShape;
    shOtherState: TShape;
    procedure FormCreate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure pmiShowClick(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var Action: TCloseAction); override;
    procedure TimerTimer(Sender: TObject);
    procedure aRebootExecute(Sender: TObject);
  private
    { Private-Deklarationen }
    FVerifyInterval  : Cardinal;
    FTabFormList     : TList;
    FModuleTimeout   : Cardinal;
    FAktModuleList   : string;
    FJournal         : TStrings;
    FMemoJournal     : TMemo;
    FMaxJournalList  : integer;
    FMaxJournalLines : integer;
  protected
    { Protected-Deklarationen }
    procedure MyExceptionHandling(E: Exception; const sText: string); virtual;

    procedure InitComponents(bState: boolean); override;
    procedure InitJournal(bState: boolean); virtual;
    procedure InitDataVerification(bState: boolean); virtual;
    procedure InitRuntimeVerification(bState: boolean); virtual;
    procedure InitTakeOverVerification(bState: boolean); virtual;

    function GetTabForm(iIndex: integer): TFormVerifyWico22Tab; overload; virtual;
    function GetTabForm(
      pFormClass: TFormVerifyWico22TabClass): TFormVerifyWico22Tab; overload; virtual;
    function IndexOfTab(pFormClass: TFormVerifyWico22TabClass): integer;
    procedure InsertTab(pFormClass: TFormVerifyWico22TabClass);

    procedure InitModuleState(bState: boolean);
    procedure WriteJournal(sText: string);
    function DataTakeOver: boolean;
  public
    { Public-Deklarationen }
    procedure Reboot;
  end;

implementation

{$R *.dfm}

{------------------------------- TFormVerifyWico22 ----------------------------}

{--------------------------------------------}
procedure TFormVerifyWico22.FormCreate(Sender: TObject);
{--------------------------------------------}
begin
//  FCaption := 'mmm';  // Für Mehrfachstart
  pmiMinimized.Checked := False;

  inherited;

  if (Assigned(VerifyWicoIni)) then begin
    pmiMinimized.Checked := False;
    FCaption := VerifyWicoIni.ReadString('SETTINGS', 'TITLE', Self.Caption);
  end;
end;

{--------------------------------------------}
procedure TFormVerifyWico22.pmiShowClick(Sender: TObject);
{--------------------------------------------}
begin
  inherited;

  PageControlChange(nil);
end;

{--------------------------------------------}
procedure TFormVerifyWico22.FormClose(
  Sender: TObject; var Action: TCloseAction);
{--------------------------------------------}
var
  i : integer;
  p : TForm;
begin
  if (MessageDlg('Soll die WICO22-Systemüberwachung wirklich beendet werden?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes) and
     (MessageDlg('Soll die WICO22-Systemüberwachung doch fortgesetzt werden?',
    mtConfirmation, [mbYes, mbNo], 0) = mrNo)
  then begin
    WriteJournal('System vom Anwender beendet - eigener Status: ' +
      MySystemStateText[VerifyWicoIni.MySystemState]);
    if (VerifyWicoIni.MySystemState = mssActiv) then
      TFormTabVerifyTakeOver(
        GetTabForm(TFormTabVerifyTakeOver)).ChangeToSecondary;

    for i := 0 to PageControl.PageCount-1 do begin
      p := GetTabForm(i);
      if (Assigned(p)) then p.Hide;
    end;
    FCanClose := True;
  end
  else FCanClose := False;

  inherited;
end;

{ Initialisiert das Formular                 }
{ Parameter: Flag (T=Init, F=Freigabe)       }
{--------------------------------------------}
procedure TFormVerifyWico22.InitComponents(bState: boolean);
{--------------------------------------------}
begin
  try
    inherited;

    if (bState) then begin
      StartFensterZeigen(Self.Caption);

      VerifyWicoIni := TVerifyWico22Ini.Create(
        ChangeFileExt(ParamStr(0), '.INI'));
      VerifyJournalCallback := Self.WriteJournal;
      WriteJournal('INI-Datei initialisiert (' + VerifyWicoIni.FileName + ')');

      sbtnReboot.Caption := '';
      FVerifyInterval := 10000;
      FModuleTimeout := 10*60000;  // Wartezeit für Starten/Beenden von Modulen
      InitJournal(True);

      WriteJournal('Starte System (Prüfintervall: ' +
        IntToStr(FVerifyInterval div 1000) + ' s, Timeout für Starten/Beenden: '
        + IntToStr(FModuleTimeout div 1000) +' s)');

      FTabFormList := TList.Create;

      WriteJournal('Initialisiere Datenprüfung');
      InitDataVerification(True);
      WriteJournal('Initialisiere Modulüberwachung');
      InitRuntimeVerification(True);
      WriteJournal('Initialisiere Systemüberwachung');
      InitTakeOverVerification(True);

      WindowState := wsMaximized;
      if (PageControl.ActivePageIndex >= 0) then
        pnTopRest.Caption := PageControl.ActivePage.Caption;
      WriteJournal('Startvorgang abgeschlossen');
      StartFensterSchliessen;
    end
    else begin
      // Alle eingetragenen Programme beenden
      InitModuleState(False);
      FreeAndNil(FTabFormList);
      WriteJournal('Beende Datenprüfung');
      InitDataVerification(False);
      WriteJournal('Beende Modulüberwachung');
      InitRuntimeVerification(False);
      WriteJournal('Beende Systemüberwachung');
      InitTakeOverVerification(False);
      WriteJournal('Beende Protokollierung');

      InitJournal(False);
      FreeAndNil(VerifyWicoIni);
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormVerifyWico22.InitComponents - ' + BoolToStr(bState, True));
  end;
end;

{--------------------------------------------}
procedure TFormVerifyWico22.InitJournal(bState: boolean);
{--------------------------------------------}
var
  pTabSheet : TTabSheet;
begin
  if (bState) then begin
    FJournal := TStringList.Create;  // Journal
    FMaxJournalList := 2000;         // 2000 Zeilen intern speichern
    FMaxJournalLines := 200;         // 200 Zeilen anzeigen
    pTabSheet := TTabSheet.Create(PageControl);
    pTabSheet.PageControl := Self.PageControl;
    pTabSheet.Caption := 'Journal';
    FMemoJournal := TMemo.Create(pTabSheet);
    FMemoJournal.Parent := pTabSheet;
    FMemoJournal.Align := alClient;
    FMemoJournal.ReadOnly := True;
  end
  else begin
    // Journal speichern
    if (FileExists(ChangeFileExt(ParamStr(0), '.LOG'))) then
      CopyFile(PChar(ChangeFileExt(ParamStr(0), '.LOG')),
        PChar(ChangeFileExt(ParamStr(0), '.~LO')), False);
    FJournal.SaveToFile(ChangeFileExt(ParamStr(0), '.LOG'));
    FreeAndNil(FJournal);
  end;
end;

{ Interne Exceptionverarbeitung              }
{ Parameter: Exception, Hilfstext            }
{--------------------------------------------}
procedure TFormVerifyWico22.MyExceptionHandling(
  E: Exception; const sText: string);
{--------------------------------------------}
begin
  WriteJournal('Exception: ' + sText + ' - ' + E.Message);
  WriteErrorLog('Fehler in VerifyWico22 -> ' + sText + ' - ' + E.Message);
end;

{----------------------------------------}
procedure TFormVerifyWico22.Reboot;
{----------------------------------------}
begin
  Self.Enabled := False;
  try
    WriteJournal('Starte Reboot ');
    // Flag für Beendigung setzen
    FCanClose := True;
    Delay(1000);
    // Timer abschalten
    Timer.Enabled := False;
    Timer.OnTimer := nil;
    // Alle Programme beenden
    FModuleTimeout := 60*1000;  // Jedes Modul hat max. 1 min zum Beenden
    WriteJournal('Beende Programme (Timout ' +
      IntToStr(FModuleTimeout div 1000) + (' sec.'));
    InitModuleState(False);
    // Status-Dateien löschen
    Delay(1000);
    WriteJournal('Lösche Flagdateien (' + VerifyWicoIni.MySystemFilePath + ')');
    MyDeleteFiles(VerifyWicoIni.MySystemFilePath +
      Format(C_SysFlagFile_SystemMask, [VerifyWicoIni.MySystemName]));
    Delay(1000);

    WriteJournal('Shut down (' + IntToStr(EWX_REBOOT + EWX_FORCE) + ')');
    WOSShutDown(EWX_REBOOT + EWX_FORCE);
  finally
    Self.Enabled := True;
  end;
end;

{--------------------------------------------}
procedure TFormVerifyWico22.PageControlChange(Sender: TObject);
{--------------------------------------------}
begin
  Self.Caption := FCaption + ' - [' + PageControl.ActivePage.Caption + ']';
  pnTopRest.Caption := PageControl.ActivePage.Caption;
  if (PageControl.ActivePageIndex >= 0) then begin
    with PageControl.Pages[PageControl.ActivePageIndex] do begin
      if (ControlCount = 1) and (Controls[0] is TForm) then
        TForm(Controls[0]).Show;
    end;
  end;
end;

{ Initialisiert das Datenbankhandling        }
{ Parameter: Flag (T=Init, F=Freigabe)       }
{--------------------------------------------}
procedure TFormVerifyWico22.InitDataVerification(bState: boolean);
{--------------------------------------------}
var
  i : integer;
begin
  try
    if (bState) then begin
      InsertTab(TFormTabVerifyData);
    end
    else begin
      i := IndexOfTab(TFormTabVerifyData);
      if (i >= 0) then PageControl.Pages[i].Free;
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormVerifyWico22.InitDataVerification - ' + BoolToStr(bState, True));
  end;
end;

{ Initialisiert die Modulüberwachung         }
{ Parameter: Flag (T=Init, F=Freigabe)       }
{--------------------------------------------}
procedure TFormVerifyWico22.InitRuntimeVerification(bState: boolean);
{--------------------------------------------}
var
  i : integer;
begin
  try
    if (bState) then begin
      InsertTab(TFormTabVerifyModules);
    end
    else begin
      i := IndexOfTab(TFormTabVerifyModules);
      if (i >= 0) then PageControl.Pages[i].Free;
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormVerifyWico22.InitRuntimeVerification - ' + BoolToStr(bState, True));
  end;
end;

{ Initialisiert die Funktionsübernahme       }
{ Parameter: Flag (T=Init, F=Freigabe)       }
{--------------------------------------------}
procedure TFormVerifyWico22.InitTakeOverVerification(bState: boolean);
{--------------------------------------------}
var
  i : integer;
begin
  try
    if (bState) then begin
      InsertTab(TFormTabVerifyTakeOver);
    end
    else begin
      i := IndexOfTab(TFormTabVerifyTakeOver);
      if (i >= 0) then PageControl.Pages[i].Free;
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormVerifyWico22.InitTakeOverVerification - ' + BoolToStr(bState, True));
  end;
end;

{ Gibt Index eines Tabs mit Fenster zurück   }
{ Parameter: Fensterklasse                   }
{ Rückgabe: TabIndex                         }
{--------------------------------------------}
function TFormVerifyWico22.IndexOfTab(
  pFormClass: TFormVerifyWico22TabClass): integer;
{--------------------------------------------}
var
  i : integer;
begin
  Result := -1;
  for i := 0 to PageControl.PageCount-1 do begin
    with PageControl.Pages[i] do begin
      if (ControlCount = 1) and (Controls[0].ClassType = pFormClass) then begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

{ Fügt Tab mit einem Fenster ein             }
{ Parameter: Fensterklasse                   }
{--------------------------------------------}
procedure TFormVerifyWico22.InsertTab(pFormClass: TFormVerifyWico22TabClass);
{--------------------------------------------}
var
  pTab : TTabSheet;
begin
  if (IndexOfTab(pFormClass) < 0) then begin
    pTab := TTabSheet.Create(Self.PageControl);
    pTab.PageControl := Self.PageControl;
    FTabFormList.Add(pFormClass.Create(pTab));
    with TFormVerifyWico22Tab(FTabFormList[FTabFormList.Count-1]) do begin
      Parent := pTab;
      BorderStyle := bsNone;
      Align := alClient;
      if (Self.Visible) then
        Show;
      Enabled := True;
      pTab.Caption := Caption;
      ExceptionHandling := Self.MyExceptionHandling;
      SetGlobalModuleState := Self.InitModuleState;
      JournalCallback := Self.WriteJournal;
      DataTakeOver := Self.DataTakeOver;
    end;
  end;
end;

{ Gibt Fenster eines Tabs zurück             }
{ Parameter: Index des TabSheet              }
{ Rückgabe: Fenster aus TabSheet             }
{--------------------------------------------}
function TFormVerifyWico22.GetTabForm(iIndex: integer): TFormVerifyWico22Tab;
{--------------------------------------------}
begin
  with PageControl.Pages[iIndex] do begin
    if (ControlCount = 1) and (Controls[0] is TFormVerifyWico22Tab)
    then Result := TFormVerifyWico22Tab(Controls[0])
    else Result := nil;
  end;
end;

{ Gibt Fenster eines Tabs zurück             }
{ Parameter: Fensterklasse                   }
{ Rückgabe: Fenster aus TabSheet             }
{--------------------------------------------}
function TFormVerifyWico22.GetTabForm(
      pFormClass: TFormVerifyWico22TabClass): TFormVerifyWico22Tab;
{--------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := IndexOfTab(pFormClass);
  if (iIndex >= 0) then Result := GetTabForm(iIndex) else Result := nil;
end;

{--------------------------------------------}
procedure TFormVerifyWico22.TimerTimer(Sender: TObject);
{--------------------------------------------}
const
  C_Started : boolean = False;
var
  i      : integer;
  s      : string;
  bError : boolean;
  pState : TMySystemState;
begin
  inherited;

  Timer.Enabled := False;
  try
    if (not pmiMinimized.Checked) then PageControlChange(nil);


    if (C_Started) then begin
      if (Timer.Interval <> FVerifyInterval) then
        Timer.Interval := FVerifyInterval;

      // Liste der aktuell laufenden Module holen
  //    FAktModuleList := ListRunningModules;
      // Alle Unterfenster Prüfung durchführen lassen
      bError := False;
      for i := 0 to FTabFormList.Count-1 do begin
        if (not TFormVerifyWico22Tab(FTabFormList[i]).Verify) then begin
          s := 'Fehler in ' + TFormVerifyWico22Tab(FTabFormList[i]).Caption;
          pnStatus.Caption := DateTimeToStr(Now) + ': ' + s;
          pnStatus.Color := clRed;
          WriteJournal(s);

          // Reaktion
          bError := True;
          Break;
        end;
      end;

      if (bError) then begin
        shMyState.Brush.Color := clRed;
        if (TFormTabVerifyData(GetTabForm(TFormTabVerifyData)).FatalBdeError) or
          (VerifyWicoIni.MySystemState = mssInactiv)
        then begin
          WriteJournal('Fataler Fehler - BDE-Status: ' + BoolToStr(
             not TFormTabVerifyData(GetTabForm(TFormTabVerifyData)).FatalBdeError,
             True));
          WriteJournal('Fataler Fehler - eigener System-Status: ' +
            MySystemStateText[VerifyWicoIni.MySystemState]);
          WriteJournal('Fataler Fehler - entfernter System-Status: ' +
            MySystemStateText[TFormTabVerifyTakeOver(
              GetTabForm(TFormTabVerifyTakeOver)).OtherState]);
          WriteJournal('Fataler Fehler - reboote System');
          Reboot;
        end
        else begin
          WriteJournal('Fataler Fehler - Schalte auf Sekundärsystem um');
          if (not TFormTabVerifyTakeOver(
            GetTabForm(TFormTabVerifyTakeOver)).ChangeToSecondary) then
          begin
            WriteJournal(
              'Umschaltung auf Sekundärsystem fehlgeschlagen - boote System');
            Reboot;
          end
          else begin
            MyDeleteFiles(
              VerifyWicoIni.ModuleFilePath + '*' + C_VerifyExtension_Module);
            MyDeleteFiles(
              VerifyWicoIni.ModuleFilePath + '*' + C_VerifyExtension_Control);
          end;
        end;
      end
      else begin
        pState := VerifyWicoIni.MySystemState;
        if (pState in [mssActivating, mssActiv]) then
          shMyState.Brush.Color := clGreen
        else if (pState in [mssDeactivating, mssInactiv]) then
          shMyState.Brush.Color := clYellow
        else shMyState.Brush.Color := clBtnFace;

        pState :=
          TFormTabVerifyTakeOver(GetTabForm(TFormTabVerifyTakeOver)).OtherState;
        if (pState in [mssActivating, mssActiv]) then
          shOtherState.Brush.Color := clGreen
        else if (pState in [mssDeactivating, mssInactiv]) then
          shOtherState.Brush.Color := clYellow
        else shOtherState.Brush.Color := clBtnFace;
      end;
    end
    else begin
      C_Started := True;
      Timer.Interval := VerifyWicoIni.StartInterval;
    end;
  finally
    Timer.Enabled := not FCanClose;
  end;
end;

{ Starten/Beenden der aktuellen Module       }
{   Globale Funktion                         }
{ Parameter: T=Start, F=Stop                 }
{--------------------------------------------}
procedure TFormVerifyWico22.InitModuleState(bState: boolean);
{-----------;---------------------------------}
const
  C_LastState: boolean = True;  // Module wurden zuletzt gestartet
var
  i : integer;
  p : TComInfoWindow;
  s : string;
  pJournal : TSetCaptionEvent;
begin
  if (bState <> C_LastState) then
  try
    C_LastState := bState;
    if (bState) then s := 'Module starten' else s := 'Module beenden';
    p := TComInfoWindow.CreateInfoWindow(s);
    pJournal := VerifyJournalCallback;
    with TStringList.Create do      // Modulliste
    try
      VerifyJournalCallback := Self.WriteJournal;
      // Infofenster initialisieren
      p.EnableStop := False;
      p.SetInfoText(s);
      p.Show;
      // Programmliste für evt. Neustart merken
      if (not (bState)) then FAktModuleList := ListRunningModules;
      CommaText := FAktModuleList;  // Module sind als Commatext gespeichert
      for i := 0 to Count-1 do begin
        p.SetInfoText(Strings[i]);
        if (bState) then begin      // Programme aus interner Liste starten
          StartModule(Strings[i], FModuleTimeout, True);
          Delay(1000);  // Pause zwischen Programmstarts
        end
        else begin                  // Alle Programme beenden
          StopModule(Strings[i], FModuleTimeout);
          Delay(1000);  // Pause zwischen Programmbeendigungen
        end;
      end;
    finally
      Free;
      p.Free;
      VerifyJournalCallback := pJournal;
    end;
  except
    on E:Exception do begin
      MyExceptionHandling(E,
        'TFormVerifyWico22.InitModuleState - ' + BoolToStr(bState, True));
    end;
  end;
end;

{--------------------------------------------}
procedure TFormVerifyWico22.aRebootExecute(Sender: TObject);
{--------------------------------------------}
begin
  if (MessageDlg('Soll der Rechner wirklich gebootet werden?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes) and
    (MessageDlg('Soll der Bootvorgang abgebrochen werden?', mtConfirmation,
    [mbYes, mbNo], 0) = mrNo) then
  begin
    WriteJournal('Bootvorgang vom Anwender ausgelöst');
    aReboot.Enabled := False;
    try
      Reboot;
    finally
      aReboot.Enabled := False;
    end;
  end;
end;

{--------------------------------------------}
procedure TFormVerifyWico22.WriteJournal(sText: string);
{--------------------------------------------}
begin
  if (Assigned(FJournal)) then begin
    FJournal.Add(DateTimeToStr(Now) + ': ' + sText);
    while (FJournal.Count > FMaxJournalList) do FJournal.Delete(0);
    FMemoJournal.Lines.Add(DateTimeToStr(Now) + ': ' + sText);
    while (FMemoJournal.Lines.Count > FMaxJournalLines) do
      FMemoJournal.Lines.Delete(0);
    if (VerifyWicoIni.IsLog) then
      WriteErrorLog(sText, ChangeFileExt(ParamStr(0), '.txt'));
  end;
end;

{--------------------------------------------}
function TFormVerifyWico22.DataTakeOver: boolean;
{--------------------------------------------}
begin
  Result :=
    TFormTabVerifyData(GetTabForm(TFormTabVerifyData)).DataTransferForTakeOver;
end;

end.
