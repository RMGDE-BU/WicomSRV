{------------------------------------------------------------------------------}
{ Tabulator-Fenster für Überwachung für WICO22-Funktionsübernahme              }
{                                                                              }
{ 21.09.2007  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2007                                      }
{------------------------------------------------------------------------------}
unit FTabVerifyTakeOver;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ShellApi, Buttons,
  FDefaultTab, VerifyFlags, VerifyWico22Ini, GD_Utils, T_Zeit, Service_Utils,
  KWLink32, InstTest, COM_Utils;

type
  TFormTabVerifyTakeOver = class(TFormVerifyWico22Tab)
    pnSystem: TPanel;
    gbSystem: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    eOwnState: TEdit;
    eOwnLastDate: TEdit;
    eOwnPath: TEdit;
    eOtherState: TEdit;
    eOtherLastDate: TEdit;
    eOtherPath: TEdit;
    Label7: TLabel;
    eOwnSignatur: TEdit;
    Label8: TLabel;
    eOtherSignatur: TEdit;
    pnJournal: TPanel;
    Splitter1: TSplitter;
    gbJournal: TGroupBox;
    lvJournal: TListView;
    sbtnChangeToSecondary: TSpeedButton;
    procedure lvExternalModulesResize(Sender: TObject); override;
    procedure sbtnChangeToSecondaryClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FOtherSystem  : string;
    FOtherState   : TMySystemState;
    FForceChange  : boolean;
    FBlockJournal : boolean;
    procedure SetSystemState(pSysState: TMySystemState);
    function GetSystemState: TMySystemState;
    procedure HandleSystemStates(pMyState, pOtherState: TMySystemState);
  protected
    { Protected-Deklarationen }
    procedure InitComponents(bState: boolean); override;
    procedure LoadExternalModuleList; override;
    function InitWico22Modules(bState: boolean): boolean;
    procedure WriteJournal(sText: string); overload; override;  // Reihenfolge beachten!
    procedure WriteJournal(pOldState, pNewState, pOtherState: TMySystemState;
      const sText: string; bRedirect: boolean = True); overload;
  public
    { Public-Deklarationen }
    function Verify: boolean; override;
    function ChangeToSecondary: boolean;
    procedure Actualize; override;
    property OtherState: TMySystemState read FOtherState;
  end;

implementation

{$R *.dfm}

resourcestring
  S_Long_EXE                 = 'Win32Exe';
  S_Long_Service             = 'NT Service';
  S_Long_Activ               = 'Aktiv';
  S_Long_Inactiv             = 'Inaktiv';
  S_Long_Error               = 'Fehler';

const
  C_ListIndex_Path           = 2;
  C_ListIndex_ExeService     = 3;
  C_ListIndex_ActState       = 4;
  C_ListIndex_SecondaryState = 5;
  C_ListIndex_PrimaryState   = 6;

  C_Short_EXE                = '1';
  C_Short_Service            = '2';
  C_Short_Activ              = '1';
  C_Short_Inactiv            = '0';

{---------------------------- TFormTabVerifyTakeOver --------------------------}

{ Start von externen Programmmodulen         }
{--------------------------------------------}
procedure TFormTabVerifyTakeOver.InitComponents(bState: boolean);
{--------------------------------------------}
begin
  inherited;

  if (bState) then begin
    Randomize;
    sbtnChangeToSecondary.Enabled := False;
    FOtherSystem := '';     // Bezeichnung Fremdsystem
    FOtherState := mssNone;
    FForceChange := False;  // Kein aktiver Statuswechsel
    FBlockJournal := False; // Doppelte Journaleinträge verhindern
    // Anzeigen initialisieren
    eOwnState.Text := '-';
    eOwnLastDate.Text := '-';
    eOwnPath.Text := VerifyWicoIni.MySystemFilePath;
    eOtherState.Text := '-';
    eOtherLastDate.Text := '-';
    eOtherPath.Text := VerifyWicoIni.OtherSystemFilePath;
    // Altdateien löschen
    if (MyDeleteFiles(VerifyWicoIni.MySystemFilePath + C_SysFlagFile_Mask) > 0)
    then Delay(15000); // Warten, dass ein Zweitsystem neue Daten schreibt
    // Zustand auf auf Initialisieren setzen
    GetSystemState;
    VerifyWicoIni.MySystemState := mssInitializing;
    SetSystemState(mssInitializing);
    // Fremdsystem abfragen
    GetSystemState;
  end
  else begin
    // Dateien löschen
    MyDeleteFiles(VerifyWicoIni.MySystemFilePath +
      Format(C_SysFlagFile_SystemMask, [VerifyWicoIni.MySystemName]));
  end;
end;

{ Laden der Liste externer Programmmodulen   }
{--------------------------------------------}
procedure TFormTabVerifyTakeOver.LoadExternalModuleList;
{--------------------------------------------}
var
  i, j : integer;
  s    : string;
begin
  inherited;

  // Flagdateien löschen .. muss noch erfolgen

  try
    lvExternalModules.Items.BeginUpdate;
    try
      for i := 0 to lvExternalModules.Items.Count-1 do begin
        s := ModuleList[i];
        with lvExternalModules.Items[i] do
          for j := C_ListIndex_Path to lvExternalModules.Columns.Count-1 do
          begin
            if (j = C_ListIndex_Path) then begin
              if (VerifyWicoIni.GetIsWin32Exe(s))
              then SubItems.Add(VerifyWicoIni.GetFilePath(s))
              else SubItems.Add(VerifyWicoIni.GetServiceName(s));
            end
            else if (j = C_ListIndex_ExeService) then begin
              if (VerifyWicoIni.GetIsWin32Exe(s))
              then SubItems.Add(S_Long_EXE)
              else SubItems.Add(S_Long_Service);
            end
            else if (j = C_ListIndex_ActState) then begin
              SubItems.Add('-');
            end
            else if (j = C_ListIndex_SecondaryState) then begin
              if (VerifyWicoIni.GetStartSecondary(s))
              then SubItems.Add(S_Long_Activ)
              else SubItems.Add(S_Long_Inactiv);
            end
            else if (j = C_ListIndex_PrimaryState) then begin
              if (VerifyWicoIni.GetStartPrimary(s))
              then SubItems.Add(S_Long_Activ)
              else SubItems.Add(S_Long_Inactiv);
            end;
          end;
      end;
    finally
      lvExternalModules.Items.EndUpdate;
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormTabVerifyTakeOver.LoadExternalModuleList');
  end;
end;

{ Fenster aktualisieren                      }
{--------------------------------------------}
procedure TFormTabVerifyTakeOver.Actualize;
{--------------------------------------------}
begin
  inherited;

  try
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormTabVerifyTakeOver.LoadExternalModuleList');
  end;
end;

{ Überprüfung durchführen                    }
{--------------------------------------------}
function TFormTabVerifyTakeOver.Verify: boolean;
{--------------------------------------------}
begin
  try
    if (not FForceChange) then begin
      SetSystemState(VerifyWicoIni.MySystemState);
      HandleSystemStates(VerifyWicoIni.MySystemState, GetSystemState);
    end;
    Result := True;
  except
    on E:Exception do begin
      Result := False;
      MyExceptionHandling(E,
        'TFormTabVerifyTakeOver.LoadExternalModuleList');
    end;
  end;
end;

{ Status des zweitem System zurückgeben      }
{--------------------------------------------}
function TFormTabVerifyTakeOver.GetSystemState: TMySystemState;
{--------------------------------------------}
var
  sFileName : string;
  sSender   : string;
  pSl       : TStrings;
  i, iState : integer;
  iFileAge  : integer;
begin
  // Alle System-Dateien listen
  pSl := TStringList.Create;
  try
    MyFindFiles(VerifyWicoIni.OtherSystemFilePath + C_SysFlagFile_Mask, pSl);

    // Falls beide Systeme sich im gleichen Pfad tummeln: Self eleminieren
    for i := pSl.Count-1 downto 0 do begin
      sFileName := ChangeFileExt(ExtractFileName(pSl[i]), '');
      sSender := UpperCase(GetStringPart(sFileName, 2, '_'));
      if (sSender = UpperCase(VerifyWicoIni.MySystemName)) then pSl.Delete(i);
    end;

    if (pSl.Count = 1) then begin  // Es kann nur einen geben ...
      iFileAge := FileAge(pSl[0]);
      eOtherLastDate.Text := DateTimeToStr(FileDateToDateTime(iFileAge));
      // Status älter als 5 min ...
      if (FileDateToDateTime(iFileAge) + EncodeTime(0, 5, 0, 0) <= Now) then
      begin
        if (FOtherState <> mssNone) then   // ... bei Wechsel Journaleintrag
          WriteJournal(VerifyWicoIni.MySystemState,
            VerifyWicoIni.MySystemState, mssInactiv, 'Zeitüberschreitung');
        Result := mssNone;   // ... bedeutet nicht vorhanden
      end
      else begin
        sFileName := ChangeFileExt(ExtractFileName(pSl[0]), '');
        iState := StrToIntDef(GetStringPart(sFileName, 3, '_'), 0);
        FOtherSystem := GetStringPart(sFileName, 2, '_');
        eOtherSignatur.Text := FOtherSystem;

        Result := TMySystemState(iState);
      end;
    end
    else begin
      if (pSl.Count > 1) then WriteJournal(VerifyWicoIni.MySystemState,
        VerifyWicoIni.MySystemState, mssNone, 'Mehrfache Steuerdateien');
      Result := mssNone;
    end;

    eOtherState.Text := MySystemStateText[Result];
    if (Result <> FOtherState) then begin
      if (not (VerifyWicoIni.MySystemState in [mssNone, mssInitializing])) then
        WriteJournal(VerifyWicoIni.MySystemState,
          VerifyWicoIni.MySystemState, Result, 'Entfernte Umschaltung: ' +
          MySystemStateText[FOtherState] + ' -> ' + MySystemStateText[Result]);
      FOtherState := Result;
    end;
  finally
    pSl.Free;
  end;
end;

{ Status des eigenen Systems setzen          }
{--------------------------------------------}
procedure TFormTabVerifyTakeOver.SetSystemState(pSysState: TMySystemState);
{--------------------------------------------}
var
  sFileName : string;
  pSl       : TStrings;
  i         : integer;
begin
  eOwnSignatur.Text := VerifyWicoIni.MySystemName;
  // FlagFile erstellen
  CreateSystemStateFile(pSysState);
(*
  if (pSysState <> mssNone)
  then sFileName := VerifyWicoIni.MySystemFilePath +
    Format(C_SysFlagFile_StateMask,
      [VerifyWicoIni.MySystemName, IntToStr(Integer(pSysState))])
  else sFileName := '';
  // Alle System-Dateien listen
  pSl := TStringList.Create;
  try
    MyFindFiles(VerifyWicoIni.MySystemFilePath +
      Format(C_SysFlagFile_SystemMask, [VerifyWicoIni.MySystemName]), pSl);
    // Alle Dateien <> sFileName löschen
    for i := 0 to pSl.Count-1 do
      if (UpperCase(pSl[i]) <> UpperCase(sFileName)) then DeleteFile(pSl[i]);
  finally
    pSl.Free;
  end;
  // Flag-File schreiben
  if (sFileName <> '') then
    with TFileStream.Create(sFileName, fmCreate) do Free;
*)
  eOwnState.Text := MySystemStateText[pSysState];
  eOwnLastDate.Text := DateTimeToStr(Now);

  // Aktivierung oder Deaktivierung ?
  if (VerifyWicoIni.MySystemState <> pSysState) then begin
    WriteJournal(
      VerifyWicoIni.MySystemState, pSysState, GetSystemState, 'Umschaltung: ' +
      MySystemStateText[VerifyWicoIni.MySystemState] + ' -> ' +
      MySystemStateText[pSysState]);
    VerifyWicoIni.MySystemState := pSysState;
  end;
  // Schaltung immer initiieren - wird ggf. intern abgefangen
  if ((pSysState in [mssActiv]) and
      (FOtherState in [mssNone, mssInactiv])) or
    ((pSysState in [mssInactiv, mssDeactivating]) and
     (FOtherState in [mssActivating, mssActiv]))
  then InitWico22Modules(pSysState in [mssActiv]);
end;

{--------------------------------------------}
procedure TFormTabVerifyTakeOver.HandleSystemStates(
  pMyState, pOtherState: TMySystemState);
{--------------------------------------------}
begin
  case pOtherState of
    mssNone :
      case pMyState of
        mssInitializing : SetSystemState(mssActivating);
        mssActiv : ;
        mssInactiv : SetSystemState(mssActivating);
        mssActivating : SetSystemState(mssActiv);
        mssDeactivating : SetSystemState(mssActivating);
      end;
    mssInitializing :
      case pMyState of
        mssInitializing : SetSystemState(mssInactiv);
        mssActiv : ;
        mssInactiv : SetSystemState(mssActivating);
        mssActivating : SetSystemState(mssActiv);
        mssDeactivating : SetSystemState(mssActivating);
      end;
    mssActiv :
      case pMyState of
        mssInitializing : SetSystemState(mssInactiv);
        mssActiv : begin
                     WriteJournal(
                       mssActiv, mssDeactivating, mssActiv, 'Kollision');
                     SetSystemState(mssDeactivating);
                     Delay(Random(5000)+1);
                     HandleSystemStates(mssDeactivating, GetSystemState);
                   end;
        mssInactiv : ;
        mssActivating :  SetSystemState(mssInactiv);
        mssDeactivating : SetSystemState(mssInactiv);
      end;
    mssInactiv :
      case pMyState of
        mssInitializing : SetSystemState(mssActivating);
        mssActiv : ;
        mssInactiv : begin
                       WriteJournal(
                         mssInactiv, mssActivating, mssInactiv, 'Kollision');
                       SetSystemState(mssActivating);
                       Delay(Random(5000)+1);
                       HandleSystemStates(mssActivating, GetSystemState);
                     end;
        mssActivating : SetSystemState(mssActiv);
        mssDeactivating : SetSystemState(mssActivating);
      end;
    mssActivating :
      case pMyState of
        mssInitializing : SetSystemState(mssActivating);
        mssActiv : ;
        mssInactiv : SetSystemState(mssActivating);
        mssActivating : begin
                          WriteJournal(
                            mssActivating, mssActiv, mssActivating, 'Kollision');
                          SetSystemState(mssActiv);
                          Delay(Random(5000)+1);
                          HandleSystemStates(mssActiv, GetSystemState);
                        end;
        mssDeactivating : SetSystemState(mssInactiv);
      end;
    mssDeactivating :
      case pMyState of
        mssInitializing : SetSystemState(mssActivating);
        mssActiv : ;
        mssInactiv : SetSystemState(mssActivating);
        mssActivating : SetSystemState(mssActiv);
        mssDeactivating : begin
                            WriteJournal(mssDeactivating, mssActivating,
                              mssDeactivating, 'Kollision');
                            SetSystemState(mssActivating);
                            Delay(Random(5000)+1);
                            HandleSystemStates(mssActivating, GetSystemState);
                          end;
      end;
  end;

  // Umschaltfunktion enablen
  sbtnChangeToSecondary.Enabled := (VerifyWicoIni.MySystemState = mssActiv) and
    (FOtherState = mssInactiv);
end;

{ Eigenen Module starten/beenden             }
{--------------------------------------------}
function TFormTabVerifyTakeOver.InitWico22Modules(bState: boolean): boolean;
{--------------------------------------------}
const
  C_IsStarted: boolean = False;
  C_LastState: boolean = False;
var
  i, iIx   : integer;
  pJournal : TSetCaptionEvent;
begin
  try
    // Gleichen Status nicht zweimal nacheinander schalten
    if (not C_IsStarted) or (C_LastState <> bState) then begin
      pJournal := VerifyJournalCallback;
      try
        C_IsStarted := True;    // Erstmalige Umschaltung erfolgt immer
        C_LastState := bState;  // letztes Status merken
        VerifyJournalCallback := Self.WriteJournal;

        Result := True;
        for i := 0 to ModuleList.Count-1 do begin
          if (not StartModule(ModuleList[i], 10*60000)) then begin
            Result := False;
            if (ModuleList.Count = lvExternalModules.Items.Count) then
              lvExternalModules.Items[i].SubItems[C_ListIndex_ActState-1] :=
                'Fehler';
          end
          else begin
            if (bState)
            then iIx := C_ListIndex_PrimaryState
            else iIx := C_ListIndex_SecondaryState;
            if (ModuleList.Count = lvExternalModules.Items.Count) then
              lvExternalModules.Items[i].SubItems[C_ListIndex_ActState-1] :=
                lvExternalModules.Items[i].SubItems[iIx-1];
          end;
          Delay(1000);  // Pause zwischen Programmstarts
        end;
      finally
        VerifyJournalCallback := pJournal;
      end;
    end
    else Result := True;
  except
    on E:Exception do begin
      Result := False;
      MyExceptionHandling(E,
        'TFormTabVerifyTakeOver.InitWico22Modules');
    end;
  end;
end;

{--------------------------------------------}
procedure TFormTabVerifyTakeOver.WriteJournal(pOldState, pNewState,
  pOtherState: TMySystemState; const sText: string; bRedirect: boolean = True);
{--------------------------------------------}
begin
  with lvJournal.Items.Add do begin
    Caption := DateTimeToStr(Now);
    SubItems.Add(MySystemStateText[pOldState]);
    SubItems.Add(MySystemStateText[pNewState]);
    SubItems.Add(MySystemStateText[pOtherState]);
    SubItems.Add(sText);
  end;

  if (bRedirect) then begin
    FBlockJournal := True;
    WriteJournal(sText);
  end;
end;

{--------------------------------------------}
procedure TFormTabVerifyTakeOver.WriteJournal(sText: string);
{--------------------------------------------}
begin
  inherited;

  if (not FBlockJournal)
  then WriteJournal(mssNone, mssNone, mssNone, sText, False)
  else FBlockJournal := False;
end;

{--------------------------------------------}
procedure TFormTabVerifyTakeOver.lvExternalModulesResize(Sender: TObject);
{--------------------------------------------}
var
  i, iWidth : integer;
begin
  inherited;

  try
    if (not (csDestroying in Self.ComponentState)) and
      (Assigned(lvJournal)) and (lvJournal.Columns.Count > 0) then
    begin
      iWidth := (lvJournal.Width - (lvJournal.Columns.Count+1) -
        GetSystemMetrics(SM_CXVSCROLL)) div (lvJournal.Columns.Count + 1);
      for i := 0 to lvJournal.Columns.Count-2 do
        lvJournal.Columns[i].Width := iWidth;
      lvJournal.Columns[lvJournal.Columns.Count-1].Width := 2*iWidth;
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormTabVerifyTakeOver.lvExternalModulesResize');
  end;
end;

{--------------------------------------------}
procedure TFormTabVerifyTakeOver.sbtnChangeToSecondaryClick(Sender: TObject);
{--------------------------------------------}
begin
  if (FOtherState = mssInactiv) then begin
    if (MessageDlg('Soll das bisherige Sekundärsystem aktiviert werden?',
         mtConfirmation, [mbYes, mbNo], 0) = mrYes) and
     (MessageDlg('Das bisherige System wird dadurch abgeschaltet!'#13#10#13#10 +
       'Soll das bisherige System aktiv bleiben?',
         mtConfirmation, [mbYes, mbNo], 0) = mrNo)
    then begin
      sbtnChangeToSecondary.Enabled := False;
      Application.MainForm.Enabled := False;
      try
        WriteJournal('Systemumschaltung durch Anwender');
        ChangeToSecondary;
      finally
        Application.MainForm.Enabled := True;
      end;
    end;
  end
  else begin
    if (FOtherState = mssNone)
    then MessageDlg('Das Sekundärsystem kann nicht identifiziert werden.',
        mtError, [mbOk], 0)
    else MessageDlg('Das Sekundärsystem muss im Status "inaktiv" sein.',
        mtInformation, [mbOk], 0)
  end;
end;

{--------------------------------------------}
function TFormTabVerifyTakeOver.ChangeToSecondary: boolean;
{--------------------------------------------}
var
  iStop : Cardinal;
  pInfo : TComInfoWindow;
begin
  FForceChange := True;
  try
    if (FOtherState = mssInactiv) and (VerifyWicoIni.MySystemState = mssActiv)
    then begin
      pInfo := TComInfoWindow.CreateInfoWindow('Systemumschaltung');
      try
        pInfo.EnableStop := False;
        pInfo.Show;
        // Module herunterfahren
        WriteJournal('Beeende Module');
        pInfo.SetInfoText('Beeende Module');
        SetGlobalModuleState(False);
        // Datenabgleich durchführen
        WriteJournal('Führe Datenabgleich durch');
        pInfo.SetInfoText('Führe Datenabgleich durch');
        DataTakeOver;
        // System erst jetzt auf "Deaktivierung" setzen
        WriteJournal('Deaktiviere System');
        pInfo.SetInfoText('Deaktiviere System');
        SetSystemState(mssDeactivating);
        iStop := GetTickCount + 60000;  // 1 min zur Statusänderung lassen
        while (GetTickCount <= iStop) do begin
          Delay(100);
          if (GetSystemState in [mssActivating, mssActiv]) then Break;
        end;
        WriteJournal(
          'Entfernter Systemstatus: ' + MySystemStateText[FOtherState]);
        pInfo.SetInfoText(
          'Entfernter Systemstatus: ' + MySystemStateText[FOtherState]);
        Delay(1000);
        Result := (FOtherState in [mssActivating, mssActiv]);
      finally
        pInfo.Free;
      end;
    end
    else begin
      WriteJournal('Umschalten nicht möglich - entfernter Systemstatus: ' +
        MySystemStateText[FOtherState]);
      Result := False;
    end;
  finally
    FForceChange := False;
  end;
end;

end.
