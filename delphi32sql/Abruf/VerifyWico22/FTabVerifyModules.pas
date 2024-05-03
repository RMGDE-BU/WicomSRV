{------------------------------------------------------------------------------}
{ Tabulator-Fenster für Überwachung von WICO22-Modules                         }
{                                                                              }
{ 21.09.2007  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2007                                      }
{------------------------------------------------------------------------------}
unit FTabVerifyModules;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  FDefaultTab, GD_Utils, VerifyWico22Ini, VerifyFlags, Buttons;

type
  TFormTabVerifyModules = class(TFormVerifyWico22Tab)
    pnSystem: TPanel;
    gbSystem: TGroupBox;
    Label1: TLabel;
    eOwnState: TEdit;
    sbtnSetGlobalModuleState: TSpeedButton;
    Splitter1: TSplitter;
    pnJournal: TPanel;
    gbJournal: TGroupBox;
    lvJournal: TListView;
    procedure sbtnSetGlobalModuleStateClick(Sender: TObject);
    procedure lvExternalModulesResize(Sender: TObject); override;
  private
    { Private-Deklarationen }
    function GetVerifyFileName(const sExeName: string): string;
    function GetLastFileDate(const sExeName: string; var iFileDate: integer;
      var dtDateTime: TDateTime): boolean;
    procedure SetLastFileDate(const sExeName: string; iFileDate: integer);
    function VerifyModuleFlag(const sExeName, sFlagFile: string): boolean;
  protected
    { Protected-Deklarationen }
    procedure LoadExternalModuleList; override;
    procedure WriteJournal(sText: string); override;
  public
    { Public-Deklarationen }
    function Verify: boolean; override;
    procedure Actualize; override;
  end;

implementation

uses TypInfo;

{$R *.dfm}

const
  C_FatalTimeDiff            = 5*60;  // 5 min;

  C_StringIndex_Description  = 1;  // Stringindex Beschreibung
  C_StringIndex_FileName     = 2;  // Stringindex Beschreibung
  C_StringIndex_LastFileDate = 3;  // Zeitstempel der letzten Flag-Datei
  C_StringIndex_LastChangeTime = 4;  // Datum des letzten Wechsels
  C_StringIndex_Commentary   = 5;  // Bemerkung

  C_ViewIndex_Description    = 0;  // Spaltenindex Beschreibung
  C_ViewIndex_FileName       = 0;  // Spaltenindex Beschreibung
  C_ViewIndex_LastFileDate   = 1;  // Datum der letzten Flag-Datei
  C_ViewIndex_LastTestDate   = 2;  // Datum der letzten Prüfung
  C_ViewIndex_Commentary     = 3;  // Bemerkung

  C_JournalIndex_Description = 1;  // Journalspalte: Beschreibung

{----------------------------- TFormTabVerifyModules --------------------------}

{ Laden der Liste externer Programmmodulen   }
{--------------------------------------------}
procedure TFormTabVerifyModules.LoadExternalModuleList;
{--------------------------------------------}
var
  i, j : integer;
begin
  inherited;

  // Flagdateien löschen
  FilePath := VerifyWicoIni.ModuleFilePath;
  MyDeleteFiles(FilePath + '*' + C_VerifyExtension_Module);
  MyDeleteFiles(FilePath + '*' + C_VerifyExtension_Control);

  try
    lvExternalModules.Items.BeginUpdate;
    try
      for i := 0 to lvExternalModules.Items.Count-1 do begin
        with lvExternalModules.Items[i] do
          for j := 2 to lvExternalModules.Columns.Count-1 do
            SubItems.Add('-');
      end;
    finally
      lvExternalModules.Items.EndUpdate;
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormTabVerifyModules.LoadExternalModuleList');
  end;
end;

{ Fenster aktualisieren                      }
{--------------------------------------------}
procedure TFormTabVerifyModules.Actualize;
{--------------------------------------------}
begin
  inherited;

  try
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormTabVerifyModules.LoadExternalModuleList');
  end;
end;

{ Überprüfung durchführen                    }
{--------------------------------------------}
function TFormTabVerifyModules.Verify: boolean;
{--------------------------------------------}
var
  i         : integer;
  sFileName : string;
  dt        : TDateTime;
  bOk       : boolean;
begin
  try
    // Statusanzeige
    if (VerifyWicoIni.MySystemState = mssActiv) then
      eOwnState.Text := S_Long_Activ
    else if (VerifyWicoIni.MySystemState = mssInactiv) then
      eOwnState.Text := S_Long_Inactiv
    else eOwnState.Text := '-';

    Result := True;
    // Nur im Aktiv- und Inaktiv-Zustand wird geprüft
    if (not (VerifyWicoIni.MySystemState in [mssActiv, mssInactiv])) then Exit;

    for i := 0 to ModuleList.Count-1 do begin
      // Muss Modul in diesem Status geprüft werden ?
      if ((VerifyWicoIni.MySystemState = mssActiv) and
        (not VerifyWicoIni.GetStartPrimary(ModuleList[i]))) or
        ((VerifyWicoIni.MySystemState = mssInactiv) and
        (not VerifyWicoIni.GetStartSecondary(ModuleList[i])))
      then Continue;

      sFileName := GetVerifyFileName(ModuleList[i]);
      if (sFileName <> '')
      then dt := FileDateToDateTime(FileAge(sFileName))
      else dt := 0;

      bOk := VerifyModuleFlag(ModuleList[i], sFileName);

      if (ModuleList.Count = lvExternalModules.Items.Count) then begin
        if (dt > 0)
        then lvExternalModules.Items[i].SubItems[C_ViewIndex_LastFileDate] :=
          DateTimeToStr(dt)
        else lvExternalModules.Items[i].SubItems[C_ViewIndex_LastFileDate] := '-';

        lvExternalModules.Items[i].SubItems[C_ViewIndex_LastTestDate] :=
          DateTimeToStr(Now);

        if (bOk) 
        then lvExternalModules.Items[i].SubItems[C_ViewIndex_Commentary] := 'OK'
        else lvExternalModules.Items[i].SubItems[C_ViewIndex_Commentary] :=
          'Fehler';
      end;

      Result := (Result and bOk);
    end;

  except
    on E:Exception do begin
      Result := False;
      MyExceptionHandling(E,
        'TFormTabVerifyModules.LoadExternalModuleList');
    end;
  end;
end;

{--------------------------------------------}
procedure TFormTabVerifyModules.WriteJournal(sText: string);
{--------------------------------------------}
begin
  inherited;

  with lvJournal.Items.Add do begin
    Caption := DateTimeToStr(Now);
    SubItems.Add(sText);
  end;
end;

{--------------------------------------------}
procedure TFormTabVerifyModules.lvExternalModulesResize(Sender: TObject);
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
        GetSystemMetrics(SM_CXVSCROLL)) div (lvJournal.Columns.Count + 3);
      for i := 0 to lvJournal.Columns.Count-1 do
        if (i = C_JournalIndex_Description)
        then lvJournal.Columns[i].Width := 4*iWidth
        else lvJournal.Columns[i].Width := iWidth;
    end;
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormTabVerifyTakeOver.lvExternalModulesResize');
  end;
end;

{--------------------------------------------}
function TFormTabVerifyModules.GetVerifyFileName(
  const sExeName: string): string;
{--------------------------------------------}
var
  s : string;
begin
  // Dateinamen bereinigen
  s := StringReplace(ChangeFileExt(sExeName, ''), '_', '', [rfReplaceAll]);
  // Zunächst prüfen: Fehlerfall
  Result := FilePath + Format(C_VerifyMask_FatalError, [s]);
  // Dann prüfen: OK-Fall
  if (not FileExists(Result)) then
    Result := FilePath + Format(C_VerifyMask_OK, [s]);
  // Keines von beidem: ''
  if (not FileExists(Result)) then Result := '';
end;

{--------------------------------------------}
function TFormTabVerifyModules.GetLastFileDate(const sExeName: string;
  var iFileDate: integer; var dtDateTime: TDateTime): boolean;
{--------------------------------------------}
var
  sText   : string;
  iIx     : integer;
begin
  try
    iIx := ModuleList.IndexOf(sExeName);
    if (iIx >= 0) then begin
      sText := DescList[iIx];
      iFileDate := StrToIntDef(
        GetStringPart(sText, C_StringIndex_LastFileDate, '|'), 0);
      if (iFileDate > 0) then begin
        dtDateTime := StrToDateTime(
          GetStringPart(sText, C_StringIndex_LastChangeTime, '|'));
        // Abgleich mit letzter Sicherung
        if (VerifyWicoIni.LastSaveData > dtDateTime) then
          dtDateTime := VerifyWicoIni.LastSaveData;

        Result := True;
      end
      else Result := False;
    end
    else Result := False;
  except
    Result := False;
  end
end;

{--------------------------------------------}
procedure TFormTabVerifyModules.SetLastFileDate(
  const sExeName: string; iFileDate: integer);
{--------------------------------------------}
var
  s, sText : string;
  iIx     : integer;
begin
  iIx := ModuleList.IndexOf(sExeName);
  if (iIx >= 0) then begin
    s := DescList[iIx];
    sText := GetStringPart(s, C_StringIndex_Description, '|') + '|' +
      GetStringPart(s, C_StringIndex_FileName, '|') + '|' +
      IntToStr(iFileDate) + '|' +  DateTimeToStr(Now) + '|' +
      GetStringPart(s, C_StringIndex_Commentary, '|');
    DescList[iIx] := sText;
  end;
end;

{--------------------------------------------}
function TFormTabVerifyModules.VerifyModuleFlag(
  const sExeName, sFlagFile: string): boolean;
{--------------------------------------------}
var
  dtLast    : TDateTime;
  iFileDate : integer;
begin
  if (sFlagFile <> '') then begin
    // Prüfen, ob fataler Fehler vorliegt
    if (Pos('_3.VEM', UpperCase(sFlagFile)) > 0) then begin
      WriteJournal(sExeName + ' meldet fatalen Fehler');
      Result := False;
    end
    // Prüfen, ob Zeitdifferenz überschritten wurde;
    else begin
      if (VerifyWicoIni.IsSavingData) then begin  // Sicherungs-Zeit ignorieren
        SetLastFileDate(sExeName, FileAge(sFlagFile));
        Result := True;
      end
      else begin
        if (GetLastFileDate(sExeName, iFileDate, dtLast)) then begin
          if (iFileDate = FileAge(sFlagFile))
          then Result := (Now < (dtLast + EncodeTime(0, 5, 0, 0)))
          else begin
            SetLastFileDate(sExeName, FileAge(sFlagFile));
            Result := True;
          end;
        end
        else begin
          SetLastFileDate(sExeName, FileAge(sFlagFile));
          Result := True;
        end;
      end;

      if (not Result) then
        WriteJournal(sExeName + ' Fehler wg. Zeitüberschreitung');
    end;
  end
  else Result := True;  // Gar keine Datei heißt: alles i.O.
end;

{--------------------------------------------}
procedure TFormTabVerifyModules.sbtnSetGlobalModuleStateClick(Sender: TObject);
{--------------------------------------------}
begin
  sbtnSetGlobalModuleState.Enabled := False;
  try
    if (sbtnSetGlobalModuleState.Tag = 0) then begin
      if (Assigned(SetGlobalModuleState)) then begin
        WriteJournal('Anwender startet WICO22-Module manuell');
        sbtnSetGlobalModuleState.Tag := 1;
        sbtnSetGlobalModuleState.Caption := 'WICO22-Module beenden';
        SetGlobalModuleState(True);
      end;
    end
    else begin
      if (Assigned(SetGlobalModuleState)) then begin
        if (MessageDlg('Sollen alle aktiven Module beendet werden?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes) and
         (MessageDlg('Das System stellt damit seine Funktion ein!'#13#10#13#10 +
           'Soll das System aktiv bleiben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrNo)
        then begin
          WriteJournal('Anwender beendet WICO22-Module manuell');
          sbtnSetGlobalModuleState.Tag := 0;
          sbtnSetGlobalModuleState.Caption := 'WICO22-Module starten';
          SetGlobalModuleState(False);
        end;
      end;
    end;
  finally
    sbtnSetGlobalModuleState.Enabled := True;
  end
end;

end.
