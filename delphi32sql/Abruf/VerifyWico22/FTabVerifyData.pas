{------------------------------------------------------------------------------}
{ Tabulator-Fenster für Überwachung für WICO22-Datenbankfunktionalität         }
{                                                                              }
{ 21.09.2007  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2007                                      }
{------------------------------------------------------------------------------}
unit FTabVerifyData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons,
  FDefaultTab, InstTest, ShellApi, VerifyWico22Ini, T_Zeit, Service_Utils,
  FAuswahlDialog, COM_Utils, GD_Utils;

type
  TFormTabVerifyData = class(TFormVerifyWico22Tab)
    pnSystem: TPanel;
    gbSystem: TGroupBox;
    Label1: TLabel;
    eOwnState: TEdit;
    sbtnCheckBde: TSpeedButton;
    sbtnStartMerger: TSpeedButton;
    sbtnStartTransfer: TSpeedButton;
    pnJournal: TPanel;
    gbJournal: TGroupBox;
    lvJournal: TListView;
    Splitter1: TSplitter;
    procedure sbtnCheckBdeClick(Sender: TObject);
    procedure sbtnStartMergerClick(Sender: TObject);
    procedure sbtnStartTransferClick(Sender: TObject);
    procedure lvExternalModulesResize(Sender: TObject); override;
  private
    { Private-Deklarationen }
    FTransferRunning  : string;
    FMergerRunning    : string;
    FDbCheckTimeList  : TStrings;
    FLastMerging      : integer;
    FBdeCheckInterval : word;
    FLastBdeCheck     : TDateTime;
    FForceChange      : boolean;
    FFatalBdeError    : boolean;
    function CheckBde: boolean;
    function MergeAbrufData(pInfo : TComInfoWindow): boolean;
    procedure CheckRunningMerger;
    procedure CheckRunningTransfer;
    function RunThisTransfer(const sDefFile: string; pInfo: TComInfoWindow;
      bEnableStop: boolean): boolean;
    procedure InitCheckTimeList(bState: boolean);
    function RunCheckTimeList: integer;
    function RunAutoMerger: boolean;
  protected
    { Protected-Deklarationen }
    procedure InitComponents(bState: boolean); override;
    procedure LoadExternalModuleList; override;
    procedure WriteJournal(
      const sText, sRemark: string; dtStart, dtStop: TDateTime); overload;
    procedure UpdateLastJournalEntry(
      const sText, sRemark: string; dtStart, dtStop: TDateTime);
  public
    { Public-Deklarationen }
    function Verify: boolean; override;
    function DataTransferForTakeOver: boolean;
    procedure Actualize; override;
    property FatalBdeError: boolean read FFatalBdeError;
  end;

implementation

{$R *.dfm}

const
  C_ListIndex_BDE       = 0;
  C_ListIndex_Merger    = 1;
  C_ListIndex_Transfer  = 2;

{------------------------------- TFormTabVerifyData ---------------------------}

{--------------------------------------------}
procedure TFormTabVerifyData.InitComponents(bState: boolean);
{--------------------------------------------}
begin
  inherited;

  if (bState) then begin
    InitCheckTimeList(True);
    FForceChange := False;  // Kein aktiver Statuswechsel
    FBdeCheckInterval := VerifyWicoIni.GetBdeCheckInterval div (1000*60); //min
    FLastBdeCheck := Now;
    FFatalBdeError := False;  // Kein fataler BDE-Fehler
  end
  else begin
    InitCheckTimeList(False);
  end;
end;

{ Liste für Datenbankupdates erzeugen/freig. }
{--------------------------------------------}
procedure TFormTabVerifyData.InitCheckTimeList(bState: boolean);
{--------------------------------------------}
var
  i : integer;
begin
  if (bState) then begin
    FDbCheckTimeList := TStringList.Create;
    // Alle Datenbank-Transfer-Aktionen listen
    FDbCheckTimeList.CommaText := VerifyWicoIni.GetDbTransferDefFiles;
    for i := 0 to FDbCheckTimeList.Count-1 do
      // Zeitpunkt des letzten Abrufs setzen
      if (VerifyWicoIni.IsDataSaveToday)   // Flag: heute/sofort starten
      then FDbCheckTimeList.Objects[i] := TObject(0)
      else FDbCheckTimeList.Objects[i] := TObject(DateTimeToFileDate(Now));
    // Abrufdatenbank-Zusammenführung initialisieren
    if (VerifyWicoIni.IsDataSaveToday)   // Flag: heute/sofort starten
      then FLastMerging := 0
      else FLastMerging := DateTimeToFileDate(Now);
  end
  else begin
    FreeAndNil(FDbCheckTimeList);
  end;
end;

{ Automatischen MERGER-Lauf prüfen           }
{--------------------------------------------}
function TFormTabVerifyData.RunAutoMerger: boolean;
{--------------------------------------------}
var
  b, bStop  : boolean;
  iInterval : integer;
  dtStart   : TDateTime;
  sTrigger  : string;
begin
  if (not FForceChange) and (FMergerRunning = '') then begin
    VerifyWicoIni.GetDbMergerDef(bStop, dtStart, iInterval, sTrigger);
    if (dtStart >= 0) then begin  // Start einmal pro Tag
      b := ((Trunc(Now) + dtStart) < Now) and  // Startzeitpunkt vorbei?
        ((FLastMerging = 0) or (Trunc(FileDateToDateTime(FLastMerging)) < Trunc(Now))); // heute noch nicht ...
    end
    else if (iInterval > 0) then begin
      b := (FLastMerging = 0) or ((FileDateToDateTime(FLastMerging) +
        iInterval/(1000*60*60*24) < Now));
    end
    else if (sTrigger <> '') then begin
      b := (FileExists(sTrigger)) and
        ((FLastMerging = 0) or (FileDateToDateTime(FileAge(sTrigger)) >
         FileDateToDateTime(FLastMerging)));
    end
    else b := False;

    if (b) then begin
       WriteJournal('Automatischer Start der Datenzusammenführung');
       Result := MergeAbrufData(nil);
       FLastMerging := DateTimeToFileDate(Now);
    end
    else Result := True;
  end
  else Result := True;
end;

{ Liste für Datenbankupdates prüfen          }
{--------------------------------------------}
function TFormTabVerifyData.RunCheckTimeList: integer;
{--------------------------------------------}

  function GetStartThisDbDef(const sDef: string; iLast: integer): boolean;
  var
    bStop     : boolean;
    iInterval : integer;
    dtStart   : TDateTime;
    sTrigger  : string;
  begin
    Result := False;  // kein Start
    VerifyWicoIni.GetDbTransferDef(sDef, bStop, dtStart, iInterval, sTrigger);
    if (dtStart >= 0) then begin  // Start einmal pro Tag
      Result := ((Trunc(Now) + dtStart) < Now) and  // Startzeitpunkt vorbei?
        ((iLast = 0) or (Trunc(FileDateToDateTime(iLast)) < Trunc(Now))); // heute noch nicht ...
    end
    else if (iInterval > 0) then begin
      Result := (iLast = 0) or ((FileDateToDateTime(iLast) +
        iInterval/(1000*60*60*24) < Now));
    end
    else if (sTrigger <> '') then begin
      Result := (FileExists(sTrigger)) and
        ((iLast = 0) or
         (FileDateToDateTime(FileAge(sTrigger)) > FileDateToDateTime(iLast)));
    end;
  end;

var
  i : integer;
begin
  try
    Result := 0;
    if (not FForceChange) and (FTransferRunning = '') then begin
      for i := 0 to FDbCheckTimeList.Count-1 do begin
        if (GetStartThisDbDef(FDbCheckTimeList[i],
          Integer(FDbCheckTimeList.Objects[i]))) then
        begin
          WriteJournal('Automatischer Start Datentransfer (' +
            FDbCheckTimeList[i] + ')');
          if (RunThisTransfer(FDbCheckTimeList[i], nil, True)) then begin
            Inc(Result);
            FDbCheckTimeList.Objects[i] := TObject(DateTimeToFileDate(Now));
          end;
          Break;  // Es wird bei jedem Durchlauf max. 1 Transfer gestartet
        end;
      end;
    end;
  except
    on E:Exception do begin
      Result := -1;
    end;
  end;
end;

{--------------------------------------------}
procedure TFormTabVerifyData.lvExternalModulesResize(Sender: TObject);
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

{ Laden der Liste externer Programmmodulen   }
{--------------------------------------------}
procedure TFormTabVerifyData.LoadExternalModuleList;
{--------------------------------------------}
var
  i, j : integer;
begin
  inherited;

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
      'TFormTabVerifyData.LoadExternalModuleList');
  end;
end;


{ Fenster aktualisieren                      }
{--------------------------------------------}
procedure TFormTabVerifyData.Actualize;
{--------------------------------------------}
begin
  inherited;

  try
  except
    on E:Exception do MyExceptionHandling(E,
      'TFormTabVerifyData.Actualize');
  end;
end;

{ Journaleintrag in Listview                 }
{--------------------------------------------}
procedure TFormTabVerifyData.WriteJournal(
  const sText, sRemark: string; dtStart, dtStop: TDateTime);
{--------------------------------------------}
begin
  with lvJournal.Items.Add do begin
    Caption := DateTimeToStr(Now);
    SubItems.Add(sText);
    if (dtStart > 1)
    then SubItems.Add(DateTimeToStr(dtStart))
    else SubItems.Add('-');
    if (dtStop > 1)
    then SubItems.Add(DateTimeToStr(dtStop))
    else SubItems.Add('-');
    SubItems.Add(sRemark);
  end;
end;

{ Journaleintrag in Listview updaten         }
{--------------------------------------------}
procedure TFormTabVerifyData.UpdateLastJournalEntry(
  const sText, sRemark: string; dtStart, dtStop: TDateTime);
{--------------------------------------------}
var
  i : integer;
begin
  for i := lvJournal.Items.Count-1 downto 0 do begin
    with lvJournal.Items[i] do begin
      if (SubItems.Count = 4) and (SubItems[0] = sText) then begin
        if (dtStart > 1) then SubItems[1] := DateTimeToStr(dtStart);
        if (dtStop > 1) then SubItems[2] := DateTimeToStr(dtStop);
        if (sRemark <> '') then SubItems[3] := sRemark;
        Break;
      end;
    end;
  end;
end;

{ Überprüfung durchführen                    }
{--------------------------------------------}
function TFormTabVerifyData.Verify: boolean;
{--------------------------------------------}
begin
  try
    // Statusanzeige
    if (VerifyWicoIni.MySystemState = mssActiv) then
      eOwnState.Text := S_Long_Activ
    else if (VerifyWicoIni.MySystemState = mssInactiv) then
      eOwnState.Text := S_Long_Inactiv
    else eOwnState.Text := '-';

    // BDE-Prüfung
    Result := CheckBde;    // BDE wird immer geprüft
    if (not FForceChange) and (not FFatalBdeError) then begin
      CheckRunningTransfer;  // Laufender Transfer ?
      CheckRunningMerger;    // Laufendes Merging ?

      if (VerifyWicoIni.MySystemState = mssActiv) and
        (DirectoryExists(VerifyWicoIni.OtherSystemFilePath)) and (Result) then
      begin
        Result := (Result) and (RunCheckTimeList >= 0);  // Transfer
        if (Result) and (FMergerRunning = '') then Result := RunAutoMerger;
      end;

      // Datenabgleichfunktion enablen
      sbtnStartMerger.Enabled := (VerifyWicoIni.MySystemState = mssActiv) and
        (FMergerRunning = '') and (FTransferRunning = '') and
        (DirectoryExists(VerifyWicoIni.OtherSystemFilePath));
      sbtnStartTransfer.Enabled := (VerifyWicoIni.MySystemState = mssActiv) and
        (FTransferRunning = '') and (FMergerRunning = '') and
        (DirectoryExists(VerifyWicoIni.OtherSystemFilePath));

      // Globaler Status: wird aktuell gesichert ?
      VerifyWicoIni.IsSavingData :=
        ((FMergerRunning <> '') or (FTransferRunning <> ''));
    end;
  except
    on E:Exception do begin
      MyExceptionHandling(E, 'TFormTabVerifyData.Verify');
      Result := False;
    end;
  end;
end;

{ Überprüfung der BDE durch Testmodul        }
{--------------------------------------------}
function TFormTabVerifyData.CheckBde: boolean;
{--------------------------------------------}
var
  sCmdLine : string;
  bInst0   : boolean;
begin
  // Vorheriges Testprogramm beendet ?
  bInst0 := (AnzahlInstanzen('TFormWOsRestartBDE', '') = 0);
  // Ergebnis schreiben
  if (lvExternalModules.Items.Count = ModuleList.Count) then begin
    if (bInst0)
    then lvExternalModules.Items[C_ListIndex_BDE].SubItems[2] :=
      'Letzte Prüfung erfolgreich'
    else lvExternalModules.Items[C_ListIndex_BDE].SubItems[2] :=
      'Prüfung läuft noch';
  end;


  if (FBdeCheckInterval > 0) and (FLastBdeCheck + EncodeTime(
    FBdeCheckInterval div 60, FBdeCheckInterval mod 60, 0, 0) <= Now) then
  begin
    Result := bInst0;

    // Ergebnis schreiben
    if (lvExternalModules.Items.Count = ModuleList.Count) then
      // Datum/Zeit schreiben
      lvExternalModules.Items[C_ListIndex_BDE].SubItems[1] :=
        DateTimeToStr(Now);

    // Wenn ja: neuen Lauf starten
    if (Result) then begin
      sCmdLine := ExtractFilePath(ParamStr(0)) + 'WOSRestartBDE.EXE' + ' ' +
        IntToStr(Self.Handle) + ' ' + IntToStr(WM_USER + 111);
      WinExec(PChar(sCmdLine), SW_SHOW);

      if (lvExternalModules.Items.Count = ModuleList.Count) then
        lvExternalModules.Items[C_ListIndex_BDE].SubItems[2] :=
        'Prüfung läuft noch';
      WriteJournal('BDE-Überprüfung gestartet');
      FLastBdeCheck := Now;
    end
    else begin
      FBdeCheckInterval := 0;  // weitere Prüfungen verhindern
      FFatalBdeError := True;
      WriteJournal(
        'BDE-Prüfung fehlgeschlagen - Umschaltung und Booten erforderlich');
    end;
  end
  else Result := True;
end;

{ Abgleich der Abrufdaten durch Merger       }
{--------------------------------------------}
function TFormTabVerifyData.MergeAbrufData(pInfo : TComInfoWindow): boolean;
{--------------------------------------------}
begin
  if (FFatalBdeError) then begin
    Result := False;
    Exit;
  end;

  WriteJournal('Starte Zusammenführung der Abrufdaten');
  SetGlobalModuleState(False);
  VerifyWicoIni.IsSavingData := True;
  try
    if (FileExists(ExtractFilePath(ParamStr(0)) + 'MERGER.EXE')) and
      (not GetDbMergerRunning) then
    begin
      if (ShellExecute(Application.Handle, nil,
        'MERGER.EXE', 'AUTOMATIK AUTO',
        PChar(ExtractFilePath(ParamStr(0))), SW_ShowNormal) > 32) then
      begin
        FMergerRunning := 'Datenbankzusammenführung ' + DateTimeToStr(Now);
        WriteJournal(FMergerRunning, '', Now, -1);

        if (Assigned(pInfo)) then begin
          pInfo.SetInfoText(FMergerRunning);
          while (FMergerRunning <> '') do begin
            if (pInfo.Stoped) then Break;
            CreateSystemStateFile(VerifyWicoIni.MySystemState);
            Delay(1000);
            CheckRunningMerger;
          end;

          Result := (FMergerRunning = '');
        end
        else Result := True;
      end
      else Result := False;
    end
    else Result := False;
    if (Assigned(pInfo)) then begin
      if (Result)
      then WriteJournal('Zusammenführung der Abrufdaten abgeschlossen')
      else WriteJournal('Fehler beim Start der Zusammenführung der Abrufdaten');
    end;
  finally
    if (not FForceChange) and (Assigned(pInfo)) then begin
      SetGlobalModuleState(True);
      FMergerRunning := '';
      VerifyWicoIni.IsSavingData := False;
    end;
  end;

  // Ergebnis schreiben
  if (lvExternalModules.Items.Count = ModuleList.Count) then
    lvExternalModules.Items[C_ListIndex_Merger].SubItems[1] :=
      DateTimeToStr(Now);
end;

{--------------------------------------------}
procedure TFormTabVerifyData.CheckRunningMerger;
{--------------------------------------------}
var
  s : string;
begin
  if (FMergerRunning <> '') then begin  // Running-Flag prüfen
    if (not GetDbMergerRunning) then begin  // Läuft Programm nicht mehr ?
      if (GetLastDbMergerResult) then s := 'Durchgeführt' else s := 'Fehler!';
      UpdateLastJournalEntry(FMergerRunning, s, -1, Now);
      WriteJournal('Datenzusammenführung beendet, Ergebnis: ' + s);
      FMergerRunning := '';  // Flag zurücksetzen
      // Ergebnis schreiben
      if (lvExternalModules.Items.Count = ModuleList.Count) then
        lvExternalModules.Items[C_ListIndex_Merger].SubItems[1] :=
          DateTimeToStr(Now);
      if (not FForceChange) and (FTransferRunning = '') then
        SetGlobalModuleState(True);
    end;
  end;
end;

{--------------------------------------------}
procedure TFormTabVerifyData.CheckRunningTransfer;
{--------------------------------------------}
var
  s : string;
begin
  if (FTransferRunning <> '') then begin  // Running-Flag prüfen
    if (not GetDbTranferRunning) then begin  // Läuft Programm nicht mehr ?
      if (GetLastDbTransferResult) then s := 'Durchgeführt' else s := 'Fehler!';
      UpdateLastJournalEntry(FTransferRunning, s, -1, Now);
      WriteJournal(
        'Transfer beendet (' + FTransferRunning + '), Ergebnis: ' + s);
      FTransferRunning := '';  // Flag zurücksetzen
      // Ergebnis schreiben
      if (lvExternalModules.Items.Count = ModuleList.Count) then
        lvExternalModules.Items[C_ListIndex_Transfer].SubItems[1] :=
          DateTimeToStr(Now);
      // Weiteren Transfer sofort starten (um mehrfachen Modulstart zu vermeiden)
      if (not FForceChange) and (RunCheckTimeList <= 0) and
      (FMergerRunning = '')
      then SetGlobalModuleState(True);
    end;
  end;
end;

{--------------------------------------------}
procedure TFormTabVerifyData.sbtnCheckBdeClick(Sender: TObject);
{--------------------------------------------}
var
  iCheckInterval : integer;
  pInfo          : TComInfoWindow;
  iStop          : Cardinal;
  bSuccess       : boolean;
begin
  if (AnzahlInstanzen('TFormWOsRestartBDE', '') = 0) then begin
    sbtnCheckBde.Enabled := False;
    iCheckInterval := FBdeCheckInterval;
    pInfo := TComInfoWindow.CreateInfoWindow('Manuelle BDE-Prüfung');
    try
      pInfo.EnableStop := True;
      pInfo.Show;
      FBdeCheckInterval := 1;
      FLastBdeCheck := Now - EncodeTime(0, 1, 1, 0);
      WriteJournal('BDE-Prüfung durch den Anwender gestartet');
      iStop := GetTickCount + 1*60*1000;
      CheckBde;

      bSuccess := False;
      while (not pInfo.Stoped) and (GetTickCount < iStop) do begin
        pInfo.SetInfoText('Verbleibende Zeit: ' +
          IntToStr((iStop - GetTickCount) div 1000) + ' sec.');
        Delay(100);
        if (AnzahlInstanzen('TFormWOsRestartBDE', '') = 0) then begin
          bSuccess := True;
          Break;
        end;
      end;

      if (bSuccess)
      then MessageDlg('BDE-Test erfolgreich', mtInformation, [mbOk], 0)
      else MessageDlg('BDE-Test nicht erfolgreich', mtWarning, [mbOk], 0);
    finally
      FBdeCheckInterval := iCheckInterval;
      sbtnCheckBde.Enabled := True;
      pInfo.Free;
    end;
  end
  else MessageDlg(
    'Aktuell ist eine BDE-Prüfung aktiv', mtInformation, [mbOk], 0);
end;

{--------------------------------------------}
procedure TFormTabVerifyData.sbtnStartMergerClick(Sender: TObject);
{--------------------------------------------}
var
  pInfo : TComInfoWindow;
begin
  sbtnStartMerger.Enabled := False;
  pInfo := TComInfoWindow.CreateInfoWindow('Abrufdaten zusammenführen');
  try
    pInfo.EnableStop := True;
    pInfo.Show;
    if (not GetDbMergerRunning)
    then MergeAbrufData(pInfo)
    else MessageDlg(
      'Aktuell ist eine Daten-Zusammenführung aktiv', mtInformation, [mbOk], 0);
  finally
    pInfo.Free;
    sbtnStartMerger.Enabled := True;
  end;
end;

{ Datenbankkopie durch Transfer              }
{--------------------------------------------}
function TFormTabVerifyData.RunThisTransfer(
  const sDefFile: string; pInfo: TComInfoWindow; bEnableStop: boolean): boolean;
{--------------------------------------------}
var
  bStop     : boolean;
  iInterval : integer;
  dtStart   : TDateTime;
  sTrigger  : string;
begin
  if (FFatalBdeError) then begin
    Result := False;
    Exit;
  end;

  if (not GetDbTranferRunning) then begin
    VerifyWicoIni.GetDbTransferDef(
      sDefFile, bStop, dtStart, iInterval, sTrigger);
    if (bStop) and (bEnableStop) then SetGlobalModuleState(False);
    try
      if (not bEnableStop) or (not bStop) or (ListRunningModules = '') then
      begin
        if (Assigned(pInfo)) then pInfo.SetInfoText('Transfer via ' + sDefFile);
        StartDbTranfer(sDefFile);
        FTransferRunning := 'Datenbanktransfer via ' + sDefFile;
        WriteJournal(FTransferRunning, '', Now, -1);

        if (Assigned(pInfo)) then begin
          while (FTransferRunning <> '') do begin
            if (Assigned(pInfo)) and (pInfo.Stoped) then Break;
            CreateSystemStateFile(VerifyWicoIni.MySystemState);
            Delay(1000);
            CheckRunningTransfer;
          end;
          Result := (FTransferRunning = '');
        end
        else Result := True;
      end
      else Result := False;
    finally
      if (not FForceChange) and (Assigned(pInfo)) and (bEnableStop) then
        SetGlobalModuleState(True);
    end;
  end
  else Result := False;
end;

{--------------------------------------------}
procedure TFormTabVerifyData.sbtnStartTransferClick(Sender: TObject);
{--------------------------------------------}
var
  pSl   : TStrings;
  i     : integer;
  s     : string;
  pInfo : TComInfoWindow;
begin
  sbtnStartTransfer.Enabled := False;
  try
    if (not GetDbTranferRunning) then begin
      pSl := TStringList.Create;
      try
        pSl.CommaText := VerifyWicoIni.GetDbTransferDefFiles;
        if (pSl.Count > 0) then begin
          pSl.Insert(0, '<Alle starten>');
          with TFormAuswahlDlg.Create(Self) do
          try
            AuswahlList := pSl;
            if (ShowModal = mrOk) then begin
              s := Auswahl;
              pInfo := TComInfoWindow.CreateInfoWindow('Datenbanktransfer');
              try
                pInfo.EnableStop := True;
                Show;

                WriteJournal('Datenbanktransfer durch Anwender ausgelöst');
                if (s = pSl[0]) then begin   // Durchlauf durch alle Defs
                  SetGlobalModuleState(False);
                  VerifyWicoIni.IsSavingData := True;
                  try
                    for i := 1 to pSl.Count-1 do
                      if (not RunThisTransfer(pSl[i], pInfo, False)) then Break;
                  finally
                    SetGlobalModuleState(True);
                    VerifyWicoIni.IsSavingData := False;
                  end;
                end
                else RunThisTransfer(s, pInfo, True);
              finally
                pInfo.Free;
              end;
            end;
          finally
            Free;
          end;
        end;
      finally
        pSl.Free;
      end;
    end
    else MessageDlg(
      'Aktuell ist ein Daten-Transfer aktiv', mtInformation, [mbOk], 0);
  finally
    sbtnStartTransfer.Enabled := True;
  end;
end;

{ Datenbankübertragung vor Funktionsübergabe }
{--------------------------------------------}
function TFormTabVerifyData.DataTransferForTakeOver: boolean;
{--------------------------------------------}
var
  pInfo : TComInfoWindow;
  i     : integer;
  iStop : Cardinal;
begin
  try
    FForceChange := True;  // Alle anderen Aktionen blockieren
    // Infofenster initialisieren
    pInfo := TComInfoWindow.CreateInfoWindow('Datenübergabe wg. Umschaltung');
    try
      Result := False;   // Default
      pInfo.EnableStop := True;
      if (FFatalBdeError) then Exit;

      // Prüfe: läuft aktuell ein Transfer ?
      iStop := GetTickCount + 10*60*1000;
      while (iStop > GetTickCount) and (FTransferRunning <> '') do begin
        Delay(100);
        CheckRunningTransfer;
        pInfo.SetInfoCaption('Warte auf Transferende (' +
          IntToStr((iStop + GetTickCount) div 1000) + ')');
        CreateSystemStateFile(VerifyWicoIni.MySystemState);
      end;
      if (FTransferRunning = '') then begin
        // Transferdefinitionen abarbeiten
        with TStringList.Create do
        try
          // Transferdateien aus INI lesen
          CommaText := VerifyWicoIni.GetDbTransferTakeOverDefFiles;
          for i := 0 to Count-1 do
            if (not RunThisTransfer(Strings[i], pInfo, False)) then Break;
        finally
          Free;
        end;
      end;

      // Merging durchführen
      iStop := GetTickCount + 10*60*1000;
      while (iStop > GetTickCount) and (FMergerRunning <> '') do begin
        Delay(100);
        CheckRunningMerger;
        pInfo.SetInfoCaption('Warte auf Mergerende (' +
          IntToStr((iStop + GetTickCount) div 1000) + ')');
        CreateSystemStateFile(VerifyWicoIni.MySystemState);
      end;
      if (FMergerRunning = '') then MergeAbrufData(pInfo);

      Result := True;
    finally
      pInfo.Free;
      FForceChange := False;
    end;
  except
    on E:Exception do begin
      Result := False;
      MyExceptionHandling(E, 'Fehler bei Datenübergabe vor Umschaltung');
    end;
  end;
end;

end.
