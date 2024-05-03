{------------------------------------------------------------------------------}
{ Einstellungen für Überwachung für WICO22                                     }
{                                                                              }
{ 21.09.2007  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2007                                      }
{------------------------------------------------------------------------------}
unit VerifyWico22Ini;

interface

uses
  Windows, SysUtils, IniFiles, Classes, Forms,
  PathIni, GD_Utils, InstTest, Service_Utils, KWLink32, T_Zeit, ShellApi;

const
  // System-Datei-Flags
  C_SysFlagFile_Mask         = 'SFF_*_*.SFF';   // allgemeine Dateimaske
  C_SysFlagFile_SystemMask   = 'SFF_%s_*.SFF';  // Maske für Name
  C_SysFlagFile_StateMask    = 'SFF_%s_%s.SFF'; // Maske für Name und Status
  C_SysFlagFile_Initializing = 'SFF_%s_1.SFF';  // %s - Bezeichnung
  C_SysFlagFile_Activ        = 'SFF_%s_2.SFF';  // %s - Bezeichnung
  C_SysFlagFile_Inactiv      = 'SFF_%s_3.SFF';  // %s - Bezeichnung
  C_SysFlagFile_Activating   = 'SFF_%s_4.SFF';  // %s - Bezeichnung
  C_SysFlagFile_Deactivating = 'SFF_%s_5.SFF';  // %s - Bezeichnung

resourcestring
  S_Long_EXE                 = 'Win32Exe';
  S_Long_Service             = 'NT Service';
  S_Long_Activ               = 'Aktiv';
  S_Long_Inactiv             = 'Inaktiv';
  S_Long_Error               = 'Fehler';

type
  // Mögliche Stati
  TMySystemState = (mssNone, mssInitializing, mssActiv, mssInactiv,
    mssActivating, mssDeactivating);

  TVerifyWico22Ini = class(TIniFile)
     constructor Create(const sFileName: TFileName);
  private
    FModuleFilePath      : TFileName;  // "Lebenszeichen"-Pfad der Module
    FMySystemFilePath    : TFileName;  // Schreiben meiner SysInfo
    FOtherSystemFilePath : TFileName;  // Lesen der weiteren SysInfo
    FMySystemName        : string;     // Identifiziert das eigene System
    FMySystemState       : TMySystemState;
    FStartInterval       : integer;
    FIsLog               : boolean;
    FIsDataSaveToday     : boolean;
    FIsSavingData        : boolean;
    FLastSaveData        : TDateTime;
    function GetMySystemName: string;
    function GetHasDataVerification: boolean;
    procedure SetHasDataVerification(bState: boolean);
    function GetHasRuntimeVerification: boolean;
    procedure SetHasRuntimeVerification(bState: boolean);
    function GetHasTakeOverVerification: boolean;
    procedure SetHasTakeOverVerification(bState: boolean);
    procedure SetIsSaveDataState(bState: boolean);
    function GetLastSaveData: TDateTime;
  protected
  public
    function GetFormModuleList(pForm: TForm): string;
    function GetLocalModuleList: string;
    function GetClassName(const sExeName: string): string;
    function GetServiceName(const sExeName: string): string;
    function GetFilePath(const sExeName: string): string;
    function GetPrimaryParams(const sExeName: string): string;
    function GetSecondaryParams(const sExeName: string): string;
    function GetIsWin32Exe(const sExeName: string): boolean;
    function GetStartPrimary(const sExeName: string): boolean;
    function GetStartSecondary(const sExeName: string): boolean;
    function GetDbTransferDefFiles: string;
    function GetDbTransferTakeOverDefFiles: string;
    function GetDbTransferDef(const sDefFile: string; var bStopModules: boolean;
      var dtStartTime: TDateTime; var iStartIntervalMs: integer;
      var sTriggerFile: string): boolean;
    function GetDbMergerDef(var bStopModules: boolean;
      var dtStartTime: TDateTime; var iStartIntervalMs: integer;
      var sTriggerFile: string): boolean;
    function GetDbMergerTakeOver: boolean;
    function GetBdeCheckInterval: Cardinal;

    property DataVerification: boolean
      read GetHasDataVerification write SetHasDataVerification;
    property RuntimeVerification: boolean
      read GetHasRuntimeVerification write SetHasRuntimeVerification;
    property TakeOverVerification: boolean
      read GetHasTakeOverVerification write SetHasTakeOverVerification;
    property ModuleFilePath: TFileName read FModuleFilePath;
    property MySystemFilePath: TFileName read FMySystemFilePath;
    property OtherSystemFilePath: TFileName read FOtherSystemFilePath;
    property MySystemName: string read GetMySystemName;
    property MySystemState: TMySystemState
      read FMySystemState write FMySystemState;
    property IsLog: boolean read FIsLog;
    property IsDataSaveToday: boolean read FIsDataSaveToday;
    property StartInterval: integer read FStartInterval;
    property IsSavingData: boolean read FIsSavingData write SetIsSaveDataState;
    property LastSaveData: TDateTime read GetLastSaveData;
  end;

  function ListRunningModules: string;
  function StartModule(const sFileName: string;
    iTimeOut: Cardinal; bAlways: boolean = False): boolean;
  function StopModule(sFileName: string; iTimeOut: Cardinal): boolean;
  function StartDbTranfer(const sDefFile: string): boolean;
  function GetDbMergerRunning: boolean;
  function GetLastDbMergerResult: boolean;
  function GetDbTranferRunning: boolean;
  function GetLastDbTransferResult: boolean;
  procedure CreateSystemStateFile(pSysState: TMySystemState);

  function MyFindFiles(const sMask: string; pList: TStrings): integer;
  function MyDeleteFiles(const sMask: string): integer;
  function ConvertPipesToCommaText(const sPipedString: string): string;
  procedure WriteJournal(const sText: string);

const
  MySystemStateText: array [TMySystemState] of string = ('-', 'Initialisieren',
    'Aktiv', 'Inaktiv', 'Aktiviere ...', 'Deaktiviere ...');

var
  VerifyWicoIni   : TVerifyWico22Ini;
  VerifyJournalCallback : TSetCaptionEvent;

implementation

const
  C_Section_Settings = 'SETTINGS';

{---------------------------- Allgemeine Funktionen ---------------------------}

{ Implizites FindFirst mit Listenrückgabe    }
{ Parameter: Dateimaske; Ergebnislste        }
{ Rückgabe: Anzahl der gefundenen Einträge   }
{--------------------------------------------}
function MyFindFiles(const sMask: string; pList: TStrings): integer;
{--------------------------------------------}
var
  pSR   : TSearchRec;
  sFile : string;
begin
  try
    Result := 0;
    if (FindFirst(sMask, 0, pSR) = 0) then
    try
      sFile := ExtractFilePath(sMask) + pSR.Name;
      pList.Add(sFile);
      Inc(Result);
      while (FindNext(pSR) = 0) do begin
        sFile := ExtractFilePath(sMask) + pSR.Name;
        pList.Add(sFile);
        Inc(Result);
      end;
    finally
      FindClose(pSR);
    end;
  except
    Result := -1;
  end;
end;

{ Löschen mit Maske                          }
{ Parameter: Dateimaske                      }
{ Rückgabe: Anzahl der gelöschten Einträge   }
{--------------------------------------------}
function MyDeleteFiles(const sMask: string): integer;
{--------------------------------------------}
var
  i   : integer;
  pSl : TStrings;
begin
  try
    Result := 0;
    pSl := TStringList.Create;
    try
      if (MyFindFiles(sMask, pSl) > 0) then begin
        for i := 0 to pSl.Count-1 do begin
          if (DeleteFile(pSl[i])) then Inc(Result);
        end;
      end;
    finally
      pSl.Free;
    end;
  except
    Result := -1;
  end;
end;

{ Schreibt Journal über Callback-Pointer     }
{--------------------------------------------}
procedure WriteJournal(const sText: string);
{--------------------------------------------}
begin
  if (Assigned(VerifyJournalCallback)) then VerifyJournalCallback(sText);
end;

{ Pipes in CommaText konvertieren (für INI)  }
{--------------------------------------------}
function ConvertPipesToCommaText(const sPipedString: string): string;
{--------------------------------------------}
var
  s : string;
begin
  s := Trim(sPipedString);
  // Struktur mit pipes in CommaText konvertieren
  if (s <> '') then
    Result := '"' + StringReplace(s, '|', '","', [rfReplaceAll]) + '"';
end;

{--------------------------------------------}
function ListRunningModules: string;
{--------------------------------------------}
var
  i          : integer;
  sClassName : string;
begin
  Result := '';
  try
    with TStringList.Create do
    try
      CommaText := VerifyWicoIni.GetLocalModuleList;
      for i := 0 to Count-1 do begin
        sClassName := VerifyWicoIni.GetClassName(Strings[i]);
        if (AnzahlInstanzen(sClassName, '') > 0) then begin
          if (Result <> '') then Result := Result + ',';
          Result := Result + Strings[i];
        end;
      end;
    finally
      Free;
    end;
  except
    Result := '';
  end;
end;

{--------------------------------------------}
procedure CreateSystemStateFile(pSysState: TMySystemState);
{--------------------------------------------}
var
  sFileName : string;
  pSl       : TStrings;
  i         : integer;
begin
  // FlagFile erstellen
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
end;

{--------------------------------------------}
function StartModule(const sFileName: string;
  iTimeOut: Cardinal; bAlways: boolean = False): boolean;
{--------------------------------------------}

  function MyHandleWinExe(const sFileName: string): boolean;
  var
    b, bState, bStart, bActiv, bInactiv : boolean;
    sPrimParams, sSecParams, sParams    : string;
    sClassName, s                       : string;
    iStop                               : Cardinal;
  begin
    bState := (VerifyWicoIni.MySystemState in [mssActivating, mssActiv]);
    sPrimParams := VerifyWicoIni.GetPrimaryParams(sFileName);
    sSecParams := VerifyWicoIni.GetSecondaryParams(sFileName);
    bActiv := VerifyWicoIni.GetStartPrimary(sFileName);
    bInactiv := VerifyWicoIni.GetStartSecondary(sFileName);
    sClassName := VerifyWicoIni.GetClassName(sFileName);

    if (bAlways) or ((bActiv or bInactiv) and
      ((bActiv <> bInactiv) or (sPrimParams <> sSecParams))) then
    begin
      // Soll gestartet werden ?
      bStart := (bAlways) or ((bState and bActiv) or (not bState and bInactiv));
      // Auf jeden Fall beenden!
      iStop := GetTickCount + iTimeOut;  // 30 sec. zum Beenden muss reichen !
      b := (AnzahlInstanzen(sClassName, '') > 0);
      if (b) then WriteJournal('Beende ' + ExtractFileName(sFileName));
      while (GetTickCount <= iStop) and (AnzahlInstanzen(sClassName, '') > 0) do
      begin
        CloseThisWindow(sClassName);
        Delay(100);
      end;
      Result := (AnzahlInstanzen(sClassName, '') = 0);
      if (b) then begin
        if (Result)
        then WriteJournal(ExtractFileName(sFileName) + ' beendet')
        else WriteJournal(
          'Fehler!: ' + ExtractFileName(sFileName) + ' nicht beendet');
        Delay(1000);
      end;
      // Jetzt ggf. starten
      if ((Result) and (bStart)) then begin
        if (bState) then sParams := sPrimParams else sParams := sSecParams;
        Result := (ShellExecute(Application.Handle, nil,
          PChar(ExtractFileName(sFileName)), PChar(sParams),
          PChar(ExtractFilePath(sFileName)), SW_ShowNormal) > 32);
        s := sFileName;
        if (sParams <> '') then s := s + ' ' + sParams;
        if (Result)
        then WriteJournal(s + ' gestartet')
        else WriteJournal('Fehler!: ' + s + ' nicht gestartet');
      end;
    end
    else Result := True; // nix zu tun ...
  end;

  function MyHandleService(const sServiceName: string): boolean;
  var
    s : string;
  begin
    if (bAlways) or (VerifyWicoIni.MySystemState in [mssActivating, mssActiv])
    then begin
      s := ' gestartet';
      Result := WStartService(sServiceName)
    end
    else begin
      s := ' gestoppt';
      Result := WStopService(sServiceName, iTimeOut);
    end;
    if (Result)
    then WriteJournal(ExtractFileName(sServiceName) + s)
    else WriteJournal('Fehler!: ' + ExtractFileName(sServiceName) + s);
  end;

begin
  if (VerifyWicoIni.GetIsWin32Exe(sFileName)) then
    Result := MyHandleWinExe(VerifyWicoIni.GetFilePath(sFileName) +
      ChangeFileExt(ExtractFileName(sFileName), '.EXE'))
  else Result := MyHandleService(VerifyWicoIni.GetServiceName(sFileName));
end;

{--------------------------------------------}
function StopModule(sFileName: string; iTimeOut: Cardinal): boolean;
{--------------------------------------------}
var
  iStop : Cardinal;
  s     : string;
begin
  if (VerifyWicoIni.GetIsWin32Exe(sFileName)) then begin
  // Win32.EXE stoppen
    s := VerifyWicoIni.GetClassName(sFileName);
    CloseThisWindow(s);
    iStop := GetTickCount + iTimeOut;
    while (AnzahlInstanzen(s, '') > 0) and (iStop >= GetTickCount) do
      Delay(100);
    Result := (AnzahlInstanzen(s, '') = 0);
    s := 'Programm ' + ChangeFileExt(ExtractFileName(sFileName), '');
  end
  else begin
  // Service stoppen
    s := VerifyWicoIni.GetServiceName(sFileName);
    Result := WStopService(s, iTimeOut);
    s := 'Dienst ' + s;
  end;

  if (Result)
  then WriteJournal(s + ' gestoppt')
  else WriteJournal('Fehler! beim Stoppen von ' + s);
end;

{--------------------------------------------}
function StartDbTranfer(const sDefFile: string): boolean;
{--------------------------------------------}
var
  sFile, sPath : TFileName;
begin
  try
    sPath := VerifyWicoIni.GetFilePath('TRANSFER.EXE');
    if (ExtractFilePath(sDefFile) = '')
    then sFile := sPath + sDefFile
    else sFile := sDefFile;
    // Prüfe: Programm, Definitionsdatei, Kopie
    if (FileExists(sPath + 'TRANSFER.EXE')) and (FileExists(sFile)) and
      (CopyFile(PChar(sFile), PChar(sPath + 'TRANSFER.DEF'), False)) then
    begin
      // Ggf. alte Fehlermeldung löschen
      DeleteFile(sPath + 'TRANSFER.ERR');
      WriteJournal('Starte Transfer ' + sDefFile);
      Result := (ShellExecute(Application.Handle, nil,
        'TRANSFER.EXE', 'AUTOMATIK', PChar(sPath), SW_ShowNormal) > 32);
    end
    else Result := False;
  except
    Result := False;
  end;
end;

{--------------------------------------------}
function GetDbTranferRunning: boolean;
{--------------------------------------------}
begin
  Result :=
    (AnzahlInstanzen(VerifyWicoIni.GetClassName('TRANSFER.EXE'), '') > 0);
end;

{--------------------------------------------}
function GetLastDbTransferResult: boolean;
{--------------------------------------------}
begin
  try
    Result := (not FileExists(
      VerifyWicoIni.GetFilePath('TRANSFER.EXE') + 'TRANSFER.ERR'));
  except
    Result := False;
  end;
end;

{--------------------------------------------}
function GetDbMergerRunning: boolean;
{--------------------------------------------}
begin
  Result :=
    (AnzahlInstanzen(VerifyWicoIni.GetClassName('MERGER.EXE'), '') > 0);
end;

{--------------------------------------------}
function GetLastDbMergerResult: boolean;
{--------------------------------------------}
begin
  try
    Result := (not FileExists(
      VerifyWicoIni.GetFilePath('MERGER.EXE') + 'MERGER.ERR'));
  except
    Result := False;
  end;
end;

{------------------------------ TVerifyWic022Ini -----------------------------}

{--------------------------------------------}
constructor TVerifyWico22Ini.Create(const sFileName: TFileName);
{--------------------------------------------}
begin
  inherited Create(sFileName);

  FMySystemName := GetMySystemName;
  FMySystemState := mssNone;
  FIsSavingData := False;  // Gegenwärtig ist keine Sicherung aktiv
  FLastSaveData := 0;      // Datum der letzten Sicherung 
  with TProgramIni.Create(True, False) do
  try
    // Austauschpfade
    FMySystemFilePath := ReadString('MONITORING', 'LocalDir', '');
    if (FMySystemFilePath <> '') then
      FMySystemFilePath := IncludeTrailingBackslash(FMySystemFilePath);
    FOtherSystemFilePath := ReadString('MONITORING', 'OtherDir', '');
    FIsLog := ReadBool('SETTINGS', 'ISLOG', False);
    FStartInterval := ReadInteger('SETTINGS', 'STARTINTERVAL', 30000); // 30 sec
    FIsDataSaveToday := ReadBool('DATABASEHANDLING', 'ISDATASAVETODAY', False);
    if (FOtherSystemFilePath <> '') then
      FOtherSystemFilePath := IncludeTrailingBackslash(FOtherSystemFilePath);
    // Dateipfad herausfinden
    with TProgramIni.Create(True, False) do
    try
      with TIniFile.Create(WieserIniFile) do
      try
        FModuleFilePath := IncludeTrailingBackslash(ReadString('MONITORING',
          'MonitoringDir', (ExtractFilePath(ExpandFileName(WieserIniFile)))));
      finally
        Free;
      end;
    finally
      Free;
    end;
  finally
    Free;
  end;
end;

{--------------------------------------------}
function TVerifyWico22Ini.GetMySystemName: string;
{--------------------------------------------}
begin
  if (FMySystemName = '') then
    FMySystemName := WGetComputerName + FormatDateTime('hhnnz', Now);
  Result := FMySystemName;
end;

{ Wird ein Datenbankhandlig vorgenommen?     }
{ Rückgabe: Datenbankhandling ja/nein        }
{--------------------------------------------}
function TVerifyWico22Ini.GetHasDataVerification: boolean;
{--------------------------------------------}
begin
  Result := ReadBool(C_Section_Settings, 'VERIFYDATA', False);
end;

{ Wird ein Datenbankhandlig vorgenommen?     }
{ Parameter: Datenbankhandling ja/nein       }
{--------------------------------------------}
procedure TVerifyWico22Ini.SetHasDataVerification(bState: boolean);
{--------------------------------------------}
begin
  WriteBool(C_Section_Settings, 'VERIFYDATA', bState);
end;

{ Wird eine Modulüberprüfung durchgeführt?   }
{ Rückgabe: Modulüberprüfung ja/nein         }
{--------------------------------------------}
function TVerifyWico22Ini.GetHasRuntimeVerification: boolean;
{--------------------------------------------}
begin
  Result := ReadBool(C_Section_Settings, 'VERIFYRUNTIME', False);
end;

{ Wird ein Modulüberprüfung vorgenommen?     }
{ Parameter: Modulüberprüfung ja/nein       }
{--------------------------------------------}
procedure TVerifyWico22Ini.SetHasRuntimeVerification(bState: boolean);
{--------------------------------------------}
begin
  WriteBool(C_Section_Settings, 'VERIFYRUNTIME', bState);
end;

{ Wird ein Funktionsübernahme vorgenommen?   }
{ Rückgabe: Funktionsübernahme ja/nein       }
{--------------------------------------------}
function TVerifyWico22Ini.GetHasTakeOverVerification: boolean;
{--------------------------------------------}
begin
  Result := ReadBool(C_Section_Settings, 'VERIFYTAKEOVER', False);
end;

{ Wird ein Funktionsübernahme vorgenommen?   }
{ Parameter: Funktionsübernahme ja/nein      }
{--------------------------------------------}
procedure TVerifyWico22Ini.SetHasTakeOverVerification(bState: boolean);
{--------------------------------------------}
begin
  WriteBool(C_Section_Settings, 'VERIFYTAKEOVER', bState);
end;

{ Gibt eine Liste von Dateimodulen zurück    }
{   -> für ein Formular                      }
{ Parameter: Formular                        }
{ Rückgabe: List als Commatext               }
{--------------------------------------------}
function TVerifyWico22Ini.GetFormModuleList(pForm: TForm): string;
{--------------------------------------------}
var
  pSl : TStringList;
begin
  pSl := TStringList.Create;
  try
    ReadSectionValues(pForm.ClassName, pSl);
    Result := pSl.CommaText;
  finally
    pSl.Free;
  end;
end;

{ Gibt eine Liste von Dateimodulen zurück    }
{   -> für Anwesenheitsprüfung               }
{ Rückgabe: Exe-Liste als Commatext          }
{--------------------------------------------}
function TVerifyWico22Ini.GetLocalModuleList: string;
{--------------------------------------------}
begin
  Result := ReadString(C_Section_Settings, 'MODULELIST', '');
end;

{--------------------------------------------}
function TVerifyWico22Ini.GetClassName(const sExeName: string): string;
{--------------------------------------------}
var
  sSection : string;
begin
  sSection := ChangeFileExt(ExtractFileName(sExeName), '');
  Result := ReadString(sSection, 'CLASSNAME', '');
end;

{--------------------------------------------}
function TVerifyWico22Ini.GetServiceName(const sExeName: string): string;
{--------------------------------------------}
var
  sSection : string;
begin
  sSection := ChangeFileExt(ExtractFileName(sExeName), '');
  Result := ReadString(sSection, 'SERVICENAME', '');
end;

{--------------------------------------------}
function TVerifyWico22Ini.GetFilePath(const sExeName: string): string;
{--------------------------------------------}
var
  sSection : string;
begin
  sSection := ChangeFileExt(ExtractFileName(sExeName), '');
  Result := ReadString(sSection, 'FILEPATH', '');
  if (Result <> '') then Result := IncludeTrailingBackslash(Result);
end;

{--------------------------------------------}
function TVerifyWico22Ini.GetPrimaryParams(const sExeName: string): string;
{--------------------------------------------}
var
  sSection : string;
begin
  sSection := ChangeFileExt(ExtractFileName(sExeName), '');
  Result := ReadString(sSection, 'STARTPARAMSPRIM', '');
end;

{--------------------------------------------}
function TVerifyWico22Ini.GetSecondaryParams(const sExeName: string): string;
{--------------------------------------------}
var
  sSection : string;
begin
  sSection := ChangeFileExt(ExtractFileName(sExeName), '');
  Result := ReadString(sSection, 'STARTPARAMSSEC', '');
end;

{--------------------------------------------}
function TVerifyWico22Ini.GetIsWin32Exe(const sExeName: string): boolean;
{--------------------------------------------}
var
  sSection : string;
begin
  sSection := ChangeFileExt(ExtractFileName(sExeName), '');
  Result := ReadBool(sSection, 'ISWIN32EXE', True);
end;

{--------------------------------------------}
function TVerifyWico22Ini.GetStartPrimary(const sExeName: string): boolean;
{--------------------------------------------}
var
  sSection : string;
begin
  sSection := ChangeFileExt(ExtractFileName(sExeName), '');
  Result := ReadBool(sSection, 'STARTPRIMARY', True);
end;

{--------------------------------------------}
function TVerifyWico22Ini.GetStartSecondary(const sExeName: string): boolean;
{--------------------------------------------}
var
  sSection : string;
begin
  sSection := ChangeFileExt(ExtractFileName(sExeName), '');
  Result := ReadBool(sSection, 'STARTSECONDARY', True);
end;

{ Definitionsdateien für Transferprogramm    }
{ Rückgabe: Definitionsdateien als Commatext }
{--------------------------------------------}
function TVerifyWico22Ini.GetDbTransferDefFiles: string;
{--------------------------------------------}
begin
  // Struktur mit pipes in CommaText konvertieren
  Result := ConvertPipesToCommaText(
    ReadString('DATABASEHANDLING', 'TRANSFERDEFS', ''));
end;

{ Definitionsdateien für Transferprogramm    }
{   -> Für Übernahme-Prozess                 }
{ Rückgabe: Definitionsdateien als Commatext }
{--------------------------------------------}
function TVerifyWico22Ini.GetDbTransferTakeOverDefFiles: string;
{--------------------------------------------}
begin
  // Struktur mit pipes in CommaText konvertieren
  Result := ConvertPipesToCommaText(
    ReadString('DATABASEHANDLING', 'TRANSFERTAKEOVERDEFS', ''));
end;

{ Definitionsparameter für einen DB-Transfer }
{ Rückgabe: Erfolg ja/nein                   }
{--------------------------------------------}
function TVerifyWico22Ini.GetDbTransferDef(const sDefFile: string;
  var bStopModules: boolean; var dtStartTime: TDateTime;
  var iStartIntervalMs: integer; var sTriggerFile: string): boolean;
{--------------------------------------------}
var
  s : string;
begin
  try
    bStopModules := False;
    dtStartTime := -1;
    iStartIntervalMs := -1;
    sTriggerFile := '';
    s := ReadString('TRANSFERDEFS', sDefFile, '');
    if (s <> '') then begin
      Result := True;
      // Struktur: <stoppen>|<startzeit>|<intervall>|<triggerfile>
      bStopModules := (GetStringPart(s, 1, '|') = '1');
      dtStartTime := StrToDateTimeDef(GetStringPart(s, 2, '|'), -1);
      if (dtStartTime < 0) then begin
        iStartIntervalMs := StrToIntDef(GetStringPart(s, 3, '|'), -1);
        if (iStartIntervalMs < 0) then sTriggerFile := GetStringPart(s, 4, '|');
      end;
    end
    else Result := False;
  except
    Result := False;
  end;
end;

{ Definitionsparameter für DB-Merging        }
{ Rückgabe: Erfolg ja/nein                   }
{--------------------------------------------}
function TVerifyWico22Ini.GetDbMergerDef(
  var bStopModules: boolean; var dtStartTime: TDateTime;
  var iStartIntervalMs: integer; var sTriggerFile: string): boolean;
{--------------------------------------------}
var
  s : string;
begin
  try
    bStopModules := False;
    dtStartTime := -1;
    iStartIntervalMs := -1;
    sTriggerFile := '';
    bStopModules := ReadBool('MERGER', 'STOPMODULES', True);
    s := ReadString('MERGER', 'STARTTIME', '-');
    dtStartTime := StrToDateTimeDef(s, -1);
    iStartIntervalMs := ReadInteger('MERGER', 'STARTINTERVAL', -1);
    sTriggerFile := ReadString('MERGER', 'TRIGGERFILE', '');
    Result := True;
  except
    Result := False;
  end;
end;

{ Merger auch vor Funktionsübergabe starten? }
{ Rückgabe: ja/nein                          }
{--------------------------------------------}
function TVerifyWico22Ini.GetDbMergerTakeOver: boolean;
{--------------------------------------------}
begin
  Result := ReadBool('MERGER', 'STARTONTAKEOVER', True);
end;

{ Intervall für BDE-Prüfung                  }
{ Rückgabe: Interval in ms, 0=aus            }
{--------------------------------------------}
function TVerifyWico22Ini.GetBdeCheckInterval: Cardinal;
{--------------------------------------------}
begin
  Result := ReadInteger('WOSRestartBDE', 'INTERVAL', 0);
end;

{ Setzt den Daten-Sicherungs-Status          }
{ Parameter: Sicherung aktiv?                }
{--------------------------------------------}
procedure TVerifyWico22Ini.SetIsSaveDataState(bState: boolean);
{--------------------------------------------}
begin
  if (FIsSavingData <> bState) then begin
    FIsSavingData := bState;
    if (not FIsSavingData) then FLastSaveData := Now;
  end;
end;

{ Gibt Datum der letzten Sicherung zurück    }
{ Rückgabe: Endedatum der letzten Sicherung  }
{--------------------------------------------}
function TVerifyWico22Ini.GetLastSaveData: TDateTime;
{--------------------------------------------}
begin
  if (FIsSavingData) then Result := Now else Result := FLastSaveData;
end;

end.
