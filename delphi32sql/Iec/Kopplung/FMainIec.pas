{******************************************************************************}
{* Unit: Hauptformular für IEC-Kopplung (32-Bit)                              *}
{* 26.05.2003  WW                                                             *}
{******************************************************************************}
unit FMainIec;

interface

uses
  ShellApi, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, WDialogs, ScktComp, DbTables, Menus,
  DateUtils,
  IecConst, FIecCfg, FIec101Port, FIec101Telegr, FIecDaten, Lizenz32, PathIni,
  WMessageReg, WMessageSend, WStrUtils, GD_Utils, WChars, WSysCon, IecIniFile,
  FIec104Port, FIec104Telegr, IecLog, VerifyFlags, KWLink32, IecImportDirList,
  F_IecKonfig_Custom, MBM_WriteData, DKurzzeitWerte, O_DH_Defs, novell, LogFile,
  O_IecRemoteMonitor, WSocketError;

type
  TFormMainIec32 = class(TForm)
    pnTop: TPanel;
    bbtnBeenden: TBitBtn;
    bbtnV24Monitor: TBitBtn;
    bbtnTelegrammanalyse: TBitBtn;
    bbtnInfo: TBitBtn;
    bbtnINIKonfiguration: TBitBtn;
    bbtnDatenReset: TBitBtn;
    pnClient: TPanel;
    pnBottom: TPanel;
    DialogInfo: TDialogInfo;
    lPort: TLabel;
    ServerSocketWMsg: TServerSocket;
    lNAD: TLabel;
    lNADZeit: TLabel;
    lNorm: TLabel;
    lFunktion: TLabel;
    bbtnCustomConfig: TBitBtn;
    Timer: TTimer;
    PopupMenu: TPopupMenu;
    pmMenueSchliessen: TMenuItem;
    N1: TMenuItem;
    mAnzeigen: TMenuItem;
    mInfo: TMenuItem;
    N2: TMenuItem;
    mBeenden: TMenuItem;
    LongTermTimer: TTimer;
    ServerSocketRemoteMonitor: TServerSocket;
    procedure bbtnInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bbtnBeendenClick(Sender: TObject);
    procedure bbtnV24MonitorClick(Sender: TObject);
    procedure bbtnTelegrammanalyseClick(Sender: TObject);
    procedure bbtnINIKonfigurationClick(Sender: TObject);
    procedure bbtnDatenResetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ServerSocketWMsgClientRead(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure TimerTimer(Sender: TObject);
    procedure btnCreateKZWClick(Sender: TObject);
    procedure bbtnCustomConfigClick(Sender: TObject);
    procedure mAnzeigenClick(Sender: TObject);
    procedure mInfoClick(Sender: TObject);
    procedure mBeendenClick(Sender: TObject);
    procedure LongTermTimerTimer(Sender: TObject);
    procedure ServerSocketRemoteMonitorClientError(Sender: TObject;
      Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
      var ErrorCode: Integer);
    procedure ServerSocketRemoteMonitorClientConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocketRemoteMonitorClientDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
  private
    { Private-Deklarationen }
    bFirst: boolean;
    bIsClosing: boolean;
    FIniDate: TDateTime;
    FTage_Merk: integer;
    FLizenzAbgelaufen: boolean;
    IPPort_WMsg: integer;
    Norm: TIEC870_Norm;
    Funktion: TIEC870_Funktion;
    FIecLog: boolean;
    FRundpufferMaxBytes: integer;
    IECLogFile: TIecLogFile;
    FRemoteMonitorLogFile: TIecLogFile;
    Norm101_freigeschaltet: boolean;
    Norm104_freigeschaltet: boolean;
    FStammDb: TDatabase;
    FAutoDb: TDatabase;
    FNetProgDir: string;
    FStammDir: string;
    FNetWorkDir: string;
    FIniDir: string;
    FPathServer: TPathServer;
    FWLizenz32: TWLizenz32;
    FImportDirList: TImportDirList;
    FRemoteMonitorObj: TRemoteMonitorObj;
    FRemoteMonitorPort: integer;
    FHeartBeatFile: TFileName;
    FHeartBeatFile_Datatransfer: TFileName;
    FTimeMerk: TDateTime;
    FNextHBTime: TDateTime;
    FLastMsgNKDTime: TDateTime;
    FRedundanzTO_Error: integer;
    FTriggerfile_DatenReset: string;
    tnid : TNOTIFYICONDATA;
    procedure GetKommandozeilenParameter;
    function GetIniMain (bCheckIniChanged: boolean): boolean;
    function CheckProgrammfunktionFreischaltung: boolean;
    procedure ShowIEC_Norm_Funktion;
    procedure ShowRedundanzbetrieb;
    procedure DatenReset;
    function StartWMsg: boolean;
    procedure StopWMsg;
    procedure ShowNAD (NeueZeit: boolean);
    function StartRemoteMonitorServer: boolean;
    procedure StopRemoteMonitorServer;
    procedure ShortTermAction;
    procedure Set_IecLogfiles;
    procedure CheckTriggerdateien;
    function TaskBarAddIcon (ATitle: string): boolean;
    procedure TaskBarRemoveIcon;
    procedure WMCLOSE(var Message: TMessage); message WM_CLOSE;
    procedure WMKWLink (var Message: TMessage); message WM_KWLink;
    procedure WMCOPYDATA(var msg: TWMCopyData); message WM_COPYDATA;
    procedure WMTASKBAREVENT (var message: TMessage); message WM_TASKBAREVENT;
  public
    { Public-Deklarationen }
    procedure Beenden_einleiten;
  end;

var
  FormMainIec32: TFormMainIec32;

implementation

{$R *.DFM}

resourcestring
  SMsgStartErfolgreich = 'IEC-Kopplung erfolgreich gestartet';

  SMsgLicNormenDatentypen = 'Liz. Normen/Datentypen:';
  SMsgLicenceKeineBetriebsart = 'Es ist keine Betriebsart (MRG, DSfG, KZW) lizenziert !';
  SMsgLicenceBetriebsart = 'Betriebsart ''%s'' ist lizenziert';
  SMsgLicenceNorm101     = 'Übertragung nach IEC 870-5-101 ist nicht lizenziert !';
  SMsgLicenceNorm104     = 'Übertragung nach IEC 870-5-104 ist nicht lizenziert !';

  SMsgImportEmptyDirName      = 'Die INI-Konfiguration enthält leeren Datenimport-Verzeichnisnamen !';
  SMsgImportInvalidTypkennung = 'Die INI-Konfiguration enthält Datenimport-Schlüssel mit ungültiger Typkennung !' + #13#13 +
                                'Gültiger Bereich: 0..255';
  SMsgImportUndefDirErr       = 'Undefinierter Fehler in der INI-Konfiguration der Datenimport-Verzeichnisse !';
  SMsgErrDirCreate            = 'Verzeichnis %s kann nicht erstellt werden !';

  SMsgWMsgServerOpened = 'Benachrichtigungs-Server: Port %d geöffnet';
  SMsgWMsgServerClosed = 'Benachrichtigungs-Server: Port %d geschlossen';
  SMsgErrOpenWMsgServer = 'Benachrichtigungs-Server konnte nicht geöffnet werden: %s';
  SMsgRemoteMonitorServerOpened = 'Remote-Monitor-Server: Port %d geöffnet';
  SMsgRemoteMonitorServerClosed = 'Remote-Monitor-Server: Port %d geschlossen';
  SMsgErrOpenRemoteMonitorServer = 'Remote-Monitor-Server konnte nicht geöffnet werden: %s';


{---------------------------------------------------}
procedure TFormMainIec32.FormCreate(Sender: TObject);
{---------------------------------------------------}
var
  ProgIni      : TProgramIni;
  bOK          : boolean;
  bCreateOK    : boolean;
  sIniFile     : string;

begin
  bFirst:=true;
  bIsClosing:=false;
  bCreateOK:=true;
  FIniDate:=-1;  // Default: Zeitstempel der INI-Datei unbekannt
  FTage_Merk:=0;  // für tägliche Laufzeit-Lizenzprüfung
  FLizenzAbgelaufen:=false;
  FTimeMerk:=0;
  FNextHBTime:=0;
  FLastMsgNKDTime:=0;

  Norm101_freigeschaltet:=false;
  Norm104_freigeschaltet:=false;

  isMsg_NAD_MRG_MW:=false;
  isMsg_NAD_MRG_ME:=false;
  isMsg_NAD_DSfG_AK:=false;
  isMsg_NAD_DSfG_LB:=false;
  isMsg_NKD:=false;

  IECLogFile:=nil;
  FRemoteMonitorLogFile:=nil;  
  IECLogDir:=ExtractFilePath (ParamStr(0));  // Default LogDir: Exe-Verzeichnis,
                                             // falls LogDir aus Wieser.ini nicht
                                             // zu ermitteln ist
  // Program-Ini ist immer IEC32.INI (auch für IecSrv32-Dienst):       
  sIniFile:=ExtractFilePath (ParamStr(0)) + ChangeFileExt (CIec32Ini, '');  // 10.03.2015, WW
  ProgIni:=TProgramIni.Create (true, false, sIniFile);
  try
    WieserIniFilename:=ProgIni.WieserIniFile;
    isBde := ProgIni.ReadBool('SETTINGS', 'ISBDE', True);
  finally
    ProgIni.Free;
  end;
  bbtnCustomConfig.Enabled := (not IsBde);

  { Lokalen PathServer für Log-Verzeichnis initialisieren (der globale wird in
    TIecImportThread verwendet !): 10.03.2015, WW }
  FPathServer:=TPathServer.Create (WieserIniFilename, [WLogDir], false);
  try
    try
      FPathServer.Check;
      IECLogDir:=FPathServer.Pathname[WLogDir];  // 10.03.2015, WW
    except
      // Fehler beim Initialisieren des PathServers (Log-Verzeichnis):
      on E:Exception do begin
        WriteProgramLog (E.Message, lt_Error);
        MessageBoxWhenExe (0, pchar (E.Message), pchar (Application.Title), MB_ICONERROR + MB_OK);
        Application.Terminate;
        exit;
      end;
    end;
  finally
    FreeAndNil (FPathServer);
  end;

  WriteProgramLog ('Programmstart (Version ' + C_Version_IEC + ')', lt_Info);

  { Lokalen PathServer für Programmlauf initialisieren (der globale wird in
    TIecImportThread verwendet !): }
  try
    if (isBde) then
      FPathServer:=TPathServer.Create (WieserIniFilename,  
        [WNetProgDir, WNetWorkDir, WWorkDir, WStammDir,
         WStammDb, AutoDb, ManuDb], False)
    else
      FPathServer:=TPathServer.Create (WieserIniFilename,
        [WNetProgDir, WNetWorkDir, WWorkDir, WStammDir], False);
  except
    // Fehler beim Initialisieren des PathServers (Verzeichnisse oder Databases):
    on E:Exception do begin
      WriteProgramLog (E.Message, lt_Error);
      MessageBoxWhenExe (0, pchar (E.Message), pchar (Application.Title), MB_ICONERROR + MB_OK);
      Application.Terminate;
      exit;
    end;
  end;

  try
    FPathServer.Check;
  except
    // Fehler bei PathServer.Check (Verzeichnisse oder Databases):
    on E:Exception do begin
      WriteProgramLog (E.Message, lt_Error);
      MessageBoxWhenExe (0, pchar (E.Message), pchar (Application.Title), MB_ICONERROR + MB_OK);
      Application.Terminate;
      exit;
    end;
  end;

  FNetProgDir:=FPathServer.Pathname[WNetProgDir];
  FStammDir:=FPathServer.Pathname[WStammDir];
  FNetWorkDir:=FPathServer.Pathname[WNetWorkDir];
  if (IsBde) then FStammDb := FPathServer.Database[WStammDb] else FStammDb := nil;
  if (IsBde) then FAutoDb := FPathServer.Database[AutoDb] else FAutoDb := nil;

  FWLizenz32:=TWLizenz32.Create (FNetProgDir + C_Lizenz_Filename, true);
  FImportDirList:=TImportDirList.Create (true);

  // Kommunikation mit Remote-Monitor-Clients initialisieren; 10.03.2015 WW
  FRemoteMonitorObj:=TRemoteMonitorObj.Create (ServerSocketRemoteMonitor.Socket);

  if (ParamCount > 0) and (DirectoryExists(ParamStr(1)))
  then FIniDir := ParamStr(1)
  else FIniDir := FNetProgDir;
  if (not GetIniMain (false)) then begin
    Application.Terminate;
    exit;
  end;

  GetKommandozeilenParameter;

  if not CheckProgrammfunktionFreischaltung then begin
    Application.Terminate;
    exit;
  end;

  ShowIEC_Norm_Funktion;

  { Daten-Formular erzeugen: }
  FormDatenIec:=TFormDatenIec.Create(Self, FStammDb, FAutoDb,
    FNetProgDir, FStammDir, FNetWorkDir, FPathServer.MaxKanaele, bOK);
  if not bOK then  // Fehler Create; 10.03.2015, WW
    bCreateOK:=false;

  { Telegramm- und Port-Formular je nach eingestellter IEC-Norm erzeugen: }
  if Norm = norm_iec870_101 then begin
    FormTelegrIec101:=TFormTelegrIec101.Create(Self, Funktion, FNetProgDir,
      FImportDirList, FRemoteMonitorObj);
    FormTelegrIec101.Set_IecLogfile (IecLogfile);
    FormTelegrIec101.HeartbeatFile_Datatransfer := FHeartBeatFile_Datatransfer;
    if not FormTelegrIec101.CheckTelegr then begin
      Application.Terminate;
      exit;
    end;

    FormPortIec101:=TFormPortIec101.Create(Self, FNetProgDir, IECLogFile,
      FRemoteMonitorObj, bOK);
    if not bOK then  // Fehler Create; 10.03.2015, WW
      bCreateOK:=false;
  end
  else if Norm = norm_iec870_104 then begin
    FormTelegrIec104:=TFormTelegrIec104.Create(Self, Funktion, FNetProgDir,
      FImportDirList, FRemoteMonitorObj);
    FormTelegrIec104.Set_IecLogfile (IecLogfile);
    FormTelegrIec104.HeartbeatFile_Datatransfer := FHeartBeatFile_Datatransfer;
    if not FormTelegrIec104.CheckTelegr then begin
      Application.Terminate;
      exit;
    end;

    FormPortIec104:=TFormPortIec104.Create(Self, FNetProgDir, Funktion, IecLogfile,
      FRemoteMonitorObj, bOK);
    if not bOK then  // Fehler Create; 10.03.2015, WW
      bCreateOK:=false;
  end;

  if not bCreateOK then begin  // 10.03.2015, WW
    Application.Terminate;
    exit;
  end;

  ShowRedundanzbetrieb;  { aktiven/passiven Redundanz-Betriebszustand anzeigen }
  if IECLogFile <> nil then begin  { Redundanz-Betriebszustand in Log-Datei protokollieren }
    if Redundanzbetrieb = C_Redundanz_Aktiv then  // aktiv
      IECLogFile.Write (SMsgRedundanzAktiv, '', '')
    else if Redundanzbetrieb = C_Redundanz_Passiv then  // passiv
      IECLogFile.Write (SMsgRedundanzPassiv, '', '');
  end;

  { Nur bei Funktion 'Station': Anmelden im Benachrichtigungssystem und
    Benachrichtigungs-Server öffnen }
  if Funktion = fkt_iec870_Station then begin
    if not StartWMsg then begin
      Application.Terminate;
      exit;
    end;
  end;

  { Remote-Monitor-Server öffnen: 10.03.2015, WW }
  if not StartRemoteMonitorServer then begin
    Application.Terminate;
    exit;
  end;

  { zentrale Systemüberwachung initialisieren: 27.09.2007, WW }
  VerifyFlagObject:=TVerifyFlags.Create;

  TaskBarAddIcon (lNorm.Caption);  // 05.03.2015, WW

  // Programm-Heartbeatfile schreiben, wenn konfiguriert; 05.03.2015, WW
  if (FHeartbeatFile <> '') then
    with TFileStreamExt.Create(FHeartbeatFile, fmCreate, bOK) do Free;

  WriteProgramLog(SMsgStartErfolgreich, lt_Info);  // 10.03.2015, WW
  Timer.Enabled:=true;
  LongTermTimer.Enabled:=true;
end;

{----------------------------------------------------}
procedure TFormMainIec32.FormDestroy(Sender: TObject);
{----------------------------------------------------}
begin
  { zentrale Systemüberwachung beenden: 27.09.2007, WW }
  FreeAndNil (VerifyFlagObject);

  { Remote-Monitor-Server schließen: }
  StopRemoteMonitorServer;

  { Nur bei Funktion 'Station': Benachrichtigungs-Server schließen und Abmelden
    vom Benachrichtigungssystem }
  if Funktion = fkt_iec870_Station then
    StopWMsg;

  FormPortIec104.Free;
  FormPortIec101.Free;
  FormTelegrIec101.Free;
  FormTelegrIec104.Free;
  FormDatenIec.Free;

  FRemoteMonitorObj.Free;
  FImportDirList.Free;
  FWLizenz32.Free;
  FPathServer.Free;

  FreeAndNil (FRemoteMonitorLogFile);
  FreeAndNil (IECLogFile);
  WriteProgramLog ('Programmende (Version ' + C_Version_IEC + ')', lt_Info);

  TaskBarRemoveIcon;  // 05.03.2015, WW
end;

{-----------------------------------------------------}
procedure TFormMainIec32.FormActivate(Sender: TObject);
{-----------------------------------------------------}
begin
  if bFirst then begin
    bFirst:=false;
    Application.Minimize;
  end;
end;

{-----------------------------------------}
procedure TFormMainIec32.Beenden_einleiten;
{-----------------------------------------}
begin
  Screen.Cursor:=crHourGlass;
  WriteProgramLog ('Programm wird beendet...', lt_Info);

  // Norm 104: Übertragung beenden
  if Assigned (FormTelegrIec104) then
    FormTelegrIec104.Close_Uebertragung;

  // Evtl. laufenden IEC-Importthread beenden
  if Assigned (FormTelegrIec101) then
    FormTelegrIec101.CloseImportThread
  else if Assigned (FormTelegrIec104) then
    FormTelegrIec104.CloseImportThread;

  bIsClosing:=true;  // Flag zum Beenden des Programms setzen
  Application.ProcessMessages;
end;

{--------------------------------------------------}
procedure TFormMainIec32.GetKommandozeilenParameter;
{--------------------------------------------------}
{ Kommandozeilenparameter auswerten }
var
  i: integer;
  S: string;
  S_Param: string;
begin
  S:='';
  for i:=1 to ParamCount do begin
    S_Param:=ParamStr(i);
    { Anzeige im Fenstertitel }
    S:=S + S_Param;
    if i > 1 then
      S:=S + ' ';
    { S_Param auswerten: }
    S_Param:=UpperCase (S_Param);
    { Redundanz (nur bei Funktion 'Station'): }
    if Funktion = fkt_iec870_Station then begin
      if S_Param = 'R1' then
        Redundanzbetrieb:=C_Redundanz_Passiv  { passiver Redundanzbetrieb }
      else if S_Param = 'R0' then
        Redundanzbetrieb:=C_Redundanz_Aktiv;  { aktiver Redundanzbetrieb }
    end;
  end;  { for }
  if length (S) > 0 then
    Caption:=Caption + ' [' + S + ']';
end;

{----------------------------------------------------------------------}
function TFormMainIec32.GetIniMain (bCheckIniChanged: boolean): boolean;
{----------------------------------------------------------------------}
{ INI-Konfiguration für Hauptfenster lesen;
  Übergabe: Flag 'bCheckIniChanged': wenn true, wird nur bei geänderter INI-
              Konfiguration gelesen
  Ergebnis: true, wenn INI-Konfiguration OK }
var
  Iec32Ini: TIec32Ini;
  Erg: integer;
  S: string;
  i: integer;
  DirName: string;

begin
  Result:=true;
  Iec32Ini:=TIec32Ini.Create (FIniDir);
  try
    // INI-Einstellungen, welche nur zum Programmstart gelesen werden (können):
    if (not bCheckIniChanged) then begin
      // Ggf. NetWorkDir übersteuern
      Self.FNetWorkDir := IncludeTrailingBackslash(
        Iec32Ini.ReadString('SETTINGS', 'WNETWORKDIR', Self.FNetWorkDir));
      Self.FNetWorkDir:= GDExpandFilePath (Self.FNetWorkDir);  // für relativen Pfad; 05.03.2015, WW

      { Norm }
      Norm:=Iec32Ini.Norm;
      { Funktion }
      Funktion:=Iec32Ini.Funktion;
      { Nur bei Funktion 'Zentrale': Import-Ausgabe-Verzeichnisse lesen und anlegen }
      if Funktion = fkt_iec870_Zentrale then begin
        Erg:=Iec32Ini.GetImportDirList (FImportDirList);
        if Erg < 0 then begin
          case Erg of
            -1: S:=SMsgImportEmptyDirName;     // fehlender Verzeichnisname (leer)
            -2: S:=SMsgImportInvalidTypkennung;  // ungültige Typkennung
          else
            S:=SMsgImportUndefDirErr;  // undefinierter Fehler in Verzeichnis-Einstellungen
          end;
          WriteProgramLog (S, lt_Error);
          MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
          Result:=false;
          exit;
        end;
        { Import-Ausgabe-Verzeichnisse anlegen, wenn nicht vorhanden: }
        for i:=0 to (FImportDirList.Count-1) do begin
          DirName:=TImportDirDataObj (FImportDirList [i]).Daten.DirName;
          if not DirectoryExists (DirName) then begin
            if not ForceDirectories(DirName) then begin
              // Pfad konnte nicht angelegt werden !
              S:=Format (SMsgErrDirCreate, [DirName]);
              WriteProgramLog (S, lt_Error);
              MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
              Result:=false;
            end;
          end;
        end;
      end;  { if Funktion = fkt_iec870_Zentrale then }
    end;

    // INI-Einstellungen, welche zur Laufzeit neu gelesen werden (können):
    if (not bCheckIniChanged) OR
       (bCheckIniChanged AND
        not (SameDateTime (Iec32Ini.FileDate, FIniDate))) then begin
      FIniDate:=Iec32Ini.FileDate;  // Zeitstempel der INI-Datei merken; 10.03.2015, WW

      // Log 
      FIecLog:=Iec32Ini.LogDatei;
      Set_IecLogfiles;  // Ggf. IEC-Logfiles aktivieren; 10.03.2015, WW
      // Log-Rundpuffer
      if Iec32Ini.DebugRundpuffer then  // 10.03.2015, WW
        FRundpufferMaxBytes:=Iec32Ini.DebugRundpuffergroesse_kB * 1000
      else
        FRundpufferMaxBytes:=0;  // kein Rundpuffer

      // Lebenszeichendatei für Datentransfer zwischen Kopplung und IEC-Gegenstelle; 05.03.2015, WW
      FHeartBeatFile_Datatransfer := Iec32Ini.HeartbeatFile_Datatransfer;
      if Assigned (FormTelegrIec101) then
        FormTelegrIec101.HeartbeatFile_Datatransfer:=FHeartBeatFile_Datatransfer
      else if Assigned (FormTelegrIec104) then
        FormTelegrIec104.HeartbeatFile_Datatransfer:=FHeartBeatFile_Datatransfer;

      // Programm-Lebenszeichendatei
      FHeartBeatFile := Iec32Ini.HeartbeatFile;
      { Redundanz: }
  //    Telegrammzahl_Standby:=Iec32Ini.Telegrammzahl_Standby;
  //    Zeitfenster_Standby:=Iec32Ini.Zeitfenster_Standby;
      FRedundanzTO_Error:=Iec32Ini.RedundanzTO_Error;

      // Remote-Monitor
      FRemoteMonitorPort:=Iec32Ini.RMPort;  // 10.03.2015, WW

      // Triggerdateien
      FTriggerfile_DatenReset:=Iec32Ini.Triggerdatei_DatenReset;  // 27.03.2015, WW
    end;
  finally
    Iec32Ini.Free;
  end;
end;

{------------------------------------------------------------------}
function TFormMainIec32.CheckProgrammfunktionFreischaltung: boolean;
{------------------------------------------------------------------}
{ Ergebnis: false, wenn eine notwendige Programmfunktionalität per Lizenzfile
            nicht freigeschaltet ist }
var
  sExeName: string;

begin
  Result:=true;
  if IsService then begin
    sExeName:=ExtractFileName(Application.ExeName);
    if not FWLizenz32.GetLizenzExe (sExeName) then begin
      // Programm ist nicht lizenziert !
      WriteProgramLog (CMsgLicExe, lt_Warning);
      Result:=false;
      exit;
    end;

    if not FWLizenz32.GetExeLaufzeit (sExeName) then begin
      // Programm-Lizenz ist abgelaufen !
      WriteProgramLog (CMsgLicExeLaufzeit, lt_Warning);
      Result:=false;
      exit;
      { Anm.: Laufzeit-Lizenz wird zusätzlich täglich geprüft (in Timer-Routine) }
    end;
  end;  { if IsService }

  if not FWLizenz32.GetLizenzPC ('') then begin
    // Programm ist für Rechner nicht lizenziert !
    WriteProgramLog (CMsgLicPC, lt_Warning);
    MessageBoxWhenExe (0, pchar(CMsgLicPC), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=false;
    exit;
  end;

  { Programmfunktion MRG, DSfG, KZW: }
  MRG_freigeschaltet:=FWLizenz32.ReadProgrammfunktionFromLizenzFile (FNExt_MRG);
  DSfG_freigeschaltet:=FWLizenz32.ReadProgrammfunktionFromLizenzFile (FNExt_DSfG);
  KZW_freigeschaltet:=FWLizenz32.ReadProgrammfunktionFromLizenzFile (FNExt_KZW);
  if not (MRG_freigeschaltet OR DSfG_freigeschaltet OR KZW_freigeschaltet) then begin
    WriteProgramLog (SMsgLicenceKeineBetriebsart, lt_Warning);
    MessageBoxWhenExe (0, pchar(SMsgLicenceKeineBetriebsart), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=false;
  end
  else begin
    if MRG_freigeschaltet then
      WriteProgramLog (Format (SMsgLicenceBetriebsart, ['MRG']), lt_Info);
    if DSfG_freigeschaltet then
      WriteProgramLog (Format (SMsgLicenceBetriebsart, ['DSfG']), lt_Info);
    if KZW_freigeschaltet then
      WriteProgramLog (Format (SMsgLicenceBetriebsart, ['KZW']), lt_Info);
  end;

  { Normen IEC 870-5-101/104: }
  Norm101_freigeschaltet:=FWLizenz32.ReadProgrammfunktionFromLizenzFile (FNExt_101);
  Norm104_freigeschaltet:=FWLizenz32.ReadProgrammfunktionFromLizenzFile (FNExt_104);
  if (Norm = norm_iec870_101) AND not Norm101_freigeschaltet then begin
    WriteProgramLog (SMsgLicenceNorm101, lt_Warning);
    MessageBoxWhenExe (0, pchar(SMsgLicenceNorm101), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=false;
  end
  else if (Norm = norm_iec870_104) AND not Norm104_freigeschaltet then begin
    WriteProgramLog (SMsgLicenceNorm104, lt_Warning);
    MessageBoxWhenExe (0, pchar(SMsgLicenceNorm104), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=false;
  end;
end;

{---------------------------------------------}
procedure TFormMainIec32.ShowIEC_Norm_Funktion;
{---------------------------------------------}
var
  S: string;
begin
  { Anzeige IEC-Norm: }
  if Norm = norm_iec870_101 then begin
    lNorm.Caption:='IEC 870-5-101';
    bbtnV24Monitor.Hint:='V24-Monitor';
  end
  else if Norm = norm_iec870_104 then begin
    lNorm.Caption:='IEC 870-5-104';
    bbtnV24Monitor.Hint:='TCP/IP-Monitor';
  end else
    lNorm.Caption:='undefinierte IEC-Norm';

  S:='Norm: ' + lNorm.Caption;
  WriteProgramLog (S, lt_Info);

  { Anzeige Funktion: }
  if Funktion = fkt_iec870_Station then
    lFunktion.Caption:='Station'
  else if Funktion = fkt_iec870_Zentrale then
    lFunktion.Caption:='Zentrale'
  else
    lFunktion.Caption:='undefinierte IEC-Funktion';

  S:='Funktion: ' + lFunktion.Caption;
  WriteProgramLog (S, lt_Info);

  { Anzeige von Kontrollelementen: }
  if Funktion = fkt_iec870_Station then begin
    { Benachrichtigungs-Port und NAD-Informationen sichtbar machen: }
    lPort.Visible:=true;
    lNAD.Visible:=true;
    lNADZeit.Visible:=true;

    { Schalter zum Rücksetzen der übertragenen Datenbereiche enablen: }
    bbtnDatenReset.Enabled:=true;
  end;
end;

{--------------------------------------------}
procedure TFormMainIec32.ShowRedundanzbetrieb;
{--------------------------------------------}
begin
  { Anzeige des aktiven/passiven Redundanz-Betriebszustands: }
  if Redundanzbetrieb = C_Redundanz_Aktiv then begin
    pnBottom.Caption:=SMsgRedundanzAktiv;
    pnBottom.Color:=C_ColorRedundanzAktiv;
  end
  else if Redundanzbetrieb = C_Redundanz_Passiv then begin
    pnBottom.Caption:=SMsgRedundanzPassiv;
    pnBottom.Color:=C_ColorRedundanzPassiv;
  end;
end;

{----------------------------------}
procedure TFormMainIec32.DatenReset;
{----------------------------------}
{ Zeitbereiche übertragener Daten zurücksetzen }
begin
  if Assigned (IECLogfile) then   { Logfile-Eintrag }
    IECLogfile.Write ('Zeitbereiche übertragener Daten wurden zurückgesetzt !', '', '');
  { gemerkte Bereiche übertragener Daten löschen und vorhandene Datenbereiche
    neu einlesen: }
  FormDatenIec.F_Reset;
  { Liniennummer-Verwaltungsliste zurücksetzen }
  if Assigned (FormTelegrIec101) then
    FormTelegrIec101.LinienNrVerwList.Clear
  else if Assigned (FormTelegrIec104) then
    FormTelegrIec104.LinienNrVerwList.Clear;
end;                               

{---------------------------------------}
procedure TFormMainIec32.ShortTermAction;
{---------------------------------------}
{ Aktionen, welche in kürzeren Zyklen durchgeführt werden sollen }
var
  bOK: boolean;
  dtNow: TDateTime;
  bWriteHB: boolean;

begin
  dtNow:=Now;
  // Aktionen ausführen, wenn nächster Zeitpunkt erreicht ist oder PC-Zeit
  // zurückgestellt wurde:
  if (dtNow > FNextHBTime) OR (dtNow < FTimeMerk) then begin
    FNextHBTime:=IncSecond (dtNow, 10);  // Heartbeatfile alle 10 s schreiben

    // INI neu lesen, wenn geändert; 10.03.2015, WW
    GetIniMain (true);

    // Heartbeatfile schreiben:
    if (FHeartbeatFile <> '') then begin  //  nur, wenn HB-File-Schreiben konfiguriert
      bWriteHB:=true;  // Default
      // bei nicht abgeschaltetem Error-Timeout nur, wenn letzte NKD-Benachrichtigung
      // weniger als Timeout zurückliegt:
      if ((FRedundanzTO_Error > 0) AND
          (dtNow > IncMilliSecond (FLastMsgNKDTime, FRedundanzTO_Error))) then
        bWriteHB:=false;

      // 101: bei nicht abgeschaltetem Error-Timeout nur, wenn letzter aufgetretener
      // serieller Schnittstellenfehler länger als Timeout zurückliegt:
      if Assigned (FormPortIec101) then begin
          if ((FRedundanzTO_Error > 0) AND
              (dtNow < IncMilliSecond (FormPortIec101.LastCommErrorTime, FRedundanzTO_Error))) then
            bWriteHB:=false;
      end;

      if bWriteHB then
        with TFileStreamExt.Create(FHeartbeatFile, fmCreate, bOK) do Free;
    end;

    // 101: Serielle Schnittstelle neu öffnen, wenn letzter aufgetretener
    // serieller Schnittstellenfehler länger als Timeout zurückliegt:
    if Assigned (FormPortIec101) then begin
        if (dtNow < IncMilliSecond (FormPortIec101.LastCommErrorTime, FRedundanzTO_Error)) then
          FormPortIec101.ReConnect;
    end;
  end;
  FTimeMerk:=dtNow;
end;

{---------------------------------------}
procedure TFormMainIec32.Set_IecLogfiles;  // 10.03.2015, WW
{---------------------------------------}
{ IEC-Logdateien aktivieren/deaktivieren;
  Übergabe: true = aktivieren }
var
  LogfileName: string;

begin
  if FIecLog then begin  // IEC-Logfiles ein
    if not Assigned (IECLogFile) then begin
      // IEC-Programm-Logfile
      LogFilename:=ChangeFileExt (ExtractFileName (ParamStr(0)), '');
      IECLogFile:=TIecLogFile.Create (IECLogDir, LogfileName, false);  // Logfile fortschreiben; 17.10.2007 WW
    end;

    if not Assigned (FRemoteMonitorLogFile) then begin
      // IEC-Remote-Monitor-Logfile
      LogFilename:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_RemoteMonitor';
      FRemoteMonitorLogFile:=TIecLogFile.Create (IECLogDir, LogfileName, false);
    end;
  end
  else begin  // IEC-Logfiles aus
    if Assigned (IECLogFile) then
      FreeAndNil (IECLogFile);
    if Assigned (FRemoteMonitorLogFile) then
      FreeAndNil (FRemoteMonitorLogFile);
  end;

  if Assigned (FormTelegrIec101) then
    FormTelegrIec101.Set_IecLogfile (IecLogfile);
  if Assigned (FormPortIec101) then
    FormPortIec101.Set_IecLogfile (IecLogfile);
  if Assigned (FormTelegrIec104) then
    FormTelegrIec104.Set_IecLogfile (IecLogfile);
  if Assigned (FormPortIec104) then
    FormPortIec104.Set_IecLogfile (IecLogfile);

  if Assigned (FRemoteMonitorObj) then
    FRemoteMonitorObj.Logfile:=FRemoteMonitorLogFile;
end;

{-------------------------------------------}
procedure TFormMainIec32.CheckTriggerdateien;
{-------------------------------------------}
{ Auf vorhandene Triggerdateien prüfen; 27.03.2015, WW }
begin
  // Prüfung auf Triggerdatei zum Zurücksetzen der Zeitbereiche übertragener Daten:
  if FTriggerfile_DatenReset <> '' then begin
    if FileExists (FTriggerfile_DatenReset) then begin
      if Assigned (IECLogfile) then   { Logfile-Eintrag }
        IECLogfile.Write ('Triggerdatei erkannt: ' + FTriggerfile_DatenReset, '', '');
      if not DeleteFile (FTriggerfile_DatenReset) then begin  // Triggerdatei löschen
        if Assigned (IECLogfile) then   { Logfile-Eintrag }
          IECLogfile.Write ('Triggerdatei konnte nicht gelöscht werden: ' +
                            FTriggerfile_DatenReset, '', '', lt_Error);
      end;

      DatenReset;  // Zeitbereiche zurücksetzen
    end;
  end;
end;

{----------------------- Wieser Benachrichtigung ------------------------------}

{-----------------------------------------}
function TFormMainIec32.StartWMsg: boolean;
{-----------------------------------------}
var
  WMsgTypes: TWMsgTypes;
  S: string;

begin
  WMsgTypes:=[];
  if MRG_freigeschaltet OR DSfG_freigeschaltet then
    WMsgTypes:=WMsgTypes + [wmt_NeueArchivdaten];

  if KZW_freigeschaltet then
    WMsgTypes:=WMsgTypes + [wmt_NeueKurzzeitDaten];

  { Anmelden im Benachrichtigungssystem: }
  if RegisterWieserMessaging (FNetProgDir, WMsgTypes, IPPort_WMsg) then begin
    lPort.Caption:='Port: ' + IntToStr(IPPort_WMsg);
    S:='Port für Benachrichtigung: ' + IntToStr(IPPort_WMsg);
    WriteProgramLog (S, lt_Info);

    { Benachrichtigungs-Server mit Benachrichtigungs-Port öffnen: }
    ServerSocketWMsg.Port:=IPPort_WMsg;
    try
      ServerSocketWMsg.Active:=True;
      WriteProgramLog (Format (SMsgWMsgServerOpened, [ServerSocketWMsg.Port]), lt_Info); 
      Result:=true;
    except
      on E:Exception do begin  // 10.03.2015, WW
        S:=Format (SMsgErrOpenWMsgServer, [E.Message]);
        WriteProgramLog (S, lt_Error);
        MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
        Result:=false;
      end;
    end;
  end
  else begin
    WriteProgramLog (SMsgErrAnmeldenWMsg, lt_Error);
    MessageBoxWhenExe (0, pchar(SMsgErrAnmeldenWMsg), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=false;
  end;
end;

{--------------------------------}
procedure TFormMainIec32.StopWMsg;
{--------------------------------}
begin
  { Benachrichtigungs-Server schließen: }
  if ServerSocketWMsg.Active then begin
    ServerSocketWMsg.Active:=false;
    WriteProgramLog (Format (SMsgWMsgServerClosed, [ServerSocketWMsg.Port]), lt_Info); 
  end;

  { Abmelden vom Benachrichtigungssystem: }
  if not UnregisterWieserMessaging (FNetProgDir, IPPort_WMsg) then begin
    WriteProgramLog (SMsgErrAbmeldenWMsg, lt_Error);
    MessageBoxWhenExe (0, pchar(SMsgErrAbmeldenWMsg), pchar (Application.Title), MB_ICONERROR + MB_OK);
  end;
end;

{------------------------------------------------------------------}
procedure TFormMainIec32.ServerSocketWMsgClientRead(Sender: TObject;
  Socket: TCustomWinSocket);
{------------------------------------------------------------------}
var
  S, RecText: string;
  i: integer;
  sEvent: string;
  sGerArt: string;
  sDatentyp: string;

begin
  RecText:='';
  S:=Socket.ReceiveText;
  while S <> '' do begin
    RecText:=RecText + S;
    S:=Socket.ReceiveText;
  end;

  if length (RecText) > 0 then begin
    i:=0;
    S:=ExtractString (RecText, STX, ETX, i);
    while length (S) > 0 do begin
      { Logfile-Eintrag: eingetroffene Nachricht }
      if Assigned (IECLogfile) then
        IECLogfile.Write ('Nachricht: ' + SonderzeichenString (STX + S + ETX), '', '');

      { Nachricht auswerten: }
      sEvent:=ExtractString (S, GS, GS, 1);
      if sEvent = CWMsg_Event_NAD then begin
        sGerArt:=ExtractString (S, GS, GS, 2);
        sDatentyp:=ExtractString (S, GS, GS, 3);
        if sGerArt = CWMsg_MRG then begin
          if sDatentyp = CWMsg_MW then
            isMsg_NAD_MRG_MW:=true
          else if sDatentyp = CWMsg_ME then
            isMsg_NAD_MRG_ME:=true;
        end
        else if sGerArt = CWMsg_DSfG then begin
          if sDatentyp = CWMsg_AK then
            isMsg_NAD_DSfG_AK:=true
          else if sDatentyp = CWMsg_LB then
            isMsg_NAD_DSfG_LB:=true;
        end;
      end
      else if sEvent = CWMsg_Event_NKD then begin
        isMsg_NKD:=true;
        FLastMsgNKDTime:=Now;  // 05.03.2015, WW
      end;

      inc (i);
      S:=ExtractString (RecText, STX, ETX, i);
    end;  { while }
  end;  { if length (RecText) }

 { Anzeige des aktuellen NAD-Zustands: }
  ShowNAD (true);
end;

{---------------------------------------------------}
procedure TFormMainIec32.ShowNAD (NeueZeit: boolean);
{---------------------------------------------------}
{ Anzeige des aktuellen NAD-Zustands }
var
  S: string;
begin
  S:='';
  if isMsg_NAD_MRG_MW then
    S:=S + ' MRG-MW';
  if isMsg_NAD_MRG_ME then
    S:=S + ' MRG-ME';
  if isMsg_NAD_DSfG_AK then
    S:=S + ' DSfG-AK';
  if isMsg_NAD_DSfG_LB then
    S:=S + ' DSfG-LB';
  if length (S) > 0 then
    S:=' NAD (' + S + ')';

  if isMsg_NKD then
    S:=S + ' NKD';

  lNAD.Caption:='Nachr.:' + S;

  if NeueZeit then
    lNADZeit.Caption:='am: ' + FormatDateTime(C_FormatDateTime, Now);
end;

{-------------------------- Remote Monitoring ---------------------------------}

{--------------------------------------------------------}
function TFormMainIec32.StartRemoteMonitorServer: boolean;
{--------------------------------------------------------}
var
  S: string;

begin
  { Remote-Monitor-Server öffnen: }
  ServerSocketRemoteMonitor.Port:=FRemoteMonitorPort;
  try
    ServerSocketRemoteMonitor.Active:=True;
    WriteProgramLog (Format (SMsgRemoteMonitorServerOpened, [ServerSocketRemoteMonitor.Port]), lt_Info); 
    Result:=true;
  except
    on E:Exception do begin
      S:=Format (SMsgErrOpenRemoteMonitorServer, [E.Message]);
      WriteProgramLog (S, lt_Error);
      MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
      Result:=false;
    end;
  end;      
end;

{-----------------------------------------------}
procedure TFormMainIec32.StopRemoteMonitorServer;
{-----------------------------------------------}
begin
  { Remote-Monitor-Server schließen: }
  if ServerSocketRemoteMonitor.Active then begin
    ServerSocketRemoteMonitor.Active:=false;
    WriteProgramLog (Format (SMsgRemoteMonitorServerClosed, [ServerSocketRemoteMonitor.Port]), lt_Info);
  end;
end;

{--------------------------------------------------------------}
procedure TFormMainIec32.ServerSocketRemoteMonitorClientConnect(
  Sender: TObject; Socket: TCustomWinSocket);
{--------------------------------------------------------------}
begin
  { Remote-Monitor-Logfile-Protokollierung: }
  if Assigned (FRemoteMonitorLogFile) then
    FRemoteMonitorLogFile.Write (Format ('%s:%d'#9'%s',
      [Socket.RemoteAddress, Socket.RemotePort, 'Open']), '', '');
end;

{-----------------------------------------------------------------}
procedure TFormMainIec32.ServerSocketRemoteMonitorClientDisconnect(
  Sender: TObject; Socket: TCustomWinSocket);
{-----------------------------------------------------------------}
begin
  { Remote-Monitor-Logfile-Protokollierung: }
  if Assigned (FRemoteMonitorLogFile) then
    FRemoteMonitorLogFile.Write (Format ('%s:%d'#9'%s',
      [Socket.RemoteAddress, Socket.RemotePort, 'Close']), '', '');
end;

{----------------------------------------------------------------------------}
procedure TFormMainIec32.ServerSocketRemoteMonitorClientError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
{----------------------------------------------------------------------------}
var
  S: string;
  sErr: string;

begin
  { Remote-Monitor-Logfile-Protokollierung: }
  if Assigned (FRemoteMonitorLogFile) then begin
    sErr:='ErrorEvent: ' + SocketErrorEventToString (ErrorEvent) +
       ' (' + IntToStr (Integer(ErrorEvent)) + '), ' +
       'ErrorCode: ' + SocketErrorCodeToString (Errorcode) +
       ' (' + IntToStr (ErrorCode) + ')';
    S:=Format ('%s:%d'#9'%s'#9'%s',
      [Socket.RemoteAddress, Socket.RemotePort, 'Error', sErr]);
    FRemoteMonitorLogFile.Write (S, '', '', lt_Error);
  end;

  ErrorCode:=0;  // Exception-Ausgabe unterdrücken
end;

{------------------------- Message-Behandlungsmethoden ------------------------}

{-------------------------------------------------------}
procedure TFormMainIec32.WMCLOSE (var Message: TMessage);
{-------------------------------------------------------}
begin
  Application.Minimize;  // nicht Beenden, nur minimieren; 05.03.2015, WW
end;

{--------------------------------------------------------}
procedure TFormMainIec32.WMKWLink (var Message: TMessage);
{--------------------------------------------------------}
var
  Redundanzbetrieb_Neu: integer;
begin
  case Message.WParam of
    KWE_Redundanz_Status:  { Redundanz-Betriebszustand aktiv/passiv }
    begin
      if Redundanzbetrieb <> C_Redundanz_Aus then begin  { Programm läuft im Redundanz-Modus }
        Redundanzbetrieb_Neu:=Message.LParam;  // neuer Redundanz-Betriebszustand
        if (Redundanzbetrieb_Neu = C_Redundanz_Aktiv) OR
           (Redundanzbetrieb_Neu = C_Redundanz_Passiv) then begin  // gültiger neuer Betriebszustand
          if Redundanzbetrieb_Neu <> Redundanzbetrieb then begin  // Betriebszustand hat gewechselt
            Redundanzbetrieb:=Redundanzbetrieb_Neu;  // neuen Betriebszustand setzen
            ShowRedundanzbetrieb;  // neuen Betriebszustand anzeigen

            // Norm 104: Redundanz-Betriebszustands-Telegramm soll gesendet werden
            if Assigned (FormTelegrIec104) then
              FormTelegrIec104.Set_SendRedundanzTelegrammNow (true);

            if Redundanzbetrieb = C_Redundanz_Aktiv then begin  // Wechsel passiv -> aktiv
              if IECLogFile <> nil then  { Wechsel in Log-Datei protokollieren }
                IECLogFile.Write (SMsgRedundanzPassiv_Aktiv, '', '');

              { vorhandenen Datenbereich einlesen (alle Datentypen) nur bei
                Funktion 'Station': }
              if Funktion = fkt_iec870_Station then begin
                FormDatenIec.F_Unlock;  // Sperren in IEC-Datentabellen aufheben
                FormDatenIec.F_Update (true, true, true, true);  // MRG/DSfG-Archivdaten
                if KZW_freigeschaltet AND ((FormDatenIec.DSfGArchivAuswahl = daa_Archiv_KZW) OR
                                           (FormDatenIec.DSfGArchivAuswahl = daa_nur_KZW)) then
                  FormDatenIec.KZWListe.LadeKZWFiles;  // DSfG-Kurzzeitwerte
              end;
            end
            else begin  // Wechsel aktiv -> passiv
              if IECLogFile <> nil then  { Wechsel in Log-Datei protokollieren }
                IECLogFile.Write (SMsgRedundanzAktiv_Passiv, '', '');
            end;
          end;
        end;
      end;
    end;
  end;
end;

{---------------------------------------------------------}
procedure TFormMainIec32.WMCOPYDATA (var Msg: TWMCopyData);
{---------------------------------------------------------}
const
  C_TelNo : integer = 0;
var
  RegisterData: TRegisterData_WindowsMessage;
  KZWData: TDKZWData;
  KZWListObj: TKZWListObj;
begin
  try
    if (Assigned(FormDatenIec)) and (Assigned(FormDatenIec.KZWListe)) then begin
      RegisterData:=PRegisterData_WindowsMessage (msg.CopyDataStruct.lpData)^;

      Inc(C_TelNo);
      with KZWData do begin
        Datentyp  := byte(citModbusMom);
        TelegrNr  := C_TelNo;  // fortlaufende Nummer wie vom Server empfangen
        Kennung   := IntToStr(RegisterData.Linie);
        EAdr      := IntToStr(RegisterData.SlaveAdr);
        DEL       := IntToStr(RegisterData.RegisterAdr);
        DatumZeit := Now;
        Wert      := StrToFloat(RegisterData.Wert);
      end;
      KZWListObj := TKZWListObj.Create;
      KZWListObj.SetData(
        KZWData, ParamStr(0), DateTimeToFileDate(KZWData.DatumZeit));
      FormDatenIec.KZWListe.Add(KZWListObj);
      isMsg_NKD := True;
      Application.ProcessMessages;
    end;
  except
    on E:Exception do begin  
      if IECLogFile <> nil then
        IECLogFile.Write('Fehler Empfang WMCOPYDATA: ' + E.Message, '', '', lt_Error);
    end;
  end;
end;

{--------------------------------------------------------------}
procedure TFormMainIec32.WMTASKBAREVENT (var message: TMessage);
{--------------------------------------------------------------}
var
  point: TPoint;
begin
  case message.LParam of
    WM_LBUTTONDBLCLK: begin
                        mAnzeigenClick (Self);
                      end;
    WM_RBUTTONDOWN: begin
                      GetCursorPos(point);
                      PopupMenu.Popup(point.x,point.y);
                    end;
  end;
end;

{---------------------- Taskbarhandling-Methoden ------------------------------}

{---------------------------------------------------------------}
function TFormMainIec32.TaskBarAddIcon (ATitle: string): boolean;
{---------------------------------------------------------------}
begin
  tnid.cbSize := SizeOf(TNOTIFYICONDATA);
  tnid.Wnd := Handle;                                 { Handle des Fensters }
  tnid.uID := 1;                                      { ID beliebig }
  tnid.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
  tnid.uCallbackMessage := WM_TASKBAREVENT;
  tnid.hIcon := Application.Icon.Handle;              { Handle des Programmicons }
  StrCopy (tnid.szTip, pchar (ATitle));               { Tooltiptext: Service-Titel }
  Result:=Shell_NotifyIcon (NIM_ADD, @tnid);          { Registrieren ... }
end;

{-----------------------------------------}
procedure TFormMainIec32.TaskBarRemoveIcon;
{-----------------------------------------}
begin
  Shell_NotifyIcon (NIM_DELETE, @tnid);                { Löschen ... }
end;

{----------------------- Timer-Methode ----------------------------------------}

{---------------------------------------------------}
procedure TFormMainIec32.TimerTimer(Sender: TObject);
{---------------------------------------------------}
{ prüft zyklisch auf neue zu übertragende Daten }
var
  d: integer;
  sExeName: string;

begin
  Timer.Enabled:=false;
  try
    Application.ProcessMessages;

    d:=Trunc (Date);
    // beim Tageswechsel und bei Start:
    if d <> FTage_Merk then begin
      FTage_Merk:=d;

      // Laufzeit-Lizenz prüfen:
      if Assigned (FWLizenz32) then begin
        sExeName:=ExtractFileName(Application.ExeName);
        if not FWLizenz32.GetExeLaufzeit (sExeName) then begin
          // Programm-Lizenz ist abgelaufen !
          // Flags setzen zum Deaktivieren der Programmfunktionen:
          FLizenzAbgelaufen:=true;
          if Assigned (FormPortIec101) then
            FormPortIec101.Stopped:=true  // 101-Portkommunikation stoppen
          else if Assigned (FormPortIec104) then
            FormPortIec104.Stopped:=true;  // 104-Portkommunikation stoppen

          WriteProgramLog (CMsgLicExeLaufzeit, lt_Warning);
          MessageBoxWhenExe (0, pchar(SMsgLicExeLaufzeit), pchar (Application.Title), MB_ICONERROR + MB_OK);
        end;
      end;
    end;

    if not FLizenzAbgelaufen then begin
  (*{$IFDEF Redundanz}
        { Normal-/Standbybetrieb-Überwachung: }
        inc (Sec_Counter_Standby);
        if Sec_Counter_Standby >= Zeitfenster_Standby then begin
          Check_Betriebsart;
          Sec_Counter_Standby:=0;
        end;
  {$ENDIF} *)

      { Anzeige des aktuellen NAD/NKD-Zustands (Zeit nicht aktualisieren): }
      ShowNAD (false);

      // Anzeige-Daten an alle verbundenen Remote-Monitor-Clients senden: 10.03.2015, WW
      if Assigned (FormPortIec104) then
        FormPortIec104.SendToRemoteClient (C_RM_104);

      if Assigned (FormTelegrIec101) then
        FormTelegrIec101.SendToRemoteClient (C_RM_101)
      else if Assigned (FormTelegrIec104) then
        FormTelegrIec104.SendToRemoteClient (C_RM_104);

      { Ausfühung kurzzyklischer Aktionen, u.a. ggf. Heartbeatfile schreiben: 05.03.2015, WW
        -> Die IEC-Kopplung ist an dieser Stelle korrekt gestartet.
        -> Das Heartbeatfile wird geschrieben, auch wenn die IEC-Gegenstelle nicht
           verbunden ist oder keine Telegramme sendet. }
      ShortTermAction;

      { Auf vorhandene Triggerdateien prüfen; 27.03.2015, WW }
      CheckTriggerdateien;
    end;
  finally
    Timer.Enabled:=true;
  end;

  if bIsClosing then
    Close;  // damit Programm sicher beendet wird, 'Close' über Timer notfalls mehrfach aufrufen
end;

{-----------------------------------------------------------}
procedure TFormMainIec32.LongTermTimerTimer(Sender: TObject);
{-----------------------------------------------------------}
{ Timer für alles was in größeren Zyklen nebenbei zu erledigen ist }
begin
  LongTermTimer.Enabled:=false;
  try
    Application.ProcessMessages;

    if not FLizenzAbgelaufen then begin
      // INI neu lesen, wenn geändert; 10.03.2015, WW
      GetIniMain (true);

      // Rundpuffer auf IEC-Logfiles ausführen, wenn konfiguriert; 10.03.2015, WW
      if FRundpufferMaxBytes > 0 then begin
        if Assigned (IECLogFile) then
          IECLogFile.DoRundpuffer (FRundpufferMaxBytes);
        if Assigned (FRemoteMonitorLogFile) then
          FRemoteMonitorLogFile.DoRundpuffer (FRundpufferMaxBytes);
      end;
    end;
  finally
    LongTermTimer.Enabled:=true;
  end;
end;

{--------------------- Popupmenü-Methoden -------------------------------------}

{-------------------------------------------------------}
procedure TFormMainIec32.mAnzeigenClick(Sender: TObject);
{-------------------------------------------------------}
begin
  Application.Restore;
  BringToFront;
end;

{---------------------------------------------------}
procedure TFormMainIec32.mInfoClick(Sender: TObject);
{---------------------------------------------------}
begin
  bbtnInfoClick(Sender);
end;

{------------------------------------------------------}
procedure TFormMainIec32.mBeendenClick(Sender: TObject);
{------------------------------------------------------}
begin
  Beenden_einleiten;
end;

{-------------------- Schalter-Methoden ---------------------------------------}

{---------------------------------------------------------}
procedure TFormMainIec32.bbtnBeendenClick(Sender: TObject);
{---------------------------------------------------------}
begin
  Application.Minimize;  // nicht Beenden, nur minimieren; 05.03.2015, WW
end;

{------------------------------------------------------------}
procedure TFormMainIec32.bbtnV24MonitorClick(Sender: TObject);
{------------------------------------------------------------}
begin
  if Assigned (FormPortIec101) then
    FormPortIec101.Show
  else if Assigned (FormPortIec104) then
    FormPortIec104.Show;
end;

{------------------------------------------------------------------}
procedure TFormMainIec32.bbtnTelegrammanalyseClick(Sender: TObject);
{------------------------------------------------------------------}
begin
  if Assigned (FormTelegrIec101) then
    FormTelegrIec101.Show
  else if Assigned (FormTelegrIec104) then
    FormTelegrIec104.Show;
end;

{---------------------------------------------------------------}
procedure TFormMainIec32.bbtnINIKonfigurationClick(Sender: TObject);
{---------------------------------------------------------------}
var
  IecConfigDlg: TIecConfigDlg;
begin
  IecConfigDlg:=TIecConfigDlg.Create (Self);
  try
    IecConfigDlg.ShowModal;
  finally
    IecConfigDlg.Free;
  end;
end;

{------------------------------------------------------------}
procedure TFormMainIec32.bbtnDatenResetClick(Sender: TObject);
{------------------------------------------------------------}
begin
  if MessageDlg (SMsgResetZeitbereiche, mtWarning, [mbOk, mbCancel], 0) = mrOk then
    DatenReset;
end;

{------------------------------------------------------}
procedure TFormMainIec32.bbtnInfoClick(Sender: TObject);
{------------------------------------------------------}
begin
  DialogInfo.LicOptCaption:=SMsgLicNormenDatentypen;
  DialogInfo.LicOptItems.Clear;
  { Lizenzoptionen: Normen IEC 870-5-101/104 }
  if Norm101_freigeschaltet then
    DialogInfo.LicOptItems.Add ('IEC 870-5-101');
  if Norm104_freigeschaltet then
    DialogInfo.LicOptItems.Add ('IEC 870-5-104');
  { Lizenzoptionen: Datentypen }
  if MRG_freigeschaltet then
    DialogInfo.LicOptItems.Add ('MRG-Daten');
  if DSfG_freigeschaltet then
    DialogInfo.LicOptItems.Add ('DSfG-Daten');
  if KZW_freigeschaltet then
    DialogInfo.LicOptItems.Add ('Kurzzeitwerte');

  DialogInfo.Execute;
end;

{--------------------------------------------------------------}
procedure TFormMainIec32.bbtnCustomConfigClick(Sender: TObject);
{--------------------------------------------------------------}
var
  pIec32Ini : TIec32Ini;
begin
  with TFormIecKonfigCustom.Create(Self) do begin
    BorderStyle := bsSizeable;
    Position := poScreenCenter;
    SetXMLDir(FNetProgDir);
    pIec32Ini := TIec32Ini.Create(FNetProgDir);
    try
      SetIecIni(pIec32Ini);
    finally
      pIec32Ini.Free;
    end;
    Show;
  end;
end;

procedure TFormMainIec32.btnCreateKZWClick(Sender: TObject);
const
  C_TelNo : integer = 1;
var
  pData : TDKZWData;
begin
  with TDSfGKurzzeitwert.Create(FNetWorkDir) do
  try
    with pData do begin
      TelegrNr := C_TelNo;
      Datentyp := Byte(citDSfGMom);
      Kennung := 'GEDA00000000';
      EAdr := 'I';
      DEL := 'caafd';
      DatumZeit := Now;
      Wert := 123456;
    end;
    Inc(C_TelNo);

    WriteFile_IECKopplung(pData);
    Application.ProcessMessages;
    isMsg_NKD := True;
    Application.ProcessMessages;
  finally
    Free;
  end;
end;

//!! Offen: Für Zählwerte (Typkennungen 15, 16, 37) ist lt. IEC-Norm
//   Übertragungsursache 20 (abgefragt durch GA) nicht erlaubt. In der
//   derzeitigen Version wird dies für alle Typkennungen bei durch GA abgefragte
//   Daten gemacht. Was tun ? GA-Zählwerte mit Übertragungsursache 'spontan' senden,
//   wie früher ?

end.




