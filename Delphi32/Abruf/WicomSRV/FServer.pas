{******************************************************************************}
{* Unit: Formular für MRG/DSfG-Abrufserver-Ausgaben                           *}
{* 19.02.2003  WW                                                             *}
{* 05.11.2007  WW  resourcestrings                                            *}
{* 06.10.2010  GD  Modemeinschränkung für GAS-X herausgenommen                *}
{* 05.11.2010  GD  Bugfix: Modemeinschränkung für GAS-X LogCom -> PhysCom     *}
{* 08.01.2018  WW  mit TCP/IP-Rufentgegennahme für DSfG                       *}
{* 03.06.2020  WW  mit optional verschlüsselter Kommunikation zum Client (SSL *}
{*                 2.0, SSL 3.0, TLS 1.0) und optionaler Basis-Authentifizier-*}
{*                 ung im Request-Header; Logdatei WicomSrv_AbrufSocketSSL.log*}
{*                 GPRS-ServerSocket stillgelegt (wird nicht mehr unterstützt)*}
{* 14.07.2020  WW  Visible = true (Bugfix "Systemfehler. Code: 1400. Ungülti- *}
{*                 ges Fensterhandle" beim Freigeben des Fensters, wenn Formu-*}
{*                 lar in Dienst eingebettet ist)                             *}
{* 03.11.2020  WW  Bugfix Timer gestoppt, wenn Initialisieren der Programm-   *}
{*                 funktionen fehlschlägt; mit Inaktivitäts-Timer 2 min für   *}
{*                 Abruf-Clientverbindung; Abruf-Clientverbindungen in Datei  *}
{*                 WicomSrv_AbrufSocketCon.log loggen (per Ini-Schalter       *}
{*                 ServiceIO aktivierbar); Erweiteres Exception-Handling für  *}
{*                 Haupt-Thread                                               *}
{* 17.12.2020  WW  Bugfix langsamer SSL-Verbindungsaufbau (Visible = false);  *}
{*                 kein Schreiben in Memo-Felder, wenn Visible = false, um    *}
{*                 "Systemfehler. Code: 1400..." zu vermeiden                 *}
{* 06.08.2021  WW  Ressourcedatei-Daten in Pufferlisten laden bei Programm-   *}
{*                 Initialisierung                                            *}
{* 11.02.2022  WW  MBAbruf-Ressource in Pufferliste laden                     *}
{* 05.10.2022  WW  mit Binding auf IP-Adresse für Abruf-Serversocket;         *}
{*                 Veraltete Temporär- und Request/Response-Dateien des Pro-  *}
{*                 gramms löschen                                             *}
{* 31.01.2023  WW  Geänderte Lizenzdatei während Programmlaufzeit automatisch *}
{*                 erkennen (nur Gas-X-Version)                               *}
{* 14.04.2023  WW  ParamMomVerb-Ressource in Pufferliste laden                *}
{******************************************************************************}
unit FServer;

interface

uses
  SvcMgr, ShellApi, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, StdCtrls, ScktComp, ComCtrls, ExtCtrls, Menus, Contnrs, DecodeReq,
  AbrufThr, AbrufCmdList, Serial, AbrufSrvIniFile, PathIni, WStrUtils, LogFile,
  Lizenz32, AbrufCmd, SrvCfgIni, MLGZKonvList, AbrufConst, VerifyFlags, WSysCon,
  GPRSVerbList, GPRSTelegrList, O_GPRSServerSocket, GD_Utils, WChars, WSocketError,
  ImgList, GPRS_Util, AusgabeDirList, WMessageReg, WMessageSend, Novell, O_DSfGSimu,
  REClntVerbList, O_WIdTCPServer, IdTCPServer, IdContext, IdTCPConnection, IdSSL,
  IdSSLOpenSSL, IdComponent, T_Zeit, O_ResFilesList, MResParam, MResMrg, MResUtil,
  WResMeld, WComm, MFileNam;

resourcestring
  SGasX = 'GAS-X';

Type
  TFormAbrufServer = class(TForm)
    PopupMenu: TPopupMenu;
    pmMenueSchliessen: TMenuItem;
    mAnzeigen: TMenuItem;
    mHilfe: TMenuItem;
    N1: TMenuItem;
    Timer: TTimer;
    pAbrufServer: TPanel;
    pPortAbrufServer: TPanel;
    pMemoAbrufServer: TPanel;
    memoAbrufServer: TMemo;
    pGPRSServer: TPanel;
    pPortGPRSServer: TPanel;
    pMemoGPRSServer: TPanel;
    memoGPRSServer: TMemo;
    pReceive: TPanel;
    Panel1: TPanel;
    Splitter: TSplitter;
    Splitter1: TSplitter;
    lvGPRSVerbindungen: TListView;
    pGPRSVerbAktiv: TPanel;
    pGPRSVerbInaktiv: TPanel;
    GPRSVerbImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
    procedure mAnzeigenClick(Sender: TObject);
    procedure mHilfeClick(Sender: TObject);
  private
    AbrufTCPServer: TWIdTCPServer;  // 03.06.2020, WW
    AbrufServerIOHandlerSSL: TIdServerIOHandlerSSLOpenSSL;  // 03.06.2020, WW
    AbrufControls: TAbrufControls;
    FDebugServiceIO: boolean;
    FDebugRohdaten: boolean;  // 05.10.2022, WW
    FServerBasicAuthentication: string;
    AbrufSocketErrLogFile: TCustomLogFile;
    AbrufSocketConLogFile: TCustomLogFile;  // 03.11.2020, WW
    AbrufSocketSSLLogFile: TCustomLogFile;
    tnid : TNOTIFYICONDATA;
    AbrufSrvIni: TAbrufSrvIni;
    FSrvCfgIni: TSrvCfg32Ini;
    FFormDestroying: boolean;
    FHBTimerCount: integer;
    FLogDir: string;
    FAllControlsClosed: boolean;  // 03.06.2020, WW
    FWLizenz32: TWLizenz32;
    FInitLizenzfreischaltung_OK: boolean;  // 28.09.2020, WW
    FAllow_UnbegrenzteLaufzeit: boolean;
    FResourceFilesList: TResourceFilesList;  // 06.08.2021, WW
    FNextDeleteOldTempFiles: TDateTime;  // 05.10.2022, WW

    { GPRS: }
    GPRSAusgabeDirList: TAusgabeDirList;
    GPRSServerSocketObj: TGPRSServerSocketObj;
    GPRSVerbindungenListe: TGPRSVerbList;
    GPRSTelegrammListe: TGPRSTelegrList;
    GPRSSocketLogFile: TCustomLogFile;
    bAnzeigeGPRSRohdaten: boolean;
    bDebugGPRSDatenProtokoll: boolean;
    bDebugGPRSStatistikProtokoll: boolean;
    bSendWieserMsg_NKD: boolean;
    bGPRS_freigeschaltet: boolean;
    dtGPRSServerStart: TDateTime;

    { IP-Rufentgegennahme: }
    RETCPServer_DSfG: TWIdTCPServer;  // 03.06.2020, WW
    FREClntVerbList: TREClntVerbList;

{$IFDEF SIMU}  // Nur für Simulationsmodus verwendet
    FSimuFileArch: TSimuFile;
    FSimuFileLogb: TSimuFile;
    FSimuDateTimeNextLogbRecord: TDateTime;
    FSimuDSfGArchivIntervall: integer;
    FSimuDSfGLogbuchIntervall_MaxRandom: integer;
    procedure Init_SimuFiles;
    procedure Add_SimuFiles;
{$ENDIF}

    procedure SetFensterTitel;
    procedure AddMemo (AMemo: TMemo; S: string);
    procedure CutMemo (AMemo: TMemo);

    procedure LoadResourceFiles;
    procedure InitLicence;
    function CheckLicence: boolean;
    function CheckLicenceAndInitAppFunctions: boolean;

    procedure WriteProgramLog (S: string; ALogType: TLogType);
    procedure WriteAbrufSocketErrLog (S: string);
    procedure WriteAbrufSocketConLog (S: string; ALogType: TLogType);
    procedure WriteHeartbeatFile;
    function GetSSLVersionName (IdSSLVersion: TIdSSLVersion): string;

    function CreateAndOpenAbrufTCPServer: boolean;
    procedure CloseAndFreeAbrufTCPServer;
    procedure AbrufTCPServerConnect(AContext: TIdContext);
    procedure AbrufTCPServerDisconnect(AContext: TIdContext);
    procedure AbrufTCPServerStatus(ASender: TObject;
      const AStatus: TIdStatus; const AStatusText: string);
    procedure IOHandlerSSLStatusInfo(Msg: string);
    procedure AbrufTCPServerExecute(AContext: TIdContext);
    procedure AbrufTCPServerException(AContext:TIdContext; AException: Exception);
    procedure AbrufTCPServerListenException(AThread: TIdListenerThread;
      AException: Exception);
    procedure ShowAbrufServerPortStatus (sSSLVersion: string);
    function AddAbrufKommando (ClientConnection: TIdTCPConnection;
      Kommando: string; StaKanalKonvDataList: TStaKanalKonvDataList;
      var iLinieNr: integer): boolean;
    function Init_ConnectionInaktivitaetTimeout: integer;
    function AbrufLinieNrToConnectionTag(iLinieNr: integer): integer;
    function ConnectionTagToAbrufLinieNr(iTag: integer;
      var iLinieNr: integer): boolean;

    procedure GPRSServerClientConnect(S: string; Socket: TCustomWinSocket);
    procedure GPRSServerClientDisconnect(S: string; Socket: TCustomWinSocket);
    procedure GPRSServerClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure GPRSServerClientError(S: string);
    procedure GPRSServerPortStatus (bActive: boolean; S: string);
    procedure GPRSData_Ausgabe (sRohdaten: string; RemoteAddress: string; RemotePort: integer);
    procedure WriteGPRSDataLog (S: string);
    procedure AktuGPRSVerbindungen;
    procedure SaveGPRSTelegrammNr (AGPRSTelegrNr: integer);
    procedure Benachrichtigen_NKD;

    function CreateAndOpenRETCPServer_DSfG: boolean;
    procedure CloseAndFreeRETCPServer_DSfG;
    procedure RETCPServer_DSfGConnect(AContext: TIdContext);
    procedure RETCPServer_DSfGException(AContext: TIdContext; AException: Exception);
    procedure RETCPServer_DSfGListenException(AThread: TIdListenerThread;
      AException: Exception);
    procedure LogREServerDSfG_PortStatus;
    procedure AddREClntVerbindung_DSfG (ClientConnection: TIdTCPConnection);

    procedure WMAbrufThreadDone (var AMessage : TMessage); message WM_AbrufThreadDone;
    procedure WMREClntThreadDone (var AMessage: TMessage); message WM_REClntThreadDone;
    procedure WMSendWieserMsg_NKD (var AMessage: TMessage); message WM_SendWieserMsg_NKD;
    procedure WMOpenGPRSServer (var AMessage: TMessage); message WM_OpenGPRSServer;
    procedure WMCloseGPRSServer (var AMessage: TMessage); message WM_CloseGPRSServer;
    procedure WMTASKBAREVENT (var message: TMessage); message WM_TASKBAREVENT;

    procedure DeleteOldTempFiles;
  public
    { Public-Deklarationen }
    function TaskBarAddIcon: boolean;
    procedure TaskBarRemoveIcon;
  end;

var
  FormAbrufServer: TFormAbrufServer;

implementation

{$R *.DFM}

uses
  IdSocketHandle, DateUtils;

resourcestring
  S_ApplicationTitle = 'Wico22 Abrufserver';

  SMsgClientConnecting = 'Client-Verbindung wird geöffnet...';
  SMsgClientName       = 'Name:';
  SMsgClientAddress    = 'Adresse:';
  SMsgClientPort       = 'Port:';
  SMsgClientConnected  = 'Client ist verbunden';

  SMsgClientDisconnecting = 'Client-Verbindung wird geschlossen...';
  SMsgClientDisconnected  = 'Client-Verbindung ist geschlossen:';


{-----------------------------------------------------}
procedure TFormAbrufServer.FormCreate(Sender: TObject);
{-----------------------------------------------------}
var
  S: string;

begin
{$IFDEF GAS-X}
  SvcMgr.Application.Title := S_ApplicationTitle + ' ' + SGasX;
  // für Gas-X-Version: unbegrenzte Laufzeit nicht gültig; 04.06.2014, WW
  FAllow_UnbegrenzteLaufzeit:=false;
{$ELSE}
  SvcMgr.Application.Title := S_ApplicationTitle;
  FAllow_UnbegrenzteLaufzeit:=true;
{$ENDIF}

  FFormDestroying:=false;
  FAllControlsClosed:=false;
  bSendWieserMsg_NKD:=false;
  SetFensterTitel;
  FHBTimerCount:=0;
  // Default: Es liegt keine Lizenz-Freischaltung bei Programminitialisierung vor
  FInitLizenzfreischaltung_OK:=false;
  FNextDeleteOldTempFiles:=Now;  // 05.10.2022, WW

  // Default LogDir: Exe-Verzeichnis, falls LogDir aus Wieser.ini nicht zu
  // ermitteln ist
  FLogDir:=ExtractFilePath (ParamStr(0));

  AbrufSrvIni:=TAbrufSrvIni.Create;

  { PathServer temporär für Log-Verzeichnis initialisieren; 21.04.2015, WW }
  PathServer:=TPathServer.Create (AbrufSrvIni.WieserIniFile, [WLogDir]);
  try
    try
      PathServer.Check;
      FLogDir:=PathServer.Pathname[WLogDir];
    except
      // Fehler beim Initialisieren des PathServers (Log-Verzeichnis):
      on E:Exception do begin
        pPortAbrufServer.Font.Color:=clred;
        S:=SMsgServerInactive + ' ' + E.Message;
        pPortAbrufServer.Caption:=' ' + S;          { Fehler-Ausgabe im Server-Fenster }
        S:=CMsgServerInactive + ' ' + E.Message;
        WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
        exit;
      end;
    end;
  finally
    FreeAndNil (PathServer);
  end;

  WriteProgramLog (CMsgSrvStarted + ' (Vs. ' + CVersion_WicomSrv + ')', lt_Info);

  { PathServer initialisieren: }
  PathServer:=TPathServer.Create (AbrufSrvIni.WieserIniFile,
                                  [WNetProgDir, WStammDir, WWorkDir, WNetWorkDir, WLogDir]);
  try
    PathServer.Check;
  except
    // Fehler beim Initialisieren des PathServers (alle für den Betrieb des
    // Programms benötigten Verzeichnisse):
    on E:Exception do begin
      pPortAbrufServer.Font.Color:=clred;
      S:=SMsgServerInactive + ' ' + E.Message;
      pPortAbrufServer.Caption:=' ' + S;          { Fehler-Ausgabe im Server-Fenster }
      S:=CMsgServerInactive + ' ' + E.Message;
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
      exit;
    end;
  end;

  { Ressourcendateien in Listen laden: }
  LoadResourceFiles;  // 06.08.2021, WW

  { Lizenz-Objekt anlegen: }
  InitLicence;

  { Lizenz-Freischaltung prüfen und Programmfunktionen initialisieren; 28.09.2020, WW }
  if CheckLicenceAndInitAppFunctions then  // 03.11.2020, WW
    Timer.Enabled:=true;
end;

{------------------------------------------------------}
procedure TFormAbrufServer.FormDestroy(Sender: TObject);
{------------------------------------------------------}
var
  Msg: TMessage;

begin
  FFormDestroying:=true;
  Timer.Enabled:=false;

  { zentrale Systemüberwachung beenden: 04.10.2007, WW }
  FreeAndNil (VerifyFlagObject);

  if GPRSServerSocketObj <> nil then   // 16.11.2011, WW
    if GPRSServerSocketObj.ServerActive then
      WMCloseGPRSServer (Msg);

  CloseAbrufControls (AbrufControls);   { Abruf-Kontrolle beenden und freigeben }

  // Flag setzen: Alle Abruf- und Rufentgegennahme-Controls (Threads) sind beendet,
  // jetzt können die zugehörigen TCP-Server beendet werden
  FAllControlsClosed:=true;  // 03.06.2020, WW

  // Abruf-TCP-Server schließen und freigeben:
  CloseAndFreeAbrufTCPServer;  // 03.06.2020, WW

  AbrufSocketErrLogFile.Free;
  AbrufSocketConLogFile.Free;

  GPRSServerSocketObj.Free;
  GPRSSocketLogFile.Free;
  GPRSTelegrammListe.Free;
  GPRSVerbindungenListe.Free;
  GPRSAusgabeDirList.Free;

  // Rufentgegennahme-TCP-Server für DSfG-Anrufe schließen und freigeben; 08.01.2018, WW
  CloseAndFreeRETCPServer_DSfG;

  FREClntVerbList.Free;

{$IFDEF SIMU}
  // 14.01.2015  // Simulationsmodus
  FreeAndNil (FSimuFileArch);
  FreeAndNil (FSimuFileLogb);
{$ENDIF}

  FResourceFilesList.Free;
  FSrvCfgIni.Free;
  PathServer.Free;
  AbrufSrvIni.Free;
  FWLizenz32.Free;

  WriteProgramLog (CMsgSrvStopped + ' (Vs. ' + CVersion_WicomSrv + ')', lt_Info);
end;

{------------------------------------------------------------------------------}
procedure TFormAbrufServer.FormClose(Sender: TObject; var Action: TCloseAction);
{------------------------------------------------------------------------------}
begin
  Action:=caHide;   // nur verstecken, nicht schließen
end;

{-----------------------------------------}
procedure TFormAbrufServer.SetFensterTitel;
{-----------------------------------------}
begin
  Caption:=SvcMgr.Application.Title + ' - Vs. ' + CVersion_Wicomsrv + ', ' +
           FormatDateTime(SFormatDate, (FileDateToDateTime (FileAge (Application.ExeName))));
end;

{------------------------------------------------}
procedure TFormAbrufServer.CutMemo (AMemo: TMemo);
{------------------------------------------------}
{ Anzahl der Einträge in einem Memo begrenzen (zur Sicherheit, wer weiß nach
  wievielen Einträgen es kracht !?) }
begin
  while AMemo.Lines.Count > 1000 do  { max. 1000 Zeilen darstellen, das reicht }
    AMemo.Lines.Delete (0);
end;

{-----------------------------------------------------------}
procedure TFormAbrufServer.AddMemo (AMemo: TMemo; S: string);
{-----------------------------------------------------------}
{ neuen Eintrag in einem Memo anhängen }
begin
  if not Visible then exit;  // Bugfix zur Vermeidung "Systemfehler. Code: 1400..."; 17.12.2020, WW

  AMemo.Lines.BeginUpdate;
  try
    AMemo.Lines.Add (S);
    CutMemo (AMemo);
  finally
    AMemo.Lines.EndUpdate;
  end;
  // ans Ende scrollen:
  AMemo.Perform (EM_LineScroll, 0, AMemo.Lines.Count-1);
end;

{------------------------ Initialisierungen -----------------------------------}

{-------------------------------------------}
procedure TFormAbrufServer.LoadResourceFiles;  // 06.08.2021, WW
{-------------------------------------------}
{ Ressourcedatei-Daten in Listen laden }
var
  sFileName: string;
  S: string;

begin
  if not Assigned (FResourceFilesList) then begin
    FResourceFilesList:=TResourceFilesList.Create;
    FResourceFilesList.Path:=PathServer.Pathname [WStammDir];

    // ParamMrg.dat
    if not FResourceFilesList.LoadList (rft_ParamMrg) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResParamMrg);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // ParamMeld.dat
    if not FResourceFilesList.LoadList (rft_ParamMeld) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResParamMeld);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // ParamEK.dat
    if not FResourceFilesList.LoadList (rft_ParamEK) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResParamEK);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // MrgTypGasX.dat
    if not FResourceFilesList.LoadList (rft_MrgTypGasX) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResMrgTypGasX);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // MrgKonv.dat
    if not FResourceFilesList.LoadList (rft_MrgKonv) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResMrgKonv);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // MrgDef.dat
    if not FResourceFilesList.LoadList (rft_MrgDef) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResMrgDef);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // MrgAbruf.dat
    if not FResourceFilesList.LoadList (rft_MrgAbruf) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResMrgAbruf);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // MrgInfo.dat
    if not FResourceFilesList.LoadList (rft_MrgInfo) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResMrgInfo);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // MrgKanalBit.dat
    if not FResourceFilesList.LoadList (rft_MrgKanalBit) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResMrgKanalBit);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // MeldNr.dat
    if not FResourceFilesList.LoadList (rft_MeldNr) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResMeldNr);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // WZ_SZ.dat
    if not FResourceFilesList.LoadList (rft_WZ_SZ) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResWZ_SZ);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // MBAbruf.dat; 11.02.2022, WW
    if not FResourceFilesList.LoadList (rft_MBAbruf) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResMBAbruf);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;

    // ParamMomVerb.dat; 14.04.2023, WW
    if not FResourceFilesList.LoadList (rft_ParamMomVerb) then begin
      sFileName:=ExpandFileName (PathServer.Pathname [WStammDir] + CResParamMomVerb);
      S:=Format (CMsgErrReadResourceFile, [sFileName]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;
  end;
end;

{-------------------------------------}
procedure TFormAbrufServer.InitLicence;
{-------------------------------------}
{ Lizenz-Objekt anlegen }
begin
  // Zuerst ein evtl. bereits angelegtes Lizenz-Objekt freigeben:
  FWLizenz32.Free;
  // Lizenz-Objekt neu anlegen:
  FWLizenz32:=TWLizenz32.Create (PathServer.Pathname [WNetProgDir] + C_Lizenz_Filename, true);
  // Ausgabe der Seriennummer in Programm-Logfile:
  WriteProgramLog ('Seriennummer: ' + FWLizenz32.Seriennummer, lt_Info);  // 11.07.2016, WW
end;

{----------------------------------------------}
function TFormAbrufServer.CheckLicence: boolean;
{----------------------------------------------}
{ Prüft, ob eine Lizenz-Freischaltung für PC, Programm und Programmlaufzeit
  vorliegt.
  Ergebnis: true, wenn Lizenz-Freischaltung vorliegt }
var
  ExeName: string;
  S: string;

begin
  Result:=false;

  ExeName:=ExtractFileName(Application.ExeName);
  if not FWLizenz32.GetLizenzExe (ExeName) then begin
    // Programm ist nicht lizenziert !
    pPortAbrufServer.Font.Color:=clred;
    S:=SMsgServerInactive + ' ' + SMsgLicExe;
    pPortAbrufServer.Caption:=' ' + S;          { Fehler-Ausgabe im Server-Fenster }
    S:=CMsgServerInactive + ' ' + CMsgLicExe;
    WriteProgramLog (S, lt_Warning);  { Fehler-Ausgabe in Programm-Logfile }
    exit;
  end;

  if not FWLizenz32.GetExeLaufzeit (ExeName, FAllow_UnbegrenzteLaufzeit) then begin
    // Programm-Lizenz ist abgelaufen !
    pPortAbrufServer.Font.Color:=clred;
    S:=SMsgServerInactive + ' ' + SMsgLicExeLaufzeit;
    pPortAbrufServer.Caption:=' ' + S;          { Fehler-Ausgabe im Server-Fenster }
    S:=CMsgServerInactive + ' ' + CMsgLicExeLaufzeit;
    WriteProgramLog (S, lt_Warning);  { Fehler-Ausgabe in Programm-Logfile }
    exit;
    { Anm.: Laufzeit-Lizenz wird zusätzlich auch vor jedem Abruf geprüft (in TAbrufThread.Kommando_Ausfuehren) }
  end;

  if not FWLizenz32.GetLizenzPC ('') then begin
    // Programm ist für Rechner nicht lizenziert !
    pPortAbrufServer.Font.Color:=clred;
    S:=SMsgServerInactive + ' ' + SMsgLicPC;
    pPortAbrufServer.Caption:=' ' + S;          { Fehler-Ausgabe im Server-Fenster }
    S:=CMsgServerInactive + ' ' + CMsgLicPC;
    WriteProgramLog (S, lt_Warning);  { Fehler-Ausgabe in Programm-Logfile }
    exit;
  end;

  Result:=true;  // OK, Lizenz-Freischaltung für PC, Programm und Programmlaufzeit liegt vor
end;

{-----------------------------------------------------------------}
function TFormAbrufServer.CheckLicenceAndInitAppFunctions: boolean;
{-----------------------------------------------------------------}
{ Prüft, ob eine Lizenz-Freischaltung für PC, Programm und Programmlaufzeit
  vorliegt. Wenn ja, werden die Programmfunktionen initialisiert.
  Ergebnis: False, wenn das Initialisieren der Programmfunktionen fehlgeschlagen ist }
var
  S: string;
  bRE_DSfG_freigeschaltet: boolean;
  Erg: integer;
  sLog: string;
  i: integer;
  DirName: string;

begin
  Result:=true;  // Default-Ergebnis, wenn keine Init-Lizenz-Freischaltung vorliegt

  // Lizenz prüfen: 
  if not CheckLicence then exit;

  // Flag setzen: Init-Lizenz-Freischaltung für PC, Programm und Programmlaufzeit liegt vor
  FInitLizenzfreischaltung_OK:=true;

  // Default-Ergebnis, wenn das nachfolgende Initialisieren der Programmfunktionen
  // fehlschlägt; 03.11.2020, WW
  Result:=false;

  // Prüfung, ob Programmfunktion 'GPRS' freigeschaltet ist:
  bGPRS_freigeschaltet:=FWLizenz32.ReadGlobaleFunktionFromLizenzFile (FNExt_GPRS);
  // Prüfung, ob Programmfunktion 'Rufentgegennahme-DSfG' freigeschaltet ist; 08.01.2018, WW
  bRE_DSfG_freigeschaltet:=FWLizenz32.ReadGlobaleFunktionFromLizenzFile (FNExt_RufentgegennahmeDSfG);

  GPRSAusgabeDirList:=TAusgabeDirList.Create (true);
  bAnzeigeGPRSRohdaten:=AbrufSrvIni.GPRS_AnzeigeRohdaten;
  bDebugGPRSDatenProtokoll:=AbrufSrvIni.GPRS_DebugDatenProtokoll;
  bDebugGPRSStatistikProtokoll:=AbrufSrvIni.GPRS_DebugStatistikProtokoll;

  AbrufSocketErrLogFile:=nil;
  AbrufSocketConLogFile:=nil;  // 03.11.2020, WW

  FSrvCfgIni:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
  FDebugServiceIO:=FSrvCfgIni.DebServiceIO;
  FDebugRohdaten:=FSrvCfgIni.DebRohdaten;  // 05.10.2022, WW
  FServerBasicAuthentication:=FSrvCfgIni.ServerBasicAuthentication;

  { Log-File für Fehler des GPRS-Serversocket: }
  if FDebugServiceIO then begin
    GPRSSocketLogFile:=nil;
    (* auskommentiert, GPRS nicht mehr unterstützt; 03.06.2020, WW
    S:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_GPRSSocketErr';
    GPRSSocketLogFile:=TCustomLogFile.Create (FLogDir, S, false); *)
  end
  else begin
    GPRSSocketLogFile:=nil;
  end;

{$IFDEF SIMU}
  // 14.01.2015  // Simulationsmodus
  Init_SimuFiles;  // Simulationsarchiv aufbauen
  FSimuDateTimeNextLogbRecord:=Now;
  Randomize;  // für Zeitpunkt des nächsten Simulations-Logbuch-Eintrags
{$ENDIF}

  { IP-Rufentgegennahme; 08.01.2018, WW }
  { Rufentgegennahme-TCP-Server für DSfG-Anrufe anlegen und öffnen:
    -> Nur wenn, DSfG-Rufentgegennahme per Lizenz freigeschaltet und die
       DSfG-TCP/IP-Rufentgegennahme in der Schnittstellen-Konfiguration aktiviert ist }
  if bRE_DSfG_freigeschaltet AND FSrvCfgIni.TCP_IP_RufAktiv [re_DSfG] then begin
    FREClntVerbList:=TREClntVerbList.Create;  // Liste für TCP/IP-Rufentgegennahme-Verbindungen anlegen

    if not CreateAndOpenRETCPServer_DSfG then exit;  // 03.06.2020, WW
  end else
    FREClntVerbList:=nil;  // Liste nicht erforderlich, wenn keine TCP/IP-Rufentgegennahme aktiv

  { Verwaltungsliste für GPRS-Verbindungen initialisieren: }
  GPRSVerbindungenListe:=TGPRSVerbList.Create ([af_Aktiv, af_IPAdresse, af_Port,
    af_AnzRecBytes, af_AnzSendBytes, af_AnzVerbOpen, af_AnzVerbClose,
    af_Kennung, af_GerTypName, af_LetzteAktion]);

  { Liste für GPRS-Telegramme initialisieren: }
  GPRSTelegrammListe:=TGPRSTelegrList.Create;
  GPRSTelegrammListe.Duplicates:=dupAccept;  // mehrfache, gleiche Einträge erlaubt
  GPRSTelegrammListe.TelegrNr:=AbrufSrvIni.GPRS_LetztTelegrammNr;  // letzte vergebene GPRS-Telegrammnummer einlesen
  GPRSTelegrammListe.CBSaveTelegrNr:=SaveGPRSTelegrammNr;  // Callback-Prozedur zum Speichern der GPRS-Telegrammnummer

  { Serversocket für GPRS-Verbindungen initialisieren: }
  GPRSServerSocketObj:=TGPRSServerSocketObj.Create;
  GPRSServerSocketObj.CBServerPortStatus:=GPRSServerPortStatus;
  GPRSServerSocketObj.CBClientConnect:=GPRSServerClientConnect;
  GPRSServerSocketObj.CBClientDisconnect:=GPRSServerClientDisconnect;
  GPRSServerSocketObj.CBClientRead:=GPRSServerClientRead;
  GPRSServerSocketObj.CBClientError:=GPRSServerClientError;

  if bGPRS_freigeschaltet then begin
    // GPRS-Ausgabe-Verzeichnisse aus INI lesen:
    Erg:=AbrufSrvIni.GetGPRS_AusgabeDirList (GPRSAusgabeDirList,
                                             PathServer.Pathname [WNetWorkDir]);
    if Erg < 0 then begin
      case Erg of
        -1: begin  // fehlender Verzeichnisname (leer)
              S:=SMsgEmptyGPRSDirName;
              sLog:=CMsgEmptyGPRSDirName;
            end;
        -2: begin  // gleicher Verzeichnisname mehrfach vorhanden
              S:=SMsgMultipleGPRSDirName;
              sLog:=CMsgMultipleGPRSDirName;
            end;
      else  // undefinierter Fehler in Verzeichnis-Einstellungen
        S:=SMsgUndefGPRSDirErr;
        sLog:=CMsgUndefGPRSDirErr;
      end;
      pPortGPRSServer.Font.Color:=clred;
      pPortGPRSServer.Caption:=S;          { Fehler-Ausgabe im Server-Fenster }
      WriteProgramLog (sLog, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
      exit;
    end;

    // Ausgabe-Verzeichnisse anlegen, wenn nicht vorhanden:
    for i:=0 to (GPRSAusgabeDirList.Count-1) do begin
      DirName:=TAusgabeDirDataObj (GPRSAusgabeDirList [i]).Daten.DirName;
      if not DirectoryExists (DirName) then begin
        if not ForceDirectories(DirName) then begin
          // Kurzzeitwerte-Pfad konnte nicht angelegt werden !
          pPortGPRSServer.Font.Color:=clred;
          S:=Format (SMsgErrDirCreate, [DirName]);
          pPortGPRSServer.Caption:=S;          { Fehler-Ausgabe im Server-Fenster }
          S:=Format (CMsgErrDirCreate, [DirName]);
          WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
          exit;
        end;
      end;
    end;
  end
  else begin  // Programmfunktion 'GPRS' ist nicht freigeschaltet
    pGPRSServer.Visible:=false;  // GPRS-Panel verstecken
    pAbrufServer.Align:=alClient;
  end;

  { Abruf-TCP-Server anlegen und öffnen: }
  if not CreateAndOpenAbrufTCPServer then exit;  // 03.06.2020, WW

  InitAbrufControls (AbrufControls);    { Abruf-Kontrolle initialisieren }

  { zentrale Systemüberwachung initialisieren: 04.10.2007, WW }
  VerifyFlagObject:=TVerifyFlags.Create;

  { Heartbeat-Datei schreiben, wenn definiert; 10.06.2014, WW }
  WriteHeartbeatFile;

  Result:=true;  // OK
end;

{------------------------------ Logdateien ------------------------------------}

{-------------------------------------------------------------------------}
procedure TFormAbrufServer.WriteProgramLog (S: string; ALogType: TLogType);
{-------------------------------------------------------------------------}
{ Eintrag in Programm-Logfile schreiben }
var
  sFileName: string;
  ProgLogFile: TCustomLogFile;

begin
  sFileName:=ChangeFileExt (ExtractFileName (Application.ExeName), '');
  ProgLogFile:=TCustomLogFile.Create (FLogDir, sFileName, false);
  try
    ProgLogFile.Write (S, true, ALogType);
  finally
    ProgLogFile.Free;
  end;
end;

{------------------------------------------------------------}
procedure TFormAbrufServer.WriteAbrufSocketErrLog (S: string);
{------------------------------------------------------------}
{ Eintrag in AbrufSocket-Error-Logfile schreiben }
var
  sFileName: string;

begin
  if FDebugServiceIO then begin
    if not Assigned (AbrufSocketErrLogFile) then begin
      sFileName:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_AbrufSocketErr';
      AbrufSocketErrLogFile:=TCustomLogFile.Create (FLogDir, sFileName, false);
    end;

    AbrufSocketErrLogFile.Write (S);
  end;
end;

{--------------------------------------------------------------------------------}
procedure TFormAbrufServer.WriteAbrufSocketConLog (S: string; ALogType: TLogType);
{--------------------------------------------------------------------------------}
{ Eintrag in AbrufSocket-Connection-Logfile schreiben }
var
  sFileName: string;

begin
  if FDebugServiceIO then begin
    if not Assigned (AbrufSocketConLogFile) then begin
      sFileName:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_AbrufSocketCon';
      AbrufSocketConLogFile:=TCustomLogFile.Create (FLogDir, sFileName, false);
    end;

    AbrufSocketConLogFile.Write (S, true, ALogType);
  end;
end;

{--------------------------- Heartbeatdatei -----------------------------------}

{--------------------------------------------}
procedure TFormAbrufServer.WriteHeartbeatFile;
{--------------------------------------------}
{ Schreibt Heartbeat-Datei, wenn konfiguriert }
var
  sHBFile: string;
  S: string;
  bOK : boolean;

begin
  sHBFile:=AbrufSrvIni.HeartbeatFile;
  if length (sHBFile) > 0 then begin  // wenn in INI konfiguriert
    bOK:=false;
    if ForceDirectories (ExtractFilePath (sHBFile)) then
      with TFileStreamExt.Create (sHBFile, fmCreate, bOK) do Free;

    if not bOK then begin   // Heartbeat-Datei konnte nicht geschrieben werden
      S:=Format (CMsgErrWriteHeartbeatFile, [sHBFile]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;
  end;
end;

{--------------------------- SSL-Versionsnamen --------------------------------}

{--------------------------------------------------------------------------------}
function TFormAbrufServer.GetSSLVersionName (IdSSLVersion: TIdSSLVersion): string;
{--------------------------------------------------------------------------------}
{ Liefert Name einer unterstützten SSL-Verschlüsselungsversion;
  Übergabe: SSL-Versionstyp
  Ergebnis: SSL-Versionsname }
begin
  case IdSSLVersion of
    sslvSSLv2:  Result:='SSL 2.0';
    sslvSSLv23: Result:='SSL 2.0/3.0';
    sslvSSLv3:  Result:='SSL 3.0';
    sslvTLSv1:  Result:='TLS 1.0';
  else
    Result:='';
  end;
end;

{---------------------------- Abruf-TCP-Server --------------------------------}

{-------------------------------------------------------------}
function TFormAbrufServer.CreateAndOpenAbrufTCPServer: boolean;
{-------------------------------------------------------------}
{ Abruf-TCP-Server anlegen und öffnen;
  Ergebnis: true, wenn erfolgreich }
var
  iSSLVersion: integer;
  sSSLVersion: string;
  sExeDir: string;
  S: string;
  sIPAdrPort: string;

begin
  Result:=false;

  AbrufTCPServer:=TWIdTCPServer.Create;  // 03.06.2020, WW
  AbrufTCPServer.OnConnect:=AbrufTCPServerConnect;
  AbrufTCPServer.OnDisconnect:=AbrufTCPServerDisconnect;
  AbrufTCPServer.OnExecute:=AbrufTCPServerExecute;
  AbrufTCPServer.OnException:=AbrufTCPServerException;
  AbrufTCPServer.OnListenException:=AbrufTCPServerListenException;
  AbrufTCPServer.OnStatus:=AbrufTCPServerStatus;

  // Binding für Endpunkt setzen; 05.10.2022, WW
  AbrufTCPServer.Bindings.Clear;
  with AbrufTCPServer.Bindings.Add do begin
    IP := FSrvCfgIni.ServerIPBind;  // IP-Adresse für Endpunkt aus Konfiguration lesen
    Port := FSrvCfgIni.ServerPortID;  // Port für Endpunkt aus Konfiguration lesen
    sIPAdrPort:=IP + ':' + IntToStr (Port);  // für Fehler-Ausgabe
  end;

  // Mit/ohne Verschlüsselung 03.06.2020, WW
  iSSLVersion:=FSrvCfgIni.ServerSSLVersion;  // SSL-Verschlüsselungsversion aus Konfiguration lesen
  AbrufSocketSSLLogFile:= nil;  // Default: Kein SSL-Logfile

  if (iSSLVersion >= integer (Low (TIdSSLVersion))) AND
     (iSSLVersion <= integer (High (TIdSSLVersion))) then begin  // wenn mit Verschlüsselung
    // SSL-Logfile anlegen
    if FDebugServiceIO then begin
      S:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_AbrufSocketSSL';   // 03.06.2020, WW
      AbrufSocketSSLLogFile:=TCustomLogFile.Create (FLogDir, S, false);
    end;

    sExeDir:=ExtractFilePath (ParamStr(0));

    AbrufServerIOHandlerSSL:=TIdServerIOHandlerSSLOpenSSL.Create;
    with AbrufServerIOHandlerSSL do begin
      SSLOptions.Method:=TIdSSLVersion (iSSLVersion);
      SSLOptions.CertFile:=sExeDir + 'cert_pub.pem';
      SSLOptions.KeyFile:=sExeDir + 'cert_key.pem';
      // RootCertFile wird nicht benötigt, Standardwert ist "leer"

      OnStatus:=AbrufTCPServerStatus;
      OnStatusInfo:=IOHandlerSSLStatusInfo;
//       OnGetPassword:=GetSSLKeyPassword;  auskommentiert, Passwort wird nicht verwendet

      sSSLVersion:=GetSSLVersionName(SSLOptions.Method);
    end;
    AbrufTCPServer.IOHandler:=AbrufServerIOHandlerSSL;
  end else
    sSSLVersion:='';

  try
    AbrufTCPServer.Active:=true;
  except
    on E:Exception do begin
      pPortAbrufServer.Font.Color:=clred;
      S:=SMsgServerInactive + ' ' + E.Message + ' ' + sIPAdrPort;  // 05.10.2022, WW
      pPortAbrufServer.Caption:=' ' + S;  { Fehler-Ausgabe im Server-Fenster }
      S:=CMsgServerInactive + ' ' + E.Message + ' ' + sIPAdrPort;  // 05.10.2022, WW
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
      exit;
    end;
  end;
  ShowAbrufServerPortStatus (sSSLVersion);  { Abrufserver-Port Anzeige }

  Result:=true;
end;

{----------------------------------------------------}
procedure TFormAbrufServer.CloseAndFreeAbrufTCPServer;
{----------------------------------------------------}
{ Abruf-TCP-Server schließen und freigeben }
begin
  if Assigned (AbrufTCPServer) then begin
    if AbrufTCPServer.Active then begin
      try  // 03.06.2020, WW
        AbrufTCPServer.Active:=false;
      except
        //
      end;

      ShowAbrufServerPortStatus ('');
    end;

    try  // 03.06.2020, WW
      AbrufTCPServer.Free;
    except
      //
    end;
  end;

  AbrufServerIOHandlerSSL.Free;  // 03.06.2020, WW

  AbrufSocketSSLLogFile.Free;  // 03.06.2020, WW
end;

(* auskommentiert, wird nicht verwendet
{------------------------------------------------------------------}
procedure TFormAbrufServer.GetSSLKeyPassword (var Password: String);
{------------------------------------------------------------------}
begin
  Password:='';  // 03.06.2020, WW
end; *)

{---------------------------------------------------------------------}
procedure TFormAbrufServer.AbrufTCPServerConnect(AContext: TIdContext);
{---------------------------------------------------------------------}
{ eine Abrufclient-Verbindung wurde geöffnet }
var
  sDateTime: string;
  sConnect: string;

begin
  // PassThrough-Flag löschen für verschlüsselte Client-Kommunikation; 03.06.2020, WW
  if (AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase) then
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough:=false;

  // Inaktivitäts-Timer für Client-Verbindung initialisieren; 03.11.2020, WW
  AContext.Connection.Tag:=Init_ConnectionInaktivitaetTimeout;

  // Log- und Fensterausgabe
  sDateTime:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9;  // 03.11.2020, WW
  sConnect:='Connect' + #9 + AContext.Binding.PeerIP + ':' +
    IntToStr(AContext.Binding.PeerPort);
  AddMemo (memoAbrufServer, sDateTime + sConnect);                         
  WriteAbrufSocketConLog (sConnect, lt_Info);  // 03.11.2020, WW
end;

{------------------------------------------------------------------------}
procedure TFormAbrufServer.AbrufTCPServerDisconnect(AContext: TIdContext);
{------------------------------------------------------------------------}
{ eine Abrufclient-Verbindung wurde geschlossen }
var
  sDateTime: string;
  sDisconnect: string;

begin
  // Log- und Fensterausgabe
  sDateTime:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9;  // 03.11.2020, WW
  sDisconnect:='Disconnect' + #9 + AContext.Binding.PeerIP + ':' +
    IntToStr(AContext.Binding.PeerPort);
  AddMemo (memoAbrufServer, sDateTime + sDisconnect);
  WriteAbrufSocketConLog (sDisconnect, lt_Info);  // 03.11.2020, WW
end;

{---------------------------------------------------------------}
procedure TFormAbrufServer.AbrufTCPServerStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);  // 03.06.2020, WW
{---------------------------------------------------------------}
var
  sDateTime: string;

begin
  // Log- und Fensterausgabe
  sDateTime:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9;  // 03.11.2020, WW
  AddMemo (memoAbrufServer, sDateTime + AStatusText);

  { SSL-Logfile-Protokollierung: }
  if AbrufSocketSSLLogFile <> nil then
    AbrufSocketSSLLogFile.Write (AStatusText);
end;

{-------------------------------------------------------------}
procedure TFormAbrufServer.IOHandlerSSLStatusInfo(Msg: string);  // 03.06.2020, WW
{-------------------------------------------------------------}
var
  sDateTime: string;

begin
  // Log- und Fensterausgabe
  sDateTime:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9;  // 03.11.2020, WW
  AddMemo (memoAbrufServer, sDateTime + Msg);

  { SSL-Logfile-Protokollierung: }
  if AbrufSocketSSLLogFile <> nil then
    AbrufSocketSSLLogFile.Write (Msg);
end;

{---------------------------------------------------------------------}
procedure TFormAbrufServer.AbrufTCPServerExecute(AContext: TIdContext);
{---------------------------------------------------------------------}
{ Task für eine Abrufclient-Verbindung wird ausgeführt }
const
  // Inaktivitäts-Timeout für Client-Verbindung
  C_TimeoutInaktiv_ms = 120000;  // 2 min fest

var
  RecBuf: string;
  sCmd: string;
  DecodeXMLREQ: TDecodeXMLREQ;
  CorrXMLRet: integer;
  StaKanalKonvDataList: TStaKanalKonvDataList;
  sErrInfo: string;
  iLinieNr: integer;
  iLastTickCount_msec: cardinal;
  iDiff_msec: cardinal;
  bInaktivTimeout: boolean;
  bConnectionImAbruf: boolean;
  iAbrufLinieNr: integer;
  sDateTime: string;
  sRead: string;
  sTimeout: string;

begin
  try
    Sleep (1);  // Prozessorauslastung niedrig halten

    // Prüfen, ob zur Connection bereits ein Abruf-Thread läuft; 03.11.2020, WW
    bConnectionImAbruf:=
      ConnectionTagToAbrufLinieNr(AContext.Connection.Tag, iAbrufLinieNr);

    // Prüfen, ob Zeichen im Eingangspuffer der Client-Verbindung enthalten sind
    if AContext.Connection.IOHandler.InputBufferIsEmpty then begin
      // Wenn zur Connection kein Abruf-Thread läuft, den Inaktivitäts-Timer der
      // Connection überwachen; 03.11.2020, WW:
      if not bConnectionImAbruf then begin
        iLastTickCount_msec:=AContext.Connection.Tag;
        iLastTickCount_msec:=iLastTickCount_msec * 1000;  // s -> ms

        iDiff_msec:=F_GetTickCountDiff(iLastTickCount_msec);
        bInaktivTimeout:=iDiff_msec > C_TimeoutInaktiv_ms;
      end
      else begin  // es läuft ein Abruf-Thread
        // Prüfen, ob der Abruf-Thread weiterhin läuft...
        if (iAbrufLinieNr >= Low (AbrufControls)) AND
           (iAbrufLinieNr <= High (AbrufControls)) then begin
          // Inaktivitäts-Timer für Client-Verbindung initialisieren, wenn der
          // Abruf-Thread nicht mehr läuft
          if not AbrufControls [iAbrufLinieNr].ThreadRunning then
            AContext.Connection.Tag:=Init_ConnectionInaktivitaetTimeout;
        end;
        bInaktivTimeout:=false;
      end;

      // Wenn beim Beenden ein "unproduktiver" Abruf-Client noch verbunden sein sollte,
      // dann Client-Verbindung schließen; 03.06.2020, WW
      // 03.11.2020, WW: Auch bei Inaktivität schließen
      if FAllControlsClosed OR bInaktivTimeout then begin
        AContext.Connection.Disconnect;
        if bInaktivTimeout then begin
          // Log- und Fensterausgabe; 03.11.2020, WW
          sDateTime:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9;
          sTimeout:='Timeout' + #9 + AContext.Binding.PeerIP + ':' +
            IntToStr(AContext.Binding.PeerPort) + #9 +
            'Client-Verbindung wird wegen Inaktivität geschlossen';
          AddMemo (memoAbrufServer, sDateTime + sTimeout);
          WriteAbrufSocketConLog (sTimeout, lt_Warning);  // 03.11.2020, WW
        end;
      end;

      exit;  // 03.06.2020, WW
    end;

    // Inaktivitäts-Timer für Client-Verbindung aktualisieren, wenn Zeichen
    // empfangen wurden und zur Connection kein Abruf-Thread läuft; 03.11.2020, WW
    if not bConnectionImAbruf then
      AContext.Connection.Tag:=Init_ConnectionInaktivitaetTimeout; 

    // Zeichen aus Eingangspuffer der Client-Verbindung lesen
    with AContext.Connection.IOHandler do begin
      RecBuf:=ReadString(InputBuffer.Size);  // 03.06.2020, WW

      // Fensterausgabe
      sDateTime:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9;  // 03.11.2020, WW
      sRead:='Read' + #9 + AContext.Binding.PeerIP + ':' +
        IntToStr(AContext.Binding.PeerPort) + #9 + RecBuf;
      AddMemo (memoAbrufServer, sDateTime + sRead);
    end;

    // Empfangsdaten im Data-Object des Contexts sammeln; 03.06.2020, WW
    if not Assigned (AContext.Data) then
      AContext.Data:=TStringStream.Create(RecBuf)
    else begin
      with TStringStream (AContext.Data) do begin
        Seek (0, soFromEnd);
        WriteString (RecBuf);
      end;
    end;

    // Wenn Datei-Datum der Schnittstellen-Konfiguration sich geändert hat,
    // Einstellungen neu lesen: 28.09.2020, WW
    //    - ServiceIO
    //    - Rohdaten
    //    - Server Basis-Authentifizierung
    if FSrvCfgIni.FileDateTimeChanged then begin
      WriteProgramLog (CMsgSrvCfgIniChanged_Reread, lt_Info);  { Logfile-Protokollierung; 28.09.2020, WW }

      FDebugServiceIO:=FSrvCfgIni.DebServiceIO;
      FDebugRohdaten:=FSrvCfgIni.DebRohdaten;  // 05.10.2022, WW
      FServerBasicAuthentication:=FSrvCfgIni.ServerBasicAuthentication;
    end;

    DecodeXMLREQ:=TDecodeXMLREQ.Create(FDebugServiceIO, PathServer.Pathname [WWorkDir]);
    try
      if DecodeXMLREQ.isCompleteXML (TStringStream (AContext.Data).DataString) then begin  // XML-Kommando komplett ?
        DecodeXMLREQ.SetRequest (TStringStream (AContext.Data).DataString);

        // Wenn Request des Clients vollständig empfangen wurde, Empfangsdaten-Puffer
        // wieder leer vorbelegen für nächsten Client-Receive:
        with TStringStream (AContext.Data) do begin  // 03.06.2020, WW
          Seek (0, soFromBeginning);
          WriteString ('');
        end;

        // XML-Kommando korrekt ?
        CorrXMLRet:=DecodeXMLREQ.isCorrectXML (FServerBasicAuthentication, sErrInfo);
        if CorrXMLRet = 0 then begin
          sCmd:=DecodeXMLREQ.GetCmdRequest;
          StaKanalKonvDataList:=DecodeXMLREQ.CreateRequestData_MRG_StaKanalKonvDataList;
          if AddAbrufKommando (AContext.Connection, sCmd, StaKanalKonvDataList, iLinieNr) then begin
            { Abruf-Liniennummer dem Tag des Connection-Objekts zuweisen; 03.11.2020, WW
              -> Connection darf solange der zur Abruf-Liniennummer gehörende
                 Abruf-Thread existiert, nicht wegen Inaktivität geschlossen werden ! }
            AContext.Connection.Tag:=AbrufLinieNrToConnectionTag(iLinieNr);
          end;
        end
        else begin
          { In AbrufSocket-Error-Logfile protokollieren: }
          WriteAbrufSocketErrLog ('XML-Kommando nicht korrekt: ' +
            IntToStr (CorrXMLRet) + ' (' + sErrInfo + ')');  // 07.04.2014, WW
        end;
      end;
    finally
      DecodeXMLREQ.Free;
    end;
  except
    on E:Exception do begin
      { Fehler-Ausgabe in Programm-Logfile; 03.11.2020, WW }
      WriteProgramLog ('TFormAbrufServer.AbrufTCPServerExecute: ' + E.Message, lt_Error);
      exit;
    end;
  end;
end;

{----------------------------------------------------------------------}
procedure TFormAbrufServer.AbrufTCPServerException(AContext: TIdContext;
  AException: Exception);
{----------------------------------------------------------------------}
{ es trat ein Fehler in einer Abrufclient-Verbindung auf }
begin
  { In AbrufSocket-Error-Logfile protokollieren: }
  WriteAbrufSocketErrLog ('Abruf-TCP-Server, Fehler bei Client-Verbindung: ' +
    AException.Message);
end;

{-------------------------------------------------------}
procedure TFormAbrufServer.AbrufTCPServerListenException(
  AThread: TIdListenerThread; AException: Exception);
{-------------------------------------------------------}
{ es trat ein Fehler beim Lauschen auf Abrufclient-Verbindungen auf }
begin
  { In AbrufSocket-Error-Logfile protokollieren: }
  WriteAbrufSocketErrLog ('Abruf-TCP-Server, Fehler beim Lauschen auf Client-Verbindungen: ' +
    AException.Message);
end;

{-------------------------------------------------------------------------}
procedure TFormAbrufServer.ShowAbrufServerPortStatus (sSSLVersion: string);
{-------------------------------------------------------------------------}
{ Status Abruf-Server-Port geöffnet/geschlossen anzeigen;
  Übergabe: SSL-Versionsname }
var
  S: string;
  sIPAdr: string;
  iPort: integer;

begin
  if AbrufTCPServer.Bindings.Count > 0 then begin  // 05.10.2022, WW
    sIPAdr:=AbrufTCPServer.Bindings [0].IP;
    iPort:=AbrufTCPServer.Bindings [0].Port;
  end
  else begin
    sIPAdr:='';
    iPort:=0;
  end;

  if AbrufTCPServer.Active then begin  { Port geöffnet }
    pPortAbrufServer.Font.Color:=clGreen;
    S:=Format (SMsgIPAdrPortOpened, [sIPAdr, iPort]);  // 05.10.2022, WW
    if sSSLVersion <> '' then
      S:=S + ' (' + sSSLVersion + ')';  // 03.06.2020, WW
    pPortAbrufServer.Caption:=' ' + S;          { Ausgabe im Server-Fenster }
    S:=Format (CMsgIPAdrPortOpened, [sIPAdr, iPort]);  // 05.10.2022, WW
    if sSSLVersion <> '' then
      S:=S + ' (' + sSSLVersion + ')';  // 03.06.2020, WW
    WriteProgramLog (S, lt_Info);  { Ausgabe in Programm-Logfile }
  end
  else begin  { Port nicht geöffnet }
    pPortAbrufServer.Font.Color:=clRed;
    S:=Format (SMsgIPAdrPortClosed, [sIPAdr, iPort]);  // 05.10.2022, WW
    pPortAbrufServer.Caption:=' ' + S;          { Ausgabe im Server-Fenster }
    S:=Format (CMsgIPAdrPortClosed, [sIPAdr, iPort]);  // 05.10.2022, WW
    WriteProgramLog (S, lt_Info);  { Ausgabe in Programm-Logfile }
  end;
end;

{------------------------------------------------------------------------------}
function TFormAbrufServer.AddAbrufKommando (ClientConnection: TIdTCPConnection;
  Kommando: string; StaKanalKonvDataList: TStaKanalKonvDataList;
  var iLinieNr: integer): boolean;
{------------------------------------------------------------------------------}
{ Kommando in Kommandoliste des zuständigen Abruf-Threads eintragen und Abruf-
  Thread starten, wenn noch nicht erfolgt
  -> bei unplausibler Schnittstellen-Angabe im Kommando kann das Kommando nicht
     ausgeführt werden und es wird ein Eintrag in das Abrufsocket-Error-Logfile
     geschrieben, falls ServiceIO-Logging aktiviert ist
  Rückgabe: Liniennummer des Abruf-Threads, dem das Kommando zugewiesen wurde
  Ergebnis: true, wenn Kommando einem Abruf-Thread zugewiesen wurde }
var
  S: string;
  SNr: integer;
  COMNr: integer;
  COMNr_Min: integer;
  COMNr_Max: integer;
{$IFNDEF GAS-X}
  sProzessID: string;
  iIPLinienNr_intern: integer;
{$ENDIF}

begin
  { Default: Kommando keinem Abruf-Thread zugewiesen }
  Result:=false;
  iLinieNr:=0;

  { Nummer der Schnittstelle aus Kommando lesen: }
  S:=GetAbrufKommandoSchnittstellenNr (Kommando);
  try
    SNr:=StrToInt (S);
  except
    { In AbrufSocket-Error-Logfile protokollieren: ungültige Schnittstellen-Nummer im Kommando }
    WriteAbrufSocketErrLog ('Ungültige Schnittstellen-Nummer im Kommando: >' + S + '<');
    FreeAndNil (StaKanalKonvDataList);  // Kanalliste freigeben, da Kommando nicht ausgeführt werden kann
    exit;
  end;

{$IFDEF GAS-X}     // 06.10.2010, am 05.11.2010 wieder aufgenommen
  { COM-Nr., welche der vom Gas-X-Client übergebenen logischen Schnittstelle zugeordnet
    ist, aus Konfiguration lesen: }
  if SNr > 0 then begin  { log. Schnittstelle }
    // SNr ab 51 werden als IP-Linien interpretiert; 06.12.2005 WW
//    if SNr <= 50 then          // 05.11.2010
      COMNr:=FSrvCfgIni.COMZuordnung [SNr]  // Umsetzung in COM-Nr.
//    else
//      COMNr:=SNr * (-1);  // Umsetzung in IP-Linien-Nr.
  end else  { IP-Abrufnummer }
    COMNr:=SNr;
{$ELSE}
  COMNr:=SNr;  // Standard: physikalische COM-Nr./IP-Linien-Nr. wird per Kommando an Abrufserver übergeben
{$ENDIF}

  { Prüfen, ob COM, an die das Kommando gerichtet ist, unterstützt wird: }
  COMNr_Min:=Low (AbrufControls);
  COMNr_Max:=High (AbrufControls);
  if not ((COMNr >= COMNr_Min) AND (COMNr <= COMNr_Max)) then begin
    { In AbrufSocket-Error-Logfile protokollieren: COMNr außerhalb des gültigen Bereichs }
    WriteAbrufSocketErrLog ('COMNr außerhalb des gültigen Bereichs ' +
                            IntToStr (COMNr_Min) + '..' + IntToStr (COMNr_Max)+ ': ' +
                            IntToStr (COMNr));
    FreeAndNil (StaKanalKonvDataList);  // Kanalliste freigeben, da Kommando nicht ausgeführt werden kann
    exit;
  end;

{$IFNDEF GAS-X}
  if COMNr < 0 then begin  // IP-Linien-Nr.
    if COMNr >= COffsetIPLinien_Ruf then begin  // nicht für IP-Rufentgegennahme-Linien; 08.01.2018, WW
      // Aus der im Kommando enthaltenen Kombination IP-Linien-Nr./Prozess-ID die
      // intern verwendete IP-Linien-Nr ermitteln (Multi-Client-Fähigkeit des WicomSrv);
      // 21.04.2015, WW
      sProzessID:=GetAbrufKommandoProzessID (Kommando);   // Prozess-ID
      if length (sProzessID) = 0 then begin
        { In AbrufSocket-Error-Logfile protokollieren: Leere Prozess-ID }
        WriteAbrufSocketErrLog ('Prozess-ID mit Länge 0 im Kommando');
        FreeAndNil (StaKanalKonvDataList);  // Kanalliste freigeben, da Kommando nicht ausgeführt werden kann
        exit;
      end;

      if not GetAbrufControls_IPLinienNr_intern (AbrufControls, COMNr, sProzessID,
                                                 iIPLinienNr_intern) then begin
        { In AbrufSocket-Error-Logfile protokollieren: Keine freie interne IP-Linien-Nr. }
        WriteAbrufSocketErrLog ('Keine freie interne IP-Linien-Nr. verfügbar zu Kommando-IP-Linien-Nr. ' +
                                IntToStr (COMNr));
        FreeAndNil (StaKanalKonvDataList);  // Kanalliste freigeben, da Kommando nicht ausgeführt werden kann
        exit;
      end;
      COMNr:=iIPLinienNr_intern;  // Abruf auf die interne IP-Linien-Nr. umlenken
    end;
  end;
{$ENDIF}

  { prüfen, ob Thread schon läuft: }
  Application.ProcessMessages;  // AbrufThreadDone-Message wirksam werden lassen !
  if not AbrufControls [COMNr].ThreadRunning then begin
     { Thread läuft nicht: starten }
     if COMNr = 0 then begin  { GPRS-Thread }
       (* auskommentiert, GPRS nicht mehr unterstützt; 03.06.2020, WW
       AbrufControls [COMNr].Thread:=TServerGPRSThread.CreateIt (COMNr,
         AbrufControls [COMNr].KommandoListe, GPRSServerSocketObj.ServerSocket,
         GPRSVerbindungenListe, GPRSTelegrammListe, Handle, GPRSAusgabeDirList); *)
     end
     else begin  { Abruf-Thread }
       AbrufControls [COMNr].Thread:=TServerAbrufThread.CreateIt (COMNr,
         AbrufControls [COMNr].COMNr_Kommando,
         AbrufControls [COMNr].KommandoListe, GPRSServerSocketObj.ServerSocket,
         GPRSVerbindungenListe, GPRSTelegrammListe, FREClntVerbList,
         FResourceFilesList, FWLizenz32.ExeList, Handle, FDebugServiceIO,
         GPRSData_Ausgabe);
       AbrufControls [COMNr].ThreadRunning:=true;
     end;
  end;

  { Kommando und Kanalliste zusammen mit Client-Socket zur Abarbeitung an
    Kommandoliste des Abruf-Thread übergeben: }
  AbrufControls [COMNr].KommandoListe.SetKommandoObj (ClientConnection, Kommando,
                                                      StaKanalKonvDataList);
  iLinieNr:=COMNr;  // 03.11.2020, WW
  Result:=true;
end;

{--------------------------------------------------------------------}
function TFormAbrufServer.Init_ConnectionInaktivitaetTimeout: integer;
{--------------------------------------------------------------------}
{ Liefert Initialisierungswert für Inaktivitäts-Timer einer Client-Verbindung }
begin
  Result:=GetTickCount() DIV 1000;  // Aktueller TickCount in Sekunden
end;

{--------------------------------------------------------------------------------}
function TFormAbrufServer.AbrufLinieNrToConnectionTag(iLinieNr: integer): integer;
{--------------------------------------------------------------------------------}
{ Wandelt eine Abruf-Liniennummer (COM oder IP) in eine Tag-Nummer für ein
  Client-Verbindung-Objekt;
  Übergabe: Liniennummer
  Ergebnis: Tag-Nummer }
begin
  if iLinieNr < 0 then  // IP-Liniennummer
    Result:=iLinieNr
  else  // COM-Liniennummer
    Result:=iLinieNr * (-1) - 1000;  // in den negativen Bereich < -1000 wandeln (-1001, -1002...)
end;

{------------------------------------------------------------------}
function TFormAbrufServer.ConnectionTagToAbrufLinieNr(iTag: integer;
  var iLinieNr: integer): boolean;
{------------------------------------------------------------------}
{ Wandelt eine Tag-Nummer eines Client-Verbindung-Objekts in eine Abruf-
  Liniennummer (COM oder IP);
  Übergabe: Tag-Nummer
  Rückgabe: Liniennummer
  Ergebnis: true, wenn Tag-Nummer eine Liniennummer enthält (< 0) }
begin
  if iTag < 0 then begin  // Tag enthält eine Liniennummer
    if iTag < -1000 then  // COM-Liniennummer
      iLinieNr:=Abs(iTag) - 1000  // aus dem negativen Bereich < -1000 zurückwandeln
    else  // IP-Liniennummer
      iLinieNr:=iTag;
    Result:=true;
  end
  else begin
    iLinieNr:=0;
    Result:=false;
  end;
end;


{----------------------- GPRS-Serversocket ------------------------------------}

// KEINE Application.ProcessMessages-Aufrufe in den Socket-Ereignisroutinen !
// Der Ablauf der Ereignisroutine wird sonst evtl. durch ein zeitgleiches neues
// Ereignis unterbrochen und erst zu einem späteren Zeitpunkt fortgeführt !                             
// Probleme bislang keine bekannt, aber die Reihenfolge der Programmschritte
// ändert sich !

{-----------------------------------------------------------}
procedure TFormAbrufServer.GPRSServerClientConnect(S: string;
  Socket: TCustomWinSocket);
{-----------------------------------------------------------}
{ eine GPRS-Verbindung wurde geöffnet }
begin
(* auskommentiert, GPRS nicht mehr unterstützt; 03.06.2020, WW
  WriteGPRSDataLog (S);
  AddMemo (memoGPRSServer, S);

  // Verbindungsliste aktualisieren
  GPRSVerbindungenListe.IncrementVerbCount (true, Socket.RemoteAddress, Socket.RemotePort); *)
end;

{--------------------------------------------------------------}
procedure TFormAbrufServer.GPRSServerClientDisconnect(S: string;
  Socket: TCustomWinSocket);
{--------------------------------------------------------------}
{ eine GPRS-Verbindung wurde geschlossen }
begin
(* auskommentiert, GPRS nicht mehr unterstützt; 03.06.2020, WW
  WriteGPRSDataLog (S);
  AddMemo (memoGPRSServer, S);

  // Verbindungsliste aktualisieren
  GPRSVerbindungenListe.IncrementVerbCount (false, Socket.RemoteAddress, Socket.RemotePort); *)
end;

{-------------------------------------------------------------------}
procedure TFormAbrufServer.GPRSServerClientRead(Sender: TObject;
  Socket: TCustomWinSocket);
{-------------------------------------------------------------------}
{ es wurden Zeichen aus einer GPRS-Verbindung empfangen }
(* var
  GPRSTelegramme: string;
  RecBuf: string;
  AnzTelegr: integer;
  OneMRGTelegramm: string;
  iGerTyp: integer; *)

begin
(* auskommentiert, GPRS nicht mehr unterstützt; 03.06.2020, WW
  // Kein Empfang von Push-Daten, wenn GPRS-Pull-Betrieb läuft:
  if GPRSVerbindungenListe.GetPull_Aktiv (Socket.RemoteAddress) then exit;

  iGerTyp:=GPRSVerbindungenListe.GetGeraetetypNr (Socket.RemoteAddress);

  RecBuf:=Socket.ReceiveText;
  GPRSTelegramme:=RecBuf;
  while length (RecBuf) > 0 do begin  // bis alle Zeichen empfangen wurden
    GPRSData_Ausgabe (RecBuf, Socket.RemoteAddress, Socket.RemotePort);

    AnzTelegr:=0;
    if iGerTyp < 0 then begin  // Gerätetyp unbekannt
      iGerTyp:=Identifiziere_Geraetetyp (GPRSTelegramme);  // Gerätetyp identifizieren
      if iGerTyp >= 0 then  // Gerätetyp in GPRS-Verbindungsliste updaten
        GPRSVerbindungenListe.UpdateGeraetetyp (Socket.RemoteAddress,
                                                GetGerTypName (iGerTyp), iGerTyp);
    end;

    if iGerTyp >= 0 then begin  // Gerätetyp bekannt
      if FIsCompletePushTelegramm (GPRSTelegramme, iGerTyp) then begin
        { GPRSTelegramme kann eines oder mehrere MRG-Datentelegramme enthalten !
          Alle MRG-Telegramme einzeln abarbeiten: }
        while length (GPRSTelegramme) > 0 do begin
          inc (AnzTelegr);  // Anzahl der enthaltenen MRG-Telegramme hochzählen
          OneMRGTelegramm:=FCutOnePushTelegramm (GPRSTelegramme, iGerTyp);  { ein MRG-Telegramm ausschneiden }
          { Telegramm in Telegrammliste eintragen: }
          GPRSTelegrammListe.SetTelegramm (OneMRGTelegramm, iGerTyp,
                                           Socket.RemoteAddress, Socket);
        end;  { while length (MRGTelegramme) > 0 }

        GPRSTelegramme:='';
      end;
    end;

    // Verbindungsliste aktualisieren
    GPRSVerbindungenListe.IncrementRecCounts (Socket.RemoteAddress, AnzTelegr, 1, length (RecBuf));

    RecBuf:=Socket.ReceiveText;
    GPRSTelegramme:=GPRSTelegramme + RecBuf;
  end; *)
end;

{----------------------------------------------------------}
procedure TFormAbrufServer.GPRSServerClientError(S: string);
{----------------------------------------------------------}
{ es trat ein GPRS-Serversocket-Fehler auf }
begin
  { Socket-Logfile-Protokollierung: }
  if GPRSSocketLogFile <> nil then
    GPRSSocketLogFile.Write (S);

  AddMemo (memoGPRSServer, S);
end;

{----------------------------------------------------------------------------}
procedure TFormAbrufServer.GPRSServerPortStatus (bActive: boolean; S: string);
{----------------------------------------------------------------------------}
{ Status GPRS-Server-Port geöffnet/geschlossen anzeigen }
begin
  if bActive then
    pPortGPRSServer.Font.Color:=clgreen
  else
    pPortGPRSServer.Font.Color:=clred;
  pPortGPRSServer.Caption:=S;          { Ausgabe im Server-Fenster }
  WriteProgramLog (S, lt_Info);  { Ausgabe in Programm-Logfile }
end;

{------------------------------------------------------------------------------------}
procedure TFormAbrufServer.GPRSData_Ausgabe (sRohdaten: string; RemoteAddress: string;
  RemotePort: integer);
{------------------------------------------------------------------------------------}
{ Ausgabe empfangener GPRS-Daten in Memo und GPRS-Datenlogfile }
var
  sData: string;
  S: string;

begin
  sData:=SonderzeichenString (sRohdaten);
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 +
     'Read' + #9 + RemoteAddress + ':' + IntToStr(RemotePort) + #9 +
      IntToStr (length (sRohdaten)) + #9 + 'Bytes';
  WriteGPRSDataLog (S + #9 + sData);  // Datenprotokolldatei immer mit Rohdaten
  if bAnzeigeGPRSRohdaten then
    S:=S + #9 + sData;
  AddMemo (memoGPRSServer, S);
end;

{------------------------------------------------------}
procedure TFormAbrufServer.WriteGPRSDataLog (S: string);
{------------------------------------------------------}
{ Eintrag in GPRS-Daten-Logdatei schreiben (IP-Ereignisse/Empfangsdaten);
  Übergabe: Logdatei-Eintrag }
var
  FName: string;
begin
  if bDebugGPRSDatenProtokoll then begin
    FName:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_GPRSData';
    WriteDebugLog (FLogDir, FName, S, false);
    // Data-Logfile ohne Header für Excel-Auswertung; 13.06.2007, WW
  end;
end;

{----------------------------------------------}
procedure TFormAbrufServer.AktuGPRSVerbindungen;
{----------------------------------------------}
{ aktuelle Info zu GPRS-Client-Verbindungen anzeigen/schreiben }
var
  i: integer;
  DirName: string;
  FName: string;

begin
  // Anzeige für aktuelle Verbindungen aktualisieren:
  GPRSVerbindungenListe.VerbindungenAnzeigen (lvGPRSVerbindungen, pGPRSVerbAktiv,
    pGPRSVerbInaktiv);

  // Verbindungsinfo-Übergabedatei für Anzeige-Client schreiben:
  for i:=0 to (GPRSAusgabeDirList.Count-1) do begin
    if TAusgabeDirDataObj (GPRSAusgabeDirList [i]).Daten.ClientVerbindungen then begin
      DirName:=TAusgabeDirDataObj (GPRSAusgabeDirList [i]).Daten.DirName;
      FName:=GetFilename_GPRS_Verbindungen (DirName);
      GPRSVerbindungenListe.WriteClientInfoFile (FName);
    end;
  end;
end; 

{----------------------------------------------------------------------}
procedure TFormAbrufServer.SaveGPRSTelegrammNr (AGPRSTelegrNr: integer);
{----------------------------------------------------------------------}
{ GPRS-Telegramm-Nummer abspeichern }
begin
  AbrufSrvIni.GPRS_LetztTelegrammNr:=AGPRSTelegrNr;
end;

{-------------------- DSfG-Rufentgegennahme-TCP-Server ------------------------}

{---------------------------------------------------------------}
function TFormAbrufServer.CreateAndOpenRETCPServer_DSfG: boolean;
{---------------------------------------------------------------}
{ Rufentgegennahme-TCP-Server für DSfG-Anrufe anlegen und öffnen;
  Ergebnis: true, wenn erfolgreich }
begin
  Result:=false;

  RETCPServer_DSfG:=TWIdTCPServer.Create;  // 03.06.2020, WW
  RETCPServer_DSfG.OnConnect:=RETCPServer_DSfGConnect;
  RETCPServer_DSfG.OnException:=RETCPServer_DSfGException;
  RETCPServer_DSfG.OnListenException:=RETCPServer_DSfGListenException;

  RETCPServer_DSfG.DefaultPort:=FSrvCfgIni.ServerRufPort_DSfG;  { DSfG-RE-Port aus Konfiguration lesen }
  try
    RETCPServer_DSfG.Active:=true;
  except
    on E:Exception do begin
      WriteProgramLog (CMsgREServerInactive + ' ' + E.Message, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
      exit;
    end;
  end;
  LogREServerDSfG_PortStatus;  { DSfG-Rufentgegegennahme-Port-Status loggen }

  Result:=true;
end;

{------------------------------------------------------}
procedure TFormAbrufServer.CloseAndFreeRETCPServer_DSfG;
{------------------------------------------------------}
{ Rufentgegennahme-TCP-Server für DSfG-Anrufe schließen und freigeben }
begin
  if Assigned (RETCPServer_DSfG) then begin
    if RETCPServer_DSfG.Active then begin
      try  // 03.06.2020, WW
        RETCPServer_DSfG.Active:=false;
      except
        //
      end;

      LogREServerDSfG_PortStatus;
    end;

    try  // 03.06.2020, WW
      RETCPServer_DSfG.Free;
    except
      //
    end;
  end;
end;

{-----------------------------------------------------------------------}
procedure TFormAbrufServer.RETCPServer_DSfGConnect(AContext: TIdContext);
{-----------------------------------------------------------------------}
{ eine DSfG-Rufentgegennahmeclient-Verbindung wurde geöffnet }
begin
  // Ankommende Verbindung in TCP/IP-Rufentgegennahme-Liste als DSfG-Client-
  // Verbindung eintragen:
  AddREClntVerbindung_DSfG (AContext.Connection); // 08.01.2018, WW
end;

{------------------------------------------------------------------------}
procedure TFormAbrufServer.RETCPServer_DSfGException(AContext: TIdContext;
  AException: Exception);
{------------------------------------------------------------------------}
{ es trat ein Fehler in einer DSfG-Rufentgegennahmeclient-Verbindung auf }
begin
  { In AbrufSocket-Error-Logfile protokollieren: }
  WriteAbrufSocketErrLog ('DSfG-Rufentgegennahme-TCP-Server, ' +
    'Fehler bei Client-Verbindung: ' + AException.Message);
end;

{---------------------------------------------------------}
procedure TFormAbrufServer.RETCPServer_DSfGListenException(
  AThread: TIdListenerThread; AException: Exception);
{---------------------------------------------------------}
{ es trat ein Fehler beim Lauschen auf DSfG-Rufentgegennahmeclient-Verbindungen auf }
begin
  { In AbrufSocket-Error-Logfile protokollieren: }
  WriteAbrufSocketErrLog ('DSfG-Rufentgegennahme-TCP-Server, ' +
    'Fehler beim Lauschen auf Client-Verbindungen: ' + AException.Message);
end;

{---------------------------------------------------------------------------------------}
procedure TFormAbrufServer.AddREClntVerbindung_DSfG (ClientConnection: TIdTCPConnection);
{---------------------------------------------------------------------------------------}
begin
  Application.ProcessMessages;  // REClntThreadDone-Message wirksam werden lassen (falls überhaupt notwendig...)
  if Assigned (FREClntVerbList) then
    FREClntVerbList.AddNeueVerbindung (ClientConnection, Handle);
end;

{----------------------------------------------------}
procedure TFormAbrufServer.LogREServerDSfG_PortStatus;
{----------------------------------------------------}
{ Status DSfG-Rufentgegennahme-Server-Port geöffnet/geschlossen protokollieren }
var
  S: string;

begin
  if RETCPServer_DSfG.Active then begin  { Port geöffnet }
    S:=Format (CMsgREDSfG_IPAdrPortOpened, ['0.0.0.0', RETCPServer_DSfG.DefaultPort]);  // 05.10.2022, WW
    WriteProgramLog (S, lt_Info);  { Ausgabe in Programm-Logfile }
  end
  else begin  { Port nicht geöffnet }
    S:=Format (CMsgREDSfG_IPAdrPortClosed, ['0.0.0.0', RETCPServer_DSfG.DefaultPort]);  // 05.10.2022, WW
    WriteProgramLog (S, lt_Info);  { Ausgabe in Programm-Logfile }
  end;
end;      

{------------------------ Wieser Benachrichtigung -----------------------------}

{---------------------------------------------}
procedure TFormAbrufServer.Benachrichtigen_NKD;
{---------------------------------------------}
{ Benachrichtigung "Kurzzeitwerte vorhanden" versenden }
var
  WMsgRegList: TObjectList;
  WMessageReg: TWMessageReg;
  bDebug: boolean;

begin
  bDebug:=true;  // Flag für Debugging der Wieser-Benachrichtigung

  WMsgRegList:=TObjectList.Create (true);
  try
    WMessageReg:=TWMessageReg.Create (PathServer.Pathname [WNetProgDir]);
    try
      { alle für "neue Kurzzeitdaten vorhanden" registrierten Nachrichten-Empfänger
        in Liste laden: }
      WMessageReg.GetMsgRegistrationList (wmt_NeueKurzzeitDaten, WMsgRegList);
      { Benachrichtigung "neue Kurzzeitdaten vorhanden": }
      SendWieserMsg_NeueKurzzeitDaten (WMsgRegList,
        PathServer.Pathname [WNetProgDir], bDebug, PathServer.Pathname [WLogDir]);
    finally
      WMessageReg.Free;
    end;
  finally
    WMsgRegList.Free;
  end;
end;

{------------------------------ Timer-Methode ---------------------------------}

{-----------------------------------------------------}
procedure TFormAbrufServer.TimerTimer(Sender: TObject);
{-----------------------------------------------------}
{ - Startet u.a. Abruf-Threads nach;
    -> Notwendig, da Client-Kommandos eintreffen können, wenn ein Abruf-Thread gerade
       dabei ist, sich zu beenden. In diesem Fall wird zwar das Kommando in die
       entsprechende Kommandoliste eingetragen, aber der Thread kann das Kommando
       nicht mehr abarbeiten !
  - Wird auch für alle übrigen zyklisch aufzurufenden Funktionen verwendet }
var
  COMNr: integer;
  bStop: boolean;
  bNewLicence: boolean;

begin
  bStop:=false;
  Timer.Enabled:=false;
  try
    try
{$IFDEF GAS-X}
      // Nur GAS-X-Version: Bei geänderter Lizenzdatei diese automatisch neu lesen; 31.01.2023, WW
      bNewLicence:=FWLizenz32.FileDateTimeChanged;  // Datum der Lizenzdatei hat sich geändert
      if bNewLicence then begin
        // Logfile-Protokollierung
        WriteProgramLog(CMsgLicenceChanged_Reread, lt_Info);

        // Lizenz-Objekt neu anlegen
        InitLicence;
      end;
{$ELSE}
      bNewLicence:=false;
{$ENDIF}

      if FInitLizenzfreischaltung_OK then begin  // wenn Init-Lizenzfreischaltung erfolgt ist; 28.09.2020, WW
        // Abruf-Threads ggf. nachstarten
        for COMNr:=Low (AbrufControls) to High (AbrufControls) do begin
          if (not AbrufControls [COMNr].ThreadRunning) AND
             (AbrufControls [COMNr].KommandoListe.GetCount > 0) then begin
             { Thread läuft nicht, aber Kommando ist vorhanden: nachstarten }
             if COMNr = 0 then begin  { GPRS-Thread }
               (* auskommentiert, GPRS nicht mehr unterstützt; 03.06.2020, WW
               AbrufControls [COMNr].Thread:=TServerGPRSThread.CreateIt (COMNr,
                 AbrufControls [COMNr].KommandoListe, GPRSServerSocketObj.ServerSocket,
                 GPRSVerbindungenListe, GPRSTelegrammListe, Handle, GPRSAusgabeDirList); *)
             end
             else begin  { Abruf-Thread }
               AbrufControls [COMNr].Thread:=TServerAbrufThread.CreateIt (COMNr,
                 AbrufControls [COMNr].COMNr_Kommando,
                 AbrufControls [COMNr].KommandoListe, GPRSServerSocketObj.ServerSocket,
                 GPRSVerbindungenListe, GPRSTelegrammListe, FREClntVerbList,
                 FResourceFilesList, FWLizenz32.ExeList, Handle, FDebugServiceIO,
                 GPRSData_Ausgabe);
               AbrufControls [COMNr].ThreadRunning := true;
             end
          end;
        end;

        // Anzeige der GPRS-Verbindungen aktualisieren, wenn erforderlich:
        if GPRSVerbindungenListe.VerbData_Changed then begin
          GPRSVerbindungenListe.VerbData_Changed:=false;
          AktuGPRSVerbindungen;
        end;

        // Wieser-Benachrichtigung für neue Kurzzeitwerte verschicken, wenn erforderlich;
        if bSendWieserMsg_NKD then begin
          Benachrichtigen_NKD;
          bSendWieserMsg_NKD:=false;
        end;

        // Prüfen, ob TCP/IP-Rufentgegennahme-Verbindungen abgearbeitet wurden und
        // aus Liste gelöscht werden können; 08.01.2018, WW
        if Assigned (FREClntVerbList) then
          FREClntVerbList.Aufraeumen;

        // Heartbeat-Datei schreiben, wenn definiert (ca. alle 30 s):
        inc (FHBTimerCount);
        if (FHBTimerCount >= 30) then begin
          FHBTimerCount:=0;
          WriteHeartbeatFile;  // 10.06.2014, WW
        end;

        // Wenn der Zeitpunkt zum Löschen veralteter Temporär-Dateien erreicht
        // ist, dann Löschen ausführen: }
        if (FNextDeleteOldTempFiles < Now) then begin
          DeleteOldTempFiles;  // 05.10.2022, WW
          // Das nächste Mal wieder in 1 Stunde:
          FNextDeleteOldTempFiles:=IncHour (FNextDeleteOldTempFiles);
        end;

{$IFDEF SIMU}
        // Simulationsarchiv fortschreiben: 14.01.2015, WW
        Add_SimuFiles;
{$ENDIF}
      end
      else begin  // Init-Lizenzfreischaltung ist noch nicht erfolgt
        if bNewLicence then begin
          // Lizenz-Freischaltung erneut prüfen und Programmfunktionen
          // initialisieren:
          if not CheckLicenceAndInitAppFunctions then
            bStop:=true;  // Programmfunktionen konnten nicht initialisiert werden; 03.11.2020, WW
        end;
      end;
    except
      on E:Exception do begin
        { Fehler-Ausgabe in Programm-Logfile; 03.11.2020, WW }
        WriteProgramLog ('TFormAbrufServer.TimerTimer: ' + E.Message, lt_Error);
        exit;
      end;
    end;
  finally
    if (not FFormDestroying) AND (not bStop) then  // 03.11.2020, WW
      Timer.Enabled:=true;
  end;
end;

{------------------------- Message-Behandlungsmethoden ------------------------}

{--------------------------------------------------------------------}
procedure TFormAbrufServer.WMAbrufThreadDone (var AMessage: TMessage);
{--------------------------------------------------------------------}
{ Message-Behandlungsroutine wird aufgerufen, wenn sich ein Abruf-Thread beendet hat
  -> AMessage.WParam enthält die COM-Nr. des Thread }
var
  COMNr: longint;
begin
  COMNr:=AMessage.WParam;

  if (COMNr >= Low (AbrufControls)) AND (COMNr <= High (AbrufControls)) then begin
    AbrufControls [COMNr].ThreadRunning:=false;
    // 21.04.2015, WW
    AbrufControls [COMNr].ProzessID_Kommando:='';
    AbrufControls [COMNr].COMNr_Kommando:=COMNr;  // Default: Schnittstellennummer aus Kommando gleich interner
  end;
end;

{---------------------------------------------------------------------}
procedure TFormAbrufServer.WMREClntThreadDone (var AMessage: TMessage); // 08.01.2018, WW
{---------------------------------------------------------------------}
{ Message-Behandlungsroutine wird aufgerufen, wenn sich ein Rufentgegennahme-Thread beendet hat
  -> AMessage.WParam enthält den 1-basierten Thread-Index des RE-Verbindungslisteneintrags }
var
  iIndex: longint;
  S: string;

begin
  iIndex:=AMessage.WParam;

  if Assigned (FREClntVerbList) then begin
    if not FREClntVerbList.VerbindungAbschliessen (iIndex - 1) then begin
      S:=Format (CMsgErrREClntThreadIndexNotFound, [iIndex]);
      WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    end;
  end;
end;

{----------------------------------------------------------------------}
procedure TFormAbrufServer.WMSendWieserMsg_NKD (var AMessage: TMessage);
{----------------------------------------------------------------------}
{ Message-Behandlungsroutine wird aufgerufen, wenn Wieser-Benachrichtigung für
  neue Kurzzeitwerte verschickt werden soll (GPRS-Thread) }
begin
  bSendWieserMsg_NKD:=true;
end;

{-------------------------------------------------------------------}
procedure TFormAbrufServer.WMOpenGPRSServer (var AMessage: TMessage);
{-------------------------------------------------------------------}
{ Message-Behandlungsroutine wird aufgerufen, wenn GPRS-Serversocket geöffnet werden soll }
var
  S: string;
  ErrMsg: string;

begin
  if not bGPRS_freigeschaltet then exit;

  { GPRS-Serversocket nicht öffnen, Funktion nicht mehr unterstützt; 03.06.2020, WW }
  S:=CMsgGPRSServerInactive + ' Funktion wird nicht mehr unterstützt';
  WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
  exit;  // 03.06.2020, WW

  { GPRS-Serversocket öffnen: }
  if not GPRSServerSocketObj.OpenServer (FSrvCfgIni.ServerGPRSPort, ErrMsg) then begin
    S:=CMsgGPRSServerInactive + ' ' + ErrMsg;
    WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
    exit;
  end;

  dtGPRSServerStart:=Now;  // für Statistik

  { Start-Eintrag in GPRS-Daten-Logfile schreiben: }
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 + 'Start';
  WriteGPRSDataLog (S);
end;

{--------------------------------------------------------------------}
procedure TFormAbrufServer.WMCloseGPRSServer (var AMessage: TMessage);
{--------------------------------------------------------------------}
{ Message-Behandlungsroutine wird aufgerufen, wenn GPRS-Serversocket geschlossen werden soll }
var
  S: string;
  dtGPRSServerLaufzeit: TDateTime;
  d: integer;
  h, m, sec, ms: word;

begin
  { GPRS-Serversocket schließen: }
  GPRSServerSocketObj.CloseServer;

  { Stop-Eintrag in GPRSDaten-Logfile schreiben: }
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 + 'Stop';
  WriteGPRSDataLog (S);

  { Statistikdaten der einzelnen Verbindungen in Statistik-Ascii-Datei schreiben: }
  if bDebugGPRSStatistikProtokoll then begin
    // Laufzeit seit GPRS-Server geöffnet wurde:
    dtGPRSServerLaufzeit:=Now - dtGPRSServerStart;
    d:=Trunc (dtGPRSServerLaufzeit);  // Tage
    DecodeTime (dtGPRSServerLaufzeit, h, m, sec, ms);
    S:=Format ('%d t  %d h  %d min  %d s', [d, h, m, sec]);

    GPRSVerbindungenListe.WriteStatistikFile (dtGPRSServerStart, S);
  end;
end;

{----------------------------------------------------------------}
procedure TFormAbrufServer.WMTASKBAREVENT (var message: TMessage);
{----------------------------------------------------------------}
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

{--------------------- Popupmenü-Methoden -------------------------------------}

{---------------------------------------------------------}
procedure TFormAbrufServer.mAnzeigenClick(Sender: TObject);
{---------------------------------------------------------}
begin
  Show;
  Application.BringToFront;
end;

{------------------------------------------------------}
procedure TFormAbrufServer.mHilfeClick(Sender: TObject);
{------------------------------------------------------}
begin
  Application.HelpFile:=ChangeFileExt (Application.Exename,'.hlp');
  Application.HelpContext(1);
end;

{---------------------- Taskbarhandling-Methoden ------------------------------}

{------------------------------------------------}
function TFormAbrufServer.TaskBarAddIcon: boolean;
{------------------------------------------------}
begin
  tnid.cbSize := SizeOf(TNOTIFYICONDATA);
  tnid.Wnd := Handle;                                 { Handle des Fensters }
  tnid.uID := 1;                                      { ID beliebig }
  tnid.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
  tnid.uCallbackMessage := WM_TASKBAREVENT;
  tnid.hIcon := Application.Icon.Handle;              { Handle des Programmicons }
  StrCopy (tnid.szTip, pchar (SvcMgr.Application.Title));    { Tooltiptext: Service-Titel }
  Result:=Shell_NotifyIcon (NIM_ADD, @tnid);          { Registrieren ... }
end;

{-------------------------------------------}
procedure TFormAbrufServer.TaskBarRemoveIcon;
{-------------------------------------------}
begin
  Shell_NotifyIcon (NIM_DELETE, @tnid);                { Löschen ... }
end;

{---------------------------- Für Simulation ----------------------------------}

{$IFDEF SIMU}
{----------------------------------------}
procedure TFormAbrufServer.Init_SimuFiles;
{----------------------------------------}
{ Simulationsarchiv aufbauen }
var
  dtFrom: TDateTime;
  y, m, d, h, min, sec, msec: word;

begin
  // 14.01.2015  // Simulationsmodus
  if (C_TestFlag) then begin
    // Datei mit Simulationsdaten für DSfG-Archiv:
    FSimuFileArch := TSimuFile.Create (PathServer.Pathname [WWorkDir] +
                                       C_WicomSrv_DSfGSimuFileArchive, 'Main',
                                       PathServer.Pathname [WLogDir]);
    // Datei mit Simulationsdaten für DSfG-Logbuch:
    FSimuFileLogb := TSimuFile.Create (PathServer.Pathname [WWorkDir] +
                                       C_WicomSrv_DSfGSimuFileLogbook, 'Main',
                                       PathServer.Pathname [WLogDir]);
    // Simulationsdaten erzeugen, falls noch nicht vorhanden:
    dtFrom:=IncMonth (Now, -AbrufSrvIni.SimuArchivInit_Monate);  // für die zurückliegenden Monate

    FSimuDSfGLogbuchIntervall_MaxRandom:=AbrufSrvIni.SimuDSfGLogbuchIntervall_MaxRandom;
    FSimuFileLogb.AddRecordsFrom (dtFrom, FSimuDSfGLogbuchIntervall_MaxRandom,
                                  true);  // Logbuch mit Random-Intervall
    FSimuFileLogb.SaveToFile;

    DecodeDateTime (dtFrom, y, m, d, h, min, sec, msec);
    dtFrom:=EncodeDateTime (y, m, d, h, 0, 0, 0);  // erster Eintrag zur volle Stunde
    FSimuDSfGArchivIntervall:=AbrufSrvIni.SimuDSfGArchivIntervall;
    FSimuFileArch.AddRecordsFrom (dtFrom, FSimuDSfGArchivIntervall);  // Archiv mit festem Intervall
    FSimuFileArch.SaveToFile;
  end
  else begin
    FSimuFileArch := nil;
    FSimuFileLogb := nil;
  end;
end;

{---------------------------------------}
procedure TFormAbrufServer.Add_SimuFiles;
{---------------------------------------}
{ Simulationsarchiv fortschreiben }
begin
  // 14.01.2015  // Simulationsmodus
  if (C_TestFlag) then begin
    if Assigned (FSimuFileArch) then begin
      FSimuFileArch.AddRecordsTo (Now, FSimuDSfGArchivIntervall);  // Archiv mit festem Intervall
      FSimuFileArch.SaveToFile;
    end;

    if Assigned (FSimuFileLogb) then begin  // Logbuch mit Random-Intervall
      if Now > FSimuDateTimeNextLogbRecord then begin
        FSimuFileLogb.AddRecordNow;
        FSimuFileLogb.SaveToFile;

        FSimuDateTimeNextLogbRecord:=IncSecond (Now,
          Random (FSimuDSfGLogbuchIntervall_MaxRandom));
      end;
    end;
  end;
end;
{$ENDIF}


{---------------------- Temporär-Dateien löschen ------------------------------}

{--------------------------------------------}
procedure TFormAbrufServer.DeleteOldTempFiles;
{--------------------------------------------}
{ Veraltete Temporär- und Request/Response-Dateien des Programms löschen }

  {---------------------------------------------------------------}
  procedure DeleteOldTempFilesMasked (sPath, sFileNameMask: string;
    iFileAgeDays: integer);
  {---------------------------------------------------------------}
  const
    C_MaxCountDelete = 1000;  // Messungen mit WicomSrv.exe: ca. 0,2 ms je Datei (unabh.von Anzahl),
                              // bei 1000 Dateien ca. 200 ms (getestet mit 100, 500, 1000 Dateien)
    C_MaxCountCancel = 10;

  var
    i: integer;
    SR: TSearchRec;
    sFileNameMaskWithPath: string;
    sFileNameWithPath: string;
    dtFileUTC: TDateTime;
    dtNowUTC: TDateTime;
    iCount: integer;
    iErrCount: integer;
    S: string;
//    dtDiff: TDateTime;
//    y, m, d, h, min, sec, ms: word;

  begin
    dtNowUTC:=WNowUTC;  // Aktuelle UTC-Zeit als Referenz für die Bestimmung, ob Datei veraltet ist
    sFileNameMaskWithPath:=sPath + sFileNameMask;

    // Vorhandene, zur Maske passenden Dateien zusammensuchen:
    //  -> Nur bis zur max. Anzahl zu löschender Dateien (Obergrenze als Schutz
    //     vor ggf. zu lange andauerndem Löschen)
    //  -> Nur bis zur max. Anzahl von fehlerhaften Löschversuchen (Abbruch, da
    //     ggf. grundsätzliches Problem vorliegen könnte)
    //  -> Beim Beenden des Programms kann ebenfalls vorzeitig abgebrochen werden.
    iCount:=0;
    iErrCount:=0;
    i:=FindFirst (sFileNameMaskWithPath, faAnyFile, SR);
    try
      while (i = 0) AND (iCount < C_MaxCountDelete) AND
            (iErrCount < C_MaxCountCancel) do begin
        sFileNameWithPath:=sPath + SR.Name;
        dtFileUTC:=WFileTimeToDateTime (SR.FindData.ftLastWriteTime);  // UTC-Zeitpunkt der letzten Änderung

        // Wenn Datei veraltet ist, dann löschen:
        if (IncDay (dtFileUTC, iFileAgeDays) < dtNowUTC) then begin
          inc (iCount);

          if not DeleteFile (sFileNameWithPath) then begin
            // Datei konnte nicht gelöscht werden
            inc (iErrCount);
            S:=Format (CMsgErrDeleteOldTempFile, [sFileNameWithPath]);
            WriteProgramLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Logfile }
          end;
        end;

        Application.ProcessMessages;
        i:=FindNext (SR);
      end;  { while i = 0 }
    finally
      FindClose (SR);
    end;

    // Nur für Debug-Zeitmessung:    
//    dtDiff:=WNowUTC - dtNowUTC;
//    DecodeDateTime(dtDiff, y, m, d, h, min, sec, ms);
//    S:=IntToStr (iCount) + ' Dateien gelöscht in ' + FormatDateTime ('hh:nn:ss,zzz', dtDiff);
//    if iCount > 0 then
//      S:=S + ': ' + FloatToStr (((sec * 1000) + ms) / iCount) + ' ms/Datei';
//    WriteProgramLog (S, lt_Debug);
    // Ende Debug-Zeitmessung

    if iErrCount >= C_MaxCountCancel then begin
      S:=Format (CMsgCancelDeleteOldTempFile, [sFileNameMaskWithPath]);
      WriteProgramLog (S, lt_Warning);  { Warnung-Ausgabe in Programm-Logfile }
    end;
  end;

const
  // Max. Lebensdauer von Dateien in Tagen:
  C_FileAgeDays_Tmp    =  2;  // Tmp-Dateien
  C_FileAgeDays_TmpErr = 30;  // Err-Dateien (länger für evtl. erforderliche Fehleranalyse)
  C_FileAgeDays_ReqRes = 30;  // Request/Response-Dateien (länger für evtl. erforderliche Fehleranalyse)

  // Masken für Dateinamen-Erweiterungen:
  C_FileExtMask_Tmp    = '*.tmp';
  C_FileExtMask_TmpErr = '*.tmp.err';
  C_FileExtMask_Base64 = '*' + ext_Base64;

  C_FileMask_Req_Log   = '*_*' + ext_Req_Log;
  C_FileMask_Req_Xml   = '*_*' + ext_Req_Xml;
  C_FileMask_Res_Xml   = '*_*' + ext_Res_Xml;

  // Die Temporär-Dateien:
  C_FileMasks_Tmp: array [1..8] of string = (
    // Rohdaten:
    (prefix_DSfG_Roh + C_FileExtMask_Tmp),     // z.B. ~DS1A0B.tmp
    (prefix_MRG_Roh + C_FileExtMask_Tmp),      // z.B. ~MR1A0B.tmp
    (prefix_MB_Konv + C_FileExtMask_Tmp),      // z.B. ~MB1A0B.tmp
    // Base64-Kodierte Rohdaten:
    (prefix_DSfG_Roh + C_FileExtMask_Base64),  // z.B. ~DS1A0B.base64
    // Zwischendateien der Konvertierungen:
    (prefix_DSfG_Ar + C_FileExtMask_Tmp),      // z.B. DAR1A0B.tmp
    (prefix_DSfG_Lb + C_FileExtMask_Tmp),      // z.B. DLB1A0B.tmp
    (prefix_Mess + C_FileExtMask_Tmp),         // z.B. ZST1A0B.tmp
    (prefix_Tags + C_FileExtMask_Tmp)          // z.B. ZTA1A0B.tmp
    );

  // Die Err-Dateien mit Rohdaten:
  C_FileMask_TmpErr = prefix_DSfG_Roh + C_FileExtMask_TmpErr;  // z.B. ~DS1A0B.tmp.err

  // Die Request/Response-Dateien:
  C_FileMasks_ReqRes: array [1..3] of string = (
    C_FileMask_Req_Log,  // z.B. 20221011_163512787_req.log
    C_FileMask_Req_Xml,  // z.B. 20221011_163512787_req.xml
    C_FileMask_Res_Xml   // z.B. 20221011_163512787_res.xml
    );

var
  sPath: string;
  i: integer;

begin
  sPath:=ExpandFileName (PathServer.PathName [WWorkDir]);  // Alle Temp-Dateien sind im WWorkDir

  if not FDebugRohdaten then begin  // Nur Löschen, wenn Rohdaten-Dateien nicht erhalten bleiben sollen
    // Temporär-Dateien löschen:
    for i:=Low (C_FileMasks_Tmp) to High (C_FileMasks_Tmp) do begin
      Application.ProcessMessages;
      DeleteOldTempFilesMasked (sPath, C_FileMasks_Tmp [i], C_FileAgeDays_Tmp);
    end;
    // Err-Dateien löschen:
    DeleteOldTempFilesMasked (sPath, C_FileMask_TmpErr, C_FileAgeDays_TmpErr);
  end;

  if not FDebugServiceIO then begin  // Nur Löschen, wenn Request/Response-Dateien nicht geschrieben werden sollen
    // Request/Response-Dateien löschen:
    for i:=Low (C_FileMasks_ReqRes) to High (C_FileMasks_ReqRes) do begin
      Application.ProcessMessages;
      DeleteOldTempFilesMasked (sPath, C_FileMasks_ReqRes [i], C_FileAgeDays_ReqRes);
    end;
  end;
end;

end.

