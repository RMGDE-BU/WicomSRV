{******************************************************************************}
{* Unit: Hauptfenster für Modbus-Master                                       *}
{* Version 1.0: 12.02.2010  WW  Polling auf mehreren seriellen Schnittstellen;*}
{*                              Multithread                                   *}
{* 1.1:  04.08.2010  GD Erweiterung für MODBUS TCP                            *}
{* 1.11: 04.11.2011  WW mit Kommandozeilenparameter SHOWMINIMIZED, HIDE;      *}
{*                      Bugfix Pollingintervall < 1000 ms (es erfolgte keine  *}
{*                      Abfrage)                                              *}
{* 1.1.2: 25.11.2014 WN Austausch Memo -> Listbox (Verbesserung Performance)  *}
{*                   GD Erweiterung Debug-Einträge                            *}
{* 1.1.3: 11.03.2015 GD Reconnect bei Modbus TCP, Bugfix ErrorCode 4          *}
{* 1.2.0: 28.06.2017 WW Erweiterte Exception-Codes 0A, 0B; mit Modbusfunktio- *}
{*                      nen 01 (Read Coil Status), 02 (Read Input Status),    *}
{*                      04 (Read Input Registers); Logdateien in Log-Pfad der *}
{*                      Wieser.ini schreiben; Zusammenfassen der Startadressen*}
{*                      für Request optimiert (bisher konnten nur die in der  *}
{*                      Register-Definitionsdatei AUFSTEIGEND enthaltenen     *}
{*                      Startadressen zusammengefaßt werden);                 *}
{*                      mit Modbus-Schreibfunktionen (06: Preset Single       *}
{*                      Register, 16: Preset Multiple Registers): zusätzliche,*}
{*                      optionale Konfig.dateien <PROGNAME>_SLAVE_WRITE.DAT   *}
{*                      und <PROGNAME>_REGISTER_WRITE.DAT; zu schreibende     *}
{*                      Werte können als Festwerte konfiguriert werden oder   *}
{*                      per Platzhalter-Definition für aktuelle Werte         *}
{*                      (<SYSTIME_DOUBLE> für Vympel-Gerätezeit,              *}
{*                      <SYSTIME_YMDHNS_W+1> für Siemens GC-Gerätezeit);      *}
{*                      Modbus-Schreibvorgänge loggen in Dateien              *}
{*                      <PROGNAME>_WRITE_<nnn>.LOG (nnn = Liniennummer, akti- *}
{*                      vieren per INI-Definition [Debug] WriteProtokoll = 1);*}
{*                      Slave-Konfigurationsdateien: im Feld 'Pollingzyklus'  *}
{*                      kann alternativ auch ein Zeitstempel HH:MM:SS konfigu-*}
{*                      riert werden (Ausführung dann einmal am Tag zur defi- *}
{*                      nierten Uhrzeit)                                      *}
{* 1.2.1: 04.06.2018 WW Timeout für Versenden der Windows-Nachrichten erhöht  *}
{*                      von 100 auf 1000 ms (für DS901-Abruf GC9300)          *}
{* 1.2.2: 08.03.2019 WW Bugfix LoadFromRegisterDataList: Registeradressen-    *}
{*                      Bereich von/bis ohne Lücke zusammenfassen             *}
{* 1.2.3: 13.11.2019 WW ByteCount Ist/Soll-Plausibilisierung der Modbus-      *}
{                       Responses präzisiert                                  *}
{* 1.2.4: 16.12.2019 WW Interne Verwaltung der Transaktions-ID für Modbus TCP *}
{*                      geändert; Überlauf der Transaktions-ID jetzt bei      *}
{*                      $FFFF (max. Word-Wertebereich, bisher bei $0FFF); mit *}
{*                      Validierung des MBAP-Headers in Modbus TCP Response;  *}
{*                      Zeitsynchronisation Siemens GC umgestellt auf Einzel- *}
{*                      parametrierung der Datum/Zeit-Komponenten (Platzhal-  *}
{*                      ter-Definitionen <SYSTIME_Y_W>, <SYSTIME_M_W>,        *}
{*                      <SYSTIME_D_W>, <SYSTIME_H_W>, <SYSTIME_N_W>,          *}
{*                      <SYSTIME_S_W>                                         *}
{* 1.2.5: 20.03.2024 WW Bugfix Schutzverletzung bei IP900; Plausibilisierung  *}
{*                      der seriellen COM-Linien angepaßt bis max. 256;       *}
{*                      Erweiterte Fehlertexte für IP-Linien                  *}
{******************************************************************************}
unit FMainModbusMaster;

interface

uses
  SvcMgr, ShellApi, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, ImgList,
  Serial, SerialConst, T_BinMask, PathIni, Lizenz32,
  ModbusMasterConst, ModbusMasterIniFile, ModbusMasterThr, LogFile, ModbusUtil,
  WSysCon, ErrConst, ErrTxt, WComm, Menus;

type
  { Modbus-Start-Modi }
  TMBStartModus = (mbs_Normal, mbs_Minimized, mbs_Hide);

  { Hauptfenster Modbus-Master }
  TFormMainModbusMaster = class(TForm)
    pStatus: TPanel;
    pLeft: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pFehler: TPanel;
    pErrors: TPanel;
    pKommunikation: TPanel;
    pReceive: TPanel;
    pVerbindungen: TPanel;
    Panel2: TPanel;
    pCOMGeoeffnet: TPanel;
    pCOMGeschlossen: TPanel;
    lvCOM: TListView;
    pCOMNichtVorhanden: TPanel;
    Timer: TTimer;
    VerbImageList: TImageList;
    PopupMenu: TPopupMenu;
    pmMenueSchliessen: TMenuItem;
    N1: TMenuItem;
    mAnzeigen: TMenuItem;
    lbError: TListBox;
    lbMonitor: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvCOMSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mAnzeigenClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FProgramIni: TModbusMasterIni;
    FWLizenz32: TWLizenz32;
    FTage_Merk: integer;
    FFormDestroying: boolean;
    FCOMControls: TModbusCOMControls;
    FCount_COM_Opened: integer;
    FCount_COM_Closed: integer;
    FCount_COM_NotAvail: integer;
    FAktCOMNr: integer;
    tnid: TNOTIFYICONDATA;
    FFirst: boolean;
    FMBStartModus: TMBStartModus;  // 04.11.2011, WW
    FLogDir: string;  // 28.06.2017, WW
    procedure SetFensterTitel;
    procedure GetKommandozeilenParameter;  // 04.11.2011, WW
    procedure WriteProgramErrorLog (S: string; ALogType: TLogType);
    procedure InitCOMControls;
    procedure CloseCOMControls;
    function StartCOMThreads: boolean;
    procedure SetAktCOMNr (bNewCOMNr: boolean);
    procedure Status_COM_Init (ACOMNr, ABaudrate: integer;
      ADatabits: TDataBits; AParityBit: TParityBit; AStopBits: TStopBits;
      const sIpAddress: string; iIpPort: integer; AModbusModus: TModbusModus);
    procedure Status_COM_Open (ACOMNr, AFehlergruppe, AFehlercode: integer;
      AFktCode: byte);
    procedure Status_COM_Request (ACOMNr, AFehlergruppe, AFehlercode: integer;
      AFktCode: byte);
    procedure Inc_Verbindungen_Fehler (ACOMNr: integer);
    procedure Monitor_Daten (ACOMNr: integer; AData: string;
      AMonitorDataType: TMonitorDataType);
    procedure Journal_Fehler (ACOMNr: integer; AFehler: string);
    procedure Update_MonitorMemo (ACOMNr: integer; S: string);
    procedure Update_FehlerMemo (ACOMNr: integer; S: string);

    procedure WMTASKBAREVENT (var message: TMessage); message WM_TASKBAREVENT;
  public
    { Public-Deklarationen }
    function TaskBarAddIcon: boolean;
    procedure TaskBarRemoveIcon;
  end;

var
  FormMainModbusMaster: TFormMainModbusMaster;

implementation

{$R *.dfm}

resourcestring
  SApplicationTitle = 'WICO22 - Modbus-Master';

  SInitialisiereCOM = 'Initialisiere COM';
  SInitialisiereIP = 'Initialisiere TCP/IP';
  SCOMn = 'COM%d';

  SnGeoeffnet = '%d geöffnet';
  SnGeschlossen = '%d geschlossen';
  SnNichtVorhanden = '%d nicht vorhanden';

  SBereit = 'Bereit';
  SAusgangsStatusLesen = 'Ausgangs-Status lesen (FC 01)';
  SEingangsStatusLesen = 'Eingangs-Status lesen (FC 02)';
  SHoldingRegisterLesen = 'Holding-Register lesen (FC 03)';
  SInputRegisterLesen = 'Input-Register lesen (FC 04)';
  SEinzelnesRegisterSchreiben = 'Einzelnes Register schreiben (FC 06)';
  SMehrereRegisterSchreiben = 'Mehrere Register schreiben (FC 16)';

  SMonitor = 'Monitor:';
  SFehler = 'Fehler:';

  SUnbekannt = 'Unbekannt';

const
  { Image-Indizes für COM-Verbindungsstatus }
  stcom_starting = 0;
  stcom_closed   = 1;
  stcom_opened   = 2;
  stcom_notavail = 3;


{----------------------------------------------------------}
procedure TFormMainModbusMaster.FormCreate(Sender: TObject);
{----------------------------------------------------------}
var
  S: string;
  ExeName: string;
  sLogDir: string;
  sErrMsg: string;

begin
try
  FFirst:=false;
  FMBStartModus:=mbs_Normal;
  GetKommandozeilenParameter;  // 04.11.2011, WW

  SvcMgr.Application.Title := SApplicationTitle;

  SetFensterTitel;

  FFormDestroying:=false;
  FTage_Merk:=-1;  // für tägliche Laufzeit-Lizenzprüfung
  FCount_COM_Opened:=0;
  FCount_COM_Closed:=0;
  FCount_COM_NotAvail:=0;
  FAktCOMNr:=-1;

  // Default LogDir: Exe-Verzeichnis, falls LogDir aus Wieser.ini nicht zu
  // ermitteln ist; 28.06.2017, WW
  FLogDir:=ExtractFilePath (ParamStr(0));

  // Log-Pfad aus Wieser.ini lesen;
  if not GetPathServerLogDir (sLogDir, sErrMsg) then begin
    pStatus.Font.Color:=clRed;
    S:=SMsgModbusMasterInactive + ' ' + sErrMsg;
    pStatus.Caption:=' ' + S;          { Fehler-Ausgabe im Fenster }
    S:=CMsgModbusMasterInactive + ' ' + sErrMsg;
    WriteProgramErrorLog (sErrMsg, lt_Error);  { Fehler-Ausgabe in Programm-Error-Logfile }
    exit;
  end;
  FLogDir:=sLogDir;

  WriteProgramErrorLog (CMsgSrvStarted + ' (Vs. ' + CVersion_ModbusMaster + ')',
                        lt_Info);

  FProgramIni:=TModbusMasterIni.Create;

  { PathServer initialisieren: }
  PathServer:=TPathServer.Create (FProgramIni.WieserIniFile,
                                  [WNetProgDir, WLogDir]);
  try
    PathServer.Check;
  except
    // Fehler beim Initialisieren des PathServers (alle für den Betrieb des
    // Programms benötigten Verzeichnisse):
    on E:Exception do begin
      pStatus.Font.Color:=clRed;
      S:=SMsgModbusMasterInactive + ' ' + E.Message;
      pStatus.Caption:=' ' + S;          { Fehler-Ausgabe im Fenster }
      S:=CMsgModbusMasterInactive + ' ' + E.Message;
      WriteProgramErrorLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Error-Logfile }
      Exit;
    end;
  end;

  { Lizenz prüfen: }
  FWLizenz32:=TWLizenz32.Create (PathServer.Pathname [WNetProgDir] + C_Lizenz_Filename, true);
  ExeName:=ExtractFileName(Application.ExeName);
  if not FWLizenz32.GetLizenzExe (ExeName) then begin
    // Programm ist nicht lizenziert !
    pStatus.Font.Color:=clRed;
    S:=SMsgModbusMasterInactive + ' ' + SMsgLicExe;
    pStatus.Caption:=' ' + S;          { Fehler-Ausgabe im Fenster }
    S:=CMsgModbusMasterInactive + ' ' + CMsgLicExe;
    WriteProgramErrorLog (S, lt_Warning);  { Fehler-Ausgabe in Programm-Error-Logfile }
    exit;
  end;

  { Anm.: Laufzeit-Lizenz wird bei Programmstart und täglich geprüft (in Timer-Methode) }

  if not FWLizenz32.GetLizenzPC ('') then begin
    // Programm ist für Rechner nicht lizenziert !
    pStatus.Font.Color:=clRed;
    S:=SMsgModbusMasterInactive + ' ' + SMsgLicPC;
    pStatus.Caption:=' ' + S;          { Fehler-Ausgabe im Fenster }
    S:=CMsgModbusMasterInactive + ' ' + CMsgLicPC;
    WriteProgramErrorLog (S, lt_Warning);  { Fehler-Ausgabe in Programm-Error-Logfile }
    exit;
  end;

  InitCOMControls;    { COM-Controls initialisieren }
  FFirst:=true;

  if FMBStartModus = mbs_Hide then begin
    Application.ShowMainForm:=false;  // 04.11.2011, WW

    { FormActivate wird nicht aufgerufen ! }
    if StartCOMThreads then  { COM-Threads starten }
      Timer.Enabled:=true;
  end;
except
end;
end;

{------------------------------------------------------------}
procedure TFormMainModbusMaster.FormActivate(Sender: TObject);
{------------------------------------------------------------}
begin
  if FFirst then begin
    FFirst:=false;
    if FMBStartModus = mbs_Minimized then
      Application.Minimize;

    if StartCOMThreads then  { COM-Threads starten }
      Timer.Enabled:=true;
  end;
end;

{-----------------------------------------------------------}
procedure TFormMainModbusMaster.FormDestroy(Sender: TObject);
{-----------------------------------------------------------}
begin
  try
    FFormDestroying:=true;
    Timer.Enabled:=false;

    CloseCOMControls;   { COM-Controls beenden und freigeben }

    FWLizenz32.Free;
    PathServer.Free;
    FProgramIni.Free;

    WriteProgramErrorLog (CMsgSrvStopped + ' (Vs. ' + CVersion_ModbusMaster + ')',
                          lt_Info);
  except
  end;
end;

{--------------------------------------------------------}
procedure TFormMainModbusMaster.FormClose(Sender: TObject;
  var Action: TCloseAction);
{--------------------------------------------------------}
begin
  Action:=caHide;   // nur verstecken, nicht schließen
end;

{----------------------------------------------}
procedure TFormMainModbusMaster.SetFensterTitel;
{----------------------------------------------}
begin
  Caption:=SvcMgr.Application.Title + ' - Vs. ' + CVersion_ModbusMaster + ', ' +
           FormatDateTime(SFormatDate, (FileDateToDateTime (FileAge (Application.ExeName)))) +
           ', Copyright © RMG Messtechnik GmbH';
end;

{---------------------------------------------------------}
procedure TFormMainModbusMaster.GetKommandozeilenParameter;  // 04.11.2011, WW
{---------------------------------------------------------}
{ Kommandozeilenparameter auswerten }
var
  i: integer;
  S_Param: string;
begin
  for i:=1 to ParamCount do begin
    S_Param:=ParamStr(i);
    { S_Param auswerten: }
    S_Param:=UpperCase (S_Param);
    { Hauptfenster bei Programmstart minimiert, versteckt:  04.11.2011, WW }
    if S_Param = 'SHOWMINIMIZED' then
      FMBStartModus:=mbs_Minimized;
    if S_Param = 'HIDE' then
      FMBStartModus:=mbs_Hide;
  end;  { for }
end;

{-----------------------------------------------------------------------------------}
procedure TFormMainModbusMaster.WriteProgramErrorLog (S: string; ALogType: TLogType);
{-----------------------------------------------------------------------------------}
{ Eintrag in Programm-Error-Logfile schreiben }
var
  ErrLogFile: TCustomLogFile;
  FileName: string;

begin
  FileName:=ChangeFileExt (ExtractFileName (Application.ExeName), '');
  ErrLogFile:=TCustomLogFile.Create (FLogDir, FileName, false);  // 28.06.2017, WW
  try
    ErrLogFile.Write (S, true, ALogType);
  finally
    ErrLogFile.Free;
  end;
end;

{----------------------------------------------}
procedure TFormMainModbusMaster.InitCOMControls;
{----------------------------------------------}
{ COM-Controls initialisieren }
var
  i: integer;
begin
  { COM-Controls initialisieren: }
  for i:=Low (FCOMControls) to High (FCOMControls) do begin
    FCOMControls [i].Thread:=nil;

    FCOMControls [i].MonitorMemo:=TListBox.Create (Self);  // 25.11.2014  WN
    with FCOMControls [i].MonitorMemo do begin
      Align:=alClient;
      Parent:=pKommunikation;
      Visible:=false;
    end;

    FCOMControls [i].ErrorMemo:=TListBox.Create (Self);  // 25.11.2014  WN
    with FCOMControls [i].ErrorMemo do begin
      Align:=alClient;
      Parent:=pFehler;
      Visible:=false;
    end;
  end;
end;

{-----------------------------------------------}
procedure TFormMainModbusMaster.CloseCOMControls;
{-----------------------------------------------}
{ COM-Controls beenden und freigeben }
var
  i: integer;
begin
  { Beenden aller noch laufenden COM-Threads einleiten: }
  for i:=Low (FCOMControls) to High (FCOMControls) do begin
    if Assigned (FCOMControls [i].Thread) then begin
      FCOMControls [i].Thread.Resume;    // Thread fortsetzen, falls er unterbrochen ist (sonst reagiert er nicht)
      FCOMControls [i].Thread.Terminate; // Thread-Beenden einleiten
    end;
  end;
  { auf das Ende jedes COM-Threads warten: }
  for i:=Low (FCOMControls) to High (FCOMControls) do begin
    if Assigned (FCOMControls [i].Thread) then begin
      try
        FCOMControls [i].Thread.WaitForTermination;   // warten bis Thread beendet ist
      except
        // Ungültiges Handle unterdrücken
      end;
    end;
  end;
end;

{------------------------------------------------------}
function TFormMainModbusMaster.StartCOMThreads: boolean;
{------------------------------------------------------}
{ COM-Threads starten }
var
  COMNr: integer;
  pSlIp, COMList: TStringList;
  i: integer;
  S: string;
  bDebugThreadProtokoll: boolean;
  bDebugCOMProtokoll: boolean;
  bDebugFehlerProtokoll: boolean;
  bDebugWriteProtokoll: boolean;

begin
  Result:=false;
  pSlIp := TStringList.Create;
  COMList:=TStringList.Create;
  try
    { alle definierten COM- und IP-Linien aus INI-Konfiguration lesen: }
    if (not FProgramIni.GetCOMList(COMList)) or
       (not FProgramIni.GetIPList(pSlIp)) then
    begin
      // ungültige COM/IP definiert
      pStatus.Font.Color:=clRed;
      S:=SMsgModbusMasterInactive + ' ' +
        Format (SMsgInvalidCOMIPConfig, [MaxPossiblePort, CComTCP_IP, CComTCP_IP+99]);  // 20.03.2024, WW
      pStatus.Caption:=' ' + S;          { Fehler-Ausgabe im Fenster }
      S:=CMsgModbusMasterInactive + ' ' +
        Format (CMsgInvalidCOMIPConfig, [MaxPossiblePort, CComTCP_IP, CComTCP_IP+99]);  // 20.03.2024, WW
      WriteProgramErrorLog (S, lt_Error);  { Fehler-Ausgabe in Programm-Error-Logfile }
      Exit;
    end
    else if (COMList.Count = 0) and (pSlIp.Count = 0) then begin
      // keine COM/IP definiert
      pStatus.Font.Color:=clRed;
      S:=SMsgModbusMasterInactive + ' ' + SMsgNoCOMIPConfig;
      pStatus.Caption:=' ' + S;          { Fehler-Ausgabe im Fenster }
      S:=CMsgModbusMasterInactive + ' ' + CMsgNoCOMIPConfig;
      WriteProgramErrorLog (S, lt_Warning);  { Fehler-Ausgabe in Programm-Error-Logfile }
      Exit;
    end;
    // Listen zusammenführen
    for i := 0 to pSlIp.Count-1 do COMList.Add(pSlIp[i]);

    bDebugThreadProtokoll:=FProgramIni.DebugThreadProtokoll;
    bDebugCOMProtokoll:=FProgramIni.DebugCOMProtokoll;
    bDebugFehlerProtokoll:=FProgramIni.DebugFehlerProtokoll;
    bDebugWriteProtokoll:=FProgramIni.DebugWriteProtokoll;  // 28.06.2017, WW

    { Modbus-Master ist aktiv: }
    pStatus.Font.Color:=clGreen;
    S:=SMsgModbusMasterActive;
    pStatus.Caption:=' ' + S;          { Ausgabe im Fenster }
    S:=CMsgModbusMasterActive;
    WriteProgramErrorLog (S, lt_Info);  { Ausgabe in Programm-Error-Logfile }

    { für jede definierte COM einen Thread erzeugen: }
    for i:=0 to COMList.Count - 1 do begin
      Application.ProcessMessages;

      COMNr:=StrToInt (COMList [i]);
      if (COMNr >= Low (FCOMControls)) AND (COMNr <= High (FCOMControls)) then begin
        FCOMControls [COMNr].Thread:=TModbusMasterCOMThread.CreateIt (COMNr,
          bDebugThreadProtokoll, bDebugCOMProtokoll, bDebugFehlerProtokoll,
          bDebugWriteProtokoll,
          Handle, Status_COM_Init, Status_COM_Open, Status_COM_Request,
          Monitor_Daten, Journal_Fehler);
      end;
    end;  { for i }
  finally
    COMList.Free;
    pSlIp.Free;
  end;
  Result:=true;
end;

{------------------------------- Anzeige --------------------------------------}

{--------------------------------------------------------------}
procedure TFormMainModbusMaster.SetAktCOMNr (bNewCOMNr: boolean);
{--------------------------------------------------------------}
{ aktuell ausgewählte COM-Nummer aktualisieren und Kontrollelemente ein-/ausblenden }
begin
  if bNewCOMNr then begin
    if Assigned (lvCOM.Selected) then
      FAktCOMNr:=StrToInt (lvCOM.Selected.SubItems [0]);
    pReceive.Caption:=' ' + SMonitor + ' ' + Format (SCOMn, [FAktCOMNr]);
    pErrors.Caption:=' ' + SFehler + ' ' + Format (SCOMn, [FAktCOMNr]);
  end;

  if (FAktCOMNr >= Low (FCOMControls)) AND (FAktCOMNr <= High (FCOMControls)) then begin
    FCOMControls [FAktCOMNr].MonitorMemo.Visible:=bNewCOMNr;
    FCOMControls [FAktCOMNr].ErrorMemo.Visible:=bNewCOMNr;
  end;
end;

{-----------------------------------------------------------------------------}
procedure TFormMainModbusMaster.Status_COM_Init (ACOMNr, ABaudrate: integer;
  ADatabits: TDataBits; AParityBit: TParityBit; AStopBits: TStopBits;
  const sIpAddress: string; iIpPort: integer; AModbusModus: TModbusModus);
{-----------------------------------------------------------------------------}
{ Anzeige COM-Initialisierung }
var
  pListItem: TListItem;
  S: string;
begin
  pListItem:=lvCOM.Items.Add;    { 1. Spalte: Aktiv }
  pListItem.StateIndex:=stcom_starting;

  pListItem.SubItems.Add (IntToStr (ACOMNr));  { 2. Spalte: COM-Nummer }

  if (AModbusModus in [modbus_RTU, modbus_ASCII]) then
    S := SInitialisiereCOM
  else if (AModbusModus in [modbus_TCPIP]) then
    S := SInitialisiereIP;  // 20.03.2024, WW
  pListItem.SubItems.Add (S);  { 3. Spalte: Status }

  pListItem.SubItems.Add ('0');  { 4. Spalte: Anzahl Fehler }

  if (AModbusModus in [modbus_RTU, modbus_ASCII]) then
    S := GetSerialDataString (ABaudrate, ADataBits, AParityBit, AStopBits)
  else if (AModbusModus in [modbus_TCPIP]) then
    S := sIpAddress + '  ' + IntToStr(iIpPort);
  pListItem.SubItems.Add (S);  { 5. Spalte: COM-Parameter }
  case AModbusModus of
    modbus_RTU: S:=SModbusRTU;
    modbus_ASCII: S:=SModbusASCII;
    modbus_TCPIP: S := SModbusTCP;
  else
    S:=SUnbekannt;
  end;

  pListItem.SubItems.Add (S);  { 6. Spalte: Protokoll }

  if lvCOM.Items.Count = 1 then begin
    lvCOM.Items [0].Selected:=true;  // 1. Eintrag selektieren
    SetAktCOMNr (true);  // aktuell ausgewählte COM-Nummer aktualisieren
  end;

  Application.ProcessMessages;
end;

{-----------------------------------------------}
procedure TFormMainModbusMaster.Status_COM_Open (
  ACOMNr, AFehlergruppe, AFehlercode: integer; AFktCode: byte);
{-----------------------------------------------}
{ Anzeige COM-Öffnen }
var
  i: integer;
  S: string;
  iStateIndex: integer;
  COMNr: integer;
  LogType: TLogType;

begin
  for i:=0 to lvCOM.Items.Count - 1 do begin   // evtl. besser mit direktem Zugriff über Listenindex   !
    COMNr:=StrToInt (lvCOM.Items [i].SubItems [0]);
    if COMNr = ACOMNr then begin  // Listen-Eintrag für COM-Nummer gefunden
      if FehlerGruppeCode_OK (AFehlergruppe, AFehlercode) then begin  // OK, COM geöffnet
        S:=SBereit;
        iStateIndex:=stcom_opened;
        inc (FCount_COM_Opened);
        LogType:=lt_Info;
      end
      else begin  // Fehler beim Öffnen der COM
        S:=GetStatusText (AFehlergruppe) + ' - ' +
           GetErrorText (AFehlergruppe, AFehlercode);
        if (AFehlergruppe = COM_PORTERROR) AND
           (AFehlercode = COMPORTERR_NICHTVORHANDEN) then begin
          iStateIndex:=stcom_notavail;
          inc (FCount_COM_NotAvail);
        end
        else begin
          iStateIndex:=stcom_closed;
          inc (FCount_COM_Closed);
        end;
        LogType:=lt_Error;
      end;
      lvCOM.Items [i].StateIndex:=iStateIndex;  { 1. Spalte: Aktiv }
      lvCOM.Items [i].SubItems [1]:=S;  { 3. Spalte: Status }

      pCOMGeoeffnet.Caption:=Format (SnGeoeffnet, [FCount_COM_Opened]);
      pCOMGeschlossen.Caption:=Format (SnGeschlossen, [FCount_COM_Closed]);
      pCOMNichtVorhanden.Caption:=Format (SnNichtVorhanden, [FCount_COM_NotAvail]);

      S:=Format (SCOMn, [COMNr]) + ': ' + S;
      WriteProgramErrorLog (S, LogType);

      Break;
    end;
  end;  { for i }

  Application.ProcessMessages;
end;

{-------------------------------------------------------------------}
procedure TFormMainModbusMaster.Status_COM_Request (
  ACOMNr, AFehlergruppe, AFehlercode: integer; AFktCode: byte);
{-------------------------------------------------------------------}
{ Anzeige COM-Abfrage }
var
  i: integer;
  S: string;
  COMNr: integer;

begin
  for i:=0 to lvCOM.Items.Count - 1 do begin   // evtl. besser mit direktem Zugriff über Listenindex   !
    COMNr:=StrToInt (lvCOM.Items [i].SubItems [0]);
    if COMNr = ACOMNr then begin  // Listen-Eintrag für COM-Nummer gefunden
      if FehlerGruppeCode_OK (AFehlergruppe, AFehlercode) then begin  // Request OK
        case AFktCode of  // 28.06.2017, WW
           1: S:=SAusgangsStatusLesen;
           2: S:=SEingangsStatusLesen;
           3: S:=SHoldingRegisterLesen;
           4: S:=SInputRegisterLesen;
           6: S:=SEinzelnesRegisterSchreiben;
          16: S:=SMehrereRegisterSchreiben;
        else
          S:=SUnbekannt;
        end;
      end else  // Fehler bei Request
        S:=GetStatusText (AFehlergruppe) + ' - ' +
           GetErrorText (AFehlergruppe, AFehlercode);
      lvCOM.Items [i].SubItems [1]:=S;  { 3. Spalte: Status }
      Break;
    end;
  end;  { for i }

  Application.ProcessMessages;
end;

{------------------------------------------------------------------------}
procedure TFormMainModbusMaster.Inc_Verbindungen_Fehler (ACOMNr: integer);
{------------------------------------------------------------------------}
{ Anzahl der Fehler in Verbindungen-Anzeige inkrementieren }
var
  i: integer;
  COMNr: integer;
  iFehler: integer;
  S: string;

begin
  for i:=0 to lvCOM.Items.Count - 1 do begin   // evtl. besser mit direktem Zugriff über Listenindex   !
    COMNr:=StrToInt (lvCOM.Items [i].SubItems [0]);
    if COMNr = ACOMNr then begin  // Listen-Eintrag für COM-Nummer gefunden
      iFehler:=StrToIntDef (lvCOM.Items [i].SubItems [2], -1);
      if iFehler <> -1 then begin
        if iFehler < High (iFehler) then begin
          inc (iFehler);
          S:=IntToStr (iFehler);
        end else
          S:='> ' + lvCOM.Items [i].SubItems [2];
        lvCOM.Items [i].SubItems [2]:=S;  { 4. Spalte: Anzahl Fehler }
      end;
      Break;
    end;
  end;  { for i }

  Application.ProcessMessages;
end;

{----------------------------------------------------------------------------}
procedure TFormMainModbusMaster.Monitor_Daten (ACOMNr: integer; AData: string;
  AMonitorDataType: TMonitorDataType);
{----------------------------------------------------------------------------}
{ Daten im Monitor anzeigen;
  Übergabe: COM-Nummer
            Sendedaten
            Monitor-Datentyp (Senden, Empfangen, ohne) }
var
  S: string;

begin
  case AMonitorDataType of
    mdt_Tx: S:='TX:' + '     ';
    mdt_Rx: S:='RX:' + '     ';
  else
    S:='';
  end;

  Update_MonitorMemo (ACOMNr, S + AData);
end;

{--------------------------------------------------------------------------------}
procedure TFormMainModbusMaster.Journal_Fehler (ACOMNr: integer; AFehler: string);
{--------------------------------------------------------------------------------}
{ Fehler in Fehlerjournal anzeigen;
  Übergabe: COM-Nummer
            Fehlertext }
begin
  Inc_Verbindungen_Fehler (ACOMNr);
  Update_FehlerMemo (ACOMNr, AFehler);
end;

{------------------------------------------------------------------------------}
procedure TFormMainModbusMaster.Update_MonitorMemo (ACOMNr: integer; S: string);
{------------------------------------------------------------------------------}
{ neuen Eintrag in Monitor-Memo schreiben;
  Übergabe: COM-Nummer
            String für neuen Monitor-Eintrag }
begin
  // 25.11.2014  WN
  if (ACOMNr >= Low (FCOMControls)) AND (ACOMNr <= High (FCOMControls)) then begin
    while FCOMControls [ACOMNr].MonitorMemo.Items.Count > 1000 do  { max. 1000 Zeilen darstellen }
      FCOMControls [ACOMNr].MonitorMemo.Items.Delete(0);
    FCOMControls [ACOMNr].MonitorMemo.Items.Add (
      FormatDateTime (SFormatDateTimeWithMs, Now) + '     ' + S);

    // ans Ende des Monitor-Memo scrollen:
    FCOMControls [ACOMNr].MonitorMemo.TopIndex :=
      FCOMControls [ACOMNr].MonitorMemo.Items.Count-1;
  end;
end;

{-----------------------------------------------------------------------------}
procedure TFormMainModbusMaster.Update_FehlerMemo (ACOMNr: integer; S: string);
{-----------------------------------------------------------------------------}
{ neuen Eintrag in Fehler-Memo schreiben;
  Übergabe: COM-Nummer
            String für neuen Fehler-Eintrag }
begin
  // 25.11.2014  WN
  if (ACOMNr >= Low (FCOMControls)) AND (ACOMNr <= High (FCOMControls)) then begin
    while FCOMControls [ACOMNr].ErrorMemo.Items.Count > 1000 do  { max. 1000 Zeilen darstellen }
      FCOMControls [ACOMNr].ErrorMemo.Items.Delete(0);
    FCOMControls [ACOMNr].ErrorMemo.Items.Add (
      FormatDateTime (SFormatDateTimeWithMs, Now) + '     ' + S);

    // ans Ende des Fehler-Memo scrollen:
    FCOMControls [ACOMNr].ErrorMemo.TopIndex :=
      FCOMControls [ACOMNr].ErrorMemo.Items.Count-1;
  end;
end;


{------------------------- Ereignis-Behandlungsmethoden -----------------------}

{--------------------------------------------------------------}
procedure TFormMainModbusMaster.lvCOMSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
{--------------------------------------------------------------}
begin
  // aktuell ausgewählte COM-Nummer aktualisieren, Kontrollelemente ein-/ausblenden:
  SetAktCOMNr (Selected);
end;


{------------------------------- Timer ----------------------------------------}

{----------------------------------------------------------}
procedure TFormMainModbusMaster.TimerTimer(Sender: TObject);
{----------------------------------------------------------}
var
  d: integer;
  ExeName: string;
  S: string;

begin
  Timer.Enabled:=false;
  try
    d:=Trunc (Date);
    // beim Tageswechsel:
    if d <> FTage_Merk then begin
      FTage_Merk:=d;
      // Laufzeit-Lizenz prüfen:
      if Assigned (FWLizenz32) then begin
        ExeName:=ExtractFileName(Application.ExeName);
        if not FWLizenz32.GetExeLaufzeit (ExeName) then begin
          // Programm-Lizenz ist abgelaufen !
          pStatus.Font.Color:=clRed;
          S:=SMsgModbusMasterInactive + ' ' + SMsgLicExeLaufzeit;
          pStatus.Caption:=' ' + S;          { Fehler-Ausgabe im Fenster }
          S:=CMsgModbusMasterInactive + ' ' + CMsgLicExeLaufzeit;
          WriteProgramErrorLog (S, lt_Warning);  { Fehler-Ausgabe in Programm-Error-Logfile }

          CloseCOMControls;   { COM-Controls beenden und freigeben }
          FFormDestroying:=true;  // Timer wird nicht mehr benötigt
        end;
      end;
    end;
  finally
    if not FFormDestroying then
      Timer.Enabled:=true;
  end;
end;


{------------------------- Message-Behandlungsmethoden ------------------------}

{---------------------------------------------------------------------}
procedure TFormMainModbusMaster.WMTASKBAREVENT (var message: TMessage);
{---------------------------------------------------------------------}
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

{--------------------------------------------------------------}
procedure TFormMainModbusMaster.mAnzeigenClick(Sender: TObject);
{--------------------------------------------------------------}
begin
  Show;
  Application.BringToFront;
end;

{---------------------- Taskbarhandling-Methoden ------------------------------}

{-----------------------------------------------------}
function TFormMainModbusMaster.TaskBarAddIcon: boolean;
{-----------------------------------------------------}
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

{------------------------------------------------}
procedure TFormMainModbusMaster.TaskBarRemoveIcon;
{------------------------------------------------}
begin
  Shell_NotifyIcon (NIM_DELETE, @tnid);                { Löschen ... }
end;

initialization
  InitLanguage;  // 29.08.2007, WW

end.
