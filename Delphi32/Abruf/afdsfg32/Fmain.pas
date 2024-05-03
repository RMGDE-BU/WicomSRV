{******************************************************************************}
{* Unit: Hauptformular für DSfG-Abrufmodul                                    *}
{* 17.11.1999  WW                                                             *}
{ 07.08.2001  GD  Erweitert um Debug-Handling                                  }
{ 17.05.2002  GD  Vs. 2.42 Erweitert um Löschen von Abrufarten                 }
{ 13.12.2002  GD  Vs. 2.5  Um Füllen der Alarm-Tabelle erweitert               }
{ 18.04.2003  GD  Vs. 2.62 Paralleler Abruf mehrerer TCP/IP-Verbindungen (VCD) }
{ 31.05.2003  GD  Vs. 2.8  Archivabruf bei Momentanwertabruf                   }
{ 10.11.2003  GD  Vs. 2.92 Bugfix: Zeitbereich für aut. Export                 }
{ 03.02.2005  GD  Vs. 2.991 Bugfix: DSfG-Export-Zwischendatei eindeutiger      }
{ 23.02.2005  GD  Vs. 2.992 Instanzname jetzt aus Tabelle                      }
{******************************************************************************}
unit FMain;

interface

uses
  ShellApi, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, DBTables, Db, Buttons,
  Serial, SerDSfG, PathIni, ProgIni, DSysCon, DAbrfMan, DDBAbruf, AbrfInfo, AuftrgDb,
  WSysCon, TerminDb, DSysDat, SrvCfgIni, DZustand, KWLink32, DMoInit, DDbSta,
  LogCom, ModemIni, DB_Attn, TbDSfGMo, Menus, ImgList, Lizenz32, AsciiExportDLL,
  TCPIP_DSfG;

const
  WM_TASKBAREVENT = WM_USER + 1;
  WM_ABRUFSTATUS  = WM_USER + 1001;

type
  TFormMainDSfGAbruf = class(TForm)
    GBKonfiguration: TGroupBox;
    L1: TLabel;
    L3: TLabel;
    LSchnittstelle: TLabel;
    LRohdatenLoeschen: TLabel;
    AbrufTimer: TTimer;
    BtnAbrufStarten: TBitBtn;
    lMaxBaudrateStatic: TLabel;
    LMaxBaudrate: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lTxdByte: TLabel;
    lRxDByte: TLabel;
    lModemstatusStatic: TLabel;
    StatusBar: TStatusBar;
    eTxD: TEdit;
    eRxD: TEdit;
    Label4: TLabel;
    LOptionen: TLabel;
    lModemstatus: TLabel;
    MomHaltenTimer: TTimer;
    ImageList: TImageList;
    PopupMenu: TPopupMenu;
    pmMenueSchliessen: TMenuItem;
    N2: TMenuItem;
    mAnzeigen: TMenuItem;
    mBeenden: TMenuItem;
    ErrorTimer: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnAbrufStartenClick(Sender: TObject);
    procedure AbrufTimerTimer(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure MomHaltenTimerTimer(Sender: TObject);
    procedure ErrorTimerTimer(Sender: TObject);
    procedure mAnzeigenClick(Sender: TObject);
    procedure mBeendenClick(Sender: TObject);
  private
    { Private-Deklarationen }
    SerialDSfG: TSerialDSfG;
    COMNr: integer;
    ComLogFile: TComLogFile;
    WithLogFile: boolean;
    StatusColor: TColor;
    tnid : TNOTIFYICONDATA;
    ErrorCount: integer;
    RingCount: integer;
    MaxRings: integer;
    Rufentgegennahme_freigeschaltet: boolean;
    ClientSocketDSfG: TClientSocketDSfG;
    overIP: boolean;
    procedure TaskBarAddIcon;
    procedure TaskBarModifyIcon (anIconIndex: integer);
    procedure TaskBarRemoveIcon;
    procedure StartProgramm;
    procedure ActivateProgramm;
    procedure FensterPositionieren;
    procedure Initialisieren;
    procedure GetKommandozeilenParameter;
    procedure SetVersion;
    procedure SetKonfigLabels;
    procedure ReadFunktionsfreischaltung;
    procedure ReadSrvCfg32Ini;
    function CheckLogPhysCom_Zuordnung: boolean;
    procedure ReadProgramIni;
    procedure ReadMaxBaudrate;
    procedure RufAbfragen;
    procedure AbrufStarten;
    function GetAbrufData(var Abrufart: TAbrufart; var AbrufData: TAbrufData): boolean;
    procedure Abrufen (Abrufart: TAbrufart; var AbrufData: TAbrufData);
    procedure KonfigurationEinlesen (var AbrufData: TAbrufData);
    procedure MomentanwerteHolen (StationId: integer);
    procedure DFUEMomentanwerteHolen (StationId: integer);
    procedure RufReaktivieren (StationId: integer);
    procedure AktualisiereAuftragsListe(Abrufart: TAbrufart; AbrufData: TAbrufData);
    procedure Aufraeumen (Abrufart: TAbrufart; AbrufData: TAbrufData);
    procedure WMKWLink (var Message: TMessage); message WM_KWLink;
    procedure WMTASKBAREVENT(var message: TMessage); message WM_TASKBAREVENT;
    procedure WMABRUFSTATUS(var message: TMessage); message WM_ABRUFSTATUS;
    procedure TxDMessage (Msg: string; Bytes: string);
    procedure RxDMessage (Msg: string; Bytes: string);
    procedure TimeoutMessage (Msg: string);
  public
    { Public-Deklarationen }
    procedure ModemstatusMessage (Msg: string);
    procedure SetStatusColor (ProgStatus: integer);
    procedure SetError (OnOff: boolean);
  end;

var
  FormMainDSfGAbruf: TFormMainDSfGAbruf;

implementation

{$R *.DFM}

Const
  ProgramEnabled: boolean = false;        { Flag zum Aktivieren des Programms }
  Abrufzeit_erreicht: boolean = true;           { Flag zum Starten des Abrufs }
  JustStarted: boolean = true;            { Flag zum Prüfen auf Programmstart }
  FCanClose: boolean = false;          { Flag für Schließen/Verstecken des Hauptfensters }

{---------------------- Taskbarhandling-Methoden ------------------------------}

{------------------------------------------}
procedure TFormMainDSfGAbruf.TaskBarAddIcon;
{------------------------------------------}
begin
  tnid.cbSize := SizeOf(TNOTIFYICONDATA);
  tnid.Wnd := Handle;                                 { Handle des Hauptfensters }
  tnid.uID := 1;                                      { ID beliebig }
  tnid.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
  tnid.uCallbackMessage := WM_TASKBAREVENT;
  tnid.hIcon := Application.Icon.Handle;              { Handle des Programmicons }
  StrCopy (tnid.szTip, pchar (Application.Title));    { Tooltiptext }
  Shell_NotifyIcon (NIM_ADD, @tnid);                  { Registrieren ... }
end;

{---------------------------------------------}
procedure TFormMainDSfGAbruf.TaskBarModifyIcon;
{---------------------------------------------}
{ anderes Icon anzeigen;
  Übergabe: Icon-Index für ImageList }
begin
  ImageList.GetIcon (anIconIndex, Application.Icon);
  tnid.hIcon:=Application.Icon.Handle;
  Shell_NotifyIcon (NIM_MODIFY, @tnid);                { Ändern ... }
end;

{---------------------------------------------}
procedure TFormMainDSfGAbruf.TaskBarRemoveIcon;
{---------------------------------------------}
begin
  Shell_NotifyIcon (NIM_DELETE, @tnid);                { Löschen ... }
end;


{----------------------------- Programm-Methoden ------------------------------}

{--------------------------------------------------------}
procedure TFormMainDSfGAbruf.FormDestroy(Sender: TObject);
{--------------------------------------------------------}
begin
  if SerialDSfG <> nil then
    SerialDSfG.Disconnect;
  SerialDSfG.Free;
  ClientSocketDSfG.Free;
  ComLogFile.Free;
  PathServer.Free;
  AfDSfG32Ini.Free;
  TaskBarRemoveIcon;                    { Programm-Icon aus Taskbar entfernen }
end;

{----------------------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
{----------------------------------------------------------------------------------}
{ statt das Hauptformular zu schließen, wird es versteckt (schließen über ALM32) }
begin
  CanClose:=FCanClose;     { FCanClose wird in mBeendenClick auf true gesetzt }
  Hide;
end;

{-----------------------------------------}
procedure TFormMainDSfGAbruf.StartProgramm;
{-----------------------------------------}
var
  Erg: integer;
  Stammdaten: TDSfGStammdaten;
begin
  ZustandMessage (-1, -1, z_NichtAktiv, '', false);
  C_AktDSfGAbrufStatus := C_Status_Waiting;
  Initialisieren;                                      { Objekt-Vorbelegungen }
  GetKommandozeilenParameter;            { Kommandozeilen-Parameter auswerten }
  SetVersion;                                              { Version anzeigen }
  FensterPositionieren;

  ImageList.GetIcon (0, Application.Icon);              { Programm-Icon laden }
  if overIP then
    Application.Title:=sthTCP_IP + ' - ' + Application.Title
  else
    Application.Title:=ComName [COMNr-1] + ' - ' + Application.Title;
  TaskBarAddIcon;                        { TaskBar-Icon für Programm erzeugen }

  AfDSfG32Ini := TAfDSfG32Ini.Create;
  ReadProgramIni;
  PathServer := TPathServer.Create (AfDSfG32Ini.WieserIniFile,
                                   [WWorkDir,
                                    WNetWorkDir,
                                    WStammDir,
                                    WNetProgDir,
                                    DArchivDir,
                                    DManuDir,
                                    AsciiDir]);

  if not overIP then
    ReadMaxBaudrate;       { max. rechnerseitige Baudrate aus Modem.ini lesen }
  SetKonfigLabels;                           { Programmkonfiguration anzeigen }
  PathServer.Check;

  ReadFunktionsfreischaltung;
  Systemdaten:=GetDSfGAbrufSystemDaten (PathServer.PathName [WNetProgDir]);
  ReadSrvCfg32Ini;
  Stammdaten:=TDSfGStammdaten.Create (PathServer.PathName[WStammDir]);
  try
    if Stammdaten.InitTabellen then begin
      if WithLogFile then
        ComLogFile:=TComLogFile.Create (PathServer.PathName [WNetProgDir], COMNr, true)
      else
        ComLogFile:=nil;
      if overIP then begin        { DSfG über TCP/IP }
        { Client-Socket initialisieren: }
        ClientSocketDSfG:=TClientSocketDSfG.Create (self, PathServer.PathName [WWorkDir],
                                                    ComLogFile);
        ClientSocketDSfG.CBTxD:=TxDMessage;
        ClientSocketDSfG.CBRxD:=RxDMessage;
        ClientSocketDSfG.CBTimeout:=TimeoutMessage;
        { aus INI gelesene Versuche-Einstellungen für DSfG-Kommunikation
          an ClientSocketDSfG übergeben: }
        ClientSocketDSfG.SetVersuche (BCCVersuche);
        ClientSocketDSfG.Tag := COMNr;
        ProgramEnabled:=true;
      end
      else begin                  { DSfG über serielle Schnittstelle }
        { Schnittstelle initialisieren: }
        SerialDSfG:=TSerialDSfG.Create (self, PathServer.PathName [WWorkDir],
                                        ComLogFile);
        SerialDSfG.CBTxD:=TxDMessage;
        SerialDSfG.CBRxD:=RxDMessage;
        SerialDSfG.CBTimeout:=TimeoutMessage;
        SerialDSfG.CBModemstatus:=ModemstatusMessage;
        { aus INI gelesene Timeout- und Versuche-Einstellungen für DSfG-Kommunikation
          an SerialDSfG übergeben: }
        SerialDSfG.SetTimeouts_Versuche (Timeout_ModemAntwort, BCCVersuche);

        Erg:=SerialDSfG.Connect (COMNr, MaxBaudrate, db_8, none, sb_1);
        case Erg of
           0: ProgramEnabled:=true;
          -1: begin
                ZustandMessage (COMNr, -1, z_ComNichtVorhanden, '', false);
                SetError (true);
              end;
          -2: begin
                ZustandMessage (COMNr, -1, z_FehlerComOeffnen, '', false);
                SetError (true);
              end;
        end;

        if ProgramEnabled then begin              { Programm ist jetzt scharf }
          if not CheckLogPhysCom_Zuordnung then begin
            ZustandMessage (COMNr, -1, z_LogComNichtZugeordnet, '', false);
            SetError (true);
            ProgramEnabled:=false;
          end;
        end;
      end;
    end
    else begin
      ZustandMessage (-1, -1, z_NichtAktiv, '', false);
      SetError (true);
      MessageDlg('Eine oder mehrere Stammdaten-Tabellen nicht gefunden !', mtError, [mbOK], 0);
    end;
  finally
    Stammdaten.Free;
  end;
end;

{--------------------------------------------}
procedure TFormMainDSfGAbruf.ActivateProgramm;
{--------------------------------------------}
{ wird nach StartProgramm aufgerufen }
begin
  if ProgramEnabled then begin
    if not overIP then
      Modem_Initialisieren (SerialDSfG);

    Application.ProcessMessages;
    if not Application.Terminated then begin
      SetStatusColor (ps_KeineVerbindung);
      ZustandMessage (COMNr, -1, z_Bereit, '', false);
      SetError (false);

      { Funktionen aktivieren, wenn alles korrekt initialisiert ist: }
      AbrufTimer.Enabled:=true;
      BtnAbrufStarten.Enabled:=true;
    end;
  end;
end;

{------------------------------------------------}
procedure TFormMainDSfGAbruf.FensterPositionieren;
{------------------------------------------------}
{ alle Abrufmodulfenster überlappend }
begin
  if overIP then
    Left:=10
  else
    Left:=10 + COMNr * 20;
  Top:=Left;
end;

{------------------------------------------}
procedure TFormMainDSfGAbruf.Initialisieren;
{------------------------------------------}
{ Vorbelegungen der Objekt-Variablen }
begin
  COMNr:=1;                       { Com1 }
  overIP:=false;                  { Kommunikation über serielle Schnittstelle }
  WithLogFile:=false;
  SetStatusColor (ps_NichtAktiv);
  Rufentgegennahme_freigeschaltet:=false;
end;

{------------------------------------------------------}
procedure TFormMainDSfGAbruf.GetKommandozeilenParameter;
{------------------------------------------------------}
{ Kommandozeilenparameter auswerten }
var
  i, j: integer;
  S: string;
  Debug: boolean;
begin
  Debug:=false;
  Caption:=Caption + ' [';
  for i := 1 to ParamCount do begin
    S:=ParamStr(i);

    { Anzeige im Fenstertitel }
    if i > 1 then Caption:=Caption + ' ';
    Caption:=Caption + S;

    S:=UpperCase (S);
    { COMNr }
    for j:=Low (ComName) to High (ComName) do
      if S = UpperCase (ComName [j]) then
        COMNr:=j + 1;

    { TCP/IP-Kommunikation: }
    if (Pos(UpperCase (sthTCP_IP), S) > 0) then begin  // 18.04.2003
      overIP := true;
      COMNr := CComTCP_IP;  { Pseudo-COM-Nummer für TCP/IP-Kommunikation }
      if (Length(S) > Length(sthTCP_IP)) then
        COMNr := COMNr + StrToIntDef(Copy(S, Length(sthTCP_IP)+1, 4), 0);
    end;

    { Debug }
    if S = 'DEBUG' then begin
      Debug:=true;
      RohdatenLoeschen:=false;
      BtnAbrufStarten.Visible:=true;          { Schalter nur für Debug-Zwecke }
      IsDebugFlag := Debug;  // 07.08.2001
      SetASCIIDebugFlag(IsDebugFlag);
    end;
  end;  { for }
  Caption:=Caption + ']';

  if not Debug then
    mBeenden.Enabled:=false;              { Abrufmodul wird von ALM32 beendet }

end;

{--------------------------------------}
procedure TFormMainDSfGAbruf.SetVersion;
{--------------------------------------}
{ Version anzeigen }
begin
  Caption:=Caption + ' - Version 2.994, ' +
           FormatDateTime('dd.mm.yyyy', (FileDateToDateTime (FileAge (Application.ExeName))));
end;

{-------------------------------------------}
procedure TFormMainDSfGAbruf.SetKonfigLabels;
{-------------------------------------------}
begin
  if overIP then begin
    LSchnittstelle.Caption:=sthTCP_IP;
    LMaxBaudrateStatic.Visible:=false;
    LMaxBaudrate.Visible:=false;
    lModemstatusStatic.Visible:=false;
    lModemstatus.Visible:=false;
  end
  else begin
    LSchnittstelle.Caption:=ComName [COMNr-1];
    LMaxBaudrate.Caption:=IntToStr (MaxBaudrate);
  end;
  if RohdatenLoeschen then
    LRohdatenLoeschen.Caption:='ja'
  else
    LRohdatenLoeschen.Caption:='nein';
  LOptionen.Caption:='';
  {$IFDEF GAMESS}
  LOptionen.Caption:=LOptionen.Caption + '-GAMESS ';
  {$ENDIF}
  if LOptionen.Caption = '' then
    LOptionen.Caption:='keine';
end;

{------------------------------------------------------}
procedure TFormMainDSfGAbruf.ReadFunktionsfreischaltung;
{------------------------------------------------------}
{ im Lizenzfile enthaltene Funktionsfreischaltungen lesen }
var
  WLizenz32: TWLizenz32;
begin
  WLizenz32:=TWLizenz32.Create;
  try
    Rufentgegennahme_freigeschaltet:=
      WLizenz32.ReadProgrammfunktionFromLizenzFile (FNExt_Rufentgegennahme);
  finally
    WLizenz32.Free;
  end;
end;

{-------------------------------------------}
procedure TFormMainDSfGAbruf.ReadSrvCfg32Ini;
{-------------------------------------------}
{ SrvCfg32.Ini lesen }
var
  SI: TSrvCfg32Ini;

begin
  SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
  try
    WithLogFile:=SI.Protokoll;
    MaxRings:=SI.RingCount [COMNr];        { Ruf nach SI.RingCount annehmen }
  finally
    SI.Free;
  end;
end;

{-------------------------------------------------------------}
function TFormMainDSfGAbruf.CheckLogPhysCom_Zuordnung: boolean;
{-------------------------------------------------------------}
var
  SI: TSrvCfg32Ini;
  i: integer;
begin
  Result:=false;
  SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
  try
    SI.ReadLogPhysCom_Zuordnung;
  finally
    SI.Free;
  end;
  for i:= Low (LogPhysComZuordnung) to High (LogPhysComZuordnung) do begin
    if LogPhysComZuordnung [i] = COMNr then begin
      Result:=true;
      Break;
    end;
  end;
end;

{------------------------------------------}
procedure TFormMainDSfGAbruf.ReadProgramIni;
{------------------------------------------}
{ AfDSfG32.Ini lesen }
begin
  Timeout_ModemAntwort:=AfDSfG32Ini.TOModemAntwort;
  Timeout_ModemInit:=AfDSfG32Ini.TOModemInit;
  Timeout_Verbindungsaufbau:=AfDSfG32Ini.TOVerbindungsaufbau;
  Timeout_Verbindungsabbau:=AfDSfG32Ini.TOVerbindungsabbau;
  Timeout_RufAnnahme:=AfDSfG32Ini.TORufAnnahme;
  Timeout_Login:=AfDSfG32Ini.TOLogin;
  Timeout_DFUETransparent:=AfDSfG32Ini.TODFUETransparent;
  Timeout_Archive:=AfDSfG32Ini.TOArchive;
  Timeout_Logbuecher:=AfDSfG32Ini.TOLogbuecher;
  Timeout_Datenelemente:=AfDSfG32Ini.TODatenelemente;
  Timeout_KonfLesen:=AfDSfG32Ini.TOKonfLesen;
  Timeout_Einstellen:=AfDSfG32Ini.TOEinstellen;
  Timeout_DFUEParameter:=AfDSfG32Ini.TODFUEParameter;
  Timeout_Binaerdatei:=AfDSfG32Ini.TOBinaerdatei;

  BCCVersuche:=AfDSfG32Ini.BCCVersuche;

  AutomatikZeitabruf:=AfDSfG32Ini.AutomatikZeitabruf;
  AutomatikErsteDatenTage:=AfDSfG32Ini.AutomatikErsteDatenTage;

  FW_Zeitfenster:=AfDSfG32Ini.FW_Zeitfenster;    { für kavernenbezogene Archivdatenkonvertierung }
end;

{-------------------------------------------}
procedure TFormMainDSfGAbruf.ReadMaxBaudrate;
{-------------------------------------------}
{ max. rechnerseitige Baudrate aus Modem.ini lesen }
var
  ModemName: string;
  MI: TModemIni;
  SI: TSrvCfg32Ini;

begin
  MI:=TModemIni.Create (PathServer.Pathname [WNetProgDir]);
  try
    SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
    try
      ModemName:=SI.Modem [COMNr];
    finally
      SI.Free;
    end;
    MaxBaudrate:=MI.GetMaxBaud(ModemName);
  finally
    MI.Free;
  end;
end;

{---------------------------------------}
procedure TFormMainDSfGAbruf.RufAbfragen;
{---------------------------------------}
{ auf ankommenden Ruf prüfen, wenn Funktion freigeschaltet (nur bei serieller DFÜ-Kommunikation !)}
var
  Ruf_angekommen: boolean;
  RufManager: TRufManager;
  dummy: integer;
begin
  if not Rufentgegennahme_freigeschaltet OR overIP then exit;

  ZustandMessage (-1, -1, z_Rufabfragen, '', false);
  if SerialDSfG.Rufabfrage (Ruf_angekommen, dummy, dummy) then begin
    if Ruf_angekommen then begin
      inc (RingCount);
      if RingCount >= MaxRings then begin
        { angekommenen Ruf erst nach MaxRings entgegennehmen: }
        RingCount:=0;
        Systemdaten:=GetDSfGAbrufSystemDaten (PathServer.PathName [WNetProgDir]);
        C_AktDSfGAbrufStatus := C_Status_RufAbfragen;
        RufManager:=TRufManager.Create (SerialDSfG, aa_ruf);
        try
          RufManager.RufEntgegennehmen;
        finally
          RufManager.Free;
          C_AktDSfGAbrufStatus := C_Status_Waiting;
        end;
      end;
    end;
  end else
    Modem_Initialisieren (SerialDSfG);
  ZustandMessage (-1, -1, z_Bereit, '', false);
end;

{----------------------------------------}
procedure TFormMainDSfGAbruf.AbrufStarten;
{----------------------------------------}
{ DSfG-Abruftabelle abarbeiten; vor Beenden des Programms müssen alle noch in der DSfG-Abruf-
  Tabelle enthaltenen (und somit noch nicht abgerufenen Aufträge) gelöscht und in der
  Auftragstabelle zurückgesetzt werden ! }
var
  Abrufart: TAbrufart;
  AbrufData: TAbrufData;

begin
  while GetAbrufData (Abrufart, AbrufData) do begin
    Application.ProcessMessages;
    if not Application.Terminated then begin
      case Abrufart of
        aa_automatisch,
        aa_manuell:    begin
                         try
                           C_AktDSfGAbrufStatus := C_Status_Abruf;
                           Abrufen (Abrufart, AbrufData);
                         finally
                           AktualisiereAuftragsListe (Abrufart, AbrufData);
                           C_AktDSfGAbrufStatus := C_Status_Waiting;
                         end;
                       end;
        aa_konflesen:  begin
                       try
                         C_AktDSfGAbrufStatus := C_Status_Konfiguration;
                         KonfigurationEinlesen (AbrufData);
                       finally
                         AktualisiereAuftragsListe (Abrufart, AbrufData);
                         C_AktDSfGAbrufStatus := C_Status_Waiting;
                       end;
                     end;
        aa_momentan: MomentanwerteHolen (AbrufData.StationId);
        aa_dfue_momentan: DFUEMomentanwerteHolen (AbrufData.StationId);
        aa_rufreakt: RufReaktivieren (AbrufData.StationId);
      end;

      if not Application.Terminated then
        RufAbfragen;                             { auf ankommenden Ruf prüfen }
    end else
      Aufraeumen (Abrufart, AbrufData);
  end;  { while }
end;

{---------------------------------------------------------------------------}
function TFormMainDSfGAbruf.GetAbrufData(var Abrufart: TAbrufart;
                                         var AbrufData: TAbrufData): boolean;
{---------------------------------------------------------------------------}
{ Abrufdaten zusammenstellen, Abrufart ermitteln (Auto/Manu/Momentan etc.) }
var
  DSfGAbruf: TDSfGAbruf;
  SI: TSrvCfg32Ini;

begin
  Result:=false;
  { vor jedem Stationsabruf lesen: AFDSFG32.INI, Schnittstellenzuordnung: logisch <-> physikalisch,
    Systemdaten }
  ReadProgramIni;
  if not overIP then begin        { nur bei Abruf über serielle Schnittstelle }
    SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
    try
      SI.ReadLogPhysCom_Zuordnung;
    finally
      SI.Free;
    end;
  end;
  Systemdaten:=GetDSfGAbrufSystemDaten (PathServer.PathName [WNetProgDir]);

  DSfGAbruf:=TDSfGAbruf.Create (PathServer.PathName [WStammDir]);
  try
    if DSfGAbruf.GetDSfGAbruf (COMNr, Abrufart, AbrufData) then
      Result:=true;
  finally
    DSfGAbruf.Free;
  end;
end;

{------------------------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.Abrufen (Abrufart: TAbrufart; var AbrufData: TAbrufData);
{------------------------------------------------------------------------------------}
{ DSfG-Station abrufen }
var
  AutoManuAbrufManager: TAutoManuAbrufManager;

begin
  if overIP then     { DSfG über TCP/IP }
    AutoManuAbrufManager:=TAutoManuAbrufManager.Create (ClientSocketDSfG, Abrufart, AbrufData)
  else               { DSfG über serielle Schnittstelle }
    AutoManuAbrufManager:=TAutoManuAbrufManager.Create (SerialDSfG, Abrufart, AbrufData);
  try
    AutoManuAbrufManager.Abrufen;
    AbrufData:=AutoManuAbrufManager.AbrufData;              { Rückgabe: mit Abrufergebnis }
  finally
    AutoManuAbrufManager.Free;
  end;
end;

{-----------------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.KonfigurationEinlesen (var AbrufData: TAbrufData);
{-----------------------------------------------------------------------------}
{ Konfiguration der DSfG-Station einlesen }
var
  KonfLesenAbrufManager: TKonfLesenAbrufManager;

begin
  if overIP then     { DSfG über TCP/IP }
    KonfLesenAbrufManager:=TKonfLesenAbrufManager.Create (ClientSocketDSfG, AbrufData)
  else               { DSfG über serielle Schnittstelle }
    KonfLesenAbrufManager:=TKonfLesenAbrufManager.Create (SerialDSfG, AbrufData);
  try
    KonfLesenAbrufManager.Abrufen;
    AbrufData:=KonfLesenAbrufManager.AbrufData;              { Rückgabe: mit Abrufergebnis }
  finally
    KonfLesenAbrufManager.Free;
  end;
end;

{-------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.MomentanwerteHolen (StationId: integer);
{-------------------------------------------------------------------}
{ Momentanwerte einer DSfG-Station abrufen }
var
  MomentanAbrufManager: TMomentanAbrufManager;

begin
  C_AktDSfGAbrufStatus := C_Status_Momentanwerte;
  MomHaltenTimer.Enabled:=true;
  try
    { Triggerfile für Momentanwertetabelle schreiben
      -> Info für Momentanwert-Client "Momentanwerteabruf läuft bereits" }
    WriteNewTime (PathServer.PathName [WNetWorkDir]+ C_TbDMom + Format('%.4d.DB', [StationId]));
    if overIP then       { DSfG über TCP/IP }
      MomentanAbrufManager:=TMomentanAbrufManager.Create (ClientSocketDSfG, StationId)
    else                 { DSfG über serielle Schnittstelle }
      MomentanAbrufManager:=TMomentanAbrufManager.Create (SerialDSfG, StationId);
    try
      MomentanAbrufManager.Abrufen;
    finally
      MomentanAbrufManager.Free;
    end;
  finally
    C_AktDSfGAbrufStatus := C_Status_Waiting;
    MomHaltenTimer.Enabled:=false;
  end;
end;

{-----------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.DFUEMomentanwerteHolen (StationId: integer);
{-----------------------------------------------------------------------}
{ DSfG-DFÜ-Momentanwerte der Station holen und parametrieren }
var
  DFUEMomentanAbrufManager: TDFUEMomentanAbrufManager;

begin
  C_AktDSfGAbrufStatus := C_Status_MomentanDfue;
  MomHaltenTimer.Enabled:=true;
  try
    if overIP then          { DSfG über TCP/IP }
      DFUEMomentanAbrufManager:=TDFUEMomentanAbrufManager.Create (ClientSocketDSfG, StationId)
    else                    { DSfG über serielle Schnittstelle }
      DFUEMomentanAbrufManager:=TDFUEMomentanAbrufManager.Create (SerialDSfG, StationId);
    try
      DFUEMomentanAbrufManager.Abrufen;
    finally
      DFUEMomentanAbrufManager.Free;
    end;
  finally
    C_AktDSfGAbrufStatus := C_Status_Waiting;
    MomHaltenTimer.Enabled:=false;
  end;
end;

{----------------------------------------------------------------}
procedure TFormMainDSfGAbruf.RufReaktivieren (StationId: integer);
{----------------------------------------------------------------}
{ Ruffunktion in einer DSfG-Station reaktivieren }
var
  RufReaktManager: TRufReaktManager;

begin
  if overIP then          { DSfG über TCP/IP }
    RufReaktManager:=TRufReaktManager.Create (ClientSocketDSfG, StationId)
  else                    { DSfG über serielle Schnittstelle }
    RufReaktManager:=TRufReaktManager.Create (SerialDSfG, StationId);
  try
    RufReaktManager.Abrufen;
  finally
    RufReaktManager.Free;
  end;
end;

{----------------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.AktualisiereAuftragsListe(Abrufart: TAbrufart;
                                                       AbrufData: TAbrufData);
{----------------------------------------------------------------------------}
{ Auftragsliste aktualisieren }
var
  AuftragDb: TAuftragDb;
  TerminDb: TTerminDb;
  NextTermin: TDateTime;
  NextDatentypen: integer;

begin
  AuftragDb:=TAuftragDb.Create (PathServer.PathName [WStammDir],
                                PathServer.PathName [WNetProgDir]);
  try
    TerminDb:=TTerminDb.Create (PathServer.PathName [WStammDir],
                                PathServer.PathName [WNetProgDir]);
    try
      if AbrufData.StationId <> 0 then begin
        if Abrufart = aa_automatisch then begin
          NextDatentypen:=AbrufData.Datentypen;  { Vorbelegung für Update des Auftrags }

          if AbrufData.Erfolgreich then begin                             { Abruf ok }
            { nächster Termin }
            if TerminDb.GetNextTermin (C_GerArtDSfG, AbrufData.StationId,
                                       NextTermin, NextDatentypen) then
              AuftragDb.UpdateAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto,
                                       NextTermin, NextDatentypen, 0, true)
            else
              AuftragDb.DeleteAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto);
          end
          else begin                                      { Fehler im Abruf }
            if AbrufData.Keine_weiteren_Versuche then begin
              { keine Wiederholung (Kennung, Paßwort falsch): nächster Termin,
                Auftrag aktiv/inaktiv je nach Systemdaten-Einstellung (ab 17.07.2001) }
              if TerminDb.GetNextTermin (C_GerArtDSfG, AbrufData.StationId,
                                         NextTermin, NextDatentypen) then begin
                if Systemdaten.StationDeaktivierung then
                  AuftragDb.UpdateAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto,
                                           NextTermin, NextDatentypen,
                                           AbrufData.Anrufversuch, false)
                else
                  AuftragDb.UpdateAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto,
                                           NextTermin, NextDatentypen,
                                           0, true);
              end else
                AuftragDb.DeleteAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto);
            end
            else begin
              if GetNextWiederholung (AbrufData.Anrufversuch, NextTermin) then
                { Wiederholung: nächster Abrufversuch nach Wartezeit }
                AuftragDb.UpdateAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto,
                                         NextTermin,
                                         AbrufData.Datentypen,
                                         AbrufData.Anrufversuch, true)
              else begin
                { max. Wiederholung erreicht: nächster Termin, Auftrag aktiv/inaktiv
                  je nach Systemdaten-Einstellung (ab 17.07.2001) }
                if TerminDb.GetNextTermin (C_GerArtDSfG, AbrufData.StationId,
                                           NextTermin, NextDatentypen) then begin
                  if Systemdaten.StationDeaktivierung then
                    AuftragDb.UpdateAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto,
                                             NextTermin, NextDatentypen,
                                             AbrufData.Anrufversuch, false)
                  else
                    AuftragDb.UpdateAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto,
                                             NextTermin, NextDatentypen,
                                             0, true);
                end else
                  AuftragDb.DeleteAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto);
              end;
            end;
          end;
        end
        else if Abrufart = aa_manuell then
          AuftragDb.DeleteAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtManu)         { manuellen Auftrag löschen }
        else if Abrufart = aa_konflesen then
          AuftragDb.DeleteAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtKonfLesen);   { Konf.lese-Auftrag löschen }
      end;
    finally
      TerminDb.Free;
    end;
  finally
    AuftragDb.Free;
  end;
end;

{-----------------------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.Aufraeumen (Abrufart: TAbrufart; AbrufData: TAbrufData);
{-----------------------------------------------------------------------------------}
{ Einträge aus DSfG-Abruftabelle löschen und in Auftragstabelle zurücksetzen }
var
  AuftragDb: TAuftragDb;

begin
  AuftragDb:=TAuftragDb.Create (PathServer.PathName [WStammDir],
                                PathServer.PathName [WNetProgDir]);
  try
    if AbrufData.StationId <> 0 then begin   // 17.05.2002
      if (Abrufart = aa_automatisch) then
        { ursprünglichen Auftrag zurückschreiben }
        AuftragDb.UpdateAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto,
                                 -1, -1, 0, true)
      else if (Abrufart = aa_manuell) then    { manueller Abruf }
        { Auftrag löschen }
        AuftragDb.DeleteAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtManu)
      else if (Abrufart = aa_momentan) then   { Momentanwert-Abruf }
        { Auftrag löschen }
        AuftragDb.DeleteAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtMomStart)
      else if (Abrufart = aa_dfue_momentan) then   { DFÜ-Parametrierung-Abruf }
        { Auftrag löschen }
        AuftragDb.DeleteAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtMomDfueStart)
    end;
  finally
    AuftragDb.Free;
  end;
end;


{------------------------- Message-Behandlungsmethoden ------------------------}

{------------------------------------------------------------}
procedure TFormMainDSfGAbruf.WMKWLink (var Message: TMessage);
{------------------------------------------------------------}
begin
  case Message.WParam of
    KWE_DSfGAbrufzeit_erreicht:       { Abruf automatisch anstoßen, wenn Botschaft empfangen }
    begin
      Abrufzeit_erreicht:=true;
    end;

    KWE_AbrufManager_Beendet:         { Programm beenden, wenn Botschaft empfangen }
    begin
      mBeendenClick (Self);
    end;
  end;
end;

{-----------------------------------------------------------------}
procedure TFormMainDSfGAbruf.WMTASKBAREVENT(var message: TMessage);
{-----------------------------------------------------------------}
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

{-----------------------------------------------------------------}
procedure TFormMainDSfGAbruf.WMABRUFSTATUS(var message: TMessage);
{-----------------------------------------------------------------}
begin
  message.Result := C_AktDSfGAbrufStatus;
end;

{-----------------------------------------------------------------}
procedure TFormMainDSfGAbruf.BtnAbrufStartenClick(Sender: TObject);
{-----------------------------------------------------------------}
{ Abruf manuell anstoßen (nur für Testzwecke) }
begin
  Abrufzeit_erreicht:=true;
end;


{ -------------------------- Timer-Methoden -----------------------------------}

{------------------------------------------------------------}
procedure TFormMainDSfGAbruf.AbrufTimerTimer(Sender: TObject);
{------------------------------------------------------------}
{ zyklische Überprüfung, ob Abruf gestartet werden soll }
begin
  AbrufTimer.Enabled:=false;

  if JustStarted then begin
    JustStarted:=false;
    Hide;
    AbrufTimer.Interval:=1000;
    StartProgramm;
    ActivateProgramm;
    exit;
  end;

  try
    RufAbfragen;                                 { auf ankommenden Ruf prüfen }
    if Abrufzeit_erreicht then begin
      Abrufzeit_erreicht:=false;
      AbrufStarten;
    end;
  finally
    AbrufTimer.Enabled:=true;
  end;
end;

{----------------------------------------------------------------}
procedure TFormMainDSfGAbruf.MomHaltenTimerTimer(Sender: TObject);
{----------------------------------------------------------------}
{ zyklische Überprüfung, ob Momentanwerte-Verbindung weiter gehalten werden soll }
begin
  CheckMomHalten := True;  { Flag setzen }
end;

{------------------------------------------------------------}
procedure TFormMainDSfGAbruf.ErrorTimerTimer(Sender: TObject);
{------------------------------------------------------------}
begin
  if ImageList.Tag = 2 then
    ImageList.Tag:= 1                       { Fehler-Icon }
  else
    ImageList.Tag:= 2;                      { transparentes Icon }
  TaskBarModifyIcon (ImageList.Tag);

  { akustische Ausgabe nur bei jedem 20. Aufruf (ein-/ausschaltbar über ALM-
    Systemeinstellungen }
  if Systemdaten.ErrorBeep then begin
    inc (ErrorCount);
    if ErrorCount >= 20 then begin
      ErrorCount:=0;
      Beep;
    end;
  end;
end;


{-------------------------- Anzeige-Methoden ----------------------------------}

{-------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.TxDMessage (Msg: string; Bytes: string);
{-------------------------------------------------------------------}
{ Ausgabe der gesendeten Zeichen und deren Anzahl;
  Übergabe: Msg-String }
begin
  eTxD.Text:=Msg;
  lTxDByte.Caption:=Bytes;
end;

{-------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.RxDMessage (Msg: string; Bytes: string);
{-------------------------------------------------------------------}
{ Ausgabe der empfangenen Zeichen und deren Anzahl;
  Übergabe: Msg-String }
begin
  eRxD.Text:=Msg;
  lRxDByte.Caption:=Bytes;
end;

{--------------------------------------------------------}
procedure TFormMainDSfGAbruf.TimeoutMessage (Msg: string);
{--------------------------------------------------------}
{ Ausgabe des Timeouts der momentan laufenden Aktion
  Übergabe: Msg-String }
begin
  Statusbar.Panels [4].Text:=Msg;
  Application.ProcessMessages;
end;

{------------------------------------------------------------}
procedure TFormMainDSfGAbruf.ModemstatusMessage (Msg: string);
{------------------------------------------------------------}
{ Ausgabe des momentanen Modemstatus (Connect, No Carrier etc.)
  Übergabe: Msg-String }
begin
  lModemstatus.Caption:=Msg;
  Application.ProcessMessages;

  { leere Modemstatus-Message -> keine Verbindung }
  if length(Msg) = 0 then
    SetStatuscolor (ps_KeineVerbindung);
end;

{----------------------------------------------------------------}
procedure TFormMainDSfGAbruf.SetStatusColor (ProgStatus: integer);
{----------------------------------------------------------------}
{ Statusfarbe in der Statusbar je nach Programmstatus setzen
  Übergabe: Programstatus-Konstante }
begin
  if (ProgStatus >= Low (cl_Programmstatus)) AND
     (ProgStatus <= High (cl_Programmstatus)) then begin
    StatusColor:=cl_Programmstatus [ProgStatus];
    Statusbar.Invalidate;
  end;
end;

{--------------------------------------------------------------------}
procedure TFormMainDSfGAbruf.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
{--------------------------------------------------------------------}
{ Statusfarbe in Statusbar setzen }
begin
  if Panel.Index = 0 then begin
    with StatusBar.Canvas do begin
      Brush.Color:=StatusColor;
      FillRect (Rect);
    end;
  end;
end;

{-----------------------------------------------------}
procedure TFormMainDSfGAbruf.SetError (OnOff: boolean);
{-----------------------------------------------------}
{ akustische und optische Fehlerausgabe aktivieren/deaktivieren }
begin
  if OnOff <> ErrorTimer.Enabled then
    ErrorCount:=99999;
  ErrorTimer.Enabled:=OnOff;                 

  if not OnOff then             { Fehlerausgabe deaktiviert: OK-Icon anzeigen }
    TaskBarModifyIcon (0);
end;


{--------------------- Popupmenü-Methoden -------------------------------------}

{-----------------------------------------------------------}
procedure TFormMainDSfGAbruf.mAnzeigenClick(Sender: TObject);
{-----------------------------------------------------------}
begin
  Show;
  Application.BringToFront;
end;

{----------------------------------------------------------}
procedure TFormMainDSfGAbruf.mBeendenClick(Sender: TObject);
{----------------------------------------------------------}
begin
  Screen.Cursor:=crHourGlass;
  FCanClose:=true;
  Close;
end;

end.

