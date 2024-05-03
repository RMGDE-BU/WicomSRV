{******************************************************************************}
{* Unit: Hauptformular für MRG-Abrufmodul                                     *}
{* 29.12.1998  WW                                                             *}
{                                                                              }
{ 27.04.1999 GD; Umstellung auf WNetProgDir                                    }
{ 23.05.2003  GD  Anpassung ASCII-Export an geändertes Rohdatenformat          }
{******************************************************************************}
unit FMain;

interface

uses
  ShellApi, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, DBTables, Db, Buttons,
  T_Tools, PathIni, ProgIni, MDBSta, MSysCon, MAbrfMan, MDBAbruf, AbrfInfo,
  AuftrgDb, WSysCon, TerminDb, MSysDat, MZustand, KWLink32, Db_Attn, LGZType,
  Serial, SerMrg, SerMrgFup, SerMrgModem, LogCom, SrvCfgIni, MModemInit, ModemIni,
  ImgList, Menus, Lizenz32, LGZUtil;

const
  WM_TASKBAREVENT = WM_USER + 1;

type
  TFormMainMRGAbruf = class(TForm)
    GBKonfiguration: TGroupBox;
    L1: TLabel;
    L2: TLabel;
    L3: TLabel;
    LSchnittstelle: TLabel;
    LThema: TLabel;
    LRohdatenLoeschen: TLabel;
    AbrufTimer: TTimer;
    MomHaltenTimer: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    LOptionen: TLabel;
    LKanalzahl: TLabel;
    BtnAbrufStarten: TBitBtn;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    lTxdByte: TLabel;
    lRxDByte: TLabel;
    lStaticStatus: TLabel;
    lStatus: TLabel;
    eTxD: TEdit;
    eRxD: TEdit;
    StatusBar: TStatusBar;
    lMaxBaudrateStatic: TLabel;
    LMaxBaudrate: TLabel;
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
    procedure MomHaltenTimerTimer(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure mBeendenClick(Sender: TObject);
    procedure mAnzeigenClick(Sender: TObject);
    procedure ErrorTimerTimer(Sender: TObject);
  private
    { Private-Deklarationen }
    SerialMRG: TSerialMRG;
    COMNr: byte;
    Thema: string;
    ComLogFile: TComLogFile;
    WithLogFile: boolean;
    StatusColor: TColor;
    Stammdaten: TMRGStammdaten;
    RingCount: integer;
    MaxRings: integer;
    MaxBaudrate: integer;
    ModemRuf: integer;
    tnid : TNOTIFYICONDATA;
    ErrorCount: integer;
    Rufentgegennahme_freigeschaltet: boolean;
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
    procedure InitKanalzahl;
    procedure ReadFunktionsfreischaltung;
    procedure ReadSrvCfg32Ini;
    function CheckLogPhysCom_Zuordnung: boolean;
    procedure ReadProgramIni;
    procedure ReadMaxBaudrate;
    procedure FUP_Modem_initialisieren_nach_Abruf;
    procedure RufAbfragen;
    procedure AbrufStarten;
    function GetAbrufListe(var Abrufart: TAbrufart; var AbrufListe: TAbrufListe): boolean;
    procedure Abrufen (Abrufart: TAbrufart; var AbrufListe: TAbrufListe);
    procedure MomentanwerteHolen (MrgId: TMrgId);
    procedure RufReaktivieren (MrgId: TMrgId);
    procedure RueckRufAusloesen (MrgId: TMrgId; Zentrale: integer);
    procedure AktualisiereAuftragsListe(Abrufart: TAbrufart; AbrufListe: TAbrufListe);
    procedure Aufraeumen (Abrufart: TAbrufart; AbrufListe: TAbrufListe);
    procedure WMKWLink (var Message: TMessage); message WM_KWLink;
    procedure WMTASKBAREVENT(var message: TMessage); message WM_TASKBAREVENT;
    procedure TxDMessage (Msg: string; Bytes: string);
    procedure RxDMessage (Msg: string; Bytes: string);
    procedure TimeoutMessage (Msg: string);
  public
    { Public-Deklarationen }
    procedure ModemstatusMessage (Msg: string);
    procedure FupVersionMessage (Msg: string);
    procedure SetStatusColor (ProgStatus: integer);
    procedure SetError (OnOff: boolean);
  end;

var
  FormMainMRGAbruf: TFormMainMRGAbruf;

implementation

{$R *.DFM}

Const
  ProgramEnabled: boolean = false;        { Flag zum Aktivieren des Programms }
  Abrufzeit_erreicht: boolean = true;           { Flag zum Starten des Abrufs }
  JustStarted: boolean = true;            { Flag zum Prüfen auf Programmstart }
  FCanClose: boolean = false;          { Flag für Schließen/Verstecken des Hauptfensters }


{---------------------- Taskbarhandling-Methoden ------------------------------}

{-----------------------------------------}
procedure TFormMainMRGAbruf.TaskBarAddIcon;
{-----------------------------------------}
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

{-------------------------------------------------------------------}
procedure TFormMainMRGAbruf.TaskBarModifyIcon (anIconIndex: integer);
{-------------------------------------------------------------------}
{ anderes Icon anzeigen;
  Übergabe: Icon-Index für ImageList }
begin
  ImageList.GetIcon (anIconIndex, Application.Icon);
  tnid.hIcon:=Application.Icon.Handle;
  Shell_NotifyIcon (NIM_MODIFY, @tnid);                { Ändern ... }
end;

{--------------------------------------------}
procedure TFormMainMRGAbruf.TaskBarRemoveIcon;
{--------------------------------------------}
begin
  Shell_NotifyIcon (NIM_DELETE, @tnid);                { Löschen ... }
end;


{----------------------------- Programm-Methoden ------------------------------}

{-------------------------------------------------------}
procedure TFormMainMRGAbruf.FormDestroy(Sender: TObject);
{-------------------------------------------------------}
begin
  if SerialMRG <> nil then
    SerialMRG.Disconnect;
  SerialMRG.Free;
  ComLogFile.Free;
  Stammdaten.Free;
  PathServer.Free;
  AfMRG32Ini.Free;
  TaskBarRemoveIcon;                    { Programm-Icon aus Taskbar entfernen }
end;

{---------------------------------------------------------------------------------}
procedure TFormMainMRGAbruf.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
{---------------------------------------------------------------------------------}
{ statt das Hauptformular zu schließen, wird es versteckt (schließen über ALM32) }
begin
  CanClose:=FCanClose;     { FCanClose wird in mBeendenClick auf true gesetzt }
  Hide;
end;

{----------------------------------------}
procedure TFormMainMRGAbruf.StartProgramm;
{----------------------------------------}
var
  Erg: integer;
  br: TBaudrate;

begin
  ZustandMessage (-1, -1, z_NichtAktiv, '', false);
  Initialisieren;                                      { Objekt-Vorbelegungen }
  GetKommandozeilenParameter;            { Kommandozeilen-Parameter auswerten }
  SetVersion;                                              { Version anzeigen }
  FensterPositionieren;

  ImageList.GetIcon (0, Application.Icon);              { Programm-Icon laden }
  Application.Title:=ComName [COMNr-1] + ' - ' + Application.Title;
  TaskBarAddIcon;                        { TaskBar-Icon für Programm erzeugen }

  AfMRG32Ini := TAfMRG32Ini.Create;
  ReadProgramIni;
  PathServer := TPathServer.Create (AfMRG32Ini.WieserIniFile,
                                   [LangzeitDir,
                                    ManuDir,
                                    AsciiDir,
                                    WWorkDir,
                                    WNetWorkDir,
                                    WStammDir,
                                    WNetProgDir]);

  InitKanalzahl;                                   { Kanalzahl aus WIESER.INI }
  if Thema = Themen [thMRGModem] then
    ReadMaxBaudrate;       { max. rechnerseitige Baudrate aus Modem.ini lesen }
  SetKonfigLabels;                           { Programmkonfiguration anzeigen }
  PathServer.Check;

  ReadFunktionsfreischaltung;
  Systemdaten:=GetMrgAbrufSystemDaten (PathServer.PathName [WNetProgDir]);
  ReadSrvCfg32Ini;
  { Kanalzahl der Langzeitdaten prüfen: }
  if CheckLGZKanalzahl (PathServer.MaxKanaele,
                        PathServer.PathName [LangzeitDir]) then begin
    Stammdaten:=TMRGStammdaten.Create (PathServer.PathName [WStammDir]);
    if Stammdaten.InitTabellen then begin
      { Schnittstelle initialisieren: }
      if WithLogFile then
        ComLogFile:=TComLogFile.Create (PathServer.PathName [WNetProgDir], COMNr, true)
      else
        ComLogFile:=nil;
      if Thema = Themen [thMRGFup] then begin
        SerialMRG:=TSerialMRGFup.Create (self, PathServer.PathName [WWorkDir],
                                         ComLogFile);
        { aus INI gelesene Timeout-Einstellungen für MRG-FUP-Kommunikation
          an SerialMRGFup übergeben: }
        TSerialMRGFup (SerialMRG).SetTimeout_FupAntwort (Timeout_FupAntwort);
        Erg:=SerialMRG.Connect (COMNr, br_MRGStandard, db_8, none, sb_1);
        lStaticStatus.Caption:='FUP-Version:';
      end
      else begin
        SerialMRG:=TSerialMRGModem.Create (self, PathServer.PathName [WWorkDir],
                                           ComLogFile);
        { Timeout- und Versuche-Einstellungen für MRG-Modem-Kommunikation
          an SerialMRGModem übergeben: }
        TSerialMRGModem (SerialMRG).SetTimeouts (Timeouts_MRGModem);
        TSerialMRGModem (SerialMRG).SetVersuche (Versuche_MRGModem);

        TSerialMRGModem (SerialMRG).MaxModemBaudrate:=SerialMRG.KonvertBaudrate (MaxBaudrate);
        { Baudrate für Modem-Betrieb: }
        if ModemRuf = mr_Modem then  { Rufentgegennahme für Modemgeräte mit max. möglicher Baudrate }
          br:=TSerialMRGModem (SerialMRG).MaxModemBaudrate
        else                         { Rufentgegennahme für FUP-Geräte mit fester Standard-Baudrate 9600 }
          br:=br_MRGStandard;
        Erg:=SerialMRG.Connect (COMNr, br, db_7, even, sb_1);
        lStaticStatus.Caption:='Modemstatus:';
      end;
      SerialMRG.CBTxD:=TxDMessage;
      SerialMRG.CBRxD:=RxDMessage;
      SerialMRG.CBTimeout:=TimeoutMessage;
      SerialMRG.CBModemstatus:=ModemstatusMessage;
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
      if ProgramEnabled then begin                     { Com ist jetzt scharf }
        if not CheckLogPhysCom_Zuordnung then begin
          ZustandMessage (COMNr, -1, z_LogComNichtZugeordnet, '', false);
          SetError (true);
          ProgramEnabled:=false;
        end;
      end;
    end
    else begin
      ZustandMessage (-1, -1, z_NichtAktiv, '', false);
      SetError (true);
      MessageDlg('Eine oder mehrere Stammdaten-Tabellen nicht gefunden !', mtError, [mbOK], 0);
    end;
  end
  else begin
    ZustandMessage (-1, -1, z_NichtAktiv, '', false);
    SetError (true);
    MessageDlg('Falsche Kanalzahl in WIESER.INI eingestellt !', mtError, [mbOK], 0);
  end;
end;

{-------------------------------------------}
procedure TFormMainMRGAbruf.ActivateProgramm;
{-------------------------------------------}
{ wird nach StartProgramm aufgerufen }
begin
  if ProgramEnabled then begin
    if Thema = Themen [thMRGFup] then
      Fup_Reset (TSerialMRGFup (SerialMRG))
    else
      Modem_Initialisieren (TSerialMRGModem (SerialMRG), false);

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

{-----------------------------------------------}
procedure TFormMainMRGAbruf.FensterPositionieren;
{-----------------------------------------------}
{ alle Abrufmodulfenster überlappend }
begin
  Left:=10 + COMNr * 20;
  Top:=Left;
end;

{-----------------------------------------}
procedure TFormMainMRGAbruf.Initialisieren;
{-----------------------------------------}
{ Vorbelegungen der Objekt-Variablen }
begin
  COMNr:=1;                           { Com1 }
  Thema:=Themen [thMRGFup];           { Thema = MRG-FUP }
  WithLogFile:=false;
  RingCount:=0;
  MaxBaudrate:=0;
  SetStatusColor (ps_NichtAktiv);
  Rufentgegennahme_freigeschaltet:=false;
end;

{-----------------------------------------------------}
procedure TFormMainMRGAbruf.GetKommandozeilenParameter;
{-----------------------------------------------------}
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
    { Thema: Fup oder Modem }
    if S = UpperCase (Themen [thMRGFup]) then
      Thema:=Themen [thMRGFup];
    if S = UpperCase (Themen [thMRGModem]) then
      Thema:=Themen [thMRGModem];

    { Debug }
    if S = 'DEBUG' then begin
      Debug:=true;
      RohdatenLoeschen:=false;
      BtnAbrufStarten.Visible:=true;          { Schalter nur für Debug-Zwecke }
    end;
  end;  { for }
  Caption:=Caption + ']';

  if not Debug then
    mBeenden.Enabled:=false;              { Abrufmodul wird von ALM32 beendet }
end;

{-------------------------------------}
procedure TFormMainMRGAbruf.SetVersion;
{-------------------------------------}
{ Version anzeigen }
begin
  Caption:=Caption + ' - Version 2.59, ' +
           FormatDateTime('dd.mm.yyyy', (FileDateToDateTime (FileAge (Application.ExeName))));
end;

{------------------------------------------}
procedure TFormMainMRGAbruf.SetKonfigLabels;
{------------------------------------------}
begin
  LSchnittstelle.Caption:=ComName [COMNr-1];
  if Thema = Themen [thMRGModem] then begin
    LMaxBaudrate.Caption:=IntToStr (MaxBaudrate);
    LMaxBaudrateStatic.Visible:=true;
    LMaxBaudrate.Visible:=true;
  end;
  LThema.Caption:=Thema;
  if RohdatenLoeschen then
    LRohdatenLoeschen.Caption:='ja'
  else
    LRohdatenLoeschen.Caption:='nein';
  LOptionen.Caption:='';
  {$IFDEF GVT}
  LOptionen.Caption:=LOptionen.Caption + '-GVT ';
  {$ENDIF}
  {$IFDEF NATGAS}
  LOptionen.Caption:=LOptionen.Caption + '-NATGAS ';
  {$ENDIF}
  if LOptionen.Caption = '' then
    LOptionen.Caption:='keine';
  LKanalzahl.Caption:=IntToStr(V_MaxKanalZahl);
end;

{----------------------------------------}
procedure TFormMainMRGAbruf.InitKanalzahl;
{----------------------------------------}
begin
  V_MaxKanalZahl:=PathServer.MaxKanaele;
end;

{-----------------------------------------------------}
procedure TFormMainMRGAbruf.ReadFunktionsfreischaltung;
{-----------------------------------------------------}
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

{------------------------------------------}
procedure TFormMainMRGAbruf.ReadSrvCfg32Ini;
{------------------------------------------}
{ SrvCfg32.Ini lesen }
var
  SI: TSrvCfg32Ini;

begin
  SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
  try
    WithLogFile:=SI.Protokoll;
    if Thema = Themen [thMRGModem] then
      MaxRings:=SI.RingCount [COMNr]  { bei Modem-Ruf: Ruf nach SI.RingCount annehmen }
    else
      MaxRings:=0;                    { bei FUP-Ruf: Ruf sofort annehmen }
    ModemRuf:=SI.ModemRuf [COMNr];    { Gerätegruppe für Modem-Rufentgegennahme }
  finally
    SI.Free;
  end;
end;

{------------------------------------------------------------}
function TFormMainMRGAbruf.CheckLogPhysCom_Zuordnung: boolean;
{------------------------------------------------------------}
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

{-----------------------------------------}
procedure TFormMainMRGAbruf.ReadProgramIni;
{-----------------------------------------}
{ AfMRG32.Ini lesen }
begin
  with Timeouts_MRGModem do begin
    ModemAntwort:=AfMRG32Ini.TOModemAntwort;
    CRCCheck:=AfMRG32Ini.TOCRCCheck;
    ACK01ProtMeldung:=AfMRG32Ini.TOProtMeldung;
  end;  
  Timeout_ModemInit:=AfMRG32Ini.TOModemInit;
  Timeout_FupAntwort:=AfMRG32Ini.TOFup;
  Timeout_FupReset:=AfMRG32Ini.TOFupReset;
  Timeout_IEC1107Telegr:=AfMRG32Ini.TOIEC1107Telegr;

  Timeout_Verbindungsaufbau:=AfMRG32Ini.TOVerbindungsaufbau;
  Timeout_Verbindungsabbau:=AfMRG32Ini.TOVerbindungsabbau;
  Timeout_RufAnnahmeModem:=AfMRG32Ini.TORufAnnahmeModem;
  Timeout_Kennung:=AfMRG32Ini.TOKennung;
  Timeout_Login:=AfMRG32Ini.TOLogin;
  Timeout_Parameter:=AfMRG32Ini.TOParameter;
  Timeout_Meldungen:=AfMRG32Ini.TOMeldungen;
  Timeout_Messwerte:=AfMRG32Ini.TOMesswerte;
  Timeout_Tagessaetze:=AfMRG32Ini.TOTagessaetze;
  Timeout_Pruefsaetze:=AfMRG32Ini.TOPruefsaetze;
  Timeout_Binaerdatei:=AfMRG32Ini.TOBinaerdatei;
  Timeout_RundpufferReset:=AfMRG32Ini.TORundpufferReset;
  Timeout_Parametrieren:=AfMRG32Ini.TOParametrieren;
  Timeout_Rufausloesung:=AfMRG32Ini.TORufausloesung;
  Timeout_DSfGUmschaltung:=AfMRG32Ini.TODSfGUmschaltung;
  Timeout_DSfGRufliste:=AfMRG32Ini.TODSfGRufliste;
  Timeout_DSfGRufQuittung:=AfMRG32Ini.TODSfGRufQuittung;

  with Versuche_MRGModem do begin
    CRC:=AfMRG32Ini.CRCVersuche;
    BCC:=AfMRG32Ini.BCCVersuche;
    ACK01Prot:=AfMRG32Ini.ProtVersuche;
  end;
end;

{------------------------------------------}
procedure TFormMainMRGAbruf.ReadMaxBaudrate;
{------------------------------------------}
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

{--------------------------------------------------------------}
procedure TFormMainMRGAbruf.FUP_Modem_initialisieren_nach_Abruf;
{--------------------------------------------------------------}
begin
  { nach jedem Abruf FUP/Modem initialisieren: }
  if Thema = Themen [thMRGFup] then begin
    { Standardmäßig wird der FUP nach einem Abruf nur neu initialisiert.
      Nach 2 aufeinanderfolgenden Abrufen mit FUP-Meldung F2 wird jedoch
      sicherheitshalber ein FUP-Reset gemacht, sonst hängt der (9600er) FUP
      eventuell und baut keine Verbindungen mehr auf ! }
    if FUP_F2_Count >= 2 then begin
      FUP_F2_Count:=0;
      Fup_Reset (TSerialMRGFup (SerialMRG))
    end
    else begin
      if not Fup_Init (TSerialMRGFup (SerialMRG)) then
        Fup_Reset (TSerialMRGFup (SerialMRG));
    end;
  end else
    Modem_Initialisieren (TSerialMRGModem (SerialMRG), false);
end;

{--------------------------------------}
procedure TFormMainMRGAbruf.RufAbfragen;
{--------------------------------------}
{ auf ankommenden Ruf prüfen, wenn Funktion freigeschaltet }
var
  Ruf_angekommen: boolean;
  RufManager: TRufManager;
  dummy: integer;
begin
  if not Rufentgegennahme_freigeschaltet then exit;

  ZustandMessage (-1, -1, z_Rufabfragen, '', false);
  if SerialMRG.Rufabfrage (Ruf_angekommen, dummy, dummy) then begin
    if Ruf_angekommen then begin
      inc (RingCount);
      if RingCount >= MaxRings then begin
        { angekommenen Ruf erst nach MaxRings entgegennehmen: }
        RingCount:=0;
        Systemdaten:=GetMrgAbrufSystemDaten (PathServer.PathName [WNetProgDir]);
        RufManager:=TRufManager.Create (SerialMRG, aa_ruf, ModemRuf);
        try
          RufManager.RufEntgegennehmen;
        finally
          RufManager.Free;
        end;
        { nach jedem Abruf FUP/Modem initialisieren: }
        FUP_Modem_initialisieren_nach_Abruf;
      end;
    end;
  end
  else begin
    if Thema = Themen [thMRGFup] then
      Fup_Reset (TSerialMRGFup (SerialMRG))
    else
      Modem_Initialisieren (TSerialMRGModem (SerialMRG), false);
  end;
  ZustandMessage (-1, -1, z_Bereit, '', false);
end;

{---------------------------------------}
procedure TFormMainMRGAbruf.AbrufStarten;
{---------------------------------------}
{ MRG-Abruftabelle abarbeiten; vor Beenden des Programms müssen alle noch in der MRG-Abruf-
  Tabelle enthaltenen (und somit noch nicht abgerufenen Aufträge) gelöscht und in der
  Auftragstabelle zurückgesetzt werden !
  Exceptions werden bei Auto/Manu abgefangen, damit Rundruf für übrige Stationen weiterlaufen kann }
var
  Abrufart: TAbrufart;
  AbrufListe: TAbrufListe;

begin
  while GetAbrufListe (Abrufart, AbrufListe) do begin
    Application.ProcessMessages;
    if not Application.Terminated then begin
      case Abrufart of
        aa_automatisch,
        aa_manuell:  begin
                       try
                         Abrufen (Abrufart, AbrufListe);
                       finally
                         AktualisiereAuftragsListe (Abrufart, AbrufListe);
                       end;
                     end;
        aa_momentan: MomentanwerteHolen (AbrufListe [1].StationId);
        aa_rufreakt: RufReaktivieren (AbrufListe [1].StationId);
        aa_rueckruf: RueckrufAusloesen (AbrufListe [1].StationId, AbrufListe [1].Datentypen);
      end;

      { nach jedem Abruf FUP/Modem initialisieren: }
      FUP_Modem_initialisieren_nach_Abruf;

      if not Application.Terminated then
        RufAbfragen;                             { auf ankommenden Ruf prüfen }
    end else
      Aufraeumen (Abrufart, AbrufListe);
  end;  { while }
end;

{-----------------------------------------------------------------------------}
function TFormMainMRGAbruf.GetAbrufListe(var Abrufart: TAbrufart;
                                         var AbrufListe: TAbrufListe): boolean;
{-----------------------------------------------------------------------------}
{ Abrufliste zusammenstellen, Abrufart ermitteln (Auto/Manu/Momentan) }
var
  MRGAbruf: TMRGAbruf;
  SI: TSrvCfg32Ini;

begin
  Result:=false;
  { vor jedem Stationsabruf lesen: AFMRG32.INI, Schnittstellenzuordnung: logisch <-> physikalisch,
    Systemdaten }
  ReadProgramIni;
  SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
  try
    SI.ReadLogPhysCom_Zuordnung;
  finally
    SI.Free;
  end;
  Systemdaten:=GetMrgAbrufSystemDaten (PathServer.PathName [WNetProgDir]);

  MRGAbruf:=TMRGAbruf.Create (PathServer.PathName [WStammDir]);
  try
    if MRGAbruf.GetMRGAbruf (COMNr, Thema, Stammdaten, Abrufart, AbrufListe) then
      Result:=true;
  finally
    MRGAbruf.Free;
  end;
end;

{-------------------------------------------------------------------------------------}
procedure TFormMainMRGAbruf.Abrufen (Abrufart: TAbrufart; var AbrufListe: TAbrufListe);
{-------------------------------------------------------------------------------------}
{ MRG-Station abrufen }
var
  AutoManuAbrufManager: TAutoManuAbrufManager;

begin
  AutoManuAbrufManager:=TAutoManuAbrufManager.Create (SerialMRG, Abrufart,
                                                      AbrufListe, ModemRuf);
  try
    AutoManuAbrufManager.Abrufen;
    AbrufListe:=AutoManuAbrufManager.AbrufListe;              { Rückgabe: mit Abrufergebnis }
  finally
    AutoManuAbrufManager.Free;
  end;
end;

{-------------------------------------------------------------}
procedure TFormMainMRGAbruf.MomentanwerteHolen (MrgId: TMrgId);
{-------------------------------------------------------------}
{ Momentanwerte einer MRG-Station abrufen }
var
  MomentanAbrufManager: TMomentanAbrufManager;

begin
  MomHaltenTimer.Enabled:=true;
  try
    { Triggerfile für Momentanwertetabelle schreiben
      -> Info für Momentanwert-Client "Momentanwerteabruf läuft bereits" }
    WriteNewTime (PathServer.PathName [WNetWorkDir]+ CDBMMom + Format('%.4d.DB', [MrgId]));

    MomentanAbrufManager:=TMomentanAbrufManager.Create (SerialMRG, MrgId, ModemRuf);
    try
      MomentanAbrufManager.Abrufen;
    finally
      MomentanAbrufManager.Free;
    end;
  finally
    MomHaltenTimer.Enabled:=false;
  end;
end;

{----------------------------------------------------------}
procedure TFormMainMRGAbruf.RufReaktivieren (MrgId: TMrgId);
{----------------------------------------------------------}
{ Ruffunktion in einer MRG-Station reaktivieren }
var
  RufReaktManager: TRufReaktManager;

begin
  RufReaktManager:=TRufReaktManager.Create (SerialMRG, MrgId, ModemRuf);
  try
    RufReaktManager.Abrufen;
  finally
    RufReaktManager.Free;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFormMainMRGAbruf.RueckRufAusloesen (MrgId: TMrgId; Zentrale: integer);
{-------------------------------------------------------------------------------}
{ Rückruffunktion in einer MRG-Station auslösen }
var
  RueckRufManager: TRueckRufManager;

begin
  RueckRufManager:=TRueckRufManager.Create (SerialMRG, MrgId, Zentrale, ModemRuf);
  try
    RueckRufManager.Abrufen;
  finally
    RueckRufManager.Free;
  end;
end;

{-----------------------------------------------------------------------------}
procedure TFormMainMRGAbruf.AktualisiereAuftragsListe(Abrufart: TAbrufart;
                                                      AbrufListe: TAbrufListe);
{-----------------------------------------------------------------------------}
{ Auftragsliste aktualisieren }
var
  AuftragDb: TAuftragDb;
  TerminDb: TTerminDb;
  i: integer;
  NextTermin: TDateTime;
  NextDatentypen: integer;

begin
  AuftragDb:=TAuftragDb.Create (PathServer.PathName [WStammDir],
                                PathServer.PathName [WNetProgDir]);
  try
    TerminDb:=TTerminDb.Create (PathServer.PathName [WStammDir],
                                PathServer.PathName [WNetProgDir]);
    try
      for i:=Low (AbrufListe) to High (AbrufListe) do begin
        if AbrufListe [i].StationId <> 0 then begin
          if Abrufart = aa_automatisch then begin
            NextDatentypen:=AbrufListe [i].Datentypen;  { Vorbelegung für Update des Auftrags }

            if AbrufListe [i].Erfolgreich then begin                             { Abruf ok }
              { nächster Termin }
              if TerminDb.GetNextTermin (C_GerArtMrg, AbrufListe [i].StationId,
                                         NextTermin, NextDatentypen) then
                AuftragDb.UpdateAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto,
                                         NextTermin, NextDatentypen, 0, true)
              else                               { kein Termin mehr definiert }
                AuftragDb.DeleteAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto);
            end
            else begin                                      { Fehler im Abruf }
              if AbrufListe [i].Keine_weiteren_Versuche then begin
                { keine Wiederholung (Kennung, Paßwort falsch): nächster Termin,
                  Auftrag aktiv/inaktiv je nach Systemdaten-Einstellung (ab 17.07.2001) }
                if TerminDb.GetNextTermin (C_GerArtMrg, AbrufListe [i].StationId,
                                           NextTermin, NextDatentypen) then begin
                  if Systemdaten.StationDeaktivierung then
                    AuftragDb.UpdateAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto,
                                             NextTermin, NextDatentypen,
                                             AbrufListe [i].Anrufversuch, false)
                  else
                    AuftragDb.UpdateAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto,
                                             NextTermin, NextDatentypen,
                                             0, true);
                end else                             { kein Termin mehr definiert }
                  AuftragDb.DeleteAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto);
              end
              else begin
                if GetNextWiederholung (AbrufListe [i].Anrufversuch, NextTermin) then
                  { Wiederholung: nächster Abrufversuch nach Wartezeit }
                  AuftragDb.UpdateAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto,
                                           NextTermin,
                                           AbrufListe [i].Datentypen,
                                           AbrufListe [i].Anrufversuch, true)
                else begin
                  { max. Wiederholung erreicht: nächster Termin, Auftrag aktiv/inaktiv
                    je nach Systemdaten-Einstellung (ab 17.07.2001) }
                  if TerminDb.GetNextTermin (C_GerArtMrg, AbrufListe [i].StationId,
                                             NextTermin, NextDatentypen) then begin
                    if Systemdaten.StationDeaktivierung then
                      AuftragDb.UpdateAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto,
                                               NextTermin, NextDatentypen,
                                               AbrufListe [i].Anrufversuch, false)
                    else
                      AuftragDb.UpdateAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto,
                                               NextTermin, NextDatentypen,
                                               0, true);
                  end else
                    AuftragDb.DeleteAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto);
                end;
              end;
            end;
          end else                                          { manueller Abruf }
            { Auftrag löschen }
            AuftragDb.DeleteAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtManu);
        end;
      end;
    finally
      TerminDb.Free;
    end;
  finally
    AuftragDb.Free;
  end;
end;

{------------------------------------------------------------------------------------}
procedure TFormMainMRGAbruf.Aufraeumen (Abrufart: TAbrufart; AbrufListe: TAbrufListe);
{------------------------------------------------------------------------------------}
{ Einträge aus MRG-Abruftabelle löschen und in Auftragstabelle zurücksetzen }
var
  AuftragDb: TAuftragDb;
  i: integer;

begin
  AuftragDb:=TAuftragDb.Create (PathServer.PathName [WStammDir],
                                PathServer.PathName [WNetProgDir]);
  try
    for i:=Low (AbrufListe) to High (AbrufListe) do begin
      if AbrufListe [i].StationId <> 0 then begin
        if Abrufart = aa_automatisch then
          { ursprünglichen Auftrag zurückschreiben }
          AuftragDb.UpdateAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtAuto,
                                   -1, -1, 0, true)
        else                                                { manueller Abruf }
          { Auftrag löschen }
          AuftragDb.DeleteAuftrag (C_GerArtMrg, AbrufListe [i].StationId, C_AbrArtManu);
      end;
    end;
  finally
    AuftragDb.Free;
  end;
end;


{------------------------- Message-Behandlungsmethoden ------------------------}

{-----------------------------------------------------------}
procedure TFormMainMRGAbruf.WMKWLink (var Message: TMessage);
{-----------------------------------------------------------}
begin
  case Message.WParam of
    KWE_MrgAbrufzeit_erreicht:       { Abruf automatisch anstoßen, wenn Botschaft empfangen }
    begin
      Abrufzeit_erreicht:=true;
    end;

    KWE_AbrufManager_Beendet:                  { Programm beenden, wenn Botschaft empfangen }
    begin
      mBeendenClick (Self);
    end;
  end;
end;

{----------------------------------------------------------------}
procedure TFormMainMRGAbruf.WMTASKBAREVENT(var message: TMessage);
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
                      popupmenu.popup(point.x,point.y);
                    end;
  end;
end;

{----------------------------------------------------------------}
procedure TFormMainMRGAbruf.BtnAbrufStartenClick(Sender: TObject);
{----------------------------------------------------------------}
{ Abruf manuell anstoßen (nur für Testzwecke) }
begin
  Abrufzeit_erreicht:=true;
end;


{ -------------------------- Timer-Methoden -----------------------------------}

{-----------------------------------------------------------}
procedure TFormMainMRGAbruf.AbrufTimerTimer(Sender: TObject);
{-----------------------------------------------------------}
{ zyklische Überprüfung, ob Ruf ansteht und angenommen werden muß bzw. Abruf gestartet
  werden soll }
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

{---------------------------------------------------------------}
procedure TFormMainMRGAbruf.MomHaltenTimerTimer(Sender: TObject);
{---------------------------------------------------------------}
{ zyklische Überprüfung, ob Momentanwerte-Verbindung weiter gehalten werden soll }
begin
  CheckMomHalten:=true;                                         { Flag setzen }
end;

{-----------------------------------------------------------}
procedure TFormMainMRGAbruf.ErrorTimerTimer(Sender: TObject);
{-----------------------------------------------------------}
{ Icon blinkt und ErrorBeep }
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

{------------------------------------------------------------------}
procedure TFormMainMRGAbruf.TxDMessage (Msg: string; Bytes: string);
{------------------------------------------------------------------}
{ Ausgabe der gesendeten Zeichen und deren Anzahl;
  Übergabe: Msg-String }
begin
  eTxD.Text:=Msg;
  lTxDByte.Caption:=Bytes;
end;

{------------------------------------------------------------------}
procedure TFormMainMRGAbruf.RxDMessage (Msg: string; Bytes: string);
{------------------------------------------------------------------}
{ Ausgabe der empfangenen Zeichen und deren Anzahl;
  Übergabe: Msg-String }
begin
  eRxD.Text:=Msg;
  lRxDByte.Caption:=Bytes;
end;

{-------------------------------------------------------}
procedure TFormMainMRGAbruf.TimeoutMessage (Msg: string);
{-------------------------------------------------------}
{ Ausgabe des Timeouts der momentan laufenden Aktion
  Übergabe: Msg-String }
begin
  Statusbar.Panels [4].Text:=Msg;
  Application.ProcessMessages;
end;

{-----------------------------------------------------------}
procedure TFormMainMRGAbruf.ModemstatusMessage (Msg: string);
{-----------------------------------------------------------}
{ Ausgabe des momentanen Modemstatus (Connect, No Carrier etc.)
  Übergabe: Msg-String }
begin
  lStatus.Caption:=Msg;
  Application.ProcessMessages;

  { leere Modemstatus-Message -> keine Verbindung }
  if length(Msg) = 0 then
    SetStatuscolor (ps_KeineVerbindung);
end;

{----------------------------------------------------------}
procedure TFormMainMRGAbruf.FupVersionMessage (Msg: string);
{----------------------------------------------------------}
{ Ausgabe der FUP-Version
  Übergabe: Msg-String }
begin
  lStatus.Caption:=Msg;
  Application.ProcessMessages;
end;

{---------------------------------------------------------------}
procedure TFormMainMRGAbruf.SetStatusColor (ProgStatus: integer);
{---------------------------------------------------------------}
{ Statusfarbe in der Statusbar je nach Programmstatus setzen
  Übergabe: Programstatus-Konstante }
begin
  if (ProgStatus >= Low (cl_Programmstatus)) AND
     (ProgStatus <= High (cl_Programmstatus)) then begin
    StatusColor:=cl_Programmstatus [ProgStatus];
    Statusbar.Invalidate;
  end;
end;

{-------------------------------------------------------------------}
procedure TFormMainMRGAbruf.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
{-------------------------------------------------------------------}
{ Statusfarbe in Statusbar setzen }
begin
  if Panel.Index = 0 then begin
    with StatusBar.Canvas do begin
      Brush.Color:=StatusColor;
      FillRect (Rect);
    end;
  end;
end;

{----------------------------------------------------}
procedure TFormMainMRGAbruf.SetError (OnOff: boolean);
{----------------------------------------------------}
{ akustische und optische Fehlerausgabe aktivieren/deaktivieren }
begin
  if OnOff <> ErrorTimer.Enabled then
    ErrorCount:=99999;
  ErrorTimer.Enabled:=OnOff;

  if not OnOff then             { Fehlerausgabe deaktiviert: OK-Icon anzeigen }
    TaskBarModifyIcon (0);
end;


{--------------------- Popupmenü-Methoden -------------------------------------}

{----------------------------------------------------------}
procedure TFormMainMRGAbruf.mAnzeigenClick(Sender: TObject);
{----------------------------------------------------------}
begin
  Show;
  Application.BringToFront;
end;

{---------------------------------------------------------}
procedure TFormMainMRGAbruf.mBeendenClick(Sender: TObject);
{---------------------------------------------------------}
begin
  Screen.Cursor:=crHourGlass;
  FCanClose:=true;
  Close;
end;

end.

