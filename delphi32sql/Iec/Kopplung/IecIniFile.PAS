{******************************************************************************}
{* Unit: Zugriffe auf IEC32.INI                                               *}
{*       enthält alle Einstellungen für das IEC-Kopplungssystem               *}
{* Version: 06.03.2007   WW                                                   *}
{******************************************************************************}
unit IecIniFile;

interface

uses
  Classes, SysUtils, IniFiles, Serial, WPorts, IecImportDirList, GD_Utils, DB_Attn;

const
  CIec32Ini = 'Iec32.INI';

  { DSfG-Archiv-Auswahl }
  daa_Archiv_KZW = 0;  // Archivdaten und Kurzzeitwerte
  daa_nur_Archiv = 1;  // nur Archivdaten aus AutoDb
  daa_nur_KZW    = 2;  // nur Kurzzeitwerte (ASCII-Dateien)

type
  { IEC 870-Normen }
  TIEC870_Norm = (norm_iec870_101, norm_iec870_104);

  { IEC 870-Funktionen }
  TIEC870_Funktion = (fkt_iec870_Station, fkt_iec870_Zentrale);

  { Übertragungsprozeduren IEC 870-5-101 }
  TUebertragungsprozedur = (tp_unsymmetrisch, tp_symmetrisch);

  TIec32Ini = class (TIniFile)
  private
    FFileName: string;
    function GetMyFilePath: TFileName;
    function GetFileDate: TDateTime;
  protected
    { IEC870 }
    function GetNorm: TIEC870_Norm;
    function GetFunktion: TIEC870_Funktion;
    { PortDef }
    function GetCOM: integer;
    function GetBaud: integer;
    function GetDatabits: integer;
    function GetParity: TParityBit;
    function GetStopbits: TStopBits;
    { Daten }
    function GetErsteMeldungenTage: integer;
    function GetMRGErsteDatenAnzahlWerte: integer;
    function GetDSfGErsteDatenAnzahlWerte: integer;
    function GetKurzzeitwerteLoeschenStunden: integer;
    function GetKurzzeitwerteLoeschenAlt: boolean;
    function GetDSfGArchivAuswahl: integer;
    procedure SetDSfGArchivAuswahl (Value: integer);
    { Generalabfrage }
    function GetGeneralabfrage_MRGAnzahlWerte: integer;
    function GetGeneralabfrage_DSfGAnzahlWerte: integer;
    { Standby }
    function GetTelegrammzahl_Standby: integer;
    function GetZeitfenster_Standby: integer;
    { Debug }
    function GetLogDatei: boolean;
    function GetDebugRohdaten: boolean;
    function GetDebugRundpuffer: boolean;
    function GetDebugRundpuffergroesse_kB: integer;
    { Verbindungsschicht }
    function GetUebertragungsprozedur: TUebertragungsprozedur;
    function GetEinzelsteuerzeichen_E5: boolean;
    function GetAdressfeld_Bytes: integer;
    function GetTelegramm_MaxBytes: integer;
    function GetTO_EmpfSekundaer: integer;
    function GetZyklus_Verbindungstest: integer;
    { Anwendungsschicht }
    function GetASDU_Bytes: integer;
    function GetInfoObj_Bytes: integer;
    function GetHerkunftsadresse: integer;
    function GetAPDU_MaxBytes: integer;
    { Prozessinformation }
    function GetDualeZeit2a_Bytes: integer;
    { IEC 870-5-104 }
    function GetPort: integer;
    function GetIPAdresse: string;
    function GetTO_t0: integer;
    function GetTO_t1: integer;
    function GetTO_t2: integer;
    function GetTO_t3: integer;
    function GetAnz_k: integer;
    function GetAnz_w: integer;
    { Redundanz }
    function GetRedundanzStatusmeldung: boolean;
    function GetRedundanzStatusmeldung_ASDU: integer;
    function GetRedundanzStatusmeldung_InfoObj: integer;
    function GetRedundanzTO_VerbUnterbrechung: integer;
    function GetRedundanzTO_Error: integer;
    { Datenimport }
    function GetImport: boolean;
    function GetImportVerzoegerungSekunden: integer;
    { Lebenszeichendateien }
    function GetHeartbeatFile: TFileName;
    function GetHeartbeatFile_Datatransfer: TFileName;
    { Typkennungen }
    function GetTypkennungAktiv (Typkennung: integer): boolean;
    { Remote-Monitor }
    function GetRMPort: integer;
    { Trigger-Dateien }
    function GetTriggerdatei_DatenReset: string;
  public
    constructor Create (Path: TFilename);
    function GetImportDirList (L: TImportDirList): integer;
    property FilePath: TFileName read GetMyFilePath;
    property FileDate: TDateTime read GetFileDate;
    property Norm: TIEC870_Norm read GetNorm;
    property Funktion: TIEC870_Funktion read GetFunktion;

    property COM: integer read GetCOM;
    property Baud: integer read GetBaud;
    property Databits: integer read GetDatabits;
    property Parity: TParityBit read GetParity;
    property Stopbits: TStopBits read GetStopbits;

    property ErsteMeldungenTage: integer read GetErsteMeldungenTage;
    property MRGErsteDatenAnzahlWerte: integer read GetMRGErsteDatenAnzahlWerte;
    property DSfGErsteDatenAnzahlWerte: integer read GetDSfGErsteDatenAnzahlWerte;
    property KurzzeitwerteLoeschenStunden: integer read GetKurzzeitwerteLoeschenStunden;
    property KurzzeitwerteLoeschenAlt: boolean read GetKurzzeitwerteLoeschenAlt;
    property DSfGArchivAuswahl: integer read GetDSfGArchivAuswahl write SetDSfGArchivAuswahl;

    property Generalabfrage_MRGAnzahlWerte: integer read GetGeneralabfrage_MRGAnzahlWerte;
    property Generalabfrage_DSfGAnzahlWerte: integer read GetGeneralabfrage_DSfGAnzahlWerte;

    property Telegrammzahl_Standby: integer read GetTelegrammzahl_Standby;
    property Zeitfenster_Standby: integer read GetZeitfenster_Standby;

    property LogDatei: boolean read GetLogDatei;
    property DebugRohdaten: boolean read GetDebugRohdaten;
    property DebugRundpuffer: boolean read GetDebugRundpuffer;
    property DebugRundpuffergroesse_kB: integer read GetDebugRundpuffergroesse_kB;

    property Uebertragungsprozedur: TUebertragungsprozedur read GetUebertragungsprozedur;
    property Einzelsteuerzeichen_E5: boolean read GetEinzelsteuerzeichen_E5;
    property Adressfeld_Bytes: integer read GetAdressfeld_Bytes;
    property Telegramm_MaxBytes: integer read GetTelegramm_MaxBytes;
    property TO_EmpfSekundaer: integer read GetTO_EmpfSekundaer;
    property Zyklus_Verbindungstest: integer read GetZyklus_Verbindungstest;

    property ASDU_Bytes: integer read GetASDU_Bytes;
    property InfoObj_Bytes: integer read GetInfoObj_Bytes;
    property Herkunftsadresse: integer read GetHerkunftsadresse;
    property APDU_MaxBytes: integer read GetAPDU_MaxBytes;

    property DualeZeit2a_Bytes: integer read GetDualeZeit2a_Bytes;

    property Port: integer read GetPort;
    property IPAdresse: string read GetIPAdresse;
    property TO_t0: integer read GetTO_t0;
    property TO_t1: integer read GetTO_t1;
    property TO_t2: integer read GetTO_t2;
    property TO_t3: integer read GetTO_t3;
    property Anz_k: integer read GetAnz_k;
    property Anz_w: integer read GetAnz_w;

    property RedundanzStatusmeldung: boolean read GetRedundanzStatusmeldung;
    property RedundanzStatusmeldung_ASDU: integer read GetRedundanzStatusmeldung_ASDU;
    property RedundanzStatusmeldung_InfoObj: integer read GetRedundanzStatusmeldung_InfoObj;
    property RedundanzTO_VerbUnterbrechung: integer read GetRedundanzTO_VerbUnterbrechung;
    property RedundanzTO_Error: integer read GetRedundanzTO_Error;

    property Import: boolean read GetImport;
    property ImportVerzoegerungSekunden: integer read GetImportVerzoegerungSekunden;

    property HeartbeatFile: TFileName read GetHeartbeatFile;
    property HeartbeatFile_Datatransfer: TFileName read GetHeartbeatFile_Datatransfer;

    property TypkennungAktiv [Typkennung: integer]: boolean read GetTypkennungAktiv;

    property RMPort: integer read GetRMPort;

    property Triggerdatei_DatenReset: string read GetTriggerdatei_DatenReset;
  end;

implementation

{ TIec32Ini }

const
  C_Iec32Ini_Path : TFilename = '';

  { Sections }
  CSectionIEC870             = 'IEC870';
  CSectionPortDef            = 'PortDef';
  CSectionDaten              = 'Daten';
  CSectionGeneralabfrage     = 'Generalabfrage';
  CSectionStandBy            = 'Standby';
  CSectionDebug              = 'Debug';
  CSectionVerbindungsschicht = 'Verbindungsschicht';
  CSectionAnwendungsschicht  = 'Anwendungsschicht';
  CSectionProzessinformation = 'Prozessinformation';
  CSection104                = '104';
  CSectionRedundanz          = 'Redundanz';
  CSectionDatenimport        = 'Datenimport';
  CSectionDatenimport_TK_    = 'Datenimport_TK_';
  CSectionTypkennungen       = 'Typkennungen';
  CSectionRemoteMonitor      = 'RemoteMonitor';
  CSectionTriggerdateien     = 'Triggerdateien';

  { Idents }
  CIdentNorm = 'Norm';
  CIdentFunktion = 'Funktion';

  CIdentCOM      = 'COM';
  CIdentBaud     = 'Baud';
  CIdentDatabits = 'Databits';
  CIdentParity   = 'Parity';
  CIdentStopbits = 'Stopbits';

  CIdentErsteMeldungenTage           = 'Erste_Meldungen_Tage';
  CIdentMRGErsteDatenAnzahlWerte     = 'MRG_Erste_Daten_AnzahlWerte';
  CIdentDSfGErsteDatenAnzahlWerte    = 'DSfG_Erste_Daten_AnzahlWerte';
  CIdentKurzzeitwerteLoeschenStunden = 'Kurzzeitwerte_Loeschen_Stunden';
  CIdentKurzzeitwerteLoeschenAlt     = 'Kurzzeitwerte_Loeschen_Alt';
  CIdentDSfGArchivAuswahl            = 'DSfGArchivAuswahl';

  CIdentMRG_AnzahlWerte  = 'MRG_AnzahlWerte';
  CIdentDSfG_AnzahlWerte = 'DSfG_AnzahlWerte';

  CIdentTelegrammAnzahl = 'TelegrammAnzahl';
  CIdentZeitfenster     = 'Zeitfenster [s]';

  CIdentLogDatei = 'LogDatei';
  CIdentRohdaten = 'Rohdaten';
  CIdentRundpuffer           = 'Rundpuffer';
  CIdentRundpuffergroesse_kB = 'Rundpuffergroesse_kB';

  CIdentUebertragungsprozedur  = 'Uebertragungsprozedur';
  CIdentEinzelsteuerzeichen_E5 = 'Einzelsteuerzeichen_E5';
  CIdentAdressfeld_Bytes       = 'Adressfeld_Bytes';
  CIdentTelegramm_MaxBytes     = 'Telegramm_MaxBytes';
  CIdentTO_EmpfSekundaer       = 'TO_EmpfSekundaer';
  CIdentZyklus_Verbindungstest = 'Zyklus_Verbindungstest';

  CIdentASDU_Bytes       = 'ASDU_Bytes';
  CIdentInfoObj_Bytes    = 'InfoObj_Bytes';
  CIdentHerkunftsadresse = 'Herkunftsadresse';
  CIdentAPDU_MaxBytes    = 'APDU_MaxBytes';

  CIdentDualeZeit2a_Bytes = 'DualeZeit2a_Bytes';

  CIdentPort = 'Port';
  CIdentIPAdresse = 'IPAdresse';
  CIdentTO_t0 = 'TO_t0';
  CIdentTO_t1 = 'TO_t1';
  CIdentTO_t2 = 'TO_t2';
  CIdentTO_t3 = 'TO_t3';
  CIdentAnz_k = 'Anz_k';
  CIdentAnz_w = 'Anz_w';

  CIdentStatusmeldung         = 'Statusmeldung';
  CIdentStatusmeldung_ASDU    = 'Statusmeldung_ASDU';
  CIdentStatusmeldung_InfoObj = 'Statusmeldung_InfoObj';
  CIdentTO_VerbUnterbrechung  = 'TO_VerbUnterbrechung';
  CIdentTO_Error              = 'TO_Error';

  CIdentImport = 'Import';
  CIdentVerzoegerung_Sekunden = 'Verzoegerung_Sekunden';
  CIdentDirName = 'DirName';

  CIdentHeartbeatFile = 'HEARTBEATFILE';
  CIdentHeartbeatFile_Datatransfer = 'HEARTBEATFILE_DATATRANSFER';

  CIdentTK_ = 'TK_';

  CIdentDatenReset = 'DatenReset';

  { Einstellwerte }
  C101           = '101';
  C104           = '104';

  CStation       = 'STATION';
  CZentrale      = 'ZENTRALE';

  Cunsymmetrisch = 'UNSYMMETRISCH';
  Csymmetrisch   = 'SYMMETRISCH';


{---------------------------------------------}
constructor TIec32Ini.Create (Path: TFilename);
{---------------------------------------------}
begin
  if (C_Iec32Ini_Path = '') then
    C_Iec32Ini_Path := IncludeTrailingBackslash(Path);
  FFileName:=C_Iec32Ini_Path + CIec32Ini;
  inherited Create (FFileName);
end;

{---------------------------------------}
function TIec32Ini.GetMyFilePath: TFileName;
{---------------------------------------}
begin
  Result := C_Iec32Ini_Path;
end;

{----------------------------------------}
function TIec32Ini.GetFileDate: TDateTime;
{----------------------------------------}
begin
  Result:=GetTriggerTime (FFilename, false);
end;

{ IEC870 }

{---------------------------------------}
function TIec32Ini.GetNorm: TIEC870_Norm;
{---------------------------------------}
var
  S: string;
begin
  S := UpperCase (ReadString (CSectionIEC870, CIdentNorm, C101));
  if S = C104 then
    Result:=norm_iec870_104
  else
    Result:=norm_iec870_101;
end;

{-----------------------------------------------}
function TIec32Ini.GetFunktion: TIEC870_Funktion;
{-----------------------------------------------}
var
  S: string;
begin
  S := UpperCase (ReadString (CSectionIEC870, CIdentFunktion, CStation));
  if S = CZentrale then
    Result:=fkt_iec870_Zentrale
  else
    Result:=fkt_iec870_Station;
end;

{ PortDef }

{---------------------------------}
function TIec32Ini.GetCOM: integer;
{---------------------------------}
begin
  Result := ReadInteger (CSectionPortDef, CIdentCOM, 1);
end;

{----------------------------------}
function TIec32Ini.GetBaud: integer;
{----------------------------------}
begin
  Result := ReadInteger (CSectionPortDef, CIdentBaud, 9600);
end;

{--------------------------------------}
function TIec32Ini.GetDatabits: integer;
{--------------------------------------}
begin
  Result := ReadInteger (CSectionPortDef, CIdentDatabits, 8);
end;

{---------------------------------------}
function TIec32Ini.GetParity: TParityBit;
{---------------------------------------}
var
  S: string;

begin
  S:=UpperCase (ReadString(CSectionPortDef, CIdentParity, 'EVEN'));
  case S[1] of
    'N': Result:=none;
    'O': Result:=odd;
    'E': Result:=even;
    'M': Result:=mark;
    'S': Result:=space;
  else
    Result:=none;
  end;
end;

{----------------------------------------}
function TIec32Ini.GetStopbits: TStopBits;
{----------------------------------------}
var
  S: string;

begin
  S:=ReadString(CSectionPortDef, CIdentStopbits, '1');
  if S = '1' then
    Result:=sb_1
  else if (S = '1.5') OR (S = '1,5') then
    Result:=sb_15
  else if S = '2' then
    Result:=sb_2
  else
    Result:=sb_1;
end;

{ Daten }

{------------------------------------------------}
function TIec32Ini.GetErsteMeldungenTage: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionDaten, CIdentErsteMeldungenTage, 14);
  if Result < 1 then
    Result:=14;
end;

{------------------------------------------------------}
function TIec32Ini.GetMRGErsteDatenAnzahlWerte: integer;
{------------------------------------------------------}
begin
  Result := ReadInteger (CSectionDaten, CIdentMRGErsteDatenAnzahlWerte, 336);
  if Result < 1 then
    Result:=336;
end;

{-------------------------------------------------------}
function TIec32Ini.GetDSfGErsteDatenAnzahlWerte: integer;
{-------------------------------------------------------}
begin
  Result := ReadInteger (CSectionDaten, CIdentDSfGErsteDatenAnzahlWerte, 336);
  if Result < 1 then
    Result:=336;
end;

{----------------------------------------------------------}
function TIec32Ini.GetKurzzeitwerteLoeschenStunden: integer;
{----------------------------------------------------------}
begin
  Result := ReadInteger (CSectionDaten, CIdentKurzzeitwerteLoeschenStunden, 1);
  if Result < 1 then
    Result:=1;
end;

{------------------------------------------------------}
function TIec32Ini.GetKurzzeitwerteLoeschenAlt: boolean;
{------------------------------------------------------}
begin
  Result := ReadBool (CSectionDaten, CIdentKurzzeitwerteLoeschenAlt, false);
end;

{-----------------------------------------------}
function TIec32Ini.GetDSfGArchivAuswahl: integer;
{-----------------------------------------------}
var
  i: integer;
begin
  i:=ReadInteger (CSectionDaten, CIdentDSfGArchivAuswahl, 0);
  if i < daa_Archiv_KZW then
    i:=daa_Archiv_KZW
  else if i > daa_nur_KZW then
    i:=daa_nur_KZW;
  Result:=i;
end;

{--------------------------------------------------------}
procedure TIec32Ini.SetDSfGArchivAuswahl (Value: integer);
{--------------------------------------------------------}
begin
  WriteInteger (CSectionDaten, CIdentDSfGArchivAuswahl, Value);
end;

{ Generalabfrage }

{-----------------------------------------------------------}
function TIec32Ini.GetGeneralabfrage_MRGAnzahlWerte: integer;
{-----------------------------------------------------------}
begin
  Result := ReadInteger (CSectionGeneralabfrage, CIdentMRG_AnzahlWerte, 48);
end;

{------------------------------------------------------------}
function TIec32Ini.GetGeneralabfrage_DSfGAnzahlWerte: integer;
{------------------------------------------------------------}
begin
  Result := ReadInteger (CSectionGeneralabfrage, CIdentDSfG_AnzahlWerte, 48);
end;

{ Standby }

{---------------------------------------------------}
function TIec32Ini.GetTelegrammzahl_Standby: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger (CSectionStandBy, CIdentTelegrammAnzahl, 10);
end;

{-------------------------------------------------}
function TIec32Ini.GetZeitfenster_Standby: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionStandBy, CIdentZeitfenster, 10);
end;

{ Debug }

{--------------------------------------}
function TIec32Ini.GetLogDatei: boolean;
{--------------------------------------}
begin
  Result := UpperCase (ReadString (CSectionDebug, CIdentLogDatei, 'AUS')) = 'EIN';
end;

{-------------------------------------------}
function TIec32Ini.GetDebugRohdaten: boolean;
{-------------------------------------------}
begin
  Result := UpperCase (ReadString (CSectionDebug, CIdentRohdaten, 'AUS')) = 'EIN';
end;

{---------------------------------------------}
function TIec32Ini.GetDebugRundpuffer: boolean;
{---------------------------------------------}
begin
  Result := UpperCase (ReadString (CSectionDebug, CIdentRundpuffer, 'AUS')) = 'EIN';
end;

{-------------------------------------------------------}
function TIec32Ini.GetDebugRundpuffergroesse_kB: integer;
{-------------------------------------------------------}
begin
  Result := ReadInteger (CSectionDebug, CIdentRundpuffergroesse_kB, 500);
end;

{ Verbindungsschicht }

{------------------------------------------------------------------}
function TIec32Ini.GetUebertragungsprozedur: TUebertragungsprozedur;
{------------------------------------------------------------------}
var
  S: string;
begin
  S := UpperCase (ReadString (CSectionVerbindungsschicht, CIdentUebertragungsprozedur,
                              Cunsymmetrisch));
  if S = Csymmetrisch then
    Result:=tp_symmetrisch
  else
    Result:=tp_unsymmetrisch;
end;

{----------------------------------------------------}
function TIec32Ini.GetEinzelsteuerzeichen_E5: boolean;
{----------------------------------------------------}
begin
  Result := ReadBool (CSectionVerbindungsschicht, CIdentEinzelsteuerzeichen_E5, true);
end;

{----------------------------------------------}
function TIec32Ini.GetAdressfeld_Bytes: integer;
{----------------------------------------------}
begin
  Result := ReadInteger (CSectionVerbindungsschicht, CIdentAdressfeld_Bytes, 1);
  if Result < 0 then
    Result:=0  // 0 = ohne Adressfeld; 30.05.2007, WW
  else if Result > 2 then
    Result:=2;
end;

{------------------------------------------------}
function TIec32Ini.GetTelegramm_MaxBytes: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionVerbindungsschicht, CIdentTelegramm_MaxBytes, 255);
  if Result < 0 then
    Result:=0
  else if Result > 255 then  // max. mögliche Anzahl L der Anwenderdatenoktette (ohne Start, Länge, Prüfsumme, Ende)
    Result:=255;
end;

{----------------------------------------------}
function TIec32Ini.GetTO_EmpfSekundaer: integer;
{----------------------------------------------}
begin
  Result := ReadInteger (CSectionVerbindungsschicht, CIdentTO_EmpfSekundaer, 5000);  // ms
  if Result < 0 then
    Result:=0;
end;

{----------------------------------------------------}
function TIec32Ini.GetZyklus_Verbindungstest: integer;
{----------------------------------------------------}
begin
  Result := ReadInteger (CSectionVerbindungsschicht, CIdentZyklus_Verbindungstest, 5000);  // ms
  if Result < 0 then
    Result:=0
  else if Result > 3599999 then  // praxisnahe obere Grenze 1 h
    Result:=3599999;
end;


{ Anwendungsschicht }

{----------------------------------------}
function TIec32Ini.GetASDU_Bytes: integer;
{----------------------------------------}
begin
  Result := ReadInteger (CSectionAnwendungsschicht, CIdentASDU_Bytes, 2);
  if Result < 1 then
    Result:=1
  else if Result > 2 then
    Result:=2;
end;

{-------------------------------------------}
function TIec32Ini.GetInfoObj_Bytes: integer;
{-------------------------------------------}
begin
  Result := ReadInteger (CSectionAnwendungsschicht, CIdentInfoObj_Bytes, 2);
  if Result < 1 then
    Result:=1
  else if Result > 3 then
    Result:=3;
end;

{----------------------------------------------}
function TIec32Ini.GetHerkunftsadresse: integer;
{----------------------------------------------}
{ Ergebnis < 0: ohne Herkunftsadresse (1 Byte Übertragungsursache ) }
begin
  Result := ReadInteger (CSectionAnwendungsschicht, CIdentHerkunftsadresse, -1);
  if Result > 255 then
    Result:=255;
end;

{-------------------------------------------}
function TIec32Ini.GetAPDU_MaxBytes: integer;
{-------------------------------------------}
{ nur IEC 870-5-104 }
begin
  Result := ReadInteger (CSectionAnwendungsschicht, CIdentAPDU_MaxBytes, 253);
  if Result < 0 then
    Result:=0
  else if Result > 253 then  // max. mögliche Anzahl der Steuerfeld- und ASDU-Oktette (ohne Start, Länge)
    Result:=253;
end;

{ Prozessinformation }

{-----------------------------------------------}
function TIec32Ini.GetDualeZeit2a_Bytes: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionProzessInformation, CIdentDualeZeit2a_Bytes, 7);
  if Result < 0 then
    Result:=0
  else if Result > 7 then
    Result:=7;
end;

{ IEC 870-5-104 }

{----------------------------------}
function TIec32Ini.GetPort: integer;
{----------------------------------}
begin
  Result := ReadInteger (CSection104, CIdentPort, CIPPortIEC870_104_Default);
end;

{--------------------------------------}
function TIec32Ini.GetIPAdresse: string;
{--------------------------------------}
begin
  Result := ReadString (CSection104, CIdentIPAdresse, '127.0.0.1');
end;

{-----------------------------------}
function TIec32Ini.GetTO_t0: integer;
{-----------------------------------}
begin
  Result := ReadInteger (CSection104, CIdentTO_t0, 30000);  // ms
  if Result < 1 then
    Result:=1
  else if Result > 255000 then
    Result:=255000;
end;

{-----------------------------------}
function TIec32Ini.GetTO_t1: integer;
{-----------------------------------}
begin
  Result := ReadInteger (CSection104, CIdentTO_t1, 15000);  // ms
  if Result < 1 then
    Result:=1
  else if Result > 255000 then
    Result:=255000;
end;

{-----------------------------------}
function TIec32Ini.GetTO_t2: integer;
{-----------------------------------}
begin
  Result := ReadInteger (CSection104, CIdentTO_t2, 10000);  // ms
  if Result < 1 then
    Result:=1
  else if Result > 255000 then
    Result:=255000;
end;

{-----------------------------------}
function TIec32Ini.GetTO_t3: integer;
{-----------------------------------}
begin
  Result := ReadInteger (CSection104, CIdentTO_t3, 20000);  // ms
  if Result < 0 then
    Result:=0  // 0 = keine Testframes
  else if Result > 255000 then
    Result:=255000;
end;

{-----------------------------------}
function TIec32Ini.GetAnz_k: integer;
{-----------------------------------}
begin
  Result := ReadInteger (CSection104, CIdentAnz_k, 12);
  if Result < 1 then
    Result:=1;
end;

{-----------------------------------}
function TIec32Ini.GetAnz_w: integer;
{-----------------------------------}
begin
  Result := ReadInteger (CSection104, CIdentAnz_w, 8);
  if Result < 1 then
    Result:=1;
end;

{ Redundanz }

{----------------------------------------------------}
function TIec32Ini.GetRedundanzStatusmeldung: boolean;
{----------------------------------------------------}
begin
  Result := ReadBool (CSectionRedundanz, CIdentStatusmeldung, false);  // Default: kein Statusmeldungs-Telegramm
end;

{---------------------------------------------------------}
function TIec32Ini.GetRedundanzStatusmeldung_ASDU: integer;
{---------------------------------------------------------}
begin
  Result := ReadInteger (CSectionRedundanz, CIdentStatusmeldung_ASDU, 0);
  if Result < 0 then
    Result:=0;
end;

{------------------------------------------------------------}
function TIec32Ini.GetRedundanzStatusmeldung_InfoObj: integer;
{------------------------------------------------------------}
begin
  Result := ReadInteger (CSectionRedundanz, CIdentStatusmeldung_InfoObj, 0);
  if Result < 0 then
    Result:=0;
end;

{-----------------------------------------------------------}
function TIec32Ini.GetRedundanzTO_VerbUnterbrechung: integer;
{-----------------------------------------------------------}
begin
  Result := ReadInteger (CSectionRedundanz, CIdentTO_VerbUnterbrechung, 180000);  // ms
  if Result < 0 then
    Result:=0;
end;

{-----------------------------------------------}
function TIec32Ini.GetRedundanzTO_Error: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionRedundanz, CIdentTO_Error, 60000);  // ms
  if Result < 0 then
    Result:=0;
end;

{ Datenimport }

{------------------------------------}
function TIec32Ini.GetImport: boolean;
{------------------------------------}
begin
  Result := UpperCase (ReadString (CSectionDatenimport, CIdentImport, 'EIN')) = 'EIN';
end;

{--------------------------------------------------------}
function TIec32Ini.GetImportVerzoegerungSekunden: integer;
{--------------------------------------------------------}
begin
  Result := ReadInteger (CSectionDatenimport, CIdentVerzoegerung_Sekunden, 10);
  if Result < 0 then
    Result:=0;
end;

{---------------------------------------------------------------}
function TIec32Ini.GetImportDirList (L: TImportDirList): integer;
{---------------------------------------------------------------}
{ liest Datenimport-Ausgabeverzeichnis-Einstellungen aus und gibt sie in L zurück;
  Ergebnis:  0 = Einstellungen OK
            -1 = Verzeichnisname fehlt (leer)
            -2 = Ungültige Typkennung in Section }
var
  SectionList: TStringList;
  i: integer;
  Section: string;
  ImportDirData: TImportDirData;
  ImportDirDataObj: TImportDirDataObj;
  sTypkennung: string;
  iBuf: integer;

begin
  Result:=0;  // OK
  if L = nil then exit;
  L.Clear;

  SectionList:=TStringList.Create;
  try
    ReadSections(SectionList);
    for i:=0 to SectionList.Count-1 do begin
      Section:=SectionList[i];
      if Pos (CSectionDatenimport_TK_, Section) = 1 then begin   { nur Sections, die mit Datenimport_TK_... beginnen }
        with ImportDirData do begin
          sTypkennung:=Copy (Section, length (CSectionDatenimport_TK_) + 1, length (Section));
          iBuf:=StrToIntDef (sTypkennung, -1);
          if (iBuf >= Low (Typkennung)) AND (iBuf <= High (Typkennung)) then
            Typkennung:=iBuf  { Typkennung im gültigen Byte-Bereich }
          else begin
            Result:=-2;
            exit;
          end;
          DirName:=ReadString (Section, CIdentDirName, '');

          { Prüfung auf leeren Verzeichnisnamen: }
          if length (DirName) = 0 then begin
            Result:=-1;
            exit;
          end;
          { vollständigen Verzeichnisnamen bilden: }
          DirName:=IncludeTrailingBackslash (ExpandUNCFileName (DirName));
        end;

        { Ausgabeverzeichnis-Einstellung in Liste laden: }
        ImportDirDataObj:=TImportDirDataObj.Create (ImportDirData);
        L.Add (ImportDirDataObj);
      end;
    end;  { for i }
  finally
    SectionList.Free;
  end;
end;

{---------------------------------------------}
function TIec32Ini.GetHeartbeatFile: TFileName;
{---------------------------------------------}
// Gibt zurück, ob eine Lebenszeichendatei geschrieben werden soll
// Rückgabe: Name der Lebenszeichendatei oder ''
begin
  Result :=
    GDExpandFilePath(ReadString(CSectionDebug, CIdentHeartbeatFile, ''));
  if (Result <> '') and (not ForceDirectories(ExtractFilePath(Result))) then
    Result := '';
end;

{----------------------------------------------------------}
function TIec32Ini.GetHeartbeatFile_Datatransfer: TFileName;
{----------------------------------------------------------}
// Gibt zurück, ob eine Lebenszeichendatei geschrieben werden soll, wenn ein
// Datentransfer zwischen Kopplung und IEC-Gegenstelle aktiv ist;
// Rückgabe: Name der Lebenszeichendatei oder ''
begin
  Result :=
    GDExpandFilePath(ReadString(CSectionDebug, CIdentHeartbeatFile_Datatransfer, ''));
  if (Result <> '') and (not ForceDirectories(ExtractFilePath(Result))) then
    Result := '';
end;

{ Typkennungen }

{-------------------------------------------------------------------}
function TIec32Ini.GetTypkennungAktiv (Typkennung: integer): boolean;
{-------------------------------------------------------------------}
var
  S: string;

begin
  S:=CIdentTK_ + IntToStr (Typkennung);
  Result := ReadBool (CSectionTypkennungen, S, false);
end;

{ Remote-Monitor }

{------------------------------------}
function TIec32Ini.GetRMPort: integer;
{------------------------------------}
begin
  Result := ReadInteger (CSectionRemoteMonitor, CIdentPort, CIPPortIEC_RM_Default);
end;

{ Trigger-Dateien }

{----------------------------------------------------}
function TIec32Ini.GetTriggerdatei_DatenReset: string;
{----------------------------------------------------}
begin
  Result := ExpandUNCFileName (
    GDExpandFilePath(ReadString (CSectionTriggerdateien, CIdentDatenReset, '')));
end;

end.
