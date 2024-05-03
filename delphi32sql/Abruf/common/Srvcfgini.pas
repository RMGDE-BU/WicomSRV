{******************************************************************************}
{* Unit: Zugriffe auf SRVCFG32.INI                                            *}
{*       enthält alle Schnittstellen-Einstellungen für das Abrufsystem        *}
{* Version: 28.11.2000   WW                                                   *}
{  01.08.2005  GD  Erweiterung um "GetLogZuordnung"                            }
{  28.11.2005  GD  Erweiterung um "GetInSxCircle"                              }
{* 03.05.2006  WW  nutzbare COMs auf Serial.MaxPort+1 erweitert               *}
{  03.04.2007  GD  Kompilerschalter GAMESS durch INI-Eintrag ersetzt           }
{* 22.03.2012  WW  Erweiterung um Einstellungen für Signaturserver            *}
{* 21.11.2013  WW  Erweiterungen "Concat_Vorwahl_Rufnummer",                  *}
{*                  "GetVorwahl_DefaultBlank"                                 *}
{* 18.12.2013  WW  Erweiterungen Debug: Rundpuffer ein/aus, Rundpuffergröße   *}
{* 08.01.2018  WW  Erweiterungen für TCP/IP-Rufentgegennahme                  *}
{* 03.06.2020  WW  Erweiterung Server SSL-Version und Basis-Authentifizierung *}
{* 05.10.2022  WW  Erweiterung Server IPBind                                  *}
{******************************************************************************}
unit SrvCfgIni;

interface

uses
  Classes, IniFiles, SysUtils, SerialConst, Lizenz32, WPorts, DB_Attn;

const
  CMaxIPLinien = 256;   { max. Anzahl an parallel abwickelbaren IP-Verbindungen
                          im WicomSrv }
  COffsetIPLinien_Ruf = -200;  { Rufentgegennahme-IP-Linien: -201..-256 }

  { Themen für serielle Schnittstellen }
  maxDevices = 3;
  Devices: array [1..maxDevices] of string = ('FUP', 'Modem', 'Seriell');

 { weitere Themen }
  sthTCP_IP = 'TCP/IP';     { Kommunikation über TCP/IP }

  devFUP     = 1;
  devModem   = 2;
  devSeriell = 3;

 { Einstellwerte für Rufentgegennahme/SMS-Datenempfang }
  re_Aus        = 0;   { keine Rufentgegegnnahme }
  re_DSfG       = 1;   { Rufentgegennahme für DSfG (Device = Modem) }
  re_MRG_Modem  = 2;   { Rufentgegennahme für MRG-Modemgeräte (Device = Modem) }
  re_MRG_FUP    = 3;   { Rufentgegennahme für MRG-FUP-Geräte (Device = Modem) }
  re_FUP        = 4;   { Rufentgegennahme für MRG-FUP-Geräte (Device = FUP) }
  re_SMS        = 5;   { SMS-Datenempfang (Device = Modem (GSM)) }
  re_MRG_FTL_SR = 6;   { Rufentgegennahme für Tritschler Stationsrechner SR (Device = Modem) }

  { Einstellwerte im Ini-File }
  CNoDevice = '';  { kein Kommunikationsgerät an COM angeschlossen }

  { Einstellwerte für Wahlverfahren }
  CTonwahl  = 't';
  CPulswahl = 'p';

  { Einstellwerte für COM-Anzeige }
  CCOMAnzeige_Undef = -1;  // keine Anzeige-Einstellung vorhanden
  CCOMAnzeige_Nein  =  0;
  CCOMAnzeige_Ja    =  1;

  { Trennzeichen zwischen den Fup- und Modem-Initialisierungsstrings: }
  CFupModemInit_Trenner = ';';

  { Sections }
  CSectionCOMn = 'COM%d';

type
  { Strukur für Zuordnung logische -> physikalische Schnittstellen
    Index: logische; Wert: physikalische }
  TLogPhysComZuordnung = array [1..MaxPort+1] of integer;

  { Objekt für SRVCFG32.INI }

  TSrvCfg32Ini = class (TIniFile)
  private
    FdtSrvCfgIni: TDateTime;
    function GetFileDateTime: TDateTime;
    procedure SetTCP_IP_RufListe (slRufe: TStrings);
  protected
    { Server: }
    function GetServerHost: string;
    procedure SetServerHost(Value: string);
    function GetServerIPAdresse: string;
    procedure SetServerIPAdresse(Value: string);
    function GetServerPortID: integer;
    procedure SetServerPortID(Value: integer);
    function GetServerGPRSPort: integer;
    procedure SetServerGPRSPort(Value: integer);
    function GetServerRufPort_DSfG: integer;
    procedure SetServerRufPort_DSfG(Value: integer);
    function GetServerSSLVersion: integer;  // 03.06.2020, WW
    function GetServerBasicAuthentication: string;  // 03.06.2020, WW
    function GetServerIPBind: string;  // 05.10.2022, WW
    procedure SetServerIPBind(Value: string);  // 05.10.2022, WW
    { Debug: }
    function GetDebServiceIO: boolean;
    procedure SetDebServiceIO(Value: boolean);
    function GetDebAbrufProtokoll: boolean;
    procedure SetDebAbrufProtokoll(Value: boolean);
    function GetDebCOMProtokoll: boolean;
    procedure SetDebCOMProtokoll(Value: boolean);
    function GetDebRohdaten: boolean;
    procedure SetDebRohdaten(Value: boolean);
    function GetDebRundpuffer: boolean;
    procedure SetDebRundpuffer(Value: boolean);
    function GetDebRundpuffergroesse_kB: integer;
    procedure SetDebRundpuffergroesse_kB(Value: integer);
    { COM: }
    function GetDevice(COM: integer): string;
    procedure SetDevice(COM: integer; Value: string);
    function GetFupHandshake(COM: integer): boolean;
    procedure SetFupHandshake(COM: integer; Value: boolean);
    function GetFupInit(COM: integer): string;
    procedure SetFupInit(COM: integer; Value: string);
    function GetFup1200(COM: integer): string;
    procedure SetFup1200(COM: integer; Value: string);
    function GetFup2400(COM: integer): string;
    procedure SetFup2400(COM: integer; Value: string);
    function GetModemname(COM: integer): string;
    procedure SetModemname(COM: integer; Value: string);
    function GetRingCount(COM: integer): integer;
    procedure SetRingCount(COM: integer; Value: integer);
    function GetRuf(COM: integer): integer;
    function GetRuf_Fup(COM: integer): integer;
    procedure SetRuf_Fup(COM: integer; Value: integer);
    function GetRuf_Modem(COM: integer): integer;
    procedure SetRuf_Modem(COM: integer; Value: integer);
    function GetPIN(COM: integer): string;
    procedure SetPIN(COM: integer; Value: string);
    function GetAbrufAktiv(COM: integer): boolean;
    procedure SetAbrufAktiv(COM: integer; Value: boolean);
    function GetInSxCircle(iCOM: integer): boolean;  // 28.11.2005
    procedure SetInSxCircle(iCOM: integer; bValue: boolean);  // 28.11.2005
    function GetPIN_Lock(iCOM: integer): boolean;  // 05.12.2005
    procedure SetPIN_Lock(iCOM: integer; bValue: boolean);  // 05.12.2005
    function GetVorwahl(COM: integer): string;
    procedure SetVorwahl(COM: integer; Value: string);
    function GetAnzeige(COM: integer): integer;
    procedure SetAnzeige(COM: integer; Value: integer);
    { Zuordnung: }
    function GetCOMZuordnung(LogS: integer): integer;
    procedure SetCOMZuordnung(LogS: integer; Value: integer);
    function GetLogZuordnung(iCom: integer): integer; // 01.08.2005
    { TCP/IP: }
    function GetTCP_IP_DSfG: boolean;
    procedure SetTCP_IP_DSfG(Value: boolean);
    function GetTCP_IP_RufAktiv(iRuftyp: integer): boolean;
    procedure SetTCP_IP_RufAktiv(iRuftyp: integer; Value: boolean);
    { Sonstiges }
    function GetDJZBDATJrnCount: byte;
    { Signatur-Server: }
    function GetSigServerVerbindung: boolean;
    procedure SetSigServerVerbindung(Value: boolean);
    function GetSigServerIPAdresse: string;
    procedure SetSigServerIPAdresse(Value: string);
    function GetSigServerPort: integer;
    procedure SetSigServerPort(Value: integer);
  public
    constructor Create(Path: TFilename);
    function FileDateTimeChanged: boolean;  // 18.12.2013, WW
    procedure DeleteCOM(COM: integer);
    procedure DeleteZuordnungen;
    function GetVorwahl_DefaultBlank(COM: integer): string;  // 21.11.2013, WW
    procedure GetTCP_IP_RufListe (slRufe: TStrings);
    procedure InitTCP_IP_RufListe (iRuftyp: integer; iRufCount: integer = 1);
    { Server: }
    property ServerHost: string read GetServerHost write SetServerHost;     { Hostname, unter dem der Abrufserver erreichbar ist }
    property ServerIPAdresse: string read GetServerIPAdresse write SetServerIPAdresse; { IP-Adresse, unter der der Abrufserver erreichbar ist }
    property ServerPortID: integer read GetServerPortID write SetServerPortID;   { Port, auf dem der Abrufserver-Socket lauscht }
    property ServerGPRSPort: integer read GetServerGPRSPort write SetServerGPRSPort;   { Port des GPRS-Serversocket }
    property ServerRufPort_DSfG: integer read GetServerRufPort_DSfG write
      SetServerRufPort_DSfG;   { Port des DSfG-Rufentgegennahme-Serversocket }
    property ServerSSLVersion: integer read GetServerSSLVersion;   { SSL-Version für Kommunikation Client/Server }
    property ServerBasicAuthentication: string read GetServerBasicAuthentication;   { HTTP-Basis-Authentifizierung für Kommunikation Client/Server }
    property ServerIPBind: string read GetServerIPBind write SetServerIPBind;   { IP-Adresse, auf der der Abrufserver-Socket lauscht }
    { Debug: }
    property DebServiceIO: boolean read GetDebServiceIO write SetDebServiceIO;
    property DebAbrufProtokoll: boolean read GetDebAbrufProtokoll write SetDebAbrufProtokoll;
    property DebCOMProtokoll: boolean read GetDebCOMProtokoll write SetDebCOMProtokoll;
    property DebRohdaten: boolean read GetDebRohdaten write SetDebRohdaten;
    property DebRundpuffer: boolean read GetDebRundpuffer write SetDebRundpuffer;
    property DebRundpuffergroesse_kB: integer read GetDebRundpuffergroesse_kB
      write SetDebRundpuffergroesse_kB;
    { COM: }
    property Device[COM: integer]: string read GetDevice write SetDevice;   { angeschlossenes Gerät (FUP, Modem) }
    property FupHandshake[COM: integer]: boolean read GetFupHandshake write SetFupHandshake;  // 04.10.2006
    property FupInit[COM: integer]: string read GetFupInit write SetFupInit;
    property Fup1200[COM: integer]: string read GetFup1200 write SetFup1200;
    property Fup2400[COM: integer]: string read GetFup2400 write SetFup2400;
    property Modemname[COM: integer]: string read GetModemname write SetModemname;
    property RingCount[COM: integer]: integer read GetRingcount write SetRingcount;
    property Ruf[COM: integer]: integer read GetRuf;
    property Ruf_Fup[COM: integer]: integer read GetRuf_Fup write SetRuf_Fup;
    property Ruf_Modem[COM: integer]: integer read GetRuf_Modem write SetRuf_Modem;
    property PIN[COM: integer]: string read GetPIN write SetPIN;
    property AbrufAktiv[COM: integer]: boolean read GetAbrufAktiv write SetAbrufAktiv;
    property InSxCircle[COM: integer]: boolean  // 28.11.2005
      read GetInSxCircle write SetInSxCircle;
    property PIN_Lock[COM: integer]: boolean read GetPIN_Lock write SetPIN_Lock;
    property Vorwahl[COM: integer]: string read GetVorwahl write SetVorwahl;  // 28.04.2006
    property Anzeige[COM: integer]: integer read GetAnzeige write SetAnzeige; // 04.05.2006
    { Zuordnung: }
    property COMZuordnung[LogS: integer]: integer read GetCOMZuordnung write SetCOMZuordnung;  { Zuordnung: log. Schnittstelle -> COM }
    property LogZuordnung[iCom: integer]: integer read GetLogZuordnung;  { Zuordnung: COM -> log. Schnittstelle}
    { TCP/IP: }
    property TCP_IP_DSfG: boolean read GetTCP_IP_DSfG write SetTCP_IP_DSfG;  { DSfG über TCP/IP }
    property TCP_IP_RufAktiv[iRuftyp: integer]: boolean read GetTCP_IP_RufAktiv
      write SetTCP_IP_RufAktiv;  { TCP/IP-Rufentgegennahme für Ruftyp aktiv ja/nein }
    { Sonstige }
    property DJZBDATJrnCount: byte read GetDJZBDATJrnCount;  // 03.04.2007
    { Signatur-Server: }
    property SigServerVerbindung: boolean read GetSigServerVerbindung write SetSigServerVerbindung; { Verbindung zum Signatur-Serversocket ja/nein }
    property SigServerIPAdresse: string read GetSigServerIPAdresse write SetSigServerIPAdresse; { IP-Adresse des Signatur-Serversocket }
    property SigServerPort: integer read GetSigServerPort write SetSigServerPort;   { Port des Signaturserver-Socket }

    function Get_LogPhysComZuordnung: TLogPhysComZuordnung;
  end;

function FCOM_Name (COMNr: integer): string;
function Concat_Vorwahl_Rufnummer (const AVorwahl, ARufnummer: string): string;

implementation

Const
  CSrvCfg32Ini = 'SRVCFG32.INI';

  { Sections }
  CSectionServer         = 'Server';
  CSectionDebug          = 'Debug';
  CSectionZuordnung      = 'Zuordnung';
  CSectionTCP_IP         = 'TCP/IP';
  CSection_Others        = 'OTHERS';  // 03.04.2007
  CSectionSignaturserver = 'Signaturserver';

  { Idents }
  CIdentHost      = 'Host';
  CIdentIPAdresse = 'IPAdresse';
  CIdentPortID    = 'PortID';
  CIdentGPRSPort  = 'GPRSPort';
  CIdentRufPort_DSfG = 'RufPort_DSfG';
  CIdentSSLVersion = 'SSLVersion';
  CIdentBasicAuthentication = 'BasicAuthentication';
  CIdentIPBind = 'IPBind';

  CIdentServiceIO      = 'ServiceIO';
  CIdentAbrufProtokoll = 'AbrufProtokoll';
  CIdentCOMProtokoll   = 'COMProtokoll';
  CIdentRohdaten       = 'Rohdaten';
  CIdentRundpuffer     = 'Rundpuffer';
  CIdentRundpuffergroesse_kB = 'Rundpuffergroesse_kB';

  CIdentLogSn       = 'S%d';

  CIdentDevice       = 'Device';
  CIdentFupHandshake = 'FupHandshake';
  CIdentFupInit      = 'FupInit';
  CIdentFup1200      = 'Fup1200';
  CIdentFup2400      = 'Fup2400';
  CIdentModemname    = 'Modemname';
  CIdentRingCount    = 'RingCount';
  CIdentRuf_Fup      = 'Ruf_Fup';
  CIdentRuf_Modem    = 'Ruf_Modem';
  CIdentDSfG         = 'DSfG';
  CIdentPIN          = 'PIN';
  CIdentAbrufAktiv   = 'AbrufAktiv';
  CIdentInSxCircle   = 'InSxCircle';
  CIdentPIN_Lock     = 'PIN_Lock';
  CIdentVorwahl      = 'Vorwahl';
  CIdentAnzeige      = 'Anzeige';

  CIdent_DJZBDATCnt =  'DJZBDATCOUNT';  // Anzahl der Einträge in DJZBDAT

  CIdentVerbindung = 'Verbindung';

  CIdentRufAktiv_n = 'RufAktiv_%d';
  CIdentRufe       = 'Rufe';
                       
  { Einstellwerte }
  CFupInit = 'D99'+CFupModemInit_Trenner+'I10'+CFupModemInit_Trenner+'S2';
  CFup1200 = 'S2';
  CFup2400 = 'S6';
  CModemAllgemein = '(Allgemein)';
  CEin = 'Ein';
  CAus = 'Aus';


{------------------------------------------}
function FCOM_Name (COMNr: integer): string;
{------------------------------------------}
{ COM-Portname zu übergebener COM-Nummer bilden }
begin
  Result:='COM' + IntToStr (COMNr);
end;

{-----------------------------------------------------------------------------}
function Concat_Vorwahl_Rufnummer (const AVorwahl, ARufnummer: string): string;
{-----------------------------------------------------------------------------}
{ Rufnummer um Vorwahl erweitern;
  Übergaben: Rufnummer
             Vorwahl
  Ergebnis: Rufnummer mit Vorwahl }
var
  S: string;

begin
  // Erstes Zeichen in der Rufnummer kann Zeichen für Ton- oder Pulswahl sein:
  S:=Copy (ARufnummer, 1, 1);
  if (LowerCase (S) = CTonwahl) OR (LowerCase (S) = CPulswahl) then
    Result:=S + AVorwahl + Copy (ARufnummer, 2, length (ARufnummer))
  else
    Result:=AVorwahl + ARufnummer;
end;


{ TSrvCfg32Ini }

{-----------------------------------------------}
constructor TSrvCfg32Ini.Create(Path: TFilename);
{-----------------------------------------------}
begin
  inherited Create(Path + CSrvCfg32Ini);

  { Zeitstempel der INI-Datei lesen: }
  FdtSrvCfgIni:=GetFileDateTime;
end;

{-----------------------------------------------}
function TSrvCfg32Ini.GetFileDateTime: TDateTime;
{-----------------------------------------------}
{ Liefert Zeitstempel der Datei }
begin
  Result:=GetTriggerTime (FileName, false);
end;

{-------------------------------------------------}
function TSrvCfg32Ini.FileDateTimeChanged: boolean;
{-------------------------------------------------}
{ Liefert true, wenn sich seit dem letzten Funktionsaufruf bzw. Create der
  Zeitstempel der Datei geändert hat }
var
  dtBuf: TDateTime;

begin
  dtBuf:=GetFileDateTime;
  if dtBuf <> FdtSrvCfgIni then begin   // Datei-Datum hat sich geändert
    FdtSrvCfgIni:=dtBuf;
    Result:=true;
  end else
    Result:=false;
end;

{ Server: }
{------------------------------------------}
function TSrvCfg32Ini.GetServerHost: string;
{------------------------------------------}
var
  S: string;
begin
  S := ReadString (CSectionServer, CIdentHost, '');
  // Default-Rückgabe 'Rechnername', wenn weder Hostname noch IP-Adresse
  // eingetragen sind; 10.01.2006 WW
  if (length (S) = 0) AND (length (ServerIPAdresse) = 0) then
    S := MyGetComputerName;
  Result:=S;
end;

{--------------------------------------------------}
procedure TSrvCfg32Ini.SetServerHost(Value: string);
{--------------------------------------------------}
begin
  WriteString(CSectionServer, CIdentHost, Value);
end;

{-----------------------------------------------}
function TSrvCfg32Ini.GetServerIPAdresse: string;
{-----------------------------------------------}
begin
  Result := ReadString (CSectionServer, CIdentIPAdresse, '');
end;

{-------------------------------------------------------}
procedure TSrvCfg32Ini.SetServerIPAdresse(Value: string);
{-------------------------------------------------------}
begin
  WriteString(CSectionServer, CIdentIPAdresse, Value);
end;

{---------------------------------------------}
function TSrvCfg32Ini.GetServerPortID: integer;
{---------------------------------------------}
begin
  Result := ReadInteger (CSectionServer, CIdentPortID, CIPPortWicomSrv_Default);
end;

{-----------------------------------------------------}
procedure TSrvCfg32Ini.SetServerPortID(Value: integer);
{-----------------------------------------------------}
begin
  WriteInteger(CSectionServer, CIdentPortID, Value);
end;

{-----------------------------------------------}
function TSrvCfg32Ini.GetServerGPRSPort: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionServer, CIdentGPRSPort, CIPPortWicomSrv_GPRS_Default);
end;

{-------------------------------------------------------}
procedure TSrvCfg32Ini.SetServerGPRSPort(Value: integer);
{-------------------------------------------------------}
begin
  WriteInteger(CSectionServer, CIdentGPRSPort, Value);
end;

{---------------------------------------------------}
function TSrvCfg32Ini.GetServerRufPort_DSfG: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger (CSectionServer, CIdentRufPort_DSfG, CIPPortWicomSrv_RufDSfG_Default);
end;

{-----------------------------------------------------------}
procedure TSrvCfg32Ini.SetServerRufPort_DSfG(Value: integer);
{-----------------------------------------------------------}
begin
  WriteInteger(CSectionServer, CIdentRufPort_DSfG, Value);
end;

{-------------------------------------------------}
function TSrvCfg32Ini.GetServerSSLVersion: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionServer, CIdentSSLVersion, -1);  // Default: Keine Verschlüsselung
end;

{---------------------------------------------------------}
function TSrvCfg32Ini.GetServerBasicAuthentication: string;
{---------------------------------------------------------}
begin
  Result := ReadString (CSectionServer, CIdentBasicAuthentication, '');  // Default: Keine Basis-Authentifizierung
end;

{--------------------------------------------}
function TSrvCfg32Ini.GetServerIPBind: string;
{--------------------------------------------}
begin
  Result := ReadString (CSectionServer, CIdentIPBind, '0.0.0.0');  // Default: Alle IP-Adressen
end;

{----------------------------------------------------}
procedure TSrvCfg32Ini.SetServerIPBind(Value: string);
{----------------------------------------------------}
begin
  WriteString(CSectionServer, CIdentIPBind, Value);
end;


{ Debug: }
{---------------------------------------------}
function TSrvCfg32Ini.GetDebServiceIO: boolean;
{---------------------------------------------}
begin
  if UpperCase (ReadString (CSectionDebug, CIdentServiceIO, CAus)) = UpperCase (CEin) then
    Result:= true
  else
    Result:= false;
end;

{-----------------------------------------------------}
procedure TSrvCfg32Ini.SetDebServiceIO(Value: boolean);
{-----------------------------------------------------}
begin
  if Value then
    WriteString(CSectionDebug, CIdentServiceIO, CEin)
  else
    WriteString(CSectionDebug, CIdentServiceIO, CAus);
end;

{--------------------------------------------------}
function TSrvCfg32Ini.GetDebAbrufProtokoll: boolean;
{--------------------------------------------------}
begin
  if UpperCase (ReadString (CSectionDebug, CIdentAbrufProtokoll, CAus)) = UpperCase (CEin) then
    Result:= true
  else
    Result:= false;
end;

{----------------------------------------------------------}
procedure TSrvCfg32Ini.SetDebAbrufProtokoll(Value: boolean);
{----------------------------------------------------------}
begin
  if Value then
    WriteString(CSectionDebug, CIdentAbrufProtokoll, CEin)
  else
    WriteString(CSectionDebug, CIdentAbrufProtokoll, CAus);
end;

{------------------------------------------------}
function TSrvCfg32Ini.GetDebCOMProtokoll: boolean;
{------------------------------------------------}
begin
  if UpperCase (ReadString (CSectionDebug, CIdentCOMProtokoll, CAus)) = UpperCase (CEin) then
    Result:= true
  else
    Result:= false;
end;

{--------------------------------------------------------}
procedure TSrvCfg32Ini.SetDebCOMProtokoll(Value: boolean);
{--------------------------------------------------------}
begin
  if Value then
    WriteString(CSectionDebug, CIdentCOMProtokoll, CEin)
  else
    WriteString(CSectionDebug, CIdentCOMProtokoll, CAus);
end;

{--------------------------------------------}
function TSrvCfg32Ini.GetDebRohdaten: boolean;
{--------------------------------------------}
begin
  if UpperCase (ReadString (CSectionDebug, CIdentRohdaten, CAus)) = UpperCase (CEin) then
    Result:= true
  else
    Result:= false;
end;

{----------------------------------------------------}
procedure TSrvCfg32Ini.SetDebRohdaten(Value: boolean);
{----------------------------------------------------}
begin
  if Value then
    WriteString(CSectionDebug, CIdentRohdaten, CEin)
  else
    WriteString(CSectionDebug, CIdentRohdaten, CAus);
end;

{----------------------------------------------}
function TSrvCfg32Ini.GetDebRundpuffer: boolean;
{----------------------------------------------}
begin
  if UpperCase (ReadString (CSectionDebug, CIdentRundpuffer, CAus)) = UpperCase (CEin) then
    Result:= true
  else
    Result:= false;
end;

{------------------------------------------------------}
procedure TSrvCfg32Ini.SetDebRundpuffer(Value: boolean);
{------------------------------------------------------}
begin
  if Value then
    WriteString(CSectionDebug, CIdentRundpuffer, CEin)
  else
    WriteString(CSectionDebug, CIdentRundpuffer, CAus);
end;

{--------------------------------------------------------}
function TSrvCfg32Ini.GetDebRundpuffergroesse_kB: integer;
{--------------------------------------------------------}
begin
  Result := ReadInteger (CSectionDebug, CIdentRundpuffergroesse_kB, 500);
end;

{----------------------------------------------------------------}
procedure TSrvCfg32Ini.SetDebRundpuffergroesse_kB(Value: integer);
{----------------------------------------------------------------}
begin
  WriteInteger(CSectionDebug, CIdentRundpuffergroesse_kB, Value);
end;

{ COM: }
{----------------------------------------------------}
function TSrvCfg32Ini.GetDevice(COM: integer): string;
{----------------------------------------------------}
begin
  Result:= ReadString(Format (CSectionCOMn, [COM]), CIdentDevice, CNoDevice);
end;

{------------------------------------------------------------}
procedure TSrvCfg32Ini.SetDevice(COM: integer; Value: string);
{------------------------------------------------------------}
begin
  WriteString(Format (CSectionCOMn, [COM]), CIdentDevice, Value);
end;

{-----------------------------------------------------------}
function TSrvCfg32Ini.GetFupHandshake(COM: integer): boolean;
{-----------------------------------------------------------}
begin
  Result:= ReadBool(Format (CSectionCOMn, [COM]), CIdentFupHandshake, true);  // 04.10.2006
end;

{-------------------------------------------------------------------}
procedure TSrvCfg32Ini.SetFupHandshake(COM: integer; Value: boolean);
{-------------------------------------------------------------------}
begin
  WriteBool(Format (CSectionCOMn, [COM]), CIdentFupHandshake, Value);  // 04.10.2006
end;

{-----------------------------------------------------}
function TSrvCfg32Ini.GetFupInit(COM: integer): string;
{-----------------------------------------------------}
begin
  Result:= ReadString(Format (CSectionCOMn, [COM]), CIdentFupInit, CFupInit);
end;

{-------------------------------------------------------------}
procedure TSrvCfg32Ini.SetFupInit(COM: integer; Value: string);
{-------------------------------------------------------------}
begin
  WriteString(Format (CSectionCOMn, [COM]), CIdentFupInit, Value);
end;

{-----------------------------------------------------}
function TSrvCfg32Ini.GetFup1200(COM: integer): string;
{-----------------------------------------------------}
begin
  Result:= ReadString(Format (CSectionCOMn, [COM]), CIdentFup1200, CFup1200);
end;

{-------------------------------------------------------------}
procedure TSrvCfg32Ini.SetFup1200(COM: integer; Value: string);
{-------------------------------------------------------------}
begin
  WriteString(Format (CSectionCOMn, [COM]), CIdentFup1200, Value);
end;

{-----------------------------------------------------}
function TSrvCfg32Ini.GetFup2400(COM: integer): string;
{-----------------------------------------------------}
begin
  Result:= ReadString(Format (CSectionCOMn, [COM]), CIdentFup2400, CFup2400);
end;

{-------------------------------------------------------------}
procedure TSrvCfg32Ini.SetFup2400(COM: integer; Value: string);
{-------------------------------------------------------------}
begin
  WriteString(Format (CSectionCOMn, [COM]), CIdentFup2400, Value);
end;

{-------------------------------------------------------}
function TSrvCfg32Ini.GetModemname(COM: integer): string;
{-------------------------------------------------------}
begin
  Result:= ReadString(Format (CSectionCOMn, [COM]), CIdentModemname, CModemAllgemein);
end;

{---------------------------------------------------------------}
procedure TSrvCfg32Ini.SetModemname(COM: integer; Value: string);
{---------------------------------------------------------------}
begin
  WriteString(Format (CSectionCOMn, [COM]), CIdentModemname, Value);
end;

{--------------------------------------------------------}
function TSrvCfg32Ini.GetRingCount(COM: integer): integer;
{--------------------------------------------------------}
begin
  Result:= ReadInteger(Format (CSectionCOMn, [COM]), CIdentRingCount, 1);
end;

{----------------------------------------------------------------}
procedure TSrvCfg32Ini.SetRingCount(COM: integer; Value: integer);
{----------------------------------------------------------------}
begin
  WriteInteger(Format (CSectionCOMn, [COM]), CIdentRingCount, Value);
end;

{--------------------------------------------------}
function TSrvCfg32Ini.GetRuf(COM: integer): integer;
{--------------------------------------------------}
var
  Device: string;
begin
  Device:=UpperCase (GetDevice(COM));
  if Device = UpperCase (Devices [devFUP]) then
    Result:=GetRuf_Fup(COM)
  else if Device = UpperCase (Devices [devModem]) then
    Result:=GetRuf_Modem(COM)
  else
    Result:=re_Aus;
end;

{------------------------------------------------------}
function TSrvCfg32Ini.GetRuf_Fup(COM: integer): integer;
{------------------------------------------------------}
begin
  Result:=ReadInteger(Format (CSectionCOMn, [COM]), CIdentRuf_Fup, re_Aus);
end;

{--------------------------------------------------------------}
procedure TSrvCfg32Ini.SetRuf_Fup(COM: integer; Value: integer);
{--------------------------------------------------------------}
begin
  WriteInteger(Format (CSectionCOMn, [COM]), CIdentRuf_Fup, Value);
end;

{--------------------------------------------------------}
function TSrvCfg32Ini.GetRuf_Modem(COM: integer): integer;
{--------------------------------------------------------}
begin
  Result:=ReadInteger(Format (CSectionCOMn, [COM]), CIdentRuf_Modem, re_Aus);
end;

{----------------------------------------------------------------}
procedure TSrvCfg32Ini.SetRuf_Modem(COM: integer; Value: integer);
{----------------------------------------------------------------}
begin
  WriteInteger(Format (CSectionCOMn, [COM]), CIdentRuf_Modem, Value);
end;

{-------------------------------------------------}
function TSrvCfg32Ini.GetPIN(COM: integer): string;
{-------------------------------------------------}
begin
  Result:= ReadString (Format (CSectionCOMn, [COM]), CIdentPIN, '');
end;

{---------------------------------------------------------}
procedure TSrvCfg32Ini.SetPIN(COM: integer; Value: string);
{---------------------------------------------------------}
begin
  WriteString(Format (CSectionCOMn, [COM]), CIdentPIN, Value);
end;

{---------------------------------------------------------}
function TSrvCfg32Ini.GetAbrufAktiv(COM: integer): boolean;
{---------------------------------------------------------}
begin
  // Standard: COM ist für Abrufe Zentrale -> Station freigegeben
  Result:= ReadBool (Format (CSectionCOMn, [COM]), CIdentAbrufAktiv, true);
end;

{-----------------------------------------------------------------}
procedure TSrvCfg32Ini.SetAbrufAktiv(COM: integer; Value: boolean);
{-----------------------------------------------------------------}
begin
  WriteBool(Format (CSectionCOMn, [COM]), CIdentAbrufAktiv, Value);
end;

{---------------------------------------------------------}
function TSrvCfg32Ini.GetInSxCircle(iCOM: integer): boolean;  // 28.11.2005
{---------------------------------------------------------}
begin
  // Standard: COM ist für wahlfreien Abruf freigegeben
  Result := ReadBool(Format(CSectionCOMn, [iCOM]), CIdentInSxCircle, True);
end;

{-----------------------------------------------------------------}
procedure TSrvCfg32Ini.SetInSxCircle(iCOM: integer; bValue: boolean);  // 28.11.2005
{-----------------------------------------------------------------}
begin
  WriteBool(Format(CSectionCOMn, [iCOM]), CIdentInSxCircle, bValue);
end;

{--------------------------------------------------------}
function TSrvCfg32Ini.GetPIN_Lock(iCOM: integer): boolean;  // 05.12.2005
{--------------------------------------------------------}
begin
  // Standard: PIN-Eingabe ist nicht gesperrt
  Result := ReadBool(Format(CSectionCOMn, [iCOM]), CIdentPIN_Lock, False);
end;

{-----------------------------------------------------------------}
procedure TSrvCfg32Ini.SetPIN_Lock(iCOM: integer; bValue: boolean);  // 05.12.2005
{-----------------------------------------------------------------}
begin
  WriteBool(Format(CSectionCOMn, [iCOM]), CIdentPIN_Lock, bValue);
end;

{------------------------------------------------------------------}
function TSrvCfg32Ini.GetVorwahl_DefaultBlank(COM: integer): string;  // 21.11.2013, WW
{------------------------------------------------------------------}
begin
  Result:= ReadString(Format (CSectionCOMn, [COM]), CIdentVorwahl, ''); // so wie in INI gespeichert
end;

{-----------------------------------------------------}
function TSrvCfg32Ini.GetVorwahl(COM: integer): string;
{-----------------------------------------------------}
begin
  Result:= ReadString(Format (CSectionCOMn, [COM]), CIdentVorwahl, CTonwahl);
end;

{-------------------------------------------------------------}
procedure TSrvCfg32Ini.SetVorwahl(COM: integer; Value: string);
{-------------------------------------------------------------}
begin
  WriteString(Format (CSectionCOMn, [COM]), CIdentVorwahl, Value);
end;

{------------------------------------------------------}
function TSrvCfg32Ini.GetAnzeige(COM: integer): integer;
{------------------------------------------------------}
begin
  Result:=ReadInteger(Format (CSectionCOMn, [COM]), CIdentAnzeige, CCOMAnzeige_Undef);
end;

{--------------------------------------------------------------}
procedure TSrvCfg32Ini.SetAnzeige(COM: integer; Value: integer);
{--------------------------------------------------------------}
begin
  WriteInteger(Format (CSectionCOMn, [COM]), CIdentAnzeige, Value);
end;

{ Einstellungen einer COM löschen }
{---------------------------------------------}
procedure TSrvCfg32Ini.DeleteCOM(COM: integer);
{---------------------------------------------}
begin
  EraseSection(Format (CSectionCOMn, [COM]));
end;

{ Zuordnung: }
{------------------------------------------------------------}
function TSrvCfg32Ini.GetCOMZuordnung(LogS: integer): integer;
{------------------------------------------------------------}
var
  S: string;
begin
  { Zuordnung logische Schnittstelle -> COM-Nr. (Default: Sn = COMn): }
  S:=ReadString(CSectionZuordnung, Format (CIdentLogSn, [LogS]), Format (CSectionCOMn, [LogS]));
  S:=UpperCase (S);
  if Pos ('COM', S) =  1 then begin
    S:=Copy (S, 4, length (S));  { COM-Nummer }
    try
      Result:=StrToInt (S);
    except
      Result:=LogS;
    end;
  end else
    Result:=LogS;
end;

{--------------------------------------------------------------------}
procedure TSrvCfg32Ini.SetCOMZuordnung(LogS: integer; Value: integer);
{--------------------------------------------------------------------}
begin
  WriteString(CSectionZuordnung, Format (CIdentLogSn, [LogS]), Format (CSectionCOMn, [Value]));
end;

{ Zuordnung COM-Nr. -> logische Schnittstelle                }
{ Parameter: Physikalische Schnittstellennummer              }
{ Rückgabe: Nummer der (1.) zugeh. logischen Schnittstelle   }
{------------------------------------------------------------}
function TSrvCfg32Ini.GetLogZuordnung(iCom: integer): integer;
{------------------------------------------------------------}
var
  i, iLog : integer;
  s          : string;
begin
  Result := -1;
  for iLog := 1 to (MaxPort+1) do begin
    s := ReadString(CSectionZuordnung,
      Format(CIdentLogSn, [iLog]), Format (CSectionCOMn, [iLog]));
    if (Pos('COM', UpperCase(s)) =  1) then begin
      i := StrToIntDef(Copy(s, 4, Length(s)-3), -1);
      if (i = iCom) then begin
        Result := iLog;
        Break;
      end;
    end;
  end;
end;

{ alle Zuordnungen COM-Nr. -> logische Schnittstelle aus INI löschen }
{---------------------------------------}
procedure TSrvCfg32Ini.DeleteZuordnungen;
{---------------------------------------}
begin
  EraseSection(CSectionZuordnung);
end;

{------------------------------------------------------------------}
function TSrvCfg32Ini.Get_LogPhysComZuordnung: TLogPhysComZuordnung;
{------------------------------------------------------------------}
{ Ergebnis: Schnittstellenzuordnung "logisch" -> "physikalisch" }
var
  l: integer;
  Zuord: TLogPhysComZuordnung;

begin
  for l:=Low (Zuord) to High (Zuord) do
    Zuord [l]:=GetCOMZuordnung (l);
  Result:=Zuord;
end;

{----------------------- TCP/IP-Einstellungen ---------------------------------}

{ DSfG über TCP/IP: }
{--------------------------------------------}
function TSrvCfg32Ini.GetTCP_IP_DSfG: boolean;
{--------------------------------------------}
begin
  if ReadString (CSectionTCP_IP, CIdentDSfG, CAus) = CEin then
    Result:= true
  else
    Result:= false;
end;

{----------------------------------------------------}
procedure TSrvCfg32Ini.SetTCP_IP_DSfG(Value: boolean);
{----------------------------------------------------}
begin
  if Value then
    WriteString(CSectionTCP_IP, CIdentDSfG, CEin)
  else
    WriteString(CSectionTCP_IP, CIdentDSfG, CAus);
end;


{ Rufentgegennahme per TCP/IP: }
{------------------------------------------------------------------}
function TSrvCfg32Ini.GetTCP_IP_RufAktiv(iRuftyp: integer): boolean;
{------------------------------------------------------------------}
begin
  Result := ReadBool (CSectionTCP_IP, Format (CIdentRufAktiv_n, [iRuftyp]), false);  // Default: RE nicht aktiviert
end;

{--------------------------------------------------------------------------}
procedure TSrvCfg32Ini.SetTCP_IP_RufAktiv(iRuftyp: integer; Value: boolean);
{--------------------------------------------------------------------------}
begin
  WriteBool (CSectionTCP_IP, Format (CIdentRufAktiv_n, [iRuftyp]), Value);
end;

{-----------------------------------------------------------}
procedure TSrvCfg32Ini.GetTCP_IP_RufListe (slRufe: TStrings);
{-----------------------------------------------------------}
var
  S: string;

begin
  if Assigned (slRufe) then begin
    S := ReadString (CSectionTCP_IP, CIdentRufe, '');
    slRufe.CommaText:=S;
  end;
end;

{-----------------------------------------------------------}
procedure TSrvCfg32Ini.SetTCP_IP_RufListe (slRufe: TStrings);
{-----------------------------------------------------------}
var
  S: string;

begin
  if Assigned (slRufe) then begin
    S:=slRufe.CommaText;
    WriteString (CSectionTCP_IP, CIdentRufe, S);
  end;
end;

{------------------------------------------------------------------------------------}
procedure TSrvCfg32Ini.InitTCP_IP_RufListe (iRuftyp: integer; iRufCount: integer = 1);
{------------------------------------------------------------------------------------}
{ TCP/IP-Rufliste für Ruftyp initialisieren, sofern Ruftyp noch nicht in der
  Rufliste enthalten ist;
  Übergabe: Ruftyp (re_..-Konstante)
            Anzahl der Ruftyp-Einträge für Initialisierung }
var
  sRuftyp: string;
  i: integer;
  slRufe: TStringList;
  bAdd: boolean;

begin
  slRufe:=TStringList.Create;
  try
    GetTCP_IP_RufListe (slRufe);  // aktuelle TCP/IP-Rufliste lesen
    // Prüfen, ob Ruftyp in der Liste bereits enthalten ist:
    if iRuftyp > re_Aus then begin
      bAdd:=true;
      for i:=0 to slRufe.Count - 1 do begin
        if StrToIntDef (slRufe [i], -1) = iRuftyp then begin
          // Ruftyp bereits enthalten
          bAdd:=false;
          Break;
        end;
      end;

      if bAdd then begin
        // Liste für Ruftyp erweitern
        sRuftyp:=IntToStr (iRuftyp);
        for i:=1 to iRufCount do
          slRufe.Add (sRuftyp);
        SetTCP_IP_RufListe (slRufe);  // TCP/IP-Rufliste speichern
      end;
    end;
  finally
    slRufe.Free;
  end;
end;

{---------------------------------- Sonstiges ---------------------------------}

{------------------------------------------------------------------}
function TSrvCfg32Ini.GetDJZBDATJrnCount: byte;
{------------------------------------------------------------------}
begin
 Result := ReadInteger(CSection_Others, CIdent_DJZBDATCnt, 5);
end;

{----------------------------- Signaturserver ---------------------------------}

{----------------------------------------------------}
function TSrvCfg32Ini.GetSigServerVerbindung: boolean;
{----------------------------------------------------}
begin
  Result := ReadBool (CSectionSignaturserver, CIdentVerbindung, false);  // Default: Keine Verbindung zum Signaturserver herstellen
end;

{----------------------------------------------------------}
procedure TSrvCfg32Ini.SetSigServerVerbindung(Value: boolean);
{----------------------------------------------------------}
begin
  WriteBool(CSectionSignaturserver, CIdentVerbindung, Value);
end;

{--------------------------------------------------}
function TSrvCfg32Ini.GetSigServerIPAdresse: string;
{--------------------------------------------------}
begin
  Result := ReadString (CSectionSignaturserver, CIdentIPAdresse, '127.0.0.1');  // Default: lokal
end;

{----------------------------------------------------------}
procedure TSrvCfg32Ini.SetSigServerIPAdresse(Value: string);
{----------------------------------------------------------}
begin
  WriteString(CSectionSignaturserver, CIdentIPAdresse, Value);
end;

{----------------------------------------------}
function TSrvCfg32Ini.GetSigServerPort: integer;
{----------------------------------------------}
begin
  Result := ReadInteger (CSectionSignaturserver, CIdentPortID, CIPPortSignaturSrv_Default);
end;

{------------------------------------------------------}
procedure TSrvCfg32Ini.SetSigServerPort(Value: integer);
{------------------------------------------------------}
begin
  WriteInteger(CSectionSignaturserver, CIdentPortID, Value);
end;


end.
