{******************************************************************************}
{* Unit: Zugriff auf INI-Datei des Abrufserverdienstes                        *}
{* Version: 23.12.2002  WW                                                    *}
{******************************************************************************}
unit AbrufSrvIniFile;

interface

uses
  Classes, SysUtils, PathIni, T_Zeit, AbrufTimeoutConst, AusgabeDirList;

type
  { Objekt für WICOMSRV.INI }

  TAbrufSrvIni = class (TProgramIni)
  protected
    { WicomSrv allgemein }
    function GetHeartbeatFile: string;
    { Zeitsynchronisation: }
    function GetZeitSyncAbweichungMin: integer;
    function GetZeitSyncAbweichungMax: integer;
    function GetZeitSyncKorrekturMax: integer;
    function GetZeitSyncGPRSAktiv: boolean;
    function GetZeitSyncGPRSStart: TDatetime;
    { Abruf allgemein: }
    function GetTOModemAntwort: integer;
    function GetTOModemInit: integer;
    function GetTOGSMModem: integer;
    function GetVerbindungHalten: integer;  // 02.05.2013, WW
    function GetXMLResponseEncodeRohdaten: integer;  // 07.04.2014, WW
    { Abruf MRG: }
    function GetMRG_TOFupAntwort: integer;
    function GetMRG_TOFupReset: integer;
    function GetMRG_TOCRCCheck: integer;
    function GetMRG_TOACK01ProtMeldung: integer;
    function GetMRG_TOIEC1107Telegr: integer;
    function GetMRG_TOTritschlerIECProt: integer;
    function GetMRG_TOTritschlerFTLProt: integer;
    function GetMRG_TOCorusSAMProt: integer;
    function GetMRG_TOElsterDS100Prot: integer;
    function GetMRG_TOModbusProt: integer;  // 08.03.2019, WW
    function GetMRG_TOVerbindungsaufbau: integer;
    function GetMRG_TOVerbindungsabbau: integer;
    function GetMRG_TORufAnnahmeModem: integer;
    function GetMRG_TOKennung: integer;
    function GetMRG_TOLogin: integer;
    function GetMRG_TOParameter: integer;
    function GetMRG_TOMeldungen: integer;
    function GetMRG_TOMesswerte: integer;
    function GetMRG_TOTagessaetze: integer;
    function GetMRG_TOPruefsaetze: integer;
    function GetMRG_TOBinaerdatei: integer;
    function GetMRG_TORundpufferReset: integer;
    function GetMRG_TOParametrieren: integer;
    function GetMRG_TORufausloesung: integer;
    function GetMRG_TODSfGUmschaltung: integer;
    function GetMRG_TODSfGRufliste: integer;
    function GetMRG_TODSfGRufQuittung: integer;
    function GetMRG_CRCVersuche: integer;
    function GetMRG_BCCVersuche: integer;
    function GetMRG_ACK01ProtVersuche: integer;
    function GetMRG_FTLProtVersuche: integer;
    function GetMRG_ModbusProtLRC_CRCVersuche: integer;  // 08.03.2019, WW
    function GetMRG_KennungExt: string;
    function GetMRG_ParaNr_Kennung (MrgName: string): string;
    function GetMRG_ArchivExt (MrgName: string; ArchivNr: integer): string;  // 24.08.2023, WW
    { Abruf DSfG: }
    function GetDSfG_TOVerbindungsaufbau: integer;
    function GetDSfG_TOVerbindungsabbau: integer;
    function GetDSfG_TORufAnnahme: integer;
    function GetDSfG_TOLogin: integer;
    function GetDSfG_TODFUETransparent: integer;
    function GetDSfG_TOArchive: integer;
    function GetDSfG_TOLogbuecher: integer;
    function GetDSfG_TODatenelemente: integer;
    function GetDSfG_TOEinstellen: integer;
    function GetDSfG_TODFUEParameter: integer;
    function GetDSfG_TOBinaerdatei: integer;
    function GetDSfG_BCCVersuche: integer;
    function GetDSfG_ISO646: boolean;  // 03.03.2010, WW
    { Gas-X }
    function GetGasX_AIX_kompatibel_MRG_Analog: boolean;
    { SMS }
    function GetSMS_BackupAktiv: boolean;
    function GetSMS_BackupDir: string;
    function GetSMS_ImportDir: string;
    { GPRS }
    function GetGPRS_AusgabeKurzzeitwerte: boolean;
    function GetGPRS_AnzeigeRohdaten: boolean;
    function GetGPRS_DebugDatenProtokoll: boolean;
    function GetGPRS_DebugStatistikProtokoll: boolean;
    function GetGPRS_LetztTelegrammNr: integer;
    procedure SetGPRS_LetztTelegrammNr (Value: integer);
    { Firmware-Update }
    function GetFwUpd_FwDir: string;
    { Simulation }
    function GetSimuArchivInit_Monate: integer;
    function GetSimuDSfGArchivIntervall: integer;
    function GetSimuDSfGLogbuchIntervall_MaxRandom: integer;
    function GetSimuZeitVerbindungsaufbau: integer;
    function GetSimuZeitAntwortTelegramm: integer;
    function GetLinienKeineVerbindung: string;
  public
    constructor Create;
    { WicomSrv allgemein }
    property HeartbeatFile: string read GetHeartbeatFile;  { Name der Lebenszeichen-Datei }
    { Zeitsynchronisation: }
    property ZeitSyncAbweichungMin: integer read GetZeitSyncAbweichungMin;  { Min. Abweichung, damit synchronisiert wird }
    property ZeitSyncAbweichungMax: integer read GetZeitSyncAbweichungMax;  { Max. Abweichung, damit synchronisiert wird }
    property ZeitSyncKorrekturMax: integer read GetZeitSyncKorrekturMax;    { Max. Korrekturzeit }
    property ZeitSyncGPRSAktiv: boolean read GetZeitSyncGPRSAktiv;          { GPRS-Zeitsynchronisation ein/aus }
    property ZeitSyncGPRSStart: TDateTime read GetZeitSyncGPRSStart;        { Uhrzeit für Zeitsynchronisation }
    { Abruf allgemein: }
    property TOModemAntwort: integer read GetTOModemAntwort;                { Timeout Modem-Antwort }
    property TOModemInit: integer read GetTOModemInit;                      { Timeout Modeminitialisierung }
    property TOGSMModem: integer read GetTOGSMModem;                        { Timeout GSM-Modem-Antwort }
    property VerbindungHalten: integer read GetVerbindungHalten;            { Verbindung zur Station halten }
    property XMLResponseEncodeRohdaten: integer read
      GetXMLResponseEncodeRohdaten;                                         { Kodierverfahren für Rohdaten in XML-Response }
    { Abruf MRG: }
    property MRG_TOFupAntwort: integer read GetMRG_TOFupAntwort;               { Timeout FUP-Antwort }
    property MRG_TOFupReset: integer read GetMRG_TOFupReset;                   { Timeout FUP-Reset }
    property MRG_TOCRCCheck: integer read GetMRG_TOCRCCheck;                   { Timeout CRC-Check }
    property MRG_TOACK01ProtMeldung: integer read GetMRG_TOACK01ProtMeldung;   { Timeout ACK0/1-Protokollmeldung }
    property MRG_TOIEC1107Telegr: integer read GetMRG_TOIEC1107Telegr;         { Timeout IEC1107-Telegramm }
    property MRG_TOTritschlerIECProt: integer read GetMRG_TOTritschlerIECProt; { Timeout Tritschler IEC-Protokoll }
    property MRG_TOTritschlerFTLProt: integer read GetMRG_TOTritschlerFTLProt; { Timeout Tritschler FTL-Protokoll }
    property MRG_TOCorusSAMProt: integer read GetMRG_TOCorusSAMProt;           { Timeout Actaris Corus (SAM-Protokoll) }
    property MRG_TOElsterDS100Prot: integer read GetMRG_TOElsterDS100Prot;     { Timeout Elster DS-100-Protokoll }
    property MRG_TOModbusProt: integer read GetMRG_TOModbusProt;               { Timeout Modbus-Protokoll }
    property MRG_TOVerbindungsaufbau: integer read GetMRG_TOVerbindungsaufbau; { Timeout Verbindungsaufbau }
    property MRG_TOVerbindungsabbau: integer read GetMRG_TOVerbindungsabbau;   { Timeout Verbindungsabbau }
    property MRG_TORufAnnahmeModem: integer read GetMRG_TORufAnnahmeModem;     { Timeout Rufannahme mit Modem }
    property MRG_TOKennung: integer read GetMRG_TOKennung;                     { Timeout Kennung lesen }
    property MRG_TOLogin: integer read GetMRG_TOLogin;                         { Timeout DFÜ-Login }
    property MRG_TOParameter: integer read GetMRG_TOParameter;                 { Timeout Parameter lesen }
    property MRG_TOMeldungen: integer read GetMRG_TOMeldungen;                 { Timeout Meldungen lesen }
    property MRG_TOMesswerte: integer read GetMRG_TOMesswerte;                 { Timeout Meßwerte lesen }
    property MRG_TOTagessaetze: integer read GetMRG_TOTagessaetze;             { Timeout Tagessätze lesen }
    property MRG_TOPruefsaetze: integer read GetMRG_TOPruefsaetze;             { Timeout Prüfungssätze lesen }
    property MRG_TOBinaerdatei: integer read GetMRG_TOBinaerdatei;             { Timeout Binärdatei-Befehl übertragen }
    property MRG_TORundpufferReset: integer read GetMRG_TORundpufferReset;     { Timeout Rundpufferreset durchführen }
    property MRG_TOParametrieren: integer read GetMRG_TOParametrieren;         { Timeout Parameter übertragen }
    property MRG_TORufausloesung: integer read GetMRG_TORufausloesung;         { Timeout Ruf im MRG auslösen }
    property MRG_TODSfGUmschaltung: integer read GetMRG_TODSfGUmschaltung;     { Timeout bei DSfG-Umleitung: umschalten auf Slave }
    property MRG_TODSfGRufliste: integer read GetMRG_TODSfGRufliste;           { Timeout bei DSfG-Umleitung: Rufliste abfragen }
    property MRG_TODSfGRufQuittung: integer read GetMRG_TODSfGRufQuittung;     { Timeout bei DSfG-Umleitung: Ruf quittieren }
    property MRG_CRCVersuche: integer read GetMRG_CRCVersuche;                 { max. Versuche bei falschem CRC }
    property MRG_BCCVersuche: integer read GetMRG_BCCVersuche;                 { max. Versuche bei falschem BCC }
    property MRG_ACK01ProtVersuche: integer read GetMRG_ACK01ProtVersuche;     { max. Versuche bei falscher ACK0/1-Protokollantwort }
    property MRG_FTLProtVersuche: integer read GetMRG_FTLProtVersuche;         { max. Versuche bei neg. Quittierung/Blocksummenfehler (Tritschler FTL-Protokoll) }
    property MRG_ModbusProtLRC_CRCVersuche: integer read
      GetMRG_ModbusProtLRC_CRCVersuche;                                        { Modbus-Protokoll: max. Versuche bei fehlerhaftem LRC (ASCII) bzw. CRC (RTU) }
    property MRG_KennungExt: string read GetMRG_KennungExt;                    { Erweiterung bei Kennungen (STGW) }
    property MRG_ParaNr_Kennung [MrgName: string]: string read                 { MRG-typabhängig: Parameternummer für Kennungsabfrage }
      GetMRG_ParaNr_Kennung;
    property MRG_ArchivExt [MrgName: string; ArchivNr: integer]: string read   { MRG-typabhängig: Erweiterte Archivabfrage für E-Befehl }
      GetMRG_ArchivExt;
    { Abruf DSfG: }
    property DSfG_TOVerbindungsaufbau: integer read GetDSfG_TOVerbindungsaufbau; { Timeout Verbindungsaufbau }
    property DSfG_TOVerbindungsabbau: integer read GetDSfG_TOVerbindungsabbau;   { Timeout Verbindungsabbau }
    property DSfG_TORufAnnahme: integer read GetDSfG_TORufAnnahme;               { Timeout Rufannahme }
    property DSfG_TOLogin: integer read GetDSfG_TOLogin;                         { Timeout DFÜ-Login }
    property DSfG_TODFUETransparent: integer read GetDSfG_TODFUETransparent;     { Timeout DFÜ transparentschalten }
    property DSfG_TOArchive: integer read GetDSfG_TOArchive;                     { Timeout Archive lesen }
    property DSfG_TOLogbuecher: integer read GetDSfG_TOLogbuecher;               { Timeout Logbücher lesen }
    property DSfG_TODatenelemente: integer read GetDSfG_TODatenelemente;         { Timeout Datenelemente lesen }
    property DSfG_TOEinstellen: integer read GetDSfG_TOEinstellen;               { Timeout Datenelemente einstellen }
    property DSfG_TODFUEParameter: integer read GetDSfG_TODFUEParameter;         { Timeout DSfG-DFÜ-Parameter lesen/einstellen }
    property DSfG_TOBinaerdatei: integer read GetDSfG_TOBinaerdatei;             { Timeout Binärdateibefehl senden }
    property DSfG_BCCVersuche: integer read GetDSfG_BCCVersuche;                 { max. Versuche bei falschem BCC }
    property DSfG_ISO646: boolean read GetDSfG_ISO646;                           { Zeichen-Konvertierung ASCII/ISO 646 ein/aus }
    { Gas-X }
    property GasX_AIX_kompatibel_MRG_Analog: boolean read                        { MRG-Analogwerte kompatibel zu AIX-System (normiert) }
      GetGasX_AIX_kompatibel_MRG_Analog;
    { SMS }
    property SMS_BackupDir: string read GetSMS_BackupDir;                        { Verzeichnis für Kopien von SMS-XML-Dateien }
    property SMS_ImportDir: string read GetSMS_ImportDir;                        { Verzeichnis für Import von SMS-Rohdaten-Dateien }
    { GPRS }
    function GetGPRS_AusgabeDirList (L: TAusgabeDirList; DefaultDir: string): integer;
    property GPRS_AusgabeKurzzeitwerte: boolean read GetGPRS_AusgabeKurzzeitwerte;
    property GPRS_AnzeigeRohdaten: boolean read GetGPRS_AnzeigeRohdaten;
    property GPRS_DebugDatenProtokoll: boolean read GetGPRS_DebugDatenProtokoll;
    property GPRS_DebugStatistikProtokoll: boolean read GetGPRS_DebugStatistikProtokoll;
    property GPRS_LetztTelegrammNr: integer read GetGPRS_LetztTelegrammNr write
      SetGPRS_LetztTelegrammNr;
    { Firmware-Update }
    property FwUpd_FwDir: string read GetFwUpd_FwDir;  { Verzeichnis mit Firmware-Update-Dateien }
    { Simulation }
    property SimuArchivInit_Monate: integer read GetSimuArchivInit_Monate;
    property SimuDSfGArchivIntervall: integer read GetSimuDSfGArchivIntervall;
    property SimuDSfGLogbuchIntervall_MaxRandom: integer read
      GetSimuDSfGLogbuchIntervall_MaxRandom;
    property SimuZeitVerbindungsaufbau: integer read GetSimuZeitVerbindungsaufbau;
    property SimuZeitAntwortTelegramm: integer read GetSimuZeitAntwortTelegramm;
    property SimuLinienKeineVerbindung: string read GetLinienKeineVerbindung;
  end;

implementation

uses IniFiles;

const
  { Sections }
  CSectionProgram = 'WicomSrv';

  CSectionAbruf_Allgemein = 'Abruf_Allgemein';
  CSectionAbruf_MRG       = 'Abruf_MRG';
  CSectionAbruf_DSfG      = 'Abruf_DSfG';
  CSectionZeitSync        = 'Zeitsynchronisation';
  CSectionGasX            = 'Gas-X';
  CSectionSMS             = 'SMS';

  CSectionGPRS_Ausgabe       = 'GPRS_Ausgabe';
  CSectionGPRS_AusgabeDir_   = 'GPRS_AusgabeDir_';
  CSectionGPRS_Anzeige       = 'GPRS_Anzeige';
  CSectionGPRS_Debug         = 'GPRS_Debug';
  CSectionGPRS_Telegramme    = 'GPRS_Telegramme';

  CSectionFirmwareupdate = 'Firmwareupdate';

  CSectionSimulation = 'Simulation';

  { Idents }
  CIdentTOModemAntwort      = 'TOModemAntwort';
  CIdentTOModemInit         = 'TOModemInit';
  CIdentTOGSMModem          = 'TOGSMModem';
  CIdentVerbindungHalten    = 'VerbindungHalten';
  CIdentXMLResponseEncodeRohdaten = 'XMLResponseEncodeRohdaten';

  CIdentTOFupAntwort        = 'TOFupAntwort';
  CIdentTOFupReset          = 'TOFupReset';
  CIdentTOCRCCheck          = 'TOCRCCheck';
  CIdentTOACK01ProtMeldung  = 'TOACK01ProtMeldung';
  CIdentTOIEC1107Telegr     = 'TOIEC1107Telegr';
  CIdentTOTritschlerIECProt = 'TOTritschlerIECProt';
  CIdentTOTritschlerFTLProt = 'TOTritschlerFTLProt';
  CIdentTOCorusSAMProt      = 'TOCorusSAMProt';
  CIdentTOElsterDS100Prot   = 'TOElsterDS100Prot';
  CIdentTOModbusProt        = 'TOModbusProt';
  CIdentTOVerbindungsaufbau = 'TOVerbindungsaufbau';
  CIdentTOVerbindungsabbau  = 'TOVerbindungsabbau';
  CIdentTORufAnnahmeModem   = 'TORufAnnahmeModem';
  CIdentTOKennung           = 'TOKennung';
  CIdentTOLogin             = 'TOLogin';
  CIdentTOParameter         = 'TOParameter';
  CIdentTOMeldungen         = 'TOMeldungen';
  CIdentTOMesswerte         = 'TOMesswerte';
  CIdentTOTagessaetze       = 'TOTagessaetze';
  CIdentTOPruefsaetze       = 'TOPruefsaetze';
  CIdentTOBinaerdatei       = 'TOBinaerdatei';
  CIdentTORundpufferReset   = 'TORundpufferReset';
  CIdentTOParametrieren     = 'TOParametrieren';
  CIdentTORufausloesung     = 'TORufausloesung';
  CIdentTODSfGUmschaltung   = 'TODSfGUmschaltung';
  CIdentTODSfGRufliste      = 'TODSfGRufliste';
  CIdentTODSfGRufQuittung   = 'TODSfGRufQuittung';
  CIdentCRCVersuche         = 'CRCVersuche';
  CIdentBCCVersuche         = 'BCCVersuche';
  CIdentACK01ProtVersuche   = 'ACK01ProtVersuche';
  CIdentFTLProtVersuche     = 'FTLProtVersuche';
  CIdentModbusProtLRC_CRCVersuche = 'ModbusProtLRC_CRCVersuche';

  CIdentTORufAnnahme        = 'TORufAnnahme';
  CIdentTODFUETransparent   = 'TODFUETransparent';
  CIdentTOArchive           = 'TOArchive';
  CIdentTOLogbuecher        = 'TOLogbuecher';
  CIdentTODatenelemente     = 'TODatenelemente';
  CIdentTOEinstellen        = 'TOEinstellen';
  CIdentTODFUEParameter     = 'TODFUEParameter';

  CIdentAbweichungMin       = 'AbweichungMin';
  CIdentAbweichungMax       = 'AbweichungMax';
  CIdentKorrekturMax        = 'KorrekturMax';
  CIdentGPRSAktiv           = 'GPRSAktiv';
  CIdentGPRSStart           = 'GPRSStart';

  CIdentAIX_kompatibel_MRG_Analog = 'AIX_kompatibel_MRG_Analog';

  CIdentKennungExt          = 'KennungExt';

  CIdentParaNr_Kennung      = 'ParaNr_Kennung';

  CIdentArchiv = 'Archiv';

  CIdentBackupAktiv = 'BackupAktiv';
  CIdentBackupDir   = 'BackupDir';
  CIdentImportDir   = 'ImportDir';

  CIdentKurzzeitwerte       = 'Kurzzeitwerte';
  CIdentDirName             = 'DirName';
  CIdentBezeichnung         = 'Bezeichnung';
  CIdentClientVerbindungen  = 'ClientVerbindungen';
  CIdentDatenformat         = 'Datenformat';
  CIdentDatenProtokoll      = 'DatenProtokoll';
  CIdentStatistikProtokoll  = 'StatistikProtokoll';
  CIdentRohdaten            = 'Rohdaten';
  CIdentLetztTelegrammNr    = 'LetztTelegrammNr';

  CIdentISO646 = 'ISO646';

  CIdentFwDir = 'FwDir';

  CIdentHeartbeatFile = 'HeartbeatFile';

  CIdentArchivInit_Monate                = 'ArchivInit_Monate';
  CIdentDSfGArchivIntervall_s            = 'DSfGArchivIntervall_s';
  CIdentDSfGLogbuchIntervall_MaxRandom_s = 'DSfGLogbuchIntervall_MaxRandom_s';
  CIdentZeitVerbindungsaufbau_ms         = 'ZeitVerbindungsaufbau_ms';
  CIdentZeitAntwortTelegramm_ms          = 'ZeitAntwortTelegramm_ms';
  CIdentLinienKeineVerbindung            = 'LinienKeineVerbindung';

  { Einstellwerte }
  CZeitSyncAbweichMin = 10;  { Standardwert in s }  // von 30 auf 10 s geändert wg. eichamtl. Geräten; 05.05.2006
  CZeitSyncAbweichMax = 60;  { Standardwert in s }
  CZeitSyncKorrMax    =  0;  { Standardwert: Zeitkorrektur aus (es wird synchronisiert) }

  CAIX_kompatibel_MRG_Analog = false;  { Standard: nicht kompatibel }

  CMRG_KennungExt = '';   { Standard: Bilden einer Ersatzkennung ohne Erweiterung
                                      ist bei Kennungsvergleich ausgeschaltet }

  CMRG_ParaNr_Kennung = '';   { gerätetypabhängige Standard-Parameternummer wird für
                                Kennungsabfrage verwendet; 18.12.2009 }

  CMRG_ArchivExt = '';   { Standard: Erweiterte Archivabfrage ist ausgeschaltet }

  CBackupAktiv = false;  { Standard: kein Backup }
  CBackupDir   = '..\Daten\';  { Standard: Datenverzeichnis }

  CDefaultDir = '<DEFAULTDIR>';  // Platzhalter für Standard-Verzeichnis
  CDefaultBezeichnung        = '';
  CDefaultClientVerbindungen = false;

  CGPRSStart  = '04:31:00';  { Standard-Uhrzeit für GPRS-Zeitsynchronisation }

  CFirmwareDir = '..\Daten\';  { Standard: Datenverzeichnis }


{ TAbrufSrvIni }

{------------------------------}
constructor TAbrufSrvIni.Create;
{------------------------------}
begin
  inherited Create (true, false);  // kein benutzerdefinierter Zugriff
end;

{ WicomSrv allgemein }
{---------------------------------------------}
function TAbrufSrvIni.GetHeartbeatFile: string;
{---------------------------------------------}
var
  S: string;

begin
  S := ReadString (CSectionProgram, CIdentHeartbeatFile, '');  // Default: Keine Heartbeat-Datei definiert
  if length (S) > 0 then begin
    // bei relativen Pfaden Verzeichnis der Anwendung voranstellen:
    if Copy (S, 1, 1) = '.' then
      S:=ExtractFilePath (ParamStr (0)) + S;
    Result := ExpandUNCFileName (S);
  end else
    Result:='';
end;

{ Abruf allgemein: }
{-----------------------------------------------}
function TAbrufSrvIni.GetTOModemAntwort: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_Allgemein, CIdentTOModemAntwort, CTimeout_ModemAntwort);  { ms }
end;

{--------------------------------------------}
function TAbrufSrvIni.GetTOModemInit: integer;
{--------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_Allgemein, CIdentTOModemInit, CTimeout_ModemInit);      { ms }
end;

{-------------------------------------------}
function TAbrufSrvIni.GetTOGSMModem: integer;
{-------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_Allgemein, CIdentTOGSMModem, CTimeout_GSMModem);      { ms }
end;

{-------------------------------------------------}
function TAbrufSrvIni.GetVerbindungHalten: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_Allgemein, CIdentVerbindungHalten, 0);  { ms (0 = aus) }
  if (Result > 0) AND (Result < 10000) then
    Result:=10000;  { Minimum: 10 s }
end;

{----------------------------------------------------------}
function TAbrufSrvIni.GetXMLResponseEncodeRohdaten: integer;
{----------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_Allgemein, CIdentXMLResponseEncodeRohdaten, 0);  { 0 = Keine Rohdaten in XML-Response }
end;


{ Abruf MRG: }
{-------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOFupAntwort: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOFupAntwort, CMRG_Timeout_FupAntwort);      { ms }
end;

{-----------------------------------------------}
function TAbrufSrvIni.GetMRG_TOFupReset: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOFupReset, CMRG_Timeout_FupReset);      { ms }
end;

{-----------------------------------------------}
function TAbrufSrvIni.GetMRG_TOCRCCheck: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOCRCCheck, CMRG_Timeout_CRCCheck);      { ms }
end;

{-------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOACK01ProtMeldung: integer;
{-------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOACK01ProtMeldung, CMRG_Timeout_ACK01ProtMeldung);      { ms }
end;

{----------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOIEC1107Telegr: integer;
{----------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOIEC1107Telegr, CMRG_Timeout_IEC1107Telegr);      { ms }
end;

{--------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOTritschlerIECProt: integer;
{--------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOTritschlerIECProt, CMRG_Timeout_TritschlerIECProt);      { ms }
end;

{--------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOTritschlerFTLProt: integer;
{--------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOTritschlerFTLProt, CMRG_Timeout_TritschlerFTLProt);      { ms }
end;

{---------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOCorusSAMProt: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOCorusSAMProt, CMRG_Timeout_CorusSAMProt);      { ms }
end;

{------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOElsterDS100Prot: integer;
{------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOElsterDS100Prot, CMRG_Timeout_ElsterDS100Prot);      { ms }
end;

{-------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOModbusProt: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOModbusProt, CMRG_Timeout_ModbusProt);      { ms }
end;

{--------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOVerbindungsaufbau: integer;
{--------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOVerbindungsaufbau, CMRG_Timeout_Verbindungsaufbau);      { ms }
end;

{-------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOVerbindungsabbau: integer;
{-------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOVerbindungsabbau, CMRG_Timeout_Verbindungsabbau);      { ms }
end;

{------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TORufAnnahmeModem: integer;
{------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTORufAnnahmeModem, CMRG_Timeout_RufAnnahmeModem);      { ms }
end;

{----------------------------------------------}
function TAbrufSrvIni.GetMRG_TOKennung: integer;
{----------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOKennung, CMRG_Timeout_Kennung);      { ms }
end;

{--------------------------------------------}
function TAbrufSrvIni.GetMRG_TOLogin: integer;
{--------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOLogin, CMRG_Timeout_Login);      { ms }
end;

{------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOParameter: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOParameter, CMRG_Timeout_Parameter);      { ms }
end;

{------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOMeldungen: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOMeldungen, CMRG_Timeout_Meldungen);      { ms }
end;

{------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOMesswerte: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOMesswerte, CMRG_Timeout_Messwerte);      { ms }
end;

{--------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOTagessaetze: integer;
{--------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOTagessaetze, CMRG_Timeout_Tagessaetze);      { ms }
end;

{--------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOPruefsaetze: integer;
{--------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOPruefsaetze, CMRG_Timeout_Pruefsaetze);      { ms }
end;

{--------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOBinaerdatei: integer;
{--------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOBinaerdatei, CMRG_Timeout_Binaerdatei);      { ms }
end;

{------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TORundpufferReset: integer;
{------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTORundpufferReset, CMRG_Timeout_RundpufferReset);      { ms }
end;

{----------------------------------------------------}
function TAbrufSrvIni.GetMRG_TOParametrieren: integer;
{----------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTOParametrieren, CMRG_Timeout_Parametrieren);      { ms }
end;

{----------------------------------------------------}
function TAbrufSrvIni.GetMRG_TORufausloesung: integer;
{----------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTORufausloesung, CMRG_Timeout_Rufausloesung);      { ms }
end;

{------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TODSfGUmschaltung: integer;
{------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTODSfGUmschaltung, CMRG_Timeout_DSfGUmschaltung);      { ms }
end;

{---------------------------------------------------}
function TAbrufSrvIni.GetMRG_TODSfGRufliste: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTODSfGRufliste, CMRG_Timeout_DSfGRufliste);      { ms }
end;

{------------------------------------------------------}
function TAbrufSrvIni.GetMRG_TODSfGRufQuittung: integer;
{------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentTODSfGRufQuittung, CMRG_Timeout_DSfGRufQuittung);      { ms }
end;

{------------------------------------------------}
function TAbrufSrvIni.GetMRG_CRCVersuche: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentCRCVersuche, CMRG_CRCVersuche);
end;

{------------------------------------------------}
function TAbrufSrvIni.GetMRG_BCCVersuche: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentBCCVersuche, CMRG_BCCVersuche);
end;

{------------------------------------------------------}
function TAbrufSrvIni.GetMRG_ACK01ProtVersuche: integer;
{------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentACK01ProtVersuche, CMRG_ACK01ProtVersuche);
end;

{----------------------------------------------------}
function TAbrufSrvIni.GetMRG_FTLProtVersuche: integer;
{----------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentFTLProtVersuche, CMRG_FTLProtVersuche);
end;

{--------------------------------------------------------------}
function TAbrufSrvIni.GetMRG_ModbusProtLRC_CRCVersuche: integer;
{--------------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_MRG, CIdentModbusProtLRC_CRCVersuche, CModbus_LRC_CRCVersuche);
end;

{----------------------------------------------}
function TAbrufSrvIni.GetMRG_KennungExt: string;
{----------------------------------------------}
begin
  Result := ReadString (CSectionAbruf_MRG, CIdentKennungExt, CMRG_KennungExt);
end;

{--------------------------------------------------------------------}
function TAbrufSrvIni.GetMRG_ParaNr_Kennung (MrgName: string): string;
{--------------------------------------------------------------------}
begin
  Result := ReadString (MrgName, CIdentParaNr_Kennung, CMRG_ParaNr_Kennung);
end;

{--------------------------------------------------------------------}
function TAbrufSrvIni.GetMRG_ArchivExt (MrgName: string; ArchivNr: integer): string;
{--------------------------------------------------------------------}
begin
  Result := ReadString (MrgName, CIdentArchiv + IntToStr(ArchivNr), CMRG_ArchivExt);
end;


{ Abruf DSfG: }
{---------------------------------------------------------}
function TAbrufSrvIni.GetDSfG_TOVerbindungsaufbau: integer;
{---------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTOVerbindungsaufbau, CDSfG_Timeout_Verbindungsaufbau);      { ms }
end;

{--------------------------------------------------------}
function TAbrufSrvIni.GetDSfG_TOVerbindungsabbau: integer;
{--------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTOVerbindungsabbau, CDSfG_Timeout_Verbindungsabbau);      { ms }
end;

{--------------------------------------------------}
function TAbrufSrvIni.GetDSfG_TORufAnnahme: integer;
{--------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTORufAnnahme, CDSfG_Timeout_RufAnnahme);      { ms }
end;

{---------------------------------------------}
function TAbrufSrvIni.GetDSfG_TOLogin: integer;
{---------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTOLogin, CDSfG_Timeout_Login);              { ms }
end;

{-------------------------------------------------------}
function TAbrufSrvIni.GetDSfG_TODFUETransparent: integer;
{-------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTODFUETransparent, CDSfG_Timeout_DFUETransparent);              { ms }
end;

{-----------------------------------------------}
function TAbrufSrvIni.GetDSfG_TOArchive: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTOArchive, CDSfG_Timeout_Archive);              { ms }
end;

{--------------------------------------------------}
function TAbrufSrvIni.GetDSfG_TOLogbuecher: integer;
{--------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTOLogbuecher, CDSfG_Timeout_Logbuecher);              { ms }
end;

{-----------------------------------------------------}
function TAbrufSrvIni.GetDSfG_TODatenelemente: integer;
{-----------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTODatenelemente, CDSfG_Timeout_Datenelemente);              { ms }
end;

{--------------------------------------------------}
function TAbrufSrvIni.GetDSfG_TOEinstellen: integer;
{--------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTOEinstellen, CDSfG_Timeout_Einstellen);              { ms }
end;

{-----------------------------------------------------}
function TAbrufSrvIni.GetDSfG_TODFUEParameter: integer;
{-----------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTODFUEParameter, CDSfG_Timeout_DFUEParameter);        { ms }
end;

{---------------------------------------------------}
function TAbrufSrvIni.GetDSfG_TOBinaerdatei: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentTOBinaerdatei, CDSfG_Timeout_Binaerdatei);        { ms }
end;

{-------------------------------------------------}
function TAbrufSrvIni.GetDSfG_BCCVersuche: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionAbruf_DSfG, CIdentBCCVersuche, CDSfG_BCCVersuche);
end;

{--------------------------------------------}
function TAbrufSrvIni.GetDSfG_ISO646: boolean;
{--------------------------------------------}
begin
  Result := ReadBool (CSectionAbruf_DSfG, CIdentISO646, true);  // 03.03.2010, WW
end;

{ Zeitsynchronisation: }
{------------------------------------------------------}
function TAbrufSrvIni.GetZeitSyncAbweichungMin: integer;
{------------------------------------------------------}
begin
  Result := ReadInteger (CSectionZeitSync, CIdentAbweichungMin, CZeitSyncAbweichMin);
end;

{------------------------------------------------------}
function TAbrufSrvIni.GetZeitSyncAbweichungMax: integer;
{------------------------------------------------------}
begin
  Result := ReadInteger (CSectionZeitSync, CIdentAbweichungMax, CZeitSyncAbweichMax);
end;

{-----------------------------------------------------}
function TAbrufSrvIni.GetZeitSyncKorrekturMax: integer;
{-----------------------------------------------------}
begin
  Result := ReadInteger (CSectionZeitSync, CIdentKorrekturMax, CZeitSyncKorrMax);
end;

{--------------------------------------------------}
function TAbrufSrvIni.GetZeitSyncGPRSAktiv: boolean;
{--------------------------------------------------}
begin
  Result := ReadBool (CSectionZeitSync, CIdentGPRSAktiv, false);
end;

{----------------------------------------------------}
function TAbrufSrvIni.GetZeitSyncGPRSStart: TDatetime;
{----------------------------------------------------}
var
  S: string;

begin
  S:=ReadString (CSectionZeitSync, CIdentGPRSStart, CGPRSStart);
  if not EncodeTimeStr (S, 'HH:MM:SS', Result) then
    EncodeTimeStr (CGPRSStart, 'HH:MM:SS', Result);
end;

{ Gas-X: }
{---------------------------------------------------------------}
function TAbrufSrvIni.GetGasX_AIX_kompatibel_MRG_Analog: boolean;
{---------------------------------------------------------------}
begin
  Result := ReadBool (CSectionGasX, CIdentAIX_kompatibel_MRG_Analog, CAIX_kompatibel_MRG_Analog);
end;

{ SMS: }
{------------------------------------------------}
function TAbrufSrvIni.GetSMS_BackupAktiv: boolean;
{------------------------------------------------}
begin
  Result := ReadBool (CSectionSMS, CIdentBackupAktiv, CBackupAktiv);
end;

{---------------------------------------------}
function TAbrufSrvIni.GetSMS_BackupDir: string;
{---------------------------------------------}
var
  S: string;

begin
  if GetSMS_BackupAktiv then begin
    S := ReadString (CSectionSMS, CIdentBackupDir, CBackupDir);
    // bei relativen Pfaden Verzeichnis der Anwendung voranstellen:
    if Copy (S, 1, 1) = '.' then
      S:=ExtractFilePath (ParamStr (0)) + S;
    Result := ExpandUNCFileName (IncludeTrailingBackslash (S));
  end else
    Result:='';
end;

{---------------------------------------------}
function TAbrufSrvIni.GetSMS_ImportDir: string;
{---------------------------------------------}
var
  S: string;

begin
  S := ReadString (CSectionSMS, CIdentImportDir, '');  // Default: Importfunktion deaktiviert
  if length (S) > 0 then begin
    // bei relativen Pfaden Verzeichnis der Anwendung voranstellen:
    if Copy (S, 1, 1) = '.' then
      S:=ExtractFilePath (ParamStr (0)) + S;
    Result := ExpandUNCFileName (IncludeTrailingBackslash (S));
  end else
    Result:='';
end;

{ GPRS: }
{----------------------------------------------------------}
function TAbrufSrvIni.GetGPRS_AusgabeKurzzeitwerte: boolean;
{----------------------------------------------------------}
begin
  Result := ReadBool (CSectionGPRS_Ausgabe, CIdentKurzzeitwerte, true);
end;

{---------------------------------------------------------------------------------------------}
function TAbrufSrvIni.GetGPRS_AusgabeDirList (L: TAusgabeDirList; DefaultDir: string): integer;
{---------------------------------------------------------------------------------------------}
{ liest GPRS-Ausgabeverzeichnis-Einstellungen aus und gibt sie in L zurück;
  Ergebnis:  0 = Einstellungen OK
            -1 = Verzeichnisname fehlt (leer)
            -2 = gleiche Verzeichnisname(n) mehrfach vorhanden }
var
  SectionList: TStringList;
  i, j: integer;
  Section: string;
  AusgabeDirData: TAusgabeDirData;
  AusgabeDirDataObj: TAusgabeDirDataObj;

begin
  Result:=0;  // OK
  if L = nil then exit;
  L.Clear;

  SectionList:=TStringList.Create;
  try
    ReadSections(SectionList);
    for i:=0 to SectionList.Count-1 do begin
      Section:=SectionList[i];
      if Pos (CSectionGPRS_AusgabeDir_, Section) = 1 then begin   { nur Sections, die mit AusgabeDir_... beginnen }
        with AusgabeDirData do begin
          DirName:=ReadString (Section, CIdentDirName, '');
          Bezeichnung:=ReadString (Section, CIdentBezeichnung, CDefaultBezeichnung);
          ClientVerbindungen:=ReadBool (Section, CIdentClientVerbindungen, CDefaultClientVerbindungen);
          Datenformat:=ReadInteger (Section, CIdentDatenformat, C_DF_KZW_IEC);  // Standard: Kurzzeitwerte für IEC-Kopplung

          { Prüfung auf leeren Verzeichnisnamen: }
          if length (DirName) = 0 then begin
            Result:=-1;
            exit;
          end;
          { Verzeichnisnamen-Eintrag mit Default-Platzhalter ersetzen durch
            übergebenes Default-Verzeichnis: }
          if Uppercase (DirName) = CDefaultDir then
            DirName:=DefaultDir;
          { vollständigen Verzeichnisnamen bilden: }
          DirName:=IncludeTrailingBackslash (ExpandUNCFileName (DirName));
          { Prüfung auf mehrfach vorhandene, gleiche Verzeichnisnamen: }
          for j:=0 to (L.Count-1) do begin
            if TAusgabeDirDataObj (L [j]).Daten.DirName = DirName then begin
              Result:=-2;
              exit;
            end;
          end;
        end;

        { Ausgabeverzeichnis-Einstellung in Liste laden: }
        AusgabeDirDataObj:=TAusgabeDirDataObj.Create (AusgabeDirData);
        L.Add (AusgabeDirDataObj);
      end;
    end;  { for i }
  finally
    SectionList.Free;
  end;

  { keine Ausgabeverzeichnis-Einstellungen vorhanden: Default-Verzeichnis in
    Liste laden }
  if L.Count = 0 then begin
    with AusgabeDirData do begin
      DirName:=IncludeTrailingBackslash (ExpandUNCFileName (DefaultDir));
      Bezeichnung:=CDefaultBezeichnung;
      ClientVerbindungen:=CDefaultClientVerbindungen;
      Datenformat:=C_DF_KZW_IEC;  // Standard: Kurzzeitwerte für IEC-Kopplung
    end;
    AusgabeDirDataObj:=TAusgabeDirDataObj.Create (AusgabeDirData);
    L.Add (AusgabeDirDataObj);
  end;
end;

{-----------------------------------------------------}
function TAbrufSrvIni.GetGPRS_AnzeigeRohdaten: boolean;
{-----------------------------------------------------}
begin
  Result:=ReadBool (CSectionGPRS_Anzeige, CIdentRohdaten, false);
end;

{---------------------------------------------------------}
function TAbrufSrvIni.GetGPRS_DebugDatenProtokoll: boolean;
{---------------------------------------------------------}
begin
  Result:=ReadBool (CSectionGPRS_Debug, CIdentDatenProtokoll, false);
end;

{-------------------------------------------------------------}
function TAbrufSrvIni.GetGPRS_DebugStatistikProtokoll: boolean;
{-------------------------------------------------------------}
begin
  Result:=ReadBool (CSectionGPRS_Debug, CIdentStatistikProtokoll, false);
end;

{------------------------------------------------------}
function TAbrufSrvIni.GetGPRS_LetztTelegrammNr: integer;
{------------------------------------------------------}
begin
  Result:=ReadInteger (CSectionGPRS_Telegramme, CIdentLetztTelegrammNr, 0);
end;

{---------------------------------------------------------------}
procedure TAbrufSrvIni.SetGPRS_LetztTelegrammNr (Value: integer);
{---------------------------------------------------------------}
begin
  WriteInteger (CSectionGPRS_Telegramme, CIdentLetztTelegrammNr, Value);
end;

{ Firmware-Update: }

{-------------------------------------------}
function TAbrufSrvIni.GetFwUpd_FwDir: string;
{-------------------------------------------}
var
  S: string;

begin
  S := ReadString (CSectionFirmwareupdate, CIdentFwDir, CFirmwareDir);
  // bei relativen Pfaden Verzeichnis der Anwendung voranstellen:
  if Copy (S, 1, 1) = '.' then
    S:=ExtractFilePath (ParamStr (0)) + S;
  Result := ExpandUNCFileName (IncludeTrailingBackslash (S));
end;

{ Simulation: }

{------------------------------------------------------}
function TAbrufSrvIni.GetSimuArchivInit_Monate: integer;
{------------------------------------------------------}
begin
  Result:=ReadInteger (CSectionSimulation, CIdentArchivInit_Monate, 3);
  if Result < 0 then
    Result:=0;
end;

{--------------------------------------------------------}
function TAbrufSrvIni.GetSimuDSfGArchivIntervall: integer;
{--------------------------------------------------------}
begin
  Result:=ReadInteger (CSectionSimulation, CIdentDSfGArchivIntervall_s, 3600);
  if Result < 60 then
    Result:=60;  // Minimum 60 s
end;

{-------------------------------------------------------------------}
function TAbrufSrvIni.GetSimuDSfGLogbuchIntervall_MaxRandom: integer;
{-------------------------------------------------------------------}
begin
  Result:=ReadInteger (CSectionSimulation, CIdentDSfGLogbuchIntervall_MaxRandom_s, 3600);
  if Result < 60 then
    Result:=60;  // Minimum 60 s
end;

{----------------------------------------------------------}
function TAbrufSrvIni.GetSimuZeitVerbindungsaufbau: integer;
{----------------------------------------------------------}
begin
  Result:=ReadInteger (CSectionSimulation, CIdentZeitVerbindungsaufbau_ms, 5000);
  if Result < 0 then
    Result:=0;  // Minimum 0 ms
end;

{---------------------------------------------------------}
function TAbrufSrvIni.GetSimuZeitAntwortTelegramm: integer;
{---------------------------------------------------------}
begin
  Result:=ReadInteger (CSectionSimulation, CIdentZeitAntwortTelegramm_ms, 200);
  if Result < 0 then
    Result:=0;  // Minimum 0 ms
end;

{-----------------------------------------------------}
function TAbrufSrvIni.GetLinienKeineVerbindung: string;
{-----------------------------------------------------}
begin
  Result:=ReadString (CSectionSimulation, CIdentLinienKeineVerbindung, '');
end;

end.
