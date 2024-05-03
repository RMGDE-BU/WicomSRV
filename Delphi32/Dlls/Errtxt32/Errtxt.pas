{******************************************************************************}
{* Unit: Fehlertexte für ERRTXT32.DLL                                         *}
{* 08.12.2005 WW                                                              *}
{* 14.11.2007 resourcestrings                                                 *}
{******************************************************************************}
unit ErrTxt;

interface

uses     
  Windows, Classes, SysUtils, ErrConst, ErrConstGasX, ErrConstBasic;


function GetStatusText (Status: integer): shortstring;
function GetErrorText (Status, Error: integer): shortstring;
function GetFupErrorcode (FupMeldung: shortstring): integer;
function GetKlasseText (Klasse: integer): shortstring;
function GetErrorKlasse (Status, Error: integer): integer;
function GetDSfGKonvErrorText (Status: integer): shortstring;
function GetGasXStatus (Status, Error: integer; Kommando: char): integer;
function GetErrorcode_basic (Status, Error: integer): integer;

function ExportErrtxt_WicomSrv_GasX(sExportFilename: string): integer;

implementation

resourcestring
  S_UndocumentedKlasse = 'Nicht dokumentierte Fehlerklasse';
  S_UndocumentedStatus = 'Nicht dokumentierte Fehlergruppe';
  S_UndocumentedError  = 'Nicht dokumentierter Fehler';

  S_UndocumentedDSfGKonvStatus = 'Nicht dokumentierter Status';

  { Klasse }
  S_FK_WARNUNG = 'Warnung';
  S_FK_FEHLER  = 'Fehler';

  { Status }
  S_SYS_ABRUFERROR      = 'Abrufsystem-Fehler';
  S_COM_PORTERROR       = 'COM-Zugriffsfehler';

  S_COM_ERROR           = 'Allgemeiner Schnittstellenfehler';
  S_COM_KOMMERROR       = 'Kommunikationsfehler';
  S_COM_DEVICEINITERROR = 'DÜ-Geräte-Initialisierungsfehler';
  S_SYS_LICENCEERROR    = 'Lizenz-Prüfung';
  S_COM_FUPERROR        = 'FUP-Fehlermeldung';
  S_COM_MRGERROR        = 'MRG-Fehlermeldung';
  S_COM_MODEMERROR      = 'Modemfehler';
  S_COM_MODEMPROTERROR  = 'Modem-Protokollfehler';
  S_COM_TCPIP_ERROR     = 'TCP/IP-Fehler';
  S_COM_SRV_ERROR       = 'Fehler bei Kommunikation mit Abrufserver';
  S_COM_PALMUSBERROR    = 'Palm-USB-Kommunikationsfehler';
  S_COM_LAKS            = 'LAKS-Übertragungsfehler';
  S_COM_DSFGDFUERROR    = 'DSfG-DFÜ-Fehlermeldung';
  S_COM_PDAERROR        = 'Fehler bei PDA-Kommunikation';
  S_COM_KE_ERROR        = 'Fehler bei KE-Kommunikation';
  S_COM_FTL_ERROR       = 'Geräte-Fehlermeldung (FTL-Protokoll)';
  S_COM_GPRS_ERROR      = 'GPRS-Fehler';
  S_COM_MODBUSERROR     = 'Modbus-Fehler';
  S_COM_DS100_ERROR     = 'Geräte-Fehlermeldung (DS-100-Protokoll)';
  S_SYS_SIGNSRVSYSERROR = 'Signaturserversystem-Fehler';
  S_COM_SIGNSRV_ERROR   = 'Fehler bei Kommunikation mit Signaturserver';
  S_SYS_SIGNSRV_ERROR   = 'Digitale Signatur';
  S_COM_FTLINTERN_ERROR = 'Geräte-Fehlermeldung (internes FTL-Protokoll)';

  S_ST_STAMMERROR       = 'Stammdaten-Fehler';
  S_ST_RUECKRUFPRUEF    = 'Rückrufprüfung';
  S_ST_KONVERROR        = 'Datenkonvertierung';
  S_ST_DSFGERROR        = 'Fehler/Problem bei Kommunikation mit DSfG-DFÜ';
  S_ST_DSFGUMLERROR     = 'Fehler bei Geräte-Umleitung';
  S_ST_DATACHECK        = 'Datenüberprüfung';
  S_ST_RUFREAKTERROR    = 'Rufreaktivierung';
  S_ST_RUECKRUFERROR    = 'Rückrufauslösung';
  S_ST_DATAERROR        = 'Fehler beim Zugriff auf Archivdaten';
  S_ST_KONFIGERROR      = 'Konfigurationsdaten-Fehler';
  S_ST_FILEERROR        = 'Dateizugriffsfehler';

  S_ACT_USER            = 'Aktion durch Anwender';

  S_EST_ZEITSYNCERROR   = 'Zeit-Synchronisation nicht erfolgt';
  S_EST_RPRESET_MEERROR = 'Problem bei Rundpufferreset für Meldungen';
  S_EST_RPRESET_MWERROR = 'Problem bei Rundpufferreset für Meßwerte';
  S_EST_RPRESET_PRERROR = 'Problem bei Rundpufferreset für Prüfungssätze';
  S_EST_RUFDEAKTERROR   = 'Rufdeaktivierung';
  S_EST_PARAMERROR      = 'Fehler beim Parametrieren';
  S_EST_LOGINERROR      = 'Login';
  S_EST_KENNUNGCHECK    = 'Kennungsüberprüfung';
  S_EST_ZEITBEFEHLERROR = 'Zeitbefehl';
  S_EST_FIRMWAREUPDATEERROR = 'Firmware-Update';
  S_EST_GERAETECHECK    = 'Geräteüberprüfung';
  S_EST_DSFGTRANSPARENTERROR = 'DSfG-Transparentschaltung';
  S_EST_MWLESENERROR    = 'Fehler beim Lesen von Messwerten';
  S_EST_MELESENERROR    = 'Fehler beim Lesen von Meldungen';

  S_EST_3_GBHREVISION   = 'Fehler bei GBH-Revision (AKA II)';

  { Error }

  { interne Abrufsystem-Fehlermeldungen }
  S_SYSABRFERR_KOMMANDONICHTPLAUSIBEL     = 'Server-Kommando ist nicht plausibel';
  S_SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT = 'Server-Kommando ist nicht implementiert';
  S_SYSABRFERR_KOMMANDOSOLLPROZESSID      = 'Falsche Prozess-ID im Server-Kommando';
  S_SYSABRFERR_KOMMANDOUNGUELTIG          = 'Server-Kommando ist für den Gerätetyp ungültig';
  S_SYSABRFERR_ANTWORTNICHTPLAUSIBEL      = 'Server-Antwort ist nicht plausibel';
  S_SYSABRFERR_ANTWORTUNERWARTET_CMD      = 'Unerwartetes Kommandozeichen in Server-Antwort';
  S_SYSABRFERR_ANTWORTUNERWARTET_PID      = 'Unerwartete Prozess-ID in Server-Antwort';
  S_SYSABRFERR_ANTWORTINHALTLAENGE        = 'Längenfehler in der Server-Antwort';
  S_SYSABRFERR_TIMEOUT                    = 'Timeout beim Warten auf Antwort';

  S_SYSABRFERR_MIABRUFDLLNOTFOUND         = 'Datei MIABRUF.DLL ist nicht installiert';
  S_SYSABRFERR_MIABRUFDLLACCESS           = 'Fehler beim Zugriff auf MIABRUF.DLL';

  { COM-Port-Zugriffsfehler }
  S_COMPORTERR_NICHTVORHANDEN    = 'COM-Port ist nicht vorhanden';
  S_COMPORTERR_OEFFNEN           = 'COM-Port kann nicht geöffnet werden';
  S_COMPORTERR_DUE_GERAET_FALSCH = 'Stationsabruf über angeschlossenes DÜ-Gerät nicht möglich';

  { Fehlermeldungen beim Datenaustausch über serielle Schnittstelle }
  S_ce_RxOver   = 'Überlauf der Empfangswarteschlange';
  S_ce_Overrun  = 'Datenüberlauf (Zeichen geht verloren)';
  S_ce_RxParity = 'Paritätsfehler aufgetreten';
  S_ce_Frame    = 'Rahmenfehler aufgetreten';
  S_ce_Break    = 'Unterbrechung aufgetreten';
  S_ce_TxFull   = 'Sendewarteschlange voll';
  S_ce_IOE      = 'Ein-/Ausgabefehler aufgetreten';
  S_ce_Mode     = 'Schnittstellenparameter ungültig';

  { Kommunikationsfehlermeldungen }
  S_KOMMERR_TIMEOUT            = 'Timeout beim Warten auf Daten';
  S_KOMMERR_BCC                = 'BCC-Fehler in den Empfangsdaten';
  S_KOMMERR_CRC                = 'CRC-Fehler in den Empfangsdaten';
  S_KOMMERR_VERB_UNTERBROCHEN  = 'Verbindung wurde unterbrochen';
  S_KOMMERR_FTL_BLOCKSICHERUNG = 'Blocksummen-Fehler in den Empfangsdaten (FTL-Protokoll)';
  S_KOMMERR_LRC                = 'LRC-Fehler in den Empfangsdaten';
  S_KOMMERR_BAUD_NICHT_UNTERSTUETZT = 'Geräte-Baudrate wird nicht unterstützt'; 

  { Geräteinitialisierungs-Fehlermeldungen }
  S_DEVINITERR_FUPRESET  = 'FUP kann nicht zurückgesetzt werden';
  S_DEVINITERR_FUPINIT   = 'FUP kann nicht initialisiert werden';
  S_DEVINITERR_MODEMINIT = 'Modem kann nicht initialisiert werden';
  S_DEVINITERR_PIN_LOCK  = 'PIN-Login für GSM-Modem ist gesperrt';

  { Lizenz-Fehlermeldungen }
  S_LICENCEERR_GERAETETYP      = 'Für den Gerätetyp besteht keine Abrufberechtigung';
  S_LICENCEERR_LAUFZEIT        = 'Programm-Lizenz ist abgelaufen';
  S_LICENCEERR_KEINESTATIONEN  = 'Es bestehen keine Stations-Abrufberechtigungen';
  S_LICENCEERR_ANZAHLSTATIONEN = 'Maximale Anzahl der Stations-Abrufberechtigungen ist überschritten';
  S_LICENCEERR_UNBEKANNT       = 'Unbekannter Lizenzfehler aufgetreten';
  S_LICENCEERR_PROGFUNKTION    = 'Programm-Funktion ist nicht lizenziert';

  { Fehlermeldungen des FUP }
  S_FUPERR_F0 = 'Timeout beim Warten auf MRG-Daten (F0)';
  S_FUPERR_F1 = 'Timeout beim Verbindungsaufbau (F1)';
  S_FUPERR_F2 = 'Keine Verbindung: besetzt, kein Wählton, MRG oder FUP antworten nicht (F2)';
  S_FUPERR_F3 = 'Undefinierte Zeichen vom Modem (F3)';
  S_FUPERR_F4 = 'Fehlerhafte Rufentgegennahme (F4)';
  S_FUPERR_F5 = 'Keine Wahlinformation im FUP (F5)';
  S_FUPERR_F6 = 'Rufzusammenstoß bei der Wahl (F6)';
  S_FUPERR_F7 = 'Timeout beim Senden der Daten an MRG (F7)';
  S_FUPERR_F8 = 'Maximale Wiederholung des Datensatzes erreicht (F8)';
  S_FUPERR_F9 = 'Timeout beim Warten auf PC-Daten (F9)';
  S_FUPERR_FA = 'Timeout beim Verbindungsaufbau (FA)';
  S_FUPERR_FB = 'Protokollfehler (FB)';
  S_FUPERR_FC = 'Timeout beim Warten auf Empfangsdaten (FC)';
  S_FUPERR_FD = 'Timeout beim Senden der Antwort (FD)';
  S_FUPERR_FE = 'Verbindungsabbruch durch PC (FE)';
  S_FUPERR_FF = 'Fehler im Übertragungsprotokoll (FF)';
  S_FUPERR_FG = 'Protokollfehler (FG)';
  S_FUPERR_FH = 'Verbindungsabbruch durch PC (FH)';
  S_FUPERR_FI = 'Verbindungsabbruch durch MRG (FI)';
  S_FUPERR_FM = 'Abbruch der Datenübertragung, kein Senderecht (FM)';
  S_FUPERR_FN = 'Ruf steht nicht an (FN)';
  S_FUPERR_FS = 'Kommunikation mit Gegenstelle nicht möglich (FS)';
  S_FUPERR_FT = 'Interner FUP-Timeout (FT)';
  S_FUPERR_FU = 'Bereit nach FUP-Reset (FU)';
  S_FUPERR_FZ = 'Ruf steht an (FZ)';

  { Fehlermeldungen vom MRG an Host }
  S_MRGERR_KOMMANDOUNPLAUSIBEL     = 'Kommando ist nicht plausibel';
  S_MRGERR_KEINEBERECHTIGUNG       = 'Fehlende Berechtigung';
  S_MRGERR_FALSCHESFORMAT          = 'Falsches Format des Kommandos';
  S_MRGERR_REVISION                = 'Gerät steht auf Revision (Arbeiten vor Ort)';
  S_MRGERR_KOMMANDOUNBEKANNT       = 'Kommando wird vom Gerät nicht unterstützt';
  S_MRGERR_AENDERUNGNICHTZULAESSIG = 'Parameteränderung nicht zulässig (falsches Paßwort o. Eichparameter)';
  S_MRGERR_ANTWORTUNVOLLSTAENDIG   = 'Antwort ist unvollständig';
  S_MRGERR_ANTWORTUNERWARTET       = 'Unerwartete Antwort vom Gerät';
  S_MRGERR_FEHLERTELEGRAMM         = 'Fehlertelegramm-Antwort';
  S_MRGERR_DATASIZE_SOLL_IST       = 'Fehler Soll/Ist-Datengröße in der Antwort';
  S_MRGERR_DATASIZE_EXCEEDED_NACK  = 'Max. mögliche Antwort-Datengröße überschritten (NACK)';
  S_MRGERR_MOMENTANNICHTMOEGLICH   = 'Kommando ist momentan nicht möglich';

  { Fehlermeldungen Modem }
  S_CME_ERROR       = 'Fehler bei Kommandoeingabe';
  S_CME_RING        = 'Ankommender Ruf';
  S_CME_CONNECT     = 'Verbindung steht';
  S_CME_NODIALTONE  = 'Keinen Wählton erhalten';
  S_CME_NOCARRIER   = 'Verbindung nicht zustandegekommen oder unterbrochen';
  S_CME_BUSY        = 'Gerufener Anschluß besetzt';
  S_CME_DIALLOCKED  = 'Wählfunktion gesperrt';
  S_CME_DELAYED     = 'Wahlverzögerung aktiv';
  S_CME_NOANSWER    = 'Gerufener Anschluß hebt nicht ab';
  S_CME_ABORT       = 'Abbruch';
  S_CME_OK          = 'Kommando abgearbeitet';
  S_CME_SONST       = 'Unbestimmter Fehler';
  S_CME_CTS         = 'Modem nicht betriebsbereit (kein CTS-Signal)';
  S_CME_DCD         = 'Verbindung wurde unterbrochen';
  S_CME_BLACKLISTED = 'Rufnummer steht auf der schwarzen Liste';

  S_CME_GSM_PIN_CHECK                   = 'Fehler bei PIN-Abfrage';
  S_CME_GSM_PIN_WRITE                   = 'Fehler bei PIN-Eingabe';
  S_CME_GSM_PIN_WRONG                   = 'PIN ist falsch';
  S_CME_GSM_PIN_LOGINIMPOSSIBLE_PUK     = 'Login ist über PIN nicht möglich (Entsperren mit PUK)';
  S_CME_GSM_PIN_LOGINIMPOSSIBLE_UNKNOWN = 'Login ist über PIN nicht möglich (unbekannter PIN-Status)';
  S_CME_GSM_SMS_FORMAT_WRITE            = 'Fehler beim Setzen des SMS-Datenformats';
  S_CME_GSM_SMS_READ                    = 'Fehler beim Lesen der SMS';
  S_CME_GSM_SMS_DELETE                  = 'Fehler beim Löschen der SMS';
  S_CME_GSM_SMS_SEND                    = 'Fehler beim Senden der SMS';
  S_CME_GSM_CSQ_READ                    = 'Fehler beim Lesen der GSM-Signalqualität';
  S_CME_GSM_COPS_READ                   = 'Fehler beim Lesen des Netzbetreibers';
  S_CME_GSM_REG_READ                    = 'Fehler beim Lesen des Registrierungszustands';

  { Modemprotokoll-Fehlermeldungen }
  S_CMPE_ENQ            = 'Maximale Wiederholung des Datensatzes erreicht (ENQ)';
  S_CMPE_NAK            = 'Maximale Wiederholung des Datensatzes erreicht (NAK)';
  S_CMPE_EOT            = 'EOT empfangen';
  S_CMPE_DLE_EOT        = 'Verbindungsabbruch durch MRG (DLE EOT)';
  S_CMPE_UNGUELTIG      = 'Ungültiges Zeichen empfangen';
  S_CMPE_UNVOLLSTAENDIG = 'Unvollständigen Datensatz empfangen';
  S_CMPE_UNBESTIMMT     = 'Unbestimmter Fehler';

  { TCP/IP-Fehlermeldungen }
  S_TCPIP_ERR_GENERAL             = 'Allgemeiner Fehler aufgetreten';
  S_TCPIP_ERR_SEND                = 'Fehler beim Senden von Daten aufgetreten';
  S_TCPIP_ERR_RECEIVE             = 'Fehler beim Empfangen von Daten aufgetreten';
  S_TCPIP_ERR_CONNECT             = 'Fehler bei der Verbindungsanforderung aufgetreten';
  S_TCPIP_ERR_DISCONNECT          = 'Fehler beim Schließen der Verbindung aufgetreten';
  S_TCPIP_ERR_ACCEPT              = 'Fehler beim Annehmen einer Client-Verbindungsanforderung aufgetreten';
  S_TCPIP_ERR_TIMEOUTCONNECT      = 'Timeout bei der Verbindungsanforderung aufgetreten';
  S_TCPIP_ERR_CONNECTION_INACTIVE = 'Verbindung wurde unterbrochen';
  S_TCPIP_ERR_TIMEOUTDISCONNECT   = 'Timeout beim Schließen der Verbindung aufgetreten';
  S_TCPIP_ERR_LOOKUP              = 'Hostname konnte nicht gefunden werden';
  S_TCPIP_ERR_UNDEFINIERT         = 'Undefinierter Fehler aufgetreten';

  { Kommunikationsfehlermeldungen Abrufserver, Signaturserver }
  S_SRV_ERR_GENERAL             = 'Allgemeiner Fehler aufgetreten';
  S_SRV_ERR_SEND                = 'Fehler beim Senden von Daten aufgetreten';
  S_SRV_ERR_RECEIVE             = 'Fehler beim Empfangen von Daten aufgetreten';
  S_SRV_ERR_CONNECT             = 'Fehler bei der Verbindungsanforderung aufgetreten';
  S_SRV_ERR_DISCONNECT          = 'Fehler beim Schließen der Verbindung aufgetreten';
  S_SRV_ERR_ACCEPT              = 'Fehler beim Annehmen einer Client-Verbindungsanforderung aufgetreten';
  S_SRV_ERR_TIMEOUTCONNECT      = 'Timeout bei der Verbindungsanforderung aufgetreten';
  S_SRV_ERR_CONNECTION_INACTIVE = 'Verbindung wurde unterbrochen';
  S_SRV_ERR_TIMEOUTDISCONNECT   = 'Timeout beim Schließen der Verbindung aufgetreten';
  S_SRV_ERR_LOOKUP              = 'Hostname konnte nicht gefunden werden';
  S_SRV_ERR_UNDEFINIERT         = 'Undefinierter Fehler aufgetreten';
  S_SRV_ERR_TIMEOUTRECEIVE      = 'Timeout beim Warten auf Daten';

  { Fehlermeldungen LAKS }
  S_CLAKS_PARITYBCC   = 'Paritäts- oder BCC-Fehler';
  S_CLAKS_ERROR       = 'Befehl nicht plausibel';
  S_CLAKS_WRONGNUMBER = 'Nummer im Befehl falsch';
  S_CLAKS_FORBIDDEN   = 'Parameteränderung nicht zulässig';

  { Fehlermeldungen DSfG-DFÜ }
  S_DSFGDFUERR_ANTWORTUNVOLLSTAENDIG         = 'Antwort ist unvollständig';
  S_DSFGDFUERR_UNBEKANNT                     = 'Befehl wird vom Gerät nicht unterstützt';
  S_DSFGDFUERR_FALSCHESYNTAX                 = 'Befehl ist syntaktisch falsch';
  S_DSFGDFUERR_WIESER_UNBEKANNT              = 'Befehl wird vom Gerät nicht unterstützt (evtl. Format inkompatibel)';
  S_DSFGDFUERR_WIESER_FALSCHENUMMER          = 'Falsche Parameternummer im Befehl';
  S_DSFGDFUERR_WIESER_AENDEREUNGNICHTERLAUBT = 'Parameteränderung ist nicht möglich';
  S_DSFGDFUERR_WIESER_VERBOTEN               = 'Befehl ist verboten (Zugang nicht erlaubt)';

  { Fehlermeldungen beim Zugriff auf Stammdaten }
  S_SMERR_STAMMSATZLESEN    = 'Fehler beim Lesen des Geräte-Stammsatzes';
  S_SMERR_RUFSTAMMDATEN     = 'DFÜ-Stammdaten nicht vorhanden';
  S_SMERR_STAMMSATZNOTFOUND = 'Stammsatz nicht vorhanden';
  S_SMERR_GERAETETYPFALSCH  = 'Gerätetyp falsch';
  S_SMERR_DSFGINST_NOTFOUND = 'DSfG-Instanz-Stammsatz nicht vorhanden';
  S_SMERR_DSFGAK_NOTFOUND   = 'DSfG-Archivkanal-Stammsatz nicht vorhanden';
  S_SMERR_DSFGLB_NOTFOUND   = 'DSfG-Logbuch-Stammsatz nicht vorhanden';
  S_SMERR_MRGLGZARCHORIG    = 'Langzeitdaten-Archivierung nur als Originalwerte möglich';
  S_SMERR_DSFGINSTTYP_WRONG = 'DSfG-Instanztyp falsch';
  S_SMERR_KENNUNG_MEHRFACH_KEIN_GPRS_ABRUF = 'Kennung mehrfach in Stammdaten vorhanden, kein GPRS-Abruf';
  S_SMERR_STATIONZAEHLPKT_NICHTEINDEUTIG = 'Zählpunkt für Station in Stammdaten nicht eindeutig';
  S_SMERR_KANALNOTFOUND  = 'Kanal in Stammsatz nicht vorhanden';
  S_SMERR_KANALOBISKENNZ_NICHTEINDEUTIG = 'OBIS-Kennziffer in Kanal-Stammdaten nicht eindeutig';
  S_SMERR_EINGABEUNGUELTIG = 'Eingabedaten ungültig';

  { Fehlermeldungen beim Zugriff auf Konfigurationsdaten }
  S_KFERR_KONFIGDATANOTFOUND = 'Konfigurationsdaten für den Gerätetyp nicht gefunden';
  S_KFERR_PARAMETERNOTFOUND  = 'Parameter nicht gefunden';
  S_KFERR_KONFIGDATAINVALID  = 'Konfigurationsdaten ungültig';

  { Konvertierungsfehlermeldungen }
  S_SKERR_MELDKONV                      = 'Fehler beim Konvertieren der Meldungen';
  S_SKERR_MESSKONV                      = 'Fehler beim Konvertieren der Meßwerte';
  S_SKERR_EBANDKONV                     = 'Fehler beim Konvertieren der Energiebandänderungen';
  S_SKERR_PARAMKONV                     = 'Fehler beim Konvertieren der Parameter';
  S_SKERR_LASTZSTNDKONV                 = 'Fehler beim Konvertieren der Lastzustände';
  S_SKERR_KARCHIVKONV                   = 'Fehler beim Konvertieren des Kurzzeitarchivs';
  S_SKERR_ARCHIVKONV                    = 'Fehler beim Konvertieren der Archivdaten';
  S_SKERR_LOGBKONV                      = 'Fehler beim Konvertieren der Logbuchdaten';
  S_SKERR_INSTWERTKONV                  = 'Fehler beim Konvertieren der Datenelemente';
  S_SKERR_AUFMTELEGRAMMKONV             = 'Fehlerhaftes DSfG-Aufmerksamkeitstelegramm';
  S_SKERR_KONVGERTYPNOTAVAILABLE        = 'Konvertierung für den Gerätetyp nicht verfügbar';
  S_SKERR_AUSSERPLANMAESSIGEANTW_AR     = 'Außerplanmäßige DSfG-Antwort (Archivdaten)';
  S_SKERR_AUSSERPLANMAESSIGEANTW_LB     = 'Außerplanmäßige DSfG-Antwort (Logbuchdaten)';
  S_SKERR_AUSSERPLANMAESSIGEANTW_DE     = 'Außerplanmäßige DSfG-Antwort (Datenelemente)';
  S_SKERR_DSFGDFUEDATAKONV              = 'Fehler beim Konvertieren der DSfG-DFÜ-Daten';
  S_SKERR_AUSSERPLANMAESSIGEANTW_KONFIG = 'Außerplanmäßige DSfG-Antwort (Teilnehmer antwortet nicht)';

  { Fehlermeldungen bei Kommunikation mit DSfG-DFÜ-Instanz }
  S_DSFGERR_ENQ                 = 'Formaler Fehler im Sendetelegramm (ENQ)';
  S_DSFGERR_CAN                 = 'Lokaler DSfG-Teilnehmer ist nicht vorhanden (CAN)';
  S_DSFGERR_NAK                 = 'Telegramm wurde nicht übertragen innerhalb TS (NAK)';
  S_DSFGERR_BCC                 = 'BCC-Fehler im Empfangstelegramm';
  S_DSFGERR_QUITTUNGUNDEFINIERT = 'Undefiniertes Quittungszeichen';

  { Fehlermeldungen für DSfG-Umleitung }
  S_UMLERR_UMSCHALTUNG                  = 'Fehler beim Umschalten auf Gerät';
  S_UMLERR_INVKENNUNG_KEINE_UMSCHALTUNG = 'Kennung des Geräts falsch, Umschaltung ist nicht erfolgt';
  S_UMLERR_RUFLISTE                     = 'Fehler beim Abfragen der Rufanregungsliste';
  S_UMLERR_RUFQUITTIEREN                = 'Fehler beim Quittieren des Rufs';
  S_UMLERR_INVKENNUNG_UMSCHALTUNG       = 'Kennung des Geräts falsch, Umschaltung ist erfolgt';

  { Fehlermeldungen für Zeitsynchronisation }
  S_ZSYNCERR_SUCCESS         = 'Zeitsynchronisation ist erfolgt';
  S_ZSYNCERR_DIFFERENTHOURS  = 'Abweichende Stunden in Gerät und PC';
  S_ZSYNCERR_UNDEFINIERT     = 'Undefinierter Fehler aufgetreten';
  S_ZSYNCERR_PERIODEND       = 'Meßperiodenende steht unmittelbar bevor';
  S_ZSYNCERR_HIGHERMAX       = 'Zeitabweichung zu groß';
  S_ZSYNCERR_WRONGPASSWORDNR = 'Falsche Paßwort-Nummer (ungleich 1)';
  S_ZSYNCERR_NOSUCCESS       = 'Zeit konnte im Gerät nicht verändert werden';
  S_ZSYNCERR_SZTOWZ          = 'Zeitpunkt der Umstellung auf Winterzeit';
  S_ZSYNCERR_CORRMAX         = 'Zeit wurde im Gerät korrigiert';
  S_ZSYNCERR_READDEVICETIME    = 'Geräte-Zeit konnte nicht gelesen werden';
  S_ZSYNCERR_INVALIDDEVICETIME = 'Ungültige Geräte-Zeit';

  S_ZSYNCERR_VERZOEGERT           = 'Zeitsynchronisation wird verzögert durchgeführt';
  S_ZSYNCERR_NICHTDURCHGEFUEHRT   = 'Zeitsynchronisation wurde nicht durchgeführt';
  S_ZSYNCERR_SYNTAXFEHLER         = 'Zeitsynchronisation wurde abgelehnt (Syntaxfehler)';
  S_ZSYNCERR_GERAETBUSY           = 'Zeitsynchronisation wurde abgelehnt (Gerät anderweitig beschäftigt)';
  S_ZSYNCERR_BEREITSSYNCHRON      = 'Zeitsynchronisation nicht nötig (Gerät läuft bereits synchron)';
  S_ZSYNCERR_SICHERUNGFALSCH      = 'Zeitsynchronisation wurde abgelehnt (Sicherungszeichen falsch)';
  S_ZSYNCERR_VERSTELLWEITEGEKAPPT = 'Zeitsynchronisation wurde durchgeführt (Verstellweite innerhalb der MP gekappt)';
  S_ZSYNCERR_WIEDERHOLSPERRE      = 'Zeitsynchronisation wurde abgelehnt (Wiederholsperre bei Änderung < 1%)';
  S_ZSYNCERR_SONSTIG              = 'Zeitsynchronisation wurde abgelehnt (keine nähere Erläuterung)';

  { Fehlermeldungen für Rundpuffer-Rücksetzen }
  S_RPRESETERR_WRONGPASSWORDNR = 'Falsche Paßwort-Nummer (ungleich 1)';
  S_RPRESETERR_NOSUCCESS       = 'Rundpuffer konnte nicht zurückgesetzt werden';

  { Fehlermeldungen für Datenüberprüfung }
  S_DCH_MWMISSING  = 'Fehlende Meßwerte im Abruf';
  S_DCH_MEINVALID  = 'Ungültige Rohdaten (Meldungen)';
  S_DCH_ARINVALID  = 'Ungültige Rohdaten (Archive)';
  S_DCH_LBINVALID  = 'Ungültige Rohdaten (Logbücher)';
  S_DCH_DEINVALID  = 'Ungültige Rohdaten (Datenelemente)';
  S_DCH_ORDNRFOLGE = 'Ordnungsnummern nicht fortlaufend';
  S_DCH_MWINVALID  = 'Ungültige Rohdaten (Meßwerte)';
  S_DCH_INVALID    = 'Ungültige Rohdaten';
  S_DCH_KZWINVALID = 'Ungültige Rohdaten (Kurzzeitwerte)';
  S_DCH_ORDNRFUELLSTANDBIS = 'Ungültige Ordnungsnummer (Füllstand bis)';  

  { Fehlermeldungen für Rufdeaktivierung }
  S_RUFDEAKTERR_WRONGPASSWORDNR = 'Falsche Paßwort-Nummer (ungleich 1)';
  S_RUFDEAKTERR_NOSUCCESS       = 'Ruffunktion konnte im Gerät nicht deaktiviert werden';
  S_RUFDEAKTERR_SUCCESS         = 'Ruffunktion wurde im Gerät deaktiviert';

  { Fehlermeldungen für Rufreaktivierung }
  S_RUFREAKTERR_WRONGPASSWORDNR = 'Falsche Paßwort-Nummer (ungleich 1)';
  S_RUFREAKTERR_NOSUCCESS       = 'Ruffunktion konnte im Gerät nicht reaktiviert werden';
  S_RUFREAKTERR_FINDRUFNR       = 'Zu übertragende Rufnummer konnte nicht ermittelt werden';

  { Fehlermeldungen für Parametrierung }
  S_PARAMERR_NOSUCCESS                  = 'Parameter wurde nicht verändert';
  S_PARAMERR_DSFG_WRONGANSWER           = 'Fehler in der Einstelltelegramm-Antwort';
  S_PARAMERR_DSFG_WRONGCODENR_UNKNOWNDE = 'Zugangscode falsch, ungültiges Format des Wertes oder unbekanntes Datenelement';  // Text geändert; 11.11.2020, WW
  S_PARAMERR_DSFG_NOCHANGEABLEDE_NOES   = 'Zugangscode falsch, Eichschalter geschlossen, ungültiges Format des Wertes oder DE nicht änderbar';
  S_PARAMERR_FORMATWERT                 = 'Ungültiges Format des Wertes';  // Text geändert; 30.04.2019, WW
  S_PARAMERR_NOTAVAILABLE               = 'Parameter nicht verfügbar';

  { Fehlermeldungen für Rückrufauslösung }
  S_RUECKRUFERR_AUSLOESUNG = 'Rückruf konnte nicht ausgelöst werden';

  { Fehlermeldungen für Rückrufprüfung }
  S_RUECKRUFPRUEF_AUSLOESUNGERR     = 'Fehler bei Rufauslösung aufgetreten';
  S_RUECKRUFPRUEF_RUFERR            = 'Fehler bei Rufannahme aufgetreten';
  S_RUECKRUFPRUEF_KEINRUF           = 'Ruf ist nicht erfolgt';
  S_RUECKRUFPRUEF_JOURNALERR        = 'Fehler bei Zugriff auf Journal';
  S_RUECKRUFPRUEF_AUSLOESUNGABBRUCH = 'Rufauslösung wurde abgebrochen';

  { Fehlermeldungen für Login }
  S_LOGINERR_WRONGPW          = 'Ungültiges Paßwort';
  S_LOGINERR_NODFUEBUSADDRESS = 'Busadresse in DFÜ-Instanz nicht eingestellt, Verbindung beendet';
  S_LOGINERR_NOACTIVEUSER     = 'Keine aktiven Teilnehmer am DSfG-Bus, Verbindung beendet';
  S_LOGINERR_TIMEOUT_WRONGPW  = 'Timeout beim Warten auf Daten (evtl. ungültiges Paßwort)';
  S_LOGINERR_MEHRERELEITSTATIONEN = 'Mehrere DFÜ-Leitstationen sind aktiv';
  S_LOGINERR_LIEFERANTENSCHLOSS_GEOEFFNET = 'Gültigkeit des Paßworts nicht überprüfbar, Administrator-/Lieferantenschloß ist geöffnet';
  S_LOGINERR_KUNDENSCHLOSS_GEOEFFNET = 'Gültigkeit des Paßworts nicht überprüfbar, Kundenschloß ist geöffnet';
  S_LOGINERR_DATENAUSLESERSCHLOSS_GEOEFFNET = 'Gültigkeit des Paßworts nicht überprüfbar, Datenausleserschloß ist geöffnet';

  { Fehlermeldungen bei Kennungsüberprüfung }
  S_KENNERR_VERBINDUNG       = 'Kennung falsch, Verbindung bleibt bestehen';
  S_KENNERR_KEINE_VERBINDUNG = 'Kennung falsch, Verbindung beendet';
  S_KENNERR_WRONGANSWER      = 'Falsche Antwort auf Kennungsabfrage';
  S_KENNERR_KEINSTAMMSATZ    = 'Kein Stammsatz für Kennung vorhanden';
  S_KENNERR_ALREADYEXISTS    = 'Kennung in den Stammdaten bereits vorhanden';
  S_KENNERR_MEHRFACH_NODATA  = 'Kennung mehrfach in den Stammdaten vorhanden, keine Abfrage von Archivdaten';
  S_KENNERR_RE_PWACTIVATED   = 'Kennungsabfrage nicht möglich (Passwort-Aktivierung im Gerät)';

  { Fehlermeldungen für Archivdatenzugriff }
  S_DERR_DELETE = 'Daten konnten nicht gelöscht werden';

  { Fehlermeldungen für allgemeine Dateizugriffe }
  S_FILEERR_COULDNOTOPEN  = 'Datei konnte nicht geöffnet werden';
  S_FILEERR_COULDNOTWRITE = 'Fehler beim Schreiben in Datei aufgetreten';
  S_FILEERR_FTL_SR_ALARM_COULDNOTCREATEDIR  = 'Verzeichnis für FTL-Alarmdatei konnte nicht angelegt werden';
  S_FILEERR_FTL_SR_ALARM_COULDNOTCREATEFILE = 'FTL-Alarmdatei konnte nicht angelegt werden';
  S_FILEERR_FTL_SR_ALARM_COULDNOTOPENFILE   = 'FTL-Alarmdatei konnte nicht geöffnet werden';
  S_FILEERR_RUECKRUFPRUEF_COULDNOTCREATEDIR  = 'Verzeichnis für Rückrufprüfungsdatei konnte nicht angelegt werden';
  S_FILEERR_RUECKRUFPRUEF_COULDNOTCREATEFILE = 'Rückrufprüfungsdatei konnte nicht angelegt werden';
  S_FILEERR_RUECKRUFPRUEF_COULDNOTOPENFILE   = 'Rückrufprüfungsdatei konnte nicht geöffnet werden';
  S_FILEERR_IMPORT_LOADFILE  = 'Import-Rohdatendatei konnte nicht geladen werden';
  S_FILEERR_FIRMWUPD_PARA_COULDNOTWRITE = 'Fehler beim Schreiben der Parameter-Sicherungsdatei';
  S_FILEERR_FIRMWUPD_PARA_COULDNOTREAD  = 'Fehler beim Lesen der Parameter-Sicherungsdatei';
  S_FILEERR_FIRMWUPD_PARA_DOESNOTEXIST  = 'Parameter-Sicherungsdatei nicht gefunden';

  { Fehlermeldungen für PDA-Kommunikation }
  S_PDAERR_ANTW_UNDEFINIERT = 'Undefinierte Antwort';
  S_PDAERR_CRC              = 'CRC-Fehler';
  S_PDAERR_CTS              = 'PDA nicht betriebsbereit (kein CTS-Signal)';

  { Fehlermeldungen für KE-Kommunikation }
  S_KEERR_HEADER                 = 'Fehler im Datentelegrammkopf';
  S_KEERR_BEFEHLSYNTAX           = 'Befehl ist syntaktisch falsch';
  S_KEERR_BEFEHL_NICHT_ERLAUBT   = 'Befehl ist nicht erlaubt';
  S_KEERR_ANTWORT_UNVOLLSTAENDIG = 'Antwort ist unvollständig';

  { Fehlermeldungen für Palm-USB-Kommunikation }
  S_PALMUSBERR_Unknown           = 'Unbekannter Fehler aufgetreten';
  S_PALMUSBERR_SendTimedOut      = 'Timeout beim Senden von Daten';
  S_PALMUSBERR_RecvTimedOut      = 'Timeout beim Empfangen von Daten';
  S_PALMUSBERR_PortNotOpen       = 'USB-Port ist nicht geöffnet';
  S_PALMUSBERR_IOError           = 'Ein-/Ausgabe- oder Leitungsfehler aufgetreten';
  S_PALMUSBERR_PortBusy          = 'USB-Port ist bereits in Benutzung';
  S_PALMUSBERR_NotSupported      = 'IOCTL-Code wird vom USB-Treiber nicht unterstützt';
  S_PALMUSBERR_BufferTooSmall    = 'Puffergröße ist zu klein';
  S_PALMUSBERR_NoAttachedDevices = 'Kein Palm-Handheld angeschlossen';
  S_PALMUSBERR_DontMatchFilter   = 'Palm-Gegenstellenprogramm ist nicht aktiv';

  S_PALMUSBERR_DLLAccess        = 'Fehler beim Zugriff auf USBPORT.DLL';
  S_PALMUSBERR_CouldNotOpenPort = 'USB-Port konnte nicht geöffnet werden';
  S_PALMUSBERR_SendIncomplete   = 'Daten unvollständig versendet';
  S_PALMUSBERR_ReceiveBuffer    = 'Interner Empfangspuffer-Fehler';

  { Fehlermeldungen für Status = ACT_USER }
  S_USERACT_ABBRUCH_ABRUF = 'Abbruch des Datenabrufs';

  { Fehlermeldungen von Tritschler-Geräten mit FTL-Protokoll }
  S_FTLERR_EMPFANGSPUFFERUEBERLAUF               = 'Empfangspufferüberlauf';
  S_FTLERR_NICHTEMPFANGSBEREIT_TIMEOUT           = 'Nicht empfangsbereit oder Timeout-Zeit abgelaufen';
  S_FTLERR_STARTSTOPBIT                          = 'Start- oder Stopbit-Fehler';
  S_FTLERR_BLOCKSICHERUNG                        = 'Blocksumme falsch';
  S_FTLERR_KOMMANDONICHTAUSFUEHRBAR_FALSCHEFKTNR = 'Kommando nicht ausführbar oder falsche Funktion-Nr.';
  S_FTLERR_KEINEDATENVORHANDEN                   = 'Keine weiteren Daten vorhanden';
  S_FTLERR_UNBEKZEICHEN_KOMMANDO                 = 'Unbekanntes Zeichen oder Kommando';
  S_FTLERR_PARAMETERUNZULAESSIG                  = 'Unzulässiger Parameter';
  S_FTLERR_QUITTUNGUNDEFINIERT                   = 'Undefinierte Quittung';
  S_FTLERR_ANTWORTUNVOLLSTAENDIG                 = 'Antwort ist unvollständig';
  S_FTLERR_ANTWORTUNERWARTET                     = 'Unerwartete Antwort vom Gerät';

  { Fehlermeldungen von Tritschler-Geräten (internes FTL-Protokoll) }
  S_FTLINTERNERR_BLOCKZUKURZ                     = 'Block zu kurz';
  S_FTLINTERNERR_BLOCKNRKEINEZAHL                = 'Blocknummer ist keine Zahl';
  S_FTLINTERNERR_BLOCKFOLGEMARKEUNGUELTIG        = 'Blockfolgemarke ist ungültig';
  S_FTLINTERNERR_CRCKEINEZAHL                    = 'CRC ist keine Zahl';
  S_FTLINTERNERR_CRCFALSCH                       = 'CRC ist falsch';
  S_FTLINTERNERR_TESTERGEBNISSCHLECHT            = 'Test-Ergebnis ''schlecht''';
  S_FTLINTERNERR_PARAMETERGESCHRIEBEN            = 'Parameter schreiben erfolgreich (OK)';
  S_FTLINTERNERR_BLOCKNRUNBEKANNT                = 'Blocknummer ist unbekannt';
  S_FTLINTERNERR_PARAMETERNICHTFLOAT             = 'Parameter ist nicht Float';
  S_FTLINTERNERR_PARAMETERNICHTINTEGER           = 'Parameter ist nicht Integer';
  S_FTLINTERNERR_PARAMETERNICHTLONGINTEGER       = 'Parameter ist nicht Long Integer';
  S_FTLINTERNERR_PARAMETERNICHTCHAR              = 'Parameter ist nicht Char/Zeiger';
  S_FTLINTERNERR_PARAMETERAUSSERHALBGRENZWERTE   = 'Parameter hält Grenzwerte nicht ein';
  S_FTLINTERNERR_BLOCKGESPERRT                   = 'Block gesperrt (falsches Paßwort o. Eichparameter)';
  S_FTLINTERNERR_BLOCKKANNNICHTGESCHRIEBENWERDEN = 'Block kann nicht geschrieben werden';
  S_FTLINTERNERR_FALSCHEBLOCKLAENGE              = 'Falsche Blocklänge';
  S_FTLINTERNERR_BLOCKNRUNDEFINIERT              = 'Undefinierte Blocknummer';

  { Fehlermeldungen für GBH-Revision }
  S_GBHREVERR_KEINE_REV_INSTANZ = 'Keine Revisions-Instanz definiert';
  S_GBHREVERR_PARAM_SOLL_IST    = 'Parameterwert entspricht nicht der Soll-Vorgabe';

  { Fehlermeldungen für GPRS-Kommunikation }
  S_GPRSERR_KENNUNG_MEHRFACH_KEIN_ABRUF = 'Mehrere aktive GPRS-Verbindungen für Kennung vorhanden, kein Abruf';
  S_GPRSERR_KEINE_VERBINDUNG            = 'Es besteht keine Verbindung';

  { Fehlermeldungen für Modbus-Kommunikation }
  S_MODBUSERR_EXC_ILLEGALFUNCTION     = 'Unerlaubter Funktionscode in der Anfrage (01)';
  S_MODBUSERR_EXC_ILLEGALDATAADDRESS  = 'Unerlaubte Datenadresse in der Anfrage (02)';
  S_MODBUSERR_EXC_ILLEGALDATAVALUE    = 'Unerlaubter Datenwert in der Anfrage (03)';
  S_MODBUSERR_EXC_SLAVEDEVICEFAILURE  = 'Fehler bei der Kommandoausführung im Slave aufgetreten (04)';
  S_MODBUSERR_EXC_ACKNOWLEDGE         = 'Positive Bestätigung, Kommando wird ausgeführt (05)';
  S_MODBUSERR_EXC_SLAVEDEVICEBUSY     = 'Slave ist ausgelastet, Kommandoausführung momentan nicht möglich (06)';
  S_MODBUSERR_EXC_NEGATIVEACKNOWLEDGE = 'Slave kann Programmfunktion nicht ausführen (07)';
  S_MODBUSERR_EXC_MEMORYPARITYERROR   = 'Paritätsfehler im erweiterten Speicher des Slave aufgetreten (08)';
  S_MODBUSERR_EXC_UNDEFINED           = 'Undefinierter Exception-Code';

  S_MODBUSERR_RESP_INCOMPLETE         = 'Antwort ist unvollständig';
  S_MODBUSERR_RESP_WRONGFUNCTION      = 'Unerwarteter Funktionscode in der Antwort';
  S_MODBUSERR_RESP_WRONGSLAVEADDRESS  = 'Unerwartete Slave-Adresse in der Antwort';
  S_MODBUSERR_RESP_BYTECOUNT_SOLL_IST = 'Fehler Soll/Ist-Byteanzahl in der Antwort';
  S_MODBUSERR_RESP_UNKNOWNFUNCTION    = 'Unbekannter Funktionscode in der Antwort';
  S_MODBUSERR_RESP_WRONGREGISTEREADDRESS = 'Unerwarteter Register/Coil-Adressbereich in der Antwort auf Einstell-Befehl';

  S_MODBUSERR_EXC_GATEWAYPATHUNAVAILABLE = 'Fehlermeldung des Gateways: Verbindung konnte nicht hergestellt werden (0A)';
  S_MODBUSERR_EXC_GATEWAYTARGETDEVICEFAILEDTORESPOND = 'Fehlermeldung des Gateways: Keine Antwort vom adressierten Gerät (0B)';

  S_MODBUSERR_QUERY_CREATE            = 'Anfrage konnte nicht erstellt werden (z.B. ungültiger Wert)';  // 23.07.2019, WW

  S_MODBUSERR_RMG_INDEX               = 'Fehler in Index (RMG)';
  S_MODBUSERR_RMG_EXCEPTION           = 'Exception (RMG)';

  S_MODBUSERR_RESPTCP_WRONGTID        = 'Unerwartete Transaktions-ID in der Modbus TCP/IP-Antwort';
  S_MODBUSERR_RESPTCP_WRONGPROTID     = 'Unerwartete Protokoll-ID in der Modbus TCP/IP-Antwort';
  S_MODBUSERR_RESPTCP_LENGTH_SOLL_IST = 'Fehler Soll/Ist-Länge der Modbus TCP/IP-Antwort';

  { Fehlermeldungen für Elster DS-100-Kommunikation }
  S_DS100ERR_QUITTUNGNEGATIV    = 'Negative Quittung';
  S_DS100ERR_QUITTUNGUNERWARTET = 'Unerwartete Quittung';

  { Signaturserver-Systemfehlermeldungen }
  S_SIGNSRVSYSERR_ANTWORTNICHTPLAUSIBEL = 'Server-Antwort ist nicht plausibel';
  S_SIGNSRVSYSERR_ANTWORTUNERWARTET_CMD = 'Unerwartetes Kommandozeichen in Server-Antwort';
  S_SIGNSRVSYSERR_ANTWORTUNERWARTET_TID = 'Unerwartete Transaktions-ID in Server-Antwort';

  { Fehlermeldungen zu digitaler Signatur }
  { Signaturserver-Fehlermeldungen }
  S_SIGNSRVERR_SIGNINVALID                =	'Signatur ist ungültig';
  S_SIGNSRVERR_UNKNOWNSIGNALGORITHM       =	'Unbekanntes Signaturverfahren';
  S_SIGNSRVERR_HASHGENERATION             =	'Fehler bei Hashwertbildung';
  S_SIGNSRVERR_SIGN                       = 'Interner Fehler beim Signieren';
  S_SIGNSRVERR_KEYGEN                     = 'Interner Fehler beim Schlüssel erzeugen';
  S_SIGNSRVERR_UNKNOWNKEY                 = 'Schlüssel ist unbekannt';
  S_SIGNSRVERR_INVALIDHEXFORMAT           = 'Ungültiges Hex-Format in Daten oder öffentlichem Schlüssel';
  S_SIGNSRVERR_VERIFY                     = 'Interner Fehler beim Verifizieren';
  S_SIGNSRVERR_KEYADMINFILEEMPTY          = 'Schlüsselverwaltungsdatei ist leer';
  S_SIGNSRVERR_LOADKEYADMINFILE           = 'Fehler beim Laden der Schlüsselverwaltungsdatei';
  S_SIGNSRVERR_KEYADMINFILENOTFOUND       = 'Schlüsselverwaltungsdatei nicht gefunden';
  S_SIGNSRVERR_KEYADMINFILEWRITEPROTECTED = 'Schlüsselverwaltungsdatei ist schreibgeschützt';
  S_SIGNSRVERR_KEYSAVE                    = 'Fehler beim Schlüssel speichern';
  { Allgemein }
  S_SIGNSRVERR_NOTVERIFIED                = 'Signatur ist nicht verifiziert';
  S_SIGNSRVERR_LICENCE                    = 'Signatur ist nicht lizenziert';

  { Zeitbefehl-Fehlermeldungen }
  S_ZEITBEFERR_ZEITLESEN = 'Fehler beim Ermitteln der Zeitinformation';

  { Firmwareupdate-Fehlermeldungen }
  S_FIRMWUPDERR_ADRSTARTGLEICHENDE    = 'Firmwareupdate-Befehl ist unplausibel (Start- gleich Endadresse)';
  S_FIRMWUPDERR_ADRSTARTZUKLEIN       = 'Firmwareupdate-Befehl ist unplausibel (Startadresse zu klein)';
  S_FIRMWUPDERR_ADRSTARTGROESSERENDE  = 'Firmwareupdate-Befehl ist unplausibel (Start- größer Endadresse)';
  S_FIRMWUPDERR_ANTWORTUNERWART       = 'Unerwartete Antwort vom Gerät';
  S_FIRMWUPDERR_ANTWORTUNVOLLSTAENDIG = 'Antwort ist unvollständig';
  S_FIRMWUPDERR_UNDEFINIERT           = 'Undefinierter Fehler aufgetreten';

  S_FIRMWUPDERR_BINDATA_NICHTGEFUNDEN     = 'Firmware-Updatedatei nicht gefunden';
  S_FIRMWUPDERR_BINDATA_ZUGROSS           = 'Firmware-Updatedatei ist zu groß';        
  S_FIRMWUPDERR_BINDATA_STARTADRUNGUELTIG = 'Ungültige Startadresse in Firmware-Updatedatei';
  S_FIRMWUPDERR_BINDATA_ENDADRUNGUELTIG   = 'Ungültige Endadresse in Firmware-Updatedatei';
  S_FIRMWUPDERR_BINDATA_TYP_FEHLT         = 'Typ-Information fehlt in Firmware-Updatedatei';
  S_FIRMWUPDERR_BINDATA_TYP_FALSCH        = 'Firmware-Updatedatei paßt nicht zum Gerät (falsche Typ-Information)';
  S_FIRMWUPDERR_BINDATA_BUILD_FEHLT       = 'Erstellungs-Information fehlt in Firmware-Updatedatei';
  S_FIRMWUPDERR_BINDATA_BUILD_UNGUELTIG   = 'Ungültige Erstellungs-Information in Firmware-Updatedatei';
  S_FIRMWUPDERR_BINDATA_BUILD_ALT         = 'Firmware-Updatedatei enthält ältere Version als Gerät (kein Update)';

  S_FIRMWUPDERR_BINDATA_TRANSFER_NAK   = 'Prüfzeichen-Fehler beim Übertragen der Firmware-Binärdaten (NAK)';
  S_FIRMWUPDERR_BINDATA_TRANSFER_CAN   = 'Ungültige Adresse beim Übertragen der Firmware-Binärdaten (CAN)';
  S_FIRMWUPDERR_BINDATA_TRANSFER_ENQ   = 'Fehlender Adresssatz beim Übertragen der Firmware-Binärdaten (ENQ)';
  S_FIRMWUPDERR_BINDATA_TRANSFER_UNDEF = 'Undefinierter Fehler beim Übertragen der Firmware-Binärdaten';
  S_FIRMWUPDERR_BINDATA_TRANSFER_X     = 'Fehler beim Übertragen der Firmware-Binärdaten (X)';

  S_FIRMWUPDERR_FLASH_ERASE = 'Fehler beim Löschen eines Flash-Blocks im Gerät';
  S_FIRMWUPDERR_FLASH_PROG  = 'Fehler beim Programmieren eines Flash-Blocks im Gerät';
  S_FIRMWUPDERR_FLASH_UNDEF = 'Undefinierter Fehler beim Flash-Vorgang im Gerät';

  { Fehlermeldungen bei Geräteüberprüfung }
  S_GERCHECKERR_TYP_FALSCH = 'Gerätetyp falsch';
  S_GERCHECKERR_ARCH_ZEILENWEISE_LESBAR = 'Funktion ''Komplettes Archiv'' wird vom Gerät nicht unterstützt';

  { Fehlermeldungen beim DSfG-Transparentschalten }
  S_DSFGTRANSPERR_DFUEBUSADDRESS_KEINLOKALERTEILNEHMER = 'Busadresse der DFÜ-Instanz ist kein Teilnehmer am lokalen Bus, Verbindung beendet';

  { Fehlermeldungen beim Lesen von Messwerten }
  S_MWLESENERR_ZUGRIFFSRECHT = 'Keine Zugriffsberechtigung';

  { Fehlermeldungen beim Lesen von Meldungen }
  S_MELESENERR_ZUGRIFFSRECHT = 'Keine Zugriffsberechtigung';


  { Fehlertexte für DSfG-Journal-Detailtabelle }

  { Fehler/Warnungen beim Konvertieren von DSfG-Daten }
  S_DSFGKONVERR_OK              = 'Datenabruf OK';
  S_DSFGKONVERR_HEADER          = 'Fehler im DSfG-Telegrammheader';
  S_DSFGKONVERR_DATA            = 'Unerwartetes DSfG-Telegramm';
  S_DSFGKONVERR_FILEACCESS      = 'Fehler beim Zugriff auf Rohdatendatei';
  S_DSFGKONVERR_INDEX           = 'Fehler beim Zugriff auf Index-Tabelle';
  S_DSFGKONVERR_NODATA          = 'Keine Daten verfügbar';
  S_DSFGKONVERR_KONFDATA        = 'Fehler beim Zugriff auf Konfigurationsdaten';
  S_DSFGKONVERR_UNKNOWNLBSOURCE = 'Unbekannte Logbuch-Quellinstanz';
  S_DSFGKONVERR_ZAE_SOLL_IST    = 'Anzahl der DSfG-Rohdatensätze ungleich Soll-Wert im Telegrammheader (ZAE)';
  S_DSFGKONVERR_FILENOTFOUND    = 'Rohdatendatei nicht gefunden';

  S_DSFGKONVERR_INVVALUE  = 'Wert im DSfG-Telegramm ungültig';
  S_DSFGKONVERR_INVTIME   = 'Zeitstempel im DSfG-Telegramm ungültig';
  S_DSFGKONVERR_INVNUMBER = 'Ordnungsnummer im DSfG-Telegramm ungültig';
  S_DSFGKONVERR_INVADDR   = 'Adresse im DSfG-Telegramm ungültig';
  S_DSFGKONVERR_INVSTATE  = 'Status im DSfG-Telegramm ungültig';
  S_DSFGKONVERR_INVCRC    = 'Prüfsumme im DSfG-Telegramm ungültig';
  S_DSFGKONVERR_EMPTYREC  = 'DSfG-Datensatz leer';

  S_DSFGKONVERR_DATACHECK_INTERN = 'Interner Fehler bei Rohdatenprüfung';
  S_DSFGKONVERR_DATAKONV_INTERN  = 'Interner Fehler bei Rohdatenkonvertierung';

  S_DSFGKONVERR_SIGVERIFY_INVALID     = 'Signatur ist ungültig';
  S_DSFGKONVERR_SIGVERIFY_NOTVERIFIED = 'Signatur ist nicht verifiziert';

  S_DSFGKONVERR_AA_NICHT_PLAUSIBEL    = 'Vorgang nicht plausibel (AA)';
  S_DSFGKONVERR_AA_BESTAETIGUNG       = 'Bestätigung einer ordnungsgemäßen Übermittlung (AA)';
  S_DSFGKONVERR_AA_KEINE_BERECHTIGUNG = 'Fehlende Zugangsberechtigung (AA)';
  S_DSFGKONVERR_AA_UNBEKANNT          = 'Vorgang bzw. Teilnehmer unbekannt (AA)';
  S_DSFGKONVERR_AA_NICHT_BEHAND       = 'Vorgang kann z.Z. nicht behandelt werden (AA)';
  S_DSFGKONVERR_AA_SONSTIGE           = 'Sonstige außerplanmäßige Antwort';

Type

  TKlasse = record
    Code: integer;
    Text: string;
  End;

  TStatus = record
    Code: integer;
    Text: string;
  End;

  TError = record
    Code: integer;
    GasXStatus: integer;
    Code_basic: integer;  // 16.08.2011, WW
    Klasse: integer;
    Text: string;
  End;

Const
  UndocumentedKlasse = -1;
  UndocumentedError  = -1;

  { Klasse }

  MaxKlasseMsgs = 2;

  KlasseMessage : Array [1..MaxKlasseMsgs] of TKlasse = (
    (Code: FK_WARNUNG; Text: S_FK_WARNUNG),
    (Code: FK_FEHLER;  Text: S_FK_FEHLER));

{------------------------------------------------------------------------------}

  { Status (Gruppe) }

  MaxStatusMsgs = 52;

  StatusMessage : Array [1..MaxStatusMsgs] of TStatus = (
    (Code: SYS_ABRUFERROR;       Text: S_SYS_ABRUFERROR),
    (Code: COM_PORTERROR;        Text: S_COM_PORTERROR),

    (Code: COM_ERROR;            Text: S_COM_ERROR),
    (Code: COM_KOMMERROR;        Text: S_COM_KOMMERROR),
    (Code: COM_DEVICEINITERROR;  Text: S_COM_DEVICEINITERROR),
    (Code: SYS_LICENCEERROR;     Text: S_SYS_LICENCEERROR),
    (Code: COM_FUPERROR;         Text: S_COM_FUPERROR),
    (Code: COM_MRGERROR;         Text: S_COM_MRGERROR),
    (Code: COM_MODEMERROR;       Text: S_COM_MODEMERROR),
    (Code: COM_MODEMPROTERROR;   Text: S_COM_MODEMPROTERROR),
    (Code: COM_TCPIP_ERROR;      Text: S_COM_TCPIP_ERROR),
    (Code: COM_SRV_ERROR;        Text: S_COM_SRV_ERROR),
    (Code: COM_PALMUSBERROR;     Text: S_COM_PALMUSBERROR),
    (Code: COM_LAKS;             Text: S_COM_LAKS),
    (Code: COM_DSFGDFUERROR;     Text: S_COM_DSFGDFUERROR),
    (Code: COM_PDAERROR;         Text: S_COM_PDAERROR),
    (Code: COM_KE_ERROR;         Text: S_COM_KE_ERROR),
    (Code: COM_FTL_ERROR;        Text: S_COM_FTL_ERROR),
    (Code: COM_GPRS_ERROR;       Text: S_COM_GPRS_ERROR),
    (Code: COM_MODBUSERROR;      Text: S_COM_MODBUSERROR),
    (Code: COM_DS100_ERROR;      Text: S_COM_DS100_ERROR),
    (Code: SYS_SIGNSRVSYSERROR;  Text: S_SYS_SIGNSRVSYSERROR),
    (Code: COM_SIGNSRV_ERROR;    Text: S_COM_SIGNSRV_ERROR),
    (Code: SYS_SIGNSRV_ERROR;    Text: S_SYS_SIGNSRV_ERROR),
    (Code: COM_FTLINTERN_ERROR;  Text: S_COM_FTLINTERN_ERROR),

    (Code: ST_STAMMERROR;        Text: S_ST_STAMMERROR),
    (Code: ST_RUECKRUFPRUEF;     Text: S_ST_RUECKRUFPRUEF),
    (Code: ST_KONVERROR;         Text: S_ST_KONVERROR),
    (Code: ST_DSFGERROR;         Text: S_ST_DSFGERROR),
    (Code: ST_DSFGUMLERROR;      Text: S_ST_DSFGUMLERROR),
    (Code: ST_DATACHECK;         Text: S_ST_DATACHECK),
    (Code: ST_RUFREAKTERROR;     Text: S_ST_RUFREAKTERROR),
    (Code: ST_RUECKRUFERROR;     Text: S_ST_RUECKRUFERROR),
    (Code: ST_DATAERROR;         Text: S_ST_DATAERROR),
    (Code: ST_KONFIGERROR;       Text: S_ST_KONFIGERROR),
    (Code: ST_FILEERROR;         Text: S_ST_FILEERROR),

    (Code: ACT_USER;             Text: S_ACT_USER),

    (Code: EST_ZEITSYNCERROR;    Text: S_EST_ZEITSYNCERROR),
    (Code: EST_RPRESET_MEERROR;  Text: S_EST_RPRESET_MEERROR),
    (Code: EST_RPRESET_MWERROR;  Text: S_EST_RPRESET_MWERROR),
    (Code: EST_RPRESET_PRERROR;  Text: S_EST_RPRESET_PRERROR),
    (Code: EST_RUFDEAKTERROR;    Text: S_EST_RUFDEAKTERROR),
    (Code: EST_PARAMERROR;       Text: S_EST_PARAMERROR),
    (Code: EST_LOGINERROR;       Text: S_EST_LOGINERROR),
    (Code: EST_KENNUNGCHECK;     Text: S_EST_KENNUNGCHECK),
    (Code: EST_ZEITBEFEHLERROR;  Text: S_EST_ZEITBEFEHLERROR),
    (Code: EST_FIRMWAREUPDATEERROR; Text: S_EST_FIRMWAREUPDATEERROR),
    (Code: EST_GERAETECHECK;     Text: S_EST_GERAETECHECK),
    (Code: EST_DSFGTRANSPARENTERROR; Text: S_EST_DSFGTRANSPARENTERROR),
    (Code: EST_MWLESENERROR    ; Text: S_EST_MWLESENERROR),
    (Code: EST_MELESENERROR    ; Text: S_EST_MELESENERROR),

    (Code: EST_3_GBHREVISION;    Text: S_EST_3_GBHREVISION));

{------------------------------------------------------------------------------}

  { Error }

  { interne Abrufsystem-Fehlermeldungen }

  MaxSysAbrfErrorMsgs = 11;

  SysAbrfErrorMessage : Array [1..MaxSysAbrfErrorMsgs] of TError = (
    (Code: SYSABRFERR_KOMMANDONICHTPLAUSIBEL;     GasXStatus: GASX_ERR_KOMMANDONICHTPLAUSIBEL;     Code_basic: BASIC_ERR_ServerKommando; Klasse: fk_Fehler; Text: S_SYSABRFERR_KOMMANDONICHTPLAUSIBEL),
    (Code: SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT; GasXStatus: GASX_ERR_KOMMANDONICHTIMPLEMENTIERT; Code_basic: BASIC_ERR_ServerKommando; Klasse: fk_Fehler; Text: S_SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT),
    (Code: SYSABRFERR_KOMMANDOSOLLPROZESSID;      GasXStatus: GASX_ERR_KOMMANDOSOLLPROZESSID;      Code_basic: BASIC_ERR_ServerKommando; Klasse: fk_Fehler; Text: S_SYSABRFERR_KOMMANDOSOLLPROZESSID),
    (Code: SYSABRFERR_KOMMANDOUNGUELTIG;          GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_ServerKommando; Klasse: fk_Fehler; Text: S_SYSABRFERR_KOMMANDOUNGUELTIG),
    (Code: SYSABRFERR_ANTWORTNICHTPLAUSIBEL;      GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SYSABRFERR_ANTWORTNICHTPLAUSIBEL),
    (Code: SYSABRFERR_ANTWORTUNERWARTET_CMD;      GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SYSABRFERR_ANTWORTUNERWARTET_CMD),
    (Code: SYSABRFERR_ANTWORTUNERWARTET_PID;      GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SYSABRFERR_ANTWORTUNERWARTET_PID),
    (Code: SYSABRFERR_ANTWORTINHALTLAENGE;        GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SYSABRFERR_ANTWORTINHALTLAENGE),
    (Code: SYSABRFERR_TIMEOUT;                    GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SYSABRFERR_TIMEOUT),

    (Code: SYSABRFERR_MIABRUFDLLNOTFOUND;         GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SYSABRFERR_MIABRUFDLLNOTFOUND),
    (Code: SYSABRFERR_MIABRUFDLLACCESS;           GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SYSABRFERR_MIABRUFDLLACCESS));

  { COM-Port-Zugriffsfehler }

  MaxCOMPortErrorMsgs = 3;

  COMPortErrorMessage : Array [1..MaxCOMPortErrorMsgs] of TError = (
    (Code: COMPORTERR_NICHTVORHANDEN;    GasXStatus: GASX_ERR_V_COM_NICHT_FREI; Code_basic: BASIC_ERR_Fehler_COM_PortOeffnen; Klasse: fk_Fehler; Text: S_COMPORTERR_NICHTVORHANDEN),
    (Code: COMPORTERR_OEFFNEN;           GasXStatus: GASX_ERR_V_COM_NICHT_FREI; Code_basic: BASIC_ERR_Fehler_COM_PortOeffnen; Klasse: fk_Fehler; Text: S_COMPORTERR_OEFFNEN),
    (Code: COMPORTERR_DUE_GERAET_FALSCH; GasXStatus: GASX_ERR_V_MODEM_FALSCH;   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_COMPORTERR_DUE_GERAET_FALSCH));

  { Fehlermeldungen beim Datenaustausch über serielle Schnittstelle }

  MaxComErrorMsgs = 8;

  ComErrorMessage : Array [1..MaxComErrorMsgs] of TError = (
    (Code: ce_RxOver;   GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_ce_RxOver),
    (Code: ce_Overrun;  GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_ce_Overrun),
    (Code: ce_RxParity; GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_ce_RxParity),
    (Code: ce_Frame;    GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_ce_Frame),
    (Code: ce_Break;    GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_ce_Break),
    (Code: ce_TxFull;   GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_ce_TxFull),
    (Code: ce_IOE;      GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_ce_IOE),
    (Code: ce_Mode;     GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_ce_Mode));

  { Kommunikationsfehlermeldungen }

  MaxKommErrorMsgs = 7;

  KommErrorMessage : Array [1..MaxKommErrorMsgs] of TError = (
    (Code: KOMMERR_TIMEOUT;            GasXStatus: GASX_ERR_TIMEOUT;             Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_KOMMERR_TIMEOUT),
    (Code: KOMMERR_BCC;                GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_BCC_Fehler; Klasse: fk_Fehler; Text: S_KOMMERR_BCC),
    (Code: KOMMERR_CRC;                GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_BCC_Fehler; Klasse: fk_Fehler; Text: S_KOMMERR_CRC),
    (Code: KOMMERR_VERB_UNTERBROCHEN;  GasXStatus: GASX_ERR_VERB_UNTERBROCHEN;   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_KOMMERR_VERB_UNTERBROCHEN),
    (Code: KOMMERR_FTL_BLOCKSICHERUNG; GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_BCC_Fehler; Klasse: fk_Fehler; Text: S_KOMMERR_FTL_BLOCKSICHERUNG),
    (Code: KOMMERR_LRC;                GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_BCC_Fehler; Klasse: fk_Fehler; Text: S_KOMMERR_LRC),
    (Code: KOMMERR_BAUD_NICHT_UNTERSTUETZT; GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Baud_nicht_unterstuetzt; Klasse: fk_Fehler; Text: S_KOMMERR_BAUD_NICHT_UNTERSTUETZT));

  { Geräteinitialisierungs-Fehlermeldungen }

  MaxDeviceInitErrorMsgs = 4;

  DeviceInitErrorMessage : Array [1..MaxDeviceInitErrorMsgs] of TError = (
    (Code: DEVINITERR_FUPRESET;  GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DEVINITERR_FUPRESET),
    (Code: DEVINITERR_FUPINIT;   GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DEVINITERR_FUPINIT),
    (Code: DEVINITERR_MODEMINIT; GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DEVINITERR_MODEMINIT),
    (Code: DEVINITERR_PIN_LOCK;  GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DEVINITERR_PIN_LOCK));

  { Lizenz-Fehlermeldungen }

  MaxLicenceErrorMsgs = 6;

  LicenceErrorMessage : Array [1..MaxLicenceErrorMsgs] of TError = (
    (Code: LICENCEERR_GERAETETYP;      GasXStatus: GASX_ERR_MRGTYPNICHTIMPLEMENTIERT; Code_basic: BASIC_ERR_Lizenz; Klasse: fk_Fehler; Text: S_LICENCEERR_GERAETETYP),
    (Code: LICENCEERR_LAUFZEIT;        GasXStatus: GASX_ERR_LIZENZ_ZEIT_ABGELAUFEN;   Code_basic: BASIC_ERR_Lizenz; Klasse: fk_Fehler; Text: S_LICENCEERR_LAUFZEIT),
    (Code: LICENCEERR_KEINESTATIONEN;  GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_LICENCEERR_KEINESTATIONEN),
    (Code: LICENCEERR_ANZAHLSTATIONEN; GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_LICENCEERR_ANZAHLSTATIONEN),
    (Code: LICENCEERR_UNBEKANNT;       GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_LICENCEERR_UNBEKANNT),
    (Code: LICENCEERR_PROGFUNKTION;    GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Lizenz; Klasse: fk_Fehler; Text: S_LICENCEERR_PROGFUNKTION));

  { Fehlermeldungen des FUP }

  MaxFupErrorMsgs = 25;

  FupErrorMessage : Array [1..MaxFupErrorMsgs] of TError = (
    (Code: FUPERR_F0; GasXStatus: GASX_ERR_TIMEOUT;             Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_F0),
    (Code: FUPERR_F1; GasXStatus: GASX_ERR_TIMEOUT;             Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_F1),
    (Code: FUPERR_F2; GasXStatus: GASX_ERR_V_LEITUNG_BESETZT;   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_F2),
    (Code: FUPERR_F3; GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_F3),
    (Code: FUPERR_F4; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_F4),
    (Code: FUPERR_F5; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_F5),
    (Code: FUPERR_F6; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_F6),
    (Code: FUPERR_F7; GasXStatus: GASX_ERR_TIMEOUT;             Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_F7),
    (Code: FUPERR_F8; GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_F8),
    (Code: FUPERR_F9; GasXStatus: GASX_ERR_TIMEOUT;             Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_F9),
    (Code: FUPERR_FA; GasXStatus: GASX_ERR_TIMEOUT;             Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_FA),
    (Code: FUPERR_FC; GasXStatus: GASX_ERR_TIMEOUT;             Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_FC),
    (Code: FUPERR_FD; GasXStatus: GASX_ERR_TIMEOUT;             Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_FD),
    (Code: FUPERR_FE; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FE),
    (Code: FUPERR_FF; GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_FUPERR_FF),
    (Code: FUPERR_FG; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FG),
    (Code: FUPERR_FH; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FH),
    (Code: FUPERR_FI; GasXStatus: GASX_ERR_VERB_UNTERBROCHEN;   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FI),
    (Code: FUPERR_FM; GasXStatus: GASX_ERR_VERB_UNTERBROCHEN;   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FM),
    (Code: FUPERR_FN; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FN),
    (Code: FUPERR_FZ; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FZ),
    (Code: FUPERR_FU; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FU),
    (Code: FUPERR_FS; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FS),
    (Code: FUPERR_FT; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FT),
    (Code: FUPERR_FB; GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FUPERR_FB));

  { Fehlermeldungen vom MRG an Host }

  MaxMRGErrorMsgs = 12;

  MRGErrorMessage : Array [1..MaxMRGErrorMsgs] of TError = (
    (Code: MRGERR_KOMMANDOUNPLAUSIBEL;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_KOMMANDOUNPLAUSIBEL),
    (Code: MRGERR_KEINEBERECHTIGUNG;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_KEINEBERECHTIGUNG),
    (Code: MRGERR_FALSCHESFORMAT;          GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_FALSCHESFORMAT),
    (Code: MRGERR_REVISION;                GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_REVISION),
    (Code: MRGERR_KOMMANDOUNBEKANNT;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_KOMMANDOUNBEKANNT),
    (Code: MRGERR_AENDERUNGNICHTZULAESSIG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_AENDERUNGNICHTZULAESSIG),
    (Code: MRGERR_ANTWORTUNVOLLSTAENDIG;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Geraetedaten_nicht_plausibel; Klasse: fk_Fehler; Text: S_MRGERR_ANTWORTUNVOLLSTAENDIG),
    (Code: MRGERR_ANTWORTUNERWARTET;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_ANTWORTUNERWARTET),
    (Code: MRGERR_FEHLERTELEGRAMM;         GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Geraetedaten_nicht_plausibel; Klasse: fk_Fehler; Text: S_MRGERR_FEHLERTELEGRAMM),
    (Code: MRGERR_DATASIZE_SOLL_IST;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_DATASIZE_SOLL_IST),
    (Code: MRGERR_DATASIZE_EXCEEDED_NACK;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_DATASIZE_EXCEEDED_NACK),
    (Code: MRGERR_MOMENTANNICHTMOEGLICH;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MRGERR_MOMENTANNICHTMOEGLICH));


  { Fehlermeldungen Modem }

  MaxModemErrorMsgs = 27;

  ModemErrorMessage : Array [1..MaxModemErrorMsgs] of TError = (
    (Code: CME_CONNECT;     GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_CONNECT),
    (Code: CME_ERROR;       GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_ERROR),
    (Code: CME_RING;        GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_RING),
    (Code: CME_NOCARRIER;   GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_NOCARRIER),
    (Code: CME_NODIALTONE;  GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_NODIALTONE),
    (Code: CME_DIALLOCKED;  GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_DIALLOCKED),
    (Code: CME_BUSY;        GasXStatus: GASX_ERR_V_LEITUNG_BESETZT;        Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_BUSY),
    (Code: CME_DELAYED;     GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_DELAYED),
    (Code: CME_NOANSWER;    GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_NOANSWER),
    (Code: CME_ABORT;       GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_ABORT),
    (Code: CME_OK;          GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_OK),
    (Code: CME_SONST;       GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_SONST),
    (Code: CME_DCD;         GasXStatus: GASX_ERR_VERB_UNTERBROCHEN;        Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_DCD),
    (Code: CME_CTS;         GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_CTS),
    (Code: CME_BLACKLISTED; GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_BLACKLISTED),

    (Code: CME_GSM_PIN_CHECK;                   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_PIN_CHECK),
    (Code: CME_GSM_PIN_WRITE;                   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_PIN_WRITE),
    (Code: CME_GSM_PIN_WRONG;                   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_PIN_WRONG),
    (Code: CME_GSM_PIN_LOGINIMPOSSIBLE_PUK;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_PIN_LOGINIMPOSSIBLE_PUK),
    (Code: CME_GSM_SMS_FORMAT_WRITE;            GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_SMS_FORMAT_WRITE),
    (Code: CME_GSM_SMS_READ;                    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_SMS_READ),
    (Code: CME_GSM_SMS_DELETE;                  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_SMS_DELETE),
    (Code: CME_GSM_PIN_LOGINIMPOSSIBLE_UNKNOWN; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_PIN_LOGINIMPOSSIBLE_UNKNOWN),
    (Code: CME_GSM_SMS_SEND;                    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_SMS_SEND),
    (Code: CME_GSM_CSQ_READ;                    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_CSQ_READ),
    (Code: CME_GSM_COPS_READ;                   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_COPS_READ),
    (Code: CME_GSM_REG_READ;                    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CME_GSM_REG_READ));


  { Modemprotokoll-Fehlermeldungen }

  MaxModemProtErrorMsgs = 7;

  ModemProtErrorMessage : Array [1..MaxModemProtErrorMsgs] of TError = (
    (Code: CMPE_ENQ;            GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_CMPE_ENQ),
    (Code: CMPE_NAK;            GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_CMPE_NAK),
    (Code: CMPE_EOT;            GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CMPE_EOT),
    (Code: CMPE_DLE_EOT;        GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CMPE_DLE_EOT),
    (Code: CMPE_UNGUELTIG;      GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_CMPE_UNGUELTIG),
    (Code: CMPE_UNVOLLSTAENDIG; GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_CMPE_UNVOLLSTAENDIG),
    (Code: CMPE_UNBESTIMMT;     GasXStatus: GASX_ERR_SONSTIG;             Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CMPE_UNBESTIMMT));

  { TCP/IP-Fehlermeldungen }

  MaxTCPIPErrorMsgs = 11;

  TCPIPErrorMessage : Array [1..MaxTCPIPErrorMsgs] of TError = (
    (Code: TCPIP_ERR_GENERAL;             GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER;      Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_TCPIP_ERR_GENERAL),
    (Code: TCPIP_ERR_SEND;                GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER;      Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_TCPIP_ERR_SEND),
    (Code: TCPIP_ERR_RECEIVE;             GasXStatus: GASX_ERR_UEBERTRAGUNGSFEHLER;      Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_TCPIP_ERR_RECEIVE),
    (Code: TCPIP_ERR_CONNECT;             GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_TCPIP_ERR_CONNECT),
    (Code: TCPIP_ERR_DISCONNECT;          GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_TCPIP_ERR_DISCONNECT),
    (Code: TCPIP_ERR_ACCEPT;              GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_TCPIP_ERR_ACCEPT),
    (Code: TCPIP_ERR_TIMEOUTCONNECT;      GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_TCPIP_ERR_TIMEOUTCONNECT),
    (Code: TCPIP_ERR_CONNECTION_INACTIVE; GasXStatus: GASX_ERR_VERB_UNTERBROCHEN;        Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_TCPIP_ERR_CONNECTION_INACTIVE),
    (Code: TCPIP_ERR_TIMEOUTDISCONNECT;   GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_TCPIP_ERR_TIMEOUTDISCONNECT),
    (Code: TCPIP_ERR_LOOKUP;              GasXStatus: GASX_ERR_V_LEITUNG_MODEM_GESTOERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_TCPIP_ERR_LOOKUP),
    (Code: TCPIP_ERR_UNDEFINIERT;         GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_TCPIP_ERR_UNDEFINIERT));

  { Kommunikationsfehlermeldungen Abrufserver, Signaturserver }

  MaxSrvErrorMsgs = 12;

  SrvErrorMessage : Array [1..MaxSrvErrorMsgs] of TError = (
    (Code: SRV_ERR_GENERAL;             GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_GENERAL),
    (Code: SRV_ERR_SEND;                GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_SEND),
    (Code: SRV_ERR_RECEIVE;             GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_RECEIVE),
    (Code: SRV_ERR_CONNECT;             GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_CONNECT),
    (Code: SRV_ERR_DISCONNECT;          GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_DISCONNECT),
    (Code: SRV_ERR_ACCEPT;              GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_ACCEPT),
    (Code: SRV_ERR_TIMEOUTCONNECT;      GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_TIMEOUTCONNECT),
    (Code: SRV_ERR_CONNECTION_INACTIVE; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_CONNECTION_INACTIVE),
    (Code: SRV_ERR_TIMEOUTDISCONNECT;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_TIMEOUTDISCONNECT),
    (Code: SRV_ERR_LOOKUP;              GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_LOOKUP),
    (Code: SRV_ERR_UNDEFINIERT;         GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_UNDEFINIERT),
    (Code: SRV_ERR_TIMEOUTRECEIVE;      GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SRV_ERR_TIMEOUTRECEIVE));

  { Fehlermeldungen LAKS }

  MaxLaksErrorMsgs = 4;

  LaksErrorMessage : array [1..MaxLaksErrorMsgs] of TError = (
    (Code: CLAKS_PARITYBCC;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_BCC_Fehler; Klasse: fk_Fehler; Text: S_CLAKS_PARITYBCC),
    (Code: CLAKS_ERROR;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CLAKS_ERROR),
    (Code: CLAKS_WRONGNUMBER; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CLAKS_WRONGNUMBER),
    (Code: CLAKS_FORBIDDEN;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_CLAKS_FORBIDDEN));

  { Fehlermeldungen DSfG-DFÜ }

  MaxDSfGDFUErrorMsgs = 7;

  DSfGDFUErrorMessage : array [1..MaxDSfGDFUErrorMsgs] of TError = (
    (Code: DSFGDFUERR_ANTWORTUNVOLLSTAENDIG;         GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DSFGDFUERR_ANTWORTUNVOLLSTAENDIG),
    (Code: DSFGDFUERR_UNBEKANNT;                     GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DSFGDFUERR_UNBEKANNT),
    (Code: DSFGDFUERR_FALSCHESYNTAX;                 GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DSFGDFUERR_FALSCHESYNTAX),
    (Code: DSFGDFUERR_WIESER_UNBEKANNT;              GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DSFGDFUERR_WIESER_UNBEKANNT),
    (Code: DSFGDFUERR_WIESER_FALSCHENUMMER;          GasXStatus: GASX_ERR_PARAMETER_UNBEK;           Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DSFGDFUERR_WIESER_FALSCHENUMMER),
    (Code: DSFGDFUERR_WIESER_AENDEREUNGNICHTERLAUBT; GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DSFGDFUERR_WIESER_AENDEREUNGNICHTERLAUBT),
    (Code: DSFGDFUERR_WIESER_VERBOTEN;               GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DSFGDFUERR_WIESER_VERBOTEN));

  { Fehlermeldungen beim Zugriff auf Stammdaten }

  MaxStammdatenErrorMsgs = 14;

  StammdatenErrorMessage : Array [1..MaxStammDatenErrorMsgs] of TError = (
    (Code: SMERR_STAMMSATZLESEN;    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_STAMMSATZLESEN),
    (Code: SMERR_RUFSTAMMDATEN;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_RUFSTAMMDATEN),
    (Code: SMERR_STAMMSATZNOTFOUND; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_STAMMSATZNOTFOUND),
    (Code: SMERR_GERAETETYPFALSCH;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_GERAETETYPFALSCH),
    (Code: SMERR_DSFGINST_NOTFOUND; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_DSFGINST_NOTFOUND),
    (Code: SMERR_DSFGAK_NOTFOUND;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_DSFGAK_NOTFOUND),
    (Code: SMERR_DSFGLB_NOTFOUND;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_DSFGLB_NOTFOUND),
    (Code: SMERR_MRGLGZARCHORIG;    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_MRGLGZARCHORIG),
    (Code: SMERR_DSFGINSTTYP_WRONG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_DSFGINSTTYP_WRONG),
    (Code: SMERR_KENNUNG_MEHRFACH_KEIN_GPRS_ABRUF; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_KENNUNG_MEHRFACH_KEIN_GPRS_ABRUF),
    (Code: SMERR_STATIONZAEHLPKT_NICHTEINDEUTIG;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_STATIONZAEHLPKT_NICHTEINDEUTIG),
    (Code: SMERR_KANALNOTFOUND;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_KANALNOTFOUND),
    (Code: SMERR_KANALOBISKENNZ_NICHTEINDEUTIG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_KANALOBISKENNZ_NICHTEINDEUTIG),
    (Code: SMERR_EINGABEUNGUELTIG;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_SMERR_EINGABEUNGUELTIG));

  { Fehlermeldungen beim Zugriff auf Konfigurationsdaten }

  MaxKonfigdatenErrorMsgs = 3;

  KonfigdatenErrorMessage : Array [1..MaxKonfigDatenErrorMsgs] of TError = (
    (Code: KFERR_KONFIGDATANOTFOUND; GasXStatus: GASX_ERR_MRGTYPNICHTIMPLEMENTIERT; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_KFERR_KONFIGDATANOTFOUND),
    (Code: KFERR_PARAMETERNOTFOUND;  GasXStatus: GASX_ERR_PARAMETER_UNBEK;          Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_KFERR_PARAMETERNOTFOUND),
    (Code: KFERR_KONFIGDATAINVALID;  GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_KFERR_KONFIGDATAINVALID));

  { Konvertierungsfehlermeldungen }

  MaxKonvErrorMsgs = 16;

  KonvErrorMessage : Array [1..MaxKonvErrorMsgs] of TError = (
    (Code: SKERR_MELDKONV;                      GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_MELDKONV),
    (Code: SKERR_MESSKONV;                      GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_MESSKONV),
    (Code: SKERR_EBANDKONV;                     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_EBANDKONV),
    (Code: SKERR_PARAMKONV;                     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_PARAMKONV),
    (Code: SKERR_LASTZSTNDKONV;                 GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_LASTZSTNDKONV),
    (Code: SKERR_KARCHIVKONV;                   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_KARCHIVKONV),
    (Code: SKERR_ARCHIVKONV;                    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_ARCHIVKONV),
    (Code: SKERR_LOGBKONV;                      GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_LOGBKONV),
    (Code: SKERR_INSTWERTKONV;                  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_INSTWERTKONV),
    (Code: SKERR_AUFMTELEGRAMMKONV;             GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_AUFMTELEGRAMMKONV),
    (Code: SKERR_KONVGERTYPNOTAVAILABLE;        GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_SKERR_KONVGERTYPNOTAVAILABLE),
    (Code: SKERR_AUSSERPLANMAESSIGEANTW_AR;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_SKERR_AUSSERPLANMAESSIGEANTW_AR),
    (Code: SKERR_AUSSERPLANMAESSIGEANTW_LB;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_SKERR_AUSSERPLANMAESSIGEANTW_LB),
    (Code: SKERR_AUSSERPLANMAESSIGEANTW_DE;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_SKERR_AUSSERPLANMAESSIGEANTW_DE),
    (Code: SKERR_DSFGDFUEDATAKONV;              GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_SKERR_DSFGDFUEDATAKONV),
    (Code: SKERR_AUSSERPLANMAESSIGEANTW_KONFIG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_SKERR_AUSSERPLANMAESSIGEANTW_KONFIG));

  { Fehlermeldungen bei Kommunikation mit DSfG-DFÜ-Instanz }

  MaxDSfGErrorMsgs = 5;

  DSfGErrorMessage : Array [1..MaxDSfGErrorMsgs] of TError = (
    (Code: DSFGERR_ENQ;                 GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_DSFGERR_ENQ),
    (Code: DSFGERR_CAN;                 GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DSFGERR_CAN),
    (Code: DSFGERR_NAK;                 GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_DSFGERR_NAK),
    (Code: DSFGERR_BCC;                 GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_BCC_Fehler; Klasse: fk_Fehler;  Text: S_DSFGERR_BCC),
    (Code: DSFGERR_QUITTUNGUNDEFINIERT; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_DSFGERR_QUITTUNGUNDEFINIERT));

  { Fehlermeldungen für DSfG-Umleitung }

  MaxDSfGUmlErrorMsgs = 5;

  DSfGUmlErrorMessage : Array [1..MaxDSfGUmlErrorMsgs] of TError = (
    (Code: UMLERR_UMSCHALTUNG;                  GasXStatus: GASX_ERR_DSFGUMSCHALTUNG_AUF_ADRESSE; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_UMLERR_UMSCHALTUNG),
    (Code: UMLERR_INVKENNUNG_KEINE_UMSCHALTUNG; GasXStatus: GASX_ERR_DSFGUMSCHALTUNG_AUF_ADRESSE; Code_basic: BASIC_ERR_Kennung_falsch; Klasse: fk_Fehler;  Text: S_UMLERR_INVKENNUNG_KEINE_UMSCHALTUNG),
    (Code: UMLERR_RUFLISTE;                     GasXStatus: GASX_ERR_SONSTIG;                     Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_UMLERR_RUFLISTE),
    (Code: UMLERR_RUFQUITTIEREN;                GasXStatus: GASX_ERR_SONSTIG;                     Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_UMLERR_RUFQUITTIEREN),
    (Code: UMLERR_INVKENNUNG_UMSCHALTUNG;       GasXStatus: 0; { OK }                             Code_basic: BASIC_ERR_Kennung_falsch; Klasse: fk_Warnung; Text: S_UMLERR_INVKENNUNG_UMSCHALTUNG));

  { Fehlermeldungen für Zeitsynchronisation }

  MaxZSyncErrorMsgs = 20;

  ZSyncErrorMessage : Array [1..MaxZSyncErrorMsgs] of TError = (
    (Code: ZSYNCERR_SUCCESS;         GasXStatus: GASX_ERR_ZSYNC_DURCHGEFUEHRT;       Code_basic: BASIC_ERR_OK; Klasse: fk_Warnung; Text: S_ZSYNCERR_SUCCESS),
    (Code: ZSYNCERR_DIFFERENTHOURS;  GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_DIFFERENTHOURS),
    (Code: ZSYNCERR_PERIODEND;       GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_PERIODEND),
    (Code: ZSYNCERR_UNDEFINIERT;     GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_UNDEFINIERT),
    (Code: ZSYNCERR_HIGHERMAX;       GasXStatus: GASX_ERR_ZSYNC_ABWEICHUNG_ZU_GROSS; Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_HIGHERMAX),
    (Code: ZSYNCERR_WRONGPASSWORDNR; GasXStatus: GASX_ERR_PW_PWNR_FALSCH;            Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_WRONGPASSWORDNR),
    (Code: ZSYNCERR_NOSUCCESS;       GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_NOSUCCESS),
    (Code: ZSYNCERR_SZTOWZ;          GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_SZTOWZ),
    (Code: ZSYNCERR_CORRMAX;         GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_CORRMAX),
    (Code: ZSYNCERR_READDEVICETIME;    GasXStatus: GASX_ERR_SONSTIG;                 Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_READDEVICETIME),
    (Code: ZSYNCERR_INVALIDDEVICETIME; GasXStatus: GASX_ERR_SONSTIG;                 Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_INVALIDDEVICETIME),

    (Code: ZSYNCERR_VERZOEGERT;           GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_VERZOEGERT),
    (Code: ZSYNCERR_NICHTDURCHGEFUEHRT;   GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_NICHTDURCHGEFUEHRT),
    (Code: ZSYNCERR_SYNTAXFEHLER;         GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_SYNTAXFEHLER),
    (Code: ZSYNCERR_GERAETBUSY;           GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_GERAETBUSY),
    (Code: ZSYNCERR_BEREITSSYNCHRON;      GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_BEREITSSYNCHRON),
    (Code: ZSYNCERR_SICHERUNGFALSCH;      GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_SICHERUNGFALSCH),
    (Code: ZSYNCERR_VERSTELLWEITEGEKAPPT; GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_VERSTELLWEITEGEKAPPT),
    (Code: ZSYNCERR_WIEDERHOLSPERRE;      GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_WIEDERHOLSPERRE),
    (Code: ZSYNCERR_SONSTIG;              GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Fehler_ZeitSynch; Klasse: fk_Warnung; Text: S_ZSYNCERR_SONSTIG));

  { Fehlermeldungen für Rundpuffer-Rücksetzen }

  MaxRPResetErrorMsgs = 2;

  RPResetErrorMessage : Array [1..MaxRPResetErrorMsgs] of TError = (
    (Code: RPRESETERR_WRONGPASSWORDNR; GasXStatus: GASX_ERR_PW_PWNR_FALSCH;            Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_RPRESETERR_WRONGPASSWORDNR),
    (Code: RPRESETERR_NOSUCCESS;       GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_RPRESETERR_NOSUCCESS));

  { Fehlermeldungen für Datenüberprüfung }

  MaxDataCheckMsgs = 10;

  DataCheckMessage : Array [1..MaxDataCheckMsgs] of TError = (
    (Code: DCH_MWMISSING;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_MWMISSING),
    (Code: DCH_MEINVALID;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_MEINVALID),
    (Code: DCH_ARINVALID;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_ARINVALID),
    (Code: DCH_LBINVALID;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_LBINVALID),
    (Code: DCH_DEINVALID;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_DEINVALID),
    (Code: DCH_ORDNRFOLGE; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_ORDNRFOLGE),
    (Code: DCH_MWINVALID;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_MWINVALID),
    (Code: DCH_INVALID;    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_INVALID),
    (Code: DCH_KZWINVALID; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_KZWINVALID),
    (Code: DCH_ORDNRFUELLSTANDBIS; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_DCH_ORDNRFUELLSTANDBIS));

  { Fehlermeldungen für Rufdeaktivierung }

  MaxRufDeaktErrorMsgs = 3;

  RufDeaktErrorMessage : Array [1..MaxRufDeaktErrorMsgs] of TError = (
    (Code: RUFDEAKTERR_WRONGPASSWORDNR; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_RUFDEAKTERR_WRONGPASSWORDNR),
    (Code: RUFDEAKTERR_NOSUCCESS;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_RUFDEAKTERR_NOSUCCESS),
    (Code: RUFDEAKTERR_SUCCESS;         GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_RUFDEAKTERR_SUCCESS));

  { Fehlermeldungen für Rufreaktivierung }

  MaxRufReaktErrorMsgs = 3;

  RufReaktErrorMessage : Array [1..MaxRufReaktErrorMsgs] of TError = (
    (Code: RUFREAKTERR_WRONGPASSWORDNR; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_RUFREAKTERR_WRONGPASSWORDNR),
    (Code: RUFREAKTERR_NOSUCCESS;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_RUFREAKTERR_NOSUCCESS),
    (Code: RUFREAKTERR_FINDRUFNR;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_RUFREAKTERR_FINDRUFNR));

  { Fehlermeldungen für Parametrierung }

  MaxParamErrorMsgs = 6;

  ParamErrorMessage : Array [1..MaxParamErrorMsgs] of TError = (
    (Code: PARAMERR_NOSUCCESS;                  GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PARAMERR_NOSUCCESS),
    (Code: PARAMERR_DSFG_WRONGANSWER;           GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PARAMERR_DSFG_WRONGANSWER),
    (Code: PARAMERR_DSFG_WRONGCODENR_UNKNOWNDE; GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PARAMERR_DSFG_WRONGCODENR_UNKNOWNDE),
    (Code: PARAMERR_DSFG_NOCHANGEABLEDE_NOES;   GasXStatus: GASX_ERR_PARAMETRIERUNG_ABGEWIESEN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PARAMERR_DSFG_NOCHANGEABLEDE_NOES),
    (Code: PARAMERR_FORMATWERT;                 GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PARAMERR_FORMATWERT),
    (Code: PARAMERR_NOTAVAILABLE;               GasXStatus: GASX_ERR_SONSTIG;                   Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PARAMERR_NOTAVAILABLE));

  { Fehlermeldungen für Rückrufauslösung }

  MaxRueckRufErrorMsgs = 1;

  RueckRufErrorMessage : Array [1..MaxRueckRufErrorMsgs] of TError = (
    (Code: RUECKRUFERR_AUSLOESUNG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_RUECKRUFERR_AUSLOESUNG));

  { Fehlermeldungen für Rückrufprüfung }

  MaxRueckRufPruefMsgs = 5;

  RueckRufPruefMessage : Array [1..MaxRueckRufPruefMsgs] of TError = (
    (Code: RUECKRUFPRUEF_AUSLOESUNGERR;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_RUECKRUFPRUEF_AUSLOESUNGERR),
    (Code: RUECKRUFPRUEF_RUFERR;            GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_RUECKRUFPRUEF_RUFERR),
    (Code: RUECKRUFPRUEF_KEINRUF;           GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_RUECKRUFPRUEF_KEINRUF),
    (Code: RUECKRUFPRUEF_JOURNALERR;        GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_RUECKRUFPRUEF_JOURNALERR),
    (Code: RUECKRUFPRUEF_AUSLOESUNGABBRUCH; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_RUECKRUFPRUEF_AUSLOESUNGABBRUCH));

  { Fehlermeldungen für Login }

  MaxLoginErrorMsgs = 8;

  LoginErrorMessage : Array [1..MaxLoginErrorMsgs] of TError = (
    (Code: LOGINERR_WRONGPW;          GasXStatus: GASX_ERR_V_PWERR_KEINE_VERBINDUNG; Code_basic: BASIC_ERR_Loginfehler; Klasse: fk_Fehler; Text: S_LOGINERR_WRONGPW),
    (Code: LOGINERR_NODFUEBUSADDRESS; GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_LOGINERR_NODFUEBUSADDRESS),
    (Code: LOGINERR_NOACTIVEUSER;     GasXStatus: GASX_ERR_SONSTIG;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_LOGINERR_NOACTIVEUSER),
    (Code: LOGINERR_TIMEOUT_WRONGPW;  GasXStatus: GASX_ERR_TIMEOUT;                  Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_LOGINERR_TIMEOUT_WRONGPW),
    (Code: LOGINERR_MEHRERELEITSTATIONEN; GasXStatus: GASX_ERR_SONSTIG;              Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_LOGINERR_MEHRERELEITSTATIONEN),
    (Code: LOGINERR_LIEFERANTENSCHLOSS_GEOEFFNET; GasXStatus: 0; { OK }              Code_basic: BASIC_ERR_OK; Klasse: fk_Warnung; Text: S_LOGINERR_LIEFERANTENSCHLOSS_GEOEFFNET),
    (Code: LOGINERR_KUNDENSCHLOSS_GEOEFFNET;      GasXStatus: 0; { OK }              Code_basic: BASIC_ERR_OK; Klasse: fk_Warnung; Text: S_LOGINERR_KUNDENSCHLOSS_GEOEFFNET),
    (Code: LOGINERR_DATENAUSLESERSCHLOSS_GEOEFFNET; GasXStatus: 0; { OK }            Code_basic: BASIC_ERR_OK; Klasse: fk_Warnung; Text: S_LOGINERR_DATENAUSLESERSCHLOSS_GEOEFFNET));

  { Fehlermeldungen bei Kennungsüberprüfung }

  MaxKennErrorMsgs = 8;

  KennErrorMessage : Array [1..MaxKennErrorMsgs] of TError = (
    (Code: KENNERR_VERBINDUNG;       GasXStatus: GASX_ERR_V_KENNERR_VERBINDUNG;       Code_basic: BASIC_ERR_Kennung_falsch; Klasse: fk_Warnung; Text: S_KENNERR_VERBINDUNG),
    (Code: KENNERR_KEINE_VERBINDUNG; GasXStatus: GASX_ERR_V_KENNERR_KEINE_VERBINDUNG; Code_basic: BASIC_ERR_Kennung_falsch; Klasse: fk_Fehler;  Text: S_KENNERR_KEINE_VERBINDUNG),
    (Code: KENNERR_WRONGANSWER;      GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_KENNERR_WRONGANSWER),
    (Code: KENNERR_KEINSTAMMSATZ;    GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_KENNERR_KEINSTAMMSATZ),
    (Code: KENNERR_ALREADYEXISTS_W;  GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_KENNERR_ALREADYEXISTS),
    (Code: KENNERR_MEHRFACH_NODATA;  GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_KENNERR_MEHRFACH_NODATA),
    (Code: KENNERR_RE_PWACTIVATED;   GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_KENNERR_RE_PWACTIVATED),
    (Code: KENNERR_ALREADYEXISTS_F;  GasXStatus: GASX_ERR_SONSTIG;                    Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler;  Text: S_KENNERR_ALREADYEXISTS));

  { Fehlermeldungen für Archivdatenzugriff }

  MaxDataErrorMsgs = 1;

  DataErrorMessage : Array [1..MaxDataErrorMsgs] of TError = (
    (Code: DERR_DELETE; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DERR_DELETE));

  { Fehlermeldungen für allgemeine Dateizugriffe }

  MaxFileErrorMsgs = 12;

  FileErrorMessage : Array [1..MaxFileErrorMsgs] of TError = (
    (Code: FILEERR_COULDNOTOPEN;  GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_COULDNOTOPEN),
    (Code: FILEERR_COULDNOTWRITE; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Fehler_Datenschreiben; Klasse: fk_Fehler; Text: S_FILEERR_COULDNOTWRITE),
    (Code: FILEERR_FTL_SR_ALARM_COULDNOTCREATEDIR; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_FTL_SR_ALARM_COULDNOTCREATEDIR),
    (Code: FILEERR_FTL_SR_ALARM_COULDNOTCREATEFILE; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_FTL_SR_ALARM_COULDNOTCREATEFILE),
    (Code: FILEERR_FTL_SR_ALARM_COULDNOTOPENFILE; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_FTL_SR_ALARM_COULDNOTOPENFILE),
    (Code: FILEERR_RUECKRUFPRUEF_COULDNOTCREATEDIR; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_RUECKRUFPRUEF_COULDNOTCREATEDIR),
    (Code: FILEERR_RUECKRUFPRUEF_COULDNOTCREATEFILE; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_RUECKRUFPRUEF_COULDNOTCREATEFILE),
    (Code: FILEERR_RUECKRUFPRUEF_COULDNOTOPENFILE; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_RUECKRUFPRUEF_COULDNOTOPENFILE),
    (Code: FILEERR_IMPORT_LOADFILE; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_IMPORT_LOADFILE),
    (Code: FILEERR_FIRMWUPD_PARA_COULDNOTWRITE; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_FIRMWUPD_PARA_COULDNOTWRITE),
    (Code: FILEERR_FIRMWUPD_PARA_COULDNOTREAD; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_FIRMWUPD_PARA_COULDNOTREAD),
    (Code: FILEERR_FIRMWUPD_PARA_DOESNOTEXIST; GasXStatus: GASX_ERR_DATENZUGRIFF_INTERN; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FILEERR_FIRMWUPD_PARA_DOESNOTEXIST));


  { Fehlermeldungen für PDA-Kommunikation }

  MaxPDAErrorMsgs = 3;

  PDAErrorMessage : Array [1..MaxPDAErrorMsgs] of TError = (
    (Code: PDAERR_ANTW_UNDEFINIERT; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PDAERR_ANTW_UNDEFINIERT),
    (Code: PDAERR_CRC;              GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PDAERR_CRC),
    (Code: PDAERR_CTS;              GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PDAERR_CTS));

  { Fehlermeldungen für KE-Kommunikation }

  MaxKEErrorMsgs = 4;

  KEErrorMessage : Array [1..MaxKEErrorMsgs] of TError = (
    (Code: KEERR_HEADER;                 GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_KEERR_HEADER),
    (Code: KEERR_BEFEHLSYNTAX;           GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_KEERR_BEFEHLSYNTAX),
    (Code: KEERR_BEFEHL_NICHT_ERLAUBT;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_KEERR_BEFEHL_NICHT_ERLAUBT),
    (Code: KEERR_ANTWORT_UNVOLLSTAENDIG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_KEERR_ANTWORT_UNVOLLSTAENDIG));

  { Fehlermeldungen für Palm-USB-Kommunikation }

  MaxPalmUSBErrorMsgs = 14;

  PalmUSBErrorMessage : Array [1..MaxPalmUSBErrorMsgs] of TError = (
    (Code: PALMUSBERR_Unknown;           GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_Unknown),
    (Code: PALMUSBERR_SendTimedOut;      GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_SendTimedOut),
    (Code: PALMUSBERR_RecvTimedOut;      GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_RecvTimedOut),
    (Code: PALMUSBERR_PortNotOpen;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_PortNotOpen),
    (Code: PALMUSBERR_IOError;           GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_IOError),
    (Code: PALMUSBERR_PortBusy;          GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_PortBusy),
    (Code: PALMUSBERR_NotSupported;      GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_NotSupported),
    (Code: PALMUSBERR_BufferTooSmall;    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_BufferTooSmall),
    (Code: PALMUSBERR_NoAttachedDevices; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_NoAttachedDevices),
    (Code: PALMUSBERR_DontMatchFilter;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_DontMatchFilter),

    (Code: PALMUSBERR_DLLAccess;         GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_DLLAccess),
    (Code: PALMUSBERR_CouldNotOpenPort;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_CouldNotOpenPort),
    (Code: PALMUSBERR_SendIncomplete;    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_SendIncomplete),
    (Code: PALMUSBERR_ReceiveBuffer;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_PALMUSBERR_ReceiveBuffer));

  { Fehlermeldungen für Status = ACT_USER }
  MaxUserActionMsgs = 1;

  UserActionMessage : Array [1..MaxUserActionMsgs] of TError = (
    (Code: USERACT_ABBRUCH_ABRUF; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Warnung; Text: S_USERACT_ABBRUCH_ABRUF));

  { Fehlermeldungen von Tritschler-Geräten mit FTL-Protokoll }
  MaxFTLErrorMsgs = 11;

  FTLErrorMessage : Array [1..MaxFTLErrorMsgs] of TError = (
    (Code: FTLERR_EMPFANGSPUFFERUEBERLAUF;               GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_EMPFANGSPUFFERUEBERLAUF),
    (Code: FTLERR_NICHTEMPFANGSBEREIT_TIMEOUT;           GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_NICHTEMPFANGSBEREIT_TIMEOUT),
    (Code: FTLERR_STARTSTOPBIT;                          GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_STARTSTOPBIT),
    (Code: FTLERR_BLOCKSICHERUNG;                        GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_BLOCKSICHERUNG),
    (Code: FTLERR_KOMMANDONICHTAUSFUEHRBAR_FALSCHEFKTNR; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_KOMMANDONICHTAUSFUEHRBAR_FALSCHEFKTNR),
    (Code: FTLERR_KEINEDATENVORHANDEN;                   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_KEINEDATENVORHANDEN),
    (Code: FTLERR_UNBEKZEICHEN_KOMMANDO;                 GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_UNBEKZEICHEN_KOMMANDO),
    (Code: FTLERR_PARAMETERUNZULAESSIG;                  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_PARAMETERUNZULAESSIG),
    (Code: FTLERR_QUITTUNGUNDEFINIERT;                   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_QUITTUNGUNDEFINIERT),
    (Code: FTLERR_ANTWORTUNVOLLSTAENDIG;                 GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_ANTWORTUNVOLLSTAENDIG),
    (Code: FTLERR_ANTWORTUNERWARTET;                     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLERR_ANTWORTUNERWARTET));

  { Fehlermeldungen von Tritschler-Geräten (internes FTL-Protokoll) }
  MaxFTLInternErrorMsgs = 17;

  FTLInternErrorMessage : Array [1..MaxFTLInternErrorMsgs] of TError = (
    (Code: FTLINTERNERR_BLOCKZUKURZ;                     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_BLOCKZUKURZ),
    (Code: FTLINTERNERR_BLOCKNRKEINEZAHL;                GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_BLOCKNRKEINEZAHL),
    (Code: FTLINTERNERR_BLOCKFOLGEMARKEUNGUELTIG;        GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_BLOCKFOLGEMARKEUNGUELTIG),
    (Code: FTLINTERNERR_CRCKEINEZAHL;                    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_CRCKEINEZAHL),
    (Code: FTLINTERNERR_CRCFALSCH;                       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_CRCFALSCH),
    (Code: FTLINTERNERR_TESTERGEBNISSCHLECHT;            GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_TESTERGEBNISSCHLECHT),
    (Code: FTLINTERNERR_PARAMETERGESCHRIEBEN;            GasXStatus: 0; { OK }         Code_basic: BASIC_ERR_OK; Klasse: fk_Warnung; Text: S_FTLINTERNERR_PARAMETERGESCHRIEBEN),
    (Code: FTLINTERNERR_BLOCKNRUNBEKANNT;                GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_BLOCKNRUNBEKANNT),
    (Code: FTLINTERNERR_PARAMETERNICHTFLOAT;             GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_PARAMETERNICHTFLOAT),
    (Code: FTLINTERNERR_PARAMETERNICHTINTEGER;           GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_PARAMETERNICHTINTEGER),
    (Code: FTLINTERNERR_PARAMETERNICHTLONGINTEGER;       GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_PARAMETERNICHTLONGINTEGER),
    (Code: FTLINTERNERR_PARAMETERNICHTCHAR;              GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_PARAMETERNICHTCHAR),
    (Code: FTLINTERNERR_PARAMETERAUSSERHALBGRENZWERTE;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_PARAMETERAUSSERHALBGRENZWERTE),
    (Code: FTLINTERNERR_BLOCKGESPERRT;                   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_BLOCKGESPERRT),
    (Code: FTLINTERNERR_BLOCKKANNNICHTGESCHRIEBENWERDEN; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_BLOCKKANNNICHTGESCHRIEBENWERDEN),
    (Code: FTLINTERNERR_FALSCHEBLOCKLAENGE;              GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_FALSCHEBLOCKLAENGE),
    (Code: FTLINTERNERR_BLOCKNRUNDEFINIERT;              GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FTLINTERNERR_BLOCKNRUNDEFINIERT));

  { Fehlermeldungen bei GBH-Revision }

  MaxGBHRevErrorMsgs = 2;

  GBHRevErrorMessage : Array [1..MaxGBHRevErrorMsgs] of TError = (
    (Code: GBHREVERR_KEINE_REV_INSTANZ; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_GBHREVERR_KEINE_REV_INSTANZ),
    (Code: GBHREVERR_PARAM_SOLL_IST;    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_GBHREVERR_PARAM_SOLL_IST));

  { Fehlermeldungen für GPRS-Kommunikation }

  MaxGPRSErrorMsgs = 2;

  GPRSErrorMessage : Array [1..MaxGPRSErrorMsgs] of TError = (
    (Code: GPRSERR_KEINE_VERBINDUNG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_GPRSERR_KEINE_VERBINDUNG),
    (Code: GPRSERR_KENNUNG_MEHRFACH_KEIN_ABRUF; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_GPRSERR_KENNUNG_MEHRFACH_KEIN_ABRUF));

  { Fehlermeldungen für Modbus-Kommunikation }

  MaxModbusErrorMsgs = 23;

  ModbusErrorMessage : Array [1..MaxModbusErrorMsgs] of TError = (
    (Code: MODBUSERR_EXC_ILLEGALFUNCTION;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_ILLEGALFUNCTION),
    (Code: MODBUSERR_EXC_ILLEGALDATAADDRESS;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_ILLEGALDATAADDRESS),
    (Code: MODBUSERR_EXC_ILLEGALDATAVALUE;    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_ILLEGALDATAVALUE),
    (Code: MODBUSERR_EXC_SLAVEDEVICEFAILURE;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_SLAVEDEVICEFAILURE),
    (Code: MODBUSERR_EXC_ACKNOWLEDGE;         GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_ACKNOWLEDGE),
    (Code: MODBUSERR_EXC_SLAVEDEVICEBUSY;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_SLAVEDEVICEBUSY),
    (Code: MODBUSERR_EXC_NEGATIVEACKNOWLEDGE; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_NEGATIVEACKNOWLEDGE),
    (Code: MODBUSERR_EXC_MEMORYPARITYERROR;   GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_MEMORYPARITYERROR),
    (Code: MODBUSERR_EXC_UNDEFINED;           GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_UNDEFINED),
    (Code: MODBUSERR_RESP_INCOMPLETE;         GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RESP_INCOMPLETE),
    (Code: MODBUSERR_RESP_WRONGFUNCTION;      GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RESP_WRONGFUNCTION),
    (Code: MODBUSERR_RESP_WRONGSLAVEADDRESS;  GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RESP_WRONGSLAVEADDRESS),
    (Code: MODBUSERR_RESP_BYTECOUNT_SOLL_IST; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RESP_BYTECOUNT_SOLL_IST),
    (Code: MODBUSERR_RMG_INDEX;               GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RMG_INDEX),
    (Code: MODBUSERR_RMG_EXCEPTION;           GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RMG_EXCEPTION),
    (Code: MODBUSERR_EXC_GATEWAYPATHUNAVAILABLE; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_GATEWAYPATHUNAVAILABLE),
    (Code: MODBUSERR_EXC_GATEWAYTARGETDEVICEFAILEDTORESPOND; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_EXC_GATEWAYTARGETDEVICEFAILEDTORESPOND),
    (Code: MODBUSERR_QUERY_CREATE;            GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_QUERY_CREATE),
    (Code: MODBUSERR_RESP_UNKNOWNFUNCTION;    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RESP_UNKNOWNFUNCTION),
    (Code: MODBUSERR_RESPTCP_WRONGTID;        GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RESPTCP_WRONGTID),
    (Code: MODBUSERR_RESPTCP_WRONGPROTID;     GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RESPTCP_WRONGPROTID),
    (Code: MODBUSERR_RESPTCP_LENGTH_SOLL_IST; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RESPTCP_LENGTH_SOLL_IST),
    (Code: MODBUSERR_RESP_WRONGREGISTEREADDRESS; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MODBUSERR_RESP_WRONGREGISTEREADDRESS));

  { Fehlermeldungen für Elster DS-100-Kommunikation }
  MaxDS100ErrorMsgs = 2;

  DS100ErrorMessage : Array [1..MaxDS100ErrorMsgs] of TError = (
    (Code: DS100ERR_QUITTUNGNEGATIV;    GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_DS100ERR_QUITTUNGNEGATIV),
    (Code: DS100ERR_QUITTUNGUNERWARTET; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Uebertragungsfehler; Klasse: fk_Fehler; Text: S_DS100ERR_QUITTUNGUNERWARTET));

  { Fehlermeldungen für Kommunikation Signaturserver/Client
    -> ab 14.04.2014: Gas-X-Status GASX_ERR_SIGNATURFEHLER }
  MaxSignSrvSysErrorMsgs = 3;

  SignSrvSysErrorMessage : Array [1..MaxSignSrvSysErrorMsgs] of TError = (
    (Code: SIGNSRVSYSERR_ANTWORTNICHTPLAUSIBEL; GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_SIGNSRVSYSERR_ANTWORTNICHTPLAUSIBEL),
    (Code: SIGNSRVSYSERR_ANTWORTUNERWARTET_CMD; GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_SIGNSRVSYSERR_ANTWORTUNERWARTET_CMD),
    (Code: SIGNSRVSYSERR_ANTWORTUNERWARTET_TID; GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_SIGNSRVSYSERR_ANTWORTUNERWARTET_TID));

  { Signaturserver-Fehlermeldungen
    -> ab 14.04.2014: Gas-X-Status GASX_ERR_SIGNATURFEHLER }
  MaxSignaturSrvErrorMsgs = 15;

  SignaturSrvErrorMessage : Array [1..MaxSignaturSrvErrorMsgs] of TError = (
    (Code: SIGNSRVERR_SIGNINVALID;                GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_WARNUNG; Text: S_SIGNSRVERR_SIGNINVALID),
    (Code: SIGNSRVERR_UNKNOWNSIGNALGORITHM;       GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_WARNUNG; Text: S_SIGNSRVERR_UNKNOWNSIGNALGORITHM),
    (Code: SIGNSRVERR_HASHGENERATION;             GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_HASHGENERATION),
    (Code: SIGNSRVERR_SIGN;                       GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_SIGN),
    (Code: SIGNSRVERR_KEYGEN;                     GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_KEYGEN),
    (Code: SIGNSRVERR_UNKNOWNKEY;                 GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_UNKNOWNKEY),
    (Code: SIGNSRVERR_INVALIDHEXFORMAT;           GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_INVALIDHEXFORMAT),
    (Code: SIGNSRVERR_VERIFY;                     GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_VERIFY),
    (Code: SIGNSRVERR_KEYADMINFILEEMPTY;          GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_KEYADMINFILEEMPTY),
    (Code: SIGNSRVERR_LOADKEYADMINFILE;           GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_LOADKEYADMINFILE),
    (Code: SIGNSRVERR_KEYADMINFILENOTFOUND;       GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_KEYADMINFILENOTFOUND),
    (Code: SIGNSRVERR_KEYADMINFILEWRITEPROTECTED; GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_KEYADMINFILEWRITEPROTECTED),
    (Code: SIGNSRVERR_KEYSAVE;                    GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER;  Text: S_SIGNSRVERR_KEYSAVE),

    (Code: SIGNSRVERR_NOTVERIFIED;                GasXStatus: GASX_ERR_SIGNATURFEHLER; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_WARNUNG; Text: S_SIGNSRVERR_NOTVERIFIED),
    (Code: SIGNSRVERR_LICENCE;                    GasXStatus: GASX_ERR_SIGNATURLIZENZ; Code_basic: BASIC_ERR_Lizenz;  Klasse: FK_WARNUNG; Text: S_SIGNSRVERR_LICENCE));

  { Zeitbefehl-Fehlermeldungen }
  MaxZeitbefehlErrorMsgs = 1;

  ZeitbefehlErrorMessage : Array [1..MaxZeitbefehlErrorMsgs] of TError = (
    (Code: ZEITBEFERR_ZEITLESEN; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_ZEITBEFERR_ZEITLESEN));

  { Firmwareupdate-Fehlermeldungen }
  MaxFirmwareUpdateErrorMsgs = 23;

  FirmwareUpdateErrorMessage : Array [1..MaxFirmwareUpdateErrorMsgs] of TError = (
    (Code: FIRMWUPDERR_ADRSTARTGLEICHENDE; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_ADRSTARTGLEICHENDE),
    (Code: FIRMWUPDERR_ADRSTARTZUKLEIN; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_ADRSTARTZUKLEIN),
    (Code: FIRMWUPDERR_ADRSTARTGROESSERENDE; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_ADRSTARTGROESSERENDE),
    (Code: FIRMWUPDERR_UNDEFINIERT; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_UNDEFINIERT),
    (Code: FIRMWUPDERR_ANTWORTUNERWARTET; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_ANTWORTUNERWART),
    (Code: FIRMWUPDERR_ANTWORTUNVOLLSTAENDIG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_ANTWORTUNVOLLSTAENDIG),

    (Code: FIRMWUPDERR_BINDATA_NICHTGEFUNDEN; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_BINDATA_NICHTGEFUNDEN),
    (Code: FIRMWUPDERR_BINDATA_ZUGROSS; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_BINDATA_ZUGROSS),
    (Code: FIRMWUPDERR_BINDATA_STARTADRUNGUELTIG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_BINDATA_STARTADRUNGUELTIG),
    (Code: FIRMWUPDERR_BINDATA_ENDADRUNGUELTIG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_BINDATA_ENDADRUNGUELTIG),
    (Code: FIRMWUPDERR_BINDATA_TYP_FEHLT; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_BINDATA_TYP_FEHLT),
    (Code: FIRMWUPDERR_BINDATA_TYP_FALSCH; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_BINDATA_TYP_FALSCH),
    (Code: FIRMWUPDERR_BINDATA_BUILD_FEHLT; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_BINDATA_BUILD_FEHLT),
    (Code: FIRMWUPDERR_BINDATA_BUILD_UNGUELTIG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_FIRMWUPDERR_BINDATA_BUILD_UNGUELTIG),
    (Code: FIRMWUPDERR_BINDATA_BUILD_ALT; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_WARNUNG; Text: S_FIRMWUPDERR_BINDATA_BUILD_ALT),

    (Code: FIRMWUPDERR_BINDATA_TRANSFER_NAK; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_FIRMWUPDERR_BINDATA_TRANSFER_NAK),
    (Code: FIRMWUPDERR_BINDATA_TRANSFER_CAN; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_FIRMWUPDERR_BINDATA_TRANSFER_CAN),
    (Code: FIRMWUPDERR_BINDATA_TRANSFER_ENQ; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_FIRMWUPDERR_BINDATA_TRANSFER_ENQ),
    (Code: FIRMWUPDERR_BINDATA_TRANSFER_UNDEF; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_FIRMWUPDERR_BINDATA_TRANSFER_UNDEF),
    (Code: FIRMWUPDERR_BINDATA_TRANSFER_X; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_FIRMWUPDERR_BINDATA_TRANSFER_X),

    (Code: FIRMWUPDERR_FLASH_ERASE; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_FIRMWUPDERR_FLASH_ERASE),
    (Code: FIRMWUPDERR_FLASH_PROG; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_FIRMWUPDERR_FLASH_PROG),
    (Code: FIRMWUPDERR_FLASH_UNDEF; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: FK_FEHLER; Text: S_FIRMWUPDERR_FLASH_UNDEF));

  { Fehlermeldungen bei Geräteüberprüfung }
  MaxGeraeteCheckErrorMsgs = 2;

  GeraeteCheckErrorMessage : Array [1..MaxGeraeteCheckErrorMsgs] of TError = (
    (Code: GERCHECKERR_TYP_FALSCH; GasXStatus: 0; { OK } Code_basic: BASIC_ERR_OK; Klasse: fk_Warnung; Text: S_GERCHECKERR_TYP_FALSCH),
    (Code: GERCHECKERR_ARCH_ZEILENWEISE_LESBAR; GasXStatus: 0; { OK } Code_basic: BASIC_ERR_OK; Klasse: fk_Warnung; Text: S_GERCHECKERR_ARCH_ZEILENWEISE_LESBAR));

  { Fehlermeldungen bei DSfG-Transparentschaltung }
  MaxDSfGTransparentErrorMsgs = 1;

  DSfGTransparentErrorMessage : Array [1..MaxDSfGTransparentErrorMsgs] of TError = (
    (Code: DSFGTRANSPERR_DFUEBUSADDRESS_KEINLOKALERTEILNEHMER; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_DSFGTRANSPERR_DFUEBUSADDRESS_KEINLOKALERTEILNEHMER));

  { Fehlermeldungen beim Lesen von Messwerten }
  MaxMWLesenErrorMsgs = 1;

  MWLesenErrorMessage : Array [1..MaxMWLesenErrorMsgs] of TError = (
    (Code: MWLESENERR_ZUGRIFFSRECHT; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MWLESENERR_ZUGRIFFSRECHT));

  { Fehlermeldungen beim Lesen von Meldungen }
  MaxMELesenErrorMsgs = 1;

  MELesenErrorMessage : Array [1..MaxMELesenErrorMsgs] of TError = (
    (Code: MELESENERR_ZUGRIFFSRECHT; GasXStatus: GASX_ERR_SONSTIG; Code_basic: BASIC_ERR_Sonstig; Klasse: fk_Fehler; Text: S_MELESENERR_ZUGRIFFSRECHT));


{--------------------------------------------------------------------------------------------}


{---------------------------------------------------------------------}
Function GetSysAbrfErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxSysAbrfErrorMsgs Do
  Begin
    If SysAbrfErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := SysAbrfErrorMessage [i].GasXStatus;
      Fehlercode_basic := SysAbrfErrorMessage [i].Code_basic;
      Klasse := SysAbrfErrorMessage [i].Klasse;
      Result := SysAbrfErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetCOMPortErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxCOMPortErrorMsgs Do
  Begin
    If COMPortErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := COMPortErrorMessage [i].GasXStatus;
      Fehlercode_basic := COMPortErrorMessage [i].Code_basic;
      Klasse := COMPortErrorMessage [i].Klasse;
      Result := COMPortErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetComErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  if Error >= ERR_OFFSET_GETLASTERROR then begin  // Auswertung 'GetLastError'; 03.06.2013, WW
    GasXStatus := GASX_ERR_UEBERTRAGUNGSFEHLER;
    Fehlercode_basic := BASIC_ERR_Uebertragungsfehler;
    Klasse := fk_Fehler;
    Result := SysErrorMessage (Error - ERR_OFFSET_GETLASTERROR);
  end
  else begin
    For i := 1 to MaxComErrorMsgs Do
    Begin
      If (ComErrorMessage [i].Code AND Error) <> 0 Then
      Begin
        GasXStatus := ComErrorMessage [i].GasXStatus;
        Fehlercode_basic := ComErrorMessage [i].Code_basic;
        Klasse := ComErrorMessage [i].Klasse;
        Result := ComErrorMessage [i].Text;
        Break;
      End;
    End;
  end;
End;

{---------------------------------------------------------------------}
Function GetKommErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxKommErrorMsgs Do
  Begin
    If KommErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := KommErrorMessage [i].GasXStatus;
      Fehlercode_basic := KommErrorMessage [i].Code_basic;
      Klasse := KommErrorMessage [i].Klasse;
      Result := KommErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetDeviceInitErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxDeviceInitErrorMsgs Do
  Begin
    If DeviceInitErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := DeviceInitErrorMessage [i].GasXStatus;
      Fehlercode_basic := DeviceInitErrorMessage [i].Code_basic;
      Klasse := DeviceInitErrorMessage [i].Klasse;
      Result := DeviceInitErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetLicenceErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxLicenceErrorMsgs Do
  Begin
    If LicenceErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := LicenceErrorMessage [i].GasXStatus;
      Fehlercode_basic := LicenceErrorMessage [i].Code_basic;
      Klasse := LicenceErrorMessage [i].Klasse;
      Result := LicenceErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetFupErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxFupErrorMsgs Do
  Begin
    If FupErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := FupErrorMessage [i].GasXStatus;
      Fehlercode_basic := FupErrorMessage [i].Code_basic;
      Klasse := FupErrorMessage [i].Klasse;
      Result := FupErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetMRGErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxMRGErrorMsgs Do
  Begin
    If MrgErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := MrgErrorMessage [i].GasXStatus;
      Fehlercode_basic := MrgErrorMessage [i].Code_basic;
      Klasse := MrgErrorMessage [i].Klasse;
      Result := MrgErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetModemErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxModemErrorMsgs Do
  Begin
    If ModemErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := ModemErrorMessage [i].GasXStatus;
      Fehlercode_basic := ModemErrorMessage [i].Code_basic;
      Klasse := ModemErrorMessage [i].Klasse;
      Result := ModemErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetModemProtErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxModemProtErrorMsgs Do
  Begin
    If ModemProtErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := ModemProtErrorMessage [i].GasXStatus;
      Fehlercode_basic := ModemProtErrorMessage [i].Code_basic;
      Klasse := ModemProtErrorMessage [i].Klasse;
      Result := ModemProtErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetTCPIPErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxTCPIPErrorMsgs Do
  Begin
    If TCPIPErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := TCPIPErrorMessage [i].GasXStatus;
      Fehlercode_basic := TCPIPErrorMessage [i].Code_basic;
      Klasse := TCPIPErrorMessage [i].Klasse;
      Result := TCPIPErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetSrvErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxSrvErrorMsgs Do
  Begin
    If SrvErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := SrvErrorMessage [i].GasXStatus;
      Fehlercode_basic := SrvErrorMessage [i].Code_basic;
      Klasse := SrvErrorMessage [i].Klasse;
      Result := SrvErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
function GetLAKSErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
var
  i: integer;

Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxLAKSErrorMsgs Do
  Begin
    If LAKSErrorMessage[i].Code = Error Then
    Begin
      GasXStatus := LAKSErrorMessage [i].GasXStatus;
      Fehlercode_basic := LAKSErrorMessage [i].Code_basic;
      Klasse := LAKSErrorMessage [i].Klasse;
      Result := LAKSErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
function GetDSfGDFUErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
var
  i: integer;

Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxDSfGDFUErrorMsgs Do
  Begin
    If DSfGDFUErrorMessage[i].Code = Error Then
    Begin
      GasXStatus := DSfGDFUErrorMessage [i].GasXStatus;
      Fehlercode_basic := DSfGDFUErrorMessage [i].Code_basic;
      Klasse := DSfGDFUErrorMessage [i].Klasse;
      Result := DSfGDFUErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetStammdatenErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxStammdatenErrorMsgs Do
  Begin
    If StammdatenErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := StammdatenErrorMessage [i].GasXStatus;
      Fehlercode_basic := StammdatenErrorMessage [i].Code_basic;
      Klasse := StammdatenErrorMessage [i].Klasse;
      Result := StammdatenErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetKonfigdatenErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxKonfigdatenErrorMsgs Do
  Begin
    If KonfigdatenErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := KonfigdatenErrorMessage [i].GasXStatus;
      Fehlercode_basic := KonfigdatenErrorMessage [i].Code_basic;
      Klasse := KonfigdatenErrorMessage [i].Klasse;
      Result := KonfigdatenErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetKonvErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxKonvErrorMsgs Do
  Begin
    If KonvErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := KonvErrorMessage [i].GasXStatus;
      Fehlercode_basic := KonvErrorMessage [i].Code_basic;
      Klasse := KonvErrorMessage [i].Klasse;
      Result := KonvErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetDSfGErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxDSfGErrorMsgs Do
  Begin
    If DSfGErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := DSfGErrorMessage [i].GasXStatus;
      Fehlercode_basic := DSfGErrorMessage [i].Code_basic;
      Klasse := DSfGErrorMessage [i].Klasse;
      Result := DSfGErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetDSfGUmlErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxDSfGUmlErrorMsgs Do
  Begin
    If DSfGUmlErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := DSfGUmlErrorMessage [i].GasXStatus;
      Fehlercode_basic := DSfGUmlErrorMessage [i].Code_basic;
      Klasse := DSfGUmlErrorMessage [i].Klasse;
      Result := DSfGUmlErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetZSyncErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxZSyncErrorMsgs Do
  Begin
    If ZSyncErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := ZSyncErrorMessage [i].GasXStatus;
      Fehlercode_basic := ZSyncErrorMessage [i].Code_basic;
      Klasse := ZSyncErrorMessage [i].Klasse;
      Result := ZSyncErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetRPResetErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxRPResetErrorMsgs Do
  Begin
    If RPResetErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := RPResetErrorMessage [i].GasXStatus;
      Fehlercode_basic := RPResetErrorMessage [i].Code_basic;
      Klasse := RPResetErrorMessage [i].Klasse;
      Result := RPResetErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetDataCheckText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxDataCheckMsgs Do
  Begin
    If DataCheckMessage [i].Code = Error Then
    Begin
      GasXStatus := DataCheckMessage [i].GasXStatus;
      Fehlercode_basic := DataCheckMessage [i].Code_basic;
      Klasse := DataCheckMessage [i].Klasse;
      Result := DataCheckMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetRufDeaktErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxRufDeaktErrorMsgs Do
  Begin
    If RufDeaktErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := RufDeaktErrorMessage [i].GasXStatus;
      Fehlercode_basic := RufDeaktErrorMessage [i].Code_basic;
      Klasse := RufDeaktErrorMessage [i].Klasse;
      Result := RufDeaktErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetRufReaktErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxRufReaktErrorMsgs Do
  Begin
    If RufReaktErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := RufReaktErrorMessage [i].GasXStatus;
      Fehlercode_basic := RufReaktErrorMessage [i].Code_basic;
      Klasse := RufReaktErrorMessage [i].Klasse;
      Result := RufReaktErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetParamErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxParamErrorMsgs Do
  Begin
    If ParamErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := ParamErrorMessage [i].GasXStatus;
      Fehlercode_basic := ParamErrorMessage [i].Code_basic;
      Klasse := ParamErrorMessage [i].Klasse;
      Result := ParamErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetRueckRufErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxRueckRufErrorMsgs Do
  Begin
    If RueckRufErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := RueckRufErrorMessage [i].GasXStatus;
      Fehlercode_basic := RueckRufErrorMessage [i].Code_basic;
      Klasse := RueckRufErrorMessage [i].Klasse;
      Result := RueckRufErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetRueckRufPruefText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxRueckRufPruefMsgs Do
  Begin
    If RueckRufPruefMessage [i].Code = Error Then
    Begin
      GasXStatus := RueckRufPruefMessage [i].GasXStatus;
      Fehlercode_basic := RueckRufPruefMessage [i].Code_basic;
      Klasse := RueckRufPruefMessage [i].Klasse;
      Result := RueckRufPruefMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetLoginErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxLoginErrorMsgs Do
  Begin
    If LoginErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := LoginErrorMessage [i].GasXStatus;
      Fehlercode_basic := LoginErrorMessage [i].Code_basic;
      Klasse := LoginErrorMessage [i].Klasse;
      Result := LoginErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetKennErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxKennErrorMsgs Do
  Begin
    If KennErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := KennErrorMessage [i].GasXStatus;
      Fehlercode_basic := KennErrorMessage [i].Code_basic;
      Klasse := KennErrorMessage [i].Klasse;
      Result := KennErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetGBHRevisionErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxGBHRevErrorMsgs Do
  Begin
    If GBHRevErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := GBHRevErrorMessage [i].GasXStatus;
      Fehlercode_basic := GBHRevErrorMessage [i].Code_basic;
      Klasse := GBHRevErrorMessage [i].Klasse;
      Result := GBHRevErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetDataErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxDataErrorMsgs Do
  Begin
    If DataErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := DataErrorMessage [i].GasXStatus;
      Fehlercode_basic := DataErrorMessage [i].Code_basic;
      Klasse := DataErrorMessage [i].Klasse;
      Result := DataErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetFileErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxFileErrorMsgs Do
  Begin
    If FileErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := FileErrorMessage [i].GasXStatus;
      Fehlercode_basic := FileErrorMessage [i].Code_basic;
      Klasse := FileErrorMessage [i].Klasse;
      Result := FileErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetPDAErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxPDAErrorMsgs Do
  Begin
    If PDAErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := PDAErrorMessage [i].GasXStatus;
      Fehlercode_basic := PDAErrorMessage [i].Code_basic;
      Klasse := PDAErrorMessage [i].Klasse;
      Result := PDAErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetKEErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxKEErrorMsgs Do
  Begin
    If KEErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := KEErrorMessage [i].GasXStatus;
      Fehlercode_basic := KEErrorMessage [i].Code_basic;
      Klasse := KEErrorMessage [i].Klasse;
      Result := KEErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetPalmUSBErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxPalmUSBErrorMsgs Do
  Begin
    If PalmUSBErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := PalmUSBErrorMessage [i].GasXStatus;
      Fehlercode_basic := PalmUSBErrorMessage [i].Code_basic;
      Klasse := PalmUSBErrorMessage [i].Klasse;
      Result := PalmUSBErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetUserActionText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxUserActionMsgs Do
  Begin
    If UserActionMessage [i].Code = Error Then
    Begin
      GasXStatus := UserActionMessage [i].GasXStatus;
      Fehlercode_basic := UserActionMessage [i].Code_basic;
      Klasse := UserActionMessage [i].Klasse;
      Result := UserActionMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetFTLErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxFTLErrorMsgs Do
  Begin
    If FTLErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := FTLErrorMessage [i].GasXStatus;
      Fehlercode_basic := FTLErrorMessage [i].Code_basic;
      Klasse := FTLErrorMessage [i].Klasse;
      Result := FTLErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetFTLInternErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxFTLInternErrorMsgs Do
  Begin
    If FTLInternErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := FTLInternErrorMessage [i].GasXStatus;
      Fehlercode_basic := FTLInternErrorMessage [i].Code_basic;
      Klasse := FTLInternErrorMessage [i].Klasse;
      Result := FTLInternErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetGPRSErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxGPRSErrorMsgs Do
  Begin
    If GPRSErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := GPRSErrorMessage [i].GasXStatus;
      Fehlercode_basic := GPRSErrorMessage [i].Code_basic;
      Klasse := GPRSErrorMessage [i].Klasse;
      Result := GPRSErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetModbusErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxModbusErrorMsgs Do
  Begin
    If ModbusErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := ModbusErrorMessage [i].GasXStatus;
      Fehlercode_basic := ModbusErrorMessage [i].Code_basic;
      Klasse := ModbusErrorMessage [i].Klasse;
      Result := ModbusErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetDS100ErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxDS100ErrorMsgs Do
  Begin
    If DS100ErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := DS100ErrorMessage [i].GasXStatus;
      Fehlercode_basic := DS100ErrorMessage [i].Code_basic;
      Klasse := DS100ErrorMessage [i].Klasse;
      Result := DS100ErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetSignSrvSysErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxSignSrvSysErrorMsgs Do
  Begin
    If SignSrvSysErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := SignSrvSysErrorMessage [i].GasXStatus;
      Fehlercode_basic := SignSrvSysErrorMessage [i].Code_basic;
      Klasse := SignSrvSysErrorMessage [i].Klasse;
      Result := SignSrvSysErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetSignaturSrvErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxSignaturSrvErrorMsgs Do
  Begin
    If SignaturSrvErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := SignaturSrvErrorMessage [i].GasXStatus;
      Fehlercode_basic := SignaturSrvErrorMessage [i].Code_basic;
      Klasse := SignaturSrvErrorMessage [i].Klasse;
      Result := SignaturSrvErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{---------------------------------------------------------------------}
Function GetZeitbefehlErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{---------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxZeitbefehlErrorMsgs Do
  Begin
    If ZeitbefehlErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := ZeitbefehlErrorMessage [i].GasXStatus;
      Fehlercode_basic := ZeitbefehlErrorMessage [i].Code_basic;
      Klasse := ZeitbefehlErrorMessage [i].Klasse;
      Result := ZeitbefehlErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{-----------------------------------------------------------------------}
Function GetFirmwareUpdateErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{-----------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxFirmwareUpdateErrorMsgs Do
  Begin
    If FirmwareUpdateErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := FirmwareUpdateErrorMessage [i].GasXStatus;
      Fehlercode_basic := FirmwareUpdateErrorMessage [i].Code_basic;
      Klasse := FirmwareUpdateErrorMessage [i].Klasse;
      Result := FirmwareUpdateErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{-----------------------------------------------------------------------}
Function GetGeraeteCheckErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{-----------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxGeraeteCheckErrorMsgs Do
  Begin
    If GeraeteCheckErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := GeraeteCheckErrorMessage [i].GasXStatus;
      Fehlercode_basic := GeraeteCheckErrorMessage [i].Code_basic;
      Klasse := GeraeteCheckErrorMessage [i].Klasse;
      Result := GeraeteCheckErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{-----------------------------------------------------------------------}
Function GetDSfGTransparentErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{-----------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxDSfGTransparentErrorMsgs Do
  Begin
    If DSfGTransparentErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := DSfGTransparentErrorMessage [i].GasXStatus;
      Fehlercode_basic := DSfGTransparentErrorMessage [i].Code_basic;
      Klasse := DSfGTransparentErrorMessage [i].Klasse;
      Result := DSfGTransparentErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{-----------------------------------------------------------------------}
Function GetMWLesenErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{-----------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxMWLesenErrorMsgs Do
  Begin
    If MWLesenErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := MWLesenErrorMessage [i].GasXStatus;
      Fehlercode_basic := MWLesenErrorMessage [i].Code_basic;
      Klasse := MWLesenErrorMessage [i].Klasse;
      Result := MWLesenErrorMessage [i].Text;
      Break;
    End;
  End;
End;

{-----------------------------------------------------------------------}
Function GetMELesenErrorText (Error: integer; var Klasse: integer;
  var GasXStatus: integer; var Fehlercode_basic: integer): shortstring;
{-----------------------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedError;
  Klasse := UndocumentedKlasse;
  GasXStatus := GASX_ERR_SONSTIG;
  Fehlercode_basic:= BASIC_ERR_Sonstig;

  For i := 1 to MaxMELesenErrorMsgs Do
  Begin
    If MELesenErrorMessage [i].Code = Error Then
    Begin
      GasXStatus := MELesenErrorMessage [i].GasXStatus;
      Fehlercode_basic := MELesenErrorMessage [i].Code_basic;
      Klasse := MELesenErrorMessage [i].Klasse;
      Result := MELesenErrorMessage [i].Text;
      Break;
    End;
  End;
End;


{------------------------------------------------------------------------------}

{----------------------------------------------------}
Function GetStatusText (Status: integer): shortstring;
{----------------------------------------------------}
Var
  i : Integer;
  S: integer;
Begin
  Result := S_UndocumentedStatus;

  S:=Get_StatusHigh (Status);  // es interessiert nur der Statuscode der höchsten Prioritätsstufe
  For i := 1 to MaxStatusMsgs Do
  Begin
    If StatusMessage [i].Code = S Then
    Begin
      Result := StatusMessage [i].Text;
      Break;
    End;
  End;
End;

{----------------------------------------------------------}
Function GetErrorText (Status, Error: integer): shortstring;
{----------------------------------------------------------}
var
  Dummy: integer;
  S: integer;
Begin
  Result := S_UndocumentedError;

  S:=Get_StatusLow (Status);  // es interessiert nur der Statuscode der niedrigsten Prioritätsstufe
  Case S of
    SYS_ABRUFERROR      : Result := GetSysAbrfErrorText (Error, Dummy, Dummy, Dummy);
    COM_PORTERROR       : Result := GetCOMPortErrorText (Error, Dummy, Dummy, Dummy);
    COM_ERROR           : Result := GetComErrorText (Error, Dummy, Dummy, Dummy);
    COM_KOMMERROR       : Result := GetKommErrorText (Error, Dummy, Dummy, Dummy);
    COM_DEVICEINITERROR : Result := GetDeviceInitErrorText (Error, Dummy, Dummy, Dummy);
    SYS_LICENCEERROR    : Result := GetLicenceErrorText (Error, Dummy, Dummy, Dummy);
    COM_FUPERROR        : Result := GetFupErrorText (Error, Dummy, Dummy, Dummy);
    COM_MRGERROR        : Result := GetMRGErrorText (Error, Dummy, Dummy, Dummy);
    COM_MODEMERROR      : Result := GetModemErrorText (Error, Dummy, Dummy, Dummy);
    COM_MODEMPROTERROR  : Result := GetModemProtErrorText (Error, Dummy, Dummy, Dummy);
    COM_TCPIP_ERROR     : Result := GetTCPIPErrorText (Error, Dummy, Dummy, Dummy);
    COM_SRV_ERROR,
    COM_SIGNSRV_ERROR   : Result := GetSrvErrorText (Error, Dummy, Dummy, Dummy);
    COM_LAKS            : Result := GetLAKSErrorText (Error, Dummy, Dummy, Dummy);
    COM_DSFGDFUERROR    : Result := GetDSFGDFUErrorText (Error, Dummy, Dummy, Dummy);
    ST_STAMMERROR       : Result := GetStammDatenErrorText (Error, Dummy, Dummy, Dummy);
    ST_RUECKRUFPRUEF    : Result := GetRueckRufPruefText (Error, Dummy, Dummy, Dummy);
    ST_KONVERROR        : Result := GetKonvErrorText (Error, Dummy, Dummy, Dummy);
    ST_DSFGERROR        : Result := GetDSfGErrorText (Error, Dummy, Dummy, Dummy);
    ST_DSFGUMLERROR     : Result := GetDSfGUmlErrorText (Error, Dummy, Dummy, Dummy);
    ST_DATACHECK        : Result := GetDataCheckText (Error, Dummy, Dummy, Dummy);
    ST_RUFREAKTERROR    : Result := GetRufReaktErrorText (Error, Dummy, Dummy, Dummy);
    ST_RUECKRUFERROR    : Result := GetRueckRufErrorText (Error, Dummy, Dummy, Dummy);
    ST_DATAERROR        : Result := GetDataErrorText (Error, Dummy, Dummy, Dummy);
    ST_KONFIGERROR      : Result := GetKonfigdatenErrorText (Error, Dummy, Dummy, Dummy);
    ST_FILEERROR        : Result := GetFileErrorText (Error, Dummy, Dummy, Dummy);
    EST_ZEITSYNCERROR   : Result := GetZSyncErrorText (Error, Dummy, Dummy, Dummy);
    EST_RPRESET_MEERROR,
    EST_RPRESET_MWERROR,
    EST_RPRESET_PRERROR : Result := GetRPResetErrorText (Error, Dummy, Dummy, Dummy);
    EST_RUFDEAKTERROR   : Result := GetRufDeaktErrorText (Error, Dummy, Dummy, Dummy);
    EST_PARAMERROR      : Result := GetParamErrorText (Error, Dummy, Dummy, Dummy);
    EST_LOGINERROR      : Result := GetLoginErrorText (Error, Dummy, Dummy, Dummy);
    EST_KENNUNGCHECK    : Result := GetKennErrorText (Error, Dummy, Dummy, Dummy);
    COM_PDAERROR        : Result := GetPDAErrorText (Error, Dummy, Dummy, Dummy);
    COM_KE_ERROR        : Result := GetKEErrorText (Error, Dummy, Dummy, Dummy);
    COM_PALMUSBERROR    : Result := GetPalmUSBErrorText (Error, Dummy, Dummy, Dummy);
    ACT_USER            : Result := GetUserActionText (Error, Dummy, Dummy, Dummy);
    COM_FTL_ERROR       : Result := GetFTLErrorText (Error, Dummy, Dummy, Dummy);
    EST_3_GBHREVISION   : Result := GetGBHRevisionErrorText (Error, Dummy, Dummy, Dummy);
    COM_GPRS_ERROR      : Result := GetGPRSErrorText (Error, Dummy, Dummy, Dummy);
    COM_MODBUSERROR     : Result := GetModbusErrorText (Error, Dummy, Dummy, Dummy);
    COM_DS100_ERROR     : Result := GetDS100ErrorText (Error, Dummy, Dummy, Dummy);
    SYS_SIGNSRVSYSERROR : Result := GetSignSrvSysErrorText (Error, Dummy, Dummy, Dummy);
    SYS_SIGNSRV_ERROR   : Result := GetSignaturSrvErrorText (Error, Dummy, Dummy, Dummy);
    EST_ZEITBEFEHLERROR : Result := GetZeitbefehlErrorText (Error, Dummy, Dummy, Dummy);
    EST_FIRMWAREUPDATEERROR: Result := GetFirmwareUpdateErrorText (Error, Dummy, Dummy, Dummy);
    EST_GERAETECHECK    : Result := GetGeraeteCheckErrorText (Error, Dummy, Dummy, Dummy);
    EST_DSFGTRANSPARENTERROR : Result := GetDSfGTransparentErrorText (Error, Dummy, Dummy, Dummy);
    EST_MWLESENERROR    : Result := GetMWLesenErrorText (Error, Dummy, Dummy, Dummy);
    EST_MELESENERROR    : Result := GetMELesenErrorText (Error, Dummy, Dummy, Dummy);
    COM_FTLIntern_ERROR : Result := GetFTLInternErrorText (Error, Dummy, Dummy, Dummy);
  End;
End;

{----------------------------------------------------------}
Function GetFupErrorcode (FupMeldung: shortstring): integer;
{----------------------------------------------------------}
{ liefert Fehlercode zu einer FUP-Fehlermeldung 'F?';
  -> gültig für FUP-1200 und FUP-9600 }
Begin
  Result:=UndocumentedError;
  if length (FupMeldung) >= 2 then begin
    case FupMeldung [2] of
      '0': Result:=FUPERR_F0;
      '1': Result:=FUPERR_F1;
      '2': Result:=FUPERR_F2;
      '3': Result:=FUPERR_F3;
      '4': Result:=FUPERR_F4;
      '5': Result:=FUPERR_F5;
      '6': Result:=FUPERR_F6;
      '7': Result:=FUPERR_F7;
      '8': Result:=FUPERR_F8;
      '9': Result:=FUPERR_F9;
      'A': Result:=FUPERR_FA;
      'B': Result:=FUPERR_FB;
      'C': Result:=FUPERR_FC;
      'D': Result:=FUPERR_FD;
      'E': Result:=FUPERR_FE;
      'F': Result:=FUPERR_FF;
      'G': Result:=FUPERR_FG;
      'H': Result:=FUPERR_FH;
      'I': Result:=FUPERR_FI;
      'M': Result:=FUPERR_FM;
      'N': Result:=FUPERR_FN;
      'S': Result:=FUPERR_FS;
      'T': Result:=FUPERR_FT;
      'U': Result:=FUPERR_FU;
      'Z': Result:=FUPERR_FZ;
    end;
  end;
End;

{----------------------------------------------------}
function GetKlasseText (Klasse: integer): shortstring;
{----------------------------------------------------}
var
  i : integer;
Begin
  Result := S_UndocumentedKlasse;
  For i := 1 to MaxKlasseMsgs Do
  Begin
    If KlasseMessage [i].Code = Klasse Then
    Begin
      Result := KlasseMessage [i].Text;
      Break;
    End;
  End;
End;

{--------------------------------------------------------}
Function GetErrorKlasse (Status, Error: integer): integer;
{--------------------------------------------------------}
var
  S: integer;
  Dummy: integer;
Begin
  Result := UndocumentedKlasse;

  S:=Get_StatusLow (Status);  // es interessiert nur der Statuscode der niedrigsten Prioritätsstufe
  Case S of
    SYS_ABRUFERROR      : GetSysAbrfErrorText (Error, Result, Dummy, Dummy);
    COM_PORTERROR       : GetCOMPortErrorText (Error, Result, Dummy, Dummy);
    COM_ERROR           : GetComErrorText (Error, Result, Dummy, Dummy);
    COM_KOMMERROR       : GetKommErrorText (Error, Result, Dummy, Dummy);
    COM_DEVICEINITERROR : GetDeviceInitErrorText (Error, Result, Dummy, Dummy);
    SYS_LICENCEERROR    : GetLicenceErrorText (Error, Result, Dummy, Dummy);
    COM_FUPERROR        : GetFupErrortext (Error, Result, Dummy, Dummy);
    COM_MRGERROR        : GetMRGErrorText (Error, Result, Dummy, Dummy);
    COM_MODEMERROR      : GetModemErrorText (Error, Result, Dummy, Dummy);
    COM_MODEMPROTERROR  : GetModemProtErrorText (Error, Result, Dummy, Dummy);
    COM_TCPIP_ERROR     : GetTCPIPErrorText (Error, Result, Dummy, Dummy);
    COM_SRV_ERROR,
    COM_SIGNSRV_ERROR   : GetSrvErrorText (Error, Result, Dummy, Dummy);
    COM_LAKS            : GetLAKSErrorText (Error, Result, Dummy, Dummy);
    COM_DSFGDFUERROR    : GetDSFGDFUErrorText (Error, Result, Dummy, Dummy);
    ST_STAMMERROR       : GetStammDatenErrorText (Error, Result, Dummy, Dummy);
    ST_RUECKRUFPRUEF    : GetRueckRufPruefText (Error, Result, Dummy, Dummy);
    ST_KONVERROR        : GetKonvErrorText (Error, Result, Dummy, Dummy);
    ST_DSFGERROR        : GetDSfGErrorText (Error, Result, Dummy, Dummy);
    ST_DSFGUMLERROR     : GetDSfGUmlErrorText (Error, Result, Dummy, Dummy);
    ST_DATACHECK        : GetDataCheckText (Error, Result, Dummy, Dummy);
    ST_RUFREAKTERROR    : GetRufReaktErrorText (Error, Result, Dummy, Dummy);
    ST_RUECKRUFERROR    : GetRueckRufErrorText (Error, Result, Dummy, Dummy);
    ST_DATAERROR        : GetDataErrorText (Error, Result, Dummy, Dummy);
    ST_KONFIGERROR      : GetKonfigdatenErrorText (Error, Result, Dummy, Dummy);
    ST_FILEERROR        : GetFileErrorText (Error, Result, Dummy, Dummy);
    EST_ZEITSYNCERROR   : GetZSyncErrorText (Error, Result, Dummy, Dummy);
    EST_RPRESET_MEERROR,
    EST_RPRESET_MWERROR,
    EST_RPRESET_PRERROR : GetRPResetErrorText (Error, Result, Dummy, Dummy);
    EST_RUFDEAKTERROR   : GetRufDeaktErrorText (Error, Result, Dummy, Dummy);
    EST_PARAMERROR      : GetParamErrorText (Error, Result, Dummy, Dummy);
    EST_LOGINERROR      : GetLoginErrorText (Error, Result, Dummy, Dummy);
    EST_KENNUNGCHECK    : GetKennErrorText (Error, Result, Dummy, Dummy);
    COM_PDAERROR        : GetPDAErrorText (Error, Result, Dummy, Dummy);
    COM_KE_ERROR        : GetKEErrorText (Error, Result, Dummy, Dummy);
    COM_PALMUSBERROR    : GetPalmUSBErrorText (Error, Result, Dummy, Dummy);
    ACT_USER            : GetUserActionText (Error, Result, Dummy, Dummy);
    COM_FTL_ERROR       : GetFTLErrorText (Error, Result, Dummy, Dummy);
    EST_3_GBHREVISION   : GetGBHRevisionErrorText (Error, Result, Dummy, Dummy);
    COM_GPRS_ERROR      : GetGPRSErrorText (Error, Result, Dummy, Dummy);
    COM_MODBUSERROR     : GetModbusErrorText (Error, Result, Dummy, Dummy);
    SYS_SIGNSRVSYSERROR : GetSignSrvSysErrorText (Error, Result, Dummy, Dummy);
    SYS_SIGNSRV_ERROR   : GetSignaturSrvErrorText (Error, Result, Dummy, Dummy);
    EST_ZEITBEFEHLERROR : GetZeitbefehlErrorText (Error, Result, Dummy, Dummy);
    EST_FIRMWAREUPDATEERROR: GetFirmwareUpdateErrorText (Error, Result, Dummy, Dummy);
    EST_GERAETECHECK    : GetGeraeteCheckErrorText (Error, Result, Dummy, Dummy);
    EST_DSFGTRANSPARENTERROR: GetDSfGTransparentErrorText (Error, Result, Dummy, Dummy);
    EST_MWLESENERROR    : GetMWLesenErrorText (Error, Result, Dummy, Dummy);
    EST_MELESENERROR    : GetMELesenErrorText (Error, Result, Dummy, Dummy);
    COM_FTLIntern_ERROR : GetFTLInternErrorText (Error, Result, Dummy, Dummy);
  End;
End;

{-----------------------------------------------------------------------}
Function GetGasXStatus (Status, Error: integer; Kommando: char): integer;
{-----------------------------------------------------------------------}
var
  S: integer;
  Dummy: integer;
  GasXStatus: integer;

Begin
  GasXStatus := GASX_ERR_SONSTIG;

  S:=Get_StatusLow (Status);  // es interessiert nur der Statuscode der niedrigsten Prioritätsstufe
  { Ermittlung des internen Gas-X-Fehlercodes: }
  Case S of
    SYS_ABRUFERROR      : GetSysAbrfErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_PORTERROR       : GetCOMPortErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_ERROR           : GetComErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_KOMMERROR       : GetKommErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_DEVICEINITERROR : GetDeviceInitErrorText (Error, Dummy, GasXStatus, Dummy);
    SYS_LICENCEERROR    : GetLicenceErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_FUPERROR        : GetFupErrortext (Error, Dummy, GasXStatus, Dummy);
    COM_MRGERROR        : GetMRGErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_MODEMERROR      : GetModemErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_MODEMPROTERROR  : GetModemProtErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_TCPIP_ERROR     : GetTCPIPErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_SRV_ERROR       : GetSrvErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_SIGNSRV_ERROR   : GasXStatus:=GASX_ERR_SIGNATURFEHLER;  // 14.04.2014, WW
    COM_LAKS            : GetLAKSErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_DSFGDFUERROR    : GetDSFGDFUErrorText (Error, Dummy, GasXStatus, Dummy);
    ST_STAMMERROR       : GetStammDatenErrorText (Error, Dummy, GasXStatus, Dummy);
    ST_RUECKRUFPRUEF    : GetRueckRufPruefText (Error, Dummy, GasXStatus, Dummy);
    ST_KONVERROR        : GetKonvErrorText (Error, Dummy, GasXStatus, Dummy);
    ST_DSFGERROR        : GetDSfGErrorText (Error, Dummy, GasXStatus, Dummy);
    ST_DSFGUMLERROR     : GetDSfGUmlErrorText (Error, Dummy, GasXStatus, Dummy);
    ST_DATACHECK        : GetDataCheckText (Error, Dummy, GasXStatus, Dummy);
    ST_RUFREAKTERROR    : GetRufReaktErrorText (Error, Dummy, GasXStatus, Dummy);
    ST_RUECKRUFERROR    : GetRueckRufErrorText (Error, Dummy, GasXStatus, Dummy);
    ST_DATAERROR        : GetDataErrorText (Error, Dummy, GasXStatus, Dummy);
    ST_KONFIGERROR      : GetKonfigdatenErrorText (Error, Dummy, GasXStatus, Dummy);
    ST_FILEERROR        : GetFileErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_ZEITSYNCERROR   : GetZSyncErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_RPRESET_MEERROR,
    EST_RPRESET_MWERROR,
    EST_RPRESET_PRERROR : GetRPResetErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_RUFDEAKTERROR   : GetRufDeaktErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_PARAMERROR      : GetParamErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_LOGINERROR      : GetLoginErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_KENNUNGCHECK    : GetKennErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_PDAERROR        : GetPDAErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_KE_ERROR        : GetKEErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_PALMUSBERROR    : GetPalmUSBErrorText (Error, Dummy, GasXStatus, Dummy);
    ACT_USER            : GetUserActionText (Error, Dummy, GasXStatus, Dummy);
    COM_FTL_ERROR       : GetFTLErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_3_GBHREVISION   : GetGBHRevisionErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_GPRS_ERROR      : GetGPRSErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_MODBUSERROR     : GetModbusErrorText (Error, Dummy, GasXStatus, Dummy);
    SYS_SIGNSRVSYSERROR : GetSignSrvSysErrorText (Error, Dummy, GasXStatus, Dummy);
    SYS_SIGNSRV_ERROR   : GetSignaturSrvErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_ZEITBEFEHLERROR : GetZeitbefehlErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_FIRMWAREUPDATEERROR: GetFirmwareUpdateErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_GERAETECHECK    : GetGeraeteCheckErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_DSFGTRANSPARENTERROR: GetDSfGTransparentErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_MWLESENERROR    : GetMWLesenErrorText (Error, Dummy, GasXStatus, Dummy);
    EST_MELESENERROR    : GetMELesenErrorText (Error, Dummy, GasXStatus, Dummy);
    COM_FTLIntern_ERROR : GetFTLInternErrorText (Error, Dummy, GasXStatus, Dummy);
  end;

  { Gas-X-Fehlerstatus:
    besondere Behandlung von 'Verbindung unterbrochen' beim Verbindungsauf-/abbau: 08.07.2003, WW
    nochmalige Korrektur: 31.10.2003, WW; 13.01.2006 WW }
  if ((Kommando = 'v') OR (Kommando = 'e')) AND
     (GasXStatus = GASX_ERR_VERB_UNTERBROCHEN) then
    GasXStatus:=GASX_ERR_SONSTIG;    { 'Sonstigen Fehler' zuordnen, sonst Fehler-Rückgabe 'COM nicht frei',
                                        da gleiche Gas-X-Fehlercodenummer }
  Result:=GasXStatus;
End;

{------------------------------------------------------------}
function GetErrorcode_basic (Status, Error: integer): integer;
{------------------------------------------------------------}
var
  S: integer;
  Dummy: integer;
  Fehlercode_basic: integer;
  FC_basic_buf: integer;

begin
  Fehlercode_basic := BASIC_ERR_Sonstig;


  { Prüfung der Fehlergruppe 'Zeitsynchronisation': }
  S:=Get_StatusHigh (Status);  // Statuscode der höchsten Prioritätsstufe
  if S = EST_ZEITSYNCERROR then begin  // Fehler bei Zeitsynchronisation
    Fehlercode_basic:=BASIC_ERR_Fehler_ZeitSynch;

    { Prüfung auf 'Zeitsynchronisation OK': }
    S:=Get_StatusLow (Status);  // es interessiert nur der Statuscode der niedrigsten Prioritätsstufe
    if S = EST_ZEITSYNCERROR then begin
      GetZSyncErrorText (Error, Dummy, Dummy, FC_basic_buf);
      if FC_basic_buf = BASIC_ERR_OK then
        Fehlercode_basic:=BASIC_ERR_OK;
    end;
  end

  else begin
    S:=Get_StatusLow (Status);  // es interessiert nur der Statuscode der niedrigsten Prioritätsstufe
    { Ermittlung des internen Gas-X-Fehlercodes: }
    Case S of
      SYS_ABRUFERROR      : GetSysAbrfErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_PORTERROR       : GetCOMPortErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_ERROR           : GetComErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_KOMMERROR       : GetKommErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_DEVICEINITERROR : GetDeviceInitErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      SYS_LICENCEERROR    : GetLicenceErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_FUPERROR        : GetFupErrortext (Error, Dummy, Dummy, Fehlercode_basic);
      COM_MRGERROR        : GetMRGErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_MODEMERROR      : GetModemErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_MODEMPROTERROR  : GetModemProtErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_TCPIP_ERROR     : GetTCPIPErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_SRV_ERROR,
      COM_SIGNSRV_ERROR   : GetSrvErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_LAKS            : GetLAKSErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_DSFGDFUERROR    : GetDSFGDFUErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_STAMMERROR       : GetStammDatenErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_RUECKRUFPRUEF    : GetRueckRufPruefText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_KONVERROR        : GetKonvErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_DSFGERROR        : GetDSfGErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_DSFGUMLERROR     : GetDSfGUmlErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_DATACHECK        : GetDataCheckText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_RUFREAKTERROR    : GetRufReaktErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_RUECKRUFERROR    : GetRueckRufErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_DATAERROR        : GetDataErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_KONFIGERROR      : GetKonfigdatenErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ST_FILEERROR        : GetFileErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_ZEITSYNCERROR   : GetZSyncErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_RPRESET_MEERROR,
      EST_RPRESET_MWERROR,
      EST_RPRESET_PRERROR : GetRPResetErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_RUFDEAKTERROR   : GetRufDeaktErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_PARAMERROR      : GetParamErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_LOGINERROR      : GetLoginErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_KENNUNGCHECK    : GetKennErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_PDAERROR        : GetPDAErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_KE_ERROR        : GetKEErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_PALMUSBERROR    : GetPalmUSBErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      ACT_USER            : GetUserActionText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_FTL_ERROR       : GetFTLErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_3_GBHREVISION   : GetGBHRevisionErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_GPRS_ERROR      : GetGPRSErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_MODBUSERROR     : GetModbusErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      SYS_SIGNSRVSYSERROR : GetSignSrvSysErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      SYS_SIGNSRV_ERROR   : GetSignaturSrvErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_ZEITBEFEHLERROR : GetZeitbefehlErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_FIRMWAREUPDATEERROR: GetFirmwareUpdateErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_GERAETECHECK    : GetGeraeteCheckErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_DSFGTRANSPARENTERROR: GetDSfGTransparentErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_MWLESENERROR    : GetMWLesenErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      EST_MELESENERROR    : GetMELesenErrorText (Error, Dummy, Dummy, Fehlercode_basic);
      COM_FTLIntern_ERROR : GetFTLInternErrorText (Error, Dummy, Dummy, Fehlercode_basic);
    end;
  end;

  Result:=Fehlercode_basic;
end;


{------------------------------------------------------------------------------}

  { Fehlertexte für DSfG-Journal-Detailtabelle }

  { Fehler/Warnungen beim Konvertieren von DSfG-Daten }

Const
  MaxDSfGKonvErrorMsgs = 26;

  DSfGKonvErrorMessage : Array [0..MaxDSfGKonvErrorMsgs] of TError = (
    (Code: DSFGKONVERR_OK;                Klasse: -1; Text: S_DSFGKONVERR_OK),
    (Code: DSFGKONVERR_HEADER;            Klasse: -1; Text: S_DSFGKONVERR_HEADER),
    (Code: DSFGKONVERR_DATA;              Klasse: -1; Text: S_DSFGKONVERR_DATA),
    (Code: DSFGKONVERR_FILEACCESS;        Klasse: -1; Text: S_DSFGKONVERR_FILEACCESS),
    (Code: DSFGKONVERR_INDEX;             Klasse: -1; Text: S_DSFGKONVERR_INDEX),
    (Code: DSFGKONVERR_NODATA;            Klasse: -1; Text: S_DSFGKONVERR_NODATA),
    (Code: DSFGKONVERR_KONFDATA;          Klasse: -1; Text: S_DSFGKONVERR_KONFDATA),
    (Code: DSFGKONVERR_UNKNOWNLBSOURCE;   Klasse: -1; Text: S_DSFGKONVERR_UNKNOWNLBSOURCE),
    (Code: DSFGKONVERR_ZAE_SOLL_IST;      Klasse: -1; Text: S_DSFGKONVERR_ZAE_SOLL_IST),
    (Code: DSFGKONVERR_FILENOTFOUND;      Klasse: -1; Text: S_DSFGKONVERR_FILENOTFOUND),

    (Code: DSFGKONVERR_INVVALUE;          Klasse: -1; Text: S_DSFGKONVERR_INVVALUE),
    (Code: DSFGKONVERR_INVTIME;           Klasse: -1; Text: S_DSFGKONVERR_INVTIME),
    (Code: DSFGKONVERR_INVNUMBER;         Klasse: -1; Text: S_DSFGKONVERR_INVNUMBER),
    (Code: DSFGKONVERR_INVADDR;           Klasse: -1; Text: S_DSFGKONVERR_INVADDR),
    (Code: DSFGKONVERR_INVSTATE;          Klasse: -1; Text: S_DSFGKONVERR_INVSTATE),
    (Code: DSFGKONVERR_INVCRC;            Klasse: -1; Text: S_DSFGKONVERR_INVCRC),
    (Code: DSFGKONVERR_EMPTYREC;          Klasse: -1; Text: S_DSFGKONVERR_EMPTYREC),

    (Code: DSFGKONVERR_DATACHECK_INTERN;  Klasse: -1; Text: S_DSFGKONVERR_DATACHECK_INTERN),
    (Code: DSFGKONVERR_DATAKONV_INTERN;   Klasse: -1; Text: S_DSFGKONVERR_DATAKONV_INTERN),

    (Code: DSFGKONVERR_SIGVERIFY_INVALID;     Klasse: -1; Text: S_DSFGKONVERR_SIGVERIFY_INVALID),
    (Code: DSFGKONVERR_SIGVERIFY_NOTVERIFIED; Klasse: -1; Text: S_DSFGKONVERR_SIGVERIFY_NOTVERIFIED),

    (Code: DSFGKONVERR_AA_NICHT_PLAUSIBEL;    Klasse: -1; Text: S_DSFGKONVERR_AA_NICHT_PLAUSIBEL),
    (Code: DSFGKONVERR_AA_BESTAETIGUNG;       Klasse: -1; Text: S_DSFGKONVERR_AA_BESTAETIGUNG),
    (Code: DSFGKONVERR_AA_KEINE_BERECHTIGUNG; Klasse: -1; Text: S_DSFGKONVERR_AA_KEINE_BERECHTIGUNG),
    (Code: DSFGKONVERR_AA_UNBEKANNT;          Klasse: -1; Text: S_DSFGKONVERR_AA_UNBEKANNT),
    (Code: DSFGKONVERR_AA_NICHT_BEHAND;       Klasse: -1; Text: S_DSFGKONVERR_AA_NICHT_BEHAND),
    (Code: DSFGKONVERR_AA_SONSTIGE;           Klasse: -1; Text: S_DSFGKONVERR_AA_SONSTIGE));


{-----------------------------------------------------------}
Function GetDSfGKonvErrorText (Status: integer): shortstring;
{-----------------------------------------------------------}
Var
  i : Integer;
Begin
  Result := S_UndocumentedDSfGKonvStatus;
  For i := 0 to MaxDSfGKonvErrorMsgs Do
  Begin
    If DSfGKonvErrorMessage [i].Code = Status Then
    Begin
      Result := DSfGKonvErrorMessage [i].Text;
      Break;
    End;
  End;
End;


{-------------- Hilfsfunktionen, von DLL nicht veröffentlicht -----------------}

{--------------------------------------------------------------------}
function ExportErrtxt_WicomSrv_GasX(sExportFilename: string): integer;
{--------------------------------------------------------------------}
{ Fehlercodeliste mit Texten für WicomSrv (Gas-X-Version) als CSV-Datei exportieren;
  -> Es werden nur die Statusgruppen exportiert, welche im WicomSrv Anwendung finden.
     Dabei werden (der Einfachheit halber) ALLE der jeweiligen Statusgruppe
     zugehörigen Statuscodes exportiert (damit auch einige Statuscodes, welche
     im WicomSrv nicht Anwendung finden).
  Übergabe: Exportdateiname
  Ergebnis: Anzahl der exportierten Fehlercodes }
const
  C_Trenner = ';';

var
  slCSV: TStringList;
  i: integer;
  sCSV: string;
  iStatus: integer;


begin
  slCSV:=TStringList.Create;
  try
    sCSV:='Statusgruppe' + C_Trenner + 'Statuscode' + C_Trenner +
      'Statusgruppe-Bedeutung' + C_Trenner + 'Statuscode-Bedeutung';
    slCSV.Add(sCSV);

    { OK-Fall }
    sCSV:=IntToStr(0) + C_Trenner + IntToStr(0) + C_Trenner +
      '' + C_Trenner + 'Erfolgreich';
    slCSV.Add(sCSV);  

    { Allgemeine Statuscodes, Prioritätsstufe 1: }

    iStatus:=SYS_ABRUFERROR;
    for i := Low(SysAbrfErrorMessage) to High(SysAbrfErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(SysAbrfErrorMessage [i].Code) + C_Trenner +
        S_SYS_ABRUFERROR + C_Trenner +
        SysAbrfErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_PORTERROR;
    for i := Low(ComPortErrorMessage) to High(ComPortErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(ComPortErrorMessage [i].Code) + C_Trenner +
        S_COM_PORTERROR + C_Trenner +
        ComPortErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_ERROR;
    for i := Low(ComErrorMessage) to High(ComErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(ComErrorMessage [i].Code) + C_Trenner +
        S_COM_ERROR + C_Trenner +
        ComErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_KOMMERROR;
    for i := Low(KommErrorMessage) to High(KommErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(KommErrorMessage [i].Code) + C_Trenner +
        S_COM_KOMMERROR + C_Trenner +
        KommErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_DEVICEINITERROR;
    for i := Low(DeviceInitErrorMessage) to High(DeviceInitErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(DeviceInitErrorMessage [i].Code) + C_Trenner +
        S_COM_DEVICEINITERROR + C_Trenner +
        DeviceInitErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=SYS_LICENCEERROR;
    for i := Low(LicenceErrorMessage) to High(LicenceErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(LicenceErrorMessage [i].Code) + C_Trenner +
        S_SYS_LICENCEERROR + C_Trenner +
        LicenceErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_FUPERROR;
    for i := Low(FupErrorMessage) to High(FupErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(FupErrorMessage [i].Code) + C_Trenner +
        S_COM_FUPERROR + C_Trenner +
        FupErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_MRGERROR;
    for i := Low(MrgErrorMessage) to High(MrgErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(MrgErrorMessage [i].Code) + C_Trenner +
        S_COM_MRGERROR + C_Trenner +
        MrgErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_MODEMERROR;
    for i := Low(ModemErrorMessage) to High(ModemErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(ModemErrorMessage [i].Code) + C_Trenner +
        S_COM_MODEMERROR + C_Trenner +
        ModemErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_MODEMPROTERROR;
    for i := Low(ModemProtErrorMessage) to High(ModemProtErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(ModemProtErrorMessage [i].Code) + C_Trenner +
        S_COM_MODEMPROTERROR + C_Trenner +
        ModemProtErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_TCPIP_ERROR;
    for i := Low(TCPIPErrorMessage) to High(TCPIPErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(TCPIPErrorMessage [i].Code) + C_Trenner +
        S_COM_TCPIP_ERROR + C_Trenner +
        TCPIPErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    // Im WicomSrv nicht verwendet: COM_SRV_ERROR, COM_PALMUSBERROR, COM_LAKS

    iStatus:=COM_DSFGDFUERROR;
    for i := Low(DSfGDfuErrorMessage) to High(DSfGDfuErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(DSfGDfuErrorMessage [i].Code) + C_Trenner +
        S_COM_DSFGDFUERROR + C_Trenner +
        DSfGDfuErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    // Im WicomSrv nicht verwendet: COM_PDAERROR

    iStatus:=COM_KE_ERROR;
    for i := Low(KEErrorMessage) to High(KEErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(KEErrorMessage [i].Code) + C_Trenner +
        S_COM_KE_ERROR + C_Trenner +
        KEErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_FTL_ERROR;
    for i := Low(FTLErrorMessage) to High(FTLErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(FTLErrorMessage [i].Code) + C_Trenner +
        S_COM_FTL_ERROR + C_Trenner +
        FTLErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    // Im WicomSrv nicht verwendet: COM_GPRS_ERROR

    // 23.10.2019, WW: Modbus-Fehler (Primus 400, Prilog 400)
    iStatus:=COM_MODBUSERROR;
    for i := Low(ModbusErrorMessage) to High(ModbusErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(ModbusErrorMessage [i].Code) + C_Trenner +
        S_COM_MODBUSERROR + C_Trenner +
        ModbusErrorMessage [i].Text;               
      slCSV.Add(sCSV);
    end;

    // In der Gas-X-Version des WicomSrv nicht verwendet: COM_DS100_ERROR

    iStatus:=SYS_SIGNSRVSYSERROR;
    for i := Low(SignSrvSysErrorMessage) to High(SignSrvSysErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(SignSrvSysErrorMessage [i].Code) + C_Trenner +
        S_SYS_SIGNSRVSYSERROR + C_Trenner +
        SignSrvSysErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_SIGNSRV_ERROR;
    for i := Low(SrvErrorMessage) to High(SrvErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(SrvErrorMessage [i].Code) + C_Trenner +
        S_COM_SIGNSRV_ERROR + C_Trenner +
        SrvErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=SYS_SIGNSRV_ERROR;
    for i := Low(SignaturSrvErrorMessage) to High(SignaturSrvErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(SignaturSrvErrorMessage [i].Code) + C_Trenner +
        S_SYS_SIGNSRV_ERROR + C_Trenner +
        SignaturSrvErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=COM_FTLIntern_ERROR;
    for i := Low(FTLInternErrorMessage) to High(FTLInternErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(FTLInternErrorMessage [i].Code) + C_Trenner +
        S_COM_FTLIntern_ERROR + C_Trenner +
        FTLInternErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    // Im WicomSrv nicht verwendet: ST_STAMMERROR, ST_RUECKRUFPRUEF

    iStatus:=ST_KONVERROR;
    for i := Low(KonvErrorMessage) to High(KonvErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(KonvErrorMessage [i].Code) + C_Trenner +
        S_ST_KONVERROR + C_Trenner +
        KonvErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=ST_DSFGERROR;
    for i := Low(DSfGErrorMessage) to High(DSfGErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(DSfGErrorMessage [i].Code) + C_Trenner +
        S_ST_DSFGERROR + C_Trenner +
        DSfGErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=ST_DSFGUMLERROR;
    for i := Low(DSfGUmlErrorMessage) to High(DSfGUmlErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(DSfGUmlErrorMessage [i].Code) + C_Trenner +
        S_ST_DSFGUMLERROR + C_Trenner +
        DSfGUmlErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=ST_DATACHECK;
    for i := Low(DataCheckMessage) to High(DataCheckMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(DataCheckMessage [i].Code) + C_Trenner +
        S_ST_DATACHECK + C_Trenner +
        DataCheckMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    // Im WicomSrv nicht verwendet: ST_RUFREAKTERROR

    iStatus:=ST_RUECKRUFERROR;
    for i := Low(RueckRufErrorMessage) to High(RueckRufErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(RueckRufErrorMessage [i].Code) + C_Trenner +
        S_ST_RUECKRUFERROR + C_Trenner +
        RueckRufErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    // Im WicomSrv nicht verwendet: ST_DATAERROR

    iStatus:=ST_KONFIGERROR;
    for i := Low(KonfigdatenErrorMessage) to High(KonfigdatenErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(KonfigdatenErrorMessage [i].Code) + C_Trenner +
        S_ST_KONFIGERROR + C_Trenner +
        KonfigdatenErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=ST_FILEERROR;
    for i := Low(FileErrorMessage) to High(FileErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(FileErrorMessage [i].Code) + C_Trenner +
        S_ST_FILEERROR + C_Trenner +
        FileErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    // Im WicomSrv nicht verwendet: ACT_USER


  { erweiterte Statuscodes, Prioritätsstufe 2 }

    iStatus:=EST_ZEITSYNCERROR;
    for i := Low(ZSyncErrorMessage) to High(ZSyncErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(ZSyncErrorMessage [i].Code) + C_Trenner +
        S_EST_ZEITSYNCERROR + C_Trenner +
        ZSyncErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=EST_RPRESET_MEERROR;
    for i := Low(RPResetErrorMessage) to High(RPResetErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(RPResetErrorMessage [i].Code) + C_Trenner +
        S_EST_RPRESET_MEERROR + C_Trenner +
        RPResetErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=EST_RPRESET_MWERROR;
    for i := Low(RPResetErrorMessage) to High(RPResetErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(RPResetErrorMessage [i].Code) + C_Trenner +
        S_EST_RPRESET_MWERROR + C_Trenner +
        RPResetErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=EST_RPRESET_PRERROR;
    for i := Low(RPResetErrorMessage) to High(RPResetErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(RPResetErrorMessage [i].Code) + C_Trenner +
        S_EST_RPRESET_PRERROR + C_Trenner +
        RPResetErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    // Im WicomSrv nicht verwendet: EST_RUFDEAKTERROR

    iStatus:=EST_PARAMERROR;
    for i := Low(ParamErrorMessage) to High(ParamErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(ParamErrorMessage [i].Code) + C_Trenner +
        S_EST_PARAMERROR + C_Trenner +
        ParamErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=EST_LOGINERROR;
    for i := Low(LoginErrorMessage) to High(LoginErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(LoginErrorMessage [i].Code) + C_Trenner +
        S_EST_LOGINERROR + C_Trenner +
        LoginErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=EST_KENNUNGCHECK;
    for i := Low(KennErrorMessage) to High(KennErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(KennErrorMessage [i].Code) + C_Trenner +
        S_EST_KENNUNGCHECK + C_Trenner +
        KennErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=EST_ZEITBEFEHLERROR;
    for i := Low(ZeitbefehlErrorMessage) to High(ZeitbefehlErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(ZeitbefehlErrorMessage [i].Code) + C_Trenner +
        S_EST_ZEITBEFEHLERROR + C_Trenner +
        ZeitbefehlErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    // In der Gas-X-Version des WicomSrv nicht verwendet: EST_FIRMWAREUPDATEERROR

    iStatus:=EST_GERAETECHECK;
    for i := Low(GeraeteCheckErrorMessage) to High(GeraeteCheckErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(GeraeteCheckErrorMessage [i].Code) + C_Trenner +
        S_EST_GERAETECHECK + C_Trenner +
        GeraeteCheckErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=EST_DSFGTRANSPARENTERROR;
    for i := Low(DSfGTransparentErrorMessage) to High(DSfGTransparentErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(DSfGTransparentErrorMessage [i].Code) + C_Trenner +
        S_EST_DSFGTRANSPARENTERROR + C_Trenner +
        DSfGTransparentErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=EST_MWLESENERROR;
    for i := Low(MWLesenErrorMessage) to High(MWLesenErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(MWLesenErrorMessage [i].Code) + C_Trenner +
        S_EST_MWLESENERROR + C_Trenner +
        MWLesenErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    iStatus:=EST_MELESENERROR;
    for i := Low(MELesenErrorMessage) to High(MELesenErrorMessage) do begin
      sCSV:=IntToStr(iStatus) + C_Trenner +
        IntToStr(MELesenErrorMessage [i].Code) + C_Trenner +
        S_EST_MELESENERROR + C_Trenner +
        MELesenErrorMessage [i].Text;
      slCSV.Add(sCSV);
    end;

    slCSV.SaveToFile(sExportFilename);
    Result:=slCSV.Count;
  finally
    slCSV.Free;
  end;
end;  

end.
