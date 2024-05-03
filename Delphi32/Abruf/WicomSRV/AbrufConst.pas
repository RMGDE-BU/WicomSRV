{******************************************************************************}
{* Unit: Typen und Konstanten für Abrufserver                                 *}
{* 23.12.2002 WW                                                              *}
(* 19.04.2013 GD/WW  Simulationsmodus					                                *)
{******************************************************************************}
Unit AbrufConst;

INTERFACE

uses
  Messages, syncobjs, AbrufTimeoutConst;

const
{$IFDEF SIMU}     // 19.04.2013
  C_WicomSrv_DSfGSimuFileArchive = 'WicomSrv_DSfGArchSimu.DAT';  // Datei mit Simulationsdaten für DSfG-Archiv
  C_WicomSrv_DSfGSimuFileLogbook = 'WicomSrv_DSfGLogbSimu.DAT';  // Datei mit Simulationsdaten für DSfG-Logbuch

  C_TestFlag : boolean = True;  // Flag für Testbetrieb (keine Geräte-Kommunikation)
{$ELSE}
  C_TestFlag : boolean = False;  // Flag für Produktivbetrieb (mit Geräte-Kommunikation)
{$ENDIF}

  { aktuelle Version Abrufserver }
  CVersion_WicomSrv = '4.6.8' {$IFDEF GAS-X} + ' (GAS-X)' {$ENDIF}
             {$IFDEF NO_XML_SIGVERIFYSTATE} + ' (ohne XML-Signaturstatus)' {$ENDIF}
                              {$IFDEF SIMU} + ' (Simulation)' {$ENDIF};

  CTagesende = 6;   { Standard-Wert }

  { Windows-Botschaften }
  WM_TASKBAREVENT         = WM_USER +  1;

  WM_AbrufThreadDone      = WM_User +  8;
  WM_SendWieserMsg_NKD    = WM_User +  9;
  WM_OpenGPRSServer       = WM_User + 10;
  WM_CloseGPRSServer      = WM_User + 11;
  WM_REClntThreadDone     = WM_User + 12;

  { Digitale Signatur }
  CPubKey_DoNotVerify = '-1';  // Default-Wert für öffentl. Signaturschlüssel:
                               // Signaturen sollen nicht verifiziert werden.
type
  { Programmversions-Typ }
  TVersionTyp = (vs_GasX, vs_Wieser);

  { Record für Timeouts bei MRG-Abruf }
  TMRGTimeouts = record
    FupAntwort       : integer;
    FupReset         : integer;
    ModemAntwort     : integer;
    ModemInit        : integer;
    GSMModem         : integer;
    CRCCheck         : integer;
    ACK01_ProtMeldung: integer;

    Verbindungsaufbau: integer;
    Verbindungsabbau : integer;
    RufAnnahmeModem  : integer;
    Kennung          : integer;
    Login            : integer;
    Parameter        : integer;
    Meldungen        : integer;
    Messwerte        : integer;
    Tagessaetze      : integer;
    Pruefsaetze      : integer;
    Binaerdatei      : integer;
    RundpufferReset  : integer;
    Parametrieren    : integer;
    Rufausloesung    : integer;
    DSfGUmschaltung  : integer;
    DSfGRufliste     : integer;
    DSfGRufQuittung  : integer;
    IEC1107Telegr    : integer;
    TritschlerIECProt: integer;
    TritschlerFTLProt: integer;
    CorusSAMProt     : integer;
    ElsterDS100Prot  : integer;
    ModbusProt       : integer;
  end;

  { Record für Timeouts bei DSfG-Abruf }
  TDSfGTimeouts = record
    ModemAntwort     : integer;
    ModemInit        : integer;
    GSMModem         : integer;
    Verbindungsaufbau: integer;
    Verbindungsabbau : integer;
    RufAnnahme       : integer;
    Login            : integer;
    DFUETransparent  : integer;
    Archive          : integer;
    Logbuecher       : integer;
    Datenelemente    : integer;
    Einstellen       : integer;
    DFUEParameter    : integer;
    Binaerdatei      : integer;
  end;

{$IFDEF SIMU}     // 14.01.2015
  { Record für simulierte Kommunikationszeiten bei DSfG-Abruf }
  TSimuDSfGKommunikationZeiten = record
    Verbindungsaufbau: integer;
    AntwortTelegramm: integer;
  end;
{$ENDIF}

const
  { Logfile-Fehlertexte (nur deutsch) }
  CMsgSrvStarted          = 'Dienst ist gestartet';
  CMsgSrvStopped          = 'Dienst ist beendet';

  CMsgServerInactive      = 'Abruf-Server ist nicht aktiv:';
  CMsgGPRSServerInactive  = 'GPRS-Server ist nicht aktiv:';
  CMsgREServerInactive    = 'Rufentgegennahme-Server inaktiv:';

//  CMsgErrtxt32DLLNotFound = 'Datei ERRTXT32.DLL wurde nicht gefunden.';

  CMsgIPAdrPortOpened     = '%s:%d geöffnet, Abruf-Server ist aktiv';
  CMsgIPAdrPortClosed     = '%s:%d geschlossen, Abruf-Server ist beendet';
  CMsgREDSfG_IPAdrPortOpened = '%s:%d geöffnet, DSfG-Rufentgegennahme-Server ist aktiv';
  CMsgREDSfG_IPAdrPortClosed = '%s:%d geschlossen, DSfG-Rufentgegennahme-Server ist beendet';

  CMsgEmptyGPRSDirName    = 'Die INI-Konfiguration enthält leeren GPRS-Ausgabe-Verzeichnisnamen !';
  CMsgMultipleGPRSDirName = 'In der INI-Konfiguration der GPRS-Ausgabe-Verzeichnisse sind gleiche Verzeichnisnamen mehrfach vorhanden !';
  CMsgUndefGPRSDirErr     = 'Undefinierter Fehler in der INI-Konfiguration der GPRS-Ausgabe-Verzeichnisse !';
  CMsgErrDirCreate        = 'Verzeichnis %s kann nicht erstellt werden !';

  CMsgErrWriteHeartbeatFile = 'Heartbeat-Datei konnte nicht geschrieben werden: %s';

  CMsgErrDeleteOldTempFile    = 'Veraltete Temp-Datei konnte nicht gelöscht werden: %s';
  CMsgCancelDeleteOldTempFile = 'Löschen veralteter Temp-Dateien wurde abgebrochen: %s';

  CMsgErrREClntThreadIndexNotFound = 'Index in IP-Anrufverbindungsliste nicht gefunden: %d';

  CMsgSrvCfgIniChanged_Reread = 'Datum der Datei SrvCfg32.ini hat sich geändert, Einstellungen neu lesen';
  CMsgLicenceChanged_Reread = 'Datum der Lizenzdatei hat sich geändert, Lizenz neu lesen';

  CMsgErrReadResourceFile = 'Ressourcedatei wurde nicht gefunden, konnte nicht gelesen werden oder enthält unplausible Daten: %s';

resourcestring
  { Fehlertexte für Fensterausgabe }
  SMsgServerInactive = CMsgServerInactive;

  SMsgIPAdrPortOpened = CMsgIPAdrPortOpened;
  SMsgIPAdrPortClosed = CMsgIPAdrPortClosed;

  SMsgEmptyGPRSDirName    = CMsgEmptyGPRSDirName;
  SMsgMultipleGPRSDirName = CMsgMultipleGPRSDirName;
  SMsgUndefGPRSDirErr     = CMsgUndefGPRSDirErr;
  SMsgErrDirCreate        = CMsgErrDirCreate;


procedure Init_DSfGTimeouts_Default (var DSfGTimeouts: TDSfGTimeouts);

IMPLEMENTATION

{--------------------------------------------------------------------}
procedure Init_DSfGTimeouts_Default (var DSfGTimeouts: TDSfGTimeouts);
{--------------------------------------------------------------------}
{ Liefert Record mit Standard-Timeouts für DSfG-Abruf }
begin
  with DSfGTimeouts do begin
    ModemAntwort:=CTimeout_ModemAntwort;
    ModemInit:=CTimeout_ModemInit;
    GSMModem:=CTimeout_GSMModem;
    Verbindungsaufbau:=CDSfG_Timeout_Verbindungsaufbau;
    Verbindungsabbau:=CDSfG_Timeout_Verbindungsabbau;
    RufAnnahme:=CDSfG_Timeout_RufAnnahme;
    Login:=CDSfG_Timeout_Login;
    DFUETransparent:=CDSfG_Timeout_DFUETransparent;
    Archive:=CDSfG_Timeout_Archive;
    Logbuecher:=CDSfG_Timeout_Logbuecher;
    Datenelemente:=CDSfG_Timeout_Datenelemente;
    Einstellen:=CDSfG_Timeout_Einstellen;
    DFUEParameter:=CDSfG_Timeout_DFUEParameter;
    Binaerdatei:=CDSfG_Timeout_Binaerdatei;
  end;  { with }
end;

end.
