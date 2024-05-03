{**************************************************************************************}
{* Unit: Klassen-, Status- und Errorcodes für ERRTXT32.DLL                            *}
{* 13.01.1999 WW                                                                      *}
{**************************************************************************************}
Unit ErrConst;

INTERFACE                           

Const

  { Klassencodes }

  FK_WARNUNG = 1;
  FK_FEHLER  = 2;

{------------------------------------------------------------------------------}

  { Allgemeine Statuscodes, Prioritätsstufe 1 (Bereich: 1..999): }

  SYS_ABRUFERROR       =  1;   { interne Abrufsystem-Fehler }
  COM_PORTERROR        =  2;   { COM-Port-Zugriffsfehler }
  COM_ERROR            =  3;   { allgemeiner Schnittstellenfehler }
  COM_KOMMERROR        =  4;   { Kommunikationsfehler }
  COM_DEVICEINITERROR  =  5;   { Geräte-Initialisierungsfehler }
  SYS_LICENCEERROR     =  6;   { Lizenz-Meldung }
  COM_FUPERROR         =  7;   { FUP-Fehlermeldung }
  COM_MRGERROR         =  8;   { MRG-Fehlercode erhalten }
  COM_MODEMERROR       =  9;   { Modemfehlermeldung }
  COM_MODEMPROTERROR   = 10;   { Modemprotokoll-Fehlermeldung }
  COM_TCPIP_ERROR      = 11;   { TCP/IP-Fehlermeldung }
  COM_SRV_ERROR        = 12;   { Kommunikationfehler Abrufserver/Client }
  COM_PALMUSBERROR     = 13;   { Palm-USB-Kommunikationsfehler }
  COM_LAKS             = 14;   { LAKS-Fehlermeldung }
  COM_DSFGDFUERROR     = 15;   { DSfG-DFÜ-Fehlermeldung }
  COM_PDAERROR         = 16;   { PDA-Kommunikationsfehler }
  COM_KE_ERROR         = 17;   { KE-Anlagen-Kommunikationsfehler }
  COM_FTL_ERROR        = 18;   { Tritschler FTL-Protokollfehler }
  COM_GPRS_ERROR       = 19;   { GPRS-Fehler }
  COM_MODBUSERROR      = 20;   { Modbus-Fehler/Exception }
  COM_DS100_ERROR      = 21;   { Elster DS-100-Protokollfehler }
  SYS_SIGNSRVSYSERROR  = 22;   { interne Signaturserversystem-Fehler }
  COM_SIGNSRV_ERROR    = 23;   { Kommunikationsfehler Signaturserver/Client }
  SYS_SIGNSRV_ERROR    = 24;   { Signaturserver-Fehlermeldung }
  COM_FTLINTERN_ERROR  = 25;   { Tritschler Fehler internes FTL-Protokoll }

  ST_STAMMERROR    = 101; { Stammdaten-Fehler }
  ST_RUECKRUFPRUEF = 102; { Rückrufprüfung }
  ST_KONVERROR     = 103; { Datenkonvertierung }
  ST_DSFGERROR     = 105; { Fehler bei Kommunikation mit DSfG-DFÜ }
  ST_DSFGUMLERROR  = 107; { Fehler bei DSfG-Umleitung }
  ST_DATACHECK     = 108; { Datenüberprüfung }
  ST_RUFREAKTERROR = 109; { Fehler bei Rufreaktivierung }
  ST_RUECKRUFERROR = 110; { Fehler bei Rückrufauslösung }
  ST_DATAERROR     = 111; { Fehler beim Zugriff auf Bewegungsdaten }
  ST_KONFIGERROR   = 112; { Fehler beim Zugriff auf Konfigurationsdaten }
  ST_FILEERROR     = 113; { allgemeiner Zugriffsfehler auf Dateien }

  ACT_USER         = 200; { Aktion durch Anwender }

  { erweiterte Statuscodes, Prioritätsstufe 2 (Offset zu allgemeinen Statuscodes;
    Bereich: 1000..99000 in 1000er-Schritten) }

  EST_ZEITSYNCERROR        =  1000;  { Fehler bei Zeitsynchronisation }
  EST_RPRESET_MEERROR      =  2000;  { Fehler bei Rundpufferreset für Meldungen }
  EST_RPRESET_MWERROR      =  3000;  { Fehler bei Rundpufferreset für Meßwerte }
  EST_RPRESET_PRERROR      =  4000;  { Fehler bei Rundpufferreset für Prüfungssätze }
  EST_RUFDEAKTERROR        =  5000;  { Fehler bei Rufdeaktivierung }
  EST_PARAMERROR           =  6000;  { Fehler bei Parametrierung }
  EST_LOGINERROR           =  7000;  { Fehler beim Login }
  EST_KENNUNGCHECK         =  8000;  { Kennungsüberprüfung }
  EST_ZEITBEFEHLERROR      =  9000;  { Zeit-Befehl }
  EST_FIRMWAREUPDATEERROR  = 10000;  { Fehler bei Firmware-Update }
  EST_GERAETECHECK         = 11000;  { Geräteüberprüfung }
  EST_DSFGTRANSPARENTERROR = 12000;  { Fehler bei DSfG-Transparentschaltung }
  EST_MWLESENERROR         = 13000;  { Fehler beim Lesen von Messwerten }
  EST_MELESENERROR         = 14000;  { Fehler beim Lesen von Meldungen }

  { erweiterte Statuscodes, Prioritätsstufe 3 (Offset zu allgemeinen Statuscodes
    und/oder erweiterten Statuscodes der Prioritätsstufe 2;
    Bereich: >= 100000 in 100000er-Schritten) }
  EST_3_GBHREVISION = 100000;  { GBH-Revision (AKA II) }

{------------------------------------------------------------------------------}

  { Errorcodes }

  ERR_OFFSET_GETLASTERROR = 10000;  // Offset zur Kennzeichnung eines
                                    // Errorcodes aus 'GetLastError'

  { Fehlerkonstanten für Status = SYS_ABRUFERROR }
  SYSABRFERR_KOMMANDONICHTPLAUSIBEL     = 1;
  SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT = 2;
  SYSABRFERR_KOMMANDOSOLLPROZESSID      = 3;
  SYSABRFERR_KOMMANDOUNGUELTIG          = 4;
  SYSABRFERR_ANTWORTNICHTPLAUSIBEL      = 5;
  SYSABRFERR_ANTWORTUNERWARTET_CMD      = 6;
  SYSABRFERR_ANTWORTUNERWARTET_PID      = 7;
  SYSABRFERR_ANTWORTINHALTLAENGE        = 8;
  SYSABRFERR_TIMEOUT                    = 9;
  { MI-Abruf-DLL: }
  SYSABRFERR_MIABRUFDLLNOTFOUND         = 20;
  SYSABRFERR_MIABRUFDLLACCESS           = 21;

  { Fehlerkonstanten für Status = COM_PORTERROR }
  COMPORTERR_NICHTVORHANDEN    = 1;
  COMPORTERR_OEFFNEN           = 2;
  COMPORTERR_DUE_GERAET_FALSCH = 3;

  { Fehlerkonstanten für Status = COM_KOMMERROR }
  KOMMERR_TIMEOUT            = 1;
  KOMMERR_BCC                = 2;
  KOMMERR_CRC                = 3;
  KOMMERR_VERB_UNTERBROCHEN  = 4;
  KOMMERR_FTL_BLOCKSICHERUNG = 5;
  KOMMERR_LRC                = 6;  { Modbus ASCII }
  KOMMERR_BAUD_NICHT_UNTERSTUETZT = 7;  { IEC1107 }

  { Fehlerkonstanten für Status = COM_DEVICEINITERROR }
  DEVINITERR_FUPRESET  = 1;
  DEVINITERR_FUPINIT   = 2;
  DEVINITERR_MODEMINIT = 3;
  DEVINITERR_PIN_LOCK  = 4;

  { Fehlerkonstanten für Status = SYS_LICENCEERROR }
  LICENCEERR_GERAETETYP      = 1;  { Gerätetyp nicht lizenziert }
  LICENCEERR_LAUFZEIT        = 2;  { Lizenz abgelaufen (Nutzungszeit) }
  LICENCEERR_KEINESTATIONEN  = 3;  { Keine Stations-Abrufberechtigungen vorhanden }
  LICENCEERR_ANZAHLSTATIONEN = 4;  { Max. Anzahl Stations-Abrufberechtigungen überschritten }
  LICENCEERR_UNBEKANNT       = 5;  { unbekannter Lizenzfehler }
  LICENCEERR_PROGFUNKTION    = 6;  { Programmfunktion nicht lizenziert }

  { Fehlerkonstanten für Status = COM_FUPERROR; Zuordnung zu den FUP-Fehlermeldungen
    sind in der DLL-Funktion 'GetFupErrorcode' enthalten }

  FUPERR_F0 =  1;
  FUPERR_F1 =  2;
  FUPERR_F2 =  3;
  FUPERR_F3 =  4;
  FUPERR_F4 =  5;
  FUPERR_F5 =  6;
  FUPERR_F6 =  7;
  FUPERR_F7 =  8;
  FUPERR_F8 =  9;
  FUPERR_F9 = 10;
  FUPERR_FA = 11;
  FUPERR_FC = 12;
  FUPERR_FD = 13;
  FUPERR_FE = 14;
  FUPERR_FF = 15;
  FUPERR_FG = 16;
  FUPERR_FH = 17;
  FUPERR_FI = 18;
  FUPERR_FM = 19;
  FUPERR_FN = 20;
  FUPERR_FZ = 21;
  FUPERR_FU = 22;
  FUPERR_FS = 23;
  FUPERR_FT = 24;
  FUPERR_FB = 25;

  { Fehlerkonstanten für Status = COM_MRGERROR }

  MRGERR_KOMMANDOUNPLAUSIBEL     = 1;
  MRGERR_KEINEBERECHTIGUNG       = 2;
  MRGERR_FALSCHESFORMAT          = 3;
  MRGERR_REVISION                = 4;
  MRGERR_KOMMANDOUNBEKANNT       = 5;
  MRGERR_AENDERUNGNICHTZULAESSIG = 6;
  MRGERR_ANTWORTUNVOLLSTAENDIG   = 7;
  MRGERR_ANTWORTUNERWARTET       = 8;
  { IEC 1107: }
  MRGERR_FEHLERTELEGRAMM         = 11;  { Fehlertelegramm }
  { Actaris Corus: }
  MRGERR_DATASIZE_SOLL_IST       = 20;  { Fehler Soll/Ist-Datengröße }
  MRGERR_DATASIZE_EXCEEDED_NACK  = 21;  { NACK, Datengröße > 255 Byte }
  { EC 900: }
  MRGERR_MOMENTANNICHTMOEGLICH   = 31;  { ! }

  { Fehlerkonstanten für Status = COM_MODEMERROR; mit GSM-Modemfehler ab 51 }

  CME_CONNECT     =  1;
  CME_ERROR       =  2;
  CME_RING        =  3;
  CME_NOCARRIER   =  4;
  CME_NODIALTONE  =  5;
  CME_DIALLOCKED  =  6;
  CME_BUSY        =  7;
  CME_DELAYED     =  8;
  CME_NOANSWER    =  9;
  CME_ABORT       = 10;
  CME_OK          = 11;
  CME_SONST       = 12;
  CME_DCD         = 13;
  CME_CTS         = 14;
  CME_BLACKLISTED = 15;

  CME_GSM_PIN_CHECK                   = 51;
  CME_GSM_PIN_WRITE                   = 52;
  CME_GSM_PIN_WRONG                   = 53;
  CME_GSM_PIN_LOGINIMPOSSIBLE_PUK     = 54;
  CME_GSM_SMS_FORMAT_WRITE            = 55;
  CME_GSM_SMS_READ                    = 56;
  CME_GSM_SMS_DELETE                  = 57;
  CME_GSM_PIN_LOGINIMPOSSIBLE_UNKNOWN = 58;
  CME_GSM_SMS_SEND                    = 59;
  CME_GSM_CSQ_READ                    = 60;
  CME_GSM_COPS_READ                   = 61;
  CME_GSM_REG_READ                    = 62;

  { Fehlerkonstanten für Status = COM_MODEMPROTERROR }

  CMPE_ENQ            = 1;
  CMPE_NAK            = 2;
  CMPE_EOT            = 3;
  CMPE_DLE_EOT        = 4;
  CMPE_UNGUELTIG      = 5;
  CMPE_UNVOLLSTAENDIG = 6;
  CMPE_UNBESTIMMT     = 7;

  { Fehlerkonstanten für Status = COM_TCPIP_ERROR }

  TCPIP_ERR_GENERAL             =  1;
  TCPIP_ERR_SEND                =  2;
  TCPIP_ERR_RECEIVE             =  3;
  TCPIP_ERR_CONNECT             =  4;
  TCPIP_ERR_DISCONNECT          =  5;
  TCPIP_ERR_ACCEPT              =  6;
  TCPIP_ERR_TIMEOUTCONNECT      =  7;
  TCPIP_ERR_CONNECTION_INACTIVE =  8;
  TCPIP_ERR_TIMEOUTDISCONNECT   =  9;
  TCPIP_ERR_LOOKUP              = 10;
  TCPIP_ERR_UNDEFINIERT         = 11;

  { Fehlerkonstanten für Status = COM_SRV_ERROR, COM_SIGNSRV_ERROR }

  SRV_ERR_GENERAL             =  1;
  SRV_ERR_SEND                =  2;
  SRV_ERR_RECEIVE             =  3;
  SRV_ERR_CONNECT             =  4;
  SRV_ERR_DISCONNECT          =  5;
  SRV_ERR_ACCEPT              =  6;
  SRV_ERR_TIMEOUTCONNECT      =  7;
  SRV_ERR_CONNECTION_INACTIVE =  8;
  SRV_ERR_TIMEOUTDISCONNECT   =  9;
  SRV_ERR_LOOKUP              = 10;
  SRV_ERR_UNDEFINIERT         = 11;
  SRV_ERR_TIMEOUTRECEIVE      = 12;

  { Fehlerkonstanten für Status = COM_LAKS }

  CLAKS_PARITYBCC   = 1;  { ! }
  CLAKS_ERROR       = 2;  { ? }
  CLAKS_WRONGNUMBER = 3;  { # }
  CLAKS_FORBIDDEN   = 4;  { = }

  { Fehlerkonstanten für Status = COM_DSFGDFUERROR }

  { allgemein }
  DSFGDFUERR_ANTWORTUNVOLLSTAENDIG         =  1;

  { für genormte Befehle an eine DSfG-DFÜ }
  DSFGDFUERR_UNBEKANNT                     = 11;  { ? }
  DSFGDFUERR_FALSCHESYNTAX                 = 12;  { ! }

  { für Befehle einer Wieser-DSfG-DFÜ }
  DSFGDFUERR_WIESER_UNBEKANNT              = 21;  { ? }
  DSFGDFUERR_WIESER_FALSCHENUMMER          = 22;  { # }
  DSFGDFUERR_WIESER_AENDEREUNGNICHTERLAUBT = 23;  { ! }
  DSFGDFUERR_WIESER_VERBOTEN               = 24;  { = } { bei NG }

  { Fehlerkonstanten für Status = ST_STAMMERROR }
  SMERR_STAMMSATZLESEN    =  1;
  SMERR_STAMMSATZNOTFOUND =  3;
  SMERR_RUFSTAMMDATEN     =  4;
  SMERR_GERAETETYPFALSCH  =  5;
  SMERR_DSFGINST_NOTFOUND =  6;
  SMERR_DSFGAK_NOTFOUND   =  7;
  SMERR_DSFGLB_NOTFOUND   =  8;
  SMERR_MRGLGZARCHORIG    =  9;
  SMERR_DSFGINSTTYP_WRONG = 10;
  SMERR_KENNUNG_MEHRFACH_KEIN_GPRS_ABRUF = 11;
  SMERR_STATIONZAEHLPKT_NICHTEINDEUTIG   = 12;
  SMERR_KANALNOTFOUND                    = 13;
  SMERR_KANALOBISKENNZ_NICHTEINDEUTIG    = 14;
  SMERR_EINGABEUNGUELTIG                 = 15;

  { Fehlerkonstanten für Status = ST_KONVERROR }

  SKERR_MELDKONV                      = 1;
  SKERR_MESSKONV                      = 2;
  SKERR_EBANDKONV                     = 3;
  SKERR_PARAMKONV                     = 4;
  SKERR_LASTZSTNDKONV                 = 5;
  SKERR_KARCHIVKONV                   = 6;
  SKERR_ARCHIVKONV                    = 7;
  SKERR_LOGBKONV                      = 8;
  SKERR_INSTWERTKONV                  = 9;
  SKERR_AUFMTELEGRAMMKONV             = 10;
  SKERR_KONVGERTYPNOTAVAILABLE        = 11;
  SKERR_AUSSERPLANMAESSIGEANTW_AR     = 12;
  SKERR_AUSSERPLANMAESSIGEANTW_LB     = 13;
  SKERR_AUSSERPLANMAESSIGEANTW_DE     = 14;
  SKERR_DSFGDFUEDATAKONV              = 15;
  SKERR_AUSSERPLANMAESSIGEANTW_KONFIG = 16;

  { Fehlerkonstanten für Status = ST_DSFGERROR (Quittungen im Erweiterungsgrad 1 der DSfG-DFÜ-Instanz) }

  DSFGERR_ENQ                 = 102;  { Telegramm nicht versendet, Fehler in HDCL oder BCC }
  DSFGERR_CAN                 = 103;  { lokaler Zielteilnehmer nicht vorhanden }
  DSFGERR_NAK                 = 104;  { Telegramm nicht innerhalb TS versendet }
  DSFGERR_BCC                 = 105;  { BCC-Fehler in Antwort festgestellt, Wiederholung beendet }
  DSFGERR_QUITTUNGUNDEFINIERT = 106;  { undefiniertes Quittungszeichen }

  { Fehlerkonstanten für Status = ST_DSFGUMLERROR }

  UMLERR_UMSCHALTUNG                  = 1;
  UMLERR_INVKENNUNG_KEINE_UMSCHALTUNG = 2;
  UMLERR_RUFLISTE                     = 3;
  UMLERR_RUFQUITTIEREN                = 4;
  UMLERR_INVKENNUNG_UMSCHALTUNG       = 5;

  { Fehlerkonstanten für Status = ST_DATACHECK }
  DCH_MWMISSING  = 1;
  DCH_MEINVALID  = 2;
  DCH_ARINVALID  = 3;
  DCH_LBINVALID  = 4;
  DCH_DEINVALID  = 5;
  DCH_ORDNRFOLGE = 6;
  DCH_MWINVALID  = 7;
  DCH_INVALID    = 8;
  DCH_KZWINVALID = 9;
  DCH_ORDNRFUELLSTANDBIS = 10;

  { Fehlerkonstanten für Status = ST_RUFREAKTERROR }
  RUFREAKTERR_WRONGPASSWORDNR = 1;
  RUFREAKTERR_NOSUCCESS       = 2;
  RUFREAKTERR_FINDRUFNR       = 3;

  { Fehlerkonstanten für Status = ST_RUECKRUFERROR }
  RUECKRUFERR_AUSLOESUNG = 1;

  { Fehlerkonstanten für Status = ST_RUECKRUFPRUEF }
  RUECKRUFPRUEF_AUSLOESUNGERR     = 1;
  RUECKRUFPRUEF_RUFERR            = 2;
  RUECKRUFPRUEF_KEINRUF           = 3;
  RUECKRUFPRUEF_JOURNALERR        = 4;
  RUECKRUFPRUEF_AUSLOESUNGABBRUCH = 5;

  { Fehlerkonstanten für Status = ST_DATAERROR }
  DERR_DELETE = 1;

  { Fehlerkonstanten für Status = ST_KONFIGERROR }
  KFERR_KONFIGDATANOTFOUND = 1;
  KFERR_PARAMETERNOTFOUND  = 2;
  KFERR_KONFIGDATAINVALID  = 3;

  { Fehlerkonstanten für Status = EST_ZEITSYNCERROR }
  ZSYNCERR_SUCCESS           =  0;  { OK }
  ZSYNCERR_DIFFERENTHOURS    =  1;
  ZSYNCERR_PERIODEND         =  2;
  ZSYNCERR_UNDEFINIERT       =  3;
  ZSYNCERR_HIGHERMAX         =  4;
  ZSYNCERR_WRONGPASSWORDNR   =  5;
  ZSYNCERR_NOSUCCESS         =  6;
  ZSYNCERR_SZTOWZ            =  7;
  ZSYNCERR_CORRMAX           =  8;
  ZSYNCERR_READDEVICETIME    =  9;
  ZSYNCERR_INVALIDDEVICETIME = 10;

  { Tritschler Synchronisations-Stati }
  ZSYNCERR_VERZOEGERT           = 21;  // 01
  ZSYNCERR_NICHTDURCHGEFUEHRT   = 22;  // 02
  ZSYNCERR_SYNTAXFEHLER         = 23;  // 03
  ZSYNCERR_GERAETBUSY           = 24;  // 04
  ZSYNCERR_BEREITSSYNCHRON      = 25;  // 05
  ZSYNCERR_SICHERUNGFALSCH      = 26;  // 06
  ZSYNCERR_VERSTELLWEITEGEKAPPT = 27;  // 07
  ZSYNCERR_WIEDERHOLSPERRE      = 28;  // 08
  ZSYNCERR_SONSTIG              = 99;  // 99

  { Fehlerkonstanten für Stati = EST_RPRESET_MEERROR, EST_RPRESET_MWERROR,
                                 EST_RPRESET_PRERROR }
  RPRESETERR_WRONGPASSWORDNR = 1;
  RPRESETERR_NOSUCCESS       = 2;

  { Fehlerkonstanten für Status = EST_RUFDEAKTERROR }
  RUFDEAKTERR_WRONGPASSWORDNR = 1;
  RUFDEAKTERR_NOSUCCESS       = 2;
  RUFDEAKTERR_SUCCESS         = 3;

  { Fehlerkonstanten für Status = EST_PARAMERROR }
  PARAMERR_NOSUCCESS                  = 2;
  PARAMERR_DSFG_WRONGANSWER           = 3;  { für DSfG: allg. Fehler in der Einstelltelegramm-Antwort }
  PARAMERR_DSFG_WRONGCODENR_UNKNOWNDE = 4;  { für DSfG: Zugangscode(s) falsch o. unbekanntes Datenelement }
  PARAMERR_DSFG_NOCHANGEABLEDE_NOES   = 5;  { für DSfG: Datenelement nicht veränderbar o. Eichschalter geschlossen }
  PARAMERR_FORMATWERT                 = 6;
  PARAMERR_NOTAVAILABLE               = 7;

  { Fehlerkonstanten für Status = EST_LOGINERROR }
  LOGINERR_WRONGPW          = 1;
  LOGINERR_NODFUEBUSADDRESS = 2;
  LOGINERR_NOACTIVEUSER     = 3;
  LOGINERR_TIMEOUT_WRONGPW  = 4;
  LOGINERR_MEHRERELEITSTATIONEN = 5;
  LOGINERR_LIEFERANTENSCHLOSS_GEOEFFNET = 6;
  LOGINERR_KUNDENSCHLOSS_GEOEFFNET      = 7;
  LOGINERR_DATENAUSLESERSCHLOSS_GEOEFFNET = 8;

  { Fehlerkonstanten für Status = EST_KENNUNGCHECK }

  KENNERR_VERBINDUNG        = 1;  { Kennung falsch, Verbindung bleibt bestehen }
  KENNERR_KEINE_VERBINDUNG  = 2;  { Kennung falsch, keine Verbindung }
  KENNERR_WRONGANSWER       = 3;  { falsche Antwort auf Kennungsabfrage }
  KENNERR_KEINSTAMMSATZ     = 4;  { Kennung in Stammdaten nicht gefunden }
  KENNERR_ALREADYEXISTS_W   = 5;  { Kennung in Stammdaten bereits vorhanden (Warnung) }
  KENNERR_MEHRFACH_NODATA   = 6;  { Kennung in Stammdaten mehrfach vorhanden, keine Datenabfrage }
  KENNERR_RE_PWACTIVATED    = 7;  { Kennungsabfrage bei Rufentgegennahme von MRG 910 bis 10/2002
                                    nur im Auslesemodus "ohne Passwort" möglich ! }
  KENNERR_ALREADYEXISTS_F   = 8;  { Kennung in Stammdaten bereits vorhanden (Fehler) }

  { Fehlerkonstanten für Status = ST_FILEERROR }

  FILEERR_COULDNOTOPEN  = 1;  { Datei konnte nicht geöffnet werden }
  FILEERR_COULDNOTWRITE = 2;  { in Datei konnte nicht geschrieben werden }
  { Tritschler Alarmdatei für Stationsrechner-Meldungen: }
  FILEERR_FTL_SR_ALARM_COULDNOTCREATEDIR  = 3;  { Verzeichnis für Alarmdatei konnte nicht angelegt werden }
  FILEERR_FTL_SR_ALARM_COULDNOTCREATEFILE = 4;  { Alarmdatei konnte nicht angelegt werden }
  FILEERR_FTL_SR_ALARM_COULDNOTOPENFILE   = 5;  { Alarmdatei konnte nicht geöffnet werden }
  { Ascii-Datei für Rückrufprüfung: }
  FILEERR_RUECKRUFPRUEF_COULDNOTCREATEDIR  = 6;  { Verzeichnis für Ascii-Datei konnte nicht angelegt werden }
  FILEERR_RUECKRUFPRUEF_COULDNOTCREATEFILE = 7;  { Ascii-Datei konnte nicht angelegt werden }
  FILEERR_RUECKRUFPRUEF_COULDNOTOPENFILE   = 8;  { Ascii-Datei konnte nicht geöffnet werden }
  FILEERR_IMPORT_LOADFILE = 9;  { Import-Rohdatendatei konnte nicht geladen werden }
  { Datei mit gesicherten DSfG-DFÜ-Parametern bei Firmware-Update: }
  FILEERR_FIRMWUPD_PARA_COULDNOTWRITE = 10;
  FILEERR_FIRMWUPD_PARA_COULDNOTREAD  = 11;
  FILEERR_FIRMWUPD_PARA_DOESNOTEXIST  = 12;

  { Fehlerkonstanten für Status = COM_PDAERROR }

  PDAERR_ANTW_UNDEFINIERT = 1;  { undefinierte PDA-Antwort }
  PDAERR_CRC              = 2;  { PDA-Antwort: CRC-Fehler }
  PDAERR_CTS              = 3;  { PDA nicht angeschlossen oder PC-Transfer im PDA nicht aktiv }

  { Fehlerkonstanten für Status = COM_KE_ERROR }

  KEERR_HEADER                 = 1;
  KEERR_BEFEHLSYNTAX           = 2;
  KEERR_BEFEHL_NICHT_ERLAUBT   = 3;
  KEERR_ANTWORT_UNVOLLSTAENDIG = 4;

  { Fehlerkonstanten für Status = COM_PALMUSBERROR
    -> Fehlercodes für Funktionen aus USBPORT.DLL (-> UsbPortPrc.pas) }
  PALMUSBERR_NoError           = $00000000;	// No error
  PALMUSBERR_Unknown	         = $00000001;	// Unknown error
  PALMUSBERR_SendTimedOut      = $00000002;	// Send timed out
  PALMUSBERR_RecvTimedOut      = $00000003;	// Receive timed out
  PALMUSBERR_PortNotOpen       = $00000004;	// USB port is not open
  PALMUSBERR_IOError	         = $00000005;	// I/O or line error
  PALMUSBERR_PortBusy	         = $00000006;	// USB port is already in use
  PALMUSBERR_NotSupported      = $00000007;	// IOCTL code unsupported
  PALMUSBERR_BufferTooSmall    = $00000008;	// Buffer size too small
  PALMUSBERR_NoAttachedDevices = $00000009;	// No devices currently attached
  PALMUSBERR_DontMatchFilter   = $00000010;	// Creator ID provided doesn't
                                            // match the USB-active device
                                            // application creator ID
  { -> zusätzliche Fehlercodes (Wieser) }
  PALMUSBERR_DLLAccess         = $00001000; // DLL-Zugriffsfehler
  PALMUSBERR_CouldNotOpenPort  = $00001001; // USB-Port konnte nicht geöffnet werden
  PALMUSBERR_SendIncomplete    = $00001002; // Daten unvollständig versendet
  PALMUSBERR_ReceiveBuffer     = $00001004;	// mehr Zeichen aus Empfangspuffer gelesen als angefordert

  { Fehlerkonstanten für Status = ACT_USER }

  USERACT_ABBRUCH_ABRUF = 1;

  { Fehlerkonstanten für Status = COM_FTL_ERROR }
  FTLERR_EMPFANGSPUFFERUEBERLAUF               =  1;  { neg. Quittung '?01' }
  FTLERR_NICHTEMPFANGSBEREIT_TIMEOUT           =  2;  { neg. Quittung '?02' }
  FTLERR_STARTSTOPBIT                          =  3;  { neg. Quittung '?04' }
  FTLERR_BLOCKSICHERUNG                        =  4;  { neg. Quittung '?06' }
  FTLERR_KOMMANDONICHTAUSFUEHRBAR_FALSCHEFKTNR =  5;  { neg. Quittung '?08' }
  FTLERR_KEINEDATENVORHANDEN                   =  6;  { neg. Quittung '?09' }
  FTLERR_UNBEKZEICHEN_KOMMANDO                 =  7;  { neg. Quittung '?16' }
  FTLERR_PARAMETERUNZULAESSIG                  =  8;  { neg. Quittung '?17' }
  FTLERR_QUITTUNGUNDEFINIERT                   =  9;
  FTLERR_ANTWORTUNVOLLSTAENDIG                 = 10;
  FTLERR_ANTWORTUNERWARTET                     = 11;

  { Fehlerkonstanten für Status = COM_FTLINTERN_ERROR }
  FTLINTERNERR_BLOCKZUKURZ                     =  1;  { Blocknr. 802 }
  FTLINTERNERR_BLOCKNRKEINEZAHL                =  2;  { Blocknr. 803 }
  FTLINTERNERR_BLOCKFOLGEMARKEUNGUELTIG        =  3;  { Blocknr. 804 }
  FTLINTERNERR_CRCKEINEZAHL                    =  4;  { Blocknr. 805 }
  FTLINTERNERR_CRCFALSCH                       =  5;  { Blocknr. 806 }
  FTLINTERNERR_TESTERGEBNISSCHLECHT            =  6;  { Blocknr. 807 }
  FTLINTERNERR_BLOCKNRUNBEKANNT                =  7;  { Blocknr. 810 }
  FTLINTERNERR_PARAMETERGESCHRIEBEN            =  8;  { Blocknr. 809 }
  FTLINTERNERR_PARAMETERNICHTFLOAT             =  9;  { Blocknr. 811 }
  FTLINTERNERR_PARAMETERNICHTINTEGER           = 10;  { Blocknr. 812 }
  FTLINTERNERR_PARAMETERNICHTLONGINTEGER       = 11;  { Blocknr. 813 }
  FTLINTERNERR_PARAMETERNICHTCHAR              = 12;  { Blocknr. 814 }
  FTLINTERNERR_PARAMETERAUSSERHALBGRENZWERTE   = 13;  { Blocknr. 815 }
  FTLINTERNERR_BLOCKGESPERRT                   = 14;  { Blocknr. 816 }
  FTLINTERNERR_BLOCKKANNNICHTGESCHRIEBENWERDEN = 15;  { Blocknr. 817 }
  FTLINTERNERR_FALSCHEBLOCKLAENGE              = 16;  { Blocknr. 819 }
  FTLINTERNERR_BLOCKNRUNDEFINIERT              = 17;

  { Fehlerkonstanten für Status = EST_GBHREVISION }

  GBHREVERR_KEINE_REV_INSTANZ = 1;
  GBHREVERR_PARAM_SOLL_IST    = 2;

  { Fehlerkonstanten für Status = COM_GPRS_ERROR }
  GPRSERR_KEINE_VERBINDUNG            = 1;  { Keine Verbindung vorhanden }
  GPRSERR_KENNUNG_MEHRFACH_KEIN_ABRUF = 2;  { Mehrere aktive Verbindungen für Kennung vorhanden, kein Datenabruf }

  { Fehlerkonstanten für Status = COM_MODBUSERROR }
  MODBUSERR_EXC_ILLEGALFUNCTION     =  1;  { Exceptioncode $01 }
  MODBUSERR_EXC_ILLEGALDATAADDRESS  =  2;  { Exceptioncode $02 }
  MODBUSERR_EXC_ILLEGALDATAVALUE    =  3;  { Exceptioncode $03 }
  MODBUSERR_EXC_SLAVEDEVICEFAILURE  =  4;  { Exceptioncode $04 }
  MODBUSERR_EXC_ACKNOWLEDGE         =  5;  { Exceptioncode $05 }
  MODBUSERR_EXC_SLAVEDEVICEBUSY     =  6;  { Exceptioncode $06 }
  MODBUSERR_EXC_NEGATIVEACKNOWLEDGE =  7;  { Exceptioncode $07 }
  MODBUSERR_EXC_MEMORYPARITYERROR   =  8;  { Exceptioncode $08 }
  MODBUSERR_EXC_UNDEFINED           =  9;  { undefinierter Exceptioncode }

  MODBUSERR_RESP_INCOMPLETE         = 10;
  MODBUSERR_RESP_WRONGFUNCTION      = 11;
  MODBUSERR_RESP_WRONGSLAVEADDRESS  = 12;
  MODBUSERR_RESP_BYTECOUNT_SOLL_IST = 13;

  MODBUSERR_EXC_GATEWAYPATHUNAVAILABLE             =  14;  { Exceptioncode $0A }
  MODBUSERR_EXC_GATEWAYTARGETDEVICEFAILEDTORESPOND =  15;  { Exceptioncode $0B }

  MODBUSERR_QUERY_CREATE         = 16;
  MODBUSERR_RESP_UNKNOWNFUNCTION = 17;
  MODBUSERR_RESP_WRONGREGISTEREADDRESS = 18;

  { Funktion "RMG lesen von/bis": }
  MODBUSERR_RMG_INDEX               = 20;  { Fehler in Index (Anzahl Datenbytes = 2) }
  MODBUSERR_RMG_EXCEPTION           = 21;  { Exception (Anzahl Datenbytes = 255) }

  { Modbus-TCP/IP: }
  MODBUSERR_RESPTCP_WRONGTID        = 30;
  MODBUSERR_RESPTCP_WRONGPROTID     = 31;
  MODBUSERR_RESPTCP_LENGTH_SOLL_IST = 32;

  { Fehlerkonstanten für Status = COM_DS100_ERROR }
  DS100ERR_QUITTUNGNEGATIV    = 1;
  DS100ERR_QUITTUNGUNERWARTET = 2;

  { Fehlerkonstanten für Status = SYS_SIGNSRVSYSERROR }
  SIGNSRVSYSERR_ANTWORTNICHTPLAUSIBEL = 1;
  SIGNSRVSYSERR_ANTWORTUNERWARTET_CMD = 2;
  SIGNSRVSYSERR_ANTWORTUNERWARTET_TID = 3;

  { Fehlerkonstanten für Status = SYS_SIGNSRV_ERROR }
  SIGNSRVERR_SIGNINVALID                =   1;
  SIGNSRVERR_UNKNOWNSIGNALGORITHM       =   2;
  SIGNSRVERR_HASHGENERATION             =   3;
  SIGNSRVERR_SIGN                       =   4;
  SIGNSRVERR_KEYGEN                     =   5;
  SIGNSRVERR_UNKNOWNKEY                 =   6;
  SIGNSRVERR_INVALIDHEXFORMAT           =   7;
  SIGNSRVERR_VERIFY                     =   8;
  SIGNSRVERR_KEYADMINFILEEMPTY          =   9;
  SIGNSRVERR_LOADKEYADMINFILE           =  10;
  SIGNSRVERR_KEYADMINFILENOTFOUND       =  11;
  SIGNSRVERR_KEYADMINFILEWRITEPROTECTED =  12;
  SIGNSRVERR_KEYSAVE                    =  13;

  SIGNSRVERR_NOTVERIFIED                = 100;
  SIGNSRVERR_LICENCE                    = 101;  { Signatur nicht lizenziert }

  { Fehlerkonstanten für Status = EST_ZEITBEFEHLERROR }
  ZEITBEFERR_ZEITLESEN = 1;

  { Fehlerkonstanten für Status = EST_FIRMWAREUPDATEERROR }
  FIRMWUPDERR_ADRSTARTGLEICHENDE    = 1;  { Firmwareupdate-Befehl, Fehler 0: Start- gleich Endadresse }
  FIRMWUPDERR_ADRSTARTZUKLEIN       = 2;  { Firmwareupdate-Befehl, Fehler 1: Startadresse < F00000 }
  FIRMWUPDERR_ADRSTARTGROESSERENDE  = 3;  { Firmwareupdate-Befehl, Fehler 2: Start- größer Endadresse }
  FIRMWUPDERR_UNDEFINIERT           = 4;  { Firmwareupdate-Befehl, undefinierter Fehler }
  FIRMWUPDERR_ANTWORTUNERWARTET     = 5;
  FIRMWUPDERR_ANTWORTUNVOLLSTAENDIG = 6;

  FIRMWUPDERR_BINDATA_NICHTGEFUNDEN     = 10;
  FIRMWUPDERR_BINDATA_ZUGROSS           = 11;
  FIRMWUPDERR_BINDATA_STARTADRUNGUELTIG = 12;
  FIRMWUPDERR_BINDATA_ENDADRUNGUELTIG   = 13;
  FIRMWUPDERR_BINDATA_TYP_FEHLT         = 14;
  FIRMWUPDERR_BINDATA_TYP_FALSCH        = 15;
  FIRMWUPDERR_BINDATA_BUILD_FEHLT       = 16;
  FIRMWUPDERR_BINDATA_BUILD_UNGUELTIG   = 17;
  FIRMWUPDERR_BINDATA_BUILD_ALT         = 18;

  FIRMWUPDERR_BINDATA_TRANSFER_NAK   = 30;
  FIRMWUPDERR_BINDATA_TRANSFER_CAN   = 31;
  FIRMWUPDERR_BINDATA_TRANSFER_ENQ   = 32;
  FIRMWUPDERR_BINDATA_TRANSFER_UNDEF = 33;
  FIRMWUPDERR_BINDATA_TRANSFER_X     = 34;

  FIRMWUPDERR_FLASH_ERASE = 40;
  FIRMWUPDERR_FLASH_PROG  = 41;
  FIRMWUPDERR_FLASH_UNDEF = 42;

  { Fehlerkonstanten für Status = EST_GERAETECHECK }
  GERCHECKERR_TYP_FALSCH = 1;
  GERCHECKERR_ARCH_ZEILENWEISE_LESBAR = 2;

  { Fehlerkonstanten für Status = EST_DSFGTRANSPARENTERROR }
  DSFGTRANSPERR_DFUEBUSADDRESS_KEINLOKALERTEILNEHMER = 1;

  { Fehlerkonstanten für Status = EST_MWLESENERROR }
  MWLESENERR_ZUGRIFFSRECHT = 1;

  { Fehlerkonstanten für Status = EST_MELESENERROR }
  MELESENERR_ZUGRIFFSRECHT = 1;

{------------------------------------------------------------------------------}

  { Fehlercodes für Journal-Detailtabellen }

  { DSfG }

  DSFGKONVERR_OK              =  0;                  { Daten vorhanden und ok }
  DSFGKONVERR_HEADER          =  1;               { Fehler im Rohdaten-Header }
  DSFGKONVERR_DATA            =  2;                  { Fehler in den Rohdaten }
  DSFGKONVERR_FILEACCESS      =  3;    { Fehler beim Zugriff auf Rohdatenfile }
  DSFGKONVERR_INDEX           =  4;                    { Fehler Index-Tabelle }
  DSFGKONVERR_NODATA          =  5;                   { keine Daten verfügbar }
  DSFGKONVERR_KONFDATA        =  6;  { Fehler beim Zugriff auf Konfigurationsdaten }
  DSFGKONVERR_UNKNOWNLBSOURCE =  7;         { unbekannte Logbuch-Quellinstanz }
  DSFGKONVERR_ZAE_SOLL_IST    =  8;                   { ZAE-Soll ungleich Ist }
  DSFGKONVERR_FILENOTFOUND    =  9;             { Rohdatenfile nicht gefunden }

  { Warnungen }
  DSFGKONVERR_INVVALUE        = 10;           { Wert im Datenelement ungültig }
  DSFGKONVERR_INVTIME         = 11;    { Zeitstempel im Datenelement ungültig }
  DSFGKONVERR_INVNUMBER       = 12; { Ordnungsnummer im Datenelement ungültig }
  DSFGKONVERR_INVADDR         = 13;        { Adresse im Datenelement ungültig }
  DSFGKONVERR_INVSTATE        = 14;         { Status im Datenelement ungültig }
  DSFGKONVERR_INVCRC          = 15;      { Prüfsumme im Datenelement ungültig }
  DSFGKONVERR_EMPTYREC        = 16;                        { leerer Datensatz }

  { interne Verarbeitungsfehler }
  DSFGKONVERR_DATACHECK_INTERN = 30;  { interner Fehler bei Rohdatenprüfung }
  DSFGKONVERR_DATAKONV_INTERN  = 31;  { interner Fehler bei Rohdatenkonvertierung }

  DSFGKONVERR_STAMM            = 40;  { Fehlende Stammdaten }

  { Signaturstati: Code >= 90 }
  C_MinCode_DSfGKonvErr_SigVerify   = 90;

  DSFGKONVERR_SIGVERIFY_INVALID     = 92;  { Signatur ungültig }
  DSFGKONVERR_SIGVERIFY_NOTVERIFIED = 93;  { Signatur wurde nicht verifiziert }

  { außerplanmäßige Antworten: Code >= 100 }
  C_MinCode_DSfGKonvErr_AA          = 100;     { kleinster Code für außerplanmäßige Antworten }

  DSFGKONVERR_AA_NICHT_PLAUSIBEL    = 100;     { ? = Vorgang nicht plausibel }
  DSFGKONVERR_AA_BESTAETIGUNG       = 101;     { *  = Bestätigung einer ordnungsgemäßen Übermittlung }
  DSFGKONVERR_AA_KEINE_BERECHTIGUNG = 102;     { ! = fehlende Zugangsberechtigung }
  DSFGKONVERR_AA_UNBEKANNT          = 103;     { # = Vorgang bzw. Teilnehmer unbekannt }
  DSFGKONVERR_AA_NICHT_BEHAND       = 104;     { : = Vorgang kann z.Z. nicht behandelt werden }
  DSFGKONVERR_AA_SONSTIGE           = 105;     { sonstige, z.B. freier Textstring }


function Get_StatusHigh (Status: integer): integer;
function Get_StatusLow (Status: integer): integer;

IMPLEMENTATION

{-------------------------------------------------}
function Get_StatusHigh (Status: integer): integer;
{-------------------------------------------------}
{ liefert den in Status enthaltenen Statuscode der höchsten Prioritätsstufe }
begin
  if Status >= 100000 then  { Statuscode der 3. Prioritätsstufe enthalten }
    Result:=(Status DIV 100000) * 100000      { z.B. 101001 -> 100000 }
  else if Status >= 1000 then  { Statuscode der 2. Prioritätsstufe enthalten }
    Result:=(Status DIV 1000) * 1000          { z.B.   1001 ->   1000 }
  else  { Statuscode der 1. Prioritätsstufe enthalten }
    Result:=Status;                           { z.B.      1 ->      1 }
end;

{------------------------------------------------}
function Get_StatusLow (Status: integer): integer;
{------------------------------------------------}
{ liefert den in Status enthaltenen Statuscode der niedrigsten Prioritätsstufe }
begin
  Result:=Status MOD 1000;           { z.B. 100001 ->      1 }
  if Result = 0 then begin  { Statuscode der 1. Prioritätsstufe nicht enthalten }
    Result:=Status MOD 100000;       { z.B. 101000 ->   1000 }
    if Result = 0 then  { Statuscode der 2. Prioritätsstufe nicht enthalten }
      Result:=Status;                { z.B. 100000 -> 100000 }
  end;
end;

End.



