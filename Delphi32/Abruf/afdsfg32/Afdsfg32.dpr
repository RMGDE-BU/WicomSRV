{****************************************************************************************}
{* Programm: DSfG-Abrufmodul (DFÜ) für Client/Server-Abrufsystem                        *}
{* Funktionen: - Abruf von DSfG-Stationen über eine feste oder beliebige                *}
{*               serielle Schnittstelle                                                 *}
{*             - automatischer und manueller Abruf von Archiven, Logbüchern und         *}
{*               Datenelementen; manueller Abruf über Zeitbereich oder Ordnungsnummern  *}
{*             - Einlesen der Konfiguration einer DSfG-Station und Abspeichern in den   *}
{*               Stammdaten                                                             *}
{*             - Journal mit Info über evtl. aufgetretene Fehler (Abruf global) und     *}
{*               Detailinfo für Zeitbereiche/Detailfehler abgerufener Daten             *}
{*             - temporäre Aufzeichnung der aktuellen Abrufzustände                     *}
{* Kommandozeilenparameter: - Schnittstelle (Com1..Com8)                                *}
{*                          - Debug-Modus (DEBUG) optional -> kein Rohdatenlöschen      *}
{* Version: 08.03.2000  WW                                                              *}
{*          06.04.2000  WW  Comserve ersetzt durch Klasse TSerialDSfG, Timeout-Fehler   *}
{*                          raus bei KonfigLesen ohne Teilnehmerliste, Archiv/Logbuch-  *}
{*                          tabellenstruktur geändert (Feld 'CRC')                      *}
{*          05.05.2000  WW  Fahrwegkanäle erfassen bei KonfigLesen (Kanaltyp FW)        *}
{*          29.06.2000  WW  Abruf mit maximaler Baudrate (je nach DSfG-DFÜ),            *}
{*                          für Gamess: gespeicherte Datenelemente vor Automatik-Abruf  *}
{*                          löschen und ausgelesene Konfigurationsdaten in Datei mit-   *}
{*                          schreiben                                                   *}
{*          04.08.2000  WW  unplausible Telegrammdaten abfangen beim Konfigurationlesen *}
{*                          (RMG-Umwerter !) und bei Archiv/Logbuch-Abfrage             *}
{*          13.10.2000  WW  CPU-Auslastung reduziert mit Sleep-Funktion                 *}
{*                          Konfiguration lesen mit GerätetypNr für Wieser-Instanzen,   *}
{*                          Datum für 1. Abruf aus Systemdaten statt Stammdaten lesen,  *}
{*                          Momentanwertabfrage und Parametrierung,                     *}
{*                          Journal-Reorganisation beschleunigt,                        *}
{*                          Datenmenge der Journal-Zeitbereichtabelle reduziert,        *}
{*                          Journaltabellen, Auftrags-/Zustands-/InstWert-Tabelle re-   *}
{*                          strukturieren, Fehler Delay-Funktion raus                   *}
{*                          KonfigLesen: Archivgruppennamen aus DE ca?a (Kennung der AG)*}
{*                                       lesen                                          *}
{*          13.12.2000  WW  Konfiguration-Einlesen: ENQ abfangen (kein Abbruch),        *}
{*                          Archivgruppenname mit vorangestellter Nummer der AG         *}
{*          01.02.2001  WW  V. 2.0: auf 16 COM erweitert				*}
{*   			    Comserve.ini ersetzt durch SrvCfg32.ini + Modem.ini,	*}
{*                          keine Automatik-Deaktivierung mehr beim x-ten Fehlabruf oder*}
{*                          falscher Kennung bzw. falschem Passwort, RestructDaten raus,*}
{*                          Datenkonvertierung: kein Überschreiben mehr schon vorhan-   *}
{*                          dener Tabellendaten, Verbindungsabbau mit Warten auf OK vom *}
{*                          Modem, Modem-Initialisierung vor jedem Abruf,               *}
{*                          Taskleisten-Icon, optische und akustische Warnung (Icon     *}
{*                          blinkt, ErrorBeep über ALM32.INI ein-/ausschaltbar)         *}
{*          28.05.2001  WW  V. 2.1: Parametrierung der DFÜ-Instanz; Rufentgegennahme;   *}
{*                          Journal-Warnung beim Konfiguration-Einlesen, wenn Kennung   *}
{*                          in den Stammdaten bereits existiert; Fehler Vorwahl in      *}
{*                          Stammdaten abspeichern beim Konfiguration-Einlesen raus;    *}
{*                          Befehlsausführung aus Binärfile (Momentanwert-Abruf und     *}
{*                          DSfG-DFÜ-Parametrierung)                                    *}
{*          17.07.2001  WW  V. 2.2: Umstellung auf gemeinsame MRG/DSfG-Meldungstabellen *}
{*                          und Meldungskonfigurations-Tabellen; automatischer Abruf:   *}
{*                          Meldungs-Tabellen als Rundpuffer organisieren; Stations-    *}
{*                          Aktivierung/Deaktivierung nach letztem Anrufversuch über    *}
{*                          Systemdateneinstellung; Freischaltung der Rufentgegennahme  *}
{*                          über Lizenzfile                                             *}
{*          25.09.2001  WW  V. 2.21: TOKonfLesen erhöht auf 90 s (muß auf jeden Fall    *}
{*                          größer als TS der DFÜ sein; TS-Standardwert für Wieser-DSfG-*}
{*                          DFÜs: 60 s)                                                 *}
{*          05.11.2001  WW  V. 2.22: Konfigurationsdaten-Einlesen für Registrier-       *}
{*                          Instanzen über DE-Bereich 'c'-'cbdae' statt 'c'-'d' wegen   *}
{*                          Gerätefehler in RMG-Registrierinstanz ERZ9000T (Vs. ...D)   *}
{*          07.11.2001  WW  V. 2.23: Fehler in GetNextTermin behoben (Query für MRG-Grp)*}
{*          14.11.2001  WW  V. 2.24: Konfiguration-Einlesen: jetzt alle 5 Adressen von  *}
{*                          Wieser-DFÜs lesen (sonst Fehler beim Einlesen, wenn 5.      *}
{*                          Adresse belegt)                                             *}
{*          14.01.2002  WW  V. 2.25: Delay nach Verbindungsaufbau wegen MRG 910         *}
{*          11.02.2002  WW  V. 2.3: Konfiguration-Einlesen: Kanaltypen für MRG2201 ver- *}
{*                          vollständigt (Wieser-Quelldatenelemente), Kanaltypen für    *}
{*                          Quell-Datenelemente von Steuer-/Überwachungsinstanzen (f..) *}
{*                          eingebaut, Vollanalyse-Kanäle (AG 3,4 im MRG2201)           *}
{*                          standardmäßig auf Automatik=False gesetzt                   *}
{*          15.02.2002  WW  V. 2.31: Konfiguration-Einlesen: auch LGZ-Speicher-Kanäle   *}
{*                          (AG 15,16 im MRG2201) standardmäßig auf Automatik=False     *}
{*                          gesetzt; Kanaltyp für DE weaad korrigiert                   *}
{*          21.02.2002  GD  V. 2.32: Pflege allgemeiner Module                          *}
{*          11.03.2002  WW  V. 2.33: bei DFÜ-Parametrierung nur von der DFÜ unterstützte*}
{*                          Befehle abfragen (MRG910); Konfiguration-Einlesen: Kanaltyp *}
{*                          wird jetzt über DelKanaltyp.db bestimmt; Modem-Antworten    *}
{*                          werden jetzt richtig gelesen und ausgewertet unabhängig von *}
{*                          der Anzahl der enthaltenen CR LF (war Problem u.a. bei      *}
{*                          internen Toshiba-Notebook-Modems)                           *}
{*          23.04.2002  WW  V. 2.4: Turbo-Abrufmodul ! Automatik-Abruf läuft jetzt über *}
{*                          Ordnungsnummer (Automatik über Zeit weiterhin möglich per   *}
{*                          INI-File-Konfiguration); Log-Filename mit vollständiger     *}
{*                          Pfadangabe (Exe-Pfad); Fehler beim Reorganisieren der DSfG- *}
{*                          Zeitbereichsjournal-Tabelle behoben; Auto-/Manu-Abruf und   *}
{*                          Rufentgegennahme: Zeitangaben-Abruf jetzt vor und nach dem  *}
{*                          Datenlesen (sonst fehlen die Zeitzonen in den Archiv- und   *}
{*                          Logbuchdaten, wenn eine Verbindungsunterbrechung auftritt); *}
{*                          TID im DSfG-Befehl max. 4 stellig; ERZ2200-Umwerter mit     *}
{*                          Gerätetyp-Nr <> 0 wegen Wieser-spezifischer Meldungsnummern *}
{*          08.05.2002  WW  V. 2.41: Fehler Zeitangabenabruf von DFÜ-Instanz raus (trat *}
{*                          bei Stammsatz mit mehr als einer DFÜ-Instanz auf)           *}
{*          06.09.2002  WW  V. 2.43: Variablenübergabe-Änderung in LOGCOM.PAS           *}
{*          31.10.2002  WW  V. 2.44: alle Meldungstabellen mit Exklusivzugriff und Flush*}
{*                          bei Schreib-/Löschvorgängen                                 *}
{*          05.11.2002  WW  V. 2.45: Zeitzonen-Berechnung für Logbuch-Daten korrigiert  *}
{*          18.12.2002  GD  V. 2.5: Schreiben von Tabelle WMeldungenAlarm               *}
{*          03.03.2003  WW  V. 2.51: Quellcode-Umstrukturierungen (Daten-Konvertierung) *}
{*          01.04.2003  WW  V. 2.6: mit Kommunikation über TCP/IP (Kommandozeilen-      *}
{*                          Parameter 'TCP/IP')                                         *}
{*	    08.04.2003	WW  V. 2.61: Blockungsgröße für Anforderungstelegramm mit       *}
{*                          mehreren Datenelementen reduziert (sonst Verbindungsabbruch)*}
{*	    21.05.2003	WW  V. 2.7: optionale Konvertierung in kavernenbezogene Daten   *}
{*	    23.05.2003	WW  V. 2.71: Verbindungsabbruch bei "sonst. Fehler/BCC-Fehler"  *}
{*                          bei Momentanwertdarstellung behoben                         *}
{*	    16.06.2003	WW  V. 2.72: Fehler Passwort-Eintragung in Stammsatz beim       *}
{*                          Konfiguration-Einlesen behoben (PW 1..4 von vorangegangenen *}
{*                          Einlesevorgängen wurden mitabgespeichert)                   *}
{*          31.05.2003  GD  Vs. 2.8  Archivabruf bei Momentanwertabrufc                 *}
{*          30.07.2003  WW / H.-P.R. 2.81 TERMINDB.PAS für MRG Gruppen angepaßt         *}
{*	    09.09.2003	WW  V. 2.9: mit erweiterter Stationslizenzierung (freischalten  *}
{*                          von Wieser MRG 910 und/oder sonstiger Wieser-Stationen)     *}
{*	    13.10.2003	WW  V. 2.91  Verbindungsaufbau: jetzt mit Journalstatus-Rückgabe*}
{*                          beim Lesen der DSfG-DFÜ-Versionsdaten und der DSfG-DFÜ-     *}
{*                          Adressen                                                    *}
{*          10.11.2003  GD  Vs. 2.92 Bugfix: Zeitbereich für aut. Export                *}
{*          15.01.2004  WW/GD  V. 2.93: Filezugriffe jetzt threadsicher mit TFileStream;*}
{*                          Erweiterung GetRufStammdaten bei Momentanwertabruf          *}
{*          22.03.2004  WW  Vs. 2.94: DSfG-Abruf MRG910 über Zeit mit Status im Kommando*}
{*                          (Gerätefehler: Ordnungsnummern in den gelieferten Archiv-   *}
{*                          daten falsch);                                              *}
{*          20.07.2004  WW  Vs. 2.95: Auswertung außerplanmäßiger DSfG-Antworten        *}
{*          06.08.2004  GD  Vs. 2.96: Zustand bei Momentanwertabrufen                   *}
{*          22.09.2004  WW  Vs. 2.97: Timeout auswerten beim Transparentschalten der DFÜ*}
{*          29.11.2004  WW  Vs. 2.98: Abruf als fehlerhaft kennzeichnen, wenn Fehler bei*}
{*                          Abfrage 'AG/LB Füllstand bis' auftritt (Wdh !)              *}
{*          13.01.2005  WW  Vs. 2.99: Stammdaten-Einlesen: ab jetzt ohne Auswertung des *}
{*                          Inbetriebnahmezeitpunkts einer Instanz                      *}
{*          03.02.2005  GD  Vs. 2.991: Export-Zwischendatei eindeutig                   *}
{*          23.02.2005  GD  Vs. 2.992 Instanzname jetzt aus Tabelle                     *}
{*          10.05.2005  WW  V. 2.993 Stammdateneinlesen (Update): kein automatisches    *}
{*                          Löschen mehr von vorhandenen Stammdaten und abhängigen      *}
{*                          Archivdaten                                                 *}
{*          16.06.2005  WW  V. 2.994 Stammdaten-Einlesen: Gerätetyp-Erkennung erweitert *}
{*                          auf Instanzen vom Typ Q (RMG GC 9000-EMC, FlowComp Q1)      *}
{****************************************************************************************}
program Afdsfg32;

uses
  ShareMem,
  Forms,
  Fmain in 'V:\DELPHI32\Abruf\AFDSFG32\Fmain.pas' {FormMainDSfGAbruf},
  Pathini in 'V:\DELPHI32\Soft\Pathini.pas',
  Errconst in 'V:\DELPHI32\Dlls\ERRTXT32\Errconst.pas',
  FPrgBar in 'V:\DELPHI32\soft\Fprgbar.pas',
  DLLUtil in 'V:\DELPHI32\Soft\Dllutil.pas',
  ErrPrc32 in 'V:\DELPHI32\Soft\Errprc32.pas',
  Kwlink32 in 'V:\DELPHI32\soft\Kwlink32.pas',
  DSYSCON in 'V:\DELPHI32\Abruf\AFDSFG32\DSYSCON.PAS',
  Abrfinfo in 'V:\DELPHI32\Abruf\AFDSFG32\Abrfinfo.pas',
  ZustndDb in 'V:\DELPHI32\Abruf\Common\Zustnddb.pas',
  Wsyscon in 'V:\DELPHI32\Abruf\common\Wsyscon.pas',
  DB_Attn in 'V:\DELPHI32\Soft\Db_attn.pas',
  Dzustand in 'V:\DELPHI32\Abruf\AFDSFG32\Dzustand.pas',
  DMOINIT in 'V:\DELPHI32\Abruf\AFDSFG32\DMOINIT.PAS',
  DSFGUTIL in 'V:\DELPHI32\Abruf\Common\DSFGUTIL.PAS',
  ModemIni in 'V:\DELPHI32\Soft\ModemIni.pas',
  Dabrfman in 'V:\DELPHI32\Abruf\AFDSFG32\Dabrfman.pas',
  DDBABRUF in 'V:\DELPHI32\Abruf\Common\DDBABRUF.PAS',
  Journldb in 'V:\DELPHI32\Abruf\Common\Journldb.pas',
  ZSyncDb in 'V:\DELPHI32\Abruf\Common\Zsyncdb.pas',
  Zbdatdb in 'V:\DELPHI32\Abruf\Common\Zbdatdb.pas',
  AuftrgDb in 'V:\DELPHI32\Abruf\Common\Auftrgdb.pas',
  Termindb in 'V:\DELPHI32\Abruf\common\Termindb.pas',
  Wsysdat in 'V:\DELPHI32\Abruf\Common\Wsysdat.pas',
  Dsysdat in 'V:\DELPHI32\Abruf\AFDSFG32\Dsysdat.pas',
  DJOURNAL in 'V:\DELPHI32\Abruf\AFDSFG32\DJOURNAL.PAS',
  Ddbsta in 'V:\DELPHI32\Abruf\common\Ddbsta.pas',
  DABRUF in 'DABRUF.PAS',
  Dlisten in 'V:\DELPHI32\Abruf\Common\Dlisten.pas',
  UnixDT in 'V:\DELPHI32\soft\UnixDT.pas',
  DIWKONV in 'V:\DELPHI32\Abruf\AFDSFG32\DIWKONV.PAS',
  ProgIni in 'V:\DELPHI32\Abruf\AFDSFG32\ProgIni.pas',
  TBDSFGIW in 'V:\DELPHI32\Abruf\Common\TBDSFGIW.PAS',
  DALKONV in 'V:\DELPHI32\Abruf\AFDSFG32\DALKONV.PAS',
  RFilesDb in 'V:\DELPHI32\Abruf\Common\Rfilesdb.pas',
  DDELLIST in 'V:\DELPHI32\Abruf\AFDSFG32\DDELLIST.PAS',
  Dd_allg in 'V:\DELPHI32\Abruf\Common\Dd_allg.pas',
  Tbdsfgar in 'V:\DELPHI32\Abruf\Common\Tbdsfgar.pas',
  DKFKONV in 'V:\DELPHI32\Abruf\AFDSFG32\DKFKONV.PAS',
  DDLoesch in 'V:\DELPHI32\Abruf\Common\DDLoesch.pas',
  My_utils in 'V:\DELPHI32\soft\My_utils.pas',
  Djloesch in 'V:\DELPHI32\Abruf\Common\Djloesch.pas',
  LogCom in 'V:\DELPHI32\Soft\Logcom.pas',
  Serdsfg in 'V:\DELPHI32\Abruf\AFDSFG32\Serdsfg.pas',
  Tbdsfgde in 'V:\DELPHI32\Abruf\common\Tbdsfgde.pas',
  TBDSFGMO in 'V:\DELPHI32\Abruf\Common\TBDSFGMO.PAS',
  SrvCfgIni in 'V:\DELPHI32\Abruf\Common\Srvcfgini.pas',
  DValidAnswer in 'V:\DELPHI32\Abruf\AFDSFG32\DValidAnswer.PAS',
  DDfueParaList in 'V:\DELPHI32\Abruf\AFDSFG32\DDfueParaList.pas',
  Tbdsfgmomdfue in 'V:\DELPHI32\Abruf\Common\Tbdsfgmomdfue.pas',
  Tbdsfgdfuepe in 'V:\DELPHI32\Abruf\Common\Tbdsfgdfuepe.pas',
  DDfuParaNr in 'V:\DELPHI32\Abruf\Common\DDfuParaNr.pas',
  DbDSfGParams in 'V:\DELPHI32\Abruf\Common\DbDSfGParams.pas',
  DSG_Utils in 'V:\DELPHI32\DSFG\DSfG_Utils\DSG_Utils.pas',
  GD_Utils in 'V:\DELPHI32\Soft\GD_Utils.pas',
  DbDSfGMom in 'V:\DELPHI32\Abruf\common\DbDSfGMom.pas',
  DMomLists in 'V:\DELPHI32\DSFG\DSfG_Utils\DMomLists.pas',
  DAufmTelegr in 'V:\DELPHI32\Abruf\AFDSFG32\DAufmTelegr.pas',
  Telegrdb in 'V:\DELPHI32\Abruf\Common\Telegrdb.pas',
  RufeDb in 'V:\DELPHI32\Abruf\Common\Rufedb.pas',
  MeldungenDB in 'V:\DELPHI32\Abruf\Common\MeldungenDB.pas',
  MeldKonfigDb in 'V:\DELPHI32\Abruf\Common\MeldKonfigDb.pas',
  ExportSkript in 'V:\DELPHI32\Dlls\Ascii_Export\ExportSkript.pas',
  AsciiExportDLL in 'V:\DELPHI32\soft\AsciiExportDLL.pas',
  MrgMeldExportList in 'V:\DELPHI32\DLLs\ASCII_Export\MrgMeldExportList.pas',
  ExportTextDef in 'V:\DELPHI32\Dlls\Ascii_Export\ExportTextDef.pas',
  LGZType in 'V:\DELPHI32\Soft\Lgztype.pas',
  DALKonvDb in 'V:\DELPHI32\Abruf\AFDSFG32\DALKonvDb.pas',
  WComm in 'V:\DELPHI32\Abruf\common\WComm.pas',
  TCPIP_DSfG in 'V:\DELPHI32\Abruf\AFDSFG32\TCPIP_DSfG.pas',
  DKavKonv in 'V:\DELPHI32\DSFG\Kaverne\Konv\DKavKonv.pas',
  DKavKonfigDb in 'V:\DELPHI32\DSFG\Kaverne\Konv\DKavKonfigDb.pas',
  IecKonfDb in 'V:\DELPHI32\IEC\KONFIGURATION\IecKonfDb.pas',
  WResMeld in 'V:\DELPHI32\Abruf\common\WResMeld.pas',
  WResConst in 'V:\DELPHI32\Abruf\Common\WResConst.pas',
  AbrufTimeoutConst in 'V:\DELPHI32\Abruf\Common\AbrufTimeoutConst.pas',
  WXmlConst in 'V:\DELPHI32\Abruf\common\WXmlConst.pas',
  DSfGXmlDecodeObj in 'V:\DELPHI32\Abruf\Common\DSfGXmlDecodeObj.pas',
  DecodeResp in 'V:\DELPHI32\Abruf\Common\DecodeResp.pas',
  AbrufCmd in 'V:\DELPHI32\Abruf\WicomSrv\AbrufCmd.pas',
  TbdsfgarKav in 'V:\DELPHI32\DSFG\Kaverne\Konv\TbdsfgarKav.pas';

{$R *.RES}

begin
  if not InitLibraryErrTxt32 then exit;
  try
    Application.Initialize;
    Application.Title := 'DSfG-Abrufmodul';
    Application.CreateForm(TFormMainDSfGAbruf, FormMainDSfGAbruf);
  Application.Run;
  finally
    DoneLibraryErrTxt32;
  end;
end.
