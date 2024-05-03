{****************************************************************************************}
{* Programm: MRG-Abrufmodul (DFÜ) für Client/Server-Abrufsystem                         *}
{* Funktionen: - Abruf von MRGs mit und ohne DSfG-Umleitung über eine feste oder        *}
{*               beliebige serielle Schnittstelle                                       *}
{*             - automatischer und manueller Abruf von Meßwerten, Tagessätzen,          *}
{*               Meldungen, Prüfungssätzen und Parametern                               *}
{*             - Rufentgegennahme                                                       *}
{*             - Momentanwerte abrufen                                                  *}
{*             - Zeitsynchronisation                                                    *}
{*             - Rundpuffer zurücksetzen                                                *}
{*             - Journal mit Detailinfo über evtl. aufgetretene Fehler,                 *}
{*               PC/MRG-Zeiten vor Synchronisation, Zeitbereich der abgerufenen         *}
{*               Daten                                                                  *}
{*             - temporäre Aufzeichnung der aktuellen Abrufzustände                     *}
{*             - Momentanwerteabruf                                                     *}
{* Kommandozeilenparameter: - Schnittstelle (Com1..Com9)                                *}
{*                          - Comserve-Thema (FUP oder MODEM)                           *}
{*                          - Debug-Modus (DEBUG) optional -> kein Rohdatenlöschen      *}
{* Version: 23.03.1999  WW                                                              *}
{*          20.08.1999  WW  Kennungsprüfung nur noch bei Automatikabrufen,              *}
{*                          immer MRG-Kennung (statt Sta-Kennung) im Journal,           *}
{*                          ASCII-Konvertierung (ifdef ASCII),                          *}
{*                          diverse Mängelbehebungen                                    *}
{*          02.09.1999  WW  auf COM1..COM8 erweitert                                    *}
{*          08.09.1999  WW  Kanalzahl über Wieser.INI einstellbar, LGZ- und Manu-       *}
{*                          Meldungstabellen automatisch anlegen, Rufdeaktivierung,     *}
{*                          Rufreaktivierung                                            *}
{*          10.11.1999  WW  Parametrieren während Momentanwertsitzung,                  *}
{*                          Fehler Konvertierung manuelle Prüfsätze raus                *}
{*          18.01.2000  WW  Fehler Meldungsrohfile-Konvertierung raus                   *}
{*                          -> liefert falsche Journal-Warnung bei MRG 800PTB mit CRC   *}
{*          14.04.2000  WW  Zeitsynchronisation: Abweichung < Min ist keine Warnung mehr*}
{*                          Rufkennung-File im Work-Verzeichnis                         *}
{*          17.05.2000  WW  Fehlerbehebung für Momentanwert-/Parametrier-Tabellen       *}
{*          15.06.2000  WW  Erweiterungen für EC 694 					*}
{*	    29.06.2000  WW  Anpassungen für MRG 1009	             		    	*}
{*          11.07.2000  WW  Fehler beim Sortieren der Impuls- und Analogkanallisten raus*}
{*                          (Integerüberlauf beim Konvertieren)                         *}
{*          31.08.2000  WW  Befehlsausführung aus Binärfile (Automatik/manuell),        *}
{*                          Timeouts für Transparentbefehle (t:...) reduziert           *}
{*          13.10.2000  WW  Journal-Reorganisation beschleunigt,                        *}
{*                          Journaltabellen, Auftrags-/Zustands-/GesMeld-Tabellen re-   *}
{*                          strukturieren, Fehler Delay-Funktion raus                   *}
{*          01.02.2001  WW  V. 2.0: auf 16 COM erweitert;  			        *}
{*			    Comserve ersetzt durch TSerialMRG und Nachfahren;   	*}
{*                          Comserve.ini ersetzt durch SrvCfg32.ini + Modem.ini,        *}
{*                          keine Automatik-Deaktivierung mehr beim x-ten Fehlabruf oder*}
{*                          falscher Kennung bzw. falschem Passwort, RestructDaten raus,*}
{*                          manueller Abruf: alte Manu-Daten vor dem Verbindungsaufbau  *}
{*                          löschen, Fehler NetReset unter Delphi 5 raus,               *}
{*                          Meldungskonvertierung: Exklusivzugriff auf LGESMELD.DB und  *}
{*                          MGESMELD.DB, Verbindungsabbau mit Warten auf OK vom Modem,  *}
{*                          zusätzlicher Abruf der Eingangszähler für MRG 3113 und      *}
{*                          kompatible, optionaler ASCII-Export mit DLL (alte ASCII-    *}
{*                          Konvertierung mit IFDEFs ASCII und MELD5 entfallen dafür),  *}
{*                          Taskleisten-Icon, optische und akustische Warnung (Icon     *}
{*                          blinkt, ErrorBeep über ALM32.INI ein-/ausschaltbar)         *}
{*          22.03.2001  WW  V. 2.01: Fehler beim Empfang mit ACK0/1-Protokoll raus      *}
{*          17.07.2001  WW  V. 2.1: Umstellung auf gemeinsame MRG/DSfG-Meldungstabellen *}
{*                          und Meldungskonfigurations-Tabellen; Stations-Aktivierung/  *}
{*                          Deaktivierung nach letztem Anrufversuch über Systemdaten-   *}
{*                          einstellung; Freischaltung der Rufentgegennahme über        *}
{*                          Lizenzfile                                                  *}
{*          17.08.2001  WW  V. 2.2: Einbau MRG 910; beschleunigtes Lesen von der        *}
{*                          seriellen Schnittstelle bei FUP-Abruf (sonst Pufferüberlauf *}
{*                          im FUP)                                                     *}
{*          28.08.2001  WW  V. 2.21: Fehler bei Zeitsynchronisation raus (in der zu     *}
{*                          übertragenden neuen MRG-Zeit war der SZ-Stundenoffset nicht *}
{*                          berücksichtigt)                                             *}
{*          25.09.2001  WW  V. 2.22: Anpassungen für FUP-1200 (FUP-Reset, Timeout für   *}
{*                          Verbindungsabbau)                                           *}
{*          07.11.2001  WW  V. 2.23: Fehler in GetNextTermin behoben (Query für MRG-Grp)*}
{*          19.11.2001  WW  V. 2.24: in SerMrgModem.pas Gerätekonfiguration-Ermitteln   *}
{*                          für MRG800 PTB korrigiert                                   *}
{*          17.01.2002  WW  V. 2.25: Delay nach Verbindungsaufbau; Gerät wecken bei     *}
{*                          Modemabruf geändert (Problem mit Break bei Verbindung mit   *}
{*                          Funkmodem), Konvertierung EC694-Meldungen mit Prüfung auf   *}
{*                          plausibles Datum                                            *}
{*          11.03.2002  WW  V. 2.26: Modem-Antworten werden jetzt richtig gelesen und   *}
{*                          ausgewertet unabhängig von der Anzahl der enthaltenen CR LF *}
{*                          (war Problem u.a. bei internen Toshiba-Notebook-Modems)     *}
{*          23.04.2002  WW  V. 2.3: mit Meldungs-Alarmtabelle; Log-Filename mit voll-   *}
{*                          ständiger Pfadangabe (Exe-Pfad)                             *}
{*          17.05.2002  WW  V. 2.31: nur Quellcode-Umstrukturierungen (Langzeitdaten)   *}
{*          06.09.2002  WW  V. 2.32: Korrektur Parameterkonvertierung (Leer-Werte wurden*}
{*                          bislang nicht abgespeichert); CRC-Prüfung verfeinert wg.    *}
{*                          Abrufproblem EC694; CRC-Prüfung funktioniert jetzt auch,    *}
{*                          wenn Gerät (EC694 !) CRC als Kleinbuchstaben schickt;       *}
{*                          Variablenübergabe-Änderung in LOGCOM.PAS                    *}
(*	    17.09.2002	H.-P.R. FUP Reset auch mit RTS Signal				*)
{*	    07.10.2002	WW  V. 2.4 Einbau Elster DL240 und EK260 (IFDEF NATGAS liefert  *}
{*                          ASCII-Daten im natGAS-Format (ohne ASCII-DLL !)             *}
{*	    23.10.2002	WW  V. 2.5 Einbau Parameter- und Momentanwert-Abruf für Elster  *}
{*                          DL240 und EK260; LGZ-Daten Elstergeräte: Rückrechnung phys. *}
{*                          Gerätedaten in LGZ-Rohwerte über Geräteparameter (cp-Werte, *}
{*                          Meßbereiche) statt Stammdateneinstellungen; Einbau Rufent-  *}
{*                          gegennahme für MRG 910 -> Modem-Rufentgegennahme-Abwicklung *}
{*                          von Modem- oder FUP-Geräten je nach SRVCFG32-Einstellung;   *}
{*                          alle Meldungstabellen mit Exklusivzugriff und Flush bei     *}
{*                          Schreib-/Löschvorgängen                                     *}
{*	    04.11.2002	WW  V. 2.51  Abruf MRG 910 über Stammsatz MRG 800PTB:           *}
{*                          MW-Konvertierung lieferte bisher falsche Werte (andere Roh- *}
{*                          datenstruktur !), ab jetzt keine MW/TA-Konvertierung mehr   *}
{*          07.03.2003  WW  V. 2.52: bis-Datum bei Tagessatz-Abfrage um 1 Tag erhöht wg.*}
{*                          MRG910; Quellcode-Umstrukturierungen (Daten-Konvertierungen,*}
{*                          Aufbereitung, Zeitsynchronisation)                          *}
{*	    09.04.2003	WW  V. 2.53:  Konvertierung 13er-Serie: Stunde für Tagessätze   *}
{*                          korrigiert (25 -> richtige Stunde); Fehler Messwert-Abruf   *}
{*                          DL240, EK260 behoben (falsche Fehlermeldung "Fehlende Kon-  *}
{*                          figurationsdaten"); Fehler ETX-Prüfung beim Datenempfang mit*}
{*                          IEC1107-Protokoll korrigiert (DL240, EK 260)                *}
{*	    11.04.2003	WW  V. 2.54: Fehlerbehebungen für DL 240/EK260: Break-Zustand   *}
{*                          wird bei Modem-Kommunikation nicht mehr als Fehler behandelt*}
{*                          Fehler in Routine zum Schreiben der Rohdatendatei in        *}
{*                          TSerialMRG behoben (Zeichen STRG-Z wurde überschrieben)     *}
{*	    23.05.2003  GD  V. 2.55 Anpassung ASCII-Export an geändertes Rohdatenformat *}
{*          30.07.2003  WW / H.-P.R. V. 2.56 Gruppenoffset berücksichtigen              *}
{*          15.01.2004  WW  V. 2.57: Filezugriffe jetzt threadsicher mit TFileStream    *}
{*          11.07.2005  WW  V. 2.58: mit Überprüfung des Befehlsbuchstabens in Antwort  *}
{*                          von Wieser MRGs (wg. MRG 910 mit Weckantwort auf Kennung-   *}
{*                          Abfrage)                                                    *}
{*          26.07.2005  WW  V. 2.59: mit Auswertung der Baudraten-Identifizierung (IEC  *}
{*                          1107-Protokoll) für neue Geräteversionen Elster EK260       *}
{****************************************************************************************}

program AfMRG32;

uses
  ShareMem,
  Forms,
  GVTMKONV in 'V:\DELPHI32\Abruf\AFMRG32\GVTMKONV.PAS',
  progini in 'V:\DELPHI32\Abruf\AFMRG32\progini.pas',
  Upumfo in 'V:\DELPHI32\soft\Upumfo.pas',
  Mfilenam in 'V:\DELPHI32\Abruf\AFMRG32\Mfilenam.pas',
  Mlgzaufb in 'V:\DELPHI32\Abruf\AFMRG32\Mlgzaufb.pas',
  MLgzkonv in 'V:\DELPHI32\Abruf\AFMRG32\MLgzkonv.pas',
  MOBJKONV in 'V:\DELPHI32\Abruf\AFMRG32\MOBJKONV.PAS',
  MObjList in 'V:\DELPHI32\Abruf\AFMRG32\Mobjlist.pas',
  Mobjmeld in 'V:\DELPHI32\Abruf\AFMRG32\Mobjmeld.pas',
  Mobjpara in 'V:\DELPHI32\Abruf\AFMRG32\Mobjpara.pas',
  MP_Imp in 'V:\DELPHI32\Abruf\common\Mp_imp.pas',
  Mabrfman in 'V:\DELPHI32\Abruf\AFMRG32\Mabrfman.pas',
  MValidAnswer in 'V:\DELPHI32\Abruf\AFMRG32\MValidAnswer.PAS',
  Mstartda in 'V:\DELPHI32\Abruf\AFMRG32\Mstartda.pas',
  MABRFZB in 'V:\DELPHI32\Abruf\AFMRG32\MABRFZB.PAS',
  WSYSCON in 'V:\DELPHI32\Abruf\Common\WSYSCON.PAS',
  Mdbabruf in 'V:\DELPHI32\Abruf\Common\Mdbabruf.pas',
  Mp_allg in 'V:\DELPHI32\Abruf\common\Mp_allg.pas',
  Mdtformt in 'V:\DELPHI32\Abruf\AFMRG32\Mdtformt.pas',
  MeldKonfigDb in 'V:\DELPHI32\Abruf\Common\MeldKonfigDb.pas',
  Kwlink32 in 'V:\DELPHI32\soft\Kwlink32.pas',
  Zustnddb in 'V:\DELPHI32\Abruf\common\Zustnddb.pas',
  Abrfinfo in 'V:\DELPHI32\Abruf\AFMRG32\Abrfinfo.pas',
  DLLUtil in 'V:\DELPHI32\soft\Dllutil.pas',
  Mjournal in 'V:\DELPHI32\Abruf\AFMRG32\Mjournal.pas',
  ZBDatDb in 'V:\DELPHI32\Abruf\common\Zbdatdb.pas',
  Zsyncdb in 'V:\DELPHI32\Abruf\common\Zsyncdb.pas',
  Journldb in 'V:\DELPHI32\Abruf\Common\Journldb.pas',
  Msysdat in 'V:\DELPHI32\Abruf\AFMRG32\Msysdat.pas',
  ErrPrc32 in 'V:\DELPHI32\soft\Errprc32.pas',
  Db_attn in 'V:\DELPHI32\SOFT\Db_attn.pas',
  My_utils in 'V:\DELPHI32\Soft\My_utils.pas',
  Auftrgdb in 'V:\DELPHI32\Abruf\common\Auftrgdb.pas',
  Fmain in 'V:\DELPHI32\Abruf\AFMRG32\Fmain.pas' {FormMainMRGAbruf},
  Pathini in 'V:\DELPHI32\Soft\Pathini.pas',
  Termindb in 'V:\DELPHI32\Abruf\Common\Termindb.pas',
  FPrgBar in 'V:\DELPHI32\Soft\FPrgBar.pas' {FormProgressBar},
  Mdbsta in 'V:\DELPHI32\Abruf\Common\Mdbsta.pas',
  Mdbmrg in 'V:\DELPHI32\Abruf\Common\Mdbmrg.pas',
  MABRUF in 'V:\DELPHI32\Abruf\AFMRG32\MABRUF.PAS',
  Mzustand in 'V:\DELPHI32\Abruf\AFMRG32\Mzustand.pas',
  Errconst in 'V:\DELPHI32\Dlls\Errtxt32\Errconst.pas',
  WSysDat in 'V:\DELPHI32\Abruf\common\Wsysdat.pas',
  Mp_dfue in 'V:\DELPHI32\Abruf\common\Mp_dfue.pas',
  Rufedb in 'V:\DELPHI32\Abruf\common\Rufedb.pas',
  TbMrgpe in 'V:\DELPHI32\Abruf\Common\TbMrgpe.pas',
  RFilesDb in 'V:\DELPHI32\Abruf\Common\Rfilesdb.pas',
  Mlgzverw in 'V:\DELPHI32\Abruf\AFMRG32\Mlgzverw.pas',
  SrvCfgIni in 'V:\DELPHI32\Abruf\Common\Srvcfgini.pas',
  Sermrg in 'V:\DELPHI32\Abruf\AFMRG32\Sermrg.pas',
  Logcom in 'V:\DELPHI32\Soft\Logcom.pas',
  SerMrgFup in 'V:\DELPHI32\Abruf\AFMRG32\SerMrgFup.pas',
  SerMrgModem in 'V:\DELPHI32\Abruf\AFMRG32\SerMrgModem.pas',
  Mmodeminit in 'V:\DELPHI32\Abruf\AFMRG32\Mmodeminit.pas',
  ModemIni in 'V:\DELPHI32\Soft\ModemIni.pas',
  Msyscon in 'V:\DELPHI32\Abruf\AFMRG32\Msyscon.pas',
  MDBParam in 'V:\DELPHI32\Abruf\Common\MDBParam.pas',
  MrgBefehl in 'V:\DELPHI32\Abruf\AFMRG32\MrgBefehl.pas',
  CRC16 in 'V:\DELPHI32\Soft\Crc16.pas',
  MDatensatzList in 'V:\DELPHI32\Abruf\AFMRG32\MDatensatzList.pas',
  ExportSkript in 'V:\DELPHI32\Dlls\Ascii_Export\ExportSkript.pas',
  AsciiExportDLL in 'V:\DELPHI32\Soft\AsciiExportDLL.pas',
  GD_Utils in 'V:\DELPHI32\Soft\GD_Utils.pas',
  MrgMeldExportList in 'V:\DELPHI32\Dlls\Ascii_Export\MrgMeldExportList.pas',
  MeldungenDB in 'V:\DELPHI32\Abruf\Common\MeldungenDB.pas',
  DListen in 'V:\DELPHI32\Abruf\Common\Dlisten.pas',
  ExportTextDef in 'V:\Delphi32\Dlls\Ascii_Export\ExportTextDef.pas',
  Tbdsfgar in 'V:\DELPHI32\Abruf\Common\Tbdsfgar.pas',
  LGZUtil in 'V:\DELPHI32\soft\LGZUtil.pas',
  Lgztype in 'V:\DELPHI32\soft\Lgztype.pas',
  COM_Utils in 'V:\DELPHI32\Soft\COM_Utils.pas',
  MLetztLGZ in 'V:\DELPHI32\Abruf\AFMRG32\MLetztLGZ.pas',
  MP_Boritec in 'V:\DELPHI32\Abruf\Common\MP_Boritec.pas',
  MP_Elster in 'V:\DELPHI32\Abruf\Common\MP_Elster.pas',
  MLgzKonvFremd in 'V:\DELPHI32\Abruf\AFMRG32\MLGZKonvFremd.pas',
  MP_Tritschler in 'V:\DELPHI32\Abruf\Common\MP_Tritschler.pas',
  DTUtils in 'V:\DELPHI32\soft\DTUtils.pas',
  UnixDT in 'V:\DELPHI32\Soft\UnixDT.pas',
  DSfGUtil in 'V:\DELPHI32\Abruf\Common\DSfGUtil.pas',
  DALKonv in 'V:\DELPHI32\Abruf\Afdsfg32\DALKonv.pas',
  MLgzKonvList in 'V:\DELPHI32\Abruf\AFMRG32\MLgzkonvList.pas',
  MResMrg in 'V:\DELPHI32\Abruf\Common\MResMrg.pas',
  MResParam in 'V:\DELPHI32\Abruf\Common\MResParam.pas',
  MObjMeldDB in 'V:\DELPHI32\Abruf\AFMRG32\MObjMeldDb.pas',
  MObjParaDb in 'V:\DELPHI32\Abruf\AFMRG32\MObjParaDb.pas',
  WComm in 'V:\DELPHI32\Abruf\Common\WComm.pas',
  TelegrDb in 'V:\DELPHI32\Abruf\Common\TelegrDb.pas',
  CrcSealink in 'V:\Delphi32\soft\CrcSealink.pas',
  WResConst in 'V:\DELPHI32\Abruf\Common\WResConst.pas',
  WResMeld in 'V:\DELPHI32\Abruf\common\WResMeld.pas',
  MP_Datacon in 'V:\DELPHI32\Abruf\Common\MP_Datacon.pas',
  WXmlConst in 'V:\Delphi32\Abruf\common\WXmlConst.pas',
  DecodeResp in 'V:\Delphi32\Abruf\Common\DecodeResp.pas',
  AbrufTimeoutConst in 'V:\Delphi32\Abruf\common\AbrufTimeoutConst.pas';

{$R *.RES}

begin
  if not InitLibraryErrTxt32 then exit;
  try
    Application.Initialize;
    Application.Title := 'MRG-Abrufmodul';
    Application.CreateForm(TFormMainMRGAbruf, FormMainMRGAbruf);
  Application.Run;
  finally
    DoneLibraryErrTxt32;
  end;  
end.
