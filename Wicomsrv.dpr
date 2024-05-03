{******************************************************************************}
{* Dienstanwendung: Abrufserver f�r MRG- und DSfG-Abrufe (BEB)                *}
{* Funktionen: - Kommunikation mit Client-Systemen �ber TCP/IP-Socket-Schnitt-*}
{*               stelle                                                       *}
{*             - Daten�bergabe zwischen Server und Client (Request/Response)  *}
{*               im XML-Format                                                *}
{*             - keine Datenbank-Anbindung                                    *}
{*             - Abruf von MRG- und DSfG-Stationen �ber wahlfreie COM-Schnitt-*}
{*               stelle                                                       *}
{*             - MRG: Abruf von Me�werten, Tagess�tzen, Meldungen, Pr�fungs-  *}
{*                    s�tzen und Parametern                                   *}
{*             - DSfG: Abruf von Archivdaten, Logb�chern, Datenelementen und  *}
{*                     Busanalysedaten                                        *}
{* Version: 19.02.2003  WW  V. 1.0                                            *}
{*	    12.03.2003	WW  V. 1.1  MRG-Abruf mit DSfG-Umleitung; Zuordnung       *}
{*                          logische Schnittstelle -> COM-Port-Nr. �ber INI;  *}
{*                          Zeitsynchronisation f�r MRG-Abruf; Programm-      *}
{*                          Lizenz mit Laufzeitbeschr�nkung; Online-Hilfe;    *}
{*                          IFDEF 'NO_XMLHEADERLENGTH_CHECK' zum Ausschalten  *}
{*                          der XML-Header-L�ngenpr�fung                      *}
{*	    08.04.2003	WW  V. 1.12  MRG-Konvertierung 13er-Serie: Stunde f�r     *}
{*                          Tagess�tze korrigiert (25 -> richtige Stunde);    *}
{*                          DSfG-Abruf: Blockungsgr��e f�r Anforderungstele-  *}
{*                          gramm mit mehreren Datenelementen reduziert (sonst*}
{*                          Verbindungsabbruch); globale Exception-Behandlung *}
{*                          in Execute-Methode des Abruf-Threads              *}
{*	    18.07.2003	WW  V. 1.2  Ger�tetypen KE-Ruhrgas, KE-PPN und Datacon    *}
{*                          FWU aufgenommen; falsche Fehlerr�ckgabe "COM nicht*}
{*                          frei" beim Verbindungsaufbau korrigiert (Ursache: *}
{*                          Verbindungsunterbrechung bei v-Kommando, jetzt    *}
{*                          "sonstiger Fehler"); Zusammensetzen des XML-String*}
{*                          beschleunigt                                      *}
{*	    13.10.2003	WW  V. 1.21  Ger�tetyp KE-PPN: Messwert-Datenr�ckgabe     *}
{*                          erweitert um Kan�le f�r Normdichte-Status und     *}
{*                          Brennwert-Status; DSfG-Abruf, Verbindungsaufbau:  *}
{*                          jetzt mit Status-R�ckgabe beim Lesen der DSfG-DF�-*}
{*                          Versionsdaten und der DSfG-DF�-Adressen           *}
{*	    15.01.2004	WW  V. 1.3  Zeitfilter f�r Datenr�ckgabe;                 *}
{*                          Timer zum Nachstarten von Abrufthreads; alle      *}
{*                          Filezugriffe jetzt threadsicher mit TFileStream;  *}
{*                          Content-Length in XML-R�ckgaben berichtigt;       *}
{*                          Timeout f�r DSfG-Konfiguration-Einlesen entf�llt, *}
{*                          daf�r allg., verk�rzter Timeout f�r Datenelemente-*}
{*                          Lesen; Strukturerweiterung MRGDEF.DAT;            *}
{*	    05.02.2004	WW  V. 1.31  Fehler beim Rohfiles-Zusammenfassen in       *}
{*                          Messwert-Konvertierung MRG3113 behoben            *}
{*	    31.03.2004	WW  V. 1.4  Erweiterungen/Anpassungen f�r WK22-System;    *}
{*                          SRVCFG32.INI ersetzt ABRUFCFG.INI;                *}
{*                          COM offen halten, wenn FUP angeschlossen;         *}
{*                          Einbau Tritschler VC2 und TTG (IEC-Protokoll);    *}
{*                          Erweiterungen MRG: Messwertabruf mit Kanalmaske,  *}
{*                          Rundpufferreset f�r Meldungen und Messwerte, Para-*}
{*                          metrierung, mit allen MRG-Typen wie im WICOM;     *}
{*                          Erweiterungen DSfG: Archiv-/Logbuchdatenabruf �ber*}
{*                          Ordnungsnummer, R�ckgabe Konv-Errorcode f�r       *}
{*                          Archivkanal/Logbuch/Datenelemente;                *}
{*                          alle Messagebox-Ausgaben ersetzt durch Ausgabe in *}
{*                          Programm-Fenster und Logfile WICOMSRV.LOG;        *}
{*                          DSfG-Abruf MRG910 �ber Zeit mit Status im Kommando*}
{*                          (Ger�tefehler: Ordnungsnummern in den gelieferten *}
{*                          Archivdaten falsch); Speicherleck (FillChar) be-  *}
{*                          seitigt;                                          *}
{*                          IFDEF GAS-X f�r alle speziellen Gas-X-Funktionali-*}
{*                          t�ten und zum Ausblenden aller WK22-Erweiterungen *}
{*                          bzgl. Client-Requests und Datenr�ckgaben;         *}
{*                          mit Zuordnungsressource "MRG-Typnummer Gas-X ->   *}
{*                          MRG-Typnummer Wieser"                             *}
{*	    08.04.2004	WW  V. 1.41  Fehler in Daten-Konvertierungen f�r KE-      *}
{*                          Stationen behoben (falscher Zeitstempel wegen     *}
{*                          Schaltjahr 2004, Verschiebung um 1 Tag)           *}
{*	    15.04.2004	WW  V. 1.42  Fehler in  Messwertkonvertierung f�r TTG     *}
{*                          mit IEC-Protokoll behoben (Rohdatei f�r Kanal 2   *}
{*                          wurde nicht gel�scht)                             *}
{*	    04.05.2004	WW  V. 1.43  Fehler Kanalmaske bei MW/TA-Abruf behoben    *}
{*                          (trat in Gas-X-Version auf); Erweiterung f�r TTG  *}
{*                          mit IEC-Protokoll: 1-Kanal-Version erkennen, Kon- *}
{*                          vertierung der Z�hlerst�nde des letzten Monatsab- *}
{*                          schlusses                                         *}
{*          21.07.2004  WW  V. 1.44 Auswertung au�erplanm��iger DSfG-Antworten*}
{*          16.08.2004  WW  V. 1.5 mit Zeitsynchronisation der DSfG-DF� bei   *}
{*                          DSfG-Abruf; Modemantwort 'BLACKLISTED' auswerten  *}
{*          23.08.2004  WW  V. 1.51 mit Korrektur Gastagbildung bei TTG-IEC   *}
{*                          (TTG ab Vs. 7.x liefern bereits Gastag bei Monats-*}
{*                          abschlu�-Z�hlerstand)                             *}
{*          09.10.2004  WW  V. 1.52 Versionsinfos jetzt enthalten             *}
{*          29.10.2004  WW  V. 1.53 WNET_LIZ.DAT, SRVCFG32.INI, MODEM.INI �ber*}
{*                          WNetProgDir, Log-Files �ber ParamStr(0) ansprechen*}
{*          19.11.2004  WW  V. 1.6 mit SMS-Datenempfang (�ber Lizenz); DSfG-  *}
{*                          Stationsabrufe �ber TCP/IP                        *}
{*          16.12.2004  WW  V. 1.61 mit MRG 4210                              *}
{*          21.12.2004  WW  V. 1.62 mit DSfG-Datenelemente-Parametrierung     *}
{*          12.01.2005  WW  V. 1.63 mit DSfG-DF�-Momentanwertabruf/Parametrie-*}
{*                          rung; Erweiterung Server-Verbindungsaufbau-       *}
{*                          Kommando um Feld "DSfG-Transparentmodus"          *}
{*          01.02.2005  WW  V. 1.64 Kommando-Wartezeitmp_ verkleinert (schnellere*}
{*                          Reaktion auf neues Client-Kommando !)             *}
{*          16.02.2005  WW  V. 2.0 mit Rufentgegennahme; Responseteil der XML-*}
{*                          Parameter-Antwort f�r Gas-X jetzt ohne Feld 'Wert'*}
{*          11.04.2005  WW  V. 2.01 mit Lesen der Ger�teversion bei MRG800PTB-*}
{*                          Messwertabruf (nur Gas-X-Variante); Korrektur MW- *}
{*                          Konvertierung MRG 800, MRG 800PTB, EC 694 (Byte-  *}
{*                          �berlauf bei 120 K, Gas-X)                        *}
{*          28.04.2005  WW  V. 2.02 Rufentgegennahme: zus�tzliche sofortige   *}
{*                          Meldung an Client, wenn Ruf ansteht               *}
{*          11.07.2005  WW  V. 2.03: mit �berpr�fung des Befehlsbuchstabens in*}
{*                          Antwort von Wieser MRGs (wg. MRG 910 mit Weck-    *}
{*                          antwort auf Kennung-Abfrage); Stellenzahl f�r Ruf-*}
{*                          nummer in Verbindungsaufbau-Kommando vergr��ert;  *}
{*                          Bugfix Parameterabruf IEC 1107-Protokoll (Elster  *}
{*                          DL240, EK260)                                     *}
{*          26.07.2005  WW  V. 2.04: mit Auswertung der Baudraten-Identifizie-*}
{*                          rung (IEC 1107-Protokoll) f�r neue Ger�teversionen*}
{*                          Elster EK260                                      *}
{*          04.08.2005  WW  V. 2.05: Erweiterte Plausibilit�tspr�fung bei SMS-*}
{*                          Datenkonvertierung Veribox                        *}
{*          21.09.2005  GD  V. 2.06: Fehlertoleranz bei DSfG-Telegrammen      *}
{*          05.10.2005  WW  V. 2.07: Timeout f�r Datenabruf Tritschler-IEC-   *}
{*                          Protokoll erh�ht (f�r VC2 Vs. 3.xx)               *}
{*          14.11.2005  WW  V. 2.1:  Wieser-MRG-Abruf von Originalwerten;     *}
{*                          Senden der Server-Antwort umgestellt (jetzt �ber  *}
{*                          gemerkten Zeiger auf Client-Socket statt Port und *}
{*                          Adresse); mit Elster DL 220                       *}
{*          06.12.2005  WW  V. 2.2: PIN-Login f�r GSM-Modems jetzt auch bei   *}
{*                          MRG-/DSfG-Abrufen und Rufentgegennahme; PIN-Sperre*}
{*                          in Schnittstellenkonfigurtion setzen bei Login-   *}
{*                          Versuch mit falscher PIN; Gas-X-Version: log.     *}
{*                          Schnittstellen ab 51 als IP-Abruflinien           *}
{*          02.02.2006  WW  V. 2.21: Abruf IEC 1107-Ger�te alternativ mit     *}
{*                          Schnittstellen-Parametern '8N1' (f�r Elster DL240,*}
{*                          EK260)                                            *}
{*          15.03.2006  WW  V. 2.22: MRG 905: Korrektur MRG 910-Konvertierung *}
{*                          (Zeitstempel und Satzstatus Tagess�tze); DSfG-    *}
{*                          Abrufkommando mit Status wie bei MRG 910          *}
{*          25.04.2006  WW  V. 2.3: serielle Datenformate 8N1, 7E1 f�r MRG-   *}
{*                          Abruf einstellbar �ber Feld 'Modemtyp' im Verbin- *}
{*                          dungsaufbau-Kommando; MRG-Zeitsynchronisation f�r *}
{*                          Elster DL 220/240, EK 260                         *}
{*          05.05.2006  WW  V. 2.31: Standardwert f�r min. Abweichung bei     *}
{*                          Zeitsynchronisation jetzt 10s (Gas-X)             *}
{*          07.08.2006  WW  V. 2.4: MRG 905/910-SMS-Daten (Formate v, r ->    *}
{*                          DSfG-Archivdaten/Logbuchdaten); Bugfix: Endlos-   *}
{*                          schleife bei Modeminitialisierung                 *}
{*          11.10.2006  WW  V. 2.5: FUP-Abruf mit/ohne RTS-Handshake �ber     *}
{*                          SRVCFG32.INI; gesicherter SMS-Empfang bei Problem *}
{*                          in Client-Weiterverarbeitung (SMS-Dateien als     *}
{*                          Zwischenpuffer, Empfang einer Client-Best�tigung  *}
{*                          bei erfolgreicher SMS-Verarbeitung)               *}
{*          31.10.2006  WW  V. 2.51: vor FUP-Reset COM schlie�en und wieder   *}
{*                          �ffnen (wg. Problem mit FUP-Betrieb an ComServer- *}
{*                          Schnittstellen)                                   *}
{*          10.01.2007  WW  V. 2.52: mit Fehlerkonstanten 'TCPIP_ERR_LOOKUP', *}
{*                          'TCPIP_ERR_UNDEFINIERT' bei DSfG-IP-Kommunikation *}
{*          23.01.2007  WW  V. 2.53: Bugfix 'Ung�ltiges Thread-Handle' beim   *}
{*                          Beenden (TThread.WaitFor in CloseAbrufControls)   *}
{*          14.03.2007  WW  V. 2.54: erweiterte Pr�fungen f�r DSfG-Archiv-,   *}
{*                          Logbuch- und Datenelemente-Rohdaten (Telegramm-   *}
{*                          syntax, plausible Telegramminhalte, Pr�fung der   *}
{*                          korrekten Zuordnung zu abgefragter DE-Adresse     *}
{*                          jetzt auch bei Logbuchdaten); mit Wiederholung bei*}
{*                          ung�ltigen Archiv-, Logbuch- und Datenelemente-   *}
{*                          rohdaten (Fehler ERZ2000, Vs. 1.3.1); MRG-Abruf   *}
{*                          Elster DL220/240, EK260 mit CL-Schnittstelle      *}
{*          05.04.2007  WW  V. 2.55: nochmal DSfG-Rohdatenpr�fung: Fehler beim*}
{*                          Bilden von Ordnungsnummer/Zeit/DE-Adresse f�r     *}
{*                          Folgeabruf abgefangen und erweiterte Fehlercodes; *}
{*                          f�r GAMESS: XML-Responselog erweitert auf Rohfile *}
{*                          namenliste;                                       *}
{*          16.05.2007  WW  V. 2.56: schon wieder DSfG-Rohdatenpr�fung:       *}
{*                          Pr�fung von Datenelemente-Rohdaten auf Felder     *}
{*                          Unixzeit, Ordnungsnummer, Status und CRC erweitert*}
{*                          (MRG 910-Fehler bei DE-Adressen ca??d), Schreiben *}
{*                          von WicomSrv_DataErr.log (unplausible Rohdaten)   *}
{*                          �ber INI-Schalter COMProtokoll ein-/ausschaltbar; *}
{*                          Abruf Tritschler VC2 �ber Multiplexer SSU K935;   *}
{*                          VC2-Messwerte mit aktuellen Z�hlerst�nden zum Aus-*}
{*                          lesezeitpunkt (nur Gas-X-Version); COM-Logfile mit*}
{*                          BCC/CRC-Fehlerprotokollierung                     *}
{*          04.06.2007  WW  V. 2.57: Login f�r Tritschler Multiplexer SSU mit *}
{*                          Wiederholungen                                    *}
{*          05.06.2007  WW  V. 2.58: MRG 910-Abruf mit 8 Kan�len (4 Analog)   *}
{*          04.07.2007  WW  V. 2.59: Abruf Tritschler Multiplexer SSU: jetzt  *}
{*                          �ber Modemtyp 'T' im Verbindungsaufbau-Kommando   *}
{*                          und max. 4 Loginversuche                          *}
{*          10.07.2007  WW  V. 2.60: auch bei Kommunikationsfehler werden alle*}
{*                          korrekt empfangenen DSfG-Teilantwort-Telegramme   *}
{*                          einer DE-Adresse konvertiert                      *}
{*          04.10.2007  WW  V. 2.61: mit Meldung an zentrale System�berwachung*}
{*                          (OK, Modem-Initialisierungsfehler)                *}
{*          05.11.2007  WW  V. 2.62: resourcestrings                          *}
{*          03.03.2008  WW  V. 2.7: Veribox SMS-Datenkonvertierung erweitert  *}
{*                          f�r Analogkan�le; MRG-FUP-Abruf: Empfangsdaten    *}
{*                          blockweise in Rohfile/Logfile schreiben; Bugfix   *}
{*                          Passwort-Login f�r KE-Anlagen: ohne MRG-Antwort-  *}
{*                          validierung; Abruf Tritschler TTG mit FTL-Prot.;  *}
{*                          Abruf Tritschler TDS; Abruf Tritschler TTG-IEC,   *}
{*                          TDS �ber Multiplexer SSU K935; Bugfix Konvertier- *}
{*                          ung TTG-IEC: 24-Uhr-Zeitstempel bei Z�hlerst�nden;*}
{*                          �berarbeitung SMS-Empfang: Weitergabe der SMS-    *}
{*                          Daten an Client erst, wenn alle SMS aus Modem ge- *}
{*                          lesen wurden (evtl. mehrere Lesevorg�nge)         *}
{*          04.03.2008  WW  V. 2.71: optionaler Kennungsvergleich ohne Kenn-  *}
{*                          ungserweiterung bei Tritschler Multiplexerger�ten *}
{*                          (WicomSrv.ini)                                    *}
{*          28.03.2008  WW  V. 2.72: Abruf Tritschler TTG-FTL �ber Multiplexer*}
{*                          SSU K935; Bugfix SMS-Konvertierung Veribox: neg.  *}
{*                          Z�hlerstandsdifferenz als �berlauf kennzeichnen   *}
{*                          (Stundenwert); optionaler Kennungsvergleich ohne  *}
{*                          Kennungserweiterung auch bei Nicht-Multiplexer-   *}
{*                          ger�ten; Bugfix Kennungsr�ckgabe bei DSfG-Umlei-  *}
{*                          tung: Default ist Kennung aus Umschaltkommando    *}
{*                          statt Vorg�nger-Kennung                           *}
{*          10.04.2008  WW  V. 2.73: Tritschler-Abruf �ber Multiplexer: bei   *}
{*                          Umschaltung des Ausgangs Abschaltbefehl (bei FTL- *}
{*                          Protokoll) und Break-Befehl senden (Bugfix), Abruf*}
{*                          mit 8N1 (kombinierte Modemtyp-Konstanten 'TA');   *}
{*                          Fehlercode LOGINERR_TIMEOUT_WRONGPW bei Login     *}
{*          30.04.2008  WW  V. 2.74: Wecksequenz f�r Tritschler Ger�te mit    *}
{*                          IEC-Protokoll �berarbeitet                        *}
{*          16.06.2008  WW  V. 2.75: mit Rufentgegennahme und R�ckrufausl�sung*}
{*                          f�r Tritschler Stationsrechner; mit Zeitsynchroni-*}
{*                          sation f�r Tritschler VC2, TDS                    *}
{*          23.06.2008  WW  V. 2.76: mit Zeitsynchronisation f�r EC694;       *}
{*                          Bugfix Integer�berlauf bei Veribox SMS (Datum in  *}
{*                          in Rohdaten)                                      *}
{*          26.06.2008  WW  V. 2.77: Bugfix "Wert fehlend" bei Konvertierung  *}
{*                          von TDS-Z�hlerst�nden in normierte Stundenmengen  *}
{*          08.09.2008  WW  V. 2.78: mit Actaris Corus                        *}
{*          16.10.2008  WW  V. 2.79: Erweiterung Server-Transparent-Kommando  *}
{*                          um Felder "Timeout" und "EmpfModus"; Bugfix:      *}
{*                          TSerialMRGModem.WakeUpMRG_ByCommand (Endlos-      *}
{*                          schleife Y-Befehl); Actaris Corus: mit zus�tz-    *}
{*                          lichem Neu-Initialisieren der Ger�tekommunikation *}
{*                          bei Timeout w�hrend des Abrufs; Abruf Parameter-  *}
{*                          Logbuch und eichtechnisches Logbuch;              *}
{*                          Vorbereitung Actaris Sparklog                     *}
{*          03.12.2008  WW  V. 2.80: mit Actaris Sparklog und Elster DL210;   *}
{*                          mit max. Korrekturzeit bei Zeitsynchronisation    *}
{*                          (optional); Bugfix Login-Rohdatei l�schen bei     *}
{*                          Tritschler-Abruf SR und �ber Multiplexer SSU;     *}
{*                          SMS-Best�tigungsmodus 'Wiederholung';             *}
{*                          mit SMS-Messdaten-Abruf f�r Veribox Mini          *}
{*          17.12.2008  WW  V. 2.81: mit Tritschler MCO                       *}
{*          21.01.2009  WW  V. 2.82: MRG-Datenkonvertierungen: Impulswerte-   *}
{*                          bereich f�r normierte LGZ-Daten vergr��ert;       *}
{*                          Bugfix: MRG-Ger�tetyp aktualisieren bei DSfG-Um-  *}
{*                          schalt-Kommando, Gas-X-Version: Gas-X-spezifische *}
{*                          Ger�tetypnummer in XML-Antworten zur�ckgeben;     *}
{*                          Konvertierung Veribox-SMS: mit Datenl�cke-Pr�fung *}
{*                          und Zwischenpuffern der Roh-SMS (wegen SMS, welche*}
{*                          nicht chronologisch vom Modem empfangen werden);  *}
{*                          Bugfix Konvertierung Tritschler TTG-FTL: Messwerte*}
{*                          enthalten unbewertete Impulse                     *}
{*          15.05.2009  WW  V. 3.00: mit GPRS (push: MRG 910 und Tritschler   *}
{*                          TDS/MCO; pull: MRG 910); Bugfix MRG-Zeitsynchroni-*}
{*                          sation: Korrektur auch bei abweichender MRG/PC-   *}
{*                          Stunde; Veribox Mini mit Originaldaten (SMS);     *}
{*                          Abruf Actaris Sparklog mit Neuinitialisierung der *}
{*                          Kommunikation nach Timeout; VC2-Konvertierung:    *}
{*                          Pr�fung auf Rohdatenende �ber Ger�tedatum/-zeit   *}
{*                          aus Parameterliste; Abruf Actaris Sparklog: mit   *}
{*                          Wiederholung beim Senden des Aufforderungstele-   *}
{*                          gramms (Init_IEC1107_Kommunikation) und verk�rztem*}
{*                          Timeout                                           *}
{*          18.06.2009  WW  V. 3.01: mit EC 900; MRG-Zeitsynchronisation:     *}
{*                          Einschr�nkung "nur mit Passwort 1" entfernt f�r   *}
{*                          MRG 800, 800 PTB, 905, 910; DSfG-Rohdatenpr�fung: *}
{*                          Keine L�ngenpr�fung mehr bei Archivwerten (Texte  *}
{*                          k�nnen leer sein); Konvertierung Tritschler TDS,  *}
{*                          ZS-Differenzbildung f�r normierte LGZ-Daten: mit  *}
{*                          Toleranzbereich f�r Zeitstempel ungleich Mess-    *}
{*                          periodenende                                      *}
{*          09.07.2009  WW  V. 3.02: Veribox Mini (SMS): Originaldaten von    *}
{*                          Impulskan�len jetzt als Mengen (vorher Z�hler-    *}
{*                          st�nde; Kompatibilit�t zu Daten per e-Mail/FTP)   *}
{*          30.07.2009  WW  V. 3.03: Bugfix DSfG-DF�-Parametrierung: Para-    *}
{*                          metrierung von Wieser-Parametern 2-stufig (ohne   *}
{*                          und mit abschlie�enden Leerzeichen)               *}
{*          22.09.2009  WW  V. 3.04: Bugfix Konvertierung Tritschler TDS: MW- *}
{*                          Originaldaten mit Nachkommastellen; mit Tritschler*}
{*                          VC3; mit zus�tzlichem Aufweck-Timeout bei Trit-   *}
{*                          schler IEC-Protokoll-Kommandos                    *}
{*          02.10.2009  WW  V. 3.05: DSfG-Abruf: mit Abfrage von Fabriknummer *}
{*                          und Baujahr der DSfG-DF� (f�r Konfiguration-Ein-  *}
{*                          lesen)                                            *}
{*          14.10.2009  WW  V. 3.06: Bugfix Konvertierung Tritschler TDS: Z�h-*}
{*                          lerstand des letzten Kanals mit Nachkommastellen  *}
{*          21.12.2009  WW  V. 3.07: Kennziffer f�r Tritschler VC3-Kennung per*}
{*                          INI einstellbar; Abruf Actaris Sparklog: Daten-   *}
{*                          abruf �ber Kommunikationsroutine 'SendCommand'    *}
{*                          statt 'SendIEC1107DatenTelegramm' (keine Quittie- *}
{*                          rung, keine Teildatens�tze !); Bugfix Kennungsab- *}
{*                          frage bei MRG-Rufentgegennahme von Wieser-Ger�ten *}
{*          25.01.2010  WW  V. 3.08: Anpassungen f�r RohTRec-Struktur mit Wert*}
{*                          als double                                        *}
{*          03.03.2010  WW  V. 3.09: Zeichen-Konvertierung ASCII/ISO 646 bei  *}
{*                          DSfG-Abruf (DE lesen/einstellen, Archivwerte      *}
{*                          lesen), �ber INI-Schalter ISO646 ein-/ausschaltbar*}
{*          24.03.2010  WW  V. 3.10: Plausibilisierung Datum in               *}
{*                                   SMS-Zwischendatei                        *}
{*          12.07.2010  GD  V. 3.11: DSfG Erweiterungsgrad 2 als ErwGrd 1     *}
{*          17.08.2010  GD  V. 3.12: Neues Zeitkommando "t" (f�r GasX), DSfG  *}
{*          06.10.2010  GD  V. 3.13: Beschr�nkung ModemNr > 50 = IP Abruf     *}
{*                          f�r Gas-X entfernt                                *}
{*          05.11.2010  GD  V. 3.14: Bugfix;                                  *)
{*                          Modemeinschr�nkung f�r GAS-X LogCom -> PhysCom    *}
{*                      WN  V. 3.14: Nachholen von zwischengespeicherten SMS  *}
{*          13.01.2011  GD  V. 3.15: Neues Zeitkommando "t" (f�r GasX), MRG   *}
{*                                   BCC-Fehler bei VC3 umgangen              *}
{*          24.02.2011  GD  V. 3.16: Zeichenhandling XML / nur bei Hersteller *}
{*          24.03.2011  GD  V. 3.17: Bugfix: Zeichenhandling XML / t-Befehl   *}
{*          05.05.2011  WW  V. 3.18: Modem-Verbindung bei MRG-/DSfG-Abrufen:  *}
{*                          Wartezeit nach Connect-Antwort erh�ht (f�r Ver-   *}
{*                          bindungen zu Tainy-Zentrale)                      *}
{*          14.06.2011  WW  V. 3.19: Bugfix Verbindungsaufbau/Rufentgegennahme*}
{*                          DSfG: Stations-Kennung zur�ckgeben auch bei Ver-  *}
{*                          bindungsaufbau-/Rufentgegennahme-Fehler           *}
{*          20.07.2011  WW  V. 3.2: Wieser DSfG-DF�-Parameter mit YWa-Befehl  *}
{*                          lesen (NG)                                        *}
{*          16.08.2011  WW  V. 3.3: Serielles Auslesen; COM vor dem �ffnen auf*}
{*                          Vorhandensein pr�fen; mit Elster DS-100; XML-Res- *}
{*                          ponse erweitert um Basis-Fehlercode (RMG-Version);*}
{*                          ben�tigt AlmProc32 und AlmIPProc32 ab Vs. 2.2     *}
{*          19.10.2011  WW  V. 3.31: mit VC3; VC2-Fehlercode als Meldungen    *}
{*                          auslesen; Bugfixes: MRG-Modemabruf, XML-Response  *}
{*                          f�r DSfG-Umschaltung mit Basis-Fehlercode         *}
{*          09.11.2011  WW  V. 3.32: Bugfix BCC-Fehlerpr�fung, wenn Startzei- *}
{*                          chen STX, SOH nicht vorhanden (EK260, RWE);       *}
{*                          Timeout_Init_IEC1107 von 6 s auf 7,5 s erh�ht     *}
{*                          (EK260, RWE)                                      *}
{*          14.12.2011  WW  V. 3.33: DSfG-DF�-Befehl V: Antwort 'Nicht inter- *}
{*                          pretierbare Dateneinheit' (STX ? ETX) abfangen;   *}
{*                          IP-Abrufe: Bugfix Connect bei IP-Adresse mit      *}
{*                          f�hrenden Nullen                                  *}
{*          27.01.2012  WW  V. 3.4: mit Verifizierung signierter DSfG-Daten   *}
{*                          �ber Signaturserver (�ffentlicher Schl�ssel global*}
{*                          in Prog\Signatur_PublicKey.TXT hinterlegt),       *}
{*                          Protokollierung des Verifizierungsergebnis in     *}
{*                          WicomSrv_SignaturVerify.log; XML-Ersatzstring f�r *}
{*                          Zeichen '&' korrigiert                            *}
{*          05.03.2012  WW  V. 3.41: mit Firmware-Update f�r NG-DSfG-DF� per  *}
{*                          Verbindungsaufbau-Kommando; G�ltigkeitspr�fung von*}
{*                          DSfG-DF�-Antworten erweitert f�r NG ('Antwort ver-*}
{*                          boten'); �ffentlicher Signatur-Schl�ssel per      *}
{*                          Client-Kommando 'DSfG-Messwertabruf', Signatur-   *}
{*                          Verifizierungsergebnis der abgefragten DSfG-      *}
{*                          Archivdaten in Antwort mitzur�ckgeben             *}
{*          30.04.2012  WW  V. 3.42: Zus�tzliche Rohdaten-Plausibilisierung   *}
{*                          bei Elster IEC1107-Archiv- und Logbuchdatenabfrage*}
{*                          (Problem RWE mit undefinierter Antwort von Gegen- *}
{*                           stelle bei EK260-Abruf)                          *}
{*          07.05.2012  WW  V. 3.43: Parametrierung DSfG-Datenelement: Erweit-*}
{*                          erte Pr�fung Soll/Ist-Wert (M�gliche Fehlermeldung*}
{*                          bei erfolgreicher �nderung von Float-Werten);     *}
{*                          Signatur, DSfG-PTB-Feld: Pr�fung auf Trennzeichen *}
{*                          ';' (DSfG-konform) und ':' (ERZ2000 Vs. 2.0.0,    *}
{*                          nicht DSfG-konform)                               *}
{*          23.05.2012  WW  V. 3.44: Nochmaliger Bugfix BCC-Fehler bei VC3    *}
{*                          (Schmutzzeichen "Space" nach dem BCC-Zeichen);    *}
{*                          mit IFDEF 'NO_XML_SIGVERIFYSTATE' f�r XML-Response*}
{*                          von DSfG-Archiv- und Logbuchdaten ohne Tag f�r    *}
{*                          Signaturverifizierungsstatus (Workaround f�r in-  *}
{*                          kompatibles Verhalten von AlmProc32/AlmIPProc32   *}
{*                          bei XML-Datenstrukturerweiterung)                 *}
{*          19.06.2012  WW  V. 3.45: DSfG-Signaturverifizierungsfehler in     *}
{*                          Fehlergruppe/-code und in convstate des Response- *}
{*                          log zur�ckliefern; auf �nderung der Schnitt-      *}
{*                          stellenkonfiguration 'Debug COM-Protokoll' w�hrend*}
{*                          des Rufpolling pr�fen                             *}
{*          17.07.2012  WW  V. 3.46: DSfG: Erweiterungsgrad 2; Firmware-      *}
{*                          Update: Einstellbare Parameter lesen mit sb-Befehl*}
{*                          und Systemzeit-Passwort (NG unterst�tzt b-Befehl  *}
{*                          nicht mehr in Versionen ab 05/2012); Bugfix NG-   *}
{*                          Systemzeitpasswort codieren; DSfG: Wieser-Teilneh-*}
{*                          merliste '!!!2-LS-active!!!' abfangen             *}
{*          16.08.2012  WW  V. 3.47: Verbindungsaufbau-Kommando: Automatisches*}
{*                          Ermitteln des seriellen MRG-Datenformats (Modemtyp*}
{*                          = 0) und der Passwortnummer (PasswortNr = 0) f�r  *}
{*                          Elster IEC1107-Ger�te; Bugfix Konvertieren von    *}
{*                          Parametern von Elster IEC1107-Ger�ten mit mehreren*}
{*                          Einzelwerten (Stati); DSfG-Datenelemente-Abruf mit*}
{*                          optionalem Timeout aus Server-Parameterabruf-     *}
{*                          Kommando                                          *}
{*          25.09.2012  WW  V. 3.48: Actaris Corus: Beschleunigter Parameter- *}
{*                          Abruf (Parameter-Gruppenbefehle), Parametrieren;  *}
{*                          ZAE Soll/Ist-Pr�fung DSfG-Rohdaten: OK, wenn kein *}
{*                          ung�ltiger Rohsatz enthalten (GC9000, DE a-z)     *}
{*          07.11.2012  WW  V. 3.49: Bugfix Rohdatenplausibilisierung bei     *}
{*                          MW/ME-Abruf von Elster-Ger�ten mit IEC 1107-Proto-*}
{*                          koll (DL210, DL220, DL240, EK260)                 *}
{*          04.01.2013  WW  V. 3.5: mit Verifizierung signierter DSfG-Daten   *}
{*                          aus Datenelemente-Abruf                           *}
{*          24.01.2013  WW  V. 3.51: Bugfix in DSfG-Datenelemente-Konvertier- *}
{*                          ung (TDELList): Signaturverifizierungs-Fehler     *}
{*                          jetzt mit richtigem Fehlercode zur�ckgeben (bisher*}
{*                          "Konvertierungsfehler Datenelemente")             *}
{*          27.02.2013  WW  V. 3.52: Bugfix DSfG-Verbindungsaufbau: Fehler    *}
{*                          'Kennung falsch, Verbindung bleibt bestehen' wird *}
{*                          jetzt zur�ckgegeben; MRG-Stationsabrufe �ber      *}
{*                          TCP/IP                                            *}
{*          27.03.2013  WW  V. 3.53: mit Kamstrup UNIGAS 300; mit Simulations-*}
{*                          betrieb ohne DSfG-Ger�tekommunikation (IFDEF SIMU)*)
{*                          Messwerte-Konvertierung Elster EK260, DL 210/220/ *}
{*                          240 mit doppelter 2-Uhr-Stunde bei SZ/WZ-Wechsel; *}
{*                          MRG-Konvertierung: Rundung der Tagessatz-Werte    *}
{*                          raus (EC900, EK260, DL210/220/240, TTG-IEC, TDS,  *}
{*                          MCO, VC3, FWU, Corus, Sparklog, UNIGAS 300);      *}
{*                          f�r Gas-X: Modem-Verbindung halten, solange kein  *}
{*                          Client-Befehl vorliegt (nur DSfG, per WicomSrv.ini*}
{*                          aktivierbar)                                      *}
{*          17.05.2013  WW  V. 3.54: mit DSfG-Zeitsynchronisation �ber XML-Z- *}
{*                          Befehl (ZeitSynch-Rundsendung); Bugfix: Signatur- *}
{*                          fehler-R�ckgaben ausblenden bei XML-Kommandos     *}
{*                          "DSfG-Busanalyse" und "Zeitabruf"                 *}
{*          28.05.2013  WW  V. 3.55: Messwert-Abruf MRG 905/910 mit k-Faktor; *}
{*                          Bugfix R�ckgabe Fehlercode 'GetLastError', wenn   *}
{*                          ClearCommError fehlschl�gt (Absturz, wenn z.B. COM*}
{*                          w�hrend des Datenempfangs nicht mehr vorhanden);  *}
{*                          Default bei Dienst-Registrierung: Keine Inter-    *}
{*                          aktion zwischen Dienst und Windows-Desktop;       *}
{*                          Rufentgegenname: Schnittstelle neu initialisieren *}
{*                          nach erfolgloser Modeminitialisierung/FUP-Reset   *}
{*          03.07.2013  WW  V. 3.56: IFDEF NO_BDETEMPDIR (ohne BDE-Temp-      *}
{*                          Verzeichnis-Aktionen in WTables.pas); max. 10     *}
{*                          Versuche f�r das Einf�gen des Taskbar-Icons       *}
{*          31.07.2013  WW  V. 3.57: Firmware-Update NG-DSfG-DF�: Timeout-    *}
{*                          Zeiten f�r das �bertragen und Flashen der Firm-   *}
{*                          ware-Bin�rdaten erh�ht (Fehler: "Timeout beim     *}
{*                          Warten auf Daten" wegen l�ngerer Laufzeiten bei   *}
{*                          GSM-/GPRS-Verbindung), Auswertung NG-Fehlerantwort*}
{*                          'X' beim �bertragen der Firmware-Bin�rdaten       *}
{*          26.09.2013  WW  V. 3.58: Messwert-Abruf EK260 erweitert auf Kan�le*}
{*                          f�r K-Zahl und Z-Zahl                             *}
{*          09.10.2013  WW  V. 3.59: Corus-Messwerte mit aktuellen Z�hler-    *}
{*                          st�nden zum Auslesezeitpunkt (nur Gas-X-Version); *}
{*                          Stehende IP-Verbindungen zu MRG-/DSfG-Ger�ten     *}
{*                          explizit beenden, wenn Dienst beendet wird        *}
{*          19.11.2013  WW  V. 3.60: Anpassungen GSM-Modeminitialisierung f�r *}
{*                          Sierra Wireless Modem USB (PIN-Statusabfrage,     *}
{*                          PIN-Login); Gas-X-Version: Rufnummer aus XML-     *}
{*                          Verbindungsaufbau-Kommando um COM-spezifische     *}
{*                          Vorwahl aus SrvCfgIni-Konfiguration erweitern     *}
{*          03.12.2013  WW  V. 3.7.0: Neuer Dienstname (nicht Gas-X-Version)  *}
{*                          und Dienst-Anzeigename; Rundpuffer f�r COM- und   *}
{*                          Abruf-Logfile �ber Schnittstellen-Konfiguration;  *}
{*                          auf ge�nderte Einstellungen der Schnittstellen-   *}
{*                          Konfiguration w�hrend Laufzeit pr�fen: COM- und   *}
{*                          Abruf-Logfile, Rundpuffer auf Logfiles; Firmware- *}
{*                          Update NG-DSfG-DF�: Timeout-Zeit f�r das Initiali-*}
{*                          sieren der Firmware-�bertragung erh�ht (Fehler:   *}
{*                          "Undefinierter Fehler beim �bertragen der Firm-   *}
{*                          ware-Bin�rdaten"); XML-Responses 'Verbindungsauf- *}
{*                          bau', 'Rufentgegennahme', 'Rufannahme' erweitert  *}
{*                          um Verbindungsinformationen (Zeitpunkte 'Verbin-  *}
{*                          dung steht', 'Login erfolgt'); Bugfix Rufentgegen-*}
{*                          nahme (Fehler 'Timeout beim Warten auf Daten',    *}
{*                          ATHR-Datei: 'Kommando-Timeout, keine weiteren Kom-*}
{*                          mandos vorhanden'); mit Elster EK 280; Abruf      *}
{*                          Archive/Meldungen Elster DL/EK-Serie mit Block-   *}
{*                          gr��e 10                                          *}
{*          04.03.2014  WW  V. 3.7.1: Bugfix Gas-X-Version: Messwerte-Konver- *}
{*                          tierung von aktuellen Corus-Z�hlerst�nden der Gas-*}
{*                          tagende-Stunde                                    *}
{*          21.03.2014  WW  V. 3.7.2: mit Ger�tezustand f�r DL 240, 220, 210, *}
{*                          MRG 905, 910                                      *}
{*          25.04.2014  WW  V. 3.7.3: Parameter- und Zeitabruf f�r EC694;     *}
{*                          SMS-Puffer-Dateien mit Zeitstempel der Erstellung *}
{*                          im Dateiname (Bugfix f�r mehrfache SMSen mit glei-*}
{*                          chem Zeitstempel)                                 *}
{*          16.05.2014  WW  V. 3.7.4: DSfG-Parametrierung: bei Pr�fung des    *}
{*                          Parametrierergebnisses abschlie�ende Spaces in    *}
{*                          Soll- und Ist-Datenelementwert ignorieren;        *}
{*                          MRG-Parametrierung Wieser/RMG-Ger�te: bei Fehlpa- *}
{*                          rametrierung Wiederholung mit modifiziertem neuem *}
{*                          Parameterwert                                     *}
{*          23.05.2014  WW  V. 3.7.5: mit MRG-Abruf f�r Elster DL 230; abge-  *}
{*                          rufene MRG-Meldungen nach Datum/Zeit sortieren f�r*}
{*                          Ger�te mit mehreren Meldungs-Abrufbefehlen (EC 900*}
{*                          Corus, Sparklog, VC3, UNIGAS 300, EK280, DL 230); *}
{*                          DL 2xx-Reihe: Auslesen Tagesende �ber Parameter   *}
{*                          'Tagesende Kanal 1'; nur Gas-X-Version: unbegrenz-*}
{*                          te Laufzeit nicht mehr g�ltig                     *}
{*          10.06.2014  WW  V. 3.7.6: mit Schreiben einer Heartbeat-Datei (per*}
{*                          INI konfigurierbar); MRG-Konfiguration einlesen:  *}
{*                          Warnung bei bereits ge�ffnetem Zugangsschloss f�r *}
{*                          Elster DL- und EK-Serie, mit Ger�tetyp�berpr�fung *}
{*                          f�r: Elster DL- und EK-Serie, Corus, Sparklog,    *}
{*                          UNIGAS 300, Tritschler VC2 VC3 TDS MCO direkt     *}
{*                          (nicht an Multiplexer), MRG 800PTB 905 910, EC900;*}
{*                          MRG-Parameterabruf von Elster EK260/280 Gasqua-   *}
{*                          lit�tsdaten mit Versions- und K-Zahl Modus-       *}
{*                          abh�ngigen Parameteradressen (mit ParamEK.dat)    *}
{*          08.08.2014  WW  V. 3.7.7: MRG-Messwerteabruf f�r normierte LGZ-   *}
{*                          Daten: Schnellerer Abruf durch reduzierte Menge   *}
{*                          abzurufender, f�r die Messwert-Konvertierung      *}
{*                          ben�tigter Ger�te-Parameter bei Elster DL- und EK-*}
{*                          Serie, Actaris Sparklog, Kamstrup UNIGAS 300 (er- *}
{*                          weiterte Struktur in ParamMrg.dat)                *}
{*          06.11.2014  WW  V. 3.7.8: Bugfix DSfG-DF�-NG Firmwareupdate, R�ck-*}
{*                         speichern der gesicherten Parameter: Wenn ein oder *}
{*                         mehrere Parameter nicht �bertragen werden konnten, *}
{*                         wurde Fehlergruppe 0, Fehlercode 0 zur�ckgegeben   *}
{*                         ("Nicht dokumentierter Fehler"),                   *}
{*                         wenn Fehler beim Parameter-Wiederherstellen auftritt}
{*                         Log-Datei mit Parametrier-Ergebnissen schreiben    *}
{*                         (ParaRestoreResult_FwUpd_... .log); Bei DSfG-DF�-NG*}
{*                         Firmwareupate 'Baujahr' und 'Fabriknummer' �ber    *}
{*                         Parameter 322, 323 lesen (bisher: 103, 104; Para-  *}
{*                         meter 103 'Fabriknummer' fehlt jedoch in fr�hen NG-*}
{*                         Versionen, z.B. 00.01)                             *}
{*          06.05.2014  WW  V. 3.8.0: Signatur in Gas-X-Version implementiert;*}
{*                          Allgemein: Signatur-Lizenz pr�fen, Base64-kodierte*}
{*                          Rohdaten in XML-Responses f�r DSfG-Archive,       *}
{*                          -Logb�cher, -Datenelemente (E, M, B) per INI ein-/*}
{*                          ausschaltbar; Signaturverifizierung Request-      *}
{*                          gesteuert (wenn XML-Tag 'digitalsignature' vorhan-*}
{*                          den)                                              *}
{*          14.01.2015  WW  V. 3.8.1: Simulation DSfG-Abruf �berarbeitet: dy- *}
{*                          namisches Simu-Archiv, Abfrage Archiv-F�llst�nde, *}
{*                          Folgetelegramme, simulierter Verbindungsaufbau und*}
{*                          Antworttelegramme mit Delay, INI-Konfiguration    *}
{*          21.04.2015  WW  V. 3.8.2: Multi-Client-F�higkeit f�r IP-Abrufe    *}
{*                          (erm�glicht gleichzeitige Verwendung der gleichen *}
{*                          IP-Linien-Nr. durch mehrere Clients); Logdateien  *}
{*                          in Log-Pfad der Wieser.ini schreiben              *}
{*          31.07.2015  WW  V. 3.8.3: Simulation erweitert: Simulierter nicht *}
{*                          erfolgreicher Verbindungsaufbau (INI-Konfiguration*}
{*                          f�r Liniennummern)                                *}
{*          06.10.2015  WW  V. 3.8.4: Beschleunigte Rohdateizugriffe bei Kon- *}
{*                          vertierungen (TFileOfCharStream)                  *}
{*          11.01.2016  WW  V. 3.8.5: mit Tritschler VCC; COM-Logfile mit     *}
{*                          Speicherinfos > 4 GB (GlobalMemoryStatusEx)       *}
{*          11.07.2016  WW  V. 3.8.6: DSfG-Archivgruppen zeilenweise lesen;   *}
{*                          Ausgabe Lizenz-Seriennummer im Program-Logfile    *}
{*          31.01.2017  WW  V. 3.8.7: XML-Response 'Verbindungsaufbau DSfG'   *}
{*                          erweitert um Erweiterungsgrad der Login-DSfG-DF�  *}
{*          21.02.2017  WW  V. 3.8.8: Konvertierung MRG-EK280-Messwerte mit   *}
{*                          optionalen, zus�tzlichen Kan�len (STGW), auch f�r *}
{*                          Gas-X-Version)                                    *}
{*          11.04.2017  WW  V. 3.8.9: Kennungsabfrage �ber frei definierbare  *}
{*                          Kennziffer f�r Kamstrup UNIGAS 300 und Actaris    *}
{*                          Sparklog                                          *}
{*          05.12.2017  WW  V. 3.8.10: Parameter-Konvertierung Tritschler IEC:*}
{*                          Werte des letzten Monatsabschlu� konvertieren;    *}
{*                          MRG-Abruf, Abrufgruppe 11: Korrektur 8. Bit in den*}
{*                          empfangenen 7-Bit-Ger�tedaten l�schen (f�r UNIGAS *}
{*                          300, lt. Vorgabe Fa. Wigersma & Sikkema)          *}
{*          08.01.2018  WW  V. 4.0.0: mit TCP/IP-Rufentgegennahme f�r DSfG    *}
{*          13.03.2018  WW  V. 4.1.0: Erweiterungen f�r Gas-X-Version: mit er-*}
{*                          weiterten Ergebnis-R�ckgaben bei allen Responses; *}
{*                          Busadresse der Login-DF�-Instanz zur�ckgeben in   *}
{*                          v-Response DSfG; Neuer Signaturverifizierungs-    *}
{*                          status 4 bei fehlendem �ffentlichem Schl�ssel;    *}
{*                          Bugfix C-Response bei DSfG-DF�-Parametrierung:    *}
{*                          Parametrier-Ergebnisdaten bei Verstellung eines   *}
{*                          Normparameters fehlten                            *}
{*          07.05.2018  WW  V. 4.1.1: Anpassung DSfG-Busanalyse (Befehl I) an *}
{*                          DF�-Instanz Typ E; Gas-X-Version: Vorbereitung    *}
{*                          Erweiterung XML-Response f�r MRG-Messwertabruf um *}
{*                          Ordnungsnummer f�r Elster DL2nn- und EK2nn-Ger�te *}
{*          28.06.2018  WW  V. 4.1.2: Abruf Tritschler MC2; Zeitsynchronisa-  *}
{*                          tion f�r Tritschler IEC-Ger�te erweitert um R�ck- *}
{*                          gabe der Ger�tezeit (Auswertung der UTC-Ger�tezeit*}
{*                          (Kennziffer 30.) und Verstell-Sekunden (Kennziffer*}
{*                          0.3))                                             *}
{*          24.08.2018  WW  V. 4.2.0: Bugfix fehlender valchange-Abschlu� / in*}
{*                          Parametrieren-Response (C-Befehl); Zugriff auf    *}
{*                          Errtxt32.dll eliminiert, Errtxt-Funktionen sind   *}
{*                          jetzt direkt in den Quellcode miteingebunden      *}
{*                          (Funktionsaufruf GetGasXStatus aus Errtxt32.dll   *}
{*                          liefert in Gas-X-Version bei OGE mit sehr vielen  *}
{*                          parallelen IP-Linien Exceptions "Ung�ltige Zeiger-*}
{*                          operation", "Zugriffsverletzung bei Adresse..."   *}
{*                          -> DLL-Zugriff offensichtlich nicht Thread-f�hig) *}
{*          25.02.2019  WW  V. 4.2.1: SendMRGCommand, Abrufgruppe 8: Korrektur*}
{*                          8. Bit in den 7-Bit-Ger�tedaten l�schen (f�r      *}
{*                          Tritschler IEC-Ger�te (MC2, VC3 etc.) mit neuem   *}
{*                          Ethernet-Modem lt. Vorgabe Fa. Tritschler         *}
{*          08.03.2019  WW  V. 4.3.0: mit MRG-Abruf f�r RMG Primus 400 (Modbus-*}
{*                          Protokoll): Lesen von Messwerten (Daten-Archiv),  *}
{*                          Meldungen (Status-Archiv), Parameter (aktuelle    *}
{*                          Werte), Zeitsynchronisation (allg. Parametrierung *}
{*                          noch nicht implementiert); Neue Einstellungen in  *}
{*                          WicomSrv.ini: TOModbusProt, ModbusProtLRC_CRCVersuche; *}
{*                          Logdatei WICOMSRV_ModbusErr.log f�r detailliertere*}
{*                          Ausgabe von Modbus-Fehlern (bei aktiviertem COM-  *}
{*                          Log)                                              *}
{*          30.04.2019  WW  V. 4.3.1: MRG-Parametrierung: Erweitert f�r Trit- *}
{*                          schler VC2, VC3, VCC (FTL-internes Protokoll,     *}
{*                          Achtung: Laut Nutzungsvertrag mit Fa. Tritschler  *}
{*                          nur f�r die Parametrierung von Z�hlerst�nden und  *}
{*                          Gasanalysedaten im GM-T erlaubt ! Funktion ist in *}
{*                          Gas-X-Version gesperrt);                          *}
{*                          XML-Parametrier-Kommando: bei MRG optional Feld   *}
{*                          "Zugangscode 1" f�r Parametrier-Passwort und Feld *}
{*                          "Zugangscode 2" f�r Parametrier-Passwortnummer    *}
{*                          verwendet (zuvor nicht verwendet); Anpassung MRG- *}
{*                          Parameter-Konvertierung f�r Tritschler IEC-Ger�te/*}
{*                          Datacon FWU (Allg. Parameternummern f�r Parameter *}
{*                          ohne Kennziffer aus Resourcen-Datei lesen, bisher *}
{*                          im Quellcode festverdrahtet; neu zugeordnete Para-*}
{*                          metergruppen bei Tritschler-Ger�ten);             *}
{*                          Standard-Timeout f�r MRG-IEC1107-Kommunikation von*}
{*                          10 auf 20 s erh�ht (Elster EK per GSM-Modemverbin-*}
{*                          dung); WicomSrv_Signatur-Logdatei nur bei akti-   *}
{*                          viertem ServiceIO-Debugschalter schreiben         *}
{*          11.07.2019  WW  V. 4.3.2: DSfG-Verbindung nicht beenden, wenn nach*}
{*                          dem Transparent-Schalten keine Teilnehmer erkannt *}
{*                          werden oder die DF�-Login-Adresse 0 ist (ange-    *}
{*                          steuert per XML-Kommando Verbindungsaufbau, DSfG- *}
{*                          Transparentmodus = I)                             *}
{*          22.07.2019  WW  V. 4.3.3: Erweiterungen und Anpassungen f�r RMG   *}
{*                          Primus 400, FW-Version 1.12-3873/52FD, Modbusliste*}
{*                          ID 9 mit Archiv-Zeiger: Zeitsynchronisation erwei-*}
{*                          tert um Senden des Passworts; Einbau Parameterein-*}
{*                          stellfunktion; Abruf Meldungs- und Messwert-Archiv*}
{*                          �ber Archivzeiger-Parameter; Zeitabruf (t-Befehl);*}
{*                          MRG-Messwerte-Abruf: Korrektur des von-Abrufzeit- *}
{                           punkts bei von-Stunde zwischen 0..Tagesende nur   *}
{*                          noch f�r die Wieser-Ger�te (f�r die die Korrektur *}
{*                          ursp�nglich erforderlich war (MRG 2xxx, 3xxx) bzw.*}
{*                          sich die Abrufzeit nicht merklich verl�ngert (MRG *}
{*                          900, EC900)); Parameterausgabe formatiert;        *}
{*                          Modbus-Kommunikation: ByteCount Ist/Soll-Plausibi-*}
{*                          lisierung der Modbus-Responses pr�zisiert; Modbus-*}
{*                          Werttyp ile (IPv4-Adresse, Little-Endian); Work-  *}
{*                          around Archivzeiger f�r auf Sommerzeit laufendes  *}
{*                          Ger�t                                             *}
{*          10.12.2019  WW  V. 4.3.4: Bugfix Zeitsynchronisation UNIGAS 300   *}
{*                          (Zeitzone im DS7-Rohdatenformat)                  *}
{*          16.12.2019  WW  V. 4.3.5: IP-Abrufe RMG Primus 400 mit Protokoll  *}
{*                          Modbus TCP/IP (bisher Modbus RTU); Bugfix Pr�fung *}
{*                          auf vollst�ndige Modbus-Antwort (RTU, TCP/IP):    *}
{*                          Exception-Antwort, unbekannter Funktionscode      *}
{*          25.02.2020  WW  V. 4.3.6: Erweiterung IP-Verbindung zu MRG/DSfG-  *}
{*                          Station per DNS Hostname                          *}
{*          03.06.2020  WW  V. 4.4.0: mit optional verschl�sselter Kommunika- *}
{*                          tion zum Client (SSL 2.0, SSL 3.0, TLS 1.0) und   *}
{*                          optionaler Basis-Authentifizierung im Request-    *}
{*                          Header, einstellbar per SrvCfg32.ini ([Server]    *}
{*                          SSLVersion, BasicAuthentication);                 *}
{*                          bei verschl�sselter Client-Kommunikation: Zertifi-*}
{*                          katdatei cert_pub.pem, Schl�sseldatei cert_key.pem*}
{*                          und Indy-10-Dll's libeay32.dll, ssleay32.dll im   *}
{*                          Exe-Ordner;                                       *}
{*                          mit Logdatei WicomSrv_AbrufSocketSSL.log (bei     *}
{*                          verschl�sselter Client-Kommunikation und aktivier-*}
{*                          tem ServiceIO-Debugschalter);
{*                          TServerSocket's f�r Abruf-Client und IP-Rufentge- *}
{*                          gennahme DSfG ersetzt durch TIdTCPServer,         *}
{*                          TClientSocket's f�r IP-Verbindungen zu Ger�ten und*}
{*                          Signaturserver ersetzt durch TIdTCPClient (Problem*}
{*                          nicht nachvollziehbarer Abst�rze) -> Indy-10, Vs. *}
{*                          10.0.52;                                          *}
{*                          GPRS-ServerSocket stillgelegt (wird nicht mehr    *}
{*                          unterst�tzt);                                     *}
{*                          Erweiterung MRG-Login �ber Datenausleserschloss   *}
{*                          f�r Elster DL230, EK280;                          *}
{*                          Kommunikation mit Signaturserver: Spezifischerer  *}
{*                          Fehlercode SRV_ERR_TIMEOUTRECEIVE statt           *}
{*                          KOMMERR_TIMEOUT;                                  *)
{*                          XML-Ersatzzeichen in Responses f�r Parameter MRG/ *}
{*                          DSfG-DF� und Parametrierung MRG/DSfG/DSfG-DF�;    *}
{*                          XML-Ersatzzeichen decodieren in Request-Teil und  *}
{*                          codieren in Response-Teil                         *}
{*          14.07.2020  WW  V. 4.4.1: Bugfix Windows Ereignislog-Eintr�ge beim*}
{*                          Beenden des Dienstes                              *}
{*          24.08.2020  WW  V. 4.4.2: MRG-Parametrierung Tritschler VC2, VC3, *}
{*                          VCC erweitert f�r Stell-Befehle (z.B. Meldeliste  *}
{*                          l�schen) �ber virtuelle allgemeine Parameter-     *}
{*                          nummern 1600nnnnn                                 *}
{*          28.09.2020  WW  V. 4.5.0: Erweiterungen und Korrekturen f�r       *}
{*                          Gas-X 2.0 (Interface zu DCWebS-Webservice):       *}
{*                          Verbindungsaufbau-XML-Response DSfG erweitert um  *}
{*                          Erweiterungsgrad der Login-DF� (bei Gas-X-Version,*}
{*                          in RMG-Version bereits implementiert);            *}
{*                          Zeitsynchronisation-XML-Response erweitert um     *}
{*                          Responsedata mit Server-Zeit und Ger�te-Zeit vor  *}
{*                          der Synchronisierung (bei Gas-X-Version, in RMG-  *}
{*                          Version bereits implementiert);                   *}
{*                          Parameterabruf-Kommando mit optionalem DSfG-Time- *}
{*                          out (bei Gas-X-Version, in RMG-Version bereits    *}
{*                          implementiert);                                   *}
{*                          Zeitsynchronisation (Kommandos v, Z) mit optiona- *}
{*                          lem UTC-Normalzeit-Offset des Ger�ts;             *}
{*                          XML-Response f�r DSfG-Messwerteabruf mit optiona- *}
{*                          lem COM-Tracelog (Base64-kodiert, gesteuert �ber  *}
{*                          neues Feld "TraceLog" im Verbindungsaufbau-       *}
{*                          Kommando);                                        *}
{*                          Erweiterung XML-Responses f�r MRG-Messwerteabruf  *}
{*                          und MRG-Meldungenabruf um optionale Ordnungsnum-  *}
{*                          mer: Ger�tetypen Elster DL-/EK-Serie; Tritschler  *}
{*                          TDS, MCO, MC2, VC3, VCC; RMG EC 900 (nur Gas-X-   *}
{*                          Version);                                         *}
{*                          auf ge�nderte Einstellungen 'ServiceIO' und 'Roh- *}
{*                          daten' der Schnittstellen-Konfiguration w�hrend   *}
{*                          Laufzeit pr�fen;                                  *}
{*                          Automatisches Freischalten der Programmfunktionen,*}
{*                          wenn zur Laufzeit PC-, Programm- und Programmlauf-*}
{*                          zeit-Lizenz aktiviert werden (nur Gas-X-Version); *}
{*                          Bugfixes: fehlende Sommerzeit-R�ckgabe bei MRG-   *}
{*                          Zeitabruf Corus, fehlender Datencode bei MRG-Um-  *}
{*                          schaltung-Response der Gas-X-Version              *}
{*          03.11.2020  WW  V. 4.5.1: Bugfix Timer gestoppt, wenn Initialisie-*}
{*                          ren der Programmfunktionen fehlschl�gt;           *}
{*                          mit Inaktivit�ts-Timer 2 min f�r Abruf-Clientver- *}
{*                          bindung; Abruf-Clientverbindungen in Datei        *}
{*                          WicomSrv_AbrufSocketCon.log loggen (per Ini-      *}
{*                          Schalter ServiceIO aktivierbar);                  *}
{*                          Erweiteres Exception-Handling f�r Haupt-Thread    *}
{*          25.11.2020  WW  V. 4.5.2: mit MRG-Abruf f�r RMG Prilog 400, FW-   *}
{*                          Version 1.15-68DA/5ACA (Modbus-Protokoll, Modbus- *}
{*                          liste ID 15): Lesen von Messwerten (Daten-Archiv),*}
{*                          Meldungen (Status-Archiv), Parameter (aktuelle    *}
{*                          Werte), Zeitsynchronisation, Parametrierung;      *}
{*                          Bugfix Primus/Prilog-Messwerte/Meldungen lesen:   *}
{*                          J�ngster Datensatz wurde ggf. nicht gelesen (erste*}
{*                          Datens�tze nach L�schen des Archivs im Ger�t)     *}
{*          17.12.2020  WW  V. 4.5.3: Bugfix langsamer SSL-Verbindungsaufbau  *}
{*                          mit Client                                        *}
{*          17.03.2021  WW  V. 4.5.4: Timeout f�r Warten auf Signaturserver-  *}
{*                          Antwort erh�ht auf 30s (f�r GAS-X 2.0)            *}
{*          26.03.2021  WW  V. 4.5.5: mit MRG-Abruf f�r RMG TME400-VCF/VMF,   *}
{*                          FW-Version 1.06 (Modbus-Protokoll, Matrix-        *}
{*                          Version 4): Lesen von Messwerten (Periodenarchiv),*}
{*                          Meldungen (Ereignisarchiv, eichamtliches und      *}
{*                          nicht-eichamtliches Parameterarchiv), Parameter,  *}
{*                          Zeitsynchronisation, Parametrierung;              *}
{*                          COM/IP-Log: Unterbrochene Verbindung loggen       *}
{*          09.07.2021  WW  V. 4.5.6: Reduzierung der Resourcendateizugriffe  *}
{*                          bei MRG-Abrufen (MrgKonv.dat, ParamEK.dat,        *}
{*                          ParamMeld.dat, ParamMrg.dat)                      *}
{*          06.08.2021  WW  V. 4.5.7: Reduzierung der Prozessorauslastung     *}
{*                          w�hrend der Abrufe durch Lesen der Ressourcedaten *}
{*                          aus Pufferlisten statt Dateien, Laden der         *}
{*                          Ressourcedatei-Daten in Pufferlisten bei Programm-*}
{*                          Initialisierung (GAS-X 2.0, Westnetz, MRG-Abrufe);*}
{*                          Reduzierung der Lizenzdatei-Zugriffe;             *}
{*                          Timeout f�r Warten auf Signaturserver-Antwort     *}
{*                          nochmal erh�ht auf 90s (f�r GAS-X 2.0, OGE, DSfG- *}
{*                          Abrufe)                                           *}
{*          29.09.2021  WW  V. 4.5.8: mit MRG-Abruf f�r SICK FLOWSIC500,      *}
{*                          FW-Versionen 2.15.00, 2.16.00 (Modbus-Protokoll): *}
{*                          Lesen von Messwerten (Messperiodenarchiv),        *}
{*                          Meldungen (Ereignisarchiv, Parameterarchiv, metro-*}
{*                          logisches Archiv, Gasparameterarchiv), Parameter, *}
{*                          Zeitsynchronisation, Parametrierung               *}
{*          03.01.2022  WW  V. 4.5.9: Optionales COM-Tracelog erweitert f�r   *}
{*                          alle GAS-X-relevanten Responses                   *}
{*          11.02.2022  WW  V. 4.5.10: Erweiterungen f�r Prilog 400 mit Mod-  *}
{*                          buslisten-Variante (Energie Steiermark): Auslesen *}
{*                          der Modbuslisten-ID des Ger�ts; Ressource         *}
{*                          ParamMrg.dat mit zus�tzlichem Feld 'Parameter-    *}
{*                          untergruppe'; mit neuer Ressourcedatei MBAbruf.dat*}
{*          22.06.2022  WW  V. 4.5.11: Workaround in Konvertierung von abge-  *}
{*                          fragten DSfG-Datenelementen (DE aus Umwerter-     *}
{*                          Standardabfragen, leere Werte bei ERZ2000-NG bei  *}
{*                          Abfrage 'a-z'); Anpassung f�r Abruf und Konvertie-*}
{*                          rung von Standardabfragen mit Archiveigenschaft   *}
{*                          (f�r GM-M, R-Win)                                 *}
{*          04.10.2022  WW  V. 4.5.12: MRG-TCP/IP-Abruf von Modbus-Ger�ten:   *}
{*                          optional mit Standardprotokoll des Ger�ts anstatt *}
{*                          Protokoll "Modbus TCP" (angesteuert per XML-      *}
{*                          Kommando Verbindungsaufbau, Modemtyp = M);        *}
{*                          Modbus-Slaveadresse Primus/Prilog 400 umgestellt  *}
{*                          auf 1 (bisher 248 im reservierten Bereich)        *}
{*          05.10.2022  WW  V. 4.6.0: Erweiterungen (Sopra): mit Binding auf  *}
{*                          IP-Adresse f�r Abruf-Serversocket einstellbar per *}
{*                          SrvCfg32.ini ([Server] IPBind); Einschr�nkung auf *}
{*                          Passwort 1 bei Zeitsynchronisation von DL 230,    *}
{*                          EK 280 entfernt; Gas-X-Version: mit R�ckgabe der  *}
{*                          Stations-Kennung statt Kennung aus Kommando (f�r  *}
{*                          Gas-X 2.0 DCWebS-Webservice -> Abw�rtskompatibi-  *}
{*                          libilit�t zu Gas-X 1.0 lt. Sopra nicht mehr erfor-*}
{*                          derlich); Veraltete Tempor�r- und Request/Res-    *}
{*                          ponse-Dateien des Programms l�schen (wenn INI-    *}
{*                          Schalter Rohdaten bzw. ServiceIO ausgeschaltet);  *}
{*                          Bugfix ZeitSync-Info f�r Server-Zeit bei MRG-Zeit-*}
{*                          Korrektur (Soll-Zeit der Korrektur)               *}
{*          19.12.2022  WW  V. 4.6.1: Bugfix Timeout-Fehler bei DSfG-IP-Ruf-  *}
{*                          entgegennahme (Timeout-�berwachung per TickCount  *}
{*                          statt Timer-Event bei Kommunikation mit MRG/DSfG- *}
{*                          Station); Bugfix Endlosschleife in DSfG-Abruf bei *}
{*                          Antworttelegrammen mit NTY = U, ZAE = 0 (Regis-   *}
{*                          trierung 'Marquis MRG2203A Alternative' mit DF�-  *}
{*                          Instanz 'Elster enCore MC1'; GAS-X, Thyssengas)   *}
{*          31.01.2023  WW  V. 4.6.2: Verfeinerte Fehlerauswertung bei Abfrage*}
{*                          der k-Faktoren-Parameter f�r MRG 905/910-Messwert-*}
{*                          konvertierung (GAS-X; EON); Ge�nderte Lizenzdatei *}
{*                          w�hrend Programmlaufzeit automatisch  erkennen    *}
{*                          (nur Gas-X-Version)                               *}
{*          14.04.2023  WW  V. 4.6.3: f�r GAS-X: Verbindung halten, solange   *}
{*                          kein Client-Befehl vorliegt, erweitert f�r MRG und*}
{*                          DSfG-IP-Verbindungen; neue Ressourcen-Datei       *}
{*                          ParamMomVerb.dat mit Parametern zum Halten der    *}
{*                          MRG-Verbindung                                    *}
{*          24.08.2023  WW  V. 4.6.4: Optionale, erweiterte Abfrage des EK280 *}
{*                          Messwert-Archivs 13: R�ckgabe der Werte in XML-E- *}
{*                          Response unter Kanalnummer 51 ff., per            *}
{*                          WicomSrv.ini konfigurierbar: [EK 280] Archiv13,   *}
{*                          f�r Westnetz (GAS-X);                             *}
{*                          Bugfix: Parameter-Rohdatei l�schen bei Halten der *}
{*                          Verbindung zu MRG-Ger�ten (Elster, Datacon FWU,   *}
{*                          Tritschler, Actaris Corus, Sparklog, UNIGAS 300)  *}
{*          17.10.2023  WW  V. 4.6.5: Mit Wandlung von Hex-kodierten Ersatz-  *}
{*                          zeichen in Parameter-Rohdaten f�r Elster DL-/EK-  *}
{*                          Ger�te                                            *}
{*          09.01.2024  WW  V. 4.6.6: mit MRG-Abruf f�r RMG RSM200-VCF/VMF,   *}
{*                          FW-Version 1.25 (Modbus-Protokoll, Matrix-Version *}
{*                          125): Lesen von Messwerten (Periodenarchiv),      *}  
{*                          Meldungen (Ereignisarchiv, eichamtliches und      *}
{*                          nicht-eichamtliches Parameterarchiv), Parameter,  *}
{*                          Zeitsynchronisation, Parametrierung;              *}
{*                          Konvertierung MRG-Meldungen: Bugfix Meldungsliste *}
{*                          sortieren bei Meldungen mit gleichen Zeitstempeln *}
{*                          (bei Ger�ten mit mehr als 1 Meldungsarchiv: EC900,*}
{*                          Actaris Corus/Sparklog, Tritschler VC3/VCC,       *}
{*                          Kamstrup UNIGAS 300, Elster EK280/DL230, SICK     *}
{*                          FLOWSIC500, TME400, RSM200), f�r EC900 Anpassung  *}
{*                          an DSfG-Ereignisliste mit herstellerunabh�ngigem  *}
{*                          Meldungsnummern-Bereich 1000-9999;                *}
{*                          MRG-Ger�te mit Modbus-Kommunikation: Bugfix Bin�r-*}
{*                          datenwandlung 0-terminierter Strings              *}
{*          14.03.2024  WW  V. 4.6.7: Bugfixes f�r TCP/IP-Rufentgegennahme    *}
{*                          DSfG (Terranets): Speicherleck beim Freigeben des *}
{                           TCP/IP-Rufentgegennahme-Thread; synchronisiertes  *}
{*                          Terminieren des TCP/IP-Rufentgegennahme-Thread    *}
{*                          durch Abruf-Thread; freigeben des Abruf-Objekts   *}
{*                          nur bei nicht erfolgreichem Entgegennehmen eines  *}
{*                          Anrufs (R-Response); Anruf nicht als beendet kenn-*}
{*                          zeichnen bei nicht erfolgreicher Ausf�hrung der   *}
{*                          Rufannahme (a-Kommando)                           *}
{*          04.04.2024  WW  V. 4.6.8: mit Ger�tezustand f�r TME400 und RSM200 *}
{*                          (virtuelle Parameter); Umstellung lesen der Para- *}
{*                          meter-Ressourcedaten-Konfiguration 'Ger�tezustand-*}
{*                          Parameter' aus Feld 'Filtertyp' statt 'ParaDaten- *}
{*                          typ' (Parammrg.dat, auch f�r Elster DL-/EK-Ger�te *}
{*                          und MRG 905/910);                                 *}
{*                          Erweiterung RSM200: Beim Auslesen von Z�hler-Para-*}
{*                          metern immer Parameter 'Z�hlerfaktor' mitauslesen *}
{*                         (f�r Rohwert-Verrechnung)                          *}
{******************************************************************************}
Program WICOMSRV;

uses
  SvcMgr,
  SysUtils,
  PathIni,
  SMain in 'DELPHI32\Abruf\WicomSrv\SMain.pas' {WIPServerService: TService},
  FServer in 'DELPHI32\Abruf\WicomSrv\FServer.pas' {FormAbrufServer},
  Lgztype in 'DELPHI32Sql\Package\LGZType.pas',
  DecodeReq in 'delphi32\abruf\wicomsrv\DecodeReq.pas',
  AbrufThr in 'delphi32\abruf\wicomsrv\AbrufThr.pas',
  AbrufCmdList in 'DELPHI32\Abruf\WicomSrv\AbrufCmdList.pas',
  AbrufCmd in 'delphi32\abruf\wicomsrv\AbrufCmd.pas',
  AbrufCmdExec in 'delphi32\abruf\wicomsrv\AbrufCmdExec.pas',
  RespConst in 'DELPHI32\Abruf\common\RespConst.pas',
  AbrufAnsw in 'delphi32\abruf\wicomsrv\AbrufAnsw.pas',
  Errprc32 in 'DELPHI32\Abruf\WicomSrv\Errprc32.pas',
  ErrConst in 'DELPHI32\Dlls\errtxt32\ErrConst.pas',
  AbrufCmdPlausib in 'delphi32\abruf\wicomsrv\AbrufCmdPlausib.pas',
  SerMrgModem in 'delphi32\abruf\afmrg32\SerMrgModem.pas',
  SerMrgFup in 'DELPHI32\Abruf\Afmrg32\SerMrgFup.pas',
  MDatensatzList in 'DELPHI32\Abruf\afmrg32\MDatensatzList.pas',
  MValidAnswer in 'DELPHI32\Abruf\Afmrg32\MValidAnswer.PAS',
  Logcom in 'DELPHI32\soft\Logcom.pas',
  CRC16 in 'DELPHI32\Soft\Crc16.pas',
  ModemIni in 'DELPHI32\Soft\ModemIni.pas',
  AbrufConst in 'DELPHI32\Abruf\WicomSrv\AbrufConst.pas',
  AbrufSrvIniFile in 'DELPHI32\Abruf\WicomSrv\AbrufSrvIniFile.pas',
  FupModemInit in 'DELPHI32\Abruf\WicomSrv\FupModemInit.pas',
  LogFile in 'DELPHI32\Soft\LogFile.pas',
  AbrufObjMrg in 'delphi32\abruf\wicomsrv\AbrufObjMrg.pas',
  MrgBefehl in 'DELPHI32\Abruf\WicomSrv\MrgBefehl.pas',
  MP_DFUE in 'DELPHI32\Abruf\Common\Mp_dfue.pas',
  MResMrg in 'DELPHI32\Abruf\common\MResMrg.pas',
  MResParam in 'DELPHI32\Abruf\Common\MResParam.pas',
  Wsyscon in 'DELPHI32\Abruf\common\Wsyscon.pas',
  MRG_ObjKonv in 'DELPHI32\Abruf\WicomSrv\MRG_ObjKonv.PAS',
  Mobjlist in 'DELPHI32\Abruf\Afmrg32\Mobjlist.pas',
  mlgzkonv in 'DELPHI32\Abruf\afmrg32\mlgzkonv.pas',
  MLgzKonvList in 'DELPHI32\Abruf\Afmrg32\MLgzKonvList.pas',
  MLgzKonvFremd in 'DELPHI32\Abruf\afmrg32\MLGZKonvFremd.pas',
  Mobjpara in 'DELPHI32\Abruf\Afmrg32\Mobjpara.pas',
  COM_Utils in 'DELPHI32\Soft\COM_Utils.pas',
  GD_Utils in 'DELPHI32\Soft\GD_Utils.pas',
  Mp_Tritschler in 'delphi32\abruf\common\Mp_Tritschler.pas',
  MP_Boritec in 'DELPHI32\Abruf\Common\Mp_Boritec.pas',
  Mp_Elster in 'delphi32\abruf\common\Mp_Elster.pas',
  Dtutils in 'DELPHI32\Soft\Dtutils.pas',
  Mp_allg in 'delphi32\abruf\common\Mp_allg.pas',
  MP_Imp in 'DELPHI32\Abruf\Common\Mp_imp.pas',
  MFileNam in 'DELPHI32\Abruf\WicomSrv\Mfilenam.pas',
  Mobjmeld in 'DELPHI32\Abruf\Afmrg32\MObjMeld.pas',
  AbrufObjDSfG in 'delphi32\abruf\wicomsrv\AbrufObjDSfG.PAS',
  Serdsfg in 'DELPHI32\Abruf\afdsfg32\Serdsfg.pas',
  UnixDT in 'DELPHI32\Soft\UnixDT.pas',
  DSFGUTIL in 'DELPHI32\Abruf\common\DSFGUTIL.PAS',
  AbrufObj in 'DELPHI32\Abruf\WicomSrv\AbrufObj.PAS',
  DD_Allg in 'DELPHI32\Abruf\Common\Dd_allg.pas',
  DDelList in 'DELPHI32\Abruf\Afdsfg32\DDelList.PAS',
  DAbrufList in 'DELPHI32\Abruf\WicomSrv\DAbrufList.pas',
  DALKonv in 'DELPHI32\Abruf\afdsfg32\DALKonv.PAS',
  DListen in 'DELPHI32\Abruf\common\Dlisten.pas',
  MP_DSfG in 'DELPHI32\Abruf\common\Mp_DSfG.pas',
  MDTFormt in 'DELPHI32\Abruf\afmrg32\MDTFormt.pas',
  MResUtil in 'DELPHI32\Abruf\common\MResUtil.pas',
  WComm in 'DELPHI32\Abruf\Common\WComm.pas',
  Mp_Datacon in 'DELPHI32\Abruf\common\Mp_Datacon.pas',
  WResMeld in 'DELPHI32\Abruf\common\WResMeld.pas',
  WResConst in 'DELPHI32\Abruf\Common\WResConst.pas',
  CrcSealink in 'DELPHI32\soft\CrcSealink.pas',
  SrvCfgIni in 'DELPHI32Sql\Abruf\common\Srvcfgini.pas',
  WErrMsg in 'DELPHI32\soft\WErrMsg.pas',
  WXmlConst in 'delphi32\abruf\common\WXmlConst.pas',
  DSfGXmlDecodeObj in 'delphi32\abruf\common\DSfGXmlDecodeObj.pas',
  DecodeResp in 'DELPHI32\Abruf\Common\DecodeResp.pas',
  AbrufTimeoutConst in 'DELPHI32\Abruf\WicomSrv\AbrufTimeoutConst.pas',
  DAufmTelegr in 'DELPHI32\Abruf\WicomSrv\DAufmTelegr.pas',
  DSG_Utils in 'DELPHI32\DSfG\DSfG_Utils\DSG_Utils.pas',
  DValidAnswer in 'DELPHI32\Abruf\AFDSFG32\DValidAnswer.PAS',
  DDfueParaList in 'DELPHI32\Abruf\AFDSFG32\DDfueParaList.pas',
  DDfuParaNr in 'DELPHI32\Abruf\common\DDfuParaNr.pas',
  GSMModemFkt in 'DELPHI32\Abruf\WicomSrv\GSMModemFkt.pas',
  MSMSKonv in 'Delphi32\Abruf\WicomSrv\MSMSKonv.pas',
  SMSList in 'DELPHI32\Abruf\WicomSrv\SMSList.pas',
  TCPIP_DSfG in 'DELPHI32\Abruf\afdsfg32\Indy-10\TCPIP_DSfG.pas',
  SMSDecode in 'DELPHI32\Abruf\WicomSrv\SMSDecode.pas',
  SrvModemtyp in 'DELPHI32\Abruf\common\SrvModemtyp.pas',
  DSMSKonv in 'DELPHI32\Abruf\WicomSrv\DSMSKonv.pas',
  WicomSrvUtil in 'DELPHI32\Abruf\WicomSrv\WicomSrvUtil.pas',
  VerifyFlags in 'Delphi32Sql\abruf\VerifyWico22\VerifyFlags.pas',
  Mp_Actaris in 'delphi32\abruf\common\Mp_Actaris.pas',
  GPRSVerbList in 'Delphi7\Gprs\GprsSrv\GPRSVerbList.pas',
  O_GPRSServerSocket in 'DELPHI32\Abruf\WicomSrv\O_GPRSServerSocket.pas',
  WSocketError in 'DELPHI32\Soft\WSocketError.pas',
  O_Comm in 'DELPHI32\Abruf\WicomSrv\O_Comm.pas',
  TCPIP_Mrg in 'DELPHI32\Abruf\afmrg32\Indy-10\TCPIP_Mrg.pas',
  GPRSTelegrList in 'Delphi7\Gprs\GPRSSrv\Indy-10\GPRSTelegrList.pas',
  GPRS_Util in 'Delphi7\Gprs\GPRSSrv\GPRS_Util.pas',
  DGPRSKonv in 'Delphi7\GPRS\GPRSSrv\DGPRSKonv.pas',
  AusgabeDirList in 'Delphi7\Gprs\GPRSSrv\AusgabeDirList.pas',
  DKurzzeitWerte in 'Delphi32\Abruf\common\DKurzzeitWerte.PAS',
  DGPRS_KZWKonv in 'Delphi7\Gprs\GPRSSrv\DGPRS_KZWKonv.pas',
  WMessageSend in 'DELPHI32\Soft\WMessageSend.pas',
  WMessageReg in 'DELPHI32\Soft\WMessageReg.pas',
  Mp_RMG in 'delphi32\abruf\common\Mp_RMG.pas',
  CommUtil in 'DELPHI32\soft\CommUtil.pas',
  WBluetoothAdapter in 'DELPHI32\Abruf\common\WBluetoothAdapter.pas',
  ErrConstBasic in 'DELPHI32\Dlls\Errtxt32\ErrConstBasic.pas',
  IecConst in 'delphi32sql\Iec\Kopplung\IecConst.pas',
  O_TCPIP_CustomSrv in 'DELPHI32\Abruf\Common\Indy-10\O_TCPIP_CustomSrv.pas',
  O_Signatur in 'DELPHI32\soft\O_Signatur.pas',
  T_MUSysTime in 'delphi32\soft\T_MUSysTime.pas',
  T_FirmwareBin in 'delphi32\soft\T_FirmwareBin.pas',
  Db_attn in 'delphi32\soft\Db_attn.pas',
  O_DSfGSimu in 'Delphi7\DataHub\Test\DSfG_Simu\O_DSfGSimu.pas',
  Mp_Kamstrup in 'Delphi32\Abruf\common\Mp_Kamstrup.pas',
  T_GerZustand in 'Delphi32\Abruf\common\T_GerZustand.pas',
  REClntThr in 'Delphi32\Abruf\WicomSrv\REClntThr.pas',
  REClntVerbList in 'Delphi32\Abruf\WicomSrv\REClntVerbList.pas',
  REClntCmdList in 'Delphi32\Abruf\WicomSrv\REClntCmdList.pas',
  Errtxt in 'DELPHI32\DLLs\errtxt32\Errtxt.pas',
  ErrconstGasX in 'Delphi32\Dlls\Errtxt32\ErrconstGasX.pas',
  ModbusUtil in 'Delphi32\Abruf\Modbus\ModbusUtil.pas',
  ModbusMasterRes in 'Delphi7\ModbusMaster\ModbusMasterRes.pas',
  ModbusMasterUtil in 'Delphi7\ModbusMaster\ModbusMasterUtil.pas',
  O_WIdTCPServer in 'DELPHI32\Abruf\Common\O_WIdTCPServer.pas',
  O_ResFilesList in 'Delphi32\Abruf\Wicomsrv\O_ResFilesList.pas',
  MP_SICK in 'Delphi32\Abruf\Common\Mp_SICK.pas',
  AbrufObjMrgCustom in 'Delphi32\Abruf\Wicomsrv\AbrufObjMrgCustom.pas';

{$R *.RES}

var
  sLogDir: string;
  sErrMsg: string;

begin
  if not GetPathServerLogDir (sLogDir, sErrMsg) then begin
    with TCustomLogFile.Create (ExtractFilePath (ParamStr(0)),
                                ChangeFileExt (ExtractFileName (ParamStr(0)), ''), false) do
      try
        { Fehler-Ausgabe in Programm-Error-Logfile im EXE-Pfad, wenn Pathserver-LogDir
          nicht existiert: }
        Write (sErrMsg, true, lt_Error);
      finally
        Free;
      end;
    exit;
  end;

  Application.Initialize;
  Application.CreateForm(TWIPServerService, WIPServerService);
  Application.Run;
end.
