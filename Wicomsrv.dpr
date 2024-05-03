{******************************************************************************}
{* Dienstanwendung: Abrufserver für MRG- und DSfG-Abrufe (BEB)                *}
{* Funktionen: - Kommunikation mit Client-Systemen über TCP/IP-Socket-Schnitt-*}
{*               stelle                                                       *}
{*             - Datenübergabe zwischen Server und Client (Request/Response)  *}
{*               im XML-Format                                                *}
{*             - keine Datenbank-Anbindung                                    *}
{*             - Abruf von MRG- und DSfG-Stationen über wahlfreie COM-Schnitt-*}
{*               stelle                                                       *}
{*             - MRG: Abruf von Meßwerten, Tagessätzen, Meldungen, Prüfungs-  *}
{*                    sätzen und Parametern                                   *}
{*             - DSfG: Abruf von Archivdaten, Logbüchern, Datenelementen und  *}
{*                     Busanalysedaten                                        *}
{* Version: 19.02.2003  WW  V. 1.0                                            *}
{*	    12.03.2003	WW  V. 1.1  MRG-Abruf mit DSfG-Umleitung; Zuordnung       *}
{*                          logische Schnittstelle -> COM-Port-Nr. über INI;  *}
{*                          Zeitsynchronisation für MRG-Abruf; Programm-      *}
{*                          Lizenz mit Laufzeitbeschränkung; Online-Hilfe;    *}
{*                          IFDEF 'NO_XMLHEADERLENGTH_CHECK' zum Ausschalten  *}
{*                          der XML-Header-Längenprüfung                      *}
{*	    08.04.2003	WW  V. 1.12  MRG-Konvertierung 13er-Serie: Stunde für     *}
{*                          Tagessätze korrigiert (25 -> richtige Stunde);    *}
{*                          DSfG-Abruf: Blockungsgröße für Anforderungstele-  *}
{*                          gramm mit mehreren Datenelementen reduziert (sonst*}
{*                          Verbindungsabbruch); globale Exception-Behandlung *}
{*                          in Execute-Methode des Abruf-Threads              *}
{*	    18.07.2003	WW  V. 1.2  Gerätetypen KE-Ruhrgas, KE-PPN und Datacon    *}
{*                          FWU aufgenommen; falsche Fehlerrückgabe "COM nicht*}
{*                          frei" beim Verbindungsaufbau korrigiert (Ursache: *}
{*                          Verbindungsunterbrechung bei v-Kommando, jetzt    *}
{*                          "sonstiger Fehler"); Zusammensetzen des XML-String*}
{*                          beschleunigt                                      *}
{*	    13.10.2003	WW  V. 1.21  Gerätetyp KE-PPN: Messwert-Datenrückgabe     *}
{*                          erweitert um Kanäle für Normdichte-Status und     *}
{*                          Brennwert-Status; DSfG-Abruf, Verbindungsaufbau:  *}
{*                          jetzt mit Status-Rückgabe beim Lesen der DSfG-DFÜ-*}
{*                          Versionsdaten und der DSfG-DFÜ-Adressen           *}
{*	    15.01.2004	WW  V. 1.3  Zeitfilter für Datenrückgabe;                 *}
{*                          Timer zum Nachstarten von Abrufthreads; alle      *}
{*                          Filezugriffe jetzt threadsicher mit TFileStream;  *}
{*                          Content-Length in XML-Rückgaben berichtigt;       *}
{*                          Timeout für DSfG-Konfiguration-Einlesen entfällt, *}
{*                          dafür allg., verkürzter Timeout für Datenelemente-*}
{*                          Lesen; Strukturerweiterung MRGDEF.DAT;            *}
{*	    05.02.2004	WW  V. 1.31  Fehler beim Rohfiles-Zusammenfassen in       *}
{*                          Messwert-Konvertierung MRG3113 behoben            *}
{*	    31.03.2004	WW  V. 1.4  Erweiterungen/Anpassungen für WK22-System;    *}
{*                          SRVCFG32.INI ersetzt ABRUFCFG.INI;                *}
{*                          COM offen halten, wenn FUP angeschlossen;         *}
{*                          Einbau Tritschler VC2 und TTG (IEC-Protokoll);    *}
{*                          Erweiterungen MRG: Messwertabruf mit Kanalmaske,  *}
{*                          Rundpufferreset für Meldungen und Messwerte, Para-*}
{*                          metrierung, mit allen MRG-Typen wie im WICOM;     *}
{*                          Erweiterungen DSfG: Archiv-/Logbuchdatenabruf über*}
{*                          Ordnungsnummer, Rückgabe Konv-Errorcode für       *}
{*                          Archivkanal/Logbuch/Datenelemente;                *}
{*                          alle Messagebox-Ausgaben ersetzt durch Ausgabe in *}
{*                          Programm-Fenster und Logfile WICOMSRV.LOG;        *}
{*                          DSfG-Abruf MRG910 über Zeit mit Status im Kommando*}
{*                          (Gerätefehler: Ordnungsnummern in den gelieferten *}
{*                          Archivdaten falsch); Speicherleck (FillChar) be-  *}
{*                          seitigt;                                          *}
{*                          IFDEF GAS-X für alle speziellen Gas-X-Funktionali-*}
{*                          täten und zum Ausblenden aller WK22-Erweiterungen *}
{*                          bzgl. Client-Requests und Datenrückgaben;         *}
{*                          mit Zuordnungsressource "MRG-Typnummer Gas-X ->   *}
{*                          MRG-Typnummer Wieser"                             *}
{*	    08.04.2004	WW  V. 1.41  Fehler in Daten-Konvertierungen für KE-      *}
{*                          Stationen behoben (falscher Zeitstempel wegen     *}
{*                          Schaltjahr 2004, Verschiebung um 1 Tag)           *}
{*	    15.04.2004	WW  V. 1.42  Fehler in  Messwertkonvertierung für TTG     *}
{*                          mit IEC-Protokoll behoben (Rohdatei für Kanal 2   *}
{*                          wurde nicht gelöscht)                             *}
{*	    04.05.2004	WW  V. 1.43  Fehler Kanalmaske bei MW/TA-Abruf behoben    *}
{*                          (trat in Gas-X-Version auf); Erweiterung für TTG  *}
{*                          mit IEC-Protokoll: 1-Kanal-Version erkennen, Kon- *}
{*                          vertierung der Zählerstände des letzten Monatsab- *}
{*                          schlusses                                         *}
{*          21.07.2004  WW  V. 1.44 Auswertung außerplanmäßiger DSfG-Antworten*}
{*          16.08.2004  WW  V. 1.5 mit Zeitsynchronisation der DSfG-DFÜ bei   *}
{*                          DSfG-Abruf; Modemantwort 'BLACKLISTED' auswerten  *}
{*          23.08.2004  WW  V. 1.51 mit Korrektur Gastagbildung bei TTG-IEC   *}
{*                          (TTG ab Vs. 7.x liefern bereits Gastag bei Monats-*}
{*                          abschluß-Zählerstand)                             *}
{*          09.10.2004  WW  V. 1.52 Versionsinfos jetzt enthalten             *}
{*          29.10.2004  WW  V. 1.53 WNET_LIZ.DAT, SRVCFG32.INI, MODEM.INI über*}
{*                          WNetProgDir, Log-Files über ParamStr(0) ansprechen*}
{*          19.11.2004  WW  V. 1.6 mit SMS-Datenempfang (über Lizenz); DSfG-  *}
{*                          Stationsabrufe über TCP/IP                        *}
{*          16.12.2004  WW  V. 1.61 mit MRG 4210                              *}
{*          21.12.2004  WW  V. 1.62 mit DSfG-Datenelemente-Parametrierung     *}
{*          12.01.2005  WW  V. 1.63 mit DSfG-DFÜ-Momentanwertabruf/Parametrie-*}
{*                          rung; Erweiterung Server-Verbindungsaufbau-       *}
{*                          Kommando um Feld "DSfG-Transparentmodus"          *}
{*          01.02.2005  WW  V. 1.64 Kommando-Wartezeitmp_ verkleinert (schnellere*}
{*                          Reaktion auf neues Client-Kommando !)             *}
{*          16.02.2005  WW  V. 2.0 mit Rufentgegennahme; Responseteil der XML-*}
{*                          Parameter-Antwort für Gas-X jetzt ohne Feld 'Wert'*}
{*          11.04.2005  WW  V. 2.01 mit Lesen der Geräteversion bei MRG800PTB-*}
{*                          Messwertabruf (nur Gas-X-Variante); Korrektur MW- *}
{*                          Konvertierung MRG 800, MRG 800PTB, EC 694 (Byte-  *}
{*                          Überlauf bei 120 K, Gas-X)                        *}
{*          28.04.2005  WW  V. 2.02 Rufentgegennahme: zusätzliche sofortige   *}
{*                          Meldung an Client, wenn Ruf ansteht               *}
{*          11.07.2005  WW  V. 2.03: mit Überprüfung des Befehlsbuchstabens in*}
{*                          Antwort von Wieser MRGs (wg. MRG 910 mit Weck-    *}
{*                          antwort auf Kennung-Abfrage); Stellenzahl für Ruf-*}
{*                          nummer in Verbindungsaufbau-Kommando vergrößert;  *}
{*                          Bugfix Parameterabruf IEC 1107-Protokoll (Elster  *}
{*                          DL240, EK260)                                     *}
{*          26.07.2005  WW  V. 2.04: mit Auswertung der Baudraten-Identifizie-*}
{*                          rung (IEC 1107-Protokoll) für neue Geräteversionen*}
{*                          Elster EK260                                      *}
{*          04.08.2005  WW  V. 2.05: Erweiterte Plausibilitätsprüfung bei SMS-*}
{*                          Datenkonvertierung Veribox                        *}
{*          21.09.2005  GD  V. 2.06: Fehlertoleranz bei DSfG-Telegrammen      *}
{*          05.10.2005  WW  V. 2.07: Timeout für Datenabruf Tritschler-IEC-   *}
{*                          Protokoll erhöht (für VC2 Vs. 3.xx)               *}
{*          14.11.2005  WW  V. 2.1:  Wieser-MRG-Abruf von Originalwerten;     *}
{*                          Senden der Server-Antwort umgestellt (jetzt über  *}
{*                          gemerkten Zeiger auf Client-Socket statt Port und *}
{*                          Adresse); mit Elster DL 220                       *}
{*          06.12.2005  WW  V. 2.2: PIN-Login für GSM-Modems jetzt auch bei   *}
{*                          MRG-/DSfG-Abrufen und Rufentgegennahme; PIN-Sperre*}
{*                          in Schnittstellenkonfigurtion setzen bei Login-   *}
{*                          Versuch mit falscher PIN; Gas-X-Version: log.     *}
{*                          Schnittstellen ab 51 als IP-Abruflinien           *}
{*          02.02.2006  WW  V. 2.21: Abruf IEC 1107-Geräte alternativ mit     *}
{*                          Schnittstellen-Parametern '8N1' (für Elster DL240,*}
{*                          EK260)                                            *}
{*          15.03.2006  WW  V. 2.22: MRG 905: Korrektur MRG 910-Konvertierung *}
{*                          (Zeitstempel und Satzstatus Tagessätze); DSfG-    *}
{*                          Abrufkommando mit Status wie bei MRG 910          *}
{*          25.04.2006  WW  V. 2.3: serielle Datenformate 8N1, 7E1 für MRG-   *}
{*                          Abruf einstellbar über Feld 'Modemtyp' im Verbin- *}
{*                          dungsaufbau-Kommando; MRG-Zeitsynchronisation für *}
{*                          Elster DL 220/240, EK 260                         *}
{*          05.05.2006  WW  V. 2.31: Standardwert für min. Abweichung bei     *}
{*                          Zeitsynchronisation jetzt 10s (Gas-X)             *}
{*          07.08.2006  WW  V. 2.4: MRG 905/910-SMS-Daten (Formate v, r ->    *}
{*                          DSfG-Archivdaten/Logbuchdaten); Bugfix: Endlos-   *}
{*                          schleife bei Modeminitialisierung                 *}
{*          11.10.2006  WW  V. 2.5: FUP-Abruf mit/ohne RTS-Handshake über     *}
{*                          SRVCFG32.INI; gesicherter SMS-Empfang bei Problem *}
{*                          in Client-Weiterverarbeitung (SMS-Dateien als     *}
{*                          Zwischenpuffer, Empfang einer Client-Bestätigung  *}
{*                          bei erfolgreicher SMS-Verarbeitung)               *}
{*          31.10.2006  WW  V. 2.51: vor FUP-Reset COM schließen und wieder   *}
{*                          öffnen (wg. Problem mit FUP-Betrieb an ComServer- *}
{*                          Schnittstellen)                                   *}
{*          10.01.2007  WW  V. 2.52: mit Fehlerkonstanten 'TCPIP_ERR_LOOKUP', *}
{*                          'TCPIP_ERR_UNDEFINIERT' bei DSfG-IP-Kommunikation *}
{*          23.01.2007  WW  V. 2.53: Bugfix 'Ungültiges Thread-Handle' beim   *}
{*                          Beenden (TThread.WaitFor in CloseAbrufControls)   *}
{*          14.03.2007  WW  V. 2.54: erweiterte Prüfungen für DSfG-Archiv-,   *}
{*                          Logbuch- und Datenelemente-Rohdaten (Telegramm-   *}
{*                          syntax, plausible Telegramminhalte, Prüfung der   *}
{*                          korrekten Zuordnung zu abgefragter DE-Adresse     *}
{*                          jetzt auch bei Logbuchdaten); mit Wiederholung bei*}
{*                          ungültigen Archiv-, Logbuch- und Datenelemente-   *}
{*                          rohdaten (Fehler ERZ2000, Vs. 1.3.1); MRG-Abruf   *}
{*                          Elster DL220/240, EK260 mit CL-Schnittstelle      *}
{*          05.04.2007  WW  V. 2.55: nochmal DSfG-Rohdatenprüfung: Fehler beim*}
{*                          Bilden von Ordnungsnummer/Zeit/DE-Adresse für     *}
{*                          Folgeabruf abgefangen und erweiterte Fehlercodes; *}
{*                          für GAMESS: XML-Responselog erweitert auf Rohfile *}
{*                          namenliste;                                       *}
{*          16.05.2007  WW  V. 2.56: schon wieder DSfG-Rohdatenprüfung:       *}
{*                          Prüfung von Datenelemente-Rohdaten auf Felder     *}
{*                          Unixzeit, Ordnungsnummer, Status und CRC erweitert*}
{*                          (MRG 910-Fehler bei DE-Adressen ca??d), Schreiben *}
{*                          von WicomSrv_DataErr.log (unplausible Rohdaten)   *}
{*                          über INI-Schalter COMProtokoll ein-/ausschaltbar; *}
{*                          Abruf Tritschler VC2 über Multiplexer SSU K935;   *}
{*                          VC2-Messwerte mit aktuellen Zählerständen zum Aus-*}
{*                          lesezeitpunkt (nur Gas-X-Version); COM-Logfile mit*}
{*                          BCC/CRC-Fehlerprotokollierung                     *}
{*          04.06.2007  WW  V. 2.57: Login für Tritschler Multiplexer SSU mit *}
{*                          Wiederholungen                                    *}
{*          05.06.2007  WW  V. 2.58: MRG 910-Abruf mit 8 Kanälen (4 Analog)   *}
{*          04.07.2007  WW  V. 2.59: Abruf Tritschler Multiplexer SSU: jetzt  *}
{*                          über Modemtyp 'T' im Verbindungsaufbau-Kommando   *}
{*                          und max. 4 Loginversuche                          *}
{*          10.07.2007  WW  V. 2.60: auch bei Kommunikationsfehler werden alle*}
{*                          korrekt empfangenen DSfG-Teilantwort-Telegramme   *}
{*                          einer DE-Adresse konvertiert                      *}
{*          04.10.2007  WW  V. 2.61: mit Meldung an zentrale Systemüberwachung*}
{*                          (OK, Modem-Initialisierungsfehler)                *}
{*          05.11.2007  WW  V. 2.62: resourcestrings                          *}
{*          03.03.2008  WW  V. 2.7: Veribox SMS-Datenkonvertierung erweitert  *}
{*                          für Analogkanäle; MRG-FUP-Abruf: Empfangsdaten    *}
{*                          blockweise in Rohfile/Logfile schreiben; Bugfix   *}
{*                          Passwort-Login für KE-Anlagen: ohne MRG-Antwort-  *}
{*                          validierung; Abruf Tritschler TTG mit FTL-Prot.;  *}
{*                          Abruf Tritschler TDS; Abruf Tritschler TTG-IEC,   *}
{*                          TDS über Multiplexer SSU K935; Bugfix Konvertier- *}
{*                          ung TTG-IEC: 24-Uhr-Zeitstempel bei Zählerständen;*}
{*                          Überarbeitung SMS-Empfang: Weitergabe der SMS-    *}
{*                          Daten an Client erst, wenn alle SMS aus Modem ge- *}
{*                          lesen wurden (evtl. mehrere Lesevorgänge)         *}
{*          04.03.2008  WW  V. 2.71: optionaler Kennungsvergleich ohne Kenn-  *}
{*                          ungserweiterung bei Tritschler Multiplexergeräten *}
{*                          (WicomSrv.ini)                                    *}
{*          28.03.2008  WW  V. 2.72: Abruf Tritschler TTG-FTL über Multiplexer*}
{*                          SSU K935; Bugfix SMS-Konvertierung Veribox: neg.  *}
{*                          Zählerstandsdifferenz als Überlauf kennzeichnen   *}
{*                          (Stundenwert); optionaler Kennungsvergleich ohne  *}
{*                          Kennungserweiterung auch bei Nicht-Multiplexer-   *}
{*                          geräten; Bugfix Kennungsrückgabe bei DSfG-Umlei-  *}
{*                          tung: Default ist Kennung aus Umschaltkommando    *}
{*                          statt Vorgänger-Kennung                           *}
{*          10.04.2008  WW  V. 2.73: Tritschler-Abruf über Multiplexer: bei   *}
{*                          Umschaltung des Ausgangs Abschaltbefehl (bei FTL- *}
{*                          Protokoll) und Break-Befehl senden (Bugfix), Abruf*}
{*                          mit 8N1 (kombinierte Modemtyp-Konstanten 'TA');   *}
{*                          Fehlercode LOGINERR_TIMEOUT_WRONGPW bei Login     *}
{*          30.04.2008  WW  V. 2.74: Wecksequenz für Tritschler Geräte mit    *}
{*                          IEC-Protokoll überarbeitet                        *}
{*          16.06.2008  WW  V. 2.75: mit Rufentgegennahme und Rückrufauslösung*}
{*                          für Tritschler Stationsrechner; mit Zeitsynchroni-*}
{*                          sation für Tritschler VC2, TDS                    *}
{*          23.06.2008  WW  V. 2.76: mit Zeitsynchronisation für EC694;       *}
{*                          Bugfix Integerüberlauf bei Veribox SMS (Datum in  *}
{*                          in Rohdaten)                                      *}
{*          26.06.2008  WW  V. 2.77: Bugfix "Wert fehlend" bei Konvertierung  *}
{*                          von TDS-Zählerständen in normierte Stundenmengen  *}
{*          08.09.2008  WW  V. 2.78: mit Actaris Corus                        *}
{*          16.10.2008  WW  V. 2.79: Erweiterung Server-Transparent-Kommando  *}
{*                          um Felder "Timeout" und "EmpfModus"; Bugfix:      *}
{*                          TSerialMRGModem.WakeUpMRG_ByCommand (Endlos-      *}
{*                          schleife Y-Befehl); Actaris Corus: mit zusätz-    *}
{*                          lichem Neu-Initialisieren der Gerätekommunikation *}
{*                          bei Timeout während des Abrufs; Abruf Parameter-  *}
{*                          Logbuch und eichtechnisches Logbuch;              *}
{*                          Vorbereitung Actaris Sparklog                     *}
{*          03.12.2008  WW  V. 2.80: mit Actaris Sparklog und Elster DL210;   *}
{*                          mit max. Korrekturzeit bei Zeitsynchronisation    *}
{*                          (optional); Bugfix Login-Rohdatei löschen bei     *}
{*                          Tritschler-Abruf SR und über Multiplexer SSU;     *}
{*                          SMS-Bestätigungsmodus 'Wiederholung';             *}
{*                          mit SMS-Messdaten-Abruf für Veribox Mini          *}
{*          17.12.2008  WW  V. 2.81: mit Tritschler MCO                       *}
{*          21.01.2009  WW  V. 2.82: MRG-Datenkonvertierungen: Impulswerte-   *}
{*                          bereich für normierte LGZ-Daten vergrößert;       *}
{*                          Bugfix: MRG-Gerätetyp aktualisieren bei DSfG-Um-  *}
{*                          schalt-Kommando, Gas-X-Version: Gas-X-spezifische *}
{*                          Gerätetypnummer in XML-Antworten zurückgeben;     *}
{*                          Konvertierung Veribox-SMS: mit Datenlücke-Prüfung *}
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
{*                          Prüfung auf Rohdatenende über Gerätedatum/-zeit   *}
{*                          aus Parameterliste; Abruf Actaris Sparklog: mit   *}
{*                          Wiederholung beim Senden des Aufforderungstele-   *}
{*                          gramms (Init_IEC1107_Kommunikation) und verkürztem*}
{*                          Timeout                                           *}
{*          18.06.2009  WW  V. 3.01: mit EC 900; MRG-Zeitsynchronisation:     *}
{*                          Einschränkung "nur mit Passwort 1" entfernt für   *}
{*                          MRG 800, 800 PTB, 905, 910; DSfG-Rohdatenprüfung: *}
{*                          Keine Längenprüfung mehr bei Archivwerten (Texte  *}
{*                          können leer sein); Konvertierung Tritschler TDS,  *}
{*                          ZS-Differenzbildung für normierte LGZ-Daten: mit  *}
{*                          Toleranzbereich für Zeitstempel ungleich Mess-    *}
{*                          periodenende                                      *}
{*          09.07.2009  WW  V. 3.02: Veribox Mini (SMS): Originaldaten von    *}
{*                          Impulskanälen jetzt als Mengen (vorher Zähler-    *}
{*                          stände; Kompatibilität zu Daten per e-Mail/FTP)   *}
{*          30.07.2009  WW  V. 3.03: Bugfix DSfG-DFÜ-Parametrierung: Para-    *}
{*                          metrierung von Wieser-Parametern 2-stufig (ohne   *}
{*                          und mit abschließenden Leerzeichen)               *}
{*          22.09.2009  WW  V. 3.04: Bugfix Konvertierung Tritschler TDS: MW- *}
{*                          Originaldaten mit Nachkommastellen; mit Tritschler*}
{*                          VC3; mit zusätzlichem Aufweck-Timeout bei Trit-   *}
{*                          schler IEC-Protokoll-Kommandos                    *}
{*          02.10.2009  WW  V. 3.05: DSfG-Abruf: mit Abfrage von Fabriknummer *}
{*                          und Baujahr der DSfG-DFÜ (für Konfiguration-Ein-  *}
{*                          lesen)                                            *}
{*          14.10.2009  WW  V. 3.06: Bugfix Konvertierung Tritschler TDS: Zäh-*}
{*                          lerstand des letzten Kanals mit Nachkommastellen  *}
{*          21.12.2009  WW  V. 3.07: Kennziffer für Tritschler VC3-Kennung per*}
{*                          INI einstellbar; Abruf Actaris Sparklog: Daten-   *}
{*                          abruf über Kommunikationsroutine 'SendCommand'    *}
{*                          statt 'SendIEC1107DatenTelegramm' (keine Quittie- *}
{*                          rung, keine Teildatensätze !); Bugfix Kennungsab- *}
{*                          frage bei MRG-Rufentgegennahme von Wieser-Geräten *}
{*          25.01.2010  WW  V. 3.08: Anpassungen für RohTRec-Struktur mit Wert*}
{*                          als double                                        *}
{*          03.03.2010  WW  V. 3.09: Zeichen-Konvertierung ASCII/ISO 646 bei  *}
{*                          DSfG-Abruf (DE lesen/einstellen, Archivwerte      *}
{*                          lesen), über INI-Schalter ISO646 ein-/ausschaltbar*}
{*          24.03.2010  WW  V. 3.10: Plausibilisierung Datum in               *}
{*                                   SMS-Zwischendatei                        *}
{*          12.07.2010  GD  V. 3.11: DSfG Erweiterungsgrad 2 als ErwGrd 1     *}
{*          17.08.2010  GD  V. 3.12: Neues Zeitkommando "t" (für GasX), DSfG  *}
{*          06.10.2010  GD  V. 3.13: Beschränkung ModemNr > 50 = IP Abruf     *}
{*                          für Gas-X entfernt                                *}
{*          05.11.2010  GD  V. 3.14: Bugfix;                                  *)
{*                          Modemeinschränkung für GAS-X LogCom -> PhysCom    *}
{*                      WN  V. 3.14: Nachholen von zwischengespeicherten SMS  *}
{*          13.01.2011  GD  V. 3.15: Neues Zeitkommando "t" (für GasX), MRG   *}
{*                                   BCC-Fehler bei VC3 umgangen              *}
{*          24.02.2011  GD  V. 3.16: Zeichenhandling XML / nur bei Hersteller *}
{*          24.03.2011  GD  V. 3.17: Bugfix: Zeichenhandling XML / t-Befehl   *}
{*          05.05.2011  WW  V. 3.18: Modem-Verbindung bei MRG-/DSfG-Abrufen:  *}
{*                          Wartezeit nach Connect-Antwort erhöht (für Ver-   *}
{*                          bindungen zu Tainy-Zentrale)                      *}
{*          14.06.2011  WW  V. 3.19: Bugfix Verbindungsaufbau/Rufentgegennahme*}
{*                          DSfG: Stations-Kennung zurückgeben auch bei Ver-  *}
{*                          bindungsaufbau-/Rufentgegennahme-Fehler           *}
{*          20.07.2011  WW  V. 3.2: Wieser DSfG-DFÜ-Parameter mit YWa-Befehl  *}
{*                          lesen (NG)                                        *}
{*          16.08.2011  WW  V. 3.3: Serielles Auslesen; COM vor dem Öffnen auf*}
{*                          Vorhandensein prüfen; mit Elster DS-100; XML-Res- *}
{*                          ponse erweitert um Basis-Fehlercode (RMG-Version);*}
{*                          benötigt AlmProc32 und AlmIPProc32 ab Vs. 2.2     *}
{*          19.10.2011  WW  V. 3.31: mit VC3; VC2-Fehlercode als Meldungen    *}
{*                          auslesen; Bugfixes: MRG-Modemabruf, XML-Response  *}
{*                          für DSfG-Umschaltung mit Basis-Fehlercode         *}
{*          09.11.2011  WW  V. 3.32: Bugfix BCC-Fehlerprüfung, wenn Startzei- *}
{*                          chen STX, SOH nicht vorhanden (EK260, RWE);       *}
{*                          Timeout_Init_IEC1107 von 6 s auf 7,5 s erhöht     *}
{*                          (EK260, RWE)                                      *}
{*          14.12.2011  WW  V. 3.33: DSfG-DFÜ-Befehl V: Antwort 'Nicht inter- *}
{*                          pretierbare Dateneinheit' (STX ? ETX) abfangen;   *}
{*                          IP-Abrufe: Bugfix Connect bei IP-Adresse mit      *}
{*                          führenden Nullen                                  *}
{*          27.01.2012  WW  V. 3.4: mit Verifizierung signierter DSfG-Daten   *}
{*                          über Signaturserver (öffentlicher Schlüssel global*}
{*                          in Prog\Signatur_PublicKey.TXT hinterlegt),       *}
{*                          Protokollierung des Verifizierungsergebnis in     *}
{*                          WicomSrv_SignaturVerify.log; XML-Ersatzstring für *}
{*                          Zeichen '&' korrigiert                            *}
{*          05.03.2012  WW  V. 3.41: mit Firmware-Update für NG-DSfG-DFÜ per  *}
{*                          Verbindungsaufbau-Kommando; Gültigkeitsprüfung von*}
{*                          DSfG-DFÜ-Antworten erweitert für NG ('Antwort ver-*}
{*                          boten'); Öffentlicher Signatur-Schlüssel per      *}
{*                          Client-Kommando 'DSfG-Messwertabruf', Signatur-   *}
{*                          Verifizierungsergebnis der abgefragten DSfG-      *}
{*                          Archivdaten in Antwort mitzurückgeben             *}
{*          30.04.2012  WW  V. 3.42: Zusätzliche Rohdaten-Plausibilisierung   *}
{*                          bei Elster IEC1107-Archiv- und Logbuchdatenabfrage*}
{*                          (Problem RWE mit undefinierter Antwort von Gegen- *}
{*                           stelle bei EK260-Abruf)                          *}
{*          07.05.2012  WW  V. 3.43: Parametrierung DSfG-Datenelement: Erweit-*}
{*                          erte Prüfung Soll/Ist-Wert (Mögliche Fehlermeldung*}
{*                          bei erfolgreicher Änderung von Float-Werten);     *}
{*                          Signatur, DSfG-PTB-Feld: Prüfung auf Trennzeichen *}
{*                          ';' (DSfG-konform) und ':' (ERZ2000 Vs. 2.0.0,    *}
{*                          nicht DSfG-konform)                               *}
{*          23.05.2012  WW  V. 3.44: Nochmaliger Bugfix BCC-Fehler bei VC3    *}
{*                          (Schmutzzeichen "Space" nach dem BCC-Zeichen);    *}
{*                          mit IFDEF 'NO_XML_SIGVERIFYSTATE' für XML-Response*}
{*                          von DSfG-Archiv- und Logbuchdaten ohne Tag für    *}
{*                          Signaturverifizierungsstatus (Workaround für in-  *}
{*                          kompatibles Verhalten von AlmProc32/AlmIPProc32   *}
{*                          bei XML-Datenstrukturerweiterung)                 *}
{*          19.06.2012  WW  V. 3.45: DSfG-Signaturverifizierungsfehler in     *}
{*                          Fehlergruppe/-code und in convstate des Response- *}
{*                          log zurückliefern; auf Änderung der Schnitt-      *}
{*                          stellenkonfiguration 'Debug COM-Protokoll' während*}
{*                          des Rufpolling prüfen                             *}
{*          17.07.2012  WW  V. 3.46: DSfG: Erweiterungsgrad 2; Firmware-      *}
{*                          Update: Einstellbare Parameter lesen mit sb-Befehl*}
{*                          und Systemzeit-Passwort (NG unterstützt b-Befehl  *}
{*                          nicht mehr in Versionen ab 05/2012); Bugfix NG-   *}
{*                          Systemzeitpasswort codieren; DSfG: Wieser-Teilneh-*}
{*                          merliste '!!!2-LS-active!!!' abfangen             *}
{*          16.08.2012  WW  V. 3.47: Verbindungsaufbau-Kommando: Automatisches*}
{*                          Ermitteln des seriellen MRG-Datenformats (Modemtyp*}
{*                          = 0) und der Passwortnummer (PasswortNr = 0) für  *}
{*                          Elster IEC1107-Geräte; Bugfix Konvertieren von    *}
{*                          Parametern von Elster IEC1107-Geräten mit mehreren*}
{*                          Einzelwerten (Stati); DSfG-Datenelemente-Abruf mit*}
{*                          optionalem Timeout aus Server-Parameterabruf-     *}
{*                          Kommando                                          *}
{*          25.09.2012  WW  V. 3.48: Actaris Corus: Beschleunigter Parameter- *}
{*                          Abruf (Parameter-Gruppenbefehle), Parametrieren;  *}
{*                          ZAE Soll/Ist-Prüfung DSfG-Rohdaten: OK, wenn kein *}
{*                          ungültiger Rohsatz enthalten (GC9000, DE a-z)     *}
{*          07.11.2012  WW  V. 3.49: Bugfix Rohdatenplausibilisierung bei     *}
{*                          MW/ME-Abruf von Elster-Geräten mit IEC 1107-Proto-*}
{*                          koll (DL210, DL220, DL240, EK260)                 *}
{*          04.01.2013  WW  V. 3.5: mit Verifizierung signierter DSfG-Daten   *}
{*                          aus Datenelemente-Abruf                           *}
{*          24.01.2013  WW  V. 3.51: Bugfix in DSfG-Datenelemente-Konvertier- *}
{*                          ung (TDELList): Signaturverifizierungs-Fehler     *}
{*                          jetzt mit richtigem Fehlercode zurückgeben (bisher*}
{*                          "Konvertierungsfehler Datenelemente")             *}
{*          27.02.2013  WW  V. 3.52: Bugfix DSfG-Verbindungsaufbau: Fehler    *}
{*                          'Kennung falsch, Verbindung bleibt bestehen' wird *}
{*                          jetzt zurückgegeben; MRG-Stationsabrufe über      *}
{*                          TCP/IP                                            *}
{*          27.03.2013  WW  V. 3.53: mit Kamstrup UNIGAS 300; mit Simulations-*}
{*                          betrieb ohne DSfG-Gerätekommunikation (IFDEF SIMU)*)
{*                          Messwerte-Konvertierung Elster EK260, DL 210/220/ *}
{*                          240 mit doppelter 2-Uhr-Stunde bei SZ/WZ-Wechsel; *}
{*                          MRG-Konvertierung: Rundung der Tagessatz-Werte    *}
{*                          raus (EC900, EK260, DL210/220/240, TTG-IEC, TDS,  *}
{*                          MCO, VC3, FWU, Corus, Sparklog, UNIGAS 300);      *}
{*                          für Gas-X: Modem-Verbindung halten, solange kein  *}
{*                          Client-Befehl vorliegt (nur DSfG, per WicomSrv.ini*}
{*                          aktivierbar)                                      *}
{*          17.05.2013  WW  V. 3.54: mit DSfG-Zeitsynchronisation über XML-Z- *}
{*                          Befehl (ZeitSynch-Rundsendung); Bugfix: Signatur- *}
{*                          fehler-Rückgaben ausblenden bei XML-Kommandos     *}
{*                          "DSfG-Busanalyse" und "Zeitabruf"                 *}
{*          28.05.2013  WW  V. 3.55: Messwert-Abruf MRG 905/910 mit k-Faktor; *}
{*                          Bugfix Rückgabe Fehlercode 'GetLastError', wenn   *}
{*                          ClearCommError fehlschlägt (Absturz, wenn z.B. COM*}
{*                          während des Datenempfangs nicht mehr vorhanden);  *}
{*                          Default bei Dienst-Registrierung: Keine Inter-    *}
{*                          aktion zwischen Dienst und Windows-Desktop;       *}
{*                          Rufentgegenname: Schnittstelle neu initialisieren *}
{*                          nach erfolgloser Modeminitialisierung/FUP-Reset   *}
{*          03.07.2013  WW  V. 3.56: IFDEF NO_BDETEMPDIR (ohne BDE-Temp-      *}
{*                          Verzeichnis-Aktionen in WTables.pas); max. 10     *}
{*                          Versuche für das Einfügen des Taskbar-Icons       *}
{*          31.07.2013  WW  V. 3.57: Firmware-Update NG-DSfG-DFÜ: Timeout-    *}
{*                          Zeiten für das Übertragen und Flashen der Firm-   *}
{*                          ware-Binärdaten erhöht (Fehler: "Timeout beim     *}
{*                          Warten auf Daten" wegen längerer Laufzeiten bei   *}
{*                          GSM-/GPRS-Verbindung), Auswertung NG-Fehlerantwort*}
{*                          'X' beim Übertragen der Firmware-Binärdaten       *}
{*          26.09.2013  WW  V. 3.58: Messwert-Abruf EK260 erweitert auf Kanäle*}
{*                          für K-Zahl und Z-Zahl                             *}
{*          09.10.2013  WW  V. 3.59: Corus-Messwerte mit aktuellen Zähler-    *}
{*                          ständen zum Auslesezeitpunkt (nur Gas-X-Version); *}
{*                          Stehende IP-Verbindungen zu MRG-/DSfG-Geräten     *}
{*                          explizit beenden, wenn Dienst beendet wird        *}
{*          19.11.2013  WW  V. 3.60: Anpassungen GSM-Modeminitialisierung für *}
{*                          Sierra Wireless Modem USB (PIN-Statusabfrage,     *}
{*                          PIN-Login); Gas-X-Version: Rufnummer aus XML-     *}
{*                          Verbindungsaufbau-Kommando um COM-spezifische     *}
{*                          Vorwahl aus SrvCfgIni-Konfiguration erweitern     *}
{*          03.12.2013  WW  V. 3.7.0: Neuer Dienstname (nicht Gas-X-Version)  *}
{*                          und Dienst-Anzeigename; Rundpuffer für COM- und   *}
{*                          Abruf-Logfile über Schnittstellen-Konfiguration;  *}
{*                          auf geänderte Einstellungen der Schnittstellen-   *}
{*                          Konfiguration während Laufzeit prüfen: COM- und   *}
{*                          Abruf-Logfile, Rundpuffer auf Logfiles; Firmware- *}
{*                          Update NG-DSfG-DFÜ: Timeout-Zeit für das Initiali-*}
{*                          sieren der Firmware-Übertragung erhöht (Fehler:   *}
{*                          "Undefinierter Fehler beim Übertragen der Firm-   *}
{*                          ware-Binärdaten"); XML-Responses 'Verbindungsauf- *}
{*                          bau', 'Rufentgegennahme', 'Rufannahme' erweitert  *}
{*                          um Verbindungsinformationen (Zeitpunkte 'Verbin-  *}
{*                          dung steht', 'Login erfolgt'); Bugfix Rufentgegen-*}
{*                          nahme (Fehler 'Timeout beim Warten auf Daten',    *}
{*                          ATHR-Datei: 'Kommando-Timeout, keine weiteren Kom-*}
{*                          mandos vorhanden'); mit Elster EK 280; Abruf      *}
{*                          Archive/Meldungen Elster DL/EK-Serie mit Block-   *}
{*                          größe 10                                          *}
{*          04.03.2014  WW  V. 3.7.1: Bugfix Gas-X-Version: Messwerte-Konver- *}
{*                          tierung von aktuellen Corus-Zählerständen der Gas-*}
{*                          tagende-Stunde                                    *}
{*          21.03.2014  WW  V. 3.7.2: mit Gerätezustand für DL 240, 220, 210, *}
{*                          MRG 905, 910                                      *}
{*          25.04.2014  WW  V. 3.7.3: Parameter- und Zeitabruf für EC694;     *}
{*                          SMS-Puffer-Dateien mit Zeitstempel der Erstellung *}
{*                          im Dateiname (Bugfix für mehrfache SMSen mit glei-*}
{*                          chem Zeitstempel)                                 *}
{*          16.05.2014  WW  V. 3.7.4: DSfG-Parametrierung: bei Prüfung des    *}
{*                          Parametrierergebnisses abschließende Spaces in    *}
{*                          Soll- und Ist-Datenelementwert ignorieren;        *}
{*                          MRG-Parametrierung Wieser/RMG-Geräte: bei Fehlpa- *}
{*                          rametrierung Wiederholung mit modifiziertem neuem *}
{*                          Parameterwert                                     *}
{*          23.05.2014  WW  V. 3.7.5: mit MRG-Abruf für Elster DL 230; abge-  *}
{*                          rufene MRG-Meldungen nach Datum/Zeit sortieren für*}
{*                          Geräte mit mehreren Meldungs-Abrufbefehlen (EC 900*}
{*                          Corus, Sparklog, VC3, UNIGAS 300, EK280, DL 230); *}
{*                          DL 2xx-Reihe: Auslesen Tagesende über Parameter   *}
{*                          'Tagesende Kanal 1'; nur Gas-X-Version: unbegrenz-*}
{*                          te Laufzeit nicht mehr gültig                     *}
{*          10.06.2014  WW  V. 3.7.6: mit Schreiben einer Heartbeat-Datei (per*}
{*                          INI konfigurierbar); MRG-Konfiguration einlesen:  *}
{*                          Warnung bei bereits geöffnetem Zugangsschloss für *}
{*                          Elster DL- und EK-Serie, mit Gerätetypüberprüfung *}
{*                          für: Elster DL- und EK-Serie, Corus, Sparklog,    *}
{*                          UNIGAS 300, Tritschler VC2 VC3 TDS MCO direkt     *}
{*                          (nicht an Multiplexer), MRG 800PTB 905 910, EC900;*}
{*                          MRG-Parameterabruf von Elster EK260/280 Gasqua-   *}
{*                          litätsdaten mit Versions- und K-Zahl Modus-       *}
{*                          abhängigen Parameteradressen (mit ParamEK.dat)    *}
{*          08.08.2014  WW  V. 3.7.7: MRG-Messwerteabruf für normierte LGZ-   *}
{*                          Daten: Schnellerer Abruf durch reduzierte Menge   *}
{*                          abzurufender, für die Messwert-Konvertierung      *}
{*                          benötigter Geräte-Parameter bei Elster DL- und EK-*}
{*                          Serie, Actaris Sparklog, Kamstrup UNIGAS 300 (er- *}
{*                          weiterte Struktur in ParamMrg.dat)                *}
{*          06.11.2014  WW  V. 3.7.8: Bugfix DSfG-DFÜ-NG Firmwareupdate, Rück-*}
{*                         speichern der gesicherten Parameter: Wenn ein oder *}
{*                         mehrere Parameter nicht übertragen werden konnten, *}
{*                         wurde Fehlergruppe 0, Fehlercode 0 zurückgegeben   *}
{*                         ("Nicht dokumentierter Fehler"),                   *}
{*                         wenn Fehler beim Parameter-Wiederherstellen auftritt}
{*                         Log-Datei mit Parametrier-Ergebnissen schreiben    *}
{*                         (ParaRestoreResult_FwUpd_... .log); Bei DSfG-DFÜ-NG*}
{*                         Firmwareupate 'Baujahr' und 'Fabriknummer' über    *}
{*                         Parameter 322, 323 lesen (bisher: 103, 104; Para-  *}
{*                         meter 103 'Fabriknummer' fehlt jedoch in frühen NG-*}
{*                         Versionen, z.B. 00.01)                             *}
{*          06.05.2014  WW  V. 3.8.0: Signatur in Gas-X-Version implementiert;*}
{*                          Allgemein: Signatur-Lizenz prüfen, Base64-kodierte*}
{*                          Rohdaten in XML-Responses für DSfG-Archive,       *}
{*                          -Logbücher, -Datenelemente (E, M, B) per INI ein-/*}
{*                          ausschaltbar; Signaturverifizierung Request-      *}
{*                          gesteuert (wenn XML-Tag 'digitalsignature' vorhan-*}
{*                          den)                                              *}
{*          14.01.2015  WW  V. 3.8.1: Simulation DSfG-Abruf überarbeitet: dy- *}
{*                          namisches Simu-Archiv, Abfrage Archiv-Füllstände, *}
{*                          Folgetelegramme, simulierter Verbindungsaufbau und*}
{*                          Antworttelegramme mit Delay, INI-Konfiguration    *}
{*          21.04.2015  WW  V. 3.8.2: Multi-Client-Fähigkeit für IP-Abrufe    *}
{*                          (ermöglicht gleichzeitige Verwendung der gleichen *}
{*                          IP-Linien-Nr. durch mehrere Clients); Logdateien  *}
{*                          in Log-Pfad der Wieser.ini schreiben              *}
{*          31.07.2015  WW  V. 3.8.3: Simulation erweitert: Simulierter nicht *}
{*                          erfolgreicher Verbindungsaufbau (INI-Konfiguration*}
{*                          für Liniennummern)                                *}
{*          06.10.2015  WW  V. 3.8.4: Beschleunigte Rohdateizugriffe bei Kon- *}
{*                          vertierungen (TFileOfCharStream)                  *}
{*          11.01.2016  WW  V. 3.8.5: mit Tritschler VCC; COM-Logfile mit     *}
{*                          Speicherinfos > 4 GB (GlobalMemoryStatusEx)       *}
{*          11.07.2016  WW  V. 3.8.6: DSfG-Archivgruppen zeilenweise lesen;   *}
{*                          Ausgabe Lizenz-Seriennummer im Program-Logfile    *}
{*          31.01.2017  WW  V. 3.8.7: XML-Response 'Verbindungsaufbau DSfG'   *}
{*                          erweitert um Erweiterungsgrad der Login-DSfG-DFÜ  *}
{*          21.02.2017  WW  V. 3.8.8: Konvertierung MRG-EK280-Messwerte mit   *}
{*                          optionalen, zusätzlichen Kanälen (STGW), auch für *}
{*                          Gas-X-Version)                                    *}
{*          11.04.2017  WW  V. 3.8.9: Kennungsabfrage über frei definierbare  *}
{*                          Kennziffer für Kamstrup UNIGAS 300 und Actaris    *}
{*                          Sparklog                                          *}
{*          05.12.2017  WW  V. 3.8.10: Parameter-Konvertierung Tritschler IEC:*}
{*                          Werte des letzten Monatsabschluß konvertieren;    *}
{*                          MRG-Abruf, Abrufgruppe 11: Korrektur 8. Bit in den*}
{*                          empfangenen 7-Bit-Gerätedaten löschen (für UNIGAS *}
{*                          300, lt. Vorgabe Fa. Wigersma & Sikkema)          *}
{*          08.01.2018  WW  V. 4.0.0: mit TCP/IP-Rufentgegennahme für DSfG    *}
{*          13.03.2018  WW  V. 4.1.0: Erweiterungen für Gas-X-Version: mit er-*}
{*                          weiterten Ergebnis-Rückgaben bei allen Responses; *}
{*                          Busadresse der Login-DFÜ-Instanz zurückgeben in   *}
{*                          v-Response DSfG; Neuer Signaturverifizierungs-    *}
{*                          status 4 bei fehlendem öffentlichem Schlüssel;    *}
{*                          Bugfix C-Response bei DSfG-DFÜ-Parametrierung:    *}
{*                          Parametrier-Ergebnisdaten bei Verstellung eines   *}
{*                          Normparameters fehlten                            *}
{*          07.05.2018  WW  V. 4.1.1: Anpassung DSfG-Busanalyse (Befehl I) an *}
{*                          DFÜ-Instanz Typ E; Gas-X-Version: Vorbereitung    *}
{*                          Erweiterung XML-Response für MRG-Messwertabruf um *}
{*                          Ordnungsnummer für Elster DL2nn- und EK2nn-Geräte *}
{*          28.06.2018  WW  V. 4.1.2: Abruf Tritschler MC2; Zeitsynchronisa-  *}
{*                          tion für Tritschler IEC-Geräte erweitert um Rück- *}
{*                          gabe der Gerätezeit (Auswertung der UTC-Gerätezeit*}
{*                          (Kennziffer 30.) und Verstell-Sekunden (Kennziffer*}
{*                          0.3))                                             *}
{*          24.08.2018  WW  V. 4.2.0: Bugfix fehlender valchange-Abschluß / in*}
{*                          Parametrieren-Response (C-Befehl); Zugriff auf    *}
{*                          Errtxt32.dll eliminiert, Errtxt-Funktionen sind   *}
{*                          jetzt direkt in den Quellcode miteingebunden      *}
{*                          (Funktionsaufruf GetGasXStatus aus Errtxt32.dll   *}
{*                          liefert in Gas-X-Version bei OGE mit sehr vielen  *}
{*                          parallelen IP-Linien Exceptions "Ungültige Zeiger-*}
{*                          operation", "Zugriffsverletzung bei Adresse..."   *}
{*                          -> DLL-Zugriff offensichtlich nicht Thread-fähig) *}
{*          25.02.2019  WW  V. 4.2.1: SendMRGCommand, Abrufgruppe 8: Korrektur*}
{*                          8. Bit in den 7-Bit-Gerätedaten löschen (für      *}
{*                          Tritschler IEC-Geräte (MC2, VC3 etc.) mit neuem   *}
{*                          Ethernet-Modem lt. Vorgabe Fa. Tritschler         *}
{*          08.03.2019  WW  V. 4.3.0: mit MRG-Abruf für RMG Primus 400 (Modbus-*}
{*                          Protokoll): Lesen von Messwerten (Daten-Archiv),  *}
{*                          Meldungen (Status-Archiv), Parameter (aktuelle    *}
{*                          Werte), Zeitsynchronisation (allg. Parametrierung *}
{*                          noch nicht implementiert); Neue Einstellungen in  *}
{*                          WicomSrv.ini: TOModbusProt, ModbusProtLRC_CRCVersuche; *}
{*                          Logdatei WICOMSRV_ModbusErr.log für detailliertere*}
{*                          Ausgabe von Modbus-Fehlern (bei aktiviertem COM-  *}
{*                          Log)                                              *}
{*          30.04.2019  WW  V. 4.3.1: MRG-Parametrierung: Erweitert für Trit- *}
{*                          schler VC2, VC3, VCC (FTL-internes Protokoll,     *}
{*                          Achtung: Laut Nutzungsvertrag mit Fa. Tritschler  *}
{*                          nur für die Parametrierung von Zählerständen und  *}
{*                          Gasanalysedaten im GM-T erlaubt ! Funktion ist in *}
{*                          Gas-X-Version gesperrt);                          *}
{*                          XML-Parametrier-Kommando: bei MRG optional Feld   *}
{*                          "Zugangscode 1" für Parametrier-Passwort und Feld *}
{*                          "Zugangscode 2" für Parametrier-Passwortnummer    *}
{*                          verwendet (zuvor nicht verwendet); Anpassung MRG- *}
{*                          Parameter-Konvertierung für Tritschler IEC-Geräte/*}
{*                          Datacon FWU (Allg. Parameternummern für Parameter *}
{*                          ohne Kennziffer aus Resourcen-Datei lesen, bisher *}
{*                          im Quellcode festverdrahtet; neu zugeordnete Para-*}
{*                          metergruppen bei Tritschler-Geräten);             *}
{*                          Standard-Timeout für MRG-IEC1107-Kommunikation von*}
{*                          10 auf 20 s erhöht (Elster EK per GSM-Modemverbin-*}
{*                          dung); WicomSrv_Signatur-Logdatei nur bei akti-   *}
{*                          viertem ServiceIO-Debugschalter schreiben         *}
{*          11.07.2019  WW  V. 4.3.2: DSfG-Verbindung nicht beenden, wenn nach*}
{*                          dem Transparent-Schalten keine Teilnehmer erkannt *}
{*                          werden oder die DFÜ-Login-Adresse 0 ist (ange-    *}
{*                          steuert per XML-Kommando Verbindungsaufbau, DSfG- *}
{*                          Transparentmodus = I)                             *}
{*          22.07.2019  WW  V. 4.3.3: Erweiterungen und Anpassungen für RMG   *}
{*                          Primus 400, FW-Version 1.12-3873/52FD, Modbusliste*}
{*                          ID 9 mit Archiv-Zeiger: Zeitsynchronisation erwei-*}
{*                          tert um Senden des Passworts; Einbau Parameterein-*}
{*                          stellfunktion; Abruf Meldungs- und Messwert-Archiv*}
{*                          über Archivzeiger-Parameter; Zeitabruf (t-Befehl);*}
{*                          MRG-Messwerte-Abruf: Korrektur des von-Abrufzeit- *}
{                           punkts bei von-Stunde zwischen 0..Tagesende nur   *}
{*                          noch für die Wieser-Geräte (für die die Korrektur *}
{*                          urspünglich erforderlich war (MRG 2xxx, 3xxx) bzw.*}
{*                          sich die Abrufzeit nicht merklich verlängert (MRG *}
{*                          900, EC900)); Parameterausgabe formatiert;        *}
{*                          Modbus-Kommunikation: ByteCount Ist/Soll-Plausibi-*}
{*                          lisierung der Modbus-Responses präzisiert; Modbus-*}
{*                          Werttyp ile (IPv4-Adresse, Little-Endian); Work-  *}
{*                          around Archivzeiger für auf Sommerzeit laufendes  *}
{*                          Gerät                                             *}
{*          10.12.2019  WW  V. 4.3.4: Bugfix Zeitsynchronisation UNIGAS 300   *}
{*                          (Zeitzone im DS7-Rohdatenformat)                  *}
{*          16.12.2019  WW  V. 4.3.5: IP-Abrufe RMG Primus 400 mit Protokoll  *}
{*                          Modbus TCP/IP (bisher Modbus RTU); Bugfix Prüfung *}
{*                          auf vollständige Modbus-Antwort (RTU, TCP/IP):    *}
{*                          Exception-Antwort, unbekannter Funktionscode      *}
{*          25.02.2020  WW  V. 4.3.6: Erweiterung IP-Verbindung zu MRG/DSfG-  *}
{*                          Station per DNS Hostname                          *}
{*          03.06.2020  WW  V. 4.4.0: mit optional verschlüsselter Kommunika- *}
{*                          tion zum Client (SSL 2.0, SSL 3.0, TLS 1.0) und   *}
{*                          optionaler Basis-Authentifizierung im Request-    *}
{*                          Header, einstellbar per SrvCfg32.ini ([Server]    *}
{*                          SSLVersion, BasicAuthentication);                 *}
{*                          bei verschlüsselter Client-Kommunikation: Zertifi-*}
{*                          katdatei cert_pub.pem, Schlüsseldatei cert_key.pem*}
{*                          und Indy-10-Dll's libeay32.dll, ssleay32.dll im   *}
{*                          Exe-Ordner;                                       *}
{*                          mit Logdatei WicomSrv_AbrufSocketSSL.log (bei     *}
{*                          verschlüsselter Client-Kommunikation und aktivier-*}
{*                          tem ServiceIO-Debugschalter);
{*                          TServerSocket's für Abruf-Client und IP-Rufentge- *}
{*                          gennahme DSfG ersetzt durch TIdTCPServer,         *}
{*                          TClientSocket's für IP-Verbindungen zu Geräten und*}
{*                          Signaturserver ersetzt durch TIdTCPClient (Problem*}
{*                          nicht nachvollziehbarer Abstürze) -> Indy-10, Vs. *}
{*                          10.0.52;                                          *}
{*                          GPRS-ServerSocket stillgelegt (wird nicht mehr    *}
{*                          unterstützt);                                     *}
{*                          Erweiterung MRG-Login über Datenausleserschloss   *}
{*                          für Elster DL230, EK280;                          *}
{*                          Kommunikation mit Signaturserver: Spezifischerer  *}
{*                          Fehlercode SRV_ERR_TIMEOUTRECEIVE statt           *}
{*                          KOMMERR_TIMEOUT;                                  *)
{*                          XML-Ersatzzeichen in Responses für Parameter MRG/ *}
{*                          DSfG-DFÜ und Parametrierung MRG/DSfG/DSfG-DFÜ;    *}
{*                          XML-Ersatzzeichen decodieren in Request-Teil und  *}
{*                          codieren in Response-Teil                         *}
{*          14.07.2020  WW  V. 4.4.1: Bugfix Windows Ereignislog-Einträge beim*}
{*                          Beenden des Dienstes                              *}
{*          24.08.2020  WW  V. 4.4.2: MRG-Parametrierung Tritschler VC2, VC3, *}
{*                          VCC erweitert für Stell-Befehle (z.B. Meldeliste  *}
{*                          löschen) über virtuelle allgemeine Parameter-     *}
{*                          nummern 1600nnnnn                                 *}
{*          28.09.2020  WW  V. 4.5.0: Erweiterungen und Korrekturen für       *}
{*                          Gas-X 2.0 (Interface zu DCWebS-Webservice):       *}
{*                          Verbindungsaufbau-XML-Response DSfG erweitert um  *}
{*                          Erweiterungsgrad der Login-DFÜ (bei Gas-X-Version,*}
{*                          in RMG-Version bereits implementiert);            *}
{*                          Zeitsynchronisation-XML-Response erweitert um     *}
{*                          Responsedata mit Server-Zeit und Geräte-Zeit vor  *}
{*                          der Synchronisierung (bei Gas-X-Version, in RMG-  *}
{*                          Version bereits implementiert);                   *}
{*                          Parameterabruf-Kommando mit optionalem DSfG-Time- *}
{*                          out (bei Gas-X-Version, in RMG-Version bereits    *}
{*                          implementiert);                                   *}
{*                          Zeitsynchronisation (Kommandos v, Z) mit optiona- *}
{*                          lem UTC-Normalzeit-Offset des Geräts;             *}
{*                          XML-Response für DSfG-Messwerteabruf mit optiona- *}
{*                          lem COM-Tracelog (Base64-kodiert, gesteuert über  *}
{*                          neues Feld "TraceLog" im Verbindungsaufbau-       *}
{*                          Kommando);                                        *}
{*                          Erweiterung XML-Responses für MRG-Messwerteabruf  *}
{*                          und MRG-Meldungenabruf um optionale Ordnungsnum-  *}
{*                          mer: Gerätetypen Elster DL-/EK-Serie; Tritschler  *}
{*                          TDS, MCO, MC2, VC3, VCC; RMG EC 900 (nur Gas-X-   *}
{*                          Version);                                         *}
{*                          auf geänderte Einstellungen 'ServiceIO' und 'Roh- *}
{*                          daten' der Schnittstellen-Konfiguration während   *}
{*                          Laufzeit prüfen;                                  *}
{*                          Automatisches Freischalten der Programmfunktionen,*}
{*                          wenn zur Laufzeit PC-, Programm- und Programmlauf-*}
{*                          zeit-Lizenz aktiviert werden (nur Gas-X-Version); *}
{*                          Bugfixes: fehlende Sommerzeit-Rückgabe bei MRG-   *}
{*                          Zeitabruf Corus, fehlender Datencode bei MRG-Um-  *}
{*                          schaltung-Response der Gas-X-Version              *}
{*          03.11.2020  WW  V. 4.5.1: Bugfix Timer gestoppt, wenn Initialisie-*}
{*                          ren der Programmfunktionen fehlschlägt;           *}
{*                          mit Inaktivitäts-Timer 2 min für Abruf-Clientver- *}
{*                          bindung; Abruf-Clientverbindungen in Datei        *}
{*                          WicomSrv_AbrufSocketCon.log loggen (per Ini-      *}
{*                          Schalter ServiceIO aktivierbar);                  *}
{*                          Erweiteres Exception-Handling für Haupt-Thread    *}
{*          25.11.2020  WW  V. 4.5.2: mit MRG-Abruf für RMG Prilog 400, FW-   *}
{*                          Version 1.15-68DA/5ACA (Modbus-Protokoll, Modbus- *}
{*                          liste ID 15): Lesen von Messwerten (Daten-Archiv),*}
{*                          Meldungen (Status-Archiv), Parameter (aktuelle    *}
{*                          Werte), Zeitsynchronisation, Parametrierung;      *}
{*                          Bugfix Primus/Prilog-Messwerte/Meldungen lesen:   *}
{*                          Jüngster Datensatz wurde ggf. nicht gelesen (erste*}
{*                          Datensätze nach Löschen des Archivs im Gerät)     *}
{*          17.12.2020  WW  V. 4.5.3: Bugfix langsamer SSL-Verbindungsaufbau  *}
{*                          mit Client                                        *}
{*          17.03.2021  WW  V. 4.5.4: Timeout für Warten auf Signaturserver-  *}
{*                          Antwort erhöht auf 30s (für GAS-X 2.0)            *}
{*          26.03.2021  WW  V. 4.5.5: mit MRG-Abruf für RMG TME400-VCF/VMF,   *}
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
{*                          während der Abrufe durch Lesen der Ressourcedaten *}
{*                          aus Pufferlisten statt Dateien, Laden der         *}
{*                          Ressourcedatei-Daten in Pufferlisten bei Programm-*}
{*                          Initialisierung (GAS-X 2.0, Westnetz, MRG-Abrufe);*}
{*                          Reduzierung der Lizenzdatei-Zugriffe;             *}
{*                          Timeout für Warten auf Signaturserver-Antwort     *}
{*                          nochmal erhöht auf 90s (für GAS-X 2.0, OGE, DSfG- *}
{*                          Abrufe)                                           *}
{*          29.09.2021  WW  V. 4.5.8: mit MRG-Abruf für SICK FLOWSIC500,      *}
{*                          FW-Versionen 2.15.00, 2.16.00 (Modbus-Protokoll): *}
{*                          Lesen von Messwerten (Messperiodenarchiv),        *}
{*                          Meldungen (Ereignisarchiv, Parameterarchiv, metro-*}
{*                          logisches Archiv, Gasparameterarchiv), Parameter, *}
{*                          Zeitsynchronisation, Parametrierung               *}
{*          03.01.2022  WW  V. 4.5.9: Optionales COM-Tracelog erweitert für   *}
{*                          alle GAS-X-relevanten Responses                   *}
{*          11.02.2022  WW  V. 4.5.10: Erweiterungen für Prilog 400 mit Mod-  *}
{*                          buslisten-Variante (Energie Steiermark): Auslesen *}
{*                          der Modbuslisten-ID des Geräts; Ressource         *}
{*                          ParamMrg.dat mit zusätzlichem Feld 'Parameter-    *}
{*                          untergruppe'; mit neuer Ressourcedatei MBAbruf.dat*}
{*          22.06.2022  WW  V. 4.5.11: Workaround in Konvertierung von abge-  *}
{*                          fragten DSfG-Datenelementen (DE aus Umwerter-     *}
{*                          Standardabfragen, leere Werte bei ERZ2000-NG bei  *}
{*                          Abfrage 'a-z'); Anpassung für Abruf und Konvertie-*}
{*                          rung von Standardabfragen mit Archiveigenschaft   *}
{*                          (für GM-M, R-Win)                                 *}
{*          04.10.2022  WW  V. 4.5.12: MRG-TCP/IP-Abruf von Modbus-Geräten:   *}
{*                          optional mit Standardprotokoll des Geräts anstatt *}
{*                          Protokoll "Modbus TCP" (angesteuert per XML-      *}
{*                          Kommando Verbindungsaufbau, Modemtyp = M);        *}
{*                          Modbus-Slaveadresse Primus/Prilog 400 umgestellt  *}
{*                          auf 1 (bisher 248 im reservierten Bereich)        *}
{*          05.10.2022  WW  V. 4.6.0: Erweiterungen (Sopra): mit Binding auf  *}
{*                          IP-Adresse für Abruf-Serversocket einstellbar per *}
{*                          SrvCfg32.ini ([Server] IPBind); Einschränkung auf *}
{*                          Passwort 1 bei Zeitsynchronisation von DL 230,    *}
{*                          EK 280 entfernt; Gas-X-Version: mit Rückgabe der  *}
{*                          Stations-Kennung statt Kennung aus Kommando (für  *}
{*                          Gas-X 2.0 DCWebS-Webservice -> Abwärtskompatibi-  *}
{*                          libilität zu Gas-X 1.0 lt. Sopra nicht mehr erfor-*}
{*                          derlich); Veraltete Temporär- und Request/Res-    *}
{*                          ponse-Dateien des Programms löschen (wenn INI-    *}
{*                          Schalter Rohdaten bzw. ServiceIO ausgeschaltet);  *}
{*                          Bugfix ZeitSync-Info für Server-Zeit bei MRG-Zeit-*}
{*                          Korrektur (Soll-Zeit der Korrektur)               *}
{*          19.12.2022  WW  V. 4.6.1: Bugfix Timeout-Fehler bei DSfG-IP-Ruf-  *}
{*                          entgegennahme (Timeout-Überwachung per TickCount  *}
{*                          statt Timer-Event bei Kommunikation mit MRG/DSfG- *}
{*                          Station); Bugfix Endlosschleife in DSfG-Abruf bei *}
{*                          Antworttelegrammen mit NTY = U, ZAE = 0 (Regis-   *}
{*                          trierung 'Marquis MRG2203A Alternative' mit DFÜ-  *}
{*                          Instanz 'Elster enCore MC1'; GAS-X, Thyssengas)   *}
{*          31.01.2023  WW  V. 4.6.2: Verfeinerte Fehlerauswertung bei Abfrage*}
{*                          der k-Faktoren-Parameter für MRG 905/910-Messwert-*}
{*                          konvertierung (GAS-X; EON); Geänderte Lizenzdatei *}
{*                          während Programmlaufzeit automatisch  erkennen    *}
{*                          (nur Gas-X-Version)                               *}
{*          14.04.2023  WW  V. 4.6.3: für GAS-X: Verbindung halten, solange   *}
{*                          kein Client-Befehl vorliegt, erweitert für MRG und*}
{*                          DSfG-IP-Verbindungen; neue Ressourcen-Datei       *}
{*                          ParamMomVerb.dat mit Parametern zum Halten der    *}
{*                          MRG-Verbindung                                    *}
{*          24.08.2023  WW  V. 4.6.4: Optionale, erweiterte Abfrage des EK280 *}
{*                          Messwert-Archivs 13: Rückgabe der Werte in XML-E- *}
{*                          Response unter Kanalnummer 51 ff., per            *}
{*                          WicomSrv.ini konfigurierbar: [EK 280] Archiv13,   *}
{*                          für Westnetz (GAS-X);                             *}
{*                          Bugfix: Parameter-Rohdatei löschen bei Halten der *}
{*                          Verbindung zu MRG-Geräten (Elster, Datacon FWU,   *}
{*                          Tritschler, Actaris Corus, Sparklog, UNIGAS 300)  *}
{*          17.10.2023  WW  V. 4.6.5: Mit Wandlung von Hex-kodierten Ersatz-  *}
{*                          zeichen in Parameter-Rohdaten für Elster DL-/EK-  *}
{*                          Geräte                                            *}
{*          09.01.2024  WW  V. 4.6.6: mit MRG-Abruf für RMG RSM200-VCF/VMF,   *}
{*                          FW-Version 1.25 (Modbus-Protokoll, Matrix-Version *}
{*                          125): Lesen von Messwerten (Periodenarchiv),      *}  
{*                          Meldungen (Ereignisarchiv, eichamtliches und      *}
{*                          nicht-eichamtliches Parameterarchiv), Parameter,  *}
{*                          Zeitsynchronisation, Parametrierung;              *}
{*                          Konvertierung MRG-Meldungen: Bugfix Meldungsliste *}
{*                          sortieren bei Meldungen mit gleichen Zeitstempeln *}
{*                          (bei Geräten mit mehr als 1 Meldungsarchiv: EC900,*}
{*                          Actaris Corus/Sparklog, Tritschler VC3/VCC,       *}
{*                          Kamstrup UNIGAS 300, Elster EK280/DL230, SICK     *}
{*                          FLOWSIC500, TME400, RSM200), für EC900 Anpassung  *}
{*                          an DSfG-Ereignisliste mit herstellerunabhängigem  *}
{*                          Meldungsnummern-Bereich 1000-9999;                *}
{*                          MRG-Geräte mit Modbus-Kommunikation: Bugfix Binär-*}
{*                          datenwandlung 0-terminierter Strings              *}
{*          14.03.2024  WW  V. 4.6.7: Bugfixes für TCP/IP-Rufentgegennahme    *}
{*                          DSfG (Terranets): Speicherleck beim Freigeben des *}
{                           TCP/IP-Rufentgegennahme-Thread; synchronisiertes  *}
{*                          Terminieren des TCP/IP-Rufentgegennahme-Thread    *}
{*                          durch Abruf-Thread; freigeben des Abruf-Objekts   *}
{*                          nur bei nicht erfolgreichem Entgegennehmen eines  *}
{*                          Anrufs (R-Response); Anruf nicht als beendet kenn-*}
{*                          zeichnen bei nicht erfolgreicher Ausführung der   *}
{*                          Rufannahme (a-Kommando)                           *}
{*          04.04.2024  WW  V. 4.6.8: mit Gerätezustand für TME400 und RSM200 *}
{*                          (virtuelle Parameter); Umstellung lesen der Para- *}
{*                          meter-Ressourcedaten-Konfiguration 'Gerätezustand-*}
{*                          Parameter' aus Feld 'Filtertyp' statt 'ParaDaten- *}
{*                          typ' (Parammrg.dat, auch für Elster DL-/EK-Geräte *}
{*                          und MRG 905/910);                                 *}
{*                          Erweiterung RSM200: Beim Auslesen von Zähler-Para-*}
{*                          metern immer Parameter 'Zählerfaktor' mitauslesen *}
{*                         (für Rohwert-Verrechnung)                          *}
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
