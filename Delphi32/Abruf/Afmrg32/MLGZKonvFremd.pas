{******************************************************************************}
{* Unit: Langzeitdaten-Konvertierung von Fremdhersteller-Geräten              *}
{* 06.09.2002 WW  Konvertierung Boritec MemoDat 65/67                         *}
{* 10.10.2002 WW  erweitert um Konvertierung für Elster DL240 und EK260       *}
{* 12.11.2002 WW  erweitert um Konvertierung für Tritschler VC2 (IEC-Prot.)   *}
{* 19.02.2003 WW  Konvertierungen jetzt ohne interne Stammdatenzugriffe       *}
{*                (erweiterter Übergabe-Record);  Messwerte jetzt in RohSRec- *}
{*                Struktur mit Original-Kanalwerten                           *}
{* 14.07.2003 WW  erweitert um Konvertierung für KE-Anlagen (Ruhrgastyp, PPN- *}
{*                -Typ) und Datacon FWU                                       *}
{* 25.02.2004 WW  VC2-Konvertierung: zeitliche Zuordnung von Tagessummen der  *}
{*                Störzähler und Tagesmittelwerte der Analogkanäle zu den     *}
{*                Lastprofildaten überarbeitet (fehlerhafte Zuordnung bei VC2-*}
{*                Version 6.x); Fehler ImpOrg-Faktor-Zuweisung behoben; Lesen *}
{*                des Tagesendes überarbeitet; mit zusätzlicher Prüfung bei   *}
{*                über Stundenwechsel abgerufenen Lastprofildaten             *}
{* 23.08.2004 WW  Korrektur Gastagbildung bei TTG-IEC (TTG ab Vs. 7.x liefern *}
{*                bereits Gastag bei Monatsabschluß-Zählerstand)              *}
{* 16.05.2007 WW  VC2-Konvertierung optional mit aktuellen Zählerständen      *}
{* 06.02.2008 WW  Konvertierung Tritschler TTG mit FTL-Protokoll; Korrektur   *}
{*                24-Uhr-Zeitstempel bei TTG-IEC-Zählerständen; Konvertierung *}
{*                Tritschler TDS                                              *}
{* 02.09.2008 WW  Konvertierung Actaris Corus                                 *}
{* 19.11.2008 WW  Konvertierung Actaris Sparklog                              *}
{* 12.01.2009 WW  Impulswertebereich für normierte LGZ-Daten vergrößert       *}
{* 21.01.2009 WW  Bugfix TTG-FTL: Messwerte enthalten unbewertete Impulse     *}
{* 06.05.2009 WW  VC2-Konvertierung: Prüfung auf Rohdatenende über Geräte-    *}
{*                datum/-zeit aus Parameterliste                              *}
{* 01.07.2009 WW  TDS-Konvertierung, ZS-Differenzbildung für normierte LGZ-   *}
{*                Daten: mit Toleranzbereich für Zeitstempel ungleich Mess-   *}
{*                periodenende                                                *}
{* 21.09.2009 WW  Bugfix TDS-Konvertierung: Zählerstände mit Nachkommastellen *}
{* 14.10.2009 WW  Bugfix TDS-Konvertierung: Zählerstand des letzten Kanals mit*}
{*                Nachkommastellen                                            *}
{* 31.10.2011 WW  Mess_Konv_Tritschler_TDS auch für VC3                       *}
{* 31.01.2012 WW  Mess_Konv_Tritschler_TDS: Zugriff auf Rohdateien über       *}
{*                StdQuelldateiNameListe (wg. VC3-Import)                     *}
{* 30.04.2012 WW  Erweiterte Datenplausibilisierung für Elster-Konvertierungen*}
{*                Mess_Konv_DL240 und Mess_Konv_EK260                         *}
{* 27.03.2013 WW  Konvertierung Kamstrup UNIGAS 300; Doppelte 2-Uhr-Stunde    *}
{*                bei Sommer/Winterzeitwechsel für Elster EK260, DL210/22/240;*}
{*                Rundung der Tagessatz-Werte raus (EK260, DL210/220/240,     *}
{*                TTG-IEC, TDS, MCO, VC3, FWU, Corus, Sparklog, UNIGAS 300)   *}
{* 26.09.2013 WW  Konvertierung EK260 erweitert auf Kanäle für K-Zahl und     *}
{*                Z-Zahl                                                      *}
{* 09.10.2013 WW  Corus-Konvertierung optional mit aktuellen Zählerständen    *}
{* 24.01.2014 WW  Konvertierungen DL240 u. EK260: angepaßt an Abruf mit Block-*}
{*                größe > 1                                                   *}
{* 04.03.2014 WW  Bugfix Corus-Konvertierung von aktuellen Zählerständen der  *}
{*                Gastagende-Stunde                                           *}
{* 06.10.2015 WW  Beschleunigte Rohdateizugriffe mit TFileOfCharStream        *}
{* 11.01.2016 WW  Mess_Konv_Tritschler_TDS auch für VCC                       *}
{* 21.02.2017 WW  Mess_Konv_EK260: erweitert für optionale, zusätzliche Kanäle*}
{*                im EK280                                                    *}
{* 18.05.2018 WW  Mess_Konv_EK260, Mess_Konv_DL240: erweitert um Rückgabe der *}
{*                Archiv-Ordnungsnummer                                       *}
{* 28.09.2020 WW  Mess_Konv_Tritschler_TDS: erweitert um Rückgabe der Archiv- *}
{*                Ordnungsnummer                                              *}
{* 18.02.2021 WW  Ordnungsnummer-Defaultwert 'fehlend' geändert auf -1 (wegen *}
{*                TME400)                                                     *}
{* 29.09.2021 WW  Konvertierung SICK FLOWSIC500                               *}
{* 24.08.2023 WW  Konvertierung EK280 für optionales, erweitertes Messwert-   *}
{*                Archiv                                                      *}
{******************************************************************************}
Unit MLGZKonvFremd;

INTERFACE

Uses
  Forms, Classes, SysUtils, Contnrs, Math, LGZType, T_Tools, T_Zeit, MFilenam,
  WStrUtils, MP_Boritec, MP_Elster, MLGZKonv, WChars, DTUtils, MLGZKonvList,
  UnixDT, WStream, T_BinMask, MP_Actaris, MP_Tritschler, WSysCon, MP_Kamstrup,
  MResUtil, MP_SICK;


function Mess_Konv_MemoDat (MessKonvRec: TMessTagKonv): Boolean;
function Mess_Konv_DL240 (MessKonvRec: TMessTagKonv): shortint;
function Mess_Konv_EK260 (MessKonvRec: TMessTagKonv;
  var iMaxMessKanal: integer; var iMinMessKanalExt: integer;
  var iMaxMessKanalExt: integer): shortint;
function Mess_Konv_DS100 (MessKonvRec: TMessTagKonv): Boolean;
function Mess_Konv_Tritschler_IEC (MessKonvRec: TMessTagKonv): Boolean;
function Mess_Konv_Tritschler_TDS (MessKonvRec: TMessTagKonv): Boolean;
function Mess_Konv_Tritschler_FTL (MessKonvRec: TMessTagKonv): shortint;
function Mess_Konv_KE_Ruhrgas (MessKonvRec: TMessTagKonv): Boolean;
function Mess_Konv_KE_PPN (MessKonvRec: TMessTagKonv): Boolean;
function Mess_Konv_FWU (MessKonvRec: TMessTagKonv): Boolean;
function Mess_Konv_Corus (MessKonvRec: TMessTagKonv): Boolean;
function Mess_Konv_Sparklog (MessKonvRec: TMessTagKonv): Boolean;
function Mess_Konv_Unigas (MessKonvRec: TMessTagKonv): boolean;
function Mess_Konv_SICK (MessKonvRec: TMessTagKonv): Boolean;

IMPLEMENTATION

uses DateUtils;

{$IFDEF NATGAS}
const
  CnatGAS_ASCIIKopf    = '#F2';   { fester Kopf für natGAS-ASCII-File }
  CnatGAS_ASCIITrenner = ',';     { Trennzeichen im natGAS-ASCII-Format }

{--------------------------- Hilfsroutinen ------------------------------------}

{----------------------------------------------------------------}
function Get_natGAS_ASCIIZielkanal (Stationsname: string): string;
{----------------------------------------------------------------}
{ liefert Zielkanal für ASCII-Daten aus Stationsname des Stammsatzes
  -> speziell für natGAS
  -> Der Zielkanal ist im Stationsnamen des Stammsatzes enthalten zwischen dem
     bis zum 1. Unterstrich, z.B. Stationsname = '3002_NATGAS001.DAT_Speicher Potsdam:
     ASCII-Zielkanal = 3002 }
Const
  CTrenner = '_';
begin
  Result:=ExtractString (Stationsname, NUL, CTrenner, 0);
end;

{-------------------------------------------------------------------------------}
procedure Get_natGAS_LetztASCIIDatumZeit (FileName: TFileName;
                                          var Datum: daterec; var Zeit: timerec);
{-------------------------------------------------------------------------------}
{ gibt Datum und Zeit des letzten Datensatzes im übergebenen natGAS-ASCII-File zurück;
  Übergabe: ASCI-Filename
  Rückgabe: Datum, Zeit }
var
  fs_ascii: TTextFileStream;
  FSize: integer;
  S: string;
  S_Datum: string;
  S_Zeit: string;
  Code: integer;

begin
  { Vorbelegung für Rückgabe: früher als die im Gerät gespeicherten Daten-Zeitstempel }
  with Datum do begin
    year:=1900;
    month:=1;
    day:=1;
  end;
  FillChar (Zeit, SizeOf (Zeit), 0);

  if not FileExists (FileName) then exit;
  try
    { ASCII-File öffnen: }
    fs_ascii:=TTextFileStream.Create (FileName, fmOpenRead OR fmShareDenyWrite);
    try
      FSize:=fs_ascii.Size;

      while fs_ascii.Position < FSize do begin
        Application.ProcessMessages;
        fs_ascii.ReadLn (S);                             { letzten Satz lesen }
      end;
      if Pos (CnatGAS_ASCIIKopf, S) <> 1 then begin    { gelesener Satz ist nicht der Kopf }
        S_Datum:=ExtractString (S, CnatGAS_ASCIITrenner, CnatGAS_ASCIITrenner, 0);
        S_Zeit:=ExtractString (S, CnatGAS_ASCIITrenner, CnatGAS_ASCIITrenner, 1);

        S:=Copy(S_Datum, 1, 2);
        Val (S, Datum.day, Code);
        S:=Copy(S_Datum, 4, 2);
        Val (S, Datum.month, Code);
        S:=Copy(S_Datum, 7, 4);
        Val (S, Datum.year, Code);

        S:=Copy(S_Zeit, 1, 2);
        Val (S, Zeit.hour, Code);
        S:=Copy(S_Zeit, 4, 2);
        Val (S, Zeit.min, Code);
        S:=Copy(S_Zeit, 7, 2);
        Val (S, Zeit.sec, Code);
      end;  { if Pos }
    finally
      fs_ascii.Free;
    end;
  except
  end;
end;
{$ENDIF}

{--------------------------------------------------------------------------------------}
function Search_ZS_ZwList_DatumZeit (AZS_ZwList: TObjectList; var AZS_List_Pos: integer;
  ADatumZeit: TDateTime; bAbsteigend: boolean): boolean;
{--------------------------------------------------------------------------------------}
{ Ergebnis: true, wenn Eintrag in Tagessatz-Zwischenliste mit übergebenem Datum/Zeit
            gefunden wurde (Index des gefundenen Eintrags steht in AZS_List_Pos)
            Wenn Eintrag nicht gefunden wurde, enthält AZS_List_Pos den Index, an dem
            ein neuer Eintrag eingefügt werden muß. }
var
  DatumZeit_List: TDateTime;
  erg: integer;
begin
  Result:=false;
  { Eintrag suchen: }
  while AZS_List_Pos <= (AZS_ZwList.Count-1) do begin
    Application.ProcessMessages;
    DatumZeit_List:=TRohTRecExtObj (AZS_ZwList.Items [AZS_List_Pos]).RohTRec.DatumZeit;
    erg:=CmpDateTime(ADatumZeit, DatumZeit_List);
    if bAbsteigend then begin  // Listeneinträge sind zeitlich absteigend
      if erg < 0 then
        inc (AZS_List_Pos)   { weitersuchen }
      else if erg = 0 then begin
        Result:=true;       { gefunden }
        Break;
      end
      else begin
        { Listeneintrag ist jünger, weitere Suche stoppen (an AZS_List_Pos muß ein
          neuer Eintrag eingefügt werden }
        Break;
      end;
    end
    else begin  // Listeneinträge sind zeitlich aufsteigend
      if erg > 0 then
        inc (AZS_List_Pos)   { weitersuchen }
      else if erg = 0 then begin
        Result:=true;       { gefunden }
        Break;
      end
      else begin
        { Listeneintrag ist älter, weitere Suche stoppen (an AZS_List_Pos muß ein
          neuer Eintrag eingefügt werden }
        Break;
      end;
    end;
  end;  { while }
end;

{-----------------------------------------------------------------------------------------}
function Search_ZS_ZwList_RueckstellNr (AZS_ZwList: TObjectList; var AZS_List_Pos: integer;
  ARueckstellNr: integer): boolean;
{-----------------------------------------------------------------------------------------}
{ Ergebnis: true, wenn Eintrag in Tagessatz-Zwischenliste mit übergebener Rückstellnummer
            gefunden wurde (Index des gefundenen Eintrags steht in AZS_List_Pos)
            Wenn Eintrag nicht gefunden wurde: AZS_List_Pos = -1 }
var
  RueckstellNr_List: TDateTime;
  i: integer;
begin
  Result:=false;
  AZS_List_Pos:=-1;

  { Eintrag suchen: }
  i:=0;
  while i <= (AZS_ZwList.Count-1) do begin
    Application.ProcessMessages;
    RueckstellNr_List:=TRohTRecExtObj (AZS_ZwList.Items [i]).RueckstellNr;
    if RueckstellNr_List = ARueckstellNr then begin
      AZS_List_Pos:=i;
      Result:=true;  { gefunden }
      Break;
    end;
    inc (i);
  end;  { while }
end;

{--------------------------------------------------------------------------------------}
function Search_MW_ZwList_DatumZeit (AMW_ZwList: TObjectList; var AMW_List_Pos: integer;
                           ADatumZeit: TDateTime; bAbsteigend: boolean): boolean;
{--------------------------------------------------------------------------------------}
{ Ergebnis: true, wenn Eintrag in Messwert-Zwischenliste mit übergebenem Datum, Zeit
            gefunden wurde (Index des gefundenen Eintrags steht in AMW_List_Pos)
            Wenn Eintrag nicht gefunden wurde, enthält AMW_List_Pos den Index, an dem
            ein neuer Eintrag eingefügt werden muß. }
var
  DatumZeit_List: TDateTime;
  erg: integer;
begin
  Result:=false;
  { Eintrag suchen: }
  while AMW_List_Pos <= (AMW_ZwList.Count-1) do begin
    Application.ProcessMessages;
    DatumZeit_List:=TRohSRecObj (AMW_ZwList.Items [AMW_List_Pos]).RohSRec.DatumZeit;
    erg:=CmpDateTime(ADatumZeit, DatumZeit_List);
    if bAbsteigend then begin  // Listeneinträge sind zeitlich absteigend
      if erg < 0 then
        inc (AMW_List_Pos)   { weitersuchen }
      else if erg = 0 then begin
        Result:=true;       { gefunden }
        Break;
      end
      else begin
        { Listeneintrag ist jünger, weitere Suche stoppen (an AMW_List_Pos muß ein
          neuer Eintrag eingefügt werden }
        Break;
      end;
    end
    else begin  // Listeneinträge sind zeitlich aufsteigend
      if erg > 0 then
        inc (AMW_List_Pos)   { weitersuchen }
      else if erg = 0 then begin
        Result:=true;       { gefunden }
        Break;
      end
      else begin
        { Listeneintrag ist älter, weitere Suche stoppen (an AMW_List_Pos muß ein
          neuer Eintrag eingefügt werden }
        Break;
      end;
    end;
  end;  { while }
end;


{------------------------- Boritec MemoDat ------------------------------------}

{--------------------------------------------------------------}
function Mess_Konv_MemoDat (MessKonvRec: TMessTagKonv): Boolean;
{--------------------------------------------------------------}
{ Konvertierung Boritec MemoDat 65/67: nur Messwerte konvertieren, Gerät hat kein
  Zählerstandsarchiv
  -> von der Konvertierung erwarteter Aufbau der Rohdaten (Abruf mit Befehls-Telegramm l):
  alle abgerufenen Antwort-Telegramme und -Folgetelegramme aneinandergereiht }

  {--------------------------}
  function Mess_Konv: Boolean;
  {--------------------------}
  const
    C_DataHeaderLen = 16;  { Datenlänge des Lastprofil-Headers }

  type
    { Zustände für Rohdatenkonvertierung }
    TModus = (m_StartHeader, m_HeaderLength, m_Header,
              m_StartData, m_DataHeaderLength, m_Data);

  var
    fs_roh: TFileOfCharStream;  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;
    zeichen: char;
    rohsatz: string;
    std_satz: RohSRec;
    i: integer;
    j: byte;
    Modus: TModus;
    Intervall_min: integer;
    AnzWertepaare: integer;
    AnzDataChar: integer;
    S: string;
    Wert_K1: word;
    Wert_K2: word;
    DatumBuf: DateRec;
    ZeitBuf: TimeRec;
    HeaderLen: byte;
    DataLen: byte;
    KanalDaten: string;
    Betriebsart: integer;
    AnzKanaele: integer;

  begin
    Mess_Konv := False;
    { Kanalzahl aus Parameter "Betriebsart" holen: }
    if MessKonvRec.ParameterListe = nil then exit;
    if not MessKonvRec.ParameterListe.GetValueInt (CP_BOR_Betriebsart, Betriebsart) then exit;
    if (Betriebsart AND $02) <> 0 then            { Bit 1 gesetzt -> 2 Kanäle }
      AnzKanaele:=2
    else
      AnzKanaele:=1;
    if not FileExists (MessKonvRec.StdQuelldateiname) then exit;

    Mess_Konv := True;
    try
      { Stundenwert-Rohfile öffnen: }
      fs_roh:=TFileOfCharStream.Create (MessKonvRec.StdQuelldateiname, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_roh:=fs_roh.Size;

        { Stundenwert-Zielfile erzeugen: }
        fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (RohSRec));
        try
          rohsatz := '';
          Intervall_min:=0;
          AnzDataChar:=0;
          HeaderLen:=0;
          DataLen:=0;
          Modus:=m_StartHeader;   { als erstes wird das Startzeichen des 1. Telegramms gelesen }
          while (fs_roh.Position < FSize_roh) do begin
            Application.ProcessMessages;
            fs_roh.Read (zeichen);
            rohsatz := rohsatz + zeichen;

            case Modus of
              m_StartHeader:
                begin
                  if length (rohsatz) = 1 then begin
                    if rohsatz [1] = '#' then     { Startzeichen gelesen }
                      Modus:=m_HeaderLength;  { als Nächstes bis zum Längenbyte des Headers lesen }
                  end;
                  rohsatz:='';
                end;

              m_HeaderLength:
                begin
                  if length (rohsatz) = 2 then begin  { Befehlszeichen und Längebyte gelesen }
                    if rohsatz [1] <> 'l' then begin   { Befehlszeichen prüfen }
                      Mess_konv:=false;
                      Break;
                    end;
                    HeaderLen:=Ord (rohsatz [2]);
                    Modus:=m_Header;    { als Nächstes den ganzen Header lesen }
                    rohsatz:='';
                  end;
                end;

              m_Header:
                begin
                  if length (rohsatz) = (HeaderLen + 2) then begin  { Header-Daten plus Checksumme und Endezeichen gelesen }
                    { Telegramm ohne Lastprofildaten abfangen (z.B. Antwort # l STX ? R $): }
                    if HeaderLen = C_DataHeaderLen then begin
                      { Header-Rohsatz konvertieren: }
                      { Kanal-Faktoren und -Teiler (1. - 8. Byte) werden für die Konvertierung
                        in LGZ-Werte nicht benötigt, da das Gerät unbewertete Impulse (keine physikalischen
                        Werte) liefert. }
                      Intervall_min:=Ord (rohsatz [9]);
                      DatumBuf.Year:=BCDToInt (Ord (rohsatz [10])) +        { BCD Jahr (10er, 1er) }
                                     (BCDToInt (Ord (rohsatz [11])) * 100); { BCD Jahr (1000er, 100er) }
                      DatumBuf.Month:=BCDToInt (Ord (rohsatz [12]));        { BCD Monat }
                      DatumBuf.Day:=BCDToInt (Ord (rohsatz [13]));          { BCD Tag }
                      ZeitBuf.Hour:=BCDToInt (Ord (rohsatz [14]));          { BCD Minute }
                      ZeitBuf.Min:=BCDToInt (Ord (rohsatz [15]));           { BCD Sekunde }
                      ZeitBuf.Sec:=0;
                      ZeitBuf.HSec:=0;
                      AnzWertepaare:=Ord (rohsatz [16]);                    { Anzahl der Wertepaare }

                      AnzDataChar:=(AnzWertepaare * AnzKanaele * 2) + 1;  { 2 Byte je Wert plus Checksummen-Byte }
                      KanalDaten:='';
                      Modus:=m_StartData;         { als Nächstes: Startzeichen von Daten-Folgetelegramm lesen }
                    end;
                    rohsatz:='';
                  end;
                end;

              m_StartData: begin
                  if length (rohsatz) = 1 then begin
                    if rohsatz [1] = '#' then     { Startzeichen gelesen }
                      Modus:=m_DataHeaderLength;   { als Nächstes bis zum Längenbyte des Daten-Folgetelegramms lesen }
                  end;
                  rohsatz:='';
              end;

              m_DataHeaderLength:
                begin
                  if length (rohsatz) = 2 then begin  { Befehlszeichen und Längebyte gelesen }
                    if rohsatz [1] <> 'l' then begin   { Befehlszeichen prüfen }
                      Mess_konv:=false;
                      Break;
                    end;
                    DataLen:=Ord (rohsatz [2]);
                    Modus:=m_Data;       { als Nächstes Kanaldaten lesen }
                    rohsatz:='';
                  end;
                end;

              m_Data:
                begin
                  if length (rohsatz) = (DataLen + 2) then begin  { Kanal-Werte plus Checksumme und Endezeichen gelesen }
                    KanalDaten:=KanalDaten + Copy (rohsatz, 1, length (rohsatz)-2);  { nur Kanalwerte anhängen }
                    rohsatz:='';
                    if length (KanalDaten) >= AnzDataChar then begin
                      if length (KanalDaten) = AnzDataChar then begin   { alle Wertepaare gelesen, jetzt Konvertieren }
                        i:=1;
                        while i < length (KanalDaten) do begin     { Werte ohne das Checksummen-Byte über Datensatz }
                          S:=KanalDaten [i] + KanalDaten [i+1];
                          Wert_K1:=Bin2Word (S);

                          FillChar (std_satz, SizeOf (std_satz), 0);       { Vorbelegung std_satz; 18.05.2018, WW }
                          with std_satz do begin
                            if RecToDateTime (DatumBuf, ZeitBuf, DatumZeit) then begin
                              { Satzstatus }
                              satzstatus:= $00;
                              if ((Wert_K1 and $8000) <> 0) then     { Uhr gestellt-Status: Bit 15 im Wert von Kanal 1 abfragen }
                                SatzStatus := SatzStatus or $02;

                              { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                              for j:=1 to c_maxKanalZahl do begin
                                kanal[j].wert:=0;
                                KanalOrig[j].wert:=0;
                                kanal[j].kanalstatus:=$80;   { nur Impulskanäle }
                                KanalOrig[j].KanalStatus:=Kanal[j].KanalStatus;
                                KanalOrig[j].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                              end;

                              { Kanalwerte: unbewertete Impulse (keine Rückrechnung erforderlich): }
                              Wert_K1:=Wert_K1 AND not $8000;  { Uhr gestellt-Status-Bit im Wert wegmaskieren }
                              Kanal [1].Wert:=Wert_K1;
                              Kanal [1].KanalStatus:=Kanal [1].KanalStatus and $7F;  { Fehlend-Bit löschen }
                              KanalOrig [1].Wert:=Wert_K1;
                              KanalOrig [1].KanalStatus:=KanalOrig [1].KanalStatus and $7F;  { Fehlend-Bit löschen }

                              if AnzKanaele = 2 then begin
                                if (i+2) < length (KanalDaten) then begin
                                  S:=KanalDaten [i+2] + KanalDaten [i+3];
                                  Wert_K2:=Bin2Word (S);
                                  Wert_K2:=Wert_K2 AND not $8000;  { Uhr gestellt-Status-Bit im Wert wegmaskieren }
                                  Kanal [2].Wert:=Wert_K2;
                                  Kanal [2].KanalStatus:=Kanal [2].KanalStatus and $7F;  { Fehlend-Bit löschen }
                                  KanalOrig [2].Wert:=Wert_K2;
                                  KanalOrig [2].KanalStatus:=KanalOrig [2].KanalStatus and $7F;  { Fehlend-Bit löschen }
                                end;
                                inc (i, 4);           { 2 Kanal-Werte weiterpositionieren }
                              end else
                                inc (i, 2);           { 1 Kanal-Wert weiterpositionieren }
                              fs_RohSRec.WriteRec (std_satz);
                            end;  { if RecToDateTime }
                          end;   { of with }

                          P_AddMinuten (DatumBuf, ZeitBuf, Intervall_min);  { Datum, Zeit für nächsten Wert setzen }
                        end;  { while i < length (rohsatz) }

                        Modus:=m_StartHeader; { als Nächstes: wieder Header-Startzeichen lesen }
                      end
                      else begin       { Fehler Rohdatenstruktur }
                        Mess_Konv:=false;
                        Break;
                      end;
                    end else
                      Modus:=m_StartData;     { weiteres Daten-Folgetelegramm lesen }
                  end;
                end;
            end;  { case }
          end;   { while not eof (d_roh) }
        finally
          fs_RohSRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if length (rohsatz) > 0 then        { Fehler Rohdatenlänge }
        Mess_Konv := False;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.StdQuelldateiname);  { Rohfile löschen }
    except
      Mess_Konv := False;
      exit;
    end;
  end;   { mess_konv }

begin  { main }
  mess_konv;
  Mess_Konv_MemoDat:=true;
end;


{-------------------------- Elster DL240 --------------------------------------}

{-------------------------------------------------------------}
function Mess_Konv_DL240 (MessKonvRec: TMessTagKonv): shortint;
{-------------------------------------------------------------}
{ Konvertierung Elster DL240 (ab Version 1.00): Es werden die vom Gerät gelieferten
  stündlichen Zählerstände (Archive 2, 4, 6, 8 enthalten MP-Werte der Kanäle 1, 2, 3, 4)
  in normierte LGZ-Stundenwerte konvertiert.
  -> Die Anzahl der Datensätze pro Teilblock STX..EOT im Rohfile darf beim Abruf
     innerhalb des vorgegebenen Wertebereichs beliebig gewählt werden.
  -> MessKonvRec.StdQuellDateinameListe enthält Einträge mit Format: <Kanalnummer>;<Rohdateiname>
     Fehlender Kanal: Listeneintrag fehlt oder Eintrag mit <Rohdateiname> = Leer-String
  -> mit spezieller natGAS-ASCII-Konvertierung der vom Gerät gelieferten stündlichen
     Zählerstände (nur mit IFDEF NATGAS und Automatik-Abruf)
  -> auch für DL210 (1 Kanal) und DL220 (2 Kanäle)
  Ergebnis:  0 = OK
            -1 = Rohfile konnte nicht geöffnet werden
            -2 = Fehler in Rohfile-Struktur bzw. Roh-Datensatz nicht plausibel }
const
  CMaxDL240_Kanaele = 4;
  CMaxDL240_Zaehler = 8;

var
  rohfile_k: array [1..CMaxDL240_Kanaele] of TFileOfCharStream;  // 06.10.2015, WW
  FSize_roh_k: array [1..CMaxDL240_Kanaele] of integer;
  do_konv_k: array [1..CMaxDL240_Kanaele] of boolean;

  {----------------------------------------------------------------------}
  function read_rohsatz_dl240 (KanalNr: integer;
                               var datum: daterec; var zeit: timerec;
                               var zaehler1: string; var zaehler2: string;
                               var iOrdNr: longint;
                               var fehler: integer): boolean;
  {----------------------------------------------------------------------}
  { Hilfsroutine: Rohsatz aus geöffnetem DL-240-Rohfile lesen
    Übergabe: Kanal-Nummer
    Rückgabe (Rohsatz-Werte): Datum, Zeit
                              Wert-String Zähler 1
                              Wert-String Zähler 2
                              Archiv-Odnungsnummer
     Übergabe/Rückgabe: Fehler-Status für aufrufende Routine 'konv_dl240'
     Ergebnis:  true  = OK
                false = Rohsatz wurde nicht gelesen oder Rohsatz enthält Fehlertelegramm
                        -> es werden keine Daten zurückgegeben }
  var
    satz_ok: boolean;
    rohsatz: string;
    zeichen: char;
    dummy: char;
    i: integer;
    s: string;
    wert_ok: boolean;
    len: integer;
    szTemp: string [10];
    Code : Integer;
    iPos: integer;
    iBuf: longint;

  begin
    { Vorbelegungen: }
    FillChar (datum, SizeOf (datum), 0);
    FillChar (zeit, SizeOf (zeit), 0);
    zaehler1:='';
    zaehler2:='';
    iOrdNr:=-1;  { Default-Ordnungsnummer; 18.05.2018, WW }

    { wenn Rohfile nicht geöffnet ist oder bei eof, raus }
    if not do_konv_k [KanalNr] then begin
      Result:=false;
      exit;
    end;

    satz_ok:=false;
    { bis zum ersten korrekten Satz lesen }
    while not satz_ok AND (rohfile_k [KanalNr].Position < FSize_roh_k [KanalNr]) do begin
      rohsatz:='';
      zeichen:=NUL;

      { Datensätze bilden: bis EOT, ETX oder LF lesen }
      while (zeichen <> ETX) AND (zeichen <> EOT) AND (zeichen <> LF) AND
            (rohfile_k [KanalNr].Position < FSize_roh_k [KanalNr]) do begin
        rohfile_k [KanalNr].Read (zeichen);
        if (zeichen <> CR) AND (zeichen <> LF) AND
           (zeichen <> EOT) AND (zeichen <> ETX) then
          rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne CR, LF, EOT, ETX) }

        { das auf ETX und EOT folgende BCC überlesen }
        if ((zeichen = ETX) OR (zeichen = EOT)) AND
           (rohfile_k [KanalNr].Position < FSize_roh_k [KanalNr]) then begin
          rohfile_k [KanalNr].Read (dummy);
        end;
      end;

      Application.ProcessMessages;
      { vom Rohsatz interessiert nur der Teil ab STX:
        -> geändert wegen EK260 mit NUL-Zeichen bei Datenempfang mit Break; 11.04.2003, WW
        -> geändert für Abruf mit Blockgröße > 1; 03.12.2013, WW }
      iPos:=Pos (STX, rohsatz);
      if iPos > 0 then
        rohsatz:=Copy (rohsatz, iPos+1, length (rohsatz));

      { rohsatz in Datensatz-Werte '(..)' aufsplitten
         -> Index-Nummerierung i der einzelnen Werte gilt für alle DL240-Versionen }
      i:=0;
      iOrdNr:=-1;  { Default-Ordnungsnummer; 18.05.2018, WW }
      while length (rohsatz) > 0 do begin
        inc (i);
        s:=F_Zerlegen (rohsatz, ')');  { lesen bis ')' }

        { Datensatz-Wert auf richtige Struktur '(..)' prüfen }
        wert_ok:=false;  { Vorbelegung: Datensatz-Wert ist falsch }
        len:=length (s);
        if len > 0 then begin
          if s[1] = '(' then begin     { erstes Zeichen muß '(' sein }
            wert_ok:=true;           { Datensatz-Wert ist ok }
            s:=Copy (s, 2, length (s));         { '(' wegschneiden }

            { auf Fehlertelegramm prüfen (gekennzeichnet durch #): }
            if length (s) > 0 then begin
              if s[1] = '#' then begin
                satz_ok:=false;
                break;
              end;
            end;
          end;
        end;
        if not wert_ok then
          fehler:=-2;  { Rückgabe: Fehler in Rohfile-Struktur }

        { i = 2: Archiv-Ordnungsnummer; 18.05.2018, WW }
        if i = 2 then begin
          if (length (s) > 0) AND wert_ok then begin
            Val (s, iBuf, Code);
            if Code = 0 then
              iOrdNr:=iBuf;
          end;
        end

        { i = 3: Zeitstempel (kompletten Zeitstempel einschl. Minuten, Sekunden lesen
                              für Zeitvergleich in der aufrufenden Routine) }
        else if i = 3 then begin
          satz_ok:=true;   { Neue Vorbelegung: Datensatz ok, wenn Zeitstempel vorhanden; 30.04.2012, WW }
          if not wert_ok OR (length (s) <> 19) then begin  { bei Strukturfehler: ganzen Datensatz verwerfen }
            satz_ok:=false;
            fehler:=-2;  { Rückgabe: Roh-Datensatz nicht plausibel }
            break;
          end;

          with datum do begin
            szTemp:=Copy (s, 1, 4);        { Jahr }
            Val (szTemp, year, Code);
            if Code <> 0 then
              wert_ok:=false;
            szTemp:=Copy (s, 6, 2);        { Monat }
            Val (szTemp, month, Code);
            if Code <> 0 then
              wert_ok:=false;
            szTemp:=Copy (s, 9, 2);        { Tag }
            Val (szTemp, day, Code);
            if Code <> 0 then
              wert_ok:=false;
          end;
          with Zeit do begin
            szTemp:=Copy (s, 12, 2);       { Stunde }
            Val (szTemp, hour, Code);
            if Code <> 0 then
              wert_ok:=false;
            szTemp:=Copy (s, 15, 2);       { Minute }
            Val (szTemp, min, Code);
            if Code <> 0 then
              wert_ok:=false;
            szTemp:=Copy (s, 18, 2);       { Sekunde }
            Val (szTemp, sec, Code);
            if Code <> 0 then
              wert_ok:=false;
            hsec:=0;
          end;

          try                        { Überprüfung auf plausibles Datum, Zeit }
            EncodeDate (Datum.year, Datum.month, Datum.day);
            EncodeTime (Zeit.hour, Zeit.min, Zeit.sec, 0);
          except
            wert_ok:=false;
          end;

          if not wert_ok then begin
            satz_ok:=false;
            fehler:=-2;
            break;
          end;
        end

        { i = 4: Hauptzähler (Standard, anderer Zähler parametrierbar)
                  -> Kanäle 1,3,5,7 im LGZ-File sind die Hauptzähler der Gerätekanäle 1-4 }
        else if i = 4 then begin
          if (length (s) > 0) AND wert_ok then
            zaehler1:=s;           { Wert Zähler 1 }
        end

        { i = 5: setzbarer Zähler (Standard, anderer Zähler parametrierbar)
                  -> Kanäle 2,4,6,8 im LGZ-File sind die setzbaren Zähler der Gerätekanäle 1-4 }
        else if i = 5 then begin
          if (length (s) > 0) AND wert_ok then
            zaehler2:=s;           { Wert Zähler 2 }
        end;
      end;  { while length (rohsatz) }

    end;  { while not satz_ok AND .. }

    Result:=satz_ok;
  end; { read_rohsatz_dl240 }

type
  { Struktur zum Merken eines Kanalwerts für Stundenwert-Differenzbildung }
  Tletzt_daten = record
    datum: daterec;
    zeit: timerec;
    wert: double;
  end;

var
  erg: integer;
  i, k: integer;
  raw_fname_k: array [1..CMaxDL240_Kanaele] of TFileName;
  weiter: boolean;
  fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
  fs_RohTRec: TFileOfRecStream;       { Tagessätze-Datei in RohTRec-Struktur }
  std_satz: RohSRec;
  tag_satz: RohTRec;
  do_read_k: array [1..CMaxDL240_Kanaele] of boolean;
  do_write_k: array [1..CMaxDL240_Kanaele] of boolean;
  k_ok: array [1..CMaxDL240_Kanaele] of boolean;
  datum_k: array [1..CMaxDL240_Kanaele] of daterec;
  zeit_k: array [1..CMaxDL240_Kanaele] of timerec;
  aeltester_datum: daterec;
  aeltester_zeit: timerec;
  zaehler1_k: array [1..CMaxDL240_Kanaele] of string;
  zaehler2_k: array [1..CMaxDL240_Kanaele] of string;
  ordnr_k: array [1..CMaxDL240_Kanaele] of longint;
  KanalNrStr: string;
  S: string;
  Code: integer;
  wert_z: array [1..2] of double;   { physikalische Werte für Zähler 1 und 2 }
  l: Int64;
  letzt_daten_k: array [1..CMaxDL240_Zaehler] of Tletzt_daten;
  datebuf: daterec;
  timebuf: timerec;
  Faktor: double;
  Diff: double;
  k_z1, k_z2: integer;
  z: integer;
  letzt_zwsatz_datum: daterec;
  letzt_zwsatz_zeit: timerec;
  datum: DateRec;
  zeit: TimeRec;
  tagsatz_ok: boolean;
  dtOK: boolean;
{$IFDEF NATGAS}
  fs_ascii: TTextFileStream;
  ASCIIFileName: TFileName;
  ASCIIZielkanal: string;
  s_ascii: string;
  Letzt_ASCII_Datum: daterec;
  Letzt_ASCII_Zeit: timerec;
  do_write_ascii: boolean;
{$ENDIF}

begin
  erg:=0;
  try
    { Rohdateinamen der einzelnen Kanäle: }
    for k:=Low (raw_fname_k) to High (raw_fname_k) do
      raw_fname_k [k]:='';         { Vorbelegung: nicht vorhanden }
    for i:=0 to MessKonvRec.StdQuellDateinameListe.Count - 1 do begin
      S:=MessKonvRec.StdQuellDateinameListe [i];
      KanalNrStr:=F_Zerlegen (S, ';');        { Kanalnummer bis zum Strichpunkt }
      k:=StrToInt (KanalNrStr);  { Kanalnummer }
      if (k >= Low (raw_fname_k)) AND (k <= High (raw_fname_k)) then
        raw_fname_k [k]:=S;                   { Rohdateiname ab dem Strichpunkt }
    end;

    { Rohfiles öffnen
       -> do_konv_k [n] = false bedeutet Rohfile von Kanal n ist nicht geöffnet,
          Kanal n wird nicht konvertiert }
    for i:=1 to CMaxDL240_Kanaele do begin
      do_konv_k [i]:=false;
      if length (raw_fname_k [i]) > 0 then begin
        if  FileExists (raw_fname_k [i]) then begin     { Rohfile öffnen }
          rohfile_k [i]:=TFileOfCharStream.Create (raw_fname_k [i], fmOpenRead OR fmShareDenyWrite);
          FSize_roh_k [i]:=rohfile_k [i].Size;
          do_konv_k [i]:=true;
        end;
      end;
    end;  { for }

    try
      { wenn keines der 4 Rohfiles geöffnet wurde: raus mit Fehler-Ergebnis }
      weiter:=false;
      for i:=1 to CMaxDL240_Kanaele do begin
        if do_konv_k [i] then
          weiter:=true;
      end;
      if not weiter then begin
        Result:=-1;
        exit;
      end;

      { Stundenwert-Zielfile erzeugen: }
      fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (RohSRec));
      try
        { Tagessatz-Zielfile erzeugen: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (RohTRec));
        try
          {$IFDEF NATGAS}
            { ASCII-Zielkanal aus Stationsname: }
            ASCIIZielkanal:=Get_natGAS_ASCIIZielkanal (MessKonvRec.Sta_Stationsname);
            { ASCII-Filename aus Stationsname: }
            ASCIIFileName:=Get_natGAS_ASCIIFilename (MessKonvRec.Sta_Stationsname);

            do_write_ascii:=false;
            if MessKonvRec.Select = 1 then begin              { nur bei Automatik-Abruf }
              { Datum und Zeit des letzten ASCII-Satzes ermitteln: }
              Get_natGAS_LetztASCIIDatumZeit (ASCIIFileName, Letzt_ASCII_Datum, Letzt_ASCII_Zeit);

              if FileExists (ASCIIFileName) then
                fs_ascii:=TTextFileStream.Create (ASCIIFileName, fmOpenReadWrite OR fmShareDenyWrite)  { ASCII-Datei: anhängen }
              else begin
                fs_ascii:=TTextFileStream.Create (ASCIIFileName, fmCreate);      { neuanlegen, falls nicht vorhanden }
                fs_ascii.WriteLn (CnatGAS_ASCIIKopf);  { nur beim Neuanlegen Kopf schreiben }
              end;
            end else
              fs_ascii:=nil;
            try
          {$ENDIF}

              { Vorbelegung von Datum/Zeit und Wert der Vorstunden-Kanalwerte für
                Zählerstands-Differenzbildung: Vorstundenwert unbekannt }
              for i:=Low (letzt_daten_k) to High (letzt_daten_k) do begin
                FillChar (letzt_daten_k [i].datum, SizeOf (letzt_daten_k [i].datum), 0);
                FillChar (letzt_daten_k [i].zeit, SizeOf (letzt_daten_k [i].zeit), 0);
                letzt_daten_k [i].wert:=-1.0;
              end;

              { Vorbelegung für Datum/Zeit des zuletzt geschriebenen std_satz: }
              FillChar (letzt_zwsatz_datum, SizeOf (letzt_zwsatz_datum), 0);
              FillChar (letzt_zwsatz_zeit, SizeOf (letzt_zwsatz_zeit), 0);

              { Konvertierung (Zeitstempel in den Datensätzen der Rohfiles laufen
                 nicht zwangsläufig für alle Kanäle synchron !) }
              weiter:=true;
              for i:=1 to CMaxDL240_Kanaele do
                do_read_k [i]:=true;

              while weiter do begin
                Application.ProcessMessages;
                { wenn Lese-Flag gesetzt: aus jedem Rohfile einen Datensatz lesen und
                   jeweils Datum/Zeit, Werte für Zähler 1, 2 eines jeden Kanals ermitteln }
                for i:=1 to CMaxDL240_Kanaele do begin
                  if do_read_k [i] then
                    k_ok [i]:=read_rohsatz_dl240 (i, datum_k [i], zeit_k [i],
                                                  zaehler1_k [i], zaehler2_k [i],
                                                  ordnr_k [i], erg);
                end;

                if k_ok [1] OR k_ok [2] OR k_ok [3] OR k_ok [4] then begin
            {$IFDEF NATGAS}
                  s_ascii:=ASCIIZielkanal;          { ASCII-Datensatz beginnt mit Zielkanal }
            {$ENDIF}

                  { ältesten Zeitstempel aller Kanal-Datensätze ermitteln, dazu fehlende Datum/Zeit-Werte
                    mit Jahr 9999 vorbelegen: }
                  for i:=1 to CMaxDL240_Kanaele do begin
                    if not k_ok [i] then
                      datum_k [i].year:=9999;
                  end;

                  aeltester_datum:=datum_k [1];
                  aeltester_zeit:=zeit_k [1];
                  for i:=2 to CMaxDL240_Kanaele do begin
                    if (CmpDate (datum_k [i], aeltester_datum) + CmpTime (zeit_k [i], aeltester_zeit)) < 0 then begin
                      aeltester_datum:=datum_k [i];
                      aeltester_zeit:=zeit_k [i];
                    end;
                  end;

                  { alle Kanal-Datensätze ermitteln, welche den ältesten Zeitstempel tragen }
                  for i:=1 to CMaxDL240_Kanaele do
                    do_write_k [i]:=false;
                  for i:=1 to CMaxDL240_Kanaele do begin
                    if (CmpDate (datum_k [i], aeltester_datum) + CmpTime (zeit_k [i], aeltester_zeit)) = 0 then
                      do_write_k[i]:=true;
                  end;

                  FillChar (tag_satz, SizeOf (tag_satz), 0);       { Vorbelegung tag_satz }
                  with tag_satz do begin
                    satzstatus:= $00;  { Satzstatus fest: OK }

                    { Vorbelegung Zählerstati für fehlend }
                    for i:=1 to c_maxKanalZahl do begin
                      E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                      E_zaehler[i].wert:=0;
                      K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                      K_zaehler[i].wert:=0;
                    end;
                  end;  { with tag_satz }

                  FillChar (std_satz, SizeOf (std_satz), 0);       { Vorbelegung std_satz }
                  with std_satz do begin
                    satzstatus:= $00;  { Satzstatus fest: OK }

                    { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                    for i:=1 to c_maxKanalZahl do begin
                      kanal[i].wert:=0;
                      KanalOrig[i].wert:=0;
                      kanal[i].kanalstatus:=$80;                           { Impulskanal }
                      KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                      KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                    end;

                    Datum:=aeltester_datum;
                    Zeit:=aeltester_zeit;
                    { Datum/Stunden in tag_satz belegen: }
                    if RecToDateTime (Datum, Zeit, tag_satz.DatumZeit) then begin
                      tagsatz_ok:=true;
                      if Zeit.hour <= MessKonvRec.TagesEnde then
                        tag_satz.DatumZeit:=tag_satz.DatumZeit - 1; { Zählerstand für Gastag }
                    end
                    else begin
                      tagsatz_ok:=false;
                      erg:=-2;
                    end;

            {$IFDEF NATGAS}
                    { ASCII-Datenstring: }
                    s_ascii:=s_ascii + CnatGAS_ASCIITrenner +
                             FDateStr (Datum) + CnatGAS_ASCIITrenner + FTimeStr (Zeit);
            {$ENDIF}

                  { Kanal-Datensätze mit ältestem Zeitstempel abspeichern:
                    Zähler1, K1 -> Kanal 1 im Stundensatz- und Tagessatz-Zwischenfile
                    Zähler2, K1 -> Kanal 2      "
                    Zähler1, K2 -> Kanal 3      "
                    Zähler2, K2 -> Kanal 4      "
                    Zähler1, K3 -> Kanal 5      "
                    Zähler2, K3 -> Kanal 6      "
                    Zähler1, K4 -> Kanal 7      "
                    Zähler2, K4 -> Kanal 8      "

                    bei Standardparametrierung des Geräts gilt:
                    Zähler 1 = Hauptzähler
                    Zähler 2 = setzbarer Zähler }

                    for i:=1 to CMaxDL240_Kanaele do begin
                      if do_write_k [i] then begin       { Zähler 1 und 2 für Kanal i schreiben }
                        Val (zaehler1_k [i], wert_z [1], Code);  { Kanalwert Zähler 1 aus Gerät (physikalisch !) }
                        Val (zaehler2_k [i], wert_z [2], Code);  { Kanalwert Zähler 1 aus Gerät (physikalisch !) }

                        k_z1:=(2*i) - 1;            { LGZ-Tagessatz-Kanal für Zähler 1 }
                        k_z2:=k_z1 + 1;             { LGZ-Tagessatz-Kanal für Zähler 2 }
                        for k:=k_z1 to k_z2 do begin       { Schleife über LGZ-Kanäle für Zähler 1 und 2 }
                          if k = k_z1 then
                            z:=1                    { Zähler 1 }
                          else
                            z:=2;                   { Zähler 2 }

                          { Stundensatz-Record: }
                          KanalOrig [k].wert:=wert_z [z];              { Original-Rohwert }
                          KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                          KanalOrig [k].OrdNr := ordnr_k [i]; { Ordnungsnummer zuweisen; 18.05.2018, WW }

                          { für LGZ-Daten: vom Gerät gelieferte physikalische Zählerstände
                            rückrechnen auf normierte Zählerstandsdifferenzen }
                          if (Zeit.min = 0) AND (Zeit.sec = 0) then begin  { nur volle Stunden interessieren ! }
                            if letzt_daten_k [k].wert > -1 then begin     { Differenzbildung nur möglich, wenn Vor-Zählerstand bekannt }
                              datebuf:=letzt_daten_k [k].datum;
                              timebuf:=letzt_daten_k [k].zeit;
                              P_AddMinuten (datebuf, timebuf, 60);
                              { Vor-Zählerstand muß von Vorstunde sein: }
                              if (CmpDate (datebuf, Datum) + CmpTime (timebuf, Zeit)) = 0 then begin
                                { physikalischen Impulswert rückrechnen auf LGZ-Rohwert über
                                  Geräteparameter "cp-Wert Eingang i": }
                                if MessKonvRec.ParameterListe <> nil then begin
                                  if MessKonvRec.ParameterListe.GetValue (CP_ELS_cpWert [i], S) then begin
                                    S:=F_Zerlegen (S, '*');       { die Einheit abschneiden }
                                    Val (S, Faktor, Code);
                                    if Faktor > 0 then begin
                                      Diff:=wert_z [z] - letzt_daten_k [k].wert;   { Zählerstands-Differenz }
                                      l:=Round (Diff / Faktor);
                                      if l >= 0 then begin
                                        if l > High (Kanal [k].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                                          Kanal [k].Wert:=High (Kanal [k].Wert);
                                          Kanal [k].KanalStatus := Kanal [k].KanalStatus or $01;  { Überlauf-Bit setzen }
                                        end else
                                          Kanal [k].Wert:=l;
                                        Kanal [k].KanalStatus := Kanal [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                                      end;
                                    end;
                                  end;
                                end;
                              end;
                            end;
                            { wert_k mit Datum/Zeit merken: }
                            letzt_daten_k [k].datum:=Datum;
                            letzt_daten_k [k].zeit:=Zeit;
                            letzt_daten_k [k].wert:=wert_z [z];
                          end;

                          { in Tagessatz-Record physikalische Zählerstände von
                            Zähler 1 und 2 eintragen: }
                          if (k >= 1) AND (k <= c_maxKanalZahl) then begin
                            tag_satz.E_zaehler[k].wert:=wert_z [z];  // Rundung raus; 27.03.2013, WW
                            tag_satz.E_zaehler[k].zaehlerstatus:=
                              tag_satz.E_zaehler[k].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                          end;
                        end;
                      end;  { if do_write_k [i] }

            {$IFDEF NATGAS}
                      { ASCII-Datenstring: }
                      if do_write_k [i] then begin
                        s_ascii:=s_ascii + CnatGAS_ASCIITrenner;
                        s_ascii:=s_ascii + zaehler1_k [i];      { Wert-String für Zähler 1 aus Gerät }
                        s_ascii:=s_ascii + CnatGAS_ASCIITrenner;
                        s_ascii:=s_ascii + zaehler2_k [i];      { Wert-String für Zähler 2 aus Gerät }
                      end else
                        { bei fehlendem Wert oder Strukturfehler: leer, nur Trenner für beide Zähler schreiben }
                        s_ascii:=s_ascii + CnatGAS_ASCIITrenner + CnatGAS_ASCIITrenner;
            {$ENDIF}
                    end;  { for }
                  end;  { with std_satz }

                  dtOK:=RecToDateTime (datum, zeit, std_satz.DatumZeit);
                  { Datensätze in Stundensatz- und Tagessatz-Zwischendatei schreiben:
                    -> Falls mehrere Sätze mit gleichem Zeitstempel hintereinander folgen, wird nur
                       der erste geschrieben. Im ersten Satz stehen (durch Tests ermittelt) ALLE
                       Kanalwerte.
                    -> Mit doppelter 2-Uhr-Stunde bei SZ/WZ-Wechsel; 23.04.2013, WW }
                  if ((CmpDate (letzt_zwsatz_datum, datum) +
                       CmpTime (letzt_zwsatz_zeit, zeit)) <> 0) OR
                     (letzt_zwsatz_datum.day = 0) OR
                     (Assigned (MessKonvRec.WZ_SZKonfigList) AND
                      MessKonvRec.WZ_SZKonfigList.FindUmstellungstermin (
                        IncHour (std_satz.DatumZeit), CResSZtoWZ)) then begin  // 06.08.2021, WW
                    letzt_zwsatz_datum:=datum;
                    letzt_zwsatz_zeit:=zeit;

                    if dtOK then
                      fs_RohSRec.WriteRec (std_satz)    { Stundensatz in Zwischendatei schreiben }
                    else
                      erg:=-2;

                    { Tagessatz nur in Zwischendatei schreiben, wenn vom Tagesende: }
                    if (Zeit.hour = MessKonvRec.Tagesende) AND
                       (Zeit.min = 0) AND (Zeit.sec = 0) AND tagsatz_ok then
                      fs_RohTRec.WriteRec (tag_satz);
                  end;

            {$IFDEF NATGAS}
                  { ASCII-Daten: }
                  if Assigned (fs_ascii) then begin
                    if DateTimeToRec (std_satz.DatumZeit, datebuf, timebuf) then begin
                      if (CmpDate (datebuf, Letzt_ASCII_Datum) +
                          CmpTime (timebuf, Letzt_ASCII_Zeit)) > 0 then
                        do_write_ascii:=true;        { ab einschließlich diesem Satz ASCII-Daten schreiben }
                    end;
                    if do_write_ascii then
                      fs_ascii.WriteLn (s_ascii);                 { ASCII-Datei schreiben }
                  end;
            {$ENDIF}

                  { für jeden Kanal: wenn Wert abgespeichert wurde, Lese-Flag setzen, um
                    nächsten Datensatz aus Rohfile zu lesen }
                  for i:=1 to CMaxDL240_Kanaele do
                    do_read_k [i]:=do_write_k [i] OR not k_ok [i];
                end;  { if k_ok [1] OR ... }

                { weiter, solange nicht alle Rohfiles vollständig durchgelesen wurden }
                weiter:=false;
                for i:=1 to CMaxDL240_Kanaele do begin
                  if do_konv_k [i] then begin
                    if rohfile_k [i].Position < FSize_roh_k [i] then
                      weiter:=true;
                  end;
                end;
              end;   { while weiter }

          {$IFDEF NATGAS}
            finally
              fs_ascii.Free;   { ASCII-File schließen }
            end;
          {$ENDIF}
        finally
          fs_RohTRec.Free;       { Zwischenfile für Tagessätze schließen }
        end;
      finally
        fs_RohSRec.Free;      { Zwischenfile für Stundensätze schließen }
      end;

      Result:=erg;
    finally
      { geöffnete Rohfiles wieder schließen }
      for i:=1 to CMaxDL240_Kanaele do begin
        if do_konv_k [i] then
          rohfile_k [i].Free;
      end;
    end;

    if MessKonvRec.loeschen then begin
      for i:=1 to CMaxDL240_Kanaele do begin  { Rohfiles löschen }
        if length (raw_fname_k [i]) > 0 then
          DeleteFile (raw_fname_k [i]);
      end;
    end;
  except
    Result:=-1;
  end;
end;  { Mess_Konv_DL240 }


{-------------------------- Elster EK260 --------------------------------------}

{------------------------------------------------------------------------------}
function Mess_Konv_EK260 (MessKonvRec: TMessTagKonv;
  var iMaxMessKanal: integer; var iMinMessKanalExt: integer;
  var iMaxMessKanalExt: integer): shortint;
{------------------------------------------------------------------------------}
{ Konvertierung Elster EK260 (ab Version 2.00): Es werden die vom Gerät gelieferten
  stündlichen Zählerstände (Archiv 3, enthält MP-Werte aller Kanäle) in
  normierte LGZ-Stundenwerte konvertiert.
  -> Auch für EK280 (auch mit optionalen, zusätzlich konfigurierten Kanälen)
  -> Die Anzahl der Datensätze pro Teilblock STX..EOT im Rohfile darf beim Abruf
     innerhalb des vorgegebenen Wertebereichs beliebig gewählt werden.
  -> mit spezieller natGAS-ASCII-Konvertierung der vom Gerät gelieferten stündlichen
     Zählerstände bzw. Analogwerte (nur mit IFDEF NATGAS und Automatik-Abruf)
  -> mit Konvertierung eines optionalen, erweiterten Messwert-Archivs des Geräts; 24.08.2023, WW
  Rückgaben: Anzahl der aus den Rohdaten konvertierten Messwert-Kanäle
             Niedrigste und höchste Kanalnummer des optional konvertierten,
               erweiterten Messwert-Archivs
  Ergebnis:  0 = OK
            -1 = Rohfile konnte nicht geöffnet werden
            -2 = Fehler in Rohfile-Struktur bzw. Roh-Datensatz nicht plausibel
            -3 = Fehler beim Zusammenführen der Daten von Standard- und erweitertem Archiv
            -4 = Stundenwert-Zielfile nicht gefunden
            -5 = Stundenwert-Zwischenfile des erweiterten Archivs nicht gefunden }

  {----------------------------------}
  function Mess_Konv_Archiv: shortint;
  {----------------------------------}
  { Konvertierung Rohdaten des Messwert-Archivs }
  const
    CMaxImpKanaeleEK260 = 4;
    CPos_LetztRohKanal_Standard = 11;  // Position der letzten Standard-Wertespalte im Rohsatz
    CPos_ErstRohKanal_optional  = 19;  // Position der ersten optionalen, zusätzlichen Wertespalte im Rohsatz

  type
    { Struktur zum Merken eines Kanalwerts für Stundenwert-Differenzbildung }
    Tletzt_daten = record
      datum: daterec;
      zeit: timerec;
      wert: double;
    end;

  var
    fs_roh: TFileOfCharStream;       { Rohdaten-Datei }  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    fs_RohTRec: TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
    FSize_roh: integer;
    std_satz: RohSRec;
    tag_satz: RohTRec;
    erg: shortint;
    Fehlertelegramm: boolean;
    rohsatz: string;
    zeichen: char;
    dummy: char;
    satz_ok: boolean;
    wert_ok: boolean;
    i, k: integer;
    s: string;
    len: integer;
    szTemp: string [10];
    Code : Integer;
    wert_k: double;
    l: Int64;
    Delta: double;
    letzt_daten_k: array [1..CMaxImpKanaeleEK260] of Tletzt_daten;
    datebuf: daterec;
    timebuf: timerec;
    Faktor: double;
    MBoben: double;
    MBunten: double;
    Diff: double;
    letzt_zwsatz_datum: daterec;
    letzt_zwsatz_zeit: timerec;
    ParaNrAllg: string;
    Datum: DateRec;
    Zeit: TimeRec;
    MB_OK: boolean;
    SBuf: string;
    dtOK: boolean;
    StaKanalKonvDataObj: TStaKanalKonvDataObj;
    iPos: integer;
    iOrdNr: longint;
    iBuf: longint;
  {$IFDEF NATGAS}
    fs_ascii: TTextFileStream;
    ASCIIFileName: TFileName;
    ASCIIZielkanal: string;
    s_ascii: string;
    Letzt_ASCII_Datum: daterec;
    Letzt_ASCII_Zeit: timerec;
    do_write_ascii: boolean;
  {$ENDIF}

  begin
    Result:=-1;
    if not FileExists (MessKonvRec.StdQuelldateiname) then exit;

    erg:=0;
    try
      { Rohfile öffnen: }
      fs_roh:=TFileOfCharStream.Create (MessKonvRec.StdQuelldateiname, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_roh:=fs_roh.Size;

        { Stundenwert-Zielfile erzeugen: }
        fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (RohSRec));
        try
          { Tagessatz-Zielfile erzeugen: }
          fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (RohTRec));
          try
          {$IFDEF NATGAS}
            { ASCII-Zielkanal aus Stationsname: }
            ASCIIZielkanal:=Get_natGAS_ASCIIZielkanal (MessKonvRec.Sta_Stationsname);
            { ASCII-Filename aus Stationsname: }
            ASCIIFileName:=Get_natGAS_ASCIIFilename (MessKonvRec.Sta_Stationsname);

            do_write_ascii:=false;
            if MessKonvRec.Select = 1 then begin              { nur bei Automatik-Abruf }
              { Datum und Zeit des letzten ASCII-Satzes ermitteln: }
              Get_natGAS_LetztASCIIDatumZeit (ASCIIFileName, Letzt_ASCII_Datum, Letzt_ASCII_Zeit);

              if FileExists (ASCIIFileName) then
                fs_ascii:=TTextFileStream.Create (ASCIIFileName, fmOpenReadWrite OR fmShareDenyWrite)  { ASCII-Datei: anhängen }
              else begin
                fs_ascii:=TTextFileStream.Create (ASCIIFileName, fmCreate);      { neuanlegen, falls nicht vorhanden }
                fs_ascii.WriteLn (CnatGAS_ASCIIKopf);  { nur beim Neuanlegen Kopf schreiben }
              end;
            end else
              fs_ascii:=nil;
            try
          {$ENDIF}

              { Vorbelegung von Datum/Zeit und Wert der Vorstunden-Kanalwerte für
                Zählerstands-Differenzbildung: Vorstundenwert unbekannt }
              for i:=Low (letzt_daten_k) to High (letzt_daten_k) do begin
                FillChar (letzt_daten_k [i].datum, SizeOf (letzt_daten_k [i].datum), 0);
                FillChar (letzt_daten_k [i].zeit, SizeOf (letzt_daten_k [i].zeit), 0);
                letzt_daten_k [i].wert:=-1.0;
              end;

              { Vorbelegung für Datum/Zeit des zuletzt geschriebenen std_satz: }
              FillChar (letzt_zwsatz_datum, SizeOf (letzt_zwsatz_datum), 0);
              FillChar (letzt_zwsatz_zeit, SizeOf (letzt_zwsatz_zeit), 0);

              { Konvertierung }
              Fehlertelegramm:=false; { Vorbelegung: Rohfile enthält kein Fehlertelegramm }
              while fs_roh.Position < FSize_roh do begin
                rohsatz:='';
                zeichen:=NUL;
                { Datensätze bilden: bis EOT, ETX oder LF lesen }
                while (zeichen <> ETX) AND (zeichen <> EOT) AND (zeichen <> LF) AND
                       (fs_roh.Position < FSize_roh) do begin
                  fs_roh.Read (zeichen);
                  if (zeichen <> CR) AND (zeichen <> LF) AND
                     (zeichen <> EOT) AND (zeichen <> ETX) then
                    rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne CR, LF) }

                  { das auf ETX und EOT folgende BCC überlesen }
                  if ((zeichen = ETX) OR (zeichen = EOT)) AND (fs_roh.Position < FSize_roh) then begin
                    fs_roh.Read (dummy);
                  end;
                end;

                Application.ProcessMessages;
                { vom Rohsatz interessiert nur der Teil ab STX:
                  -> geändert wegen EK260 mit NUL-Zeichen bei Datenempfang mit Break; 11.04.2003, WW
                  -> geändert für Abruf mit Blockgröße > 1; 03.12.2013, WW }
                iPos:=Pos (STX, rohsatz);
                if iPos > 0 then
                  rohsatz:=Copy (rohsatz, iPos+1, length (rohsatz));

                FillChar (std_satz, SizeOf (std_satz), 0);         { Vorbelegung std_satz }
                with std_satz do begin
                  satzstatus:= $00;  { Satzstatus fest: OK }

                  { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                  for i:=1 to c_maxKanalZahl do begin
                    kanal[i].wert:=0;
                    KanalOrig[i].wert:=0;
                    if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then
                      kanal[i].kanalstatus:=$A0
                    else                                  { Impulskanal }
                      kanal[i].kanalstatus:=$80;
                    KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                    KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                  end;
                end;  { with std_satz }

                FillChar (tag_satz, SizeOf (tag_satz), 0);         { Vorbelegung tag_satz }
                with tag_satz do begin
                  satzstatus:= $00;  { Satzstatus fest: OK }

                  { Vorbelegung Zählerstati für fehlend }
                  for i:=1 to c_maxKanalZahl do begin
                    E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                    E_zaehler[i].wert:=0;
                    K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                    K_zaehler[i].wert:=0;
                  end;
                end;  { with tag_satz }

                { rohsatz in Datensatz-Werte '(..)' aufsplitten
                  Achtung: Index-Nummerierung i der einzelnen Werte gilt nur für EK260, ab Version 2.00 ! }
                satz_ok:=false;   { Vorbelegung: Datensatz ist nicht ok; 30.04.2012, WW }
                i:=0;
                iOrdNr:=-1;  { Default-Ordnungsnummer; 18.05.2018, WW }
            {$IFDEF NATGAS}
                s_ascii:=ASCIIZielkanal;          { ASCII-Datensatz beginnt mit Zielkanal }
            {$ENDIF}
                while length (rohsatz) > 0 do begin
                  inc (i);
                  s:=F_Zerlegen (rohsatz, ')');  { lesen bis ')' }

                  { Datensatz-Wert auf richtige Struktur '(..)' prüfen }
                  wert_ok:=false;  { Vorbelegung: Datensatz-Wert ist falsch }
                  len:=length (s);
                  if len > 0 then begin
                    if s[1] = '(' then begin     { erstes Zeichen muß '(' sein }
                      wert_ok:=true;           { Datensatz-Wert ist ok }
                      s:=Copy (s, 2, length (s));         { '(' wegschneiden }

                      { auf Fehlertelegramm prüfen (gekennzeichnet durch #): }
                      if length(s) > 0 then begin
                        if s[1] = '#' then begin
                          Fehlertelegramm:=true;
                          break;
                        end;
                      end;
                    end;
                  end;
                  if not wert_ok then
                    erg:=-2;    { Ergebnis: Fehler in Rohfile-Struktur }

                  with std_satz do begin
                    { i = 2: Archiv-Ordnungsnummer; 18.05.2018, WW }
                    if i = 2 then begin
                      if (length (s) > 0) AND wert_ok then begin
                        Val (s, iBuf, Code);
                        if Code = 0 then
                          iOrdNr:=iBuf;
                      end;
                    end

                    { i = 3: Zeitstempel }
                    else if i = 3 then begin
                      satz_ok:=true;   { Neue Vorbelegung: Datensatz ok, wenn Zeitstempel vorhanden; 30.04.2012, WW }
                      if not wert_ok OR (length (s) <> 19) then begin  { bei Strukturfehler: ganzen Datensatz verwerfen }
                        satz_ok:=false;
                        erg:=-2;
                        break;
                      end;
                      with Datum do begin
                        szTemp:=Copy (s, 1, 4);        { Jahr }
                        Val (szTemp, year, Code);
                        if Code <> 0 then
                          wert_ok:=false;
                        szTemp:=Copy (s, 6, 2);        { Monat }
                        Val (szTemp, month, Code);
                        if Code <> 0 then
                          wert_ok:=false;
                        szTemp:=Copy (s, 9, 2);        { Tag }
                        Val (szTemp, day, Code);
                        if Code <> 0 then
                          wert_ok:=false;
                      end;

                      with Zeit do begin
                        szTemp:=Copy (s, 12, 2);       { Stunde }
                        Val (szTemp, hour, Code);
                        if Code <> 0 then
                          wert_ok:=false;
                        szTemp:=Copy (s, 15, 2);       { Minute }
                        Val (szTemp, min, Code);
                        if Code <> 0 then
                          wert_ok:=false;
                        szTemp:=Copy (s, 18, 2);       { Sekunde }
                        Val (szTemp, sec, Code);
                        if Code <> 0 then
                          wert_ok:=false;
                        hsec:=0;
                      end;

                      if RecToDateTime (Datum, Zeit, tag_satz.DatumZeit) then begin
                        if Zeit.hour <= MessKonvRec.TagesEnde then
                          tag_satz.DatumZeit:=tag_satz.DatumZeit - 1; { Zählerstand für Gastag }
                      end else
                        wert_ok:=false;

                      if not wert_ok then begin
                        satz_ok:=false;
                        erg:=-2;
                        break;
                      end;

            {$IFDEF NATGAS}
                      { ASCII-Datenstring: }
                      s_ascii:=s_ascii + CnatGAS_ASCIITrenner +
                               FDateStr (Datum) + CnatGAS_ASCIITrenner + FTimeStr (Zeit);
            {$ENDIF}
                    end

                    { i = 4: Vn  -> Kanal 1 im Stundensatz- und Tagessatz-Zwischenfile
                          5: VnG -> Kanal 2     "
                          6: Vb  -> Kanal 3     "
                          7: Vo  -> Kanal 4     "
                          8: p   -> Kanal 5 im Stundensatz-Zwischenfile
                          9: T   -> Kanal 6     "
                         10: K   -> Kanal 7     "     ab 25.9.2013
                         11: Z   -> Kanal 8     "          "
                      >= 19: optionale, zusätzlich im EK280 konfigurierte Kanäle -> Kanal 9 ff. (-> nur Originalwerte); ab 21.02.2017 }
                    else if ((i >= 4) AND (i <= CPos_LetztRohKanal_Standard)) OR
                            (i >= CPos_ErstRohKanal_optional) then begin  // 21.02.2017, WW
                      if (length (s) > 0) AND wert_ok then begin
                        // k = LGZ-Kanal
                        if (i <= CPos_LetztRohKanal_Standard) then  // feste Standard-Kanäle
                          k:=i - 3
                        else  // optionale, zusätzliche Kanäle; 21.02.2017, WW
                          k:=8 + i - (CPos_ErstRohKanal_optional - 1);  // folgen nach den 8 Standard-Kanälen

                        if (k <= C_MaxKanalZahl) then begin  // 21.02.2017, WW
                          if k > iMaxMessKanal then
                            iMaxMessKanal:=k;  // Höchste LGZ-Kanalnummer merken

                          Val (s, wert_k, Code);     { Kanalwert aus Gerät (physikalisch !) }

                          KanalOrig [k].Wert:=wert_k;                    { Original-Rohwert }
                          KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                          KanalOrig [k].OrdNr := iOrdNr; { Ordnungsnummer zuweisen; 18.05.2018, WW }

                          if (i <= CPos_LetztRohKanal_Standard) then begin  // feste Standard-Kanäle: normierte LGZ-Werte berechnen; 21.02.2017, WW
                            if (k >= MessKonvRec.Analogkanal_von) AND (k <= MessKonvRec.Analogkanal_bis) then begin { Analogkanal }
                              { Stundensatz-Record: vom Gerät gelieferten physikalischen
                                Analogwert rückrechnen auf LGZ-Rohwert
                                -> p und T: über obere und untere Meßbereichgrenze aus Geräteparameter
                                -> Z- und K-Zahl: über obere und untere Meßbereichgrenze aus
                                                  Stammdaten (es gibt logischerweise keine Geräteparameter) }
                              MB_OK:=true;
                              MBoben:=0.0;
                              MBunten:=0.0;
                              if i = 8 then begin  // p
                                if MessKonvRec.ParameterListe <> nil then begin
                                  if MessKonvRec.ParameterListe.GetValue (CP_ELS_MBoben_p, SBuf) then begin
                                    SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                                    Val (SBuf, MBoben, Code);
                                  end else
                                    MB_OK:=false;
                                  if MessKonvRec.ParameterListe.GetValue (CP_ELS_MBunten_p, SBuf) then begin
                                    SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                                    Val (SBuf, MBunten, Code);
                                  end else
                                    MB_OK:=false;
                                end else
                                  MB_OK:=false;
                              end
                              else if i = 9 then begin  // T
                                if MessKonvRec.ParameterListe <> nil then begin
                                  if MessKonvRec.ParameterListe.GetValue (CP_ELS_MBoben_T, SBuf) then begin
                                    SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                                    Val (SBuf, MBoben, Code);
                                  end else
                                    MB_OK:=false;
                                  if MessKonvRec.ParameterListe.GetValue (CP_ELS_MBunten_T, SBuf) then begin
                                    SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                                    Val (SBuf, MBunten, Code);
                                  end else
                                    MB_OK:=false;
                                end else
                                  MB_OK:=false;
                              end
                              else begin  // K, Z
                                if MessKonvRec.KanalList <> nil then begin
                                  StaKanalKonvDataObj:=MessKonvRec.KanalList.GetMRGKanal (k);
                                  if StaKanalKonvDataObj <> nil then begin
                                    MBoben:=StaKanalKonvDataObj.Daten.MessBereichMax;
                                    MBunten:=StaKanalKonvDataObj.Daten.MessBereichMin;
                                  end else
                                    MB_OK:=false;
                                end else
                                  MB_OK:=false;
                              end;

                              if MB_OK then begin
                                Delta:=MBoben - MBunten;
                                if Delta <> 0 then begin
                                  L:= Round ((wert_k - MBunten) / Delta * 10000);
                                  if L >= 0 then begin
                                    if L > 9999 then
                                      Kanal [k].Wert:=9999
                                      { Fehlend-Bit hier nicht zurücksetzen, um Analogwert-"Überlauf"
                                        behelfsmäßig zu kennzeichnen }
                                    else begin
                                      Kanal [k].Wert:=L;
                                      Kanal [k].KanalStatus := Kanal [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                                    end;
                                  end;
                                end;
                              end;
                            end
                            else begin                                          { Impulskanal }
                              { Stundensatz-Record: vom Gerät gelieferte physikalische Zählerstände rückrechnen auf
                                normierte Zählerstandsdifferenzen }
                              if (Zeit.min = 0) AND (Zeit.sec = 0) then begin  { nur volle Stunden interessieren ! }
                                if letzt_daten_k [k].wert > -1 then begin     { Differenzbildung nur möglich, wenn Vor-Zählerstand bekannt }
                                  datebuf:=letzt_daten_k [k].datum;
                                  timebuf:=letzt_daten_k [k].zeit;
                                  P_AddMinuten (datebuf, timebuf, 60);
                                  { Vor-Zählerstand muß von Vorstunde sein: }
                                  if (CmpDate (datebuf, Datum) + CmpTime (timebuf, Zeit)) = 0 then begin
                                    Faktor:=0.0;
                                    if k >= 3 then begin    { Vb und Vo }
                                      { physikalischen Impulswert für Vb und Vo rückrechnen auf LGZ-Rohwert
                                        über Geräteparameter "cp-Wert Eingang 1": }
                                      if MessKonvRec.ParameterListe <> nil then begin
                                        if MessKonvRec.ParameterListe.GetValue (CP_ELS_cpWert [1], SBuf) then begin
                                          SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                                          Val (SBuf, Faktor, Code);
                                        end;
                                      end;
                                    end
                                    else begin
                                      { physikalischen Impulswert für Vn und VnG rückrechnen auf LGZ-Rohwert
                                        über OrgFaktor aus Stammdaten:
                                        -> für Normvolumen gibts keine Umrechnung in Impulse, da es aus dem
                                           Betriebsvolumen errechnet wird ! }
                                      if MessKonvRec.KanalList <> nil then
                                        Faktor:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (k)).Daten.OrgFaktor;
                                    end;
                                    if Faktor <> 0 then begin
                                      Diff:=wert_k - letzt_daten_k [k].wert;   { Zählerstands-Differenz }
                                      l:=Round (Diff / Faktor);
                                      if l >= 0 then begin
                                        if l > High (Kanal [k].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                                          Kanal [k].Wert:=High (Kanal [k].Wert);
                                          Kanal [k].KanalStatus := Kanal [k].KanalStatus or $01; { Überlauf-Bit setzen }
                                        end else
                                          Kanal [k].Wert:=l;
                                        Kanal [k].KanalStatus := Kanal [k].KanalStatus and $7F;  { Fehlend-Bit löschen }
                                      end;
                                    end;
                                  end;
                                end;
                                { wert_k mit Datum/Zeit merken: }
                                letzt_daten_k [k].datum:=Datum;
                                letzt_daten_k [k].zeit:=Zeit;
                                letzt_daten_k [k].wert:=wert_k;
                              end;

                              { in Tagessatz-Record physikalische Zählerstände der
                                Kanäle 1..4 eintragen: }
                              if (k >= 1) AND (k <= c_maxKanalZahl) then begin
                                tag_satz.E_zaehler[k].wert:=wert_k;  // Rundung raus; 27.03.2013, WW
                                tag_satz.E_zaehler[k].zaehlerstatus:=
                                  tag_satz.E_zaehler[k].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                              end;
                            end;
                          end;  // if (i <= CPos_LetztRohKanal_Standard)
                        end;  // if (k <= C_MaxKanalZahl)
                      end;

            {$IFDEF NATGAS}
                      { ASCII-Datenstring: }
                      s_ascii:=s_ascii + CnatGAS_ASCIITrenner;
                      if wert_ok then
                        s_ascii:=s_ascii + s;      { Wert-String aus Gerät, bei fehlendem Wert oder Strukturfehler: leer }
            {$ENDIF}
                    end;
                  end;  { with std_satz }
                end;  { while length (rohsatz) }

                if Fehlertelegramm then
                  break           { bei Fehlertelegramm: raus ohne Datenfile-Eintrag }
                else begin
                  if satz_ok then begin
                    dtOK:=RecToDateTime (Datum, Zeit, std_satz.DatumZeit);
                    { Datensätze in Stundensatz- und Tagessatz-Zwischendatei schreiben:
                      -> Falls mehrere Sätze mit gleichem Zeitstempel hintereinander folgen, wird nur
                         der erste geschrieben. Im ersten Satz stehen (durch Tests ermittelt) ALLE
                         Kanalwerte.
                      -> Mit doppelter 2-Uhr-Stunde bei SZ/WZ-Wechsel; 23.04.2013, WW }
                    if ((CmpDate (letzt_zwsatz_datum, Datum) +
                         CmpTime (letzt_zwsatz_zeit, Zeit)) <> 0) OR                         
                       (letzt_zwsatz_datum.day = 0) OR
                       (Assigned (MessKonvRec.WZ_SZKonfigList) AND
                        MessKonvRec.WZ_SZKonfigList.FindUmstellungstermin (
                          IncHour (std_satz.DatumZeit), CResSZtoWZ)) then begin  // 06.08.2021, WW
                      letzt_zwsatz_datum:=Datum;
                      letzt_zwsatz_zeit:=Zeit;

                      if dtOK then
                        fs_RohSRec.WriteRec (std_satz);   { Stundensatz in Zwischendatei schreiben }

                      { Tagessatz nur in Zwischendatei schreiben, wenn vom Tagesende: }
                      if (Zeit.hour = MessKonvRec.Tagesende) AND
                         (Zeit.min = 0) AND (Zeit.sec = 0) then
                        fs_RohTRec.WriteRec (tag_satz);
                    end;

            {$IFDEF NATGAS}
                    { ASCII-Daten: }
                    if Assigned (fs_ascii) then begin
                      if DateTimeToRec (std_satz.DatumZeit, datebuf, timebuf) then begin
                        if (CmpDate (datebuf, Letzt_ASCII_Datum) +
                            CmpTime (timebuf, Letzt_ASCII_Zeit)) > 0 then
                          do_write_ascii:=true;        { ab einschließlich diesem Satz ASCII-Daten schreiben }
                      end;
                      if do_write_ascii then
                        fs_ascii.WriteLn (s_ascii);               { ASCII-Datei schreiben }
                    end;
            {$ENDIF}
                  end;  { if satz_ok }
                end;
              end;   { while not EOF }

          {$IFDEF NATGAS}
            finally
              fs_ascii.Free;   { ASCII-File schließen }
            end;
          {$ENDIF}
          finally
            fs_RohTRec.Free;      { Zwischenfile für Tagessätze schließen }
          end;
        finally
          fs_RohSRec.Free;      { Zwischenfile für Stundensätze schließen }
        end;
      finally
        fs_roh.Free;       { Rohfile schließen }
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.StdQuelldateiname);  { Rohfile löschen }

      Result:=erg;
    except
      Result:=-1;
    end;
  end;  { Mess_Konv_Archiv }


  {---------------------------------------------------------------------------}
  function Mess_Konv_ArchivErweitert (var StdZwDateinameExt: string): shortint;
  {---------------------------------------------------------------------------}
  { Konvertierung Rohdaten eines optionalen, erweiterten Archivs;
    Rückgabe: Name des Zwischenfiles mit Daten des erweiterten Messwert-Archivs
                (leer, wenn nicht konvertiert/erzeugt) }
  const
    COffset_LgzKanalNr_Ext = 50;  // Offset für LGZ-Kanalnummern des erweiterten Archivs

  var
    Rohfilename: string;
    fs_roh: TFileOfCharStream;       { Rohdaten-Datei }
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;
    std_satz: RohSRec;
    erg: shortint;
    Fehlertelegramm: boolean;
    rohsatz: string;
    zeichen: char;
    dummy: char;
    satz_ok: boolean;
    wert_ok: boolean;
    i, k: integer;
    s: string;
    len: integer;
    szTemp: string [10];
    Code : Integer;
    wert_k: double;
    letzt_zwsatz_datum: daterec;
    letzt_zwsatz_zeit: timerec;
    Datum: DateRec;
    Zeit: TimeRec;
    dtOK: boolean;
    iPos: integer;
    iOrdNr: longint;
    iBuf: longint;
    KanalNrStr: string;
    iIndex: integer;
    slKanalNr: TStringList;

  begin
    Result:=-1;
    { Vorbelegung Rückgabe: }
    StdZwDateinameExt := '';  // Zwischenfile für das erweiterte Messwerte-Archiv nicht erstellt

    { Rohdateiname des erweiterten Archivs hat fest Index 1 in der StdQuellDateiNameListe: }
    iIndex:=1;

    if MessKonvRec.StdQuellDateiNameListe.Count > iIndex then begin
      { Name des Rohfiles des erweiterten Messwerte-Archivs: }
      Rohfilename:=MessKonvRec.StdQuellDateiNameListe [iIndex];    { enthält Kanalnummer(n) und Rohfilename }
      KanalNrStr:=F_Zerlegen (Rohfilename, ';');  { Kanalnummer(n) bis zum Strichpunkt }
      if length (Rohfilename) = 0 then exit;
      if not FileExists (Rohfilename) then exit;
    end
    else begin
      Result:=0;  // OK, kein erweitertes Messwerte-Archiv zu konvertieren (Standardfall)
      exit;
    end;

    { Name des Zwischenfiles für das erweiterte Messwerte-Archiv (mit angehängtem Index): }
    StdZwDateinameExt := MessKonvRec.StdZieldateiname + '_' + IntToStr (iIndex);

    erg:=0;
    try
      slKanalNr := TStringList.Create;
      try
        { Kanalnummern sind durch Komma getrennt -> in Liste laden: }
        slKanalNr.CommaText := KanalNrStr;
        for i:=slKanalNr.Count - 1 downto 0 do begin
          if slKanalNr [i] <> '' then
            slKanalNr [i] := IntToStr (StrToIntDef (slKanalNr [i], 0))
          else
            slKanalNr.Delete(i);
        end;

        { Rohfile öffnen: }
        fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
        try
          FSize_roh:=fs_roh.Size;

          { Stundenwert-Zwischenfile für erweitertes Archiv erzeugen: }
          fs_RohSRec:=TFileOfRecStream.Create (StdZwDateinameExt, fmCreate, SizeOf (RohSRec));
          try
            { Vorbelegung für Datum/Zeit des zuletzt geschriebenen std_satz: }
            FillChar (letzt_zwsatz_datum, SizeOf (letzt_zwsatz_datum), 0);
            FillChar (letzt_zwsatz_zeit, SizeOf (letzt_zwsatz_zeit), 0);

            { Konvertierung }
            Fehlertelegramm:=false; { Vorbelegung: Rohfile enthält kein Fehlertelegramm }
            while fs_roh.Position < FSize_roh do begin
              rohsatz:='';
              zeichen:=NUL;
              { Datensätze bilden: bis EOT, ETX oder LF lesen }
              while (zeichen <> ETX) AND (zeichen <> EOT) AND (zeichen <> LF) AND
                     (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                if (zeichen <> CR) AND (zeichen <> LF) AND
                   (zeichen <> EOT) AND (zeichen <> ETX) then
                  rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne CR, LF) }

                { das auf ETX und EOT folgende BCC überlesen }
                if ((zeichen = ETX) OR (zeichen = EOT)) AND (fs_roh.Position < FSize_roh) then begin
                  fs_roh.Read (dummy);
                end;
              end;

              Application.ProcessMessages;
              { vom Rohsatz interessiert nur der Teil ab STX: }
              iPos:=Pos (STX, rohsatz);
              if iPos > 0 then
                rohsatz:=Copy (rohsatz, iPos+1, length (rohsatz));

              FillChar (std_satz, SizeOf (std_satz), 0);         { Vorbelegung std_satz }
              with std_satz do begin
                satzstatus:= $00;  { Satzstatus fest: OK }

                { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                for i:=1 to c_maxKanalZahl do begin
                  kanal[i].wert:=0;
                  KanalOrig[i].wert:=0;
                  kanal[i].kanalstatus:=$80;
                  KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                  KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer
                end;
              end;  { with std_satz }

              { rohsatz in Datensatz-Werte '(..)' aufsplitten
                Achtung: Index-Nummerierung i der einzelnen Werte gilt nur für EK260, ab Version 2.00 ! }
              satz_ok:=false;   { Vorbelegung: Datensatz ist nicht ok }
              i:=0;
              iOrdNr:=-1;  { Default-Ordnungsnummer }
              k:=COffset_LgzKanalNr_Ext;  { Default für LGZ-Kanalnummer }

              while length (rohsatz) > 0 do begin
                inc (i);
                s:=F_Zerlegen (rohsatz, ')');  { lesen bis ')' }

                { Datensatz-Wert auf richtige Struktur '(..)' prüfen }
                wert_ok:=false;  { Vorbelegung: Datensatz-Wert ist falsch }
                len:=length (s);
                if len > 0 then begin
                  if s[1] = '(' then begin     { erstes Zeichen muß '(' sein }
                    wert_ok:=true;           { Datensatz-Wert ist ok }
                    s:=Copy (s, 2, length (s));         { '(' wegschneiden }

                    { auf Fehlertelegramm prüfen (gekennzeichnet durch #): }
                    if length(s) > 0 then begin
                      if s[1] = '#' then begin
                        Fehlertelegramm:=true;
                        break;
                      end;
                    end;
                  end;
                end;
                if not wert_ok then
                  erg:=-2;    { Ergebnis: Fehler in Rohfile-Struktur }

                with std_satz do begin
                  { i = 2: Archiv-Ordnungsnummer }
                  if i = 2 then begin
                    if (length (s) > 0) AND wert_ok then begin
                      Val (s, iBuf, Code);
                      if Code = 0 then
                        iOrdNr:=iBuf;
                    end;
                  end

                  { i = 3: Zeitstempel }
                  else if i = 3 then begin
                    satz_ok:=true;   { Neue Vorbelegung: Datensatz ok, wenn Zeitstempel vorhanden }
                    if not wert_ok OR (length (s) <> 19) then begin  { bei Strukturfehler: ganzen Datensatz verwerfen }
                      satz_ok:=false;
                      erg:=-2;
                      break;
                    end;
                    with Datum do begin
                      szTemp:=Copy (s, 1, 4);        { Jahr }
                      Val (szTemp, year, Code);
                      if Code <> 0 then
                        wert_ok:=false;
                      szTemp:=Copy (s, 6, 2);        { Monat }
                      Val (szTemp, month, Code);
                      if Code <> 0 then
                        wert_ok:=false;
                      szTemp:=Copy (s, 9, 2);        { Tag }
                      Val (szTemp, day, Code);
                      if Code <> 0 then
                        wert_ok:=false;
                    end;

                    with Zeit do begin
                      szTemp:=Copy (s, 12, 2);       { Stunde }
                      Val (szTemp, hour, Code);
                      if Code <> 0 then
                        wert_ok:=false;
                      szTemp:=Copy (s, 15, 2);       { Minute }
                      Val (szTemp, min, Code);
                      if Code <> 0 then
                        wert_ok:=false;
                      szTemp:=Copy (s, 18, 2);       { Sekunde }
                      Val (szTemp, sec, Code);
                      if Code <> 0 then
                        wert_ok:=false;
                      hsec:=0;
                    end;

                    if not wert_ok then begin
                      satz_ok:=false;
                      erg:=-2;
                      break;
                    end;
                  end

                  { i >= 4: Kanäle des erweiterten EK280-Archivs -> Kanal 51 ff. (-> nur Originalwerte) }
                  else if (i >= 4) then begin
                    if slKanalNr.IndexOf(IntToStr (i)) >= 0 then begin  // nur, wenn Kanal konvertiert werden soll
                      if (length (s) > 0) AND wert_ok then begin
                        // k = LGZ-Kanal
                        inc (k);
                        if (k <= C_MaxKanalZahl) then begin
                          if (iMinMessKanalExt < 0) OR (k < iMinMessKanalExt) then
                            iMinMessKanalExt:=k;  // Niedrigste LGZ-Kanalnummer des erweiterten Archivs merken
                          if k > iMaxMessKanalExt then
                            iMaxMessKanalExt:=k;  // Höchste LGZ-Kanalnummer des erweiterten Archivs merken

                          Val (s, wert_k, Code);     { Kanalwert aus Gerät (physikalisch !) }

                          KanalOrig [k].Wert:=wert_k;                    { Original-Rohwert }
                          KanalOrig [k].KanalStatus:=KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                          KanalOrig [k].OrdNr:=iOrdNr; { Ordnungsnummer zuweisen }  

                        end;  // if (k <= C_MaxKanalZahl)
                      end;
                    end;
                  end;
                end;  { with std_satz }
              end;  { while length (rohsatz) }

              if Fehlertelegramm then
                break           { bei Fehlertelegramm: raus ohne Datenfile-Eintrag }
              else begin
                if satz_ok then begin
                  dtOK:=RecToDateTime (Datum, Zeit, std_satz.DatumZeit);
                  { Datensatz in Stundensatz-Zwischendatei schreiben:
                    -> Falls mehrere Sätze mit gleichem Zeitstempel hintereinander folgen, wird nur
                       der erste geschrieben. Im ersten Satz stehen (durch Tests ermittelt) ALLE
                       Kanalwerte.
                    -> Mit doppelter 2-Uhr-Stunde bei SZ/WZ-Wechsel }
                  if ((CmpDate (letzt_zwsatz_datum, Datum) +
                       CmpTime (letzt_zwsatz_zeit, Zeit)) <> 0) OR
                     (letzt_zwsatz_datum.day = 0) OR
                     (Assigned (MessKonvRec.WZ_SZKonfigList) AND
                      MessKonvRec.WZ_SZKonfigList.FindUmstellungstermin (
                        IncHour (std_satz.DatumZeit), CResSZtoWZ)) then begin 
                    letzt_zwsatz_datum:=Datum;
                    letzt_zwsatz_zeit:=Zeit;

                    if dtOK then
                      fs_RohSRec.WriteRec (std_satz);   { Stundensatz in Zwischendatei schreiben }
                  end;
                end;  { if satz_ok }
              end;
            end;   { while not EOF }
          finally
            fs_RohSRec.Free;      { Zwischenfile für Stundensätze schließen }
          end;
        finally
          fs_roh.Free;       { Rohfile schließen }
        end;

        if MessKonvRec.loeschen then
          DeleteFile (Rohfilename);  { Rohfile löschen }

        Result:=erg;
      finally
        slKanalNr.Free;
      end;
    except
      Result:=-1;
    end;
  end;  { Mess_Konv_ArchivErweitert }


  {------------------------------------------------------------------------}
  function Mess_Merge_ArchivErweitert (StdZwDateinameExt: string): shortint;
  {------------------------------------------------------------------------}
  { Zusammenführen der konvertierten Daten aus Standard-Messwert-Archiv und
    erweitertem Archiv;
    Übergabe: Name des Zwischenfiles mit Daten des erweiterten Messwert-Archivs }
  var
    FS: TFileOfRecStream;
    FS_Ext: TFileOfRecStream;
    FRecCount: integer;
    FRecCount_Ext: integer;
    Std_Satz: RohSRec;
    Std_Satz_Ext: RohSRec;
    k: integer;
    dtMerk: TDateTime;

  begin
    try
      if FileExists (StdZwDateinameExt) then begin
        if FileExists (MessKonvRec.StdZieldateiname) then begin
          if (iMinMessKanalExt > 0) AND (iMaxMessKanalExt > 0) then begin
            { Stundenwert-Zielfile öffnen: }
            FS:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmOpenReadWrite OR fmShareDenyWrite, SizeOf (RohSRec));
            try
              { Stundenwert-Zwischenfile des erweiterten Archivs öffnen: }
              FS_Ext:=TFileOfRecStream.Create (StdZwDateinameExt, fmOpenRead OR fmShareDenyWrite, SizeOf (RohSRec));
              try
                FRecCount:=FS.RecCount;
                FRecCount_Ext:=FS_Ext.RecCount;

                FillChar (Std_Satz_Ext, SizeOf (Std_Satz_Ext), 0);
                Std_Satz_Ext.DatumZeit := 0;
                dtMerk := 0;

                // Alle Datensätze in Zielfile lesen
                while FS.RecPosition < FRecCount do begin
                  Application.ProcessMessages;
                  FS.ReadRec (Std_Satz);

                  // Lesen in Zwischenfile bis Datensatz mit gleichem Zeitstempel gefunden
                  while (FS_Ext.RecPosition < FRecCount_Ext) AND
                        ((CmpDateTime (Std_Satz_Ext.DatumZeit, Std_Satz.DatumZeit) < 0) OR
                         (CmpDateTime (Std_Satz.DatumZeit, dtMerk) = 0)) do begin
                    Application.ProcessMessages;
                    FS_Ext.ReadRec (Std_Satz_Ext);

                    // bei gleichem Zeitstempel (z.B. doppelte 2-Uhr-Stunde bei
                    // SZ/WZ-Wechsel) nur 1 Datensatz weiterlesen:
                    if CmpDateTime (Std_Satz.DatumZeit, dtMerk) = 0 then
                      Break;
                  end;  { while FS_Ext.RecPosition < FRecCount_Ext }

                  if CmpDateTime (Std_Satz_Ext.DatumZeit, Std_Satz.DatumZeit) = 0 then begin
                    // Zeitstempel beider Datensätze sind gleich: Kanalwerte des erweiteren
                    // Archivs in Zielfile übernehemen
                    for k:=iMinMessKanalExt to iMaxMessKanalExt do begin
                      if (k >= 1) AND (k <= C_maxKanalZahl) then begin
                        // nur Originalwerte
                        Std_Satz.KanalOrig [k].Wert := Std_Satz_Ext.KanalOrig [k].Wert;
                        Std_Satz.KanalOrig [k].KanalStatus := Std_Satz_Ext.KanalOrig [k].KanalStatus;
                        Std_Satz.KanalOrig [k].OrdNr := Std_Satz_Ext.KanalOrig [k].OrdNr;
                      end;
                    end;  // for k

                    { Auf korrespondierenden Eintrag zurückpositionieren, um ihn in der
                      Datei zu überschreiben: }
                    FS.RecPosition:=FS.RecPosition - 1;
                    FS.WriteRec (Std_Satz);
                  end;

                  dtMerk := Std_Satz.DatumZeit;  // Zeitstempel merken
                end;  { while FS.RecPosition < FRecCount }
              finally
                FS_Ext.Free;
              end;
            finally
              FS.Free;
            end;
          end;  { if (iMinMessKanalExt > 0) AND (iMaxMessKanalExt > 0) }

          Result:=0;
        end else
          Result:=-4;  { Stundenwert-Zielfile nicht gefunden }

        if MessKonvRec.loeschen then
          DeleteFile (StdZwDateinameExt);  { Stundenwert-Zwischenfile des erweiterten Archivs löschen }
      end else
        Result:=-5;  { Stundenwert-Zwischenfile des erweiterten Archivs nicht gefunden }
    except
      Result:=-3;
    end;
  end;  { Mess_Merge_ArchivErweitert }


var
  erg: shortint;
  StdZwDateinameExt: string;

begin
  // Vorbelegungen Rückgabe
  iMaxMessKanal:=-1;
  iMinMessKanalExt:=-1;
  iMaxMessKanalExt:=-1;

  // Konvertierung Rohdaten des Messwert-Archivs
  Result := Mess_Konv_Archiv;

  // Konvertierung Rohdaten eines optionalen, erweiterten Archivs:
  erg := Mess_Konv_ArchivErweitert (StdZwDateinameExt);
  if Result = 0 then  // Fehler nicht überschreiben
    Result := erg;

  if (StdZwDateinameExt <> '') then begin
    // Zusammenführen der konvertierten Daten aus Standard- und erweitertem Archiv:
    erg := Mess_Merge_ArchivErweitert (StdZwDateinameExt);
    if Result = 0 then  // Fehler nicht überschreiben
      Result := erg;
  end;
end;  { Mess_Konv_EK260 }


{-------------------------- Elster DS-100 -------------------------------------}

{------------------------------------------------------------}
function Mess_Konv_DS100 (MessKonvRec: TMessTagKonv): boolean;
{------------------------------------------------------------}
{ Konvertierung Elster DS-100 (1-Kanal- oder 4-Kanal-Version)
  -> Rohfileformat (je Kanal ein eigenes Rohfile): aneinander gereihte Rohantworten
     auf Verbrauchsdaten-Lesebefehl ?x (Rohdaten enthalten Messwerte und Tagessätze);
  Beispiel: x<Datenteil>%<Checksumme, 2 Zeichen><CR><LF>
            x<Datenteil>%<Checksumme, 2 Zeichen><CR><LF>
            .
            .
  -> MessKonvRec.StdQuellDateinameListe enthält Einträge mit Format: <Kanalnummer>;<Rohdateiname>
     Fehlender Kanal: Listeneintrag fehlt oder Eintrag mit <Rohdateiname> = Leer-String }

  {---------------------------------------------------------------------------------}
  procedure Insert_MW_ZwList (AMW_ZwList: TObjectList; var AMW_List_Pos: integer;
                              AKanalNr: byte; ADatumZeit: TDateTime; AWert: longint);
  {---------------------------------------------------------------------------------}
  { Meßwert in Zwischenliste eintragen }
  var
    i: integer;
    std_satz: RohSRec;
    RohSRecObj: TRohSRecObj;

  begin
    if Search_MW_ZwList_DatumZeit (AMW_ZwList, AMW_List_Pos, ADatumZeit, true) then begin  // Rohdaten/Liste zeitlich absteigend
      { Eintrag für Datum, Zeit in MW-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
      if (AMW_List_Pos >= 0) AND (AMW_List_Pos < AMW_ZwList.Count) then begin
        with TRohSRecObj (AMW_ZwList.Items [AMW_List_Pos]).RohSRec do begin
          KanalOrig [AKanalNr].Wert:=AWert;         { Original-Rohwert }
          KanalOrig [AKanalNr].KanalStatus:=KanalOrig [AKanalNr].KanalStatus and $7F; { Fehlend-Bit löschen }
          Kanal [AKanalNr].Wert:=AWert;
          Kanal [AKanalNr].KanalStatus:=Kanal [AKanalNr].KanalStatus and $7F; { Fehlend-Bit löschen }
        end;
      end;
    end
    else begin
      { nicht gefunden: neuen Eintrag in MW-Zwischenliste an mw_list_pos eintragen }
      FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
      with std_satz do begin
        satzstatus:= $00;  { Satzstatus fest: OK }

        { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
        for i:=1 to c_maxKanalZahl do begin
          kanal[i].wert:=0;
          KanalOrig[i].wert:=0;
          kanal[i].kanalstatus:=$80;          { nur Impulskanäle }
          KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
          KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
        end;
      end;  { with std_satz }

      { Daten setzen: }
      std_satz.DatumZeit:=ADatumZeit;
      std_satz.KanalOrig [AKanalNr].Wert:=AWert;  { Original-Rohwert }
      std_satz.KanalOrig [AKanalNr].KanalStatus:=std_satz.KanalOrig [AKanalNr].KanalStatus and $7F;       { Fehlend-Bit löschen }
      std_satz.Kanal [AKanalNr].Wert:=AWert;
      std_satz.Kanal [AKanalNr].KanalStatus:=std_satz.Kanal [AKanalNr].KanalStatus and $7F;       { Fehlend-Bit löschen }

      { Objekt in Liste anhängen: }
      RohSRecObj:=TRohSRecObj.Create (std_satz);
      AMW_ZwList.Insert (AMW_List_Pos, RohSRecObj);
    end;
    inc (AMW_List_Pos);
  end;

  {-----------------------------------------------------------------------------}
  procedure Insert_ZS_ZwList (AZS_ZwList: TObjectList; var AZS_List_Pos: integer;
                              AKanalNr: byte; ADatumZeit: TDateTime;
                              AKontrZWert: longint);
  {-----------------------------------------------------------------------------}
  { Tagessatz in Zwischenliste eintragen }
  var
    i: integer;
    tag_satz: RohTRec;
    RohTRecExtObj: TRohTRecExtObj;

  begin
    if Search_ZS_ZwList_DatumZeit (AZS_ZwList, AZS_List_Pos, ADatumZeit, true) then begin  // Rohdaten/Liste zeitlich absteigend
      { Eintrag für Datum, Zeit in ZS-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
      if (AZS_List_Pos >= 0) AND (AZS_List_pos < AZS_ZwList.Count) then begin
        with TRohTRecExtObj (AZS_ZwList.Items [AZS_List_Pos]).RohTRec do begin
          K_zaehler[AKanalNr].wert:=AKontrZWert;
          K_zaehler[AKanalNr].zaehlerstatus:=
            K_zaehler[AKanalNr].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
        end;
      end;
    end
    else begin
      { nicht gefunden: neuen Eintrag in Zählerstands-Zwischenliste an zs_list_pos eintragen }
      FillChar (tag_satz, SizeOf (tag_satz), 0);   { Vorbelegung tag_satz }
      with tag_satz do begin
        satzstatus:= $00;  { Satzstatus fest: OK }

        { Vorbelegung Kanalstatus für fehlend }
        for i:=1 to c_maxKanalZahl do begin
          E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
          E_zaehler[i].wert:=0;
          K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
          K_zaehler[i].wert:=0;
        end;
      end;  { with tag_satz }

      { Kanal-Daten setzen:
        -> mit physikalischem Datum (Gastag-Berechnung erst beim Schreiben der
           Zwischenliste in Zwischendatei) }
      tag_satz.DatumZeit:=ADatumZeit;
      tag_satz.K_zaehler[AKanalNr].wert:=AKontrZWert;
      tag_satz.K_zaehler[AKanalNr].zaehlerstatus:=
        tag_satz.K_zaehler[AKanalNr].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }

      { Objekt in Liste anhängen: }
      RohTRecExtObj:=TRohTRecExtObj.Create (tag_satz, -1);
      AZS_ZwList.Insert (AZS_List_Pos, RohTRecExtObj);    { Listen-Index wurde in Seach_ZS_ZwList gesetzt }
    end;
    inc (AZS_List_Pos);    { zs_list_pos weiterpositionieren für nächsten Eintrag }
  end;

  {-------------------------------------------------------------------------------}
  procedure KonvTaggrenzeDaten (AMW_ZwList: TObjectList; var AMW_List_Pos: integer;
                                AKanalNr: byte; ADaten: string; AMWDaten: string;
                                var DTagGrenze: daterec; var ZTagGrenze: timerec);
  {-------------------------------------------------------------------------------}
  { Konvertiert Meßwert-Rohdaten in Zwischenliste für Meßwerte;
    Übergabe: Zwischenliste für Meßwerte
              Kanalnummer
              Daten-String
              MWDaten: angesammelte Meßwert-Rohdaten
    Rückgabe: DTagGrenze, ZTagGrenze = Zeitpunkt der Tag-Grenze im Daten-String }
  var
    L, code: integer;
    Pos: integer;
    MWStr: string[4];
    MW: integer;
    Datum: daterec;
    Zeit: timerec;
    Tag: integer;
    Stunden: byte;
    DatumZeit: TDateTime;

  begin
    L:=length (ADaten);
    { Monatswechsel erkennen: }
    Val (Copy (ADaten, L-1, 2), Tag, code);
    if Tag > DTagGrenze.day then
      P_Vortag(DTagGrenze);

    DTagGrenze.day:=Tag;
    Val (Copy (ADaten, L-4, 2), ZTagGrenze.hour, code);
    Val (Copy (ADaten, L-7, 2), ZTagGrenze.min, code);

    Datum:=DTagGrenze;
    Zeit:=ZTagGrenze;
    AMWDaten:=AMWDaten + Copy (ADaten, 1, L-12);   { Meßwerte-String vervollständigen }
    L:=length (AMWDaten);
    Stunden:=L DIV 3;
    P_AddDatumStunden (Datum, Zeit, Stunden);
    Pos:=1;
    while Pos < L do begin
      MWStr:=Copy (AMWDaten, Pos, 3);
      Val('$'+ MWStr, MW, code);
      if RecToDateTime (Datum, Zeit, DatumZeit) then
        Insert_MW_ZwList (AMW_ZwList, AMW_List_Pos, AKanalNr, DatumZeit, MW);
      inc(Pos,3);
      P_AddDatumStunden (Datum, Zeit, -1);
    end;
    { es sind jetzt Stundenwerte bis "Taggrenze-Zeit + 1 Std." ausgelesen: }
    P_AddDatumStunden (DTagGrenze, ZTagGrenze, 1);
  end;

  {------------------------------------------------------------------------------------}
  procedure KonvMonatgrenzeDaten (AMW_ZwList: TObjectList; AZS_ZwList: TObjectList;
                                  var AMW_List_Pos: integer; var AZS_List_Pos: integer;
                                  AKanalNr: byte; ADaten: string; AMWDaten: string;
                                  var DMonatGrenze: daterec; var ZMonatGrenze: timerec);
  {------------------------------------------------------------------------------------}
  { Konvertiert Rohdaten des nicht setzbaren Zählers (Gesamtzähler) und Meßwerte
    in Zwischenlisten für Meßwerte und Tagessätze;
    Übergabe: Zwischenliste für Meßwerte
              Zwischenliste für Tagessätze
              Kanalnummer
              Daten-String,
              MWDaten: angesammelte Meßwert-Rohdaten
    Rückgabe: DMonatGrenze, ZMonatGrenze = Zeitpunkt der Monatsgrenze im Daten-String }
  var
    L, code: integer;
    ZSStr: string;
    AktIntervall: string[3];
    Datum: daterec;
    Zeit: timerec;
    Pos: integer;
    MWStr: string[4];
    MW: integer;
    Stunden: byte;
    KontrZ: longint;
    DTBuf: TDateTime;

  begin
    L:=length (ADaten);
    with DMonatGrenze do begin
      Val (Copy (ADaten, L-1, 2), year, code);
      year:=Jahr2to4stellig (year);
      Val (Copy (ADaten, L-4, 2), month, code);
      Val (Copy (ADaten, L-7, 2), day, code);
    end;
    with ZMonatGrenze do begin
      Val (Copy (ADaten, L-10, 2), hour, code);
      Val (Copy (ADaten, L-13, 2), min, code);
      sec:=0;
      hsec:=0;
    end;
    ZSStr:=Copy (ADaten, L-26, 12);
    AktIntervall:=Copy (ADaten, L-28, 2);
    AMWDaten:=AMWDaten + Copy (ADaten, 1, length (ADaten)-33); { Meßwerte-String vervollständigen }

    { Zählerstand des nicht setzbaren Zählers (Gesamtzähler) konvertieren:
      -> Gesamtzähler wird Kontrollzähler zugeordnet (wie im WFG-MDE-System)
      -> Gesamtzähler liefert Impulse (unbewertet) }
    ZSStr:=Copy (ZSStr, 11, 2) + Copy (ZSStr, 8, 2) + Copy (ZSStr, 5, 2) + Copy (ZSStr, 2, 2);
    Val (ZSStr, KontrZ, code);  { wird dem Kontrollzähler zugeordnet }
    if RecToDateTime(DMonatGrenze, ZMonatGrenze, DTBuf) then
      Insert_ZS_ZwList (AZS_ZwList, AZS_List_Pos, AKanalNr, DTBuf, KontrZ);

    { Meßwerte konvertieren: }
    ZMonatGrenze.min:=0;                   { für Meßwerte: immer volle Stunde }
    Datum:=DMonatGrenze;
    Zeit:=ZMonatGrenze;
    L:=length (AMWDaten);
    Stunden:=L DIV 3;
    P_AddDatumStunden (Datum, Zeit, Stunden);
    Pos:=1;
    while Pos < L do begin
      MWStr:=Copy (AMWDaten, Pos, 3);
      Val ('$'+ MWStr, MW, code);
      if RecToDateTime (Datum, Zeit, DTBuf) then
        Insert_MW_ZwList (AMW_ZwList, AMW_List_Pos, AKanalNr, DTBuf, MW);
      inc(Pos,3);
      P_AddDatumStunden(Datum,Zeit,-1);
    end;
    { es sind jetzt Stundenwerte bis "Monatgrenze-Zeit + 1 Std." ausgelesen: }
    P_AddDatumStunden (DMonatGrenze, ZMonatGrenze, 1);
  end;


var
  fs_roh: TFileOfCharStream;       { Rohdatendatei }  // 06.10.2015, WW
  fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
  fs_RohTRec: TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
  FSize_roh: integer;
  Rohfilename: string;
  rohsatz: string;
  zeichen: char;
  KanalNr: integer;
  s: string;
  datum: DateRec;
  zeit: TimeRec;
  zs_zwlist: TObjectList;
  mw_zwlist: TObjectList;
  tag_satz: RohTRec;
  std_satz: RohSRec;
  i: integer;
  zs_list_pos: integer;
  mw_list_pos: integer;
  l: integer;
  KanalNrStr: string;
  FFxSatz: string;
  MWDatenStr: string;
  PosFF: integer;  // Integer statt Byte; 04.10.2011, WW
  Daten: string;
  Steuercode: string;
  c: char;
  MWDaten: string;
  first: boolean;
  Rohdatenstruktur_gueltig: boolean;
  hour, min, sec, msec: word;

begin
  Result := True;

  zs_zwlist:=TObjectList.Create;             { Zwischenliste für Zählerstände }
  try
    mw_zwlist:=TObjectList.Create;           { Zwischenliste für Messwerte }
    try

{--- Konvertierung Messwert-Rohdateien -> Messwert-/Tagessatz-Zwischenliste ---}
      Rohdatenstruktur_gueltig:=true;   { Vorbelegung: Rohdatenstruktur ist OK }
      try
        for l:=0 to MessKonvRec.StdQuellDateiNameListe.Count-1 do begin  { alle Messwert-Rohdateien abarbeiten }
          Rohfilename:=MessKonvRec.StdQuellDateiNameListe [l];    { enthält Kanalnummer und Rohfilename }
          KanalNrStr:=F_Zerlegen (Rohfilename, ';');  { Kanalnummer bis zum Strichpunkt }
          KanalNr:=StrToInt (KanalNrStr);
          if length (Rohfilename) = 0 then Continue;
          if not FileExists (Rohfilename) then begin
            Result := False;
            Continue;
          end;

          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;

            mw_list_pos:=0;     { Vorbelegung Stundensatz-Listenposition }
            zs_list_pos:=0;     { Vorbelegung Tagsatz-Listenposition }
            FFxSatz:='';
            MWDatenStr:='';
            first:=true;

            { Datum, Zeit initialisieren (wird während des Konvertierungsdurchlaufs ständig aktualisiert: }
            with Datum do begin
              year:=2100;
              month:=1;
              day:=1;
            end;
            with Zeit do begin
              hour:=0;
              min:=0;
              sec:=0;
              hsec:=0;
            end;

            { Konvertierung }
            while fs_roh.Position < FSize_roh do begin
              rohsatz:='';
              zeichen:=NUL;
              while (zeichen <> LF) AND (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
              end;

              Application.ProcessMessages;

              { Rohsatz-Struktur prüfen: erstes Zeichen muß 'x' sein und '%' als
                Trennzeichen zur Checksumme muß enthalten sein: }
              if (Pos ('x', rohsatz) <> 1) OR (Pos ('%', rohsatz) = 0) then begin
                Rohdatenstruktur_gueltig:=false;
                Break;
              end;

              S:=ExtractString (rohsatz, 'x', '%', 0);    { reinen Datenteil rauskopieren }
              FFxSatz:=FFxSatz + S;
              PosFF:=DS100_FFxPos (FFxSatz); { Position eines evtl. vorhand. FFx-Steuercodes }
              { FFxSatz solange abarbeiten bis kein FFx-Steuercode mehr enthalten ist: }
              while PosFF <> 0 do begin
                Daten:=Copy (FFxSatz, 1, PosFF-1);
                Steuercode:=Copy (FFxSatz, PosFF, 3);
                c:=Steuercode[3];                                          { Code FFx }
                FFxSatz:=Copy (FFxSatz, PosFF+3, length (FFxSatz));  { Daten des nächsten FFx-Satzes }
                case c of
                  '1': begin  { Neustart }
                         DS100_MWFiltern_FFx (Daten, MWDaten, 0);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                  '2': begin  { Altstart }
                         DS100_MWFiltern_FFx (Daten, MWDaten, 18);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                  '3': begin  { falscher Wert }
                         DS100_MWFiltern_FFx (Daten, MWDaten, 0);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                  '4': begin  { Ersatzwert }
                         DS100_MWFiltern_FFx (Daten, MWDaten, 0);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                  '5': begin  { korrigierter Wert }
                         DS100_MWFiltern_FFx (Daten, MWDaten, 0);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                  '8': begin  { definierbarer Steuercode }
                         DS100_MWFiltern_DefFStCode (Daten, MWDaten);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                  '9': begin  { I/O-Marke }
                         DS100_MWFiltern_FFx (Daten, MWDaten, 3);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                  'A': begin  { Monat-Grenze }
                         KonvMonatgrenzeDaten (mw_zwlist, zs_zwlist, mw_list_pos, zs_list_pos,
                                               KanalNr, Daten, MWDatenStr, Datum, Zeit);
                         MWDatenStr:='';     { Meßwertdaten-Puffer ist konvertiert, jetzt leeren }
                       end;
                  'B': begin  { Tag-Grenze }
                         KonvTaggrenzeDaten (mw_zwlist, mw_list_pos, KanalNr, Daten, MWDatenStr, Datum, Zeit);
                         MWDatenStr:='';     { Meßwertdaten-Puffer ist konvertiert, jetzt leeren }
                       end;
                  'C': begin  { Auslesen-Beginn }
                         if first then begin   { Datum, Zeit bei erstem FFC-Satz setzen }
                           Get_DS100_FFCDatumZeit (Daten, Datum, Zeit);
                           Zeit.sec:=0;
                           first:=false;
                         end;
                         DS100_MWFiltern_FFx (Daten, MWDaten, 21);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                  'D': begin  { Auslesen-Ende }
                         DS100_MWFiltern_FFx (Daten, MWDaten, 18);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                  'E': begin  { neue Intervall-Größe }
                         DS100_MWFiltern_FFx (Daten, MWDaten, 3);
                         MWDatenStr:=MWDatenStr + MWDaten;         { Meßwertdaten aufsammeln }
                       end;
                end; { case }

                PosFF:=DS100_FFxPos (FFxSatz); { Position eines evtl. weiteren FFx-Steuercodes }
              end;  { while PosFF }
            end;  { while not EOF }
          finally
            fs_roh.Free;
          end;

          if MessKonvRec.loeschen then
            DeleteFile (Rohfilename);  { Rohfile löschen }

          if not Rohdatenstruktur_gueltig then Break;
        end;  { for l }

  {------------ Konvertierung Zwischenlisten -> Zwischendateien -----------------}

        if Rohdatenstruktur_gueltig then begin
          { ZS-Zwischenliste in ZS-Zwischenfile konvertieren; Zählerstände sind zeitlich
            absteigend enthalten: }
          fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
          try
            for i:=zs_zwlist.Count - 1 downto 0 do begin
              Application.ProcessMessages;
              tag_satz:=TRohTRecExtObj (zs_zwlist.Items [i]).RohTRec;
              DecodeTime (tag_satz.DatumZeit, hour, min, sec, msec);
              if hour <= MessKonvRec.TagesEnde then
                tag_satz.DatumZeit:=tag_satz.DatumZeit - 1;  { Zählerstand für Gastag }
              fs_RohTRec.WriteRec (tag_satz);
            end;  { for }
          finally
            fs_RohTRec.Free;
          end;

          { MW-Zwischenliste in MW-Zwischenfile konvertieren; Messwerte sind zeitlich
            absteigend enthalten: }
          fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (std_satz));
          try
            for i:=mw_zwlist.Count - 1 downto 0 do begin
              Application.ProcessMessages;
              std_satz:=TRohSRecObj (mw_zwlist.Items [i]).RohSRec;
              fs_RohSRec.WriteRec (std_satz);
            end;  { for }
          finally
            fs_RohSRec.Free;
          end;
        end;  { if Rohdatenstruktur_gueltig }
      except
        Result:=False;
      end;
    finally;
      mw_zwlist.Free;
    end;
  finally
    zs_zwlist.Free;
  end;
end;


{------------------------------ Tritschler ------------------------------------}

{---------------------------------------------------------------------}
function Mess_Konv_Tritschler_IEC (MessKonvRec: TMessTagKonv): Boolean;
{---------------------------------------------------------------------}
{ Konvertierung Tritschler IEC-Protokoll für: VC2, TTG }

  {-----------------------------------------------------------------------}
  function Is_ZS_Rohsatz (ARohsatz: string; var KanalNr: integer): boolean;
  {-----------------------------------------------------------------------}
  { prüft, ob Rohsatz Zählerstand enthält;
    Übergabe: Rohsatz
    Rückgabe: dem ZS-Rohsatz zugeordnete Wieser-Kanalnummer (wenn Ergebnis true)
    Ergebnis: true, wenn Rohsatz ZS-Rohsatz ist }
  begin
    Result:=false;
    KanalNr:=0;  { Vorbelegung }

    case MessKonvRec.Konvgruppe of
      13: begin     { VC2 }
            if (Copy (ARohsatz, 1, 5) = '7.8.1') OR
               (Copy (ARohsatz, 1, 5) = '7.8.2') OR
               (Copy (ARohsatz, 1, 5) = '7.9.1') OR
               (Copy (ARohsatz, 1, 5) = '7.9.2') OR
               (Copy (ARohsatz, 1, 4) = '7.12') then begin

              { Kennziffer -> Wieser-Kanalnummer }
              if Copy (ARohsatz, 1, 5) = '7.8.1' then                    { Vb }
                KanalNr:=1
              else if Copy (ARohsatz, 1, 5) = '7.8.2' then               { Vb Stör }
                KanalNr:=3
              else if Copy (ARohsatz, 1, 5) = '7.9.1' then               { Vn }
                KanalNr:=2
              else if Copy (ARohsatz, 1, 5) = '7.9.2' then               { Vn Stör }
                KanalNr:=4
              else if Copy (ARohsatz, 1, 4) = '7.12' then                { Vref }
                KanalNr:=5;
              Result:=true;
            end;
          end;

      18: begin    { TTG }
            if (Copy (ARohsatz, 1, 3) = '180') OR
               (Copy (ARohsatz, 1, 3) = '280') then begin

              { Kennziffer -> Wieser-Kanalnummer }
              if Copy (ARohsatz, 1, 3) = '180' then                    { Vb }
                KanalNr:=1
              else if Copy (ARohsatz, 1, 3) = '280' then               { Vn }
                KanalNr:=2;
              Result:=true;
            end;
          end;
    end;
  end;

  {-------------------------------------------------------------------------------------}
  function Is_ZS_Rohsatz_mit_Zeitstempel (ARohsatz: string;
                                          ALetztMonatsabschlussNummer: integer): boolean;
  {-------------------------------------------------------------------------------------}
  { prüft, ob Rohsatz Zählerstand enthält, zu dem auch ein Zeitstempel verfügbar ist;
    Übergabe: Rohsatz
              Nummer des letzten Monatsabschlusses
    Ergebnis: true, wenn Rohsatz ZS-Rohsatz mit Zeitstempel ist }
  const
    C_Stern = '*';
  var
    s: string;
    intbuf: integer;
    code: integer;

  begin
    Result:=false;
    case MessKonvRec.Konvgruppe of
      13: begin     { VC2 }
            { nur monatl. Zählerstände (mit Stern) haben einen Zeitstempel im Rohsatz }
            if Pos (C_Stern, ARohsatz) <> 0 then
              Result:=true;
          end;

      18: begin     { TTG }
            if ALetztMonatsabschlussNummer <> -1 then begin  { Infos zum letzten Monatsabschluss sind vorhanden }
              { Nummer des Monatsabschlusses aus Rohsatz extrahieren, wenn vorhanden: }
              s:=ExtractString (ARohsatz, NUL, '(', 0);
              s:=Copy (s, 4, length (s));
              if length (s) > 0 then begin  { Rohsatz enthält Monatsabschluss-Nummer }
                Val (s, intbuf, code);
                if code = 0 then begin
                  if intbuf = ALetztMonatsabschlussNummer then
                    Result:=true;  { Rohsatz trägt die Nummer des letzten Monatsabschlusses
                                     -> ZS-Rohsatz kann konvertiert werden }
                end;
              end;
            end;
          end;
    end;
  end;

  {-----------------------------------------------------------------------}
  function Is_MW_Rohsatz (ARohsatz: string; var KanalNr: integer): boolean;
  {-----------------------------------------------------------------------}
  { prüft, ob Rohsatz Messwerte enthält;
    Übergabe: Rohsatz
    Rückgabe: dem MW-Rohsatz zugeordnete Wieser-Kanalnummer (wenn Ergebnis true)
    Ergebnis: true, wenn Rohsatz MW-Rohsatz ist }
  begin
    Result:=false;
    KanalNr:=0;  { Vorbelegung }

    case MessKonvRec.Konvgruppe of
      13: begin     { VC2 }
            if (Copy (ARohsatz, 1, 5) = '7.72.') OR
               (Copy (ARohsatz, 1, 5) = '7.73.') OR
               (Copy (ARohsatz, 1, 6) = '7.74.3') OR
               (Copy (ARohsatz, 1, 6) = '7.74.4') OR
               (Copy (ARohsatz, 1, 6) = '7.74.5') OR
               (Copy (ARohsatz, 1, 6) = '7.74.6') OR
               (Copy (ARohsatz, 1, 5) = '7.75.') then begin

              { Kennziffer -> Wieser-Kanalnummer }
              if Copy (ARohsatz, 1, 5) = '7.72.' then         { Vb (Impuls) }
                KanalNr:=1
              else if Copy (ARohsatz, 1, 5) = '7.73.' then    { Vn (Impuls) }
                KanalNr:=2
              else if Copy (ARohsatz, 1, 6) = '7.74.3' then   { Vb Stör (Impuls) }
                KanalNr:=3
              else if Copy (ARohsatz, 1, 6) = '7.74.4' then   { Vn Stör (Impuls) }
                KanalNr:=4
              else if Copy (ARohsatz, 1, 6) = '7.74.5' then   { Druck (Analog) }
                KanalNr:=6
              else if Copy (ARohsatz, 1, 6) = '7.74.6' then   { Temperatur (Analog) }
                KanalNr:=7
              else if Copy (ARohsatz, 1, 5) = '7.75.' then    { Vref (Impuls) }
                KanalNr:=5;
              Result:=true;
            end;
          end;

      18: begin     { TTG }
            if (Copy (ARohsatz, 1, 5) = '7.72.') OR
               (Copy (ARohsatz, 1, 5) = '7.73.') then begin

              { Kennziffer -> Wieser-Kanalnummer }
              if Copy (ARohsatz, 1, 5) = '7.72.' then         { Vb (Impuls) }
                KanalNr:=1
              else if Copy (ARohsatz, 1, 5) = '7.73.' then    { Vn (Impuls) }
                KanalNr:=2;
              Result:=true;
            end;
          end;
    end;
  end;

  {---------------------------------------------------}
  function Is_Zeit_Rohsatz (ARohsatz: string): boolean;
  {---------------------------------------------------}
  { prüft, ob Rohsatz Gerätezeit enthält;
    Übergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Zeit-Rohsatz ist }
  begin
    Result:=false;
    case MessKonvRec.Konvgruppe of
      13: begin     { VC2 }
            if Copy (ARohsatz, 1, 3) = '28.' then                  { Uhrzeit }
              Result:=true;
          end;

      18: begin     { TTG }
            if Copy (ARohsatz, 1, 3) = '200' then                  { Uhrzeit }
              Result:=true;
          end;
    end;
  end;

  {----------------------------------------------------}
  function Is_Datum_Rohsatz (ARohsatz: string): boolean;
  {----------------------------------------------------}
  { prüft, ob Rohsatz Gerätedatum enthält;
    Übergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Datum-Rohsatz ist }
  begin
    Result:=false;
    case MessKonvRec.Konvgruppe of
      13: begin     { VC2 }
            if Copy (ARohsatz, 1, 3) = '29.' then                  { Datum }
              Result:=true;
          end;

      18: begin     { TTG }
            if Copy (ARohsatz, 1, 3) = '201' then                  { Datum }
              Result:=true;
          end;
    end;
  end;

  {--------------------------------------------------------}
  function Is_Tagesende_Rohsatz (ARohsatz: string): boolean;
  {--------------------------------------------------------}
  { prüft, ob Rohsatz Tagesende enthält;
    Übergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Tagesende-Rohsatz ist }
  begin
    Result:=false;
    case MessKonvRec.Konvgruppe of
      13: begin     { VC2 }
            if Copy (ARohsatz, 1, 3) = '94.' then                  { Tagesende }
              Result:=true;
          end;

      { Tagesende ist in TTG-Daten nicht enthalten }

    end;
  end;

  {--------------------------------------------------------}
  function Is_TempKelvin_Kanal (AKanalNr: integer): boolean;
  {--------------------------------------------------------}
  { Ergebnis: true, wenn Kanal Temperaturen in Kelvin liefert }
  begin
    Result:=false;
    case MessKonvRec.Konvgruppe of
      13: begin     { VC2 }
            if AKanalNr = 7 then
              Result:=true;
          end;

      { Temperatur-Kanal ist in TTG-Daten nicht enthalten }

    end;
  end;

  {-------------------------------------------------------------------}
  function Is_LetztMonatsabschluss_Rohsatz (ARohsatz: string): boolean;
  {-------------------------------------------------------------------}
  { prüft, ob Rohsatz Daten zum letzten Monatsabschluss enthält;
    Übergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Rohsatz des letzten Monatsabschlusses ist }
  begin
    Result:=false;
    case MessKonvRec.Konvgruppe of
      18: begin     { TTG }
            if Copy (ARohsatz, 1, 3) = '700' then   { letzter Monatsabschluss }
              Result:=true;
          end;

      { Daten zum letzten Monatsabschluss müssen beim VC2 nicht ausgewertet werden }

    end;
  end;


const
  { Vorbelegung für Zeitstempel der aktuellen VC2-Zählerstände: }
  C_AktDatumDefault: DateRec = (year: 2100; month: 1; day: 1);
  C_AktZeitDefault:  TimeRec = (hour: 0; min: 0; sec: 0; hsec: 1);

type
  { Record mit Informationen zum letzten Monatsabschluss }
  TLetztMonatsabschluss = record
    Nummer: integer;
    Datum: DateRec;
    Zeit: TimeRec;
  end;

var
  fs_roh: TFileOfCharStream;       { Rohdatendatei }  // 06.10.2015, WW
  fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
  fs_RohTRec: TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
  FSize_roh: integer;
  rohsatz: string;
  zeichen: char;
  KanalNr: integer;
  s: string;
  zs: double;
  code: integer;
  datum: DateRec;
  zeit: TimeRec;
  szTemp: string [10];
  tagesende: integer;
  faktor: double;
  anzmp: integer;
  anztage: integer;
  tag: integer;
  mp: integer;
  datumbuf: DateRec;
  zeitbuf: TimeRec;
  mw_phys: double;
  mwstr: string [6];
  aktdatum: DateRec;
  aktzeit: TimeRec;
  zs_zwlist: TObjectList;
  mw_zwlist: TObjectList;
  tag_satz: RohTRec;
  std_satz: RohSRec;
  RohTRecExtObj: TRohTRecExtObj;
  RohSRecObj: TRohSRecObj;
  i, l: integer;
  zs_list_pos: integer;
  mw_list_pos: integer;
  LetztKanalNr: integer;
  mw_lgz: Int64;
  Sta_Faktor: double;
  Sta_Delta: double;
  dtAktDatumZeit: TDateTime;
  dtZeitfenster_Stundenwechsel: TDateTime;
  dtBuf: TDateTime;
  gefunden: boolean;
  Rohfilename: string;
  lesedurchgang: integer;
  start_lesedurchgang: integer;
  intbuf: integer;
  LetztMonatsabschluss: TLetztMonatsabschluss; { nur für TTG notwendig }
  isTTG_mit_Gastag_bei_ZS: boolean;
  bKonvRohsatz: boolean;
  tagesende_rec: TimeRec;
  dtDatumZeit_vor_Abruf: TDateTime;
  dtBuf_Datum: TDateTime;
  dtBuf_Zeit: TDateTime;

begin
  Result := True;
  zs_zwlist:=TObjectList.Create;             { Zwischenliste für Zählerstände }
  try
    mw_zwlist:=TObjectList.Create;           { Zwischenliste für Messwerte }
    try

{------------- Konvertierung Rohdateien -> Zwischenlisten ---------------------}
{  VC2: 1 Rohdatei mit allen Messwert- und Tagessatz-Daten aller Kanäle
   TTG: je Kanal 1 Rohdatei mit Messwert- und Tagessatz-Daten                  }

      try
        tagesende:=-1;  { Vorbelegung: Tagesende aus Rohdaten noch nicht ermittelt }
        for l:=0 to MessKonvRec.StdQuellDateiNameListe.Count-1 do begin  { alle Messwert-Rohdateien abarbeiten }
          Rohfilename:=MessKonvRec.StdQuellDateiNameListe [l];    { Rohfilename }
          if length (Rohfilename) = 0 then Continue;
          if not FileExists (Rohfilename) then begin
            Result := False;
            Continue;
          end;

          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;

            // für die Konvertierung von VC2-Daten ist die Kenntnis des im Gerät
            // eingestellten Tagesendes notwendig. Das Tagesende wird aus den
            // Rohdaten gelesen. Da das Tagesende erst am Ende der Rohdaten
            // steht, muß das Rohfile zweimal durchgelesen werden:
            //   Lesedurchgang 1: Ermittlung des Tagesendes (nur für VC2)
            //   Lesedurchgang 2: Daten-Konvertierung
            tagesende:=-1;  { Vorbelegung: Tagesende aus Rohdaten noch nicht ermittelt }
            FillChar (LetztMonatsabschluss, SizeOf(LetztMonatsabschluss), 0);
            LetztMonatsabschluss.Nummer:=-1;  { Vorbelegung: Info zum letzten Monatsabschluss noch nicht ermittelt }

            case MessKonvRec.KonvGruppe of
              13: start_lesedurchgang:=1; { VC2 }
            else
              start_lesedurchgang:=2; { TTG }
            end;

            for lesedurchgang:=start_lesedurchgang to 2 do begin
              if (MessKonvRec.KonvGruppe = 13) AND (lesedurchgang = 2) AND (tagesende = -1) then
                Break;  { Fehler ! Tagesende konnte in Lesedurchgang 1 nicht
                          ermittelt werden. Konvertierungs-Durchgang abbrechen }

              fs_roh.Seek (0, soFromBeginning);  { an den Anfang der Rohdatei positionieren }
              mw_list_pos:=0;               { Vorbelegung Stundensatz-Listenposition }
              zs_list_pos:=0;               { Vorbelegung Tagsatz-Listenposition }
              LetztKanalNr:=1;

              rohsatz:='';
              zeichen:=NUL;
              { Rohfileheader bis zum STX lesen }
              while (zeichen <> STX) AND (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                rohsatz:=rohsatz + zeichen;                    { Zeile bilden }
              end;
              { TTG-Version an Stellen 9..13 -> VE.RS
                -> Versionsstelle 'E' (Stelle vor dem Dezimalpunkt) wird ausgewertet }
              isTTG_mit_Gastag_bei_ZS:=false;
              try
                { ab Version 7.x enthalten die Rohdaten als Datum des letzten Monatsabschlusses
                  (Kennziffer 700) den Gastag, davor den physikalischen Tag: }
                if length (rohsatz) >= 10 then
                  isTTG_mit_Gastag_bei_ZS:=StrToInt (rohsatz[10]) >= 7;
              except
              end;

              { Konvertierung }
              while (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
                rohsatz:='';
                zeichen:=NUL;
                while (zeichen <> LF) AND (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
                  fs_roh.Read (zeichen);
                  rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
                end;

                Application.ProcessMessages;

                if lesedurchgang = 1 then begin  { Lesedurchgang 1 ermittelt Tagesende }
                  if Is_Tagesende_Rohsatz (rohsatz) then begin
                    s:=ExtractString (rohsatz, '(', ')', 0);
                    Val (s, intbuf, code);                            { Wert }
                    if code = 0 then
                      tagesende:=intbuf;
                  end;
                end
                else begin                       { Lesedurchgang 2 konvertiert Rohdaten }
                  { Zählerstände konvertieren in ZS-Zwischenliste }
                  if Is_ZS_Rohsatz (rohsatz, KanalNr) then begin
                    if KanalNr <> LetztKanalNr then begin
                      LetztKanalNr:=KanalNr;
                      zs_list_pos:=0;
                    end;

                    bKonvRohsatz:=false;  // Vorbelegung: ZS-Rohsatz nicht konvertieren
                    if MessKonvRec.KonvGruppe = 13 then begin         { VC2 }
                      { Zählerstände mit Zeitstempel konvertieren: }
                      if Is_ZS_Rohsatz_mit_Zeitstempel (rohsatz, -1) then begin
                        bKonvRohsatz:=true;
                        s:=ExtractString (rohsatz, '(', ';', 0);
                        Val (s, zs, code);                            { Wert }
                        s:=ExtractString (rohsatz, ';', ')', 0);
                        with Datum do begin                           { Datum-/Zeitstempel }
                          szTemp:=Copy (s, 1, 2);        { Jahr }
                          Val (szTemp, year, Code);
                          if Code <> 0 then;
                          if year < 70 then
                            year:=year + 2000
                          else
                            year:=year + 1900;
                          szTemp:=Copy (s, 4, 2);        { Monat }
                          Val (szTemp, month, Code);
                          if Code <> 0 then;
                          szTemp:=Copy (s, 7, 2);        { Tag }
                          Val (szTemp, day, Code);
                          if Code <> 0 then;
                        end;
                        with Zeit do begin
                          szTemp:=Copy (s, 10, 2);       { Stunde }
                          Val (szTemp, hour, Code);
                          if Code <> 0 then;
                          szTemp:=Copy (s, 13, 2);       { Minute }
                          Val (szTemp, min, Code);
                          if Code <> 0 then;
                          sec:=0;
                          hsec:=0;
                        end;
                        P_Vortag (Datum);  { Datum in Rohdaten ist immer physikalisch -> auf Gastag umrechnen }
                      end
                      else begin
                        { Zählerstand ohne Zeitstempel (aktueller ZS) konvertieren,
                          wenn lt. Übergabe-Flag gewünscht: 16.05.2007, WW }
                        if MessKonvRec.bAktZaehler then begin
                          bKonvRohsatz:=true;
                          s:=ExtractString (rohsatz, '(', ';', 0);
                          Val (s, zs, code);                            { Wert }
                          { Datum/Zeitstempel wird hier nur vorbelegt, kann erst später
                            mit aktdatum, aktzeit belegt werden }
                          Datum:=C_AktDatumDefault;
                          Zeit:=C_AktZeitDefault;
                        end;
                      end;
                    end
                    else begin                                        { TTG }
                      { nur Zählerstand mit Zeitstempel (über Nummer des letzten
                        Monatsabschlusses) kann konvertiert werden: }
                      if Is_ZS_Rohsatz_mit_Zeitstempel (rohsatz, LetztMonatsabschluss.Nummer) then begin
                        bKonvRohsatz:=true;
                        s:=ExtractString (rohsatz, '(', ')', 0);
                        Val (s, zs, code);      { Rohwert kann Gleitkommazahl sein }
                                                { Rundung raus; 27.03.3013, WW }
                        { Zeitstempel des letzten Monatsabschlusses zuweisen: }
                        Datum:=LetztMonatsabschluss.Datum;
                        Zeit:=LetztMonatsabschluss.Zeit;
                        { Datum in Rohdaten ist bereits Gastagsdatum ab Version 7.x ! }
                        if not isTTG_mit_Gastag_bei_ZS then
                          P_Vortag (Datum);
                      end;
                    end;

                    if bKonvRohsatz then begin  // ZS-Rohsatz soll konvertiert werden
                      RohTRecExtObj:=nil;
                      if RecToDateTime (Datum, Zeit, DTBuf) then begin
                        gefunden:=Search_ZS_ZwList_DatumZeit (zs_zwlist, zs_list_pos, DTBuf, true);  // Rohdaten/Liste zeitlich absteigend
                        if gefunden then begin
                          { Eintrag für Datum, Zeit in ZS-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
                          if (zs_list_pos >= 0) AND (zs_list_pos < zs_zwlist.Count) then
                            RohTRecExtObj:=TRohTRecExtObj (zs_zwlist.Items [zs_list_pos]);
                        end
                        else begin
                          { nicht gefunden: neues ZS-Zwischenlisten-Objekt erzeugen }
                          FillChar (tag_satz, SizeOf (tag_satz), 0);   { Vorbelegung tag_satz }
                          with tag_satz do begin
                            satzstatus:= $00;  { Satzstatus fest: OK }

                            { Vorbelegung Kanalstatus für fehlend }
                            for i:=1 to c_maxKanalZahl do begin
                              E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                              E_zaehler[i].wert:=0;
                              K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                              K_zaehler[i].wert:=0;
                            end;
                          end;  { with tag_satz }

                          tag_satz.DatumZeit:=DTBuf;
                          RohTRecExtObj:=TRohTRecExtObj.Create (tag_satz, -1);
                        end;

                        if Assigned (RohTRecExtObj) then begin
                          { Kanaldaten setzen: }
                          with RohTRecExtObj.RohTRec do begin
                            E_zaehler[KanalNr].wert:=zs;
                            E_zaehler[KanalNr].zaehlerstatus:=
                              E_zaehler[KanalNr].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                          end;

                          { nicht gefunden: Objekt-Eintrag in ZS-Zwischenliste an zs_list_pos eintragen }
                          if not gefunden then
                            zs_zwlist.insert (zs_list_pos, RohTRecExtObj);
                        end;
                      end;  { if RecToDateTime }
                      inc (zs_list_pos);
                    end;  { if bKonvRohsatz }
                  end  { if Is_ZS_Rohsatz }

                  { Lastprofile konvertieren in MW-Zwischenliste -> Meßwerte }
                  else if Is_MW_Rohsatz (rohsatz, KanalNr) then begin
                    s:=ExtractString (rohsatz, '(', ';', 0);
                    Val (s, faktor, code);                                     { Faktor }
                    s:=ExtractString (rohsatz, ';', ';', 1);
                    Val (s, anzmp, code);                  { Anzahl Meßperioden pro Tag }
                    s:=ExtractString (rohsatz, ';', ';', 2);
                    Val (s, anztage, code);                    { Anzahl Tagesdatensätze }

                    if KanalNr <> LetztKanalNr then begin
                      LetztKanalNr:=KanalNr;
                      mw_list_pos:=0
                    end;

                    for tag:=1 to anztage do begin           { Tagesdatensätze auslesen }
                      rohsatz:='';
                      zeichen:=NUL;
                      while (zeichen <> LF) AND (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
                        fs_roh.Read (zeichen);
                        rohsatz:=rohsatz + zeichen;                       { Zeile bilden }
                      end;

                      s:=Copy (rohsatz, 1, 8);                     { Datumstempel lesen }
                      with Datum do begin
                        szTemp:=Copy (s, 1, 2);        { Jahr }
                        Val (szTemp, year, Code);
                        if Code <> 0 then;
                        if year < 70 then
                          year:=year + 2000
                        else
                          year:=year + 1900;
                        szTemp:=Copy (s, 4, 2);        { Monat }
                        Val (szTemp, month, Code);
                        if Code <> 0 then;
                        szTemp:=Copy (s, 7, 2);        { Tag }
                        Val (szTemp, day, Code);
                        if Code <> 0 then;
                      end;

                      { Meßperiodenwerte extrahieren: }
                      s:=Copy (rohsatz, 9, length (rohsatz));
                      for mp:=1 to anzmp do begin
                        datumbuf:=datum;
                        FillChar (zeitbuf, SizeOf (zeitbuf), 0);
                        if anzmp > 1 then begin
                          case MessKonvRec.KonvGruppe of
                            13: begin  { VC2: Reihenfolge abh. vom Tagesende }
                                  zeitbuf.hour:=IndexToHourExt (mp, tagesende, false);  { Umrechnung mp -> Stunde }
                                  if zeitbuf.hour <= tagesende then
                                    P_NachTag (datumbuf);        { Gastag -> physik. Tag }
                                end;
                            18: begin  { TTG: Reihenfolge unabh. vom Tagesende 0, 23...1 }
                                  zeitbuf.hour:=IndexToHourExt (mp, 0, false);  { Umrechnung mp -> Stunde }
                                  if zeitbuf.hour = 0 then
                                    P_NachTag (datumbuf);
                                end;
                          end;  { case }
                          mwstr:=Copy (s, 1+(mp-1)*4, 4);  { Wert 4-stellig für Meßperiodenwerte von K1, K2, K5 }
                        end
                        else begin   { gibts nur bei VC2 }
                          zeitbuf.hour:=tagesende;
                          P_NachTag (datumbuf);      { Gastag -> physik. Tag }
                          mwstr:=Copy (s, 1, 6);           { Wert 6-stellig für Tagessummen/-mittelwerte von K3, K4, K6, K7 }
                        end;

                        mw_phys:=StrToIntDef ('$' + mwstr, -1);        { Rohwert im Hex-Format }
                        mw_phys:=mw_phys * faktor;         { physikalischer Wert = Rohwert * Faktor }

                        RohSRecObj:=nil;

                        if RecToDateTime (datumbuf, zeitbuf, dtBuf) then begin
                          gefunden:=Search_MW_ZwList_DatumZeit (mw_zwlist, mw_list_pos, dtBuf, true);  // Rohdaten/Liste zeitlich absteigend
                          if gefunden then begin
                            { Eintrag für Datum, Zeit in MW-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
                            if (mw_list_pos >= 0) AND (mw_list_pos < mw_zwlist.Count) then
                              RohSRecObj:=TRohSRecObj (mw_zwlist.Items [mw_list_pos]);
                          end
                          else begin
                            { nicht gefunden: neues MW-Zwischenlisten-Objekt erzeugen }
                            FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
                            with std_satz do begin
                              satzstatus:= $00;  { Satzstatus fest: OK }

                              { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                              for i:=1 to c_maxKanalZahl do begin
                                kanal[i].wert:=0;
                                KanalOrig[i].wert:=0;
                                if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then  { Analogkanal }
                                  kanal[i].kanalstatus:=$A0
                                else                                  { Impulskanal }
                                  kanal[i].kanalstatus:=$80;
                                KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                                KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                              end;
                              DatumZeit:=dtBuf;
                            end;  { with std_satz }
                            RohSRecObj:=TRohSRecObj.Create (std_satz);
                          end;

                          if Assigned (RohSRecObj) then begin
                            { Kanaldaten setzen: }
                            with RohSRecObj.RohSRec do begin
                              KanalOrig [KanalNr].Wert:=mw_phys;  { Original-Rohwert }
                              KanalOrig [KanalNr].KanalStatus:=KanalOrig [KanalNr].KanalStatus and $7F;       { Fehlend-Bit löschen }

                              if (KanalNr >= MessKonvRec.Analogkanal_von) AND (KanalNr <= MessKonvRec.Analogkanal_bis) then begin { Analogkanal }
                                if Is_TempKelvin_Kanal (KanalNr) then
                                  mw_phys:=mw_phys - C_To_Kelvin;  { Temperatur in Kelvin -> Umrechnung in °C }
                                { physikalischen Analogwert rückrechnen auf LGZ-Rohwert über obere und untere Meßbereichgrenze aus
                                  Stammdaten: }
                                if MessKonvRec.KanalList <> nil then begin
                                  Sta_Delta:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (KanalNr)).Daten.MessBereichMax -
                                             TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (KanalNr)).Daten.MessBereichMin;
                                  if Sta_Delta <> 0 then begin
                                    mw_lgz:=Round ((mw_phys - TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (KanalNr)).Daten.MessBereichMin) /
                                                   Sta_Delta * 10000);
                                    if mw_lgz >= 0 then begin
                                      if mw_lgz > 9999 then
                                        Kanal [KanalNr].Wert:=9999
                                        { Fehlend-Bit hier nicht zurücksetzen, um Analogwert-"Überlauf"
                                          behelfsmäßig zu kennzeichnen }
                                      else begin
                                        Kanal [KanalNr].Wert:=mw_lgz;
                                        Kanal [KanalNr].KanalStatus:=Kanal [KanalNr].KanalStatus and $7F; { Fehlend-Bit löschen }
                                      end;
                                    end;
                                  end;
                                end;
                              end
                              else begin
                                { physikalischen Impulswert rückrechnen auf LGZ-Rohwert über OrgFaktor aus Stammdaten: }
                                if MessKonvRec.KanalList <> nil then begin
                                  Sta_Faktor:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (KanalNr)).Daten.OrgFaktor;
                                  if Sta_Faktor > 0 then begin
                                    mw_lgz := Round (mw_phys / Sta_Faktor);
                                    if mw_lgz > High (Kanal [KanalNr].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                                      Kanal [KanalNr].Wert:=High (Kanal [KanalNr].Wert);
                                      Kanal [KanalNr].KanalStatus:=Kanal [KanalNr].KanalStatus or $01;        { Überlauf-Bit setzen }
                                    end else
                                      Kanal [KanalNr].Wert:=mw_lgz;
                                    Kanal [KanalNr].KanalStatus := Kanal [KanalNr].KanalStatus and $7F;       { Fehlend-Bit löschen }
                                  end;
                                end;
                              end;
                            end;  { RohSRecObj.RohSRec }

                            { nicht gefunden: Objekt-Eintrag in MW-Zwischenliste an mw_list_pos eintragen }
                            if not gefunden then
                              mw_zwlist.insert (mw_list_pos, RohSRecObj);
                          end;  { if Assigned (RohSRecObj) }
                        end;  { if RecToDateTime }
                        inc (mw_list_pos);
                      end;  { for mp }
                    end;  { for tag }
                  end

                  { aktuelles Datum und Uhrzeit im Gerät }
                  else if Is_Zeit_Rohsatz (rohsatz) then begin          { Uhrzeit }
                    s:=ExtractString (rohsatz, '(', ')', 0);
                    with aktzeit do begin
                      szTemp:=Copy (s, 1, 2);       { Stunde }
                      Val (szTemp, hour, Code);
                      if Code <> 0 then;
                      szTemp:=Copy (s, 4, 2);       { Minute }
                      Val (szTemp, min, Code);
                      if Code <> 0 then;
                      szTemp:=Copy (s, 7, 2);       { Sekunde }
                      Val (szTemp, sec, Code);
                      if Code <> 0 then;
                      hsec:=0;
                    end;
                  end

                  else if Is_Datum_Rohsatz (rohsatz) then begin          { Datum }
                    s:=ExtractString (rohsatz, '(', ')', 0);
                    with aktdatum do begin
                      szTemp:=Copy (s, 1, 2);        { Jahr }
                      Val (szTemp, year, Code);
                      if Code <> 0 then;
                      if year < 70 then
                        year:=year + 2000
                      else
                        year:=year + 1900;
                      szTemp:=Copy (s, 4, 2);        { Monat }
                      Val (szTemp, month, Code);
                      if Code <> 0 then;
                      szTemp:=Copy (s, 7, 2);        { Tag }
                      Val (szTemp, day, Code);
                      if Code <> 0 then;
                    end;
                  end

                  else if Is_LetztMonatsabschluss_Rohsatz (rohsatz) then begin { letzter Monatsabschluss (für TTG-Zählerstand) }
                    s:=ExtractString (rohsatz, '(', ' ', 0);         { Nummer }
                    Val (s, intbuf, code);
                    if code = 0 then
                      LetztMonatsabschluss.Nummer:=intbuf;

                    s:=ExtractString (rohsatz, ' ', ' ', 0);          { Datum }
                    with LetztMonatsabschluss.Datum do begin
                      szTemp:=Copy (s, 1, 2);        { Jahr }
                      Val (szTemp, year, Code);
                      if Code <> 0 then;
                      if year < 70 then
                        year:=year + 2000
                      else
                        year:=year + 1900;
                      szTemp:=Copy (s, 4, 2);        { Monat }
                      Val (szTemp, month, Code);
                      if Code <> 0 then;
                      szTemp:=Copy (s, 7, 2);        { Tag }
                      Val (szTemp, day, Code);
                      if Code <> 0 then;
                    end;

                    s:=ExtractString (rohsatz, ' ', ')', 1);
                    with LetztMonatsabschluss.Zeit do begin
                      szTemp:=Copy (s, 1, 2);       { Stunde }
                      Val (szTemp, hour, Code);
                      if Code <> 0 then;
                      szTemp:=Copy (s, 4, 2);       { Minute }
                      Val (szTemp, min, Code);
                      if Code <> 0 then;
                      szTemp:=Copy (s, 7, 2);       { Sekunde }
                      Val (szTemp, sec, Code);
                      if Code <> 0 then;
                      hsec:=0;
                    end;

                    { 24 Uhr -> 0 Uhr des Folgetags: 21.02.2008, WW }
                    if LetztMonatsabschluss.Zeit.Hour = 24 then begin
                      P_Nachtag (LetztMonatsabschluss.Datum);
                      LetztMonatsabschluss.Zeit.Hour:=0;
                    end;
                  end;
                end;
              end;   { while not EOF }
            end;  { for lesedurchgang }
          finally
            fs_roh.Free;
          end;

          if MessKonvRec.loeschen then
            DeleteFile (Rohfilename);  { Rohfile löschen }
        end;  { for l }

  {------------ Konvertierung Zwischenlisten -> Zwischendateien -----------------}

        dtAktDatumZeit:=EncodeDate (aktdatum.year, aktdatum.month, aktdatum.day) +
                        EncodeTime (aktzeit.hour, aktzeit.min, aktzeit.sec, aktzeit.hsec);

        { ZS-Zwischenliste in ZS-Zwischenfile konvertieren: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          { ZS-Zwischenliste umgekehrt auslesen: }
          for i:=zs_zwlist.Count - 1 downto 0 do begin
            Application.ProcessMessages;
            tag_satz:=TRohTRecExtObj (zs_zwlist.Items [i]).RohTRec;

            { aktuelles Gerätedatum/-zeit für aktuellen Zählerstand updaten (VC2): 16.05.2007, WW }
            if MessKonvRec.bAktZaehler AND (i = 0) AND (MessKonvRec.KonvGruppe = 13) then
              if DateTimeToRec(tag_satz.DatumZeit, datumbuf, zeitbuf) then
                if (datumbuf.Year = C_AktDatumDefault.Year) AND
                   (zeitbuf.Hsec = C_AktZeitDefault.Hsec) then begin
                  tag_satz.DatumZeit:=dtAktDatumZeit;

                  { Gastag-Umrechnung: }
                  if tagesende > -1 then begin
                    with tagesende_rec do begin
                      hour:=tagesende;
                      min:=0;
                      sec:=0;
                      hsec:=0;
                    end;
                    if CmpTime (aktzeit, tagesende_rec) <= 0 then
                      tag_satz.DatumZeit:=tag_satz.DatumZeit - 1;  { Aktuelles Datum ist physikalisch -> auf Gastag umrechnen }
                  end;
                end;

            fs_RohTRec.WriteRec (tag_satz);
          end;  { for }
        finally
          fs_RohTRec.Free;
        end;

        { MW-Zwischenliste in MW-Zwischenfile konvertieren: }

        { für VC2: Datum/Zeit vor dem Messwerte-Abruf aus Parameterliste lesen, falls vorhanden }
        dtDatumZeit_vor_Abruf:=-1;  // Vorbelegung: Datum/Zeit vor Abruf unbekannt
        if MessKonvRec.ParameterListe <> nil then
          if MessKonvRec.ParameterListe.GetValue (CP_FTL_IEC_Datum, S) then
            if EncodeDateStr (S, 'YY-MM-DD', dtBuf_Datum) then
              if MessKonvRec.ParameterListe.GetValue (CP_FTL_IEC_Zeit, S) then
                if EncodeTimeStr (S, 'HH:MM:SS', dtBuf_Zeit) then
                  dtDatumZeit_vor_Abruf:=dtBuf_Datum + dtBuf_Zeit;

        { für VC2: Sicherheits-Abstand zur vollen Stunde (ein Abruf aller Daten dauert ca. 6 min)
          -> für ersatzweise Ermittlung, ob über einen Stundenwechsel hinweg abgerufen
             wurde (die aktuelle Gerätezeit steht (leider) am Ende der Rohdaten !): }
        dtZeitfenster_Stundenwechsel:=EncodeTime (0, 10, 0, 0);

        fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (std_satz));
        try
          { MW-Zwischenliste umgekehrt auslesen bis Auslesezeitpunkt: }
          for i:=mw_zwlist.Count - 1 downto 0 do begin
            Application.ProcessMessages;
            std_satz:=TRohSRecObj (mw_zwlist.Items [i]).RohSRec;

            { Prüfung, wann Schreiben des Zwischenfiles gestoppt werden muß (das
              Gerät liefert immer volle Tage !): }
            if (MessKonvRec.KonvGruppe = 13) then begin  // VC2
              if dtDatumZeit_vor_Abruf > -1 then begin  // 'Datum/Zeit vor Abruf' bekannt
                if std_satz.DatumZeit > dtDatumZeit_vor_Abruf then
                  Break;  { raus, wenn 'Datum/Zeit vor Abruf' überschritten }
              end
              else begin  // 'Datum/Zeit vor Abruf' nicht bekannt
                { ersatzweise Prüfung über aktuelle Gerätezeit aus Messwert-Antwort: }
                if std_satz.DatumZeit > dtAktDatumZeit then
                  Break          { raus, wenn aktuelle Gerätezeit überschritten }
                else begin
                  if ((std_satz.DatumZeit + dtZeitfenster_Stundenwechsel) > dtAktDatumZeit) then begin
                    { VC2: aktuelles Gerätedatum/-zeit liegt NACH den zu konvertierenden Daten !
                      Daher Lastprofil-Kanäle auf Null-Wert prüfen, wenn Gerätezeit innerhalb
                      des Stundenwechsel-Zeitfensters liegt:
                      -> wenn mind. 1 Nullwert im letzten Datensatz der vorhandenen Lastprofilkanäle
                         vorhanden ist, dann sicherheitshalber raus. Nullwerte können nicht von
                         fehlenden Werten unterschieden werden ! }
                    if (((std_satz.KanalOrig [1].KanalStatus AND $80) = 0) AND (Trunc (std_satz.KanalOrig [1].Wert) = 0)) OR
                       (((std_satz.KanalOrig [2].KanalStatus AND $80) = 0) AND (Trunc (std_satz.KanalOrig [2].Wert) = 0)) OR
                       (((std_satz.KanalOrig [5].KanalStatus AND $80) = 0) AND (Trunc (std_satz.KanalOrig [5].Wert) = 0)) then
                      Break;
                  end;
                end;
              end;
            end
            else begin  // TTG
              { Prüfung kann über aktuelles Gerätedatum/-zeit aus Messwert-Antwort erfolgen, da
                Gerätedatum/-zeit VOR den zu konvertierenden Daten liegt ! }
              if std_satz.DatumZeit > dtAktDatumZeit then
                Break;          { raus, wenn aktuelle Gerätezeit überschritten }
            end;

            fs_RohSRec.WriteRec (std_satz);
          end;  { for }
        finally
          fs_RohSRec.Free;
        end;
      except
        Result:=false;
      end;
    finally;
      mw_zwlist.Free;
    end;
  finally
    zs_zwlist.Free;
  end;
end;


{---------------------------------------------------------------------}
function Mess_Konv_Tritschler_TDS (MessKonvRec: TMessTagKonv): Boolean;
{---------------------------------------------------------------------}
{ Konvertierung Tritschler TDS, MCO, MC2, VC3, VCC mit IEC-Protokoll
  -> 1 Rohdatei mit Zählerstandsprofil-Daten aller Kanäle }

  {--------------------------------------------------------------------------}
  function Is_ZSProfilBeginn_Rohsatz (ARohsatz, AGeraeteTyp: string): boolean;
  {--------------------------------------------------------------------------}
  { prüft, ob Rohsatz den Beginn des Zählerstandsprofils kennzeichnet;
    Übergabe: Rohsatz
              Gerätetyp-String
    Ergebnis: true, wenn Rohsatz Beginn des ZS-Profils kennzeichnet }
  begin
    if AGeraeteTyp = 'VCC' then
      Result:=Copy (ARohsatz, 1, 5) = '7.77.'  // 11.01.2016, WW
    else
      Result:=Copy (ARohsatz, 1, 5) = '7.71.';
  end;

  {-----------------------------------------------------------}
  function Is_ZSProfilEnde_Rohsatz (ARohsatz: string): boolean;
  {-----------------------------------------------------------}
  { prüft, ob Rohsatz das Ende des Zählerstandsprofils kennzeichnet;
    Übergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Ende des ZS-Profils kennzeichnet }
  begin
    Result:=Copy (ARohsatz, 1, 1) = ')';
  end;

  {-----------------------------------------------------------------------------}
  function Is_TempKelvin_Kanal (AKanalNr: integer; AGeraeteTyp: string): boolean;
  {-----------------------------------------------------------------------------}
  { Ergebnis: true, wenn Kanal Temperaturen in Kelvin liefert
    Übergabe: Kanalnummer
              Gerätetyp-String }
  begin
    Result:=false;

    if (AGeraeteTyp = 'VC3') OR
       (AGeraeteTyp = 'VCC') then begin  // 11.01.2016, WW
      if AKanalNr = 9 then      
        Result:=true;
    end;

    { Temperatur-Kanal ist in TDS-, MCO-, MC2-Daten nicht enthalten }

  end;

  {---------------------------------------------------------------}
  function GetSatzstatusFromGesamtbitleiste (sGesBitleiste: string;
    AIgnoreSatzstatus: boolean; var Satzstatus_Std: byte;
    var Satzstatus_Tag: byte): boolean;
  {---------------------------------------------------------------}
  { ermittelt Stundensatz- und Tagessatz-Satzstatus aus Gesamtbitleiste;
    Übergabe: Gesamtbitleiste
              Flag 'Satzstatus in Rohdaten ignorieren' ja/nein
    Rückgaben: Stundensatz-Satzstatus
               Tagessatz-Satzstatus
    Ergebnis: true, wenn Satzstati erfolgreich ermittelt werden konnten }
  var
    sLowByte: string;
    b: byte;
    Code: integer;

  begin
    Result:=false;
    Satzstatus_Std:=$00;
    Satzstatus_Tag:=$00;

    if length (sGesBitleiste) <> 4 then exit;
    if not AIgnoreSatzstatus then begin  // 31.10.2011, WW
      { für den Satzstatus interessiert nur das Lowbyte: }
      sLowByte:=Copy (sGesBitleiste, 3, 2);
      Val ('$' + sLowByte, b, Code);
      if Code <> 0 then exit;                                          
      case b of  { das Lowbyte enthält die Meldungsnummer }
        6, 7, 8, 9:
          begin   { Uhr gestellt }
            Satzstatus_Std:=$02;
            Satzstatus_Tag:=$04;
          end;
        14, 15, 16, 17:
          Satzstatus_Tag:=$02;  { Netzausfall }
        20, 21:
          Satzstatus_Std:=$08;  { Revision }
      end;
    end;
    Result:=true;
  end;

Const
  CMaxTDS_Kanaele = 11;  // erhöht von 4 auf 10 für VC3; 17.10.2011, WW
                         // erhöht von 10 auf 11 für VCC; 11.01.2016, WW
  CTrenner = ';';
  CKorrSec_MP = 5;  // Max. Abweichung in s, sodaß Zeitstempel noch als volle Messperiode
                    // betrachtet wird; 01.07.2009, WW

type
  { Struktur zum Merken eines Kanalwerts für Stundenwert-Differenzbildung }
  Tletzt_daten = record
    Datumzeit: TDateTime;
    Wert: double;
    Wert_vorhanden: boolean;
  end;

var
  fs_roh: TFileOfCharStream;     { Rohdatendatei }  // 06.10.2015, WW
  fs_RohSRec: TFileOfRecStream;  { Stundenwerte-Datei in RohSRec-Struktur }
  fs_RohTRec: TFileOfRecStream;  { Tagessätze-Datei in RohTRec-Struktur }
  FSize_roh: integer;
  rohsatz: string;
  zeichen: char;
  s: string;
  zs_phys: double;  // 21.09.2009, WW (ZS-Rohdaten mit Nachkommastellen !)
  code: integer;
  tag_satz: RohTRec;
  std_satz: RohSRec;
  i: integer;
  Sta_Faktor: double;
  dtBuf: TDateTime;
  Rohfilename: string;
  Satz_OK: boolean;
  S1: string;
  zsstr: string;
  k: integer;
  Hour, Min, Sec, MSec: word;
  letzt_daten_k: array [1..CMaxTDS_Kanaele] of Tletzt_daten;
  Diff: double;
  l: Int64;
  bZSProfilBeginn_gelesen: boolean;
  zs_zwlist: TObjectList;
  mw_zwlist: TObjectList;
  RohTRecExtObj: TRohTRecExtObj;
  RohSRecObj: TRohSRecObj;
  DatumZeit_volleMP: TDateTime;
  year, month, day: word;
  DiffSec: longint;
  mw_phys: double;
  mw_lgz: Int64;
  Sta_Delta: double;
  bIgnoreStati: boolean;
  sGerTyp: string;
  sGerVersion: string;
  iVsMain: integer;
  iVsSub: integer;
  f: integer;
  iOrdNr: longint;
  iBuf: longint;

begin
  Result := True;

  zs_zwlist:=TObjectList.Create;           { Zwischenliste für Zählerstände }
  try
    mw_zwlist:=TObjectList.Create;           { Zwischenliste für Messwerte }
    try

{--------------- Konvertierung Rohdatei -> Zwischenlisten ---------------------}

      try
        { alle Messwert-Rohdateien abarbeiten:  31.01.2012, WW
            -> wegen Import (Rohdateien ohne verwertbaren Inhalt werden gelöscht)
            -> per DFÜ wird nur 1 Rohdatei verarbeitet }
        for f:=0 to MessKonvRec.StdQuellDateiNameListe.Count-1 do begin
          Rohfilename:=MessKonvRec.StdQuellDateiNameListe [f];    { Rohfilename }
          if length (Rohfilename) = 0 then Continue;
          if not FileExists (Rohfilename) then begin
            Result := False;
            Continue;
          end;

          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;

            rohsatz:='';
            zeichen:=NUL;
            bIgnoreStati:=false;  // Vorbelegung: in Rohdaten enthaltene Satz- und Kanalstati verarbeiten
            { Rohfileheader bis zum STX lesen }
            while (zeichen <> STX) AND (fs_roh.Position < FSize_roh) do begin
              fs_roh.Read (zeichen);
              rohsatz:=rohsatz + zeichen;                    { Zeile bilden }
            end;

            { Gerätetyp und -version: 31.10.2011, WW }
            S:=ExtractString (rohsatz, '/', CR, 0);
            sGerTyp:=Copy (S, 5, 3);
            if Copy (S, 1, 3) = 'FTL' then begin  // Hersteller-Kürzel
              if (sGerTyp = 'VC3') OR
                 (sGerTyp = 'VCC') then begin  // 11.01.2016, WW
                sGerVersion:=Copy (S, 8, 5);  // VC3/VCC-Version
                sGerVersion:=F_LeftTrunc (sGerVersion, ' ');
                S:=F_Zerlegen (sGerVersion, '.');
                Val (S, iVsMain, Code);
                if Code = 0 then begin
                  Val (sGerVersion, iVsSub, Code);
                  if Code = 0 then begin
                    { Im VC3 und VCC sind die in den Rohdaten enthaltenen Satz- und Kanalstati
                      erst ab Version 7.40 nutzbar: }
                    if not (((iVsMain = 7) AND (iVsSub >= 40)) OR (iVsMain > 7)) then
                      bIgnoreStati:=true;
                  end;
                end;
              end;
            end;

            { Konvertierung }
            bZSProfilBeginn_gelesen:=false;   { Vorbelegung: Beginn des Zählerstandsprofils noch nicht gelesen }
            while (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
              rohsatz:='';
              zeichen:=NUL;
              while (zeichen <> LF) AND (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
              end;

              Application.ProcessMessages;

              rohsatz:=ExtractString (rohsatz, NUL, CR, 0);  { alles ab einschließlich CR wegschneiden; 14.10.2009, WW }
              if not bZSProfilBeginn_gelesen then  { Beginn des Zählerstandsprofils noch nicht gelesen }
                { prüfen auf Beginn des Zählerstandsprofils: }
                bZSProfilBeginn_gelesen:=Is_ZSProfilBeginn_Rohsatz (rohsatz, sGerTyp)
              else begin
                { prüfen auf Ende des Zählerstandsprofils: }
                if Is_ZSProfilEnde_Rohsatz (rohsatz) then
                  Break;

                { Zählerstandsprofil-Datensatz konvertieren in Stundenwert- und
                  Tagessatz-Zwischendatei: }
                if length (rohsatz) > 0 then begin
                  FillChar (tag_satz, SizeOf (tag_satz), 0);       { Vorbelegung tag_satz }
                  with tag_satz do begin
                    satzstatus:= $00;  { Satzstatus fest: OK }

                    { Vorbelegung Zählerstati für fehlend }
                    for i:=1 to c_maxKanalZahl do begin
                      E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                      E_zaehler[i].wert:=0;
                      K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                      K_zaehler[i].wert:=0;
                    end;
                  end;  { with tag_satz }

                  FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
                  with std_satz do begin
                    satzstatus:= $00;  { Satzstatus: OK }

                    { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                    for i:=1 to c_maxKanalZahl do begin
                      kanal[i].wert:=0;
                      KanalOrig[i].wert:=0;
                      kanal[i].kanalstatus:=$80;  { nur Impulskanäle }
                      KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                      KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                    end;

                    Satz_OK:=true;
                    iOrdNr:=-1;  { Default-Ordnungsnummer; 28.09.2020, WW }
                    i:=0;
                    while (length (rohsatz) > 0) AND Satz_OK do begin
                      S:=F_Zerlegen (rohsatz, CTrenner);
                      if length (S) > 0 then begin
                        inc (i);  // Zähler für Teilsätze erhöhen
                        if i = 1 then begin  { 1. Teilsatz mit Zeitstempel, Gesamtbitleiste, Statusflag, Ordnungsnummer }
                          S1:=Copy (S, 1, 6);  { Datum }
                          if EncodeDateStr (S1, 'YYMMDD', dtBuf) then
                            DatumZeit:=dtBuf
                          else
                            Satz_OK:=false;

                          S1:=Copy (S, 7, 6);  { Zeit }
                          if EncodeTimeStr (S1, 'HHMMSS', dtBuf) then
                            DatumZeit:=DatumZeit + dtBuf
                          else
                            Satz_OK:=false;

                          S1:=Copy (S, 13, 3);  { UTC-Zeitzonen-Differenz }
                          if UTCTimezoneStrToDateTime (S1, dtBuf) then
                            DatumZeit:=DatumZeit + dtBuf
                          else
                            Satz_OK:=false;

                          S1:=Copy (S, 21, length (S));  { Ordnungsnummer; 28.09.2020, WW }
                          if length (S1) > 0 then begin
                            Val (S1, iBuf, Code);
                            if Code = 0 then
                              iOrdNr:=iBuf;
                          end;

                          { Datum/Stunden in tag_satz belegen: }
                          tag_satz.DatumZeit:=DatumZeit;

                          S1:=Copy (S, 16, 4);  { Gesamtbitleiste }
                          { Satzstatus für Stundensatz und Tagessatz: }
                          if not GetSatzstatusFromGesamtbitleiste (S1, bIgnoreStati,
                                                                   satzstatus,
                                                                   tag_satz.satzstatus) then
                            Satz_OK:=false;
                        end
                        else begin  { alle weiteren Teilsätze: Kanaldaten }
                          k:=i - 1;  { Kanalnummer }

                          S1:=Copy (S, 1, 4);  { kanalbezogene Bitleiste }
                          if length (S1) = 4 then begin
                            S1:=Copy (S, 1, 1);  { Highbyte der kanalbezogenen Bitleiste }
                            if (S1 <> '8') OR bIgnoreStati then begin  { Wert vorhanden (Kanal nicht deaktiviert)
                                                                         nur 1. Zeichen prüfen wg. VC3/VCC; 17.10.2011 WW }
                              zsstr:=Copy (S, 5, length (S) - 5);  { ohne Bitleiste und Kennbuchstabe für Einheit }
                              if length (zsstr) > 0 then begin
                                Val (zsstr, zs_phys, Code);  { Zählerstand (physikalisch !) }
                                if (k >= 1) AND (k <= c_maxKanalZahl) then begin
                                  { in Stundensatz-Record hier nur die Original-Rohwerte
                                    (Zählerstände, Analogwerte) eintragen. Die Rückrechnung auf normierte
                                    Zählerstandsdifferenzen und Analogwerte erfolgt erst beim Schreiben in die Zwischendatei: }
                                  KanalOrig [k].Wert:=zs_phys;  { Original-Rohwert }
                                  KanalOrig [k].KanalStatus:=KanalOrig [k].KanalStatus and $7F;  { Fehlend-Bit löschen }
                                  KanalOrig [k].OrdNr := iOrdNr; { Ordnungsnummer zuweisen; 28.09.2020, WW }

                                  { in Tagessatz-Record physikalische Zählerstände eintragen: }
                                  tag_satz.E_zaehler[k].wert:=zs_phys;  // Rundung raus; 27.03.2013, WW
                                  tag_satz.E_zaehler[k].zaehlerstatus:=
                                    tag_satz.E_zaehler[k].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                                end;
                              end else  { if length (zsstr) > 0 }
                                Satz_OK:=false;
                            end;  { if S1 <> '80' }
                          end else  { if length (S) }
                            Satz_OK:=false;
                        end;
                      end;  { if length (S) > 0 }
                    end;  { while length (rohsatz) > 0 }
                  end;  { with std_satz }

                  if Satz_OK then begin
                    { Stundensatz-Objekt in MW-Zwischenliste anhängen: }
                    RohSRecObj:=TRohSRecObj.Create (std_satz);
                    mw_zwlist.Add (RohSRecObj);
                    { Tagessatz-Objekt in ZS-Zwischenliste anhängen: }
                    RohTRecExtObj:=TRohTRecExtObj.Create (tag_satz, -1);
                    zs_zwlist.Add (RohTRecExtObj);
                  end;
                end;  { if length (rohsatz) > 0 }
              end;  { if not bZSProfilBeginn_gelesen }
            end;  { while (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) }
          finally
            fs_roh.Free;           { Rohfile schließen }
          end;

          if MessKonvRec.loeschen then
            DeleteFile (Rohfilename);  { Rohfile löschen }
        end;  { for f }


{-------------- Konvertierung Zwischenlisten -> Zwischendateien ---------------}

        { ZS-Zwischenliste in ZS-Zwischenfile konvertieren; Zählerstände sind zeitlich
          absteigend enthalten: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          for i:=zs_zwlist.Count - 1 downto 0 do begin
            Application.ProcessMessages;
            tag_satz:=TRohTRecExtObj (zs_zwlist.Items [i]).RohTRec;
            DecodeTime (tag_satz.DatumZeit, hour, min, sec, msec);
            if hour <= MessKonvRec.TagesEnde then
              tag_satz.DatumZeit:=tag_satz.DatumZeit - 1;  { Zählerstand für Gastag }
            { Tagessatz nur in Zwischendatei schreiben, wenn vom Tagesende: }
            if (hour = MessKonvRec.Tagesende) AND (min = 0) AND (sec = 0) then
              fs_RohTRec.WriteRec (tag_satz);
          end;  { for }
        finally
          fs_RohTRec.Free;
        end;

        { MW-Zwischenliste in MW-Zwischenfile konvertieren; Messwerte sind zeitlich
          absteigend enthalten: }
        fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (std_satz));
        try
          { Vorbelegung von Datum/Zeit und Wert der Vorstunden-Kanalwerte für
            Zählerstands-Differenzbildung: Vorstundenwert unbekannt }
          for i:=Low (letzt_daten_k) to High (letzt_daten_k) do begin
            letzt_daten_k [i].Datumzeit:= 0;
            letzt_daten_k [i].Wert:=0;
            letzt_daten_k [i].Wert_vorhanden:=false;
          end;

          for i:=mw_zwlist.Count - 1 downto 0 do begin
            Application.ProcessMessages;
            std_satz:=TRohSRecObj (mw_zwlist.Items [i]).RohSRec;

            { für LGZ-Daten: vom Gerät gelieferte physikalische Zählerstände
              rückrechnen auf normierte Zählerstandsdifferenzen }
            if MessKonvRec.KanalList <> nil then begin
              with std_satz do begin
                DecodeTime (DatumZeit, hour, min, sec, msec);
                { nur volle Stunden interessieren ! }
                if (min = 0) then begin
                  if (sec = 0) OR
                     (sec <= CKorrSec_MP) then begin  { wenn Sekunden-Abweichung toleriert wird; 01.07.2009  WW }
                    // Vergleichs-Zeitstempel: volle Stunde }
                    DecodeDate (DatumZeit, year, month, day);
                    DatumZeit_volleMP:=EncodeDateTime (year, month, day, hour, 0, 0, 0);

                    for k:=Low (letzt_daten_k) to high (letzt_daten_k) do begin
                      if letzt_daten_k [k].Wert_vorhanden AND     { Differenzbildung nur möglich, wenn Vor-Zählerstand... }
                         ((KanalOrig [k].KanalStatus AND $80) = 0) then begin    { und aktueller Zählerstand nicht fehlend; 26.08.2008, WW }
                        dtBuf:=letzt_daten_k [k].Datumzeit;
                        dtBuf:=dtBuf + EncodeTime (1, 0, 0, 0);
                        F_TimeDiff (dtBuf, DatumZeit, DiffSec);
                        { Vor-Zählerstand muß von Vorstunde plus/minus Toleranz-Sekunden sein: 01.07.2009, WW }
                        if Abs (DiffSec) <= CKorrSec_MP then begin
                          if (k >= MessKonvRec.Analogkanal_von) AND (k <= MessKonvRec.Analogkanal_bis) then begin { Analogkanal }
                            mw_phys:=KanalOrig [k].Wert;
                            if Is_TempKelvin_Kanal (k, sGerTyp) then
                              mw_phys:=mw_phys - C_To_Kelvin;  { Temperatur in Kelvin -> Umrechnung in °C }
                            { physikalischen Analogwert rückrechnen auf LGZ-Rohwert über obere und untere Meßbereichgrenze aus
                              Stammdaten: }
                            if MessKonvRec.KanalList <> nil then begin
                              Sta_Delta:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (k)).Daten.MessBereichMax -
                                         TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (k)).Daten.MessBereichMin;
                              if Sta_Delta <> 0 then begin
                                mw_lgz:=Round ((mw_phys - TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (k)).Daten.MessBereichMin) /
                                               Sta_Delta * 10000);
                                if mw_lgz >= 0 then begin
                                  if mw_lgz > 9999 then
                                    Kanal [k].Wert:=9999
                                    { Fehlend-Bit hier nicht zurücksetzen, um Analogwert-"Überlauf"
                                      behelfsmäßig zu kennzeichnen }
                                  else begin
                                    Kanal [k].Wert:=mw_lgz;
                                    Kanal [k].KanalStatus:=Kanal [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                                  end;
                                end;
                              end;
                            end;
                          end
                          else begin
                            { physikalischen Impulswert rückrechnen auf LGZ-Rohwert über
                              OrgFaktor aus Stammdaten: }
                            Sta_Faktor:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (k)).Daten.OrgFaktor;  //!! bei GPRS-Daten brauchen wir in .Kanal.Wert die unnormierten ZS-Differenzen (Sta_Faktor = 1 !?)
                            if Sta_Faktor > 0 then begin
                              Diff:=KanalOrig [k].Wert - letzt_daten_k [k].Wert;   { Zählerstands-Differenz }
                              l:=Round (Diff / Sta_Faktor);
                              if l >= 0 then begin
                                if l > High (Kanal [k].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                                  Kanal [k].Wert:=High (Kanal [k].Wert);
                                  Kanal [k].KanalStatus := Kanal [k].KanalStatus or $01;  { Überlauf-Bit setzen }
                                end else
                                  Kanal [k].Wert:=l;
                                Kanal [k].KanalStatus := Kanal [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                              end;
                            end;
                          end;
                        end;
                      end;

                      { Wert mit Datum/Zeit merken: }
                      letzt_daten_k [k].Datumzeit:=DatumZeit_volleMP;
                      letzt_daten_k [k].Wert:=KanalOrig [k].Wert;
                      letzt_daten_k [k].Wert_vorhanden:=(KanalOrig [k].KanalStatus AND $80) = 0;
                    end;  { for k }
                  end;
                end;
              end;  { with std_satz }
            end;

            fs_RohSRec.WriteRec (std_satz);
          end;  { for }
        finally
          fs_RohSRec.Free;
        end;
      except
        Result:=false;
      end;
    finally
      mw_zwlist.Free;
    end;
  finally
    zs_zwlist.Free;
  end;
end;


{----------------------------------------------------------------------}
function Mess_Konv_Tritschler_FTL (MessKonvRec: TMessTagKonv): shortint;
{----------------------------------------------------------------------}
{ Konvertierung Tritschler FTL-Protokoll für TTG
  -> konvertiert Antworten auf Befehle: F0001 (Datentelegramm 1/Kanal 1 -> Zählerstände)
                                        F0002 (Datentelegramm 2/Kanal 1 -> Messwerte)
                                        F0011 (Datentelegramm 1/Kanal 2 -> Zählerstände)
                                        F0012 (Datentelegramm 2/Kanal 2 -> Messwerte)
  -> von der Konvertierung erwarteter Aufbau der Rohdaten:
     zusammengefasste Teil-Antworten ohne K23-Blöcke (Blocksumme) und CR }

  {----------------------------------------------------------}
  function Konv_FTL_ZS (var AktDatumZeit: TDateTime): integer;
  {----------------------------------------------------------}
  { FTL-Rohdaten konvertieren (Zählerstände)
    Rückgabe: aktuelles Gerätedatum/-zeit }
  const
    CLenZS_Satz = 24;  { Länge eines ZS-Satzes mit Rückstellnummer, Datum, Zeit, Spaces, ZS-Wert }

  var
    zs_zwlist: TObjectList;
    l: integer;
    Rohfilename: string;
    KanalNrStr: string;
    fs_roh: TFileOfCharStream;  // 06.10.2015, WW
    FSize_roh: integer;
    KanalNr: integer;
    rohsatz: string;
    zeichen: char;
    S, S1: string;
    dtBuf: TDateTime;
    RueckstellNr: integer;
    Code: integer;
    ZS: integer;
    zs_list_pos: integer;
    dtDatumZeit: TDateTime;
    Satz_OK: boolean;
    RohTRecExtObj: TRohTRecExtObj;
    gefunden: boolean;
    tag_satz: RohTRec;
    i: integer;
    fs_RohTRec: TFileOfRecStream;       { Tagessätze-Datei in RohTRec-Struktur }
    hour, min, sec, msec: word;
    sKennziffer: string;

  begin
    { Vorbelegungen für Ergebnis, Rückgabe }
    Result:=0;
    AktDatumZeit:=0;

    zs_zwlist:=TObjectList.Create;           { Zwischenliste für Zählerstände }
    try
      {----- Konvertierung Tagessatz-Rohdateien -> Tagessatz-Zwischenliste ----}
      try
        for l:=0 to MessKonvRec.TagQuellDateiNameListe.Count-1 do begin  { alle Tagessatz-Rohdateien abarbeiten }
          Rohfilename:=MessKonvRec.TagQuellDateiNameListe [l];
          if not FileExists (Rohfilename) then begin
            Result:=-1;
            Continue;
          end;

          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;
            zs_list_pos:=0;               { Vorbelegung Tagsatz-Listenposition }
            KanalNr:=-1;      { Vorbelegung: Kanalnummer nicht gelesen }

            while fs_roh.Position < FSize_roh do begin
              rohsatz:='K';
              zeichen:=NUL;
              { Datensätze bilden: bis K lesen }
              while (zeichen <> 'K') AND (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                if zeichen <> 'K' then
                  rohsatz:=rohsatz + zeichen;   { Datensatz bilden }
              end;

              Application.ProcessMessages;
              sKennziffer:=Copy (rohsatz, 1, 3);  { Knn }

              { aktuelles Gerätedatum/-zeit für Meßwertkonvertierung ermitteln (Rückgabe) }
              if l = 0 then begin                                { nur einmal }
                { Datum (K02-Block) }
                if sKennziffer = 'K02' then begin
                  S:=Copy (rohsatz, 7, 6);
                  if not EncodeDateStr (S, 'DDMMYY', dtBuf) then begin
                    Result:=-2;
                    exit;
                  end;
                  AktDatumZeit:=AktDatumZeit + dtBuf;
                end

                { Zeit (K42-Block) }
                else if sKennziffer = 'K42' then begin
                  S:=Copy (rohsatz, 7, 6);
                  if not EncodeTimeStr (S, 'HHMMSS', dtBuf) then begin
                    Result:=-3;
                    exit;
                  end;
                  AktDatumZeit:=AktDatumZeit + dtBuf;
                end;
              end;  { if l = 0 }

              { Funktionsnummer (K85-Block) }
              if sKennziffer = 'K85' then begin
                KanalNrStr:=Copy (rohsatz, 7, 1);  { Kanal steht an 3. Stelle der Funktionsnummer }
                Val (KanalNrStr, KanalNr, Code);
                if Code <> 0 then begin
                  Result:=-4;
                  exit;
                end;
                KanalNr:=KanalNr + 1;  { 0 = Kanal 1; 1 = Kanal 2 }
              end

              { Zählerstände (K43-Block) }
              else if sKennziffer = 'K43' then begin
                if KanalNr < 0 then begin  { Kanalnummer wurde nicht gelesen }
                  Result:=-5;
                  exit;
                end;

                S:=Copy (rohsatz, 13, length (rohsatz));   { K43 und 9 Leerzeichen wegschneiden }
                while length (S) >= CLenZS_Satz do begin
                  Satz_OK:=true;
                  dtDatumZeit:=0;

                  S1:=Copy (S, 1, 2);  { Rückstellnummer }
                  Val (S1, RueckstellNr, Code);
                  if Code <> 0 then
                    Satz_OK:=false;

                  S1:=Copy (S, 3, 6);  { Datumstempel }
                  if EncodeDateStr (S1, 'DDMMYY', dtBuf) then
                    dtDatumZeit:=dtBuf
                  else
                    Satz_OK:=false;

                  S1:=Copy (S, 9, 4);  { Zeitstempel }
                  if EncodeTimeStr (S1 + '00', 'HHMMSS', dtBuf) then
                    dtDatumZeit:=dtDatumZeit + dtBuf
                  else
                    Satz_OK:=false;

                  S1:=Copy (S, 17, 8);  { Wert }
                  Val (S1, ZS, Code);
                  if Code <> 0 then
                    Satz_OK:=false;

                  RohTRecExtObj:=nil;
                  if Satz_OK then begin
                    gefunden:=Search_ZS_ZwList_DatumZeit (zs_zwList, zs_list_pos, dtDatumZeit, true);  // Rohdaten/Liste zeitlich absteigend
                    if gefunden then begin
                      { Eintrag für Datum, Zeit in ZS-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
                      if (zs_list_pos >= 0) AND (zs_list_pos < zs_zwlist.Count) then
                        RohTRecExtObj:=TRohTRecExtObj (zs_zwlist.Items [zs_list_pos]);
                    end
                    else begin
                      { nicht gefunden: neues ZS-Zwischenlisten-Objekt erzeugen }
                      FillChar (tag_satz, SizeOf (tag_satz), 0);   { Vorbelegung tag_satz }
                      with tag_satz do begin
                        satzstatus:= $00;  { Satzstatus fest: OK }

                        { Vorbelegung Kanalstatus für fehlend }
                        for i:=1 to c_maxKanalZahl do begin
                          E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                          E_zaehler[i].wert:=0;
                          K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                          K_zaehler[i].wert:=0;
                        end;
                      end;  { with tag_satz }

                      tag_satz.DatumZeit:=dtDatumZeit;
                      RohTRecExtObj:=TRohTRecExtObj.Create (tag_satz, -1);
                    end;

                    if Assigned (RohTRecExtObj) then begin
                      { Kanaldaten setzen: }
                      with RohTRecExtObj.RohTRec do begin
                        E_zaehler[KanalNr].wert:=ZS;
                        E_zaehler[KanalNr].zaehlerstatus:=
                          E_zaehler[KanalNr].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                      end;

                      { nicht gefunden: Objekt-Eintrag in ZS-Zwischenliste an zs_list_pos eintragen }
                      if not gefunden then
                        zs_zwlist.insert (zs_list_pos, RohTRecExtObj);
                    end;
                  end;  { if Satz_OK }
                  inc (zs_list_pos);

                  if RueckstellNr = 0 then  { nach Rückstellnummer 0 raus, keine weiteren Werte mehr }
                    Break;

                  Delete (S, 1, CLenZS_Satz);  { konvertierten Teil wegschneiden }
                end;  { while length (S) }
              end;  { if K43 }
            end;   { while fs_roh.Position < FSize_roh }
          finally
            fs_roh.Free;
          end;

          if MessKonvRec.loeschen then
            DeleteFile (Rohfilename);  { Rohfile löschen }
        end;  { for l }

        {-- Konvertierung Tagessatz-Zwischenliste -> Tagessatz-Zwischendatei --}
        { -> Zählerstände sind zeitlich absteigend enthalten: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          for i:=zs_zwlist.Count - 1 downto 0 do begin
            Application.ProcessMessages;
            tag_satz:=TRohTRecExtObj (zs_zwlist.Items [i]).RohTRec;
            DecodeTime (tag_satz.DatumZeit, hour, min, sec, msec);
            if hour <= MessKonvRec.TagesEnde then
              tag_satz.DatumZeit:=tag_satz.DatumZeit - 1;  { Zählerstand für Gastag }
            fs_RohTRec.WriteRec (tag_satz);
          end;  { for }
        finally
          fs_RohTRec.Free;
        end;
      except
        Result:=-10;
      end;
    finally
      zs_zwlist.Free;
    end;
  end;    { Konv_FTL_ZS }


  {------------------------------------------------------}
  function Konv_FTL_MW (AktDatumZeit: TDateTime): integer;
  {------------------------------------------------------}
  { FTL-Rohdaten konvertieren (Messwerte)
    Übergabe: aktuelles Gerätedatum/-zeit }
  const
    CLenMW_Satz = 6;  { Länge eines MW-Satzes mit Space, Messwert }

  var
    mw_zwlist: TObjectList;
    l: integer;
    Rohfilename: string;
    KanalNrStr: string;
    fs_roh: TFileOfCharStream;  // 06.10.2015, WW
    FSize_roh: integer;
    KanalNr: integer;
    rohsatz: string;
    zeichen: char;
    S, S1: string;
    Code: integer;
    Mw: Int64;
    mw_list_pos: integer;
    dtDatumZeit: TDateTime;
    Satz_OK: boolean;
    RohSRecObj: TRohSRecObj;
    gefunden: boolean;
    std_satz: RohSRec;
    i: integer;
    fs_RohSRec: TFileOfRecStream;       { Stundensätze-Datei in RohSRec-Struktur }
    MwPos: integer;
    WerteProTag: integer;
    Datum_OK: boolean;
    sKennziffer: string;

  begin
    { Vorbelegungen für Ergebnis }
    Result:=0;

    mw_zwlist:=TObjectList.Create;           { Zwischenliste für Messwerte }
    try
      {----- Konvertierung Messwerte-Rohdateien -> Messwerte-Zwischenliste ----}
      try
        for l:=0 to MessKonvRec.StdQuellDateiNameListe.Count-1 do begin  { alle Stundensatz-Rohdateien abarbeiten }
          Rohfilename:=MessKonvRec.StdQuellDateiNameListe [l];
          if not FileExists (Rohfilename) then begin
            Result:=-21;
            Continue;
          end;

          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;
            mw_list_pos:=0;               { Vorbelegung Messwert-Listenposition }
            KanalNr:=-1;      { Vorbelegung: Kanalnummer nicht gelesen }
            WerteProTag:=-1;  { Vorbelegung: Anzahl der Messwerte pro Tag nicht gelesen }
            Datum_OK:=false;  { Vorbelegung: Datum der Messwerte nicht gelesen }

            while fs_roh.Position < FSize_roh do begin
              rohsatz:='K';
              zeichen:=NUL;
              { Datensätze bilden: bis K lesen }
              while (zeichen <> 'K') AND (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                if zeichen <> 'K' then
                  rohsatz:=rohsatz + zeichen;   { Datensatz bilden }
              end;

              Application.ProcessMessages;
              sKennziffer:=Copy (rohsatz, 1, 3);  { Knn }

              { Funktionsnummer (K85-Block) }
              if sKennziffer = 'K85' then begin
                KanalNrStr:=Copy (rohsatz, 7, 1);  { Kanal steht an 3. Stelle der Funktionsnummer }
                Val (KanalNrStr, KanalNr, Code);
                if Code <> 0 then begin
                  Result:=-22;
                  exit;
                end;
                KanalNr:=KanalNr + 1;  { 0 = Kanal 1; 1 = Kanal 2 }
              end

              { Anzahl der Werte pro Tag (XXXX im K18-Block) }
              else if sKennziffer = 'K18' then begin
                if Copy (rohsatz, 4, 4) = '    ' then  { Vs. 4.2 bis 4.7: Format K18   XXXXDDDD }
                  S:=Copy (rohsatz, 8, 4)
                else  { übrige Versionen: Format K18YXXXXDDDD }
                  S:=Copy (rohsatz, 5, 4);

                Val (S, WerteProTag, Code);
                if Code <> 0 then begin
                  Result:=-23;
                  exit;
                end;
              end

              { Messwerte (K15-Block) }
              else if sKennziffer = 'K15' then begin
                if KanalNr < 0 then begin  { Kanalnummer wurde nicht gelesen }
                  Result:=-24;
                  exit;
                end;

                if WerteProTag < 0 then begin
                  Result:=-25;  { Anzahl der Werte pro Tag wurde nicht gelesen }
                  exit;
                end;

                { Prüfen, ob Datumstempel enthalten ist: }
                S:=Copy (rohsatz, 7, 6);
                if S <> '      ' then begin  { Datumstempel enthalten -> neuer Tag }
                  Datum_OK:=EncodeDateStr (S, 'DDMMYY', dtDatumZeit);
                  { der erste Wert (0 Uhr) gehört schon zum nächsten Tag }
                  if Datum_OK then
                    dtDatumZeit:=dtDatumZeit + 1;
                end;

                S:=Copy (rohsatz, 13, length (rohsatz));  { ab Beginn der Messwerte }
                if Datum_OK then begin
                  while length (S) >= CLenMW_Satz do begin  { Rohsatz rückwärts lesen (0,23..1 Uhr) }
                    MwPos:=length (S) - CLenMW_Satz + 1;  { Positon des letzten Werts im Rohsatz }
                    S1:=Copy (S, MwPos+1, 5);  { der letzte Wert im Rohsatz }
                    Val (S1, Mw, Code);  { Rohwert: unbewertete Impulse; 21.01.2009, WW }
                    Satz_OK:=Code = 0;

                    RohSRecObj:=nil;
                    if Satz_OK then begin
                      gefunden:=Search_MW_ZwList_DatumZeit (mw_zwlist, mw_list_pos, dtDatumZeit, true);  // Rohdaten/Liste zeitlich absteigend
                      if gefunden then begin
                        { Eintrag für Datum, Zeit in MW-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
                        if (mw_list_pos >= 0) AND (mw_list_pos < mw_zwlist.Count) then
                          RohSRecObj:=TRohSRecObj (mw_zwlist.Items [mw_list_pos]);
                      end
                      else begin
                        { nicht gefunden: neues MW-Zwischenlisten-Objekt erzeugen }
                        FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
                        with std_satz do begin
                          satzstatus:= $00;  { Satzstatus fest: OK }

                          { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                          for i:=1 to c_maxKanalZahl do begin
                            kanal[i].wert:=0;
                            KanalOrig[i].wert:=0;
                            kanal[i].kanalstatus:=$80;          { nur Impulskanäle }
                            KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                            KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                          end;
                          DatumZeit:=dtDatumZeit;
                        end;  { with std_satz }
                        RohSRecObj:=TRohSRecObj.Create (std_satz);
                      end;

                      if Assigned (RohSRecObj) then begin
                        { Kanaldaten setzen: }
                        with RohSRecObj.RohSRec do begin
                          KanalOrig [KanalNr].Wert:=Mw;  { Original-Rohwert }
                          KanalOrig [KanalNr].KanalStatus:=KanalOrig [KanalNr].KanalStatus and $7F;       { Fehlend-Bit löschen }

                          if Mw > High (Kanal [KanalNr].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                            Kanal [KanalNr].Wert:=High (Kanal [KanalNr].Wert);
                            Kanal [KanalNr].KanalStatus:=Kanal [KanalNr].KanalStatus or $01;        { Überlauf-Bit setzen }
                          end else
                            Kanal [KanalNr].Wert:=Mw;
                          Kanal [KanalNr].KanalStatus := Kanal [KanalNr].KanalStatus and $7F;       { Fehlend-Bit löschen }
                        end;  { RohSRecObj.RohSRec }

                        { nicht gefunden: Objekt-Eintrag in MW-Zwischenliste an mw_list_pos eintragen }
                        if not gefunden then
                          mw_zwlist.insert (mw_list_pos, RohSRecObj);
                      end;
                    end;  { if Satz_OK }
                    inc (mw_list_pos);

                    Delete (S, MwPos, length (S));  { konvertierten Teil am Ende wegschneiden }
                    dtDatumZeit:=dtDatumZeit - (1/WerteProTag);  { Zeitstempel für den Wert davor }
                  end;  { while length (S) }
                end; { if EncodeDateStr }
              end;  { if K15 }
            end;   { while fs_roh.Position < FSize_roh }
          finally
            fs_roh.Free;
          end;

          if MessKonvRec.loeschen then
            DeleteFile (Rohfilename);  { Rohfile löschen }
        end;  { for l }

        {-- Konvertierung Messwerte-Zwischenliste -> Messwerte-Zwischendatei --}
        { -> Messwerte sind zeitlich absteigend enthalten }
        { -> Zwischenfile auslesen bis zum übergebenen Auslesezeitpunkt (das Gerät
             liefert immer volle Tage !) }
        fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (std_satz));
        try
          for i:=mw_zwlist.Count - 1 downto 0 do begin
            Application.ProcessMessages;
            std_satz:=TRohSRecObj (mw_zwlist.Items [i]).RohSRec;
            if CmpDateTime (std_satz.DatumZeit, AktDatumZeit) > 0 then
              Break;  { raus, wenn aktuelle Gerätezeit überschritten ist }

            fs_RohSRec.WriteRec (std_satz);
          end;  { for }
        finally
          fs_RohSRec.Free;
        end;
      except
        Result:=-30;
      end;
    finally
      mw_zwlist.Free;
    end;
  end;    { Konv_FTL_MW }

var
  AktDatumZeit: TDateTime;

begin
  { zuerst Tagessatzfiles konvertieren -> liefert aktuelles Gerätedatum/-zeit zurück }
  Result:=Konv_FTL_ZS (AktDatumZeit);
  if Result <> 0 then exit;  { Fehler bei Zählerstandskonvertierung aufgetreten }
  { Stundensatzfiles konvertieren }
  Result:=Konv_FTL_MW (AktDatumZeit);
end;


{--------------------- KE-Anlage (Bopp & Reuther) -----------------------------}

{-----------------------------------------------------------------}
function Mess_Konv_KE_Ruhrgas (MessKonvRec: TMessTagKonv): Boolean;
{-----------------------------------------------------------------}
{ Konvertierung von Daten aus KE-Anlage (Ruhrgastyp-Rohdaten, abgerufen mit S026-Befehl) }

type
  { KE-Datentypen }
  TKE_Wert = (ke_W1, ke_W2, ke_VN1, ke_VN2,
              ke_Rhon, ke_Rhob, ke_Druck, ke_Temp, ke_CO2, ke_HO);

  {---------------------------------------------}
  procedure Init_RohSRec (var std_satz: RohSRec);
  {---------------------------------------------}
  { Record für Zwischenfile mit RohSRec-Struktur vorbelegen }
  var
    i: integer;
  begin
    FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
    with std_satz do begin
      satzstatus:= $00;  { Satzstatus fest: OK }

      { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
      for i:=1 to c_maxKanalZahl do begin
        kanal[i].wert:=0;
        KanalOrig[i].wert:=0;
        if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then  { Analogkanal }
          kanal[i].kanalstatus:=$A0
        else                                  { Impulskanal }
          kanal[i].kanalstatus:=$80;
        KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
        KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
      end;
    end;  { with std_satz }
  end;

  {--------------------------------------------------------------------------------}
  function Get_WieserKanalnummer (AMS: byte; AHV: byte; KE_Wert: TKE_Wert): integer;
  {--------------------------------------------------------------------------------}
  { Zuordnung KE-Daten der einzelnen Meßstrecken, Haupt-/Vgl.rechner -> Wieser Kanalnummern
    -> wie im MRG-Stammdatensystem vorgeschrieben, erst alle Impulskanäle, dann alle Analogkanäle !
    Ergebis: Wieser-Kanalnummer
    MS: 0     HV: 0    W1  -> Kanal 1
                       W2  -> Kanal 2
                       VN1 -> Kanal 3
                       VN2 -> Kanal 4
    MS: 0     HV: 1    W1  -> Kanal 5
                       W2  -> Kanal 6
                       VN1 -> Kanal 7
                       VN2 -> Kanal 8
    MS: 1     HV: 0    W1  -> Kanal 9                 Impulskanäle
                       W2  -> Kanal 10
                       VN1 -> Kanal 11
                       VN2 -> Kanal 12
                       .
                       .
    MS: 5     HV: 1    W1  -> Kanal 45
                       W2  -> Kanal 46
                       VN1 -> Kanal 47
                       VN2 -> Kanal 48
    ----------------------------------------------------------------------
    MS: 0     HV: 0    Rhon  -> Kanal 49
                       Rhob  -> Kanal 50
                       Druck -> Kanal 51
                       Temp  -> Kanal 52
                       CO2   -> Kanal 53
                       HO    -> Kanal 54
    MS: 0     HV: 1    Rhon  -> Kanal 55
                       Rhob  -> Kanal 56
                       Druck -> Kanal 57
                       Temp  -> Kanal 58
                       CO2   -> Kanal 59
                       HO    -> Kanal 60
    MS: 1     HV: 0    Rhon  -> Kanal 61              Analogkanäle
                       Rhob  -> Kanal 62
                       Druck -> Kanal 63
                       Temp  -> Kanal 64
                       CO2   -> Kanal 65
                       HO    -> Kanal 66
                       .
                       .
    MS: 5     HV: 1    Rhon  -> Kanal 115
                       Rhob  -> Kanal 116
                       Druck -> Kanal 117
                       Temp  -> Kanal 118
                       CO2   -> Kanal 119
                       HO    -> Kanal 120                              }
  begin
    Result:=0;
    if (AMS <= 5) AND (AHV <= 1) then begin
      case KE_Wert of
        ke_W1:    Result:=(AMS * 8) + (AHV * 4) + 1;
        ke_W2:    Result:=(AMS * 8) + (AHV * 4) + 2;
        ke_VN1:   Result:=(AMS * 8) + (AHV * 4) + 3;
        ke_VN2:   Result:=(AMS * 8) + (AHV * 4) + 4;

        ke_Rhon:  Result:=(AMS * 12) + (AHV * 6) + 49;
        ke_Rhob:  Result:=(AMS * 12) + (AHV * 6) + 50;
        ke_Druck: Result:=(AMS * 12) + (AHV * 6) + 51;
        ke_Temp:  Result:=(AMS * 12) + (AHV * 6) + 52;
        ke_CO2:   Result:=(AMS * 12) + (AHV * 6) + 53;
        ke_HO:    Result:=(AMS * 12) + (AHV * 6) + 54;
      end;  { case }
    end;
  end;

const
  { Länge Binär-Rohdatensatz }
  CLenBinRec = 111;     { Zeitstempel (4 Byte time)
                          laufende Nummer des Eintrags (4 Byte long)
                          Meßstreckennummer (1 Byte)
                          Flag  Hauptrechner/Vergleichsrechner (1 Byte)
                          Name des Meßrechners (12 Byte string)
                          Wärmemengenzähler, Abrechnungsmodus 1 (8 Byte double)
                          Wärmemengenzähler, Abrechnungsmodus 2 (8 Byte double)
                          Normvolumenzähler, Abrechnungsmodus 1 (8 Byte double)
                          Normvolumenzähler, Abrechnungsmodus 2 (8 Byte double)
                          Normdichte (4 Byte float)
                          Betriebsdichte (4 Byte float)
                          Druck (4 Byte float)
                          Temperatur (4 Byte float)
                          CO2 (4 Byte float)
                          Brennwert (4 Byte float)
                          Meßstreckenstatus (1 Byte)
                          Meßrechnerstati 1 - 32 (32 Byte) }

var
  fs_roh: TFileOfCharStream;  // 06.10.2015, WW
  fs_RohSRec: TFileofRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
  FSize_roh: integer;
  std_satz: RohSRec;
  zeichen: char;
  rohsatz: string;
  S: string;
  DT, DT_merk: TDateTime;
  MS, HV: byte;
  W1, W2, VN1, VN2: double;
  Rhon, Rhob, Druck, Temp, CO2, HO: double;
  k: integer;
  dt_offset: TDateTime;
  sec_offset: cardinal;
  sec_daten: cardinal;

begin
  Result := False;
  if not FileExists (MessKonvRec.StdQuelldateiname) then exit;

  Result:=true;
  try
    { Rohfile öffnen: }
    fs_roh:=TFileOfCharStream.Create (MessKonvRec.StdQuelldateiname, fmOpenRead OR fmShareDenyWrite);
    try
      FSize_roh:=fs_roh.Size;
      { Prüfung, ob Rohfilegröße ein ganzzahliges Vielfaches der Binär-Recordlänge ist.
        Wenn nicht, ist mit dem Rohfile was faul ! }
      if (FSize_roh MOD CLenBinRec) <> 0 then begin
        Result:=false;
        exit;
      end;

      { Stundenwert-Zielfile erzeugen: }
      fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (RohSRec));
      try
        { KE rechnet die Zeitstempel ab 1980. Deshalb für Umrechnung mit
          UnixTime-Routine die Sekunden von 1.1.1970 bis 1.1.1980 als Offset
          aufaddieren: 08.04.2004, WW }
        dt_offset:=EncodeDate (1980, 1, 1);
        sec_offset:=GetUnixSekundenFromDateTime (dt_offset);

        rohsatz := '';
        DT_merk:=0;
        Init_RohSRec (std_satz);
        while (fs_roh.Position < FSize_roh) do begin
          Application.ProcessMessages;
          fs_roh.Read (zeichen);
          rohsatz := rohsatz + zeichen;
          if length (rohsatz) = CLenBinRec then begin
            S:=Copy (rohsatz, 1, 4);      { Zeitstempel, Rohdaten }
            S:=Format ('%.2x',[Ord (S [4])]) +
               Format ('%.2x',[Ord (S [3])]) +
               Format ('%.2x',[Ord (S [2])]) +
               Format ('%.2x',[Ord (S [1])]);         { Zeitstempel, Hex-Form }
            GetUnixSekundenFromUnixTimeStr (S, sec_daten);
            sec_daten:=sec_daten + sec_offset;  { Sekunden-Offset aufaddieren }
            UnixSekundenToDateTime (sec_daten, DT);

            { laufende Nummer des Eintrags: wird nicht ausgewertet }

            MS:=Ord (rohsatz [9]);      { Meßstreckennummer (0..5) }
            HV:=Ord (rohsatz [10]);     { Flag: Hauptrechner (0), Vergleichsrechner (1) }

            { Name des Meßrechners: wird nicht ausgewertet }

            S:=Copy (rohsatz, 23, 8);   { Wärmemengenzähler, Abrechnungsmodus 1 }
            W1:=Bin2Double (S);
            S:=Copy (rohsatz, 31, 8);   { Wärmemengenzähler, Abrechnungsmodus 2 }
            W2:=Bin2Double (S);
            S:=Copy (rohsatz, 39, 8);   { Normvolumenzähler, Abrechnungsmodus 1 }
            VN1:=Bin2Double (S);
            S:=Copy (rohsatz, 47, 8);   { Normvolumenzähler, Abrechnungsmodus 2 }
            VN2:=Bin2Double (S);
            S:=Copy (rohsatz, 55, 4);   { Normdichte }
            Rhon:=Bin2Single (S);
            S:=Copy (rohsatz, 59, 4);   { Betriebsdichte }
            Rhob:=Bin2Single (S);
            S:=Copy (rohsatz, 63, 4);   { Druck }
            Druck:=Bin2Single (S);
            S:=Copy (rohsatz, 67, 4);   { Temperatur }
            Temp:=Bin2Single (S);
            S:=Copy (rohsatz, 71, 4);   { CO2 }
            CO2:=Bin2Single (S);
            S:=Copy (rohsatz, 75, 4);   { Brennwert }
            HO:=Bin2Single (S);

            { Meßstreckenstatus: wird nicht ausgewertet }
            { Meßrechnerstati 1 - 32: werden nicht ausgewertet }

            { bei Wechsel des Zeitstempels: Stundensatz wegschreiben }
            if (DT <> DT_merk) AND (DT_merk > 0) then begin
              fs_RohSRec.WriteRec (std_satz);
              Init_RohSRec (std_satz);
            end;  { if DT <> DT_merk }

            { std_satz mit Werten des aktuellen Rohsatzes belegen: }
            with std_satz do begin
              DatumZeit:=DT;
              { W1: }
              k:=Get_WieserKanalnummer (MS, HV, ke_W1);
              KanalOrig [k].wert:=W1;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { W2: }
              k:=Get_WieserKanalnummer (MS, HV, ke_W2);
              KanalOrig [k].wert:=W2;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { VN1: }
              k:=Get_WieserKanalnummer (MS, HV, ke_VN1);
              KanalOrig [k].wert:=VN1;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { VN2: }
              k:=Get_WieserKanalnummer (MS, HV, ke_VN2);
              KanalOrig [k].wert:=VN2;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { Rhon: }
              k:=Get_WieserKanalnummer (MS, HV, ke_Rhon);
              KanalOrig [k].wert:=Rhon;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { Rhob: }
              k:=Get_WieserKanalnummer (MS, HV, ke_Rhob);
              KanalOrig [k].wert:=Rhob;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { Druck: }
              k:=Get_WieserKanalnummer (MS, HV, ke_Druck);
              KanalOrig [k].wert:=Druck;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { Temp: }
              k:=Get_WieserKanalnummer (MS, HV, ke_Temp);
              KanalOrig [k].wert:=Temp;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { CO2: }
              k:=Get_WieserKanalnummer (MS, HV, ke_CO2);
              KanalOrig [k].wert:=CO2;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { HO: }
              k:=Get_WieserKanalnummer (MS, HV, ke_HO);
              KanalOrig [k].wert:=HO;              { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }

              { -> Konvertierung für LGZ-Daten (Feld .Kanal) vorerst nicht erforderlich,
                   da KE-Anlagen bislang nur über WicomSrv-Abrufdienst für Gas-X ausgelesen werden }

            end;  { with std_satz }

            { wenn Rohfileende erreicht: Stundensatz wegschreiben }
            if fs_roh.Position = FSize_roh then
              fs_RohSRec.WriteRec (std_satz);
            DT_merk:=DT;
            rohsatz:='';
          end;
        end;  { while not eof }
      finally
        fs_RohSRec.Free;
      end;
    finally
      fs_roh.Free;
    end;

    if length (rohsatz) > 0 then        { Fehler Rohdatenlänge }
      Result := False;

    if MessKonvRec.loeschen then
      DeleteFile (MessKonvRec.StdQuelldateiname);  { Rohfile löschen }
  except
    Result:=false;
  end;
end;


{-------------------------------------------------------------}
function Mess_Konv_KE_PPN (MessKonvRec: TMessTagKonv): Boolean;
{-------------------------------------------------------------}
{ Konvertierung von Daten aus KE-Anlage (PPN-Rohdaten, abgerufen mit T090-Befehl) }

type
  { KE-Datentypen (nur Impuls) }
  TKE_Wert = (ke_W, ke_VN, ke_W_stoer, ke_VN_stoer);

  {--------------------------------------------------------------------------------}
  function Get_WieserImpKanalnummer (AStrecke: integer; KE_Wert: TKE_Wert): integer;
  {--------------------------------------------------------------------------------}
  { Zuordnung KE-Impulsdaten der einzelnen Strecken -> Wieser Kanalnummern
    Ergebis: Wieser-Kanalnummer
    Strecke: 1         W       -> Kanal 1
                       VN      -> Kanal 2
                       W stör  -> Kanal 3
                       VN stör -> Kanal 4
    Strecke: 2         W       -> Kanal 5
                       VN      -> Kanal 6
                       W stör  -> Kanal 7
                       VN stör -> Kanal 8
                       .
                       .
    Strecke: 12        W       -> Kanal 45
                       VN      -> Kanal 46
                       W stör  -> Kanal 47
                       VN stör -> Kanal 48 }
  begin
    Result:=0;
    if (AStrecke >= 1) AND (AStrecke <= 12) then begin
      case KE_Wert of
        ke_W:        Result:=((AStrecke-1) * 4) + 1;
        ke_VN:       Result:=((AStrecke-1) * 4) + 2;
        ke_W_stoer:  Result:=((AStrecke-1) * 4) + 3;
        ke_VN_stoer: Result:=((AStrecke-1) * 4) + 4;
      end;  { case }
    end;
  end;

const
  { Länge Binär-Rohdatensatz }
  CLenBinRec = 606;     { laufende Nummer des Eintrags (4 Byte long)
                          Zeitstempel (4 Byte time)
                          Brennwert Tagesmittel, Hauptmessung (4 Byte float)
                          Normdichte Stundenmittel, Hauptmessung (4 Byte float)
                          Normdichte Stundenmittel, Vergleichsmessung (4 Byte float)
                          Brennwert Stundenmittel, Hauptmessung (4 Byte float)
                          Brennwert Stundenmittel, Vergleichsmessung (4 Byte float)
                          Normdichte-Status (aktive Haupt-/Vergleichsmessung) (1 Byte)
                          Brennwert-Status (aktive Haupt-/Vergleichsmessung) (1 Byte)
                        jeweils für Strecke 1 bis 12:
                          Wärmemengenzähler (8 Byte double)
                          Normvolumenzähler (8 Byte double)
                          Wärmemengenstörzähler (8 Byte double)
                          Normvolumenstörzähler (8 Byte double)
                          Wärmemengentageszähler (8 Byte double)
                          Normvolumentageszähler (8 Byte double) }

var
  fs_roh: TFileOfCharStream;  // 06.10.2015, WW
  fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
  FSize_roh: integer;
  std_satz: RohSRec;
  zeichen: char;
  rohsatz: string;
  rohsatz_strecke: string;
  S: string;
  i, k: integer;
  Strecke: integer;
  dt_offset: TDateTime;
  sec_offset: cardinal;
  sec_daten: cardinal;

begin
  Result:=False;
  if not FileExists (MessKonvRec.StdQuelldateiname) then exit;

  Result:=true;
  try
    { Rohfile öffnen: }
    fs_roh:=TFileOfCharStream.Create (MessKonvRec.StdQuelldateiname, fmOpenRead OR fmShareDenyWrite);
    try
      FSize_roh:=fs_roh.Size;
      { Stundenwert-Zielfile erzeugen: }
      fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (RohSRec));
      try
        { KE rechnet die Zeitstempel ab 1980. Deshalb für Umrechnung mit
          UnixTime-Routine die Sekunden von 1.1.1970 bis 1.1.1980 als Offset
          aufaddieren: 08.04.2004, WW }
        dt_offset:=EncodeDate (1980, 1, 1);
        sec_offset:=GetUnixSekundenFromDateTime (dt_offset);

        rohsatz := '';
        while fs_roh.Position < FSize_roh do begin
          Application.ProcessMessages;
          fs_roh.Read (zeichen);
          rohsatz := rohsatz + zeichen;
          { auf GENAUE Binärsatz-Länge prüfen, da Rohfile in der Regel am Ende mit
            Dummy-Zeichen <SUB> aufgefüllt ist (Rohdaten-Länge ist immer ein ganzzahliges
            Vielfaches von 128-Byte-Blöcken, Sealink-Protokoll !): }
          if length (rohsatz) = CLenBinRec then begin
            FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
            with std_satz do begin
              satzstatus:= $00;  { Satzstatus fest: OK }
              for i:=1 to c_maxKanalZahl do begin          { Fehlend-Werte für alle Kanäle }
                kanal[i].wert:=0;
                KanalOrig[i].wert:=0;
                if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then  { Analogkanal }
                  kanal[i].kanalstatus:=$A0
                else                                  { Impulskanal }
                  kanal[i].kanalstatus:=$80;
                KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
              end;

              { std_satz mit Werten des aktuellen Rohsatzes belegen: }

              { laufende Nummer des Eintrags: wird nicht ausgewertet }

              S:=Copy (rohsatz, 5, 4);      { Zeitstempel, Rohdaten }
              S:=Format ('%.2x',[Ord (S [4])]) +
                 Format ('%.2x',[Ord (S [3])]) +
                 Format ('%.2x',[Ord (S [2])]) +
                 Format ('%.2x',[Ord (S [1])]);         { Zeitstempel, Hex-Form }
              GetUnixSekundenFromUnixTimeStr (S, sec_daten);
              sec_daten:=sec_daten + sec_offset;  { Sekunden-Offset aufaddieren }
              UnixSekundenToDateTime (sec_daten, DatumZeit);

              { es folgen Werte von 5 Analogkanälen: }
              { Brennwert Tagesmittel, Hauptmessung: wird nicht ausgewertet }

              { Normdichte Stundenmittel, Hauptmessung -> Wieserkanal 49: }
              S:=Copy (rohsatz, 13, 4);
              k:=51;
              KanalOrig [k].wert:=Bin2Single (S);                   { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { Normdichte Stundenmittel, Vergleichsmessung -> Wieserkanal 50: }
              S:=Copy (rohsatz, 17, 4);
              k:=52;
              KanalOrig [k].wert:=Bin2Single (S);                   { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { Brennwert Stundenmittel, Hauptmessung -> Wieserkanal 51: }
              S:=Copy (rohsatz, 21, 4);
              k:=53;
              KanalOrig [k].wert:=Bin2Single (S);                   { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { Brennwert Stundenmittel, Vergleichsmessung -> Wieserkanal 52: }
              S:=Copy (rohsatz, 25, 4);
              k:=54;
              KanalOrig [k].wert:=Bin2Single (S);                   { Original-Rohwert }
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }

              { es folgen 2 Stati (-> Impulskanäle): 13.10.2003, WW }
              { Normdichte-Status: }
              k:=49;
              KanalOrig [k].wert:=Ord (rohsatz [29]);
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
              { Brennwert-Status: }
              k:=50;
              KanalOrig [k].wert:=Ord (rohsatz [30]);
              KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }

              { es folgen Werte von jeweils 6 Impulskanälen für Strecke 1 bis 12: }
              rohsatz:=Copy (rohsatz, 31, length (rohsatz));   { Impulskanal-Rohdaten rauskopieren }
              Strecke:=0;
              while length (rohsatz) > 0 do begin
                inc (Strecke);
                rohsatz_strecke:=Copy (rohsatz, 1, 48);   { Rohdaten einer Strecke: 6 Doubles mit je 8 Byte }
                { Wärmemengenzähler: }
                S:=Copy (rohsatz_strecke, 1, 8);
                k:=Get_WieserImpKanalnummer (Strecke, ke_W);
                KanalOrig [k].wert:=Bin2Double (S);              { Original-Rohwert }
                KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                { Normvolumenzähler: }
                S:=Copy (rohsatz_strecke, 9, 8);
                k:=Get_WieserImpKanalnummer (Strecke, ke_VN);
                KanalOrig [k].wert:=Bin2Double (S);              { Original-Rohwert }
                KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                { Wärmemengenstörzähler: }
                S:=Copy (rohsatz_strecke, 17, 8);
                k:=Get_WieserImpKanalnummer (Strecke, ke_W_stoer);
                KanalOrig [k].wert:=Bin2Double (S);              { Original-Rohwert }
                KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                { Normvolumenstörzähler: }
                S:=Copy (rohsatz_strecke, 25, 8);
                k:=Get_WieserImpKanalnummer (Strecke, ke_VN_stoer);
                KanalOrig [k].wert:=Bin2Double (S);              { Original-Rohwert }
                KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }

                { Wärmemengentageszähler: wird nicht ausgewertet }
                { Normvolumentageszähler: wird nicht ausgewertet }

                Delete (rohsatz, 1, 48);
              end;

              { -> Konvertierung für LGZ-Daten (Feld .Kanal) vorerst nicht erforderlich,
                   da KE-Anlagen bislang nur über WicomSrv-Abrufdienst für Gas-X ausgelesen werden }

            end;  { with std_satz }

            fs_RohSRec.WriteRec (std_satz);
            rohsatz:='';
          end;
        end;  { while not eof }
      finally
        fs_RohSRec.Free;
      end;
    finally
      fs_roh.Free;
    end;

    if MessKonvRec.loeschen then
      DeleteFile (MessKonvRec.StdQuelldateiname);  { Rohfile löschen }
  except
    Result:=false;
  end;
end;


{----------------------------- Datacon FWU ------------------------------------}

{----------------------------------------------------------}
function Mess_Konv_FWU (MessKonvRec: TMessTagKonv): Boolean;
{----------------------------------------------------------}
{ Konvertierung Datacon FWU (Rohdaten nach IEC-1107-Protokoll)
  -> abgeleitet von Konvertierung für Tritschler VC2 (mit einigen Formatabweichungen)
  -> Die Daten jedes Zählerstands- und Messwert-Kanals steht in einem eigenen Rohfile,
  -> Die Rohdaten der einzelnen FWU-Kanäle (insbesondere die Werteprofile, aber auch
     die Zählerstände, da diese nicht nur zum Tagesende des FWU, sondern auch bei
     Revision gebildet werden) können unterschiedliche Zeitbereiche und Zeitstempel
     aufweisen, da sie aus verschiedenen unabhängigen Quellen stammen (MU, Tarifgeräte,
     FWU selbst). Genaue zeitliche Zuordnung der einzelnen Kanaldaten ist erforderlich ! }
var
  fs_roh: TFileOfCharStream;       { Rohdatendatei }  // 06.10.2015, WW
  fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
  fs_RohTRec: TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
  FSize_roh: integer;
  Rohfilename: string;
  rohsatz: string;
  zeichen: char;
  KanalNr: integer;
  s: string;
  code: integer;
  datum: DateRec;
  zeit: TimeRec;
  szTemp: string [10];
  faktor: double;
  anzmp: integer;
  anztage: integer;
  tag: integer;
  mp: integer;
  mw_phys: double;
  mwstr: string;
  zs_phys: double;
  zsstr: string;
  zs_zwlist: TObjectList;
  mw_zwlist: TObjectList;
  tag_satz: RohTRec;
  std_satz: RohSRec;
  RohTRecExtObj: TRohTRecExtObj;
  RohSRecObj: TRohSRecObj;
  i: integer;
  zs_list_pos: integer;
  mw_list_pos: integer;
  mw_lgz: LongInt;
  l: integer;
  anz_ges_stellen: integer;
  DTBuf: TDateTime;
  hour, min, sec, msec: word;

begin
  Result := True;

  zs_zwlist:=TObjectList.Create;             { Zwischenliste für Zählerstände }
  try
    mw_zwlist:=TObjectList.Create;           { Zwischenliste für Messwerte }
    try

{-------- Konvertierung Messwert-Rohdateien -> Messwert-Zwischenliste ---------}
      try
        for l:=0 to MessKonvRec.StdQuellDateiNameListe.Count-1 do begin  { alle Messwert-Rohdateien abarbeiten }
          Rohfilename:=MessKonvRec.StdQuellDateiNameListe [l];    { Rohfilename }
          if length (Rohfilename) = 0 then Continue;
          if not FileExists (Rohfilename) then begin
            Result := False;
            Continue;
          end;

          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;

            KanalNr:=0;
            mw_list_pos:=0;              { Vorbelegung Stundensatz-Listenposition }

            rohsatz:='';
            zeichen:=NUL;
            { Rohfileanfang bis zum STX überlesen }
            while (zeichen <> STX) AND (fs_roh.Position < FSize_roh) do
              fs_roh.Read (zeichen);

            { Konvertierung }
            while (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
              rohsatz:='';
              zeichen:=NUL;
              while (zeichen <> LF) AND (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
              end;

              Application.ProcessMessages;

              { Werteprofile konvertieren in MW-Zwischenliste -> Meßwerte }
              if (Copy (rohsatz, 1, 10) = '7-1z:V.b.p') OR
                 (Copy (rohsatz, 1, 10) = '7-1m:V.b.p') OR
                 (Copy (rohsatz, 1, 10) = '7-1m:V.n.p') OR
                 (Copy (rohsatz, 1, 10) = '7-1j:V.b.p') OR
                 (Copy (rohsatz, 1, 10) = '7-1i:V.b.p') OR
                 (Copy (rohsatz, 1, 10) = '7-1i:V.n.p') OR
                 (Copy (rohsatz, 1, 10) = '7-1X:T.0.p') OR
                 (Copy (rohsatz, 1, 10) = '7-1X:P.0.p') OR
                 (Copy (rohsatz, 1, 10) = '7-2z:V.b.p') OR
                 (Copy (rohsatz, 1, 10) = '7-2m:V.b.p') OR
                 (Copy (rohsatz, 1, 10) = '7-2m:V.n.p') OR
                 (Copy (rohsatz, 1, 10) = '7-2j:V.b.p') OR
                 (Copy (rohsatz, 1, 10) = '7-2i:V.b.p') OR
                 (Copy (rohsatz, 1, 10) = '7-2i:V.n.p') OR
                 (Copy (rohsatz, 1, 10) = '7-2X:T.0.p') OR
                 (Copy (rohsatz, 1, 10) = '7-2X:P.0.p') then begin

                { Kennziffer -> Wieser-Kanalnummer }
                if Copy (rohsatz, 1, 10) = '7-1z:V.b.p' then       { Vb Z, Tarifgerät 1 }
                  KanalNr:=5
                else if Copy (rohsatz, 1, 10) = '7-1m:V.b.p' then  { Vb MU, Tarifgerät 1 }
                  KanalNr:=6
                else if Copy (rohsatz, 1, 10) = '7-1m:V.n.p' then  { Vn MU, Tarifgerät 1 }
                  KanalNr:=7
                else if Copy (rohsatz, 1, 10) = '7-1j:V.b.p' then  { Vb Z Impuls, FWU MS 1 }
                  KanalNr:=8
                else if Copy (rohsatz, 1, 10) = '7-1i:V.b.p' then  { Vb MU Impuls, FWU MS 1 }
                  KanalNr:=9
                else if Copy (rohsatz, 1, 10) = '7-1i:V.n.p' then  { Vn MU Impuls, FWU MS 1 }
                  KanalNr:=10
                else if Copy (rohsatz, 1, 10) = '7-1X:T.0.p' then  { Temperatur, FWU MS 1 }
                  KanalNr:=21
                else if Copy (rohsatz, 1, 10) = '7-1X:P.0.p' then  { Druck, FWU MS 1 }
                  KanalNr:=22
                else if Copy (rohsatz, 1, 10) = '7-2z:V.b.p' then  { Vb Z, Tarifgerät 2 }
                  KanalNr:=15
                else if Copy (rohsatz, 1, 10) = '7-2m:V.b.p' then  { Vb MU, Tarifgerät 2 }
                  KanalNr:=16
                else if Copy (rohsatz, 1, 10) = '7-2m:V.n.p' then  { Vn MU, Tarifgerät 2 }
                  KanalNr:=17
                else if Copy (rohsatz, 1, 10) = '7-2j:V.b.p' then  { Vb Z Impuls, FWU MS 2 }
                  KanalNr:=18
                else if Copy (rohsatz, 1, 10) = '7-2i:V.b.p' then  { Vb MU Impuls, FWU MS 2 }
                  KanalNr:=19
                else if Copy (rohsatz, 1, 10) = '7-2i:V.n.p' then  { Vn MU Impuls, FWU MS 2 }
                  KanalNr:=20
                else if Copy (rohsatz, 1, 10) = '7-2X:T.0.p' then  { Temperatur, FWU MS 2 }
                  KanalNr:=23
                else if Copy (rohsatz, 1, 10) = '7-2X:P.0.p' then  { Druck, FWU MS 2 }
                  KanalNr:=24;

                s:=ExtractString (rohsatz, '(', ';', 0);
                Val (s, faktor, code);                    { Faktor }
                s:=ExtractString (rohsatz, ';', ';', 1);
                Val (s, anzmp, code);                     { Anzahl Meßperioden pro Tag }
                s:=ExtractString (rohsatz, ';', ';', 2);
                Val (s, anztage, code);                   { Anzahl Tagesdatensätze }
                s:=ExtractString (rohsatz, ';', ',', 3);
                Val (s, anz_ges_stellen, code);           { Anzahl Gesamtstellen eines Rohwertes (incl. Dezimalzeichen) }

                for tag:=1 to anztage do begin  { alle nachfolgenden Tagesdatensätze auslesen }
                  rohsatz:='';
                  zeichen:=NUL;
                  while (zeichen <> LF) AND (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
                    fs_roh.Read (zeichen);
                    rohsatz:=rohsatz + zeichen;                     { Zeile bilden }
                  end;

                  s:=Copy (rohsatz, 1, 8);                   { Datumstempel lesen }
                  with Datum do begin
                    szTemp:=Copy (s, 1, 2);        { Jahr }
                    Val (szTemp, year, Code);
                    if Code <> 0 then;
                    if year < 70 then
                      year:=year + 2000
                    else
                      year:=year + 1900;
                    szTemp:=Copy (s, 4, 2);        { Monat }
                    Val (szTemp, month, Code);
                    if Code <> 0 then;
                    szTemp:=Copy (s, 7, 2);        { Tag }
                    Val (szTemp, day, Code);
                    if Code <> 0 then;
                  end;
                  s:=Copy (rohsatz, 9, 8);                    { Zeitstempel lesen }
                  with Zeit do begin
                    szTemp:=Copy (s, 1, 2);        { Stunde }
                    Val (szTemp, hour, Code);
                    if Code <> 0 then;
                    szTemp:=Copy (s, 4, 2);        { Minute }
                    Val (szTemp, min, Code);
                    if Code <> 0 then;
                    szTemp:=Copy (s, 7, 2);        { Sekunde }
                    Val (szTemp, sec, Code);
                    if Code <> 0 then;
                    hsec:=0;
                  end;

                  { Meßperiodenwerte extrahieren: }
                  s:=Copy (rohsatz, 17, length (rohsatz));
                  s:=ExtractString (s, NUL, CR, 0);    { alles ab einschließlich CR wegschneiden }

                  { Prüfung, ob Länge des Messperiodenwert-Strings ein ganzzahliges
                    Vielfaches der Gesamtstellenzahl eines Rohwertes ist. Wenn nicht,
                    ist mit dem Rohstring was faul ! }
                  if (length (s) MOD anz_ges_stellen) = 0 then begin
                    for mp:=1 to anzmp do begin
                      mwstr:=Copy (s, 1+(mp-1)*anz_ges_stellen, anz_ges_stellen);  { Meßperiodenwert }
                      if length (mwstr) = 0 then Break;    { es können auch weniger Werte als in anzmp steht kommen ! }

                      if mp > 1 then begin
                        inc (zeit.hour);
                        if zeit.hour > 23 then begin
                          zeit.hour:=0;
                          P_NachTag (datum);        { Gastag -> physik. Tag }
                        end;
                      end;

                      StrSubst(mwstr, ',', '.');  { Dezimal-Komma durch Punkt ersetzen }
                      Val (mwstr, mw_phys, Code);     { Meßperiodenwert (Integer- oder Float-String) wandeln }
                      mw_phys:=mw_phys * faktor;         { physikalischer Wert = Rohwert * Faktor }

                      if RecToDateTime (datum, zeit, DTBuf) then begin
                        if Search_MW_ZwList_DatumZeit (mw_zwlist, mw_list_pos, DTBuf, false) then begin  // Rohdaten/Liste zeitlich aufsteigend
                          { Eintrag für Datum, Zeit in MW-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
                          if (mw_list_pos >= 0) AND (mw_list_pos < mw_zwlist.Count) then begin
                            with TRohSRecObj (mw_zwlist.Items [mw_list_pos]).RohSRec do begin
                              KanalOrig [KanalNr].Wert:=mw_phys;         { Original-Rohwert }
                              KanalOrig [KanalNr].KanalStatus:=KanalOrig [KanalNr].KanalStatus and $7F; { Fehlend-Bit löschen }

                              { -> Konvertierung für LGZ-Daten (Feld .Kanal) vorerst nicht implementiert,
                                   da FWU-Geräte bislang nur über WicomSrv-Abrufdienst für Gas-X ausgelesen werden }
                            end;
                          end;
                        end
                        else begin
                          { nicht gefunden: neuen Eintrag in MW-Zwischenliste an mw_list_pos eintragen }
                          FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
                          with std_satz do begin
                            satzstatus:= $00;  { Satzstatus fest: OK }

                            { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                            for i:=1 to c_maxKanalZahl do begin
                              kanal[i].wert:=0;
                              KanalOrig[i].wert:=0;
                              if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then  { Analogkanal }
                                kanal[i].kanalstatus:=$A0
                              else                                  { Impulskanal }
                                kanal[i].kanalstatus:=$80;
                              KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                              KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                            end;
                          end;  { with std_satz }

                          { Daten setzen: }
                          std_satz.DatumZeit:=DTBuf;
                          std_satz.KanalOrig [KanalNr].Wert:=mw_phys;  { Original-Rohwert }
                          std_satz.KanalOrig [KanalNr].KanalStatus:=std_satz.KanalOrig [KanalNr].KanalStatus and $7F;       { Fehlend-Bit löschen }

                          { -> Konvertierung für LGZ-Daten (Feld .Kanal) vorerst nicht implementiert,
                               da FWU-Geräte bislang nur über WicomSrv-Abrufdienst für Gas-X ausgelesen werden }

                          { Objekt in Liste anhängen: }
                          RohSRecObj:=TRohSRecObj.Create (std_satz);
                          mw_zwlist.insert (mw_list_pos, RohSRecObj);
                        end;
                      end;  { if RecToDateTime }
                      inc (mw_list_pos);
                    end;  { for mp }
                  end; { if length (s) MOD anz_ges_stellen }
                end;  { for tag }
              end;
            end;   { while not EOF }
          finally
            fs_roh.Free;
          end;

          if MessKonvRec.loeschen then
            DeleteFile (Rohfilename);  { Rohfile löschen }
        end;  { for l }
      except
        Result:=false;
      end;

{------- Konvertierung Tagessatz-Rohdateien -> Tagessatz-Zwischenliste --------}
      try
        for l:=0 to MessKonvRec.TagQuellDateiNameListe.Count-1 do begin  { alle Tagessatz-Rohdateien abarbeiten }
          Rohfilename:=MessKonvRec.TagQuellDateiNameListe [l];    { Rohfilename }
          if length (Rohfilename) = 0 then Continue;
          if not FileExists (Rohfilename) then begin
            Result := False;
            Continue;
          end;

          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;

            KanalNr:=0;
            zs_list_pos:=0;               { Vorbelegung Tagsatz-Listenposition }

            rohsatz:='';
            zeichen:=NUL;
            { Rohfileanfang bis zum STX überlesen }
            while (zeichen <> STX) AND (fs_roh.Position < FSize_roh) do
              fs_roh.Read (zeichen);

            { Konvertierung }
            while (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
              rohsatz:='';
              zeichen:=NUL;
              while (zeichen <> LF) AND (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
              end;

              Application.ProcessMessages;

              { Zählerstände konvertieren in ZS-Zwischenliste }
              if (Copy (rohsatz, 1, 10) = '7-1M:V.b.t') OR
                 (Copy (rohsatz, 1, 10) = '7-1M:V.n.t') OR
                 (Copy (rohsatz, 1, 10) = '7-1z:V.b.t') OR
                 (Copy (rohsatz, 1, 10) = '7-1m:V.b.t') OR
                 (Copy (rohsatz, 1, 10) = '7-1m:V.n.t') OR
                 (Copy (rohsatz, 1, 10) = '7-2M:V.b.t') OR
                 (Copy (rohsatz, 1, 10) = '7-2M:V.n.t') OR
                 (Copy (rohsatz, 1, 10) = '7-2z:V.b.t') OR
                 (Copy (rohsatz, 1, 10) = '7-2m:V.b.t') OR
                 (Copy (rohsatz, 1, 10) = '7-2m:V.n.t') then begin

                { Kennziffer -> Wieser-Kanalnummer
                  Anm.: Für die zukünftig im FWU vorgesehenen Störzähler Vb, Vn für MU1 und MU2
                        wurden die Kanalnummern 3, 4 und 13, 14 schon mal vorab reserviert. }
                if Copy (rohsatz, 1, 10) = '7-1M:V.b.t' then         { Vb, MU1 }
                  KanalNr:=1
                else if Copy (rohsatz, 1, 10) = '7-1M:V.n.t' then    { Vn, MU1 }
                  KanalNr:=2
                { Wieser-Kanal 3 reserviert für Vb stör, MU1 }
                { Wieser-Kanal 4 reserviert für Vn stör, MU1 }
                else if Copy (rohsatz, 1, 10) = '7-1z:V.b.t' then    { Vb Z, Tarifgerät 1 }
                  KanalNr:=5
                else if Copy (rohsatz, 1, 10) = '7-1m:V.b.t' then    { Vb MU, Tarifgerät 1 }
                  KanalNr:=6
                else if Copy (rohsatz, 1, 10) = '7-1m:V.n.t' then    { Vn MU, Tarifgerät 1 }
                  KanalNr:=7
                else if Copy (rohsatz, 1, 10) = '7-2M:V.b.t' then    { Vb, MU2 }
                  KanalNr:=11
                else if Copy (rohsatz, 1, 10) = '7-2M:V.n.t' then    { Vn, MU2 }
                  KanalNr:=12
                { Wieser-Kanal 13 reserviert für Vb stör, MU2 }
                { Wieser-Kanal 14 reserviert für Vn stör, MU2 }
                else if Copy (rohsatz, 1, 10) = '7-2z:V.b.t' then    { Vb Z, Tarifgerät 2 }
                  KanalNr:=15
                else if Copy (rohsatz, 1, 10) = '7-2m:V.b.t' then    { Vb MU, Tarifgerät 2 }
                  KanalNr:=16
                else if Copy (rohsatz, 1, 10) = '7-2m:V.n.t' then    { Vn MU, Tarifgerät 2 }
                  KanalNr:=17;

                s:=ExtractString (rohsatz, '(', ';', 0);
                Val (s, faktor, code);                 { Faktor }
                { Anzahl Meßperioden pro Tag nicht relevant, es gibt immer nur einen Wert pro Tag }
                s:=ExtractString (rohsatz, ';', ';', 2);
                Val (s, anztage, code);                { Anzahl Tagesdatensätze }

                for tag:=1 to anztage do begin         { Tagesdatensätze auslesen }
                  rohsatz:='';
                  zeichen:=NUL;
                  while (zeichen <> LF) AND (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
                    fs_roh.Read (zeichen);
                    rohsatz:=rohsatz + zeichen;                     { Zeile bilden }
                  end;

                  s:=Copy (rohsatz, 1, 8);                   { Datumstempel lesen }
                  with Datum do begin
                    szTemp:=Copy (s, 1, 2);        { Jahr }
                    Val (szTemp, year, Code);
                    if Code <> 0 then;
                    if year < 70 then
                      year:=year + 2000
                    else
                      year:=year + 1900;
                    szTemp:=Copy (s, 4, 2);        { Monat }
                    Val (szTemp, month, Code);
                    if Code <> 0 then;
                    szTemp:=Copy (s, 7, 2);        { Tag }
                    Val (szTemp, day, Code);
                    if Code <> 0 then;
                  end;
                  s:=Copy (rohsatz, 9, 8);                    { Zeitstempel lesen }
                  with Zeit do begin
                    szTemp:=Copy (s, 1, 2);        { Stunde }
                    Val (szTemp, hour, Code);
                    if Code <> 0 then;
                    szTemp:=Copy (s, 4, 2);        { Minute }
                    Val (szTemp, min, Code);
                    if Code <> 0 then;
                    szTemp:=Copy (s, 7, 2);        { Sekunde }
                    Val (szTemp, sec, Code);
                    if Code <> 0 then;
                    hsec:=0;
                  end;

                  { Tagessatz extrahieren: }
                  zsstr:=Copy (rohsatz, 17, length (rohsatz));
                  StrSubst(zsstr, ',', '.');  { Dezimal-Komma durch Punkt ersetzen }
                  Val (zsstr, zs_phys, Code);     { Tagessatzwert (Integer- oder Float-String) wandeln }
                  zs_phys:=zs_phys * faktor;      { physikalischer Wert = Rohwert * Faktor }

                  if RecToDateTime (Datum, Zeit, DTBuf) then begin
                    if Search_ZS_ZwList_DatumZeit (zs_zwlist, zs_list_pos, DTBuf, false) then begin  // Rohdaten/Liste zeitlich aufsteigend
                      { Eintrag für Datum, Zeit in ZS-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
                      if (zs_list_pos >= 0) AND (zs_list_pos < zs_zwlist.Count) then begin
                        with TRohTRecExtObj (zs_zwlist.Items [zs_list_pos]).RohTRec do begin
                          E_zaehler[KanalNr].wert:=zs_phys;  // Rundung raus; 27.03.2013, WW
                          E_zaehler[KanalNr].zaehlerstatus:=
                            E_zaehler[KanalNr].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                        end;
                      end;
                    end
                    else begin
                      { nicht gefunden: neuen Eintrag in Zählerstands-Zwischenliste an zs_list_pos eintragen }
                      FillChar (tag_satz, SizeOf (tag_satz), 0);   { Vorbelegung tag_satz }
                      with tag_satz do begin
                        satzstatus:= $00;  { Satzstatus fest: OK }

                        { Vorbelegung Kanalstatus für fehlend }
                        for i:=1 to c_maxKanalZahl do begin
                          E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                          E_zaehler[i].wert:=0;
                          K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                          K_zaehler[i].wert:=0;
                        end;
                      end;  { with tag_satz }

                      { Kanal-Daten setzen:
                        -> mit physikalischem Datum (Gastag-Berechnung erst beim Schreiben der
                           Zwischenliste in Zwischendatei) }
                      tag_satz.DatumZeit:=DTBuf;
                      tag_satz.E_zaehler[KanalNr].wert:=zs_phys;  // Rundung raus; 27.03.2013, WW
                      tag_satz.E_zaehler[KanalNr].zaehlerstatus:=
                        tag_satz.E_zaehler[KanalNr].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }

                      { Objekt in Liste anhängen: }
                      RohTRecExtObj:=TRohTRecExtObj.Create (tag_satz, -1);
                      zs_zwlist.Insert (zs_list_pos, RohTRecExtObj);    { Listen-Index wurde in Seach_ZS_ZwList gesetzt }
                    end;
                  end;
                  inc (zs_list_pos);    { zs_list_pos weiterpositionieren für nächsten Eintrag }
                end;  { for tag }
              end;
            end;   { while not EOF }
          finally
            fs_roh.Free;
          end;

          if MessKonvRec.loeschen then
            Deletefile (Rohfilename);  { Rohfile löschen }
        end;  { for l }
      except
        Result:=false;
      end;

{------------ Konvertierung Zwischenlisten -> Zwischendateien -----------------}

      { ZS-Zwischenliste in ZS-Zwischenfile konvertieren; Zählerstände sind zeitlich
        aufsteigend enthalten: }
      try
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          for i:=0 to zs_zwlist.Count - 1 do begin
            Application.ProcessMessages;
            tag_satz:=TRohTRecExtObj (zs_zwlist.Items [i]).RohTRec;
            DecodeTime (tag_satz.DatumZeit, hour, min, sec, msec);
            if hour <= MessKonvRec.TagesEnde then
              tag_satz.DatumZeit:=tag_satz.DatumZeit - 1; { Zählerstand für Gastag }
            fs_RohTRec.WriteRec (tag_satz);
          end;  { for }
        finally
          fs_RohTRec.Free;
        end;
      except
        Result:=false;
      end;

      { MW-Zwischenliste in MW-Zwischenfile konvertieren; Messwerte sind zeitlich
        aufsteigend enthalten: }
      try
        fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (RohSRec));
        try
          for i:=0 to mw_zwlist.Count - 1 do begin
            Application.ProcessMessages;
            std_satz:=TRohSRecObj (mw_zwlist.Items [i]).RohSRec;
            fs_RohSRec.WriteRec (std_satz);
          end;  { for }
        finally
          fs_RohSRec.Free;
        end;
      except
        Result:=false;
      end;
    finally;
      mw_zwlist.Free;
    end;
  finally
    zs_zwlist.Free;
  end;
end;

{---------------------------- Actaris Corus -----------------------------------}

{------------------------------------------------------------}
function Mess_Konv_Corus (MessKonvRec: TMessTagKonv): Boolean;
{------------------------------------------------------------}
{ Konvertierung Actaris Corus Hauptspeicherdaten (Interval log);
  -> von der Konvertierung erwarteter Aufbau der Rohdaten:
     alle abgerufenen Antwort-Telegramme und -Folgetelegramme aneinandergereiht }

  {-------------------------------------------}
  function Mess_Konv (Datenart: byte): Boolean;
  {-------------------------------------------}
  { Übergabe: Datenart (0 = Messwerte (MessKonvRec.StdQuelldateiname wird konvertiert)
                        1 = Tagessätze (MessKonvRec.TagQuelldateiname wird konvertiert) }
  const
    CMaxCorus_MwKanaele = 7;
    CMaxCorus_TaKanaele = 4;

    CRecSize_IntervalLog = 24;  { Recordgröße Hauptdatenspeicher mit gesetzten
                                  Datamask-Bits für/ Date_End, Vb, Vn, Vb stör,
                                  Vn ges, Status, Druck, Temperatur, Vb kontr -> Messwerte }
    CRecSize_MonthlyLog = 38;  { Recordgröße Monatsspeicher mit gesetzten
                                 Datamask-Bits für Date_End, Status, Vb, Vn,
                                 Vb stör, Vn ges -> Tagessätze }
  type
    { Zustände für Rohdatenkonvertierung }
    TModus = (m_Start, m_Size, m_NumFrame_End, m_RecSize_Num, m_Data);

  var
    fs_roh: TFileOfCharStream;  // 06.10.2015, WW
    fs_RohRec: TFileOfRecStream;    { Datei mit strukturierten Rohdaten (RohSRec oder RohTRec) }
    FSize_roh: integer;
    zeichen: char;
    rohsatz: string;
    std_satz: RohSRec;
    tag_satz: RohTRec;
    i: integer;
    Modus: TModus;
    S: string;
    zwlist: TObjectList;
    RohSRecObj: TRohSRecObj;
    RohTRecExtObj: TRohTRecExtObj;
    RohFilename: string;
    l: longint;
    iSize: byte;
    iNumframe: word;
    iRecSize: byte;
    iRecSize_Soll: byte;
    iSizeData_Soll: integer;
    sData: string;
    sDataRec: string;
    cWert: cardinal;
    Wert_norm: Int64;
    Wert_orig: double;
    Delta: double;
    dtBuf_Datum: TDateTime;
    dtBuf_Zeit: TDateTime;
    Code: integer;
    bWert_OK: boolean;
    time_rec: TimeRec;
    tagesende_rec: TimeRec;

  begin
    Result := False;
    case Datenart of
      0: begin  // Messwerte konvertieren
           RohFilename:=MessKonvRec.StdQuelldateiname;
           iRecSize_Soll:=CRecSize_IntervalLog;
         end;
      1: begin  // Tagessätze konvertieren
           RohFilename:=MessKonvRec.TagQuelldateiname;
           iRecSize_Soll:=CRecSize_MonthlyLog;
         end;
    else
      exit;
    end;
    if not FileExists (RohFilename) then exit;

    Result := True;
    zwlist:=TObjectList.Create;             { Zwischenliste für strukturierte Rohdaten }
    try
      try
        { Aktuelle Zählerstände werden mitkonvertiert, wenn lt. Übergabe-Flag
          gewünscht: 09.10.2013, WW }
        if MessKonvRec.bAktZaehler AND (Datenart = 1) AND
           (MessKonvRec.ParameterListe <> nil) then begin
          with tag_satz do begin
            DatumZeit:=0;  // Vorbelegung: Datum/Zeit fehlt
            // Aktuelle Gerätezeit aus Parameterliste holen:
            if MessKonvRec.ParameterListe.GetValue (CP_ACT_CORUS_DateTime, S) then
              if EncodeDateStr (Copy (S, 1, 10), Copy (C_FormatDateTime, 1, 10), dtBuf_Datum) then
                if EncodeTimeStr (Copy (S, 12, 8), Copy (C_FormatDateTime, 12, 8), dtBuf_Zeit) then
                  DatumZeit:=dtBuf_Datum + dtBuf_Zeit;

            if DatumZeit > 0 then begin
              { Satzstatus }
              satzstatus:= $00;

              { Vorbelegung Zählerstati für fehlend }
              for i:=1 to c_maxKanalZahl do begin
                E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                E_zaehler[i].wert:=0;
                K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                K_zaehler[i].wert:=0;
              end;

              // Aktuelle Zählerstände aus Parameterliste holen:
              for i:=1 to CMaxCorus_TaKanaele do begin
                if i <= c_maxKanalZahl then begin
                  bWert_OK:=false;  // Vorbelegung Flag: Wert fehlt
                  Wert_orig:=0;
                  case i of
                    1: begin  { Unconverted consumption -> Vb, Wieser-Kanal 1 }
                         if MessKonvRec.ParameterListe.GetValue (CP_ACT_CORUS_Vb, S) then begin
                           StrSubst (S, ',', '.');  { Dezimal-Komma durch Punkt ersetzen }
                           Val (S, Wert_orig, Code);
                           if Code = 0 then
                             bWert_OK:=true;
                         end;
                       end;

                    2: begin  { Converted consumption -> Vn, Wieser-Kanal 2 }
                         if MessKonvRec.ParameterListe.GetValue (CP_ACT_CORUS_Vn, S) then begin
                           StrSubst (S, ',', '.');  { Dezimal-Komma durch Punkt ersetzen }
                           Val (S, Wert_orig, Code);
                           if Code = 0 then
                             bWert_OK:=true;
                         end;
                       end;

                    3: begin  { Unconverted counter (Al) -> Vb stör, Wieser-Kanal 3 }
                         if MessKonvRec.ParameterListe.GetValue (CP_ACT_CORUS_Vb_stoer, S) then begin
                           StrSubst (S, ',', '.');  { Dezimal-Komma durch Punkt ersetzen }
                           Val (S, Wert_orig, Code);
                           if Code = 0 then
                             bWert_OK:=true;
                         end;
                       end;

                    4: begin  { Converted counter (Al) -> Vn ges, Wieser-Kanal 4 }
                         if MessKonvRec.ParameterListe.GetValue (CP_ACT_CORUS_Vn_ges, S) then begin
                           StrSubst (S, ',', '.');  { Dezimal-Komma durch Punkt ersetzen }
                           Val (S, Wert_orig, Code);
                           if Code = 0 then
                             bWert_OK:=true;
                         end;
                       end;
                  end;  { case }

                  if bWert_OK then begin
                    E_zaehler[i].wert:=Wert_orig;
                    E_zaehler[i].zaehlerstatus:=
                      E_zaehler[i].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                  end;
                end;  { if i <= c_maxKanalZahl }
              end;  { for i }

              { Tagessatz-Objekt in Zwischenliste anhängen: }
              RohTRecExtObj:=TRohTRecExtObj.Create (tag_satz, -1);
              zwlist.Add (RohTRecExtObj);
            end;  { if DatumZeit > 0 }
          end;   { with tag_satz }
        end;

        { Stundenwert-Rohfile öffnen: }
        fs_roh:=TFileOfCharStream.Create (RohFilename, fmOpenRead OR fmShareDenyWrite);
        try
          FSize_roh:=fs_roh.Size;

          rohsatz := '';
          sData:='';
          iSize:=0;
          iNumframe:=0;
          iRecSize:=0;
          Modus:=m_Start;  { als erstes wird das Startzeichen des 1. Telegramms gelesen }

          while fs_roh.Position < FSize_roh do begin
            Application.ProcessMessages;
            fs_roh.Read (zeichen);
            rohsatz := rohsatz + zeichen;

            case Modus of
              m_Start:
                begin
                  if length (rohsatz) = 1 then begin
                    if rohsatz [1] = SOH then  { Startzeichen SOH gelesen }
                      Modus:=m_Size;  { als Nächstes bis zum Längenbyte lesen }
                  end;
                  rohsatz:='';
                end;

              m_Size:
                begin
                  if length (rohsatz) = 1 then begin  { Längenbyte gelesen }
                    iSize:=Ord (rohsatz [1]);
                    Modus:=m_NumFrame_End;  { als Nächstes Numframe_End-Bytes lesen  }
                    rohsatz:='';
                  end;
                end;

              m_NumFrame_End:
                begin
                  if length (rohsatz) = 2 then begin  { Numframe_End-Bytes gelesen }
                    iNumframe:=Bin2Word (rohsatz) AND $7FFF;
                    if iNumframe = 0 then  { erstes Telegramm }
                      Modus:=m_RecSize_Num  { als Nächstes Recordgröße und Anzahl der Records lesen }
                    else  { alle weiteren Folgetelegramme }
                      Modus:=m_Data;  { als Nächstes der Datenteil des Telegramms }
                    rohsatz:='';
                  end;
                end;

              m_RecSize_Num:
                begin
                  if length (rohsatz) = 3 then begin  { Bytes für Recordgröße und Anzahl der Records gelesen }
                    iRecSize:=Ord (rohsatz [1]);  { Recordgröße }
                    if iRecSize <> iRecSize_Soll then begin  { Soll/Ist-Fehler Recordgröße }
                      Result:=false;
                      Break;
                    end;
                    Modus:=m_Data;   { als Nächstes der Datenteil des Telegramms }
                    rohsatz:='';
                  end;
                end;

              m_Data:
                begin
                  if iNumframe = 0 then
                    iSizeData_Soll:=iSize - 5  { ohne Bytes für Numframe_End und Recordgröße/Anzahl }
                  else
                    iSizeData_Soll:=iSize - 2;  { ohne Bytes für Numframe_End }

                  if length (rohsatz) = (iSizeData_Soll + 3) then begin  { Telegramm-Daten, Endezeichen und 2 CRC-Zeichen gelesen }
                    sData:=sData + Copy (rohsatz, 1, length (rohsatz)-3);  { Datenteil anhängen }
                    Modus:=m_Start;     { weiteres Daten-Folgetelegramm lesen }
                    rohsatz:='';
                    while length (sData) >= iRecSize do begin    { alle vollständig enthaltenen Records jetzt Konvertieren }
                      sDataRec:=Copy (sData, 1, iRecSize);  // einen Datenrecord rauskopieren
                      Delete (sData, 1, iRecSize);  // Rest des Datenteils

                      if Datenart = 0 then begin  // Messwerte konvertieren
                        FillChar (std_satz, SizeOf (std_satz), 0);       { Vorbelegung std_satz; 18.05.2018, WW }
                        with std_satz do begin
                          S:=Copy (sDataRec, 1, 4);  { Date_End: Ende der Messperiode -> Datum }
                          DatumZeit:=Bin2Date_Corus (S);
                          if DatumZeit > 0 then begin
                            { Satzstatus }
                            satzstatus:= $00;
                            if ((Ord (sDataRec [17]) and $08) <> 0) then     { Uhr gestellt-Status: Bit 3 im Status-Byte abfragen }
                              SatzStatus := SatzStatus or $02;

                            { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                            for i:=1 to c_maxKanalZahl do begin
                              kanal[i].wert:=0;
                              KanalOrig[i].wert:=0;
                              if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then
                                kanal[i].kanalstatus:=$A0
                              else                                  { Impulskanal }
                                kanal[i].kanalstatus:=$80;
                              KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                              KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                            end;

                            for i:=1 to CMaxCorus_MwKanaele do begin
                              if i <= c_maxKanalZahl then begin
                                case i of
                                  1: begin  { Unconverted consumption -> Vb, Wieser-Kanal 1 }
                                       S:=Copy (sDataRec, 5, 2);
                                       Wert_orig:=Bin2Word (S);
                                     end;

                                  2: begin  { Converted consumption -> Vn, Wieser-Kanal 2 }
                                       S:=Copy (sDataRec, 7, 4);
                                       cWert:=Bin2Longword (S);
                                       Wert_orig:=cWert / 1000;  // muß durch 1000 geteilt werden
                                     end;

                                  3: begin  { Unconverted counter (Al) -> Vb stör, Wieser-Kanal 3 }
                                       S:=Copy (sDataRec, 11, 2);
                                       Wert_orig:=Bin2Word (S);
                                     end;

                                  4: begin  { Converted counter (Al) -> Vn ges, Wieser-Kanal 4 }
                                       S:=Copy (sDataRec, 13, 4);
                                       cWert:=Bin2Longword (S);
                                       Wert_orig:=cWert / 1000;  // muß durch 1000 geteilt werden
                                     end;

                                  5: begin  { Unconverted consumption LF2 -> Vb kontr, Wieser-Kanal 5 }
                                       S:=Copy (sDataRec, 23, 2);
                                       Wert_orig:=Bin2Word (S);
                                     end;

                                  6: begin  { Average pressure -> p, Wieser-Kanal 6 }
                                       S:=Copy (sDataRec, 21, 2);
                                       Wert_orig:=Bin2Float16_2_Corus (S);
                                     end;

                                  7: begin  { Average temperature -> T, Wieser-Kanal 7 }
                                       S:=Copy (sDataRec, 19, 2);
                                       Wert_orig:=Bin2Float16_1_Corus (S);
                                     end;
                                else
                                  Wert_orig:=0;
                                end;  { case }

                                if (i >= MessKonvRec.Analogkanal_von) AND
                                   (i <= MessKonvRec.Analogkanal_bis) then begin  { Analogkanal }
                                  { physikalischen Analogwert rückrechnen auf LGZ-Rohwert über obere und untere Meßbereichgrenze aus
                                    Stammdaten: }
                                  if MessKonvRec.KanalList <> nil then begin
                                    Delta:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (i)).Daten.MessBereichMax -
                                           TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (i)).Daten.MessBereichMin;
                                    if Delta <> 0 then begin
                                      L:= Round ((Wert_orig - TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (i)).Daten.MessBereichMin) / Delta * 10000);
                                      if L >= 0 then begin
                                        if L > 9999 then
                                          Kanal [i].Wert:=9999
                                          { Fehlend-Bit hier nicht zurücksetzen, um Analogwert-"Überlauf"
                                            behelfsmäßig zu kennzeichnen }
                                        else begin
                                          Kanal [i].Wert:=L;
                                          Kanal [i].KanalStatus := Kanal [i].KanalStatus and $7F; { Fehlend-Bit löschen }
                                        end;
                                      end;
                                    end;
                                  end;
                                end
                                else begin  { Impulskanal }
                                  { Kanalwerte: unbewertete Impulse (keine Rückrechnung erforderlich) }
                                  Wert_norm:=Round (Wert_orig);
                                  if Wert_norm >= 0 then begin
                                    if Wert_norm > High (Kanal [i].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                                      Kanal [i].Wert:=High (Kanal [i].Wert);
                                      Kanal [i].KanalStatus := Kanal [i].KanalStatus or $01;  { Überlauf-Bit setzen }
                                    end else
                                      Kanal [i].Wert:=Wert_norm;
                                    Kanal [i].KanalStatus := Kanal [i].KanalStatus and $7F; { Fehlend-Bit löschen }
                                  end;
                                end;

                                KanalOrig [i].Wert:=Wert_orig;
                                KanalOrig [i].KanalStatus:=KanalOrig [i].KanalStatus and $7F;  { Fehlend-Bit löschen }
                              end;  { if i <= c_maxKanalZahl }
                            end;  { for i }

                            { Stundensatz-Objekt in Zwischenliste anhängen: }
                            RohSRecObj:=TRohSRecObj.Create (std_satz);
                            zwlist.Add (RohSRecObj);
                          end;  { if DatumZeit > 0 }
                        end;   { with std_satz }
                      end

                      else if Datenart = 1 then begin  // Tagessätze konvertieren
                        with tag_satz do begin
                          S:=Copy (sDataRec, 1, 4);  { Date_End: Ende der Messperiode -> Datum }
                          DatumZeit:=Bin2Date_Corus (S);
                          if DatumZeit > 0 then begin
                            { Satzstatus }
                            satzstatus:= $00;
                            if ((Ord (sDataRec [5]) and $08) <> 0) then     { Uhr gestellt-Status: Bit 3 im Status-Byte abfragen }
                              SatzStatus := SatzStatus or $04;

                            { Vorbelegung Zählerstati für fehlend }
                            for i:=1 to c_maxKanalZahl do begin
                              E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                              E_zaehler[i].wert:=0;
                              K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                              K_zaehler[i].wert:=0;
                            end;

                            for i:=1 to CMaxCorus_TaKanaele do begin
                              if i <= c_maxKanalZahl then begin
                                case i of
                                  1: begin  { Unconverted consumption -> Vb, Wieser-Kanal 1 }
                                       S:=Copy (sDataRec, 7, 8);
                                       Wert_orig:=Bin2Index_Corus (S);
                                     end;

                                  2: begin  { Converted consumption -> Vn, Wieser-Kanal 2 }
                                       S:=Copy (sDataRec, 15, 8);
                                       Wert_orig:=Bin2Index_Corus (S);
                                     end;

                                  3: begin  { Unconverted counter (Al) -> Vb stör, Wieser-Kanal 3 }
                                       S:=Copy (sDataRec, 23, 8);
                                       Wert_orig:=Bin2Index_Corus (S);
                                     end;

                                  4: begin  { Converted counter (Al) -> Vn ges, Wieser-Kanal 4 }
                                       S:=Copy (sDataRec, 31, 8);
                                       Wert_orig:=Bin2Index_Corus (S);
                                     end;
                                else
                                  Wert_orig:=0;
                                end;  { case }

                                E_zaehler[i].wert:=Wert_orig;  // Rundung raus; 27.03.2013, WW
                                E_zaehler[i].zaehlerstatus:=
                                  E_zaehler[i].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                              end;  { if i <= c_maxKanalZahl }
                            end;  { for i }

                            { Tagessatz-Objekt in Zwischenliste anhängen: }
                            RohTRecExtObj:=TRohTRecExtObj.Create (tag_satz, -1);
                            zwlist.Add (RohTRecExtObj);
                          end;  { if DatumZeit > 0 }
                        end;   { with tag_satz }
                      end;  { if Datenart }
                    end;  { while length (sData) >= iRecSize }
                  end;  { if length (rohsatz) }
                end;
            end;  { case }
          end;   { while fs_roh.Position < FSize_roh }
        finally
          fs_roh.Free;
        end;

        if MessKonvRec.loeschen then
          DeleteFile (MessKonvRec.StdQuelldateiname);  { Rohfile löschen }

{------------ Konvertierung Zwischenlisten -> Zwischendateien -----------------}

        if length (sData) = 0 then begin  { Rohdatenlänge OK }
          if Datenart = 0 then begin  // Messwerte konvertieren
            { MW-Zwischenliste in MW-Zwischenfile konvertieren; Messwerte sind zeitlich
              absteigend enthalten: }
            fs_RohRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (std_satz));
            try
              for i:=zwlist.Count - 1 downto 0 do begin
                Application.ProcessMessages;
                std_satz:=TRohSRecObj (zwlist.Items [i]).RohSRec;
                fs_RohRec.WriteRec (std_satz);
              end;  { for }
            finally
              fs_RohRec.Free;
            end;
          end

          else if Datenart = 1 then begin  // Tagessätze konvertieren
            { ZS-Zwischenliste in ZS-Zwischenfile konvertieren; Zählerstände sind zeitlich
              absteigend enthalten: }
            fs_RohRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
            try
              for i:=zwlist.Count - 1 downto 0 do begin
                Application.ProcessMessages;
                tag_satz:=TRohTRecExtObj (zwlist.Items [i]).RohTRec;
                DecodeTime (tag_satz.DatumZeit, word (time_rec.hour), word (time_rec.min),
                                                word (time_rec.sec), word (time_rec.hsec));
                { Gastag-Umrechnung: }
                with tagesende_rec do begin
                  hour:=MessKonvRec.TagesEnde;
                  min:=0;
                  sec:=0;
                  hsec:=0;
                end;
                if CmpTime (time_rec, tagesende_rec) <= 0 then  // 04.03.2014, WW
                  tag_satz.DatumZeit:=tag_satz.DatumZeit - 1;  { Zählerstand für Gastag }

                fs_RohRec.WriteRec (tag_satz);
              end;  { for }
            finally
              fs_RohRec.Free;
            end;
          end;
        end else
          Result := False;  { Fehler Rohdatenlänge }
      except
        Result := False;
      end;
    finally
      zwlist.Free;
    end;
  end;   { mess_konv }

begin  { main }
  mess_konv (0);  // Messwerte konvertieren
  mess_konv (1);  // Tagessätze konvertieren
  Mess_Konv_Corus:=true;
end;


{---------------------------- Actaris Sparklog --------------------------------}

{---------------------------------------------------------------}
function Mess_Konv_Sparklog (MessKonvRec: TMessTagKonv): Boolean;
{---------------------------------------------------------------}
{ Konvertierung Actaris Sparklog: Es werden die vom Gerät gelieferten Lastgänge
  der einzelnen Kanäle in Stundenwertdaten konvertiert.
  -> Die Anzahl der Elemente pro Teilblock STX..EOT im Rohfile darf beim Abruf
     innerhalb des vorgegebenen Wertebereichs beliebig gewählt werden.
  -> MessKonvRec.StdQuellDateinameListe enthält Einträge mit Format: <Kanalnummer>;<Rohdateiname>
     Fehlender Kanal: Listeneintrag fehlt oder Eintrag mit <Rohdateiname> = Leer-String }

  {---------------------------------}
  function Konv_Sparklog_MW: Boolean;
  {---------------------------------}
  { Messwerte konvertieren }

    {-----------------------------------------------------}
    function Is_Header_Rohsatz (ARohsatz: string): boolean;
    {-----------------------------------------------------}
    { prüft, ob Rohsatz Daten-Header enthält;
      Übergabe: Rohsatz
      Ergebnis: true, wenn Rohsatz Header-Rohsatz ist }
    begin
      Result:=Copy (ARohsatz, 1, 4) = 'P.01';
    end;

    {---------------------------------------------------}
    function Is_Wert_Rohsatz (ARohsatz: string): boolean;
    {---------------------------------------------------}
    { prüft, ob Rohsatz einen Lastgangwert enthält;
      Übergabe: Rohsatz
      Ergebnis: true, wenn Rohsatz Wert-Rohsatz ist }
    begin
      Result:=Copy (ARohsatz, 1, 1) = '(';
    end;

  var
    fs_roh: TFileOfCharStream;       { Rohdatendatei }  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;
    rohsatz: string;
    zeichen: char;
    KanalNr: integer;
    S, S1: string;
    code: integer;
    dtBuf: TDateTime;
    mw_phys: double;
    mw_zwlist: TObjectList;
    std_satz: RohSRec;
    RohSRecObj: TRohSRecObj;
    i, l: integer;
    mw_list_pos: integer;
    mw_lgz: Int64;
    Teiler: double;
    Dezimalstelle: integer;
    gefunden: boolean;
    Rohfilename: string;
    sKanalNr: string;
    iPos: integer;
    dummy: char;
    dtDatumZeit: TDateTime;
    MP_min: integer;
    iStatus: cardinal;

  begin
    Result := True;
    mw_zwlist:=TObjectList.Create;           { Zwischenliste für Messwerte }
    try

  {-------------- Konvertierung Rohdateien -> Zwischenliste -------------------}

      try
        for l:=0 to MessKonvRec.StdQuellDateiNameListe.Count-1 do begin  { alle Messwert-Rohdateien abarbeiten }
          S:=MessKonvRec.StdQuellDateinameListe [l];
          sKanalNr:=F_Zerlegen (S, ';');  { Kanalnummer bis zum Strichpunkt }
          KanalNr:=StrToInt (sKanalNr);  { Kanalnummer }
          Rohfilename:=S;  { Rohfilename }
          if length (Rohfilename) = 0 then Continue;
          if not FileExists (Rohfilename) then begin
            Result := False;
            Continue;
          end;

          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;

            mw_list_pos:=0;  { Vorbelegung Stundensatz-Listenposition: Listenanfang }
            dtDatumZeit:=-1;  { Vorbelegung Datum/Zeit für Lastgangwerte: undefiniert }
            MP_min:=-1;  { Vorbelegung Messperiodenlänge: undefiniert }
            iStatus:=0;  { Vorbelegung Status: OK }
            while fs_roh.Position < FSize_roh do begin
              rohsatz:='';
              zeichen:=NUL;
              { Datensätze bilden: bis EOT, ETX oder LF lesen }
              while (zeichen <> ETX) AND (zeichen <> EOT) AND (zeichen <> LF) AND
                    (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                if (zeichen <> CR) AND (zeichen <> LF) AND
                   (zeichen <> ETX) AND (zeichen <> EOT) then
                  rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne CR, LF, ETX, EOT) }

                if (zeichen = ETX) OR (zeichen = EOT) then begin
                  if (fs_roh.Position < FSize_roh) then
                    fs_roh.Read (dummy);  { das auf ETX und EOT folgende BCC überlesen }
                end;
              end;

              Application.ProcessMessages;

              { Falls STX im Rohsatz vorhanden ist, interessiert nur der nachfolgende Datenteil: }
              iPos:=Pos (STX, rohsatz);
              if iPos > 0 then
                rohsatz:=Copy (rohsatz, iPos + 1, length (rohsatz));

              { Header mit Zeitstempel und Messperiodenlänge: }
              if Is_Header_Rohsatz (rohsatz) then begin
                S:=ExtractString (rohsatz, '(', ')', 0);  { Zeitstempel oder ERROR }
                if S = 'ERROR' then Break;  { keine Daten vorhanden }

                S1:=Copy (S, 1, 6);  { Datum }
                if EncodeDateStr (S1, 'YYMMDD', dtBuf) then begin
                  dtDatumZeit:=dtBuf;

                  S1:=Copy (S, 7, 6);  { Zeit }
                  if EncodeTimeStr (S1, 'HHMMSS', dtBuf) then
                    dtDatumZeit:=dtDatumZeit + dtBuf
                  else
                    dtDatumZeit:=-1;  { Zeit ungültig }
                end else
                  dtDatumZeit:=-1;  { Datum ungültig }

                S:=ExtractString (rohsatz, '(', ')', 1);  { Status (Hex) }
                S:=Copy (S, 1, 8);  { max. 32 Bit }
                Val ('$' + S, iStatus, Code);

                S:=ExtractString (rohsatz, '(', ')', 2);  { Messperiodenlänge in min }
                Val (S, MP_min, Code);
                if Code <> 0 then
                  MP_min:=-1;  { Messperiodenlänge ungültig }
              end

              { Lastgangwert konvertieren in MW-Zwischenliste -> Meßwerte }
              else if Is_Wert_Rohsatz (rohsatz) then begin
                { nur wenn Zeitstempel und Messperiodenlänge gültig sind, darf der
                  Wert weiterverarbeitet werden: }
                if (dtDatumZeit > -1) AND (MP_min > -1) then begin
                  S:=ExtractString (rohsatz, '(', ')', 0);  { Wert, physikalisch }
                  Val (S, mw_phys, Code);
                  if Code = 0 then begin  { gültiger Wert }
                    RohSRecObj:=nil;

                    gefunden:=Search_MW_ZwList_DatumZeit (mw_zwlist, mw_list_pos, dtDatumZeit, false);  // Rohdaten/Liste zeitlich aufsteigend
                    if gefunden then begin
                      { Eintrag für Datum, Zeit in MW-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
                      if (mw_list_pos >= 0) AND (mw_list_pos < mw_zwlist.Count) then
                        RohSRecObj:=TRohSRecObj (mw_zwlist.Items [mw_list_pos]);
                    end
                    else begin
                      { nicht gefunden: neues MW-Zwischenlisten-Objekt erzeugen }
                      FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
                      with std_satz do begin
                        if (iStatus AND $20) <> 0 then  { Bit5: Geräteuhr wurde gestellt }
                          satzstatus:=$02   { Uhr gestellt }
                        else
                          satzstatus:=$00;  { OK }

                        { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                        for i:=1 to c_maxKanalZahl do begin
                          kanal[i].wert:=0;
                          KanalOrig[i].wert:=0;
                          kanal[i].kanalstatus:=$80;  { nur Impulskanäle }
                          KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                          KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                        end;
                        DatumZeit:=dtDatumZeit;
                      end;  { with std_satz }
                      RohSRecObj:=TRohSRecObj.Create (std_satz);
                    end;

                    if Assigned (RohSRecObj) then begin
                      { Kanaldaten setzen: }
                      with RohSRecObj.RohSRec do begin
                        if (KanalNr >= 1) AND (KanalNr <= c_maxKanalZahl) then begin
                          KanalOrig [KanalNr].Wert:=mw_phys;  { Original-Rohwert }
                          KanalOrig [KanalNr].KanalStatus:=KanalOrig [KanalNr].KanalStatus and $7F;       { Fehlend-Bit löschen }

                          { physikalischen Impulswert rückrechnen auf LGZ-Rohwert über
                            Geräteparameter "Impulseingangskonstante" und "Dezimalstelle
                            der Impulseingangskonstante" (Teiler !): }
                          if (MessKonvRec.ParameterListe <> nil) AND
                             (KanalNr >= Low (CP_ACT_SPARKLOG_ImpEingKonst)) AND
                             (KanalNr <= High (CP_ACT_SPARKLOG_ImpEingKonst)) then begin
                            if MessKonvRec.ParameterListe.GetValue (CP_ACT_SPARKLOG_ImpEingKonst [KanalNr], S) then begin
                              Val (S, Teiler, Code);
                              if MessKonvRec.ParameterListe.GetValue (CP_ACT_SPARKLOG_DezImpEingKonst [KanalNr], S) then begin
                                Val (S, Dezimalstelle, Code);
                                Teiler:=Teiler / IntPower (10, Dezimalstelle);
                                if Teiler > 0 then begin
                                  mw_lgz := Round (mw_phys * Teiler);
                                  if mw_lgz >= 0 then begin
                                    if mw_Lgz > High (Kanal [KanalNr].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                                      Kanal [KanalNr].Wert:=High (Kanal [KanalNr].Wert);
                                      Kanal [KanalNr].KanalStatus:=Kanal [KanalNr].KanalStatus or $01;        { Überlauf-Bit setzen }
                                    end else
                                      Kanal [KanalNr].Wert:=mw_lgz;
                                    Kanal [KanalNr].KanalStatus := Kanal [KanalNr].KanalStatus and $7F;       { Fehlend-Bit löschen }
                                  end;
                                end;
                              end;
                            end;
                          end;
                        end;
                      end;  { RohSRecObj.RohSRec }

                      { nicht gefunden: Objekt-Eintrag in MW-Zwischenliste an mw_list_pos eintragen }
                      if not gefunden then
                        mw_zwlist.insert (mw_list_pos, RohSRecObj);
                    end;  { if Assigned (RohSRecObj) }
                  end;  { if Code = 0 }
                  inc (mw_list_pos);

                  { Zeitstempel für nächsten Wert setzen: }
                  dtDatumZeit:=IncMinute (dtDatumZeit, MP_min);
                end;  { if (dtDatumZeit > -1) AND (MP_min > -1) }
              end;
            end;  { while fs_roh.Position < FSize_roh }
          finally
            fs_roh.Free;
          end;

          if MessKonvRec.loeschen then
            DeleteFile (Rohfilename);  { Rohfile löschen }
        end;  { for l }

  {--------------- Konvertierung Zwischenliste -> Zwischendatei ---------------}

        fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (std_satz));
        try
          for i:=0 to mw_zwlist.Count - 1 do begin
            Application.ProcessMessages;
            std_satz:=TRohSRecObj (mw_zwlist.Items [i]).RohSRec;
            fs_RohSRec.WriteRec (std_satz);
          end;  { for }
        finally
          fs_RohSRec.Free;
        end;
      except
        Result:=false;
      end;
    finally;
      mw_zwlist.Free;
    end;
  end;  { Konv_Sparklog_MW }


  {---------------------------------}
  function Konv_Sparklog_ZS: Boolean;
  {---------------------------------}
  { Zählerstände konvertieren }

    {---------------------------------------------------------}
    function Is_ZS_Vorwert_Rohsatz (ARohsatz: string): boolean;
    {---------------------------------------------------------}
    { prüft, ob Rohsatz Vorwert-Zählerstand (monatl. Zählerstand mit Rückstellnummer) enthält;
      Übergabe: Rohsatz
      Ergebnis: true, wenn Rohsatz Vorwert-ZS-Rohsatz ist }
    var
      S: string;

    begin
      Result:=false;

      S:=ExtractString (ARohsatz, NUL, '(', 0);  // Kennziffer geht bis zu (
      if Pos (':', S) > 0 then   // Kanal vorhanden
        F_Zerlegen (S, ':');  // alles bis einschließlich Kanal wegschneiden

      if (Copy (S, 1, 5) = '1.8.1') OR           // Wirkenergie +A, Kalt-/Warmwasser
         (Copy (S, 1, 5) = '2.8.1') OR           // Wirkenergie -A
         (Copy (S, 1, 5) = '3.8.1') OR           // Blindenergie +R
         (Copy (S, 1, 5) = '4.8.1') OR           // Blindenergie -R
         (Copy (S, 1, 6) = '23.0.0') OR          // Vb
         (Copy (S, 1, 6) = '23.2.0') then begin  // Vn
        if Pos ('*', S) > 0 then  // Zählerstand mit Rückstellnummer
          Result:=true;
      end;
    end;

    {-----------------------------------------------------------}
    function Is_Zeit_Vorwert_Rohsatz (ARohsatz: string): boolean;
    {-----------------------------------------------------------}
    { prüft, ob Rohsatz Vorwert-Zeitpunkt enthält;
      Übergabe: Rohsatz
      Ergebnis: true, wenn Rohsatz Vorwert-Zeit-Rohsatz ist }
    var
      S: string;

    begin
      Result:=false;

      S:=ExtractString (ARohsatz, NUL, '(', 0);  // Kennziffer geht bis zu (
      if Pos (':', S) > 0 then   // Kanal vorhanden
        F_Zerlegen (S, ':');  // alles bis einschließlich Kanal wegschneiden

      if (Copy (S, 1, 5) = '0.1.2') then   // Zeitpunkt des Vorwertes
        if Pos ('*', S) > 0 then  // Zählerstand mit Rückstellnummer
          Result:=true;
    end;

  var
    fs_roh: TFileOfCharStream;          { Rohdatendatei }  // 06.10.2015, WW
    fs_RohTRec: TFileOfRecStream;       { Tagessätze-Datei in RohTRec-Struktur }
    FSize_roh: integer;
    rohsatz: string;
    zeichen: char;
    KanalNr: integer;
    s: string;
    code: integer;
    zs_zwlist: TObjectList;
    tag_satz: RohTRec;
    RohTRecExtObj: TRohTRecExtObj;  // erweitertes RohTRec-Objekt mit Rückstellnummer
    i, l: integer;
    zs_list_pos: integer;
    dtBuf: TDateTime;
    gefunden: boolean;
    Rohfilename: string;
    realbuf: double;
    hour, min, sec, msec: word;
    iRueckstellNr: integer;
    sBuf: string;
    dtDatumZeit: TDateTime;
    sKanalNr: string;

  begin
    Result := True;
    zs_zwlist:=TObjectList.Create;             { Zwischenliste für Zählerstände }
    try

  {------------- Konvertierung Rohdateien -> Zwischenliste --------------------}

      try
        for l:=0 to MessKonvRec.TagQuellDateiNameListe.Count-1 do begin  { alle ZS-Rohdateien abarbeiten }
          S:=MessKonvRec.TagQuellDateinameListe [l];
          sKanalNr:=F_Zerlegen (S, ';');  { Kanalnummer bis zum Strichpunkt }
          KanalNr:=StrToInt (sKanalNr);  { Kanalnummer }
          Rohfilename:=S;  { Rohfilename }
          if length (Rohfilename) = 0 then Continue;
          if not FileExists (Rohfilename) then begin
            Result := False;
            Continue;
          end;

          // Rückstellnummern in Zwischenliste löschen:
          for i:=0 to zs_zwlist.Count - 1 do
            TRohTRecExtObj (zs_zwlist.Items [i]).RueckstellNr:=-1;  // unbelegt

          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;

            zs_list_pos:=0;               { Vorbelegung Tagsatz-Listenposition }
            zeichen:=NUL;
            { bis zum STX lesen }
            while (zeichen <> STX) AND (fs_roh.Position < FSize_roh) do
              fs_roh.Read (zeichen);

            { Konvertierung }
            while (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
              rohsatz:='';
              zeichen:=NUL;
              while (zeichen <> LF) AND (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) do begin
                fs_roh.Read (zeichen);
                rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
              end;

              Application.ProcessMessages;

              { Rückstellnummern und Zeitpunkte der Vorwerte in ZS-Zwischenliste eintragen }
              if Is_Zeit_Vorwert_Rohsatz (rohsatz) then begin
                s:=ExtractString (rohsatz, '*', '(', 0);
                Val (s, iRueckstellNr, Code);  { Nummer des Vorwerts (Rückstellnummer) }
                if Code <> 0 then
                  iRueckstellNr:=-1;  // Rückstellnummer ungültig

                sBuf:=ExtractString (rohsatz, '(', ')', 0);
                s:=Copy (sBuf, 1, 6);  { Datum }
                if EncodeDateStr (s, 'YYMMDD', dtBuf) then begin
                  dtDatumZeit:=dtBuf;

                  s:=Copy (sBuf, 7, 4);  { Zeit }
                  if EncodeTimeStr (s, 'HHMM', dtBuf) then
                    dtDatumZeit:=dtDatumZeit + dtBuf
                  else
                    dtDatumZeit:=-1;  { Zeit ungültig }
                end else
                  dtDatumZeit:=-1;  { Datum ungültig }

                { nur wenn Rückstellnummer und Zeitstempel gültig sind: }
                if (iRueckstellNr > -1) AND (dtDatumZeit > -1) then begin
                  RohTRecExtObj:=nil;
                  gefunden:=Search_ZS_ZwList_DatumZeit (zs_zwlist, zs_list_pos, dtDatumZeit, true);  // Rohdaten/Liste zeitlich absteigend
                  if gefunden then begin
                    { Eintrag für Datum, Zeit in ZS-Zwischenliste gefunden: Eintrag mit Kanaldaten updaten }
                    if (zs_list_pos >= 0) AND (zs_list_pos < zs_zwlist.Count) then
                      RohTRecExtObj:=TRohTRecExtObj (zs_zwlist.Items [zs_list_pos]);
                  end
                  else begin
                    { nicht gefunden: neues ZS-Zwischenlisten-Objekt erzeugen }
                    FillChar (tag_satz, SizeOf (tag_satz), 0);   { Vorbelegung tag_satz }
                    with tag_satz do begin
                      satzstatus:= $00;  { Satzstatus fest: OK }

                      { Vorbelegung Kanalstatus für fehlend }
                      for i:=1 to c_maxKanalZahl do begin
                        E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                        E_zaehler[i].wert:=0;
                        K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                        K_zaehler[i].wert:=0;
                      end;
                    end;  { with tag_satz }

                    tag_satz.DatumZeit:=dtDatumZeit;
                    RohTRecExtObj:=TRohTRecExtObj.Create (tag_satz, -1);
                  end;

                  if Assigned (RohTRecExtObj) then begin
                    { Rückstellnummer setzen: }
                    RohTRecExtObj.RueckstellNr:=iRueckstellNr;

                    { nicht gefunden: Objekt-Eintrag in ZS-Zwischenliste an zs_list_pos eintragen }
                    if not gefunden then
                      zs_zwlist.insert (zs_list_pos, RohTRecExtObj);
                  end;

                  inc (zs_list_pos);
                end;  { if (iRueckstellNr > -1) AND (dtDatumZeit > -1) then }
              end

              { Vorwert-Zählerstände in Zwischenliste konvertieren: }
              else if Is_ZS_Vorwert_Rohsatz (rohsatz) then begin
                s:=ExtractString (rohsatz, '*', '(', 0);
                Val (s, iRueckstellNr, Code);  { Nummer des Vorwerts (Rückstellnummer) }
                if Code <> 0 then
                  iRueckstellNr:=-1;  // Rückstellnummer ungültig

                s:=ExtractString (rohsatz, '(', ')', 0);  { Wert }
                s:=ExtractString (s, NUL, '*', 0);  { Einheit abschneiden }
                Val (s, realbuf, code);      { Rohwert kann Gleitkommazahl sein }
                { in Tagessatz-Record Zählerstand eintragen:
                  -> nur wenn RückstellNummer gültig ist }
                if (iRueckstellNr > -1) then begin
                  gefunden:=Search_ZS_ZwList_RueckstellNr (zs_zwlist, i, iRueckstellNr);
                  if gefunden then begin
                    { Eintrag für Rückstellnummer in ZS-Zwischenliste gefunden: Eintrag mit Zählerstand updaten }
                    if (i >= 0) AND (i < zs_zwlist.Count) then begin
                      with TRohTRecExtObj (zs_zwlist.Items [i]).RohTRec do begin
                        E_zaehler[KanalNr].wert:=realbuf;  // Rundung raus; 27.03.2013, WW
                        E_zaehler[KanalNr].zaehlerstatus:=
                          E_zaehler[KanalNr].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                      end;
                    end;
                  end;
                end;  { if (iRueckstellNr > -1) then }
              end;  { if Is_Zeit_Vorwert_Rohsatz }
            end;  { while (zeichen <> ETX) AND (fs_roh.Position < FSize_roh) }
          finally
            fs_roh.Free;
          end;

          if MessKonvRec.loeschen then
            DeleteFile (Rohfilename);  { Rohfile löschen }
        end;  { for l }

  {------------ Konvertierung Zwischenliste -> Zwischendatei ------------------}

        { ZS-Zwischenliste in ZS-Zwischenfile konvertieren: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          { ZS-Zwischenliste umgekehrt auslesen: }
          for i:=zs_zwlist.Count - 1 downto 0 do begin
            Application.ProcessMessages;

            tag_satz:=TRohTRecExtObj (zs_zwlist.Items [i]).RohTRec;
            DecodeTime (tag_satz.DatumZeit, hour, min, sec, msec);
            if hour <= MessKonvRec.TagesEnde then
              tag_satz.DatumZeit:=tag_satz.DatumZeit - 1;  { Zählerstand für Gastag }
            fs_RohTRec.WriteRec (tag_satz);
          end;  { for }
        finally
          fs_RohTRec.Free;
        end;
      except
        Result:=false;
      end;
    finally
      zs_zwlist.Free;
    end;
  end;  { Konv_Sparklog_ZS }

begin
  Konv_Sparklog_MW;
  Konv_Sparklog_ZS;
  Result:=true;
end;


{---------------------------- Kamstrup UNIGAS 300 -----------------------------}

{-------------------------------------------------------------}
function Mess_Konv_Unigas (MessKonvRec: TMessTagKonv): boolean;
{-------------------------------------------------------------}
{ Konvertierung Kamstrup UNIGAS 300: Es wirden der vom Gerät gelieferte Lastgang
  (P.01-Archiv, enthält MP-Werte aller Kanäle) in Stundenwertdaten konvertiert.
  -> Die Anzahl der Elemente pro Teilblock STX..EOT im Rohfile darf beim Abruf
     innerhalb des vorgegebenen Wertebereichs beliebig gewählt werden. }

  {-----------------------------------------------------}
  function Is_Header_Rohsatz (ARohsatz: string): boolean;
  {-----------------------------------------------------}
  { prüft, ob Rohsatz Daten-Header enthält;
    Übergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Header-Rohsatz ist }
  begin
    Result:=Copy (ARohsatz, 1, 4) = 'P.01';
  end;

  {---------------------------------------------------}
  function Is_Wert_Rohsatz (ARohsatz: string): boolean;
  {---------------------------------------------------}
  { prüft, ob Rohsatz einen Lastgangwert enthält;
    Übergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Wert-Rohsatz ist }
  begin
    Result:=Copy (ARohsatz, 1, 1) = '(';
  end;

const
  CMaxImpKanaeleUNIGAS300 = 6;

type
  { Struktur zum Merken eines Kanalwerts für Stundenwert-Differenzbildung }
  Tletzt_daten = record
    Datumzeit: TDateTime;
    Wert: double;
    Wert_vorhanden: boolean;
  end;

var
  fs_roh: TFileOfCharStream;       { Rohdatendatei }  // 06.10.2015, WW
  fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
  fs_RohTRec: TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
  FSize_roh: integer;
  rohsatz: string;
  zeichen: char;
  S, S1: string;
  code: integer;
  dtBuf: TDateTime;
  std_satz: RohSRec;
  tag_satz: RohTRec;
  i, k: integer;
  mw_lgz: Int64;
  iPos: integer;
  dummy: char;
  dtDatumZeit: TDateTime;
  MP_min: integer;
  iStatus: cardinal;
  letzt_daten_k: array [1..CMaxImpKanaeleUNIGAS300] of Tletzt_daten;
  wert_k: double;
  Datum: daterec;
  Zeit: timerec;
  datebuf: daterec;
  timebuf: timerec;
  Faktor: double;
  Diff: double;
  SBuf: string;
  ParaOK: boolean;
  ParaNrAllg: string;
  MBoben: double;
  MBunten: double;
  Delta: double;

begin
  Result:=false;
  if not FileExists (MessKonvRec.StdQuelldateiname) then exit;

  Result:=true;
  try
    { Rohfile öffnen: }
    fs_roh:=TFileOfCharStream.Create (MessKonvRec.StdQuelldateiname, fmOpenRead OR fmShareDenyWrite);
    try
      FSize_roh:=fs_roh.Size;

      { Stundenwert-Zielfile erzeugen: }
      fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (std_satz));
      try
        { Tagessatz-Zielfile erzeugen: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (RohTRec));
        try
          { Vorbelegung von Datum/Zeit und Wert der Vorstunden-Kanalwerte für
            Zählerstands-Differenzbildung: Vorstundenwert unbekannt }
          for i:=Low (letzt_daten_k) to High (letzt_daten_k) do begin
            letzt_daten_k [i].Datumzeit:= 0;
            letzt_daten_k [i].Wert:=0;
            letzt_daten_k [i].Wert_vorhanden:=false;
          end;

          dtDatumZeit:=-1;  { Vorbelegung Datum/Zeit für Lastgangwerte: undefiniert }
          MP_min:=-1;  { Vorbelegung Messperiodenlänge: undefiniert }
          iStatus:=0;  { Vorbelegung Status: OK }
          while fs_roh.Position < FSize_roh do begin
            rohsatz:='';
            zeichen:=NUL;
            { Datensätze bilden: bis EOT, ETX oder LF lesen }
            while (zeichen <> ETX) AND (zeichen <> EOT) AND (zeichen <> LF) AND
                  (fs_roh.Position < FSize_roh) do begin
              fs_roh.Read (zeichen);
              if (zeichen <> CR) AND (zeichen <> LF) AND
                 (zeichen <> ETX) AND (zeichen <> EOT) then
                rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne CR, LF, ETX, EOT) }

              if (zeichen = ETX) OR (zeichen = EOT) then begin
                if (fs_roh.Position < FSize_roh) then
                  fs_roh.Read (dummy);  { das auf ETX und EOT folgende BCC überlesen }
              end;
            end;

            Application.ProcessMessages;

            { Falls STX im Rohsatz vorhanden ist, interessiert nur der nachfolgende Datenteil: }
            iPos:=Pos (STX, rohsatz);
            if iPos > 0 then
              rohsatz:=Copy (rohsatz, iPos + 1, length (rohsatz));

            { Header mit Zeitstempel und Messperiodenlänge: }
            if Is_Header_Rohsatz (rohsatz) then begin
              S:=ExtractString (rohsatz, '(', ')', 0);  { Zeitstempel oder ERROR }
              if S = 'ERROR' then Break;  { keine Daten vorhanden }

              { Zeitzonen-Kennzeichen wird nicht ausgewertet }

              S1:=Copy (S, 2, 6);  { Datum }
              if EncodeDateStr (S1, 'YYMMDD', dtBuf) then begin
                dtDatumZeit:=dtBuf;

                S1:=Copy (S, 8, 6);  { Zeit }
                if EncodeTimeStr (S1, 'HHMMSS', dtBuf) then
                  dtDatumZeit:=dtDatumZeit + dtBuf
                else
                  dtDatumZeit:=-1;  { Zeit ungültig }
              end else
                dtDatumZeit:=-1;  { Datum ungültig }

              S:=ExtractString (rohsatz, '(', ')', 1);  { Status (Hex) }
              if length (S) > 8 then
                S:=Copy (S, length (S) - 8 + 1, 8);  { max. 32 Bit }       
              Val ('$' + S, iStatus, Code);

              S:=ExtractString (rohsatz, '(', ')', 2);  { Messperiodenlänge in min }
              Val (S, MP_min, Code);
              if Code <> 0 then
                MP_min:=-1;  { Messperiodenlänge ungültig }
            end

            { Lastgangwerte konvertieren -> Meßwerte }
            else if Is_Wert_Rohsatz (rohsatz) then begin
              { nur wenn Zeitstempel und Messperiodenlänge gültig sind, dürfen die
                Lastgangwerte weiterverarbeitet werden: }
              if (dtDatumZeit > -1) AND (MP_min > -1) then begin
                FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
                with std_satz do begin
                  if (iStatus AND $20) <> 0 then  { Bit5: Geräteuhr wurde gestellt }
                    satzstatus:=$02   { Uhr gestellt }
                  else
                    satzstatus:=$00;  { OK }

                  { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                  for i:=1 to c_maxKanalZahl do begin
                    kanal[i].wert:=0;
                    KanalOrig[i].wert:=0;
                    if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then  { Analogkanal }
                      kanal[i].kanalstatus:=$A0
                    else                                  { Impulskanal }
                      kanal[i].kanalstatus:=$80;
                    KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                    KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                  end;
                  DatumZeit:=dtDatumZeit;
                end;  { with std_satz }

                FillChar (tag_satz, SizeOf (tag_satz), 0);  { Vorbelegung tag_satz }
                with tag_satz do begin
                  satzstatus:= $00;  { Satzstatus OK }
                  if (iStatus AND $40) <> 0 then          { Systemreset }
                    SatzStatus := SatzStatus or $01;
                  if (iStatus AND $80) <> 0 then          { Spannungsausfall }
                    SatzStatus := SatzStatus or $02;
                  { Ein evtl. vorhandener "Uhr gestellt"-Status muß unterdrückt
                    werden, da ansonsten die Aufbereitung den Tagesatz verwirft ! }

                  { Vorbelegung Zählerstati für fehlend }
                  for i:=1 to c_maxKanalZahl do begin
                    E_zaehler[i].zaehlerstatus:=$80;   { Eingangszähler }
                    E_zaehler[i].wert:=0;
                    K_zaehler[i].zaehlerstatus:=$82;   { Kontrollzähler }
                    K_zaehler[i].wert:=0;
                  end;
                  DatumZeit:=dtDatumZeit;
                end;  { with tag_satz }

                DateTimeToRec (dtDatumzeit, Datum, Zeit);
                if Zeit.hour <= MessKonvRec.TagesEnde then
                  tag_satz.DatumZeit:=tag_satz.DatumZeit - 1; { Zählerstand für Gastag }

                { rohsatz in Datensatz-Werte '(..)' aufsplitten }
                i:=0;
                while length (rohsatz) > 0 do begin
                  inc (i);
                  S:=F_Zerlegen (rohsatz, ')');  { lesen bis ')' }

                  S:=ExtractString (S, '(', NUL, 0);  { Wert, physikalisch }
                  with std_satz do begin
                    { i = 1: Vb1     -> Kanal 1 im Stundensatz- und Tagessatz-Zwischenfile
                          2: Vb1 err -> Kanal 2     "
                          3: Vm1     -> Kanal 3     "
                          4: Vc1 err -> Kanal 4     "
                          5: Vm2     -> Kanal 5     "
                          6: Vm3     -> Kanal 6     "
                          7: T       -> Kanal 7 im Stundensatz-Zwischenfile
                          8: p       -> Kanal 8     " }
                    if (i >= 1) AND (i <= 8) then begin
                      Val (S, wert_k, Code);     { Kanalwert aus Gerät (physikalisch !) }
                      if Code = 0 then begin  { gültiger Wert }
                        k:=i;                                        { k = LGZ-Kanal }

                        if (k >= MessKonvRec.Analogkanal_von) AND (k <= MessKonvRec.Analogkanal_bis) then begin { Analogkanal }
                          wert_k:=wert_k / 100;  { Analogwerte kommen als Integer mit 100 multipliziert }
                          KanalOrig [k].Wert:=wert_k;                    { Original-Rohwert }
                          KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }

                          { Stundensatz-Record: vom Gerät gelieferten physikalischen Analogwert rückrechnen auf
                            LGZ-Rohwert über obere und untere Meßbereichgrenze aus Geräteparameter }
                          if MessKonvRec.ParameterListe <> nil then begin
                            ParaOK:=true;
                            if i = 7 then
                               ParaNrAllg:=CP_KAM_UNIGAS300_tMax
                            else
                               ParaNrAllg:=CP_KAM_UNIGAS300_pMax;
                            MBoben:=0.0;
                            if MessKonvRec.ParameterListe.GetValue (ParaNrAllg, SBuf) then begin
                              SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                              Val (SBuf, MBoben, Code);
                            end else
                              ParaOK:=false;
                            if i = 7 then
                               ParaNrAllg:=CP_KAM_UNIGAS300_tMin
                            else
                               ParaNrAllg:=CP_KAM_UNIGAS300_pMin;
                            MBunten:=0.0;
                            if MessKonvRec.ParameterListe.GetValue (ParaNrAllg, SBuf) then begin
                              SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                              Val (SBuf, MBunten, Code);
                            end else
                              ParaOK:=false;

                            if ParaOK then begin
                              Delta:=MBoben - MBunten;
                              if Delta <> 0 then begin
                                mw_lgz:= Round ((wert_k - MBunten) / Delta * 10000);
                                if mw_lgz >= 0 then begin
                                  if mw_lgz > 9999 then
                                    Kanal [k].Wert:=9999
                                    { Fehlend-Bit hier nicht zurücksetzen, um Analogwert-"Überlauf"
                                      behelfsmäßig zu kennzeichnen }
                                  else begin
                                    Kanal [k].Wert:=mw_lgz;
                                    Kanal [k].KanalStatus := Kanal [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                                  end;
                                end;
                              end;
                            end;
                          end;  { if MessKonvRec.ParameterListe <> nil }
                        end
                        else begin                                          { Impulskanal }
                          KanalOrig [k].Wert:=wert_k;                    { Original-Rohwert }
                          KanalOrig [k].KanalStatus := KanalOrig [k].KanalStatus and $7F; { Fehlend-Bit löschen }

                          { Stundensatz-Record: vom Gerät gelieferte physikalische Zählerstände rückrechnen auf
                            normierte Zählerstandsdifferenzen }
                          if (Zeit.Min = 0) AND (Zeit.Sec = 0) then begin  { nur volle Stunden interessieren ! }
                            if letzt_daten_k [k].Wert_vorhanden then begin     { Differenzbildung nur möglich, wenn Vor-Zählerstand bekannt }
                              DateTimeToRec (letzt_daten_k [k].Datumzeit, datebuf, timebuf);
                              P_AddMinuten (datebuf, timebuf, 60);
                              { Vor-Zählerstand muß von Vorstunde sein: }
                              if (CmpDate (datebuf, Datum) + CmpTime (timebuf, Zeit)) = 0 then begin
                                Faktor:=0.0;
                                if k = 5 then begin  { Vm2 }
                                  { physikalischen Impulswert für Vm2 rückrechnen auf LGZ-Rohwert
                                    über Geräteparameter "Teilfaktor Eingang 2": }
                                  if MessKonvRec.ParameterListe <> nil then begin
                                    if MessKonvRec.ParameterListe.GetValue (CP_KAM_UNIGAS300_TeilfaktorEing [2], SBuf) then begin
                                      SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                                      Val (SBuf, Faktor, Code);
                                    end;
                                  end;
                                end
                                else if k = 6 then begin  { Vm3 }
                                  { physikalischen Impulswert für Vm3 rückrechnen auf LGZ-Rohwert
                                    über Geräteparameter "Teilfaktor Eingang 3": }
                                  if MessKonvRec.ParameterListe <> nil then begin
                                    if MessKonvRec.ParameterListe.GetValue (CP_KAM_UNIGAS300_TeilfaktorEing [3], SBuf) then begin
                                      SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                                      Val (SBuf, Faktor, Code);
                                    end;
                                  end;
                                end
                                else begin  { Vb1, Vb1 err, Vm1, Vc1 err }
                                  { physikalischen Impulswert für Vb1, Vb1 err, Vm1, Vc1 err
                                    rückrechnen auf LGZ-Rohwert über Geräteparameter
                                    "Teilfaktor Eingang 1" bzw. "Teilfaktor Eingang 1, HF-Betrieb": }
                                  if MessKonvRec.ParameterListe <> nil then begin
                                    if MessKonvRec.ParameterListe.GetValue (CP_KAM_UNIGAS300_FunktionalitaetEing1, SBuf) then begin
                                      if length (SBuf) > 0 then begin
                                        SBuf:=Copy (SBuf, length (Sbuf), 1);  { LNibble }
                                        if SBuf = '1' then begin  { 1 = HF-Betrieb }
                                          if MessKonvRec.ParameterListe.GetValue (CP_KAM_UNIGAS300_TeilfaktorEing1_HF, SBuf) then begin
                                            SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                                            Val (SBuf, Faktor, Code);
                                          end;
                                        end
                                        else begin
                                          if MessKonvRec.ParameterListe.GetValue (CP_KAM_UNIGAS300_TeilfaktorEing [1], SBuf) then begin
                                            SBuf:=F_Zerlegen (SBuf, '*');       { die Einheit abschneiden }
                                            Val (SBuf, Faktor, Code);
                                          end;
                                        end;
                                      end;
                                    end;
                                  end;
                                end;

                                if Faktor <> 0 then begin
                                  Diff:=wert_k - letzt_daten_k [k].wert;   { Zählerstands-Differenz }
                                  mw_lgz:=Round (Diff / Faktor);
                                  if mw_lgz >= 0 then begin
                                    if mw_lgz > High (Kanal [k].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                                      Kanal [k].Wert:=High (Kanal [k].Wert);
                                      Kanal [k].KanalStatus := Kanal [k].KanalStatus or $01; { Überlauf-Bit setzen }
                                    end else
                                      Kanal [k].Wert:=mw_lgz;
                                    Kanal [k].KanalStatus := Kanal [k].KanalStatus and $7F;  { Fehlend-Bit löschen }
                                  end;
                                end;
                              end;
                            end;

                            { wert_k mit Datum/Zeit merken: }
                            letzt_daten_k [k].Datumzeit:=dtDatumZeit;
                            letzt_daten_k [k].Wert:=wert_k;
                            letzt_daten_k [k].Wert_vorhanden:=true;
                          end;

                          { in Tagessatz-Record gerundete physikalische Zählerstände der
                            Kanäle 1..6 eintragen: }
                          if (k >= 1) AND (k <= c_maxKanalZahl) then begin
                            tag_satz.E_zaehler[k].wert:=wert_k;  // Rundung raus; 27.03.2013, WW
                            tag_satz.E_zaehler[k].zaehlerstatus:=
                              tag_satz.E_zaehler[k].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                          end;
                        end;
                      end;  { if Code = 0 }
                    end;  { if (i >= 1) AND (i <= 8) }
                  end;  { with std_satz }
                end;  { while length (rohsatz) }

                { Datensätze in Stundensatz- und Tagessatz-Zwischendatei schreiben: }
                fs_RohSRec.WriteRec (std_satz);   { Stundensatz in Zwischendatei schreiben }

                { Tagessatz nur in Zwischendatei schreiben, wenn vom Tagesende: }
                if (Zeit.Hour = MessKonvRec.Tagesende) AND
                   (Zeit.Min = 0) AND (Zeit.Sec = 0) then
                  fs_RohTRec.WriteRec (tag_satz);

                { Zeitstempel für nächsten Wert setzen: }
                dtDatumZeit:=IncMinute (dtDatumZeit, MP_min);
              end;  { if (dtDatumZeit > -1) AND (MP_min > -1) }
            end;
          end;  { while fs_roh.Position < FSize_roh }
        finally
          fs_RohTRec.Free;      { Zwischenfile für Tagessätze schließen }
        end;
      finally
        fs_RohSRec.Free;
      end;
    finally
      fs_roh.Free;
    end;

    if MessKonvRec.loeschen then
      DeleteFile (MessKonvRec.StdQuelldateiname);  { Rohfile löschen }
  except
    Result:=false;
  end;
end;


{---------------------------- SICK FLOWSIC500 ---------------------------------}

{-----------------------------------------------------------}
function Mess_Konv_SICK (MessKonvRec: TMessTagKonv): Boolean;
{-----------------------------------------------------------}
{ Konvertierung SICK FLOWSIC500 Messperiodenarchiv-Daten in Stundenwertdaten;
  -> von der Konvertierung erwarteter Aufbau der Rohdaten in Quell-Datei:
     alle abgerufenen Downloadpuffer-Daten aneinandergereiht (ohne 1. Byte (Anzahl
     der Datensätze) und ohne Füllbytes), Reihenfolge der Datensätze chronologisch
     absteigend
  -> Format MessKonvRec.StdQuellDateiname: <Datensatz-Typnummer>;<Rohdateiname> }
type
  { Basis-Datensatz-Struktur des Messperiodenarchivs für alle Typnummern }
  TMPArchivRec_Custom = packed record
    RecordAddr: word;
    RecordID: longword;
    Timestamp: longword;
    UnitIdent: byte;
    RecordStatus: byte;
    DetailStatus: longword;
    Vm: longword;
    VmErr: longword;
    ResolutionVm: shortint;
    Vb: longword;
    VbErr: longword;
    ResolutionVb: shortint;
    QmMax: single;
    QbMax: single;
    pMax: single;
    pMin: single;
    pAvg: single;
    TAvg: single;
    KAvg: single;
    CAvg: single;
    SOSAvg: single;
  end;

  { Erweiterte Datensatz-Struktur des Messperiodenarchivs, Typ 3 }
  TMPArchivRec_Typ3 = packed record
    Custom: TMPArchivRec_Custom;
    ChecksumStatus: word;
  end;

  { Erweiterte Datensatz-Struktur des Messperiodenarchivs, Typ 5 }
  TMPArchivRec_Typ5 = packed record
    Custom: TMPArchivRec_Custom;
    FlowTime: word;
    ChecksumStatus: word;
  end;

var
  fs_roh: TFileOfRecStream;
  std_satz: RohSRec;
  fs_RohSRec: TFileOfRecStream;
  MPArchivRec_Typ3: TMPArchivRec_Typ3;
  MPArchivRec_Typ5: TMPArchivRec_Typ5;
  MPArchivRec_Custom: TMPArchivRec_Custom;
  iRecTyp: word;
  iRecSize: integer;
  iRecPos: integer;
  i: integer;
  satz_OK: boolean;
  sRohfilename: string;
  sRecTyp: string;
  iZeitzone_Minuten: integer;
  iBuf: integer;

begin
  Result:=false;

  sRohfilename:=MessKonvRec.StdQuelldateiName;    { enthält Datensatz-Typnummer und Rohfilename }
  sRecTyp:=F_Zerlegen (sRohfilename, ';');  { Datensatz-Typnummer bis zum Strichpunkt }
  iRecTyp:=StrToInt (sRecTyp);
  if length (sRohfilename) = 0 then exit;
  if not FileExists (sRohfilename) then exit;

  try
    // Es werden die Datensatztypen 3 und 5 unterstützt:
    case iRecTyp of
      3: iRecSize:=SizeOf (TMPArchivRec_Typ3);
      5: iRecSize:=SizeOf (TMPArchivRec_Typ5);
    else
      exit;
    end;

    // Aktuelle Zeitzone des Geräts aus Parameterliste lesen:
    // -> Zur Umrechnung der UTC-Zeitstempel der Datensätze in lokale Zeit
    iZeitzone_Minuten:=0;  // Default
    if MessKonvRec.ParameterListe <> nil then
      if MessKonvRec.ParameterListe.GetValueInt (CP_SICK_FLOWSIC500_Zeitzone_Minuten, iBuf) then
        iZeitzone_Minuten:=iBuf;

    { Quellfile öffnen: }
    fs_roh:=TFileOfRecStream.Create (sRohfilename, fmOpenRead OR fmShareDenyWrite, iRecSize);
    try
      { MW-Zwischenfile anlegen: }
      fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname,
        fmCreate, SizeOf (std_satz));
      try
        { Quelldatei rückwärts lesen (Datensätze sind chronologisch absteigend
          enthalten, jüngster zuerst): }
        for iRecPos:=fs_roh.RecCount - 1 downto 0 do begin
          Application.ProcessMessages;
          fs_roh.SeekRec (iRecPos, soFromBeginning);
          case iRecTyp of                       
            3: begin
                 fs_roh.ReadRec (MPArchivRec_Typ3);
                 MPArchivRec_Custom:=MPArchivRec_Typ3.Custom;
               end;
            5: begin
                 fs_roh.ReadRec (MPArchivRec_Typ5);
                 MPArchivRec_Custom:=MPArchivRec_Typ5.Custom;
               end;
          end;

          FillChar (std_satz, SizeOf (std_satz), 0);    { Vorbelegung std_satz }
          with std_satz do begin
            satzstatus:= $00;  { Satzstatus: OK }
            for i:=1 to C_MaxKanalZahl do begin          { Fehlend-Werte für alle Kanäle }
              kanal[i].wert:=0;
              KanalOrig[i].wert:=0;
              if (i >= MessKonvRec.Analogkanal_von) AND
                 (i <= MessKonvRec.Analogkanal_bis) then  { Analogkanal }
                kanal[i].kanalstatus:=$A0
              else                                  { Impulskanal }
                kanal[i].kanalstatus:=$80;
              KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
              KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer
            end;

            { Zeitstempel }
            UnixSekundenToDateTime (MPArchivRec_Custom.Timestamp, DatumZeit);  // liegt in UTC vor
            // In lokale Zeit umrechnen (Zeitzone, DST):
            if (MPArchivRec_Custom.RecordStatus AND $04) <> 0 then  { Bit 2: DST active at the record generation time }
              DatumZeit:=IncHour (DatumZeit);  // DST
            if (MPArchivRec_Custom.RecordStatus AND $08) = 0 then  { Bit 3: Local time (time zone value was 0 at the record generation time) }
              DatumZeit:=IncMinute (DatumZeit, iZeitzone_Minuten);  // Zeitzonen-Minuten

            { Satzstatus }
            if iRecTyp = 5 then begin
              if ((MPArchivRec_Custom.RecordStatus AND $10) <> 0) OR  { Bit 4: Time has been synchronized in period }
                 ((MPArchivRec_Custom.RecordStatus AND $20) <> 0) then  { Bit 5: Time has been set in period }
                SatzStatus:=SatzStatus OR $02;  { -> Uhr gestellt-Bit setzen }

              // Nicht ausgewertet werden:
              // Bit 0: invalid (not accountable, failure)
              // Bit 1: recorded duration invalid (d.h. verkürzte Messperiode, "incomplete")
            end;

            { Kanal-Werte }
            KanalOrig [1].Wert:=MPArchivRec_Custom.Vm;  // Kanal 1: Vb
            KanalOrig [2].Wert:=MPArchivRec_Custom.VmErr;  // Kanal 2: Vb stör
            { Faktor-Rohwert für Vb und Vb stör enthält den Exponent zur Basis 10: }
            KanalOrig [1].Wert:=KanalOrig [1].Wert *
                                exp(MPArchivRec_Custom.ResolutionVm * ln(10));  // 10 hoch Exponent
            KanalOrig [2].Wert:=KanalOrig [2].Wert *
                                exp(MPArchivRec_Custom.ResolutionVm * ln(10));  // 10 hoch Exponent

            KanalOrig [3].Wert:=MPArchivRec_Custom.Vb;  // Kanal 3: Vn
            KanalOrig [4].Wert:=MPArchivRec_Custom.VbErr;  // Kanal 4: Vn stör
            { Faktor-Rohwert für Vn und Vn stör enthält den Exponent zur Basis 10: }
            KanalOrig [3].Wert:=KanalOrig [3].Wert *
                                exp(MPArchivRec_Custom.ResolutionVb * ln(10));  // 10 hoch Exponent
            KanalOrig [4].Wert:=KanalOrig [4].Wert *
                                exp(MPArchivRec_Custom.ResolutionVb * ln(10));  // 10 hoch Exponent

            KanalOrig [5].Wert:=MPArchivRec_Custom.pAvg;  // Kanal 9: Druck
            KanalOrig [6].Wert:=MPArchivRec_Custom.TAvg;  // Kanal 10: Temperatur
            KanalOrig [7].Wert:=MPArchivRec_Custom.KAvg;  // Kanal 11: K-Zahl
            KanalOrig [8].Wert:=MPArchivRec_Custom.CAvg;  // Kanal 12: Z-Zahl
            KanalOrig [9].Wert:=MPArchivRec_Custom.SOSAvg;  // Kanal 13: Schallgeschwindigkeit
            KanalOrig [10].Wert:=MPArchivRec_Custom.QmMax;  // Kanal 5: Qb max
            KanalOrig [11].Wert:=MPArchivRec_Custom.QbMax;  // Kanal 6: Qn max
            KanalOrig [12].Wert:=MPArchivRec_Custom.pMin;  // Kanal 8: Druck min
            KanalOrig [13].Wert:=MPArchivRec_Custom.pMax;  // Kanal 7: Druck max
            for i:=1 to 13 do begin
              KanalOrig [i].KanalStatus:=KanalOrig [i].KanalStatus and $7F; { Fehlend-Bit löschen }
              KanalOrig [i].OrdNr:=MPArchivRec_Custom.RecordID;  // Ordnungsnummer
            end;

            case iRecTyp of
              3: begin
                   // ChecksumStatus (0 = CRC error, 1 = CRC valid)
                   satz_OK:=MPArchivRec_Typ3.ChecksumStatus = 1;
                 end;
              5: begin
                   KanalOrig [14].Wert:=MPArchivRec_Typ5.FlowTime;  // Kanal 14: Durchflusszeit
                   KanalOrig [14].KanalStatus:=KanalOrig [14].KanalStatus and $7F; { Fehlend-Bit löschen }
                   KanalOrig [14].OrdNr:=MPArchivRec_Custom.RecordID;  // Ordnungsnummer

                   // ChecksumStatus (0 = CRC error, 1 = CRC valid)
                   satz_OK:=MPArchivRec_Typ5.ChecksumStatus = 1;
                 end;
            else
              satz_OK:=false;
            end;

            // Normierte LGZ-Werte werden bei diesen Gerätetypen nicht mehr unterstützt.

          end;  { with std_satz }

          if satz_OK then
            fs_RohSRec.WriteRec (std_satz);
        end;
      finally
        fs_RohSRec.Free;
      end;
    finally
      fs_roh.Free;
    end;

    if MessKonvRec.loeschen then
      DeleteFile (sRohfilename);
    Result:=true;
  except
    Result:=false;
    exit;
  end;
end;  { Mess_Konv_SICK }

end.


