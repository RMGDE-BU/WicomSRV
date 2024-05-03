{******************************************************************************}
{* Unit: Langzeitdaten-Konvertierung                                          *}
{* 23.11.1998 WW  abgeleitet von Konvertierung für 16-Bit-Win-DFÜ             *}
{* 08.06.2000 WW  Konvertierung für EC694                                     *}
{* 29.06.2000 WW/H.-P.R. MRG 1009 ohne Tagessätze (Konvertierung 1029)        *}
{* 15.02.2001 WW  zusätzliche Konvertierung von F-Befehl-Rohdaten für MRG3113 *}
{*                und kompatible (-> Eingangszähler fehlen in den E-Befehl-   *}
{*                Rohdaten mancher Gerätetypen)                               *}
{* 17.08.2001 WW  Konvertierung für MRG 910                                   *}
{* 19.02.2003 WW  Konvertierungen jetzt ohne interne Stammdatenzugriffe       *}
{*                (dafür erweiterter Übergabe-Record); Messwerte jetzt in     *}
{*                RohSRec-Struktur mit Original-Kanalwerten                   *}
{* 08.06.2004 WW  Tagessätze jetzt in RohTRec-Struktur mit TDateTime;         *}
{* 23.09.2004 WW  Mess_Konv_3113 angepaßt an Rohdaten aus V24-Auslesung (LF CR*}
{*                statt US als Rohsatz-Trennzeichen)                          *}
{* 11.04.2005 HPR/WW  Korrektur Byte-Überlauf in Messwert-Konvertierungen für *}
{*                MRG 800, MRG 800PTB, EC 694 (Gas-X mit 120 Kanälen !)       *}
{* 15.03.2006 WW  Korrektur MRG 910-Konvertierung für MRG 905                 *}
{* 16.05.2007 WW  Übergabe-Record erweitert: Feld 'bAktZaehler' (für VC2)     *}
{* 12.01.2009 WW  Impulswertebereich für normierte LGZ-Daten vergrößert       *}
{* 04.06.2009 WW  erweitert um Konvertierung für EC 900                       *}
{* 25.01.2010 WW  Anpassungen für RohTRec: Feld 'Wert' als Double             *}
{* 27.03.2013 WW  erweitert um Konvertierung für Kamstrup UNIGAS 300; Rundung *}
{*                der Tagessatz-Werte raus (EC900)                            *}
{* 28.05.2013 WW  Konvertierung für MRG 910 mit k-Faktor                      *}
{* 06.10.2015 WW  Beschleunigte Rohdateizugriffe mit TFileOfCharStream        *}
{* 08.03.2019 WW  Konvertierung für RMG Primus/Prilog (Datenarchiv), Übergabe-*}
{*                Record erweitert: Feld 'MrgTyp' (für Primus/Prilog)         *}
{* 28.09.2020 WW  Mess_Konv_EC900: erweitert um Rückgabe der Archiv-Ordnungs- *}
{*                nummer                                                      *}
{* 18.02.2021 WW  Anpassung Konvertierung Modbus für TME400; Ordnungsnummer-  *}
{*                Defaultwert 'fehlend' geändert auf -1 (wegen TME400)        *}
{* 29.09.2021 WW  erweitert um Konvertierung für SICK FLOWSIC500              *}
{* 11.02.2022 WW  Übergaberecord erweitert um MBAbrufData (für Modbuslisten-  *}
{*                Variante Prilog 400)                                        *}
{* 09.01.2024 WW  Anpassung Konvertierung Modbus für RSM200                   *}
{******************************************************************************}
Unit MLGZKonv;

INTERFACE

Uses
  Forms, Classes, Windows, SysUtils, DateUtils, Contnrs, WChars, WStrUtils, WStream,
  LGZType, T_Tools, T_Zeit, UnixDT, MP_Allg, MP_Imp, MObjPara, MObjList, MLGZKonvList,
  MP_RMG, MResMrg, MResUtil, ModbusUtil, MrgBefehl, WSysCon;

type

  { Übergaberecord für Messwert/Tagesatz-Konvertierung }

  TMessTagKonv = record
    { obligatorisch: }
    KonvGruppe: integer;                 { geräteabh. Konvertierungsgruppe lt. Konfiguration }
    Tagesende: integer;                  { Tagesende im Gerät }
    KanalAktivMaske: string;             { Maske für aktive Kanäle, z.B. '100': Kanal 1 aktiv, Kanäle 2, 3 inaktiv }
    AnzEingZaehler: integer;             { Anzahl der Eingangszähler (gerätetypabhängig) }
    AnzKontrZaehler: integer;            { Anzahl der Kontrollzähler (gerätetypabhängig) }
    AnalogKanal_von: integer;            { Nummer des niedrigsten Analogkanal }
    AnalogKanal_bis: integer;            { Nummer des höchsten Analogkanal }
    StdQuelldateiName: TFileName;        { vollständiger Pfadname der Rohdatei Stundensätze }
    StdQuelldateiNameListe: TTextListe;  { Liste mit vollständigen Pfadnamen aller Rohdateien Stundensätze
                                           -> für Konvertierungen nötig, welche mehrere Rohdateien parallel
                                              verarbeiten, z.B. Elster DL240 }
    TagQuelldateiName: TFileName;        { Pfadname der Rohdatei Tagessätze }
    TagQuelldateiNameListe: TTextListe;  { Liste mit vollständigen Pfadnamen aller Rohdateien Tagessätze
                                           -> für Konvertierungen nötig, welche mehrere Rohdateien parallel
                                              verarbeiten, z.B. Datacon FWU }
    StdZieldateiName: TFileName;         { vollständiger Pfadname der Datei mit strukturierten Stundensätzen }
    TagZieldateiName: TFileName;         { vollständiger Pfadname der Datei mit strukturierten Tagessätzen }
    loeschen: boolean;                   { Rohdateien nach Konvertierung löschen ja/nein }

    { obligatorisch, falls LGZ-Daten erzeugt werden sollen (Rückrechnung phys. Werte
      in normierte LGZ-Werte), für Gerätetyp "Boritec MemoDat" (Kanalzahl),
      für Gerätetyp "MRG 800PTB" (Prüfung der Geräteversion),
      für Gerätetyp "FLOWSIC500" mit von UTC abweichender Zeitzone: }
    ParameterListe: TParameterListe;
    { optional: nur notwendig, wenn LGZ-Daten erzeugt werden sollen }
    AufzMax_Analog: integer;             { Aufzeichnungs-Maximum der Analogkanäle }
    KanalList: TStaKanalKonvDataList;    { Liste mit Kanalinfo aus Stammdaten }
    { optional: nur notwendig, wenn mit IFDEF natGas übersetzt wird }
    Sta_Stationsname: string;            { Stationsname wird für ASCII-Exportdateiname verwendet }
    Select: integer;                     { 1 = Automatik-Daten / 0 = manuelle Daten }
    { optional: nur notwendig, wenn Original-Werte erzeugt werden sollen }
    AnalogOrig_normiert: boolean;        { true: Original-Analogwerte werden auf 10000 normiert (nicht bei
                                                 physikalischen Analogwerten) }

    bAktZaehler: boolean;                { true: aktuelle Zählerstände werden mitkonvertiert
                                                 (Tritschler VC2, Actaris Corus) }
    { optional: nur notwendig, wenn Daten von Geräten konvertiert werden sollen,
      für die per Resource eine Kanalbitmaske hinterlegt ist (z.B. EC 900) } 
    MrgKanalBitMaskList: TMrgKanalBitMaskList;
    { optional: nur notwendig, wenn Daten von Geräten konvertiert werden sollen,
      für die Winter-/Sommerzeit-Umstellungszeitpunkte erforderlich sind (EK260,
      DL210/220/240) }
    WZ_SZKonfigList: TWZ_SZKonfigList;
    { optional: nur notwendig, wenn Modbus-Daten konvertiert werden sollen (Primus/Prilog,
      TME400, FLOWSIC500, RSM200) }
    MrgTyp: integer;
    MBAbrufData: TMBAbrufData; { Konfigurationsdaten für Modbusregister-Konvertierung; 11.02.2022, WW }
  end;

  { Übergaberecord für Prüfsatzkonvertierung }

  TPruefKonv = record
    QuelldateiName: TFileName; { vollständiger Pfadname der Rohdatei }
    ZieldateiName: TFileName;  { vollständiger Pfadname der Datei mit strukturierten Prüfungssätzen }
    loeschen: boolean;         { Rohdatei nach Konvertierung loeschen }
  end;


function MessTag_Konv (MessKonvRec: TMessTagKonv; var fertig: boolean;
  var iMaxMessKanal: integer; var iMinMessKanalExt: integer;
  var iMaxMessKanalExt: integer): Boolean;
function Pruef_Konv (PruefKonvRec: TPruefKonv): Boolean;

IMPLEMENTATION

uses
  MLGZKonvFremd;


const
  L_RohSatz = 255;

type
  Rohsatztyp = string [l_rohsatz];


{------------------------ Hilfsfunktionen -------------------------------------}

{---------------------------------------------------------------------------}
function IsAktivKanal (KanalNr: integer; MessKonvRec: TMessTagKonv): boolean;
{---------------------------------------------------------------------------}
{ Übergabe: MRG-Kanalnummer
            MessKonvRec
  Ergebnis: true, wenn MRG-Kanal aktiv ist }
begin
  Result:=false;
  if (length (MessKonvRec.KanalAktivMaske) >= KanalNr) then begin
    if (MessKonvRec.KanalAktivMaske [KanalNr] = '1') then
      Result:=true;
  end;
end;

{-----------------------------------------------------------------------------}
function IsAktivEingang (KanalNr: integer; MessKonvRec: TMessTagKonv): boolean;
{-----------------------------------------------------------------------------}
{ Übergabe: MRG-Kanalnummer
            MessKonvRec
  Ergebnis: true, wenn MRG-Kanal aktiver Eingangszähler ist }
begin
  Result:=false;
  if MessKonvRec.AnzEingZaehler > 0 then begin
    if (length (MessKonvRec.KanalAktivMaske) >= KanalNr) then begin
      if (MessKonvRec.KanalAktivMaske [KanalNr] = '1') AND (KanalNr <= MessKonvRec.AnzEingZaehler) then
        Result:=true;
    end;
  end;
end;

{------------------------------------------------------------------------------}
function IsAktivKontroll (KanalNr: integer; MessKonvRec: TMessTagKonv): boolean;
{------------------------------------------------------------------------------}
{ Übergabe: MRG-Kanalnummer
            MessKonvRec
  Ergebnis: true, wenn MRG-Kanal aktiver Kontrollzähler ist }
begin
  Result:=false;
  if MessKonvRec.AnzKontrZaehler > 0 then begin
    if (length (MessKonvRec.KanalAktivMaske) >= KanalNr) then begin
      if (MessKonvRec.KanalAktivMaske [KanalNr] = '1') AND (KanalNr <= MessKonvRec.AnzKontrZaehler) then
        Result:=true;
    end;
  end;
end;

{---------------------------------------------------------------------}
Procedure P_Datum_Decode_800 (Datum: Word; Var Jahr, Monat, Tag: Word);
{---------------------------------------------------------------------}
Begin
  Jahr  := Word (Datum AND $007F);
  Monat := Word ((Datum AND $0780) SHR 7);
  Tag   := Word ((Datum AND $F800) SHR 11);
End;

{--------------------------------------------------------------------------}
Procedure P_Zeit_Decode_800 (Zeit: Word; Var Reserve, Stunde, Minute: Word);
{--------------------------------------------------------------------------}
Begin
  Reserve := Word ((Zeit AND $F800) SHR 11);
  Stunde  := Word ((Zeit AND $07C0) SHR 6);
  Minute  := Word (Zeit AND $003F);
End;

{----------------------------------------------------------}
function Rohdaten_Analogkanal (cKanalstatus: char): boolean;
{----------------------------------------------------------}
{ Prüfung auf Analogkanal über Kanalstatus der Rohdaten }
begin
  Rohdaten_Analogkanal:=ord(cKanalstatus) AND $10 = 0;      { Bit 4 abfragen }
end;

{---------------------------------------------------}
function F_Val (s: string; var ok: boolean): integer;
{---------------------------------------------------}
var
  d1,d2:integer;
begin
  val(s,d1,d2);
  if d2=0 then f_val:=d1     { Ok! }
  else begin                { Fehler }
    f_val:=0;
    ok:=false;
  end;
end;

{-----------------------------------------------------------------}
function HexToSingle (HexWert: string; var Value: single): boolean;
{-----------------------------------------------------------------}
{ Konvertierung Hex-Float in Single (Analogwert-Rohformat bei EC694) }
var
  hilftext : array [1..4] of byte;
  psingle : ^single;
  st : string;
  code : integer;
begin
  Result:=false;
  st:=copy(HexWert,1,2);
  val('$'+st,hilftext[4],code);
  if code=0 then begin
    st:=copy(HexWert,3,2);
    val('$'+st,hilftext[3],code);
    if code=0 then begin
      st := copy(HexWert,5,2);
      val('$'+st,hilftext[2],code);
      if code=0 then begin
        st := copy(HexWert,7,2);
        val('$'+st,hilftext[1],code);
        if code=0 then begin
          psingle := @hilftext;
          value:= psingle^;
          Result:=true;
        end;
      end;
    end;
  end;
end;

{---------------- geräteabhängige Konvertierungsroutinen ----------------------}

{-----------------------------------------------------------}
function Mess_Konv_3113 (MessKonvRec: TMessTagKonv): Boolean;
{-----------------------------------------------------------}

  {--------------------------}
  function mess_konv: Boolean;       { Messwerte konvertieren }
  {--------------------------}

  const
    MAX_FEHLTAGE = 20;

  var
    fs_roh    : TFileOfCharStream;  { Rohdatei }   // 06.10.2015, WW
    fs_zw_S   : TFileOfRecStream;   { Zwischendatei Std in RohSRec-Struktur }
    fs_zw_T   : TFileOfRecStream;   { Zwischendatei Tag in RohTRec-Struktur }
    fs_RohSRec: TFileOfRecStream;   { Stundenwerte-Datei in RohSRec-Struktur }
    fs_RohTRec: TFileOfRecStream;   { Tagessätze-Datei in RohTRec-Struktur }

    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : string;

    std_satz : RohSRec;
    tag_satz : LGZTRec;
    rohtag_satz: RohTRec;

    ZwSpeicher: array [1..max_fehltage] of LGZTRec;
                        { Zwischenspeicher, um Stunden-Komp.}
                        { mit Uhrzeit aus Stundenwert zu belegen}
    SpeicherZaehler: byte;

    satz_1   : boolean;       { 1.Satz ? }
    posi     : byte;          { Zeichenzeiger im Rohsatz }
    nr       : byte;     { Nummer des Kontrollzählers im Tagessatz }
    i, j     : longint;  { Laufvariable }
    merkdatum: DateRec;  { aus Tagessatz für Stundensatz }
    x        : string[1];
    HilfsStat: byte;     { für Statusverknüpfung mit Vorbelegung }
    zeitbuf  : timerec;

                   { für Datumsrichtung vertauschen  }
    std_satz2: RohSRec;
    zeiger   : longint;
    Ende, Anfang: longint;
    realwert: real; { Zwischenspeicher bei Normierung }
    wert: word;

    { für Umschaltung Kontroll/Eingangszähler }
    first,
    kontroll : boolean;
    merknr   : byte;
    sondersatz:boolean;
    ok : boolean;

    t_neu    : boolean;
    t_merk   : DateRec;
    f_hsec   : boolean;
    s_merk   : boolean;

    { Zwischendateien vor der Umsortierung }
    pZwFileNameStd: array [0..255] of char;
    pZwFileNameTag: array [0..255] of char;

    { Fehlercode für StrToInt-Wandlung }
    error: integer;

    Datum, datum2: DateRec;
    Zeit, zeit2: TimeRec;
    tagsatz_stunden_merk: byte;


    { lokal }

    { Prozedur wird bei der Umsortierung der Stundenwerte benötigt }
    { sucht bis zum 1. Satz nach einem Tagessatz, markiert mit sec = 1 }
    {----------------------------------------------------------}
    procedure GetDateBlock (Ende: longint; var Anfang: longint);
    {----------------------------------------------------------}
    var
      Datensatz  : RohSRec;
      MerkDate   : integer;
      DatumNeu   : boolean;
      DateiAnfang: boolean;
      Hour, Min, Sec, MSec: word;

    begin
      DatumNeu := False;
      DateiAnfang := False;
      if Ende = 0 then
        Anfang := 0
      else
      begin
        fs_zw_S.SeekRec (Ende, soFromBeginning);
        fs_zw_S.ReadRec (Datensatz);
        MerkDate := Trunc (Datensatz.DatumZeit);
        DecodeTime (Datensatz.DatumZeit, Hour, Min, Sec, MSec);
        if Sec = 1 then    { nur 1 StdSatz nach Tagessatz }
        begin
          anfang := ende;
          exit;
        end;
        repeat
          fs_zw_S.SeekRec (-2, soFromCurrent);
          if fs_zw_S.RecPosition = 0 then
            DateiAnfang := true;
          fs_zw_S.ReadRec (Datensatz);
          if MerkDate <> Trunc (Datensatz.DatumZeit) then
            DatumNeu := true;
          DecodeTime (Datensatz.DatumZeit, Hour, Min, Sec, MSec);
        until DateiAnfang or DatumNeu or (Sec = 1);
        if DatumNeu then
          Anfang := fs_zw_S.RecPosition
        else
          Anfang := fs_zw_S.RecPosition - 1;
      end;
    end;


  begin
    Mess_Konv := False;
    if not FileExists (MessKonvRec.StdQuelldateiname) then exit;

    Mess_Konv := True;
    try
      { temporäres Zwischenfile für Stundenwerte erzeugen: }
      GetTempFileName (pchar (ExtractFilePath(MessKonvRec.StdZieldateiname)), 'Std', 0, pZwFileNameStd);
      fs_zw_s:=TFileOfRecStream.Create (string (pZwFileNameStd), fmOpenReadWrite OR fmShareDenyWrite,
                                        SizeOf (RohSRec));
      try
        { temporäres Zwischenfile für Tagessätze erzeugen: }
        GetTempFileName (pchar (ExtractFilePath(MessKonvRec.StdZieldateiname)), 'Tag', 0, pZwFileNameTag);
        fs_zw_t:=TFileOfRecStream.Create (string (pZwFileNameTag), fmOpenReadWrite OR fmShareDenyWrite,
                                          SizeOf (RohTRec));
        try
          { Rohfile öffnen: }
          fs_roh:=TFileOfCharStream.Create (MessKonvRec.StdQuelldateiname, fmOpenRead OR fmShareDenyWrite);
          try
            FSize_roh:=fs_roh.Size;

            satz_1 := true;
            SpeicherZaehler := 1;
            t_neu := false;
            t_merk.year := 1234;
            s_merk := true;
            sondersatz:=false;
            f_hsec:=false;
            tagsatz_stunden_merk:=25;      { Vorbelegung für Stunde des ersten Tagessatzes: "nicht bekannt"
                                             ->  Die Stunde wird normalerweise (d.h wenn vorhanden) aus erstem
                                                 nachfolgenden Stundenwert gebildet ! }
            while fs_roh.Position < FSize_roh do begin
              { Satz bilden }

              rohsatz := '';
              zeichen := #0;
              while (fs_roh.Position < FSize_roh) and (length (rohsatz) <= 255) and
                    (zeichen <> #$1f) and (zeichen <> #$0d) and   // Trennzeichen bei V24-Daten ist CR, 23.09.2004 WW
                    (zeichen <> #$03) do
              begin
                fs_roh.Read (zeichen);
                if zeichen <> #$0a then   // LF bei V24-Daten überlesen, 23.09.2004 WW
                  rohsatz := rohsatz + zeichen;
              end;

              Application.ProcessMessages;
              { Satz konvertieren }

              if length (rohsatz) > 5 then
              begin    { 1.Satz muß gültiger Tagessatz sein }
                if not (rohsatz [1] in ['t', 'n', 'u', 'r', 's', 'T', 'N', 'U', 'R', 'S']) and
                   satz_1 then
                begin
                  Mess_Konv := False;
                  exit;
                end;
                satz_1 := false;

                case rohsatz [1] of
                  't', 'n', 'u', 'r', 's', 'T', 'N', 'U', 'R', 'S':
                  begin   { Tagessatz wandeln }
                    ok := true;
                    with tag_satz do
                    begin
                      posi := 1;
                      satzstatus := ord (rohsatz [posi]);
                      { Satzstatus wird umgeschlüsselt }
                      case satzstatus of
                        ord('t'), ord('T'): satzstatus:=$00;
                        ord('r'), ord('R'): satzstatus:=$01;
                        ord('n'), ord('N'): satzstatus:=$02;
                        ord('u'), ord('U'): satzstatus:=$04;
                        ord('s'), ord('S'): satzstatus:=$08;
                      end;

                      inc(posi);

                      { Jahr wandeln }

                      val (copy (rohsatz, posi, 2), datum.year, error);
                      if error <> 0 then
                        ok := false
                      else
                      begin
                        if datum.year > 80 then
                          datum.year := datum.year + 1900
                        else
                          datum.year := datum.year + 2000;
                      end; { error = 0 }

                      inc(posi,2);

                      { Monat wandeln }

                      val (copy (rohsatz, posi, 2), datum.month, error);
                      if error <> 0 then
                        ok := false;

                      inc(posi,2);

                      { Tag wandeln }

                      val(copy(rohsatz, posi, 2), datum.day, error);
                      if error <> 0 then
                        ok := false;

                      if not ok then
                        merkdatum.year := 0
                      else
                      begin
                        t_neu:=true;
                        merkdatum:=datum; { für Stundensatz }
                        sondersatz:=false;
                        if upcase(rohsatz[1]) <> 'T' then
                        begin
                          sondersatz:=true;
                          t_merk.year:=1234;
                        end;
                      end;

                      stunden:=tagsatz_stunden_merk;  { Vorbelegung: Stunde des vorherigen Tagessatzes
                                                        ->  falls zu diesem Tagessatz keine Stundensätze existieren }
                      { Vorbelegung der Kanalstati }

                      for i := 1 to C_maxKanalZahl do
                        if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then
                        begin
                          E_zaehler [i].zaehlerstatus:=$A0;
                          E_zaehler [i].wert:=0;               { was sonst ? }
                          K_zaehler [i].zaehlerstatus:=$A2;
                          K_zaehler[i].wert:=0;                { was sonst ? }
                        end
                        else                                  { Impulskanal }
                        begin
                          E_zaehler [i].zaehlerstatus:=$80;
                          E_zaehler[i].wert:=0;               { was sonst ? }
                          K_zaehler[i].zaehlerstatus:=$82;
                          K_zaehler[i].wert:=0;               { was sonst ? }
                        end;

                      posi:=10;
                      first:=true;
                      merknr:=0;
                      kontroll:=true;                         { zuerst Kontrollzähler }
                      while posi < length (rohsatz) do
                      begin
                        val (copy (rohsatz, posi, 2), nr, error);
                        if error <> 0 then
                          ok := false;
                        inc (posi,2);
                        if nr in [1..C_maxkanalzahl] then
                        begin        { Nr ok ? }
                          if first then
                          begin
                            first:=false;
                            merknr:=nr;                      { Nummer 1.Zähler merken }
                          end
                          else
                          begin
                            if merknr = nr then
                              kontroll := false;              { 1.Nummer kommt wieder }
                          end;
                          if kontroll then
                          begin   { Kontrollzähler }
                            val (copy (rohsatz, posi, 8), k_zaehler[nr].wert, error);
                            k_zaehler [nr].zaehlerstatus:=
                              k_zaehler[nr].zaehlerstatus and $7F;  { 1. Bit löschen }

                            { 5. Bit löschen, da Voreinstellung evtl. "Analog" war
                             ( bei MRG2100V1 und neuere): }

                            k_zaehler [nr].zaehlerstatus:=
                              k_zaehler [nr].zaehlerstatus AND $DF;

                            if error <> 0 then
                            begin  { wenn Fehler bei val }
                              k_zaehler[nr].wert:=0;
                              k_zaehler[nr].zaehlerstatus:=
                              k_zaehler[nr].zaehlerstatus or $82;  { 1. Bit setzen }
                              Mess_Konv := False;
                            end;
                          end
                          else
                          begin  { Eingangszähler }
                            val(copy(rohsatz, posi, 8), e_zaehler[nr].wert, error);
                            e_zaehler[nr].zaehlerstatus:=
                              e_zaehler[nr].zaehlerstatus and $7F;  { 1. Bit löschen }

                            { 5. Bit löschen, da Voreinstellung evtl. "Analog" war
                              ( z.B. bei MRG2100V1 und neuere): }

                            e_zaehler[nr].zaehlerstatus:=
                              e_zaehler[nr].zaehlerstatus AND $DF;
                            if error <> 0 then
                            begin   { wenn Fehler bei val }
                              e_zaehler[nr].wert:=0;
                              e_zaehler[nr].zaehlerstatus:=
                                e_zaehler[nr].zaehlerstatus or $80;  { 1. Bit setzen }
                              Mess_Konv := False;
                            end;
                          end;
                        end;
                        inc(posi,8);
                      end;

                      if not ok then
                        Mess_Konv := False;

                      if satzstatus=0 then            { 't'- Satz schreiben }
                      begin
                        stunden:=MessKonvRec.Tagesende;
                        tagsatz_stunden_merk:=stunden;  { merken für evtl. nachfolgenden Tagessatz ohne Stundenwerte }
                        p_Vortag(datum);            { für Langzeitspeicherung wie beim Kartenlesen }

                        zeitbuf.hour:=stunden;
                        zeitbuf.min:=0;
                        zeitbuf.sec:=0;
                        zeitbuf.hsec:=0;
                        if RecToDateTime (datum, zeitbuf, rohtag_satz.DatumZeit) then begin
                          rohtag_satz.satzstatus:=tag_satz.satzstatus;
                          for i:=1 to C_maxkanalzahl do begin  // 25.01.2010, WW
                            rohtag_satz.E_Zaehler [i].zaehlerstatus:=tag_satz.E_Zaehler [i].zaehlerstatus;
                            rohtag_satz.E_Zaehler [i].wert:=tag_satz.E_Zaehler [i].wert;
                            rohtag_satz.K_Zaehler [i].zaehlerstatus:=tag_satz.K_Zaehler [i].zaehlerstatus;
                            rohtag_satz.K_Zaehler [i].wert:=tag_satz.K_Zaehler [i].wert;
                          end;  { for i }

                          fs_zw_T.WriteRec (rohtag_satz);
                        end;
                      end
                      else                            { Sondersatz zwischenspeichern }
                      begin
                        ZwSpeicher [SpeicherZaehler] := tag_satz;
                        if speicherzaehler < MAX_FEHLTAGE then
                          inc(SpeicherZaehler);
                      end;
                    end; { with  tag_satz}
                  end;  { Tagessatz konvertieren }


                  '0','1','2':
                  begin             { Stundensatz wandeln }
                    ok:=true;
                    with std_satz do
                    begin
                      if merkdatum.year=0 then
                        Mess_konv := False;

                      Datum:=merkdatum;         { Datum aus Tagessatz }
                      posi:=1;
                      with zeit do begin  { nur Stundeninformation }
                        val(copy(rohsatz, posi, 2), hour, error);
                        if error <> 0 then
                          Mess_Konv := False;
                        min:=0;
                        sec:=0;
                        if t_neu then
                        begin   { 1.Stundensatz nach Tagessatz }
                          t_neu:=false;
                          sec:=1;             { markiert mit sec=1 }

                          { Tagessatz doppelt }

                          if (cmpdate (t_merk,datum) = 0) and (not s_merk) then
                            f_hsec:=true       { kein Vortag }
                          else
                            f_hsec:=false;

                          t_merk:=merkdatum;
                          s_merk:=sondersatz;
                        end;

                        { Sondersatz VOR Tageswechsel }

                        if (hour > MessKonvRec.Tagesende) or (hour = 0) then
                          sondersatz:=false;
                        { Sondersatz zwischen 0 und Tagesende }
                        if sondersatz then
                          hsec:=1
                        else
                          hsec:=0;

                        if f_hsec then
                          hsec:=1;     { Tagessatz doppelt }
                      end;

                      if RecToDateTime (Datum, Zeit, DatumZeit) then begin
                        inc(posi,2);
                        x:=copy(rohsatz,posi,1);              { Satzstatus }
                        satzstatus:=ord(x[1]);
                        satzstatus:=satzstatus and $0F; {Bit 4-7 für Aufbereitung löschen}

                        { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                        for i:=1 to c_maxKanalZahl do
                        begin
                          kanal[i].wert:=0;
                          KanalOrig[i].wert:=0;
                          if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then
                            kanal[i].kanalstatus:=$A0
                          else                                  { Impulskanal }
                            kanal[i].kanalstatus:=$80;
                          KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                          KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                        end;

                        posi:=4;                              { einzelne Kanäle }

                        while posi < length(rohsatz) do
                        begin
                          val(copy(rohsatz, posi, 2), nr, error);
                          if error <> 0 then
                            Mess_konv := False;
                          inc(posi,2);
                          x:=copy(rohsatz,posi,1);            { Kanalstatus lesen }
                          inc(posi);
                          if nr in [1..c_maxKanalZahl] then
                          begin
                            val(copy(rohsatz, posi, 4), wert, error);
                            if error <> 0 then
                              Mess_Konv := False;
                            HilfsStat:=ord(x[1]) and $07;         { Status: Bit 3-7 wegmaskieren }

                            KanalOrig[nr].Wert:=wert;              { Original-Rohwert }
                            KanalOrig[nr].KanalStatus := KanalOrig[nr].KanalStatus or Hilfsstat;  { mit Vorbelegung verknüpfen }
                            KanalOrig[nr].KanalStatus := KanalOrig[nr].KanalStatus and $7F;   { Fehlend-Bit löschen }

                            if (nr >= MessKonvRec.Analogkanal_von) AND (nr <= MessKonvRec.Analogkanal_bis) then begin
                              { zusätzl. Prüfung auf Analogkanal über Kanalstatus wg.
                               MRG 2100V1 und neuere: }
                              if Rohdaten_Analogkanal (x[1]) then begin
                                if MessKonvRec.AufzMax_Analog <> 0 then begin
                                  realwert := wert * (10000 / MessKonvRec.AufzMax_Analog);
                                  kanal[nr].wert:=round(realwert);
                                  kanal[nr].kanalstatus := kanal[nr].kanalstatus or Hilfsstat;  { mit Vorbelegung verknüpfen }
                                  kanal[nr].kanalstatus := kanal[nr].kanalstatus and $7F;   { Fehlend-Bit löschen }

                                  if MessKonvRec.AnalogOrig_normiert then
                                    KanalOrig[nr].Wert:=kanal[nr].wert;  { normierter Original-Analogrohwert }
                                end;
                              end
                              else begin                   { Impulskanal lt. Rohdaten }
                                kanal[nr].wert:=wert;
                                kanal[nr].kanalstatus:=Hilfsstat;   { Fehlend-Bit und Analogkanal-Bit nicht setzen ! }
                                KanalOrig[nr].KanalStatus:=kanal[nr].kanalstatus;   { Kanalstatus für Originalwert anpassen }
                              end;
                            end
                            else begin            { Impulskanal lt. Kanalnummerierung }
                              kanal[nr].wert:=wert;
                              kanal[nr].kanalstatus := kanal[nr].kanalstatus or Hilfsstat;  { mit Vorbelegung verknüpfen }
                              kanal[nr].kanalstatus := kanal[nr].kanalstatus and $7F;   { Fehlend-Bit löschen }
                            end;
                          end;
                          inc(posi,4);
                        end;
                      end;   { of with }

                      { Stundensatz nur bei gültiger Datumsinformation schreiben }

                      if (merkdatum.year<>0) and ok then
                        fs_zw_S.WriteRec (std_satz);     {?}
                    end;  { if RecToDateTime }

                    if SpeicherZaehler > 1 then  { gespeicherte Tagessätze vorhanden }
                    begin
                      if Zeit.hour > 0 then
                        for i:=1 to SpeicherZaehler-1 do
                        begin
                          ZwSpeicher[i].stunden:=Zeit.hour-1;
                          tagsatz_stunden_merk:=ZwSpeicher[i].stunden;  { merken für evtl. nachfolgenden Tagessatz ohne Stundenwerte }
                          if ZwSpeicher[i].stunden in [0..MessKonvRec.Tagesende] then
                            p_Vortag(ZwSpeicher[i].Datum);  { Gasdatum für Sondersätze }

                          zeitbuf.hour:=ZwSpeicher[i].stunden;
                          zeitbuf.min:=0;
                          zeitbuf.sec:=0;
                          zeitbuf.hsec:=0;
                          if RecToDateTime (ZwSpeicher[i].datum, zeitbuf, rohtag_satz.DatumZeit) then begin
                            rohtag_satz.satzstatus:=ZwSpeicher[i].satzstatus;
                            for j:=1 to C_maxkanalzahl do begin  // 25.01.2010, WW
                              rohtag_satz.E_Zaehler [j].zaehlerstatus:=ZwSpeicher[i].E_Zaehler [j].zaehlerstatus;
                              rohtag_satz.E_Zaehler [j].wert:=ZwSpeicher[i].E_Zaehler [j].wert;
                              rohtag_satz.K_Zaehler [j].zaehlerstatus:=ZwSpeicher[i].K_Zaehler [j].zaehlerstatus;
                              rohtag_satz.K_Zaehler [j].wert:=ZwSpeicher[i].K_Zaehler [j].wert;
                            end;  { for j }

                            fs_zw_T.WriteRec (rohtag_satz);
                          end;
                          Application.ProcessMessages;
                        end
                      else
                        for i:=1 to Speicherzaehler-1 do
                        begin
                          ZwSpeicher[i].stunden:=23;
                          tagsatz_stunden_merk:=ZwSpeicher[i].stunden;  { merken für evtl. nachfolgenden Tagessatz ohne Stundenwerte }

                          zeitbuf.hour:=ZwSpeicher[i].stunden;
                          zeitbuf.min:=0;
                          zeitbuf.sec:=0;
                          zeitbuf.hsec:=0;
                          if RecToDateTime (ZwSpeicher[i].datum, zeitbuf, rohtag_satz.DatumZeit) then begin
                            rohtag_satz.satzstatus:=ZwSpeicher[i].satzstatus;
                            for j:=1 to C_maxkanalzahl do begin  // 25.01.2010, WW
                              rohtag_satz.E_Zaehler [j].zaehlerstatus:=ZwSpeicher[i].E_Zaehler [j].zaehlerstatus;
                              rohtag_satz.E_Zaehler [j].wert:=ZwSpeicher[i].E_Zaehler [j].wert;
                              rohtag_satz.K_Zaehler [j].zaehlerstatus:=ZwSpeicher[i].K_Zaehler [j].zaehlerstatus;
                              rohtag_satz.K_Zaehler [j].wert:=ZwSpeicher[i].K_Zaehler [j].wert;
                            end;  { for j }

                            fs_zw_T.WriteRec (rohtag_satz);
                          end;
                          Application.ProcessMessages;
                        end;
                      Speicherzaehler:=1;
                    end;
                  end;      { Stundensatz }
                else
                  Mess_Konv := False; { weder Stunden noch Tagessatz erkannt }
                end; { case Rohsatztyp }
              end; { if length(rohsatz > 5 }
            end; { while not EOF Roh }

            if Speicherzaehler > 1 then begin       { Sondersätze ohne nachfolgende Stundenwerte schreiben }
              for i:=1 to Speicherzaehler-1 do begin
                if tagsatz_stunden_merk <> 25 then begin  { Tagessatz ohne echten Zeitstempel nicht schreiben, WW/HPR 07.04.2003 }
                  ZwSpeicher[i].stunden:=tagsatz_stunden_merk;

                  zeitbuf.hour:=ZwSpeicher[i].stunden;
                  zeitbuf.min:=0;
                  zeitbuf.sec:=0;
                  zeitbuf.hsec:=0;
                  if RecToDateTime (ZwSpeicher[i].datum, zeitbuf, rohtag_satz.DatumZeit) then begin
                    rohtag_satz.satzstatus:=ZwSpeicher[i].satzstatus;
                    for j:=1 to C_maxkanalzahl do begin  // 25.01.2010, WW
                      rohtag_satz.E_Zaehler [j].zaehlerstatus:=ZwSpeicher[i].E_Zaehler [j].zaehlerstatus;
                      rohtag_satz.E_Zaehler [j].wert:=ZwSpeicher[i].E_Zaehler [j].wert;
                      rohtag_satz.K_Zaehler [j].zaehlerstatus:=ZwSpeicher[i].K_Zaehler [j].zaehlerstatus;
                      rohtag_satz.K_Zaehler [j].wert:=ZwSpeicher[i].K_Zaehler [j].wert;
                    end;  { for j }

                    fs_zw_T.WriteRec (rohtag_satz);
                  end;
                  Application.ProcessMessages;
                end;
              end;
            end;
          finally
            fs_roh.Free;
          end;

          if MessKonvRec.loeschen then
            DeleteFile (MessKonvRec.StdQuelldateiname);   { Rohdatei löschen }

{------------------------------------------------------------------------------}

          { Umspeichern in der Reihenfolge }
          { aus Zwischendatei von hinten lesen und in Zieldatei schreiben }
          fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (RohSRec));
          try
            Ende:=fs_zw_S.RecCount-1;
            while Ende>=0 do
            begin
              GetDateBlock(Ende, Anfang);
              fs_zw_S.SeekRec (Anfang, soFromBeginning);
              for i:=Anfang to Ende do
              begin                                   { 1 Tag lesen und kopieren }
                Application.ProcessMessages;
                fs_zw_S.ReadRec (std_satz2);
                if DateTimeToRec (std_satz2.DatumZeit, datum2, zeit2) then begin
                  { Korrektur Gastag -> phys.Tag }

                  if MessKonvRec.Tagesende=0 then
                  begin
                    if (zeit2.hour=0) then
                      P_Nachtag(datum2);
                  end
                  else
                  begin
                    if ((zeit2.hour in [0..MessKonvRec.Tagesende-1]) or
                       ((zeit2.hour=MessKonvRec.Tagesende) and (zeit2.min=0))) AND
                       (zeit2.hsec<>1) then
                      P_Nachtag(datum2);
                  end;

                  { eigentlich Korrektur (logisch !) 1..tagesende, }
                  { aber bisherige Festlegung 0..tagesende wird vorerst beibehalten }

                  zeit2.sec:=0;   { immer }
                  zeit2.hsec:=0;  { immer }

                  if RecToDateTime (datum2, zeit2, std_satz2.DatumZeit) then
                    fs_RohSRec.WriteRec (std_satz2);
                end;
              end;
              Ende:=Anfang-1;
            end;
          finally
            fs_RohSRec.Free;
          end;

          fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (RohTRec));
          try
            for zeiger:=fs_zw_T.RecCount-1 downto 0 do
            begin   { nur Dateiumkehrung }
              Application.ProcessMessages;
              fs_zw_T.SeekRec (zeiger, soFromBeginning);
              fs_zw_T.ReadRec (rohtag_satz);
              fs_RohTRec.WriteRec (rohtag_satz);
            end;
          finally
            fs_RohTRec.Free;
          end;
        finally
          fs_zw_t.Free;
        end;
      finally
        fs_zw_s.Free;
      end;

      if MessKonvRec.loeschen then begin    { temporäre Zwischendateien löschen }
        DeleteFile (string (pZwFileNameStd));
        DeleteFile (string (pZwFileNameTag));
      end;
    except
      Mess_Konv := False;
      exit;
    end;
  end; { Mess_konv }


  {----------------------------------------}
  function Rohfiles_zusammenfassen: boolean;
  {----------------------------------------}
  { Eingangszähler aus separat abgefragtem Tagessatz-Rohfile und
    Stundensatz-Rohfile zu neuer Gesamt-Rohdatei zusammenfassen }
  var
    fs_rohstd: TFileOfCharStream;  // 06.10.2015, WW
    fs_rohtag: TFileOfCharStream;  // 06.10.2015, WW
    fs_rohgesamt: TFileStream;
    pRohGesFileName: array [0..255] of char;
    FSize_rohstd: integer;
    FSize_rohtag: integer;
    zeichen: char;
    rohsatz_std: string;
    rohsatz_tag: string;
    rohsatz_gesamt: string;
    weiterlesen: boolean;
    zusammenfassen_nicht_noetig: boolean;

  begin
    Result := False;
    if not FileExists (MessKonvRec.StdQuelldateiname) OR
       not FileExists (MessKonvRec.TagQuelldateiname) then exit;

    try
      { Stundenwert-Rohfile öffnen: }
      fs_rohstd:=TFileOfCharStream.Create (MessKonvRec.StdQuelldateiname, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_rohstd:=fs_rohstd.Size;

        { Tagessatz-Rohfile öffnen: }
        fs_rohtag:=TFileOfCharStream.Create (MessKonvRec.TagQuelldateiname, fmOpenRead OR fmShareDenyWrite);
        try
          FSize_rohtag:=fs_rohtag.Size;

          { neue Gesamt-Rohdatei mit Inhalten aus Stundenwert-Rohdatei und
            Tagessatz-Rohdatei erzeugen: }
          GetTempFileName (pchar (ExtractFilePath(MessKonvRec.StdQuelldateiname)), 'Roh', 0, pRohGesFileName);
          fs_rohgesamt:=TFileStream.Create (string (pRohGesFileName), fmOpenWrite OR fmShareDenyWrite);
          try
            zusammenfassen_nicht_noetig:=false;
            while (fs_rohstd.Position < FSize_rohstd) AND not zusammenfassen_nicht_noetig do begin
              { Satz bilden }
              rohsatz_std := '';
              zeichen := #0;
              while (fs_rohstd.Position < FSize_rohstd) and (length (rohsatz_std) <= 255) and
                    (zeichen <> #$1f) and (zeichen <> #$0d) and   // Trennzeichen bei V24-Daten ist CR, 23.09.2004 WW
                    (zeichen <> #$03) do begin
                fs_rohstd.Read (zeichen);
                if zeichen <> #$0a then   // LF bei V24-Daten überlesen, 23.09.2004 WW
                  rohsatz_std := rohsatz_std + zeichen;
              end;

              Application.ProcessMessages;

              rohsatz_gesamt:=rohsatz_std;
              if length (rohsatz_std) > 0 then begin
                { Tagessätze: Eingangszähler aus Tagessatz-Rohfile anhängen }
                if rohsatz_std [1] in ['t', 'n', 'u', 'r', 's', 'T', 'N', 'U', 'R', 'S'] then begin

                  { zugehörigen Tagessatz im Tagessatz-Rohfile lesen:
                    Anm.: Es ist möglich, daß im Tagessatz-Rohfile mehr Tagessätze enthalten
                          sind als im Stundensatz-Rohfile, nämlich dann, wenn das Stundensatz-Rohfile
                          noch vor und das Tagessatz-Rohfile bereits nach dem Tageswechsel
                          abgerufen wurde. Diese Tagessätze stehen im Tagessatz-Rohfile
                          am Anfang und müssen übersprungen werden, um die richtige Zuordnung
                          zu den Tagessätzen im Stundensatz-Rohfile sicherzustellen. }
                  rohsatz_tag := '';
                  weiterlesen:=true;
                  while weiterlesen AND (fs_rohtag.Position < FSize_rohtag) do begin
                    { Satz bilden }
                    rohsatz_tag := '';
                    zeichen := #0;
                    while (fs_rohtag.Position < FSize_rohtag) and (length (rohsatz_tag) <= 255) and
                          (zeichen <> #$1f) and (zeichen <> #$0d) and   // Trennzeichen bei V24-Daten ist CR, 23.09.2004 WW
                          (zeichen <> #$03) do begin
                      fs_rohtag.Read (zeichen);
                      if zeichen <> #$0a then   // LF bei V24-Daten überlesen, 23.09.2004 WW
                        rohsatz_tag := rohsatz_tag + zeichen;
                    end;

                    Application.ProcessMessages;

                    if length (rohsatz_tag) > 5 then begin          { nur Datensätze  }
                      if rohsatz_tag [1] in ['t', 'n', 'u', 'r', 's', 'T', 'N', 'U', 'R', 'S'] then begin
                        { zu rohsatz_std gehöriger rohsatz_tag ?
                          -> Statuszeichen und Datum vergleichen }
                        if Copy (rohsatz_std, 1, 7) = Copy (rohsatz_tag, 1, 7) then begin
                          weiterlesen:=false;

                          { Prüfung, ob im Stundensatz-Rohfile nicht schon
                            Eingangs- und Kontrollzähler vorhanden sind. Dann kann das
                            Stundensatz-Rohfile alleine zur Konvertierung herangezogen
                            werden }
                          if length (rohsatz_std) > length (rohsatz_tag) then
                            zusammenfassen_nicht_noetig:=true;
                        end else
                          rohsatz_tag := '';
                      end else  { if rohsatz_std [1] in [... }
                        rohsatz_tag := '';
                    end else  { if length (rohsatz_std) > 5 }
                      rohsatz_tag := '';
                  end;  { while not weiterlesen }

                  if length (rohsatz_tag) > 0 then
                    { in rohsatz_gesamt das abschließende Trennzeichen (US bei DFÜ-Abruf,
                      CR bei V24-Auslesung) wegschneiden, Eingangszähler aus rohsatz_tag
                      anhängen: }
                    rohsatz_gesamt:=Copy (rohsatz_gesamt, 1, length (rohsatz_gesamt)-1) +
                                    Copy (rohsatz_tag, 10, length (rohsatz_tag));
                end;  { if rohsatz_std [1] in [... }
              end;  { if length (rohsatz_std) > 0 }

              { Gesamt-Rohsatz schreiben: }
              if not zusammenfassen_nicht_noetig then
                fs_rohgesamt.Write (rohsatz_gesamt[1], length (rohsatz_gesamt));
            end;  { while not eof (d_rohstd) }

          finally
            fs_rohgesamt.Free;
          end;
        finally
          fs_rohtag.Free;
        end;
      finally
        fs_rohstd.Free;
      end;
    except
      Result:=false;
      exit;
    end;

    { Stundenwert-Rohfile enthält schon Eingangs- und Kontrollzähler: }
    if zusammenfassen_nicht_noetig then begin
      DeleteFile (string (pRohGesFileName));
      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.TagQuelldateiname);
      exit;
    end;

    { nach dem Zusammenfassen den Stundenwert-Quelldateiname ändern für die
      Konvertierung und durch Abruf erzeugte Rohdateien löschen: }
    if MessKonvRec.loeschen then begin
      DeleteFile (MessKonvRec.StdQuelldateiname);
      DeleteFile (MessKonvRec.TagQuelldateiname);
    end;
    MessKonvRec.StdQuelldateiname:=string (pRohGesFilename);
    Result := True;
  end;


begin
  if length (MessKonvRec.TagQuelldateiName) > 0 then
    Rohfiles_zusammenfassen;

  Mess_Konv;
  Mess_konv_3113 := True;
end;



{-------------------------------------------------------------------}
function Mess_Konv_3100 (MessKonvRec: TMessTagKonv;
                         TagesSatzStatusVorhanden: Boolean): Boolean;
{-------------------------------------------------------------------}
var
  OK: Boolean;

  {-------------------------}
  function Tag_Konv: Boolean;    { Tagessätze }
  {-------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohTRec: TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
    FSize_roh: integer;
    rohsatz  : string;
    tag_satz : RohTRec;
    i        : byte;          { Laufvariable }
    PC_datum : daterec;       { aus PC  }
    x        : char;
    first    : boolean; { erste Zeile im Rohfile ? }
    zeichen  : char;
    datum    : daterec;
    zeit     : timerec;

  begin
    Tag_Konv := False;
    if not FileExists (MessKonvRec.TagQuelldateiname) then exit;

    Tag_Konv := True;
    try
      { Tagessatz-Rohfile öffnen: }
      fs_roh:=TFileOfCharStream.Create (MessKonvRec.TagQuelldateiname, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_roh:=fs_roh.Size;

        { Tagessatz-Zielfile erzeugen: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          { Datum hier alt -> neu }
          { aktuelles Datum: }
          DecodeDate(Date, word(PC_datum.Year), word(PC_datum.Month), word(PC_datum.Day));

          first := true;
          while fs_roh.Position < FSize_roh do
          begin
            rohsatz:='';
            zeichen:=#0;
            while (fs_roh.Position < FSize_roh) and (zeichen<>#$1f) and              { Satz bilden }
                  (length(rohsatz)<=l_rohsatz) and (zeichen<>#$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz:=rohsatz+zeichen;
            end;

            Application.ProcessMessages;

            if first then
            begin
              rohsatz := copy(rohsatz, 3, length(rohsatz)-2);
              first := false;
            end;

            if TagesSatzStatusVorhanden then
            begin
              x:=rohsatz[1];                     { 1.Zeichen = Satzstatus }
              rohsatz:=copy(rohsatz,2,255);      { 1.Zeichen abschneiden }
            end
            else
              x:='t';  { kein echter Satzstatus }

            ok:=true;
            if length(rohsatz) > 1 then
              with tag_satz do
              begin
                satzstatus:=ord(x);
                { Satzstatus wird umgeschlüsselt }
                case satzstatus of
                  ord('t'): satzstatus:=$00;
                  ord('r'): satzstatus:=$01;
                  ord('n'): satzstatus:=$02;
                  ord('u'): satzstatus:=$04;
                  ord('s'): satzstatus:=$08;
                end;

                datum.month:=f_val(copy(rohsatz,1,2),ok);            { Monat }
                datum.day:=f_val(copy(rohsatz,3,2),ok);                { Tag }
                if datum.month in [1..PC_datum.month] then
                  datum.year:=PC_datum.year
                else
                  datum.year:=PC_datum.year-1;
                P_vortag(datum);  { Zählerstand für Gastag }

                zeit.hour:=f_val(copy(rohsatz,5,2),ok);               { Stunde }
                zeit.min:=0;
                zeit.sec:=0;
                zeit.hsec:=0;

                { Vorbelegung der Kanalstati }

                for i:=1 to C_maxKanalZahl do
                  if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then
                  begin
                    E_zaehler[i].zaehlerstatus:=$A0;
                    E_zaehler[i].wert:=0;                                            K_zaehler[i].zaehlerstatus:=$A2;
                    K_zaehler[i].wert:=0;
                  end
                  else                                  { Impulskanal }
                  begin
                    E_zaehler[i].zaehlerstatus:=$80;
                    E_zaehler[i].wert:=0;
                    K_zaehler[i].zaehlerstatus:=$82;
                    K_zaehler[i].wert:=0;
                  end;

                RohSatz:=COPY(RohSatz,7,255);  { Monat, Tag, Stunde abschneiden }
                if length(rohsatz) > 1 then begin  { mehr als das US-Trennzeichen entahlten, WW 04.05.2004 }
                  for i := 1 to C_MaxKanalZahl do
                  begin
                    if IsAktivEingang (i, MessKonvRec) then
                    begin
                      e_zaehler[i].wert:=StrToInt(copy(rohsatz,1,8));
                      e_zaehler[i].zaehlerstatus:=
                        e_zaehler[i].zaehlerstatus and $7F; { Bit 7 löschen }
                      RohSatz:=COPY(Rohsatz,9,255);
                    end;
                  end;

                  { Kontrollzählerwandlung }

                  for i := 1 to C_MaxKanalZahl do
                  begin
                    if IsAktivKontroll (i, MessKonvRec) then
                    begin
                      k_zaehler [i].wert := StrToInt(COPY(rohsatz,1,8));
                      k_zaehler [i].zaehlerstatus:=
                        k_zaehler [i].zaehlerstatus and $7F; { Bit 7 löschen }
                      RohSatz:=COPY(Rohsatz,9,255);
                    end;
                  end;
                end;

                if not ok then
                  Tag_Konv := False;    {31}

                if RecToDateTime (datum, zeit, tag_satz.DatumZeit) then
                  fs_RohTRec.WriteRec (tag_satz);             { Zwischendatei schreiben }
              end;   { if Länge > 1, with }
          end;     { while not EOF }
        finally
          fs_RohTRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.TagQuelldateiname);
    except
      Tag_Konv := False;
      exit;
    end;
  end;     { Tag_konv }


  {--------------------------}
  function mess_konv: Boolean;       { Messwerte konvertieren }
  {--------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;
    zeichen  : char;
    rohsatz  : rohsatztyp;
    std_satz : RohSRec;
    posi     : byte;          { Zeichenzeiger im Rohsatz }
    i        : byte;          { Laufvariable }
    PC_datum : daterec;       { aus PC  }
    x        : string[1];
    HilfsStat: byte;
    first    : boolean;
    datum    : DateRec;
    zeit     : TimeRec;
    wert     : word;

  begin
    Mess_Konv := False;
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
          { aktuelles Datum: }
          DecodeDate(Date, word(PC_datum.Year), word(PC_datum.Month), word(PC_datum.Day));

          first := true;
          while fs_roh.Position < FSize_roh do
          begin
            rohsatz:='';
            zeichen:=#0;
            while (fs_roh.Position < FSize_roh) and (zeichen<>#$1f) and (zeichen<>#$03) do      { Satz bilden }
            begin
              fs_roh.Read (zeichen);
              rohsatz:=rohsatz+zeichen;
            end;

            Application.ProcessMessages;

            if first then
            begin
              rohsatz := copy(rohsatz, 3, length(rohsatz)-2);
              first := false;
            end;
            if length(rohsatz) > 1 then
            begin        { gültiger Satz ? }
              ok:=true;
              with std_satz do
              begin
                x:=copy(rohsatz,1,1);              { Satzstatus }
                satzstatus:=ord(x[1]);
                satzstatus:=satzstatus and $0F;   { Bit 4-7 für Aufbereitung löschen }

                with datum,zeit do
                begin
                  month:=f_val(copy(rohsatz,2,2),ok);
                  if month in [1..PC_datum.month] then
                    year:=PC_datum.year
                  else
                    year:=PC_datum.year-1;
                  day:=f_val(copy(rohsatz,4,2),ok);
                  hour:=f_val(copy(rohsatz,6,2),ok);
                  min:=f_val(copy(rohsatz,8,2),ok);
                  sec:=0;
                  hsec:=0;
                end;
                if not RecToDateTime (datum, zeit, DatumZeit) then
                  ok:=false;

                { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                for i:=1 to c_maxKanalZahl do
                begin
                  kanal[i].wert:=0;
                  KanalOrig[i].Wert:=0;
                  if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then
                    kanal[i].kanalstatus:=$A0
                  else                                  { Impulskanal }
                    kanal[i].kanalstatus:=$80;
                  KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                  KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                end;

                posi:=10;                              { einzelne Kanäle }
                i:=0;
                while i < c_maxKanalZahl do
                begin
                  inc(i);
                  if IsAktivKanal (i, MessKonvRec) then
                  begin
                    if posi < length(rohsatz) then
                    begin
                      x:=copy(rohsatz,posi,1);         { Kanalstatus }
                      inc(posi);
                      HilfsStat:=ord(x[1]) and $07;       { Bit 3-7 wegmaskieren }

                      if i < MessKonvRec.Analogkanal_von then begin
                        Kanal [i].Wert := f_val (copy(rohsatz,posi,4),ok);
                        KanalOrig [i].Wert:=Kanal [i].Wert;      { Original-Rohwert }

                        kanal [i].kanalstatus := kanal [i].kanalstatus or Hilfsstat;
                        kanal [i].kanalstatus := kanal [i].kanalstatus and $7F;  { Bit 7 löschen }
                        KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                      end
                      else if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then begin
                        Wert := f_val (copy(rohsatz,posi,4),ok);
                        KanalOrig [i].Wert:=Wert;      { Original-Rohwert }
                        KanalOrig [i].KanalStatus := KanalOrig [i].KanalStatus or Hilfsstat;
                        KanalOrig [i].KanalStatus := KanalOrig [i].KanalStatus and $7F;  { Bit 7 löschen }
                  
                        if MessKonvRec.AufzMax_Analog <> 0 then begin
                          Kanal [i].Wert := Round (Wert * (10000 / MessKonvRec.AufzMax_Analog));
                          Kanal [i].KanalStatus:=KanalOrig [i].KanalStatus;

                          if MessKonvRec.AnalogOrig_normiert then
                            KanalOrig [i].Wert:=Kanal [i].Wert;  { normierter Original-Analogrohwert }
                        end;
                      end;
                      inc(posi,4);
                    end;
                  end;
                end; { while i }
              end;   { of with }
              if not ok then
                Mess_Konv := False;    {32}
              if ok then
                fs_RohSRec.WriteRec (std_satz);
            end;      { Stundensatz }
          end;   { while not EOF Roh }
        finally
          fs_RohSRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.StdQuelldateiname);
    except
      Mess_Konv := False;
      exit;
    end;
  end;   { mess_konv }

begin
  Mess_Konv;
  Tag_Konv;
  Mess_konv_3100 := True;
end;


{-----------------------------------------------------------}
function Mess_Konv_1029 (MessKonvRec: TMessTagKonv): Boolean;
{-----------------------------------------------------------}
const
  KanalNamen='123456789A';
var
  ok: boolean;  { Fehlercode der String->INT Funktion }
                { TRUE=ok   FALSE=Fehler,Wert=0 }

  {-------------------------}
  function tag_konv: Boolean;
  {-------------------------}
  const
    ZeitOffset=17;
    KanalLaenge=9;
    WertLaenge=8;
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohTRec   : TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
    FSize_roh: integer;
    rohsatz  : string;
    tag_satz : RohTRec;
    satzlaenge,
    off,i,j  : byte;          { Laufvariable }
    code     : integer;
    zeichen  : char;
    datum    : daterec;
    zeit     : timerec;

  begin
    Tag_Konv := True;
    { Für MRG 1009: Keine Tagessatzabfrage, deshalb auch kein Rohfile ! }
    if not FileExists (MessKonvRec.TagQuelldateiname) then exit;

    try
      { Tagessatz-Rohfile öffnen: }
      fs_roh:=TFileOfCharStream.Create (MessKonvRec.TagQuelldateiname, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_roh:=fs_roh.Size;

        { Tagessatz-Zielfile erzeugen: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          while fs_roh.Position < FSize_roh do
          begin
            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen<>#$1f) and              { Satz bilden }
                  (length(rohsatz)<=l_rohsatz) and (zeichen<>#$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz:=rohsatz+zeichen;
            end;

            Application.ProcessMessages;

            ok:=true;
            satzlaenge:=length(rohsatz);
            if satzlaenge > 20 then
              with tag_satz do
              begin
                satzstatus:=0;
                datum.year:=f_val(copy(rohsatz,5,2),ok);             { Jahr }
                if datum.year>80 then
                  inc(datum.year,1900)
                else
                  inc(datum.year,2000);
                datum.month:=f_val(copy(rohsatz,3,2),ok);            { Monat }
                datum.day:=f_val(copy(rohsatz,1,2),ok);                { Tag }
                P_vortag(datum);          { Zählerstand für Gastag }

                zeit.hour:=6;             { Stunde }
                zeit.min:=0;
                zeit.sec:=0;
                zeit.hsec:=0;

                { nur t-Sätze beim 1029 }
                for i:=1 to C_maxKanalZahl do
                begin
                  E_zaehler[i].zaehlerstatus:=$80;    { fehlend ! }
                  E_zaehler[i].wert:=0;               { was sonst ? }
                  K_zaehler[i].zaehlerstatus:=$82;    { fehlend ! }
                  K_zaehler[i].wert:=0;               { was sonst ? }
                end;
                j:=0;
                for i:=1 to C_maxKanalZahl do
                begin
                  off:=ZeitOffset+j*KanalLaenge;
                  if satzlaenge >= off then
                  begin
                    if copy(KanalNamen,i,1)=rohsatz[off] then
                    begin
                      val(copy(rohsatz,off+1,WertLaenge),E_zaehler[i].wert,code);
                      E_zaehler[i].zaehlerstatus:=0;
                      inc(j);
                    end;
                  end;
                end;
                for i:=1 to C_maxKanalZahl do
                begin
                  off:=ZeitOffset+j*KanalLaenge;
                  if satzLaenge >= off then
                  begin
                    if copy(KanalNamen,i,1)=rohsatz[off] then
                    begin
                      val(copy(rohsatz,off+1,WertLaenge),K_zaehler[i].wert,code);
                      K_zaehler[i].zaehlerstatus:=0;
                      inc(j);
                    end;
                  end;
                end;
                if not ok then
                  Tag_Konv := False   {31}       { Zwischendatei schreiben }
                else begin
                  if RecToDateTime (datum, zeit, tag_satz.DatumZeit) then
                    fs_RohTRec.WriteRec (tag_satz);
                end;
              end;   { if Länge >1 with }
          end;     { while not EOF }
        finally
          fs_RohTRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.TagQuelldateiname);
    except
      Tag_Konv := False;
      exit;
    end;
  end;     { Tag_konv }


  {--------------------------}
  function mess_konv: Boolean;
  {--------------------------}       { Messwerte konvertieren }
  const
    ZeitOffset=9;   {9}
    KanalLaenge=6;  {6}
    WertLaenge=5;   {4}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;
    rohsatz  : string;
    std_satz : RohSRec;
    i,j,
    satzlaenge        : byte;          { Laufvariable }
    PC_datum : daterec;       { aus PC  }
    off,code:integer;
    zeichen: char;
    datum: DateRec;
    zeit: TimeRec;

  begin
    Mess_Konv := False;
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
          { aktuelles Datum: }
          DecodeDate(Date, word(PC_datum.Year), word(PC_datum.Month), word(PC_datum.Day));

          while fs_roh.Position < FSize_roh do
          begin
            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen<>#$1f) and              { Satz bilden }
                  (length(rohsatz)<=l_rohsatz) and (zeichen<>#$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz:=rohsatz+zeichen;
            end;

            Application.ProcessMessages;

            satzlaenge:=length(rohsatz);
            if satzlaenge > 15 then
            begin        { gültiger Satz ? }
              ok:=true;
              with std_satz do
              begin
                satzstatus:=0;
                with datum,zeit do
                begin
                  month:=f_val(copy(rohsatz,3,2),ok);
                  if month in [1..PC_datum.month] then
                    year:=PC_datum.year
                  else
                    year:=PC_datum.year-1;
                  day:=f_val(copy(rohsatz,1,2),ok);
                  hour:=f_val(copy(rohsatz,5,2),ok);
                  min:=f_val(copy(rohsatz,7,2),ok);
                  sec:=0;
                  hsec:=0;
                end;
                if not RecToDateTime (datum, zeit, DatumZeit) then
                  ok:=false;

                { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                for i:=1 to c_maxkanalzahl do
                begin
                  kanal[i].wert:=0;
                  KanalOrig[i].wert:=0;

                  kanal[i].kanalstatus:=128;
                  KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                  KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                end;

                j:=0;
                for i:=1 to c_maxKanalZahl do with kanal[i] do
                begin
                  off:=ZeitOffset+j*KanalLaenge;
                  if satzlaenge >= off then
                  begin
                    if copy(KanalNamen,i,1)=rohsatz[off] then
                    begin
                      val(copy(rohsatz,off+1,WertLaenge),wert,code);
                      kanalstatus:=0;
                      KanalOrig[i].Wert:=wert;                   { Original-Rohwert }
                      KanalOrig[i].KanalStatus:=KanalStatus;
                      inc(j);
                    end;
                  end;
                end;

              end;   { of with }
              if not ok then
                Mess_Konv := False;    {32}
              if ok then
                fs_RohSRec.WriteRec (std_satz);
            end;      { Stundensatz }
          end;   { while not EOF Roh }
        finally
          fs_RohSRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.StdQuelldateiname);
    except
      Mess_Konv := False;
      exit;
    end;
  end;   { mess_konv }

begin
  mess_konv;
  tag_konv;
  Mess_konv_1029 := True;
end;


{ Messwert- und Zählerstandskonvertierung für Geräte der Konvertierungsgruppe 5 }
{----------------------------------------------------------}
function Mess_Konv_800 (MessKonvRec: TMessTagKonv): Boolean;
{----------------------------------------------------------}

  { Zählerstandskonvertierung }
  {-------------------------}
  function Tag_Konv: Boolean;
  {-------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohTRec   : TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : rohsatztyp;

    tag_satz : RohTRec;

    i        : byte;          { Laufvariable }
    aktiv    : array[1..c_maxkanalzahl] of byte;
    count    : byte;
    L        : LongInt;
    Reserve  : word;
    Min      : word;
    Code: Integer;
    StdBuf: word;
    datum: daterec;
    zeit: timerec;
    iWert: longint;

  begin
    Tag_Konv := False;
    if not FileExists (MessKonvRec.TagQuelldateiname) then exit;

    Tag_Konv := True;
    try
      { Tagessatz-Rohfile öffnen: }
      fs_roh:=TFileOfCharStream.Create (MessKonvRec.TagQuelldateiname, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_roh:=fs_roh.Size;

        { Tagessatz-Zielfile erzeugen: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          FillChar (Aktiv, sizeof (Aktiv), 0);
          count:=0;

          { Anfang wegschneiden und aktive Kanäle bestimmen }

          rohsatz := '';
          zeichen := #0;
          while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and
                (zeichen <> #$03) do
          begin
            fs_roh.Read (zeichen);
            rohsatz := rohsatz + zeichen;
          end;
          rohsatz := Copy (Rohsatz, 3, 3);

          for i := 1 to 3 do
          if rohsatz [i] = '1' then
          begin
            inc (count);
            aktiv [count] := i;
          end;

          { Konvertierung }

          while fs_roh.Position < FSize_roh do
          begin
            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and         { Satz bilden }
                  (zeichen <> #$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz := rohsatz + zeichen;
            end;

            Application.ProcessMessages;

            if length (rohsatz) > 1 then
              with tag_satz do
              begin
                satzstatus := $00;                  { kein echter Satzstatus }

      {$IFDEF ZAEHL14}
                umwstatus:=0;
      {$ENDIF}

                Val ('$' + Copy (Rohsatz, 1, 4), L, Code);
                FillChar (Datum, Sizeof (Datum), 0);
                With Datum Do
                Begin
                  P_Datum_Decode_800 (Word (L), Word (Year), Word (Month), Word (Day));
                  If Year > 80 Then
                    Year := 1900 + Year
                  Else
                    Year := 2000 + Year;
                End;
                P_vortag (datum);  { Zählerstand für Gastag }

                Val ('$' + Copy (Rohsatz, 5, 4), L, Code);
                P_Zeit_Decode_800 (Word (L), Reserve, StdBuf, Word (Min));
                zeit.hour:=StdBuf;               { Stunde }
                zeit.min:=0;
                zeit.sec:=0;
                zeit.hsec:=0;

                { Vorbelegung der Kanalstati und -werte }

                for i := 1 to c_maxKanalZahl do
                begin
                  E_zaehler[i].zaehlerstatus:=$80;
                  E_zaehler[i].wert:=0;
                  K_zaehler[i].zaehlerstatus:=$82;
                  K_zaehler[i].wert:=0;
                end;

                RohSatz := COPY (RohSatz,9,255);

                i:=0;
                while i < c_maxKanalZahl do
                begin   { Eingangszählerwandlung }
                  inc(i);
                  if IsAktivEingang (i, MessKonvRec) and
                     (aktiv [i] > 0) then
                  begin
                    Val ('$' + Copy (Rohsatz, 1, 8), iWert, Code);
                    e_zaehler[aktiv[i]].wert:=iWert;
                    e_zaehler[aktiv[i]].zaehlerstatus:=
                      e_zaehler[aktiv[i]].zaehlerstatus and $7F; { Bit 7 löschen }
                    RohSatz:=COPY(Rohsatz,9,255);
                  end;
                end; { of while i }

                i:=0;
                while i < c_maxKanalZahl do
                begin   { Kontrollzählerwandlung }
                  inc(i);
                  if IsAktivKontroll (i, MessKonvRec) and
                     (aktiv [i] > 0) then
                  begin
                    Val ('$' + Copy (Rohsatz, 1, 8), iWert, Code);
                    k_zaehler[aktiv[i]].wert:=iWert;
                    k_zaehler[aktiv[i]].zaehlerstatus:=
                      k_zaehler[aktiv[i]].zaehlerstatus and $7F; { Bit 7 löschen }
                    RohSatz:=COPY(Rohsatz,9,255);
                  end;
                end; { of while i }

                if RecToDateTime (datum, zeit, tag_satz.DatumZeit) then
                  fs_RohTRec.WriteRec (tag_satz);             { Zwischendatei schreiben }
              end;   { if Länge >1 with }
          end;     { while not EOF }
        finally
          fs_RohTRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.TagQuelldateiname);
    except
      Tag_Konv := False;
      exit;
    end;
  end;     { Tag_Konv }


  { Messwerte konvertieren }
  {--------------------------}
  function Mess_Konv: Boolean;
  {--------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : rohsatztyp;

    std_satz : RohSRec;

    posi     : byte;          { Zeichenzeiger im Rohsatz }
    i        : byte;          { Laufvariable }
    aktiv    : array[1..c_maxkanalzahl] of byte;
    count    : byte;
    L        : LongInt;
    Reserve  : Word;
    Code     : Integer;
    Datum    : DateRec;
    Zeit     : TimeRec;

  begin
    Mess_Konv := false;
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
          FillChar (Aktiv, sizeof (Aktiv), 0);
          count:=0;

          { Anfang wegschneiden und aktive Kanäle bestimmen }

          rohsatz := '';
          zeichen := #0;
          while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and
                (zeichen <> #$03) do
          begin
            fs_roh.Read (zeichen);
            rohsatz := rohsatz + zeichen;
          end;
          rohsatz := Copy (Rohsatz, 3, 3);

          for i := 1 to 3 do
            if rohsatz [i] = '1' then
            begin
              inc (count);
              aktiv [count]:=i;
            end;

          while fs_roh.Position < FSize_roh do
          begin
            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and            { Satz bilden }
                  (zeichen <> #$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz := rohsatz + zeichen;
            end;

            Application.ProcessMessages;

            if length (rohsatz) > 1 then
            begin        { gültiger Satz ? }
              with std_satz do
              begin
                satzstatus:= $00;                      { kein echter Satzstatus }

                Val ('$' + Copy (Rohsatz, 1, 4), L, Code);
                FillChar (Datum, Sizeof (Datum), 0);
                With Datum Do
                Begin
                  P_Datum_Decode_800 (Word (L), Word (Year), Word (Month), Word (Day));
                  If Year > 80 Then
                    Year := 1900 + Year
                  Else
                    Year := 2000 + Year;
                End;

                Val ('$' + Copy (Rohsatz, 5, 4), L, Code);
                FillChar (Zeit, Sizeof (Zeit), 0);
                With Zeit Do
                  P_Zeit_Decode_800 (Word (L), Reserve, Word (Hour), Word (Min));

                if RecToDateTime (Datum, Zeit, DatumZeit) then begin
                  { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                  for i:=1 to c_maxKanalZahl do
                  begin
                    kanal[i].wert:=0;
                    KanalOrig[i].wert:=0;
                    kanal[i].kanalstatus:=$80;                     { Impulskanal }
                    KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                    KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                  end;

                  posi:=9;                              { einzelne Kanäle }
                  i:=0;
                  while i < c_maxKanalZahl do
                  begin
                    inc(i);
                    if posi < length(rohsatz) then
                    begin
                      if IsAktivKanal (i, MessKonvRec) and (aktiv [i] > 0) then
                      begin
                        Val ('$' + Copy (Rohsatz, posi, 4), kanal[aktiv[i]].wert, Code);
                        KanalOrig[aktiv[i]].wert:=kanal[aktiv[i]].wert;  { Original-Rohwert }

                        kanal[aktiv[i]].kanalstatus :=
                          kanal[aktiv[i]].kanalstatus and $7F;  { Bit 7 löschen }
                        KanalOrig[aktiv[i]].KanalStatus:=Kanal[aktiv[i]].KanalStatus;
                      end;
                      inc (posi,4);
                    end else
                      Break;  // nach Rohsatzende raus, sonst Byte-Überlauf posi
                              // bei 120 Kanälen (Gas-X),; 11.04.2005 HPR/WW
                 end; { while i }
                  fs_RohSRec.WriteRec (std_satz);
                end;  { if RecToDateTime }
              end;  { with Stundensatz }
            end;  { if length }
          end;   { while not EOF Roh }
        finally
          fs_RohSRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.StdQuelldateiname);
    except
      Mess_Konv := False;
      exit;
    end;
  end;   { mess_konv }


begin  { main }
  mess_konv;
  tag_konv;
  Mess_Konv_800:=true;
end;


{-----------------------------------------------------------}
function Mess_Konv_2001 (MessKonvRec: TMessTagKonv): Boolean;
{-----------------------------------------------------------}
var
  ok       : boolean;  { Fehlercode der String->INT Funktion }
				           { TRUE=ok   FALSE=Fehler,Wert=0 }

  {-------------------------}
  function Tag_Konv: Boolean;    { Tagessätze }
  {-------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohTRec   : TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : rohsatztyp;

    tag_satz : RohTRec;

    i        : byte;          { Laufvariable }
    PC_datum : daterec;       { aus PC  }
    x        : char;
    datum    : daterec;
    zeit     : timerec;

  begin
    Tag_Konv := False;
    if not FileExists (MessKonvRec.TagQuelldateiname) then exit;

    Tag_Konv := True;
    try
      { Tagessatz-Rohfile öffnen: }
      fs_roh:=TFileOfCharStream.Create (MessKonvRec.TagQuelldateiname, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_roh:=fs_roh.Size;

        { Tagessatz-Zielfile erzeugen: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          { aktuelles Datum: }
          DecodeDate(Date, word(PC_datum.Year), word(PC_datum.Month), word(PC_datum.Day));

          {***  Anfang wegschneiden ***}
          fs_roh.Read (zeichen);
          fs_roh.Read (zeichen);

          while fs_roh.Position < FSize_roh do
          begin
            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen<>#$1f) and              { Satz bilden }
                  (zeichen<>#$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz:=rohsatz+zeichen;
            end;

            Application.ProcessMessages;

          x:='t';  { kein echter Satzstatus }

          ok:=true;
          if length(rohsatz) > 1 then
            with tag_satz do
            begin
              satzstatus:=ord(x);
      {$IFDEF ZAEHL14}
              umwstatus:=0;
      {$ENDIF}

              { Satzstatus wird umgeschlüsselt }
              case satzstatus of
                ord('t'): satzstatus:=$00;
                ord('r'): satzstatus:=$01;
                ord('n'): satzstatus:=$02;
                ord('u'): satzstatus:=$04;
                ord('s'): satzstatus:=$08;
              end;

              datum.month:=f_val(copy(rohsatz,1,2),ok);            { Monat }
              datum.day:=f_val(copy(rohsatz,3,2),ok);                { Tag }
              if datum.month in [1..PC_datum.month] then
                datum.year:=PC_datum.year
              else
                datum.year:=PC_datum.year-1;

              P_vortag(datum);  { Zählerstand für Gastag }

              zeit.hour:=f_val(copy(rohsatz,5,2),ok);               { Stunde }
              zeit.min:=0;
              zeit.sec:=0;
              zeit.hsec:=0;

              { Vorbelegung der Kanalstati }

              for i:=1 to C_maxKanalZahl do
                if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then
                begin
                  E_zaehler[i].zaehlerstatus:=$A0;
                  E_zaehler[i].wert:=0;
                  K_zaehler[i].zaehlerstatus:=$A2;
                  K_zaehler[i].wert:=0;
                end
                else                                  { Impulskanal }
                begin
                  E_zaehler[i].zaehlerstatus:=$80;
                  E_zaehler[i].wert:=0;
                  K_zaehler[i].zaehlerstatus:=$82;
                  K_zaehler[i].wert:=0;
                end;

                RohSatz:=COPY(RohSatz,7,255);

                for i := 1 to C_MaxKanalZahl do
                begin
                  if IsAktivEingang (i, MessKonvRec) then
                  begin
                    e_zaehler[i].wert:=  StrToInt (copy(rohsatz,1,8));
                    e_zaehler[i].zaehlerstatus:=
                    e_zaehler[i].zaehlerstatus and $7F; { Bit 7 löschen }
                    RohSatz:=COPY(Rohsatz,9,255);
                  end;
                end;

                for i := 1 to C_MaxKanalZahl do
                begin
                  if IsAktivKontroll (i, MessKonvRec) then
                  begin
                    k_zaehler[i].wert:= StrToInt(COPY(rohsatz,1,8));
                    k_zaehler[i].zaehlerstatus:=
                    k_zaehler[i].zaehlerstatus and $7F; { Bit 7 löschen }
                    RohSatz:=COPY(Rohsatz,9,255);
                  end;
                end; { of for i }

                if not ok then
                  Tag_Konv := False;

                if RecToDateTime (datum, zeit, tag_satz.DatumZeit) then
                  fs_RohTRec.WriteRec (tag_satz);             { Zwischendatei schreiben }
              end;   { if Länge >1 with }
            end;     { while not EOF }
        finally
          fs_RohTRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.TagQuelldateiname);
    except
      Tag_Konv := False;
      exit;
    end;
  end;     { Tag_konv }


  {--------------------------}
  function Mess_Konv: Boolean;       { Messwerte konvertieren }
  {--------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : rohsatztyp;

    std_satz : RohSRec;

    posi     : byte;          { Zeichenzeiger im Rohsatz }
    i        : byte;          { Laufvariable }
    PC_datum : daterec;       { aus PC  }
    x        : string[1];
    HilfsStat: byte;
    Datum    : DateRec;
    Zeit     : TimeRec;
    wert     : word;

  begin
    Mess_Konv := false;
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
          { aktuelles Datum: }
          DecodeDate(Date, word(PC_datum.Year), word(PC_datum.Month), word(PC_datum.Day));

          {***  Anfang wegschneiden ***}
          fs_roh.Read (zeichen);
          fs_roh.Read (zeichen);

          while fs_roh.Position < FSize_roh do
          begin
            rohsatz:='';
            zeichen:=#0;
            while (fs_roh.Position < FSize_roh) and (zeichen<>#$1f) and              { Satz bilden }
                  (zeichen<>#$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz:=rohsatz+zeichen;
            end;

            Application.ProcessMessages;

            if length(rohsatz)>1 then
            begin        { gültiger Satz ? }
              ok:=true;
              with std_satz do
              begin
                x:=copy(rohsatz,1,1);              { Satzstatus }
                satzstatus:=ord(x[1]);
                satzstatus:=satzstatus and $0F;   { Bit 4-7 für Aufbereitung löschen }
                with datum,zeit do
                begin
                  month:=f_val(copy(rohsatz,2,2),ok);
                  if month in [1..PC_datum.month] then
                    year:=PC_datum.year
                  else
                    year:=PC_datum.year-1;
                  day:=f_val(copy(rohsatz,4,2),ok);
                  hour:=f_val(copy(rohsatz,6,2),ok);
                  min:=f_val(copy(rohsatz,8,2),ok);
                  sec:=0;
                  hsec:=0;
                end;
                if not RecToDateTime (datum, zeit, DatumZeit) then
                  ok:=false;

                { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                for i:=1 to c_maxKanalZahl do
                begin
                  kanal[i].wert:=0;
                  KanalOrig[i].wert:=0;

                  if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then
                    kanal[i].kanalstatus:=$A0
                  else                                  { Impulskanal }
                    kanal[i].kanalstatus:=$80;
                  KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                  KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                end;

                posi:=10;                              { einzelne Kanäle }
                i:=0;

                while i < c_maxKanalZahl do
                begin
                  inc(i);
                  if posi < length(rohsatz) then
                  begin
                    x:=copy(rohsatz,posi,1);         { Kanalstatus }
                    HilfsStat:=ord(x[1]) and $07;       { Bit 3-7 wegmaskieren }

                    if IsAktivKanal (i, MessKonvRec) then
                    begin
                      inc(posi);
                      if i < MessKonvRec.Analogkanal_von then begin
                        kanal [i].wert := f_val(copy(rohsatz,posi,4), ok);
                        KanalOrig [i].Wert:=kanal [i].wert;      { Original-Rohwert }

                        kanal [i].kanalstatus := kanal [i].kanalstatus or Hilfsstat;
                        kanal [i].kanalstatus := kanal[i].kanalstatus and $7F;  { Bit 7 löschen }
                        KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                      end
                      else if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then begin
                        wert := f_val(copy(rohsatz,posi,4), ok);
                        KanalOrig [i].Wert:=wert;      { Original-Rohwert }
                        KanalOrig [i].KanalStatus := KanalOrig [i].KanalStatus or Hilfsstat;
                        KanalOrig [i].KanalStatus := KanalOrig [i].KanalStatus and $7F;  { Bit 7 löschen }

                        if MessKonvRec.AufzMax_Analog <> 0 then begin
                          Kanal [i].Wert := Round (wert * (10000 / MessKonvRec.AufzMax_Analog));
                          Kanal [i].KanalStatus:=KanalOrig [i].KanalStatus;

                          if MessKonvRec.AnalogOrig_normiert then
                            KanalOrig [i].Wert:=Kanal [i].Wert;  { normierter Original-Analogrohwert }
                        end;
                      end;
                      inc(posi,4);
                    end;
                  end;
                end; { while i }
              end;   { of with }

              if not ok then
                Mess_konv := False;
              if ok then
                fs_RohSRec.WriteRec (std_satz);
            end;      { Stundensatz }
          end;   { while not EOF Roh }
        finally
          fs_RohSRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.StdQuelldateiname);
    except
      Mess_Konv := False;
      exit;
    end;
  end;   { mess_konv }

begin  { main }
  mess_konv;
  tag_konv;
  Mess_Konv_2001 := True;
end;


{-------------------------------------------------------------}
function Mess_Konv_800PTB (MessKonvRec: TMessTagKonv): Boolean;
{-------------------------------------------------------------}

  { Zählerstandskonvertierung }
  {-------------------------}
  function Tag_Konv: Boolean;
  {-------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohTRec   : TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : rohsatztyp;

    tag_satz : RohTRec;

    i        : byte;          { Laufvariable }
    aktiv    : array[1..c_maxkanalzahl] of byte;
    count    : byte;
    L        : LongInt;
    Reserve  : Word;
    Min      : word;
    Code: Integer;
    StdBuf: word;
    GerVersion: string;
    datum: daterec;
    zeit: timerec;
    iWert: longint;

  begin
    Tag_Konv := False;
    if not FileExists (MessKonvRec.TagQuelldateiname) then exit;

    Tag_Konv := True;
    try
      { Tagessatz-Rohfile öffnen: }
      fs_roh:=TFileOfCharStream.Create (MessKonvRec.TagQuelldateiname, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_roh:=fs_roh.Size;

        { Tagessatz-Zielfile erzeugen: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          { wenn Parameterliste vorhanden:
            prüfen, ob die Rohdaten auch wirklich von einem MRG 800PTB stammen
            31.10.2002, WW/SM: }
          if MessKonvRec.ParameterListe <> nil then
            MessKonvRec.ParameterListe.GetValue (CP_ALLG_Geraeteversion, GerVersion)
          else
            GerVersion:='800';
          if Pos ('800', GerVersion) > 0 then begin
            FillChar (Aktiv, sizeof (Aktiv), 0);
            count:=0;

            { Anfang wegschneiden und aktive Kanäle bestimmen }

            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and
                  (zeichen <> #$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz := rohsatz + zeichen;
            end;
            rohsatz := Copy (Rohsatz, 3, 3);

            for i := 1 to 3 do
            if rohsatz [i] = '1' then
            begin
              inc (count);
              aktiv [count] := i;
            end;

            { Konvertierung }

            while (fs_roh.Position < FSize_roh) and (zeichen <> #$03) do
            begin
              rohsatz := '';
              zeichen := #0;
              while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and         { Satz bilden }
                    (zeichen <> #$03) do
              begin
                fs_roh.Read (zeichen);
                rohsatz := rohsatz + zeichen;
              end;

              Application.ProcessMessages;

              if length (rohsatz) > 1 then
                with tag_satz do
                begin
                  Val ('$' + Copy (Rohsatz, 1, 4), L, Code);
                  FillChar (Datum, Sizeof (Datum), 0);
                  With Datum Do
                  Begin
                    P_Datum_Decode_800 (Word (L), Word (Year), Word (Month), Word (Day));
                    If Year > 80 Then
                      Year := 1900 + Year
                    Else
                      Year := 2000 + Year;
                  End;

                  Val ('$' + Copy (Rohsatz, 5, 4), L, Code);
                  P_Zeit_Decode_800 (Word (L), Reserve, StdBuf, Word (Min));
                  zeit.hour:=StdBuf;               { Stunde }
                  zeit.min:=0;
                  zeit.sec:=0;
                  zeit.hsec:=0;

                  if zeit.hour <= MessKonvRec.TagesEnde then
                    P_vortag (datum);  { Zählerstand für Gastag }

                  { Satzstatus }
                  satzstatus := $00;
                  if ((Reserve and $01) <> 0) then          { Spannungsausfall }
                    SatzStatus := SatzStatus or $02;
                  if ((Reserve and $02) <> 0) then;         { Uhr gestellt }
                  { 14.08.2001 WW; der "Uhr gestellt"-Status muß beim MRG800PTB unterdrückt
                                   werden, da er in den "normalen" Tagessätzen, die am
                                   Tagesende geschrieben werden, steht und nicht in einem
                                   Sondertagessatz. Ansonsten verwirft die Aufbereitung
                                   den normalen Tagesatz ! }

        {$IFDEF ZAEHL14}
                  UmwStatus := 0;
        {$ENDIF}

                  { Vorbelegung der Kanalstati und -werte }
                  for i := 1 to c_maxKanalZahl do
                  begin
                    E_zaehler[i].zaehlerstatus:=$80;
                    E_zaehler[i].wert:=0;
                    K_zaehler[i].zaehlerstatus:=$82;
                    K_zaehler[i].wert:=0;
                  end;

                  RohSatz := copy (RohSatz, 9, 255);

                  i:=0;
                  while i < c_maxKanalZahl do
                  begin   { Eingangszählerwandlung }
                    inc(i);
                    if aktiv [i] > 0 then
                    begin
                      Val ('$' + Copy (Rohsatz, 1, 8), iWert, Code);
                      e_zaehler[aktiv[i]].wert:=iWert;
                      e_zaehler[aktiv[i]].zaehlerstatus:=
                        e_zaehler[aktiv[i]].zaehlerstatus and $7F; { Bit 7 löschen }
                      RohSatz := copy (Rohsatz, 9, 255);
                    end;
                  end; { of while i }

                  i:=0;
                  while i < c_maxKanalZahl do
                  begin   { Kontrollzählerwandlung }
                    inc(i);
                    if aktiv [i] > 0 then
                    begin
                      Val ('$' + Copy (Rohsatz, 1, 8), iWert, Code);
                      k_zaehler[aktiv[i]].wert:=iWert;
                      k_zaehler[aktiv[i]].zaehlerstatus:=
                        k_zaehler[aktiv[i]].zaehlerstatus and $7F; { Bit 7 löschen }
                      RohSatz:=COPY(Rohsatz,9,255);
                    end;
                  end; { of while i }

                  if RecToDateTime (datum, zeit, tag_satz.DatumZeit) then
                    fs_RohTRec.WriteRec (tag_satz);             { Zwischendatei schreiben }
                end;   { if Länge >1 with }
            end;     { while not EOF }
          end;  { if Pos ('800', GerVersion) > 0 }
        finally
          fs_RohTRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.TagQuelldateiname);
    except
      Tag_Konv := False;
      exit;
    end;
  end;     { Tag_Konv }


  { Messwerte konvertieren }
  {--------------------------}
  function Mess_Konv: Boolean;
  {--------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : rohsatztyp;

    std_satz : RohSRec;

    posi     : byte;          { Zeichenzeiger im Rohsatz }
    i        : byte;          { Laufvariable }
    aktiv    : array[1..c_maxkanalzahl] of byte;
    count    : byte;
    L        : LongInt;
    Teiler   : Double;
    Faktor   : Double;
    Reserve  : Word;
    Code     : Integer;
    GerVersion: string;
    Datum    : DateRec;
    Zeit     : TimeRec;

  begin
    Mess_Konv := false;
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
          { wenn ParameterListe vorhanden:
            prüfen, ob die Rohdaten auch wirklich von einem MRG 800PTB stammen
            -> MRG 910-Rohdaten besitzen eine andere Struktur. Die 800er-Konvertierung würde
               in diesem Fall falsche Werte liefern !    31.10.2002, WW/SM }
          if MessKonvRec.ParameterListe <> nil then
            MessKonvRec.ParameterListe.GetValue (CP_ALLG_Geraeteversion, GerVersion)
          else
            GerVersion:='800';
          if Pos ('800', GerVersion) > 0 then begin
            FillChar (Aktiv, sizeof (Aktiv), 0);
            count:=0;

            { Anfang wegschneiden und aktive Kanäle bestimmen }

            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and
                  (zeichen <> #$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz := rohsatz + zeichen;
            end;
            rohsatz := Copy (Rohsatz, 3, 3);

            for i := 1 to 3 do
              if rohsatz [i] = '1' then
              begin
                inc (count);
                aktiv [count]:=i;
              end;

            while (fs_roh.Position < FSize_roh) and (zeichen <> #$03) do
            begin
              rohsatz := '';
              zeichen := #0;
              while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and            { Satz bilden }
                    (zeichen <> #$03) do
              begin
                fs_roh.Read (zeichen);
                rohsatz := rohsatz + zeichen;
              end;

              Application.ProcessMessages;

              if length (rohsatz) > 1 then
              begin        { gültiger Satz ? }
                with std_satz do
                begin
                  Val ('$' + Copy (Rohsatz, 1, 4), L, Code);
                  FillChar (Datum, Sizeof (Datum), 0);
                  With Datum Do
                  Begin
                    P_Datum_Decode_800 (Word (L), Word (Year), Word (Month), Word (Day));
                    If Year > 80 Then
                      Year := 1900 + Year
                    Else
                      Year := 2000 + Year;
                  End;

                  Val ('$' + Copy (Rohsatz, 5, 4), L, Code);
                  FillChar (Zeit, Sizeof (Zeit), 0);
                  With Zeit Do
                    P_Zeit_Decode_800 (Word (L), Reserve, Word (Hour), Word (Min));

                  if RecToDateTime (datum, zeit, DatumZeit) then begin
                    { Satzstatus }
                    satzstatus:= $00;
                    if ((Reserve and $01) <> 0) then             { Uhr gestellt }
                      SatzStatus := SatzStatus or $02;

                    { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                    for i:=1 to c_maxKanalZahl do
                    begin
                      kanal[i].wert:=0;
                      KanalOrig[i].wert:=0;
                      kanal[i].kanalstatus:=$80;                     { Impulskanal }
                      KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                      KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                    end;

                    posi:=9;                              { einzelne Kanäle }
                    i:=0;
                    while i < c_maxKanalZahl do
                    begin
                      inc(i);
                      if posi < length(rohsatz) then
                      begin
                        if aktiv [i] > 0 then
                        begin
                          Val ('$' + Copy (Rohsatz, posi, 6), L, Code);
                          KanalOrig [aktiv [i]].Wert:=L;         { Original-Rohwert }
                          KanalOrig [aktiv [i]].KanalStatus := $00;
                          if (Reserve and ($01 SHL aktiv[i])) <> 0 then
                            KanalOrig [aktiv [i]].KanalStatus := $01;

                          if MessKonvRec.ParameterListe <> nil then begin
                            if MessKonvRec.ParameterListe.GetValueDouble (CP_IMP_Faktor [aktiv [i]], Faktor) AND
                               MessKonvRec.ParameterListe.GetValueDouble (CP_IMP_Teiler [aktiv [i]], Teiler) then begin
                              if Faktor <> 0 then begin
                                Kanal [aktiv [i]].Wert := LoWord (Round (L * Teiler / Faktor));
                                Kanal [aktiv [i]].KanalStatus := KanalOrig [aktiv [i]].KanalStatus;
                              end;
                            end;
                          end;
                        end;
                        inc (posi,6);
                      end else
                        Break;  // nach Rohsatzende raus, sonst Byte-Überlauf posi
                                // bei 120 Kanälen (Gas-X),; 11.04.2005 HPR/WW
                    end;  { while i }
                    fs_RohSRec.WriteRec (std_satz);
                  end;  { if RecToDateTime }
                end;  { with stundensatz }
              end;   { if length }
            end;   { while not EOF Roh }
          end;  { if Pos ('800', GerVersion) > 0 }
        finally
          fs_RohSRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.StdQuelldateiname);
    except
      Mess_Konv := False;
      exit;
    end;
  end;   { mess_konv }


begin  { main }
  mess_konv;
  tag_konv;
  Mess_Konv_800PTB:=true;
end;


{------------------------------------------------------------}
function Mess_Konv_EC694 (MessKonvRec: TMessTagKonv): Boolean;
{------------------------------------------------------------}

  { nur Messwerte konvertieren }
  {--------------------------}
  function Mess_Konv: Boolean;
  {--------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : rohsatztyp;

    std_satz : RohSRec;

    posi     : byte;          { Zeichenzeiger im Rohsatz }
    i        : byte;          { Laufvariable }
    aktiv    : array[1..c_maxkanalzahl] of byte;
    count    : byte;
    L        : Int64;
    Faktor   : Double;
    Code: Integer;
    WertStr: string;
    s: single;
    Delta: double;
    Datum: DateRec;
    Zeit: TimeRec;

  begin
    Mess_Konv := false;
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
          FillChar (Aktiv, sizeof (Aktiv), 0);
          count:=0;

          { Anfang wegschneiden und aktive Kanäle bestimmen }

          rohsatz := '';
          zeichen := #0;
          while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and
                (zeichen <> #$03) do
          begin
            fs_roh.Read (zeichen);
            rohsatz := rohsatz + zeichen;
          end;
          rohsatz := Copy (Rohsatz, 3, 6);

          for i := 1 to 6 do
            if rohsatz [i] = '1' then
            begin
              inc (count);
              aktiv [count]:=i;
            end;

          while (fs_roh.Position < FSize_roh) and (zeichen <> #$03) do
          begin
            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and            { Satz bilden }
                 (zeichen <> #$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz := rohsatz + zeichen;
            end;

            Application.ProcessMessages;

            if length (rohsatz) > 1 then
            begin        { gültiger Satz ? }
              with std_satz do
              begin
                FillChar (Datum, Sizeof (Datum), 0);                                       { Datum }
                With Datum Do Begin
                  Year := StrToInt (Copy (Rohsatz, 1, 2));
                  If Year > 80 Then
                    Year := 1900 + Year
                  Else
                    Year := 2000 + Year;
                  Month := StrToInt (Copy (Rohsatz, 3, 2));
                  Day := StrToInt (Copy (Rohsatz, 5, 2));
                End;

                FillChar (Zeit, Sizeof (Zeit), 0);                                         { Zeit }
                With Zeit Do Begin
                  Hour := StrToInt (Copy (Rohsatz, 7, 2));
                  Min := StrToInt (Copy (Rohsatz, 9, 2));
                  Sec := StrToInt (Copy (Rohsatz, 11, 2));
                End;
                if RecToDateTime (datum, zeit, DatumZeit) then begin
                  satzstatus:= $00;               { kein echter Satzstatus, Status in Rohdaten wird nicht verwendet }

                  { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig
                    -> in .Kanal auch "fehlend", wenn LGZ-Rückrechnung ungültigen Wert liefert }
                  for i:=1 to c_maxKanalZahl do
                  begin
                    kanal[i].wert:=0;
                    KanalOrig[i].wert:=0;
                    if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then
                      kanal[i].kanalstatus:=$A0
                    else                                  { Impulskanal }
                      kanal[i].kanalstatus:=$80;
                    KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                    KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                  end;

                  posi:=17;                              { einzelne Kanäle }
                  i:=0;
                  while i < c_maxKanalZahl do
                  begin
                    inc(i);
                    if posi < length(rohsatz) then
                    begin
                      if aktiv [i] > 0 then
                      begin
                        WertStr:=Copy (Rohsatz, posi, 8);
                        if (aktiv [i] >= MessKonvRec.Analogkanal_von) AND (aktiv [i] <= MessKonvRec.Analogkanal_bis) then  { Analogkanal }
                        begin
                          if HexToSingle (WertStr, s) then begin
                            KanalOrig [aktiv [i]].Wert:=s;       { Original-Rohwert }
                            KanalOrig [Aktiv [i]].KanalStatus := KanalOrig [Aktiv [i]].KanalStatus and $7F; { Fehlend-Bit löschen }

                            { physikalischen Analogwert rückrechnen auf LGZ-Rohwert über obere und untere Meßbereichgrenze aus
                              Stammdaten: }
                            if MessKonvRec.KanalList <> nil then begin
                              Delta:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (aktiv [i])).Daten.MessBereichMax -
                                     TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (aktiv [i])).Daten.MessBereichMin;
                              if Delta <> 0 then begin
                                L:= Round ((s - TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (aktiv [i])).Daten.MessBereichMin) / Delta * 10000);
                                if L >= 0 then begin
                                  if L > 9999 then
                                    Kanal [aktiv [i]].Wert:=9999
                                    { Fehlend-Bit hier nicht zurücksetzen, um Analogwert-"Überlauf"
                                      behelfsmäßig zu kennzeichnen }
                                  else begin
                                    Kanal [aktiv [i]].Wert:=L;
                                    Kanal [Aktiv [i]].KanalStatus := Kanal [Aktiv [i]].KanalStatus and $7F; { Fehlend-Bit löschen }
                                  end;
                                end;
                              end;
                            end;
                          end;
                        end
                        else begin                                                                                { Impulskanal }
                          Val ('$' + WertStr, L, Code);
                          KanalOrig [aktiv [i]].Wert:=L;         { Original-Rohwert }
                          KanalOrig [Aktiv [i]].KanalStatus := KanalOrig [Aktiv [i]].KanalStatus and $7F; { Fehlend-Bit löschen }

                          { physikalischen Impulswert rückrechnen auf LGZ-Rohwert über OrgFaktor aus Stammdaten: }
                          if MessKonvRec.KanalList <> nil then begin
                            Faktor:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (aktiv [i])).Daten.OrgFaktor;
                            if Faktor <> 0 then begin
                              L := Round (L / Faktor);
                              if L > High (Kanal [aktiv [i]].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                                Kanal [aktiv [i]].Wert:=High (Kanal [aktiv [i]].Wert);
                                Kanal [aktiv [i]].KanalStatus:=Kanal [Aktiv [i]].KanalStatus or $01;        { Überlauf-Bit setzen }
                              end else
                                Kanal [aktiv [i]].Wert:=L;
                              Kanal [Aktiv [i]].KanalStatus := Kanal [Aktiv [i]].KanalStatus and $7F;       { Fehlend-Bit löschen }
                            end;
                          end;
                        end;
                      end;
                      inc (posi,8);
                    end else
                      Break;  // nach Rohsatzende raus, sonst Byte-Überlauf posi
                              // bei 120 Kanälen (Gas-X),; 11.04.2005 HPR/WW
                  end; { while i }
                  fs_RohSRec.WriteRec (std_satz);
                end;  { if RecToDateTime }
              end;   { with stundensatz }
            end;    { if length }
          end;   { while not EOF Roh }
        finally
          fs_RohSRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.StdQuelldateiname);
    except
      Mess_Konv := False;
      exit;
    end;
  end;   { mess_konv }


begin  { main }
  mess_konv;
  Mess_Konv_EC694:=true;
end;


{----------------------------------------------------------}
function Mess_Konv_910 (MessKonvRec: TMessTagKonv): Boolean;
{----------------------------------------------------------}

  { Zählerstandskonvertierung }
  {-------------------------}
  function Tag_Konv: Boolean;
  {-------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohTRec   : TFileOfRecStream;    { Tagessätze-Datei in RohTRec-Struktur }
    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : rohsatztyp;

    tag_satz : RohTRec;

    i        : byte;          { Laufvariable }
    aktiv    : array[1..c_maxkanalzahl] of byte;
    count    : byte;
    L        : LongInt;
    Reserve  : Word;
    Code     : Integer;
    datum    : daterec;
    zeit     : timerec;
    iWert    : longint;
    k_Faktor : double;

  begin
    Tag_Konv := False;
    if not FileExists (MessKonvRec.TagQuelldateiname) then exit;

    Tag_Konv := True;
    try
      { Tagessatz-Rohfile öffnen: }
      fs_roh:=TFileOfCharStream.Create (MessKonvRec.TagQuelldateiname, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_roh:=fs_roh.Size;

        { Tagessatz-Zielfile erzeugen: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          FillChar (Aktiv, sizeof (Aktiv), 0);
          count:=0;

          { Anfang wegschneiden und aktive Kanäle bestimmen }

          rohsatz := '';
          zeichen := #0;
          while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and
                (zeichen <> #$03) do
          begin
            fs_roh.Read (zeichen);
            rohsatz := rohsatz + zeichen;
          end;
          rohsatz := Copy (Rohsatz, 3, length (Rohsatz));

          i:=1;
          while i <= length (Rohsatz) do begin
            if rohsatz [i] = '1' then begin
              inc (count);
              aktiv [count] := i;
            end;
            inc (i);
          end;

          { Konvertierung }

          while (fs_roh.Position < FSize_roh) and (zeichen <> #$03) do
          begin
            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and         { Satz bilden }
                  (zeichen <> #$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz := rohsatz + zeichen;
            end;

            Application.ProcessMessages;

            if length (rohsatz) > 1 then
              with tag_satz do
              begin
                Val ('$' + Copy (Rohsatz, 1, 4), L, Code);
                FillChar (Datum, Sizeof (Datum), 0);
                With Datum Do
                Begin
                  P_Datum_Decode_800 (Word (L), Word (Year), Word (Month), Word (Day));
                  If Year > 80 Then
                    Year := 1900 + Year
                  Else
                    Year := 2000 + Year;
                End;

                Val ('$' + Copy (Rohsatz, 5, 4), L, Code);
                with Zeit do  // für MRG 905 auch Minuten konvertieren; 15.03.2006 WW 
                  P_Zeit_Decode_800 (Word (L), Reserve, Word (Hour), Word (Min));
                zeit.sec:=0;
                zeit.hsec:=0;

                if zeit.hour <= MessKonvRec.TagesEnde then
                  P_vortag (datum);  { Zählerstand für Gastag }

                { Satzstatus }
                satzstatus := $00;
                if ((Reserve and $01) <> 0) then              { Spannungsausfall }
                  SatzStatus := SatzStatus or $02;
                if ((Reserve and $02) <> 0) then              { Uhr gestellt }
                  if not ((Zeit.Hour = MessKonvRec.TagesEnde) and (Zeit.Min = 0)) then
                    SatzStatus := SatzStatus or $04;
                { 14.08.2001 WW; der "Uhr gestellt"-Status muß beim MRG910 unterdrückt
                                 werden, da er in den "normalen" Tagessätzen, die am
                                 Tagesende geschrieben werden, steht und nicht in einem
                                 Sondertagessatz. Ansonsten verwirft die Aufbereitung
                                 den normalen Tagesatz !
                  15.03.2006 WW; nur beim Tagesende-Satz "Uhr gestellt"-Status
                                 unterdrücken. MRG 905 speichert Sondertagessätze
                                 beim Verstellen der Uhr. }

      {$IFDEF ZAEHL14}
                UmwStatus := 0;
      {$ENDIF}
                { Vorbelegung der Kanalstati und -werte }
                for i := 1 to c_maxKanalZahl do
                begin
                  E_zaehler[i].zaehlerstatus:=$80;
                  E_zaehler[i].wert:=0;
                  K_zaehler[i].zaehlerstatus:=$82;
                  K_zaehler[i].wert:=0;
                end;

                RohSatz := copy (RohSatz, 9, 255);

                i:=0;
                while i < c_maxKanalZahl do
                begin   { Eingangszählerwandlung }
                  inc(i);
                  if aktiv [i] > 0 then
                  begin
                    Val ('$' + Copy (Rohsatz, 1, 8), iWert, Code);
                    if MessKonvRec.ParameterListe <> nil then begin
                      // Für Geräte mit k-Faktor: k-Faktor in physikalischen
                      // Eingangszählerwert einrechnen; 28.05.2013, WW
                      if not MessKonvRec.ParameterListe.GetValueDouble (
                               CP_IMP_k_Faktor [aktiv [i]], k_Faktor) then
                        k_Faktor:=1;  // wenn k-Faktor im Gerät nicht existiert

                      e_zaehler[aktiv[i]].wert:=iWert / k_Faktor;  // Eingangszähler mit k-Faktor; 28.05.2013, WW
                      e_zaehler[aktiv[i]].zaehlerstatus:=
                        e_zaehler[aktiv[i]].zaehlerstatus and $7F; { Bit 7 löschen }
                    end;
                    RohSatz := copy (Rohsatz, 9, 255);
                  end;
                end; { of while i }

                i:=0;
                while i < c_maxKanalZahl do
                begin   { Kontrollzählerwandlung }
                  inc(i);
                  if aktiv [i] > 0 then
                  begin
                    Val ('$' + Copy (Rohsatz, 1, 8), iWert, Code);
                    if MessKonvRec.ParameterListe <> nil then begin
                      // Für Geräte mit k-Faktor: k-Faktor in physikalischen
                      // Eingangszählerwert einrechnen; 28.05.2013, WW
                      if not MessKonvRec.ParameterListe.GetValueDouble (
                               CP_IMP_k_Faktor [aktiv [i]], k_Faktor) then
                        k_Faktor:=1;  // wenn k-Faktor im Gerät nicht existiert

                      k_zaehler[aktiv[i]].wert:=iWert / k_Faktor;  // Kontrollzähler mit k-Faktor; 28.05.2013, WW
                      k_zaehler[aktiv[i]].zaehlerstatus:=
                        k_zaehler[aktiv[i]].zaehlerstatus and $7F; { Bit 7 löschen }
                    end;
                    RohSatz:=COPY(Rohsatz,9,255);
                  end;
                end; { of while i }

                if RecToDateTime (datum, zeit, tag_satz.DatumZeit) then
                  fs_RohTRec.WriteRec (tag_satz);             { Zwischendatei schreiben }
              end;   { if Länge >1 with }
          end;     { while not EOF }
        finally
          fs_RohTRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.TagQuelldateiname);
    except
      Tag_Konv := False;
      exit;
    end;
  end;     { Tag_Konv }


  { Messwerte konvertieren }
  {--------------------------}
  function Mess_Konv: Boolean;
  {--------------------------}
  var
    fs_roh    : TFileOfCharStream;  // 06.10.2015, WW
    fs_RohSRec: TFileOfRecStream;    { Stundenwerte-Datei in RohSRec-Struktur }
    FSize_roh: integer;

    zeichen  : char;
    rohsatz  : rohsatztyp;

    std_satz : RohSRec;

    posi     : byte;          { Zeichenzeiger im Rohsatz }
    i        : byte;          { Laufvariable }
    aktiv    : array[1..c_maxkanalzahl] of byte;
    count    : byte;
    L        : Int64;
    D        : Double;
    Teiler   : Double;
    Faktor   : Double;
    Reserve  : Word;
    dummy    : Word;
    Code     : Integer;
    Datum    : DateRec;
    Zeit     : TimeRec;
    k_Faktor : double;

  begin
    Mess_Konv := false;
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
          FillChar (Aktiv, sizeof (Aktiv), 0);
          count:=0;

          { Anfang wegschneiden und aktive Kanäle bestimmen }

          rohsatz := '';
          zeichen := #0;
          while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and
                (zeichen <> #$03) do
          begin
            fs_roh.Read (zeichen);
            rohsatz := rohsatz + zeichen;
          end;
          rohsatz := Copy (Rohsatz, 3, length (Rohsatz));

          i:=1;
          while i <= length (Rohsatz) do begin
            if rohsatz [i] = '1' then begin
              inc (count);
              aktiv [count] := i;
            end;
            inc (i);
          end;

          while (fs_roh.Position < FSize_roh) and (zeichen <> #$03) do
          begin
            rohsatz := '';
            zeichen := #0;
            while (fs_roh.Position < FSize_roh) and (zeichen <> #$1f) and            { Satz bilden }
                  (zeichen <> #$03) do
            begin
              fs_roh.Read (zeichen);
              rohsatz := rohsatz + zeichen;
            end;

            Application.ProcessMessages;

            if length (rohsatz) > 1 then
            begin        { gültiger Satz ? }
              with std_satz do
              begin
                Val ('$' + Copy (Rohsatz, 1, 4), L, Code);
                FillChar (Datum, Sizeof (Datum), 0);
                With Datum Do
                Begin
                  { Datum-Decodier-Routine vom MRG 800 kann verwendet werden: }
                  P_Datum_Decode_800 (Word (L), Word (Year), Word (Month), Word (Day));
                  If Year > 80 Then
                    Year := 1900 + Year
                  Else
                    Year := 2000 + Year;
                End;

                Val ('$' + Copy (Rohsatz, 5, 4), L, Code);
                FillChar (Zeit, Sizeof (Zeit), 0);
                With Zeit Do
                  { Zeit-Decodier-Routine vom MRG 800 kann nur für Zeit, nicht für
                    Satzstatus verwendet werden: }
                  P_Zeit_Decode_800 (Word (L), dummy, Word (Hour), Word (Min));

                if RecToDateTime (datum, zeit, DatumZeit) then begin
                  Val ('$' + Copy (Rohsatz, 9, 2), Reserve, Code);

                  { Satzstatus }
                  satzstatus:= $00;
                  if ((Reserve and $04) <> 0) then             { Uhr gestellt }
                    SatzStatus := SatzStatus or $02;

                  { Vorbelegung von Wert und Kanalstatus mit "fehlend" in .Kanal und .KanalOrig }
                  for i:=1 to c_maxKanalZahl do
                  begin
                    kanal[i].wert:=0;
                    KanalOrig[i].wert:=0;
                    if (i >= MessKonvRec.Analogkanal_von) AND (i <= MessKonvRec.Analogkanal_bis) then  { Analogkanal }
                      kanal[i].kanalstatus:=$A0
                    else                                  { Impulskanal }
                      kanal[i].kanalstatus:=$80;
                    KanalOrig[i].KanalStatus:=Kanal[i].KanalStatus;
                    KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
                  end;

                  posi:=11;                              { einzelne Kanäle }
                  i:=0;
                  while i < c_maxKanalZahl do
                  begin
                    inc(i);
                    if posi < length(rohsatz) then
                    begin
                      if aktiv [i] > 0 then
                      begin
                        if (aktiv [i] >= MessKonvRec.Analogkanal_von) AND (aktiv [i] <= MessKonvRec.Analogkanal_bis) then begin { Analogkanal }
                          Val ('$' + Copy (Rohsatz, posi, 3), L, Code);
                          D:=L * 25/20;   { Umrechnung Messstrom 0..25 mA: 20 mA -> 10000 }
                          KanalOrig [aktiv [i]].Wert:=D;  { umgerechneter Original-Rohwert, 18.03.2004  WW }
                          KanalOrig [Aktiv [i]].KanalStatus:=KanalOrig [Aktiv [i]].KanalStatus and $7F;       { Fehlend-Bit löschen }

                          L:=round (D * 10000/4095);    { auf 10000 normierter Analogwert }
                          if L > 9999 then
                            Kanal [aktiv [i]].Wert:=9999
                            { Fehlend-Bit hier nicht zurücksetzen, um Analogwert-"Überlauf"
                              behelfsmäßig zu kennzeichnen }
                          else begin
                            Kanal [aktiv [i]].Wert:=L;
                            Kanal [Aktiv [i]].KanalStatus:=Kanal [Aktiv [i]].KanalStatus and $7F;       { Fehlend-Bit löschen }
                          end;

                          if MessKonvRec.AnalogOrig_normiert then
                            KanalOrig [Aktiv [i]].Wert:=Kanal [Aktiv [i]].Wert;  { normierter Original-Analogrohwert }

                          inc (posi,3);
                        end
                        else begin                                                       { Impulskanal }
                          Val ('$' + Copy (Rohsatz, posi, 6), L, Code);
                          if MessKonvRec.ParameterListe <> nil then begin
                            // Für Geräte mit k-Faktor: k-Faktor in physikalischen
                            // Impulswert einrechnen; 28.05.2013, WW
                            if not MessKonvRec.ParameterListe.GetValueDouble (
                                     CP_IMP_k_Faktor [aktiv [i]], k_Faktor) then
                              k_Faktor:=1;  // wenn k-Faktor im Gerät nicht existiert

                            KanalOrig [aktiv [i]].Wert:=L / k_Faktor;  { Original-Rohwert mit k-Faktor; 28.05.2013, WW }
                            KanalOrig [Aktiv [i]].KanalStatus:=KanalOrig [Aktiv [i]].KanalStatus and $7F;       { Fehlend-Bit löschen }

                            { physikalischen Impulswert rückrechnen auf LGZ-Rohwert über Faktor/Teiler aus Gerät: }
                            if MessKonvRec.ParameterListe.GetValueDouble (CP_IMP_Faktor [aktiv [i]], Faktor) AND
                               MessKonvRec.ParameterListe.GetValueDouble (CP_IMP_Teiler [aktiv [i]], Teiler) then begin
                              if Faktor <> 0 then begin
                                L := Round (L * Teiler / Faktor / k_Faktor);  // mit k-Faktor; 28.05.2013, WW
                                if L > High (Kanal [aktiv [i]].Wert) then begin  // Impulswertebereich vergrößert; 12.01.2009, WW
                                  Kanal [aktiv [i]].Wert:=High (Kanal [aktiv [i]].Wert);
                                  Kanal [aktiv [i]].KanalStatus:=
                                    Kanal [Aktiv [i]].KanalStatus or $01;        { Überlauf (bei Rückrechnung) }
                                end
                                else begin
                                  Kanal [aktiv [i]].Wert:=L;
                                  if (Reserve and ($04 SHL aktiv[i])) <> 0 then
                                    Kanal [Aktiv [i]].KanalStatus :=
                                      Kanal [Aktiv [i]].KanalStatus or $01;      { Überlauf (Statusinfo vom Gerät) }
                                end;
                                Kanal [Aktiv [i]].KanalStatus :=
                                  Kanal [Aktiv [i]].KanalStatus and $7F;       { Fehlend-Bit löschen }
                              end;
                            end;
                          end;
                          inc (posi,6);
                        end;
                      end;
                    end;
                  end; { while i }
                  fs_RohSRec.WriteRec (std_satz);
                end;  { if RecToDateTime }
              end;   { with stundensatz }
            end;    { if length }
          end;   { while not EOF Roh }
        finally
          fs_RohSRec.Free;
        end;
      finally
        fs_roh.Free;
      end;

      if MessKonvRec.loeschen then
        DeleteFile (MessKonvRec.StdQuelldateiname);
    except
      Mess_Konv := False;
      exit;
    end;
  end;   { mess_konv }


begin  { main }
  mess_konv;
  tag_konv;
  Mess_Konv_910:=true;
end;


{------------------------------------------------------------}
function Mess_Konv_EC900 (MessKonvRec: TMessTagKonv): Boolean;
{------------------------------------------------------------}
{ Konvertierung RMG EC 900 (Periodenarchiv) }
const
  CMaxEC900_Kanaele = 11;

type
  { Struktur zum Merken eines Kanalwerts für Stundenwert-Differenzbildung }
  Tletzt_daten = record
    Datumzeit: TDateTime;
    Wert: double;
    Wert_vorhanden: boolean;
  end;

var
  fs_roh: TFileOfCharStream;  // 06.10.2015, WW
  FSize_roh: integer;
  rohsatz: string;
  zeichen: char;
  s: string;
  mw_zwlist: TObjectList;
  zs_zwlist: TObjectList;
  Satz_OK: boolean;
  iPos: integer;
  dtDatumZeit: TDateTime;
  i: integer;
  STX_gelesen: boolean;
  iArchivNr: integer;
  iKanaele: integer;
  sbuf: string;
  Maske: cardinal;
  Aktiv: array [1..c_maxkanalzahl] of byte;
  count: integer;
  KanalPos: integer;
  KanalNr: integer;
  sWert: string;
  std_satz: RohSRec;
  tag_satz: RohTRec;
  RohSRecObj: TRohSRecObj;
  RohTRecObj: TRohTRecObj;
  fs_RohSRec: TFileOfRecStream;
  fs_RohTRec: TFileOfRecStream;
  mw_phys: double;
  Code: integer;
  cSysStatus: cardinal;
  letzt_daten_k: array [1..CMaxEC900_Kanaele] of Tletzt_daten;
  k: integer;
  hour, min, sec, msec: word;
  dtBuf: TDateTime;
  Faktor: double;
  Diff: double;
  l: Int64;
  dBuf: double;
  MB_OK: boolean;
  MBoben: double;
  MBunten: double;
  Delta: double;
  iOrdNr: longint;
  iBuf: longint;

begin
  Result:=false;
  if not FileExists (MessKonvRec.StdQuelldateiname) then exit;

  zs_zwlist:=TObjectList.Create;            { Zwischenliste für Zählerstände }
  try
    mw_zwlist:=TObjectList.Create;             { Zwischenliste für Messwerte }
    try
      try
        { Rohfile öffnen: }
        fs_roh:=TFileOfCharStream.Create (MessKonvRec.StdQuelldateiname, fmOpenRead OR fmShareDenyWrite);
        try
          FSize_roh:=fs_roh.Size;
          STX_gelesen:=false;

          while fs_roh.Position < FSize_roh do begin
            rohsatz:='';
            zeichen:=NUL;
            { Datensätze bilden: bis ETX oder US lesen }
            while (zeichen <> ETX) AND (zeichen <> US) AND
                  (fs_roh.Position < FSize_roh) do begin
              fs_roh.Read (zeichen);

              if (zeichen <> ETX) AND (zeichen <> US) then
                rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne ETX, US) }
            end;

            Application.ProcessMessages;

            { Falls STX im Rohsatz vorhanden ist, Header auswerten (Befehlszeichen,
              Archivnummer, Kanäle): }
            if not STX_gelesen then begin
              iPos:=Pos (STX, rohsatz);
              if iPos > 0 then begin
                STX_gelesen:=true;
                rohsatz:=Copy (rohsatz, iPos + 1, length (rohsatz));

                S:=Copy (rohsatz, 1, 1);  // Befehlszeichen
                if S = 'A' then begin  // Rohdaten aus Archiv-Abfragebefehl
                  S:=Copy (rohsatz, 2, 2);  // Archivnummer
                  iArchivNr:=StrToIntDef (S, -1);
                  S:=Copy (rohsatz, 5, length (rohsatz));  // Kanäle
                  iKanaele:=StrToIntDef ('$' + S, -1);
                  if iKanaele < 0 then exit;
                  if iArchivNr <> 1 then exit;  // A01: Periodenarchiv

                  // Kanalbits in Antwort auswerten:
                  FillChar (Aktiv, SizeOf (Aktiv), 0);  // Vorbelegung 0: Kanal interessiert nicht, wird nicht konvertiert
                  count:=0;
                  Maske:=1;
                  for i:=0 to 31 do begin
                    Application.ProcessMessages;
                    if (iKanaele AND Maske) <> 0 then begin   { Bit gesetzt -> Kanal ist aktiv }
                      inc (count);
                      //  Zuordnung Bitnummer -> Kanalnummer:
                      if (count >= Low (Aktiv)) AND (count <= High (Aktiv)) then
                        if Assigned (MessKonvRec.MrgKanalBitMaskList) then  // 06.08.2021, WW
                          if (i >= 0) AND (i < MessKonvRec.MrgKanalBitMaskList.Count) then
                            Aktiv [count] := TMrgKanalBitMaskDataObj (MessKonvRec.MrgKanalBitMaskList [i]).Kanal;
                    end;
                    Maske:=Maske SHL 1;
                  end;  { for i }
                end else
                  exit;
                Result:=true;
              end;
            end
            else begin  { Archiv-Eintrag }
              if  (length (rohsatz) > 0) then begin
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

                  { rohsatz aufsplitten  }
                  satz_ok:=true;                            { Vorbelegung: Datensatz ist ok }
                  iOrdNr:=-1;  { Default-Ordnungsnummer; 28.09.2020, WW }
                  i:=0;
                  while length (rohsatz) > 0 do begin
                    inc (i);
                    sbuf:=F_Zerlegen (rohsatz, RS);      { lesen bis RS }

                    { i = 1: Zeitstempel, Unix-Format }
                    if i = 1 then begin
                      if UnixTimeStrToDateTime (sbuf, dtDatumZeit) then begin
                        DatumZeit:=dtDatumZeit;
                        { Datum/Stunden in tag_satz belegen: }
                        tag_satz.DatumZeit:=DatumZeit;
                      end
                      else begin
                        satz_ok:=false;
                        break;
                      end;
                    end

                    { i = 2: Ordnungsnummer; 28.09.2020, WW }
                    else if i = 2 then begin
                      if length (sbuf) > 0 then begin
                        Val (sbuf, iBuf, Code);
                        if Code = 0 then
                          iOrdNr:=iBuf;
                      end;
                    end

                    { i = 3: Systemstatus, Hex-Format }
                    else if i = 3 then begin
                      Val ('$' + sbuf, cSysStatus, Code);
                      if (cSysStatus AND $01) <> 0 then begin  { Uhr gestellt }
                        SatzStatus := SatzStatus or $02;
                        { Satzstatus in tag_satz belegen: }
                        tag_satz.satzstatus:=tag_satz.satzstatus or $04;
                      end;
                    end

                    else if i >= 4 then begin  { Archivkanalwerte }
                      KanalPos:=i-3;
                      if (KanalPos >= Low (Aktiv)) AND (KanalPos <= High (Aktiv)) then begin
                        KanalNr:=Aktiv [KanalPos];
                        if KanalNr > 0 then begin  // Kanal soll konvertiert werden
                          { Je nach Kanal kann ein durch GS vom Kanalwert getrennter
                            Kanalstatus vorhanden sein. Roh-Kanalstatus enthält
                            keine auswertbare Information für LGZ-Kanalstatus: }
                          sWert:=F_Zerlegen (sbuf, GS);
                          Val (sWert, mw_phys, Code);     { Meßperiodenwert (Integer- oder Float-String) wandeln }

                          KanalOrig [KanalNr].Wert:=mw_phys;         { Original-Rohwert }
                          KanalOrig [KanalNr].KanalStatus:=KanalOrig [KanalNr].KanalStatus and $7F; { Fehlend-Bit löschen }
                          KanalOrig [KanalNr].OrdNr := iOrdNr; { Ordnungsnummer zuweisen; 28.09.2020, WW }

                          { in Tagessatz-Record gerundete physikalische Zählerstände eintragen: }
                          tag_satz.E_zaehler[KanalNr].wert:=mw_phys;  // Rundung raus; 27.03.2013, WW
                          tag_satz.E_zaehler[KanalNr].zaehlerstatus:=
                            tag_satz.E_zaehler[KanalNr].zaehlerstatus AND $7F; { Fehlend-Bit 7 löschen }
                        end;  { if KanalNr > 0 }
                      end;  { if (KanalPos >= Low (Aktiv) AND (KanalPos <= High (Aktiv)) }
                    end;  { else if i >= 4 }
                  end;  { while length (rohsatz) }
                end;  { with std_satz }

                if satz_ok then begin
                  { Stundensatz-Objekt in MW-Zwischenliste anhängen: }
                  RohSRecObj:=TRohSRecObj.Create (std_satz);
                  mw_zwlist.Add (RohSRecObj);
                  { Tagessatz-Objekt in ZS-Zwischenliste anhängen: }
                  RohTRecObj:=TRohTRecObj.Create (tag_satz);
                  zs_zwlist.Add (RohTRecObj);
                end;
              end;  { if length (rohsatz) > 0) }
            end;  { if STX_gelesen }

            if zeichen = ETX then
              Break;  // nach ETX ist Schluss mit Rohdaten
          end;  { while fs_roh.Position < FSize_roh }
        finally
          fs_roh.Free;
        end;

        if MessKonvRec.loeschen then
          DeleteFile (MessKonvRec.StdQuelldateiname);

{------------ Konvertierung Zwischenlisten -> Zwischendateien -----------------}

        { ZS-Zwischenliste in ZS-Zwischenfile konvertieren: }
        fs_RohTRec:=TFileOfRecStream.Create (MessKonvRec.TagZieldateiname, fmCreate, SizeOf (tag_satz));
        try
          for i:=0 to zs_zwlist.Count - 1 do begin
            Application.ProcessMessages;
            tag_satz:=TRohTRecObj (zs_zwlist.Items [i]).RohTRec;
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

        { MW-Zwischenliste in MW-Zwischenfile konvertieren: }
        fs_RohSRec:=TFileOfRecStream.Create (MessKonvRec.StdZieldateiname, fmCreate, SizeOf (std_satz));
        try
          { Vorbelegung von Datum/Zeit und Wert der Vorstunden-Kanalwerte für
            Zählerstands-Differenzbildung: Vorstundenwert unbekannt }
          for i:=Low (letzt_daten_k) to High (letzt_daten_k) do begin
            letzt_daten_k [i].Datumzeit:= 0;
            letzt_daten_k [i].Wert:=0;
            letzt_daten_k [i].Wert_vorhanden:=false;
          end;

          for i:=0 to mw_zwlist.Count - 1 do begin
            Application.ProcessMessages;
            std_satz:=TRohSRecObj (mw_zwlist.Items [i]).RohSRec;

            with std_satz do begin
              DecodeTime (std_satz.DatumZeit, hour, min, sec, msec);

              for k:=1 to CMaxEC900_Kanaele do begin
                if (k >=MessKonvRec.Analogkanal_von) AND (k <= MessKonvRec.Analogkanal_bis) then begin  { Analogkanäle }
                  { für LGZ-Daten: vom Gerät gelieferte physikalische Analogwerte
                    rückrechnen auf LGZ-Rohwert
                    -> P und T: über obere und untere Meßbereichgrenze aus Geräteparameter
                    -> Z- und K-Zahl: über obere und untere Meßbereichgrenze aus
                                          Stammdaten (es gibt logischerweise keine Geräteparameter) }
                  MB_OK:=true;
                  MBoben:=0.0;
                  MBunten:=0.0;
                  if k = 8 then begin  // P
                    if MessKonvRec.ParameterListe <> nil then begin
                      if MessKonvRec.ParameterListe.GetValueDouble (CP_RMG_EC900_AlarmGWoben_P, dBuf) then
                        MBoben:=dBuf
                      else
                        MB_OK:=false;
                      if MessKonvRec.ParameterListe.GetValueDouble (CP_RMG_EC900_AlarmGWunten_P, dBuf) then
                        MBunten:=dBuf
                      else
                        MB_OK:=false;
                    end else
                      MB_OK:=false;
                  end
                  else if k = 9 then begin  // T
                    if MessKonvRec.ParameterListe <> nil then begin
                      if MessKonvRec.ParameterListe.GetValueDouble (CP_RMG_EC900_AlarmGWoben_T, dBuf) then
                        MBoben:=dBuf
                      else
                        MB_OK:=false;
                      if MessKonvRec.ParameterListe.GetValueDouble (CP_RMG_EC900_AlarmGWunten_T, dBuf) then
                        MBunten:=dBuf
                      else
                        MB_OK:=false;
                    end else
                      MB_OK:=false;
                  end
                  else begin  // Z, K
                    if MessKonvRec.KanalList <> nil then begin
                      MBoben:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (k)).Daten.MessBereichMax;
                      MBunten:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (k)).Daten.MessBereichMin;
                    end else
                      MB_OK:=false;
                  end;

                  if MB_OK then begin
                    Delta:=MBoben - MBunten;
                    if Delta <> 0 then begin
                      L:= Round ((KanalOrig [k].Wert - MBunten) / Delta * 10000);
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
                  { für LGZ-Daten: vom Gerät gelieferte physikalische Zählerstände
                    rückrechnen auf normierte Zählerstandsdifferenzen
                    -> VM, SVM: über Geräteparameter "Zählerfaktor Messkanal"
                    -> VO: über OrgFaktor aus Stammdaten (für VO gibts keinen Geräteparameter "Faktor")
                    -> VB, SVB: über OrgFaktor aus Stammdaten (für Normvolumen gibts
                                keine Umrechnung in Impulse, da es aus dem Betriebsvolumen
                                errechnet wird !)
                    -> VU, SVU: werden nicht rückgerechnet (sind ja unbewertete Impulse) }
                  if (min = 0) AND (sec = 0) then begin  { nur volle Stunden interessieren ! }
                    if letzt_daten_k [k].Wert_vorhanden AND     { Differenzbildung nur möglich, wenn Vor-Zählerstand... }
                       ((KanalOrig [k].KanalStatus AND $80) = 0) then begin    { und aktueller Zählerstand nicht fehlend }
                      dtBuf:=letzt_daten_k [k].Datumzeit;
                      dtBuf:=dtBuf + EncodeTime (1, 0, 0, 0);
                      { Vor-Zählerstand muß von Vorstunde sein: }
                      if CmpDateTime (dtBuf, DatumZeit) = 0 then begin
                        Faktor:=0.0;
                        if (k = 2) OR (k = 5) then begin  // VM, SVM
                          if MessKonvRec.ParameterListe <> nil then
                            if MessKonvRec.ParameterListe.GetValueDouble (CP_RMG_EC900_Zaehlerfaktor_Mess, dBuf) then
                              Faktor:=1 / dBuf;   // ist Teiler !
                        end
                        else if (k = 1) OR (k = 4) OR (k = 7) then begin // VO, VB, SVB
                          if MessKonvRec.KanalList <> nil then
                            Faktor:=TStaKanalKonvDataObj (MessKonvRec.KanalList.GetMRGKanal (k)).Daten.OrgFaktor;
                        end else  // VU, SVU
                          Faktor:=1.0;

                        if Faktor <> 0 then begin
                          Diff:=KanalOrig [k].Wert - letzt_daten_k [k].Wert;   { Zählerstands-Differenz }
                          l:=Round (Diff / Faktor);
                          if l >= 0 then begin
                            if l > High (Kanal [k].Wert) then begin
                              Kanal [k].Wert:=High (Kanal [k].Wert);
                              Kanal [k].KanalStatus := Kanal [k].KanalStatus or $01;  { Überlauf-Bit setzen }
                            end else
                              Kanal [k].Wert:=l;
                            Kanal [k].KanalStatus := Kanal [k].KanalStatus and $7F; { Fehlend-Bit löschen }
                          end;
                        end;
                      end;
                    end;
                    { Wert mit Datum/Zeit merken: }
                    letzt_daten_k [k].Datumzeit:=DatumZeit;
                    letzt_daten_k [k].Wert:=KanalOrig [k].Wert;
                    letzt_daten_k [k].Wert_vorhanden:=(KanalOrig [k].KanalStatus AND $80) = 0;
                  end;
                end;  { if k }
              end;  { for k }
            end;  { with std_satz }

            fs_RohSRec.WriteRec (std_satz);
          end;  { for }
        finally
          fs_RohSRec.Free;
        end;
      except
        Result:=false;
        exit;
      end;
    finally
      mw_zwlist.Free;
    end;
  finally
    zs_zwlist.Free;
  end;
end;  { Mess_Konv_EC900 }


{-------------------------------------------------------------}
function Mess_Konv_Modbus (MessKonvRec: TMessTagKonv): Boolean;
{-------------------------------------------------------------}
{ Konvertierung Messwerte aus Modbus-Abfrage
  -> RMG Primus/Prilog (Datenarchiv)
  -> RMG TME400, RSM200 (Periodenarchiv)
  -> Die Quell-Datei enthält die zu konvertierenden Datensätze in
     TRegisterKonvDataArchivRec-Struktur. }
var
  fs_roh: TFileOfRecStream;
  std_satz: RohSRec;
  fs_RohSRec: TFileOfRecStream;
  iRecPos: integer;
  RegisterKonvDataArchivRec: TRegisterKonvDataArchivRec;
  RegisterKonvData: TRegisterKonvData;
  i: integer;
  satz_OK: boolean;
  sMB_KanalDef: string;
  iKanalNr: integer;
  mw_phys: double;
  i64: Int64;
  SatzStatus_fehlt: boolean;
  iOrdNr: longint;
  dFaktor: double;
  bMitFaktor: boolean;
  iStatus: integer;
  iMaxImpKanaele: integer;

begin
  Result:=false;
  if not FileExists (MessKonvRec.StdQuelldateiname) then exit;

  try
    { Quellfile öffnen: }
    fs_roh:=TFileOfRecStream.Create (MessKonvRec.StdQuelldateiname,
      fmOpenRead OR fmShareDenyWrite, SizeOf (TRegisterKonvDataArchivRec));
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
          fs_roh.ReadRec (RegisterKonvDataArchivRec);

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
              KanalOrig[i].OrdNr:=-1;  // Default-Ordnungsnummer; 18.05.2018, WW
            end;

            iOrdNr:=-1;  { Default-Ordnungsnummer; 18.02.2021, WW }
            dFaktor:=1;  { Default-Faktor; 18.02.2021, WW }
            bMitFaktor:=false;  { Default: Modbus-Datensatz enthält keinen Faktor }

            satz_OK:=true;                    { Vorbelegung: Datensatz ist ok }
            SatzStatus_fehlt:=true;           { Vorbelegung: Satzstatus fehlt }

            for i:=Low (RegisterKonvDataArchivRec) to High (RegisterKonvDataArchivRec) do begin
              RegisterKonvData:=RegisterKonvDataArchivRec [i];

              if RegisterKonvData.AnzahlBytes > -1 then begin  // nur belegte Datensätze; 18.02.2021, WW
                // Modbus-Kanaldefinition zu der Startadresse des Werts des
                // Messwertarchiv-Datensatzes ermitteln:
                sMB_KanalDef:=GetKanalDef_MBRegister_Archiv (MessKonvRec.MrgTyp, at_Periodenarchiv,
                                                             RegisterKonvData.StartAdresse,
                                                             MessKonvRec.MBAbrufData);  // 11.02.2022, WW
                if sMB_KanalDef = C_MBKanalDef_DZ then begin
                  { Zeitstempel }
                  try
                    DatumZeit:=StrToDateTime (RegisterKonvData.Wert);
                    { Anm.: Die Kodierung in der Modbus-Konvertierung erfolgt mittels DateTimeToStr }
                  except
                    satz_OK:=false;
                    Break;
                  end;
                end

                else if sMB_KanalDef = C_MBKanalDef_SatzStatus then begin
                  { Satzstatus }
                  if length (RegisterKonvData.Wert) > 0 then begin
                    case MessKonvRec.MrgTyp of
                      mrgtyp_Primus,
                      mrgtyp_Prilog:
                        begin
                          { Gerätestatus -> Satzstatus }
                          i64:=StrToInt64 (RegisterKonvData.Wert);  { Status des Geräts (Int64) wandeln }
                          if (i64 AND $80000000) <> 0 then  { Status, Bit 31: Maintenance }
                            SatzStatus:=SatzStatus OR $08;  { -> Revision-Bit setzen}
                          if (i64 AND $0400000000000000) <> 0 then  { Status, Bit 58: RTC setting }
                            SatzStatus:=SatzStatus OR $02;  { -> Uhr gestellt-Bit setzen}
                        end;

                      mrgtyp_TME400_VCF,
                      mrgtyp_TME400_VMF:  // TME400                     
                        begin
                          iStatus:=StrToInt (RegisterKonvData.Wert);  { Status des Messwert-Archivdatensatzes wandeln }
                          if (iStatus AND $100) <> 0 then  { Status, Bit 8: RTC adjusted }
                            SatzStatus:=SatzStatus OR $02;  { -> Uhr gestellt-Bit setzen}                          
                        end;

                      // Bei RSM200 keine auswertbaren Status-Bits

                    end;  // case MessKonvRec.MrgTyp

                    SatzStatus_fehlt:=false;
                  end
                  else begin
                    satz_OK:=false;
                    Break;
                  end;
                end

                else if sMB_KanalDef = C_MBKanalDef_ONr then begin  // 18.02.2021, WW
                  { Ordnungsnummer }
                  iOrdNr:=StrToIntDef (RegisterKonvData.Wert, -1);
                end

                else if sMB_KanalDef = C_MBKanalDef_Faktor then begin  // 18.02.2021, WW
                  { Faktor für Werte-Verrechnung }
                  try
                    dFaktor:=StrToFloat (RegisterKonvData.Wert);  { Faktor (Integer- oder Float-String) wandeln }
                    bMitFaktor:=true;
                  except
                    satz_OK:=false;
                    Break;
                  end;
                end

                else begin
                  { Kanal-Werte }
                  iKanalNr:=StrToIntDef (sMB_KanalDef, -1);  // Kanal-Nr. steht in der Modbus-Kanaldefinition
                  if (iKanalNr >= 1) AND (iKanalNr <= C_MaxKanalZahl) then begin
                    try
                      mw_phys:=StrToFloat (RegisterKonvData.Wert);  { Meßperiodenwert (Integer- oder Float-String) wandeln
                                                                      Anm.: Die Kodierung in der Modbus-Konvertierung erfolgt mittels FloatToStr }
                      KanalOrig [iKanalNr].Wert:=mw_phys;         { Original-Rohwert }
                      KanalOrig [iKanalNr].KanalStatus:=KanalOrig [iKanalNr].KanalStatus and $7F; { Fehlend-Bit löschen }
                    except
                      //
                    end;

                    // Normierte LGZ-Werte werden bei diesen Gerätetypen nicht mehr unterstützt.
                  end;
                end;
              end;
            end;  { for i }

            { Evtl. vorhandene Ordnungsnummer und Faktor in std_satz übernehmen;
              18.02.2021, WW }
            for i:=1 to C_MaxKanalZahl do begin
              if (KanalOrig [i].KanalStatus AND $80) = 0 then begin  { Kanal nicht fehlend }
                { Ordnungsnummer: }
                if iOrdNr <> -1 then
                  KanalOrig [i].OrdNr:=iOrdNr;

                { Faktor: }
                case MessKonvRec.MrgTyp of
                  mrgtyp_TME400_VCF,
                  mrgtyp_TME400_VMF,
                  mrgtyp_RSM200_VCF,
                  mrgtyp_RSM200_VMF:  // TME400, RSM200
                    begin
                      { Verrechnung mit Faktor nur für die Impulskanalwerte:
                          -> TME400-VCF, RSM200-VCF: 4 Impulskanäle (Vn, Vb, Vn stör, Vb stör)
                          -> TME400-VMF, RSM200-VMF: 2 Impulskanäle (Vb, Vb stör) }
                      case MessKonvRec.MrgTyp of
                        mrgtyp_TME400_VCF,
                        mrgtyp_RSM200_VCF: iMaxImpKanaele:=4;
                        mrgtyp_TME400_VMF,
                        mrgtyp_RSM200_VMF: iMaxImpKanaele:=2;
                      else
                        iMaxImpKanaele:=0;
                      end;

                      if (i >= 1) AND (i <= iMaxImpKanaele) then begin
                        if bMitFaktor then begin
                          { Faktor-Rohwert enthält den Exponent zur Basis 10: }
                          KanalOrig [i].Wert:=KanalOrig [i].Wert *
                                              exp(dFaktor * ln(10));  // 10 hoch Exponent
                        end
                        else begin
                          // Faktor ist für diese Kanäle zwingend !
                          // Bei fehlendem Faktor -> Wert zurück auf fehlend setzen:
                          KanalOrig [i].Wert:=0;
                          KanalOrig [i].KanalStatus:=KanalOrig [i].KanalStatus or $80; { Fehlend-Bit setzen }
                        end;
                      end;
                    end;
                end;  // case MessKonvRec.MrgTyp
              end;
            end;  { for i }
          end;  { with std_satz }

          if satz_OK AND (std_satz.DatumZeit > 0) AND (not SatzStatus_fehlt) then begin
            fs_RohSRec.WriteRec (std_satz);
          end;
        end;
      finally
        fs_RohSRec.Free;
      end;
    finally
      fs_roh.Free;
    end;

    if MessKonvRec.loeschen then
      DeleteFile (MessKonvRec.StdQuelldateiname);
    Result:=true;
  except
    Result:=false;
    exit;
  end;
end;  { Mess_Konv_Primus }


{------------------------------------------------------------------------------}

{ Messwert- und Tagessatzkonvertierung }
{------------------------------------------------------------------------------}
function MessTag_Konv (MessKonvRec: TMessTagKonv; var fertig: boolean;
  var iMaxMessKanal: integer; var iMinMessKanalExt: integer;
  var iMaxMessKanalExt: integer): Boolean;
{------------------------------------------------------------------------------}
{ Rückgabe: fertig -> true, nach Parallel-Konvertierung aller in
                      MessKonvRec.StdQuelldateiNameListe bzw. MessKonvRec.TagQuelldateiNameListe
                      übergebener Rohdateien, sonst immer false
            iMaxMessKanal -> Dynamisch in Konvertierungsroutine ermittelte Anzahl
                      der konvertierten Messwert-Kanäle (-1, wenn unbekannt)
            iMinMessKanalExt -> Dynamisch in Konvertierungsroutine ermittelte
                      niedrigste Kanalnummer des konvertierten, erweiterten
                      Messwert-Archivs (-1, wenn unbekannt)
            iMaxMessKanalExt -> Dynamisch in Konvertierungsroutine ermittelte
                      höchste Kanalnummer des konvertierten, erweiterten
                      Messwert-Archivs (-1, wenn unbekannt) }
begin
  Result := False;
  fertig := False;
  // Vorbelegungen: Kanalzahlen aus Konvertierung unbekannt
  iMaxMessKanal:=-1;
  iMinMessKanalExt:=-1;
  iMaxMessKanalExt:=-1;

  case MessKonvRec.KonvGruppe of
     1: Result := Mess_Konv_3113 (MessKonvRec);
     2: Result := Mess_Konv_3100 (MessKonvRec, True);
     3: Result := Mess_Konv_3100 (MessKonvRec, False);
     4: Result := Mess_Konv_1029 (MessKonvRec);
     5: Result := Mess_Konv_800 (MessKonvRec);
     6: Result := Mess_Konv_2001 (MessKonvRec);
     7: Result := Mess_Konv_800PTB (MessKonvRec);
     8: Result := Mess_Konv_EC694 (MessKonvRec);
     9: Result := Mess_Konv_910 (MessKonvRec);
    10: Result := Mess_Konv_MemoDat (MessKonvRec);
    11: begin
          Result := Mess_Konv_EK260 (MessKonvRec, iMaxMessKanal,
            iMinMessKanalExt, iMaxMessKanalExt) = 0;
          fertig := True;  { -> alle Archiv-Rohfiles werden auf einmal konvertiert; 24.08.2023, WW }
        end;
    12: begin
          Result := Mess_Konv_DL240 (MessKonvRec) = 0;
          fertig := True;  { -> alle Kanal-Rohfiles werden auf einmal konvertiert }
        end;
    13, 18:
        begin
          Result := Mess_Konv_Tritschler_IEC (MessKonvRec);
          fertig := True;  { -> alle Kanal-Rohfiles werden auf einmal konvertiert }
        end;
    14: Result := Mess_Konv_KE_Ruhrgas (MessKonvRec);
    15: begin
          Result := Mess_Konv_FWU (MessKonvRec);
          fertig := True;  { -> alle Stundenwert-/Tagesatzkanal-Rohfiles werden auf einmal konvertiert }
        end;
    16: Result := Mess_Konv_KE_PPN (MessKonvRec);
    17: begin
          Result := Mess_Konv_DS100 (MessKonvRec);
          fertig := True;  { -> alle Kanal-Rohfiles werden auf einmal konvertiert }
        end;
    20:
        begin
          Result := Mess_Konv_Tritschler_FTL (MessKonvRec) = 0;
          fertig := True;  { -> alle Kanal-Rohfiles werden auf einmal konvertiert }
        end;
    21: Result:=Mess_Konv_Tritschler_TDS (MessKonvRec);
    22: Result:=Mess_Konv_Corus (MessKonvRec);
    23: begin
          Result := Mess_Konv_Sparklog (MessKonvRec);
          fertig := True;  { -> alle Kanal-Rohfiles werden auf einmal konvertiert }
        end;
    24: Result:=Mess_Konv_EC900 (MessKonvRec);
    25: Result:=Mess_Konv_Unigas (MessKonvRec);
    26: Result:=Mess_Konv_Modbus (MessKonvRec);
    27: Result:=Mess_Konv_SICK (MessKonvRec);
  end;
end;

{ Prüfungssatzkonvertierung }
{------------------------------------------------------}
function Pruef_Konv (PruefKonvRec: TPruefKonv): boolean;
{------------------------------------------------------}

const
  l_PRFSatz4013Q = 47;  { keine AKA ! }

var
  fs_roh         :  TFileOfCharStream;  // 06.10.2015, WW
  fs_work        :  TFileOfRecStream;
  fs_LGZPr       :  TFileOfRecStream;
  FSize_roh      : integer;

  p_satz         :  PruefRec;
  zeichen        :  char;
  satz           :  string;
  ok             :  boolean;
  position       :  longint;

  { Zwischendateien vor der Umsortierung }
  pZwFileName: array [0..255] of char;

  error : integer;
  first : boolean;

begin
  Result:=false;
  if not FileExists (PruefKonvRec.Quelldateiname) then exit;

  try
    { temporäres Zwischenfile erzeugen: }
    GetTempFileName (pchar (ExtractFilePath(PruefKonvRec.Zieldateiname)), 'Prf', 0, pZwFileName);
    fs_work:=TFileOfRecStream.Create (string (pZwFileName), fmOpenReadWrite OR fmShareDenyWrite,
                                      SizeOf (PruefRec));
    try
      { Rohfile öffnen: }
      fs_roh:=TFileOfCharStream.Create (PruefKonvRec.QuelldateiName, fmOpenRead OR fmShareDenyWrite);
      try
        FSize_roh:=fs_roh.Size;

        first:=true;
        while (fs_roh.Position < FSize_roh) do begin
          satz:='';
          zeichen:=#0;
          while (fs_roh.Position < FSize_roh) and (zeichen<>#$1f) and
                (length(satz)<=l_rohsatz) and (zeichen<>#$03) do begin
            fs_roh.Read (zeichen);
            satz:=satz+zeichen;
          end;

          Application.ProcessMessages;

          if first then
          begin { STX und X abschneiden }
            satz := copy(satz, 3, length(satz)-2);
            first := false;
          end;

          if length(satz) = l_PRFSatz4013Q then begin
            with p_satz do begin
              kanal:=satz[2];
              with von_datum,von_zeit do begin
                val(copy(satz,3,2),year,error);
                ok:=error=0;
                if year > 80 then year:=1900+year
                             else year:=2000+year;
                val(copy(satz,5,2),month,error);
                if ok then ok:=error=0;
                val(copy(satz,7,2),day,error);
                if ok then ok:=error=0;
                val(copy(satz,9,2),hour,error);
                if ok then ok:=error=0;
                val(copy(satz,11,2),min,error);
                if ok then ok:=error=0;
                val(copy(satz,13,2),sec,error);
                if ok then ok:=error=0;
                hsec:=0;
              end;
              with bis_datum,bis_zeit do begin
                val(copy(satz,15,2),year,error);
                if ok then ok:=error=0;
                if year > 80 then year:=1900+year
                             else year:=2000+year;
                val(copy(satz,17,2),month,error);
                if ok then ok:=error=0;
                val(copy(satz,19,2),day,error);
                if ok then ok:=error=0;
                val(copy(satz,21,2),hour,error);
                if ok then ok:=error=0;
                val(copy(satz,23,2),min,error);
                if ok then ok:=error=0;
                val(copy(satz,25,2),sec,error);
                if ok then ok:=error=0;
                hsec:=0;
              end;
              wert[1]:=copy(satz,27,5);
              wert[2]:=copy(satz,32,5);
              wert[3]:=copy(satz,37,5);
              wert[4]:=copy(satz,42,5);

              if ok then
                fs_work.WriteRec (p_satz);
            end;      { with p_satz }
          end;        { Länge=47 }
        end;          { while not EOF }
      finally
        fs_roh.Free;
      end;

      if PruefKonvRec.loeschen then
        DeleteFile (PruefKonvRec.QuelldateiName);   { löschen falls vorhanden }

{------------------------------------------------------------------------------}

      { Prüfsatz-Zielfile erzeugen: }
      fs_LGZPr:=TFileOfRecStream.Create (PruefKonvRec.ZielDateiName, fmCreate, SizeOf (PruefRec));
      try
        position:=fs_work.RecCount - 1;
        while position >= 0 do
        begin
          Application.ProcessMessages;
          fs_work.SeekRec (position, soFromBeginning);
          fs_work.ReadRec (p_satz);
          fs_LGZPr.WriteRec (p_satz);
          dec(position);
        end;
      finally
        fs_LGZPr.Free;
      end;
    finally
      fs_work.Free;
    end;

    if PruefKonvRec.loeschen then
      DeleteFile (string (pZwFileName));   { löschen falls vorhanden }

    Result:=true;
  except
    Result:=false;
    exit;
  end;
end;     { Prüfsatzkonvertierung }

end.
