{******************************************************************************}
{* Unit: Basisroutinen für IEC 870-Telegramme                                 *}
{* 30.05.2007  WW                                                             *}
{* 23.04.2012  WW  Get_IEC870_WertString_Messwert_normiert: Bugfix Division   *}
{*                 durch Null                                                 *}
{******************************************************************************}
unit Iec870Util;

interface

uses
  SysUtils, DateUtils, WChars, IecConst, IecIniFile, T_BinMask;

const
  C_IEC870_104_MaxTelegrFolgenummer = 32767;  // max. Wert für Sende-und Empfangsfolgenummer IEC 870-5-104


procedure IEC870_InfoObj_IntToBytes (iInfoObj: integer; var bInfoObj_1: byte;
  var bInfoObj_2: byte; var bInfoObj_3: byte);
function Get_IEC870_LaengeDatenfeld (aTypKennung: byte): integer;

function Get_IEC870_Informationsobjekt_Mess (aTypKennung: byte;
  aInfoObj_low, aInfoObj_medium, aInfoObj_high: byte; aInfoObj_Bytes: byte;
  dMesswert: double; aMesswert_gueltig: boolean; aDatumZeit: TDateTime;
  aAufzMax_MW_normiert, aAufzMin_MW_normiert: double): string;
function Get_IEC870_Informationsobjekt_Meld (aTypKennung: byte;
  aInfoObj_low, aInfoObj_medium, aInfoObj_high: byte; aInfoObj_Bytes: byte;
  aMeldung_ein: boolean; aDatumZeit: TDateTime): string;
function Get_IEC870_Informationsobjekt_Generalabfrage (aInfoObj_Bytes: byte;
  aQOI: byte): string;

function Get_IEC870_101_Checksumme_Modulo256 (S: string): integer;
function Get_IEC870_101_FCV (aFktCode: integer): boolean;

function Get_IEC870_101_TelegrammFesteLaenge_Unsymmetrisch_Sekundaer (aACD, aDFC: boolean;
  aFktCode: integer; aLinienNr: integer; aAdressfeld_Bytes: byte): string;
function Get_IEC870_101_TelegrammFesteLaenge_Symmetrisch_Primaer (aDIR, aFCB, aFCV: boolean;
  aFktCode: integer; aLinienNr: integer; aAdressfeld_Bytes: byte): string;
function Get_IEC870_101_TelegrammFesteLaenge_Symmetrisch_Sekundaer (aDIR, aDFC: boolean;
  aFktCode: integer; aLinienNr: integer; aAdressfeld_Bytes: byte): string;

function Get_IEC870_101_TelegrammDaten_Unsymmetrisch_Sekundaer (aACD, aDFC: boolean;
  aFktCode: integer; aLinienNr: integer; aUrsache: byte; aHerkunftsadresse: integer;
  aTypKennung: byte; aStationsnummer: integer; aObjekte: string;
  aAnzahlObjekte: byte; aAdressfeld_Bytes, aASDU_Bytes: byte): string;
function Get_IEC870_101_TelegrammDaten_Symmetrisch_Primaer (aDIR, aFCB, aFCV: boolean;
  aFktCode: integer; aLinienNr: integer; aUrsache: byte; aHerkunftsadresse: integer;
  aTypKennung: byte; aStationsnummer: integer; aObjekte: string;
  aAnzahlObjekte: byte; aAdressfeld_Bytes, aASDU_Bytes: byte): string;

procedure Inc_IEC870_104_Telegrammfolgenummer (var aTelegrFolgeNr: integer);
function Get_IEC870_104_Telegramm_Format_S (aEmpfFolgeNr: integer): string;
function Get_IEC870_104_Telegramm_Format_U (aFunktion_U: byte): string;
function Is_IEC870_104_con_Funktion_U (aFunktion_U: byte): boolean;

function Get_IEC870_104_TelegrammDaten_I (aSendeFolgeNr, aEmpfFolgeNr: integer;
  aUrsache: byte; aHerkunftsadresse: integer; aTypKennung: byte;
  aStationsnummer: integer; aObjekte: string; aAnzahlObjekte: byte;
  aAdressfeld_Bytes: byte; aASDU_Bytes: byte): string;


function Decode_IEC870_ZeitString_CP24Time2a (aZeitString: string;
  var msec, min: word; var IV: boolean): boolean;
function Decode_IEC870_ZeitString_CP56Time2a (aZeitString: string;
  var msec, min, hour, day, month, year: word; var IV, SU: boolean): boolean;



implementation


{------------------- IEC 870-5 allgemein: Informationsobjekte -----------------}

{---------------------------------------------------------------}
function Get_IEC870_LaengeDatenfeld (aTypKennung: byte): integer;
{---------------------------------------------------------------}
{ liefert die Typkennungs-abhängige Länge des Datenfelds (ohne Info-Objektadresse);
  Übergabe: Typkennung
  Ergebnis: Datenfeldlänge (0 = Fehler, keine Berechnung möglich wegen
                                unbekannter Typkennung }
const
  CLenMeldung             = 1;  // Meldung mit Qualitätskennung
  CLenMesswert_normiert   = 2;
  CLenMesswert_Gleitkomma = 4;
  CLenZaehlwert           = 4;
  CLenQualitaetskennung   = 1;
  CLenCP24Time2a          = 3;
  CLenCP56Time2a          = 7;
  CLenInitUrsache         = 1;  // COI = Initialisierungsursache
  CLenAbfrageKennung      = 1;  // QOI = Abfragekennung

begin
  case aTypKennung of
    C_TK_Einzelmeldung:
      Result:=CLenMeldung;

    C_TK_Einzelmeldung_CP24Time2a:
      Result:=CLenMeldung + CLenCP24Time2a;

    C_TK_Einzelmeldung_CP56Time2a:
      Result:=CLenMeldung + CLenCP56Time2a;

    C_TK_Messwert_normiert:
      Result:=CLenMesswert_normiert + CLenQualitaetskennung;

    C_TK_Messwert_normiert_CP24Time2a:
      Result:=CLenMesswert_normiert + CLenQualitaetskennung + CLenCP24Time2a;

    C_TK_Messwert_normiert_CP56Time2a:
      Result:=CLenMesswert_normiert + CLenQualitaetskennung + CLenCP56Time2a;

    C_TK_Messwert_Gleitkomma:
      Result:=CLenMesswert_Gleitkomma + CLenQualitaetskennung;

    C_TK_Messwert_Gleitkomma_CP24Time2a:
      Result:=CLenMesswert_Gleitkomma + CLenQualitaetskennung + CLenCP24Time2a;

    C_TK_Messwert_Gleitkomma_CP56Time2a:
      Result:=CLenMesswert_Gleitkomma + CLenQualitaetskennung + CLenCP56Time2a;

    C_TK_Zaehlwert:
      Result:=CLenZaehlwert + CLenQualitaetskennung;

    C_TK_Zaehlwert_CP24Time2a:
      Result:=CLenZaehlwert + CLenQualitaetskennung + CLenCP24Time2a;

    C_TK_Zaehlwert_CP56Time2a:
      Result:=CLenZaehlwert + CLenQualitaetskennung + CLenCP56Time2a;

    C_TK_Initialisierungsende:
      Result:=CLenInitUrsache;

    C_TK_Generalabfrage:  
      Result:=CLenAbfrageKennung;
  else  // unbekannte Typkennung
    Result:=0;  // keine Objekte
  end;
end;

{---------------------------------------------------------------------------}
procedure IEC870_InfoObj_IntToBytes (iInfoObj: integer; var bInfoObj_1: byte;
  var bInfoObj_2: byte; var bInfoObj_3: byte);
{---------------------------------------------------------------------------}
{ wandelt InfoObjekt-Adresse von Integer in Bytes (1 = niedrigstes Byte) }
var
  iBuf: integer;
begin
  iBuf:=iInfoObj;

  bInfoObj_3:=iBuf DIV (256*256);                                { 2e23..2e16 }
  iBuf:=iBuf MOD (256*256);
  bInfoObj_2:=iBuf DIV 256;                                       { 2e15..2e8 }
  bInfoObj_1:=iBuf MOD 256;                                        { 2e7..2e0 }
end;

{----------------------------------------------------------------}
function Get_IEC870_WertString_Zaehlwert (aWert: longint): string;
{----------------------------------------------------------------}
{ Umrechnung Ganzzahl-Wert (32-Bit-Integer mit Vorzeichen) in IEC-Zählwert;
  02.02.2009: Bugfix negative Werte }
var
  HexStr: string;
  ErgStr: string;
  iBuf: integer;

begin
  HexStr:=IntToHex (aWert, 8);

  { Byte-Reihenfolge umdrehen: }
  iBuf:=StrToInt ('$' + HexStr[7] + HexStr[8]);
  ErgStr:=char(iBuf);
  iBuf:=StrToInt ('$' + HexStr[5] + HexStr[6]);
  ErgStr:=ErgStr + char(iBuf);
  iBuf:=StrToInt ('$' + HexStr[3] + HexStr[4]);
  ErgStr:=ErgStr + char(iBuf);
  iBuf:=StrToInt ('$' + HexStr[1] + HexStr[2]);
  Result:=ErgStr + char(iBuf);
end;

{--------------------------------------------------------------}
function Get_IEC870_WertString_Messwert_normiert (aWert: double;
  aWert_mit_Offset: boolean; aAufzMax: double; fAufzMin: double = 0): string;
{--------------------------------------------------------------}
{ Umrechnung Analogwert als Ganzzahl (z.B. normierte MRG-LGZ-Daten im Bereich 0 .. 9999;
  MRG 910-Originalwerte 0..4095) in normierten IEC-Meßwert (-1 .. 0 .. +1 - 2 hoch 15)
  -> Wert wird als double übergeben, da MRG 910-Originalwerte mit Faktor 1,25
     beaufschlagt sind (ansonsten Rundungsfehler bei Integer-Übergabe !)
  Übergabe: Analogwert
            Wert_mit_Offset (auf true setzen, wenn LGZ-Werte im Bereich 2000 .. 9999)
            AufzMax (Aufzeichnungsmaximum des Analogwerts, z.B. normierte LGZ-Werte: 9999) }
var
  W1,W2: byte;
  negativ: boolean;
  halbes_AufzMax: double;
  W: longint;
  fVal : double;
  fMax : double;
begin
  // Werte auf untere Grenze = 0 normieren 
  fVal := aWert - fAufzMin;
  fMax := aAufzMax - fAufzMin;
  { Eingangs-Wertebereich sicherheitshalber einschränken: }
  if (fVal > fMax) then fVal := fMax;
  if fVal < 0 then fVal := 0;

  { Wertumsetzung: }
  if aWert_mit_Offset then begin
    if fVal < 2000 then fVal:=2000;
    fVal := fVal - 2000;                           { 2000 .. 9999 -> 0 ..7999 }
    if (fVal >= 4000) then begin                  { 4000 .. 7999 -> 0 .. 32767 }
      fVal := fVal - 4000;
      negativ := false;
    end else                                        { 0 .. 3999 -> 0 .. 32767 }
      negativ := true;
    W := Round((fVal / (4000-1)) * 32767);
  end
  else begin
    halbes_AufzMax := (fMax) / 2;  // die Hälfte des Aufzeichnungsbereichs

    if halbes_AufzMax <> 0 then begin
      if (fVal >= halbes_AufzMax) then begin  { norm. LGZ: 5000 .. 9999 -> 0 .. 32767 }
        fVal := fVal - halbes_AufzMax;
        negativ := false;
      end
      else begin                              { norm. LGZ: 0 .. 4999 -> 0 .. 32767 }
        fVal := fVal;
        negativ := true;
      end;
      W := Round((fVal / halbes_AufzMax) * 32767);
    end
    else begin  // Bugfix Division durch null; 23.04.2012, WW
      W:=0;
      negativ:=false;
    end;
  end;

  W2:=W DIV 256;                                               { 2e15..2e8 }
  if negativ then
    W2:=W2 OR $80;                                     { Vorzeichenbit setzen }
  W1:=W MOD 256;                                               { 2e7..2e0 }
  Result:=char(W1)+char(W2);
end;

{-------------------------------------------------------------------------}
function Get_IEC870_WertString_Messwert_Gleitkomma (aWert: double): string;
{-------------------------------------------------------------------------}
{ Umrechnung Gleitkomma-Messwert (z.B. DSfG-Analogwerte, Elster DL-240 und EK260
  Analogwerte, RMG EC 694 Analogwerte -> alles physikalische Werte) in IEC-Meßwert,
  verkürzte Gleitkommazahl }
var
  L: longint;
  PSingle: ^single;
  P: pointer;
  HexStr: string;
  ErgStr: string;
  iBuf: integer;

begin
  New (PSingle);
  try
    PSingle^:=aWert;
    P:=PSingle;
    l:=longint(P^);
  finally
    Dispose (PSingle);
  end;
  HexStr:=IntToHex (l, 8);

  { Byte-Reihenfolge umdrehen: }
  iBuf:=StrToInt ('$' + HexStr[7] + HexStr[8]);
  ErgStr:=char(iBuf);
  iBuf:=StrToInt ('$' + HexStr[5] + HexStr[6]);
  ErgStr:=ErgStr + char(iBuf);
  iBuf:=StrToInt ('$' + HexStr[3] + HexStr[4]);
  ErgStr:=ErgStr + char(iBuf);
  iBuf:=StrToInt ('$' + HexStr[1] + HexStr[2]);
  Result:=ErgStr + char(iBuf);
end;

{------------------------------------------------------------------------------}
function Get_IEC870_ZeitString_2a (aBytes: byte; aDatumZeit: TDateTime): string;
{------------------------------------------------------------------------------}
{ Umrechnung Datum/Zeit als TDateTime in IEC-Zeitmarke 'Duale Zeit 2a' mit
  3 Bytes (CP24Time2a) oder 7 Bytes (CP56Time2a) }
var
  W1,W2: byte;
  year, month, day: word;
  hour, min, sec, msec: word;

begin
  Result:='';
  if aBytes = 0 then exit;  // keine Zeitmarke

  DecodeDateTime (aDatumZeit, year, month, day, hour, min, sec, msec);
  msec:=(sec * 1000) + msec;   { Sekunden in Millisekunden umrechnen }
  { Millisekunden low, high: }
  W2:=msec DIV 256;  { 2e15..2e8 }
  W1:=msec MOD 256;  { 2e7..2e0 }
  Result:=char(W1)+char(W2);

  { IV, R1, Minuten: }
  Result:=Result + char(min);  { 3-Byte-Zeitmarke }
  if aBytes <= 3 then exit;

  { SU, R2, Stunden: }
  Result:=Result + char(hour);
  { Wochentag, Tag des Monats: }
  Result:=Result + char(day);
  { R3, Monat: }
  Result:=Result + char(month);
  { R4, Jahr zweistellig: }
  year:=year mod 100;
  Result:=Result + char(year);  { 7-Byte-Zeitmarke }
end;

{----------------------------------------------------------------}
function Decode_IEC870_ZeitString_CP24Time2a (aZeitString: string;
  var msec, min: word; var IV: boolean): boolean;
{----------------------------------------------------------------}
{ Decodierung der IEC-Zeitmarke CP24Time2a (3 Bytes);
  Übergabe: Zeitstring
  Rückgabe: Millisekunden
            Minuten
            Gültigkeits-Bit IV
  Ergebnis: true, wenn ZeitString gültig }
var
  S: string;
  b: byte;

begin
  Result:=false;
  // Vorbelegung Rückgaben
  msec:=0;
  min:=0;
  IV:=true;

  if length (aZeitString) <> 3 then exit;  // Längenfehler IEC-Zeitmarke

  { Millisekunden low, high: }
  S:=Copy (aZeitString, 1, 2);
  msec:=Bin2Word (S);

  b:=Bin2Byte (Copy (aZeitString, 3, 1));
  { IV: }
  IV:=(b AND $80) <> 0;
  { Minuten: }
  min:=b AND $3F;
  Result:=true;
end;


{---------------------------------------------------------------------------}
function Decode_IEC870_ZeitString_CP56Time2a (aZeitString: string;
  var msec, min, hour, day, month, year: word; var IV, SU: boolean): boolean;
{---------------------------------------------------------------------------}
{ Decodierung der IEC-Zeitmarke CP24Time2a (3 Bytes);
  Übergabe: Zeitstring
  Rückgabe: Millisekunden
            Minuten
            Stunden
            Tag
            Monat
            Jahr
            Gültigkeits-Bit IV
            Sommerzeit-Bit SU
  Ergebnis: true, wenn ZeitString gültig }
var
  S: string;
  b: byte;

begin
  Result:=false;
  // Vorbelegung Rückgaben
  msec:=0;
  min:=0;
  hour:=0;
  day:=0;
  month:=0;
  year:=0;
  IV:=true;
  SU:=false;

  if length (aZeitString) <> 7 then exit;  // Längenfehler IEC-Zeitmarke

  { Millisekunden low, high: }
  S:=Copy (aZeitString, 1, 2);
  msec:=Bin2Word (S);

  b:=Bin2Byte (Copy (aZeitString, 3, 1));
  { IV: }
  IV:=(b AND $80) <> 0;
  { Minuten: }
  min:=b AND $3F;

  b:=Bin2Byte (Copy (aZeitString, 4, 1));
  { SU: }
  SU:=(b AND $80) <> 0;
  { Stunden: }
  hour:=b AND $1F;

  b:=Bin2Byte (Copy (aZeitString, 5, 1));
  { Tag: }
  day:=b AND $1F;

  b:=Bin2Byte (Copy (aZeitString, 6, 1));
  { Monat: }
  month:=b AND $0F;

  b:=Bin2Byte (Copy (aZeitString, 7, 1));
  { Jahr: }
  year:=b AND $7F;
  if year < 80 then
    year:=year + 2000
  else
    year:=year + 1900;
  Result:=true;
end;

{-------------------------------------------------------------------------}
function Get_IEC870_Informationsobjekt_Mess (aTypKennung: byte;
  aInfoObj_low, aInfoObj_medium, aInfoObj_high: byte; aInfoObj_Bytes: byte;
  dMesswert: double; aMesswert_gueltig: boolean; aDatumZeit: TDateTime;
  aAufzMax_MW_normiert, aAufzMin_MW_normiert: double): string;
{-------------------------------------------------------------------------}
{ Informationsobjekte-String für Messdaten zusammenstellen }
var
  S: string;
  iWert: Int64;

begin
  { Infoobjektadresse: }
  S:=char(aInfoObj_low);  { 1 Byte InfoObj-Adresse }
  if aInfoObj_Bytes > 2 then
    S:=S + char(aInfoObj_medium) + char(aInfoObj_high)  { 3 Byte InfoObj-Adresse }
  else if aInfoObj_Bytes > 1 then
    S:=S + char(aInfoObj_high);  { 2 Byte InfoObj-Adresse }

  if (aTypKennung = C_TK_Messwert_Gleitkomma) OR
     (aTypKennung = C_TK_Messwert_Gleitkomma_CP24Time2a) OR
     (aTypKennung = C_TK_Messwert_Gleitkomma_CP56Time2a) then begin
    { Wert: }
    S:=S + Get_IEC870_WertString_Messwert_Gleitkomma(dMesswert);
    { IV, NT, SB, BL, OV: }
    if not aMesswert_gueltig then
      S:=S + #128  { IV=1: Wert ist ungültig }
    else
      S:=S + #0;   { IV=0: Wert ist gültig }
  end
  else if (aTypKennung = C_TK_Messwert_normiert) OR
          (aTypKennung = C_TK_Messwert_normiert_CP24Time2a) OR
          (aTypKennung = C_TK_Messwert_normiert_CP56Time2a) then begin
    { Wert: }
    S:=S + Get_IEC870_WertString_Messwert_normiert(
      dMesswert, false, aAufzMax_MW_normiert, aAufzMin_MW_normiert);
    { IV, NT, SB, BL, OV: }
    if not aMesswert_gueltig then
      S:=S + #128  { IV=1: Wert ist ungültig }
    else
      S:=S + #0;   { IV=0: Wert ist gültig }
  end
  else if (aTypKennung = C_TK_Zaehlwert) OR
          (aTypKennung = C_TK_Zaehlwert_CP24Time2a) OR
          (aTypKennung = C_TK_Zaehlwert_CP56Time2a) then begin
    { Wert: es werden nur maximal 9 gültige Stellen (die niederwertigsten) übertragen,
            da das Format nur 32-Bit-Integer vorsieht }
    iWert:=Round (dMesswert);
    iWert:=iWert MOD 1000000000;
    S:=S + Get_IEC870_WertString_Zaehlwert(iWert);
    { IV, CA, CY, Sequenznummer: }
    if not aMesswert_gueltig then
      S:=S + #128  { IV=1: Wert ist ungültig }
    else
      S:=S + #0;   { IV=0: Wert ist gültig }
  end
  else begin  // Typkennung unbekannt
    Result:='';
    exit;
  end;

  { Zeitmarke: 3-Byte, 7-Byte oder ohne }
  if (aTypKennung = C_TK_Messwert_Gleitkomma_CP24Time2a) OR
     (aTypKennung = C_TK_Messwert_normiert_CP24Time2a) OR
     (aTypKennung = C_TK_Zaehlwert_CP24Time2a) then
    S:=S + Get_IEC870_ZeitString_2a (3, aDatumZeit)  { 3-Byte-Zeitmarke }
  else if (aTypKennung = C_TK_Messwert_Gleitkomma_CP56Time2a) OR
     (aTypKennung = C_TK_Messwert_normiert_CP56Time2a) OR
     (aTypKennung = C_TK_Zaehlwert_CP56Time2a) then
    S:=S + Get_IEC870_ZeitString_2a (7, aDatumZeit);  { 7-Byte-Zeitmarke }
  Result:=S;
end;

{-------------------------------------------------------------------------}
function Get_IEC870_Informationsobjekt_Meld (aTypKennung: byte;
  aInfoObj_low, aInfoObj_medium, aInfoObj_high: byte; aInfoObj_Bytes: byte;
  aMeldung_ein: boolean; aDatumZeit: TDateTime): string;
{-------------------------------------------------------------------------}
{ Informationsobjekte-String für Meldungen zusammenstellen }
var
  S: string;

begin
  { Infoobjektadresse: }
  S:=char(aInfoObj_low);  { 1 Byte InfoObj-Adresse }
  if aInfoObj_Bytes > 2 then
    S:=S + char(aInfoObj_medium) + char(aInfoObj_high)  { 3 Byte InfoObj-Adresse }
  else if aInfoObj_Bytes > 1 then
    S:=S + char(aInfoObj_high);  { 2 Byte InfoObj-Adresse }

  { IV, NT, SB, BL, SPI: }
  if aMeldung_ein then
    S:=S + #1
  else
    S:=S + #0;

  { Zeitmarke: 3-Byte, 7-Byte oder ohne }
  if (aTypKennung = C_TK_Einzelmeldung_CP24Time2a) then
    S:=S + Get_IEC870_ZeitString_2a (3, aDatumZeit)  { 3-Byte-Zeitmarke }
  else if (aTypKennung = C_TK_Einzelmeldung_CP56Time2a) then
    S:=S + Get_IEC870_ZeitString_2a (7, aDatumZeit);  { 7-Byte-Zeitmarke }
  Result:=S;
end;

{--------------------------------------------------------------------------}
function Get_IEC870_Informationsobjekt_Generalabfrage (aInfoObj_Bytes: byte;
  aQOI: byte): string;
{--------------------------------------------------------------------------}
{ Informationsobjekt-String für Generalabfrage-Telegramm zusammenstellen }
var
  S: string;

begin
  { Infoobjektadresse = 0: }
  S:=char(0);  { 1 Byte InfoObj-Adresse }
  if aInfoObj_Bytes > 2 then
    S:=S + char(0) + char(0)  { 3 Byte InfoObj-Adresse }
  else if aInfoObj_Bytes > 1 then
    S:=S + char(0);  { 2 Byte InfoObj-Adresse }

  { QOI (Abfragekennung): }
  S:=S + char (aQOI);

  Result:=S;
end;


{------------------- IEC 870-5-101: Checksumme/Steuerfelder -------------------}

{----------------------------------------------------------------}
function Get_IEC870_101_Checksumme_Modulo256 (S: string): integer;
{----------------------------------------------------------------}
{ Ergebnis: Modulo-256-Prüfsumme über S }
var
  CS: integer;
  i: integer;

begin
  CS:=0;
  for i:=1 to length(S) do
    CS:=CS + integer(S[i]);
  Result:=CS MOD 256;
end;

{-------------------------------------------------------}
function Get_IEC870_101_FCV (aFktCode: integer): boolean;
{-------------------------------------------------------}
{ liefert dem Funktionscode zugeordnetes FCV-Bit (Telegrammfolgebit gültig) für
  Telegramme der Primärstation }
begin
  case aFktCode of
    C_FKT_PRM_2_LINK_TEST,
    C_FKT_PRM_3_DATEN_CONFIRM,
    C_FKT_PRM_10_ABFRAGE_DATEN_KLASSE1,
    C_FKT_PRM_11_ABFRAGE_DATEN_KLASSE2:
      Result:=true;  { FCV=1 }
  else
    Result:=false;  { FCV=0 }
  end;
end;

{------------------------------------------------------------------------------}
function Get_IEC870_101_Steuerfeld_Unsymmetrisch_Sekundaer (aACD, aDFC: boolean;
  aFktCode: integer): string;
{------------------------------------------------------------------------------}
{ Steuerfeld-Byte für unsymmetrische Übertragung zusammenstellen (Sekundär-
  an Primärstation) }
var
  b: byte;
begin
  b:=aFktCode AND $0F;  { RES=0, PRM=0, ACD=0, DFC=0, Funktion }
  if aACD then
    b:=b OR $20;  { ACD=1 (Zugriffsanforderung Daten Klasse 1) }
  if aDFC then
    b:=b OR $10;  { DFC=1 (Datenflußsteuerung) }
  Result:=char (b);
end;

{--------------------------------------------------------------------------}
function Get_IEC870_101_Steuerfeld_Symmetrisch_Primaer (aDIR, aFCB: boolean;
  aFCV: boolean; aFktCode: integer): string;
{--------------------------------------------------------------------------}
{ Steuerfeld-Byte für symmetrische Übertragung zusammenstellen (Primär-
  an Sekundärstation) }
var
  b: byte;
begin
  b:=$40 OR (aFktCode AND $0F);  { DIR=0, PRM=1, FCB=0, FCV=0, Funktion }
  if aDIR then
    b:=b OR $80;  { DIR=1 (phys. Übertragungsrichtung) }
  if aFCB then
    b:=b OR $20;  { FCB=1 (Telegrammfolgebit) }
  if aFCV then
    b:=b OR $10;  { FCV=1 (Telegrammfolgebit gültig) }
  Result:=char (b);
end;

{----------------------------------------------------------------------------}
function Get_IEC870_101_Steuerfeld_Symmetrisch_Sekundaer (aDIR, aDFC: boolean;
  aFktCode: integer): string;
{----------------------------------------------------------------------------}
{ Steuerfeld-Byte für symmetrische Übertragung zusammenstellen (Sekundär-
  an Primärstation) }
var
  b: byte;
begin
  b:=aFktCode AND $0F;  { DIR=0, PRM=0, RES=0, DFC=0, Funktion }
  if aDIR then
    b:=b OR $80;  { DIR=1 (phys. Übertragungsrichtung) }
  if aDFC then
    b:=b OR $10;  { DFC=1 (Datenflußsteuerung) }
  Result:=char (b);
end;


{------------------- IEC 870-5-101: Telegramme fester Länge -------------------}

{----------------------------------------------------------------}
function Get_IEC870_101_TelegrammFesteLaenge (aSteuerfeld: string;
  aLinienNr: integer; aAdressfeld_Bytes: byte): string;
{----------------------------------------------------------------}
{ IEC-870-5-101-Telegramm fester Länge zusammenstellen }
var
  sCS: string;
  CS: integer;
  sTelegramm: string;
  W1,W2: byte;

begin
  { Startzeichen: }
  sTelegramm:=char(C_START_FEST);
  { Steuerfeld: }
  sTelegramm:=sTelegramm + aSteuerfeld;
  { Liniennummer: }
  if aAdressfeld_Bytes > 0 then begin
    if aAdressfeld_Bytes > 1 then begin
      { Liniennummer low, high: }
      W2:=aLinienNr DIV 256;  { 2e15..2e8 }
      W1:=aLinienNr MOD 256;  { 2e7..2e0 }
      sTelegramm:=sTelegramm + char(W1) + char(W2);  { 2 Byte Liniennummer }
    end else
      sTelegramm:=sTelegramm + char(aLinienNr);  { 1 Byte Liniennummer }
  end;
  { Checksumme: }
  sCS:=Copy (sTelegramm, 2, length (sTelegramm));  // String, über den Checksumme gebildet wird
  CS:=Get_IEC870_101_Checksumme_Modulo256(sCS);
  sTelegramm:=sTelegramm + char(CS);
  { Stopzeichen: }
  sTelegramm:=sTelegramm + char(C_STOP);

  Result:=sTelegramm;
end;

{----------------------------------------------------------------------------------------}
function Get_IEC870_101_TelegrammFesteLaenge_Unsymmetrisch_Sekundaer (aACD, aDFC: boolean;
  aFktCode: integer; aLinienNr: integer; aAdressfeld_Bytes: byte): string;
{----------------------------------------------------------------------------------------}
{ IEC-870-5-101-Telegramm fester Länge für unsymmetrische Übertragung zusammenstellen
  (Sekundär- an Primärstation) }
var
  StFeld: string;

begin
  StFeld:=Get_IEC870_101_Steuerfeld_Unsymmetrisch_Sekundaer (aACD, aDFC, aFktCode);  // Steuerfeld zusammensetzen
  Result:=Get_IEC870_101_TelegrammFesteLaenge (StFeld, aLinienNr, aAdressfeld_Bytes);
end;

{------------------------------------------------------------------------------------------}
function Get_IEC870_101_TelegrammFesteLaenge_Symmetrisch_Primaer (aDIR, aFCB, aFCV: boolean;
  aFktCode: integer; aLinienNr: integer; aAdressfeld_Bytes: byte): string;
{------------------------------------------------------------------------------------------}
{ IEC-870-5-101-Telegramm fester Länge für symmetrische Übertragung zusammenstellen
  (Primär- an Sekundärstation) }
var
  StFeld: string;

begin
  StFeld:=Get_IEC870_101_Steuerfeld_Symmetrisch_Primaer (aDIR, aFCB, aFCV, aFktCode);  // Steuerfeld zusammensetzen
  Result:=Get_IEC870_101_TelegrammFesteLaenge (StFeld, aLinienNr, aAdressfeld_Bytes);
end;

{--------------------------------------------------------------------------------------}
function Get_IEC870_101_TelegrammFesteLaenge_Symmetrisch_Sekundaer (aDIR, aDFC: boolean;
  aFktCode: integer; aLinienNr: integer; aAdressfeld_Bytes: byte): string;
{--------------------------------------------------------------------------------------}
{ IEC-870-5-101-Telegramm fester Länge für symmetrische Übertragung zusammenstellen
  (Sekundär- an Primärstation) }
var
  StFeld: string;

begin
  StFeld:=Get_IEC870_101_Steuerfeld_Symmetrisch_Sekundaer (aDIR, aDFC, aFktCode);  // Steuerfeld zusammensetzen
  Result:=Get_IEC870_101_TelegrammFesteLaenge (StFeld, aLinienNr, aAdressfeld_Bytes);
end;


{------------ IEC 870-5-101/104: Datentelegramm (variable Länge) --------------}

{------------------------------------------------------------------------------------}
function Get_IEC870_TelegrammDaten (aIEC870_Norm: TIEC870_Norm;
  aSteuerfeld: string; aLinienNr: integer; aUrsache: byte; aHerkunftsadresse: integer;
  aTypKennung: byte; aStationsnummer: integer; aObjekte: string;
  aAnzahlObjekte: byte; aAdressfeld_Bytes, aASDU_Bytes: byte): string;
{------------------------------------------------------------------------------------}
{ IEC-870-5-101/104-Datentelegramm zusammenstellen (variable Länge) }
var
  CS: integer;
  l: integer;
  StLNr: string;
  UeStatNr: string;
  ADB: string;
  W1,W2: byte;
  sTelegramm: string;

begin
  { Steuerfeld: }
  StLNr:=aSteuerfeld;

  { Liniennummer: }
  if aAdressfeld_Bytes > 0 then begin
    if aAdressfeld_Bytes > 1 then begin
      { Liniennummer low, high: }
      W2:=aLinienNr DIV 256;  { 2e15..2e8 }
      W1:=aLinienNr MOD 256;  { 2e7..2e0 }
      StLNr:=StLNr + char(W1) + char(W2);  { 2 Byte Liniennummer }
    end else
      StLNr:=StLNr + char(aLinienNr);  { 1 Byte Liniennummer }
  end;

  { Übertragungsursache: T=0, P/N=0, Ursache }
  UeStatNr:=char(aUrsache AND $3F);
  if aHerkunftsadresse >= 0 then  { 2 Byte Übertragungsursache: mit Herkunftsadresse }
    UeStatNr:=UeStatNr + char(aHerkunftsadresse);

  if aASDU_Bytes > 1 then begin
    { Stationsnummer low, high: }
    W2:=aStationsnummer DIV 256;  { 2e15..2e8 }
    W1:=aStationsnummer MOD 256;  { 2e7..2e0 }
    UeStatNr:=UeStatNr + char(W1) + char(W2);  { 2 Byte ASDU }
  end else
    UeStatNr:=UeStatNr + char(aStationsnummer);  { 1 Byte ASDU }

  { Anwenderdatenbytes zusammensetzen: }
  { Steuerfeld, Liniennummer: }
  ADB:=StLNr;
  { Typ-Kennung: }
  ADB:=ADB + char(aTypKennung);
  { variable Strukturkennung: SQ=0, Anzahl der Objekte }
  ADB:=ADB + char(aAnzahlObjekte AND $7F);
  { Übertragungsursache, Stationsnummer low u. high: }
  ADB:=ADB + UeStatNr;
  { Objekte: }
  ADB:=ADB + aObjekte;

  { Telegrammkomponenten zusammensetzen: }
  case aIEC870_Norm of
    norm_iec870_101:  // Telegramm variabler Länge
      begin
        { Startzeichen: }
        sTelegramm:=char(C_START_VAR);
        { Länge zweimal : }
        l:=length(ADB);
        sTelegramm:=sTelegramm + char(l) + char(l);
        { Startzeichen: }
        sTelegramm:=sTelegramm + char(C_START_VAR);
        { Anwenderdatenbytes: }
        sTelegramm:=sTelegramm + ADB;
        { Checksumme: }
        CS:=Get_IEC870_101_Checksumme_Modulo256(ADB);
        sTelegramm:=sTelegramm + char(CS);
        { Stopzeichen: }
        sTelegramm:=sTelegramm + char(C_STOP);
      end;

    norm_iec870_104:
      begin  // APDU im I-Format (Datenübermittlungsformat)
        { Startzeichen: }
        sTelegramm:=char(C_START_VAR);
        { Länge: }
        l:=length(ADB);
        sTelegramm:=sTelegramm + char (l);
        { Anwenderdatenbytes: }
        sTelegramm:=sTelegramm + ADB;
      end;
  else
    sTelegramm:='';
  end;

  Result:=sTelegramm;
end;


{------------- IEC 870-5-101: Datentelegramme (variable Länge) ----------------}

{----------------------------------------------------------------------------------}
function Get_IEC870_101_TelegrammDaten_Unsymmetrisch_Sekundaer (aACD, aDFC: boolean;
  aFktCode: integer; aLinienNr: integer; aUrsache: byte; aHerkunftsadresse: integer;
  aTypKennung: byte; aStationsnummer: integer; aObjekte: string;
  aAnzahlObjekte: byte; aAdressfeld_Bytes, aASDU_Bytes: byte): string;
{----------------------------------------------------------------------------------}
{ IEC-870-5-101-Datentelegramm für unsymmetrische Übertragung zusammenstellen
  (Sekundär- an Primärstation) }
var
  StFeld: string;

begin
  StFeld:=Get_IEC870_101_Steuerfeld_Unsymmetrisch_Sekundaer (aACD, aDFC, aFktCode);  // Steuerfeld zusammensetzen
  Result:=Get_IEC870_TelegrammDaten (norm_iec870_101, StFeld, aLinienNr, aUrsache,
    aHerkunftsadresse, aTypKennung, aStationsnummer, aObjekte, aAnzahlObjekte,
    aAdressfeld_Bytes, aASDU_Bytes);
end;

{------------------------------------------------------------------------------------}
function Get_IEC870_101_TelegrammDaten_Symmetrisch_Primaer (aDIR, aFCB, aFCV: boolean;
  aFktCode: integer; aLinienNr: integer; aUrsache: byte; aHerkunftsadresse: integer;
  aTypKennung: byte; aStationsnummer: integer; aObjekte: string;
  aAnzahlObjekte: byte; aAdressfeld_Bytes: byte; aASDU_Bytes: byte): string;
{------------------------------------------------------------------------------------}
{ IEC-870-5-101-Datentelegramm für symmetrische Übertragung zusammenstellen
  (Primär- an Sekundärstation) }
var
  StFeld: string;

begin
  StFeld:=Get_IEC870_101_Steuerfeld_Symmetrisch_Primaer (aDIR, aFCB, aFCV, aFktCode);  // Steuerfeld zusammensetzen
  Result:=Get_IEC870_TelegrammDaten (norm_iec870_101, StFeld, aLinienNr, aUrsache,
    aHerkunftsadresse, aTypKennung, aStationsnummer, aObjekte, aAnzahlObjekte,
    aAdressfeld_Bytes, aASDU_Bytes);
end;


{------------------------ IEC 870-5-104: Steuerfelder -------------------------}

{--------------------------------------------------------------------------}
procedure Inc_IEC870_104_Telegrammfolgenummer (var aTelegrFolgeNr: integer);
{--------------------------------------------------------------------------}
{ Telegrammfolgenummer hochzählen. Wenn Max-Wert erreicht wieder bei 0 beginnen. }
begin
  if aTelegrFolgeNr < C_IEC870_104_MaxTelegrFolgenummer then
    inc (aTelegrFolgeNr)
  else
    aTelegrFolgeNr:=0;
end;

{----------------------------------------------------------------------------------}
function Get_IEC870_104_Steuerfeld_I (aSendeFolgeNr, aEmpfFolgeNr: integer): string;
{----------------------------------------------------------------------------------}
{ Steuerfeld zusammenstellen (I-Format) }
var
  i: integer;
  W1,W2: byte;

begin
  { Sendefolgenummer low, high: }
  i:=aSendeFolgeNr SHL 1;
  W2:=i DIV 256;  { 2e14..2e7 }
  W1:=i MOD 256;  { 2e6..2e0 }
  Result:=char (W1) + char (W2);  //  Oktette 1, 2

  { Empfangsfolgenummer low, high: }
  i:=aEmpfFolgeNr SHL 1;
  W2:=i DIV 256;  { 2e14..2e7 }
  W1:=i MOD 256;  { 2e6..2e0 }
  Result:=Result + char (W1) + char (W2);  //  Oktette 3, 4
end;

{-------------------------------------------------------------------}
function Get_IEC870_104_Steuerfeld_S (aEmpfFolgeNr: integer): string;
{-------------------------------------------------------------------}
{ Steuerfeld zusammenstellen (S-Format) }
var
  b: byte;
  i: integer;
  W1,W2: byte;

begin
  b:=$01;  // Oktett 1

  { Empfangsfolgenummer low, high: }
  i:=aEmpfFolgeNr SHL 1;
  W2:=i DIV 256;  { 2e14..2e7 }
  W1:=i MOD 256;  { 2e6..2e0 }
  Result:=char (b) + NUL + char (W1) + char (W2);  //  Oktett 2 = 0
end;

{-------------------------------------------------------------}
function Get_IEC870_104_Steuerfeld_U (aFunktion: byte): string;
{-------------------------------------------------------------}
{ Steuerfeld zusammenstellen (U-Format) }
var
  b: byte;

begin
  b:=(aFunktion AND $FC) OR $03;  // Oktett 1
  Result:=char (b) + NUL + NUL + NUL;  //  Oktette 2, 3, 4 = 0
end;

{------------------------------------------------------------------------}
function Get_IEC870_104_APCI (aLaenge: byte; aSteuerfeld: string): string;
{------------------------------------------------------------------------}
{ APCI zusammenstellen }
var
  sAPCI: string;

begin
  { Startzeichen: }
  sAPCI:=char(C_START_VAR);
  { Länge: }
  sAPCI:=sAPCI + char (aLaenge);
  { Steuerfeld: }
  sAPCI:=sAPCI + aSteuerfeld;
  Result:=sAPCI;
end;


{---------------- IEC 870-5-104: Telegramme im S-und U-Format -----------------}

{-------------------------------------------------------------------------}
function Get_IEC870_104_Telegramm_Format_S (aEmpfFolgeNr: integer): string;
{-------------------------------------------------------------------------}
{ IEC-870-5-104-Telegramm zusammenstellen (S-Format) }
var
  StFeld: string;

begin
  StFeld:=Get_IEC870_104_Steuerfeld_S (aEmpfFolgeNr);
  Result:=Get_IEC870_104_APCI (4, StFeld);  // feste Länge 4
end;

{-------------------------------------------------------------------}
function Get_IEC870_104_Telegramm_Format_U (aFunktion_U: byte): string;
{-------------------------------------------------------------------}
{ IEC-870-5-104-Telegramm zusammenstellen (U-Format) }
var
  StFeld: string;

begin
  StFeld:=Get_IEC870_104_Steuerfeld_U (aFunktion_U);
  Result:=Get_IEC870_104_APCI (4, StFeld);  // feste Länge 4
end;


{-----------------------------------------------------------------}
function Is_IEC870_104_con_Funktion_U (aFunktion_U: byte): boolean;
{-----------------------------------------------------------------}
{ Ergebnis: true, wenn übergebene Steuerfeldfunktion (U-Format) eine Bestätigung
            (con) ist }
begin
  Result:=(aFunktion_U = C_FKT_STARTDT_CON) OR (aFunktion_U = C_FKT_STOPDT_CON) OR
          (aFunktion_U = C_FKT_TESTFR_CON);
end;


{---------------- IEC 870-5-104: Datentelegramm (I-Format) --------------------}

{-----------------------------------------------------------------------------}
function Get_IEC870_104_TelegrammDaten_I (aSendeFolgeNr, aEmpfFolgeNr: integer;
  aUrsache: byte; aHerkunftsadresse: integer; aTypKennung: byte;
  aStationsnummer: integer; aObjekte: string; aAnzahlObjekte: byte;
  aAdressfeld_Bytes: byte; aASDU_Bytes: byte): string;
{-----------------------------------------------------------------------------}
{ IEC-870-5-104-Datentelegramm zusammenstellen (I-Format) }
var
  StFeld: string;

begin
  StFeld:=Get_IEC870_104_Steuerfeld_I (aSendeFolgeNr, aEmpfFolgeNr);  // Steuerfeld zusammensetzen
  Result:=Get_IEC870_TelegrammDaten (norm_iec870_104, StFeld, -1, aUrsache,
    aHerkunftsadresse, aTypKennung, aStationsnummer, aObjekte, aAnzahlObjekte,
    aAdressfeld_Bytes, aASDU_Bytes);
end;

end.

