Unit T_Tools;

INTERFACE

uses
  Windows, Classes, SysUtils, WStrUtils, WinSock;

Type

  DateRec = packed record  { packed, da auch in LGZ-Daten verwendet }
    Year  : SmallInt;
    Month : SmallInt;
    Day   : SmallInt;
  End;

  TimeRec = packed record  { packed, da auch in LGZ-Daten verwendet }
    Hour : SmallInt;
    Min  : SmallInt;
    Sec  : SmallInt;
    Hsec : SmallInt;
  End;

  DStrType = string[10];
  TStrType = string[8];


Function FDateStr (D: DateRec): DStrType;
Function FTimeStr (T: TimeRec): TStrType;
function FilterKennung(Kennung: string): string;
function KennungComp (Kenn_MRG: string; Kenn_Soll: string; Kenn_Ext: string = ''): boolean;
Function Gleich (var Item1, Item2; Anzahl: integer): boolean;
Function F_Check (Dateiname: TFileName): longint;
Function MRGParameter_geaendert (Antwort: string;
                                 var AltStr: string; var NeuStr: string): boolean;
function DSfGDfueNTYMasken_gleich (Maske1, Maske2:  string): boolean;
function DSfGDfueNTYMaske_geaendert (Busadresse: string; WertNeuSoll: string;
                                     Antwort: string; var NeuStr: string): boolean;
function DSfGDfueParameter_geaendert (WertAltPos: integer; Antwort: string;
                                      var AltStr: string; var NeuStr: string): boolean;
function DS100_FFxPos (FFxSatz: string): integer;
procedure DS100_MWFiltern_FFx (Daten: string; var MWDaten: string; AnzStellen_FFxParam: byte);
procedure DS100_MWFiltern_DefFStCode (Daten: string; var MWDaten: string);
procedure Get_DS100_FFADatumZeit (Daten: string; var Jahr: SmallInt; var Monat: SmallInt;
  var Tag: SmallInt; var Stunde: SmallInt; var Minute: SmallInt);
procedure Get_DS100_FFBDatumZeit (Daten: string; var Tag: SmallInt; var Stunde: SmallInt;
  var Minute: SmallInt);
procedure Get_DS100_FFCDatumZeit (Daten: string; var Datum: daterec; var Zeit: timerec);
function BCDToInt (BCD: byte): byte;
function IntToBCD (iInt: byte): byte;
function Int64AsSingle (i64: Int64): single;
function Basis64CodeStrToInt (sCode: string; var Wert: Int64): boolean;
function IntToBasis64CodeStr (Wert: cardinal; Stellen: integer; var sCode: string): integer;
function PalmCreatorIdToInt (sCreatorId: string; var Wert: cardinal): boolean;
function GetXOR (S: string): byte;
function BufToString (var Buf; iBufLen: integer): string;

function CreateTempRohFile (Path: TFileName; Prefix: string): TFileName;
function WriteRohFile (FileName: TFileName; S: string; bOverwrite: boolean = false): boolean;
function WriteRohFileBuf (FileName: TFileName; var Buf; BufLen: integer): boolean;
function ClearRohFile (FileName: TFileName): boolean;

function EscPressed: boolean;
procedure ResetEscPressed;

function GetMittelwert_arith (mw_alt: double; count_alt: integer;
  wert_neu: double): double;
function GetMittelwert_rad (mw_alt: double; count_alt: integer;
  wert_neu: double): double;
function GetMittelwert_quadr (mw_alt: double; count_alt: integer;
  wert_neu: double): double;

function WFileSize (const FileName: string): integer;

function WInAddrToStr (const InAddr: TInAddr): string;
function WIsValidIPv4Address (const sIpAddr: string): boolean;

function TicksElapsed (var LastTickCount: cardinal): cardinal;

IMPLEMENTATION

uses
  T_Zeit;

{---------------------------------------}
function EscPressed: boolean;
{---------------------------------------}
begin
  // Aus der WinAPI-Doku zu GetAsyncKeyState:
  // if the function succeeds, the return value specifies whether the key was pressed
  // since the last call to GetAsyncKeyState, and whether the key is currently up or down.
  // If the most significant bit is set, the key is down, and if the least significant bit is set,
  // the key was pressed after the previous call to GetAsyncKeyState.
  // The return value is zero if a window in another thread or process currently has the keyboard focus
  Result := (GetAsyncKeyState(VK_ESCAPE) and $0001 > 0);
end;

// muss vor dem Benutzen von EscPressed() aufgerufen werden
{---------------------------------------}
procedure ResetEscPressed;
{---------------------------------------}
begin
  GetAsyncKeyState(VK_ESCAPE);
end;

{ Strings }

{---------------------------------------}
Function FDateStr (D: DateRec): DStrType;
{---------------------------------------}
{- Wie DateStr, Leerzeichen werden jedoch durch Nullen ersetzt -}
Var
  DS : String [2];
  MS : String [2];
  JS : String [4];

Begin
  Str (D.Day, DS);
  While Length (DS) < 2 Do DS := '0' + DS;
  Str (D.Month, MS);
  While Length (MS) < 2 Do MS := '0' + MS;
  Str (D.Year, JS);
  While Length (JS) < 4 Do JS := '0' + JS;
  FDateStr := DS + '.' + MS + '.' + JS;
End;

{---------------------------------------}
Function FTimeStr (T: TimeRec): TStrType;
{---------------------------------------}
{- Wie TimeStr, Leerzeichen werden jedoch durch Nullen ersetzt -}
Var
  HS : String [2];
  MS : String [2];
  SS : String [2];
Begin
  Str (T.Hour, HS);
  While Length (HS) < 2 Do HS := '0' + HS;
  Str (T.Min, MS);
  While Length (MS) < 2 Do MS := '0' + MS;
  Str (T.Sec, SS);
  While Length (SS) < 2 Do SS := '0' + SS;
  FTimeStr := HS + ':' + MS + ':' + SS;
End;

{----------------------------------------------}
function FilterKennung(Kennung: string): string;
{----------------------------------------------}
{ von Kennung werden rechts und links Blanks und #0 weggeschnitten und als Ergebnis zur�ck-
  gegeben (es gibt tats�chlich Ger�te die NUL-Zeichen in der Kennung schicken !) }
var
  S: string;
  i:integer;
begin
  { F_LeftTrunc }
  i := 1;
  While (i <= Length (Kennung)) AND (Kennung[i] in [' ',#0]) Do
    Inc (i);
  S := Copy (Kennung, i, Length (Kennung) - i + 1);

  { F_RightTrunc }
  i := Length(S);
  While (i > 0) AND (S[i] in [' ',#0]) Do
    Dec (i);
  Result:= Copy (S, 1, i);
End;

{-----------------------------------------------------------------------------------------}
function KennungComp (Kenn_MRG: string; Kenn_Soll: string; Kenn_Ext: string = ''): boolean;
{-----------------------------------------------------------------------------------------}
{ vergleicht MRG-Kennung mit Soll-Kennung;
  -> f�hrende bzw. abschlie�ende Leerzeichen und Punkte werden nicht ber�cksichtigt
  -> optionale Kennungserweiterung wird beim Vergleich nicht ber�cksichtigt
  �bergaben: Kennung des MRG
             Soll-Kennung f�r Vergleich
             Kennungserweiterung
  Ergebnis: true, wenn MRG-Kennung und Soll-Kennung identisch }
var
  S1, S2: string;
  Len: integer;
begin
  S1:=F_LeftRightTrunc (Kenn_MRG, ' ');
  S1:=StrFilter (S1, '.');
                                                              
  S2:=F_LeftRightTrunc (Kenn_Soll, ' ');
  S2:=StrFilter (S2, '.');
  if length (Kenn_Ext) > 0 then begin
    { optional Kennungserweiterung bei Vergleich ignorieren (f�r STGW); 04.03.2008, WW }
    Len:=length (S2);
    if Len > 0 then
      if S2 [Len] = Kenn_Ext then
        S2:=Copy (S2, 1, Len-1);
  end;

  Result:=S1 = S2;
end;
{-----------------------------------------------}
Function F_Check (Dateiname: TFileName): longint;
{-----------------------------------------------}
{ liefert Filegr��e einer Datei;
  Ergebnis: Filegr��e
            -1, falls Datei nicht vorhanden }
Var
 FS: TFileStream;
begin
  try
    FS:=TFileStream.Create (Dateiname, fmOpenRead OR fmShareDenyWrite);
    try
      Result:=FS.Size;
    finally
      FS.Free;
    end;
  except
    Result:=-1;
  end;
end;

{-----------------------------------------------------------}
Function Gleich (var Item1, Item2; Anzahl: integer): boolean;
{-----------------------------------------------------------}
{ vergleicht Item1 und Item2 auf Byte-Ebene;
  Ergebnis: true, wenn Item1 = Item2 }
Type
  TBytes = array [0..{Maxint}32767] of Byte;   { f�r 32-Bit Umgebung angepa�t }

Var
  n: integer;

begin
  Result:=true;
  for n:=0 to Anzahl-1 do begin
    if TBytes (Item1)[n] <> TBytes (Item2)[n] then begin
      Result:=false;
      Break;
    end;
  end;
end;

{------------------------------------------------------}
function StringFilter_Sonderzeichen (S: string): string;
{------------------------------------------------------}
{ Filter alle Sonderzeichen NUL..US aus S }
var
  SBuf: string;
  i: integer;

begin
  SBuf:='';
  for i:=1 to length (S) do
    if (Ord (S [i]) >= 32) AND (Ord (S [i]) <= 127) then
      SBuf:=SBuf + S [i];
  Result:=SBuf;
end;

{--------------------------------------------------------------------------------}
Function MRGParameter_geaendert (Antwort: string;
                                 var AltStr: string; var NeuStr: string): boolean;
{--------------------------------------------------------------------------------}
{ pr�ft anhand des MRG-Antwortstrings, ob der Parameter ge�ndert wurde;
  gibt alten und neuen Wert zur�ck
  �bergabe: Antwort auf Befehl
            NeuStr (neu parametrierter Wert) -> f�r String-L�ngenbestimmung !
  R�ckgabe: AltStr, NeuStr aus Antwort
  Ergebnis: true, wenn ge�ndert }
var
  S, S1, S2: string;
  laengealt, laengeneu: integer;

begin
  Antwort:=ExtractString (Antwort, #2, #3, 0);  { nur der Teil zwischen STX und ETX }
  if length (Antwort) > 1 then begin
    S:=StringFilter_Sonderzeichen (Antwort);  { ohne Sonderzeichen damit ger�teunabh�ngig ! }
    S:=Copy (S, 5, length (S));                                                 { ohne Cnnn }
    { Pr�fung, ob S Altwert = Neuwert enth�lt: }
    laengeneu:=length(S) DIV 2;
    laengealt:=length (S) - laengeneu;
    S1:=Copy (S, 1, laengealt);
    S2:=Copy (S, laengealt + 1, length (S));
    if S1 = S2 then begin
      AltStr:=S1;
      NeuStr:=S2;
      Result:=false;
    end
    else begin
      laengeneu:=length(NeuStr);
      laengealt:=length(S)-laengeneu;
      AltStr:=Copy (S, 1, laengealt);
      NeuStr:=Copy (S, laengealt + 1, length (S));
      Result:=AltStr <> NeuStr;
    end;
  end
  else begin
    AltStr:='';
    NeuStr:='';
    Result:=false;
  end;
end;

{-------------------------------------------------------------------}
function DSfGDfueNTYMasken_gleich (Maske1, Maske2:  string): boolean;
{-------------------------------------------------------------------}
{ pr�fen, ob beide Masken die gleichen Zeichen (Nachrichtentypen-K�rzel) enthalten;
  Es kommt dabei nicht auf die Reihenfolge der Zeichen an !
  Ergebnis: true, wenn beide Masken inhaltlich gleich }
var
  i: integer;
begin
  Result:=false;
  if length (Maske1) <> length (Maske2) then exit;
  for i:=1 to length (Maske1) do
    if Pos (Maske1 [i], Maske2) = 0 then exit;
  Result:=true;
end;

{---------------------------------------------------------------------------------}
function DSfGDfueNTYMaske_geaendert (Busadresse: string; WertNeuSoll: string;
                                     Antwort: string; var NeuStr: string): boolean;
{---------------------------------------------------------------------------------}
{ pr�ft, ob eine NTY-Maske der DSfG-DF� ge�ndert wurde und gibt den alten und neuen
  NTY-Masken-Wert zur�ck;
  �bergabe: Busadresse, f�r welche die NTY-Maske ge�ndert werden sollte
            Soll-Wert der neuen NTY-Maske
            Antwort auf Befehl
  R�ckgabe: tats�chliche, neue NTY-Maske f�r Busadresse aus Antwort
  Ergebnis: true, wenn ge�ndert }
var
  S: string;
  TeilStr: string;
  USCount: integer;
  gefunden: boolean;
  NTY: string;
  DNO: string;

begin
  Result:=false;
  NeuStr:='';
  S:=ExtractString (Antwort, #2, #3, 0);  { nur der Teil zwischen STX und ETX }
  if length (S) > 1 then begin
    USCount:=0;
    TeilStr:=ExtractString (S, 'A', #31, 0);
    gefunden:=false;
    while length (TeilStr) > 0 do begin
      NTY:=ExtractString (TeilStr, #0, '/', 0);   { Teil-String bis zum Slash }
      DNO:=ExtractString (TeilStr, '/', #0, 0);   { Teil-String ab dem Slash }
      if DNO = Busadresse then begin
        gefunden:=true;
        Break;
      end;
      TeilStr:=ExtractString (S, #31, #31, USCount);
      inc (USCount);
    end;
    if gefunden then begin
      NeuStr:=NTY;
      Result:=DSfGDfueNTYMasken_gleich (WertNeuSoll, NeuStr);
    end;
  end;
end;

{-------------------------------------------------------------------------------------}
function DSfGDfueParameter_geaendert (WertAltPos: integer; Antwort: string;
                                      var AltStr: string; var NeuStr: string): boolean;
{-------------------------------------------------------------------------------------}
{ pr�ft, ob ein DSfG-DF�-Parameter (z.B. genormte DSfG-Parameter wie Kennung (K-Befehl),
  Passwort (I-Befehl) etc. oder auch Wieser-Parameter (YWC-Befehl)) ge�ndert wurde und
  gibt den alten und neuen Parameter-Wert zur�ck;
  �bergabe: Position im Antwort-String, an welcher der Alt-Wert beginnt
            Antwort auf Befehl
  R�ckgabe: AltStr, NeuStr aus Antwort
  Ergebnis: true, wenn ge�ndert }
var
  S: string;

begin
  S:=Copy (Antwort, WertAltPos, length (Antwort)-WertAltPos);     { rauskopieren: Alt-Wert US NeuWert }
  if length (S) > 1 then begin
    AltStr:=ExtractString (S, #0, #31, 0);          { nur der Teil bis zum US }
    NeuStr:=ExtractString (S, #31, #0, 0);          { nur der Teil ab dem US }
    Result:=AltStr <> NeuStr;
  end
  else begin
    AltStr:='';
    NeuStr:='';
    Result:=false;
  end;
end;

{-----------------------------------------------}
function DS100_FFxPos (FFxSatz: string): integer;
{-----------------------------------------------}
{ ermittelt die Position eines FFx-Steuercodes im FFxSatz (Verbrauchs-Rohdaten
  aus Elster DS-100);
  Integer statt Byte; 04.10.2011, WW }
var
  W, AnzWorte: integer;
  S: string;
  P: integer;

begin
  Result:=0;
  AnzWorte:=length (FFxSatz) DIV 3;
  for W:=1 to AnzWorte do begin
    S:=Copy (FFxSatz, (W-1)*3+1, 3); { Unterteilung in "Worte" zu je 3 Zeichen }
    P:=Pos('FF',S);
    if P = 1 then begin                                { P mu� immer 1 sein }
      Result:=(W-1)*3+P;
      exit;
    end;
  end;
end;

{----------------------------------------------------------------}
procedure DS100_MWFiltern_FFx (Daten: string; var MWDaten: string;
                               AnzStellen_FFxParam: byte);
{----------------------------------------------------------------}
{ liefert den in "Daten" enthaltenen Messwert-Datenteil eines FFx-Satzes
  (Verbrauchs-Rohdaten aus Elster DS-100);
  �bergabe: Daten
            Anzahl der zum FFx-Satz geh�renden Parameter-Zeichen
  R�ckgabe: Messwert-Datenteil }
begin
  MWDaten:=Copy (Daten, 1, length (Daten)-AnzStellen_FFxParam);   { reine Me�werte }
end;

{------------------------------------------------------------------------}
procedure DS100_MWFiltern_DefFStCode (Daten: string; var MWDaten: string);
{------------------------------------------------------------------------}
{ liefert den in "Daten" enthaltenen Messwert-Datenteil eines FF8-Satzes
  (definierbarer Steuercode in den Verbrauchs-Rohdaten aus Elster DS-100);
  �bergabe: Daten
  R�ckgabe: Messwert-Datenteil }
var
  L, code: integer;
  DatenOffset: byte;  { Anzahl der zum FF8-Satz geh�renden Parameter-Zeichen }

begin
  L:=length (Daten);
  { Anzahl der Parameter-Zeichen: }
  Val ('$'+ Copy (Daten, L-2, 3), DatenOffset, code);
  DatenOffset:=DatenOffset * 3;
  DS100_MWFiltern_FFx (Daten, MWDaten, DatenOffset);                { reine Me�werte }
end;

{------------------------------------------------------------------}
procedure Get_DS100_FFADatumZeit (Daten: string; var Jahr: SmallInt;
  var Monat: SmallInt; var Tag: SmallInt; var Stunde: SmallInt;
  var Minute: SmallInt);
{------------------------------------------------------------------}
{ liefert Zeitstempel eines FFA-Satzes (Verbrauchs-Rohdaten aus Elster DS-100);
  �bergabe:	FFA-Daten
  R�ckgabe:	Jahr, Monat, Tag, Stunde, Minute }
var
  L, code: integer;

begin
	L:=length (Daten);
  Val (Copy (Daten, L-1, 2), Jahr, code);
  Jahr:=Jahr2to4stellig (Jahr);
  Val (Copy (Daten, L-4, 2), Monat, code);
  Val (Copy (Daten, L-7, 2), Tag, code);
  Val (Copy (Daten, L-10, 2), Stunde, code);
  Val (Copy (Daten, L-13, 2), Minute, code);
end;

{-----------------------------------------------------------------}
procedure Get_DS100_FFBDatumZeit (Daten: string; var Tag: SmallInt;
  var Stunde: SmallInt; var Minute: SmallInt);
{-----------------------------------------------------------------}
{ liefert Zeitstempel eines FFB-Satzes (Verbrauchs-Rohdaten aus Elster DS-100);
  �bergabe:	FFB-Daten
  R�ckgabe:	Tag, Stunde, Minute }
var
  L, code: integer;

begin
	L:=length (Daten);
  Val (Copy (Daten, L-1, 2), Tag, code);
  Val (Copy (Daten, L-4, 2), Stunde, code);
  Val (Copy (Daten, L-7, 2), Minute, code);
end;

{--------------------------------------------------------------------------------------}
procedure Get_DS100_FFCDatumZeit (Daten: string; var Datum: daterec; var Zeit: timerec);
{--------------------------------------------------------------------------------------}
{ liefert Zeitstempel eines FFC-Satzes (Verbrauchs-Rohdaten aus Elster DS-100);
  �bergabe: FFC-Daten
  R�ckgabe: Datum, Zeit }
var
  L, code: integer;
begin
  L:=length (Daten);
  with Datum do begin
    Val (Copy (Daten, L-1, 2), year, code);
    year:=Jahr2to4stellig (year);
    Val (Copy (Daten, L-4, 2), month, code);
    Val (Copy (Daten, L-7, 2), day, code);
  end;
  with Zeit do begin
    Val (Copy (Daten, L-10, 2), hour, code);
    Val (Copy (Daten, L-13, 2), min, code);
    Val (Copy (Daten, L-16, 2), sec, code);
    hsec:=0;
  end;
end;

{----------------------------------}
function BCDToInt (BCD: byte): byte;
{----------------------------------}
{ wandelt einen BCD-codierten Hex-Wert in einen dezimalen Wert,
  z.B. $32 -> 20 }
var
  HexStr: string;
begin
  HexStr:=IntToHex (BCD, 2);
  try
    Result:=StrToInt (HexStr);
  except
    Result:=0;  { bei unerlaubten BCD-Werten (mit Buchstaben A..F) }
  end;
end;

{-----------------------------------}
function IntToBCD (iInt: byte): byte;
{-----------------------------------}
{ wandelt einen dezimalen Wert in einen BCD-codierten Hex-Wert,
  z.B. 20 -> $32 }
var
  IntStr: string;
begin
  if iInt <= 99 then begin
    IntStr:=IntToStr (iInt);
    Result:=StrToInt ('$' + IntStr);
  end else
    Result:=0;  { bei unerlaubten dezimalen Werten (> 99) }
end;

{------------------------------------------}
function Int64AsSingle (i64: Int64): single;
{------------------------------------------}
{ interpretiert Integer als Single;
  �bergabe: Integer-Wert
  Ergebnis: Single-Wert }
var
  Pi64: PInt64;
  P: pointer;

begin
  New (Pi64);
  try
    Pi64^:=i64;
    P:=Pi64;
    Result:=single(P^);
  finally
    Dispose (Pi64);
  end;
end;

{---------------------------------------------------------------------}
function Basis64CodeStrToInt (sCode: string; var Wert: Int64): boolean;
{---------------------------------------------------------------------}
{ Decodiert Basis-64-codierten String in Ganzzahl;
  �bergabe: codierter String
  R�ckgabe: Ganzzahl-Wert
  Ergebnis: true, wenn S g�ltig ist }
var
  S: string;
  Buf: Int64;
  i: integer;
  j: integer;

begin
  S:=F_LeftTrunc (sCode, ' ');  // f�hrende Space wegschneiden (F�llzeichen im Veribox-Format)

  Wert:=0;
  for i:=1 to length (S) do begin
    if (S[i] >= '0') AND (S[i] <= 'o') then begin  { g�ltiges Zeichen }
      Buf:=Ord (S[i]) - 48;
      for j:=1 to (i - 1) do
        Buf:=Buf * 64;
      Wert:=Wert + Buf;
    end
    else begin     { ung�ltiges Zeichen enthalten }
      Wert:=0;
      Result:=false;
      exit;
    end;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------------------}
function IntToBasis64CodeStr (Wert: cardinal; Stellen: integer; var sCode: string): integer;
{------------------------------------------------------------------------------------------}
{ Codiert Ganzzahl in Basis-64-codierten String mit �bergebenen Stellen;
  -> thanks to Mike Ullmann for c-code !
  �bergabe: Ganzzahl-Wert
            Anzahl der Stellen f�r R�ckgabe-String (0 = es werden so wenig
              Stellen wie n�tig gewandelt (ohne f�hrende 0en!).
  R�ckgabe: codierter String
  Ergebnis: gewandelte Stellen }
const
  MaxLen = 6;
  z64e: array [0..MaxLen] of cardinal = (1,64,4096,262144,16777216,1073741824,$ffffffff);

var
  i: integer;
  z1, z2: cardinal;

begin
	z1:=Wert;
	if Stellen > 0 then begin // mit L�ngenangabe
		i:=Stellen;
    if i > (MaxLen + 1) then
      i:=MaxLen + 1;
  end
	else if Wert > 0 then begin  // ohne L�ngenangabe und Wert > 0?
		i:=0;
		while (z1 >= z64e[i])	do  // n�tige Stellenzahl ermitteln
			inc (i);
	end
	else begin  // Wert = 0!
		sCode:='0';
		Result:=1;
    exit;
	end;

  sCode:='';
	Result:=i;
	while i > 0 do begin
		dec (i);
		z2:=z64e[i];
    sCode:=Chr ((z1 DIV z2) + 48) + sCode;
		z1:=z1 MOD z2;
	end;
end;

{----------------------------------------------------------------------------}
function PalmCreatorIdToInt (sCreatorId: string; var Wert: cardinal): boolean;
{----------------------------------------------------------------------------}
{ wandelt Creator-ID einer Palm-Applikation in Zahlenwert um;
  �bergabe: als max. vier-stelliger String �bergebene Creator-ID (z.B. 'Wabc')
  R�ckgabe: Zahlenwert
  Ergebnis: true, wenn sCreatorId g�ltig ist }
var
  S: string;
  i, Len: integer;

begin
  Wert:=0;
  Len:=length (sCreatorId);
  if (Len > 0) AND (Len <= 4) then begin
    S:='$';
    for i:=1 to Len do
      S:=S + IntToHex (Ord (sCreatorId [i]), 2);
    try
      Wert:=StrToInt (S);
      Result:=true;
    except
      Result:=false;
    end;
  end else
    Result:=false;
end;

{--------------------------------}
function GetXOR (S: string): byte;
{--------------------------------}
{ Berechnet XOR-Byte �ber alle Zeichen eines Strings;
  �bergabe: String
  Ergebnis: XOR-Byte }
var
  i: integer;

begin
  Result:=0;
  for i:=1 to length (S) do
    Result:=Result XOR Ord (S [i]);
end;

// 29.09.2021, WW
{-------------------------------------------------------}
function BufToString (var Buf; iBufLen: integer): string;
{-------------------------------------------------------}
{ Kopiert eine bestimmte Anzahl Bytes von einem Puffer in einen String;
  �bergaben: Byte-Puffer
             Anzahl der zu kopierenden Bytes
  Ergebnis: String mit kopierten Bytes }
begin
  if iBufLen > 0 then begin
    SetLength (Result, iBufLen);
    System.Move (Buf, Result [1], iBufLen);
  end else
    Result:='';
end;


{----------------------------------------------------------------------}
function CreateTempRohFile (Path: TFileName; Prefix: string): TFileName;
{----------------------------------------------------------------------}
{ Datei mit Zufallsnamen erzeugen;
  �bergabe: Pfad, in dem die Datei erzeugt werden soll
            Prefix, der im Dateinamen enthalten sein soll
  Ergebnis: vollst�ndiger Name der erzeugten Datei (Leer, wenn nicht erfolgreich) }
var
  TempFileName: array [0..255] of char;
  iNumber: cardinal;

begin
  iNumber:=GetTempFileName (pchar (Path), pchar (Prefix), 0, TempFileName);
  if iNumber > 0 then  // Erzeugen erfolgreich; 25.07.2018, WW
    Result:=string (TempFileName)
  else
    Result:='';
end;

{-------------------------------------------------------------------------------------------}
function WriteRohFile (FileName: TFileName; S: string; bOverwrite: boolean = false): boolean;
{-------------------------------------------------------------------------------------------}
{ String in vorhandenes Rohfile schreiben;
  �bergabe: Dateiname
            String, der in Datei geschrieben werden soll
  Ergebnis: true, wenn in Datei geschrieben werden konnte }
var
  FS: TFileStream;
begin
  // ge�ndert 20.05.2003, WW:
  // Bisher verwendetes Append funktioniert nicht zuverl�ssig ! Es �berschreibt
  // das Zeichen STRG+Z (ASCII 26) am Dateiende !
  if bOverwrite then
    Result:=ClearRohFile (FileName)
  else
    Result:=true;
  if not Result then exit;
  if length (S) = 0 then exit;
  try
    FS:=TFileStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite);
    try
      FS.Seek (0, soFromEnd);
      FS.Write (S [1], length (S));
    finally
      FS.Free;
    end;
  except
    Result:=false;
  end;
end;

{--------------------------------------------------------------------------------}
function WriteRohFileBuf (FileName: TFileName; var Buf; BufLen: integer): boolean;
{--------------------------------------------------------------------------------}
{ �bergebenen Buffer in vorhandenes Rohfile schreiben;
  �bergabe: Dateiname
            Buffer-Inhalt, der in Datei geschrieben werden soll
            Buffer-L�nge
  Ergebnis: true, wenn in Datei geschrieben werden konnte }
var
  FS: TFileStream;
begin
  // ge�ndert 20.05.2003, WW:
  // Bisher verwendetes Append funktioniert nicht zuverl�ssig ! Es �berschreibt
  // das Zeichen STRG+Z (ASCII 26) am Dateiende !
  Result:=true;
  if BufLen <= 0 then exit;
  try
    FS:=TFileStream.Create (FileName, fmOpenReadWrite OR fmShareDenyWrite);
    try
      FS.Seek (0, soFromEnd);
      FS.Write (Buf, BufLen);
    finally
      FS.Free;
    end;
  except
    Result:=false;
  end;
end;

{---------------------------------------------------}
function ClearRohFile (FileName: TFileName): boolean;
{---------------------------------------------------}
{ setzte die Gr��e der Rohdatei auf 0;
  �bergabe: Dateiname
            String, der in Datei geschrieben werden soll
  Ergebnis: true, wenn in Datei geschrieben werden konnte }
begin
  Result:=true;
  try
    with TFileStream.Create (FileName, fmCreate) do Free;
  except
    Result:=false;
  end;
end;


{---------------------------------------------------------------}
function GetMittelwert_arith (mw_alt: double; count_alt: integer;
  wert_neu: double): double;
{---------------------------------------------------------------}
{ Arithmetischen Mittelwert berechnen;
  �bergabe: Bisheriger Mittelwert
            Anzahl der Einzelwerte, aus denen der bisherige Mittelwert gebildet wurde
            Neuer Einzelwert
  Ergebnis: Neu berechneter Mittelwert }
begin
	Result:=((mw_alt * count_alt) + wert_neu) / (count_alt + 1);
end;

{-------------------------------------------------------------}
function GetMittelwert_rad (mw_alt: double; count_alt: integer;
  wert_neu: double): double;
{-------------------------------------------------------------}
{ Radizierenden Mittelwert berechnen (Ruhrgas-Gasdruckmessung);
  �bergabe: Bisheriger Mittelwert
            Anzahl der Einzelwerte, aus denen der bisherige Mittelwert gebildet wurde
            Neuer Einzelwert
  Ergebnis: Neu berechneter Mittelwert }
var
	f: double;
begin
	f:=((Sqrt (mw_alt) * count_alt) + Sqrt (wert_neu)) / (count_alt + 1);
	Result:=Sqr (f);
end;

{---------------------------------------------------------------}
function GetMittelwert_quadr (mw_alt: double; count_alt: integer;
  wert_neu: double): double;
{---------------------------------------------------------------}
{ Quadratischen Mittelwert berechnen;
  �bergabe: Bisheriger Mittelwert
            Anzahl der Einzelwerte, aus denen der bisherige Mittelwert gebildet wurde
            Neuer Einzelwert
  Ergebnis: Neu berechneter Mittelwert }
var
	f: double;
begin
	f:=((mw_alt * mw_alt * count_alt) + (wert_neu * wert_neu)) / (count_alt + 1);
	Result:=Sqrt (f);
end;

{---------------------------------------------------}
function WFileSize (const FileName: string): integer;
{---------------------------------------------------}
{ Liefert Dateigr��e ohne Datei zu �ffnen;
  �bergabe: Dateiname
  Ergebnis: Dateigr��e (-1, wenn Datei nicht gefunden) }
var
  SR: TSearchRec;
  i: integer;

begin
  i:=FindFirst (FileName, faAnyFile, SR);
  if i = 0 then
    Result:=SR.Size
  else
    Result:=-1;
  FindClose (SR);
end;

{----------------------------------------------------}
function WInAddrToStr (const InAddr: TInAddr): string;
{----------------------------------------------------}
{ Wandelt IPv4-Adresse von TInAddr-Struktur in String }
begin
  with InAddr.S_un_b do
    Result := IntToStr(Byte(s_b1)) + '.' + IntToStr(Byte(s_b2)) + '.' +
              IntToStr(Byte(s_b3)) + '.' + IntToStr(Byte(s_b4));
end;

{------------------------------------------------------------}
function WIsValidIPv4Address (const sIpAddr: string): boolean;
{------------------------------------------------------------}
{ Pr�ft, ob �bergebener String eine g�ltige IPv4-Adresse darstellt;
  Ergebnis: true, wenn g�ltig }
var
  iIpAdr: integer;

begin
   iIpAdr := inet_addr(PChar(sIpAddr));
   Result := iIpAdr <> INADDR_NONE;
end;

{------------------------------------------------------------}
function TicksElapsed (var LastTickCount: cardinal): cardinal;
{------------------------------------------------------------}
{ Misst die Ticks seit dem letzten TickCount;
  R�ckgabe: Aktueller TickCount
  Ergebnis: Gemessene Ticks (ms) }
var
  TickCount: cardinal;
  ms: cardinal;

begin
  TickCount:=GetTickCount;
  // Verstrichene Ticks (ms):
  if TickCount >= LastTickCount then  // GetTickCount beginnt zyklisch wieder bei 0 !
    ms:=TickCount - LastTickCount
  else  // �berlauf GetTickCount
    ms:=(High (LastTickCount) - LastTickCount + 1) + TickCount;

  LastTickCount:=TickCount;
  Result:=ms;
end;

End.
