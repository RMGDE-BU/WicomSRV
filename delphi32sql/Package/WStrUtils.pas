{-------------------------------------------------------------------}
{ WStrUtils                                                         }
{ Utilities für String-Operationen                                  }
{ 15.11.1996 DA, Copyright Karl Wieser GmbH                         }
{ 02.06.1998 GD, Ergänzt um 'F_LeftRigthTrunc' ('Trim' tut's auch)  }
{ 02.06.2001 GD, 'IntToBinOrOtherBase'                              }
{ 15.03.2002 GD, 'WCharToOem', WOemToChar'                          }
{ 10.10.2019 WW  WFormatString                                      }
{ 25.02.2020 WW  WIsIPv4Address                                     }
{ 27.07.2023 WW  HexToInt64Rec                                      }
{ 17.10.2023 WW  WElsterHexToAscii, WAsciiToElsterHex               }
{-------------------------------------------------------------------}
unit WStrUtils;

interface

uses
  Windows, SysUtils;

function IntToStrWithPoints (Value: LongInt): string;
function IntToBinOrOtherBase (Value: LongInt; iDigits: integer;
                              iBase: byte = 2): string;
function DoubleToStrWithKomma (Value: Double; NachKommaStellen: Integer): string;
procedure WFormatString (var S: string; sFormat: string);
Function F_RightTrunc (s: String; c: Char): String;
Function F_LeftTrunc (s: String; c: Char): String;
Function F_LeftRightTrunc (s: String; c: Char): String;
Function F_RightPad (s: String; padChar: Char; padlen: Byte): String;
Function F_LeftPad (s: String; padChar: Char; padlen: Byte): String;
Function FilterString (Dest, Source, Left, Right, Trunc: PChar; Before: Word): PChar;
Function ExtractString (S: string; Left, Right: char; Before: integer;
                        Feldbegrenzer: char = #0): string;
Function F_Zerlegen(var Wert: string; Trennzeichen: char): string;
Function F_TruncNumStr(s: string): string;
Function StrToComp(x: string): comp;
function StrFilter(S: string; c: char): string;
Procedure StrSubst(var S: string; oldChar: char; newChar: char);
Function F_RightChars (s: string; c: char): integer;
Function F_TotalChars (s: string; c: char): integer;
function WCharToOem (sANSIString: string): string;
function WOemToChar (sOEMString: string): string;
function isIntString (S: string): boolean;
function WTrimIPAddress (sIPAddress: string): string;
function WExpandIPAddress (sIPAddress: string): string;
function WIsIPv4Address (sIPAddress: string): boolean;
function WAsciiToISO646_De (sAscii: string): string;
function WISO646_DeToAscii (sISO646: string): string;
function HexToInt64Rec (sHex: string; var Rec: Int64Rec): boolean;
function WElsterHexToAscii (var S: string): boolean;
function WAsciiToElsterHex (var S: string): boolean;

implementation

{---------------------------------------------------}
function IntToStrWithPoints (Value: LongInt): string;
{---------------------------------------------------}
var
  StrValue: string;
  i, j: Integer;
begin
  Result := '';
  StrValue := IntToStr (Value);
  j := 0;
  for i := Length (StrValue) downto 1 do
  begin
    Result := StrValue [i] + Result;
    if (StrValue [i] >= '0') or (StrValue [i] <= '9') then
    begin
      Inc (j);
      if (i > 1) and ((j mod 3) = 0) then
        Result := '.' + Result;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function DoubleToStrWithKomma (Value: Double; NachKommaStellen: Integer): string;
{-------------------------------------------------------------------------------}
begin
  Result := Format ('%.' + IntToStr (NachKommaStellen) + 'f', [Value]);
end;

{------------------------------------------------------------------------------}
procedure WFormatString (var S: string; sFormat: string);  // 10.10.2019, WW
{------------------------------------------------------------------------------}
{ Formatiert String (Ganzzahl oder Gleitkommazahl) mit übergebenem Format (Delphi
  Format-Strings) }
var
  i64: Int64;
  d: double;

begin
  if length (sFormat) > 0 then begin
    // Format-Strings für Gleitkommazahlen
    if (Pos('e', sFormat) > 0) OR
       (Pos('f', sFormat) > 0) OR
       (Pos('g', sFormat) > 0) OR
       (Pos('n', sFormat) > 0) OR
       (Pos('m', sFormat) > 0) then begin
      try
        d:=StrToFloat(S);
        S:=Format(sFormat,[d]);
      except
        //  Formatierung nicht möglich: String beibehalten
      end;
    end
    // Format-Strings für Ganzzahlen
    else if (Pos('d', sFormat) > 0) OR
            (Pos('u', sFormat) > 0) OR
            (Pos('x', sFormat) > 0) then begin
      try
        i64:=StrToInt64(S);
        S:=Format(sFormat,[i64]);
      except
        //  Formatierung nicht möglich: String beibehalten
      end;
    end;
  end;
end;

{-------------------------------------------------------------------}
Function F_RightPad (s: String; padChar: Char; padlen: Byte): String;
{-------------------------------------------------------------------}
Var
  hs : String;
Begin
  hs := s;
  While (Length(hs) < padlen) Do
    hs := hs + padChar;
  Result := hs;
End;

{-------------------------------------------------------------------}
Function F_LeftPad (s: String; padChar: Char; padlen: Byte): String;
{-------------------------------------------------------------------}
Var
  hs : String;
Begin
  hs := s;
  While (Length(hs) < padlen) Do
    hs := padChar + hs;
  Result := hs;
End;

{-------------------------------------------------}
Function F_RightTrunc (s: String; c: Char): String;
{-------------------------------------------------}
Var
  i : Integer;
Begin
  i := Length(s);
  While (i > 0) AND (c = s [i]) Do
    Dec (i);
  Result := Copy (s, 1, i);
End;

{-------------------------------------------------}
Function F_LeftTrunc (s: String; c: Char): String;
{-------------------------------------------------}
Var
  i,l : Integer;
Begin
  l := Length(s);
  i := 1;
  While (i <= l) AND (c = s [i]) Do
    inc (i);
  Result := Copy (s, i, l);
End;

{-------------------------------------------------}
Function F_LeftRightTrunc (s: String; c: Char): String;
{-------------------------------------------------}
{ Kombination von Left- und RightTrunc }
Var
  hilf : string;
Begin
  hilf:= F_LeftTrunc(s, c);
  result:= F_RightTrunc(hilf, c);
End;

{ FilterString für nullterminierte Strings: Filtert Teilstring aus String
  Parameter
    Dest : enthält gefilterten String
    Source : String, welcher gefiltert werden soll
    Left : Zeiger auf linkes Begrenzungszeichen (zählt nicht zu Dest)
           nil -> ab 1. Zeichen von Source
    Right : Zeiger auf rechtes Begrenzungszeichen (zählt nicht zu Dest)
            nil -> bis letztes Zeichen von Source
    Trunc : Zeiger auf Zeichen, welches links und rechts von Dest abgeschnitten
            wird
    Before : Anzahl der linken Begrenzungszeichen vor Left }
{-----------------------------------------------------------------------------------}
Function FilterString (Dest, Source, Left, Right, Trunc: PChar; Before: Word): PChar;
{-----------------------------------------------------------------------------------}
Var
  L : PChar;
  R : PChar;
  i : Word;
Begin
  FilterString := Nil;
  If (Dest <> Nil) AND (Source <> Nil) Then
  Begin
    Dest[0]:=#0;
    L := Source;
    If Left <> Nil Then
    Begin
      For i := 0 To Before Do
      Begin
        L := StrScan (L, Left [0]);
        If L = Nil Then
          Exit;
        Inc (L);
      End;
    End;
    If Trunc <> Nil Then
    Begin
      While (L [0] = Trunc [0]) Do
        Inc (L);
    End;
    StrCopy (Dest, L);
//    R := @Dest;
    If Right <> Nil Then
    Begin
      R := StrScan (Dest, Right [0]);
      If R <> Nil Then
        R^ := #0;
    End;
    If Trunc <> Nil Then
    Begin
      R := StrEnd (Dest) - 1;
      While (R > @Dest) AND (R^ = Trunc^) Do
      Begin
        R^ := #0;
        Dec (R);
      End;
    End;
    If Strlen (Dest) > 0 Then
    Begin
      FilterString := Dest;
    End;
  End;
End;

{--------------------------------------------------------------------}
Function ExtractString (S: string; Left, Right: char; Before: integer;
                        Feldbegrenzer: char = #0): string;
{--------------------------------------------------------------------}
{ Extrahiert Teilstring aus ANSI-String S
  Parameter
    S: String, aus dem extrahiert werden soll
    Left: linkes Begrenzungszeichen (zählt nicht zum Ergebnis)
          NUL -> ab dem 1. Zeichen von S
    Right: rechtes Begrenzungszeichen (zählt nicht zum Ergebnis)
           NUL -> bis zum letzten Zeichen von S
    Before: Anzahl der linken Begrenzungszeichen vor Left
    Feldbegrenzer: zwischen Feldbegrenzer stehende Left/Right-Chars werden ignoriert
                   NUL -> Feldbegrenzer-Funktion ausgeschaltet (alle Left/Right-Chars
                          werden gezählt)
  Ergebnis
    extrahierter String }
var
  L: integer;
  LeftPos, RightPos: integer;
  i: integer;
  doCount_LeftRight: boolean;

begin
  L:=length(S);
  if L > 0 then begin
    LeftPos:=0;
    if Left <> #0 then begin
      LeftPos:=1;
      for i:=1 to Before + 1 do begin
        doCount_LeftRight:=true;
        while (LeftPos <= L) do begin
          if (Feldbegrenzer <> #0) AND (S[LeftPos] = Feldbegrenzer) then
            doCount_LeftRight:=not doCount_LeftRight;  // zwischen Feldbegrenzer stehende Left-Chars ignorieren

          if (S[LeftPos] <> Left) OR not doCount_LeftRight then
            inc(LeftPos)
          else
            Break;
        end;
        inc (LeftPos);
      end;
      dec(LeftPos);

      if LeftPos >= L then begin
        Result:= '';
        exit;
      end;
    end;

    if Right <> #0 then begin
      RightPos:=LeftPos + 1;

      doCount_LeftRight:=true;
      while (RightPos <= L) do begin
        if (Feldbegrenzer <> #0) AND (S[RightPos] = Feldbegrenzer) then
          doCount_LeftRight:=not doCount_LeftRight;  // zwischen Feldbegrenzer stehende Left-Chars ignorieren

        if (S[RightPos] <> Right) OR not doCount_LeftRight then
          inc(RightPos)
        else
          Break;
      end;
    end else
      RightPos:=L + 1;

    Result:=Copy (S, LeftPos + 1, RightPos - LeftPos - 1);
  end else
    Result:='';
end;

{----------------------------------------------------------------}
Function F_Zerlegen(var Wert: string; Trennzeichen: char): string;
{----------------------------------------------------------------}
{ Ergebnis: Teilstring bis Trennzeichen
  Rückgabe: Wert = Teilstring ab Trennzeichen }
var
  position: integer;
  Buf: string;
begin
  Buf:='';
  position:=1;
  while position <= length(wert) do begin
    if Wert[position] <> Trennzeichen then
      Buf:=Buf + Wert[position]
    else
      Break;
    inc(position);
  end;
  Wert:=copy(Wert,position+1,length(Wert));   { Trennzeichen wird überlesen }
  Result:=Buf;
end;

{----------------------------------------}
Function F_TruncNumStr(s: string): string;
{----------------------------------------}
{ entfernt führende Nullen in s (Integer- oder Real-String) }

var
  c: char;
begin
  if s <> '' then begin
    c:=' ';
    if (s[1] = '+') OR (s[1] = '-') then begin
      c:=s[1];
      s:=copy(s,2,length(s)-1);
    end;
    s:=F_LeftTrunc(s,'0');
    if s = '' then                                                           { s war 0...0 }
      s:='0'
    else begin
      if (s[1] = ',') OR (s[1] = '.') then s:='0' + s;         { wenn s mit Dezimalzeichen }
      if c <> ' ' then s:=c + s;
    end;
  end;
  Result:=s;
end;

{-------------------------------------------------------------------------}
function StrToComp(x: string): comp;
var
  y: comp;
  code: integer;

begin
  val(x, y, code);
  if code = 0 then
    Result := y
  else
    Result := 0;
end;


{---------------------------------------------}
function StrFilter(S: string; c: char): string;
{---------------------------------------------}
var
  i: integer;
  ErgStr: string;

begin
  ErgStr:='';
  for i:=1 to length(S) do
    if S[i] <> c then ErgStr:=ErgStr + S[i];
  StrFilter:=ErgStr;
end;


{--------------------------------------------------------------}
Procedure StrSubst(var S: string; oldChar: char; newChar: char);
{--------------------------------------------------------------}
var
  i, Len: word;
begin
  Len:=Length(S);
  i:=1;
  while i <= Len do begin
    if S[i] = oldChar then S[i]:=newChar;
    inc(i);
  end;
end;

{--------------------------------------------------}
Function F_RightChars (s: string; c: char): integer;
{--------------------------------------------------}
{ liefert die Anzahl der Zeichen c, mit denen s endet }
var
  count: integer;
  i: integer;
begin
  count:=0;
  for i:=Length (s) downto 1 do
    if s[i] = c then
      inc (count)
    else
      Break;
  Result:=count;
end;

{--------------------------------------------------}
Function F_TotalChars (s: string; c: char): integer;
{--------------------------------------------------}
{ liefert die Anzahl der in s enthaltenen Zeichen c }
var
  count: integer;
  i: integer;
begin
  count:=0;
  for i:=1 to Length (s) do
    if s[i] = c then
      inc (count);
  Result:=count;
end;

{ Wandelt einen Integer in einen Binärwert um      }
{ Parameter: Wert, min. Stellenzahl, Basis (max 10)}
{ Rückgabe: Binärwert als string                   }
{--------------------------------------------------}
function IntToBinOrOtherBase (
  Value: LongInt; iDigits: integer; iBase: byte = 2): string;
{--------------------------------------------------}
var
  i : LongInt;
begin
  Result := '';
  i := Value;

  if (iBase < 2) or (iBase > 10) then iBase := 2;

  while (i > 0) do begin
    Result := IntToStr(i mod iBase) + Result;
    i := i div iBase;
  end;

  for i := Length(Result) to iDigits-1 do Result := '0' + Result;
end;

{ Wandelt einen ANSI-String in einen OEM-String um }
{ Parameter: ANSI-String                           }
{ Rückgabe: OEM-String                             }
{--------------------------------------------------}
function WCharToOem (sANSIString: string): string;
{--------------------------------------------------}
var
  pAnsi, pOem : PChar;
begin
  GetMem(pAnsi, Length(sANSIString) + 1);
  GetMem(pOem, Length(sANSIString) + 1);
  try
     StrPCopy(pAnsi, sANSIString);
     CharToOem(pAnsi, pOem);
     Result := string(pOem);
  finally
    FreeMem(pAnsi, Length(sANSIString) + 1);
    FreeMem(pOem, Length(sANSIString) + 1);
  end;
end;

{ Wandelt einen OEM-String in einen ANSI-String um }
{ Parameter: OEM-String                            }
{ Rückgabe: ANSI-String                            }
{--------------------------------------------------}
function WOemToChar (sOEMString: string): string;
{--------------------------------------------------}
var
  pAnsi, pOem : PChar;
begin
  GetMem(pAnsi, Length(sOEMString) + 1);
  GetMem(pOem, Length(sOEMString) + 1);
  try
     StrPCopy(pOem, sOEMString);
     OemToChar(pOem, pAnsi);
     Result := string(pAnsi);
  finally
    FreeMem(pAnsi, Length(sOEMString) + 1);
    FreeMem(pOem, Length(sOEMString) + 1);
  end;
end;

{----------------------------------------}
function isIntString (S: string): boolean;
{----------------------------------------}
{ Ergebnis: true, wenn S einen Integer darstellt }
var
  I, Code: Integer;

begin
  Val (S, I, Code);
  if I = 0 then;
  Result:=Code = 0;
end;

{---------------------------------------------------}
function WTrimIPAddress (sIPAddress: string): string;
{---------------------------------------------------}
{ entfernt führende Nullen in IP-Adresse }
var
  sBuf: string;
  c: char;
  i: integer;

begin
  Result:='';
  sBuf:='';
  for i:=1 to length (sIPAddress) do begin
    c:=sIPAddress [i];
    if c <> '.' then
      sBuf:=sBuf + c;

    if (c = '.') OR (i = length (sIPAddress)) then begin
      if length (sBuf) > 0 then begin
        sBuf:=F_LeftTrunc (sBuf, '0');
        if length (sBuf) = 0 then
          sBuf:='0';
        Result:=Result + sBuf;
      end;

      if (c = '.') then
        Result:=Result + '.';
      sBuf:='';
    end;
  end;  // for i
end;

{-----------------------------------------------------}
function WExpandIPAddress (sIPAddress: string): string;
{-----------------------------------------------------}
{ Füllt führende Nullen in IP-Adresse auf }
var
  sBuf: string;
  S: string;

begin
  Result:='';
  sBuf:=sIPAddress;
  while length (sBuf) > 0 do begin
    S:=F_Zerlegen (sBuf, '.');
    if length (Result) > 0 then
      Result:=Result + '.';
    Result:=Result + F_LeftPad (S, '0', 3);
  end;  { while length (sBuf) > 0 }
end;

{----------------------------------------------------}
function WIsIPv4Address (sIPAddress: string): boolean;
{----------------------------------------------------}
{ true, wenn übergebener String eine IPv4-Adresse darstellt (ohne Berücksichtigung
  der Oktal-Notation (Ziffern 1..7 mit führenden Nullen) }
var
  sBuf: string;
  c: char;
  i: integer;
  iDotCount: integer;
  iInt: integer;

begin
  Result:=false;
  sBuf:='';
  iDotCount:=0;
  for i:=1 to length (sIPAddress) do begin
    c:=sIPAddress [i];
    if c = '.' then
      inc (iDotCount)
    else                               
      sBuf:=sBuf + c;

    if (c = '.') OR (i = length (sIPAddress)) then begin
      iInt:=StrToIntDef (sBuf, -1);
      if (iInt < 0) OR (iInt > 255) then exit;  // jedes Segment im Bereich 0..255 bei IPv4

      sBuf:='';
    end;
  end;  // for i

  if iDotCount <= 3 then  // max. 3 Punkte bei IPv4   
    Result:=true;
end;

{--------------------------------------------------}
function WAsciiToISO646_De (sAscii: string): string;
{--------------------------------------------------}
{ Wandelt einen ASCII-String in die deutsche ISO 646-Variante;
  Übergabe: ASCII-String
  Ergebnis: String nach ISO 646, deutsche Variante }
var
  i: integer;

begin
  Result:=sAscii;
  for i:=1 to length (Result) do begin
    case Result [i] of
      '@': Result [i]:='§';
      '[': Result [i]:='Ä';
      '\': Result [i]:='Ö';
      ']': Result [i]:='Ü';
      '{': Result [i]:='ä';
      '|': Result [i]:='ö';
      '}': Result [i]:='ü';
      '~': Result [i]:='ß';
    end;
  end;
end;

{---------------------------------------------------}
function WISO646_DeToAscii (sISO646: string): string;
{---------------------------------------------------}
{ Wandelt einen ISO 646-String (Variante deutsch) in ASCII-String;
  Übergabe: String nach ISO 646, deutsche Variante
  Ergebnis: ASCII-String }
var
  i: integer;

begin
  Result:=sISO646;
  for i:=1 to length (Result) do begin
    case Result [i] of
      '§': Result [i]:='@';
      'Ä': Result [i]:='[';
      'Ö': Result [i]:='\';
      'Ü': Result [i]:=']';
      'ä': Result [i]:='{';
      'ö': Result [i]:='|';
      'ü': Result [i]:='}';
      'ß': Result [i]:='~';
    end;
  end;
end;

{----------------------------------------------------------------}
function HexToInt64Rec (sHex: string; var Rec: Int64Rec): boolean;
{----------------------------------------------------------------}
{ Wandelt einen Hexadezimal-Wertstring in Int64Rec-Struktur (volle 64-Bit Unter-
  stützung);
  Übergabe: Hex-String
  Rückgabe: Int64Rec
  Ergebnis: true, wenn Wandlung erfolgreich }
var
  sVal, s32: string;
  i32Lo, i32Hi: cardinal;
  iCode: integer;
  iValIndex: integer;

begin
  sVal := sHex;

  // 32-Bit Lo-Wert:
  iValIndex := length (sVal) - 7;
  s32 := Copy (sVal, iValIndex, 8);
  if iValIndex >= 1 then
    System.Delete (sVal, iValIndex, 8)
  else
    System.Delete (sVal, 1, 8);
  Val ('$' + s32, i32Lo, iCode);  // 32-Bit Lo-Wert wandeln

  // 32-Bit Hi-Wert:
  i32Hi := 0;  // Default 32-Bit Hi Wert
  if iCode = 0 then begin
    iValIndex := length (sVal) - 7;
    s32 := Copy (sVal, iValIndex, 8);
    if s32 <> '' then
      Val ('$' + s32, i32Hi, iCode);  // 32-Bit Hi-Wert wandeln
  end;

  if (iCode = 0) then begin  // wenn Wert gültig
    Rec.Lo := i32Lo;
    Rec.Hi := i32Hi;
    Result := true;
  end
  else begin
    Rec.Lo := 0;
    Rec.Hi := 0;
    Result := false;
  end;
end;

{--------------------------------------------------}
function WElsterHexToAscii (var S: string): boolean;
{--------------------------------------------------}
{ Wandelt String mit hexadezimal-kodierten Elster-Ersatzzeichen in ASCII-String;
  Übergabe: String mit ggf. enthaltenen Ersatzzeichen
  Rückgabe: Gewandelter ASCII-String
  Ergebnis: True, wenn String-Wandlung erfolgte }
var
  sBuf: string;

begin
  sBuf:=S;
  // \x2f -> /
  sBuf:=StringReplace (sBuf, '\x2f', '/', [rfReplaceAll]);
  // \xb0 -> °
  sBuf:=StringReplace (sBuf, '\xb0', '°', [rfReplaceAll]);
  // Gewandelt ?
  Result:=Length (S) <> Length (sBuf);
  // Rückgabe
  S:=sBuf;
end;

{--------------------------------------------------}
function WAsciiToElsterHex (var S: string): boolean;
{--------------------------------------------------}
{ Wandelt ASCII-String in String mit hexadezimal-kodierten Elster-Ersatzzeichen;
  Übergabe: ASCII-String
  Rückgabe: Gewandelter String mit ggf. enthaltenen Ersatzzeichen
  Ergebnis: True, wenn Wandlung erfolgte }
var
  sBuf: string;

begin
  sBuf:=S;
  // / -> \x2f
  sBuf:=StringReplace (sBuf, '/', '\x2f', [rfReplaceAll]);
  // ° -> \xb0
  sBuf:=StringReplace (sBuf, '°', '\xb0', [rfReplaceAll]);
  // Gewandelt ?
  Result:=Length (S) <> Length (sBuf);
  // Rückgabe
  S:=sBuf;
end;

end.
