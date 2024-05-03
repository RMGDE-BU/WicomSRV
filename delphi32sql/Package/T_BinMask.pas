{******************************************************************************}
{* Unit: Konvertierung von Bin‰rzeichen in Struktur                           *}
{* 28.08.2008 WW                                                              *}
{* 20.10.2009 WW  Bugfix Bin2Word, Word2Bin, Bin2Integer, Integer2Bin mit     *}
{*                Byte-Order "Little-Byte-Swap"                               *}
{******************************************************************************}
unit T_BinMask;

interface

uses
  Windows, Classes, SysUtils, DateUtils, Math;

resourcestring
  SBigEndian      = 'Big-Endian';
  SLittleEndian   = 'Little-Endian';
  SLittleByteSwap = 'Little-Byte-Swap';

type
  { Byte-Orders }
  TByteOrder = (bo_BigEndian,        // Big-Endian (4321): 'Motorola', z.B. Modbus
                bo_LittleEndian,     // Little-Endian (1234): 'Intel', z.B. Windows
                bo_LittleByteSwap);  // Little-Byte-Swap (2143)

  { Corus-Struktur ALARM }
  TCorusAlarm = record
    Active: byte;
    Memo: byte;
    Value: single;
    Start: TDateTime;
    Ende: TDateTime;
    MStart: TDateTime;
    MEnde: TDateTime;
  end;

function EndianWord (w: word): word;
function EndianString (s: string): string;

function Hex2Bin (sHex: string): string;
function Bin2Hex (sBin: string; bSpace: boolean = false): string;

function Bin2Byte (sBin: string): byte;
function Byte2Bin (iValue: byte): string;
function Bin2Shortint (sBin: string): shortint;
function Shortint2Bin (iValue: shortint): string;
function Bin2Smallint (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): smallint;
function Smallint2Bin (iValue: smallint; ByteOrder: TByteOrder = bo_LittleEndian): string;
function Bin2Word (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): word;
function Word2Bin (wValue: word; ByteOrder: TByteOrder = bo_LittleEndian): string;
function Bin2Integer (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): integer;
function Integer2Bin (iValue: integer; ByteOrder: TByteOrder = bo_LittleEndian): string;
function Bin2Longword (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): cardinal;
function Longword2Bin (cValue: cardinal; ByteOrder: TByteOrder = bo_LittleEndian): string;
function Bin2Int64 (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): Int64;
function Int64_2Bin (iValue: Int64; ByteOrder: TByteOrder = bo_LittleEndian): string;
function Bin2Single (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): single;
function Single2Bin (siValue: single; ByteOrder: TByteOrder = bo_LittleEndian): string;
function Bin2Double (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): double;
function Double2Bin (dValue: double; ByteOrder: TByteOrder = bo_LittleEndian): string;

function Bin2Date_Corus (sBin: string): TDateTime;
function Date2Bin_Corus (dtDatum: TDateTime): string;
function Bin2Float16_1_Corus (sBin: string): double;
function Bin2Float16_2_Corus (sBin: string): double;
function Bin2Index_Corus (sBin: string): double;
function Index2Bin_Corus (dValue: double): string;
function Bin2Alarm_Corus (sBin: string): TCorusAlarm;
function Alarm2Bin_Corus (ca: TCorusAlarm): string;

implementation


{---------------------- Bit-Maskierung: Float/Integer -------------------------}

{----------------------------------------------------}
function SingleAsLongword (siValue: single): cardinal;
{----------------------------------------------------}
{ stellt Single (32 Bit-Float) bit-m‰ﬂig als Longword (32 Bit-Integer) dar }
var
  PSingle: ^single;
  P: pointer;

begin
  New (PSingle);
  try
    PSingle^:=siValue;
    P:=PSingle;
    Result:=cardinal (P^);
  finally
    Dispose (PSingle);
  end;
end;

{---------------------------------------------------}
function LongwordAsSingle (iValue: cardinal): single;
{---------------------------------------------------}
{ stellt Longword (32 Bit-Integer) bit-m‰ﬂig als Single (32 Bit-Float) dar }
var
  PCardinal: ^cardinal;
  P: pointer;

begin
  New (PCardinal);
  try
    PCardinal^:=iValue;
    P:=PCardinal;
    Result:=single (P^);
  finally
    Dispose (PCardinal);
  end;
end;

{---------------------------------------------}
function DoubleAsInt64 (dValue: double): Int64;
{---------------------------------------------}
{ stellt Double (64 Bit-Float) bit-m‰ﬂig als Int64 (64 Bit-Integer) dar }
var
  PDouble: ^double;
  P: pointer;

begin
  New (PDouble);
  try
    PDouble^:=dValue;
    P:=PDouble;
    Result:=Int64 (P^);
  finally
    Dispose (PDouble);
  end;
end;

{---------------------------------------------}
function Int64AsDouble (iValue: Int64): double;
{---------------------------------------------}
{ stellt Int64 (64 Bit-Integer) bit-m‰ﬂig als Double (64 Bit-Float) dar }
var
  PInt64: ^Int64;
  P: pointer;

begin
  New (PInt64);
  try
    PInt64^:=iValue;
    P:=PInt64;
    Result:=double (P^);
  finally
    Dispose (PInt64);
  end;
end;


{-------------------------- Byte-Order-Wandlung -------------------------------}

{----------------------------------}
function EndianWord (w: word): word;
{----------------------------------}
{ Wandelt die Byte-Order eines Word: Little-Endian (12) <-> Big-Endian (21) }
begin
  Result:=Swap (w);
end;

{----------------------------------------------}
function EndianSmallint (i: smallint): smallint;
{----------------------------------------------}
{ Wandelt die Byte-Order eines SmallInt: Little-Endian (12) <-> Big-Endian (21) }
begin
  Result:=Swap (i);
end;

{-------------------------------------------}
function EndianInteger (i: integer): integer;
{-------------------------------------------}
{ Wandelt die Byte-Order eines Integer: Little-Endian (1234) <-> Big-Endian (4321) }
begin
  Result:=Swap (i shr 16) or
          (Swap (i and $FFFF) shl 16);
end;

{----------------------------------------------}
function EndianLongword (c: cardinal): cardinal;
{----------------------------------------------}
{ Wandelt die Byte-Order eines Longword: Little-Endian (1234) <-> Big-Endian (4321) }
begin
  Result:=Swap (c shr 16) or
          (Swap (c and $FFFF) shl 16);
end;

{-------------------------------------}
function EndianInt64 (i: Int64): Int64;
{-------------------------------------}
{ Wandelt die Byte-Order eines Int64: Little-Endian (12345678) <-> Big-Endian (87654321) }
begin
  Result:=(Swap (word (i shr 48))) or
          (Int64 (Swap (word ((i and $FFFF00000000) shr 32))) shl 16) or
          (Int64 (Swap (word ((i and $FFFF0000) shr 16))) shl 32) or
          (Int64 (Swap (word (i and $FFFF))) shl 48);
end;

{-----------------------------------------}
function EndianSingle (si: single): single;
{-----------------------------------------}
{ Wandelt die Byte-Order eines Single: Little-Endian (1234) <-> Big-Endian (4321) }
var
  c: cardinal;

begin
  c:=SingleAsLongword (si);
  c:=EndianLongword (c);
  Result:=LongwordAsSingle (c);
end;

{----------------------------------------}
function EndianDouble (d: double): double;
{----------------------------------------}
{ Wandelt die Byte-Order eines Double: Little-Endian (12345678) <-> Big-Endian (87654321) }
var
  i: Int64;
begin
  i:=DoubleAsInt64 (d);
  i:=EndianInt64 (i);
  Result:=Int64AsDouble (i);
end;

{----------------------------------------}
function EndianString (s: string): string;
{----------------------------------------}
{ Wandelt die Byte-Order eines String: Little-Endian (123456789...) <-> Big-Endian (...987654321) }
var
  i: integer;
begin
  Result:='';
  for i:=length (s) downto 1 do
    Result:=Result + s [i];
end;

{---------------------------------------------}
function ByteSwapInteger (i: integer): integer;
{---------------------------------------------}
{ Wandelt die Byte-Order eines Integer: Little-Endian (1234) <-> Little Byte Swap (2143) }
begin
  Result:=(Swap (word ((i and $FFFF0000) shr 16)) shl 16) or
          (Swap (word (i and $FFFF)));
end;

{------------------------------------------------}
function ByteSwapLongword (c: cardinal): cardinal;
{------------------------------------------------}
{ Wandelt die Byte-Order eines Longword: Little-Endian (1234) <-> Little Byte Swap (2143) }
begin
  Result:=(Swap (word ((c and $FFFF0000) shr 16)) shl 16) or
          (Swap (word (c and $FFFF)));
end;

{---------------------------------------}
function ByteSwapInt64 (i: Int64): Int64;
{---------------------------------------}
{ Wandelt die Byte-Order eines Int64: Little-Endian (12345678) <-> Little Byte Swap (21436587) }
begin
  Result:=(Int64 (Swap (word ((i and $FFFF000000000000) shr 48))) shl 48) or
          (Int64 (Swap (word ((i and $FFFF00000000) shr 32))) shl 32) or
          (Int64 (Swap (word ((i and $FFFF0000) shr 16))) shl 16) or
          (Int64 (Swap (word (i and $FFFF))));
end;

{-------------------------------------------}
function ByteSwapSingle (si: single): single;
{-------------------------------------------}
{ Wandelt die Byte-Order eines Single: Little-Endian (1234) <-> Little Byte Swap (2143) }
var
  c: cardinal;

begin
  c:=SingleAsLongword (si);
  c:=ByteSwapLongword (c);
  Result:=LongwordAsSingle (c);
end;

{------------------------------------------}
function ByteSwapDouble (d: double): double;
{------------------------------------------}
{ Wandelt die Byte-Order eines Double: Little-Endian (12345678) <-> Little Byte Swap (21436587) }
var
  i: Int64;
begin
  i:=DoubleAsInt64 (d);
  i:=ByteSwapInt64 (i);
  Result:=Int64AsDouble (i);
end;


{---------------------- String-Wandlung: Hex/Bin‰r ----------------------------}

{--------------------------------------}
function Hex2Bin (sHex: string): string;
{--------------------------------------}
{ wandelt Hex-String in Bin‰r-String (z.B. '4142' -> 'AB') }
var
  S: string;
  i: integer;
  b: byte;
  Code: integer;

begin
  S:='';
  i := 1;
  while i < length (sHex) do begin
    Val ('$' + Copy (sHex, i, 2), b, Code);
    if Code <> 0 then begin
      Result:='';
      exit;
    end;
    S:=S + Chr (b);
    Inc (i, 2);
  end;
  Result:=S;
end;

{---------------------------------------------------------------}
function Bin2Hex (sBin: string; bSpace: boolean = false): string;
{---------------------------------------------------------------}
{ wandelt Bin‰r-String in Hex-String (z.B. 'AB' -> '4142');
  Optional: Ausgabe mit Leerzeichen als Trenner f¸r bessere Leserlichkeit in
            Anzeigen (z.B. 'AB' -> '41 42') }
var
  i: integer;

begin
  Result:='';
  for i:=1 to length (sBin) do begin
    Result:=Result + IntToHex (Ord (sBin [i]), 2);
    if bSpace AND (i < length (sBin)) then
      Result:=Result + ' ';
  end;
end;


{----------- Standard-Strukturen: ganze und reelle Zahlen ---------------------}

{-------------------------------------}
function Bin2Byte (sBin: string): byte;
{-------------------------------------}
{ wandelt Bin‰r-String in Byte (8 Bit ohne Vorzeichen) }
var
  S: string [1];
  b: byte;

begin
  S:=Copy(sBin, 1, 1);
  if length (S) = 1 then
    b:=Ord (S [1])
  else
    b:=0;
  Result:=b;
end;

{---------------------------------------}
function Byte2Bin (iValue: byte): string;
{---------------------------------------}
{ wandelt Byte (8 Bit ohne Vorzeichen) in Bin‰r-String }
begin
  Result:=Chr (iValue);
end;

{---------------------------------------------}
function Bin2Shortint (sBin: string): shortint;
{---------------------------------------------}
{ wandelt Bin‰r-String in Shortint (8 Bit mit Vorzeichen) }
var
  S: string [1];
  i: shortint;

begin
  S:=Copy(sBin, 1, 1);
  if length (S) = 1 then
    i:=shortint (S [1])
  else
    i:=0;
  Result:=i;
end;

{-----------------------------------------------}
function Shortint2Bin (iValue: shortint): string;
{-----------------------------------------------}
{ wandelt Shortint (8 Bit mit Vorzeichen) in Bin‰r-String }
begin
  Result:=Chr (iValue AND $FF);
end;

{--------------------------------------------------------------------------------------}
function Bin2Smallint (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): smallint;
{--------------------------------------------------------------------------------------}
{ wandelt Bin‰r-String (Default-Format Little-Endian: 12) in Smallint (16 Bit mit Vorzeichen) }
var
  S: string [2];
  i: smallint;
  StrStream: TStringStream;

begin
  S:=Copy(sBin, 1, 2);
  if length (S) = 2 then begin
    StrStream:=TStringStream.Create (S);
    try
      StrStream.Read (i, length (S));
    finally
      StrStream.Free;
    end;
  end else
    i:=0;

  if (ByteOrder = bo_BigEndian) OR
     (ByteOrder = bo_LittleByteSwap) then  // 20.10.2009, WW
    Result:=EndianSmallInt (i)
  else  // Little-Endian
    Result:=i;
end;

{----------------------------------------------------------------------------------------}
function Smallint2Bin (iValue: smallint; ByteOrder: TByteOrder = bo_LittleEndian): string;
{----------------------------------------------------------------------------------------}
{ wandelt Smallint (16 Bit mit Vorzeichen) in Bin‰r-String (Default-Format Little-Endian: 12) }
var
  i: smallint;

begin
  if (ByteOrder = bo_BigEndian) OR
     (ByteOrder = bo_LittleByteSwap) then  // 20.10.2009, WW
    i:=EndianSmallint (iValue)
  else  // Little-Endian
    i:=iValue;

  Result:=Chr (i AND $00FF) +
          Chr ((i AND $FF00) SHR 8);
end;

{------------------------------------------------------------------------------}
function Bin2Word (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): word;
{------------------------------------------------------------------------------}
{ wandelt Bin‰r-String (Default-Format Little-Endian: 12) in Word (16 Bit ohne Vorzeichen) }
var
  S: string [2];
  w: word;
  StrStream: TStringStream;

begin
  S:=Copy(sBin, 1, 2);
  if length (S) = 2 then begin
    StrStream:=TStringStream.Create (S);
    try
      StrStream.Read (w, length (S));
    finally
      StrStream.Free;
    end;
  end else
    w:=0;

  if (ByteOrder = bo_BigEndian) OR
     (ByteOrder = bo_LittleByteSwap) then  // 20.10.2009, WW
    Result:=EndianWord (w)
  else  // Little-Endian
    Result:=w;
end;

{--------------------------------------------------------------------------------}
function Word2Bin (wValue: word; ByteOrder: TByteOrder = bo_LittleEndian): string;
{--------------------------------------------------------------------------------}
{ wandelt Word (16 Bit ohne Vorzeichen) in Bin‰r-String (Default-Format Little-Endian: 12) }
var
  w: word;

begin
  if (ByteOrder = bo_BigEndian) OR
     (ByteOrder = bo_LittleByteSwap) then  // 20.10.2009, WW
    w:=EndianWord (wValue)
  else  // Little-Endian
    w:=wValue;

  Result:=Chr (w AND $00FF) +
          Chr ((w AND $FF00) SHR 8);
end;

{------------------------------------------------------------------------------------}
function Bin2Integer (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): integer;
{------------------------------------------------------------------------------------}
{ wandelt Bin‰r-String (Default-Format Little-Endian: 1234) in Integer (32 Bit mit Vorzeichen) }
var
  S: string [4];
  i: integer;
  StrStream: TStringStream;

begin
  S:=Copy(sBin, 1, 4);
  if length (S) = 4 then begin
    StrStream:=TStringStream.Create (S);
    try
      StrStream.Read (i, length (S));
    finally
      StrStream.Free;
    end;
  end else
    i:=0;

  if ByteOrder = bo_BigEndian then
    Result:=EndianInteger (i)
  else if ByteOrder = bo_LittleByteSwap then
    Result:=ByteSwapInteger (i)
  else
    Result:=i;
end;

{--------------------------------------------------------------------------------------}
function Integer2Bin (iValue: integer; ByteOrder: TByteOrder = bo_LittleEndian): string;
{--------------------------------------------------------------------------------------}
{ wandelt Integer (32 Bit mit Vorzeichen) in Bin‰r-String (Default-Format Little-Endian: 1234) }
var
  i: integer;

begin
  if ByteOrder = bo_BigEndian then
    i:=EndianInteger (iValue)
  else if ByteOrder = bo_LittleByteSwap then
    i:=ByteSwapInteger (iValue)
  else
    i:=iValue;

  Result:=Chr (i AND $000000FF) +
          Chr ((i AND $0000FF00) SHR 8) +
          Chr ((i AND $00FF0000) SHR 16) +
          Chr ((i AND $FF000000) SHR 24);
end;

{--------------------------------------------------------------------------------------}
function Bin2Longword (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): cardinal;
{--------------------------------------------------------------------------------------}
{ wandelt Bin‰r-String (Default-Format Little-Endian: 1234) in Longword (32 Bit ohne Vorzeichen) }
var
  S: string [4];
  c: cardinal;
  StrStream: TStringStream;

begin
  S:=Copy(sBin, 1, 4);
  if length (S) = 4 then begin
    StrStream:=TStringStream.Create (S);
    try
      StrStream.Read (c, length (S));
    finally
      StrStream.Free;
    end;
  end else
    c:=0;

  if ByteOrder = bo_BigEndian then
    Result:=EndianLongword (c)
  else if ByteOrder = bo_LittleByteSwap then
    Result:=ByteSwapLongword (c)
  else
    Result:=c;
end;

{----------------------------------------------------------------------------------------}
function Longword2Bin (cValue: cardinal; ByteOrder: TByteOrder = bo_LittleEndian): string;
{----------------------------------------------------------------------------------------}
{ wandelt Longword (32 Bit ohne Vorzeichen) in Bin‰r-String (Default-Format Little-Endian: 1234) }
var
  c: cardinal;

begin
  if ByteOrder = bo_BigEndian then
    c:=EndianLongword (cValue)
  else if ByteOrder = bo_LittleByteSwap then
    c:=ByteSwapLongword (cValue)
  else
    c:=cValue;

  Result:=Chr (c AND $000000FF) +
          Chr ((c AND $0000FF00) SHR 8) +
          Chr ((c AND $00FF0000) SHR 16) +
          Chr ((c AND $FF000000) SHR 24);
end;

{--------------------------------------------------------------------------------}
function Bin2Int64 (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): Int64;
{--------------------------------------------------------------------------------}
{ wandelt Bin‰r-String (Default-Format Little-Endian: 12345678) in Int64 (64 Bit ohne Vorzeichen) }
var
  S: string [8];
  i: Int64;
  StrStream: TStringStream;

begin
  S:=Copy(sBin, 1, 8);
  if length (S) = 8 then begin
    StrStream:=TStringStream.Create (S);
    try
      StrStream.Read (i, length (S));
    finally
      StrStream.Free;
    end;
  end else
    i:=0;

  if ByteOrder = bo_BigEndian then
    Result:=EndianInt64 (i)
  else if ByteOrder = bo_LittleByteSwap then
    Result:=ByteSwapInt64 (i)
  else
    Result:=i;
end;

{-----------------------------------------------------------------------------------}
function Int64_2Bin (iValue: Int64; ByteOrder: TByteOrder = bo_LittleEndian): string;
{-----------------------------------------------------------------------------------}
{ wandelt Int64 (64 Bit ohne Vorzeichen) in Bin‰r-String (Default-Format Little-Endian: 12345678) }
var
  i: Int64;

begin
  if ByteOrder = bo_BigEndian then
    i:=EndianInt64 (iValue)
  else if ByteOrder = bo_LittleByteSwap then
    i:=ByteSwapInt64 (iValue)
  else
    i:=iValue;

  Result:=Chr (i AND $00000000000000FF) +
          Chr ((i AND $000000000000FF00) SHR 8) +
          Chr ((i AND $0000000000FF0000) SHR 16) +
          Chr ((i AND $00000000FF000000) SHR 24) +
          Chr ((i AND $000000FF00000000) SHR 32) +
          Chr ((i AND $0000FF0000000000) SHR 40) +
          Chr ((i AND $00FF000000000000) SHR 48) +
          Chr ((i AND $FF00000000000000) SHR 56);
end;


{----------------------------------------------------------------------------------}
function Bin2Single (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): single;
{----------------------------------------------------------------------------------}
{ wandelt Bin‰r-String (Format: 4 Byte float) in Single }
var
  S: string [4];
  si: single;
  StrStream: TStringStream;

begin
  S:=Copy(sBin, 1, 4);
  if length (S) = 4 then begin
    StrStream:=TStringStream.Create (S);
    try
      StrStream.Read (si, length (S));
    finally
      StrStream.Free;
    end;
  end else
    si:=0;

  if ByteOrder = bo_BigEndian then
    Result:=EndianSingle (si)
  else if ByteOrder = bo_LittleByteSwap then
    Result:=ByteSwapSingle (si)
  else
    Result:=si;
end;

{-------------------------------------------------------------------------------------}
function Single2Bin (siValue: single; ByteOrder: TByteOrder = bo_LittleEndian): string;
{-------------------------------------------------------------------------------------}
{ wandelt Single (32 Bit-Float) in Bin‰r-String (Default-Format Little-Endian: 1234) }
var
  c: cardinal;

begin
  c:=SingleAsLongword (siValue);
  Result:=Longword2Bin (c, ByteOrder);
end;

{----------------------------------------------------------------------------------}
function Bin2Double (sBin: string; ByteOrder: TByteOrder = bo_LittleEndian): double;
{----------------------------------------------------------------------------------}
{ wandelt Bin‰r-String (Format: 8 Byte float) in Double }
var
  S: string [8];
  d: double;
  StrStream: TStringStream;

begin
  S:=Copy(sBin, 1, 8);
  if length (S) = 8 then begin
    StrStream:=TStringStream.Create (S);
    try
      StrStream.Read (d, length (S));
    finally
      StrStream.Free;
    end;
  end else
    d:=0;

  if ByteOrder = bo_BigEndian then
    Result:=EndianDouble (d)
  else if ByteOrder = bo_LittleByteSwap then
    Result:=ByteSwapDouble (d)
  else
    Result:=d;
end;

{------------------------------------------------------------------------------------}
function Double2Bin (dValue: double; ByteOrder: TByteOrder = bo_LittleEndian): string;
{------------------------------------------------------------------------------------}
{ wandelt Double (64 Bit-Float) in Bin‰r-String (Default-Format Little-Endian: 12345678) }
var
  i: Int64;

begin
  i:=DoubleAsInt64 (dValue);
  Result:=Int64_2Bin (i, ByteOrder);
end;


{--------------------- Strukturen f¸r Actaris Corus ---------------------------}

{------------------------------------------------}
function Bin2Date_Corus (sBin: string): TDateTime;
{------------------------------------------------}
{ wandelt Bin‰r-String (Corus-Format: 4 Byte DATE) in TDateTime }
var
  c: cardinal;
  sec: word;
  min: word;
  hour: word;
  day: word;
  month: word;
  year: word;

begin
  c:=Bin2Longword (sBin);
  if c > 0 then begin  // nicht "leer"
    year := (c AND $FC000000) SHR 26;
    month:= (c AND $03C00000) SHR 22;
    day  := (c AND $003E0000) SHR 17;
    hour := (c AND $0001F000) SHR 12;
    min  := (c AND $00000FC0) SHR 6;
    sec  := (c AND $0000003F);

    year := year + 2000;  // Jahr 4-stellig
    try
      Result:=EncodeDateTime (year, month, day, hour, min, sec, 0);
    except
      Result:=0;
    end;
  end else
    Result:=0;
end;

{---------------------------------------------------}
function Date2Bin_Corus (dtDatum: TDateTime): string;
{---------------------------------------------------}
{ wandelt TDateTime in Bin‰r-String (Corus-Format: 4 Byte DATE) }
var
  c: cardinal;
  msec: word;
  sec: word;
  min: word;
  hour: word;
  day: word;
  month: word;
  year: word;

begin
  DecodeDateTime (dtDatum, year, month, day, hour, min, sec, msec);
  year:=year MOD 100;  // Jahr 2-stellig

  c:=((year AND $0000003F) SHL 26) +
     ((month AND $0000000F) SHL 22 )+
     ((day AND $0000001F) SHL 17) +
     ((hour AND $0000001F) SHL 12) +
     ((min AND $0000003F) SHL 6) +
     (sec AND $0000003F);
  Result:=Longword2Bin (c);
end;

{--------------------------------------------------}
function Bin2Float16_1_Corus (sBin: string): double;
{--------------------------------------------------}
{ wandelt Bin‰r-String (Corus-Format: 2 Byte FLOAT16_1) in Double }
var
  i: smallint;

begin
  i:=Bin2Smallint (sBin);
  Result:=i / 100;
end;

{--------------------------------------------------}
function Bin2Float16_2_Corus (sBin: string): double;
{--------------------------------------------------}
{ wandelt Bin‰r-String (Corus-Format: 2 Byte FLOAT16_2) in Double }
var
  w: word;
  e: word;
  m: word;

begin
  w:=Bin2Word (sBin);
  e := (w AND $8000) SHR 15;
  m := (w AND $7FFF);
  Result:=m * IntPower (10, e-3);
end;

{----------------------------------------------}
function Bin2Index_Corus (sBin: string): double;
{----------------------------------------------}
{ wandelt Bin‰r-String (Corus-Format: INDEX) in Double }
var
  S: string [8];
  c_int: cardinal;
  c_dec: cardinal;
  d: double;

begin
  S:=Copy(sBin, 1, 8);
  if length (S) = 8 then begin
    c_int:=Bin2Longword (Copy (S, 1, 4));
    c_dec:=Bin2Longword (Copy (S, 5, 4));
    d:=c_int + (c_dec / 100000000);
  end else
    d:=0;
  Result:=d;
end;

{------------------------------------------------}
function Index2Bin_Corus (dValue: double): string;
{------------------------------------------------}
{ wandelt Double in Bin‰r-String (Corus-Format: INDEX) }
var
  c_int: cardinal;
  c_dec: cardinal;

begin
  c_int:=Trunc (dValue);
  c_dec:=Trunc (Frac (dValue) * 100000000);
  Result:=Longword2Bin (c_int) + Longword2Bin (c_dec);
end;

{---------------------------------------------------}
function Bin2Alarm_Corus (sBin: string): TCorusAlarm;
{---------------------------------------------------}
{ wandelt Bin‰r-String (Corus-Format: ALARM) in TCorusAlarm }
var
  S: string [22];
  ca: TCorusAlarm;

begin
  S:=Copy(sBin, 1, 22);
  if length (S) = 22 then begin
    with ca do begin
      Active:=Bin2Byte (Copy (S, 1, 1));
      Memo:=Bin2Byte (Copy (S, 2, 1));
      Value:=Bin2Single (Copy (S, 3, 4));
      Start:=Bin2Date_Corus (Copy (S, 7, 4));
      Ende:=Bin2Date_Corus (Copy (S, 11, 4));
      MStart:=Bin2Date_Corus (Copy (S, 15, 4));
      MEnde:=Bin2Date_Corus (Copy (S, 19, 4));
    end;
  end else
    FillChar (ca, SizeOf (ca), 0);
  Result:=ca;
end;

{-------------------------------------------------}
function Alarm2Bin_Corus (ca: TCorusAlarm): string;
{-------------------------------------------------}
{ wandelt TCorusAlarm in Bin‰r-String (Corus-Format: ALARM) }
begin
  with ca do
    Result:=Byte2Bin (Active) +
            Byte2Bin (Memo) +
            Single2Bin (Value) +
            Date2Bin_Corus (Start) +
            Date2Bin_Corus (Ende) +
            Date2Bin_Corus (MStart) +
            Date2Bin_Corus (MEnde);
end;

end.
