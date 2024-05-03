{****************************************************************************}
{* Unit: Berechnen des Blockprüfzeichens CRC16                              *}
{* Version: 14.01.97  SM/WW                                                 *}
{****************************************************************************}
unit CRC16;

interface

function bcrc16_all(zeichen: char; crc: word): word;
function bcrc16(zeichen: char; crc: word): word;
function scrc16(S: string; Start: word): word;
function GetCRC16_Chars_Hex (ACRC: word): string;
function scrc16_Corus(S: string; Start: word): word;
function GetCRC16_Chars_LoHi (CRC: word): string;

implementation

uses
  Windows, SysUtils;

{ Tabelle für CRC16  }

type
   table_8408_Feld = array[0..255] of word;

const
  PRESET : word = 0;
  table_8408 : table_8408_Feld = (
    $0000, $1189, $2312, $329b, $4624, $57ad, $6536, $74bf,
    $8c48, $9dc1, $af5a, $bed3, $ca6c, $dbe5, $e97e, $f8f7,
    $1081, $0108, $3393, $221a, $56a5, $472c, $75b7, $643e,
    $9cc9, $8d40, $bfdb, $ae52, $daed, $cb64, $f9ff, $e876,
    $2102, $308b, $0210, $1399, $6726, $76af, $4434, $55bd,
    $ad4a, $bcc3, $8e58, $9fd1, $eb6e, $fae7, $c87c, $d9f5,
    $3183, $200a, $1291, $0318, $77a7, $662e, $54b5, $453c,
    $bdcb, $ac42, $9ed9, $8f50, $fbef, $ea66, $d8fd, $c974,
    $4204, $538d, $6116, $709f, $0420, $15a9, $2732, $36bb,
    $ce4c, $dfc5, $ed5e, $fcd7, $8868, $99e1, $ab7a, $baf3,
    $5285, $430c, $7197, $601e, $14a1, $0528, $37b3, $263a,
    $decd, $cf44, $fddf, $ec56, $98e9, $8960, $bbfb, $aa72,
    $6306, $728f, $4014, $519d, $2522, $34ab, $0630, $17b9,
    $ef4e, $fec7, $cc5c, $ddd5, $a96a, $b8e3, $8a78, $9bf1,
    $7387, $620e, $5095, $411c, $35a3, $242a, $16b1, $0738,
    $ffcf, $ee46, $dcdd, $cd54, $b9eb, $a862, $9af9, $8b70,
    $8408, $9581, $a71a, $b693, $c22c, $d3a5, $e13e, $f0b7,
    $0840, $19c9, $2b52, $3adb, $4e64, $5fed, $6d76, $7cff,
    $9489, $8500, $b79b, $a612, $d2ad, $c324, $f1bf, $e036,
    $18c1, $0948, $3bd3, $2a5a, $5ee5, $4f6c, $7df7, $6c7e,
    $a50a, $b483, $8618, $9791, $e32e, $f2a7, $c03c, $d1b5,
    $2942, $38cb, $0a50, $1bd9, $6f66, $7eef, $4c74, $5dfd,
    $b58b, $a402, $9699, $8710, $f3af, $e226, $d0bd, $c134,
    $39c3, $284a, $1ad1, $0b58, $7fe7, $6e6e, $5cf5, $4d7c,
    $c60c, $d785, $e51e, $f497, $8028, $91a1, $a33a, $b2b3,
    $4a44, $5bcd, $6956, $78df, $0c60, $1de9, $2f72, $3efb,
    $d68d, $c704, $f59f, $e416, $90a9, $8120, $b3bb, $a232,
    $5ac5, $4b4c, $79d7, $685e, $1ce1, $0d68, $3ff3, $2e7a,
    $e70e, $f687, $c41c, $d595, $a12a, $b0a3, $8238, $93b1,
    $6b46, $7acf, $4854, $59dd, $2d62, $3ceb, $0e70, $1ff9,
    $f78f, $e606, $d49d, $c514, $b1ab, $a022, $92b9, $8330,
    $7bc7, $6a4e, $58d5, $495c, $3de3, $2c6a, $1ef1, $0f78
  );

{--------------------------------------------------}
function bcrc16_all(zeichen: char; crc: word): word;
{--------------------------------------------------}
{ Function für zeichenweise CRC16-Bildung (ohne Ausnahme irgendwelcher Zeichen) }
var
  ASC: Byte;
begin
  ASC := ord(zeichen);
  crc := (crc SHR 8) XOR table_8408[((crc XOR ASC) AND $ff)];
  Result := crc;
end;

{----------------------------------------------}
function bcrc16(zeichen: char; crc: word): word;
{----------------------------------------------}
{ Function für zeichenweise CRC16-Bildung (ohne STX) }
const
  STX = #2;
var
  ASC: Byte;
begin
  if zeichen <> STX then begin
    ASC := ord(zeichen);
    crc := (crc SHR 8) XOR table_8408[((crc XOR ASC) AND $ff)];
  end;
  Result := crc;
end;

{--------------------------------------------}
function scrc16(S: string; Start: word): word;
{--------------------------------------------}
{ Function für CRC16-Bildung aus String (ohne STX, bis einschließlich ETX) }
const
  ETX = #3;
var
  crc: word;
  i: integer;
begin
  crc := Start;
  for i := 1 to length(S) do begin
    crc := bcrc16(S[i], crc);
    if S[i] = ETX then Break;
  end;
  Result := crc;
end;

{-----------------------------------------------}
function GetCRC16_Chars_Hex (ACRC: word): string;
{-----------------------------------------------}
{ Ergebnis: CRC als 4-stelliger Hex-String }
begin
  Result:=Format ('%.4x', [ACRC]);
end;

{--------------------------------------------------}
function scrc16_Corus(S: string; Start: word): word;
{--------------------------------------------------}
{ Function für CRC16-Bildung aus String für Actaris Corus }
var
  i: byte;
  j: integer;
  crc: word;

begin
  crc:=Start;
{$R-}
  for j:=1 to length (S) do begin
    crc := (((crc SHR 8) XOR Ord(S[j])) SHL 8) OR (crc AND $00ff);
    for i:=1 to 8 do begin
      if (crc AND $8000) > 0 then
        crc:=(crc SHL 1) XOR $8005
      else
        crc:=crc SHL 1;
    end;
  end;
{$R+}
  Result:=crc;
end;

{-----------------------------------------------}
function GetCRC16_Chars_LoHi (CRC: word): string;
{-----------------------------------------------}
{ Ergebnis: CRC als 2-stelliger Binärstring gebildet aus Low-Byte und High-Byte }
begin
  Result:=Chr(LoByte(CRC)) + Chr(HiByte(CRC));
end;

end.
