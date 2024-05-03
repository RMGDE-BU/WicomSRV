{****************************************************************************}
{* Unit: Berechnen des CRC12                                                *}
{* Version: 22.12.99  WW                                                    *}
{****************************************************************************}
unit CRC12;

interface

function bcrc12(zeichen: char; crc: word): word;
{ Function für zeichenweise CRC12-Bildung }

function scrc12(S: string; Preset: word): word;
{ Function für CRC12-Bildung aus String }

implementation

const
  { Tabelle für CRC12 }

  table_842: array[0..255] of word = (
    $000, $d39, $af7, $7ce, $56b, $852, $f9c, $2a5,
    $ad6, $7ef, $021, $d18, $fbd, $284, $54a, $873,
    $529, $810, $fde, $2e7, $042, $d7b, $ab5, $78c,
    $fff, $2c6, $508, $831, $a94, $7ad, $063, $d5a,
    $a52, $76b, $0a5, $d9c, $f39, $200, $5ce, $8f7,
    $084, $dbd, $a73, $74a, $5ef, $8d6, $f18, $221,
    $f7b, $242, $58c, $8b5, $a10, $729, $0e7, $dde,
    $5ad, $894, $f5a, $263, $0c6, $dff, $a31, $708,
    $421, $918, $ed6, $3ef, $14a, $c73, $bbd, $684,
    $ef7, $3ce, $400, $939, $b9c, $6a5, $16b, $c52,
    $108, $c31, $bff, $6c6, $463, $95a, $e94, $3ad,
    $bde, $6e7, $129, $c10, $eb5, $38c, $442, $97b,
    $e73, $34a, $484, $9bd, $b18, $621, $1ef, $cd6,
    $4a5, $99c, $e52, $36b, $1ce, $cf7, $b39, $600,
    $b5a, $663, $1ad, $c94, $e31, $308, $4c6, $9ff,
    $18c, $cb5, $b7b, $642, $4e7, $9de, $e10, $329,
    $842, $57b, $2b5, $f8c, $d29, $010, $7de, $ae7,
    $294, $fad, $863, $55a, $7ff, $ac6, $d08, $031,
    $d6b, $052, $79c, $aa5, $800, $539, $2f7, $fce,
    $7bd, $a84, $d4a, $073, $2d6, $fef, $821, $518,
    $210, $f29, $8e7, $5de, $77b, $a42, $d8c, $0b5,
    $8c6, $5ff, $231, $f08, $dad, $094, $75a, $a63,
    $739, $a00, $dce, $0f7, $252, $f6b, $8a5, $59c,
    $def, $0d6, $718, $a21, $884, $5bd, $273, $f4a,
    $c63, $15a, $694, $bad, $908, $431, $3ff, $ec6,
    $6b5, $b8c, $c42, $17b, $3de, $ee7, $929, $410,
    $94a, $473, $3bd, $e84, $c21, $118, $6d6, $bef,
    $39c, $ea5, $96b, $452, $6f7, $bce, $c00, $139,
    $631, $b08, $cc6, $1ff, $35a, $e63, $9ad, $494,
    $ce7, $1de, $610, $b29, $98c, $4b5, $37b, $e42,
    $318, $e21, $9ef, $4d6, $673, $b4a, $c84, $1bd,
    $9ce, $4f7, $339, $e00, $ca5, $19c, $652, $b6b
  );

{----------------------------------------------}
function bcrc12(zeichen: char; crc: word): word;
{----------------------------------------------}
{ Function für zeichenweise CRC12-Bildung }
const
  C_Final = $0000;
var
  ASC: Byte;
begin
  ASC := ord (zeichen);
  crc := (crc SHR 8) XOR table_842[((crc XOR ASC) AND $ff)];
  crc := crc XOR C_Final;
  Result := crc;
end;                             

{---------------------------------------------}
function scrc12(S: string; Preset: word): word;
{---------------------------------------------}
{ Function für CRC12-Bildung aus String }
var
  crc: word;
  i: integer;
begin
  crc := Preset;
  for i := 1 to length(S) do
    crc := bcrc12(S[i], crc);
  Result := crc;
end;

end.
