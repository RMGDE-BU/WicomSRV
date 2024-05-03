{******************************************************************************}
{* Unit: Zeichenkonstanten                                                    *}
{* 11.12.2000 WW                                                              *}
{******************************************************************************}
unit WChars;

interface

Const

  NUL = #0;
  SOH = #1;
  STX = #2;
  ETX = #3;
  EOT = #4;
  ENQ = #5;
  ACK = #6;
  LF  = #10;
  CR  = #13;
  DLE = #16;
  NAK = #21;
  ETB = #23;
  CAN = #24;
  SUB = #26;
  ESC = #27;
  FS  = #28;
  GS =  #29;
  RS  = #30;
  US  = #31;

  CSonderzeichen: array [#0..#31] of PChar =
    ('<NUL>', '<SOH>', '<STX>', '<ETX>', '<EOT>', '<ENQ>', '<ACK>', '<BEL>',
     '<BS>', '<HT>', '<LF>', '<VT>', '<FF>', '<CR>', '<SO>', '<SI>',
     '<DLE>', '<DC1>', '<DC2>', '<DC3>', '<DC4>', '<NAK>', '<SYN>', '<ETB>',
     '<CAN>', '<EM>', '<SUB>', '<ESC>', '<FS>', '<GS>', '<RS>', '<US>');


function SonderzeichenString (S: string): string;

implementation

{-----------------------------------------------}
function SonderzeichenString (S: string): string;
{-----------------------------------------------}
{ Sonderzeichen in S in lesbare Zeichen konvertieren }
var
  ErgStr: string;
  i: integer;
begin
  ErgStr:='';
  if length (S) > 0 then begin
    for i:=1 to length (S) do begin
      if (S [i] >= Low (CSonderzeichen)) AND (S [i] <= High (CSonderzeichen)) then
        ErgStr:=ErgStr + string (CSonderzeichen [S [i]])
      else
        ErgStr:=ErgStr + S [i];
    end;
  end;
  Result:=ErgStr;
end;

end.

