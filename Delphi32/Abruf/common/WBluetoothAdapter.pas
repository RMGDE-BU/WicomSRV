{******************************************************************************}
{* Unit: Routinen für Kommunikation mit Bluetooth-Adapter                     *}
{* 11.08.2011  WW  Neu                                                        *}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2011                                      *}
{******************************************************************************}
unit WBluetoothAdapter;

interface

uses
  SysUtils;


function GetKonfigKommando_PuE_K01_Blue (iBaudrate: integer;
  sDataFormat: string): string;

implementation

{----------------------------------------------------------}
function GetKonfigKommando_PuE_K01_Blue (iBaudrate: integer;
  sDataFormat: string): string;
{----------------------------------------------------------}
{ Befehl zum Einstellen der Schnittstellen-Parameter im P+E K01-Bluetooth
  Auslesekopf bilden;
  Übergabe: Baudrate
            Datenformat (Datenbits, Parität, Stopbits, z.B. 8N1)
  Ergebnis: Befehl }
var
	cBaud: char;
	cDatabits: char;
	cStopbits: char;
	cParity: char;

begin
	// Default-Einstellungen des Auslesekopf nach dem Einschalten:
	cBaud:='0';	// 300 Baud
	cDatabits:='7';
	cStopbits:='1';
	cParity:='E';

  // Baudrate
	case iBaudrate of
		  300: cBaud:='0';
		  600: cBaud:='1';
		 1200: cBaud:='2';
		 2400: cBaud:='3';
		 4800: cBaud:='4';
		 9600: cBaud:='5';
		19200: cBaud:='6';
		38400: cBaud:='7';
		57600: cBaud:='8';
	end;  { case iBaudrate }

  // Datenbits
  if length (sDataFormat) >= 1 then
    if sDataFormat [1] in ['5'..'8'] then
      cDatabits:=sDataFormat [1];

	// Parität
  if length (sDataFormat) >= 2 then
    if sDataFormat [2] in ['N', 'E', 'O'] then
      cParity:=sDataFormat [2];

  // Stopbits
  if length (sDataFormat) >= 3 then
    if sDataFormat [3] in ['1', '2'] then
      cStopbits:=sDataFormat [3];

  Result:=Format ('%sBLUE0%s',
                  [#$FE + #$FE, cDatabits + cParity + cStopbits + cBaud + #$FF]);
end;

end.
 