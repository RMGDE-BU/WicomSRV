{******************************************************************************}
{* Unit: Routinen für Umrechnung UNIX-Zeitformat <-> TDateTime                *}
{* Version:   25.11.1999     WW                                               *}
{******************************************************************************}
unit UnixDT;

interface

uses SysUtils;


function GetUnixSekundenFromDateTime (DateTime: TDateTime): cardinal;
function GetUnixSekundenFromUnixTimeStr (UnixTimeStr: string; var UnixSec: cardinal): boolean;
procedure UnixSekundenToDateTime (UnixSekunden: cardinal; var DateTime: TDateTime);
procedure DateTimeToUnixTimeStr (DateTime: TDateTime; var UnixTimeStr: string);
function UnixTimeStrToDateTime (UnixTimeStr: string; var DateTime: TDateTime): boolean;

implementation

{ Info zu den in den Unixzeit-Konvertierungsroutinen verwendeten Zahlenwerten:

  Sekunden pro Jahr mit Schalttag:  31622400
  Sekunden pro Jahr ohne Schalttag: 31536000

  Sekunden pro Monat mit 31 Tagen:   2678400
  Sekunden pro Monat mit 30 Tagen:   2592000
  Sekunden pro Monat mit 29 Tagen:   2505600
  Sekunden pro Monat mit 28 Tagen:   2419200

  Sekunden pro Tag:                    86400
  Sekunden pro Stunde:                  3600
  Sekunden pro Minute:                    60 }


{-------------------------------------------------------------------}
function GetUnixSekundenFromDateTime (DateTime: TDateTime): cardinal;
{-------------------------------------------------------------------}
{ liefert Anzahl der Sekunden von 1.1.1970 bis zum übergebenen DateTime-Zeitpunkt }
var
  Jahr, Monat, Tag: word;
  Stunde, Minute, Sekunde, MilliSek: word;
  i: integer;
  jahr_org, anz_jahre, monate: integer;
  sek: cardinal;

begin
  DecodeDate(DateTime, Jahr, Monat, Tag);
  DecodeTime(DateTime, Stunde, Minute, Sekunde, MilliSek);
  Jahr:=StrToInt(Copy(IntToStr(Jahr),3,2));                  { Jahr 2-stellig }

  jahr_org:=jahr;

  sek:=0;
  anz_jahre:=jahr-70;
  if anz_jahre < 0 then
    anz_jahre:=anz_jahre+100;
  jahr:=70;
  for i:=1 to anz_jahre do begin
    if (jahr mod 4 = 0) then
      sek:=sek+31622400
    else
      sek:=sek+31536000;
    jahr:=jahr+1;
  end;

  monate:=1;
  for i:=1 to monat-1 do begin
    case monate of
      1,3,5,7,8,10,12: sek:=sek+2678400;
      4,6,9,11       : sek:=sek+2592000;
      2	             : if (jahr_org mod 4 = 0) then
                         sek:=sek+2505600
                       else
                         sek:=sek+2419200;
    end;
    monate:=monate+1;
  end;

  for i:=1 to tag-1 do
    sek:=sek+86400;

  for i:=1 to stunde do
    sek:=sek+3600;

  for i:=1 to minute do
    sek:=sek+60;

  Result:=sek+sekunde;
end;

{--------------------------------------------------------------------------------------------}
function GetUnixSekundenFromUnixTimeStr (UnixTimeStr: string; var UnixSec: cardinal): boolean;
{--------------------------------------------------------------------------------------------}
{ konvertiert den übergebenen Unix-Zeitformat-String (Anzahl der Sekunden in
  Hex-Form) in eine Zahl;
  Ergebnis: true, wenn Konvertierung ohne Fehler }
var
  code: integer;

begin
  try
    Val ('$' + UnixTimeStr, UnixSec, code);
    Result:=Code = 0;
  except
    UnixSec:=0;
    Result:=false;
  end;
end;

{---------------------------------------------------------------------------------}
procedure UnixSekundenToDateTime (UnixSekunden: cardinal; var DateTime: TDateTime);
{---------------------------------------------------------------------------------}
{ wandelt Anzahl der Sekunden ab 1.1.1970 in TDateTime-Struktur um }
var
  jahr,monat,tag,stunde,minute,sekunde: integer;
  unix_sek: cardinal;

begin
  unix_sek:=UnixSekunden;

  jahr:=1970;
  monat:=1;
  tag:=1;
  stunde:=0;
  minute:=0;

  if unix_sek >= 31536000 then begin                                 { > 1970 }
    while ((unix_sek >= 31536000) and (jahr mod 4 <> 0)) or
          ((unix_sek >= 31622400) and (jahr mod 4 = 0)) do begin
      if jahr mod 4 = 0 then
        unix_sek:=unix_sek - 31622400
      else
        unix_sek:=unix_sek - 31536000;
      jahr:=jahr+1;
    end;
  end;

  if unix_sek >= 2678400 then begin                                { > Januar }
    while ((unix_sek >= 2678400) and (monat in [1,3,5,7,8,10,12])) or
	  ((unix_sek >= 2592000) and (monat in [4,6,9,11])) or
          ((unix_sek >= 2419200) and ((monat = 2) and (jahr mod 4 <> 0))) or
	  ((unix_sek >= 2505600) and ((monat = 2) and (jahr mod 4 = 0))) do begin
      case monat of
        1,3,5,7,8,10,12: unix_sek:=unix_sek - 2678400;
        4,6,9,11       : unix_sek:=unix_sek - 2592000;
        2	       : if jahr mod 4 = 0 then
                           unix_sek:=unix_sek - 2505600
                         else
                           unix_sek:=unix_sek - 2419200;
      end;
      monat:=monat+1;
    end;
  end;

  if unix_sek >= 86400 then begin                            { mehr als 1 Tag }
    while unix_sek >= 86400 do begin
      unix_sek:=unix_sek - 86400;
      tag:=tag+1;
    end;
  end;

  if unix_sek >= 3600 then begin                          { mehr als 1 Stunde }
    while unix_sek >= 3600 do begin
      unix_sek:=unix_sek - 3600;
      stunde:=stunde + 1;
    end;
  end;

  if unix_sek >= 60 then begin                            { mehr als 1 Minute }
    while unix_sek >= 60 do begin
      unix_sek:=unix_sek - 60;
      minute:=minute+1;
    end;
  end;

  sekunde:=unix_sek;                                      { der Sekunden-Rest }

  DateTime:=EncodeDate (jahr, monat, tag) + EncodeTime (stunde, minute, sekunde, 0);
end;

{-----------------------------------------------------------------------------}
procedure DateTimeToUnixTimeStr (DateTime: TDateTime; var UnixTimeStr: string);
{-----------------------------------------------------------------------------}
{ Wandelt TDateTime-Struktur in String im Unix-Zeitformat um }
var
  sek: cardinal;

begin
  sek:=GetUnixSekundenFromDateTime (DateTime);
  UnixTimeStr:=Format ('%.8x', [sek]);
end;

{-------------------------------------------------------------------------------------}
function UnixTimeStrToDateTime (UnixTimeStr: string; var DateTime: TDateTime): boolean;
{-------------------------------------------------------------------------------------}
{ Wandelt String im Unix-Zeitformat in TDateTime-Struktur um;
  Ergebnis: true, wenn Konvertierung ohne Fehler }
var
  UnixSec: cardinal;

begin
  if GetUnixSekundenFromUnixTimeStr (UnixTimeStr, UnixSec) then begin
    UnixSekundenToDateTime (UnixSec, DateTime);
    Result:=true;
  end
  else begin
    DateTime:=0;
    Result:=false;
  end;
end;

End.
