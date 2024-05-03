{***************************************************************************}
{ Unit: Konvertierungsroutinen für die MRG-Parameter Zeit und Datum         }
{ Version 04.01.99  WW                                                      }
{************************************************************************** }
Unit MDTFormt;

INTERFACE

uses
  SysUtils, T_BinMask, UnixDT;

function MRGStrToDateTime(DateFormat: string; DateStr: string;
                          TimeFormat: string; TimeStr: string): TDateTime;

function FormatMRGTime(TimeFormat: string; DateTime: TDateTime): string;
function FormatMRGDate(DateFormat: string; DateTime: TDateTime): string;

IMPLEMENTATION

{------------------------------------------------------------------------}
function MRGStrToDateTime(DateFormat: string; DateStr: string;
                          TimeFormat: string; TimeStr: string): TDateTime;
{------------------------------------------------------------------------}
{ Konvertierung Datumstring, ZeitString -> TDateTime-Struktur;
  Übergabe: Formatstrings, Datum/Zeitstring
  Ergebnis: DateTime }
var
  dFormat: string;
  tFormat: string;
  Year, Month, Day: word;
  Hour, Min, Sec: word;
  Jahr_2stellig: boolean;
begin
  dFormat:=UpperCase(DateFormat);
  tFormat:=UpperCase(TimeFormat);
  if (dFormat = 'DATE_CORUS') AND (tFormat = 'DATE_CORUS') then begin
    Result:=Bin2Date_Corus (TimeStr);
  end
  else if (dFormat = 'UNIX') AND (tFormat = 'UNIX') then begin
    UnixTimeStrToDateTime (TimeStr, Result);
  end
  else if (dFormat = 'MB') AND (tFormat = 'MB') then begin  // per Modbus; 08.03.2019, WW
    try
      Result:=StrToDateTime (TimeStr);
    except
      Result:=0;
    end;
  end
  else begin
    { Datum: }
    Jahr_2stellig:=true;
    if dFormat = 'TTMMJJ' then begin
      Year  := StrToInt(copy(DateStr,5,2));
      Month := StrToInt(copy(DateStr,3,2));
      Day   := StrToInt(copy(DateStr,1,2));
    end
    else if dFormat = 'TTMMJJJJ' then begin  // 29.09.2021, WW
      Year  := StrToInt(copy(DateStr,5,4));
      Month := StrToInt(copy(DateStr,3,2));
      Day   := StrToInt(copy(DateStr,1,2));
      Jahr_2stellig:=false;
    end
    else if dFormat = 'TT.MM.JJ' then begin
      Year  := StrToInt(copy(DateStr,7,2));
      Month := StrToInt(copy(DateStr,4,2));
      Day   := StrToInt(copy(DateStr,1,2));
    end
    else if dFormat = 'TT.MM.JJJJ' then begin
      Year  := StrToInt(copy(DateStr,7,4));
      Month := StrToInt(copy(DateStr,4,2));
      Day   := StrToInt(copy(DateStr,1,2));
      Jahr_2stellig:=false;
    end
    else if dFormat = 'JJMMTT' then begin
      Year  := StrToInt(copy(DateStr,1,2));
      Month := StrToInt(copy(DateStr,3,2));
      Day   := StrToInt(copy(DateStr,5,2));
    end
    else if dFormat = 'JJ/MM/TT' then begin
      Year  := StrToInt(copy(DateStr,1,2));
      Month := StrToInt(copy(DateStr,4,2));
      Day   := StrToInt(copy(DateStr,7,2));
    end
    else if dFormat = 'JJJJ-MM-TT' then begin
      Year  := StrToInt(copy(DateStr,1,4));
      Month := StrToInt(copy(DateStr,6,2));
      Day   := StrToInt(copy(DateStr,9,2));
      Jahr_2stellig:=false;
    end
    else begin  { wie JJMMTT }
      Year  := StrToInt(copy(DateStr,1,2));
      Month := StrToInt(copy(DateStr,3,2));
      Day   := StrToInt(copy(DateStr,5,2));
    end;

    if Jahr_2stellig then begin
      if Year < 80 then
        Year:=Year + 2000
      else
        Year:=Year + 1900;
    end;

    { Zeit: }
    if (tFormat = 'HH-MM-SS') OR (tFormat = 'HH:MM:SS') then begin
      Hour := StrToInt(copy(TimeStr,1,2));
      Min  := StrToInt(copy(TimeStr,4,2));
      Sec  := StrToInt(copy(TimeStr,7,2));
    end
    else if tFormat = 'HHMMSS' then begin
      Hour := StrToInt(copy(TimeStr,1,2));
      Min  := StrToInt(copy(TimeStr,3,2));
      Sec  := StrToInt(copy(TimeStr,5,2));
    end
    else begin  { wie HHMMSS }
      Hour := StrToInt(copy(TimeStr,1,2));
      Min  := StrToInt(copy(TimeStr,3,2));
      Sec  := StrToInt(copy(TimeStr,5,2));
    end;

    Result:=EncodeDate (Year, Month, Day) + EncodeTime (Hour, Min, Sec, 0);
  end;
end;

{----------------------------------------------------------------------}
function FormatMRGTime(TimeFormat: string; DateTime: TDateTime): string;
{----------------------------------------------------------------------}
{ Konvertierung DateTime -> ZeitString;
  Übergabe: TimeFormat-String, DateTime
  Ergebnis: formatierter Zeitstring }
var
  S, F: string;
  Hour, Min, Sec: word;
  dummy: word;
begin
  F:=UpperCase(TimeFormat);
  if F = 'UNIX' then begin
    DateTimeToUnixTimeStr (DateTime, S);
  end
  else begin
    DecodeTime (DateTime, Hour, Min, Sec, dummy);
    if F = 'HH-MM-SS' then
      S := Format ('%.2d', [Hour]) + '-' +
           Format ('%.2d', [Min]) + '-' +
           Format ('%.2d', [Sec])
    else if F = 'HH:MM:SS' then
      S := Format ('%.2d', [Hour]) + ':' +
           Format ('%.2d', [Min]) + ':' +
           Format ('%.2d', [Sec])
    else if F = 'HHMMSS' then
      S := Format ('%.2d', [Hour]) +
           Format ('%.2d', [Min]) +
           Format ('%.2d', [Sec])
    else   { wie HHMMSS }
      S := Format ('%.2d', [Hour]) +
           Format ('%.2d', [Min]) +
           Format ('%.2d', [Sec]);
  end;
  Result:=S;
end;

{----------------------------------------------------------------------}
function FormatMRGDate(DateFormat: string; DateTime: TDateTime): string;
{----------------------------------------------------------------------}
{ Konvertierung DateTime -> Datumstring;
  Übergabe: DateFormat-String, DateTime
  Ergebnis: formatierter Datumstring }
var
  S, F: string;
  Year, Month, Day: word;
begin
  DecodeDate (DateTime, Year, Month, Day);
  F:=UpperCase(DateFormat);
  if F = 'JJJJ-MM-TT' then
    S := Format ('%.4d', [Year]) + '-' +
         Format ('%.2d', [Month]) + '-' +
         Format ('%.2d', [Day])
  else if F = 'JJMMTT' then
    S := Format ('%.2d', [Year MOD 100]) +
         Format ('%.2d', [Month]) +
         Format ('%.2d', [Day])
  else   { wie JJJJMMTT }
    S := Format ('%.4d', [Year]) +
         Format ('%.2d', [Month]) +
         Format ('%.2d', [Day]);
  Result:=S;
end;

end.




