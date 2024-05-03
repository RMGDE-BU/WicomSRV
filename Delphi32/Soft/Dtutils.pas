{--------------------------------------------------------------------}
{ 23.5.2000 Sm is_MEZ_MESZ, LetztSonntagimMonat                      }
{--------------------------------------------------------------------}
unit Dtutils;

interface

uses
  SysUtils,windows;

const
  CDTHour = 1.0 / 24;


function GetGasDay (PhysicalDay: TDateTime; TagesEnde: integer): TDateTime;
function GetPhysicalDay (GasDay: TDateTime; TagesEnde: integer): TDateTime;
Function Is_MEZ_MESZ(Var dt:TDatetime):char;

function GetHour (Datum: TDateTime): Word;
function LastDayOfMonth (DateTime: TDateTime): Word;
function MinDateTime (DT1, DT2: TDateTime): TDateTime;
function MaxDateTime (DT1, DT2: TDateTime): TDateTime;
Function HourToIndex(Hour: integer): integer;
function IndexToHourExt (anIndex: integer; aTagesende: integer; vorwaerts: boolean): integer;
Function IndexToHour(Index: integer): integer;
Procedure GetSommerzeitWechsel(Jahr: word; var Monat: word; var Tag: word; var Stunde: word);
Procedure GetWinterzeitWechsel(Jahr: word; var Monat: word; var Tag: word; var Stunde: word);

implementation

type
  PDayTable = ^TDayTable;
  TDayTable = array [1..12] of Word;

{-------------------------------------------------------------------------}
function GetGasDay (PhysicalDay: TDateTime; TagesEnde: integer): TDateTime;
{-------------------------------------------------------------------------}
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime (PhysicalDay, Hour, Min, Sec, MSec);
  if Hour < TagesEnde then
    Result := PhysicalDay - 1
  else
    Result := PhysicalDay;
end;

{-------------------------------------------------------------------------}
function GetPhysicalDay (GasDay: TDateTime; TagesEnde: integer): TDateTime;
{-------------------------------------------------------------------------}
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime (GasDay, Hour, Min, Sec, MSec);
  if Hour < TagesEnde then
    Result := GasDay + 1
  else
    Result := GasDay;
end;

{----------------------------------------}
function GetHour (Datum: TDateTime): Word;
{----------------------------------------}
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime (Datum, Hour, Min, Sec, MSec);
  Result := Hour;
end;

{----------------------------------------}
function IsLeapYear (Year: Word): Boolean;
{----------------------------------------}
begin
  Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

{-------------------------------------------}
function GetDayTable (Year: Word): PDayTable;
{-------------------------------------------}
const
  DayTable1: TDayTable = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  DayTable2: TDayTable = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  DayTables: array[Boolean] of PDayTable = (@DayTable1, @DayTable2);
begin
  Result := DayTables [IsLeapYear(Year)];
end;

{-------------------------------------------------------------------------------------------}
Procedure GetLetztSonntagimMonat(Jahr: word; Monat: word; var Tag: word);
{-------------------------------------------------------------------------------------------}
{ der letzte Sonntag eines Monats März;
  Übergabe: Jahr,Monat
  Rückgabe: Tag }
var
  DoW: integer;
  pt:PDayTable;
begin
  pt:=GetDayTable(Jahr);
  Tag:=pt^[Monat];
  DoW:=DayOfWeek(EncodeDate(Jahr,Monat,Tag));
  if DoW > 1 then
    Tag:=Tag - DoW + 1;
end;

{-------------------------------------------------------------------------------------------}
Function Is_MEZ_MESZ(Var dt:TDatetime):char;
{ Gibt Info aus PC Systemeinstellung, ob dt in Sommer oder Winterzeit ist zurück}
{  Übergabe: dt }
{  Rückgabe: S:Sommerzeit oder W: Winterzeit}
{-------------------------------------------------------------------------------------------}

Var
  tzi       : TIME_ZONE_INFORMATION;
  y,m,t,std     : Word;

  WZStart,SZStart:TDateTime;
begin
  GetTimeZoneInformation(tzi);
  DecodeDate(dt,y,m,t);

// Start Winterzeit Berechnung
  m  := tzi.StandardDate.WMonth;
  Std:=tzi.StandardDate.WHour;
  if tzi.StandardDate.WDay= 5 then    // letzter Sonntag
    GetLetztSonntagimMonat(y,m,t)
  else
    t:=tzi.StandardDate.WDay;
  WZStart:= EncodeDate(Y,M,t)+EncodeTime(Std,0,0,0);

// Start Sommerzeitberechnung
  M := tzi.DayLightDate.WMonth;
  Std:=tzi.DayLightDate.WHour;
  if tzi.DayLightDate.wDay= 5 then  // Letzter Sonntag
    GetLetztSonntagimMonat(y,m,t)
  else
    t:=tzi.DayLightDate.WDay;
  SZStart:= EncodeDate(Y,M,t)+EncodeTime(Std,0,0,0);

  if (dt >= SZStart) and (dt <= WZStart) then
    result:='S'
  else
    result:='W';
end;

{--------------------------------------------------}
function LastDayOfMonth (DateTime: TDateTime): Word;
{--------------------------------------------------}
var
  Year, Month, Day: Word;
begin
  DecodeDate (DateTime, Year, Month, Day);
  Result := GetDayTable (Year)^[Month];
end;

{----------------------------------------------------}
function MinDateTime (DT1, DT2: TDateTime): TDateTime;
{----------------------------------------------------}
begin
  if DT1 < DT2 then
    Result := DT1
  else
    Result := DT2;
end;

{----------------------------------------------------}
function MaxDateTime (DT1, DT2: TDateTime): TDateTime;
{----------------------------------------------------}
begin
  if DT1 > DT2 then
    Result := DT1
  else
    Result := DT2;
end;

{-------------------------------------------}
Function HourToIndex(Hour: integer): integer;
{-------------------------------------------}
{ Gastag-Umsetzung: Hour: 7  -> Index: 1   ...   Hour: 6 -> Index: 24 }
begin
  if Hour > 6 then
    Result:=Hour - 6
  else
    Result:=Hour + 18;
end;

{--------------------------------------------}
Function IndexToHour(Index: integer): integer;
{--------------------------------------------}
{ Gastag-Umsetzung: Index: 1  ->  Hour: 7   ...  Index: 24  -> Hour: 6 }
begin
  if Index < 18 then
    Result:=Index + 6
  else
    Result:=Index - 18;
end;

{-------------------------------------------------------------------------------------------}
function IndexToHourExt (anIndex: integer; aTagesende: integer; vorwaerts: boolean): integer;
{-------------------------------------------------------------------------------------------}
{ Umsetzung Index -> Stunde;
  z.B. Tagesende = 6:
    vorwaerts = true:  Index: 1 -> Hour: 7 ... Index: 24 -> Hour: 6
    vorwaerts = false: Index: 1 -> Hour: 6 ... Index: 24 -> Hour: 7  }
var
  i: integer;
begin
  i:=anIndex;
  if not vorwaerts then
    i:=Abs (i - 25);
  if i < (24 - aTagesende) then
    Result:=i + aTagesende
  else
    Result:=i + aTagesende - 24;
end;

{-------------------------------------------------------------------------------------------}
Procedure GetSommerzeitWechsel(Jahr: word; var Monat: word; var Tag: word; var Stunde: word);
{-------------------------------------------------------------------------------------------}
{ Zeitpunkt der Umstellung von Winter- auf Sommerzeit ermitteln -> der letzte Sonntag im März;
  Übergabe: Jahr
  Rückgabe: Monat, Tag, Stunde }
var
  DoW: integer;
begin
  Stunde:=2;
  Monat:=3;
  Tag:=31;
  DoW:=DayOfWeek(EncodeDate(Jahr,Monat,Tag));
  if DoW > 1 then
    Tag:=Tag - DoW + 1;
end;

{-------------------------------------------------------------------------------------------}
Procedure GetWinterzeitWechsel(Jahr: word; var Monat: word; var Tag: word; var Stunde: word);
{-------------------------------------------------------------------------------------------}
{ Zeitpunkt der Umstellung von Sommer- auf Winterzeit ermitteln -> der letzte Sonntag im Oktober
  Übergabe: Jahr
  Rückgabe: Monat, Tag, Stunde }
var
  DoW: integer;
begin
  Stunde:=3;
  Monat:=10;
  Tag:=31;
  DoW:=DayOfWeek(EncodeDate(Jahr,Monat,Tag));
  if DoW > 1 then
    Tag:=Tag - DoW + 1;
end;

end.
