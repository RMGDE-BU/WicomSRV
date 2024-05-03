{******************************************************************************}
{* Unit: Zeit-Routinen                                                        *}
{* Version: 17.02.99                                                          *}
{* 24.01.01  WW  Round in F_TimeDiff durch Trunc ersetzt                      *}
{* 01.02.01  GD  Neu: DateTimeToRec                                           *}
{* 14.01.02  GD  Neu: RecToDateTime                                           *}
{* 03.09.02  WW  Neu: F_VormonatsErster                                       *}
{* 03.01.03  WW  Neu: EncodeDateStr, EncodeTimeStr                            *}
{* 11.08.04  GD  Neu: CalcDayLightChange, IsDateTimeStandardTime              *}
{* 11.12.06  GD  Neu: StringMaskToDateTime                                    *)
{* 03.05.07  GD  Delay erweitert um SC-Abbruch                                *)
(* 13.06.08  GD  Bugfix bei Zeitzone                                          *)
{* 20.11.08  NW  Neu: F_NextMonatsErster                                      *}
{* 20.11.08  NW  Neu: GetLetzteVolleStunde                                    *}
{* 24.03.10  NW  Neu: IsDatumPlausibel                                        *}
{* 12.07.10  NW  Neu: UTCStrToDateTime                                        *}
{*                    F_MonatsErster                                          *}
{* 21.01.11  WN  Neu: GetVolleStunde                                          *}
{*                    IsVolleStunde                                           *}
{* 08.04.14  WN  Neu: IsWSZeitUmstellung                                      *}
{* 21.01.19  WW  WFileDateTime_System liefert lokale Zeit                     *}
{* 25.02.19  WW  Neu: IsSWZeitUmstellung                                      *}
{* 06.11.19  WW  Neu: GetNaechsteVolleStunde                                  *}
{* 13.11.19  WW  Erweiterte Plausibilisierung in F_TimeDiff                   *}
{* 31.03.20  WW  Neu: GetTimeZoneBias                                         *}
{* 03.11.20  WW  Neu: F_GetTickCountDiff                                      *}
{* 05.10.22  WW  Neu: WFileTimeToDateTime, WNowUTC                            *}
{******************************************************************************}
Unit T_Zeit;

INTERFACE

Uses
  Windows, SysUtils, Forms, T_Tools, WStrUtils, DateUtils, Math;

Function   F_LetztTag (Datum: DateRec): Integer;
Procedure  P_Vortag  (Var Datum: Daterec);
Procedure  P_Nachtag (Var Datum: DateRec);
Procedure  P_AddDatumStunden (Var Datum: DateRec; Var Zeit: TimeRec; Stunden: Integer);
Procedure  P_AddMinuten (Var datum: DateRec; Var Zeit: Timerec; minuten: longint);
Function CmpTime (Var Time1, Time2: TimeRec): Integer;
Function CmpDate (Var Date1, Date2: DateRec): Integer;
function CmpDateTime (DT1, DT2: TDateTime): integer;
procedure F_TimeDiff(DT1, DT2: TDateTime; var Secs: longint);
function F_GetTickCountDiff(var iLastTickCount: cardinal): cardinal;
function Delay (ms: cardinal; bExitOnESC: boolean = False): boolean;
function Jahr2To4Stellig (Jahr: integer): integer;
function GetDatetimeRec(Var st:string):TDateTime;
function DateTimeToRec(pDateTime: TDateTime; var pDateRec: DateRec;
  var pTimeRec: TimeRec): boolean;
function RecToDateTime(
  pDateRec: DateRec; pTimeRec: TimeRec; var pDateTime: TDateTime): boolean;
function F_VormonatsErster (ADate: TDateTime): TDateTime;
function F_MonatsErster (ADate: TDateTime): TDateTime;  // 12.07.2010  WN
function F_NextMonatsErster (ADate: TDateTime): TDateTime;
function EncodeDateStr (DateStr: string; Format: string; var Datum: TDateTime): boolean;
function EncodeTimeStr (TimeStr: string; Format: string; var Zeit: TDateTime): boolean;
function StringMaskToDateTime(
  const sString: string; const sMask: string): TDateTime;
function GetWZ_SZ_Offset (AktSZ_WZ_Flag: integer; SZ_WZ_Umstellung_im_Geraet: boolean): TDateTime;
function CalcDayLightChange(
  iYear: word; pSystemTimeInfo: SYSTEMTIME): TDateTime;
function IsDateTimeStandardTime(dtDateTime: TDateTime): boolean;
function WFileDateTime_System (const FileName: string): TDateTime;
function WFileTimeToDateTime (const FileTime: TFileTime): TDateTime;
function WNowUTC: TDateTime;
function UTCTimezoneStrToDateTime (sTz: string; var dtTz: TDateTime): boolean;
function DateTimeToUTCStr (dtDatumZeit: TDateTime; bIsSZ: boolean): string;
function UTCStrToDateTime (sMask, sZeit: String): TDateTime;  // 12.07.2010  WN
function GetLetzteVolleStunde(dt: TDateTime): TDateTime;
function GetNaechsteVolleStunde(dt: TDateTime): TDateTime;
function IsDatumPlausibel (dt: TDateTime): Boolean;
function GetVolleStunde(dt: TDateTime): TDateTime;  // 21.01.2011  WN
function IsVolleStunde(dt: TDateTime; iTimeFuzzy: Word): Boolean;  // 21.01.2011  WN
function IsWSZeitUmstellung(dt: TDateTime): Boolean;  // 08.04.2014 WN
function IsSWZeitUmstellung(dt: TDateTime): Boolean;
function GetTimeZoneBias (bIncludeDayLightBias: boolean): TDateTime;

IMPLEMENTATION

// Liefert Datetimerec
// Übergabe st:     <TT.MM.YYYY hh:mm:ss> z.B. <16.12.1999 10:21:30>
// Rückgabe TDateTime
{--------------------------------------------}
function GetDatetimeRec(Var st:string):TDateTime;
{--------------------------------------------}
Var TT,MM,YYYY,hh,mi,ss:integer;
    dt1,dt2:TDatetime;

begin
 Try
   TT :=strtoint(copy(st,1,2));
   MM :=strtoint(copy(st,4,2));
   YYYY :=strtoint(copy(st,7,4));
   hh :=strtoint(copy(st,12,2));
   mi :=strtoint(copy(st,15,2));
   ss :=strtoint(copy(st,18,2));
   dt1:=EncodeDate(YYYY,MM,TT);
   dt2:=EncodeTime(hh,mi,ss,0);
 Except
   dt1:=EncodeDate(1900,1,1);
   dt2:=EncodeTime(0,0,0,0);
 end;
   result:=dt1+dt2;
end;

{--------------------------------------------}
Function F_LetztTag (Datum: DateRec): Integer;
{--------------------------------------------}
{ gibt letzten Tag des übergebenen Datums zurück }
Begin
  case datum.month of
       1,3,5,7,8,10,12 : F_LetztTag:=31;
       4,6,9,11        : F_LetztTag:=30;
       2               : if datum.year mod 4 = 0
                         then F_LetztTag:=29
                         else F_LetztTag:=28;
  else F_LetztTag:=0;
  end;
end;

{--------------------------------------}
Procedure P_Vortag (Var Datum: Daterec);
{--------------------------------------}
begin
  if datum.day=1 then begin                    { Monatswechsel }
    if datum.month=1 then begin                { Jahreswechsel }
       dec(datum.year);
       datum.month:=12;
       datum.day:=31;
    end
    else begin
      dec(datum.month);
      datum.day:=F_LetztTag(datum);
    end;
  end else
   dec(datum.day);                             { Tageswechsel }
end;

{---------------------------------------}
Procedure P_Nachtag (Var Datum: DateRec);
{---------------------------------------}
Begin
  if datum.day=F_LetztTag(datum) then begin
    if datum.month=12 then begin               { Jahreswechsel }
       inc(datum.year);
       datum.month:=1;
       datum.day:=1;
    end
    else begin
      inc(datum.month);
      datum.day:=1;
    end;
  end else
   inc(datum.day);                             { Tageswechsel  }
End;

{------------------------------------------------------------------}
Procedure  P_AddDatumStunden (Var Datum: DateRec; Var Zeit: TimeRec;
                              Stunden: Integer);
{------------------------------------------------------------------}
{- Addiert zu einer gegebenen Datums/Zeitinformation eine beliebige
   Anzahl von Stunden
   Die Anzahl der Stunden sollte nur eine Erh”hung vin wenigen Tagen
   bedeuten
   Stunden kann negativ oder positiv sein -}
Begin
  Zeit.Hour := Zeit.Hour + Stunden;
  While Zeit.Hour > 23 Do Begin
    P_NachTag (Datum);
    Zeit.Hour := Zeit.Hour - 24;
  End;
  While Zeit.Hour < 0 Do Begin
    P_VorTag (Datum);
    Zeit.Hour := Zeit.Hour + 24;
  End;
End;

{-----------------------------------------}
Procedure P_AddMinuten(Var datum :DateRec;
                       Var Zeit  :Timerec;
                       minuten   :longint);
{-----------------------------------------}
Var
  min,std      : Integer;
  Tage : Longint;
  i            : Longint;

begin
                                               { Umrechnung min in std und tage }
   min:= (abs(minuten) mod 60);
   std:= (abs(minuten) div 60) mod 24;
   tage:= (abs(minuten) div 60) div 24;

   if minuten>=0 then begin                    { Minuten, Stunden und Tage Addieren }

     if zeit.min+min>=60 then begin            { Minutenberlauf }
        zeit.min:=zeit.min+min-60;
        if zeit.hour>23 then begin             { Stundenberlauf }
          zeit.hour:=0;
          P_Nachtag(Datum);
        end else
          inc(zeit.hour);
     end else
       zeit.min:=zeit.min+min;

     if zeit.hour+std>23 then begin            { Subtrahieren Stunden }
          zeit.hour:=zeit.hour-24+std;
          P_nachtag(datum);
     end else
          zeit.hour:=zeit.hour+std;

     for i:=1 to tage do                       { Tage }
        P_NachTag(datum);
   end else
   begin
                                               { Minuten, Stunden und Tage subtrahieren }
     if zeit.min-min<0 then begin              { Subtrahieren Minuten }
       zeit.min:=zeit.min+60-min;
       if (zeit.hour<1) then begin
          zeit.hour:=23;
          P_vortag(datum);
       end else
          dec(zeit.hour);
     end
     else
       zeit.min:=Zeit.min-min;

     if zeit.hour-std<0 then begin             { Subtrahieren Stunden }
          zeit.hour:=zeit.hour+24-std;
          P_vortag(datum);
     end else
          zeit.hour:=zeit.hour-std;

     for i:=1 to tage do P_vortag(datum);      { Subtrahieren Tage }

   end;{ Subtraktion }

end;{prc}

{----------------------------------------------------}
Function CmpTime (Var Time1, Time2: TimeRec): Integer;
{----------------------------------------------------}
Begin
  If Time1.Hour > Time2.Hour Then
    CmpTime := 1
  Else
    If Time1.Hour < Time2.Hour Then
      CmpTime := -1
    Else Begin
      If Time1.Min > Time2.Min Then
        CmpTime := 1
      Else
        If Time1.Min < Time2.Min Then
          CmpTime := -1
        Else Begin
          If Time1.Sec > Time2.Sec Then
            CmpTime := 1
          Else
            If Time1.Sec < Time2.Sec Then
            CmpTime := -1
            Else Begin
              If Time1.HSec > Time2.HSec Then
                CmpTime := 1
              Else
                If Time1.HSec < Time2.HSec Then
                  CmpTime := -1
              Else
                CmpTime := 0;
            End;
        End;
    End;
End;

{----------------------------------------------------}
Function CmpDate (Var Date1, Date2: DateRec): Integer;
{----------------------------------------------------}
Begin
  If Date1.Year > Date2.Year Then
    CmpDate := 2
  Else
    If Date1.Year < Date2.Year Then
      CmpDate := -2
    Else Begin
      If Date1.Month > Date2.Month Then
        CmpDate := 2
      Else
        If Date1.Month < Date2.Month Then
          CmpDate := -2
        Else Begin
          If Date1.Day > Date2.Day Then
            CmpDate := 2
          Else
            If Date1.Day < Date2.Day Then
              CmpDate := -2
            Else
              CmpDate := 0;
        End;
    End;
End;

{--------------------------------------------------}
function CmpDateTime (DT1, DT2: TDateTime): integer;
{--------------------------------------------------}
{ vergleicht DT1 mit DT2 auf Basis von Tag, Monat, Jahr, Stunden, Minuten und Sekunden;
  Ergebnis: -1, wenn DT1 < DT2
             0, wenn DT1 = DT2
             1, wenn DT1 > DT2 }
var
  Year1, Month1, Day1,
  Year2, Month2, Day2,
  Hour1, Min1, Sek1,
  Hour2, Min2, Sek2,
  Dummy: word;
begin
  DecodeDate (DT1, Year1, Month1, Day1);
  DecodeDate (DT2, Year2, Month2, Day2);
  if Year1 < Year2 then
    Result:=-1
  else if Year1 > Year2 then
    Result:=1
  else begin                                                  { gleiches Jahr }
    if Month1 < Month2 then
      Result:=-1
    else if Month1 > Month2 then
      Result:=1
    else begin                                               { gleicher Monat }
      if Day1 < Day2 then
        Result:=-1
      else if Day1 > Day2 then
        Result:=1
      else begin                                               { gleicher Tag }
        DecodeTime (DT1, Hour1, Min1, Sek1, Dummy);
        DecodeTime (DT2, Hour2, Min2, Sek2, Dummy);
        if Hour1 < Hour2 then
          Result:=-1
        else if Hour1 > Hour2 then
          Result:=1
        else begin                                           { gleiche Stunde }
          if Min1 < Min2 then
            Result:=-1
          else if Min1 > Min2 then
            Result:=1
          else begin                                         { gleiche Minute }
            if Sek1 < Sek2 then
              Result:=-1
            else if Sek1 > Sek2 then
              Result:=1
            else begin                                      { gleiche Sekunde }
              Result:=0;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{-----------------------------------------------------------}
procedure F_TimeDiff(DT1, DT2: TDateTime; var Secs: longint);
{-----------------------------------------------------------}
{ Ermittelt die Differenz in Sekunden zwischen zwei Zeitpunkten DT1 und DT2
  Übergabe: DT1, DT2
  Rückgabe:  Secs (ist positiv, wenn DT1 < DT2) }
var
  DT: TDateTime;

begin
  DT := (DT2 - DT1) * SecsPerDay;
  if DT > MaxLongInt then
    DT:=MaxLongInt
  else if DT < ((-1) * MaxLongInt - 1) then  // "MinLongInt"; 13.11.2019, WW
    DT:=(-1) * MaxLongInt - 1;
  Secs := round (DT);                     { trunc statt round; 24.01.2001  WW
  { round MUSS sein, sonst Rundungsfehler (0,9998 muß 1 ergeben statt 0 !), 15.11.2004 WW }
end;

{------------------------------------------------------------------}
function F_GetTickCountDiff(var iLastTickCount: cardinal): cardinal;
{------------------------------------------------------------------}
{ Ermittelt die Differenz in Millisekunden zwischen dem übergebenen und dem
  aktuellen TickCount. Ein Überlauf der GetTickCount-Funktion wird dabei mit-
  berücksichtigt.
  Übergabe: TickCount, zu dem die Differenz ermittelt werden soll
  Rückgabe: Aktueller TickCount
  Ergebnis: Differenz in Millisekunden }
var
  iTickCount: cardinal;

begin
  iTickCount:=GetTickCount;  { Aktueller TickCount }
  { Differenz in Millisekunden: }
  if iTickCount >= iLastTickCount then
    Result:=iTickCount - iLastTickCount
  else  { Überlauf (GetTickCount beginnt zyklisch wieder bei 0 !) }
    Result:=(High (iLastTickCount) - iLastTickCount + 1) + iTickCount;
  { Rückgabe aktueller TickCount: }
  iLastTickCount:=iTickCount;  // 19.12.2022, WW
end;

{-----------------------------}
function Delay (ms: cardinal; bExitOnESC: boolean = False): boolean;
{-----------------------------}
var
  TickCount: cardinal;
begin
  Result := True;  // kein Abbruch durch den Anwender;
  TickCount := GetTickCount;
  while cardinal (GetTickCount) - TickCount < ms do begin
    Sleep (1);
    Application.ProcessMessages;
    if (bExitOnESC) and (EscPressed) then begin
      Result := False;  // Abbruch durch den Anwender;
      Break;
    end;
  end;
end;

{------------------------------------------------}
function Jahr2To4Stellig (Jahr: integer): integer;
{------------------------------------------------}
begin
  if Jahr >= 70 then
    Result:=1900 + Jahr
  else
    Result:=2000 + Jahr;
end;

{ Konvertiert einen TDateTime-Wert in ein Date-  }
{ und ein TimeRec                                }
{ Parameter: DateTime, Übergaberecords           }
{ Ergebnis: Erfolg Ja/Nein                       }
{------------------------------------------------}
function DateTimeToRec(pDateTime: TDateTime; var pDateRec: DateRec;
  var pTimeRec: TimeRec): boolean;
{------------------------------------------------}
var
  y, m, d, h, n, s, ms : word;
begin
  Result := False;  // Default

  try
    DecodeDate(pDateTime, y, m, d);
    pDateRec.Year := y;
    pDateRec.Month := m;
    pDateRec.Day := d;

    DecodeTime(pDateTime, h, n, s, ms);
    pTimeRec.Hour := h;
    pTimeRec.Min := n;
    pTimeRec.Sec := s;
    pTimeRec.Hsec := ms;

    Result := True;
  except
  // Result ist False
  end;
end;

{ Konvertiert ein Date- und ein TimeRec in einen }
{ TDateTime-Wert                                 }
{ Parameter: Date-/Time-Record, Übergabeparam.   }
{ Ergebnis: Erfolg Ja/Nein                       }
{------------------------------------------------}
function RecToDateTime(
  pDateRec: DateRec; pTimeRec: TimeRec; var pDateTime: TDateTime): boolean;
{------------------------------------------------}
begin
  Result := False;  // Default

  try
    pDateTime := EncodeDate(pDateRec.Year, pDateRec.Month, pDateRec.Day) +
      EncodeTime(pTimeRec.Hour, pTimeRec.Min, pTimeRec.Sec, pTimeRec.Hsec);

    Result := True;
  except
  // Result ist False
  end;
end;

{-------------------------------------------------------}
function F_VormonatsErster (ADate: TDateTime): TDateTime;
{-------------------------------------------------------}
{ Ergebnis: Vormonats-Erster des übergebenen Datums }
var
  year, month, day: word;
begin
  DecodeDate (ADate, year, month, day);
  day:=1;
  if month > 1 then
    dec (month)
  else begin
    month:=12;
    dec (year);
  end;
  Result:=EncodeDate (year, month, day);
end;

{ 12.07.2010  WN                                        }
{-------------------------------------------------------}
function F_MonatsErster (ADate: TDateTime): TDateTime;
{-------------------------------------------------------}
{ Ergebnis: Monats-Erster des übergebenen Datums }
var
  year, month, day: word;
begin
  DecodeDate (ADate, year, month, day);
  day:=1;
  Result:=EncodeDate (year, month, day);
end;

// -----------------------------------------------------------------------------
// Ermittlung des nächsten Monatsersten
// Parameter: Referenzdatum, zu dem der nächste Monatserste ermittelt werden soll
// Ergebnis: Datum des nächsten Monatsersten, 0:00 Uhr
// -----------------------------------------------------------------------------
function F_NextMonatsErster (ADate: TDateTime): TDateTime;
// -----------------------------------------------------------------------------
var
  wYear, wMonth, wDay: word;
begin
  DecodeDate(ADate, wYear, wMonth, wDay);
  wDay:=1;
  if (wMonth < 12) then
    Inc(wMonth)
  else begin
    wMonth:=1;
    Inc(wYear);
  end;
  Result:=EncodeDate(wYear, wMonth, wDay);
end;

{--------------------------------------------------------------------------------------}
function EncodeDateStr (DateStr: string; Format: string; var Datum: TDateTime): boolean;
{--------------------------------------------------------------------------------------}
{ Datum-String in TDateTime wandeln;
  Übergabe: Datum als String
            Format
  Rückgabe: Datum als TDateTime
  Ergebnis: true, wenn DateStr gültiges Datum repräsentiert }
var
  S_Year: string;
  sString : string;
begin
  Format := Trim(UpperCase(Format));
  sString := Trim(DateStr);

  Result:=true;
  Datum:=0;     // Vorbelegung für Datum-Rückgabe
  try
    if Format = 'YYYYMMDD' then
      Datum:=EncodeDate (StrToInt (Copy (sString, 1, 4)),
                         StrToInt (Copy (sString, 5, 2)),
                         StrToInt (Copy (sString, 7, 2)))
    else if Format = 'YYMMDD' then begin
      S_Year:=Copy (sString, 1, 2);
      { 4-stelliges Jahr bilden: }
      if StrToInt (S_Year) < 80 then
        S_Year:='20' + S_Year
      else
        S_Year:='19' + S_Year;
      Datum:=EncodeDate (StrToInt (S_Year),
                         StrToInt (Copy (sString, 3, 2)),
                         StrToInt (Copy (sString, 5, 2)));
    end
    else if Format = 'YY-MM-DD' then begin
      S_Year:=Copy (sString, 1, 2);
      { 4-stelliges Jahr bilden: }
      if StrToInt (S_Year) < 80 then
        S_Year:='20' + S_Year
      else
        S_Year:='19' + S_Year;
      Datum:=EncodeDate (StrToInt (S_Year),
                         StrToInt (Copy (sString, 4, 2)),
                         StrToInt (Copy (sString, 7, 2)));
    end
    else if Format = 'YYYY-MM-DD' then begin  // 28.03.2018, WW
      Datum:=EncodeDate (StrToInt (Copy (sString, 1, 4)),
                         StrToInt (Copy (sString, 6, 2)),
                         StrToInt (Copy (sString, 9, 2)))
    end
    else if Format = 'DDMMYYYY' then
      Datum:=EncodeDate (StrToInt (Copy (sString, 5, 4)),
                         StrToInt (Copy (sString, 3, 2)),
                         StrToInt (Copy (sString, 1, 2)))
    else if (Format = 'DD.MM.YYYY') OR
            (Format = 'DD/MM/YYYY') then
      Datum:=EncodeDate (StrToInt (Copy (sString, 7, 4)),
                         StrToInt (Copy (sString, 4, 2)),
                         StrToInt (Copy (sString, 1, 2)))
    else if Format = 'DD.MM.YY' then begin
      S_Year:=Copy (sString, 7, 2);
      { 4-stelliges Jahr bilden: }
      if StrToInt (S_Year) < 80 then
        S_Year:='20' + S_Year
      else
        S_Year:='19' + S_Year;
      Datum:=EncodeDate (StrToInt (S_Year),
                         StrToInt (Copy (sString, 4, 2)),
                         StrToInt (Copy (sString, 1, 2)));
    end
    else if Format = 'DDMMYY' then begin
      S_Year:=Copy (sString, 5, 2);
      { 4-stelliges Jahr bilden: }
      if StrToInt (S_Year) < 80 then                                          
        S_Year:='20' + S_Year
      else
        S_Year:='19' + S_Year;
      Datum:=EncodeDate (StrToInt (S_Year),
                         StrToInt (Copy (sString, 3, 2)),
                         StrToInt (Copy (sString, 1, 2)));
    end
    else
      Result:=false
  except
    Datum:=0;
    Result:=false;
  end;
end;

{-------------------------------------------------------------------------------------}
function EncodeTimeStr (TimeStr: string; Format: string; var Zeit: TDateTime): boolean;
{-------------------------------------------------------------------------------------}
{ Zeit-String in TDateTime wandeln;
  Übergabe: Zeit als String
            Format
  Rückgabe: Zeit als TDateTime
  Ergebnis: true, wenn TimeStr gültige Zeit repräsentiert }
var
  sString : string;
begin
  Format := Trim(UpperCase(StringReplace(Format, 'n', 'm', [rfReplaceAll])));
  sString := Trim(TimeStr);

  Result:=true;
  Zeit:=0;     // Vorbelegung für Zeit-Rückgabe
  try
    if (Format = 'HHMMSS') then
      Zeit:=EncodeTime (StrToInt (Copy (sString, 1, 2)),
                        StrToInt (Copy (sString, 3, 2)),
                        StrToInt (Copy (sString, 5, 2)),               
                        0)
    else if (Format = 'HHMM') then
      Zeit:=EncodeTime (StrToInt (Copy (sString, 1, 2)),
                        StrToInt (Copy (sString, 3, 2)),
                        0, 0)
    else if (Format = 'HH:MM:SS') then
      Zeit:=EncodeTime (StrToInt (Copy (sString, 1, 2)),
                        StrToInt (Copy (sString, 4, 2)),
                        StrToInt (Copy (sString, 7, 2)),
                        0)
    else
      Result:=false
  except
    Zeit:=0;
    Result:=false;
  end;
end;

{------------------------------------------------------------------------}
function GetWZ_SZ_Offset (AktSZ_WZ_Flag: integer;
                          SZ_WZ_Umstellung_im_Geraet: boolean): TDateTime;
{------------------------------------------------------------------------}
{ liefert den Winter-/Sommerzeit-Offset zwischen PC- und Geräte-Zeit;
 Übergabe: Flag zur aktuellen Zeitzone (0 = es ist gerade Winterzeit; 1 = es ist gerade Somerzeit)
           Flag für Zeitzonenumstellung im Gerät (true = Gerät arbeitet mit SZ/WZ-Wechsel)
 Rückgabewert: Zeitoffset als TDateTime }
var
  tzi: TIME_ZONE_INFORMATION;
begin
  Result:= 0;                   { default - kein Offset }
  if (AktSZ_WZ_Flag = 1) then begin         { Wenn Sommerzeit ... }
    GetTimeZoneInformation (tzi);         // 30.08.2000
    { Wenn PC und Gerät NICHT in der gleichen Zeitzone laufen ... }
    if (SZ_WZ_Umstellung_im_Geraet <> (tzi.DaylightBias <> 0)) then begin  // 13.07.2000
      if (not SZ_WZ_Umstellung_im_Geraet) then   { Gerät läuft auf Winterzeit }
        Result:= -EncodeTime(1, 0, 0, 0)
      else                                       { PC läuft auf Winterzeit }
        Result:= EncodeTime(1, 0, 0, 0);
    end;
  end;
end;

// ---------------------------------------------------------
// liefert den Abstand der lokalen Zeit zu UTC
// Eregbnis: LokaleZeit - UTC (z.B. +1 h bei MEZ)
// ---------------------------------------------------------
function GetTimeZoneBias (bIncludeDayLightBias: boolean): TDateTime;
const
   MINUTES_PER_DAY = 24.0 * 60.0;
var
  tzi : TTimeZoneInformation;
begin
   case GetTimeZoneInformation(tzi) of
      TIME_ZONE_ID_STANDARD:  // Normalzeit
         Result := (tzi.Bias) / MINUTES_PER_DAY;

      TIME_ZONE_ID_DAYLIGHT: // Sommerzeit
         if bIncludeDayLightBias then
           Result := (tzi.Bias + tzi.DaylightBias) / MINUTES_PER_DAY
         else
           Result := (tzi.Bias) / MINUTES_PER_DAY;  // wie bei Normalzeit
   else
      Result := 0.0;
   end;
   Result := Result * (-1);
end;

{-----------------------------------------------------------}
function CalcDayLightChange(
  iYear: word; pSystemTimeInfo: SYSTEMTIME): TDateTime;
{-----------------------------------------------------------}
var
  iDoW   : word;
  dt     : TDateTime;
begin
  Result := 0;

  iDoW := pSystemTimeInfo.wDayOfWeek + 1;

  if (pSystemTimeInfo.wDay in [1..4]) then begin
    dt := Trunc(EncodeDate(iYear, pSystemTimeInfo.wMonth, 1));
    if (DayOfWeek(dt) <= iDoW)
    then Result := dt + (iDoW - DayOfWeek(dt))
    else Result := dt + (7 - (DayOfWeek(dt) - iDow));
  end
  else if (pSystemTimeInfo.wDay = 5) then begin
    dt := Trunc(EncodeDate(iYear, 1, 31));
    dt := IncMonth(dt, pSystemTimeInfo.wMonth-1);  // 13.06.2008
    if (DayOfWeek(dt) >= iDoW)
    then Result := dt - (DayOfWeek(dt) - iDoW)
    else Result := dt - (7 - (iDow - DayOfWeek(dt)));
  end;

  Result := Result + EncodeTime(pSystemTimeInfo.wHour, pSystemTimeInfo.wMinute,
    pSystemTimeInfo.wSecond, pSystemTimeInfo.wMilliseconds);
end;

{ Überprüft, ob die übergebene Zeit in der Winterzeit liegt }
{ Parameter: Zu überprüfender Zeitpunkt                     }
{ Rückgabe: T = Winterzeit; F = Sommerzeit                  }
{-----------------------------------------------------------}
function IsDateTimeStandardTime(dtDateTime: TDateTime): boolean;
{-----------------------------------------------------------}

  // 0: <iMin, 1: =iMin, 2: <iMax, 3: =iMax, 4: > iMax
  function IsValueGreaterLowerEqual(iMin, iMax, iVal: word): byte;
  begin
    if (iVal < iMin) then Result := 0
    else if (iVal = iMin) then Result := 1
    else if (iVal < iMax) then Result := 2
    else if (iVal = iMax) then Result := 3
    else if (iVal > iMax) then Result := 4
    else Result := 255;
  end;

var
  pTZI             : TIME_ZONE_INFORMATION;
  pStd, pDay, pDT  : SYSTEMTIME;
  iVergleich       : byte;
  dt               : TDateTime;
begin
  Result := True;
  
  GetTimeZoneInformation (pTZI);
  pStd := pTZI.StandardDate;
  pDay := pTZI.DaylightDate;
  DateTimeToSystemTime(dtDateTime, pDT);

  iVergleich := IsValueGreaterLowerEqual(pDay.wMonth, pStd.wMonth, pDT.wMonth);
  if (iVergleich in [0, 4]) then Result := True
  else if (iVergleich = 2) then Result := False;

  if (iVergleich = 1) then begin
    dt := CalcDayLightChange(pDT.wYear, pDay);
    Result := (dtDateTime < dt);
  end
  else if (iVergleich = 3) then begin
    dt := CalcDayLightChange(pDT.wYear, pStd);
    Result := (dtDateTime >= dt);
  end;
end;

{----------------------------------------------------------------}
function WFileDateTime_System (const FileName: string): TDateTime;
{----------------------------------------------------------------}
{ liefert vollständigen System-Zeitstempel des Betriebssystems von einer Datei
  (incl. ms; ab 21.01.2019 mit lokaler Zeit, d.h. mit eingerechneter lokaler
   Zeitzone und aktuell vorliegendem Sommer-/Winterzeit-Offset (bisher ohne
   eingerechnete Zeitzone und Sommer-/Winterzeit)
  -> Delphi-Funktion 'FileAge' liefert keine ms !
  -> Achtung: Die Funktion liefert nicht zwingend den Zeitstempel wie er im
     Windows-Explorer angezeigt wird ! Die Datei-Zeitstempel werden mit
     AKTUELL vorliegender Zeitzone und Sommer-/Winterzeit-Offset beaufschlagt.
     D.h. z.B. im Winter liegende Datei-Zeitstempel werden bei aktuell
     vorliegender Sommerzeit mit dem Sommerzeit-Offset beaufschlagt. }
var
  Handle: THandle;
  FindData: TWin32FindData;
  SystemTime: TSystemTime;
  ModifiedTime: TFileTime;

begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      if FileTimeToLocalFileTime(FindData.ftLastWriteTime, ModifiedTime) then begin  // 21.01.2019, WW
        if FileTimeToSystemTime(ModifiedTime, SystemTime) then begin
          Result:=SystemTimeToDateTime(SystemTime);
          exit;
        end;
      end;
    end;
  end;
  Result := -1;
end;

{------------------------------------------------------------------}
function WFileTimeToDateTime (const FileTime: TFileTime): TDateTime;
{------------------------------------------------------------------}
{ Wandelt einen TFileTime-Zeitstempel in die TDateTime-Struktur }
var
  syst: TSystemTime;

begin
  FileTimeToSystemTime(FileTime, syst);
  Result:=SystemTimeToDateTime(syst);
end;

{--------------------------}
function WNowUTC: TDateTime;
{--------------------------}
{ Liefert das aktuelle Datum und die aktuelle Uhrzeit als UTC-Zeit }
var
  sysUTC: TSystemTime;

begin
  GetSystemTime(sysUTC);
  Result:=SystemTimeToDateTime(sysUTC);
end;

{----------------------------------------------------------------}
function StringMaskToDateTime(
  const sString: string; const sMask: string): TDateTime;
{----------------------------------------------------------------}

  function GetMaskedInteger(cMask: char): SmallInt;
  var
    i, iPos, iLenght: integer;
  begin
    iPos := Pos(UpperCase(cMask), UpperCase(sMask));
    if (iPos > 0) then begin
      iLenght := 0;
      for i := iPos to Length(sMask) do begin
        if (UpperCase(sMask[i]) = UpperCase(cMask))
        then Inc(iLenght)
        else Break;
      end;
      Result := StrToIntDef(Copy(sString, iPos, iLenght), 0);
    end
    else Result := 0;
  end;

var
  y, m, d, h, n, s, ms : word;
begin
  try
    y := GetMaskedInteger('y');
    m := GetMaskedInteger('m');
    d := GetMaskedInteger('d');
    h := GetMaskedInteger('h');
    n := GetMaskedInteger('n');
    s := GetMaskedInteger('s');
    ms := GetMaskedInteger('z');

    if (y > 0) and (y < 100) then y := y + 2000;
    if ((y >= 1899) and (m in [1..12]) and (d in [1..31]))
    then Result := EncodeDate(y, m, d)
    else Result := 0;

    if ((h in [0..23]) and (n in [0..59]) and (s in [0..59]))
    then Result := Result + EncodeTime(h, n, s, ms);
  except
    Result := 0;
  end;
end;

{----------------------------------------------------------------------------}
function UTCTimezoneStrToDateTime (sTz: string; var dtTz: TDateTime): boolean;
{----------------------------------------------------------------------------}
{ wandelt String mit UTC-Zeitzonen-Differenz in TDateTime;
  Übergabe: UTC-Zeitzone als String (z.B. +01)
  Rückgabe: UTC-Zeitzone als TDateTime
  Ergebnis: true, wenn sTz gültige UTC-Zeitzone repräsentiert }
begin
  Result:=false;
  dtTz:=0;
  try
    if length (sTz) <> 3 then exit;  // Vorzeichen und 2 Zeichen für Stunde
    if (sTz [1] <> '+') AND (sTz [1] <> '-') then exit;  // ungültiges Vorzeichen
    dtTz:=EncodeTime (StrToInt (Copy (sTz, 2, 2)), 0, 0, 0);
    if sTz [1] = '-' then
      dtTz:=dtTz * (-1);
    Result:=true;
  except
    dtTz:=0;
    Result:=false;
  end;
end;

{-------------------------------------------------------------------------}
function DateTimeToUTCStr (dtDatumZeit: TDateTime; bIsSZ: boolean): string;
{-------------------------------------------------------------------------}
{ wandelt TDateTime in String mit UTC-Zeit;
  Übergaben: Datum/Zeit als TDateTime
             Flag 'bIsSZ' (true: übergebene Zeit ist Sommerzeit; false: Winterzeit)
  Ergebnis: UTC-Zeit-String }
var
  dtBuf: TDateTime;
  iOffsetHour: integer;

begin
  if bIsSZ then
    iOffsetHour:=2
  else
    iOffsetHour:=1;
  dtBuf:=dtDatumZeit - EncodeTime (iOffsetHour, 0, 0, 0);
  Result:=FormatDateTime('yyyymmddhhnnss', dtBuf) + '+' +
          Format ('%.2d', [iOffsetHour]);
end;

// 12.07.2010  WN
// -----------------------------------------------------------------------------
// wandelt String mit UTC-Zeit in TDateTime
// Parameter: Maske Datum-/Zeit-String ('yymmddhhnnss')
//            Datum/Zeit als UTC-String
// Ergebnis: TDateTime
// -----------------------------------------------------------------------------
function UTCStrToDateTime (sMask, sZeit: String): TDateTime;
// -----------------------------------------------------------------------------
var
  dtBuf: TDateTime;
  sUTC, sBuf : String;
begin
  sBuf := '';
  Result := -1;

  sUTC := sZeit;

  if (F_TotalChars (sUTC, '+') = 1) then
  begin
    sBuf  := F_Zerlegen(sUTC, '+');
    dtBuf := StringMaskToDateTime(sUTC, sMask);
  end else if (F_TotalChars (sUTC, '-') = 1) then
  begin
    sBuf  := F_Zerlegen(sUTC, '-');
    dtBuf := StringMaskToDateTime(sUTC, sMask);
  end;

  if UTCTimezoneStrToDateTime (sUTC, dtBuf) then
    Result := dtBuf;
end;

// -----------------------------------------------------------------------------
// Ermittlung der letzten vollen Stunde des übergebenen Zeitstempels
// Parameter: Referenzdatum, zu dem die letzte volle ermittelt werden soll
// Ergebnis: Datum der letzten vollen Stunde
// -----------------------------------------------------------------------------
function GetLetzteVolleStunde(dt: TDateTime): TDateTime;
// -----------------------------------------------------------------------------
var
  wHour, wMin, wSec, wMSec: word;
  dDate, dTime: Double;
begin
  dDate:=Trunc(dt);  // Datum
  dTime:=Frac(dt);   // Zeit
  DecodeTime(dTime, wHour, wMin, wSec, wMSec);
  wMin:=0;
  wSec:=0;
  wMSec:=0;
  dTime:=EncodeTime(wHour, wMin, wSec, wMSec);  // volle Stunde
  Result:=dDate + dTime;                        // Datum + volle Stunde
end;

// -----------------------------------------------------------------------------
// Ermittlung der nächsten vollen Stunde des übergebenen Zeitstempels
// Parameter: Referenzdatum, zu dem die nächste volle ermittelt werden soll
// Ergebnis: Datum der nächsten vollen Stunde
// -----------------------------------------------------------------------------
function GetNaechsteVolleStunde(dt: TDateTime): TDateTime;
// -----------------------------------------------------------------------------
begin
  Result:=IncHour(GetLetzteVolleStunde(dt));
end;

// 24.03.2010  WN
// -----------------------------------------------------------------------------
// Plausibilität eines Datums prüfen
// -> analog V:\delphi32sql\abruf\afmrg32\Mlgzaufb.pas
// Datum muß folgende Kriterien erfüllen:
//    1. Es darf nicht mehr als zwei Stunden in der Zukunft liegen
//    2. Es darf nicht mehr als drei Jahre in der Vergangenheit liegen
// -----------------------------------------------------------------------------
function IsDatumPlausibel (dt: TDateTime): Boolean;
// -----------------------------------------------------------------------------
var
  dtNow, dtZukunft, dtVergangenheit : TDateTime;
begin
  Result := False;

  dtNow := Now;                     // eingestellte PC-Zeit
  dtZukunft := dtNow + 2/24;        // Zukunft: PC-Zeit + 2 Stunden
  dtVergangenheit := dtNow - 1095;  // Vergangenheit: PC-Zeit - 3 Jahre

  // Datum darf nicht mehr als zwei Stunden in der Zukunft liegen und
  // Datum darf nicht mehr als drei Jahre in der Vergangenheit liegen
  if (CmpDateTime(dt, dtZukunft) <> 1) and
     (CmpDateTime(dt, dtVergangenheit) <> -1) then
    Result := True;
end;

// 21.01.2011  WN
// -----------------------------------------------------------------------------
// Funktion "GetHour" aus Langzeit-Komponente
// Ermittelt wird die gerundete volle Stunde (abhängig von Minuten)
// Parameter: Datum
// Ergebnis: gerundete volle Stunde
// -----------------------------------------------------------------------------
function GetVolleStunde(dt: TDateTime): TDateTime;
// -----------------------------------------------------------------------------
var
  h,n,s,ms : Word;
  iInc     : Byte;
begin
  DecodeTime(dt, h, n, s, ms);
  if (n < 30) then
    iInc := 0
  else
    iInc := 1;
   Result := Trunc(dt) + IncHour(EncodeTime(h, 0, 0, 0), iInc);
end;

// 21.01.2011  WN
// -----------------------------------------------------------------------------
// Funktion "IsHour" aus Langzeit-Komponente
// Ermittelt wird, ob es sich um eine volle Stunde handelt:
// unter Berücksichtigung einer Stundenwechsel-Unschärfe
// Parameter: Datum, Unschärfe bzgl. Stundenwechsel
// Ergebnis: (T) = volle Stunde
// -----------------------------------------------------------------------------
function IsVolleStunde(dt: TDateTime; iTimeFuzzy: Word): Boolean;
// -----------------------------------------------------------------------------
begin
  Result := SameValue(dt, GetVolleStunde(dt),
                      EncodeTime(0, 0, iTimeFuzzy div 1000, iTimeFuzzy mod 1000));
end;

// 08.04.2014  WN
// -----------------------------------------------------------------------------
// Wurde zu diesem Datum-Zeit-Stempel die Winterzeit auf Sommerzeit umgestellt?
// Ergebnis: (T) = Winter-Sommerzeit-Umstellung (letzter Sonntag im März, um 3:00 Uhr)
// -----------------------------------------------------------------------------
function IsWSZeitUmstellung(dt: TDateTime): Boolean;
// -----------------------------------------------------------------------------
var
  y,m,d,h,n,s,ms: Word;
  yLast,mLast,dLast: Word;
  dtLastSundayMarch: TDateTime;
begin
  DecodeDateTime(dt, y, m, d, h, n, s, ms);  // übergebenes Datum
  dtLastSundayMarch := EncodeDate(y, 3, 31); // letzter Tag im März
  dtLastSundayMarch := (dtLastSundayMarch - DayOfWeek(dtLastSundayMarch))+1;
  DecodeDate(dtLastSundayMarch, yLast, mLast, dLast);
  Result := (mLast = m) and (dLast = d) and (h = 3);
end;

// -----------------------------------------------------------------------------
// Wurde zu diesem Datum-Zeit-Stempel die Sommerzeit auf Winterzeit umgestellt?
// Ergebnis: (T) = Sommer-Winterzeit-Umstellung (letzter Sonntag im Oktober, um 2:00 Uhr)
// -----------------------------------------------------------------------------
function IsSWZeitUmstellung(dt: TDateTime): Boolean;
// -----------------------------------------------------------------------------
var
  y,m,d,h,n,s,ms: Word;
  yLast,mLast,dLast: Word;
  dtLastSundayOctober: TDateTime;
begin
  DecodeDateTime(dt, y, m, d, h, n, s, ms);  // übergebenes Datum
  dtLastSundayOctober := EncodeDate(y, 10, 31); // letzter Tag im Oktober
  dtLastSundayOctober := (dtLastSundayOctober - DayOfWeek(dtLastSundayOctober))+1;
  DecodeDate(dtLastSundayOctober, yLast, mLast, dLast);
  Result := (mLast = m) and (dLast = d) and (h = 2);
end;

End.
