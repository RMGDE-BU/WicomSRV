unit UpUmfo;

interface

uses
  t_tools, t_zeit, LGZType;

   procedure next_msp(var datum : dateRec; var zeit : timeRec);
   procedure next_satz(datum : dateRec; zeit : TimeRec;var ns : LGZSRec);
   FUNCTION Zeiger(EinDatum,AusDatum:DateRec;EinZeit,AusZeit:TimeRec):LongInt;
   function TagDiff(EinDatum, AusDatum: DateRec): longint;

implementation

Const
  AufzgTakt = 60;   { min }

{-------------------------------------------------------------------- }
{Berechnen von Zeit und Datum der n„chsten Meáperiode }

procedure next_msp;

begin
  inc(zeit.min,Aufzgtakt);
  if zeit.min > 59 then begin  { n„chste Stunde }
    zeit.min:=0;
    if zeit.hour < 23 then begin
     inc(zeit.hour);
    end

    else begin            { n„chster Tag }
      zeit.hour:=0;
      if datum.day < F_LetztTag(Datum) then begin
        inc(datum.day);
      end

      else begin          { n„chster Monat }
        datum.day:=1;
        if datum.month <12 then begin
          inc(datum.month);
        end

        else begin        { n„chstes Jahr }
          datum.month:=1;
          inc(datum.year);
        end; { jahr }
      end; { monat }
    end; {tag }
  end; { stunde}

end;


{-------------------------------------------------------------------- }
{ Erzeugen und initialisieren des Folgesatzes }

procedure next_satz;

var i : shortint;

begin
  ns.datum:=datum;
  ns.zeit:=zeit;
  ns.Satzstatus:=$80;        { Bit 7 .. Satz hinzugefgt }
  next_msp(ns.datum,ns.zeit);
  for i:=1 to C_MaxKanalZahl do begin
    ns.kanal[i].Kanalstatus:=$80;    { Bit 7 .. Kanal fehlt }
    ns.kanal[i].wert:=0;             { Wert wird auf Null gesetzt }
  end;
end;



{-------------------------------------------------------------------- }
{ Berechnen eines Zeigers vom Ein-Zeitpunkt zum Aus-Zeitpunkt }

FUNCTION Zeiger;

VAR Zwisch  : Integer;
    Z       : LongInt;
    Multipl : Byte;
    hilf    : daterec;

BEGIN
 Z:=0;

 IF CmpDate(EinDatum,AusDatum)+CmpTime(EinZeit,AusZeit)>0
 THEN BEGIN   { Aus < Ein }
   {Abfangen, wenn Zeitraum zu groá wird ?!}
   for Zwisch:=AusDatum.Year to (EinDatum.Year-1) DO BEGIN
     IF Zwisch mod 4=0 THEN inc(Z,366) ELSE inc(Z,365);
   END;

   For Zwisch:=1 to AusDatum.Month-1 DO BEGIN
    hilf.month:=zwisch;
    hilf.year:=ausdatum.year;
    Dec(Z,F_LetztTag(hilf));
   END;

   For Zwisch:=1 to Eindatum.Month-1 DO BEGIN
    hilf.month:=zwisch;
    hilf.year:=eindatum.year;
    Inc(Z,F_LetztTag(hilf));
   END;

   Dec(Z,AusDatum.Day);
   Inc(Z,EinDatum.Day); {Tage}

   Multipl:=60 div AufzgTakt;

   Z:=Z*24*Multipl; {Jetzt Zeiger in Stunden}

   Dec(Z,AusZeit.Hour*Multipl);
   Inc(Z,EinZeit.Hour*Multipl);

   Dec(Z,AusZeit.Min div AufzgTakt);
   Inc(Z,EinZeit.Min div AufzgTakt);

   Zeiger:=-Z;

 END
 else begin { Aus >= Ein }

   {Abfangen, wenn Zeitraum zu groá wird ?!}
   for Zwisch:=EinDatum.Year to (AusDatum.Year-1) DO BEGIN
     IF Zwisch mod 4=0 THEN inc(Z,366) ELSE inc(Z,365);
   END;

   For Zwisch:=1 to EinDatum.Month-1 DO BEGIN
    hilf.month:=zwisch;
    hilf.year:=eindatum.year;
    Dec(Z,F_LetztTag(hilf));
   END;

   For Zwisch:=1 to Ausdatum.Month-1 DO BEGIN
    hilf.month:=zwisch;
    hilf.year:=ausdatum.year;
    Inc(Z,F_LetztTag(hilf));
   END;

   Dec(Z,EinDatum.Day);
   Inc(Z,AusDatum.Day); {Tage}

   Multipl:=60 div AufzgTakt;

   Z:=Z*24*Multipl; {Jetzt Zeiger in Stunden}

   Dec(Z,EinZeit.Hour*Multipl);
   Inc(Z,AusZeit.Hour*Multipl);

   Dec(Z,EinZeit.Min div AufzgTakt);
   Inc(Z,AusZeit.Min div AufzgTakt);

   Zeiger:=Z;
 end;
END;


function TagDiff(EinDatum, AusDatum: DateRec):longint;
{ berechnet die Anzahl der Tage zwischen Ein- und AusDatum }
{ Aus > Ein, sonst -1 }

VAR Zwisch  : Integer;
    Z       : LongInt;
    hilf    : daterec;

BEGIN
 Z:=0;

 IF CmpDate(EinDatum,AusDatum)<=0
 THEN BEGIN   { Aus > Ein }
   {Abfangen, wenn Zeitraum zu groá wird ?!}
   for Zwisch:=EinDatum.Year to (AusDatum.Year-1) DO BEGIN
     IF Zwisch mod 4=0 THEN inc(Z,366) ELSE inc(Z,365);
   END;

   For Zwisch:=1 to EinDatum.Month-1 DO BEGIN
    hilf.month:=zwisch;
    hilf.year:=eindatum.year;
    Dec(Z,F_LetztTag(hilf));
   END;

   For Zwisch:=1 to Ausdatum.Month-1 DO BEGIN
    hilf.month:=zwisch;
    hilf.year:=ausdatum.year;
    Inc(Z,F_LetztTag(hilf));
   END;

   Dec(Z,EinDatum.Day);
   Inc(Z,AusDatum.Day); {Tage}
   TagDiff:=Z;
 end
 else
   TagDiff:=-1;
end;

end.
