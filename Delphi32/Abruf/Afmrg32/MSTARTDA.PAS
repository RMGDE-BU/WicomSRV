{*******************************************************************************************}
{* Unit: Startdatum f�r LGZ-Datenabruf ermitteln (Stundenwerte, Tagess�tze, Pr�fungss�tze) *}
{* 14.12.98 WW                                                                             *}
{*******************************************************************************************}
unit MStartDa;

interface

uses
  Classes, SysUtils, T_Tools, T_Zeit, MSysDat, MDBSta, MDBMrg, WSysCon, MJournal,
  my_utils, MSysCon, MLetztLGZ;


procedure HoleStartdatum_Mess (JournalId: integer; RufStammdaten: TRufStammdaten;
                               var LetztDatum: DateRec; var LetztZeit: TimeRec);
procedure HoleStartdatum_Tags (RufStammdaten: TRufStammdaten;
                               var LetztDatum: DateRec; var LetztZeit: TimeRec);
procedure HoleStartDatum_Prfs (RufStammdaten: TRufStammdaten;
                               var LetztDatum: DateRec; var LetztZeit: TimeRec);

implementation

{-------------------------------------------------------------------------------}
procedure HoleStartdatum_Mess (JournalId: integer; RufStammdaten: TRufStammdaten;
                               var LetztDatum: DateRec; var LetztZeit: TimeRec);
{-------------------------------------------------------------------------------}
{ liefert neues Startdatum/-zeit f�r Me�werteabruf; protokolliert den Soll-Zeitbereich
  �bergabe: JournalId
            RufStammdaten
  R�ckgabe: LetztDatum (neues Startdatum)
            LetztZeit (neue Startzeit) }
var
  MrgDefData: TMrgDefDataDb;
  Hour, Min, Sec, MSec: word;
  SollVon, SollBis: TDateTime;
  SoWiZeitOffset: TDateTime;

begin
  if HoleLetztDatum_Mess (RufStammdaten.Kennung, V_maxKanalzahl, LetztDatum, LetztZeit) = 0 then begin
    { Soll-Datenzeitbereich nach MZBDATEN.DB schreiben (ger�teunabh�ngig): }
    SollVon:=EncodeDate (LetztDatum.Year, LetztDatum.Month, LetztDatum.Day) +
             EncodeTime (LetztZeit.Hour, 0, 0, 0) +
             EncodeTime (1, 0, 0, 0);                              { ab der n�chsten Stunde }
    SollBis:=Now;
    DecodeTime (SollBis, Hour, Min, Sec, MSec);
    SollBis:=Int (SollBis) + EncodeTime (Hour, 0, 0, 0);               { akt. Datum, Stunde }
    SoWiZeitOffset:=GetWZ_SZ_OffsetNowFromDB (Rufstammdaten.MrgId, C_GerArtMrg);
    SollBis:=SollBis + SoWiZeitOffset;    { Sommer/Winterzeit in MRG und PC ber�cksichtigen }
    WriteDatenzeitbereich_Soll (JournalId, SollVon, SollBis);

    if GetMrgDefDataDb (RufStammdaten.MrgTyp, MrgDefData) then begin
      if not MrgDefData.Jahreswechsel then begin
        { Wenn der letzte Eintrag in Langzeitdaten vom 31.12 23:00 Uhr ist, dann beginnt der
          Abruf beim 1.1. 00:00 Uhr, da das MRG 1029 den Jahreswechsel innerhalb der
          Abfrage nicht verarbeiten kann }
        if (LetztDatum.month = 12) and (LetztDatum.day = 31) and
           (LetztZeit.hour = 23) then begin
          LetztDatum.Year  := LetztDatum.Year + 1;
          LetztDatum.Month := 1;
          LetztDatum.Day   := 1;
          LetztZeit.Hour   := 0;
          LetztZeit.Min    := 0;
          LetztZeit.Sec    := 0;
          LetztZeit.HSec   := 0;
          exit;
        end;
      end;
    end;
    if (LetztZeit.Hour < RufStammdaten.TagesEnde) then
      P_Vortag (LetztDatum);
  end
  else begin
    DecodeDate (Systemdaten.DatumErsteDaten,
                word (LetztDatum.Year), word (LetztDatum.Month), word (LetztDatum.Day));

    { Soll-Datenzeitbereich nach MZBDATEN.DB schreiben (ger�teunabh�ngig): }
    SollBis:=Now;
    DecodeTime (SollBis, Hour, Min, Sec, MSec);
    SollBis:=Int (SollBis) + EncodeTime (Hour, 0, 0, 0);               { akt. Datum, Stunde }
    SoWiZeitOffset:=GetWZ_SZ_OffsetNowFromDB (Rufstammdaten.MrgId, C_GerArtMrg);
    SollBis:=SollBis + SoWiZeitOffset;    { Sommer/Winterzeit in MRG und PC ber�cksichtigen }
    WriteDatenzeitbereich_Soll (JournalId, 0, SollBis);                       { nur SollBis }
  end;

  LetztZeit.Hour := 0;
  LetztZeit.Min  := 0;
  LetztZeit.Sec  := 0;
  LetztZeit.HSec := 0;
end;

{------------------------------------------------------------------------------}
procedure HoleStartdatum_Tags (RufStammdaten: TRufStammdaten;
                               var LetztDatum: DateRec; var LetztZeit: TimeRec);
{------------------------------------------------------------------------------}
{ liefert neues Startdatum/-zeit f�r Tagessatzabruf
  �bergabe: RufStammdaten
  R�ckgabe: LetztDatum (neues Startdatum)
            LetztZeit (neue Startzeit) }
begin
  if HoleLetztDatum_Tags (RufStammdaten.Kennung, V_maxKanalzahl, LetztDatum) = 0 then
    P_Nachtag (LetztDatum)
  else
    DecodeDate (Systemdaten.DatumErsteDaten,
                word (LetztDatum.Year), word (LetztDatum.Month), word (LetztDatum.Day));
  LetztZeit.Hour := 0;
  LetztZeit.Min  := 0;
  LetztZeit.Sec  := 0;
  LetztZeit.HSec := 0;
end;

{------------------------------------------------------------------------------}
procedure HoleStartDatum_Prfs (RufStammdaten: TRufStammdaten;
                               var LetztDatum: DateRec; var LetztZeit: TimeRec);
{------------------------------------------------------------------------------}
{ liefert neues Startdatum/-zeit f�r Pr�fungss�tzeabruf
  �bergabe: RufStammdaten
  R�ckgabe: LetztDatum (neues Startdatum)
            LetztZeit (neue Startzeit) }
begin
  if HoleLetztDatum_Prfs (RufStammdaten.Kennung, LetztDatum) <> 0 then
    DecodeDate (Systemdaten.DatumErsteDaten,
                word (LetztDatum.Year), word (LetztDatum.Month), word (LetztDatum.Day));
  LetztZeit.Hour := 0;
  LetztZeit.Min  := 0;
  LetztZeit.Sec  := 0;
  LetztZeit.HSec := 0;
end;

end.