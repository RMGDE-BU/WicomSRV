{******************************************************************************}
{* Unit: Systemdaten für MRG-Abruf                                            *}
{* 26.01.1999 WW                                                              *}
{******************************************************************************}
unit MSysDat;

interface

uses SysUtils, WSysDat, WSysCon;

var
  Systemdaten: TMrgSystemdaten = (
    Vorwahl: C_SysVorwahl;
    Wartezeit: (C_SysWartezeit1,
                C_SysWartezeit2,
                C_SysWartezeit3,
                C_SysWartezeit4,
                C_SysWartezeit5,
                C_SysWartezeit6);
    MaxAnrufversuche_Zentrale: C_SysMaxAnrufversuche_Zentrale;
    MaxAnrufversuche_Station: C_SysMaxAnrufversuche_Station;
    DatumErsteDaten: 0;
    ZeitSyncMin: C_SysMrgZeitSyncMin;
    ZeitSyncMax: C_SysMrgZeitSyncMax;
    GrMeldPuffer: C_SysMrgGrMeldPuffer;
    GrJournalPuffer: C_SysGrJournalPuffer;
    RufDatenTypen: C_IsMeldungen;
    ErrorBeep: false;
    StationDeaktivierung: C_EinstStationDeakt
  );

function GetMrgAbrufSystemDaten(FilePath: string): TMrgSystemDaten;
function GetNextWiederholung (Versuche_bisher: integer; var NextWiederholung: TDateTime): boolean;

implementation

{-----------------------------------------------------------------}
function GetMrgAbrufSystemDaten(FilePath: string): TMrgSystemDaten;
{-----------------------------------------------------------------}
{ Gibt die für MRG-Abrufe benötigten Systemdaten in einem Record zurück;
  Übergabe: Pfad zur Ini
  Ergebnis: MRG-Systemdaten }
var
  se: TSystemEinstellungen;
begin
  se:= TSystemEinstellungen.Create(FilePath);
  try
    result:= se.MrgSysDaten;
  finally
    se.free;
  end;
end;

{------------------------------------------------------------------------------------------------}
function GetNextWiederholung (Versuche_bisher: integer; var NextWiederholung: TDateTime): boolean;
{------------------------------------------------------------------------------------------------}
{ liefert Zeitpunkt für Abrufwiederholung;
  Übergabe: bisherige Versuche
  Rückgabe: Zeitpunkt für Wiederholung
  Ergebnis: false, wenn max. Anzahl der Versuche erreicht }
var
  AddMinuten: word;
begin
  Result:=false;
  NextWiederholung:=0;
  if Versuche_bisher < Systemdaten.MaxAnrufversuche_Zentrale then begin
    { max. Anzahl von Wiederholungen noch nicht erreicht }
    NextWiederholung:=Now;
    AddMinuten:=0;
    if (Versuche_bisher >= Low (Systemdaten.Wartezeit)) AND
       (Versuche_bisher <= High (Systemdaten.Wartezeit)) then
      { Wartezeit nach 1. ..  6. Versuch }
      AddMinuten:=Systemdaten.Wartezeit [Versuche_bisher]
    else
      { Wartezeit ab dem 6. Versuch }
      if Versuche_bisher > High (Systemdaten.Wartezeit) then
        AddMinuten:=Systemdaten.Wartezeit [High (Systemdaten.Wartezeit)];

    if AddMinuten > 59 then AddMinuten:=59;
    NextWiederholung:=NextWiederholung + EncodeTime (0, AddMinuten, 0, 0);
    Result:=true;
  end;
end;

end.
