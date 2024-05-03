{------------------------------------------------------------------------------}
{ 20.01.1999 WW, GD; Unit für Systemdaten - Zugriff                            }
{ 04.02.1999 GD;     Erweitert um Aktualisierungs-Zyklus                       }
{ 21.04.2015 WN;     Offset für Reporterstellung                               }
{------------------------------------------------------------------------------}
unit WSysDat;

interface

uses
  SysUtils, T_Tools, IniFiles, WSysCon;

const
  szLen_Vorwahl = 11;
  C_SysDat_IniFileName = 'ALM32.INI';

  { Standardvorbelegungen (allgemein) }
  C_EinstIsDebug = False;
  C_EinstStationDeakt = False; { Station bleibt nach letztem Anrufversuch in
                                 der Auftragsliste aktiv }

  C_SysVorwahl = 't';
  C_SysWartezeit1 = 0;   { min }
  C_SysWartezeit2 = 0;   { min }
  C_SysWartezeit3 = 2;   { min }
  C_SysWartezeit4 = 2;   { min }
  C_SysWartezeit5 = 6;   { min }
  C_SysWartezeit6 = 10;  { min }
  C_SysGrJournalPuffer = 60;
  C_SysMaxAnrufversuche_Zentrale = 3;
  C_SysMaxAnrufversuche_Station = 3;

  { Standardvorbelegungen (MRG) }
  C_SysMrgZeitSyncMin = 30;   { s }
  C_SysMrgZeitSyncMax = 60;   { s }
  C_SysMrgGrMeldPuffer = 50;
  C_SysMrg_RufEDatenTypen = C_IsMeldungen; { Meldungen }
  C_SysMrg_MinutenOffset = 0; { min }

  { Standardvorbelegungen (LAKS) }
  C_SysLaks_LaksMomDatenTypen = C_IsMesswerte+C_IsMeldungen+C_IsEBandaenderungen;
  C_SysLaks_MinutenOffset = 0; { min }

  { Standardvorbelegungen (DSfG) }
  C_SysDSfG_MinutenOffset = 0; { min }
  C_SysDSfG_RufEDatenTypen = C_IsLogbuecher; { Logbücher }
  C_SysDSfGGrLogbuchPuffer = 50;

  { Standardvorbelegungen (Report) }
  C_SysReport_MinutenOffset = 0; { min }  // 21.04.2015  WN
  
type
  { Record für Systemdaten }
  TArrayInt1_6 = array[1..6] of integer;

  TMrgSystemDaten = record
    Vorwahl: string [szLen_Vorwahl];
    Wartezeit: TArrayInt1_6;
    MaxAnrufversuche_Zentrale: integer;
    MaxAnrufversuche_Station: integer;
    DatumErsteDaten: TDateTime;
    ZeitSyncMin: integer;
    ZeitSyncMax: integer;
    GrMeldPuffer: integer;
    GrJournalPuffer: integer;
    RufDatenTypen: integer;
    MinutenOffset: integer;
    ErrorBeep: boolean;
    StationDeaktivierung: boolean;
  end;

  TLAKSSystemDaten = record
    Vorwahl: string [szLen_Vorwahl];
    Wartezeit: TArrayInt1_6;
    MaxAnrufversuche_Zentrale: integer;
    MaxAnrufversuche_Station: integer;
    DatumErsteDaten: TDateTime;
    GrJournalPuffer: integer;
    MomAbrufDatenTypen: integer;
    MomAbrufMPE: boolean;
    MinutenOffset: integer;
  end;

  TDSfGSystemDaten = record
    Vorwahl: string [szLen_Vorwahl];
    Wartezeit: TArrayInt1_6;
    MaxAnrufversuche_Zentrale: integer;
    MaxAnrufversuche_Station: integer;
    GrJournalPuffer: integer;
    MinutenOffset: integer;
    AnzahlErsteDaten: integer;
    ErrorBeep: boolean;
    RufDatenTypen: integer;
    GrLogbuchPuffer: integer;
    StationDeaktivierung: boolean;
  end;

  TSystemEinstellungen = class(TIniFile)
    constructor create(FilePath: string);
    destructor destroy; override;
  private
    function GetIsDebug: boolean;
    function GetVorwahl: string;
    procedure SetVorwahl(Value: string);
    function GetWarteZeit: TArrayInt1_6;
    procedure SetWarteZeit(Value: TArrayInt1_6);
    function GetMaxAnrufZentrale: integer;
    procedure SetMaxAnrufZentrale(Value: integer);
    function GetMaxAnrufStation: integer;
    procedure SetMaxAnrufStation(Value: integer);
    function GetRufDeaktivierung: boolean;
    procedure SetRufDeaktivierung(Value: boolean);
    function GetMrgDatumErsteDaten: double;
    procedure SetMrgDatumErsteDaten(Value: double);
    function GetZeitSyncMin: integer;
    procedure SetZeitSyncMin(Value: integer);
    function GetZeitSyncMax: integer;
    procedure SetZeitSyncMax(Value: integer);
    function GetStationDeaktivierung: boolean;
    procedure SetStationDeaktivierung(Value: boolean);

    function GetMrgSystemDaten: TMrgSystemdaten;
    function GetGroesseMeldPuffer: integer;
    procedure SetGroesseMeldPuffer(Value: integer);
    function GetGroesseJournalPuffer: integer;
    procedure SetGroesseJournalPuffer(Value: integer);
    function GetErrorBeep: boolean;
    procedure SetErrorBeep(Value: boolean);
    function GetDatentypenRufentgegennahme: integer;
    procedure SetDatentypenRufentgegennahme(Value: integer);
    function GetMRGMinOffset: integer;
    procedure SetMRGMinOffset(Value: integer);

    function GetLaksSystemDaten: TLAKSSystemDaten;
    function GetLaksDatumErsteDaten: double;
    procedure SetLaksDatumErsteDaten(Value: double);
    function GetLaksAbrufMPE: boolean;
    procedure SetLaksAbrufMPE(Value: boolean);
    function GetLaksMinOffset: integer;
    procedure SetLaksMinOffset(Value: integer);
    function GetDatentypenLaksMomentan: integer;
    procedure SetDatentypenLaksMomentan(Value: integer);

    function GetDSfGSystemDaten: TDSfGSystemDaten;
    function GetDSfGAnzErsteDaten: integer;
    procedure SetDSfGAnzErsteDaten(Value: integer);
    function GetDSfGDatentypenRufentgegennahme: integer;
    procedure SetDSfGDatentypenRufentgegennahme(Value: integer);
    function GetDSfGMinOffset: integer;
    procedure SetDSfGMinOffset(Value: integer);
    function GetDSfGGroesseLogbuchPuffer: integer;
    procedure SetDSfGGroesseLogbuchPuffer(Value: integer);
    function GetAktZyklus: integer;          // 04.02.2002
    procedure SetAktZyklus (Value: integer); // 04.02.2002
    
    // 21.04.2015  WN
    function GetReportMinOffset: integer;
    procedure SetReportMinOffset(Value: integer);
  public
    { aktueller Wert für Pollingzyklus in der  Auftragsliste in ms }
    property AktZyklus: Integer read GetAktZyklus Write SetAktZyklus; // 04.02.2002
    property isDebug: boolean read GetIsDebug;
    property Vorwahl: string read GetVorwahl write SetVorwahl;
    property WarteZeit: TArrayInt1_6 read GetWarteZeit write SetWarteZeit;
    property MaxAnrufZentrale: integer read GetMaxAnrufZentrale write SetMaxAnrufZentrale;
    property MaxAnrufStation: integer read GetMaxAnrufStation write SetMaxAnrufStation;
    property RufDeaktivierung: boolean read GetRufDeaktivierung write SetRufDeaktivierung;
    property MrgErsteDaten: double read GetMrgDatumErsteDaten write SetMrgDatumErsteDaten;
    property ZeitSyncMin: integer read GetZeitSyncMin write SetZeitSyncMin;
    property ZeitSyncMax: integer read GetZeitSyncMax write SetZeitSyncMax;
    property StationDeaktivierung: boolean read GetStationDeaktivierung write SetStationDeaktivierung;
    property MrgSysDaten: TMrgSystemdaten read GetMrgSystemDaten;
    property GroesseMeldPuffer: integer read GetGroesseMeldPuffer write SetGroesseMeldPuffer;
    property GroesseJournalPuffer: integer read GetGroesseJournalPuffer write SetGroesseJournalPuffer;
    property ErrorBeep: boolean read GetErrorBeep write SetErrorBeep;
    property DatentypenRufentgegennahme: integer read GetDatentypenRufentgegennahme
                                                 write SetDatentypenRufentgegennahme;
    property MRGMinOffset: integer read GetMRGMinOffset write SetMRGMinOffset;
    property LaksSysDaten: TLaksSystemdaten read GetLaksSystemDaten;
    property DatentypenLaksMomentan: integer read GetDatentypenLaksMomentan
                                                 write SetDatentypenLaksMomentan;
    property LaksErsteDaten: double read GetLaksDatumErsteDaten write SetLaksDatumErsteDaten;
    property LaksAbrufMPE: boolean read GetLaksAbrufMPE write SetLaksAbrufMPE;
    property LaksMinOffset: integer read GetLaksMinOffset write SetLaksMinOffset;

    property DSfGSysDaten: TDSfGSystemdaten read GetDSfGSystemDaten;
    property DSfGAnzErsteDaten: integer read GetDSfGAnzErsteDaten write SetDSfGAnzErsteDaten;
    property DSfGDatentypenRufentgegennahme: integer read GetDSfGDatentypenRufentgegennahme
                                             write SetDSfGDatentypenRufentgegennahme;
    property DSfGMinOffset: integer read GetDSfGMinOffset write SetDSfGMinOffset;
    property DSfGGroesseLogbuchPuffer: integer read GetDSfGGroesseLogbuchPuffer
                                               write SetDSfGGroesseLogbuchPuffer;
    property ReportMinOffset: integer read GetReportMinOffset write SetReportMinOffset;  // 21.04.2015  WN
  end;


implementation

const
  CSectionProgram = 'ALM';        // 04.02.2002
  CIdentAktZyklus = 'AktZyklus';

  C_Section_Einstellungen    = 'SETTINGS';

  C_Ident_IsDebug            = 'DEBUGGING';


  C_Section_SystemDaten      = 'SYSTEMDATEN';

  C_Ident_Vorwahl              = 'VORWAHL';
  C_Ident_WarteZeit1           = 'WARTEZEIT1';
  C_Ident_WarteZeit2           = 'WARTEZEIT2';
  C_Ident_WarteZeit3           = 'WARTEZEIT3';
  C_Ident_WarteZeit4           = 'WARTEZEIT4';
  C_Ident_WarteZeit5           = 'WARTEZEIT5';
  C_Ident_WarteZeit6           = 'WARTEZEIT6';
  C_Ident_MaxAnrufZentrale     = 'MAXZENTRALE';
  C_Ident_MaxAnrufStation      = 'MAXSTATION';
  C_Ident_RufDeaktivierung     = 'RUFDEAKTIVIERUNG';
  C_Ident_StationDeaktivierung = 'STATIONDEAKTIVIERUNG';
  C_Ident_Datum1               = 'ERSTER_ABRUF';
  C_Ident_ZeitSyncMin          = 'ZEITSYNCMIN';
  C_Ident_ZeitSyncMax          = 'ZEITSYNCMAX';
  C_Ident_AnzMeldPuffer        = 'GR_MELDPUFFER';
  C_Ident_AnzJournalPuffer     = 'GR_JOURNALPUFFER';
  C_Ident_ErrorBeep            = 'ERRORBEEP';
  C_Ident_RufEDatenTypen       = 'REDATENTYPEN';
  C_Ident_MinutenOffset        = 'MINUTENOFFSET';

  C_Ident_LaksMomDatenTypen  = 'LAKS_MOMDATENTYPEN';
  C_Ident_LaksDatum1         = 'LAKS_ERSTER_ABRUF';
  C_Ident_LaksAbrufMPE       = 'LAKS_ABRUFMPE';
  C_Ident_LaksMinutenOffset  = 'LAKS_MINUTENOFFSET';

  C_Ident_DSfGAnzErsteDaten    = 'DSFG_ANZ_ERSTEDATEN';
  C_Ident_DSfGMinutenOffset    = 'DSFG_MINUTENOFFSET';
  C_Ident_DSfGRufEDatenTypen   = 'DSFG_REDATENTYPEN';
  C_Ident_AnzDSfGLogbuchPuffer = 'DSFG_LOGBUCHPUFFER';

  C_Ident_ReportMinOffset = 'REPORT_MINUTENOFFSET';  // 21.04.2015  WN

{--------------------------- TSystemEinstellungen -----------------------------}

{------------------------------------------------}
constructor TSystemEinstellungen.create(FilePath: string);
{------------------------------------------------}
var
  s: string;
begin
  s:= FilePath;
  if s[length(s)] <> '\' then s:= s + '\';
  s:= s + C_SysDat_IniFileName;
  inherited create(s);
end;

{------------------------------------------------}
destructor TSystemEinstellungen.destroy;
{------------------------------------------------}
begin
  inherited destroy;
end;

{------------------------------------------------}
function TSystemEinstellungen.GetAktZyklus: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentAktZyklus, 5000);
end;

{------------------------------------------------}
procedure TSystemEinstellungen.SetAktZyklus (Value: integer);
{------------------------------------------------}
begin
  WriteInteger (CSectionProgram, CIdentAktZyklus, Value);
end;

{ Liest, ob Abrufmodule im Debug-Modus gestartet werden }
{------------------------------------------------}
function TSystemEinstellungen.GetIsDebug: boolean;
{------------------------------------------------}
begin
  result:= ReadBool(C_Section_Einstellungen, C_Ident_IsDebug, C_EinstIsDebug);
end;

{------------------------------------------------}
function TSystemEinstellungen.GetVorwahl: string;
{------------------------------------------------}
begin
  result:= ReadString(C_Section_SystemDaten, C_Ident_Vorwahl, C_SysVorwahl);
end;

{------------------------------------------------}
procedure TSystemEinstellungen.SetVorwahl(Value: string);
{------------------------------------------------}
begin
  WriteString(C_Section_SystemDaten, C_Ident_Vorwahl, Value);
end;

{------------------------------------------------}
function TSystemEinstellungen.GetWarteZeit: TArrayInt1_6;
{------------------------------------------------}
begin
  result[1]:=
    ReadInteger(C_Section_SystemDaten, C_Ident_WarteZeit1, C_SysWarteZeit1);
  result[2]:=
    ReadInteger(C_Section_SystemDaten, C_Ident_WarteZeit2, C_SysWarteZeit2);
  result[3]:=
    ReadInteger(C_Section_SystemDaten, C_Ident_WarteZeit3, C_SysWarteZeit3);
  result[4]:=
    ReadInteger(C_Section_SystemDaten, C_Ident_WarteZeit4, C_SysWarteZeit4);
  result[5]:=
    ReadInteger(C_Section_SystemDaten, C_Ident_WarteZeit5, C_SysWarteZeit5);
  result[6]:=
    ReadInteger(C_Section_SystemDaten, C_Ident_WarteZeit6, C_SysWarteZeit6);
end;

{------------------------------------------------}
procedure TSystemEinstellungen.SetWarteZeit(Value: TArrayInt1_6);
{------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_WarteZeit1, Value[1]);
  WriteInteger(C_Section_SystemDaten, C_Ident_WarteZeit2, Value[2]);
  WriteInteger(C_Section_SystemDaten, C_Ident_WarteZeit3, Value[3]);
  WriteInteger(C_Section_SystemDaten, C_Ident_WarteZeit4, Value[4]);
  WriteInteger(C_Section_SystemDaten, C_Ident_WarteZeit5, Value[5]);
  WriteInteger(C_Section_SystemDaten, C_Ident_WarteZeit6, Value[6]);
end;

{------------------------------------------------}
function TSystemEinstellungen.GetMaxAnrufZentrale: integer;
{------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_MaxAnrufZentrale,
                       C_SysMaxAnrufversuche_Zentrale);
end;

{------------------------------------------------}
procedure TSystemEinstellungen.SetMaxAnrufZentrale(Value: integer);
{------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_MaxAnrufZentrale, Value);
end;

{------------------------------------------------}
function TSystemEinstellungen.GetMaxAnrufStation: integer;
{------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_MaxAnrufStation,
                       C_SysMaxAnrufversuche_Station);
end;

{------------------------------------------------}
procedure TSystemEinstellungen.SetMaxAnrufStation(Value: integer);
{------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_MaxAnrufStation, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetRufDeaktivierung: boolean;
{---------------------------------------------------------}
begin
  result:= ReadBool(C_Section_SystemDaten, C_Ident_RufDeaktivierung, False);
end;

{-----------------------------------------------------------------}
procedure TSystemEinstellungen.SetRufDeaktivierung(Value: boolean);
{-----------------------------------------------------------------}
begin
  WriteBool(C_Section_SystemDaten, C_Ident_RufDeaktivierung, Value);
end;

{-------------------------------------------------------------}
function TSystemEinstellungen.GetStationDeaktivierung: boolean;
{-------------------------------------------------------------}
begin
  result:= ReadBool(C_Section_SystemDaten, C_Ident_StationDeaktivierung, C_EinstStationDeakt);
end;

{---------------------------------------------------------------------}
procedure TSystemEinstellungen.SetStationDeaktivierung(Value: boolean);
{---------------------------------------------------------------------}
begin
  WriteBool(C_Section_SystemDaten, C_Ident_StationDeaktivierung, Value);
end;

{------------------------------------------------}
function TSystemEinstellungen.GetMrgDatumErsteDaten: double;
{------------------------------------------------}
begin
  result:= ReadFloat(C_Section_SystemDaten, C_Ident_Datum1, 1);
  if result > 1000 then result:=1;   { Kompatibilität zu früherer Version mit festem Datum }
end;

{------------------------------------------------}
procedure TSystemEinstellungen.SetMrgDatumErsteDaten(Value: double);
{------------------------------------------------}
begin
  WriteFloat(C_Section_SystemDaten, C_Ident_Datum1, Double(Value));
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.setZeitSyncMin(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_ZeitSyncMin, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.getZeitSyncMin: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_ZeitSyncMin, C_SysMrgZeitSyncMin);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.setZeitSyncMax(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_ZeitSyncMax, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.getZeitSyncMax: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_ZeitSyncMax, C_SysMrgZeitSyncMax);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetMrgSystemDaten: TMrgSystemdaten;
{---------------------------------------------------------}
begin
  result.Vorwahl:= Vorwahl;
  result.Wartezeit:= WarteZeit;
  result.MaxAnrufversuche_Zentrale:= MaxAnrufZentrale;
  if RufDeaktivierung then
    result.MaxAnrufversuche_Station:= MaxAnrufStation
  else
    result.MaxAnrufversuche_Station:= 0;
  result.DatumErsteDaten:= date-trunc(MrgErsteDaten)+frac(MrgErsteDaten);
  result.ZeitSyncMin:= ZeitSyncMin;
  result.ZeitSyncMax:= ZeitSyncMax;
  result.RufDatenTypen:= DatentypenRufentgegennahme;
  result.MinutenOffset:= MRGMinOffset;
  result.GrMeldPuffer:= GroesseMeldPuffer;
  result.GrJournalPuffer:= GroesseJournalPuffer;
  result.ErrorBeep:=ErrorBeep;
  result.StationDeaktivierung:=StationDeaktivierung;
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetGroesseMeldPuffer: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_AnzMeldPuffer, C_SysMrgGrMeldPuffer);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetGroesseMeldPuffer(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_AnzMeldPuffer, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetGroesseJournalPuffer: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_AnzJournalPuffer, C_SysGrJournalPuffer);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetGroesseJournalPuffer(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_AnzJournalPuffer, Value);
end;

{--------------------------------------------------}
function TSystemEinstellungen.GetErrorBeep: boolean;
{--------------------------------------------------}
begin
  result:= ReadBool(C_Section_SystemDaten, C_Ident_ErrorBeep, False);
end;

{----------------------------------------------------------}
procedure TSystemEinstellungen.SetErrorBeep(Value: boolean);
{----------------------------------------------------------}
begin
  WriteBool(C_Section_SystemDaten, C_Ident_ErrorBeep, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetDatentypenRufentgegennahme: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_RufEDatenTypen, C_SysMrg_RufEDatenTypen);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetDatentypenRufentgegennahme(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_RufEDatenTypen, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetMRGMinOffset: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_MinutenOffset, C_SysMrg_MinutenOffset);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetMRGMinOffset(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_MinutenOffset, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetDatentypenLaksMomentan: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_LaksMomDatenTypen,
                       C_SysLaks_LaksMomDatenTypen);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetDatentypenLaksMomentan(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_LaksMomDatenTypen, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetLaksSystemDaten: TLAKSSystemDaten;
{---------------------------------------------------------}
begin
  result.Vorwahl:= Vorwahl;
  result.Wartezeit:= WarteZeit;
  result.MaxAnrufversuche_Zentrale:= MaxAnrufZentrale;
  if RufDeaktivierung then
    result.MaxAnrufversuche_Station:= MaxAnrufStation
  else
    result.MaxAnrufversuche_Station:= 0;
  result.DatumErsteDaten:= date-trunc(LaksErsteDaten)+frac(LaksErsteDaten);
  result.GrJournalPuffer:= GroesseJournalPuffer;
  result.MomAbrufDatenTypen:= DatentypenLaksMomentan;
  result.MomAbrufMPE:= LaksAbrufMPE;
  result.MinutenOffset:= LaksMinOffset;
end;

{------------------------------------------------}
function TSystemEinstellungen.GetLaksDatumErsteDaten: double;
{------------------------------------------------}
begin
  result:= ReadFloat(C_Section_SystemDaten, C_Ident_LaksDatum1, 1);
end;

{------------------------------------------------}
procedure TSystemEinstellungen.SetLaksDatumErsteDaten(Value: double);
{------------------------------------------------}
begin
  WriteFloat(C_Section_SystemDaten, C_Ident_LaksDatum1, Double(Value));
end;

{------------------------------------------------}
function TSystemEinstellungen.GetLaksAbrufMPE: boolean;
{------------------------------------------------}
begin
  result:= ReadBool(C_Section_SystemDaten, C_Ident_LaksAbrufMPE, False);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetLaksAbrufMPE(Value: boolean);
{---------------------------------------------------------}
begin
  WriteBool(C_Section_SystemDaten, C_Ident_LaksAbrufMPE, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetLaksMinOffset: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_LaksMinutenOffset, C_SysLaks_MinutenOffset);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetLaksMinOffset(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_LaksMinutenOffset, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetDSfGSystemDaten: TDSfGSystemdaten;
{---------------------------------------------------------}
begin
  result.Vorwahl:= Vorwahl;
  result.Wartezeit:= WarteZeit;
  result.MaxAnrufversuche_Zentrale:= MaxAnrufZentrale;
  if RufDeaktivierung then
    result.MaxAnrufversuche_Station:= MaxAnrufStation
  else
    result.MaxAnrufversuche_Station:= 0;
  result.MinutenOffset:= DSfGMinOffset;
  result.GrJournalPuffer:= GroesseJournalPuffer;
  result.AnzahlErsteDaten:=DSfGAnzErsteDaten;
  result.ErrorBeep:=ErrorBeep;
  result.RufDatenTypen:= DSfGDatentypenRufentgegennahme;
  result.GrLogbuchPuffer:= DSfGGroesseLogbuchPuffer;
  result.StationDeaktivierung:=StationDeaktivierung;
end;

{------------------------------------------------}
function TSystemEinstellungen.GetDSfGAnzErsteDaten: integer;
{------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_DSfGAnzErsteDaten, 24);
end;

{------------------------------------------------}
procedure TSystemEinstellungen.SetDSfGAnzErsteDaten(Value: integer);
{------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_DSfGAnzErsteDaten, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetDSfGMinOffset: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_DSfGMinutenOffset, C_SysDSfG_MinutenOffset);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetDSfGMinOffset(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_DSfGMinutenOffset, Value);
end;

{---------------------------------------------------------}
function TSystemEinstellungen.GetDSfGDatentypenRufentgegennahme: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_DSfGRufEDatenTypen, C_SysDSfG_RufEDatenTypen);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetDSfGDatentypenRufentgegennahme(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_DSfGRufEDatenTypen, Value);
end;

{-----------------------------------------------------------------}
function TSystemEinstellungen.GetDSfGGroesseLogbuchPuffer: integer;
{-----------------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_AnzDSfGLogbuchPuffer, C_SysDSfGGrLogbuchPuffer);
end;

{-------------------------------------------------------------------------}
procedure TSystemEinstellungen.SetDSfGGroesseLogbuchPuffer(Value: integer);
{-------------------------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_AnzDSfGLogbuchPuffer, Value);
end;

// 21.04.2015  WN
{---------------------------------------------------------}
function TSystemEinstellungen.GetReportMinOffset: integer;
{---------------------------------------------------------}
begin
  result:= ReadInteger(C_Section_SystemDaten, C_Ident_ReportMinOffset, C_SysReport_MinutenOffset);
end;

{---------------------------------------------------------}
procedure TSystemEinstellungen.SetReportMinOffset(Value: integer);
{---------------------------------------------------------}
begin
  WriteInteger(C_Section_SystemDaten, C_Ident_ReportMinOffset, Value);
end;

end.
