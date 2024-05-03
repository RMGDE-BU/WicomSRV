{******************************************************************************}
{* Unit: IniFile zum Speichern der Anzeige-Kriterien für Meldungen            *}
{* 05.07.2001   WW                                                            *}
{* 19.03.2012   WW  mit property 'FeldSigStatus'                              *}
{******************************************************************************}
unit MeldFilterIni;

interface

uses
  IniFiles, SysUtils;

type
  TMeldFilterIniFile = class(TIniFile)
  protected
    function GetFilterAbrufart: byte;
    procedure SetFilterAbrufart (Value: byte);
    function GetFilterDatumZeit: boolean;
    procedure SetFilterDatumZeit (Value: boolean);
    function GetFilterDatumVon: integer;
    procedure SetFilterDatumVon (Value: integer);
    function GetFilterDatumBis: integer;
    procedure SetFilterDatumBis (Value: integer);
    function GetFilterMRG: boolean;
    procedure SetFilterMRG (Value: boolean);
    function GetFilterDSfG: boolean;
    procedure SetFilterDSfG (Value: boolean);
    function GetFilterHinweis: boolean;
    procedure SetFilterHinweis (Value: boolean);
    function GetFilterWarnung: boolean;
    procedure SetFilterWarnung (Value: boolean);
    function GetFilterStoerung: boolean;
    procedure SetFilterStoerung (Value: boolean);
    function GetFilterRechnerfehler: boolean;
    procedure SetFilterRechnerfehler (Value: boolean);
    function GetFilterQuittiert: boolean;
    procedure SetFilterQuittiert (Value: boolean);
    function GetFilterNichtQuittiert: boolean;
    procedure SetFilterNichtQuittiert (Value: boolean);
    function GetFilterMRGStation: integer;
    procedure SetFilterMRGStation (Value: integer);
    function GetFilterDSfGInstanz: integer;
    procedure SetFilterDSfGInstanz (Value: integer);
    function GetFilterDSfGStation: integer;
    procedure SetFilterDSfGStation (Value: integer);

    function GetSortierung: byte;
    procedure SetSortierung (Value: byte);

    function GetFeldQuittiert: boolean;
    procedure SetFeldQuittiert (Value: boolean);
    function GetFeldDatumZeit: boolean;
    procedure SetFeldDatumZeit (Value: boolean);
    function GetFeldZeitzone: boolean;
    procedure SetFeldZeitzone (Value: boolean);
    function GetFeldMeldung: boolean;
    procedure SetFeldMeldung (Value: boolean);
    function GetFeldGeraeteArt: boolean;
    procedure SetFeldGeraeteArt (Value: boolean);
    function GetFeldStation: boolean;
    procedure SetFeldStation (Value: boolean);
    function GetFeldOrdNr: boolean;
    procedure SetFeldOrdNr (Value: boolean);
    function GetFeldMNrGeraet: boolean;
    procedure SetFeldMNrGeraet (Value: boolean);
    function GetFeldMNrAllg: boolean;
    procedure SetFeldMNrAllg (Value: boolean);
    function GetFeldDSfGStatus: boolean;
    procedure SetFeldDSfGStatus (Value: boolean);
    function GetFeldCRC: boolean;
    procedure SetFeldCRC (Value: boolean);
    function GetFeldBemerkung: boolean;
    procedure SetFeldBemerkung (Value: boolean);
    function GetFeldSigStatus: boolean;
    procedure SetFeldSigStatus (Value: boolean);
  public
    property FilterAbrufart: byte read GetFilterAbrufart write SetFilterAbrufart;
    property FilterDatumZeit: boolean read GetFilterDatumZeit write SetFilterDatumZeit;
    property FilterDatumVon: integer read GetFilterDatumVon write SetFilterDatumVon;
    property FilterDatumBis: integer read GetFilterDatumBis write SetFilterDatumBis;
    property FilterMRG: boolean read GetFilterMRG write SetFilterMRG;
    property FilterDSfG: boolean read GetFilterDSfG write SetFilterDSfG;
    property FilterHinweis: boolean read GetFilterHinweis write SetFilterHinweis;
    property FilterWarnung: boolean read GetFilterWarnung write SetFilterWarnung;
    property FilterStoerung: boolean read GetFilterStoerung write SetFilterStoerung;
    property FilterRechnerfehler: boolean read GetFilterRechnerfehler write SetFilterRechnerfehler;
    property FilterQuittiert: boolean read GetFilterQuittiert write SetFilterQuittiert;
    property FilterNichtQuittiert: boolean read GetFilterNichtQuittiert write SetFilterNichtQuittiert;
    property FilterMRGStation: integer read GetFilterMRGStation write SetFilterMRGStation;
    property FilterDSfGInstanz: integer read GetFilterDSfGInstanz write SetFilterDSfGInstanz;
    property FilterDSfGStation: integer read GetFilterDSfGStation write SetFilterDSfGStation;

    property Sortierung: byte read GetSortierung write SetSortierung;

    property FeldQuittiert: boolean read GetFeldQuittiert write SetFeldQuittiert;
    property FeldDatumZeit: boolean read GetFeldDatumZeit write SetFeldDatumZeit;
    property FeldZeitzone: boolean read GetFeldZeitzone write SetFeldZeitzone;
    property FeldMeldung: boolean read GetFeldMeldung write SetFeldMeldung;
    property FeldGeraeteArt: boolean read GetFeldGeraeteArt write SetFeldGeraeteArt;
    property FeldStation: boolean read GetFeldStation write SetFeldStation;
    property FeldOrdNr: boolean read GetFeldOrdNr write SetFeldOrdNr;
    property FeldMNrGeraet: boolean read GetFeldMNrGeraet write SetFeldMNrGeraet;
    property FeldMNrAllg: boolean read GetFeldMNrAllg write SetFeldMNrAllg;
    property FeldDSfGStatus: boolean read GetFeldDSfGStatus write SetFeldDSfGStatus;
    property FeldCRC: boolean read GetFeldCRC write SetFeldCRC;
    property FeldBemerkung: boolean read GetFeldBemerkung write SetFeldBemerkung;
    property FeldSigStatus: boolean read GetFeldSigStatus write SetFeldSigStatus;  // 19.03.2012, WW

    procedure SetDefaultFilterSortierung;
end;

implementation

const
  C_SectionMeldFilter = 'MeldFilter';

  C_IdentFilterAbrufart       = 'FilterAbrufart';
  C_IdentFilterDatumZeit      = 'FilterDatumZeit';
  C_IdentFilterDatumVon       = 'FilterDatumVon';
  C_IdentFilterDatumBis       = 'FilterDatumBis';
  C_IdentFilterMRG            = 'FilterMRG';
  C_IdentFilterDSfG           = 'FilterDSfG';
  C_IdentFilterHinweis        = 'FilterHinweis';
  C_IdentFilterWarnung        = 'FilterWarnung';
  C_IdentFilterStoerung       = 'FilterStoerung';
  C_IdentFilterRechnerfehler  = 'FilterRechnerfehler';
  C_IdentFilterQuittiert      = 'FilterQuittiert';
  C_IdentFilterNichtQuittiert = 'FilterNichtQuittiert';
  C_IdentFilterMRGStation     = 'FilterMRGStation';
  C_IdentFilterDSfGInstanz    = 'FilterDSfGInstanz';
  C_IdentFilterDSfGStation    = 'FilterDSfGStation';

  C_IdentSortierung = 'Sortierung';

  C_IdentFeldQuittiert  = 'FeldQuitterung';
  C_IdentFeldDatumZeit  = 'FeldDatumZeit';
  C_IdentFeldZeitzone   = 'FeldZeitzone';
  C_IdentFeldMeldung    = 'FeldMeldung';
  C_IdentFeldGeraeteArt = 'FeldGeraeteArt';
  C_IdentFeldStation    = 'FeldStation';
  C_IdentFeldOrdNr      = 'FeldOrdNr';
  C_IdentFeldMNrGeraet  = 'FeldMNrGeraet';
  C_IdentFeldMNrAllg    = 'FeldMNrAllg';
  C_IdentFeldDSfGStatus = 'FeldDSfGStatus';
  C_IdentFeldCRC        = 'FeldCRC';
  C_IdentFeldBemerkung  = 'FeldBemerkung';
  C_IdentFeldSigStatus  = 'FeldSigStatus';


  { Filter-Defaultwerte:
    -> "alle nicht quittierten Automatik-Meldungen"
    (wichtig für Meldungssichten im Rufentgegennahme-Client, nicht ändern !) }
  C_DefaultFilterAbrufart       = 0;     { Automatik-Meldungen }
  C_DefaultFilterDatumZeit      = false;
  C_DefaultFilterMRG            = true;
  C_DefaultFilterDSfG           = true;
  C_DefaultFilterMRGStation     = 0;     { alle MRG-Stationen }
  C_DefaultFilterDSfGInstanz    = 0;     { alle DSfG-Instanzen }
  C_DefaultFilterDSfGStation    = 0;
  C_DefaultFilterHinweis        = true;
  C_DefaultFilterWarnung        = true;
  C_DefaultFilterStoerung       = true;
  C_DefaultFilterRechnerfehler  = true;
  C_DefaultFilterQuittiert      = false;
  C_DefaultFilterNichtQuittiert = true;

  { Sortierungs-Defaultwert:
    -> sortiert nach Datum/Zeit
    (wichtig für Meldungssichten im Rufentgegennahme-Client, nicht ändern !) }
  C_DefaultSortierung = 0;

  { Feld-Defaultwerte: }
  C_DefaultFeldQuittiert  = true;
  C_DefaultFeldDatumZeit  = true;
  C_DefaultFeldZeitzone   = true;
  C_DefaultFeldMeldung    = true;
  C_DefaultFeldGeraeteArt = true;
  C_DefaultFeldStation    = true;
  C_DefaultFeldOrdNr      = false;
  C_DefaultFeldMNrGeraet  = false;
  C_DefaultFeldMNrAllg    = false;
  C_DefaultFeldDSfGStatus = false;
  C_DefaultFeldCRC        = false;
  C_DefaultFeldBemerkung  = true;
  C_DefaultFeldSigStatus  = true;


{--------------------------------------------------}
function TMeldFilterIniFile.GetFilterAbrufart: byte;
{--------------------------------------------------}
{ Flag für Filter 'Abrufart' lesen }
begin
  Result:=ReadInteger (C_SectionMeldFilter, C_IdentFilterAbrufart, C_DefaultFilterAbrufart);
end;

{-----------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterAbrufart (Value: byte);
{-----------------------------------------------------------}
{ Flag für Filter 'Abrufart' setzen }
begin
  WriteInteger (C_SectionMeldFilter, C_IdentFilterAbrufart, Value);
end;

{------------------------------------------------------}
function TMeldFilterIniFile.GetFilterDatumZeit: boolean;
{------------------------------------------------------}
{ Flag für Filter 'Datum/Zeit' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFilterDatumZeit, C_DefaultFilterDatumZeit);
end;

{---------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterDatumZeit (Value: boolean);
{---------------------------------------------------------------}
{ Flag für Filter 'Datum/Zeit' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFilterDatumZeit, Value);
end;

{-----------------------------------------------------}
function TMeldFilterIniFile.GetFilterDatumVon: integer;
{-----------------------------------------------------}
{ Filter 'Datum von' lesen }
var
  Default: integer;
  year, month, day: word;
begin
  { Standard-von-Datum: erster des aktuellen Monats }
  DecodeDate (Date, year, month, day);
  Default:=trunc (EncodeDate (year, month, 1));
  Result:=ReadInteger (C_SectionMeldFilter, C_IdentFilterDatumVon, Default);
end;

{--------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterDatumVon (Value: integer);
{--------------------------------------------------------------}
{ Filter 'Datum von' setzen }
begin
  WriteInteger (C_SectionMeldFilter, C_IdentFilterDatumVon, Value);
end;

{-----------------------------------------------------}
function TMeldFilterIniFile.GetFilterDatumBis: integer;
{-----------------------------------------------------}
{ Filter 'Datum bis' lesen }
var
  Default: integer;
begin
  { Standard-bis-Datum: aktueller Tag }
  Default:=trunc (Date);
  Result:=ReadInteger (C_SectionMeldFilter, C_IdentFilterDatumBis, Default);
end;

{--------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterDatumBis (Value: integer);
{--------------------------------------------------------------}
{ Filter 'Datum bis' setzen }
begin
  WriteInteger (C_SectionMeldFilter, C_IdentFilterDatumBis, Value);
end;

{------------------------------------------------}
function TMeldFilterIniFile.GetFilterMRG: boolean;
{-------------------------------------------------}
{ Flag für Filter 'MRG' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFilterMRG, C_DefaultFilterMRG);
end;

{---------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterMRG (Value: boolean);
{---------------------------------------------------------}
{ Flag für Filter 'MRG' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFilterMRG, Value);
end;

{-------------------------------------------------}
function TMeldFilterIniFile.GetFilterDSfG: boolean;
{-------------------------------------------------}
{ Flag für Filter 'DSfG' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFilterDSfG, C_DefaultFilterDSfG);
end;

{----------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterDSfG (Value: boolean);
{----------------------------------------------------------}
{ Flag für Filter 'DSfG' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFilterDSfG, Value);
end;

{----------------------------------------------------}
function TMeldFilterIniFile.GetFilterHinweis: boolean;
{----------------------------------------------------}
{ Flag für Filter 'Hinweis' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFilterHinweis, C_DefaultFilterHinweis);
end;

{-------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterHinweis (Value: boolean);
{-------------------------------------------------------------}
{ Flag für Filter 'Hinweis' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFilterHinweis, Value);
end;

{----------------------------------------------------}
function TMeldFilterIniFile.GetFilterWarnung: boolean;
{----------------------------------------------------}
{ Flag für Filter 'Warnung' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFilterWarnung, C_DefaultFilterWarnung);
end;

{-------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterWarnung (Value: boolean);
{-------------------------------------------------------------}
{ Flag für Filter 'Warnung' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFilterWarnung, Value);
end;

{-----------------------------------------------------}
function TMeldFilterIniFile.GetFilterStoerung: boolean;
{-----------------------------------------------------}
{ Flag für Filter 'Stoerung' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFilterStoerung, C_DefaultFilterStoerung);
end;

{--------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterStoerung (Value: boolean);
{--------------------------------------------------------------}
{ Flag für Filter 'Stoerung' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFilterStoerung, Value);
end;

{----------------------------------------------------------}
function TMeldFilterIniFile.GetFilterRechnerfehler: boolean;
{----------------------------------------------------------}
{ Flag für Filter 'Rechnerfehler' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFilterRechnerfehler, C_DefaultFilterRechnerfehler);
end;

{-------------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterRechnerfehler (Value: boolean);
{-------------------------------------------------------------------}
{ Flag für Filter 'Rechnerfehler' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFilterRechnerfehler, Value);
end;

{------------------------------------------------------}
function TMeldFilterIniFile.GetFilterQuittiert: boolean;
{------------------------------------------------------}
{ Flag für Filter 'Quittiert' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFilterQuittiert, C_DefaultFilterQuittiert);
end;

{---------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterQuittiert (Value: boolean);
{---------------------------------------------------------------}
{ Flag für Filter 'Quittiert' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFilterQuittiert, Value);
end;

{-----------------------------------------------------------}
function TMeldFilterIniFile.GetFilterNichtQuittiert: boolean;
{-----------------------------------------------------------}
{ Flag für Filter 'Nicht Quittiert' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFilterNichtQuittiert, C_DefaultFilterNichtQuittiert);
end;

{--------------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterNichtQuittiert (Value: boolean);
{--------------------------------------------------------------------}
{ Flag für Filter 'Nicht Quittiert' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFilterNichtQuittiert, Value);
end;

{-------------------------------------------------------}
function TMeldFilterIniFile.GetFilterMRGStation: integer;
{-------------------------------------------------------}
{ Filter 'MRGStationId' lesen }
begin
  Result:=ReadInteger (C_SectionMeldFilter, C_IdentFilterMRGStation, C_DefaultFilterMRGStation);
end;

{----------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterMRGStation (Value: integer);
{----------------------------------------------------------------}
{ Filter 'MRG-StationId' setzen }
begin
  WriteInteger (C_SectionMeldFilter, C_IdentFilterMRGStation, Value);
end;

{--------------------------------------------------------}
function TMeldFilterIniFile.GetFilterDSfGInstanz: integer;
{--------------------------------------------------------}
{ Filter 'DSfG-Instanz-Id' lesen }
begin
  Result:=ReadInteger (C_SectionMeldFilter, C_IdentFilterDSfGInstanz, C_DefaultFilterDSfGInstanz);
end;

{-----------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterDSfGInstanz (Value: integer);
{-----------------------------------------------------------------}
{ Filter 'DSfG-Instanz-Id' setzen }
begin
  WriteInteger (C_SectionMeldFilter, C_IdentFilterDSfGInstanz, Value);
end;

{--------------------------------------------------------}
function TMeldFilterIniFile.GetFilterDSfGStation: integer;
{--------------------------------------------------------}
{ Filter 'DSfG-Station-Id' lesen }
begin
  Result:=ReadInteger (C_SectionMeldFilter, C_IdentFilterDSfGStation, C_DefaultFilterDSfGStation);
end;

{-----------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFilterDSfGStation (Value: integer);
{-----------------------------------------------------------------}
{ Filter 'DSfG-Station-Id' setzen }
begin
  WriteInteger (C_SectionMeldFilter, C_IdentFilterDSfGStation, Value);
end;


{------------------------------------------------------------------------------}

{----------------------------------------------}
function TMeldFilterIniFile.GetSortierung: byte;
{----------------------------------------------}
{ Sortier-Kriterium lesen }
begin
  Result:=ReadInteger(C_SectionMeldFilter, C_IdentSortierung, C_DefaultSortierung);
end;

{-------------------------------------------------------}
procedure TMeldFilterIniFile.SetSortierung (Value: byte);
{-------------------------------------------------------}
{ Sortier-Kriterium setzen }
begin
  WriteInteger (C_SectionMeldFilter, C_IdentSortierung, Value);
end;


{------------------------------------------------------------------------------}

{----------------------------------------------------}
function TMeldFilterIniFile.GetFeldQuittiert: boolean;
{----------------------------------------------------}
{ Flag für Feldanzeige 'Quittiert' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldQuittiert, C_DefaultFeldQuittiert);
end;

{-------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldQuittiert (Value: boolean);
{-------------------------------------------------------------}
{ Flag für Feldanzeige 'Quittiert' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldQuittiert, Value);
end;

{----------------------------------------------------}
function TMeldFilterIniFile.GetFeldDatumZeit: boolean;
{----------------------------------------------------}
{ Flag für Feldanzeige 'DatumZeit' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldDatumZeit, C_DefaultFeldDatumZeit);
end;

{-------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldDatumZeit (Value: boolean);
{-------------------------------------------------------------}
{ Flag für Feldanzeige 'DatumZeit' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldDatumZeit, Value);
end;

{---------------------------------------------------}
function TMeldFilterIniFile.GetFeldZeitzone: boolean;
{---------------------------------------------------}
{ Flag für Feldanzeige 'Zeitzone' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldZeitzone, C_DefaultFeldZeitzone);
end;

{------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldZeitzone (Value: boolean);
{------------------------------------------------------------}
{ Flag für Feldanzeige 'Zeitzone' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldZeitzone, Value);
end;

{--------------------------------------------------}
function TMeldFilterIniFile.GetFeldMeldung: boolean;
{--------------------------------------------------}
{ Flag für Feldanzeige 'Meldung' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldMeldung, C_DefaultFeldMeldung);
end;

{-----------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldMeldung (Value: boolean);
{-----------------------------------------------------------}
{ Flag für Feldanzeige 'Meldung' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldMeldung, Value);
end;

{-----------------------------------------------------}
function TMeldFilterIniFile.GetFeldGeraeteArt: boolean;
{-----------------------------------------------------}
{ Flag für Feldanzeige 'GeraeteArt' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldGeraeteArt, C_DefaultFeldGeraeteArt);
end;

{--------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldGeraeteArt (Value: boolean);
{--------------------------------------------------------------}
{ Flag für Feldanzeige 'GeraeteArt' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldGeraeteArt, Value);
end;

{--------------------------------------------------}
function TMeldFilterIniFile.GetFeldStation: boolean;
{--------------------------------------------------}
{ Flag für Feldanzeige 'Station' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldStation, C_DefaultFeldStation);
end;

{-----------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldStation (Value: boolean);
{-----------------------------------------------------------}
{ Flag für Feldanzeige 'Station' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldStation, Value);
end;

{------------------------------------------------}
function TMeldFilterIniFile.GetFeldOrdNr: boolean;
{------------------------------------------------}
{ Flag für Feldanzeige 'OrdNr' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldOrdNr, C_DefaultFeldOrdNr);
end;

{---------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldOrdNr (Value: boolean);
{---------------------------------------------------------}
{ Flag für Feldanzeige 'OrdNr' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldOrdNr, Value);
end;

{----------------------------------------------------}
function TMeldFilterIniFile.GetFeldMNrGeraet: boolean;
{----------------------------------------------------}
{ Flag für Feldanzeige 'MNrGeraet' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldMNrGeraet, C_DefaultFeldMNrGeraet);
end;

{-------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldMNrGeraet (Value: boolean);
{-------------------------------------------------------------}
{ Flag für Feldanzeige 'MNrGeraet' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldMNrGeraet, Value);
end;

{--------------------------------------------------}
function TMeldFilterIniFile.GetFeldMNrAllg: boolean;
{--------------------------------------------------}
{ Flag für Feldanzeige 'MNrAllg' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldMNrAllg, C_DefaultFeldMNrAllg);
end;

{-----------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldMNrAllg (Value: boolean);
{-----------------------------------------------------------}
{ Flag für Feldanzeige 'MNrAllg' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldMNrAllg, Value);
end;

{-----------------------------------------------------}
function TMeldFilterIniFile.GetFeldDSfGStatus: boolean;
{-----------------------------------------------------}
{ Flag für Feldanzeige 'DSfGStatus' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldDSfGStatus, C_DefaultFeldDSfGStatus);
end;

{--------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldDSfGStatus (Value: boolean);
{--------------------------------------------------------------}
{ Flag für Feldanzeige 'DSfGStatus' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldDSfGStatus, Value);
end;

{----------------------------------------------}
function TMeldFilterIniFile.GetFeldCRC: boolean;
{----------------------------------------------}
{ Flag für Feldanzeige 'CRC' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldCRC, C_DefaultFeldCRC);
end;

{-------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldCRC (Value: boolean);
{-------------------------------------------------------}
{ Flag für Feldanzeige 'CRC' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldCRC, Value);
end;

{----------------------------------------------------}
function TMeldFilterIniFile.GetFeldBemerkung: boolean;
{----------------------------------------------------}
{ Flag für Feldanzeige 'Bemerkung' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldBemerkung, C_DefaultFeldBemerkung);
end;

{-------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldBemerkung (Value: boolean);
{-------------------------------------------------------------}
{ Flag für Feldanzeige 'Bemerkung' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldBemerkung, Value);
end;

{----------------------------------------------------}
function TMeldFilterIniFile.GetFeldSigStatus: boolean;  // 19.03.2012, WW
{----------------------------------------------------}
{ Flag für Feldanzeige 'Signaturstatus' lesen }
begin
  Result:=ReadBool (C_SectionMeldFilter, C_IdentFeldSigStatus, C_DefaultFeldSigStatus);
end;

{-------------------------------------------------------------}
procedure TMeldFilterIniFile.SetFeldSigStatus (Value: boolean);  // 19.03.2012, WW
{-------------------------------------------------------------}
{ Flag für Feldanzeige 'Signaturstatus' setzen }
begin
  WriteBool (C_SectionMeldFilter, C_IdentFeldSigStatus, Value);
end;

{------------------------------------------------------}
procedure TMeldFilterIniFile.SetDefaultFilterSortierung;
{------------------------------------------------------}
{ Filter-Flags und Sortierung auf ihre Defaultwerte zurücksetzen }
begin
  FilterAbrufart:=C_DefaultFilterAbrufart;
  FilterMRG:=C_DefaultFilterMRG;
  FilterDSfG:=C_DefaultFilterDSfG;
  FilterMRGStation:=C_DefaultFilterMRGStation;
  FilterDSfGInstanz:=C_DefaultFilterDSfGInstanz;
  FilterDSfGStation:=C_DefaultFilterDSfGStation;
  FilterDatumZeit:=C_DefaultFilterDatumZeit;
  FilterHinweis:=C_DefaultFilterHinweis;
  FilterWarnung:=C_DefaultFilterWarnung;
  FilterStoerung:=C_DefaultFilterStoerung;
  FilterRechnerfehler:=C_DefaultFilterRechnerfehler;
  FilterQuittiert:=C_DefaultFilterQuittiert;
  FilterNichtQuittiert:=C_DefaultFilterNichtQuittiert;
  Sortierung:=C_DefaultSortierung;;
end;

end.

