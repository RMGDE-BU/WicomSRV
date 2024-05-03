{******************************************************************************}
{* Unit: Liste für Kommt/Geht-Abgleich von Meldungen                          *}
{* 02.07.2001 WW                                                              *}
{* 25.09.2001 WW Fehler in Funktion KommtMeldungFound bei offenen MRG-Geht-   *}
{*               Meldungen behoben                                            *}
{* 05.03.2002 WW erweitert: Meldungen für alle Instanzen einer DSfG-Station   *}
{******************************************************************************}
Unit MeldAbgleichListe;

interface

uses
  Classes, SysUtils, Dialogs, DBTables, WTables, WSysCon, MeldKonfigDb, MDbSta,
  DDbSta, MeldFilterIni;

type

  { Objekt für TMeldAbgleichList }

  TMeldAbgleichListObj = class(TObject)
    Aktiv: boolean;
    MeldungId_K: integer;
    MeldungId_G: integer;
    GeraeteArt: string [szLen_GeraeteArt];
    GeraeteId: integer;
    OrdnungsNr_K: integer;
    OrdnungsNr_G: integer;
    DatumZeit_K: TDateTime;
    DatumZeit_G: TDateTime;
    Zeitzone_K: string [szLen_Zeitzone];
    Zeitzone_G: string [szLen_Zeitzone];
    MNrAllg_K: string [szLen_MNrAllg];
    MNrAllg_G: string [szLen_MNrAllg];
    MNrGeraet_K: string [szLen_MNrGeraet];
    MNrGeraet_G: string [szLen_MNrGeraet];
    MText: string [szLen_MText];
    MTyp_K: string [szLen_MTyp];
    MTyp_G: string [szLen_MTyp];
    MArt_K: string [szLen_MArt];
    MArt_G: string [szLen_MArt];
    Quittiert: boolean;
    Bemerkung_K: string [szLen_Bemerkung];
    Bemerkung_G: string [szLen_Bemerkung];
    DSfG_Status_K: string [szLen_DSfGStatus];
    DSfG_Status_G: string [szLen_DSfGStatus];
    DSfG_CRC_K: string [szLen_DSfGCRC];
    DSfG_CRC_G: string [szLen_DSfGCRC];
    Para_Text: string [szLen_ParaText];
    Para_WertAlt: string [szLen_ParaWert];
    Para_WertNeu: string [szLen_ParaWert];
    Stationstext1: string [40];
    Stationstext2: string [40];
  public
    constructor Create (AAktiv: boolean;
                        AMeldungId_K: integer;
                        AMeldungId_G: integer;
                        AGeraeteArt: string;
                        AGeraeteId: integer;
                        AOrdnungsNr_K: integer;
                        AOrdnungsNr_G: integer;
                        ADatumZeit_K: TDateTime;
                        ADatumZeit_G: TDateTime;
                        AZeitzone_K: string;
                        AZeitzone_G: string;
                        AMNrAllg_K: string;
                        AMNrAllg_G: string;
                        AMNrGeraet_K: string;
                        AMNrGeraet_G: string;
                        AMText: string;
                        AMTyp_K: string;
                        AMTyp_G: string;
                        AMArt_K: string;
                        AMArt_G: string;
                        AQuittiert: boolean;
                        ABemerkung_K: string;
                        ABemerkung_G: string;
                        ADSfG_Status_K: string;
                        ADSfG_Status_G: string;
                        ADSfG_CRC_K: string;
                        ADSfG_CRC_G: string;
                        APara_Text: string;
                        APara_WertAlt: string;
                        APara_WertNeu: string;
                        AStationstext1: string;
                        AStationstext2: string);
  end;


  { Kommt/Geht-Abgleichliste für MRG- und DSfG-Meldungen }

  TMeldAbgleichList = class(TList)
  private
    Path: string;
    GeraeteArt: string;
    GeraeteId: integer;
    MeldFilterIniFile: TMeldFilterIniFile;
    procedure ClearAll;
    function KommtMeldungFound (AGeraeteArt: string;
                                AGeraeteId: integer;
                                AMNrAllg_G: string;
                                var ListIndex: integer): boolean;
    procedure SetStationsnamen;
  public
    constructor Create (APath: string; AIniFileName: string;
                        AGeraeteArt: string; AGeraeteId: integer);
    Destructor Destroy; override;
    procedure Fuellen_und_Abgleichen (Query: TQueryExt);
    procedure SetFilterundSortierung;
    function GetAnzahlAktiv: integer;
    function GetAnzahlAktiveEinzelmeldungen: integer;
    function GetAktiv (Index: integer): boolean;
    function GetMeldungId_K (Index: integer): integer;
    function GetMeldungId_G (Index: integer): integer;
    function GetGeraeteArt (Index: integer): string;
    function GetGeraeteId (Index: integer): integer;
    function GetOrdnungsNr_K (Index: integer): integer;
    function GetOrdnungsNr_G (Index: integer): integer;
    function GetDatumZeit_K (Index: integer): TDateTime;
    function GetDatumZeit_G (Index: integer): TDateTime;
    function GetZeitzone_K (Index: integer): string;
    function GetZeitzone_G (Index: integer): string;
    function GetMNrAllg_K (Index: integer): string;
    function GetMNrAllg_G (Index: integer): string;
    function GetMNrGeraet_K (Index: integer): string;
    function GetMNrGeraet_G (Index: integer): string;
    function GetMText (Index: integer): string;
    function GetMTyp_K (Index: integer): string;
    function GetMTyp_G (Index: integer): string;
    function GetMArt_K (Index: integer): string;
    function GetMArt_G (Index: integer): string;
    function GetQuittiert (Index: integer): boolean;
    function GetBemerkung_K (Index: integer): string;
    function GetBemerkung_G (Index: integer): string;
    function GetDSfG_Status_K (Index: integer): string;
    function GetDSfG_Status_G (Index: integer): string;
    function GetDSfG_CRC_K (Index: integer): string;
    function GetDSfG_CRC_G (Index: integer): string;
    function GetPara_Text (Index: integer): string;
    function GetPara_WertAlt (Index: integer): string;
    function GetPara_WertNeu (Index: integer): string;
    function GetStationstext1 (Index: integer): string;
    function GetStationstext2 (Index: integer): string;
    procedure SetAktiv (Index: integer; Aktiv: boolean);
    procedure SetMeldungId_G (Index: integer; MeldungId: integer);
    procedure SetOrdnungsNr_G (Index: integer; OrdNr: integer);
    procedure SetDatumZeit_G (Index: integer; DatumZeit: TDateTime);
    procedure SetZeitzone_G (Index: integer; Zeitzone: string);
    procedure SetMNrAllg_G (Index: integer; MNrAllg: string);
    procedure SetMNrGeraet_G (Index: integer; MNrGeraet: string);
    procedure SetMTyp_G (Index: integer; MTyp: string);
    procedure SetMArt_G (Index: integer; MArt: string);
    procedure SetQuittiert (Index: integer; Quittiert: boolean);
    procedure SetDSfG_Status_G (Index: integer; DSfG_Status: string);
    procedure SetDSfG_CRC_G (Index: integer; DSfG_CRC: string);
    procedure SetBemerkung_K (Index: integer; Bemerkung: string);
    procedure SetBemerkung_G (Index: integer; Bemerkung: string);
    procedure SetStationstext1 (Index: integer; ST1: string);
    procedure SetStationstext2 (Index: integer; ST2: string);
  end;

implementation


{ Vergleichsfunktionen zur Sortierung der Abgleichliste }

{----------------------------------------------------------------------------------}
function Aktiv_DatumZeit_Station_MeldungId_Compare (Item1, Item2: Pointer): integer;
{----------------------------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TMeldAbgleichList-Objekten nach:
  1. Aktiv (erst aktive, dann inaktive)
  2. Datum/Zeit (aufsteigend)
  3. Station (aufsteigend)
  4. MeldungId (aufsteigend) }
var
  MeldungId_1: integer;
  MeldungId_2: integer;
  DatumZeit_1: TDateTime;
  DatumZeit_2: TDateTime;

begin
  { Kommt-MeldungId bzw. Geht-MeldungId für Sortierung bestimmen: }
  if TMeldAbgleichListObj (Item1).MeldungId_K > -1 then
    MeldungId_1:=TMeldAbgleichListObj (Item1).MeldungId_K
  else
    MeldungId_1:=TMeldAbgleichListObj (Item1).MeldungId_G;

  if TMeldAbgleichListObj (Item2).MeldungId_K > -1 then
    MeldungId_2:=TMeldAbgleichListObj (Item2).MeldungId_K
  else
    MeldungId_2:=TMeldAbgleichListObj (Item2).MeldungId_G;

  { Kommt-Datum/Zeit bzw. Geht-Datum/Zeit für Sortierung bestimmen: }
  if TMeldAbgleichListObj (Item1).DatumZeit_K > -1 then
    DatumZeit_1:=TMeldAbgleichListObj (Item1).DatumZeit_K
  else
    DatumZeit_1:=TMeldAbgleichListObj (Item1).DatumZeit_G;

  if TMeldAbgleichListObj (Item2).DatumZeit_K > -1 then
    DatumZeit_2:=TMeldAbgleichListObj (Item2).DatumZeit_K
  else
    DatumZeit_2:=TMeldAbgleichListObj (Item2).DatumZeit_G;

  { Sortier-Algorithmus: }
  { aktiv }
  if TMeldAbgleichListObj (Item1).Aktiv = TMeldAbgleichListObj (Item2).Aktiv then begin
    { Datum/Zeit }
    if DatumZeit_1 = DatumZeit_2 then begin
      { Station }
      if TMeldAbgleichListObj (Item1).Stationstext1 + TMeldAbgleichListObj (Item1).Stationstext2 =
         TMeldAbgleichListObj (Item2).Stationstext1 + TMeldAbgleichListObj (Item2).Stationstext2 then begin
        { MeldungId }
        if MeldungId_1 = MeldungId_2 then
          Result:=0
        else if MeldungId_1 > MeldungId_2 then
          Result:=1
        else
          Result:=-1;
      end
      else if TMeldAbgleichListObj (Item1).Stationstext1 + TMeldAbgleichListObj (Item1).Stationstext2 >
              TMeldAbgleichListObj (Item2).Stationstext1 + TMeldAbgleichListObj (Item2).Stationstext2 then
        Result:=1
      else
        Result:=-1;
    end
    else if DatumZeit_1 > DatumZeit_2 then
      Result:=1
    else
      Result:=-1;
  end
  else if TMeldAbgleichListObj (Item1).Aktiv then
    Result:=-1
  else
    Result:=1;
end;

{----------------------------------------------------------------------------------}
function Aktiv_Station_DatumZeit_MeldungId_Compare (Item1, Item2: Pointer): integer;
{----------------------------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TMeldAbgleichList-Objekten nach:
  1. Aktiv (erst aktive, dann inaktive)
  2. Station (aufsteigend)
  3. Datum/Zeit (aufsteigend)
  4. MeldungId (aufsteigend) }
var
  MeldungId_1: integer;
  MeldungId_2: integer;
  DatumZeit_1: TDateTime;
  DatumZeit_2: TDateTime;

begin
  { Kommt-MeldungId bzw. Geht-MeldungId für Sortierung bestimmen: }
  if TMeldAbgleichListObj (Item1).MeldungId_K > -1 then
    MeldungId_1:=TMeldAbgleichListObj (Item1).MeldungId_K
  else
    MeldungId_1:=TMeldAbgleichListObj (Item1).MeldungId_G;

  if TMeldAbgleichListObj (Item2).MeldungId_K > -1 then
    MeldungId_2:=TMeldAbgleichListObj (Item2).MeldungId_K
  else
    MeldungId_2:=TMeldAbgleichListObj (Item2).MeldungId_G;

  { Kommt-Datum/Zeit bzw. Geht-Datum/Zeit für Sortierung bestimmen: }
  if TMeldAbgleichListObj (Item1).DatumZeit_K > -1 then
    DatumZeit_1:=TMeldAbgleichListObj (Item1).DatumZeit_K
  else
    DatumZeit_1:=TMeldAbgleichListObj (Item1).DatumZeit_G;

  if TMeldAbgleichListObj (Item2).DatumZeit_K > -1 then
    DatumZeit_2:=TMeldAbgleichListObj (Item2).DatumZeit_K
  else
    DatumZeit_2:=TMeldAbgleichListObj (Item2).DatumZeit_G;

  { Sortier-Algorithmus: }
  { aktiv }
  if TMeldAbgleichListObj (Item1).Aktiv = TMeldAbgleichListObj (Item2).Aktiv then begin
    { Station }
    if TMeldAbgleichListObj (Item1).Stationstext1 + TMeldAbgleichListObj (Item1).Stationstext2 =
       TMeldAbgleichListObj (Item2).Stationstext1 + TMeldAbgleichListObj (Item2).Stationstext2 then begin
      { Datum/Zeit }
      if DatumZeit_1 = DatumZeit_2 then begin
        { MeldungId }
        if MeldungId_1 = MeldungId_2 then
          Result:=0
        else if MeldungId_1 > MeldungId_2 then
          Result:=1
        else
          Result:=-1;
      end
      else if DatumZeit_1 > DatumZeit_2 then
        Result:=1
      else
        Result:=-1;
    end
    else if TMeldAbgleichListObj (Item1).Stationstext1 + TMeldAbgleichListObj (Item1).Stationstext2 >
            TMeldAbgleichListObj (Item2).Stationstext1 + TMeldAbgleichListObj (Item2).Stationstext2 then
      Result:=1
    else
      Result:=-1;
  end
  else if TMeldAbgleichListObj (Item1).Aktiv then
    Result:=-1
  else
    Result:=1;
end;

{-----------------------------------------------------------------------------------}
function Aktiv_Station_OrdnungsNr_MeldungId_Compare (Item1, Item2: Pointer): integer;
{-----------------------------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TMeldAbgleichList-Objekten nach:
  1. Aktiv (erst aktive, dann inaktive)
  2. Station (aufsteigend)
  3. Ordnungsnummer (aufsteigend)
  4. MeldungId (aufsteigend) }
var
  MeldungId_1: integer;
  MeldungId_2: integer;
  OrdNr_1: TDateTime;
  OrdNr_2: TDateTime;
begin
  { Kommt-MeldungId bzw. Geht-MeldungId für Sortierung bestimmen: }
  if TMeldAbgleichListObj (Item1).MeldungId_K > -1 then
    MeldungId_1:=TMeldAbgleichListObj (Item1).MeldungId_K
  else
    MeldungId_1:=TMeldAbgleichListObj (Item1).MeldungId_G;

  if TMeldAbgleichListObj (Item2).MeldungId_K > -1 then
    MeldungId_2:=TMeldAbgleichListObj (Item2).MeldungId_K
  else
    MeldungId_2:=TMeldAbgleichListObj (Item2).MeldungId_G;

  { Kommt-Ordnungsnummer bzw. Geht-Ordnungsnummer für Sortierung bestimmen: }
  if TMeldAbgleichListObj (Item1).OrdnungsNr_K > -1 then
    OrdNr_1:=TMeldAbgleichListObj (Item1).OrdnungsNr_K
  else
    OrdNr_1:=TMeldAbgleichListObj (Item1).OrdnungsNr_G;

  if TMeldAbgleichListObj (Item2).OrdnungsNr_K > -1 then
    OrdNr_2:=TMeldAbgleichListObj (Item2).OrdnungsNr_K
  else
    OrdNr_2:=TMeldAbgleichListObj (Item2).OrdnungsNr_G;

  { Sortier-Algorithmus: }
  { aktiv }
  if TMeldAbgleichListObj (Item1).Aktiv = TMeldAbgleichListObj (Item2).Aktiv then begin
    { Station }
    if TMeldAbgleichListObj (Item1).Stationstext1 + TMeldAbgleichListObj (Item1).Stationstext2 =
       TMeldAbgleichListObj (Item2).Stationstext1 + TMeldAbgleichListObj (Item2).Stationstext2 then begin
      { Ordnungsnummer }
      if OrdNr_1 = OrdNr_2 then begin
        { MeldungId }
        if MeldungId_1 = MeldungId_2 then
          Result:=0
        else if MeldungId_1 > MeldungId_2 then
          Result:=1
        else
          Result:=-1;
      end
      else if OrdNr_1 > OrdNr_2 then
        Result:=1
      else
        Result:=-1;
    end
    else if TMeldAbgleichListObj (Item1).Stationstext1 + TMeldAbgleichListObj (Item1).Stationstext2 >
            TMeldAbgleichListObj (Item2).Stationstext1 + TMeldAbgleichListObj (Item2).Stationstext2 then
      Result:=1
    else
      Result:=-1;
  end
  else if TMeldAbgleichListObj (Item1).Aktiv then
    Result:=-1
  else
    Result:=1;
end;


{------------------------------------------------------------------------}
function Aktiv_Station_MeldungId_Compare (Item1, Item2: Pointer): integer;
{------------------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TMeldAbgleichList-Objekten nach:
  1. Aktiv (erst aktive, dann inaktive)
  2. Station (aufsteigend)
  3. MeldungId (aufsteigend) }
var
  MeldungId_1: integer;
  MeldungId_2: integer;

begin
  { Kommt-MeldungId bzw. Geht-MeldungId für Sortierung bestimmen: }
  if TMeldAbgleichListObj (Item1).MeldungId_K > -1 then
    MeldungId_1:=TMeldAbgleichListObj (Item1).MeldungId_K
  else
    MeldungId_1:=TMeldAbgleichListObj (Item1).MeldungId_G;

  if TMeldAbgleichListObj (Item2).MeldungId_K > -1 then
    MeldungId_2:=TMeldAbgleichListObj (Item2).MeldungId_K
  else
    MeldungId_2:=TMeldAbgleichListObj (Item2).MeldungId_G;

  { Sortier-Algorithmus: }
  { aktiv }
  if TMeldAbgleichListObj (Item1).Aktiv = TMeldAbgleichListObj (Item2).Aktiv then begin
    { Station }
    if TMeldAbgleichListObj (Item1).Stationstext1 + TMeldAbgleichListObj (Item1).Stationstext2 =
       TMeldAbgleichListObj (Item2).Stationstext1 + TMeldAbgleichListObj (Item2).Stationstext2 then begin
      { MeldungId }
      if MeldungId_1 = MeldungId_2 then
        Result:=0
      else if MeldungId_1 > MeldungId_2 then
        Result:=1
      else
        Result:=-1;
    end
    else if TMeldAbgleichListObj (Item1).Stationstext1 + TMeldAbgleichListObj (Item1).Stationstext2 >
            TMeldAbgleichListObj (Item2).Stationstext1 + TMeldAbgleichListObj (Item2).Stationstext2 then
      Result:=1
    else
      Result:=-1;
  end
  else if TMeldAbgleichListObj (Item1).Aktiv then
    Result:=-1
  else
    Result:=1;
end;


{ TMeldAbgleichListObj }

{---------------------------------------------------------------}
constructor TMeldAbgleichListObj.Create (AAktiv: boolean;
                                         AMeldungId_K: integer;
                                         AMeldungId_G: integer;
                                         AGeraeteArt: string;
                                         AGeraeteId: integer;
                                         AOrdnungsNr_K: integer;
                                         AOrdnungsNr_G: integer;
                                         ADatumZeit_K: TDateTime;
                                         ADatumZeit_G: TDateTime;
                                         AZeitzone_K: string;
                                         AZeitzone_G: string;
                                         AMNrAllg_K: string;
                                         AMNrAllg_G: string;
                                         AMNrGeraet_K: string;
                                         AMNrGeraet_G: string;
                                         AMText: string;
                                         AMTyp_K: string;
                                         AMTyp_G: string;
                                         AMArt_K: string;
                                         AMArt_G: string;
                                         AQuittiert: boolean;
                                         ABemerkung_K: string;
                                         ABemerkung_G: string;
                                         ADSfG_Status_K: string;
                                         ADSfG_Status_G: string;
                                         ADSfG_CRC_K: string;
                                         ADSfG_CRC_G: string;
                                         APara_Text: string;
                                         APara_WertAlt: string;
                                         APara_WertNeu: string;
                                         AStationstext1: string;
                                         AStationstext2: string);
{---------------------------------------------------------------}
begin
  inherited Create;
  Aktiv:=AAktiv;
  MeldungId_K:=AMeldungId_K;
  MeldungId_G:=AMeldungId_G;
  GeraeteArt:=AGeraeteArt;
  GeraeteId:=AGeraeteId;
  OrdnungsNr_K:=AOrdnungsNr_K;
  OrdnungsNr_G:=AOrdnungsNr_G;
  DatumZeit_K:=ADatumZeit_K;
  DatumZeit_G:=ADatumZeit_G;
  Zeitzone_K:=AZeitzone_K;
  Zeitzone_G:=AZeitzone_G;
  MNrAllg_K:=AMNrAllg_K;
  MNrAllg_G:=AMNrAllg_G;
  MNrGeraet_K:=AMNrGeraet_K;
  MNrGeraet_G:=AMNrGeraet_G;
  MText:=AMText;
  MTyp_K:=AMTyp_K;
  MTyp_G:=AMTyp_G;
  MArt_K:=AMArt_K;
  MArt_G:=AMArt_G;
  Quittiert:=AQuittiert;
  Bemerkung_K:=ABemerkung_K;
  Bemerkung_G:=ABemerkung_G;
  DSfG_Status_K:=ADSfG_Status_K;
  DSfG_Status_G:=ADSfG_Status_G;
  DSfG_CRC_K:=ADSfG_CRC_K;
  DSfG_CRC_G:=ADSfG_CRC_G;
  Para_Text:=APara_Text;
  Para_WertAlt:=APara_WertAlt;
  Para_WertNeu:=APara_WertNeu;
  Stationstext1:=AStationstext1;
  Stationstext2:=AStationstext2;
end;


{ TMeldAbgleichList }

{------------------------------------------------------------------------------}
constructor TMeldAbgleichList.Create (APath: string; AIniFileName: string;
                                      AGeraeteArt: string; AGeraeteId: integer);
{------------------------------------------------------------------------------}
{ Constructor für Meldungs-Abgleichliste;
  Übergabe: APath (Pfad für Meldungstabellen und Stammdaten)
            AIniFileName
            AGeraeteArt ('M' = nur MRG-Geräte; 'D' = nur DSfG-Geräte; '' = beide)
            AGeraeteId (MRG-Station-Id bzw. DSfG-Quell-InstanzId; <=0 -> "alle" ) }
begin
  inherited Create;
  Path:=APath;
  GeraeteArt:=AGeraeteArt;
  GeraeteId:=AGeraeteId;
  MeldFilterIniFile:=TMeldFilterIniFile.Create (AIniFileName);
end;

{-----------------------------------}
Destructor TMeldAbgleichList.Destroy;
{-----------------------------------}
begin
  MeldFilterIniFile.Free;
  ClearAll;
  inherited Destroy;
end;

{-----------------------------------}
procedure TMeldAbgleichList.ClearAll;
{-----------------------------------}
var
  i: integer;
Begin
  for i:=0 to Count-1 do
    TMeldAbgleichListObj (Items [i]).Free;
  Clear;
end;

{--------------------------------------------------------------------}
procedure TMeldAbgleichList.Fuellen_und_Abgleichen (Query: TQueryExt);
{--------------------------------------------------------------------}
{ Liste aus Meldungs-Query füllen und Kommt-Meldungen mit den zugehörigen
  Geht-Meldungen abgleichen
  Übergabe: aus TMeldungenDb.GetMeldungen erzeugte Query }
var
  AddEintrag: boolean;
  ListIndex: integer;
  MeldAbgleichListObj: TMeldAbgleichListObj;
  OrdNr: integer;

begin
  ClearAll;
  if Query.Active then begin
    while not Query.Eof do begin
      AddEintrag:=true;
      { Ordnungsnummern existieren nur bei DSfG-Meldungen: }
      if not Query.FieldByName(C_Tf_WMeldungen_OrdnungsNr).isNull then
        OrdNr:=Query.FieldByName(C_Tf_WMeldungen_OrdnungsNr).asInteger
      else
        OrdNr:=-1;

      if Query.FieldByName(C_Tf_WMeldungen_MTyp).asString = mtyp_geht then begin
        { Abgleich: zur Geht-Meldung gehörigen Kommt-Eintrag in der Liste finden }
        if KommtMeldungFound (Query.FieldByName(C_Tf_WMeldungen_GeraeteArt).asString,
                              Query.FieldByName(C_Tf_WMeldungen_GeraeteId).asInteger,
                              Query.FieldByName(C_Tf_WMeldungen_MNrAllg).asString,
                              ListIndex) then begin
          { Geht-Daten in bestehenden Listeneintrag mit Kommt-Meldung eintragen: }
          SetMeldungId_G (ListIndex, Query.FieldByName(C_Tf_WMeldungen_MeldungId).asInteger);
          SetOrdnungsNr_G (ListIndex, OrdNr);
          SetDatumZeit_G (ListIndex, Query.FieldByName(C_Tf_WMeldungen_DatumZeit).asDateTime);
          SetZeitzone_G (ListIndex, Query.FieldByName(C_Tf_WMeldungen_Zeitzone).asString);
          SetMNrAllg_G (ListIndex, Query.FieldByName(C_Tf_WMeldungen_MNrAllg).asString);
          SetMNrGeraet_G (ListIndex, Query.FieldByName(C_Tf_WMeldungen_MNrGeraet).asString);
          SetMTyp_G (ListIndex, Query.FieldByName(C_Tf_WMeldungen_MTyp).asString);
          SetMArt_G (ListIndex, Query.FieldByName(C_Tf_WMeldungen_MArt).asString);
          SetDSfG_Status_G (ListIndex, Query.FieldByName(C_Tf_WMeldungenDSfG_Status).asString);
          SetDSfG_CRC_G (ListIndex, Query.FieldByName(C_Tf_WMeldungenDSfG_CRC).asString);
          SetBemerkung_G (ListIndex, Query.FieldByName(C_Tf_WMeldungen_Bemerkung).asString);
          { Quittierung zurücksetzen: }
          SetQuittiert (ListIndex, Query.FieldByName(C_Tf_WMeldungen_Quittiert).asBoolean);
          AddEintrag:=false;
        end;
      end;

      if AddEintrag then begin                          { neuer Eintrag }
        if Query.FieldByName(C_Tf_WMeldungen_MTyp).asString <> mtyp_geht then
          { für einwertige, Kommt- und unbestimmte Meldungen: }
          MeldAbgleichListObj:=TMeldAbgleichListObj.Create (
            true,
            Query.FieldByName(C_Tf_WMeldungen_MeldungId).asInteger,
            -1,
            Query.FieldByName(C_Tf_WMeldungen_GeraeteArt).asString,
            Query.FieldByName(C_Tf_WMeldungen_GeraeteId).asInteger,
            OrdNr,
            -1,
            Query.FieldByName(C_Tf_WMeldungen_DatumZeit).asDateTime,
            -1,
            Query.FieldByName(C_Tf_WMeldungen_Zeitzone).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungen_MNrAllg).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungen_MNrGeraet).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungen_MText).asString,
            Query.FieldByName(C_Tf_WMeldungen_MTyp).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungen_MArt).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungen_Quittiert).asBoolean,
            Query.FieldByName(C_Tf_WMeldungen_Bemerkung).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungenDSfG_Status).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungenDSfG_CRC).asString,
            '',
            Query.FieldByName(C_TfParameterText).asString,
            Query.FieldByName(C_Tf_WMeldungenPara_WertAlt).asString,
            Query.FieldByName(C_Tf_WMeldungenPara_WertNeu).asString,
            '',
            '')
        else
          { für Geht-Meldungen: }
          MeldAbgleichListObj:=TMeldAbgleichListObj.Create (
            true,
            -1,
            Query.FieldByName(C_Tf_WMeldungen_MeldungId).asInteger,
            Query.FieldByName(C_Tf_WMeldungen_GeraeteArt).asString,
            Query.FieldByName(C_Tf_WMeldungen_GeraeteId).asInteger,
            -1,
            OrdNr,
            -1,
            Query.FieldByName(C_Tf_WMeldungen_DatumZeit).asDateTime,
            '',
            Query.FieldByName(C_Tf_WMeldungen_Zeitzone).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungen_MNrAllg).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungen_MNrGeraet).asString,
            Query.FieldByName(C_Tf_WMeldungen_MText).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungen_MTyp).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungen_MArt).asString,
            Query.FieldByName(C_Tf_WMeldungen_Quittiert).asBoolean,
            '',
            Query.FieldByName(C_Tf_WMeldungen_Bemerkung).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungenDSfG_Status).asString,
            '',
            Query.FieldByName(C_Tf_WMeldungenDSfG_CRC).asString,
            Query.FieldByName(C_TfParameterText).asString,
            Query.FieldByName(C_Tf_WMeldungenPara_WertAlt).asString,
            Query.FieldByName(C_Tf_WMeldungenPara_WertNeu).asString,
            '',
            '');

        Add (MeldAbgleichListObj);
      end;
      Query.Next;
    end;  { while }

    SetStationsnamen;                  { Stationsnamen in die Liste eintragen }
  end;
end;

{-----------------------------------------------------------------------------}
function TMeldAbgleichList.KommtMeldungFound (AGeraeteArt: string;
                                              AGeraeteId: integer;
                                              AMNrAllg_G: string;
                                              var ListIndex: integer): boolean;
{-----------------------------------------------------------------------------}
{ zur übergebenen Geht-Meldungsnummer den Listenindex der zugehörigen Kommt-Meldung
  ermitteln; die Suche erfolgt über die allgemeine Meldungsnummer !
  Übergabe: AGeraeteArt
            AGeraeteId
            allg. Meldungsnummer der Geht-Meldung
  Rückgabe: ListIndex der zugehörigen Kommt-Meldung
  Ergebnis: true, wenn zugehörige Kommt-Meldung gefunden wurde }
var
  i: integer;
  K_NrAllg: integer;
  G_NrAllg: integer;

begin
  Result:=false;
  ListIndex:=-1;
  { wenn allgemeine Meldungsnummer fehlt: kein Abgleich }
  if length (AMNrAllg_G) = 0 then exit;

  { Abgleich: zur Geht-Meldung gehörigen Kommt-Eintrag in der Liste finden }
  for i:=Count-1 downto 0 do begin
    { "Filter" auf Listeneinträge der Station: }
    if (GetGeraeteArt (i) = AGeraeteArt) AND (GetGeraeteId (i) = AGeraeteId) then begin
      { Abgleich bei MRG-Meldungen: die allg. Meldungsnummer der Kommt-Meldung
        ist immer um eins niedriger als die der zugehörigen Geht-Meldung }
      if AGeraeteArt = C_GerArtMrg then begin
        if GetMNrAllg_G (i) = AMNrAllg_G then
          { um zu verhindern, daß mehrere aufeinanderfolgende gleiche Geht-Meldungen
            (Gerätefehler EC694) eine Kommt-Meldung mehrmals abgleichen }
          Break
        else begin
          if (length (GetMNrAllg_K (i)) > 0) AND (length (AMNrAllg_G) > 0) then begin
            K_NrAllg:=StrToInt (GetMNrAllg_K (i));
            G_NrAllg:=StrToInt (AMNrAllg_G);
            if K_NrAllg = (G_NrAllg - 1) then begin
              if GetMTyp_K (i) <> mtyp_geht then begin
                { Nicht-Geht-Meldung gefunden: Abgleich-Listenindex setzen }
                ListIndex:=i;
                Result:=true;
              end;
              Break;
            end;
          end;
        end;
      end

      { Abgleich bei DSfG-Meldungen: die Kommt-Meldung und die zugehörige Geht-Meldung
        tragen die gleiche allg. Meldungsnummer }
      else if AGeraeteArt = C_GerArtDSfG then begin
        { um zu verhindern, daß mehrere aufeinanderfolgende gleiche Geht-Meldungen
          (Gerätefehler 9004) eine Kommt-Meldung mehrmals abgleichen: }
        if GetMNrAllg_G (i) = AMNrAllg_G then
          Break
        else if GetMNrAllg_K (i) = AMNrAllg_G then begin
          if GetMTyp_K (i) <> mtyp_geht then begin
            { Nicht-Geht-Meldung gefunden: Abgleich-Listenindex setzen }
            ListIndex:=i;
            Result:=true;
          end;
          Break;
        end;
      end else
        Break;
    end;
  end;  { for }
end;

{-------------------------------------------}
procedure TMeldAbgleichList.SetStationsnamen;
{-------------------------------------------}
{ zu jeder Meldung aus der GeraeteId den Stationsnamen und Kennung (MRG) bzw.
  den Stationsnamen und Instanznamen (DSfG) ermitteln und in die Liste eintragen }
var
  i: integer;
  MRGStammdaten: TMRGStammdaten;
  DSfGStammdaten: TDSfGStammdaten;
  Query: TQuery;
  OK: boolean;

begin
  if Geraeteart <> C_GerArtDSfG then begin
    { MRG-Stationsnamen und Kennungen eintragen: }
    OK:=true;
    MRGStammdaten:=TMRGStammdaten.Create (Path);
    try
      if MRGStammdaten.InitTabellen then begin
        Query:=TQuery.Create (nil);
        try
          MRGStammdaten.GetStationsnamenKennungen (Query);
          while not Query.Eof do begin
            for i:=0 to Count-1 do begin
              if (GetGeraeteArt (i) = C_GerArtMrg) AND
                 (GetGeraeteId (i) = Query.FieldByName (C_Sta_MrgId).AsInteger) then begin
                SetStationstext1 (i, Query.FieldByName (C_Sta_Stationsname).AsString); { Stationsname }
                SetStationstext2 (i, Query.FieldByName (C_Sta_Kennung).AsString);      { Kennung }
              end;
            end;
            Query.Next;
          end;  { while }
        finally
          Query.Close;
          Query.Free;
        end;
      end else  { if InitTabellen }
        OK:=false;
    finally
      MRGStammdaten.Free;
    end;
    if not OK then
      MessageDlg ('Fehler beim Zugriff auf MRG-Stammdaten aufgetreten !', mtError, [mbOk], 0);
  end;

  if Geraeteart <> C_GerArtMrG then begin
    { DSfG-Stationsnamen und Instanznamen eintragen: }
    OK:=true;
    DSfGStammdaten:=TDSfGStammdaten.Create (Path);
    try
      if DSfGStammdaten.InitTabellen then begin
        Query:=TQuery.Create (nil);
        try
          DSfGStammdaten.GetStationsInstanznamen (Query);
          while not Query.Eof do begin
            for i:=0 to Count-1 do begin
              if (GetGeraeteArt (i) = C_GerArtDSfG) AND
                 (GetGeraeteId (i) = Query.FieldByName (C_DTF_Instanz_InstanzId).AsInteger) then begin
                SetStationstext1 (i, Query.FieldByName (C_DTF_Station_Stationsname).AsString); { Stationsname }
                SetStationstext2 (i, Query.FieldByName (C_DTF_Instanz_Instanzname).AsString); { Instanzname }
              end;
            end;
            Query.Next;
          end;  { while }
        finally
          Query.Close;
          Query.Free;
        end;
      end else  { if InitTabellen }
        OK:=false;
    finally
      DSfGStammdaten.Free;
    end;
    if not OK then
      MessageDlg ('Fehler beim Zugriff auf DSfG-Stammdaten aufgetreten !', mtError, [mbOk], 0);
  end;
end;

{-------------------------------------------------}
procedure TMeldAbgleichList.SetFilterundSortierung;
{-------------------------------------------------}
{ Listeneinträge anhand der Meldungs-Filterkriterien aktiv oder inaktiv setzen und
  sortieren}
var
  i, j: integer;
  isFilterDatumZeit: boolean;
  isFilterDatumVon: integer;
  isFilterDatumBis: integer;
  isFilterMRG: boolean;
  isFilterDSfG: boolean;
  FilterMRGStation: integer;
  isFilterHinweis: boolean;
  isFilterWarnung: boolean;
  isFilterStoerung: boolean;
  isFilterRechnerfehler: boolean;
  isFilterQuittiert: boolean;
  isFilterNichtQuittiert: boolean;
  Aktiv: boolean;
  Datum: integer;
  MArt: string;
  DSfGInstanzIdList: TList;
  doFillInstanzIdList: boolean;
  DSfGStammdaten: TDSfGStammdaten;
  Query: TQuery;
  OK: boolean;
  gefunden: boolean;

begin
  DSfGInstanzIdList:=TList.Create;
  try
    doFillInstanzIdList:=false;
    if GeraeteArt = C_GerArtMrg then begin                     { nur MRG-Geräte }
      isFilterMRG:=true;
      isFilterDSfG:=false;
      if GeraeteId > 0 then
        FilterMRGStation:=GeraeteId
      else
        FilterMRGStation:=MeldFilterIniFile.FilterMRGStation;
    end
    else if GeraeteArt = C_GerArtDSfG then begin              { nur DSfG-Geräte }
      isFilterDSfG:=true;
      isFilterMRG:=false;
      if GeraeteId > 0 then
        DSfGInstanzIdList.Add (Pointer (GeraeteId))
      else
        doFillInstanzIdList:=true;
      FilterMRGStation:=0;
    end
    else begin                                           { MRG- und DSfG-Geräte }
      isFilterMRG:=MeldFilterIniFile.FilterMRG;
      isFilterDSfG:=MeldFilterIniFile.FilterDSfG;
      FilterMRGStation:=MeldFilterIniFile.FilterMRGStation;
      doFillInstanzIdList:=true;
    end;

    if doFillInstanzIdList then begin
      if MeldFilterIniFile.FilterDSfGStation > 0 then begin
        { InstanzId-Liste mit allen InstanzId's der Station füllen: }
        OK:=true;
        DSfGStammdaten:=TDSfGStammdaten.Create (Path);
        try
          if DSfGStammdaten.InitTabellen then begin
            Query:=TQuery.Create (nil);
            try
              DSfGStammDaten.GetInstanzQuery (MeldFilterIniFile.FilterDSfGStation, Query);
              while not Query.Eof do begin
                DSfGInstanzIdList.Add (
                  Pointer (Query.FieldByName (C_DTF_Instanz_InstanzId).AsInteger));
                Query.Next;
              end;  { while }
            finally
              Query.Close;
              Query.Free;
            end;
          end else  { if InitTabellen }
            OK:=false;
        finally
          DSfGStammdaten.Free;
        end;
        if not OK then
          MessageDlg ('Fehler beim Zugriff auf DSfG-Stammdaten aufgetreten !', mtError, [mbOk], 0);
      end

      else if MeldFilterIniFile.FilterDSfGInstanz > 0 then
        { InstanzId-Liste mit einzelner InstanzId füllen: }
        DSfGInstanzIdList.Add (Pointer (MeldFilterIniFile.FilterDSfGInstanz));
    end;

    isFilterDatumZeit:=MeldFilterIniFile.FilterDatumZeit;
    isFilterDatumVon:=MeldFilterIniFile.FilterDatumVon;
    isFilterDatumBis:=MeldFilterIniFile.FilterDatumBis;
    isFilterHinweis:=MeldFilterIniFile.FilterHinweis;
    isFilterWarnung:=MeldFilterIniFile.FilterWarnung;
    isFilterStoerung:=MeldFilterIniFile.FilterStoerung;
    isFilterRechnerfehler:=MeldFilterIniFile.FilterRechnerfehler;
    isFilterQuittiert:=MeldFilterIniFile.FilterQuittiert;
    isFilterNichtQuittiert:=MeldFilterIniFile.FilterNichtQuittiert;

    for i:=0 to Count-1 do begin
      Aktiv:=true;
      { Filter Geräteart, Quittierung: }
      if ((GetGeraeteArt (i) = C_GerArtMrg) AND not isFilterMRG) OR
         ((GetGeraeteArt (i) = C_GerArtDSfG) AND not isFilterDSfG) OR
         ((GetQuittiert (i) = true) AND not isFilterQuittiert) OR
         ((GetQuittiert (i) = false) AND not isFilterNichtQuittiert) then
        Aktiv:=false;
      { Filter MRG-Einzelstation: }
      if (FilterMRGStation > 0) AND
         (GetGeraeteArt (i) = C_GerArtMrg) AND (GetGeraeteId (i) <> FilterMRGStation) then
        Aktiv:=false;
      { Filter DSfG-Einzelinstanz: }
      if (DSfGInstanzIdList.Count > 0) AND
         (GetGeraeteArt (i) = C_GerArtDSfG) then begin
        { GeraeteId der Meldung in Liste suchen: }
        gefunden:=false;
        for j:=0 to DSfGInstanzIdList.Count - 1 do begin
          if integer (DSfGInstanzIdList.Items [j]) = GetGeraeteId (i) then begin
            gefunden:=true;
            Break;
          end;
        end;
        if not gefunden then
          Aktiv:=false;
      end;
      { Filter Datum: }
      if isFilterDatumZeit then begin
        if GetDatumZeit_K (i) > -1 then
          Datum:=trunc (GetDatumZeit_K (i))
        else
          Datum:=trunc (GetDatumZeit_G (i));
        if (Datum < isFilterDatumVon) OR (Datum > isFilterDatumBis) then
          Aktiv:=false;
      end;
      { Filter Meldungsart: }
      if isFilterHinweis OR isFilterWarnung OR isFilterStoerung OR isFilterRechnerfehler then begin
        if length (GetMArt_K (i)) > 0 then
          MArt:=GetMArt_K (i)
        else
          MArt:=GetMArt_G (i);
        if ((MArt = mart_Hinweis) AND not isFilterHinweis) OR
           ((MArt = mart_Warnung) AND not isFilterWarnung) OR
           ((MArt = mart_Stoerung) AND not isFilterStoerung) OR
           ((MArt = mart_Rechnerfehler) AND not isFilterRechnerfehler) then
          Aktiv:=false;
      end;
      SetAktiv (i, Aktiv);
    end;  { for }

    { Sortierung: }
    case MeldFilterIniFile.Sortierung of
      0: Sort (Aktiv_DatumZeit_Station_MeldungId_Compare);
      1: Sort (Aktiv_Station_DatumZeit_MeldungId_Compare);
      2: Sort (Aktiv_Station_OrdnungsNr_MeldungId_Compare);
      3: Sort (Aktiv_Station_MeldungId_Compare);
    end;
  finally
    DSfGInstanzIdList.Free;
  end;
end;

{-------------------------------------------------}
function TMeldAbgleichList.GetAnzahlAktiv: integer;
{-------------------------------------------------}
{ Anzahl der aktiven Listeneinträge (Abgleichmeldungen) zurückgeben }
var
  i: integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    if GetAktiv (i) then
      inc (Result);
end;

{-----------------------------------------------------------------}
function TMeldAbgleichList.GetAnzahlAktiveEinzelmeldungen: integer;
{-----------------------------------------------------------------}
{ Anzahl der aktiven Einzel-Meldungen zurückgeben }
var
  i: integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    if GetAktiv (i) then begin
      if GetMeldungId_K (i) > -1 then
        inc (Result);
      if GetMeldungId_G (i) > -1 then
        inc (Result);
    end;
end;


{-------------- Zugriff-Funktionen für abgeglichene Meldungen -----------------}

{------------------------------------------------------------}
function TMeldAbgleichList.GetAktiv (Index: integer): boolean;
{------------------------------------------------------------}
{ Anzahl der aktiven abgeglichenen Meldungen zurückgeben }
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Aktiv;
end;


{------------- Zugriff-Funktionen für einzelne Objekt-Variablen ---------------}

{------------------------------------------------------------------}
function TMeldAbgleichList.GetMeldungId_K (Index: integer): integer;
{------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MeldungId_K;
end;

{------------------------------------------------------------------}
function TMeldAbgleichList.GetMeldungId_G (Index: integer): integer;
{------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MeldungId_G;
end;

{----------------------------------------------------------------}
function TMeldAbgleichList.GetGeraeteArt (Index: integer): string;
{----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).GeraeteArt;
end;

{----------------------------------------------------------------}
function TMeldAbgleichList.GetGeraeteId (Index: integer): integer;
{----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).GeraeteId;
end;

{-------------------------------------------------------------------}
function TMeldAbgleichList.GetOrdnungsNr_K (Index: integer): integer;
{-------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).OrdnungsNr_K;
end;

{-------------------------------------------------------------------}
function TMeldAbgleichList.GetOrdnungsNr_G (Index: integer): integer;
{-------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).OrdnungsNr_G;
end;

{--------------------------------------------------------------------}
function TMeldAbgleichList.GetDatumZeit_K (Index: integer): TDateTime;
{--------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).DatumZeit_K;
end;

{--------------------------------------------------------------------}
function TMeldAbgleichList.GetDatumZeit_G (Index: integer): TDateTime;
{--------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).DatumZeit_G;
end;

{----------------------------------------------------------------}
function TMeldAbgleichList.GetZeitzone_K (Index: integer): string;
{----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Zeitzone_K;
end;

{----------------------------------------------------------------}
function TMeldAbgleichList.GetZeitzone_G (Index: integer): string;
{----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Zeitzone_G;
end;

{---------------------------------------------------------------}
function TMeldAbgleichList.GetMNrAllg_K (Index: integer): string;
{---------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MNrAllg_K;
end;

{---------------------------------------------------------------}
function TMeldAbgleichList.GetMNrAllg_G (Index: integer): string;
{---------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MNrAllg_G;
end;

{-----------------------------------------------------------------}
function TMeldAbgleichList.GetMNrGeraet_K (Index: integer): string;
{-----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MNrGeraet_K;
end;

{-----------------------------------------------------------------}
function TMeldAbgleichList.GetMNrGeraet_G (Index: integer): string;
{-----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MNrGeraet_G;
end;

{-----------------------------------------------------------}
function TMeldAbgleichList.GetMText (Index: integer): string;
{-----------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MText;
end;

{------------------------------------------------------------}
function TMeldAbgleichList.GetMTyp_K (Index: integer): string;
{------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MTyp_K;
end;

{------------------------------------------------------------}
function TMeldAbgleichList.GetMTyp_G (Index: integer): string;
{------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MTyp_G;
end;

{------------------------------------------------------------}
function TMeldAbgleichList.GetMArt_K (Index: integer): string;
{------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MArt_K;
end;

{------------------------------------------------------------}
function TMeldAbgleichList.GetMArt_G (Index: integer): string;
{------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).MArt_G;
end;

{----------------------------------------------------------------}
function TMeldAbgleichList.GetQuittiert (Index: integer): boolean;
{----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Quittiert;
end;

{-----------------------------------------------------------------}
function TMeldAbgleichList.GetBemerkung_K (Index: integer): string;
{-----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Bemerkung_K;
end;

{-----------------------------------------------------------------}
function TMeldAbgleichList.GetBemerkung_G (Index: integer): string;
{-----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Bemerkung_G;
end;

{-------------------------------------------------------------------}
function TMeldAbgleichList.GetDSfG_Status_K (Index: integer): string;
{-------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).DSfG_Status_K;
end;

{-------------------------------------------------------------------}
function TMeldAbgleichList.GetDSfG_Status_G (Index: integer): string;
{-------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).DSfG_Status_G;
end;

{----------------------------------------------------------------}
function TMeldAbgleichList.GetDSfG_CRC_K (Index: integer): string;
{----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).DSfG_CRC_K;
end;

{----------------------------------------------------------------}
function TMeldAbgleichList.GetDSfG_CRC_G (Index: integer): string;
{----------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).DSfG_CRC_G;
end;

{---------------------------------------------------------------}
function TMeldAbgleichList.GetPara_Text (Index: integer): string;
{---------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Para_Text;
end;

{------------------------------------------------------------------}
function TMeldAbgleichList.GetPara_WertAlt (Index: integer): string;
{------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Para_WertAlt;
end;

{------------------------------------------------------------------}
function TMeldAbgleichList.GetPara_WertNeu (Index: integer): string;
{------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Para_WertNeu;
end;

{-------------------------------------------------------------------}
function TMeldAbgleichList.GetStationstext1 (Index: integer): string;
{-------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Stationstext1;
end;

{-------------------------------------------------------------------}
function TMeldAbgleichList.GetStationstext2 (Index: integer): string;
{-------------------------------------------------------------------}
begin
  Result:=TMeldAbgleichListObj (Items [Index]).Stationstext2;
end;


{--------------------------------------------------------------------}
procedure TMeldAbgleichList.SetAktiv (Index: integer; Aktiv: boolean);
{--------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).Aktiv:=Aktiv;
end;

{------------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetMeldungId_G (Index: integer; MeldungId: integer);
{------------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).MeldungId_G:=MeldungId;
end;

{---------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetOrdnungsNr_G (Index: integer; OrdNr: integer);
{---------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).OrdnungsNr_G:=OrdNr;
end;

{--------------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetDatumZeit_G (Index: integer; DatumZeit: TDateTime);
{--------------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).DatumZeit_G:=DatumZeit;
end;

{---------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetZeitzone_G (Index: integer; Zeitzone: string);
{---------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).Zeitzone_G:=Zeitzone;
end;

{-------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetMNrAllg_G (Index: integer; MNrAllg: string);
{-------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).MNrAllg_G:=MNrAllg;
end;

{-----------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetMNrGeraet_G (Index: integer; MNrGeraet: string);
{-----------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).MNrGeraet_G:=MNrGeraet;
end;

{-------------------------------------------------------------------}
procedure TMeldAbgleichList.SetMTyp_G (Index: integer; MTyp: string);
{-------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).MTyp_G:=MTyp;
end;

{-------------------------------------------------------------------}
procedure TMeldAbgleichList.SetMArt_G (Index: integer; MArt: string);
{-------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).MArt_G:=MArt;
end;

{----------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetQuittiert (Index: integer; Quittiert: boolean);
{----------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).Quittiert:=Quittiert;
end;

{---------------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetDSfG_Status_G (Index: integer; DSfG_Status: string);
{---------------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).DSfG_Status_G:=DSfG_Status;
end;

{---------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetDSfG_CRC_G (Index: integer; DSfG_CRC: string);
{---------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).DSfG_CRC_G:=DSfG_CRC;
end;

{-----------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetBemerkung_K (Index: integer; Bemerkung: string);
{-----------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).Bemerkung_K:=Bemerkung;
end;

{-----------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetBemerkung_G (Index: integer; Bemerkung: string);
{-----------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).Bemerkung_G:=Bemerkung;
end;

{-------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetStationstext1 (Index: integer; ST1: string);
{-------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).Stationstext1:=ST1;
end;

{-------------------------------------------------------------------------}
procedure TMeldAbgleichList.SetStationstext2 (Index: integer; ST2: string);
{-------------------------------------------------------------------------}
begin
  TMeldAbgleichListObj (Items [Index]).Stationstext2:=ST2;
end;

end.
