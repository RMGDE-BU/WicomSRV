{******************************************************************************}
{* Unit: Dialog mit Anzeige-Kriterien für Meldungen                           *}
{* 04.07.2001   WW                                                            *}
{* 05.03.2002   WW  erweitert: alle Instanzen einer DSfG-Station filtern      *}
{******************************************************************************}
unit FDlgMeldSichtFilter;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  ComCtrls, DbTables, MeldFilterIni, MDbSta, DDbSta, WSysCon, DListen,
  Graphics;

type
  TFormDlgMeldSichtFilter = class(TForm)
    gbFilterGeraeteArt: TGroupBox;
    chbFilterMRG: TCheckBox;
    rgrpSortierung: TRadioGroup;
    gbAnzeigeFelder: TGroupBox;
    chbAnzeigeQuittiert: TCheckBox;
    chbAnzeigeDatumZeit: TCheckBox;
    chbAnzeigeZeitzone: TCheckBox;
    chbAnzeigeMeldung: TCheckBox;
    chbAnzeigeGeraeteArt: TCheckBox;
    chbAnzeigeStation: TCheckBox;
    chbAnzeigeOrdNr: TCheckBox;
    chbAnzeigeMNrGeraet: TCheckBox;
    bbtnOK: TBitBtn;
    bbtnAbbrechen: TBitBtn;
    gbFilterMArt: TGroupBox;
    chbFilterHinweis: TCheckBox;
    chbFilterWarnung: TCheckBox;
    gbFilterQuittiert: TGroupBox;
    chbFilterNichtQuittiert: TCheckBox;
    chbFilterQuittiert: TCheckBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    chbAnzeigeMNrAllg: TCheckBox;
    chbFilterDSfG: TCheckBox;
    chbAnzeigeDSfGStatus: TCheckBox;
    chbAnzeigeBemerkung: TCheckBox;
    chbAnzeigeCRC: TCheckBox;
    chbFilterStoerung: TCheckBox;
    chbFilterRechnerfehler: TCheckBox;
    gbFilterDatumZeit: TGroupBox;
    chbFilterDatumZeit: TCheckBox;
    lVon: TLabel;
    lBis: TLabel;
    dtpickVon: TDateTimePicker;
    dtpickBis: TDateTimePicker;
    gbFilterMRGStation: TGroupBox;
    cbMRGStationen: TComboBox;
    gbFilterDSfGInstanz: TGroupBox;
    cbDSfGStationen: TComboBox;
    cbDSfGInstanzen: TComboBox;
    lDSfGStation: TLabel;
    lDSfGInstanz: TLabel;
    chbFilterMRGStation: TCheckBox;
    chbFilterDSfGInstanz: TCheckBox;
    imageStoerung: TImage;
    imageRechnerfehler: TImage;
    imageWarnung: TImage;
    imageHinweis: TImage;
    imageSommerzeit: TImage;
    imageWinterzeit: TImage;
    lSommerzeit: TLabel;
    rgrpFilterAbrufart: TRadioGroup;
    lWinterzeit: TLabel;
    procedure bbtnOKClick(Sender: TObject);
    procedure isChanged(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chbFilterDatumZeitClick(Sender: TObject);
    procedure dtpickVonChange(Sender: TObject);
    procedure dtpickBisChange(Sender: TObject);
    procedure chbFilterMRGStationClick(Sender: TObject);
    procedure chbFilterDSfGInstanzClick(Sender: TObject);
    procedure cbDSfGStationenChange(Sender: TObject);
  private
    { Private-Deklarationen }
    MeldFilterIniFile: TMeldFilterIniFile;
    MRGStammdaten: TMRGStammdaten;
    DSfGStammdaten: TDSfGStammdaten;
    GeraeteArt: string;
    EinzelStation: boolean;
    procedure FillMRGStationCombobox;
    procedure FillDSfGStationCombobox;
    procedure FillDSfGInstanzCombobox (StationId: integer);
    procedure SetCbMRGStation (MRGId: integer);
    function GetCbMRGStation: integer;
    procedure SetCbDSfGStation (StationId: integer);
    procedure SetCbDSfGInstanz (InstanzId: integer);
    function GetCbDSfGStation: integer;
    function GetCbDSfGInstanz: integer;
    procedure GetFelder;
    procedure GetFilter;
    function SetFelder: boolean;
    function SetFilter: boolean;
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; AIniFileName: string; AStammPath: string;
                        AGeraeteArt: string; AEinzelStation: boolean); reintroduce;
  end;

implementation

{$R *.DFM}

{----------------------------------------------------------------------------------------}
constructor TFormDlgMeldSichtFilter.Create (AOwner: TComponent;
                                            AIniFileName: string; AStammPath: string;
                                            AGeraeteArt: string; AEinzelStation: boolean);
{----------------------------------------------------------------------------------------}
{ Constructor-Übergaben:
    AIniFileName (Datei, in der die Filter-Einstellungen abgespeichert sind)
    AStammPath (Stammdaten-Pfad)
    AGeraeteArt ('M' = nur MRG-Geräte; 'D' = nur DSfG-Geräte; '' = beide)
    AEinzelStation (true für Einzelstation-Filter) }
begin
  inherited Create (AOwner);
  GeraeteArt:=AGeraeteArt;
  EinzelStation:=AEinzelStation;

  MeldFilterIniFile:=TMeldFilterIniFile.Create (AIniFileName);
  MRGStammdaten:=TMRGStammdaten.Create (AStammPath);
  if Geraeteart <> C_GerArtDSfG then begin
    if MRGStammdaten.InitTabellen then
      FillMRGStationCombobox
    else
      MessageDlg ('Fehler beim Zugriff auf MRG-Stammdaten aufgetreten !', mtError, [mbOk], 0);
  end;
  DSfGStammdaten:=TDSfGStammdaten.Create (AStammPath);
  if Geraeteart <> C_GerArtMrg then begin
    if DSfGStammdaten.InitTabellen then
      FillDSfGStationCombobox
    else
      MessageDlg ('Fehler beim Zugriff auf DSfG-Stammdaten aufgetreten !', mtError, [mbOk], 0);
  end;
  GetFelder;
  GetFilter;
  bbtnOK.Enabled:=false;
end;

{-------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.FormDestroy(Sender: TObject);
{-------------------------------------------------------------}
begin
  MRGStammdaten.Free;
  MeldFilterIniFile.Free;
end;

{-------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.FillMRGStationCombobox;
{-------------------------------------------------------}
{ MRG-Stations-Combobox füllen }
var
  Query: TQuery;
  Save_Cursor: TCursor;

begin
  { MRG-Stationsnamen mit MrgId eintragen: }
  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  try
    cbMRGStationen.Items.Clear;
    Query:=TQuery.Create (nil);
    try
      MRGStammdaten.GetStationsnamenKennungen (Query);
      while not Query.Eof do begin
        cbMRGStationen.Items.AddObject (
          Query.FieldByName (C_Sta_Stationsname).AsString,
          Pointer (Query.FieldByName (C_Sta_MrgId).AsInteger));
        Query.Next;
      end;  { while }
    finally
      Query.Close;
      Query.Free;
    end;
  finally
    Screen.Cursor:=Save_Cursor;
  end;
end;

{--------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.FillDSfGStationCombobox;
{--------------------------------------------------------}
{ DSfG-Stations-Combobox füllen }
var
  Stationsliste: TStationDataList;
  i: integer;
  Save_Cursor: TCursor;

begin
  { DSfG-Stationsnamen mit StationsId eintragen: }
  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  try
    cbDSfGStationen.Items.Clear;
    Stationsliste:=TStationDataList.Create;
    try
      DSfGStammdaten.GetStationDataList (Stationsliste);
      for i:=0 to Stationsliste.Count-1 do
        cbDSfGStationen.Items.AddObject (PStationData (StationsListe [i]).Stationsname,
                                         Pointer (PStationData (StationsListe [i]).StationId));
    finally
      Stationsliste.Free;
    end;
  finally
    Screen.Cursor:=Save_Cursor;
  end;
end;

{-----------------------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.FillDSfGInstanzCombobox (StationId: integer);
{-----------------------------------------------------------------------------}
{ DSfG-Instanz-Combobox mit zu StationId gehörenden Instanzen füllen }
var
  Query: TQuery;
  Save_Cursor: TCursor;

begin
  { DSfG-Instanznamen mit InstanzId eintragen: }
  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  try
    cbDSfGInstanzen.Items.Clear;
    cbDSfGInstanzen.Items.AddObject ('<Alle>', Pointer (0));

    Query:=TQuery.Create (nil);
    try
      DSfGStammDaten.GetInstanzQuery (StationId, Query);
      while not Query.Eof do begin
        cbDSfGInstanzen.Items.AddObject (
          Query.FieldByName (C_DTF_Instanz_Instanzname).AsString,
          Pointer (Query.FieldByName (C_DTF_Instanz_InstanzId).AsInteger));
        Query.Next;
      end;  { while }
    finally
      Query.Close;
      Query.Free;
    end;
  finally
    Screen.Cursor:=Save_Cursor;
  end;
end;

{-----------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.SetCbMRGStation (MRGId: integer);
{-----------------------------------------------------------------}
{ MRG-Station mit MrgId in der Combobox setzen }
var
  i: integer;
begin
  for i:=0 to cbMRGStationen.Items.Count-1 do begin
    if integer (cbMRGStationen.Items.Objects [i]) = MRGId then begin
      cbMRGStationen.ItemIndex:=i;
      Break;
    end;
  end;
end;

{--------------------------------------------------------}
function TFormDlgMeldSichtFilter.GetCbMRGStation: integer;
{--------------------------------------------------------}
{ MrgId der in der Combobox ausgewählten MRG-Station zurückgeben }
begin
  if cbMRGStationen.ItemIndex > -1 then
    Result:=integer (cbMRGStationen.Items.Objects [cbMRGStationen.ItemIndex])
  else
    Result:=0;
end;

{----------------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.SetCbDSfGStation (StationId: integer);
{----------------------------------------------------------------------}
{ DSfG-Station mit StationId in der Combobox setzen }
var
  i: integer;
begin
  for i:=0 to cbDSfGStationen.Items.Count-1 do begin
    if integer (cbDSfGStationen.Items.Objects [i]) = StationId then begin
      cbDSfGStationen.ItemIndex:=i;
      Break;
    end;
  end;
end;

{----------------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.SetCbDSfGInstanz (InstanzId: integer);
{----------------------------------------------------------------------}
{ DSfG-Instanz mit InstanzId in der Combobox setzen }
var
  i: integer;
begin
  for i:=0 to cbDSfGInstanzen.Items.Count-1 do begin
    if integer (cbDSfGInstanzen.Items.Objects [i]) = InstanzId then begin
      cbDSfGInstanzen.ItemIndex:=i;
      Break;
    end;
  end;
end;

{---------------------------------------------------------}
function TFormDlgMeldSichtFilter.GetCbDSfGStation: integer;
{---------------------------------------------------------}
{ StationId der in der Combobox ausgewählten DSfG-Station zurückgeben }
begin
  if cbDSfGStationen.ItemIndex > -1 then
    Result:=integer (cbDSfGStationen.Items.Objects [cbDSfGStationen.ItemIndex])
  else
    Result:=0;
end;

{---------------------------------------------------------}
function TFormDlgMeldSichtFilter.GetCbDSfGInstanz: integer;
{---------------------------------------------------------}
{ InstanzId der in der Combobox ausgewählten DSfG-Instanz zurückgeben }
begin
  if cbDSfGInstanzen.ItemIndex > -1 then
    Result:=integer (cbDSfGInstanzen.Items.Objects [cbDSfGInstanzen.ItemIndex])
  else
    Result:=0;
end;

{------------------------------------------}
procedure TFormDlgMeldSichtFilter.GetFelder;
{------------------------------------------}
{ Einstellungen für anzuzeigende Felder aus MeldFilterIniFile lesen }
begin
  chbAnzeigeQuittiert.checked:=MeldFilterIniFile.FeldQuittiert;
  chbAnzeigeDatumZeit.checked:=MeldFilterIniFile.FeldDatumZeit;
  chbAnzeigeMeldung.checked:=MeldFilterIniFile.FeldMeldung;
  if EinzelStation then begin
    chbAnzeigeGeraeteArt.checked:=false;
    chbAnzeigeGeraeteArt.enabled:=false;
    chbAnzeigeStation.checked:=false;
    chbAnzeigeStation.enabled:=false;
  end
  else begin
    if (GeraeteArt = C_GerArtMrg) OR (GeraeteArt = C_GerArtDSfG) then begin
      chbAnzeigeGeraeteArt.checked:=false;
      chbAnzeigeGeraeteArt.enabled:=false;
    end else
      chbAnzeigeGeraeteArt.checked:=MeldFilterIniFile.FeldGeraeteArt;
    chbAnzeigeStation.checked:=MeldFilterIniFile.FeldStation;
  end;
  chbAnzeigeMNrGeraet.checked:=MeldFilterIniFile.FeldMNrGeraet;
  chbAnzeigeMNrAllg.checked:=MeldFilterIniFile.FeldMNrAllg;
  chbAnzeigeBemerkung.checked:=MeldFilterIniFile.FeldBemerkung;
  if GeraeteArt = C_GerArtMrg then begin
    chbAnzeigeZeitzone.checked:=false;
    chbAnzeigeZeitzone.enabled:=false;
    lSommerzeit.enabled:=false;
    imageSommerzeit.Canvas.Draw (-16, 0, imageSommerzeit.picture.graphic);   { deaktiviertes Image anzeigen }
    lWinterzeit.enabled:=false;
    imageWinterzeit.Canvas.Draw (-16, 0, imageWinterzeit.picture.graphic);   { deaktiviertes Image anzeigen }
    chbAnzeigeOrdNr.checked:=false;
    chbAnzeigeOrdNr.enabled:=false;
    chbAnzeigeDSfGStatus.checked:=false;
    chbAnzeigeDSfGStatus.enabled:=false;
    chbAnzeigeCRC.checked:=false;
    chbAnzeigeCRC.enabled:=false;
  end
  else begin
    chbAnzeigeZeitzone.checked:=MeldFilterIniFile.FeldZeitzone;
    chbAnzeigeOrdNr.checked:=MeldFilterIniFile.FeldOrdNr;
    chbAnzeigeDSfGStatus.checked:=MeldFilterIniFile.FeldDSfGStatus;
    chbAnzeigeCRC.checked:=MeldFilterIniFile.FeldCRC;
  end;
end;

{------------------------------------------}
procedure TFormDlgMeldSichtFilter.GetFilter;
{------------------------------------------}
{ Einstellungen für Filter aus MeldFilterIniFile lesen }
var
  InstanzData: TInstanzData;
  StationId: integer;
  InstanzId: integer;
begin
  rgrpFilterAbrufart.ItemIndex:=MeldFilterIniFile.FilterAbrufart;

  if GeraeteArt = C_GerArtMrg then begin                     { nur MRG-Geräte }
    chbFilterMRG.checked:=true;
    chbFilterMRG.enabled:=false;
    chbFilterDSfG.checked:=false;
    chbFilterDSfG.enabled:=false;
    gbFilterGeraeteArt.enabled:=false;
  end
  else if GeraeteArt = C_GerArtDSfG then begin              { nur DSfG-Geräte }
    chbFilterDSfG.checked:=true;
    chbFilterDSfG.enabled:=false;
    chbFilterMRG.checked:=false;
    chbFilterMRG.enabled:=false;
    gbFilterGeraeteArt.enabled:=false;
  end
  else begin                                           { MRG- und DSfG-Geräte }
    chbFilterMRG.checked:=MeldFilterIniFile.FilterMrg;
    chbFilterDSfG.checked:=MeldFilterIniFile.FilterDSfG;
  end;

  chbFilterDatumzeit.checked:=MeldFilterIniFile.FilterDatumZeit;
  dtpickVon.Date:=MeldFilterIniFile.FilterDatumVon;
  dtpickBis.Date:=MeldFilterIniFile.FilterDatumBis;
  chbFilterHinweis.checked:=MeldFilterIniFile.FilterHinweis;
  chbFilterWarnung.checked:=MeldFilterIniFile.FilterWarnung;
  chbFilterStoerung.checked:=MeldFilterIniFile.FilterStoerung;
  chbFilterRechnerfehler.checked:=MeldFilterIniFile.FilterRechnerfehler;
  chbFilterQuittiert.checked:=MeldFilterIniFile.FilterQuittiert;
  chbFilterNichtQuittiert.checked:=MeldFilterIniFile.FilterNichtQuittiert;

  if (GeraeteArt <> C_GerArtDSfG) AND not EinzelStation then begin
    { MRG-Einzelstation aktivieren: }
    if MeldFilterIniFile.FilterMRGStation > 0 then begin
      chbFilterMRGStation.Checked:=true;
    end else
      chbFilterMRGStation.Checked:=false;
    SetCbMRGStation (Abs (MeldFilterIniFile.FilterMRGStation));
  end
  else begin
    { MRG-Einzelstation deaktivieren: }
    chbFilterMRGStation.Checked:=false;
    chbFilterMRGStation.enabled:=false;
    cbMRGStationen.enabled:=false;
  end;

  if (GeraeteArt <> C_GerArtMrg) AND not EinzelStation then begin
    if (MeldFilterIniFile.FilterDSfGStation > 0) OR
       (MeldFilterIniFile.FilterDSfGInstanz > 0) then
      chbFilterDSfGInstanz.Checked:=true
    else
      chbFilterDSfGInstanz.Checked:=false;

    StationId:=0;
    InstanzId:=0;
    if MeldFilterIniFile.FilterDSfGStation <> 0 then begin    { alle Instanzen einer Station }
      StationId:=Abs (MeldFilterIniFile.FilterDSfGStation);
    end
    else if MeldFilterIniFile.FilterDSfGInstanz <> 0 then begin
      if DSfGStammdaten.GetInstanzData (Abs (MeldFilterIniFile.FilterDSfGInstanz),
                                        InstanzData) then begin
        StationId:=InstanzData.StationId;
        InstanzId:=InstanzData.InstanzId;
      end;
    end;
    SetCbDSfGStation (StationId);
    FillDSfGInstanzCombobox (StationId);
    SetCbDSfGInstanz (InstanzId);
  end
  else begin
    chbFilterDSfGInstanz.Checked:=false;
    chbFilterDSfGInstanz.enabled:=false;
    cbDSfGStationen.enabled:=false;
    cbDSfGInstanzen.enabled:=false;
  end;

  rgrpSortierung.ItemIndex:=MeldFilterIniFile.Sortierung;
end;

{--------------------------------------------------}
function TFormDlgMeldSichtFilter.SetFelder: boolean;
{--------------------------------------------------}
{ Einstellungen für anzuzeigende Felder in MeldFilterIniFile schreiben }
begin
  Result:=true;
  { Es muß mindestens ein Feld gewählt sein }
  if (not chbAnzeigeQuittiert.checked) and (not chbAnzeigeDatumZeit.checked) and
     (not chbAnzeigeZeitzone.checked) and (not chbAnzeigeMeldung.checked) and
     (not chbAnzeigeGeraeteArt.checked) and (not chbAnzeigeStation.checked) and
     (not chbAnzeigeOrdNr.checked) and (not chbAnzeigeMNrGeraet.checked) and
     (not chbAnzeigeMNrAllg.checked) and (not chbAnzeigeDSfGStatus.checked) and
     (not chbAnzeigeCRC.checked) and (not chbAnzeigeBemerkung.checked) then begin
    MessageDlg ('Es muß mindestens ein anzuzeigendes Feld gewählt sein !', mtWarning, [mbOk], 0);
    Result:= false;
    exit;
  end
  else begin
   MeldFilterIniFile.FeldQuittiert:=chbAnzeigeQuittiert.checked;
   MeldFilterIniFile.FeldDatumZeit:=chbAnzeigeDatumZeit.checked;
   MeldFilterIniFile.FeldMeldung:=chbAnzeigeMeldung.checked;
   if not EinzelStation then begin
     if (GeraeteArt <> C_GerArtMrg) AND (GeraeteArt <> C_GerArtDSfG) then
       MeldFilterIniFile.FeldGeraeteArt:=chbAnzeigeGeraeteArt.checked;
     MeldFilterIniFile.FeldStation:=chbAnzeigeStation.checked;
   end;
   MeldFilterIniFile.FeldMNrGeraet:=chbAnzeigeMNrGeraet.checked;
   MeldFilterIniFile.FeldMNrAllg:=chbAnzeigeMNrAllg.checked;
   MeldFilterIniFile.FeldBemerkung:=chbAnzeigeBemerkung.checked;
   if GeraeteArt <> C_GerArtMrg then begin
     MeldFilterIniFile.FeldZeitzone:=chbAnzeigeZeitzone.checked;
     MeldFilterIniFile.FeldOrdNr:=chbAnzeigeOrdNr.checked;
     MeldFilterIniFile.FeldDSfGStatus:=chbAnzeigeDSfGStatus.checked;
     MeldFilterIniFile.FeldCRC:=chbAnzeigeCRC.checked;
   end;
  end;
end;

{--------------------------------------------------}
function TFormDlgMeldSichtFilter.SetFilter: boolean;
{--------------------------------------------------}
{ Einstellungen für Filter in MeldFilterIniFile schreiben }
var
  StationId: integer;
  InstanzId: integer;
begin
  Result:=true;
  { Abrufart (Automatik/manuell) }
  MeldFilterIniFile.FilterAbrufart:=rgrpFilterAbrufart.ItemIndex;

  { Geräteart nur speichern, wenn MRG- und DSfG-Geräte möglich sind }
  if (GeraeteArt <> C_GerArtMrg) AND (GeraeteArt <> C_GerArtDSfG) then begin
    { Es muß eine Geräteart gewählt sein }
    if (not chbFilterMRG.checked) and (not chbFilterDSfG.checked) then begin
      MessageDlg ('Es muß mindestens eine Geräteart gewählt sein !', mtWarning, [mbOk], 0);
      Result:=false;
      exit;
    end;
    MeldFilterIniFile.FilterMRG:=chbFilterMRG.checked;
    MeldFilterIniFile.FilterDSfG:=chbFilterDSfG.checked;
  end;

  MeldFilterIniFile.FilterDatumZeit:=chbFilterDatumzeit.checked;
  MeldFilterIniFile.FilterDatumVon:=trunc (dtpickVon.Date);
  MeldFilterIniFile.FilterDatumBis:=trunc (dtpickBis.Date);
  { Es muß eine Meldungsart gewählt sein }
  if (not chbFilterHinweis.checked) and (not chbFilterWarnung.checked) and
     (not chbFilterStoerung.checked) and (not chbFilterRechnerfehler.checked) then begin
    MessageDlg ('Es muß mindestens eine Meldungsart gewählt sein !', mtWarning, [mbOk], 0);
    Result:=false;
    exit;
  end;
  MeldFilterIniFile.FilterHinweis:=chbFilterHinweis.checked;
  MeldFilterIniFile.FilterWarnung:=chbFilterWarnung.checked;
  MeldFilterIniFile.FilterStoerung:=chbFilterStoerung.checked;
  MeldFilterIniFile.FilterRechnerfehler:=chbFilterRechnerfehler.checked;
  { Es muß eine Quittierung gewählt sein }
  if (not chbFilterQuittiert.checked) and (not chbFilterNichtQuittiert.checked) then begin
    MessageDlg ('Es muß mindestens ein Quittierungs-Status gewählt sein !', mtWarning, [mbOk], 0);
    Result:=false;
    exit;
  end;
  MeldFilterIniFile.FilterQuittiert:=chbFilterQuittiert.checked;
  MeldFilterIniFile.FilterNichtQuittiert:=chbFilterNichtQuittiert.checked;

  if (GeraeteArt <> C_GerArtDSfG) AND not EinzelStation then begin
    { MRG-Einzelstation: }
    if chbFilterMRGStation.Checked then
      MeldFilterIniFile.FilterMRGStation:=GetCbMRGStation
    else
      MeldFilterIniFile.FilterMRGStation:=GetCbMRGStation * (-1);
  end;

  if (GeraeteArt <> C_GerArtMrg) AND not EinzelStation then begin
    { DSfG-Einzel-Station/Instanz: }
    InstanzId:=GetCbDSfGInstanz;
    if InstanzId = 0 then              { alle Instanzen einer Station }
      StationId:=GetCbDSfGStation
    else                               { einzelne Instanz einer Station }
      StationId:=0;
    if chbFilterDSfGInstanz.Checked then begin
      MeldFilterIniFile.FilterDSfGStation:=StationId;
      MeldFilterIniFile.FilterDSfGInstanz:=InstanzId;
    end
    else begin
      MeldFilterIniFile.FilterDSfGStation:=StationId * (-1);
      MeldFilterIniFile.FilterDSfGInstanz:=InstanzId * (-1);
    end;
  end;

  { Sortierreihenfolge }
  MeldFilterIniFile.Sortierung:=rgrpSortierung.ItemIndex;
end;

{-------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.bbtnOKClick(Sender: TObject);
{-------------------------------------------------------------}
begin
  if not SetFilter then begin
    ModalResult:=mrNone;
    exit;
  end;
  if not SetFelder then begin
    ModalResult:=mrNone;
    exit;
  end;
end;

{-----------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.isChanged(Sender: TObject);
{-----------------------------------------------------------}
begin
  bbtnOK.Enabled:=true;
end;

{-------------------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.chbFilterDatumZeitClick(Sender: TObject);
{-------------------------------------------------------------------------}
begin
  isChanged (Sender);
  { von/bis-Zeitraum (de-)aktivieren }
  lVon.Enabled:=chbFilterDatumZeit.checked;
  lBis.Enabled:=chbFilterDatumZeit.checked;
  dtpickVon.Enabled:=chbFilterDatumZeit.checked;
  dtpickBis.Enabled:=chbFilterDatumZeit.checked;
end;

{--------------------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.chbFilterMRGStationClick(Sender: TObject);
{--------------------------------------------------------------------------}
begin
  isChanged (Sender);
  if chbFilterMRGStation.Checked then
    cbMRGStationen.Enabled:=true
  else
    cbMRGStationen.Enabled:=false;
end;

{---------------------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.chbFilterDSfGInstanzClick(Sender: TObject);
{---------------------------------------------------------------------------}
begin
  isChanged (Sender);
  if chbFilterDSfGInstanz.Checked then begin
    lDSfGStation.Enabled:=true;
    lDSfGInstanz.Enabled:=true;
    cbDSfGStationen.Enabled:=true;
    cbDSfGInstanzen.Enabled:=true;
  end
  else begin
    lDSfGStation.Enabled:=false;
    lDSfGInstanz.Enabled:=false;
    cbDSfGStationen.Enabled:=false;
    cbDSfGInstanzen.Enabled:=false;
  end;
end;

{-----------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.dtpickVonChange(Sender: TObject);
{-----------------------------------------------------------------}
begin
  isChanged (Sender);
  if dtpickvon.Date > dtpickbis.Date then
    dtpickbis.Date:=dtpickvon.Date;
end;

{-----------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.dtpickBisChange(Sender: TObject);
{-----------------------------------------------------------------}
begin
  isChanged (Sender);
  if dtpickbis.Date < dtpickvon.date then
    dtpickvon.date:=dtpickbis.date;
end;

{-----------------------------------------------------------------------}
procedure TFormDlgMeldSichtFilter.cbDSfGStationenChange(Sender: TObject);
{-----------------------------------------------------------------------}
begin
  isChanged (Sender);
  if cbDSfGStationen.ItemIndex > -1 then begin
    FillDSfGInstanzCombobox (integer (cbDSfGStationen.Items.Objects [cbDSfGStationen.ItemIndex]));
    SetCbDSfGInstanz (0);       { alle Instanzen }
  end;
end;

end.
