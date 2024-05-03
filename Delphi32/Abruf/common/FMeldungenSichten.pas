{******************************************************************************}
{* Unit: Formular zum Sichten von MRG/DSfG-Meldungen                          *}
{* -> zur Laufzeit erstellen                                                  *}
{******************************************************************************}
unit FMeldungenSichten;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Grids,
  StdCtrls, ExtCtrls, Menus, ComCtrls, WSysCon, Buttons, MeldungenDb, WTables,
  MeldAbgleichListe, MeldKonfigDb, FDlgMeldSichtFilter, MeldFilterIni,
  FDlgMeldBemerkung, MDbSta, DDbSta, DListen, FPrintMeldungen;

{$R FMeldungenSichten.RES}

type
  { Callback-Prozedurtyp zur Übergabe des Zeitstempels der Meldungstabelle
    an Fremdmodule }

  TCBMeldTableTime = procedure (aTime: integer) of object;


  { Formular zum Sichten von Meldungen }

  TFormMeldungenSichten = class(TForm)
    pTop: TPanel;
    pmMeldungen: TPopupMenu;
    miQuittiereMarkierte: TMenuItem;
    miQuittiereAlle: TMenuItem;
    N1: TMenuItem;
    miBemerkung: TMenuItem;
    sbtnQuittieren: TSpeedButton;
    sbtnDrucken: TSpeedButton;
    sbtnAnzeigeFilter: TSpeedButton;
    DrawGrid: TDrawGrid;
    StatusBar: TStatusBar;
    sbtnAktualisieren: TSpeedButton;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DrawGridDrawCell(Sender: TObject; Col, Row: integer;
                Rect: TRect; State: TGridDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure miQuittiereMarkierteClick(Sender: TObject);
    procedure miQuittiereAlleClick(Sender: TObject);
    procedure sbtnQuittierenClick(Sender: TObject);
    procedure sbtnAnzeigeFilterClick(Sender: TObject);
    procedure sbtnAktualisierenClick(Sender: TObject);
    procedure miBemerkungClick(Sender: TObject);
    procedure pmMeldungenPopup(Sender: TObject);
    procedure sbtnDruckenClick(Sender: TObject);
  private
    Path: string;
    IniFileName: string;
    GeraeteArt: string;
    GeraeteId: integer;
    MeldFilterIniFile: TMeldFilterIniFile;
    MeldAbgleichList: TMeldAbgleichList;  { Zwischenpuffer für Kommt/Geht-Abgleich }
    EinzelStation: boolean;
    bmpQuittiert: TBitmap;
    bmpKommt: TBitmap;
    bmpGeht: TBitmap;
    bmpSommerzeit: TBitmap;
    bmpWinterzeit: TBitmap;
    bmpHinweis: TBitmap;
    bmpWarnung: TBitmap;
    bmpStoerung: TBitmap;
    bmpRechnerfehler: TBitmap;
    FMeldTableTime: TCBMeldTableTime;
    procedure SetCaption;
    procedure CreateBitmaps;
    procedure FreeBitmaps;
    function GetFilterStatus: byte;
    procedure InitAnzeige;
    procedure SetDrawGridColWidths;
    procedure DrawSpaltenueberschrift (Col, Row: integer; Rect: TRect);
    procedure DrawMeldung (Col, Row: integer; Rect: TRect);
    procedure DrawQuittiert (Row: integer; Rect: TRect);
    procedure DrawDatumZeit (Row: integer; Rect: TRect);
    procedure DrawZeitzone (Row: integer; Rect: TRect);
    procedure DrawOrdnungsnummer (Row: integer; Rect: TRect);
    procedure DrawMeldNrGeraet (Row: integer; Rect: TRect);
    procedure DrawMeldNrAllg (Row: integer; Rect: TRect);
    procedure DrawGeraeteArt (Row: integer; Rect: TRect);
    procedure DrawMeldungstext (Row: integer; Rect: TRect);
    procedure DrawStation (Row: integer; Rect: TRect);
    procedure DrawDSfGStatus (Row: integer; Rect: TRect);
    procedure DrawDSfGCRC (Row: integer; Rect: TRect);
    procedure DrawBemerkung (Row: integer; Rect: TRect);
    procedure WriteTextoben (Rect: TRect; Text: string; Ausrichtung: char);
    procedure WriteTextunten (Rect: TRect; Text: string; Ausrichtung: char);
    procedure WriteDatumZeit (Rect: TRect; DatumZeit_K: string;
                                           DatumZeit_G: string);
    procedure WriteMeldungtext (Rect: TRect; MArt: string;
                                Textoben: string; Textunten: string);
    procedure QuittiereMeldungen (AllOrSome: byte);
  public
    constructor Create (AOwner: TComponent;
                        APath: string; AIniFileName: string;
                        AGeraeteArt: string; AGeraeteId: integer;
                        ResetFilter: boolean); reintroduce;
    procedure ResetMeldFilterIniFile;
    procedure LoadMeldungen;
    property CBMeldTableTime: TCBMeldTableTime read FMeldTableTime write FMeldTableTime;
  end;

implementation

{$R *.DFM}

const
  CSpace = 2;

  { Spaltenbreiten }

  CWidthQuittiert = 20;
  CWidthDatumZeit = 138;
  CWidthZeitzone  = 20;
  CWidthOrdNr     = 60;
  CWidthMNrGeraet = 75;
  CWidthMNrAllg   = 65;
  CWidthGerArt    = 50;
  CWidthMText     = 230;
  CWidthStation   = 140;
  CWidthStatus    = 65;
  CWidthCRC       = 30;
  CWidthBemerkung = 200;


{---------------------------------------------------------------------------------}
constructor TFormMeldungenSichten.Create (AOwner: TComponent;
                                          APath: string; AIniFileName: string;
                                          AGeraeteArt: string; AGeraeteId: integer;
                                          ResetFilter: boolean);
{---------------------------------------------------------------------------------}
{ Constructor-Übergaben:
    APath (Pfad für Meldungstabellen und Stammdaten)
    AIniFileName (Datei, in der die Filter-Einstellungen abgespeichert sind)
    AGeraeteArt ('M' = nur MRG-Geräte; 'D' = nur DSfG-Geräte; '' = beide)
    AGeraeteId (MRG-Station-Id bzw. DSfG-Quell-InstanzId; <= 0 -> "alle" )
    ResetFilter (wenn true, werden vor dem Fensteraufruf im übergebenen Ini-File
                 die Filterkriterien auf "alle nicht quittierten Meldungen, sortiert
                 nach Datum/Zeit" gesetzt (für Rufentgegennahme) }
begin
  inherited Create (AOwner);
  Path:=APath;
  IniFileName:=AIniFileName;
  GeraeteArt:=AGeraeteArt;
  GeraeteId:=AGeraeteId;

  MeldFilterIniFile:=TMeldFilterIniFile.Create (AIniFileName);
  if ResetFilter then
    ResetMeldFilterIniFile;               { Filter-Einstellungen zurücksetzen }
  MeldAbgleichList:=TMeldAbgleichList.Create (Path, AIniFileName,
                                              AGeraeteArt, AGeraeteId);

  EinzelStation:=GeraeteId > 0;
  DrawGrid.Enabled:=false;
  { RowHeight für Meldungen (auf zweizeiligen Text ausgelegt): }
  DrawGrid.DefaultRowHeight:=2 * abs (DrawGrid.Font.Height) + 5 * CSpace;
  { RowHeight für Kopfzeile mit Spalten-Überschriften: }
  DrawGrid.RowHeights [0]:=abs (DrawGrid.Font.Height) + 3 * CSpace;

  DrawGrid.ColCount:=12;

  CreateBitmaps;                                           { Bitmaps erzeugen }
  LoadMeldungen;                   { Meldungen aus Tabelle laden und anzeigen }
end;

{------------------------------------------------------------}
procedure TFormMeldungenSichten.FormActivate(Sender: TObject);
{------------------------------------------------------------}
begin
  WindowState:=wsMaximized;
end;

{-----------------------------------------------------------}
procedure TFormMeldungenSichten.FormDestroy(Sender: TObject);
{-----------------------------------------------------------}
begin
  FreeBitmaps;
  MeldAbgleichList.Free;
  MeldFilterIniFile.Free;
end;

{-----------------------------------------------------------------------------------}
procedure TFormMeldungenSichten.FormClose(Sender: TObject; var Action: TCloseAction);
{-----------------------------------------------------------------------------------}
begin
  Action:=caFree;
end;

{-----------------------------------------}
procedure TFormMeldungenSichten.SetCaption;
{-----------------------------------------}
{ Fenstertitel setzen }
var
  S: string;
  MRGStammdaten: TMRGStammdaten;
  DSfGStammdaten: TDSfGStammdaten;
  OK: boolean;
  StaData: TStaData;
  StationData: TStationData;
  InstanzData: TInstanzData;

begin
  if MeldFilterIniFile.FilterAbrufart = 0 then
    S:='Automatik-Meldungen'
  else
    S:='Manuelle Meldungen';

  if EinzelStation then begin
    if Geraeteart = C_GerArtMrg then begin
      { MRG-Stationsnamen und Kennungen ermitteln: }
      OK:=true;
      MRGStammdaten:=TMRGStammdaten.Create (Path);
      try
        if MRGStammdaten.InitTabellen then begin
          if MRGStammdaten.GetStaData (GeraeteId, StaData) then
            S:=S + ' von Station ' + StaData.StationsName +
                   ' (' + StaData.Kennung + ')';
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
          if DSfGStammdaten.GetInstanzData (GeraeteId, InstanzData) then
            if DSfGStammdaten.GetStationData (InstanzData.StationId, StationData) then
              S:=S + ' von Station ' + StationData.Stationsname + '/' +
                 InstanzData.Instanzname;
        end else  { if InitTabellen }
          OK:=false;
      finally
        DSfGStammdaten.Free;
      end;
      if not OK then
        MessageDlg ('Fehler beim Zugriff auf DSfG-Stammdaten aufgetreten !', mtError, [mbOk], 0);
    end;
  end;
  Caption:=S;
end;

{-----------------------------------------------------}
procedure TFormMeldungenSichten.ResetMeldFilterIniFile;
{-----------------------------------------------------}
{ INI-Einstellungen für Filter und Sortierung auf Defaultwerte zurücksetzen }
begin
  MeldFilterIniFile.SetDefaultFilterSortierung;
end;

{-------------------------------------------}
procedure TFormMeldungenSichten.InitAnzeige;
{-------------------------------------------}
var
  BottomRect: TGridRect;
begin
  DrawGrid.Enabled:=false;
  LockWindowUpdate (WindowHandle);
  try
    { Anzahl der DrawGrid-Zeilen anhand der aktiven (nicht ausgefilterten) Einträge
      in der Abgleichliste festlegen: }
    if MeldAbgleichList.GetAnzahlAktiv = 0 then
      DrawGrid.RowCount:=DrawGrid.FixedRows + 1
    else
      DrawGrid.RowCount:=DrawGrid.FixedRows + MeldAbgleichList.GetAnzahlAktiv;

    { im DrawGrid ans Ende scrollen: }
    DrawGrid.TopRow:=DrawGrid.RowCount - DrawGrid.VisibleRowCount;

    { letzte Meldung im DrawGrid markieren: }
    BottomRect.Left:=DrawGrid.ColCount - 1;
    BottomRect.Top:=DrawGrid.RowCount - 1;
    BottomRect.Right:=0;
    BottomRect.Bottom:=DrawGrid.RowCount - 1;
    DrawGrid.Selection:=BottomRect;

    { Spalten ein-/ausblenden }
    SetDrawGridColWidths;
    DrawGrid.Invalidate;
  finally
    LockWindowUpdate (0);
    DrawGrid.Enabled:=true;
  end;

  { Anzahl der Meldungen und Filter-Status in Statusbar anzeigen: }
  Statusbar.Panels[0].Text:=
    IntToStr (MeldAbgleichList.GetAnzahlAktiveEinzelmeldungen) + ' Meldungen';
  case GetFilterStatus of
    1: Statusbar.Panels[1].Text:='Nicht quittierte Meldungen';
    2: Statusbar.Panels[1].Text:='Filter aktiv';
  else
    Statusbar.Panels[1].Text:='';
  end;

  { Schalter aktivieren/deaktivieren: }
  sbtnQuittieren.Enabled:=MeldAbgleichList.GetAnzahlAktiv > 0;
  sbtnDrucken.Enabled:=MeldAbgleichList.GetAnzahlAktiv > 0;

  { Fenstertitel setzen }
  SetCaption;
end;

{---------------------------------------------------}
procedure TFormMeldungenSichten.SetDrawGridColWidths;
{---------------------------------------------------}
{ Spaltenbreiten des DrawGrid setzen, um Spalten ein- bzw. auszublenden }
begin
  if MeldFilterIniFile.FeldQuittiert then
    DrawGrid.ColWidths [0]:=CWidthQuittiert
  else
    DrawGrid.ColWidths [0]:=-1;
  if MeldFilterIniFile.FeldDatumZeit then
    DrawGrid.ColWidths [1]:=CWidthDatumZeit
  else
    DrawGrid.ColWidths [1]:=-1;
  if (GeraeteArt = C_GerArtMrg) OR not MeldFilterIniFile.FeldZeitzone then
    DrawGrid.ColWidths [2]:=-1
  else
    DrawGrid.ColWidths [2]:=CWidthZeitzone;
  if MeldFilterIniFile.FeldMeldung then
    DrawGrid.ColWidths [3]:=CWidthMText
  else
    DrawGrid.ColWidths [3]:=-1;
  if EinzelStation OR (GeraeteArt = C_GerArtMrg) OR (GeraeteArt = C_GerArtDSfG) OR
     not MeldFilterIniFile.FeldGeraeteArt then
    DrawGrid.ColWidths [4]:=-1
  else
    DrawGrid.ColWidths [4]:=CWidthGerArt;
  if EinzelStation OR not MeldFilterIniFile.FeldStation then
    DrawGrid.ColWidths [5]:=-1
  else
    DrawGrid.ColWidths [5]:=CWidthStation;
  if (GeraeteArt = C_GerArtMrg) OR not MeldFilterIniFile.FeldOrdNr then
    DrawGrid.ColWidths [6]:=-1
  else
    DrawGrid.ColWidths [6]:=CWidthOrdNr;
  if MeldFilterIniFile.FeldMNrGeraet then
    DrawGrid.ColWidths [7]:=CWidthMNrGeraet
  else
    DrawGrid.ColWidths [7]:=-1;
  if MeldFilterIniFile.FeldMNrAllg then
    DrawGrid.ColWidths [8]:=CWidthMNrAllg
  else
    DrawGrid.ColWidths [8]:=-1;
  if (GeraeteArt = C_GerArtMrg) OR not MeldFilterIniFile.FeldDSfGStatus then
    DrawGrid.ColWidths [9]:=-1
  else
    DrawGrid.ColWidths [9]:=CWidthStatus;
  if (GeraeteArt = C_GerArtMrg) OR not MeldFilterIniFile.FeldCRC then
    DrawGrid.ColWidths [10]:=-1
  else
    DrawGrid.ColWidths [10]:=CWidthCRC;
  if MeldFilterIniFile.FeldBemerkung then
    DrawGrid.ColWidths [11]:=CWidthBemerkung
  else
    DrawGrid.ColWidths [11]:=-1;
end;

{--------------------------------------------}
procedure TFormMeldungenSichten.CreateBitmaps;
{--------------------------------------------}
{ Bitmaps erzeugen }
begin
  { "Quittiert" }
  bmpQuittiert:=TBitmap.Create;
  bmpQuittiert.Transparent:=true;
  bmpQuittiert.LoadFromResourceName (HInstance, 'QUITTIERT');

  { Meldungstyp "Kommt/Geht" }
  bmpKommt:=TBitmap.Create;
  bmpKommt.Transparent:=true;
  bmpKommt.LoadFromResourceName (HInstance, 'KOMMT');  { auch für einwertige Meldungen }

  bmpGeht:=TBitmap.Create;
  bmpGeht.Transparent:=true;
  bmpGeht.LoadFromResourceName (HInstance, 'GEHT');

  { Zeitzone "Sommerzeit/Winterzeit" }
  bmpSommerzeit:=TBitmap.Create;
  bmpSommerzeit.Transparent:=true;
  bmpSommerzeit.LoadFromResourceName (HInstance, 'SOMMERZEIT');

  bmpWinterzeit:=TBitmap.Create;
  bmpWinterzeit.Transparent:=true;
  bmpWinterzeit.LoadFromResourceName (HInstance, 'WINTERZEIT');

  { Meldungsarten "Hinweis/Warnung/Störung/Rechnerfehler" }
  bmpHinweis:=TBitmap.Create;
  bmpHinweis.Transparent:=true;
  bmpHinweis.LoadFromResourceName (HInstance, 'HINWEIS');

  bmpWarnung:=TBitmap.Create;
  bmpWarnung.Transparent:=true;
  bmpWarnung.LoadFromResourceName (HInstance, 'WARNUNG');

  bmpStoerung:=TBitmap.Create;
  bmpStoerung.Transparent:=true;
  bmpStoerung.LoadFromResourceName (HInstance, 'STOERUNG');

  bmpRechnerfehler:=TBitmap.Create;
  bmpRechnerfehler.Transparent:=true;
  bmpRechnerfehler.LoadFromResourceName (HInstance, 'RECHNERFEHLER');
end;

{------------------------------------------}
procedure TFormMeldungenSichten.FreeBitmaps;
{------------------------------------------}
{ Bitmaps freigeben }
begin
  bmpRechnerfehler.Free;
  bmpStoerung.Free;
  bmpWarnung.Free;
  bmpHinweis.Free;
  bmpWinterzeit.Free;
  bmpSommerzeit.Free;
  bmpGeht.Free;
  bmpKommt.Free;
  bmpQuittiert.Free;
end;

{---------------------------------------------------}
function TFormMeldungenSichten.GetFilterStatus: byte;
{---------------------------------------------------}
{ gibt Status der Filtereinstellungen zurück;
  Ergebnis: 0 = einschränkende Filterkriterien sind nicht gesetzt (alle Meldungen
                werden angezeigt) )
            1 = nur quittierte Meldungen werden ausgefiltert (Filterdefault,
                keine wirkliche Einschränkung)
            2 = mind. ein einschränkendes Filterkriterium ist gesetzt (Filter aktiv) }
var
  FilterAktiv: boolean;
begin
  { prüfen, ob mind. ein einschränkendes Filterkriterium gesetzt ist: }
  FilterAktiv:=MeldFilterIniFile.FilterDatumZeit OR
               not MeldFilterIniFile.FilterHinweis OR
               not MeldFilterIniFile.FilterWarnung OR
               not MeldFilterIniFile.FilterStoerung OR
               not MeldFilterIniFile.FilterRechnerfehler OR
               not MeldFilterIniFile.FilterNichtQuittiert;
  if not EinzelStation then begin
    if GeraeteArt = C_GerArtMrg then
      FilterAktiv:=FilterAktiv OR (MeldFilterIniFile.FilterMRGStation > 0)
    else if GeraeteArt = C_GerArtDSfG then
      FilterAktiv:=FilterAktiv OR (MeldFilterIniFile.FilterDSfGInstanz > 0)
    else
      FilterAktiv:=FilterAktiv OR
                   not MeldFilterIniFile.FilterMRG OR
                   not MeldFilterIniFile.FilterDSfG OR
                   (MeldFilterIniFile.FilterMRGStation > 0) OR
                   (MeldFilterIniFile.FilterDSfGInstanz > 0);
  end;

  if FilterAktiv then
    Result:=2
  else begin
    if not MeldFilterIniFile.FilterQuittiert then
      Result:=1
    else
      Result:=0;
  end;
end;

{--------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawGridDrawCell(Sender: TObject; Col,
            Row: integer; Rect: TRect; State: TGridDrawState);
{--------------------------------------------------------------------}
{ Zelleninhalt zeichnen; eine Meldung wird nur gezeichnet, wenn sie den
  Filterkriterien entspricht (d.h MeldAbgleichList.GetAktiv = true) }
begin
  if Row = 0 then
    DrawSpaltenueberschrift (Col, Row, Rect)
  else if (Row > 0) AND (MeldAbgleichList.GetAnzahlAktiv > 0) then
    DrawMeldung (Col, Row, Rect);
end;

{---------------------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawSpaltenueberschrift (Col, Row: integer; Rect: TRect);
{---------------------------------------------------------------------------------------}
{ Zeichnen einer Spaltenüberschrift }
var
  S: String;
begin
  case Col of
    0: S := 'Qu.';
    1: S := 'Datum/Zeit';
    2: S := 'Zz.';
    3: S := 'Meldung';
    4: S := 'Geräteart';
    5: S := 'Station';
    6: S := 'Ord.Nr.';
    7: S := 'Meld.Nr. Gerät';
    8: S := 'Meld.Nr. allg.';
    9: S := 'DSfG-Status';
   10: S := 'CRC';
   11: S := 'Bemerkung';
  else
    S := '';
  end;
  WriteTextoben (Rect, S, 'L');
end;

{---------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawMeldung (Col, Row: integer; Rect: TRect);
{---------------------------------------------------------------------------}
{ Zeichnen der Meldungsdaten }
begin
  with DrawGrid.Canvas do begin
    case Col of
      0: DrawQuittiert (Row, Rect);
      1: DrawDatumZeit (Row, Rect);
      2: DrawZeitzone (Row, Rect);
      3: DrawMeldungstext (Row, Rect);
      4: DrawGeraeteArt (Row, Rect);
      5: DrawStation (Row, Rect);
      6: DrawOrdnungsnummer (Row, Rect);
      7: DrawMeldNrGeraet (Row, Rect);
      8: DrawMeldNrAllg (Row, Rect);
      9: DrawDSfGStatus (Row, Rect);
     10: DrawDSfGCRC (Row, Rect);
     11: DrawBemerkung (Row, Rect);
    end;
  end;
end;

{ Ausgabe der Quittierung }
{------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawQuittiert (Row: integer; Rect: TRect);
{------------------------------------------------------------------------}
begin
  if MeldAbgleichList.GetQuittiert (Row - DrawGrid.FixedRows) then
    DrawGrid.Canvas.Draw (Rect.Left + CSpace, Rect.Top + 8, bmpQuittiert);
end;

{ Ausgabe von Kommt- und Geht-Datum }
{------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawDatumZeit (Row: integer; Rect: TRect);
{------------------------------------------------------------------------}
var
  DatumZeit: TDateTime;
  Str_K: string;
  Str_G: string;

begin
  DatumZeit:=MeldAbgleichList.GetDatumZeit_K (Row - DrawGrid.FixedRows);
  if DatumZeit > -1 then
    Str_K:=FormatDateTime ('dd. mmm. yyyy hh:nn:ss', DatumZeit)
  else
    Str_K:='';

  DatumZeit:=MeldAbgleichList.GetDatumZeit_G (Row - DrawGrid.FixedRows);
  if DatumZeit > -1 then
    Str_G:=FormatDateTime ('dd. mmm. yyyy hh:nn:ss', DatumZeit)
  else
    Str_G:='';

  WriteDatumZeit (Rect, Str_K, Str_G);
end;

{ Ausgabe der Zeitzone }
{-----------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawZeitzone (Row: integer; Rect: TRect);
{-----------------------------------------------------------------------}
var
  RectBuf: TRect;
  Zeitzone: string;
begin
  { Zeitzone-Bitmap für einwertige bzw. Kommt-Meldung: }
  Zeitzone:=UpperCase (MeldAbgleichList.GetZeitzone_K (Row - DrawGrid.FixedRows));
  if Zeitzone = 'S' then
    DrawGrid.Canvas.Draw (Rect.Left + CSpace, Rect.Top, bmpSommerzeit)
  else if Zeitzone = 'M' then
    DrawGrid.Canvas.Draw (Rect.Left + CSpace, Rect.Top, bmpWinterzeit);

  { Zeitzone-Bitmap für Geht-Meldung: }
  RectBuf:=Rect;
  RectBuf.Top:=RectBuf.Top + (RectBuf.Bottom - RectBuf.Top) DIV 2;

  Zeitzone:=UpperCase (MeldAbgleichList.GetZeitzone_G (Row - DrawGrid.FixedRows));
  if Zeitzone = 'S' then
    DrawGrid.Canvas.Draw (RectBuf.Left + CSpace, RectBuf.Top, bmpSommerzeit)
  else if Zeitzone = 'M' then
    DrawGrid.Canvas.Draw (RectBuf.Left + CSpace, RectBuf.Top, bmpWinterzeit);
end;

{ Ausgabe der DSfG-Ordnungsnummer }
{-----------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawOrdnungsnummer (Row: integer; Rect: TRect);
{-----------------------------------------------------------------------------}
var
  OrdNr: integer;
begin
 { Ordnungsnummern existieren nur bei DSfG-Meldungen, bei MRG-Meldungen sind sie
   mit -1 belegt: }
  OrdNr:=MeldAbgleichList.GetOrdnungsNr_K (Row - DrawGrid.FixedRows);
  if OrdNr > -1 then
    WriteTextoben (Rect, IntToStr (OrdNr), 'R');

  OrdNr:=MeldAbgleichList.GetOrdnungsNr_G (Row - DrawGrid.FixedRows);
  if OrdNr > -1 then
    WriteTextunten (Rect, IntToStr (OrdNr), 'R');
end;

{ Ausgabe der gerätespezifischen Meldungs-Nummer }
{---------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawMeldNrGeraet (Row: integer; Rect: TRect);
{---------------------------------------------------------------------------}
begin
  WriteTextoben (Rect, MeldAbgleichList.GetMNrGeraet_K (Row - DrawGrid.FixedRows), 'R');
  WriteTextunten (Rect, MeldAbgleichList.GetMNrGeraet_G (Row - DrawGrid.FixedRows), 'R');
end;

{ Ausgabe der allgemeinen Meldungs-Nummer }
{-------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawMeldNrAllg (Row: integer; Rect: TRect);
{-------------------------------------------------------------------------}
begin
  WriteTextoben (Rect, MeldAbgleichList.GetMNrAllg_K (Row - DrawGrid.FixedRows), 'R');
  WriteTextunten (Rect, MeldAbgleichList.GetMNrAllg_G (Row - DrawGrid.FixedRows), 'R');
end;

{ Ausgabe der Geräte-Art }
{-------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawGeraeteArt (Row: integer; Rect: TRect);
{-------------------------------------------------------------------------}
var
  GeraeteArtStr: string;
  S: string;
begin
  GeraeteArtStr:=MeldAbgleichList.GetGeraeteArt (Row - DrawGrid.FixedRows);
  if GeraeteArtStr = C_GerArtMrg then
    S:=C_AnzeigeMrg
  else if GeraeteArtStr = C_GerArtDSfG then
    S:=C_AnzeigeDSfG
  else
    S:=GeraeteArtStr;
  WriteTextoben (Rect, S, 'L');
end;

{ Ausgabe des Meldungstextes }
{---------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawMeldungstext (Row: integer; Rect: TRect);
{---------------------------------------------------------------------------}
var
  Textoben: string;
  Textunten: string;
  ParaText: string;
  MArt: string;

begin
  { Meldungstext: }
  Textoben:=MeldAbgleichList.GetMText (Row - DrawGrid.FixedRows);
  ParaText:=MeldAbgleichList.GetPara_Text (Row - DrawGrid.FixedRows);
  if length (ParaText) > 0 then begin
    Textoben:=Textoben + ': ' + ParaText;
    Textunten:='alt: ' + MeldAbgleichList.GetPara_WertAlt (Row - DrawGrid.FixedRows) + '  ' +
               'neu: ' + MeldAbgleichList.GetPara_WertNeu (Row - DrawGrid.FixedRows);
  end else
    Textunten:='';
  { Bitmap für Meldungsart: }
  if length (MeldAbgleichList.GetMArt_K (Row - DrawGrid.FixedRows)) > 0 then
    MArt:=MeldAbgleichList.GetMArt_K (Row - DrawGrid.FixedRows)
  else
    MArt:=MeldAbgleichList.GetMArt_G (Row - DrawGrid.FixedRows);

  WriteMeldungtext (Rect, MArt, Textoben, Textunten);
end;

{ Ausgabe von Stationsname und Kennung (MRG) bzw. Stationsname und Instanzname (DSfG) }
{----------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawStation (Row: integer; Rect: TRect);
{----------------------------------------------------------------------}
begin
  WriteTextoben (Rect, MeldAbgleichList.GetStationstext1 (Row - DrawGrid.FixedRows), 'L');
  WriteTextunten (Rect, MeldAbgleichList.GetStationstext2 (Row - DrawGrid.FixedRows), 'L');
end;

{ Ausgabe des DSfG-Status einer DSfG-Meldung }
{-------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawDSfGStatus (Row: integer; Rect: TRect);
{-------------------------------------------------------------------------}
begin
  WriteTextoben (Rect, MeldAbgleichList.GetDSfG_Status_K (Row - DrawGrid.FixedRows), 'R');
  WriteTextunten (Rect, MeldAbgleichList.GetDSfG_Status_G (Row - DrawGrid.FixedRows), 'R');
end;

{ Ausgabe des CRC einer DSfG-Meldung }
{----------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawDSfGCRC (Row: integer; Rect: TRect);
{----------------------------------------------------------------------}
begin
  WriteTextoben (Rect, MeldAbgleichList.GetDSfG_CRC_K (Row - DrawGrid.FixedRows), 'R');
  WriteTextunten (Rect, MeldAbgleichList.GetDSfG_CRC_G (Row - DrawGrid.FixedRows), 'R');
end;

{ Ausgabe der Kommt/Geht-Bemerkung zur Meldung }
{------------------------------------------------------------------------}
procedure TFormMeldungenSichten.DrawBemerkung (Row: integer; Rect: TRect);
{------------------------------------------------------------------------}
begin
  WriteTextoben (Rect, MeldAbgleichList.GetBemerkung_K (Row - DrawGrid.FixedRows), 'L');
  WriteTextunten (Rect, MeldAbgleichList.GetBemerkung_G (Row - DrawGrid.FixedRows), 'L');
end;


{-----------------------------------------------------------------------}
procedure TFormMeldungenSichten.WriteTextoben (Rect: TRect; Text: string;
                                               Ausrichtung: char);
{-----------------------------------------------------------------------}
{ Text ausgeben in der oberen Zeile einer Row mit linksbündiger oder
  rechtsbündiger Ausrichtung }
var
  ALeft: integer;
  RectBuf: TRect;
  TextWidth: integer;

begin
  if length (Text) > 0 then begin
    RectBuf:=Rect;
    case Ausrichtung of
      'R': begin    { rechtsbündig }
             TextWidth:=DrawGrid.Canvas.TextWidth (Text);
             ALeft:=RectBuf.Right - TextWidth - 2*CSpace;
             if ALeft > RectBuf.Left then
               RectBuf.Left:=ALeft;
           end;
    end;
    DrawGrid.Canvas.TextRect (RectBuf, RectBuf.Left + CSpace, RectBuf.Top + CSpace, Text);
  end;
end;

{------------------------------------------------------------------------}
procedure TFormMeldungenSichten.WriteTextunten (Rect: TRect; Text: string;
                                                Ausrichtung: char);
{------------------------------------------------------------------------}
{ Text ausgeben in der unteren Zeile einer Row mit linksbündiger oder
  rechtsbündiger Ausrichtung }
var
  ALeft: integer;
  RectBuf: TRect;
  TextWidth: integer;

begin
  if length (Text) > 0 then begin
    RectBuf:=Rect;
    RectBuf.Top:=RectBuf.Top + (RectBuf.Bottom - RectBuf.Top) DIV 2;
    case Ausrichtung of
      'R': begin    { rechtsbündig }
             TextWidth:=DrawGrid.Canvas.TextWidth (Text);
             ALeft:=RectBuf.Right - TextWidth - 2*CSpace;
             if ALeft > RectBuf.Left then
               RectBuf.Left:=ALeft;
           end;
    end;
    DrawGrid.Canvas.TextRect (RectBuf, RectBuf.Left + CSpace, RectBuf.Top + CSpace, Text);
  end;
end;

{-------------------------------------------------------------------}
procedure TFormMeldungenSichten.WriteDatumZeit (Rect: TRect;
                                                DatumZeit_K: string;
                                                DatumZeit_G: string);
{-------------------------------------------------------------------}
{ Datum/Zeit und Kommt/Geht-Bitmaps zeichnen }
var
  RectBuf: TRect;

begin
  if length (DatumZeit_K) > 0 then begin
    { Kommt-Bitmap: }
    DrawGrid.Canvas.Draw (Rect.Left + CSpace, Rect.Top + 1, bmpKommt);
    { Kommt-Zeit: }
    RectBuf:=Rect;
    RectBuf.Left:=RectBuf.Left + 20;
    DrawGrid.Canvas.TextRect (RectBuf, RectBuf.Left, RectBuf.Top + CSpace, DatumZeit_K);
  end;

  if length (DatumZeit_G) > 0 then begin
    { Geht-Bitmap: }
    RectBuf:=Rect;
    RectBuf.Top:=RectBuf.Top + (RectBuf.Bottom - RectBuf.Top) DIV 2;
    DrawGrid.Canvas.Draw (RectBuf.Left + CSpace, RectBuf.Top + 1, bmpGeht);
    { Geht-Zeit: }
    RectBuf:=Rect;
    RectBuf.Left:=RectBuf.Left + 20;
    RectBuf.Top:=RectBuf.Top + (RectBuf.Bottom - RectBuf.Top) DIV 2;
    DrawGrid.Canvas.TextRect (RectBuf, RectBuf.Left, RectBuf.Top + CSpace, DatumZeit_G);
  end;
end;

{-------------------------------------------------------------------}
procedure TFormMeldungenSichten.WriteMeldungtext (Rect: TRect;
                                                  MArt: string;
                                                  Textoben: string;
                                                  Textunten: string);
{-------------------------------------------------------------------}
{ Meldungsart-Bitmap und Meldungstext zeichnen }
var
  RectBuf: TRect;
begin
  { Meldungsart-Bitmap (Hinweis, Warnung etc.): }
  if MArt = mart_Hinweis then
    DrawGrid.Canvas.Draw (Rect.Left + CSpace, Rect.Top + 8, bmpHinweis)
  else if MArt = mart_Warnung then
    DrawGrid.Canvas.Draw (Rect.Left + CSpace, Rect.Top + 8, bmpWarnung)
  else if MArt = mart_Stoerung then
    DrawGrid.Canvas.Draw (Rect.Left + CSpace, Rect.Top + 8, bmpStoerung)
  else if MArt = mart_Rechnerfehler then
    DrawGrid.Canvas.Draw (Rect.Left + CSpace, Rect.Top + 8, bmpRechnerfehler);

  { Meldungstext: }
  if length (Textoben) > 0 then begin
    { Text für erste Zeile: }
    RectBuf:=Rect;
    RectBuf.Left:=RectBuf.Left + 20;
    DrawGrid.Canvas.TextRect (RectBuf, RectBuf.Left, RectBuf.Top + CSpace, Textoben);
  end;

  if length (Textunten) > 0 then begin
    { Text für zweite Zeile: }
    RectBuf:=Rect;
    RectBuf.Left:=RectBuf.Left + 20;
    RectBuf.Top:=RectBuf.Top + (RectBuf.Bottom - RectBuf.Top) DIV 2;
    DrawGrid.Canvas.TextRect (RectBuf, RectBuf.Left, RectBuf.Top + CSpace, Textunten);
  end;
end;

{--------------------------------------------}
procedure TFormMeldungenSichten.LoadMeldungen;
{--------------------------------------------}
{ Meldungen aus Meldungs-Tabellen lesen und in Abgleichliste eintragen mit
  Kommt/Geht-Abgleich }
var
  MeldungenDb: TMeldungenDb;
  Query: TQueryExt;
  Save_Cursor: TCursor;
  Benutzer: string;

begin
  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  try
    MeldungenDb:=TMeldungenDb.Create (Path);
    try
      Query:=TQueryExt.Create (nil);
      try
        if MeldFilterIniFile.FilterAbrufart = 0 then
          Benutzer:=C_AbrArtAuto
        else
          Benutzer:=C_AbrArtManu;

        { Meldungen aus Tabelle lesen: }
        if not MeldungenDb.GetMeldungen (Query, GeraeteArt, GeraeteId, Benutzer) then
          MessageDlg ('Fehler beim Zugriff auf Meldungstabellen aufgetreten !', mtError, [mbOk], 0);
        { Kommt/Geht-Abgleich in Abgleichliste durchführen: }
        MeldAbgleichList.Fuellen_und_Abgleichen (Query);
      finally
        Query.Close;
        Query.Free;
      end;
    finally
      MeldungenDb.Free;
    end;

    MeldAbgleichList.SetFilterundSortierung;     { Filterkriterien aktivieren }
    InitAnzeige;
  finally
    Screen.Cursor:=Save_Cursor;
  end;
end;

{-------------------------------------------------------------------------}
procedure TFormMeldungenSichten.miQuittiereMarkierteClick(Sender: TObject);
{-------------------------------------------------------------------------}
{ markierte Meldung(en) quittieren }
begin
  QuittiereMeldungen (1);
end;

{--------------------------------------------------------------------}
procedure TFormMeldungenSichten.miQuittiereAlleClick(Sender: TObject);
{--------------------------------------------------------------------}
{ alle angezeigten Meldungen quittieren }
begin
  QuittiereMeldungen (0);
end;

{-------------------------------------------------------------------}
procedure TFormMeldungenSichten.QuittiereMeldungen (AllOrSome: byte);
{-------------------------------------------------------------------}
{ Quittieren von Meldungen;
  Übergabe: AllOrSome (0 - Alle, 1 - Markierte) }
var
  Row: integer;
  MeldungenDb: TMeldungenDb;
  OK: boolean;
  vonRow: integer;
  bisRow: integer;
  Save_Cursor: TCursor;

begin
  Save_Cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;
  DrawGrid.Enabled:=false;
  try
    OK:=true;
    MeldungenDb:=TMeldungenDb.Create (Path);
    try
      if MeldungenDb.OpenMeldungenTable (false) then begin
        try
          if AllOrSome = 1 then begin
            vonRow:=DrawGrid.Selection.Top;
            bisRow:=DrawGrid.Selection.Bottom;
          end
          else begin
            vonRow:=DrawGrid.FixedRows;
            bisRow:=DrawGrid.RowCount - 1;
          end;

          for Row:=vonRow to bisRow do begin
            if MeldAbgleichList.GetMeldungId_K (Row - DrawGrid.FixedRows) > -1 then begin
              if MeldungenDb.QuittiereMeldung (                     { Tabelle }
                   MeldAbgleichList.GetMeldungId_K (Row - DrawGrid.FixedRows), false) then
                MeldAbgleichList.SetQuittiert (Row - DrawGrid.FixedRows, true)  { Abgleichliste }
              else
                OK:=false;
            end;

            if MeldAbgleichList.GetMeldungId_G (Row - DrawGrid.FixedRows) > -1 then begin
              if MeldungenDb.QuittiereMeldung (                       { Tabelle }
                   MeldAbgleichList.GetMeldungId_G (Row - DrawGrid.FixedRows), false) then
                MeldAbgleichList.SetQuittiert (Row - DrawGrid.FixedRows, true)  { Abgleichliste }
              else
                OK:=false;
            end;
          end;  { for }
        finally
          MeldungenDb.CloseMeldungenTable;
        end;
      end;
    finally
      MeldungenDb.Free;
    end;

    if Assigned (FMeldTableTime) then
      { aktuellen Tabellen-Zeitstempel zurückgeben: }
      FMeldTableTime (FileAge(Path + C_Tb_WMeldungen));

    DrawGrid.Invalidate;                          { DrawGrid neu zeichnen }
  finally
    DrawGrid.Enabled:=true;
    Screen.Cursor:=Save_Cursor;
  end;

  if not OK then
    MessageDlg ('Fehler beim Quittieren der Meldungen aufgetreten !', mtError, [mbOk], 0);
end;

{-------------------------------------------------------------------}
procedure TFormMeldungenSichten.sbtnQuittierenClick(Sender: TObject);
{-------------------------------------------------------------------}
begin
  if DrawGrid.Selection.Top < DrawGrid.Selection.Bottom then begin
    if MessageDlg('Markierte Meldungen quittieren ?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then begin
      Application.ProcessMessages;
      QuittiereMeldungen (1);
    end;
  end
  else begin
    if MessageDlg('Alle Meldungen quittieren ?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then begin
      Application.ProcessMessages;
      QuittiereMeldungen (0);
    end;
  end;
end;

{----------------------------------------------------------------------}
procedure TFormMeldungenSichten.sbtnAnzeigeFilterClick(Sender: TObject);
{----------------------------------------------------------------------}
{ Filter-Kriterien-Dialog anzeigen }
var
  FormDlgMeldSichtFilter: TFormDlgMeldSichtFilter;
  Abrufart_Alt: byte;

begin
  FormDlgMeldSichtFilter:=TFormDlgMeldSichtFilter.Create
    (Self, IniFileName, Path, GeraeteArt, EinzelStation);
  try
    Abrufart_alt:=MeldFilterIniFile.FilterAbrufart;
    if FormDlgMeldSichtFilter.ShowModal = mrOk then begin
      { nur beim Wechsel des Abrufart-Filters müssen die Meldungen neu aus der Tabelle
        geladen werden: }
      if MeldFilterIniFile.FilterAbrufart <> Abrufart_alt then
        LoadMeldungen
      else begin
        MeldAbgleichList.SetFilterundSortierung; { Filterkriterien aktivieren }
        InitAnzeige;
      end;
    end;
  finally
    FormDlgMeldSichtFilter.Free;
  end;
end;

{----------------------------------------------------------------------}
procedure TFormMeldungenSichten.sbtnAktualisierenClick(Sender: TObject);
{----------------------------------------------------------------------}
{ Meldungen neu aus Tabelle laden und anzeigen }
begin
  LoadMeldungen;
end;

{----------------------------------------------------------------}
procedure TFormMeldungenSichten.miBemerkungClick(Sender: TObject);
{----------------------------------------------------------------}
{ Dialog zur Eingabe einer Bemerkung öffnen }
var
  FormDlgMeldBemerkung: TFormDlgMeldBemerkung;
  OK: boolean;
  MeldungenDb: TMeldungenDb;
  Row: integer;
  forKommt: boolean;
  forGeht: boolean;
  Bemerkung_K: string;
  Bemerkung_G: string;

begin
  { nur, wenn Meldungen vorhanden sind und nicht mehrere Zeilen markiert sind: }
  if not ((MeldAbgleichList.GetAnzahlAktiv > 0) AND
          (DrawGrid.Selection.Top = DrawGrid.Selection.Bottom)) then exit;
  Row:=DrawGrid.Selection.Top;

  forKommt:=MeldAbgleichList.GetMeldungId_K (Row - DrawGrid.FixedRows) > -1;
  forGeht:=MeldAbgleichList.GetMeldungId_G (Row - DrawGrid.FixedRows) > -1;
  if forKommt then
    Bemerkung_K:=MeldAbgleichList.GetBemerkung_K (Row - DrawGrid.FixedRows)
  else
    Bemerkung_K:='';
  if forGeht then
    Bemerkung_G:=MeldAbgleichList.GetBemerkung_G (Row - DrawGrid.FixedRows)
  else
    Bemerkung_G:='';

  FormDlgMeldBemerkung:=TFormDlgMeldBemerkung.Create (
    Self,
    MeldAbgleichList.GetStationstext1 (Row - DrawGrid.FixedRows),
    MeldAbgleichList.GetStationstext2 (Row - DrawGrid.FixedRows),
    MeldAbgleichList.GetMText (Row - DrawGrid.FixedRows),
    forKommt, forGeht, Bemerkung_K, Bemerkung_G);
  try
    if FormDlgMeldBemerkung.ShowModal = mrOK then begin
      OK:=true;
      MeldungenDb:=TMeldungenDb.Create (Path);
      try
        if forKommt then begin
          { in Tabelle eintragen: }
          if MeldungenDb.WriteBemerkung (MeldAbgleichList.GetMeldungId_K (Row - DrawGrid.FixedRows),
                                         FormDlgMeldBemerkung.BemerkungNeu_K) then
            { Abgleichliste updaten: }
            MeldAbgleichList.SetBemerkung_K (Row - DrawGrid.FixedRows,
                                             FormDlgMeldBemerkung.BemerkungNeu_K)
          else
            OK:=false;
        end;

        if forGeht then begin
          { in Tabelle eintragen: }
          if MeldungenDb.WriteBemerkung (MeldAbgleichList.GetMeldungId_G (Row - DrawGrid.FixedRows),
                                         FormDlgMeldBemerkung.BemerkungNeu_G) then
            { Abgleichliste updaten: }
            MeldAbgleichList.SetBemerkung_G (Row - DrawGrid.FixedRows,
                                             FormDlgMeldBemerkung.BemerkungNeu_G)
          else
            OK:=false;
        end;

        if Assigned (FMeldTableTime) then
          { aktuellen Tabellen-Zeitstempel zurückgeben: }
          FMeldTableTime (FileAge(Path + C_Tb_WMeldungen));

        DrawGrid.Invalidate;                          { DrawGrid neu zeichnen }
      finally
        MeldungenDb.Free;
      end;
      if not OK then
        MessageDlg ('Fehler beim Speichern der Meldungs-Bemerkung aufgetreten !', mtError, [mbOk], 0);
    end;
  finally
    FormDlgMeldBemerkung.Free;
  end;
end;

{----------------------------------------------------------------}
procedure TFormMeldungenSichten.pmMeldungenPopup(Sender: TObject);
{----------------------------------------------------------------}
{ Menüpunkt "Bemerkung" nur enabled, wenn nicht mehrere Zeilen markiert sind }
begin
  if MeldAbgleichList.GetAnzahlAktiv > 0 then begin
    miQuittiereMarkierte.Enabled:=true;
    miQuittiereAlle.Enabled:=true;
    { nur, wenn nicht mehrere Zeilen markiert sind: }
    if DrawGrid.Selection.Top = DrawGrid.Selection.Bottom then  
      miBemerkung.Enabled:=true
    else
      miBemerkung.Enabled:=false;
  end
  else begin
    miQuittiereMarkierte.Enabled:=false;
    miQuittiereAlle.Enabled:=false;
    miBemerkung.Enabled:=false;
  end;
end;

{----------------------------------------------------------------}
procedure TFormMeldungenSichten.sbtnDruckenClick(Sender: TObject);
{----------------------------------------------------------------}
{ angezeigte Meldungen drucken  }
var
  FormPrintMeldungen: TFormPrintMeldungen;

begin
  if MessageDlg ('Angezeigte Meldungen drucken ?',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    FormPrintMeldungen:=TFormPrintMeldungen.Create (
      Self, MeldAbgleichList, Path, IniFileName, GeraeteArt, GeraeteId);
    try
      FormPrintMeldungen.Execute;
    finally
      FormPrintMeldungen.Free;
    end;
  end;
end;

end.
