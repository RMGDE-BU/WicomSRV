{******************************************************************************}
{* Unit: QuickReport zum Ausdrucken von Meldungen                             *}
{*       -> gedruckt wird der Inhalt einer Liste vom Typ TMeldAbgleichList    *}
{* 12.07.2001  WW                                                             *}
{* 15.03.2002  WW Ausgabe auf unter Printer.PrinterIndex eingestelltem Drucker*}
{******************************************************************************}
unit FPrintMeldungen;

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, quickrpt, Qrctrls, PathIni, MeldAbgleichListe, MDbSta, DDbSta, DListen,
  WSysCon, MeldFilterIni, MeldungenDb, WTables, Printers;

type
  TFormPrintMeldungen = class(TForm)
    QuickReport: TQuickRep;
    qrbPageHeader: TQRBand;
    qrbMeldungen: TQRBand;
    qrlMeldQuittiert: TQRLabel;
    qrbSummary: TQRBand;
    qrlMeldCount: TQRLabel;
    qrImageLogo: TQRImage;
    qrlTitel1: TQRLabel;
    qrlTitel2: TQRLabel;
    qrlTitel3: TQRLabel;
    qrlGedruckt: TQRLabel;
    qrSysDate: TQRSysData;
    qrSysTime: TQRSysData;
    qrSysPageNr: TQRSysData;
    qrbSpaltenUeberschriften: TQRBand;
    qrlQuittiert: TQRLabel;
    qrlDatumZeit: TQRLabel;
    qrlMText: TQRLabel;
    qrlZeitzone: TQRLabel;
    qrlGeraeteArt: TQRLabel;
    qrlStation: TQRLabel;
    qrlOrdNr: TQRLabel;
    qrlMNrGeraet: TQRLabel;
    qrlMNrAllg: TQRLabel;
    qrlDSfGStatus: TQRLabel;
    qrlCRC: TQRLabel;
    qrlBemerkung: TQRLabel;
    qrlMeldZeitzone_K: TQRLabel;
    qrlMeldDatumZeit_K: TQRLabel;
    qrlMeldMText1: TQRLabel;
    qrlMeldGeraeteart: TQRLabel;
    qrlMeldStation1: TQRLabel;
    qrlMeldOrdNr_K: TQRLabel;
    qrlMeldMNrGeraet_K: TQRLabel;
    qrlMeldMNrAllg_K: TQRLabel;
    qrlMeldDSfGStatus_K: TQRLabel;
    qrlMeldCRC_K: TQRLabel;
    qrlMeldBemerkung_K: TQRLabel;
    qrshapeLine: TQRShape;
    qrlMeldDatumZeit_G: TQRLabel;
    qrlMeldZeitzone_G: TQRLabel;
    qrlMeldMText2: TQRLabel;
    qrlMeldStation2: TQRLabel;
    qrlMeldOrdNr_G: TQRLabel;
    qrlMeldMNrGeraet_G: TQRLabel;
    qrlMeldMNrAllg_G: TQRLabel;
    qrlMeldDSfGStatus_G: TQRLabel;
    qrlMeldCRC_G: TQRLabel;
    qrlMeldBemerkung_G: TQRLabel;
    qrImageMeldKommt: TQRImage;
    qrImageMeldGeht: TQRImage;
    qrlFiltersortierKriterien: TQRLabel;
    qrlFSKritGerArt: TQRLabel;
    qrlFSKritMRGEinzelstation: TQRLabel;
    qrlFSKritDSfGEinzelinstanz: TQRLabel;
    qrlFSKritZeitraum: TQRLabel;
    qrlFSKritQuittierung: TQRLabel;
    qrlFSKritMeldArt: TQRLabel;
    qrlSortKrit: TQRLabel;
    procedure QuickReportBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
    procedure QuickReportNeedData(Sender: TObject; var MoreData: Boolean);
    procedure qrbMeldungenBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    nil_Liste_uebergeben: boolean;
    Path: string;
    IniFileName: string;
    GeraeteArt: string;
    GeraeteId: integer;
    ListIndex: integer;
    EinzelStation: boolean;
    MeldAbgleichList: TMeldAbgleichList;
    MeldFilterIniFile: TMeldFilterIniFile;
    procedure LoadMeldungen;
    procedure SetProtokollkopf;
    function SetSpalten: integer;
    procedure SetQuittiert (ListIndex: integer);
    procedure SetDatumZeit (ListIndex: integer);
    procedure SetZeitzone (ListIndex: integer);
    procedure SetMeldung (ListIndex: integer);
    procedure SetGeraeteArt (ListIndex: integer);
    procedure SetStation (ListIndex: integer);
    procedure SetOrdnungsnummer (ListIndex: integer);
    procedure SetMeldNrGeraet (ListIndex: integer);
    procedure SetMeldNrAllg (ListIndex: integer);
    procedure SetDSfGStatus (ListIndex: integer);
    procedure SetCRC (ListIndex: integer);
    procedure SetBemerkung (ListIndex: integer);
    procedure SetZusammenfassung;
  public
    constructor Create (AOwner: TComponent; AMeldAbgleichList: TMeldAbgleichList;
                        APath: string; AIniFileName: string;
                        AGeraeteArt: string; AGeraeteId: integer); reintroduce;
    procedure Execute;
  end;

implementation

{$R *.DFM}

const
  { Spaltenbreiten }

  CWidthQuittiert = 20;
  CWidthDatumZeit = 114;
  CWidthZeitzone  = 20;
  CWidthOrdNr     = 50;
  CWidthMNrGeraet = 50;
  CWidthMNrAllg   = 50;
  CWidthGerArt    = 50;
  CWidthMText     = 240;
  CWidthStation   = 150;
  CWidthStatus    = 40;
  CWidthCRC       = 30;


{--------------------------------------------------------------------------------}
constructor TFormPrintMeldungen.Create (AOwner: TComponent;
                                        AMeldAbgleichList: TMeldAbgleichList;
                                        APath: string; AIniFileName: string;
                                        AGeraeteArt: string; AGeraeteId: integer);
{--------------------------------------------------------------------------------}
{ Constructor-Übergaben:
    APath (Pfad für Meldungstabellen und Stammdaten)
    AMeldAbgleichList (Listeninhalt wird gedruckt bzw. wenn nil, wird die Liste
                       mit den nachfolgenden Übergaben erstellt)
    AIniFileName (Datei, in der die Filter-Einstellungen abgespeichert sind)
    AGeraeteArt ('M' = nur MRG-Geräte; 'D' = nur DSfG-Geräte; '' = beide)
    AGeraeteId (MRG-Station-Id bzw. DSfG-Quell-InstanzId; <= 0 -> "alle" ) }
begin
  inherited Create (AOwner);
  MeldAbgleichList:=AMeldAbgleichList;
  Path:=APath;
  IniFileName:=AIniFileName;
  GeraeteArt:=AGeraeteArt;
  GeraeteId:=AGeraeteId;

  EinzelStation:=GeraeteId > 0;
  nil_Liste_uebergeben:=AMeldAbgleichList = nil;
  if nil_Liste_uebergeben then                          { Liste wird erstellt }
    MeldAbgleichList:=TMeldAbgleichList.Create (Path, IniFileName,
                                                GeraeteArt, GeraeteId);
  MeldFilterIniFile:=TMeldFilterIniFile.Create (IniFileName);
end;

{------------------------------------}
procedure TFormPrintMeldungen.Execute;
{------------------------------------}
begin
  { es soll auf dem Drucker gedruckt werden, der im Programm eingestellt ist
    (z.B. über TPrinterSetupDialog): }
  QuickReport.PrinterSettings.PrinterIndex:=Printer.PrinterIndex;  // 15.03.2002 WW

  QuickReport.Print;
//  QuickReport.Preview;    // nur zum Testen
end;

{---------------------------------------------------------}
procedure TFormPrintMeldungen.FormDestroy(Sender: TObject);
{---------------------------------------------------------}
begin
  MeldFilterIniFile.Free;
  if nil_Liste_uebergeben then      { erstellte Liste wird wieder freigegeben }
    MeldAbgleichList.Free;
end;

{---------------------------------------------------------------------------}
procedure TFormPrintMeldungen.QuickReportBeforePrint(Sender: TCustomQuickRep;
            var PrintReport: Boolean);
{---------------------------------------------------------------------------}
var
  Cols: integer;
begin
  if nil_Liste_uebergeben then
    LoadMeldungen;                                 { Meldungen in Liste laden }
  SetProtokollkopf;
  Cols:=SetSpalten;
  if Cols > 5 then
    QuickReport.Page.Orientation:=poLandscape   { Querformat bei mehr als 5 Spalten }
  else
    QuickReport.Page.Orientation:=poPortrait;   { sonst Hochformat }
  qrShapeLine.Width:=qrbMeldungen.Width;    { anpassen wegen Hoch-/Querformat }
  SetZusammenfassung;
  ListIndex:=0;
end;

{----------------------------------------------------------------------------------------}
procedure TFormPrintMeldungen.QuickReportNeedData(Sender: TObject; var MoreData: Boolean);
{----------------------------------------------------------------------------------------}
begin
  if ListIndex < MeldAbgleichList.GetAnzahlAktiv then begin
    SetQuittiert (ListIndex);
    SetDatumZeit (ListIndex);
    SetZeitzone (ListIndex);
    SetMeldung (ListIndex);
    SetGeraeteArt (ListIndex);
    SetStation (ListIndex);
    SetOrdnungsnummer (ListIndex);
    SetMeldNrGeraet (ListIndex);
    SetMeldNrAllg (ListIndex);
    SetDSfGStatus (ListIndex);
    SetCRC (ListIndex);
    SetBemerkung (ListIndex);

    inc(ListIndex);
    MoreData:=true;
  end else
    MoreData:=false;
end;

{----------------------------------------------------}
procedure TFormPrintMeldungen.qrbMeldungenBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
{----------------------------------------------------}
{ nach der letzten Meldung keine gestrichelte Linie mehr zeichnen }
begin
  if ListIndex >= MeldAbgleichList.GetAnzahlAktiv then
    qrShapeLine.Enabled:=false;
end;

{------------------------------------------}
procedure TFormPrintMeldungen.LoadMeldungen;
{------------------------------------------}
{ Meldungen aus Meldungs-Tabellen lesen und in Abgleichliste eintragen mit
  Kommt/Geht-Abgleich }
var
  MeldungenDb: TMeldungenDb;
  Query: TQueryExt;
  Benutzer: string;

begin
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
end;

{---------------------------------------------}
procedure TFormPrintMeldungen.SetProtokollkopf;
{---------------------------------------------}
{ Protokollkopf ausgeben }
var
  Titel1: string;
  Titel2: string;
  MRGStammdaten: TMRGStammdaten;
  DSfGStammdaten: TDSfGStammdaten;
  OK: boolean;
  StaData: TStaData;
  StationData: TStationData;
  InstanzData: TInstanzData;

begin
  if MeldFilterIniFile.FilterAbrufart = 0 then
    Titel1:='Automatik-Meldungen'
  else
    Titel1:='Manuelle Meldungen';

  Titel2:='';
  if EinzelStation then begin
    Titel1:=Titel1 + ' von Station';
    if Geraeteart = C_GerArtMrg then begin
      { MRG-Stationsname und Kennung ermitteln: }
      OK:=true;
      MRGStammdaten:=TMRGStammdaten.Create (Path);
      try
        if MRGStammdaten.InitTabellen then begin
          if MRGStammdaten.GetStaData (GeraeteId, StaData) then
            Titel2:=StaData.StationsName + ' (' + StaData.Kennung + ')';
        end else  { if InitTabellen }
          OK:=false;
      finally
        MRGStammdaten.Free;
      end;
      if not OK then
        MessageDlg ('Fehler beim Zugriff auf MRG-Stammdaten aufgetreten !', mtError, [mbOk], 0);
    end;

    if Geraeteart <> C_GerArtMrG then begin
      { DSfG-Stationsname und Instanzname eintragen: }
      OK:=true;
      DSfGStammdaten:=TDSfGStammdaten.Create (Path);
      try
        if DSfGStammdaten.InitTabellen then begin
          if DSfGStammdaten.GetInstanzData (GeraeteId, InstanzData) then
            if DSfGStammdaten.GetStationData (InstanzData.StationId, StationData) then
              Titel2:=StationData.Stationsname + '/' + InstanzData.Instanzname;
        end else  { if InitTabellen }
          OK:=false;
      finally
        DSfGStammdaten.Free;
      end;
      if not OK then
        MessageDlg ('Fehler beim Zugriff auf DSfG-Stammdaten aufgetreten !', mtError, [mbOk], 0);
    end;
  end;

  qrImageLogo.Picture.LoadFromFile (PathServer.LogoName);
  qrlTitel1.Caption:=Titel1;
  qrlTitel2.Caption:=Titel2;
end;

{-----------------------------------------------}
function TFormPrintMeldungen.SetSpalten: integer;
{-----------------------------------------------}
{ Spaltenbreiten und Position der Detailband-Labels setzen; Labels enablen bzw.
  disablen, um Spalten ein- bzw. auszublenden;
  Ergebnis: Anzahl der Spalten }
const
  xAbstand = 10;

var
  xPos: integer;
  WidthBemerkung: integer;
  ColCount: integer;

begin
  ColCount:=0;
  xPos:=0;

  qrlMeldQuittiert.Left:=xPos;
  qrlMeldQuittiert.Width:=CWidthQuittiert;
  qrlMeldQuittiert.Enabled:=MeldFilterIniFile.FeldQuittiert;
  qrlQuittiert.Left:=qrlMeldQuittiert.Left;
  qrlQuittiert.Enabled:=qrlMeldQuittiert.Enabled;
  if qrlQuittiert.Enabled then begin
    xPos:=xPos + qrlMeldQuittiert.Width + xAbstand;
    inc (ColCount);
  end;


  qrImageMeldKommt.Left:=xPos;
  qrImageMeldGeht.Left:=xPos;
  qrlMeldDatumZeit_K.Left:=xPos + qrImageMeldKommt.Width;
  qrlMeldDatumZeit_K.Width:=CWidthDatumZeit;
  qrlMeldDatumZeit_K.Enabled:=MeldFilterIniFile.FeldDatumZeit;
  qrlMeldDatumZeit_G.Left:=qrlMeldDatumZeit_K.Left;
  qrlMeldDatumZeit_G.Width:=qrlMeldDatumZeit_K.Width;
  qrlMeldDatumZeit_G.Enabled:=qrlMeldDatumZeit_K.Enabled;
  qrlDatumZeit.Left:=qrImageMeldKommt.Left;
  qrlDatumZeit.Enabled:=qrlMeldDatumZeit_K.Enabled;
  if qrlDatumZeit.Enabled then begin
    xPos:=xPos + qrImageMeldKommt.Width + qrlMeldDatumZeit_K.Width + xAbstand;
    inc (ColCount);
  end;

  qrlMeldZeitzone_K.Left:=xPos;
  qrlMeldZeitzone_K.Width:=CWidthZeitzone;
  qrlMeldZeitzone_K.Enabled:=not ((GeraeteArt = C_GerArtMrg) OR not MeldFilterIniFile.FeldZeitzone);
  qrlMeldZeitzone_G.Left:=qrlMeldZeitzone_K.Left;
  qrlMeldZeitzone_G.Width:=qrlMeldZeitzone_K.Width;
  qrlMeldZeitzone_G.Enabled:=qrlMeldZeitzone_K.Enabled;
  qrlZeitzone.Left:=qrlMeldZeitzone_K.Left;
  qrlZeitzone.Enabled:=qrlMeldZeitzone_K.Enabled;
  if qrlZeitzone.Enabled then begin
    xPos:=xPos + qrlMeldZeitzone_K.Width + xAbstand;
    inc (ColCount);
  end;

  qrlMeldMText1.Left:=xPos;
  qrlMeldMText1.Width:=CWidthMText;
  qrlMeldMText1.Enabled:=MeldFilterIniFile.FeldMeldung;
  qrlMeldMText2.Left:=qrlMeldMText1.Left;
  qrlMeldMText2.Width:=qrlMeldMText1.Width;
  qrlMeldMText2.Enabled:=qrlMeldMText1.Enabled;
  qrlMText.Left:=qrlMeldMText1.Left;
  qrlMText.Enabled:=qrlMeldMText1.Enabled;
  if qrlMText.Enabled then begin
    xPos:=xPos + qrlMeldMText1.Width + xAbstand;
    inc (ColCount);
  end;

  { Geräteart und Station in einer Spalte: }
  qrlMeldGeraeteArt.Left:=xPos;
  qrlMeldGeraeteArt.Width:=CWidthGerArt;
  qrlMeldGeraeteArt.Enabled:=not (EinzelStation OR (GeraeteArt = C_GerArtMrg) OR
                                  (GeraeteArt = C_GerArtDSfG) OR
                                  not MeldFilterIniFile.FeldGeraeteArt);
  qrlGeraeteArt.Left:=qrlMeldGeraeteArt.Left;
  qrlGeraeteArt.Enabled:=qrlMeldGeraeteArt.Enabled;

  qrlMeldStation1.Left:=xPos;
  qrlMeldStation1.Width:=CWidthStation;
  qrlMeldStation1.Enabled:=not (EinzelStation OR not MeldFilterIniFile.FeldStation);
  qrlMeldStation2.Left:=qrlMeldStation1.Left;
  qrlMeldStation2.Width:=qrlMeldStation1.Width;
  qrlMeldStation2.Enabled:=qrlMeldStation1.Enabled;
  qrlStation.Left:=qrlMeldStation1.Left;
  qrlStation.Enabled:=qrlMeldStation1.Enabled;
  if qrlStation.Enabled then begin
    xPos:=xPos + qrlMeldStation1.Width + xAbstand;
    inc (ColCount);
  end
  else if qrlGeraeteArt.Enabled then begin
    xPos:=xPos + qrlMeldGeraeteArt.Width + xAbstand;
    inc (ColCount);
  end;

  qrlMeldOrdNr_K.Left:=xPos;
  qrlMeldOrdNr_K.Width:=CWidthOrdNr;
  qrlMeldOrdNr_K.Enabled:=not ((GeraeteArt = C_GerArtMrg) OR not MeldFilterIniFile.FeldOrdNr);
  qrlMeldOrdNr_G.Left:=qrlMeldOrdNr_K.Left;
  qrlMeldOrdNr_G.Width:=qrlMeldOrdNr_K.Width;
  qrlMeldOrdNr_G.Enabled:=qrlMeldOrdNr_K.Enabled;
  qrlOrdNr.Left:=qrlMeldOrdNr_K.Left;
  qrlOrdNr.Enabled:=qrlMeldOrdNr_K.Enabled;
  if qrlOrdNr.Enabled then begin
    xPos:=xPos + qrlMeldOrdNr_K.Width + xAbstand;
    inc (ColCount);
  end;

  qrlMeldMNrGeraet_K.Left:=xPos;
  qrlMeldMNrGeraet_K.Width:=CWidthMNrGeraet;
  qrlMeldMNrGeraet_K.Enabled:=MeldFilterIniFile.FeldMNrGeraet;
  qrlMeldMNrGeraet_G.Left:=qrlMeldMNrGeraet_K.Left;
  qrlMeldMNrGeraet_G.Width:=qrlMeldMNrGeraet_K.Width;
  qrlMeldMNrGeraet_G.Enabled:=qrlMeldMNrGeraet_K.Enabled;
  qrlMNrGeraet.Left:=qrlMeldMNrGeraet_K.Left;
  qrlMNrGeraet.Enabled:=qrlMeldMNrGeraet_K.Enabled;
  if qrlMNrGeraet.Enabled then begin
    xPos:=xPos + qrlMeldMNrGeraet_K.Width + xAbstand;
    inc (ColCount);
  end;

  qrlMeldMNrAllg_K.Left:=xPos;
  qrlMeldMNrAllg_K.Width:=CWidthMNrAllg;
  qrlMeldMNrAllg_K.Enabled:=MeldFilterIniFile.FeldMNrAllg;
  qrlMeldMNrAllg_G.Left:=qrlMeldMNrAllg_K.Left;
  qrlMeldMNrAllg_G.Width:=qrlMeldMNrAllg_K.Width;
  qrlMeldMNrAllg_G.Enabled:=qrlMeldMNrAllg_K.Enabled;
  qrlMNrAllg.Left:=qrlMeldMNrAllg_K.Left;
  qrlMNrAllg.Enabled:=qrlMeldMNrAllg_K.Enabled;
  if qrlMNrAllg.Enabled then begin
    xPos:=xPos + qrlMeldMNrAllg_K.Width + xAbstand;
    inc (ColCount);
  end;

  qrlMeldDSfGStatus_K.Left:=xPos;
  qrlMeldDSfGStatus_K.Width:=CWidthStatus;
  qrlMeldDSfGStatus_K.Enabled:=not ((GeraeteArt = C_GerArtMrg) OR not MeldFilterIniFile.FeldDSfGStatus);
  qrlMeldDSfGStatus_G.Left:=qrlMeldDSfGStatus_K.Left;
  qrlMeldDSfGStatus_G.Width:=qrlMeldDSfGStatus_K.Width;
  qrlMeldDSfGStatus_G.Enabled:=qrlMeldDSfGStatus_K.Enabled;
  qrlDSfGStatus.Left:=qrlMeldDSfGStatus_K.Left;
  qrlDSfGStatus.Enabled:=qrlMeldDSfGStatus_K.Enabled;
  if qrlDSfGStatus.Enabled then begin
    xPos:=xPos + qrlMeldDSfGStatus_K.Width + xAbstand;
    inc (ColCount);
  end;

  qrlMeldCRC_K.Left:=xPos;
  qrlMeldCRC_K.Width:=CWidthCRC;
  qrlMeldCRC_K.Enabled:=not ((GeraeteArt = C_GerArtMrg) OR not MeldFilterIniFile.FeldCRC);
  qrlMeldCRC_G.Left:=qrlMeldCRC_K.Left;
  qrlMeldCRC_G.Width:=qrlMeldCRC_K.Width;
  qrlMeldCRC_G.Enabled:=qrlMeldCRC_K.Enabled;
  qrlCRC.Left:=qrlMeldCRC_K.Left;
  qrlCRC.Enabled:=qrlMeldCRC_K.Enabled;
  if qrlCRC.Enabled then begin
    xPos:=xPos + qrlMeldCRC_K.Width + xAbstand;
    inc (ColCount);
  end;

  qrlMeldBemerkung_K.Left:=xPos;
  WidthBemerkung:=qrbMeldungen.Width - qrlMeldBemerkung_K.Left;
  if WidthBemerkung < 0 then
    WidthBemerkung:=0;
  qrlMeldBemerkung_K.Width:=WidthBemerkung;
  qrlMeldBemerkung_K.Enabled:=MeldFilterIniFile.FeldBemerkung;
  qrlMeldBemerkung_G.Left:=qrlMeldBemerkung_K.Left;
  qrlMeldBemerkung_G.Width:=qrlMeldBemerkung_K.Width;
  qrlMeldBemerkung_G.Enabled:=qrlMeldBemerkung_K.Enabled;
  qrlBemerkung.Left:=qrlMeldBemerkung_K.Left;
  qrlBemerkung.Enabled:=qrlMeldBemerkung_K.Enabled;
  if qrlBemerkung.Enabled then
    inc (ColCount);

  Result:=ColCount;
end;

{--------------------------------------------------------------}
procedure TFormPrintMeldungen.SetQuittiert (ListIndex: integer);
{--------------------------------------------------------------}
{ Ausgabe der Quittierung }
begin
  if MeldAbgleichList.GetQuittiert (ListIndex) then
    qrlMeldQuittiert.Caption:='J'
  else
    qrlMeldQuittiert.Caption:='N';
end;

{--------------------------------------------------------------}
procedure TFormPrintMeldungen.SetDatumZeit (ListIndex: integer);
{--------------------------------------------------------------}
{ Ausgabe von Kommt- und Geht-Datum }
var
  DatumZeit: TDateTime;
  S: string;
begin
  DatumZeit:=MeldAbgleichList.GetDatumZeit_K (ListIndex);
  if DatumZeit > -1 then begin
    qrImageMeldKommt.Enabled:=true;
    S:=FormatDateTime ('dd. mmm. yyyy hh:nn:ss', DatumZeit);
  end
  else begin
    qrImageMeldKommt.Enabled:=false;
    S:='';
  end;
  qrlMeldDatumZeit_K.Caption:=S;

  DatumZeit:=MeldAbgleichList.GetDatumZeit_G (ListIndex);
  if DatumZeit > -1 then begin
    qrImageMeldGeht.Enabled:=true;
    S:=FormatDateTime ('dd. mmm. yyyy hh:nn:ss', DatumZeit)
  end
  else begin
    qrImageMeldGeht.Enabled:=false;
    S:='';
  end;
  qrlMeldDatumZeit_G.Caption:=S;
end;

{-------------------------------------------------------------}
procedure TFormPrintMeldungen.SetZeitzone (ListIndex: integer);
{-------------------------------------------------------------}
{ Ausgabe der Zeitzone }
begin
  qrlMeldZeitzone_K.Caption:=MeldAbgleichList.GetZeitzone_K (ListIndex);
  qrlMeldZeitzone_G.Caption:=MeldAbgleichList.GetZeitzone_G (ListIndex);
end;

{------------------------------------------------------------}
procedure TFormPrintMeldungen.SetMeldung (ListIndex: integer);
{------------------------------------------------------------}
{ Ausgabe des Meldungstextes }
var
  Text1: string;
  Text2: string;
  ParaText: string;

begin
  { Meldungstext: }
  Text1:=MeldAbgleichList.GetMText (ListIndex);
  ParaText:=MeldAbgleichList.GetPara_Text (ListIndex);
  if length (ParaText) > 0 then begin
    Text1:=Text1 + ': ' + ParaText;
    Text2:='alt: ' + MeldAbgleichList.GetPara_WertAlt (ListIndex) + '  ' +
           'neu: ' + MeldAbgleichList.GetPara_WertNeu (ListIndex);
  end else
    Text2:='';

  qrlMeldMText1.Caption:=Text1;
  qrlMeldMText2.Caption:=Text2;
end;

{---------------------------------------------------------------}
procedure TFormPrintMeldungen.SetGeraeteArt (ListIndex: integer);
{---------------------------------------------------------------}
{ Ausgabe der Geräte-Art }
var
  GeraeteArtStr: string;
  S: string;
begin
  GeraeteArtStr:=MeldAbgleichList.GetGeraeteArt (ListIndex);
  if GeraeteArtStr = C_GerArtMrg then
    S:=C_AnzeigeMrg
  else if GeraeteArtStr = C_GerArtDSfG then
    S:=C_AnzeigeDSfG
  else
    S:=GeraeteArtStr;
  qrlMeldGeraeteArt.Caption:=S;
end;

{------------------------------------------------------------}
procedure TFormPrintMeldungen.SetStation (ListIndex: integer);
{------------------------------------------------------------}
{ Ausgabe von Stationsname und Kennung (MRG) bzw. Stationsname und Instanzname (DSfG) }
begin
  qrlMeldStation1.Caption:=MeldAbgleichList.GetStationstext1 (ListIndex);
  qrlMeldStation2.Caption:=MeldAbgleichList.GetStationstext2 (ListIndex);
end;

{-------------------------------------------------------------------}
procedure TFormPrintMeldungen.SetOrdnungsnummer (ListIndex: integer);
{-------------------------------------------------------------------}
{ Ausgabe der DSfG-Ordnungsnummer }
var
  OrdNr: integer;
  S: string;
begin
 { Ordnungsnummern existieren nur bei DSfG-Meldungen, bei MRG-Meldungen sind sie
   mit -1 belegt: }
  OrdNr:=MeldAbgleichList.GetOrdnungsNr_K (ListIndex);
  if OrdNr > -1 then
    S:=IntToStr (OrdNr)
  else
    S:='';
  qrlMeldOrdNr_K.Caption:=S;

  OrdNr:=MeldAbgleichList.GetOrdnungsNr_G (ListIndex);
  if OrdNr > -1 then
    S:=IntToStr (OrdNr)
  else
    S:='';
  qrlMeldOrdNr_G.Caption:=S;
end;

{-----------------------------------------------------------------}
procedure TFormPrintMeldungen.SetMeldNrGeraet (ListIndex: integer);
{-----------------------------------------------------------------}
{ Ausgabe der gerätespezifischen Meldungs-Nummer }
begin
  qrlMeldMNrGeraet_K.Caption:=MeldAbgleichList.GetMNrGeraet_K (ListIndex);
  qrlMeldMNrGeraet_G.Caption:=MeldAbgleichList.GetMNrGeraet_G (ListIndex);
end;

{---------------------------------------------------------------}
procedure TFormPrintMeldungen.SetMeldNrAllg (ListIndex: integer);
{---------------------------------------------------------------}
{ Ausgabe der allgemeinen Meldungs-Nummer }
begin
  qrlMeldMNrAllg_K.Caption:=MeldAbgleichList.GetMNrAllg_K (ListIndex);
  qrlMeldMNrAllg_G.Caption:=MeldAbgleichList.GetMNrAllg_G (ListIndex);
end;

{---------------------------------------------------------------}
procedure TFormPrintMeldungen.SetDSfGStatus (ListIndex: integer);
{---------------------------------------------------------------}
{ Ausgabe des DSfG-Status einer DSfG-Meldung }
begin
  qrlMeldDSfGStatus_K.Caption:=MeldAbgleichList.GetDSfG_Status_K (ListIndex);
  qrlMeldDSfGStatus_G.Caption:=MeldAbgleichList.GetDSfG_Status_G (ListIndex);
end;

{--------------------------------------------------------}
procedure TFormPrintMeldungen.SetCRC (ListIndex: integer);
{-------------------------------------------------------}
{ Ausgabe des CRC einer DSfG-Meldung }
begin
  qrlMeldCRC_K.Caption:=MeldAbgleichList.GetDSfG_CRC_K (ListIndex);
  qrlMeldCRC_G.Caption:=MeldAbgleichList.GetDSfG_CRC_G (ListIndex);
end;

{--------------------------------------------------------------}
procedure TFormPrintMeldungen.SetBemerkung (ListIndex: integer);
{--------------------------------------------------------------}
{ Ausgabe der Kommt/Geht-Bemerkung zur Meldung }
begin
  qrlMeldBemerkung_K.Caption:=MeldAbgleichList.GetBemerkung_K (ListIndex);
  qrlMeldBemerkung_G.Caption:=MeldAbgleichList.GetBemerkung_G (ListIndex);
end;

{-----------------------------------------------}
procedure TFormPrintMeldungen.SetZusammenfassung;
{-----------------------------------------------}
{ Ausgabe der Zusammenfassung am Ende des Reports }
var
  isMRG: boolean;
  isDSfG: boolean;
  MRGStammdaten: TMRGStammdaten;
  DSfGStammdaten: TDSfGStammdaten;
  OK: boolean;
  StaData: TStaData;
  StationData: TStationData;
  InstanzData: TInstanzData;
  isQuittierte: boolean;
  isNichtQuittierte: boolean;
  isHinweis: boolean;
  isWarnung: boolean;
  isStoerung: boolean;
  isRechnerfehler: boolean;
  S: string;
  yPos: integer;

begin
  { Anzahl der Meldungen: }
  qrlMeldCount.Caption:=IntToStr (MeldAbgleichList.GetAnzahlAktiveEinzelmeldungen) + ' Meldungen';

  { Filterkriterien: }
  yPos:=qrlFilterSortierKriterien.Top;

  { Geräteart: }
  if (GeraeteArt <> C_GerArtMrg) AND (GeraeteArt <> C_GerArtDSfG) then begin
    isMRG:=MeldFilterIniFile.FilterMRG;
    isDSfG:=MeldFilterIniFile.FilterDSfG;
    if isMRG AND not isDSfG then begin
      qrlFSKritGerArt.Caption:='Nur MRG-Meldungen';
      qrlFSKritGerArt.Enabled:=true;
      qrlFSKritGerArt.Top:=yPos;
      yPos:=yPos + qrlFSKritGerArt.Height;
    end
    else if not isMRG AND isDSfG then begin
      qrlFSKritGerArt.Caption:='Nur DSfG-Meldungen';
      qrlFSKritGerArt.Enabled:=true;
      qrlFSKritGerArt.Top:=yPos;
      yPos:=yPos + qrlFSKritGerArt.Height;
    end;
  end;

  if not Einzelstation then begin
    if GeraeteArt <> C_GerArtDSfG then begin              { MRG oder MRG/DSfG }
      { MRG-Einzelstation: }
      if MeldFilterIniFile.FilterMRGStation > 0 then begin
        { MRG-Stationsname und Kennung ermitteln: }
        OK:=true;
        MRGStammdaten:=TMRGStammdaten.Create (Path);
        try
          if MRGStammdaten.InitTabellen then begin
            if MRGStammdaten.GetStaData (MeldFilterIniFile.FilterMRGStation, StaData) then begin
              qrlFSKritMRGEinzelstation.Caption:='Nur MRG-Station ' +
                StaData.StationsName + ' (' + StaData.Kennung + ')';
              qrlFSKritMRGEinzelstation.Enabled:=true;
              qrlFSKritMRGEinzelstation.Top:=yPos;
              yPos:=yPos + qrlFSKritMRGEinzelstation.Height;
            end;
          end else  { if InitTabellen }
            OK:=false;
        finally
          MRGStammdaten.Free;
        end;
        if not OK then
          MessageDlg ('Fehler beim Zugriff auf MRG-Stammdaten aufgetreten !', mtError, [mbOk], 0);
      end;
    end;

    if GeraeteArt <> C_GerArtMrg then begin              { DSfG oder MRG/DSfG }
    { DSfG-Einzelinstanz: }
      if MeldFilterIniFile.FilterDSfGInstanz > 0 then begin
        OK:=true;
        DSfGStammdaten:=TDSfGStammdaten.Create (Path);
        try
          if DSfGStammdaten.InitTabellen then begin
            if DSfGStammdaten.GetInstanzData (MeldFilterIniFile.FilterDSfGInstanz, InstanzData) then begin
              if DSfGStammdaten.GetStationData (InstanzData.StationId, StationData) then begin
                qrlFSKritDSfGEinzelinstanz.Caption:='Nur DSfG-Instanz ' +
                  StationData.Stationsname + '/' + InstanzData.Instanzname;
                qrlFSKritDSfGEinzelinstanz.Enabled:=true;
                qrlFSKritDSfGEinzelinstanz.Top:=yPos;
                yPos:=yPos + qrlFSKritDSfGEinzelinstanz.Height;
              end;
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
  end;

  { Zeitraum: }
  if MeldFilterIniFile.FilterDatumZeit then begin
    qrlFSKritZeitraum.Caption:=
      'Nur Meldungen vom ' + DateTimeToStr (MeldFilterIniFile.FilterDatumVon) +
      ' bis ' + DateTimeToStr (MeldFilterIniFile.FilterDatumBis);
    qrlFSKritZeitraum.Enabled:=true;
    qrlFSKritZeitraum.Top:=yPos;
    yPos:=yPos + qrlFSKritZeitraum.Height;
  end;

  { Quittierung: }
  isQuittierte:=MeldFilterIniFile.FilterQuittiert;
  isNichtQuittierte:=MeldFilterIniFile.FilterNichtQuittiert;
  if isQuittierte AND not isNichtQuittierte then begin
    qrlFSKritQuittierung.Caption:='Nur quittierte Meldungen';
    qrlFSKritQuittierung.Enabled:=true;
    qrlFSKritQuittierung.Top:=yPos;
    yPos:=yPos + qrlFSKritQuittierung.Height;
  end
  else if not isQuittierte AND isNichtQuittierte then begin
    qrlFSKritQuittierung.Caption:='Nur nicht quittierte Meldungen';
    qrlFSKritQuittierung.Enabled:=true;
    qrlFSKritQuittierung.Top:=yPos;
    yPos:=yPos + qrlFSKritQuittierung.Height;
  end;

  { Meldungsarten: }
  isHinweis:=MeldFilterIniFile.FilterHinweis;
  isWarnung:=MeldFilterIniFile.FilterWarnung;
  isStoerung:=MeldFilterIniFile.FilterStoerung;
  isRechnerfehler:=MeldFilterIniFile.FilterRechnerfehler;
  if not (isHinweis AND isWarnung AND isStoerung AND isRechnerfehler) then begin
    S:='Nur ';
    if isHinweis then
      S:=S + 'Hinweise, ';
    if isWarnung then
      S:=S + 'Warnungen, ';
    if isStoerung then
      S:=S + 'Störungen, ';
    if isRechnerfehler then
      S:=S + 'Rechnerfehler, ';
    S:=Copy (S, 1, length (S)-2);
    qrlFSKritMeldArt.Caption:=S;
    qrlFSKritMeldArt.Enabled:=true;
    qrlFSKritMeldArt.Top:=yPos;
    yPos:=yPos + qrlFSKritMeldArt.Height;
  end;

  { Sortierkriterium: }
  case MeldFilterIniFile.Sortierung of
    0: begin
         qrlSortKrit.Caption:='Sortierung nach Datum/Zeit';
         qrlSortKrit.Enabled:=true;
         qrlSortKrit.Top:=yPos;
       end;
    1: begin
         qrlSortKrit.Caption:='Sortierung nach Station, Datum/Zeit';
         qrlSortKrit.Enabled:=true;
         qrlSortKrit.Top:=yPos;
       end;
    2: begin
         qrlSortKrit.Caption:='Sortierung nach Station, Ordnungsnr.';
         qrlSortKrit.Enabled:=true;
         qrlSortKrit.Top:=yPos;
       end;
    3: begin
         qrlSortKrit.Caption:='Sortierung nach Station, Abrufeingang';
         qrlSortKrit.Enabled:=true;
         qrlSortKrit.Top:=yPos;
       end;
  else
    qrlSortKrit.Caption:='Unbekanntes Sortierkriterium';
  end;
end;

end.
