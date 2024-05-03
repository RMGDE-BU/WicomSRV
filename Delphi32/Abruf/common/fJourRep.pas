{------------------------------------------------------------------------------}
{ 28.01.1999 GD:  Drucken des Ruf-Journals                                     }
{   Wird zur Laufzeit erzeugt -> nicht in automatische Erstellung              }
{                                                                              }
{ 12.04.1999 GD; Um Abrufarten Ruf und Momentanwerte erweitert                 }
{ 09.08.1999 WW; Erweiterungen für LAKS                                        }
{ 15.03.2002 WW; Ausgabe auf unter Printer.PrinterIndex eingestelltem Drucker  }
{ 30.01.2003 GD; Bugfix: in FormatDateTime Tage gegen Stunden getauscht        }
{------------------------------------------------------------------------------}

unit FJourRep;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, DbTables, quickrpt, Qrctrls, ExtCtrls, PathIni, WSysCon,
  Printers, JournlDb, ZSyncDb, My_Utils, ZBDatDb, ErrPrc32;

type
  TFormPrintJournal = class(TForm)
    QuickReport: TQuickRep;
    qrbdPageHeader: TQRBand;
    qrlUeberschrift: TQRLabel;
    qrlNeuAlt: TQRLabel;
    qrlIsFilter: TQRLabel;
    QRBand1: TQRBand;
    QRBandFieldNames: TQRBand;
    QRLabel1: TQRLabel;
    QRSysData1: TQRSysData;
    QRSysData3: TQRSysData;
    ImageWieser: TQRImage;
    QRBValues: TQRBand;
    qrlStatus: TQRLabel;
    qrlGeraeteArt: TQRLabel;
    qrlStationsName: TQRLabel;
    qrlKennung: TQRLabel;
    qrlAbrufArt: TQRLabel;
    qrlDatum: TQRLabel;
    qrlZeit: TQRLabel;
    qrdbtStatus: TQRDBText;
    qrdbtDatum: TQRDBText;
    qrdbtZeit: TQRDBText;
    qrdbtGeraeteArt: TQRDBText;
    qrdbtStationsName: TQRDBText;
    qrdbtKennung: TQRDBText;
    qrdbtAbrufArt: TQRDBText;
    qrbdSummary: TQRBand;
    qrlGesRecordCount: TQRLabel;
    qrlWeitereKriterien: TQRLabel;
    qrlFGerArt: TQRLabel;
    qrlFAbrArt: TQRLabel;
    qrlFZeitbereich: TQRLabel;
    qrlFZeitSync: TQRLabel;
    qrlFStatus: TQRLabel;
    qrlFZeitVon: TQRLabel;
    qrlFZeitBis: TQRLabel;
    qrlDatenTypen: TQRLabel;
    qrlZeitSync: TQRLabel;
    qrlZeitBereiche1: TQRLabel;
    qrlWarnungenFehler: TQRLabel;
    qrlDTMeldungen: TQRLabel;
    qrlDTMesswerte: TQRLabel;
    qrlDTPruefSaetze: TQRLabel;
    qrlDTParameter: TQRLabel;
    qrsysPageNr: TQRSysData;
    qrlFuW1: TQRLabel;
    qrlFuW2: TQRLabel;
    qrlFuW3: TQRLabel;
    qrlFuW4: TQRLabel;
    qrlFuW5: TQRLabel;
    qrlFuW6: TQRLabel;
    qrlFuW7: TQRLabel;
    qrlFuW8: TQRLabel;
    qrlZsPcZeit: TQRLabel;
    qrlZsMrgZeit: TQRLabel;
    qrlZsZeitDiff: TQRLabel;
    qrlZBMesswerte: TQRLabel;
    qrlZBSoll: TQRLabel;
    qrlZBIst: TQRLabel;
    qrlZeitBereiche2: TQRLabel;
    qrshapeLine: TQRShape;
    qrlDTArchive: TQRLabel;
    qrlDTLogbuecher: TQRLabel;
    qrlDTDatenelemente: TQRLabel;
    qrlDTBinaerdatei: TQRLabel;
    procedure FormCreate(Sender: TObject);
    procedure qrdbtStatusPrint(sender: TObject; var Value: String);
    procedure qrdbtGeraeteArtPrint(sender: TObject; var Value: String);
    procedure qrdbtAbrufArtPrint(sender: TObject; var Value: String);
  private
    txtGesRecCount: string;
    DetailHeight: integer;
    procedure qrlDatenTypenPrint;
    procedure qrlZeitBereichePrint;
    procedure qrlZeitSyncPrint;
    procedure qrlFuWPrint;
  public
    procedure SetJournalDruckEinstellungen(Quitt, DatenZB, ZeitSync, GerArt,
                                           AbrArt, Status: byte;
                                           ZeitVon, ZeitBis: TDateTime;
                                           GesRecCount: integer; Station: string);

    procedure PrintJournal(aQuery: TQuery;
                           isDatenTypen, isZeitBereiche,
                           isZeitSync, isFuW: boolean);
  end;

var
  FormPrintJournal: TFormPrintJournal;

implementation

{$R *.DFM}


{---------------------------------}
procedure TFormPrintJournal.FormCreate(Sender: TObject);
{---------------------------------}
begin
  ImageWieser.Picture.LoadFromFile (PathServer.LogoName);
end;

{ Umwandeln des Status-Integers in Text }
{---------------------------------}
procedure TFormPrintJournal.qrdbtStatusPrint(sender: TObject;
  var Value: String);
{---------------------------------}
begin
  if Value = '0' then Value:= 'OK' else Value:= 'Nicht OK';
  { Voreinstelung für die Höhe des Detail-Bandes }
  DetailHeight:= qrdbtKennung.Top + qrdbtKennung.Height + 4;
  { Nicht über Datenfelder verknüpfte Ausgaben werden angestoßen }
  if qrlDatenTypen.Enabled then qrlDatenTypenPrint;      { Datentypen }
  if qrlZeitBereiche1.Enabled then qrlZeitBereichePrint; { Daten-Zeitbereiche }
  if qrlZeitSync.Enabled then qrlZeitSyncPrint;          { Zeitsynchronisation }
  if qrlWarnungenFehler.Enabled then qrlFuWPrint;         { Zeitsynchronisation }
  { Voreinstelung für die Höhe des Detail-Bandes übernehmen }
  qrbValues.Height:= DetailHeight;
  if QuickReport.DataSet.RecNo = QuickReport.DataSet.RecordCount
    then qrshapeLine.Enabled:= false else qrshapeLine.Enabled:= true;
  qrshapeLine.Top:= DetailHeight - 6;
end;

{ Umwandeln des Geräteart-Kürzels in Text }
{---------------------------------}
procedure TFormPrintJournal.qrdbtGeraeteArtPrint(sender: TObject;
  var Value: String);
{---------------------------------}
begin
  if Value = C_GerArtMrg then Value:= C_AnzeigeMrg
  else if Value = C_GerArtLAKS then Value:= C_AnzeigeLAKS
  else if Value = C_GerArtGruppe then Value:= C_AnzeigeGruppe
  else if Value = C_GerArtDSfG then Value:= C_AnzeigeDSfG
  else Value:= 'Unbekannt';
end;

{ Umwandeln des Abrufart-Kürzels in Text }
{---------------------------------}
procedure TFormPrintJournal.qrdbtAbrufArtPrint(sender: TObject;
  var Value: String);
{---------------------------------}
begin
  if Value = C_AbrArtAuto then Value:= C_AnzeigeAuto
  else if Value = C_AbrArtManu then Value:= C_AnzeigeManu
  else if Value = C_AbrArtRuf then Value:= C_AnzeigeRuf
  else if (Value = C_AbrArtMomStart) or (Value = C_AbrArtMomStop) then Value:= C_AnzeigeMom
  else if Value = C_AbrArtRufReakt then Value:= C_AnzeigeRufReakt
  else if Value = C_AbrArtRueckRuf then Value:= C_AnzeigeRueckRuf
  else if Value = C_AbrArtKonfLesen then Value:= C_AnzeigeKonfLesen
  else if Value = C_AbrArtMomDfueStart then Value:= C_AnzeigeMomDfue
  else Value:= 'Unbekannt';
end;

{ Drucken der Datentypen in Labels }
{---------------------------------}
procedure TFormPrintJournal.qrlDatenTypenPrint;
{---------------------------------}
var
  i, z: integer;
begin
  i:= QuickReport.DataSet.FieldByName(C_WJournal_Datentypen).asInteger;
  z:= qrdbtGeraeteArt.Top;
  qrlDTMesswerte.Left:= qrlDatenTypen.Left;
  if (i and C_IsMesswerte) > 0 then begin
    qrlDTMesswerte.Enabled:= true;
    qrlDTMesswerte.Top:= z;
    qrlDTMesswerte.Left:= qrlDatenTypen.Left;
    z:= z + qrlDTMesswerte.Height;
  end
  else qrlDTMesswerte.Enabled:= false;
  if (i and C_IsMeldungen) > 0 then begin
    qrlDTMeldungen.Enabled:= true;
    qrlDTMeldungen.Top:= z;
    qrlDTMeldungen.Left:= qrlDatenTypen.Left;
    z:= z + qrlDTMeldungen.Height;
  end
  else qrlDTMeldungen.Enabled:= false;
  if (i and C_IsPruefsaetze) > 0 then begin
    qrlDTPruefSaetze.Enabled:= true;
    qrlDTPruefSaetze.Top:= z;
    qrlDTPruefSaetze.Left:= qrlDatenTypen.Left;
    z:= z + qrlDTPruefSaetze.Height;
  end
  else qrlDTPruefSaetze.Enabled:= false;
  if (i and C_IsParameter) > 0 then begin
    qrlDTParameter.Enabled:= true;
    qrlDTParameter.Top:= z;
    qrlDTParameter.Left:= qrlDatenTypen.Left;
    z:= z + qrlDTParameter.Height;
  end
  else qrlDTParameter.Enabled:= false;
  if (i and C_IsArchive) > 0 then begin
    qrlDTArchive.Enabled:= true;
    qrlDTArchive.Top:= z;
    qrlDTArchive.Left:= qrlDatenTypen.Left;
    z:= z + qrlDTArchive.Height;
  end
  else qrlDTArchive.Enabled:= false;
  if (i and C_IsLogbuecher) > 0 then begin
    qrlDTLogbuecher.Enabled:= true;
    qrlDTLogbuecher.Top:= z;
    qrlDTLogbuecher.Left:= qrlDatenTypen.Left;
    z:= z + qrlDTLogbuecher.Height;
  end
  else qrlDTLogbuecher.Enabled:=false;
  if (i and C_IsDatenelemente) > 0 then begin
    qrlDTDatenelemente.Enabled:= true;
    qrlDTDatenelemente.Top:= z;
    qrlDTDatenelemente.Left:= qrlDatenTypen.Left;
    z:= z + qrlDTDatenelemente.Height;
  end
  else qrlDTDatenelemente.Enabled:= false;
  if (i and C_IsBinaerdatei) > 0 then begin
    qrlDTBinaerdatei.Enabled:= true;
    qrlDTBinaerdatei.Top:= z;
    qrlDTBinaerdatei.Left:= qrlDatenTypen.Left;
    z:= z + qrlDTBinaerdatei.Height;
  end
  else qrlDTBinaerdatei.Enabled:= false;

  if z > DetailHeight then DetailHeight:= z;
end;

{ Drucken der Zeitbereiche der abgerufenen Daten in Labels }
{---------------------------------}
procedure TFormPrintJournal.qrlZeitBereichePrint;
{---------------------------------}
var
  i                : integer;
  tzb              : TZeitbereich;
  DatenZeitBereich : TMJZBDatenDB;
  strSollVon, strSollBis, strIstVon, strIstBis: string;
begin
  if (QuickReport.DataSet.FieldByName(C_WJournal_Datentypen).asInteger and C_IsMesswerte) > 0
  then begin
    qrlZBMesswerte.Left:= qrlZeitBereiche1.Left;
    qrlZBSoll.Left:= qrlZeitBereiche1.Left;
    qrlZBIst.Left:= qrlZeitBereiche1.Left;

    i:= QuickReport.DataSet.FieldByName(C_WJournal_JournalId).asInteger;
    DatenZeitBereich:= TMJZBDatenDB.Create(PathServer.Pathname[WStammDir]);
    tzb:= DatenZeitBereich.ReadZbDaten(i);
    DatenZeitBereich.Free;
    qrlZBMesswerte.Enabled:= true;
    qrlZBSoll.Enabled:= true;
    qrlZBIst.Enabled:=  true;
  { Wenn kein Eintrag vorhanden ist, sind alle Werte = 0 }
    if (tzb.SollVon = 0) and (tzb.SollVon = 0) and (tzb.SollVon = 0)
    and (tzb.SollVon = 0) then begin
      qrlZBMesswerte.Enabled:= false;
      qrlZBSoll.Enabled:= false;
      qrlZBIst.Enabled:= false;
      exit;
    end;
    
    if (tzb.SollVon = 0) then strSollVon:= 'Keine Angaben'
      else strSollVon:= FormatDateTime('dd.mm.yyyy  hh:nn', tzb.SollVon);
    if (tzb.SollBis = 0) then strSollBis:= 'Keine Angaben'
      else strSollBis:= FormatDateTime('dd.mm.yyyy  hh:nn', tzb.SollBis);
    if (tzb.IstVon = 0) then strIstVon:= 'Keine Angaben'
      else strIstVon:= FormatDateTime('dd.mm.yyyy  hh:nn', tzb.IstVon);
    if (tzb.IstBis = 0) then strIstBis:= 'Keine Angaben'
      else strIstBis:= FormatDateTime('dd.mm.yyyy  hh:nn', tzb.IstBis);

    qrlZBSoll.Caption:= strSollVon + ' - ' + strSollBis;
    qrlZBIst.Caption:= strIstVon + ' - ' + strIstBis;
  end
  else begin
    qrlZBMesswerte.Enabled:= false;
    qrlZBSoll.Enabled:= false;
    qrlZBIst.Enabled:= false;
  end;
end;

{ Drucken der ZeitSync-Daten in Labels }
{---------------------------------}
procedure TFormPrintJournal.qrlZeitSyncPrint;
{---------------------------------}
var
  Sync        : TWJZSyncDB;
  tPc, tMrg   : TDateTime;
  b           : boolean;
  i           : integer;
  diff        : integer;
begin
  qrlZsPcZeit.Left:= qrlZeitSync.Left;
  qrlZsMrgZeit.Left:= qrlZeitSync.Left;
  qrlZsZeitDiff.Left:= qrlZeitSync.Left;
  i:= QuickReport.DataSet.FieldByName(C_WJournal_JournalId).asInteger;
  Sync:= TWJZSyncDB.Create(PathServer.Pathname[WStammDir]);
  b:= Sync.ReadGeraet_PCZeit(i, tMrg, tPc);
  Sync.Free;
  { es sollte nicht synchronisiert werden }
  if not b then begin
  { Ausgabe entsprechend anpassen }
    qrlZsPcZeit.Enabled:= False;
    qrlZsMrgZeit.Enabled:= True;
    qrlZsZeitDiff.Enabled:= False;
    qrlZsMrgZeit.Caption:= 'nicht eingestellt';
  end
  { es sollte synchroniert werden }
  else begin
    qrlZsPcZeit.Enabled:= True;
    qrlZsMrgZeit.Enabled:= True;
    qrlZsZeitDiff.Enabled:= True;
    qrlZsPcZeit.Caption:= FormatDateTime('dd.mm.yyyy  hh:nn:ss"  (PC)"', tPc);
    qrlZsMrgZeit.Caption:= FormatDateTime('dd.mm.yyyy  hh:nn:ss"  (MRG)"', tMrg);
    diff:=GetTimeDiffInSec(tPc, tMrg);
    if tPc < tMrg then
      diff:=diff * (-1);
    qrlZsZeitDiff.Caption:= 'Differenz: ' + IntToStr(diff) + ' s';
  end;
end;

{ Drucken der Warnungen und Fehler in Labels }
{---------------------------------}
procedure TFormPrintJournal.qrlFuWPrint;
{---------------------------------}
var
  Fehler       : TJFehlerDB;
  i, n1, n2, z : integer;
  q            : TQuery;
  ss, se       : shortstring;
begin
  { Alle Detail-Labels erst einmal disablen }
  for i:= 1 to 8 do
    if qrbvalues.FindChildControl('qrlFuW' + IntToStr(i)) <> nil
      then qrbvalues.FindChildControl('qrlFuW' + IntToStr(i)).Enabled:= false;
  { Fehler-Objekt instantiieren }
  Fehler:= TJFehlerDB.Create(PathServer.Pathname[WStammDir]);
  { Warnungen und Fehler in Query holen }
  i:= QuickReport.DataSet.FieldByName(C_WJournal_JournalId).asInteger;
  z:= qrdbtGeraeteArt.Top;
  q:= TQuery.Create(nil);
  if Fehler.IdReadFehlerUndWarnungen(q, i) then begin
    i:= 1;
    if q.RecordCount > 0 then begin
      q.First;
      while not q.Eof do begin
        n1:= q.FieldByName(C_WJFehler_Gruppe).asInteger;
        n2:= q.FieldByName(C_WJFehler_Fehler).asInteger;
        if qrbvalues.FindChildControl('qrlFuW' + IntToStr(i)) <> nil then begin
          with (qrbvalues.FindChildControl('qrlFuW' + IntToStr(i)) as TQRLabel) do begin
            Enabled:= True;
            Top:= z;
            z:= z + Height;
            Left:= qrlWarnungenFehler.Left;
            ss:= GetStatusText(n1);
            Caption:= ss + ':';
            inc(i);
          end;
          with (qrbvalues.FindChildControl('qrlFuW' + IntToStr(i)) as TQRLabel) do begin
            Enabled:= True;
            Top:= z;
            z:= z + Height;
            Left:= qrlWarnungenFehler.Left;
            se:= GetErrorText(n1, n2);
            Caption:= '  -> ' + se;
            inc(i);
          end;
        end;
        q.Next;
      end;
    end;
  end;
  q.free;
  { Fehler-Objekt freigeben }
  Fehler.free;
  if z > DetailHeight then DetailHeight:= z;
end;

{ Einstellen der Anzeige-Kriterien }
{---------------------------------}
procedure TFormPrintJournal.SetJournalDruckEinstellungen(Quitt, DatenZB, ZeitSync,
                                                         GerArt, AbrArt, Status: byte;
                                                         ZeitVon, ZeitBis: TDateTime;
                                                         GesRecCount: integer;
                                                         Station: string);
{---------------------------------}
var
  AktLabelY: integer;
begin
  { 'Überschrift'-Label beschriften }
  if Station = 'Alle' then qrlUeberschrift.Caption:= 'Journal für alle Stationen'
    else qrlUeberschrift.caption:= 'Journal für Station: ' + Station;
  { 'Quittiert'-Label beschriften }
  if Quitt = 1 then qrlNeuAlt.caption:= 'Nur nicht quittierte Journaleinträge'
  else if Quitt = 2 then qrlNeuAlt.caption:= 'Nur quittierte Journaleinträge'
  else qrlNeuAlt.caption:= 'Quittierte und  nicht quittierte Journaleinträge';
  { 'Filter'-Label beschriften }
  if (DatenZB > 0) or (ZeitSync > 0) or (GerArt > 0) or (AbrArt > 0) or
     (Status > 0) or (trunc(ZeitVon) > 0) or (trunc(ZeitBis) > 0)
   then begin
     qrlIsFilter.Caption:= 'Weitere Filterkriterien in der Zusammenfassung';
     qrlWeitereKriterien.Caption:= 'Weitere Filterkriterien:';
     AktLabelY:= qrlWeitereKriterien.Top;
     case Status of
       1: qrlFStatus.Caption:= 'Nur fehlerfreie Verbindungen';
       2: qrlFStatus.Caption:= 'Nur Verbindungen mit Warnungen oder Fehlern';
       3: qrlFStatus.Caption:= 'Nur Verbindungen mit Warnungen';
       4: qrlFStatus.Caption:= 'Nur Verbindungen mit Fehlern';
       else qrlFStatus.Caption:= '';
     end;
     if Status > 0 then begin
       qrlFStatus.Top:= AktLabelY;
       AktLabelY:= AktLabelY + qrlFStatus.Height;
     end;
     case DatenZB of
       1: qrlFZeitBereich.Caption:= 'Zeitbereiche vollständig';
       2: qrlFZeitBereich.Caption:= 'Zeitbereiche unvollständig';
       else qrlFZeitBereich.Caption:= '';
     end;
     if DatenZB > 0 then begin
       qrlFZeitBereich.Top:= AktLabelY;
       AktLabelY:= AktLabelY + qrlFZeitBereich.Height;
     end;
     case ZeitSync of
       1: qrlFZeitSync.Caption:= 'Geräte ohne Zeitsynchronisation';
       2: qrlFZeitSync.Caption:= 'Geräte mit Zeitsynchronisation';
       else qrlFZeitSync.Caption:= '';
     end;
     if ZeitSync > 0 then begin
       qrlFZeitSync.Top:= AktLabelY;
       AktLabelY:= AktLabelY + qrlFZeitSync.Height;
     end;
     case GerArt of
       1: qrlFGerArt.Caption:= 'Nur MRG';
       2: qrlFGerArt.Caption:= 'Nur LAKS';
       3: qrlFGerArt.Caption:= 'Nur DSfG';
       else qrlFGerArt.Caption:= '';
     end;
     if GerArt > 0 then begin
       qrlFGerArt.Top:= AktLabelY;
       AktLabelY:= AktLabelY + qrlFGerArt.Height;
     end;
     case AbrArt of
       1: qrlFAbrArt.Caption:= 'Nur Automatikabrufe';
       2: qrlFAbrArt.Caption:= 'Nur manuelle Abrufe';
       3: qrlFAbrArt.Caption:= 'Nur eingehende Rufe';
       4: qrlFAbrArt.Caption:= 'Nur Momentanwertabrufe';
       5: qrlFAbrArt.Caption:= 'Nur Rufreaktivierungen';
       6: qrlFAbrArt.Caption:= 'Nur Rückrufauslösungen';
       7: qrlFAbrArt.Caption:= 'Nur Konfigurationsabrufe';
       8: qrlFAbrArt.Caption:= 'Nur DFÜ-Parametrierabrufe';
       else qrlFAbrArt.Caption:= '';
     end;
     if AbrArt > 0 then begin
       qrlFAbrArt.Top:= AktLabelY;
       AktLabelY:= AktLabelY + qrlFAbrArt.Height;
     end;
     if trunc(ZeitVon) > 0
       then qrlFZeitVon.Caption:=
         FormatDateTime('"Nur Journaleinträge ab dem "dd.mm.yyyy', ZeitVon)
       else qrlFZeitVon.Caption:= '';
     if trunc(ZeitVon) > 0 then begin
       qrlFZeitVon.Top:= AktLabelY;
       AktLabelY:= AktLabelY + qrlFZeitVon.Height;
     end;
     if trunc(ZeitBis) > 0
       then qrlFZeitBis.Caption:=
         FormatDateTime('"Nur Journaleinträge bis zum "dd.mm.yyyy', ZeitBis)
       else qrlFZeitBis.Caption:= '';
     if trunc(ZeitBis) > 0 then begin
       qrlFZeitBis.Top:= AktLabelY;
//       AktLabelY:= AktLabelY + qrlFZeitBis.Height;
     end;
   end
   else begin
     qrlIsFilter.Caption:= 'Keine weiteren Filterkriterien';
     qrlWeitereKriterien.Caption:= 'Weitere Filterkriterien: Keine';
     qrlFStatus.Caption:= '';
     qrlFZeitBereich.Caption:= '';
     qrlFZeitSync.Caption:= '';
     qrlFGerArt.Caption:= '';
     qrlFAbrArt.Caption:= '';
     qrlFZeitVon.Caption:= '';
     qrlFZeitBis.Caption:= '';
   end;
  { Weitere Kriterien für die Summaray zuweisen }
  txtGesRecCount:= ' von ' + IntToStr(GesRecCount) + ' Einträgen';
end;

{---------------------------------}
procedure TFormPrintJournal.PrintJournal(aQuery: TQuery;
                                         isDatenTypen, isZeitBereiche,
                                         isZeitSync, isFuW: boolean);
{---------------------------------}
var
  AktX, dX    : integer;
  ColCount    : integer;
begin
  { Dataset und Felder zuweisen }
  QuickReport.DataSet:= aQuery;
  qrdbtStatus.DataSet:= aQuery;
  qrdbtDatum.DataSet:= aQuery;
  qrdbtZeit.DataSet:= aQuery;
  qrdbtGeraeteArt.DataSet:= aQuery;
  qrdbtStationsName.DataSet:= aQuery;
  qrdbtKennung.DataSet:= aQuery;
  qrdbtAbrufArt.DataSet:= aQuery;

  qrdbtStatus.DataField:= 'Status';
  qrdbtDatum.DataField:= 'DZVerbAufbau';
  qrdbtZeit.DataField:= 'DZVerbAufbau';
  qrdbtGeraeteArt.DataField:= 'GeraeteArt';
  qrdbtStationsName.DataField:= 'StationsName';
  qrdbtKennung.DataField:= 'Kennung';
  qrdbtAbrufArt.DataField:= 'AbrufArt';

  qrdbtDatum.Mask:= 'dd.mm.yyyy';
  qrdbtZeit.Mask:= 'hh:nn:ss';

  { Ausgabe ist normalerweise 'Portrait', bei zusätzlichen Angaben 'Landscape' }
  if (isDatenTypen) or (isZeitBereiche) or (isZeitSync) or (isFuW)
    then QuickReport.Page.Orientation:= poLandscape
    else QuickReport.Page.Orientation:= poPortrait;

  ColCount:= 4;  { 4 Spalten werden immer gedruckt }

  if isDatenTypen then begin
    inc(ColCount);
    qrlDatenTypen.Enabled:= true;
  end
  else begin
    qrlDatenTypen.Enabled:= false;
  end;

  if isZeitBereiche then begin
    inc(ColCount);
//    qrlZeitBereiche1.Left:= AktLabelX;
    qrlZeitBereiche1.Enabled:= true;
    qrlZeitBereiche2.Enabled:= true;

    qrlZBMesswerte.Top:= qrdbtGeraeteArt.Top;
    qrlZBSoll.Top:= qrdbtStationsName.Top;
    qrlZBIst.Top:= qrdbtKennung.Top;
  end
  else begin
    qrlZeitBereiche1.Enabled:= false;
    qrlZeitBereiche2.Enabled:= false;
    qrlZBMesswerte.Enabled:= false;
    qrlZBSoll.Enabled:= false;
    qrlZBIst.Enabled:= false;
  end;

  if isZeitSync then begin
    inc(ColCount);
    qrlZeitSync.Enabled:= true;
    qrlZsPcZeit.Enabled:= true;
    qrlZsMrgZeit.Enabled:= true;
    qrlZsZeitDiff.Enabled:= true;

    qrlZsPcZeit.Top:= qrdbtGeraeteArt.Top;
    qrlZsMrgZeit.Top:= qrdbtStationsName.Top;
    qrlZsZeitDiff.Top:= qrdbtKennung.Top;

  end
  else begin
    qrlZeitSync.Enabled:= false;
    qrlZsPcZeit.Enabled:= false;
    qrlZsMrgZeit.Enabled:= false;
    qrlZsZeitDiff.Enabled:= false;
  end;

  if isFuW then begin
    inc(ColCount);
//    qrlWarnungenFehler.Left:= AktLabelX;
    qrlWarnungenFehler.Enabled:= true;
//    AktLabelX:= AktLabelX + qrlAbrufArt.Left - qrlGeraeteArt.Left;
  end
  else begin
    qrlWarnungenFehler.Enabled:= false;
  end;

  { Labels anordnen }
  AktX:= 0;
  dX:= qrbValues.Width div ColCount;
  qrlStatus.Left:= AktX;         { 1. Spalte: Status }
  qrdbtStatus.Left:= AktX;
  AktX:= AktX + dX - 70;
  qrlDatum.Left:= AktX;          { 2. Spalte: Datum, Zeit }
  qrdbtDatum.Left:= AktX;
  qrlZeit.Left:= AktX;
  qrdbtZeit.Left:= AktX;
  AktX:= AktX + dX - 60;
  qrlGeraeteArt.Left:= AktX;     { 3. Spalte: Geräteart, Stationsname, Kennnug }
  qrdbtGeraeteArt.Left:= AktX;
  qrlStationsName.Left:= AktX;
  qrdbtStationsName.Left:= AktX;
  qrlKennung.Left:= AktX;
  qrdbtKennung.Left:= AktX;
  AktX:= AktX + dX;
  qrlAbrufArt.Left:= AktX;         { 4. Spalte: Abrufart }
  qrdbtAbrufArt.Left:= AktX;
  AktX:= AktX + dX - 60;
  if isDatenTypen then begin
    qrlDatenTypen.Left:= AktX;
    AktX:= AktX + dX - 40;
  end;
  if isZeitBereiche then begin
    qrlZeitbereiche1.Left:= AktX;
    qrlZeitBereiche2.Left:= qrlZeitBereiche1.Left;
    AktX:= AktX + dX + 80;
  end;
  if isZeitSync then begin
    qrlZeitSync.Left:= AktX;
    AktX:= AktX + dX + 40;
  end;
  if isFuW then begin
    qrlWarnungenFehler.Left:= AktX;
  end;


  { Zuweisung der vordefinierten Texte an QRLabels }
  qrlGesRecordCount.Caption:=
    IntToStr(QuickReport.DataSet.RecordCount) + txtGesRecCount;

  { es soll auf dem Drucker gedruckt werden, der im Programm eingestellt ist
    (z.B. über TPrinterSetupDialog): }
  QuickReport.PrinterSettings.PrinterIndex:=Printer.PrinterIndex;  // 15.03.2002 WW

//  QuickReport.Preview;
  QuickReport.Print;
end;

end.

