{------------------------------------------------------------------------------}
{ 22.01.1999 GD; MDI-Child-Formular-Unit für die Journal-Tabelle               }
{                Compilerschalter ALS32 für Statusleiste                       }
{                                                                              }
{  -> wird zur Laufzeit erstellt - nicht in automatische Erstellung aufnehmen  }
{  -> Compilerschalter ALS32 für Aktualisierung der Statusleiste               }
{                                                                              }
{   - Tabellen: WJournal.db                                                    }
{                                                                              }
{ 19.04.1999  GD  kleine Änderungen                                            }
{ 07.07.1999  GD  Anpassung an Laks                                            }
{ 19.10.1999  WW  Compilerschalter RE32 für (De-)Aktivierung des Rückrufbuttons}
{ 14.02.2001  GD  qryJournal auf TQueryExt umgestellt                          }
{ 29.04.2002  GD  Um Textsuche erweitert                                       }
{ 16.04.2003  WW  Um Löschen aus DSfG-Telegramm-Journaltabelle erweitert       }
{                                                                              }
{  (C) Karl Wieser GmbH 1999, 2002                                             }
{------------------------------------------------------------------------------}
unit fJournal;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, ExtCtrls, Grids, DBGrids, JournlDb, PathIni, StdCtrls,
  ComCtrls, WSysCon, my_utils, fDetJour, Buttons, Menus, fJourRep, fJourDlg,
  DB_Attn, ErrConst, WStrUtils, Wtables, ZSyncDb, ZBDatDb, RFilesDb, TelegrDb;

type
  TStationRecord = class(TObject)
    StationsName : string[40];
    GeraeteArt   : string[10];
    GeraeteId    : integer;
    constructor Create(sn, ga: string; gi: integer);
  end;

  TFormJournal = class(TForm)
    pnRight: TPanel;
    dbgrdJournal: TDBGrid;
    dsJournal: TDataSource;
    pnEinstellungen: TPanel;
    bvlEinstellungen: TBevel;
    pmJournal: TPopupMenu;
    pmQuittSome: TMenuItem;
    pmQuittAll: TMenuItem;
    pmDeleteSome: TMenuItem;
    pmDeleteAll: TMenuItem;
    gbFilter: TGroupBox;
    lDatenZeitbereiche: TLabel;
    lZeitsynchronisation: TLabel;
    chbWeitereFilter: TCheckBox;
    cbDatenZeitbereiche: TComboBox;
    cbZeitsynchronisation: TComboBox;
    pnZeitBereich: TPanel;
    gbZeitraum: TGroupBox;
    lVon: TLabel;
    lBis: TLabel;
    chbZeitraum: TCheckBox;
    dtpickVon: TDateTimePicker;
    dtpickBis: TDateTimePicker;
    pnButtons: TPanel;
    sbtnAktualisieren: TSpeedButton;
    sbtnQuittRecords: TSpeedButton;
    sbtnDeleteRecords: TSpeedButton;
    pnMiddle: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbGeraeteArt: TComboBox;
    cbAbrufArt: TComboBox;
    cbStatus: TComboBox;
    cbStation: TComboBox;
    cbQuittiert: TComboBox;
    sbtnPrint: TSpeedButton;
    pmDetails: TMenuItem;
    N1: TMenuItem;
    pmPrint: TMenuItem;
    cbComPort: TComboBox;
    Label6: TLabel;
    N2: TMenuItem;
    AutoAktuTimer: TTimer;
    Label7: TLabel;
    eErrorText: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure chbZeitraumClick(Sender: TObject);
    procedure dbgrdJournalDrawDataCell(Sender: TObject; const Rect: TRect;
      Field: TField; State: TGridDrawState);
    procedure dbgrdJournalDblClick(Sender: TObject);
    procedure qryJournalCalcFields(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbtnAktualisierenClick(Sender: TObject);
    procedure sbtnDeleteRecordsClick(Sender: TObject);
    procedure sbtnQuittRecordsClick(Sender: TObject);
    procedure pmJournalPopup(Sender: TObject);
    procedure pmQuittSomeClick(Sender: TObject);
    procedure pmQuittAllClick(Sender: TObject);
    procedure pmDeleteSomeClick(Sender: TObject);
    procedure pmDeleteAllClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure chbWeitereFilterClick(Sender: TObject);
    procedure sbtnPrintClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure dtpickVonChange(Sender: TObject);
    procedure dtpickBisChange(Sender: TObject);
    procedure AutoAktuTimerTimer(Sender: TObject);
    procedure cbChange(Sender: TObject);
    procedure cbGeraeteArtChange(Sender: TObject);
  private
    { Private-Deklarationen }
    Fehler           : TJFehlerDB;
    Journal          : TJournalDB;
    isActivated      : boolean;
    isStoped         : boolean;
    StatusText       : string;
    Stationsliste    : TStringList;
    CloseWindowState : TWindowState;
    JournalTriggerTime: integer;
    qryJournalJournalId: TIntegerField;
    qryJournalGeraeteArt: TStringField;
    qryJournalGeraeteId: TIntegerField;
    qryJournalKennung: TStringField;
    qryJournalAbrufart: TStringField;
    qryJournalDatentypen: TIntegerField;
    qryJournalComNr: TSmallintField;
    qryJournalQuittiert: TBooleanField;
    qryJournalDZVerbAufbau: TDateTimeField;
    qryJournalDZVerbSteht: TDateTimeField;
    qryJournalDZLoggedIn: TDateTimeField;
    qryJournalDZVerbEnde: TDateTimeField;
    qryJournalStatus: TIntegerField;
    qryJournalStationsname: TStringField;
    qryJournalWarnung: TSmallintField;
    qryJournalFehler: TSmallintField;
    procedure InitJournalQuery(aState: boolean);  // 14.02.2001
    function GetQuery: boolean;
    procedure DrawString(Rect: TRect; s: string; aXPos, aYPos: byte);
    procedure FillCbStation;
    procedure AddJournalStationen;
    procedure DeleteJournal(AllOrSome: byte);
    procedure QuittJournal(AllOrSome: byte);
    procedure StartAutoAktuTimer;
    procedure StopAutoAktuTimer;
    procedure SetSpaltenbreiten;
  public
    { Public-Deklarationen }
    qryJournal: TQueryExt;  // Für WW's FRuechR public !
    constructor Create(AOwner: TComponent; ACloseWindowState: TWindowState); reintroduce;
  end;
var
  FormJournal: TFormJournal;

implementation

{$IFDEF ALS32}
uses
  FMain; { Für Aktualisierung der Statusleiste }
{$ENDIF}

{$IFDEF RE32}
uses
  FMain; { Für (De-)Aktivierung des Rückruf-Schalters }
{$ENDIF}

{$R *.DFM}


{---------------------------- TStationRecord ---------------------------------}

{ Record-Objekt für Stationsliste                    }
{----------------------------------------------------}
constructor TStationRecord.Create(sn, ga: string; gi: integer);
{----------------------------------------------------}
begin
  StationsName := sn;
  GeraeteArt   := ga;
  GeraeteId    := gi;
end;

{------------------------------ TFormJournal ---------------------------------}

{-----------------------------------------------------------------------------------}
constructor TFormJournal.Create(AOwner: TComponent; ACloseWindowState: TWindowState);
{-----------------------------------------------------------------------------------}
begin
  inherited Create (AOwner);
  CloseWindowState:=ACloseWindowState;
end;

{----------------------------------------------------}
procedure TFormJournal.FormCreate(Sender: TObject);
{----------------------------------------------------}
var
  y, m, t: word;
begin
  { Flag: Statusbar des Hauptfensters wird verwendet }
  isActivated:= true;
  { Flag: kann aktualisiert werden ? }
  isStoped:= False;
  { Journal und Fehler-Objekt initialisieren }
  Fehler:= TJFehlerDB.Create(Pathserver.Pathname[WStammDir]);
  Journal:= TJournalDB.Create(Pathserver.Pathname[WStammDir]);
  { DOS-Zeit des Aktualisierungsfiles für WJournal.Db holen }
  JournalTriggerTime:= GetTriggerTime(pathServer.Pathname[WStammDir] + CDBWJournal);
  { Stationsliste für Namen füllen }
  Stationsliste:= TStringList.Create;

  // qryJounal initialisieren
  InitJournalQuery(True);  // 14.02.2001

  { Voreinstellungen zur Zeitraum-Auswahl }
  chbZeitraumClick(self);
  DecodeDate(now, y, m, t);
  dtpickVon.Date:= EncodeDate(y, m, 1);
  dtpickBis.Date:= now;
  { Voreinstellungen für zusätzliche Filterkriterien }
  cbDatenZeitbereiche.ItemIndex:= 0;   { Alle }
  cbZeitSynchronisation.ItemIndex:= 0; { Alle }
  chbWeitereFilterClick(self);
  { Voreinstellung der Geräteart }
  cbGeraeteArt.ItemIndex:= 0;          { Alle }
  { Voreinstellung der Abrufart }
  cbAbrufArt.ItemIndex:= 0;            { Alle }
  { Voreinstellung für den Status }
  cbStatus.ItemIndex:= 0;              { Alle }
  { Voreinstellung für Alte/Neue }
  cbQuittiert.ItemIndex:= 1;           { Neue }
  { Voreinstellung für ComPort }
  cbComPort.ItemIndex:= 0;           { Alle }
  { Füllt die Stationsnamensliste }
  GetStationsnameListe(Stationsliste, C_AuswahlAlle);
  { Voreinstellung für die Station }
  FillCbStation;
  { Grid füllen }
  GetQuery;
end;

{----------------------------------------------------}
procedure TFormJournal.FormClose(Sender: TObject;
  var Action: TCloseAction);
{----------------------------------------------------}
begin
{$IFDEF ALS32}
  FormMain.StatusBar.Panels[3].Text:= '';
{$ENDIF}
{$IFDEF RE32}
  FormRufMain.bbtnRueckRuf.Enabled:=false;
{$ENDIF}
  WindowState:=CloseWindowState;
  Action:= caFree; { MDI-Fenster wird geschlossen }
end;

{----------------------------------------------------}
procedure TFormJournal.FormDestroy(Sender: TObject);
{----------------------------------------------------}
var
  i: integer;
begin
  { Journal und Fehler-Objekt freigeben }
  Journal.free;
  Fehler.free;
  { Stationsliste freigeben }
  if Stationsliste.Count > 0 then begin
    try
      for i:= 0 to Stationsliste.Count-1 do
        TIdRec(Stationsliste.Objects[i]).free;
    except
      // tue nix
    end;
    Stationsliste.Clear;
  end;
  Stationsliste.Free;

  // qryJounal freigeben
  InitJournalQuery(False);  // 14.02.2001

end;

{----------------------------------------------------}
procedure TFormJournal.FormActivate(Sender: TObject);
{----------------------------------------------------}
begin
  isActivated:= true;   { Statusbar des Hauptfensters wird verwendet }
{$IFDEF ALS32}
  FormMain.StatusBar.Panels[3].Text:= StatusText;
{$ENDIF}
{$IFDEF RE32}
  if qryJournal.Active then
    FormRufMain.bbtnRueckRuf.Enabled:=qryJournal.Recordcount > 0;
{$ENDIF}
  WindowState:= wsMaximized;

  SetSpaltenBreiten;
end;

{----------------------------------------------------}
procedure TFormJournal.FormDeactivate(Sender: TObject);
{----------------------------------------------------}
begin
{$IFDEF ALS32}
  StatusText:= FormMain.StatusBar.Panels[3].Text;
  FormMain.StatusBar.Panels[3].Text:= '';
{$ENDIF}
  isActivated:= false;  { Statusbar des Hauptfensters wird nicht verwendet }
end;

{----------------------------------------------------}
procedure TFormJournal.TimerTimer(Sender: TObject);
{----------------------------------------------------}
var
  i      : integer;
begin
  if not isStoped then begin
    i:= GetTriggerTime(Pathserver.Pathname[WStammDir] + CDBWJournal);
    if i <> JournalTriggerTime then begin
      if GetQuery then JournalTriggerTime:= i;


{$IFDEF ALS32}
  { Ausgabe im Statusbar der Main-Unit }
      FormMain.StatusBar.Panels[3].Text:='Letzte Aktualisierung des Journals: ' +
                                         FormatDateTime('hh:mm:ss',(now));
{$ENDIF}
    end;
  end;
end;

{ Initialisiert/Gibt frei Journalquery               }
{ Parameter: T-Initialisieren; F-Freigeben           }
{----------------------------------------------------}
procedure TFormJournal.InitJournalQuery(aState: boolean);
{----------------------------------------------------}
begin
  if (aState) and (not Assigned(qryJournal)) then begin
    qryJournal := TQueryExt.Create(Self);
    qryJournal.OnCalcFields := qryJournalCalcFields;
    dsJournal.DataSet := qryJournal;

    qryJournalStatus := TIntegerField.Create(qryJournal);
    qryJournalStatus.DisplayWidth := 7;
    qryJournalStatus.FieldName := 'Status';
    qryJournalStatus.DataSet := qryJournal;

    qryJournalJournalId := TIntegerField.Create(qryJournal);
    qryJournalJournalId.FieldName := 'JournalId';
    qryJournalJournalId.Visible := False;
    qryJournalJournalId.DataSet := qryJournal;

    qryJournalGeraeteArt := TStringField.Create(qryJournal);
    qryJournalGeraeteArt.DisplayLabel := 'Geräteart';
    qryJournalGeraeteArt.DisplayWidth := 8;
    qryJournalGeraeteArt.FieldName := 'GeraeteArt';
    qryJournalGeraeteArt.Size := 10;
    qryJournalGeraeteArt.DataSet := qryJournal;

    qryJournalStationsname := TStringField.Create(qryJournal);
    qryJournalStationsname.DisplayWidth := 27;
    qryJournalStationsname.FieldKind := fkCalculated;
    qryJournalStationsname.FieldName := 'Stationsname';
    qryJournalStationsname.Size := 40;
    qryJournalStationsname.Calculated := True;
    qryJournalStationsname.DataSet := qryJournal;

    qryJournalGeraeteId := TIntegerField.Create(qryJournal);
    qryJournalGeraeteId.FieldName := 'GeraeteId';
    qryJournalGeraeteId.Visible := False;
    qryJournalGeraeteId.DataSet := qryJournal;

    qryJournalKennung := TStringField.Create(qryJournal);
    qryJournalKennung.DisplayWidth := 17;
    qryJournalKennung.FieldName := 'Kennung';
    qryJournalKennung.Size := 14;
    qryJournalKennung.DataSet := qryJournal;

    qryJournalAbrufart := TStringField.Create(qryJournal);
    qryJournalAbrufart.DisplayWidth := 11;
    qryJournalAbrufart.FieldName := 'Abrufart';
    qryJournalAbrufart.Size := 10;
    qryJournalAbrufart.DataSet := qryJournal;

    qryJournalDatentypen := TIntegerField.Create(qryJournal);
    qryJournalDatentypen.FieldName := 'Datentypen';
    qryJournalDatentypen.Visible := False;
    qryJournalDatentypen.DataSet := qryJournal;

    qryJournalQuittiert := TBooleanField.Create(qryJournal);
    qryJournalQuittiert.FieldName := 'Quittiert';
    qryJournalQuittiert.Visible := False;
    qryJournalQuittiert.DataSet := qryJournal;

    qryJournalDZVerbAufbau := TDateTimeField.Create(qryJournal);
    qryJournalDZVerbAufbau.Alignment := taCenter;
    qryJournalDZVerbAufbau.DisplayLabel := 'Datum/Zeit';
    qryJournalDZVerbAufbau.DisplayWidth := 17;
    qryJournalDZVerbAufbau.FieldName := 'DZVerbAufbau';
    qryJournalDZVerbAufbau.DisplayFormat := 'dd.mm.yyyy hh:nn:ss';
    qryJournalDZVerbAufbau.DataSet := qryJournal;

    qryJournalDZVerbSteht := TDateTimeField.Create(qryJournal);
    qryJournalDZVerbSteht.FieldName := 'DZVerbSteht';
    qryJournalDZVerbSteht.Visible := False;
    qryJournalDZVerbSteht.DataSet := qryJournal;

    qryJournalComNr := TSmallintField.Create(qryJournal);
    qryJournalComNr.Alignment := taCenter;
    qryJournalComNr.DisplayLabel := 'COM';
    qryJournalComNr.DisplayWidth := 4;
    qryJournalComNr.FieldName := 'ComNr';
    qryJournalComNr.DataSet := qryJournal;

    qryJournalDZLoggedIn := TDateTimeField.Create(qryJournal);
    qryJournalDZLoggedIn.FieldName := 'DZLoggedIn';
    qryJournalDZLoggedIn.Visible := False;
    qryJournalDZLoggedIn.DataSet := qryJournal;

    qryJournalDZVerbEnde := TDateTimeField.Create(qryJournal);
    qryJournalDZVerbEnde.FieldName := 'DZVerbEnde';
    qryJournalDZVerbEnde.Visible := False;
    qryJournalDZVerbEnde.DataSet := qryJournal;

    qryJournalWarnung := TSmallintField.Create(qryJournal);
    qryJournalWarnung.FieldName := 'Warnung';
    qryJournalWarnung.Visible := False;
    qryJournalWarnung.DataSet := qryJournal;

    qryJournalFehler := TSmallintField.Create(qryJournal);
    qryJournalFehler.FieldName := 'Fehler';
    qryJournalFehler.Visible := False;
    qryJournalFehler.DataSet := qryJournal;
  end
  else if (not aState) and (Assigned(qryJournal)) then begin
    dsJournal.DataSet := nil;
    if (qryJournal.Active) then qryJournal.Close;
    qryJournal.Free;
    qryJournal := nil;
  end;
end;

{ Schreibt einen String in das Draw-Grid             }
{ Parameter: Rect - Zelle; s - Text; aPos - Anord.   }
{----------------------------------------------------}
procedure TFormJournal.DrawString(Rect: TRect; s: string; aXPos, aYPos: byte);
{----------------------------------------------------}
var
  x, y: integer;
begin
  with dbgrdJournal.Canvas do begin

    case aXPos of
      0: x:= Rect.Left; { links }
      1: x:= Rect.Left + ((Rect.Right-Rect.Left-TextWidth(s)) div 2); { mitte }
      2: x:= Rect.Right - TextWidth(s); { rechts }
      else x:= Rect.Left + ((Rect.Right-Rect.Left-TextWidth(s)) div 2); { mitte }
    end;
    case aYPos of
      0: y:= Rect.Top; { oben }
      1: y:= Rect.Top + ((Rect.Bottom-Rect.Top-TextHeight(s)) div 2); { mitte }
      2: y:= Rect.Bottom - TextHeight(s); { unten }
      else y:= Rect.Top + ((Rect.Bottom-Rect.Top-TextHeight(s)) div 2); { mitte }
    end;
    TextOut(x, y, s);
//    TextRect(Rect, x, y, s);
  end;
end;

{---------------------------------------}
procedure TFormJournal.SetSpaltenbreiten;
{---------------------------------------}
{ Spaltenbreiten des Grid anpassen: }
Const
  { Default-Spaltenbreiten des Grid für Auflösung 800x600 -> Breite der Spalte Stationsname (2) automatisch }
  CColWidth: array [0..6] of integer = (46, 52, 0, 106, 80, 106, 28);

var
  i, ColWidthSum: integer;

begin
  ColWidthSum:=0;
  for i := 0 to dbgrdJournal.Columns.Count - 1 do begin
    dbgrdJournal.Columns[i].Width:=trunc (CColWidth[i] * Screen.Width/800);
    ColWidthSum:=ColWidthSum + dbgrdJournal.Columns[i].Width;
  end;
  { Spalte "Stationsname" bekommt den Rest minus Scrollbar-Breite: }
  if dbgrdJournal.Columns.Count > 2 then
    dbgrdJournal.Columns[2].Width:=dbgrdJournal.ClientWidth - ColWidthSum - 19;
end;


{ cbStation füllen }
{----------------------------------------------------}
procedure TFormJournal.FillCbStation;
{----------------------------------------------------}
var
  rec: TStationRecord;
begin
  if not isStoped then begin
    isStoped:= True;
    cbStation.Items.Clear;
    rec:= TStationRecord.Create('Alle', 'Z', -1);
    cbStation.Items.AddObject(rec.StationsName, rec);
  { Index wird auf 'Alle' gesetzt }
    cbStation.ItemIndex:= cbStation.Items.IndexOf('Alle');

    AddJournalStationen;
    isStoped:= False;
  end;
end;

{ Fügt in cbStation alle in der Journal-Tabelle            }
{ vorhandenen Stationsnamen der ausgewählten Geräteart ein }
{----------------------------------------------------}
procedure TFormJournal.AddJournalStationen;
{----------------------------------------------------}
var
  rec     : TStationRecord;
  q       : TQueryExt;
  i       : integer;
  ga      : string;
begin
  if FileExists(Pathserver.Pathname[WStammDir] + CDBWJournal) then begin
    q:= TQueryExt.Create(nil);
    try
      q.DataBaseName:= Pathserver.Pathname[WStammDir];
      { Geräteart-Kürzel ermitteln: }
      case cbGeraeteArt.ItemIndex of
        0: ga:= '-1';
        1: ga:= C_GerArtMrg;
        2: ga:= C_GerArtLaks;
        3: ga:= C_GerArtDSfG;
        else ga:= '-1';
      end;
      with q.Sql do begin
        Add('SELECT DISTINCT ' + C_WJournal_GeraeteArt + ', ' + C_WJournal_GeraeteId);
        Add('FROM "' + CDBWJournal + '"');
        if ga <> '-1' then begin
          Add('WHERE ' + C_WJournal_GeraeteArt + ' = :GerArt');
          q.ParamByName ('GerArt').AsString:=ga;
        end;
      end;
      if (q.Open) then begin
        q.First;
        while not q.Eof do begin
          i:= Stationsliste.IndexOf(q.FieldByName(C_WJournal_GeraeteArt).asString +
                            IntToStr(q.FieldByName(C_WJournal_GeraeteId).asInteger));
          if i > -1 then begin
            rec:= TStationRecord.Create(TIdRec(StationsListe.Objects[i]).Name,
                                        q.FieldByName(C_WJournal_GeraeteArt).asString,
                                        q.FieldByName(C_WJournal_GeraeteId).asInteger);
            cbStation.Items.AddObject('[' + rec.GeraeteArt + '] - ' + rec.StationsName, rec);
          end;
          q.Next;
        end;
      end;
    finally
      q.Free;
    end;
  end;
end;

{ Holt mit den getroffenen Selektionseinstellungen   }
{ die Journaleinträge aus der Tabelle                }
{----------------------------------------------------}
function TFormJournal.GetQuery: boolean;
{----------------------------------------------------}
var
  dtv, dtb      : TDateTime;
  ga, aa, sText : string;
  GerId         : integer;
  Save_Cursor   : TCursor;
  i, j          : integer;
  ComPort       : integer;
  
begin
  result:= True; { default }
  if not isStoped then begin
    isStoped:= True;
    StopAutoAktuTimer;   { Timer für automatische Aktualisierung deaktivieren }
    Save_Cursor:= Screen.Cursor;
    Screen.Cursor:= crHourglass;
    try
    { Füllt die Stationsnamensliste }
      if GetStationsnameListe(Stationsliste, C_AuswahlAlle) then begin
    { Einstellung der Geräteart }
        case cbGeraeteArt.ItemIndex of
          0: ga:= '-1';
          1: ga:= C_GerArtMrg;
          2: ga:= C_GerArtLaks;
          3: ga:= C_GerArtDSfG;
          else ga:= '-1';
        end;
    { Einstellung der Abrufart }
        case cbAbrufArt.ItemIndex of
          0: aa:= '-1';
          1: aa:= C_AbrArtAuto;
          2: aa:= C_AbrArtManu;
          3: aa:= C_AbrArtRuf;
          4: aa:= C_AbrArtMomStart;
          5: aa:= C_AbrArtRufReakt;
          6: aa:= C_AbrArtRueckRuf;
          7: aa:= C_AbrArtKonfLesen;
          8: aa:= C_AbrArtMomDfueStart;
          else aa:= '-1';
        end;
    { Einstellungen für den Zeitraum }
        if chbZeitraum.Checked then begin
          dtv:= trunc(dtpickVon.DateTime);
          dtb:= trunc(dtpickBis.DateTime) + 1;
        end
        else begin
          dtv:= 0;
          dtb:= 0;
        end;
    { Einstellungen für sonstige Filter }
        if not chbWeitereFilter.Checked then begin
          cbDatenZeitBereiche.ItemIndex:= 0;
          cbZeitSynchronisation.ItemIndex:= 0;
          sText := '';                       // 29.04.2002
        end
        else sText := Trim(eErrorText.Text); // 29.04.2002

        GerId:=TStationRecord(cbStation.Items.Objects[cbStation.ItemIndex]).GeraeteId;
        if (ga = '-1') AND (GerId > 0) then                          { Alle Gerätearten, bestimmte Station ausgewählt }
          ga:=TStationRecord(cbStation.Items.Objects[cbStation.ItemIndex]).GeraeteArt;

        if cbComPort.ItemIndex >= 0 then begin
          if cbComPort.Items [cbComPort.ItemIndex] = C_AnzeigeTCP_IP then
            ComPort:=CComTCP_IP           { Pseudo-ComPort für TCP/IP-Kommunikation }
          else
            ComPort:=cbComPort.ItemIndex; { serielle Schnittstelle }
        end else
          ComPort:=0;

        qryJournal.DisableControls;
        if Journal.GetJournal(qryJournal,
                              GerId,
                              ga, aa, cbStatus.ItemIndex, cbQuittiert.ItemIndex,
                              cbDatenZeitBereiche.ItemIndex, cbZeitSynchronisation.ItemIndex,
                              ComPort, dtv, dtb, sText) then begin
          dbgrdJournal.SelectedRows.Clear;                                                       { Markierungen löschen }
          qryJournal.Last;

   { Falls keine Einträge da sind, gibt es trotzdem ein Ergebnis durch Summenbildung }
          if (qryJournal.RecordCount = 1) and (qryJournalJournalId.isNull) then
            i:= 0
          else
            i:= qryJournal.RecordCount;
          j:= Journal.GetRecordCount;
          if j = -1 then
            result:= False
          else
            StatusText:= 'Verbindungs-Journal: ' + IntToStr(i) + ' von ' + IntToStr(j) + ' Einträgen';

          { Schalter und Menüs (de-)aktivieren: }
          if i = 0 then begin
            sbtnQuittRecords.Enabled:=false;
            sbtnDeleteRecords.Enabled:=false;
            pmDetails.Enabled:=false;
            pmQuittSome.Enabled:=false;
            pmQuittAll.Enabled:=false;
            pmDeleteSome.Enabled:=false;
            pmDeleteAll.Enabled:=false;
          end
          else begin
            sbtnQuittRecords.Enabled:=true;
            sbtnDeleteRecords.Enabled:=true;
            pmDetails.Enabled:=true;
            pmQuittSome.Enabled:=true;
            pmQuittAll.Enabled:=true;
            pmDeleteSome.Enabled:=true;
            pmDeleteAll.Enabled:=true;
          end;
{$IFDEF ALS32}
          if isActivated then
            FormMain.StatusBar.Panels[3].Text:= StatusText;
{$ENDIF}
{$IFDEF RE32}
          FormRufMain.bbtnRueckRuf.Enabled:=i > 0;
{$ENDIF}

     { Wenn der Ablauf bis hierher kommt, ist result = true }
        end else
          result:= False;
        qryJournal.EnableControls;
        SetSpaltenbreiten;
      end else
        result:= False;
    finally
      Screen.Cursor:= Save_Cursor;
    end;
    isStoped:= False;
  end;
end;

{----------------------------------------------------}
procedure TFormJournal.chbZeitraumClick(Sender: TObject);
{----------------------------------------------------}
begin
  { Einstellen der Größe der Anzeige }
  if chbZeitraum.checked then begin
    gbZeitraum.Height:= dtpickBis.Top + dtpickBis.Height + 10;
  end
  else begin
    gbZeitraum.Height:= chbZeitraum.Top + chbZeitraum.Height + 10;
  end;
  pnZeitBereich.Height:= gbZeitraum.Top + gbZeitraum.Height + 2;
  pnMiddle.Top:= pnZeitBereich.Top + pnZeitBereich.Height + 2;
  gbFilter.Top:= pnMiddle.Top + pnMiddle.Height + 2;
  bvlEinstellungen.Height:= gbFilter.Top + gbFilter.Height + 4;
  pnButtons.Top:=  bvlEinstellungen.Top  + bvlEinstellungen.Height + 4;
  { Zeitraum-Einstellungen sichtbarmachen oder hiden }
  lVon.visible:= chbZeitraum.checked;
  lBis.visible:= chbZeitraum.checked;
  dtpickVon.visible:= chbZeitraum.checked;
  dtpickBis.visible:= chbZeitraum.checked;
  StartAutoAktuTimer;
end;

{----------------------------------------------------}
procedure TFormJournal.chbWeitereFilterClick(Sender: TObject);
{----------------------------------------------------}
begin
  { Einstellen der Größe der Anzeige }
  if chbWeitereFilter.checked then begin
    gbFilter.Height:= eErrorText.Top + eErrorText.Height + 10; // 29.04.2002
  end
  else begin
    gbFilter.Height:= chbWeitereFilter.Top + chbWeitereFilter.Height + 10;
  end;
  pnZeitBereich.Height:= gbZeitraum.Top + gbZeitraum.Height + 2;
  pnMiddle.Top:= pnZeitBereich.Top + pnZeitBereich.Height + 2;
  gbFilter.Top:= pnMiddle.Top + pnMiddle.Height + 2;
  bvlEinstellungen.Height:= gbFilter.Top + gbFilter.Height + 4;
  pnButtons.Top:= bvlEinstellungen.Top  + bvlEinstellungen.Height + 4;
  { Filter-Einstellungen sichtbarmachen oder hiden }
  lDatenZeitbereiche.visible:= chbWeitereFilter.checked;
  lZeitsynchronisation.visible:= chbWeitereFilter.checked;
  cbDatenZeitbereiche.visible:= chbWeitereFilter.checked;
  cbZeitSynchronisation.visible:= chbWeitereFilter.checked;
  StartAutoAktuTimer;
end;

{----------------------------------------------------}
procedure TFormJournal.sbtnAktualisierenClick(Sender: TObject);
{----------------------------------------------------}
begin
  GetQuery;
end;

{----------------------------------------------------}
procedure TFormJournal.dbgrdJournalDrawDataCell(Sender: TObject;
  const Rect: TRect; Field: TField; State: TGridDrawState);
{----------------------------------------------------}
var
  s: string;
  i: integer;
  r: TRect;
begin
  { Falls keine Einträge da sind, gibt es trotzdem ein Ergebnis durch Summenbildung }
  if (qryJournal.RecordCount > 0) and (not qryJournalJournalId.IsNull)then begin
  { Ausgabe des Rufstatus, Warnungs- und Fehleranzahl }
    if (Field.FieldName = 'Status') then begin
      dbgrdJournal.Canvas.Font.Color:= dbgrdJournal.Font.Color;
      if Field.asInteger = 0 then begin
        dbgrdJournal.Canvas.Brush.Color:= clGreen;
        dbgrdJournal.Canvas.FillRect(Rect);
        DrawString(Rect, 'OK', 1, 1);
      end
      else begin
        if (trunc(qryJournalWarnung.asFloat) = FK_WARNUNG) and
           (trunc(qryJournalFehler.asFloat) = FK_FEHLER)
        then begin
          r:= Rect;
          r.Right:= Rect.Left + (Rect.Right - Rect.Left) div 2;
          dbgrdJournal.Canvas.Brush.Color:= clRed;
          dbgrdJournal.Canvas.FillRect(r);
          DrawString(r, 'F', 1, 1);
          r.Left:= r.Right;
          r.Right:= Rect.Right;
          dbgrdJournal.Canvas.Brush.Color:= clYellow;
          dbgrdJournal.Canvas.FillRect(r);
          DrawString(r, 'W', 1, 1);
        end
        else if (trunc(qryJournalFehler.asFloat) = FK_FEHLER) then begin
          dbgrdJournal.Canvas.Brush.Color:= clRed;
          dbgrdJournal.Canvas.FillRect(Rect);
          DrawString(Rect, 'F', 1, 1);
        end
        else if (trunc(qryJournalWarnung.asFloat) = FK_WARNUNG) then begin
          dbgrdJournal.Canvas.Brush.Color:= clYellow;
          dbgrdJournal.Canvas.FillRect(Rect);
          DrawString(Rect, 'W', 1, 1);
        end
        else begin  // Eintrag <> Fehler/Warnung
          r:= Rect;
          r.Bottom:= Rect.Top + (Rect.Bottom - Rect.Top) div 2;
          dbgrdJournal.Canvas.Brush.Color:= clRed;
          dbgrdJournal.Canvas.FillRect(r);
          r.Top:= r.Bottom;
          r.Bottom:= Rect.Bottom;
          dbgrdJournal.Canvas.Brush.Color:= clYellow;
          dbgrdJournal.Canvas.FillRect(r);

          DrawString(Rect, 'undefiniert', 1, 1);
        end;
      end;
    end
  { Umwandeln des Kennbuchstabens für den Gerätetyp in lesbare Ausgabe }
    else if Field.FieldName = C_WJournal_GeraeteArt then begin
      dbgrdJournal.Canvas.Font.Color:= dbgrdJournal.Font.Color;
      if Field.asString = C_GerArtMrg then begin
        s:= C_AnzeigeMrg;
        dbgrdJournal.canvas.brush.color:= C_ColorMrg;
      end
      else if Field.asString = C_GerArtLaks then begin
        s:= C_AnzeigeLaks;
        dbgrdJournal.canvas.brush.color:= C_ColorLaks;
      end
      else if Field.asString = C_GerArtGruppe then begin
        s:= C_AnzeigeGruppe;
        dbgrdJournal.canvas.brush.color:= C_ColorGruppe;
      end
      else if Field.asString = C_GerArtDSfG then begin
        s:= C_AnzeigeDSfG;
        dbgrdJournal.canvas.brush.color:= C_ColorDSfG;
      end;
      dbgrdJournal.canvas.FillRect(Rect);
      DrawString(Rect, s, 1, 1);
    end
  { Darstellung der Abrufart }
    else if Field.FieldName = C_WJournal_Abrufart then begin
      dbgrdJournal.Canvas.Font.Color:= dbgrdJournal.Font.Color;
      if Field.asString = C_AbrArtAuto then begin
        s:= C_AnzeigeAuto;
        dbgrdJournal.canvas.brush.color:= C_ColorAuto;
      end
      else if Field.asString = C_AbrArtManu then begin
        s:= C_AnzeigeManu;
        dbgrdJournal.canvas.brush.color:= C_ColorManu;
      end
      else if Field.asString = C_AbrArtRuf then begin
        s:= C_AnzeigeRuf;
        dbgrdJournal.canvas.brush.color:= C_ColorRuf;
      end
      else if (Field.asString = C_AbrArtMomStart) or
              (Field.asString = C_AbrArtMomStop)
      then begin
        s:= C_AnzeigeMom;
        dbgrdJournal.canvas.brush.color:= C_ColorMom;
      end
      else if Field.asString = C_AbrArtRufReakt then begin
        s:= Copy (C_AnzeigeRufReakt, 1, 8) + '.';
        dbgrdJournal.canvas.brush.color:= C_ColorRufReakt;
      end
      else if Field.asString = C_AbrArtRueckRuf then begin
        s:= C_AnzeigeRueckRuf;
        dbgrdJournal.canvas.brush.color:= C_ColorRueckRuf;
      end
      else if Field.asString = C_AbrArtKonfLesen then begin
        s:= C_AnzeigeKonfLesen;
        dbgrdJournal.canvas.brush.color:= C_ColorKonfLesen;
      end
      else if (Field.asString = C_AbrArtMomDfueStart) then begin
        s:= C_AnzeigeMomDfue;
        dbgrdJournal.canvas.brush.color:= C_ColorMomDfue;
      end
      else begin
        s:= 'Unbekannt';
        dbgrdJournal.canvas.brush.color:= clRed;
      end;
      dbgrdJournal.canvas.FillRect(Rect);
      DrawString(Rect, s, 1, 1);
    end
  { Kennung aus Stammdaten, wenn 'IsNull' }
    else if Field.FieldName = C_WJournal_Kennung then begin
      if Field.isNull then begin
        i:= StationsListe.IndexOf(qryJournal.FieldByName(C_WJournal_GeraeteArt).asString
                          + qryJournal.FieldByName(C_WJournal_GeraeteId).asString);
        if i > -1 then s:= TIdRec(StationsListe.Objects[i]).Kennung else s:= '';
      end
      else s:= Field.asString;
      dbgrdJournal.canvas.FillRect(Rect);
      DrawString(Rect, F_LeftTrunc(s, ' '), 0, 1);
    end
  { Darstellung der Schnittstelle }
    else if Field.FieldName = C_WJournal_ComNr then begin
      if Field.asInteger = CComTCP_IP then    { Pseudo-COM-Nummer für TCP/IP-Kommunikation }
        s:=C_AnzeigeTCP_IP
      else
        s:= intToStr(Field.asInteger);
      dbgrdJournal.canvas.FillRect(Rect);
      DrawString(Rect, s, 1, 1);
    end;
  end
  else qryJournal.Close;
end;

{----------------------------------------------------}
procedure TFormJournal.dbgrdJournalDblClick(Sender: TObject);
{----------------------------------------------------}
begin
  if qryjournal.Active then
    if qryJournal.RecordCount > 0 then begin
      FormDetailJournal.SetJournal(qryJournal);
      FormDetailJournal.ShowJournal(qryJournal.FieldByName(C_WJournal_JournalId).asInteger);
      FormDetailJournal.ShowModal;
    end;
end;

{----------------------------------------------------}
procedure TFormJournal.qryJournalCalcFields(DataSet: TDataSet);
{----------------------------------------------------}
var
  s   : string;
  i   : integer;
begin
  if (not qryJournalJournalId.isNull) then begin
    i:= StationsListe.IndexOf(qryJournal.FieldByName(C_WJournal_GeraeteArt).asString
                              + qryJournal.FieldByName(C_WJournal_GeraeteId).asString);
    if i = -1 then s:= 'Unbekannte Station'
      else s:= TIdRec(StationsListe.Objects[i]).Name;
    qryJournalStationsname.asString:= s;
  end;
end;

{----------------------------------------------------}
procedure TFormJournal.sbtnDeleteRecordsClick(Sender: TObject);
{----------------------------------------------------}
begin
  if dbgrdJournal.SelectedRows.Count > 1 then begin
    if MessageDlg('Markierte Journaleinträge löschen ?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then begin
      Application.ProcessMessages;
      DeleteJournal(1);
    end;
  end
  else begin
    if MessageDlg('Alle Journaleinträge löschen ?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then begin
      Application.ProcessMessages;
      DeleteJournal(0);
    end;
  end;
end;

{ Löschen von Journaleinträgen                       }
{ Parameter: 0 - Alle, 1 - Ausgewählte               }
{----------------------------------------------------}
procedure TFormJournal.DeleteJournal(AllOrSome: byte);
{----------------------------------------------------}
var
  Save_Cursor  : TCursor;
  s            : string;
  i            : integer;
  ZSync        : TWJZSyncDB;
  MRGZBDat     : TMJZBDatenDB;
  DSfGZBDat    : TDJZBDatenDB;
  RFiles       : TWJRFilesDB;
  DSfGTelegr   : TDJTelegrDB;

begin
  isStoped:= True;
  Save_Cursor:= Screen.Cursor;
  Screen.Cursor:= crHourglass;
  try
    ZSync:= TWJZSyncDB.Create(PathServer.Pathname[WStammDir]);
    try
      RFiles:= TWJRFilesDB.Create(PathServer.Pathname[WStammDir]);
      try
        MRGZBDat:= TMJZBDatenDB.Create(PathServer.Pathname[WStammDir]);
        try
          DSfGZBDat:= TDJZBDatenDB.Create(PathServer.Pathname[WStammDir]);
          try
            DSfGTelegr:= TDJTelegrDB.Create(PathServer.Pathname[WStammDir]);
            try
              qryJournal.DisableControls;
              case AllOrSome of
                0: begin { Alle angezeigten Einträge löschen }
                     DSfGTelegr.DeleteAllRecordsFromQuery(qryJournal);
                     DSfGZBDat.DeleteAllRecordsFromQuery(qryJournal);
                     MRGZBDat.DeleteAllRecordsFromQuery(qryJournal);
                     RFiles.DeleteAllRecordsFromQuery(qryJournal);
                     ZSync.DeleteAllRecordsFromQuery(qryJournal);
                     Fehler.DeleteAllRecordsFromQuery(qryJournal);
                     Journal.DeleteAllRecordsFromQuery(qryJournal);
                   end;
                1: begin { Alle ausgewählten Einträge löschen }
                     for i:=0 to dbgrdJournal.SelectedRows.Count-1 do begin
                       dbgrdJournal.DataSource.DataSet.GotoBookmark(pointer(dbgrdJournal.SelectedRows.Items[i]));
                       if qryJournalGeraeteArt.AsString = C_GerArtDSfG then begin
                         DSfGTelegr.DeleteRecords(qryJournalJournalId.asInteger);
                         DSfGZBDat.DeleteRecords(qryJournalJournalId.asInteger);
                       end;
                       if qryJournalGeraeteArt.AsString = C_GerArtMrg then
                         MRGZBDat.DeleteRecord(qryJournalJournalId.asInteger);
                       RFiles.DeleteRecords(qryJournalJournalId.asInteger);
                       ZSync.DeleteRecord(qryJournalJournalId.asInteger);
                       Fehler.DeleteRecords(qryJournalJournalId.asInteger);
                       Journal.DeleteRecord(qryJournalJournalId.asInteger);
                     end;
                   end;
              end;
              qryJournal.EnableControls;
            finally
              DSfGTelegr.Free;
            end;
          finally
            DSfGZBDat.Free;
          end;
        finally
          MRGZBDat.Free;
        end;
      finally
        RFiles.Free;
      end;
    finally
      ZSync.Free;
    end;
  finally
    Screen.Cursor:= Save_Cursor;
  end;
  isStoped:= False;

  { Anzeige aktualisieren }
  s:= cbStation.Text;
  FillCbStation;
  if cbStation.Items.IndexOf(s) = -1
    then cbStation.ItemIndex:= cbStation.Items.IndexOf('Alle')
    else cbStation.ItemIndex:= cbStation.Items.IndexOf(s);
  GetQuery;
end;

{----------------------------------------------------}
procedure TFormJournal.sbtnQuittRecordsClick(Sender: TObject);
{----------------------------------------------------}
begin
  if dbgrdJournal.SelectedRows.Count > 1 then begin
    if MessageDlg('Markierte Journaleinträge quittieren ?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then begin
      Application.ProcessMessages;
      QuittJournal(1);
    end;
  end
  else begin
    if MessageDlg('Alle Journaleinträge quittieren ?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then begin
      Application.ProcessMessages;
      QuittJournal(0);
    end;
  end;
end;

{ Quittieren von Journaleinträgen                    }
{ Parameter: 0 - Alle, 1 - Ausgewählte               }
{----------------------------------------------------}
procedure TFormJournal.QuittJournal(AllOrSome: byte);
{----------------------------------------------------}
var
  Save_Cursor  : TCursor;
  s            : string;
  i            : integer;
begin
  isStoped:= True;
  Save_Cursor:= Screen.Cursor;
  Screen.Cursor:= crHourglass;
  try
    qryJournal.DisableControls;
    case AllOrSome of
      0: begin { Alle angezeigten Einträge quittieren }
           Journal.QuittAllRecordsFromQuery(qryJournal);
         end;
      1: begin { Alle ausgewählten Einträge quittieren }
           for i:=0 to dbgrdJournal.SelectedRows.Count-1 do begin
             dbgrdJournal.DataSource.DataSet.GotoBookmark(pointer(dbgrdJournal.SelectedRows.Items[i]));
             Journal.QuittRecord(qryJournalJournalId.asInteger);
           end;
         end;
    end;
    qryJournal.EnableControls;
  finally
    Screen.Cursor:= Save_Cursor;
  end;
  isStoped:= False;

  { Anzeige aktualisieren }
  s:= cbStation.Text;
  FillCbStation;
  if cbStation.Items.IndexOf(s) = -1
    then cbStation.ItemIndex:= cbStation.Items.IndexOf('Alle')
    else cbStation.ItemIndex:= cbStation.Items.IndexOf(s);
  GetQuery;
end;

{----------------------------------------------------}
procedure TFormJournal.pmJournalPopup(Sender: TObject);
{----------------------------------------------------}
begin
  if dbgrdJournal.SelectedRows.Count = 0 then begin
    pmQuittSome.enabled:= false;
    pmDeleteSome.enabled:= false;
  end
  else begin
    pmQuittSome.enabled:= true;
    pmDeleteSome.enabled:= true;
  end;
end;

{----------------------------------------------------}
procedure TFormJournal.pmQuittSomeClick(Sender: TObject);
{----------------------------------------------------}
begin
  QuittJournal(1);
end;

{----------------------------------------------------}
procedure TFormJournal.pmQuittAllClick(Sender: TObject);
{----------------------------------------------------}
begin
  QuittJournal(0);
end;

{----------------------------------------------------}
procedure TFormJournal.pmDeleteSomeClick(Sender: TObject);
{----------------------------------------------------}
begin
  if (dbgrdJournal.SelectedRows.Count < qryJournal.RecordCount)
    then DeleteJournal(1) else DeleteJournal(0);
end;

{----------------------------------------------------}
procedure TFormJournal.pmDeleteAllClick(Sender: TObject);
{----------------------------------------------------}
begin
  DeleteJournal(0);
end;

{----------------------------------------------------}
procedure TFormJournal.sbtnPrintClick(Sender: TObject);
{----------------------------------------------------}
var
  zbv, zbb: TDateTime;
  Ok: boolean;
begin
  if not isStoped then begin
    isStoped:= True;
  { Wenn keine Einträge bei dieser Einstellung vorhanden sind: Abbrechen }
    Ok:=true;
    if not qryJournal.Active then
      Ok:=false
    else if qryJournal.RecordCount = 0 then
      Ok:=false;
    if not Ok then begin
      isStoped:= False;
      MessageDlg('Keine Einträge zum Drucken vorhanden', mtInformation, [mbOk], 0);
      exit;
    end;

    if FormPrintJournalDialog.ShowModal = mrOK then begin
  { Einstellungen zur Zeitraum-Auswahl }
      if chbZeitraum.Checked then begin
        zbv:= dtpickVon.Date;
        zbb:= dtpickBis.Date;
      end
      else begin
        zbv:= 0;
        zbb:= 0;
      end;

      FormPrintJournal:= TFormPrintJournal.Create(Self);
      FormPrintJournal.SetJournalDruckEinstellungen(cbQuittiert.ItemIndex,
                              cbDatenZeitbereiche.ItemIndex,
                              cbZeitSynchronisation.ItemIndex,
                              cbGeraeteArt.ItemIndex,
                              cbAbrufArt.ItemIndex,
                              cbStatus.ItemIndex, zbv, zbb,
                              Journal.GetRecordCount,
                              cbStation.Text);
      qryJournal.DisableControls;
      FormPrintJournal.PrintJournal(qryJournal,
                              FormPrintJournalDialog.chbDatenTypen.Checked,
                              FormPrintJournalDialog.chbdatenZeitbereiche.Checked,
                              FormPrintJournalDialog.chbZeitSync.Checked,
                              FormPrintJournalDialog.chbFehlerWarnungen.Checked);
      qryJournal.EnableControls;
      FormPrintJournal.Free;
    end;
    isStoped:= False;
  end;
end;

{------------------------------------------------------}
procedure TFormJournal.dtpickVonChange(Sender: TObject);
{------------------------------------------------------}
begin
  if dtpickvon.Date > dtpickbis.date then
    dtpickbis.date:=dtpickvon.date;
  StartAutoAktuTimer;
end;

{------------------------------------------------------}
procedure TFormJournal.dtpickBisChange(Sender: TObject);
{------------------------------------------------------}
begin
  if dtpickbis.Date < dtpickvon.date then
    dtpickvon.date:=dtpickbis.date;
  StartAutoAktuTimer;
end;

{----------------------------------------}
procedure TFormJournal.StartAutoAktuTimer;
{----------------------------------------}
{ Timer für automatische Aktualisierung neustarten }
begin
  AutoAktuTimer.Enabled:=false;
  AutoAktuTimer.Enabled:=true;
end;

{---------------------------------------}
procedure TFormJournal.StopAutoAktuTimer;
{---------------------------------------}
{ Timer für automatische Aktualisierung stoppen }
begin
  AutoAktuTimer.Enabled:=false;
end;

{---------------------------------------------------------}
procedure TFormJournal.AutoAktuTimerTimer(Sender: TObject);
{---------------------------------------------------------}
{ Query aktualisieren ohne Triggertime-Vergleich, damit geänderte Filter-Einstellungen
  nach einer gewissen Zeit automatisch wirksam werden, falls der Aktualisieren-Schalter
  nicht gedrückt wurde }
begin
  if not isStoped then begin
    GetQuery;
{$IFDEF ALS32}
    { Ausgabe im Statusbar der Main-Unit }
    FormMain.StatusBar.Panels[3].Text:='Letzte Aktualisierung des Journals: ' + FormatDateTime('hh:mm:ss',(now));
{$ENDIF}
  end;
end;

{-----------------------------------------------}
procedure TFormJournal.cbChange(Sender: TObject);
{-----------------------------------------------}
begin
  StartAutoAktuTimer;
end;

{-----------------------------------------------}
procedure TFormJournal.cbGeraeteArtChange(Sender: TObject);
{-----------------------------------------------}
{ beim Wechsel der Geräteart auch Stationsliste in CbStation neu füllen }
begin
  FillCbStation;
  cbChange (Sender);
end;

end.


