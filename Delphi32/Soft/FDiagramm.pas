{------------------------------------------------------------------------------}
{ Diagramm-Formular                                                            }
{                                                                              }
{ 07.11.2001  GD  Neu                                                          }
{ 04.07.2002  GD  Erweitert                                                    }
{ 15.07.2002  GD  Umrechnung für x-/y-Werte für Vererbung ausgegliedert        }
{ 22.01.2004  GD  Bugfix: Bereichsangaben bei Diagrammen                       }
{ 05.08.2004  GD  Kombigraphik wieder bei beliebiger Diagrammanzahl möglich    }
{ 29.06.2005  GD  Anpassung an Delphi7 (AddObject)                             }
{ 29.09.2005  GD  Anpassung an neue TChart-Komponente                          }
{ 24.10.2005  GD  Erweitert um Setzen des Min-Max-Bereichs                     }
{ 18.06.2007  GD  Kopierschalter kopiert jetzt alle Graphiken auf ein Bitmap   }
{ 18.07.2008  GD  Alle Diagramme löschen                                       }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, RMG Messtechnik GmbH 2008               }
{------------------------------------------------------------------------------}
unit FDiagramm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TeeProcs, TeEngine, Chart, Series, StdCtrls, Buttons, CheckLst,
  Menus, clipbrd, Types,
  WSysCon, GD_Utils, FDateTimeDlg, FPrintDiagramme, PathIni;

type
  TFloatRect = record
    Left   : double;
    Right  : double;
    Top    : double;
    Bottom : double;
  end;

  TMyChartList = class(TList)
  private
  protected
    function GetChart(iIndex: integer): TChart;
  public
    procedure Delete(iIndex: integer); virtual;
    procedure Clear; override;
    function GetChartIndex(pChart: TChart): integer;
    property Chart [iIndex: integer]: TChart read GetChart;
  end;

  TMySeriesList = class(TStringList)
    constructor Create;
    destructor Destroy; override;
  private
    FAutoList : TList;
    FSizeList : TStrings;
  protected
    function GetSeries(iIndex: integer): TChartSeries;
    function GetAutomatik(iIndex: integer): boolean;
    procedure SetAutomatik(iIndex: integer; bValue: boolean);
    function GetMinMax(iIndex: integer): TFloatRect;
    procedure SetMinMax(iIndex: integer; Value: TFloatRect);
  public
    function Add(const sText: string): integer; override;
    function AddObject(const sText: string; pObject: TObject): integer; override;
    function HasMinMax(iIndex: integer): boolean;
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    property Series [iIndex: integer]: TChartSeries read GetSeries;
    property Automatic [iIndex: integer]: boolean
      read GetAutomatik write SetAutomatik;
    property MinMax [iIndex: integer]: TFloatRect read GetMinMax write SetMinMax;
  end;

  TFormDiagramm = class(TForm)
    pnRight: TPanel;
    pnClient: TPanel;
    pnRightRest: TPanel;
    pnRight1: TPanel;
    bbtnClose: TBitBtn;
    pnRight2: TPanel;
    chlbSeriesList: TCheckListBox;
    pnRight3: TPanel;
    gbMinMax: TGroupBox;
    Label1: TLabel;
    eXMin: TEdit;
    Label2: TLabel;
    eXMax: TEdit;
    Label3: TLabel;
    eYMin: TEdit;
    Label4: TLabel;
    eYMax: TEdit;
    chbMinMax: TCheckBox;
    pnRight4: TPanel;
    bbtnAktualisieren: TBitBtn;
    pnPrint: TPanel;
    bbtnPrint: TBitBtn;
    pnCopyToClipboard: TPanel;
    bbtnCopyToClipboard: TBitBtn;
    pnSave: TPanel;
    bbtnSave: TBitBtn;
    OpenDialog: TOpenDialog;
    pnRight5: TPanel;
    gbAnzeige: TGroupBox;
    rbtnKombiGraphik: TRadioButton;
    rbtnEinzelGraphik: TRadioButton;
    chbSameAxis: TCheckBox;
    cbSeriesStyle: TComboBox;
    Label5: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    cbAktSeries: TComboBox;
    Label6: TLabel;
    PopupMenuCharts : TPopupMenu;
    pmiCopyToClipboard: TMenuItem;
    pmiPrint: TMenuItem;
    pmiSave: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbtnCloseClick(Sender: TObject);
    procedure rgAufteilungClick(Sender: TObject);
    procedure chbMinMaxClick(Sender: TObject);
    procedure bbtnAktualisierenClick(Sender: TObject);
    procedure eXChange(Sender: TObject);
    procedure bbtnPrintClick(Sender: TObject);
    procedure bbtnCopyToClipboardClick(Sender: TObject);
    procedure bbtnSaveClick(Sender: TObject);
    procedure cbSeriesStyleChange(Sender: TObject);
    procedure chbSameAxisClick(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure cbAktSeriesChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pmiCopyToClipboardClick(Sender: TObject);
    procedure pmiPrintClick(Sender: TObject);
    procedure pmiSaveClick(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);  // 04.07.2002
  private
    { Private-Deklarationen }
    FChartList    : TMyChartList;
    FSeriesList   : TMySeriesList;
    FActiveChart  : TChart;
    FXDatumZeit   : boolean;
    FDTFormat     : string;
    FPanningMode  : TPanningMode;
    procedure InsertChart(pChart: TChart);
    procedure ClearClientRange;
    procedure InitSizeAxis(pChart: TChart; iSeriesIndex: integer = -1);  // 04.07.2002
    procedure SizeAxis(iSeriesIndex: integer = -1);
    procedure NotifyAxisSizes(
      iChartIndex: integer = 0; iSeriesIndex: integer = -1);
    procedure OnZoom(Sender: TObject);
    procedure OnClick(Sender: TObject);  // 04.07.2002
    procedure ChangeSeriesType;          // 04.07.2002
  protected
    function GetChart(iIndex: integer): TChart; virtual;
    property ChartList: TMyChartList read FChartList;
    procedure PrintDiagramms; virtual;
    procedure PrintDiagramm(iIndex: integer = 0); virtual;
    procedure CopyDiagramm(iIndex: integer = 0);  virtual;
    procedure SaveDiagramm(sFileName: TFileName; iIndex: integer = 0); virtual;
    procedure OneChart; virtual;
    procedure SingleCharts; virtual;
    property SeriesList: TMySeriesList read FSeriesList;
    property DateTimeFormat: string write FDTFormat;
    procedure EnableMyControls(Sender: TWinControl; bState: boolean;
      bHourGlass: boolean = True; bFirstParent: boolean = True); virtual;
    function CalcAxisValue(fValue: double; iAxis: byte): double; virtual; // 15.07.2002
  public
    { Public-Deklarationen }
    procedure NewChart(pSlXValues, pSlYValues: TStrings; sTitle: string;
      pColor: TColor; iXState: byte = 1; iChartIndex: integer = 0); virtual;
    procedure NewChartWithMinMax(pSlXValues, pSlYValues: TStrings;
      fMin, fMax: double; sTitle: string; pColor: TColor;
      iXState: byte = 1; iChartIndex: integer = 0); virtual;
    procedure NewChartProgScaled(pSlXValues, pSlYValues: TStrings;
      sTitle: string; pColor: TColor; iXState: byte = 1;
      iChartIndex: integer = 0);
    procedure Aktualisieren; virtual;
    procedure SetCaption(sCaption: string); virtual;  // 04.07.2002
    procedure ClearChartList;  // 18.07.2008
  end;

implementation

{$R *.DFM}

type
  TDiagramChartType = (dctLine, dctBar, dctPeak, dctPoint);

const
  C_Index_SingleDiagrams = 0;
  C_Index_OneChart = 1;

  C_Color_Array: array [0..19] of TColor = (clBlue, clLime, clAqua, clMaroon,
    clOlive, clTeal, clYellow, clFuchsia, clNavy, clWhite, clBlue, clLime,
    clAqua, clMaroon, clOlive, clTeal, clYellow, clFuchsia, clNavy, clWhite);

  C_Section_Diagramme = 'DIAGRAMME';
  C_Ident_SeriesStyle = 'SERIESSTYLE';
  C_Ident_PanningMode = 'PANNINGMODE';

{------------------------------------------------------}
function FloatRect(fLeft, fRight, fTop, fBottom: double): TFloatRect;
{------------------------------------------------------}
begin
  Result.Left := fLeft;
  Result.Right := fRight;
  Result.Top := fTop;
  Result.Bottom := fBottom;
end;

{-------------------------------- TMyChartList --------------------------------}

{------------------------------------------------------}
procedure TMyChartList.Delete(iIndex: integer);
{------------------------------------------------------}
var
  i : integer;
begin
  if (iIndex >= 0) and (iIndex < Count) then begin
    for i := TChart(Items[iIndex]).SeriesList.Count-1 downto 0 do
       TChart(Items[iIndex]).SeriesList[i].ParentChart := nil;
    TChart(Items[iIndex]).Free;
    Items[iIndex] := nil;
  end;

  inherited Delete(iIndex);
end;

{------------------------------------------------------}
procedure TMyChartList.Clear;
{------------------------------------------------------}
var
  i, j : integer;
begin
  for i := 0 to Count-1 do begin
    for j := TChart(Items[i]).SeriesList.Count-1 downto 0 do
       TChart(Items[i]).SeriesList[j].ParentChart := nil;
    TChart(Items[i]).Free;
    Items[i] := nil;
  end;

  inherited Clear;
end;

{ Gibt Diagrammpanel zurück                            }
{ Parameter: Index des Diagrammpanels in Liste         }
{------------------------------------------------------}
function TMyChartList.GetChart(iIndex: integer): TChart;
{------------------------------------------------------}
var
  i : integer;
begin
  if (iIndex >= Count) then for i := Count to iIndex do begin
    Add(TChart.Create(nil));
    TChart(Items[Count-1]).View3D := False;
  end;
  Result := TChart(Items[iIndex]);
end;

{ Gibt Diagrammpanel zurück                            }
{ Parameter: Index des Diagrammpanels in Liste         }
{------------------------------------------------------}
function TMyChartList.GetChartIndex(pChart: TChart): integer;
{------------------------------------------------------}
var
  i : integer;
begin
  Result := -1;  // Default

  for i := 0 to Count-1 do
    if (GetChart(i) = pChart) then begin
      Result := i;
      Break;
    end;
end;

{-------------------------------- TMySeriesList -------------------------------}

{------------------------------------------------------}
constructor TMySeriesList.Create;
{------------------------------------------------------}
begin
  inherited Create;

  FAutoList := TList.Create;
  FSizeList := TStringList.Create;
end;

{------------------------------------------------------}
destructor TMySeriesList.Destroy;
{------------------------------------------------------}
begin
  FAutoList.Free;
  FSizeList.Free;

  inherited Destroy;
end;

{------------------------------------------------------}
procedure TMySeriesList.Delete(iIndex: integer);
{------------------------------------------------------}
begin
  if (iIndex >= 0) and (iIndex < Count) then begin
    Objects[iIndex].Free;
    Objects[iIndex] := nil;
  end;

  inherited Delete(iIndex);
end;

{------------------------------------------------------}
procedure TMySeriesList.Clear;
{------------------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Count-1 do begin
    Objects[i].Free;
    Objects[i] := nil;
  end;

  inherited Clear;
end;

{------------------------------------------------------}
function TMySeriesList.Add(const sText: string): integer;
{------------------------------------------------------}
var
  s : string;
  i : integer;
begin
  s := sText;
  // Bei gleichen Texten um Zähler ergänzen
  if (IndexOf(s) >= 0) then begin
    i := 2;
    while (IndexOf(s + '(' + IntToStr(i) + ')') >= 0) do Inc(i);
    s := s + '(' + IntToStr(i) + ')';
  end;

  Result := inherited Add(s);
  while (FAutoList.Count < Count) do FAutoList.Add(TObject(1));
  FSizeList.Add('');
end;

{------------------------------------------------------}
function TMySeriesList.AddObject(
  const sText: string; pObject: TObject): integer;
{------------------------------------------------------}
var
  s : string;
  i : integer;
begin
  s := sText;
  // Bei gleichen Texten um Zähler ergänzen
  if (IndexOf(s) >= 0) then begin
    i := 2;
    while (IndexOf(s + '(' + IntToStr(i) + ')') >= 0) do Inc(i);
    s := s + '(' + IntToStr(i) + ')';
  end;

  Result := inherited AddObject(s, pObject);
  while (FAutoList.Count < Count) do FAutoList.Add(TObject(1));
  while (FSizeList.Count < Count) do FSizeList.Add('');
end;

{------------------------------------------------------}
function TMySeriesList.HasMinMax(iIndex: integer): boolean;
{------------------------------------------------------}
begin
  Result := (FSizeList[iIndex] <> '');
end;

{------------------------------------------------------}
function TMySeriesList.GetMinMax(iIndex: integer): TFloatRect;
{------------------------------------------------------}
begin
  Result.Left := WStrToFloatDef(GetStringPart(FSizeList[iIndex], 1), 0);
  Result.Right := WStrToFloatDef(GetStringPart(FSizeList[iIndex], 2), 0);
  Result.Bottom := WStrToFloatDef(GetStringPart(FSizeList[iIndex], 3), 0);
  Result.Top := WStrToFloatDef(GetStringPart(FSizeList[iIndex], 4), 0);
end;

{------------------------------------------------------}
procedure TMySeriesList.SetMinMax(iIndex: integer; Value: TFloatRect);
{------------------------------------------------------}
begin
  FSizeList[iIndex] := FloatToStr(Value.Left) + Chr(us) + FloatToStr(Value.Right) +
    Chr(us) + FloatToStr(Value.Bottom) + Chr(us) + FloatToStr(Value.Top);
end;

{------------------------------------------------------}
function TMySeriesList.GetAutomatik(iIndex: integer): boolean;
{------------------------------------------------------}
begin
  Result := Boolean(Integer(FAutoList[iIndex]));
end;

{------------------------------------------------------}
procedure TMySeriesList.SetAutomatik(iIndex: integer; bValue: boolean);
{------------------------------------------------------}
begin
  FAutoList[iIndex] := TObject(Integer(bValue));
end;

{ Gibt Diagrammpanel zurück                            }
{ Parameter: Index des Diagrammpanels in Liste         }
{------------------------------------------------------}
function TMySeriesList.GetSeries(iIndex: integer): TChartSeries;
{------------------------------------------------------}
var
  i : integer;
begin
  if (iIndex >= Count) then for i := Count to iIndex do begin
    AddObject('', TChartSeries.Create(nil));
  end;

  Result := TChartSeries(Objects[iIndex]);
end;

{-------------------------------- TFormAkaChart -------------------------------}

{------------------------------------------------------}
procedure TFormDiagramm.FormCreate(Sender: TObject);
{------------------------------------------------------}
begin
  // Diagrammtyp laden
  with TProgramIni.Create do
  try
    cbSeriesStyle.ItemIndex :=
      ReadInteger(C_Section_Diagramme, C_Ident_SeriesStyle, 0);
    FPanningMode := TPanningMode(ReadInteger(
      C_Section_Diagramme, C_Ident_PanningMode, Integer(pmBoth)));
  finally
    Free;
  end;

  FActiveChart := nil;                 // Zur Zeit aktives Diagramm
  FChartList := TMyChartList.Create;   // Liste für zur Laufzeit erzeugte Charts
  FSeriesList := TMySeriesList.Create; // Liste der Diagramme
  pnRightRest.Color := C_ColorWieser;
  FXDatumZeit := False;
  FDTFormat := C_FormatDateTime;
  chbMinMaxClick(nil);
end;

{------------------------------------------------------}
procedure TFormDiagramm.FormDestroy(Sender: TObject);
{------------------------------------------------------}
begin
  // Diagrammtyp speichern
  with TProgramIni.Create do
  try
    WriteInteger(
      C_Section_Diagramme, C_Ident_SeriesStyle, cbSeriesStyle.ItemIndex);
  finally
    Free;
  end;

  // Zur Laufzeit erzeugte Charts und Diagramme freigeben
  FChartList.Free;
  FSeriesList.Free;
end;

{------------------------------------------------------}
procedure TFormDiagramm.FormClose(Sender: TObject; var Action: TCloseAction);
{------------------------------------------------------}
begin
  Action := caFree;
end;

{------------------------------------------------------}
procedure TFormDiagramm.bbtnCloseClick(Sender: TObject);
{------------------------------------------------------}
begin
  Close;
end;

{ Gibt Diagrammpanel zurück                            }
{ Parameter: Index des Diagrammpanels in Liste         }
{------------------------------------------------------}
function TFormDiagramm.GetChart(iIndex: integer): TChart;
{------------------------------------------------------}
begin
  Result := FChartList.Chart[iIndex];
  Result.PopupMenu := Self.PopupMenuCharts;
  Result.OnScroll := onZoom;
  Result.OnZoom := onZoom;
  Result.OnUndoZoom := onZoom;
  Result.OnClick := OnClick;
  Result.AllowPanning := FPanningMode;
  Result.AllowZoom := (FPanningMode <> pmNone);
  if (FXDatumZeit) then begin
    Result.BottomAxis.DateTimeFormat := FDTFormat;
    Result.TopAxis.DateTimeFormat := FDTFormat;
  end;
  if (not Assigned(Result.Parent)) then InsertChart(Result);
end;

{ Fügt Diagrammpanel in Formular ein                   }
{ Parameter: Diagrammpanel                             }
{------------------------------------------------------}
procedure TFormDiagramm.InsertChart(pChart: TChart);
{------------------------------------------------------}
var
  i, iHeight, iCount : integer;
begin
  // Anzahl der Charts ermitteln
  iCount := 0;
  for i := 0 to pnClient.ControlCount-1 do
    if (pnClient.Controls[i] is TChart) then begin
      TChart(pnClient.Controls[i]).Align := alTop;
      Inc(iCount);
    end;

  if (iCount > 0) then begin
    with TSplitter.Create(pnClient) do begin
      Parent := pnClient;
      Align := alTop;
      AutoSnap := False;
      Tag := iCount;

      iHeight := (pnClient.ClientHeight - (iCount*Height)) div (iCount+1);
      for i := 0 to pnClient.ControlCount-1 do
        if (pnClient.Controls[i] is TChart) then
          pnClient.Controls[i].Height := iHeight;

      pChart.Tag := iCount;
      pChart.Parent := pnClient;
      pChart.Align := alClient;
      Application.ProcessMessages;

      Top := pChart.Top;
    end;
  end
  else begin
    pChart.Tag := iCount;
    pChart.Parent := pnClient;
    pChart.Align := alClient;
    Application.ProcessMessages;
  end;
end;

{ Zeichnet neues Diagramm                              }
{ Parameter: Listen mit X/Y-Werten, Art der X-Werte    }
{            (1=Float,2=DateTime)                      }
{------------------------------------------------------}
procedure TFormDiagramm.NewChart(pSlXValues, pSlYValues: TStrings;
  sTitle: string; pColor: TColor; iXState: byte = 1; iChartIndex: integer = 0);
{------------------------------------------------------}
resourcestring
  S_FormDiagramm_Error = 'Fehler !';

  function CheckForFloat(
    const sValue: string; var s: string; var pColor: TColor): double;
  begin
    try
      Result := StrToFloat(
        StringReplace(sValue, ThousandSeparator, '', [rfReplaceAll]));
    except
      s := S_FormDiagramm_Error;
      pColor := clRed;
      Result := 0;
    end;
  end;

var
  pSeries : TChartSeries;
  i       : integer;
  x, y    : double;
  s       : string;
  iDelay  : integer;
begin
  iDelay := 0;  // Counter für DELAY zur Entlastung des Rechners

  // Nur Diagramme mit gleichen X-Achsentyp zugelassen
  if (iXState = 1) then begin
    if (FSeriesList.Count > 0) then begin
      if (FXDatumZeit) then Exit;
    end
    else FXDatumZeit := False;
  end
  else if (iXState = 2) then begin
    if (FSeriesList.Count > 0) then begin
      if (not FXDatumZeit) then Exit;
    end
    else FXDatumZeit := True;
  end;

  if (pSlXValues.Count = pSlYValues.Count) then begin
    if (integer(pColor) = 0) and (High(C_Color_Array) > FSeriesList.Count) then
      pColor := C_Color_Array[FSeriesList.Count];

    case TDiagramChartType(cbSeriesStyle.ItemIndex) of
      dctLine  : pSeries := TLineSeries.Create(Self);
      dctBar  : begin
                   pSeries := TBarSeries.Create(Self);
                   TBarSeries(pSeries).BarWidthPercent := 70;
                 end;
      dctPeak  : begin
                   pSeries := TBarSeries.Create(Self);
                   TBarSeries(pSeries).BarWidthPercent := 1;
                 end;
      dctPoint : pSeries := TPointSeries.Create(Self);
      else pSeries := TLineSeries.Create(Self);
    end;

    pSeries.Marks.Visible := False;
    pSeries.SeriesColor := pColor;
    pSeries.Title := sTitle;
    pSeries.XValues.DateTime := (iXState = 2);

    for i := 0 to pSlXValues.Count-1 do begin
      s := '';
      pColor := pSeries.SeriesColor;
      x := CheckForFloat(pSlXValues[i], s, pColor);
      if (s <> S_FormDiagramm_Error) then x := CalcAxisValue(x, 1);
      y := CheckForFloat(pSlYValues[i], s, pColor);
      if (s <> S_FormDiagramm_Error) then y := CalcAxisValue(y, 2);
      pSeries.AddXY(x, y, s, pColor);
      
      Inc(iDelay);
      if ((iDelay mod 500) = 0) then Sleep(1);
    end;

    FSeriesList.AddObject(sTitle, pSeries);
    chlbSeriesList.Items.Add(FSeriesList[FSeriesList.Count-1]);
    chlbSeriesList.Checked[FSeriesList.Count-1] := True;
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.NewChartWithMinMax(pSlXValues, pSlYValues: TStrings;
 fMin, fMax: double; sTitle: string; pColor: TColor;
 iXState: byte = 1; iChartIndex: integer = 0);
{------------------------------------------------------}
var
  iCount : integer;
  p      : TFloatRect;
begin
  iCount := SeriesList.Count;
  NewChart(pSlXValues, pSlYValues, sTitle, pColor, iXState, iChartIndex);
  if (SeriesList.Count > iCount) then begin
    p.Left := SeriesList.Series[SeriesList.Count-1].MinXValue;
    p.Right := SeriesList.Series[SeriesList.Count-1].MaxXValue;
    p.Top := fMax;
    p.Bottom := fMin;
    SeriesList.MinMax[SeriesList.Count-1] := p;
    SeriesList.Automatic[SeriesList.Count-1] := False;
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.NewChartProgScaled(pSlXValues, pSlYValues: TStrings;
  sTitle: string; pColor: TColor; iXState: byte = 1; iChartIndex: integer = 0);
{------------------------------------------------------}
var
  iCount : integer;
  p      : TFloatRect;
begin
  iCount := SeriesList.Count;
  NewChart(pSlXValues, pSlYValues, sTitle, pColor, iXState, iChartIndex);
  if (SeriesList.Count > iCount) then begin
    p.Left := SeriesList.Series[SeriesList.Count-1].MinXValue;
    p.Right := SeriesList.Series[SeriesList.Count-1].MaxXValue;
    WRangeSet(SeriesList.Series[SeriesList.Count-1].MinYValue,
      SeriesList.Series[SeriesList.Count-1].MaxYValue, p.Bottom, p.Top);
    SeriesList.MinMax[SeriesList.Count-1] := p;
    SeriesList.Automatic[SeriesList.Count-1] := False;
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.ClearClientRange;
{------------------------------------------------------}
var
  i              : integer;
  x1, x2, y1, y2 : double;
begin
  // Einstellungen der Charts bei Series speichern
  for i := 0 to FChartList.Count-1 do
    with FChartList.Chart[i], FSeriesList do begin
      if (SeriesList.Count = 1) then begin
        Automatic[Tag] := BottomAxis.Automatic;
        if (BottomAxis.Visible) then begin
          x1 := BottomAxis.Minimum;
          x2 := BottomAxis.Maximum;
        end
        else begin
          x1 := TopAxis.Minimum;
          x2 := TopAxis.Maximum;
        end;
        if (LeftAxis.Visible) then begin
          y1 := LeftAxis.Minimum;
          y2 := LeftAxis.Maximum;
        end
        else begin
          y1 := RightAxis.Minimum;
          y2 := RightAxis.Maximum;
        end;
        MinMax[Tag] := FloatRect(x1, x2, y2, y1);
      end;
    end;

  FChartList.Clear;
  cbAktSeries.Clear;
  for i := pnClient.ControlCount-1 downto 0 do pnClient.Controls[i].Free;
end;

{ Bildet alle Diagramme in einer Chart ab      ´       }
{------------------------------------------------------}
procedure TFormDiagramm.OneChart;
{------------------------------------------------------}
var
  pChart  : TChart;
  pSeries : TChartSeries;
  i, j    : integer;
  b       : boolean;
begin
  ClearClientRange;
  pChart := GetChart(0);
  for i := 0 to chlbSeriesList.Items.Count-1 do
    if (chlbSeriesList.Checked[i]) then begin
      pSeries := FSeriesList.GetSeries(i);
      pChart.Tag := i;
      pSeries.ParentChart := pChart;
      j := pChart.SeriesList.Count-1;
      if (not chbSameAxis.Checked) and ((j mod 2) > 0) then begin
        pSeries.HorizAxis := aTopAxis;
        pSeries.VertAxis := aRightAxis;
      end
      else begin
        pSeries.HorizAxis := aBottomAxis;
        pSeries.VertAxis := aLeftAxis;
      end;
      pChart.Title.Text.Text := FSeriesList[i];
      cbAktSeries.Items.Add(FSeriesList[i]);
    end;

  if (pChart.SeriesList.Count = 0)
  then ClearClientRange
  else begin
    b := (pChart.SeriesList.Count > 1);
    for i := 0 to pChart.SeriesList.Count-1 do
      pChart.SeriesList[i].ShowInLegend := b;
    if (b) then pChart.Title.Text.Clear;
  end;

  InitSizeAxis(pChart);
end;

{ Bildet jedes Diagramm in einer eigenen Chart ab      }
{------------------------------------------------------}
procedure TFormDiagramm.SingleCharts;
{------------------------------------------------------}
var
  pChart  : TChart;
  pSeries : TChartSeries;
  i, j    : integer;
begin
  ClearClientRange;
  j := 0;
  for i := 0 to chlbSeriesList.Items.Count-1 do begin
    if (chlbSeriesList.Checked[i]) then begin
      cbAktSeries.Items.Add(FSeriesList[i]);
      pChart := GetChart(j);
      pSeries := FSeriesList.GetSeries(i);
//      pChart.SeriesList.Add(pSeries);
      pSeries.HorizAxis := aBottomAxis;
      pSeries.VertAxis := aLeftAxis;
      pSeries.ParentChart := pChart;
      pChart.Tag := i;
      pChart.Title.Text.Text := FSeriesList[i];
      pSeries.ShowInLegend := False;
      InitSizeAxis(pChart);
      Inc(j);
    end;
  end;
  FormResize(nil);
end;

{------------------------------------------------------}
procedure TFormDiagramm.InitSizeAxis(pChart: TChart; iSeriesIndex: integer = -1);
{------------------------------------------------------}
var
  i              : integer;
  r, rMax        : TFloatRect;
  bSizeAxis      : boolean;
  pXAxis, pYAxis : TChartAxis;
  pSeries        : TChartSeries;
begin
  with pChart do begin
    if (iSeriesIndex < 0) then iSeriesIndex := Tag;

    pSeries := FSeriesList.Series[iSeriesIndex];
    if (pSeries.HorizAxis = aBottomAxis)
    then pXAxis := pChart.BottomAxis
    else pXAxis := pChart.TopAxis;
    if (pSeries.VertAxis = aLeftAxis)
    then pYAxis := pChart.LeftAxis
    else pYAxis := pChart.RightAxis;

    pXAxis.Automatic := FSeriesList.Automatic[iSeriesIndex];
    pYAxis.Automatic := (FSeriesList.Automatic[iSeriesIndex] and
      (pSeries.MinYValue <> pSeries.MaxYValue));
    if (pSeries.MinYValue = pSeries.MaxYValue) then begin
      if (pSeries.MinYValue >= pYAxis.Maximum) then
        pYAxis.Maximum := Trunc(pSeries.MaxYValue) + 1;
      if (Frac(pSeries.MinYValue) = 0)
      then pYAxis.Minimum := Trunc(pSeries.MinYValue) - 1
      else pYAxis.Minimum := Trunc(pSeries.MinYValue);
      pYAxis.Maximum := Trunc(pSeries.MaxYValue) + 1;
    end;

    bSizeAxis := False;
    if (pChart.SeriesList.Count > 1) and (chbSameAxis.Checked) then begin
      rMax := FSeriesList.MinMax[0];
      for i := 1 to pChart.SeriesList.Count-1 do begin
        r := FSeriesList.MinMax[i];
        if (r.Left < rMax.Left) then rMax.Left := r.Left;
        if (r.Right > rMax.Right) then rMax.Right := r.Right;
        if (r.Bottom < rMax.Bottom) then rMax.Bottom := r.Bottom;
        if (r.Top > rMax.Top) then rMax.Top := r.Top;
      end;
      bSizeAxis := True;
    end
    else if (FSeriesList.HasMinMax(iSeriesIndex)) and
      (not FSeriesList.Automatic[iSeriesIndex]) then
    begin
      rMax := FSeriesList.MinMax[iSeriesIndex];
      bSizeAxis := True;
    end;

    if (bSizeAxis) then begin
      pXAxis.Maximum := pXAxis.Minimum;
      if (rMax.Right >= pXAxis.Minimum) then begin
        pXAxis.Maximum := rMax.Right;
        pXAxis.Minimum := rMax.Left;
      end
      else begin
        pXAxis.Minimum := rMax.Left;
        pXAxis.Maximum := rMax.Right;
      end;

      pYAxis.Maximum := pYAxis.Minimum;
      if (rMax.Top >= pYAxis.Minimum) then begin
        pYAxis.Maximum := rMax.Top;
        pYAxis.Minimum := rMax.Bottom;
      end
      else begin
        pYAxis.Minimum := rMax.Bottom;
        pYAxis.Maximum := rMax.Top;
      end;
    end;
  end;
  NotifyAxisSizes(FChartList.GetChartIndex(pChart), iSeriesIndex);
end;

{------------------------------------------------------}
procedure TFormDiagramm.rgAufteilungClick(Sender: TObject);
{------------------------------------------------------}
begin
  Aktualisieren;
end;

{ Zeigt Einteilung der Achsen in Edit-Feldern an       }
{ Parameter: Chart, deren Werte angezeigt werden       }
{------------------------------------------------------}
procedure TFormDiagramm.NotifyAxisSizes(
  iChartIndex: integer = 0; iSeriesIndex: integer = -1);
{------------------------------------------------------}
var
  pChart         : TChart;
  pXAxis, pYAxis : TChartAxis;
  pSeries        : TChartSeries;
  i              : integer;
begin
  if (FChartList.Count <= iChartIndex)
  then Exit
  else pChart := FChartList.GetChart(iChartIndex);

  if (iSeriesIndex < 0) then i := pChart.Tag else i := iSeriesIndex;
  pSeries := FSeriesList.Series[i];
  if (pSeries.HorizAxis = aBottomAxis)
  then pXAxis := pChart.BottomAxis
  else pXAxis := pChart.TopAxis;
  if (pSeries.VertAxis = aLeftAxis)
  then pYAxis := pChart.LeftAxis
  else pYAxis := pChart.RightAxis;

  if (pXAxis.Automatic) then begin
    if (FXDatumZeit) then begin
      eXMin.Text := DateTimeToStr(pChart.MinXValue(pXAxis));
      eXMax.Text := DateTimeToStr(pChart.MaxXValue(pXAxis));
    end
    else begin
      eXMin.Text := FormatFloat('0.000', pChart.MinXValue(pXAxis));
      eXMax.Text := FormatFloat('0.000', pChart.MaxXValue(pXAxis));
    end;
  end
  else begin
    if (FXDatumZeit) then begin
      eXMin.Text := DateTimeToStr(pXAxis.Minimum);
      eXMax.Text := DateTimeToStr(pXAxis.Maximum);
    end
    else begin
      eXMin.Text := FormatFloat('0.000', pXAxis.Minimum);
      eXMax.Text := FormatFloat('0.000', pXAxis.Maximum);
    end;
  end;
  if (pChart.BottomAxis.Automatic) then begin
    eYMin.Text := FormatFloat('0.000', pChart.MinYValue(pYAxis));
    eYMax.Text := FormatFloat('0.000', pChart.MaxYValue(pYAxis));
  end
  else begin
    eYMin.Text := FormatFloat('0.000', pYAxis.Minimum);
    eYMax.Text := FormatFloat('0.000', pYAxis.Maximum);
  end;

  FActiveChart := pChart;
  cbAktSeries.ItemIndex := cbAktSeries.Items.IndexOf(FSeriesList[i]);
end;

{ Setzt Einteilung der Achsen für aktuelles Diagramm   }
{------------------------------------------------------}
procedure TFormDiagramm.SizeAxis(iSeriesIndex: integer = -1);
{------------------------------------------------------}

  function GetAxisFloat(
    sText: string; fDefault: double; bXAxis: boolean = False): double;
  begin
    try
      if (bXAxis) and (FXDatumZeit) then begin
        Result := StrToDateTime(sText);
      end
      else begin
        Result := WStrToFloatDef(sText, fDefault);
      end;
    except
      Result := fDefault;
    end;
  end;

var
  pXAxis, pYAxis : TChartAxis;
  pSeries        : TChartSeries;
  i              : integer;
begin
  if (Assigned(FActiveChart)) and
     (GetAxisFloat(eXMin.Text, 0, True) < GetAxisFloat(eXMax.Text, 0, True)) and
     (WStrToFloatDef(eYMin.Text, 0) < WStrToFloatDef(eYMax.Text, 0))
  then
    with (FActiveChart) do begin

      if (iSeriesIndex < 0) then i := Tag else i := iSeriesIndex;
      pSeries := FSeriesList.Series[i];
      if (pSeries.HorizAxis = aBottomAxis)
      then pXAxis := BottomAxis
      else pXAxis := TopAxis;
      if (pSeries.VertAxis = aLeftAxis)
      then pYAxis := LeftAxis
      else pYAxis := RightAxis;

      if (not pXAxis.Automatic) then begin
        pXAxis.SetMinMax(GetAxisFloat(eXMin.Text, pXAxis.Minimum, True),
          GetAxisFloat(eXMax.Text, pXAxis.Maximum, True));
        pYAxis.SetMinMax(GetAxisFloat(eYMin.Text, pYAxis.Minimum),
          GetAxisFloat(eYMax.Text, pYAxis.Maximum));
      end;

      NotifyAxisSizes(FChartList.GetChartIndex(FActiveChart), i);
    end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.chbMinMaxClick(Sender: TObject);
{------------------------------------------------------}
var
  pXAxis, pYAxis : TChartAxis;
  pSeries        : TChartSeries;
begin
  if (Assigned(FActiveChart)) and (cbAktSeries.Text <> '') then begin
    pSeries := FSeriesList.Series[FSeriesList.IndexOf(cbAktSeries.Text)];
    with pSeries do begin
      if (HorizAxis = aBottomAxis)
      then pXAxis := FActiveChart.BottomAxis
      else pXAxis := FActiveChart.TopAxis;
      if (VertAxis = aLeftAxis)
      then pYAxis := FActiveChart.LeftAxis
      else pYAxis := FActiveChart.RightAxis;
    end;

    pXAxis.Automatic := not chbMinMax.Checked;
    pYAxis.Automatic := (not chbMinMax.Checked) and
      (pSeries.MinYValue <> pSeries.MaxYValue);
    if (pSeries.MinYValue = pSeries.MaxYValue) then begin
      if (pSeries.MinYValue >= pYAxis.Maximum) then
        pYAxis.Maximum := Trunc(pSeries.MaxYValue) + 1;
      if (Frac(pSeries.MinYValue) = 0)
      then pYAxis.Minimum := Trunc(pSeries.MinYValue) - 1
      else pYAxis.Minimum := Trunc(pSeries.MinYValue);
      pYAxis.Maximum := Trunc(pSeries.MaxYValue) + 1;
    end;
  end;

  eXMin.Enabled := chbMinMax.Checked;
  eXMax.Enabled := chbMinMax.Checked;
  eYMin.Enabled := chbMinMax.Checked;
  eYMax.Enabled := chbMinMax.Checked;
end;

{------------------------------------------------------}
procedure TFormDiagramm.Aktualisieren;
{------------------------------------------------------}
var
  i         : integer;
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    // Anzahl aktiver Series ermitteln
//    j := 0;
//    for i := 0 to chlbSeriesList.Items.Count-1 do
//      if (chlbSeriesList.Checked[i]) then Inc(j);

    // Kombigraphik nur bei zwei Series möglich
//    rbtnKombiGraphik.Enabled := (j = 2);  // 05.08.2004
//    if (not rbtnKombiGraphik.Enabled) then rbtnEinzelGraphik.Checked := True;

    // Einzel/Kombigraphik
    if (rbtnEinzelGraphik.Checked) then SingleCharts
    else if (rbtnKombiGraphik.Checked) then OneChart
    else Exit;
    chbSameAxis.Enabled := (rbtnKombiGraphik.Checked);
    for i := 0 to cbAktSeries.Items.Count-1 do
      SizeAxis(FSeriesList.IndexOf(cbAktSeries.Items[i]));
    EditExit(Self);
  finally
    Screen.Cursor := oldCursor;
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.bbtnAktualisierenClick(Sender: TObject);
{------------------------------------------------------}
begin
  Aktualisieren;
end;

{------------------------------------------------------}
procedure TFormDiagramm.OnZoom(Sender: TObject);
{------------------------------------------------------}
var
  i              : integer;
  pXAxis, pYAxis : TChartAxis;
begin
  if (Sender is TChart) then begin
    FActiveChart := TChart(Sender);

    if (rbtnKombiGraphik.Checked) and (cbAktSeries.ItemIndex >= 0)
    then i := FSeriesList.IndexOf(cbAktSeries.Items[cbAktSeries.ItemIndex])
    else i := FActiveChart.Tag;

    if (FPanningMode = pmHorizontal) then begin
      if (FSeriesList.Series[i].VertAxis = aLeftAxis)
      then pYAxis := FActiveChart.LeftAxis
      else pYAxis := FActiveChart.RightAxis;
      pYAxis.SetMinMax(
        FActiveChart.MinYValue(pYAxis), FActiveChart.MaxYValue(pYAxis));
    end
    else if (FPanningMode = pmVertical) then begin
      if (FSeriesList.Series[i].HorizAxis = aBottomAxis)
      then pXAxis := FActiveChart.BottomAxis
      else pXAxis := FActiveChart.TopAxis;
      pXAxis.SetMinMax(
        FActiveChart.MinXValue(pXAxis), FActiveChart.MaxXValue(pXAxis));
    end;

    NotifyAxisSizes(FChartList.GetChartIndex(TChart(Sender)), i);
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.OnClick(Sender: TObject);
{------------------------------------------------------}
var
  i      : integer;
  pXAxis : TChartAxis;
begin
  if (Sender is TChart) then begin
    FActiveChart := TChart(Sender);

    if (rbtnKombiGraphik.Checked) and (cbAktSeries.ItemIndex >= 0) then begin
      i := FSeriesList.IndexOf(cbAktSeries.Items[cbAktSeries.ItemIndex]);
      if (FSeriesList.Series[i].HorizAxis = aBottomAxis)
      then pXAxis := FActiveChart.BottomAxis
      else pXAxis := FActiveChart.TopAxis;
    end
    else begin
      i := -1;
      pXAxis := FActiveChart.BottomAxis;
    end;

    chbMinMax.Checked := (not pXAxis.Automatic);
    NotifyAxisSizes(FChartList.GetChartIndex(FActiveChart), i);
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.eXChange(Sender: TObject);
{------------------------------------------------------}
begin
  if (Sender is TEdit) then
    with TFormDateTimeDlg.Create(Self) do
    try
      try
        DateTime := StrToDateTime(TEdit(Sender).Text);
      except
        // nix
      end;
      if (ShowModal = mrOk) then begin
        TEdit(Sender).Text := DateTimeToStr(DateTime);
      end;
    finally
      Free;
    end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.bbtnPrintClick(Sender: TObject);
{------------------------------------------------------}
begin
  PrintDiagramms;
end;

{------------------------------------------------------}
procedure TFormDiagramm.bbtnSaveClick(Sender: TObject);
{------------------------------------------------------}
begin
  if (OpenDialog.Execute) then SaveDiagramm(OpenDialog.FileName);
end;

{------------------------------------------------------}
procedure TFormDiagramm.bbtnCopyToClipboardClick(Sender: TObject);
{------------------------------------------------------}
begin
  CopyDiagramm(-1);
end;

{------------------------------------------------------}
procedure TFormDiagramm.CopyDiagramm(iIndex: integer = 0);
{------------------------------------------------------}
var
  oldCursor : TCursor;
  pResBitmap, pBitmap : TBitmap;
  i, iWidth, iHeight, iTop  : integer;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if (iIndex = -1) and (FChartList.Count > 0) then begin  // 18.06.2007
      pResBitmap := TBitmap.Create;
      try
        iWidth := 0;
        iHeight := 0;
        iTop := 0;
        for i := 0 to FChartList.Count-1 do begin
          with GetChart(i).GetRectangle do begin
            iHeight := iHeight + Bottom;
            if (Right > iWidth) then iWidth := Right;
          end;
        end;
        pResBitmap.Height := iHeight;
        pResBitmap.Width := iWidth;

        for i := 0 to FChartList.Count-1 do begin
          pBitmap :=
            GetChart(i).TeeCreateBitmap(clWhite, GetChart(i).GetRectangle);
          try
            pResBitmap.Canvas.StretchDraw(Rect(0, iTop,
              pBitmap.Width, iTop+pBitmap.Height),
              pBitmap);
            iTop := iTop + pBitmap.Height;
          finally
            pBitmap.Free;
          end;
        end;
        ClipBoard.Assign(pResBitmap);
      finally
        pResBitmap.Free;
      end;
    end
    else begin
      if (FChartList.Count > iIndex) then begin
        pBitmap := GetChart(iIndex).TeeCreateBitmap(
          clWhite, GetChart(iIndex).GetRectangle);
        try
          ClipBoard.Assign(pBitmap);
        finally
          pBitmap.Free;
        end;
      end;
    end;

  finally
    Screen.Cursor := oldCursor;
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.PrintDiagramms;
{------------------------------------------------------}
var
  i, j      : integer;
  oldCursor : TCursor;
  pSl       : TStrings;
  s, sTitle : string;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  pSl := TStringList.Create;
  try
    for i := 0 to 2 do pSl.Add('');  // Überschriftenliste
    sTitle := Self.Caption;
    i := Pos('[', sTitle);
    while (i > 0) do begin
      sTitle := GetStringPart(sTitle, 2, '[');
      i := Pos('[', sTitle);
    end;
    sTitle := GetStringPart(sTitle, 1, ']');

    for i := 0 to FChartList.Count-1 do begin
      if (rbtnKombiGraphik.Checked) then begin
        s := '';
        for j := 0 to chlbSeriesList.Items.Count-1 do
          if (chlbSeriesList.Checked[j]) then
            s := s + chlbSeriesList.Items[j] + ',';
        System.Delete(s, Length(s), 1);
      end
      else begin
        s := Self.GetChart(i).Title.Text.CommaText;
      end;

      with TFormPrintDiagramm.Create(Self) do
      try
        pSl[1] := sTitle;
        pSl[2] := s;
        Captions := pSl;
        PrintChart(Self.GetChart(i));
      finally
        Free;
      end;
    end;
  finally
    pSl.Free;
    Screen.Cursor := oldCursor;
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.PrintDiagramm(iIndex: integer = 0);
{------------------------------------------------------}
var
  i         : integer;
  oldCursor : TCursor;
  pSl       : TStrings;
  s, sTitle : string;
begin
  if (FChartList.Count > iIndex) then begin
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    pSl := TStringList.Create;
    try
      for i := 0 to 2 do pSl.Add('');  // Überschriftenliste
      sTitle := Self.Caption;
      i := Pos('[', sTitle);
      while (i > 0) do begin
        sTitle := GetStringPart(sTitle, 2, '[');
        i := Pos('[', sTitle);
      end;
      sTitle := GetStringPart(sTitle, 1, ']');

      s := Self.GetChart(iIndex).Title.Text.CommaText;

      with TFormPrintDiagramm.Create(Self) do
      try
        pSl[1] := sTitle;
        pSl[2] := s;
        Captions := pSl;
        PrintChart(Self.GetChart(iIndex));
      finally
        Free;
      end;
    finally
      pSl.Free;
      Screen.Cursor := oldCursor;
    end;
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.SaveDiagramm(sFileName: TFileName; iIndex: integer = 0);
{------------------------------------------------------}
var
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if (FChartList.Count > iIndex) then
      GetChart(iIndex).SaveToBitmapFile(sFileName);
  finally
    Screen.Cursor := oldCursor;
  end;
end;

{ Setzt Überschrift (für Vererbung !)                  }
{ Parameter: Neue Überschrift                          }
{------------------------------------------------------}
procedure TFormDiagramm.SetCaption(sCaption: string);
{------------------------------------------------------}
begin
  Caption := sCaption;
end;

{ Änderung des Series-Typs                             }
{------------------------------------------------------}
procedure TFormDiagramm.ChangeSeriesType;
{------------------------------------------------------}
var
  pAlt, pNeu : TChartSeries;
  i          : integer;
begin
  for i := 0 to FSeriesList.Count-1 do begin

    pAlt := FSeriesList.Series[i];

    case TDiagramChartType(cbSeriesStyle.ItemIndex) of
      dctLine : pNeu := TLineSeries.Create(Self);
      dctBar  : pNeu := TBarSeries.Create(Self);
      dctPeak : pNeu := TBarSeries.Create(Self);
      dctPoint : pNeu := TPointSeries.Create(Self);
      else pNeu := nil;
    end;
    if (not Assigned(pNeu)) then Exit;

    pNeu.Assign(pAlt);
    if (TDiagramChartType(cbSeriesStyle.ItemIndex) = dctBar) then
      TBarSeries(pNeu).BarWidthPercent := 70
    else if (TDiagramChartType(cbSeriesStyle.ItemIndex) = dctPeak) then
      TBarSeries(pNeu).BarWidthPercent := 1;
    pAlt.Free;
    FSeriesList.Objects[i] := pNeu;

  end;

  Aktualisieren;
end;

{------------------------------------------------------}
procedure TFormDiagramm.cbSeriesStyleChange(Sender: TObject);
{------------------------------------------------------}
begin
  ChangeSeriesType;
end;

{------------------------------------------------------}
procedure TFormDiagramm.chbSameAxisClick(Sender: TObject);
{------------------------------------------------------}
begin
  Aktualisieren;
end;

{------------------------------------------------------}
procedure TFormDiagramm.EditExit(Sender: TObject);
{------------------------------------------------------}
var
  i : integer;
begin
  if (not (csDestroying in Self.ComponentState)) then begin  // 22.01.2004
    if (rbtnKombiGraphik.Checked) and (cbAktSeries.ItemIndex >= 0)
    then i := FSeriesList.IndexOf(cbAktSeries.Items[cbAktSeries.ItemIndex])
    else i := -1;
    SizeAxis(i);
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.cbAktSeriesChange(Sender: TObject);
{------------------------------------------------------}
var
  pChart  : TChart;
  i, iTag : integer;
begin
  if (rbtnKombiGraphik.Checked)
  then pChart := FChartList.Chart[0]
  else begin
    pChart := nil;
    iTag := FSeriesList.IndexOf(cbAktSeries.Text);
    for i := 0 to FChartList.Count-1 do
      if (FChartList.Chart[i].Tag = iTag) then begin
        pChart := FChartList.Chart[i];
        Break;
      end;
  end;

  if (Assigned(pChart)) then OnClick(pChart);
end;

{------------------------------------------------------}
procedure TFormDiagramm.EnableMyControls(Sender: TWinControl; bState: boolean;
  bHourGlass: boolean = True; bFirstParent: boolean = True);
{------------------------------------------------------}
begin
  GD_Utils.EnableMyControls(Sender, bState, bHourGlass, bFirstParent);
end;

{ Rechnet ggf. Achsenwerte um                          }
{ Parameter: Wert, Achse (1=X, 2=Y)                    }
{------------------------------------------------------}
function TFormDiagramm.CalcAxisValue(fValue: double; iAxis: byte): double;  // 15.07.2002
{------------------------------------------------------}
begin
  Result := fValue;  // Werte werden nur in Vererbungen geändert
end;

{------------------------------------------------------}
procedure TFormDiagramm.FormResize(Sender: TObject);
{------------------------------------------------------}
const
  C_LineHeight = 21;
var
  iLines, iHeight : integer;
begin
  if (not (csDestroying in ComponentState)) then begin
    iLines := chlbSeriesList.Items.Count * 21;
    iHeight := pnRightRest.Height + pnRight2.Height;
    if (iLines < iHeight) then iHeight := iLines;
    pnRight2.Height := iHeight;
  end;
end;

{------------------------------------------------------}
procedure TFormDiagramm.pmiCopyToClipboardClick(Sender: TObject);
{------------------------------------------------------}
begin
  CopyDiagramm(FChartList.GetChartIndex(FActiveChart));
end;

{------------------------------------------------------}
procedure TFormDiagramm.pmiPrintClick(Sender: TObject);
{------------------------------------------------------}
begin
  PrintDiagramm(FChartList.GetChartIndex(FActiveChart));
end;

{------------------------------------------------------}
procedure TFormDiagramm.pmiSaveClick(Sender: TObject);
{------------------------------------------------------}
begin
  if (OpenDialog.Execute) then
    SaveDiagramm(OpenDialog.FileName, FChartList.GetChartIndex(FActiveChart));
end;

{------------------------------------------------------}
procedure TFormDiagramm.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);  // 22.01.2004
{------------------------------------------------------}
begin
  if (Key = VK_RETURN) then EditExit(Sender);
end;

{ Löschen aller Diagramme                              }
{------------------------------------------------------}
procedure TFormDiagramm.ClearChartList;
{------------------------------------------------------}
begin
  FSeriesList.Clear;
  FChartList.Clear;
  chlbSeriesList.Clear;
end;

end.
