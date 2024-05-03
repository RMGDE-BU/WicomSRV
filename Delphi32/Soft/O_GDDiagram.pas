unit O_GDDiagram;

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, Graphics, Types, DateUtils,
  GD_Utils;

type
  TGDDiagPosition = (dpLeft, dpRight, dpTop, dpBottom);
  TGDDiagSizeMode = (smAbs, smRel);

  TGDDiagObject = class;
  TGDDiagram = class;

  TGDDiagValueObject = class(TObject)
    constructor Create(fX, fY: double);
  private
    FXValue  : double;
    FYValue  : double;
  protected
  public
    property XValue: double read FXValue;
    property YValue: double read FYValue;
  end;

  TGDDiagValueList = class(TList)
    constructor Create(pDiagObject: TGDDiagObject); virtual;
  private
    FDiagram : TGDDiagObject;
    FXMin    : double;
    FXMax    : double;
    FYMin    : double;
    FYMax    : double;
  protected
    function GetValues(iIndex: integer): TGDDiagValueObject;
    function GetXValue(iIndex: integer): double;
    function GetYValue(iIndex: integer): double;
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    function AddXYValue(fX, fY: double): TGDDiagValueObject;
    property Values [iIndex: integer]: TGDDiagValueObject read GetValues;
    property XValue [iIndex: integer]: double read GetXValue;
    property YValue [iIndex: integer]: double read GetYValue;
    property MinXValue: double read FXMin;
    property MaxXValue: double read FXMax;
    property MinYValue: double read FYMin;
    property MaxYValue: double read FYMax;
  end;

  TGDDiagAxis = class(TObject)
    constructor Create(pCanvas: TGDDiagram); virtual;
  private
    FCanvas       : TGDDiagram;

    FPosition     : TGDDiagPosition;
    FSizeMode     : TGDDiagSizeMode;
    FXIndent      : byte;
    FYIndent      : byte;
    FAutomatic    : boolean;
    FDateTime     : boolean;
    FTimeInterval : integer;

    FMin          : double;
    FMax          : double;

    function CalcStartEndPoint(var pStart, pEnd: TPoint): boolean;
  protected
  public
    procedure PaintOnCanvas;
    function GetCoordValue(fValue: double): integer;
    property Position: TGDDiagPosition read FPosition write FPosition;
    property SizeMode: TGDDiagSizeMode read FSizeMode write FSizeMode;
    property XIndent: byte read FXIndent write FXIndent;
    property YIndent: byte read FYIndent write FYIndent;
    property Automatic: boolean read FAutomatic write FAutomatic;
    property DateTime: boolean read FDateTime write FDateTime;
    property TimeInterval: integer read FTimeInterval write FTimeInterval;
    property Minimum: double read FMin write FMin;
    property Maximum: double read FMax write FMax;
  end;

  TGDDiagObject = class(TObject)
    constructor Create(pCanvas: TGDDiagram); virtual;
    destructor Destroy; override;
  private
    FTag         : integer;
    FCanvas      : TGDDiagram;
    FColor       : TColor;
    FPaintLine   : boolean;
    FPaintBubble : boolean;
    FLineWidth   : integer;
    FBubbleSize  : integer;
    FTitle       : string;
    FXAxis       : TGDDiagAxis;
    FYAxis       : TGDDiagAxis;
    FValueList   : TGDDiagValueList;
  protected
    function CalcValuePosition(fX, fY: double; var iX, iY: integer): boolean;
    procedure CalcMinMax(pAxis: TGDDiagAxis);
    procedure SetAxisModes;
    procedure PaintValuesOnCanvas;
  public
    procedure PaintOnCanvas;
    procedure InvalidateValueRect(fX1, fY1, fX2, fY2: double);
    property Tag: integer read FTag write FTag;
    property ValueList: TGDDiagValueList read FValueList;
    property Color: TColor read FColor write FColor;
    property PaintLine: boolean read FPaintLine write FPaintLine;
    property PaintBubble: boolean read FPaintBubble write FPaintBubble;
    property LineWidth: integer read FLineWidth write FLineWidth;
    property BubbleSize: integer read FBubbleSize write FBubbleSize;
    property Title: string read FTitle write FTitle;
    property XAxis: TGDDiagAxis read FXAxis;
    property YAxis: TGDDiagAxis read FYAxis;
  end;

  TGDDiagList = class(TList)
    constructor Create(pCanvas: TGDDiagram); virtual;
    destructor Destroy; override;
  private
    FCanvas : TGDDiagram;
  protected
    function GetDiagram(iIndex: integer): TGDDiagObject;
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    function AddDiagram: TGDDiagObject;
    function GetDiagramByTag(iTag: integer): TGDDiagObject;
    procedure PaintOnCanvas;
    procedure PaintAxisScale(
      iIndex, iHeight, iWidth: integer; var iLeft, iRight: integer);
    property Diagram [iIndex: integer]: TGDDiagObject read GetDiagram;
  end;

  TGDDiagram = class(TCustomControl)
    constructor Create(pOwner: TComponent); override;
    destructor Destroy; override;
  private
    FDiagramList     : TGDDiagList;
    FBackgroundColor : TColor;
    procedure PaintBackground;
    { Private-Deklarationen }
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
    procedure Paint; override;
  published
    { Published-Deklarationen }
    property DiagramList : TGDDiagList read FDiagramList;
  end;

procedure Register;

implementation

const
  TGDDiagColor : array [0..4] of TColor =
    (clBlue, clRed, clGreen, clFuchsia, clMaroon);

//----------------------------- TGDDiagValueObject -----------------------------

//-----------------------------------------------------
constructor TGDDiagValueObject.Create(fX, fY: double);
//-----------------------------------------------------
begin
  inherited Create;

  FXValue := fX;
  FYValue := fY;
end;

//------------------------------- TGDDiagValueList -----------------------------

//-----------------------------------------------------
constructor TGDDiagValueList.Create(pDiagObject: TGDDiagObject);
//-----------------------------------------------------
begin
  inherited Create;

  FDiagram := pDiagObject;
  FXMin := 0; FXMax := 0; FYMin := 0; FYMax := 0;
end;

//-----------------------------------------------------
procedure TGDDiagValueList.Clear;
//-----------------------------------------------------
var
  i : integer;
begin
  for i := 0 to Count-1 do
  try
    TObject(Items[i]).Free;
  except
  // tue nix
  end;

  FXMin := 0; FXMax := 0; FYMin := 0; FYMax := 0;

  inherited;
end;

//-----------------------------------------------------
procedure TGDDiagValueList.Delete(iIndex: integer); 
//-----------------------------------------------------
begin
  try
    TObject(Items[iIndex]).Free;
  except
  // tue nix
  end;

  inherited;
end;

//-----------------------------------------------------
function TGDDiagValueList.GetValues(iIndex: integer): TGDDiagValueObject;
//-----------------------------------------------------
begin
  try
    if (TObject(Items[iIndex]) is TGDDiagValueObject)
    then Result := TGDDiagValueObject(Items[iIndex])
    else Result := nil;
  except
    Result := nil;
  end;
end;

//-----------------------------------------------------
function TGDDiagValueList.GetXValue(iIndex: integer): double;
//-----------------------------------------------------
begin
  try
    Result := TGDDiagValueObject(Items[iIndex]).XValue;
  except
    Result := 0;
  end;
end;

//-----------------------------------------------------
function TGDDiagValueList.GetYValue(iIndex: integer): double;
//-----------------------------------------------------
begin
  try
    Result := TGDDiagValueObject(Items[iIndex]).YValue;
  except
    Result := 0;
  end;
end;

//-----------------------------------------------------
function TGDDiagValueList.AddXYValue(fX, fY: double): TGDDiagValueObject;
//-----------------------------------------------------
var
  iIx : integer;
begin
  if (Count = 0) then begin
    FXMin := fX; FXMax := fX;
    FYMin := fY; FYMax := fY; 
  end
  else begin
    if (FXMin > fX) then FXMin := fX;
    if (FXMax < fX) then FXMax := fX;
    if (FYMin > fY) then FYMin := fY;
    if (FYMax < fY) then FYMax := fY;
  end;

  iIx := Add(TGDDiagValueObject.Create(fX, fY));
  Result := TGDDiagValueObject(Items[iIx]);

  if (Count = 1)
  then FDiagram.InvalidateValueRect(Result.XValue-1, Result.YValue-1,
    Result.XValue+1, Result.YValue+1)
  else FDiagram.InvalidateValueRect(TGDDiagValueObject(Items[iIx-1]).XValue,
    TGDDiagValueObject(Items[iIx-1]).YValue, Result.XValue, Result.YValue);
end;

//--------------------------------- TGDDiagAxis --------------------------------

//-----------------------------------------------------
constructor TGDDiagAxis.Create(pCanvas: TGDDiagram);
//-----------------------------------------------------
begin
  inherited Create;

  FCanvas := pCanvas;

  FPosition    := dpBottom;
  FSizeMode    := smAbs;
  FXIndent     := 30;
  FYIndent     := 30;
  FAutomatic   := True;
  FDateTime    := False;
  FTimeInterval := 0;

  FMin         := 0;
  FMax         := 10;
end;

//-----------------------------------------------------
function TGDDiagAxis.CalcStartEndPoint(var pStart, pEnd: TPoint): boolean;
//-----------------------------------------------------
var
  iSX, iSY, iEX, iEY, iH, iW : integer;
begin
  try
    Result := True;

    iH := FCanvas.Height;
    iW := FCanvas.Width;
    iSX := 0; iSY := 0; iEX := 0; iEY := 0;

    if (FPosition = dpLeft) then begin
      iSX := FXIndent;
      iSY := FYIndent;
      iEX := FXIndent;
      iEY := iH - FYIndent;
    end
    else if (FPosition = dpRight) then begin
      iSX := iW - FXIndent;
      iSY := FYIndent;
      iEX := iW - FXIndent;
      iEY := iH - FYIndent;
    end
    else if (FPosition = dpTop) then begin
      iSX := FXIndent;
      iSY := FYIndent;
      iEX := iW - FXIndent;
      iEY := FYIndent;
    end
    else if (FPosition = dpBottom) then begin
      iSX := FXIndent;
      iSY := iH - FYIndent;
      iEX := iW - FXIndent;
      iEY := iH - FYIndent;
    end
    else Result := False;

    if (FSizeMode = smAbs) then begin
    end
    else if (FSizeMode = smRel) then begin
    end
    else Result := False;

    if (Result) then begin
      pStart := Point(iSX, iSY);
      pEnd := Point(iEX, iEY);
    end;
  except
    Result := False;
  end;
end;

//-----------------------------------------------------
function TGDDiagAxis.GetCoordValue(fValue: double): integer;
//-----------------------------------------------------
var
  iH, iW, iStart, iEnd : integer;
begin
  try
    iH := FCanvas.Height;
    iW := FCanvas.Width;
    if (FPosition = dpLeft) then begin
      iStart := FYIndent;
      iEnd := iH - FYIndent;
    end
    else if (FPosition = dpRight) then begin
      iStart := FYIndent;
      iEnd := iH - FYIndent;
    end
    else if (FPosition = dpTop) then begin
      iStart := FXIndent;
      iEnd := iW - FXIndent;
    end
    else if (FPosition = dpBottom) then begin
      iStart := FXIndent;
      iEnd := iW - FXIndent;
    end
    else begin
      iStart := 0;
      iEnd := 0;
    end;

    Result :=
      iStart + Round((iEnd - iStart) * ((fValue - FMin)/(FMax - FMin)));
    if (FPosition in [dpLeft, dpRight]) then Result := iH - Result;
  except
    Result := -1;
  end;
end;

//-----------------------------------------------------
procedure TGDDiagAxis.PaintOnCanvas;
//-----------------------------------------------------
var
  pPosStart, pPosEnd : TPoint;
begin
  try
    if (CalcStartEndPoint(pPosStart, pPosEnd)) then begin
      FCanvas.Canvas.PenPos := pPosStart;
      FCanvas.Canvas.LineTo(pPosEnd.X, pPosEnd.Y);
    end;
  except
  // tue nix
  end;
end;

//--------------------------------- TGDDiagObject --------------------------------

//-----------------------------------------------------
constructor TGDDiagObject.Create(pCanvas: TGDDiagram);
//-----------------------------------------------------
begin
  inherited Create;

  FTag := 0;
  FCanvas := pCanvas;
  FColor := clBlue;
  FPaintLine := True;
  FPaintBubble := False;
  FLineWidth := pCanvas.Canvas.Pen.Width;
  FBubbleSize := ((FLineWidth + 1) div 2) + 1;
  FValueList := TGDDiagValueList.Create(Self);

  FXAxis := TGDDiagAxis.Create(pCanvas);
  FXAxis.Position := dpBottom;
  FXAxis.DateTime := True;
  FXAxis.TimeInterval := 0;
  FYAxis := TGDDiagAxis.Create(pCanvas);
  FYAxis.Position := dpLeft;
  FYAxis.DateTime := False;
end;

//-----------------------------------------------------
destructor TGDDiagObject.Destroy;
//-----------------------------------------------------
begin
  FCanvas := nil;
  FreeAndNil(FXAxis);
  FreeAndNil(FYAxis);

  inherited;
end;

//-----------------------------------------------------
procedure TGDDiagObject.InvalidateValueRect(fX1, fY1, fX2, fY2: double);
//-----------------------------------------------------
var
  iX1, iX2, iY1, iY2 : integer;
  pUpdateRgn         : HRgn;
  pPointArray        : array[0..3] of TPoint;
begin
  if (CalcValuePosition(fX1, fY1, iX1, iY1)) and
    (CalcValuePosition(fX2, fY2, iX2, iY2)) then
  begin
    if (iX1 <= iX2) then begin
      pPointArray[0] := Point(iX1-10, iY1+10);
      pPointArray[0] := Point(iX2+10, iY2+10);
      pPointArray[0] := Point(iX2+10, iY2-10);
      pPointArray[0] := Point(iX1-10, iY1-10);
    end
    else begin
      pPointArray[0] := Point(iX2-10, iY2+10);
      pPointArray[0] := Point(iX1+10, iY1+10);
      pPointArray[0] := Point(iX1+10, iY1-10);
      pPointArray[0] := Point(iX2-10, iY2-10);
    end;

    pUpdateRgn := CreatePolygonRgn(pPointArray, 4, Alternate);
    try
      SelectClipRgn(FCanvas.Canvas.Handle, pUpdateRgn);
      FCanvas.Invalidate;
    finally
      SelectClipRgn(FCanvas.Canvas.Handle, 0);
      DeleteObject(pUpdateRgn);
    end;
  end;
end;

//-----------------------------------------------------
function TGDDiagObject.CalcValuePosition(
  fX, fY: double; var iX, iY: integer): boolean;
//-----------------------------------------------------
begin
  try
    // Achsenabschnitte berechnen
    iX := FXAxis.GetCoordValue(fX);
    iY := FYAxis.GetCoordValue(fY);
    Result := True;
  except
    Result := False;
  end;
end;

// Anzeigegrenzen ermitteln
//-------------------------------------------------------
procedure TGDDiagObject.CalcMinMax(pAxis: TGDDiagAxis);
//-------------------------------------------------------
var
  fLow, fHigh : double;
  h, m, s, ms : word;
begin
  try
    if (ValueList.Count > 1) then begin
      if (pAxis.DateTime) and (pAxis.TimeInterval > 0) then begin
        if ((pAxis.Maximum - pAxis.Minimum) < pAxis.TimeInterval/(24*60)) then
        begin
          DecodeTime(pAxis.Minimum, h, m, s, ms);
          pAxis.Maximum := IncMinute(Trunc(pAxis.Minimum) +
            EncodeTime(h, m, 0, 0), pAxis.TimeInterval);
        end
        else begin
          DecodeTime(pAxis.Maximum, h, m, s, ms);
          pAxis.Minimum := IncMinute(Trunc(pAxis.Maximum) +
            EncodeTime(h, m, 0, 0), -(pAxis.TimeInterval-1));
          pAxis.Maximum := IncMinute(Trunc(pAxis.Maximum) +
            EncodeTime(h, m, 0, 0), 1);
        end;
      end
      else begin
        // Y-Skalierung berechnen
        WRangeSet(pAxis.Minimum, pAxis.Maximum, fLow, fHigh);
        // Y-Skalierung setzen
        pAxis.Minimum := fLow;
        pAxis.Maximum := fHigh;
      end;
    end;
  except
  // tue nix
  end;
end;

//-----------------------------------------------------
procedure TGDDiagObject.SetAxisModes;
//-----------------------------------------------------
begin
  if (FXAxis.Automatic) then begin
    // Einstellungen für X-Achse
    FXAxis.Minimum := ValueList.MinXValue;
    FXAxis.Maximum := ValueList.MaxXValue;
    CalcMinMax(FXAxis);
    if (FXAxis.Minimum = FXAxis.Maximum) then begin
      FXAxis.Minimum := ValueList.MinXValue - 1;
      FXAxis.Maximum := ValueList.MaxXValue + 1;
    end;
  end;
  if (FYAxis.Automatic) then begin
    // Einstellungen für Y-Achse
    FYAxis.Minimum := ValueList.MinYValue;
    FYAxis.Maximum := ValueList.MaxYValue;
    CalcMinMax(FYAxis);
    if (FYAxis.Minimum = FYAxis.Maximum) then begin
      FYAxis.Minimum := ValueList.MinYValue - 1;
      FYAxis.Maximum := ValueList.MaxYValue + 1;
    end;
  end;
end;

//-----------------------------------------------------
procedure TGDDiagObject.PaintValuesOnCanvas;
//-----------------------------------------------------
var
  i, iX, iY        : integer;
  pPColor, pBColor : TColor;
  pUpdateRegion    : HRGN;
begin
  try
    pPColor := FCanvas.Canvas.Pen.Color;
    pBColor := FCanvas.Canvas.Brush.Color;
    try
      pUpdateRegion := CreateRectRgn(XAxis.XIndent-1, YAxis.YIndent-1,
        FCanvas.Width - XAxis.XIndent + 1, FCanvas.Height - YAxis.YIndent + 1);
      try
        SelectClipRgn(FCanvas.Canvas.Handle, pUpdateRegion);
        FCanvas.Canvas.Pen.Color := FColor;
        FCanvas.Canvas.Brush.Color := FColor;
        for i := 0 to FValueList.Count-1 do begin
          if (CalcValuePosition(FValueList.GetXValue(i),
            FValueList.GetYValue(i), iX, iY)) then
          begin
            if (FPaintBubble) then FCanvas.Canvas.Ellipse(Rect(
              iX-FBubbleSize, iY-FBubbleSize, iX+FBubbleSize, iY+FBubbleSize));
            if (i = 0)
            then FCanvas.Canvas.PenPos := Point(iX, iY)
            else FCanvas.Canvas.LineTo(iX, iY);
          end;
        end;
      finally
        SelectClipRgn(FCanvas.Canvas.Handle, 0);
        DeleteObject(pUpdateRegion);
      end;
    finally
      FCanvas.Canvas.Pen.Color := pPColor;
      FCanvas.Canvas.Brush.Color := pBColor;
    end;
  except
  // tue nix
  end;
end;

//-----------------------------------------------------
procedure TGDDiagObject.PaintOnCanvas;
//-----------------------------------------------------
begin
  try
    if (FCanvas.Width > 2*FXAxis.FXIndent) and
      (FCanvas.Height > 2*FYAxis.FYIndent) then
    begin
      // Achsen zeichnen
      SetAxisModes;
      FXAxis.PaintOnCanvas;
      FYAxis.PaintOnCanvas;
      // Werte zeichnen
      PaintValuesOnCanvas;
    end;
  except
  // tue nix
  end;
end;

//--------------------------------- TGDDiagList --------------------------------

//-----------------------------------------------------
constructor TGDDiagList.Create(pCanvas: TGDDiagram);
//-----------------------------------------------------
begin
  inherited Create;

  FCanvas := pCanvas;
end;

//-----------------------------------------------------
destructor TGDDiagList.Destroy;
//-----------------------------------------------------
begin
  inherited;
end;

//-----------------------------------------------------
procedure TGDDiagList.Clear;
//-----------------------------------------------------
var
  i : integer;
begin
  for i := 0 to Count-1 do
  try
    TObject(Items[i]).Free;
  except
  // tue nix
  end;

  inherited;
end;

//-----------------------------------------------------
procedure TGDDiagList.Delete(iIndex: integer);
//-----------------------------------------------------
begin
  try
    TObject(Items[iIndex]).Free;
  except
  // tue nix
  end;

  inherited;
end;

//-----------------------------------------------------
function TGDDiagList.GetDiagram(iIndex: integer): TGDDiagObject;
//-----------------------------------------------------
begin
  try
    if (TObject(Items[iIndex]) is TGDDiagObject)
    then Result := TGDDiagObject(Items[iIndex])
    else Result := nil;
  except
    Result := nil;
  end;
end;

//-----------------------------------------------------
function TGDDiagList.GetDiagramByTag(iTag: integer): TGDDiagObject;
//-----------------------------------------------------
var
 i : integer;
begin
  try
    Result := nil;
    for i := 0 to Count-1 do begin
      if (TGDDiagObject(Items[i]).Tag = iTag) then begin
        Result := TGDDiagObject(Items[i]);
        Break;
      end;
    end;
  except
    Result := nil;
  end;
end;

//-----------------------------------------------------
function TGDDiagList.AddDiagram: TGDDiagObject;
//-----------------------------------------------------
var
  iIx : integer;
begin
  try
    iIx := Add(TGDDiagObject.Create(FCanvas));
    Result := GetDiagram(iIx);
    Result.Color := TGDDiagColor[iIx mod (High(TGDDiagColor)+1)];
  except
    Result := nil;
  end;
end;

//-----------------------------------------------------
procedure TGDDiagList.PaintAxisScale(
  iIndex, iHeight, iWidth: integer; var iLeft, iRight: integer);
//-----------------------------------------------------
var
  iL, iT, iTH, iTW : integer;
  sXMin, sXMax, sYMin, sYMax  : string;
  bIgnoreXAxis     : boolean;
begin
  with GetDiagram(iIndex) do
  try
    bIgnoreXAxis := (iIndex > 0) and (iLeft < 0) and (iRight < 0);
    if (iIndex = 0) then begin
      iLeft := XAxis.XIndent;
      iRight := iWidth - XAxis.XIndent + 4;
    end;
    FCanvas.Canvas.Font.Color := Color;

    if (XAxis.DateTime) then begin
      sXMin := DateTimeToStr(XAxis.Minimum);
      sXMax := DateTimeToStr(XAxis.Maximum);
    end
    else begin
      sXMin := FormatFloat('#,##0.####', XAxis.Minimum);
      sXMax := FormatFloat('#,##0.####', XAxis.Maximum);
    end;
    if (YAxis.DateTime) then begin
      sYMin := DateTimeToStr(YAxis.Minimum);
      sYMax := DateTimeToStr(YAxis.Maximum);
    end
    else begin
      sYMin := FormatFloat('#,##0.####', YAxis.Minimum);
      sYMax := FormatFloat('#,##0.####', YAxis.Maximum);
    end;
    iTH := FCanvas.Canvas.TextHeight('X');

    iTW := FCanvas.Canvas.TextWidth(sYMax);
    if (YAxis.Position = dpLeft) then begin
      iL := YAxis.XIndent - (iTW + 4);
      iT := YAxis.YIndent + (iIndex * (iTH + 0));
      FCanvas.Canvas.TextOut(iL, iT, sYMax);

      iTW := FCanvas.Canvas.TextWidth(sYMin);
      iL := YAxis.XIndent - (iTW + 4);
      iT := iHeight - YAxis.YIndent - ((iIndex + 1) * (iTH + 0));
      FCanvas.Canvas.TextOut(iL, iT, sYMin);
    end
    else if (YAxis.Position = dpRight) then begin
      iL := (iWidth - YAxis.XIndent) + 4;
      iT := YAxis.YIndent + (iIndex * (iTH + 0));
      FCanvas.Canvas.TextOut(iL, iT, sYMax);

      iL := (iWidth - YAxis.XIndent) + 4;
      iT := iHeight - YAxis.YIndent - ((iIndex + 1) * (iTH + 0));
      FCanvas.Canvas.TextOut(iL, iT, sYMin);
    end;

    if (not bIgnoreXAxis) then begin
      if (XAxis.Position = dpBottom) then begin
        iTW := FCanvas.Canvas.TextWidth(sXMin);
        iL := iLeft + 4;
        iT := iHeight - XAxis.YIndent + 4;
        FCanvas.Canvas.TextOut(iL, iT, sXMin);
        iLeft := iL + iTW;

        iTW := FCanvas.Canvas.TextWidth(sXMax);
        iL := iRight - 4 - iTW;
        iT := iHeight - XAxis.YIndent + 4;
        FCanvas.Canvas.TextOut(iL, iT, sXMax);
        iRight := iL;
      end
      else if (XAxis.Position = dpTop) then begin
        iTW := FCanvas.Canvas.TextWidth(sXMin);
        iL := iLeft + 4;
        iT := XAxis.YIndent - (iTH + 4);
        FCanvas.Canvas.TextOut(iL, iT, sXMin);
        iLeft := iL + iTW;

        iTW := FCanvas.Canvas.TextWidth(sXMax);
        iL := iRight - 4 - iTW;
        iT := XAxis.YIndent - (iTH + 4);
        FCanvas.Canvas.TextOut(iL, iT, sXMax);
        iRight := iL;
      end;
    end;
  except
  // tue nix
  end;
end;

//-----------------------------------------------------
procedure TGDDiagList.PaintOnCanvas;
//-----------------------------------------------------
var
  i, iH, iW, iL : integer;
  iLeft, iRight : integer;
  s             : string;
begin
  iH := FCanvas.Height;
  iW := FCanvas.Width;
  iL := 4;

  for i := 0 to Count-1 do
  try
    GetDiagram(i).PaintOnCanvas;
    // Zeiten immer überschreiben
    iLeft := -1;
    iRight := -1;
    PaintAxisScale(i, iH, iW, iLeft, iRight);
    s := GetDiagram(i).Title;
    if (s <> '') then begin
      FCanvas.Canvas.TextOut(iL, 4, s);
      iL := iL + FCanvas.Canvas.TextWidth(s) + 4;
    end;
  except
  // tue nix
  end;
end;

//--------------------------------- TGDDiagram ---------------------------------

//-----------------------------------------------------
constructor TGDDiagram.Create(pOwner: TComponent);
//-----------------------------------------------------
begin
  inherited;

  DoubleBuffered := True;
  FBackgroundColor := clWhite;
  FDiagramList := TGDDiagList.Create(Self);
end;

//-----------------------------------------------------
destructor TGDDiagram.Destroy;
//-----------------------------------------------------
begin
  FreeAndNil(FDiagramList);

  inherited;
end;

//-----------------------------------------------------
procedure TGDDiagram.Paint;
//-----------------------------------------------------
begin
  inherited;

  Self.Canvas.Lock;
  try
    PaintBackground;
    FDiagramList.PaintOnCanvas;
  finally
    Self.Canvas.Unlock;
  end;
end;

//-----------------------------------------------------
procedure TGDDiagram.PaintBackground;
//-----------------------------------------------------
begin
  Self.Canvas.Brush.Color := FBackgroundColor;
  Self.Canvas.FillRect(Self.ClientRect);
end;

//------------------------ Allgemeine Funktionen -------------------------------

//-----------------------------------------------------
procedure Register;
//-----------------------------------------------------
begin
  RegisterComponents('Beispiele', [TGDDiagram]);
end;

end.
