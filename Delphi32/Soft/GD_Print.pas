unit GD_Print;

interface

uses
  Windows, SysUtils, Types, Graphics, Printers, Classes,
  O_GDObject;

type
  TAnchorPosition = (apLeftTop, apRightTop, apLeftBottom, apRightBottom);

  TGDPrintCustom = class(TObject)
    constructor Create; virtual;
  private
    FTag       : integer;
    FName      : string;
    FAnchorX   : double;
    FAnchorY   : double;
    FAnchorPos : TAnchorPosition;
  protected
  public
    property Name: string read FName write FName;
    property Tag: integer read FTag write FTag;
    property AnchorX: double read FAnchorX write FAnchorX;
    property AnchorY: double read FAnchorY write FAnchorY;
    property AnchorPos: TAnchorPosition read FAnchorPos write FAnchorPos;
  end;

  TGDPrintLabel = class(TGDPrintCustom)
    constructor Create; override;
  private
    FCaption : string;
    FFont    : TFont;
  public
    property Caption: string read FCaption write FCaption;
    property Font: TFont read FFont;
  end;

  TGDPrintObject = class(TGDObject)
  private
    FBorderRect : TRect;
    FLineHeight : double;
    FFont       : TFont;
    FPixPerCmX  : integer;
    FPixPerCmY  : integer;
    FPoList     : TList;
    FActTop     : double;
    FActPage    : integer;
    FCanvas     : TCanvas;
    FPaintText  : boolean;
  protected
    procedure MyInitializeObject(bState: boolean); override;
    procedure InitPrintObjects(bState: boolean); virtual;
    procedure ResizePageFrame;
    function CmToPixX(fCm: double): integer;
    function CmToPixY(fCm: double): integer;
    function PixToCmToPixX(iPix: integer): double;
    function PixToCmToPixY(iPix: integer): double;
    function PrintPoListObjects(iOffsHorz, iOffsVert, iTag : integer): boolean;
    function PrintContent: boolean; virtual;
    procedure ForceNewPage; virtual;

    property BorderRect: TRect read FBorderRect;
    property PixPerCmX: integer read FPixPerCmX;
    property PixPerCmY: integer read FPixPerCmY;
    property PoList: TList read FPoList;
    property ActTop: double read FActTop write FActTop;
    property ActPage: integer read FActPage write FActPage;
    property LineHeight: double read FLineHeight write FLineHeight;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property PaintText: boolean read FPaintText write FPaintText;
  public
    function Print: boolean; virtual;
    function Preview(pCanvas: TCanvas): boolean; virtual;
  end;

implementation

//------------------------------ TGDPrintCustom --------------------------------

// Object initialisieren/freigeben
//-----------------------------------------
constructor TGDPrintCustom.Create;
//-----------------------------------------
begin
  inherited Create;

  FName := '';
  FTag := 0;
  FAnchorX := 0;
  FAnchorY := 0;
  FAnchorPos := apLeftTop;
end;

//------------------------------ TGDPrintLabel --------------------------------

// Object initialisieren/freigeben
//-----------------------------------------
constructor TGDPrintLabel.Create;
//-----------------------------------------
begin
  inherited;

  FCaption := '';
  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FFont.Size := 10;
end;

//------------------------------ TGDPrintObject --------------------------------

// Object initialisieren/freigeben
//-----------------------------------------
procedure TGDPrintObject.MyInitializeObject(bState: boolean);
//-----------------------------------------
begin
  inherited;

  try
    if (bState) then begin
      FCanvas := nil;

      FFont := TFont.Create;
      FFont.Name := 'Arial';
      FFont.Size := 10;

      FActTop := 0;
      FActPage := 1;
      FLineHeight := 0;
      FPaintText := True;

      ResizePageFrame;
      InitPrintObjects(True);
    end
    else begin
      InitPrintObjects(False);
      FreeAndNil(FFont);
    end;
  except
    on E:Exception do begin
      if (not (E is EAbort)) then
        HandleError('TGDPrintObject.MyInitializeObject: ' + E.Message);
    end;
  end;
end;

// Print-Objecte initialisieren/freigeben
//-----------------------------------------
procedure TGDPrintObject.InitPrintObjects(bState: boolean);
//-----------------------------------------
var
  i : integer;
begin
  try
    if (bState) then begin
      FPoList := TList.Create;
    end
    else begin
      for i := 0 to FPoList.Count-1 do
      try
        TObject(FPoList[i]).Free;
      except
        // tue nix
      end;
      FreeAndNil(FPoList);
    end;
  except
    on E:Exception do begin
      if (not (E is EAbort)) then
        HandleError('TGDPrintObject.InitPrintObjects: ' + E.Message);
    end;
  end;
end;

// Seitenbezogene Druckparameter festlegen
//-----------------------------------------
procedure TGDPrintObject.ResizePageFrame;
//-----------------------------------------
var
  fW, fH                       : double;
  iLeft, iRight, iTop, iBottom : integer;
begin
  if (Assigned(FCanvas)) then begin
    FPixPerCmX := Round(GetDeviceCaps(FCanvas.Handle, LOGPIXELSX) / 2.54);
    FPixPerCmY := Round(GetDeviceCaps(FCanvas.Handle, LOGPIXELSY) / 2.54);

    // Ausgehend von DIN A4
    iLeft := CmToPixX(1);
    iRight := CmToPixX(20);
    iTop := CmToPixY(1);
    iBottom := CmToPixY(28.7);
  end
  else begin
    with Printer do begin
      if (Printing) then begin
        fW := GetDeviceCaps(Printer.Canvas.Handle, HORZSIZE) / 10;
        fH := GetDeviceCaps(Printer.Canvas.Handle, VERTSIZE) / 10;
      end
      else begin
        fW := GetDeviceCaps(Printer.Handle, HORZSIZE) / 10;
        fH := GetDeviceCaps(Printer.Handle, VERTSIZE) / 10;
      end;
      FPixPerCmX := Round(PageWidth / fW);
      FPixPerCmY := Round(PageHeight / fH);

      // Ausgehend von DIN A4
      iLeft := CmToPixX(1) - CmToPixX((21-fW) / 2);
      iRight := PageWidth - iLeft;
      iTop := CmToPixY(1) - CmToPixY((29.7-fH) / 2);
      iBottom := PageHeight - iTop;
    end;
  end;

  FBorderRect := Rect(iLeft, iTop, iRight, iBottom);
end;

// Neue Seite
//-----------------------------------------
procedure TGDPrintObject.ForceNewPage;
//-----------------------------------------
begin
  if (Assigned(Canvas)) then begin
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(BorderRect);
  end
  else Printer.NewPage;
  FActTop := 0;
  inc (FActPage);
end;

// cm-Angabe in Pixel umrechnen (X)
//-----------------------------------------
function TGDPrintObject.CmToPixX(fCm: double): integer;
//-----------------------------------------
begin
  Result := Round(fcm * FPixPerCmX);
end;

// cm-Angabe in Pixel umrechnen (Y)
//-----------------------------------------
function TGDPrintObject.CmToPixY(fCm: double): integer;
//-----------------------------------------
begin
  Result := Round(fcm * FPixPerCmY);
end;

// Pixel-Angabe in cm umrechnen (X)
//-----------------------------------------
function TGDPrintObject.PixToCmToPixX(iPix: integer): double;
//-----------------------------------------
begin
  Result := iPix / FPixPerCmX;
end;

// Pixel-Angabe in cm umrechnen (Y)
//-----------------------------------------
function TGDPrintObject.PixToCmToPixY(iPix: integer): double;
//-----------------------------------------
begin
  Result := iPix / FPixPerCmY;
end;

// Drucken
//-----------------------------------------
function TGDPrintObject.Print: boolean;
//-----------------------------------------
begin
  try
    FCanvas := nil;
    ResizePageFrame;
    Result := PrintContent;
  except
    on E:Exception do begin
      Result := False;
      if (not (E is EAbort)) then
        HandleError('TGDPrintObject.Print: ' + E.Message);
    end;
  end;
end;

// Vorschau
//-----------------------------------------
function TGDPrintObject.Preview(pCanvas: TCanvas): boolean;
//-----------------------------------------
begin
  try
    FCanvas := pCanvas;
    ResizePageFrame;
    Result := PrintContent;
  except
    on E:Exception do begin
      Result := False;
      if (not (E is EAbort)) then
        HandleError('TGDPrintObject.Preview: ' + E.Message);
    end;
  end;
end;

// Inhalt Drucken
//-----------------------------------------
function TGDPrintObject.PrintContent: boolean;
//-----------------------------------------
begin
  FActTop := 0;
  FActPage := 1;
  Result := True;
end;

// Drucken von Objekten
//-----------------------------------------
function TGDPrintObject.PrintPoListObjects(
  iOffsHorz, iOffsVert, iTag : integer): boolean;
//-----------------------------------------
var
  i : integer;
begin
  if (FPaintText) then
  try
    for i := 0 to FPoList.Count-1 do
      if (iTag < 0) or (iTag = TGDPrintCustom(FPoList[i]).Tag) then begin
        if (Assigned(FCanvas)) then begin
          with TGDPrintLabel(FPoList[i]) do begin
            FCanvas.Brush.Color := clWhite;
            FCanvas.Font.Assign(Font);
            FCanvas.TextOut(iOffsHorz + CmToPixX(AnchorX),
              iOffsVert + CmToPixY(AnchorY), Caption);
          end;
        end
        else begin
          with TGDPrintLabel(FPoList[i]), Printer do begin
            if (Printing) then begin
              Canvas.Brush.Color := clWhite;
              Canvas.Font.Assign(Font);
              Canvas.TextOut(iOffsHorz + CmToPixX(AnchorX),
                iOffsVert + CmToPixY(AnchorY), Caption);
            end;
          end;
        end;
      end;
    Result := True;
  except
    on E:Exception do begin
      Result := False;
      if (not (E is EAbort)) then
        HandleError('TGDPrintObject.PrintPoListObjects: ' + E.Message);
    end;
  end
  else Result := True;
end;

end.
