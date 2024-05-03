{------------------------------------------------------------------------------}
{ Verschiebbare und größenänderbare graphische Objekte für Darstellungszwecke  }
{                                                                              }
{ 20.07.2000  GD    Neu                                                        }
{ 04.05.2002  GD    Abspaltung von TMyCustomControl                            }
{ 26.10.2007  WW    resourcestrings                                            }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, 2002                                    }
{------------------------------------------------------------------------------}
unit MyShapes;

interface

uses
  Messages, Windows, Classes, Controls, Forms, Graphics, ExtCtrls, GD_Utils,
  ClipBrd, Dialogs;

type
  TMyCustomControl = class;
  TMyCustomShape = class;

  TMyCustomShapeType = (mcstMoveable, mcstSizeable);
  TMyCustomShapeTypeSet = set of TMyCustomShapeType;

  TMyCustomControl = class(TCustomControl)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure ShapeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); virtual;
    procedure ShapeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
  private
    { Private-Deklarationen }
    i_ActX, i_ActY : integer;
    FMoveLock      : integer;
    FOptions       : TMyCustomShapeTypeSet;
    procedure SetOptions(Value: TMyCustomShapeTypeSet);
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
  protected
    { Protected-Deklarationen }
    procedure Paint; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure DoOnClick(Sender: TObject); virtual;
    procedure DoOnDoubleClick(Sender: TObject); virtual;
    function GetColor: TColor; virtual;
    procedure SetColor(Value: TColor); virtual;
    property Moving: integer read FMoveLock;
    property Options: TMyCustomShapeTypeSet read FOptions write SetOptions
      default [mcstMoveable, mcstSizeable];
  public
    { Public-Deklarationen }
    isStayOnTop   : boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Release;
    procedure CopyToClipBoard;
  published
    { Published-Deklarationen }
    procedure StyleChanged(Sender: TObject);
    property Color: TColor read GetColor write SetColor;
    property OnClick;
    property OnDblClick;
  end;

  TMyCustomShape = class(TMyCustomControl)
    constructor Create(anOwner: TComponent); override;
  private
    { Private-Deklarationen }
    FShape         : TShapeType;
    procedure SetShape(Value: TShapeType);
  protected
    { Protected-Deklarationen }
    procedure Paint; override;
  public
    { Public-Deklarationen }
  published
    { Published-Deklarationen }
    property Shape: TShapeType read FShape write SetShape default stRectangle;
  end;

implementation

resourcestring
  S_ClipboardImage = 'Bild für die Zwischenablage';


{------------------------------ TMyCustomControl ------------------------------}

{----------------------------------------------------}
constructor TMyCustomControl.Create(anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited Create(anOwner);

  Options := [mcstMoveable, mcstSizeable];

  Self.Cursor := crArrow;

  Height := 100;
  Width := 100;
  FMoveLock := 0;

  Self.OnDblClick := DoOnDoubleClick;
  Self.OnMouseDown := ShapeMouseDown;
  Self.OnMouseMove := ShapeMouseMove;
  Self.OnMouseUp := ShapeMouseUp;
  Self.OnClick := DoOnClick;
  isStayOnTop := True;
end;

{----------------------------------------------------}
destructor TMyCustomControl.Destroy;
{----------------------------------------------------}
begin
  inherited Destroy;
end;

{----------------------------------------------------}
procedure TMyCustomControl.Release;
{----------------------------------------------------}
begin
  PostMessage(Handle, CM_RELEASE, 0, 0);
end;

{----------------------------------------------------}
procedure TMyCustomControl.CMRelease;
{----------------------------------------------------}
begin
  Free;
end;

{----------------------------------------------------}
procedure TMyCustomControl.StyleChanged(Sender: TObject);
{----------------------------------------------------}
begin
  Invalidate;
end;

{----------------------------------------------------}
procedure TMyCustomControl.DoOnDoubleClick(Sender: TObject);
{----------------------------------------------------}
begin
  inherited;
end;

{------------------------------------------------}
procedure TMyCustomControl.ShapeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{------------------------------------------------}
begin
  if (isStayOnTop) then Self.BringToFront;
  if (mcstMoveable in FOptions) or (mcstSizeable in FOptions) then
    if (Button = mbLeft) then begin
      FMoveLock := 0;
      i_ActX := X;
      i_ActY := Y;
    end;
end;

{------------------------------------------------}
procedure TMyCustomControl.ShapeMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
{------------------------------------------------}
var
  sPoint : TPoint;
begin
  if (mcstMoveable in FOptions) or (mcstSizeable in FOptions) then begin
    if (FMoveLock  <= 0) then begin
      inc(FMoveLock);

      if (ssLeft in Shift) then begin
        if (Self.Cursor <> crArrow) and (mcstSizeable in FOptions) then begin
        // Größenänderung
          if  (Self.Cursor <> crSizeNS) then begin
            if (i_ActX <= 4) and ((Self.Width + i_ActX - X) > 20) then begin
              Self.Width := Self.Width + i_ActX - X;
              Self.Left := Self.Left - i_ActX + X;
            end
            else if ((Self.Width - i_ActX + X) > 20) then begin
              Self.Width := Self.Width - i_ActX + X;
              i_ActX := X;
            end;
          end;
          if (Self.Cursor <> crSizeWE) then begin
            if (i_ActY <= 4) and ((Self.Height + i_ActY - Y) > 20) then begin
              Self.Height := Self.Height + i_ActY - Y;
              Self.Top := Self.Top - i_ActY + Y;
            end
            else if ((Self.Height - i_ActY + Y) > 20) then begin
              Self.Height := Self.Height - i_ActY + Y;
              i_ActY := Y;
            end;
          end
        end
        else if (mcstMoveable in FOptions) then begin
        // Verschieben
          Self.Left := Self.Left - i_ActX + X;
          Self.Top := Self.Top - i_ActY + Y;
        end;
        if (Self.Parent is TScrollBox) then TScrollBox(Parent).ScrollInView(Self);
        sPoint := TWinControl(Sender).ClientToScreen(Point(i_ActX, i_ActY));
        SetCursorPos(sPoint.x, sPoint.y);
        Self.Invalidate;
        Application.ProcessMessages;
      end
      else
      begin
      // Richtigen Cursor setzen
        if ((X <= 4) and (Y <= 4)) or // linke obere Ecke
           ((X >= (Self.Width -4)) and (Y >= (Self.Height - 4))) then   // rechte untere Ecke
          Self.Cursor := crSizeNWSE
        else if ((X >= (Self.Width - 4)) and (Y <= 4)) or // rechte obere Ecke
           ((X <= 4) and (Y >= (Self.Height - 4))) then   // linke untere Ecke
          Self.Cursor := crSizeNESW
        else if (X <= 4) or (X >= (Self.Width - 4)) then // linker oder rechter Rand
          Self.Cursor := crSizeWE
        else if (Y <= 4) or (Y >= (Self.Height - 4)) then // oberer oder unterer Rand
          Self.Cursor := crSizeNS
        else Self.Cursor := crArrow;
      end;
      dec(FMoveLock);
    end;
  end;
end;

{------------------------------------------------}
procedure TMyCustomControl.ShapeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{------------------------------------------------}
begin
  FMoveLock:= 0;
end;

{----------------------------------------------------}
function TMyCustomControl.GetColor: TColor;
{----------------------------------------------------}
begin
  Result := Canvas.Brush.Color;
end;

{----------------------------------------------------}
procedure TMyCustomControl.SetColor(Value: TColor);
{----------------------------------------------------}
begin
  if (Canvas.Brush.Color <> Value) then
  begin
    Canvas.Brush.Color := Value;
    Self.Invalidate;
  end;
end;

{----------------------------------------------------}
procedure TMyCustomControl.Paint;
{----------------------------------------------------}
begin
  if (Self.Width < 10) then Self.Width := 10;
  if (Self.Height < 10) then Self.Height := 10;

  inherited Paint;
end;

{----------------------------------------------------}
procedure TMyCustomControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
{----------------------------------------------------}
begin
  if (AHeight < 10) then AHeight := 10;
  if (AWidth < 10) then AWidth := 10;

  inherited;
end;

{----------------------------------------------------}
procedure TMyCustomControl.Notification(AComponent : TComponent; Operation : TOperation);
{----------------------------------------------------}
begin
  inherited Notification(AComponent, Operation);
end;

{----------------------------------------------------}
procedure TMyCustomControl.DoOnClick(Sender: TObject);
{----------------------------------------------------}
begin
end;

{----------------------------------------------------}
procedure TMyCustomControl.SetOptions(Value: TMyCustomShapeTypeSet);
{----------------------------------------------------}
begin
  if (FOptions <> Value) then
  begin
    FOptions := [];
    if (mcstMoveable in Value) then Include(FOptions, mcstMoveable);
    if (mcstSizeable in Value) then Include(FOptions, mcstSizeable);
  end;
end;

{------------------------------------}
procedure TMyCustomControl.CopyToClipBoard;
{------------------------------------}
var
  MyFormat : Word;
  Bitmap   : TBitmap;
  AData    : THandle;
  APalette : HPalette;
  pForm    : TForm;
  oldParent : TWinControl;
  oldAlign : TAlign;
  oldClientRect : TRect;
begin
  pForm := TForm.Create(nil);
  oldAlign := Self.Align;
  if (Assigned(Self.Parent)) then begin
    oldClientRect := Self.ClientRect;
    oldParent := Self.Parent;
  end
  else begin
    oldParent := nil;
  end;
  try
    pForm.Caption := S_ClipboardImage;
    pForm.Width := Screen.Width;
    pForm.Height := Screen.Height;
    Self.Parent := pForm;
    Self.Align := alClient;
    pForm.Invalidate;
    Self.Invalidate;
    Application.ProcessMessages;
    pForm.Show;
    pForm.Hide;
    Bitmap := GetControlImage(Self);
    try
      Bitmap.SaveToClipBoardFormat(MyFormat, AData, APalette);
      ClipBoard.SetAsHandle(MyFormat,AData);
    finally
      Bitmap.Free;
    end;
  finally
    Self.Parent := oldParent;
    Self.Align := oldAlign;
    if (Assigned(Self.Parent)) then begin
      Self.SetBounds(oldClientRect.Left, oldClientRect.Top,
        (oldClientRect.Right - oldClientRect.Left),
        (oldClientRect.Bottom - oldClientRect.Top));
    end;
    pForm.Free;
  end;
end;

{--------------------------------- TMyCustomShape ---------------------------------}

{----------------------------------------------------}
constructor TMyCustomShape.Create(anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited Create(anOwner);

  Self.Shape := stRoundRect;
end;

{----------------------------------------------------}
procedure TMyCustomShape.SetShape(Value: TShapeType);
{----------------------------------------------------}
begin
  if (FShape <> Value) then
  begin
    FShape := Value;
    Invalidate;
  end;
end;

{----------------------------------------------------}
procedure TMyCustomShape.Paint;
{----------------------------------------------------}
var
  X, Y, W, H, S: Integer;
begin
  inherited Paint;

  with Self.Canvas do
  begin
    X := Pen.Width div 2;
    Y := X;
    W := Width - Pen.Width + 1;
    H := Height - Pen.Width + 1;
    if Pen.Width = 0 then
    begin
      Dec(W);
      Dec(H);
    end;
    if W < H then S := W else S := H;
    if FShape in [stSquare, stRoundSquare, stCircle] then
    begin

      Inc(X, (W - S) div 2);
      Inc(Y, (H - S) div 2);
      W := S;
      H := S;
    end;
    case FShape of
      stRectangle, stSquare:
        Rectangle(X, Y, X + W, Y + H);
      stRoundRect, stRoundSquare:
        RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
      stCircle, stEllipse:
        Ellipse(X, Y, X + W, Y + H);
    end;
  end;
end;

end.
