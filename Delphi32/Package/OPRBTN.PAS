{------------------------------------------------------------------------------}
{ 29.06.1999 GD; Komponenten-Unit für TPressedBtn                           }
{                                                                              }
{ (C) Copyright Karl Wieser GmbH 1999                                          }
{------------------------------------------------------------------------------}
unit oPrBtn;

interface

uses
  Buttons, Controls, Classes, Windows, graphics;

type
  TPressedBtn = class(TSpeedButton)
    bm         : TBitmap;
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

procedure Register;

implementation

{----------------------------------------------------}
constructor TPressedBtn.Create(anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited Create(anOwner);
  bm:= Glyph;
  bm.Transparent:= True;
  bm.Width:= 16;
end;

{----------------------------------------------------}
destructor TPressedBtn.Destroy;
{----------------------------------------------------}
begin
  inherited Destroy;
end;

{----------------------------------------------------}
procedure TPressedBtn.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
{----------------------------------------------------}
const
  isDown : Boolean = False;
var
  r1, r2 : TRect;
begin
  try
    isDown:= not isDown;
    DrawButtonFace(Canvas, Bounds(0, 0, Width, Height), 1, bsAutoDetect, True,
                  isDown, False);
    r1:= Rect((Width-bm.Height) div 2, (Height-bm.Height) div 2,
              ((Width-bm.Height) div 2) + bm.Height,
              ((Height-bm.Height) div 2) + bm.Height);
    r2:= Rect(0, 0, bm.Height, bm.Height);
    Canvas.BrushCopy(r1, bm, r2, bm.TransparentColor);
  finally
    Click;
  end;
end;

procedure Register;
begin
  RegisterComponents('Wieser', [TPressedBtn]);
end;

end.
