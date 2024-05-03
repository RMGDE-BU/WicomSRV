{------------------------------------------------------------------------------}
{ 05.05.1999 GD; Object-Unit für Glyph-Button                                  }
{                                                                              }
{ (C) Karl Wieser GmbH 1999                                                    }
{------------------------------------------------------------------------------}
unit oIconBtn;

interface

uses
  StdCtrls, graphics, Controls, Classes, ExtCtrls;

const
  C_IconWidth  = 40;

type
  TIconBtn = class(TPanel)
    procedure MouseDownClick(Sender: TObject; Button: TMouseButton;
                             Shift: TShiftState; X, Y: Integer);
    procedure MouseUpClick(Sender: TObject; Button: TMouseButton;
                           Shift: TShiftState; X, Y: Integer);
    procedure IconClick(Sender: TObject);
  private
    FImage   : TImage;
    procedure SetGlyph(Value: TIcon);
    function GetGlyph: TIcon;
  public
    constructor Create(anOwner: TComponent); override;
    destructor Free;
    property Glyph: TIcon read GetGlyph write SetGlyph;
  end;

implementation

{--------------------------------------------------------------------}
constructor TIconBtn.Create(anOwner: TComponent);
{--------------------------------------------------------------------}
begin
  inherited Create(anOwner);
  BevelOuter:= bvRaised;
  BevelInner:= bvRaised;
  Alignment:= taRightJustify;
  onMouseDown:= MouseDownClick;
  onMouseUp:= MouseUpClick;
  FImage:= TImage.Create(self);
  FImage.Parent:= self;
  FImage.Align:= alLeft;
  FImage.Width:= C_IconWidth;
  FImage.Center:= True;
  FImage.OnClick:= IconClick;
  FImage.OnMouseDown:= MouseDownClick;
  FImage.OnMouseUp:= MouseUpClick;
end;

{--------------------------------------------------------------------}
destructor TIconBtn.Free;
{--------------------------------------------------------------------}
begin
  FImage.Free;
  inherited Destroy;
end;

{--------------------------------------------------------------------}
procedure TIconBtn.MouseDownClick(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
{--------------------------------------------------------------------}
begin
  BevelOuter:= bvLowered;
  BevelInner:= bvLowered;
end;

{--------------------------------------------------------------------}
procedure TIconBtn.MouseUpClick(Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
{--------------------------------------------------------------------}
begin
  BevelOuter:= bvRaised;
  BevelInner:= bvRaised;
end;

{--------------------------------------------------------------------}
procedure TIconBtn.IconClick(Sender: TObject);
{--------------------------------------------------------------------}
begin
  self.Click;
end;

{--------------------------------------------------------------------}
function TIconBtn.GetGlyph: TIcon;
{--------------------------------------------------------------------}
begin
  result:= FImage.Picture.Icon;
end;

{--------------------------------------------------------------------}
procedure TIconBtn.SetGlyph(Value: TIcon);
{--------------------------------------------------------------------}
begin
  FImage.Picture.Icon:= Value;
end;

end.
