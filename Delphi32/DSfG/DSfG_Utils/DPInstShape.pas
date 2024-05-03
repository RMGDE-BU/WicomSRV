{------------------------------------------------------------------------------}
{ TDSfGInstanzParameterShape (von TMyCustomShape abgeleitet); aus DSfGShapes   }
{                                                                              }
{ 28.11.2000  GD    Neu                                                        }
{ 15.01.2001  GD    StringGrid um Direktzugriff und D&D erweitert              }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000                                          }
{------------------------------------------------------------------------------}
unit DPInstShape;

interface

uses
  Windows, Classes, Controls, Forms, Graphics, Buttons, Grids, MyShapes,
  GD_Utils, ExtCtrls, SysUtils, Dialogs, DSG_Utils, ComCtrls, DSfGShapes;

type
  TShapeGridMouseDown = procedure(Button: TMouseButton; cInstAdr: char;
    sDEA, sWert: string; X, Y: integer) of object;

  TDSfGInstanzParameterShape = class(TDSfGInstanzShape)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
  private
    FGeraeteTyp   : string;
    FHersteller   : string;
    FFabrikNummer : string;
    FShapeGridMouseDown : TShapeGridMouseDown;
    procedure DEADragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DEADragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure GridStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure GridMouseDown(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure Paint; override;
    procedure SetColor(Value: TColor); override;
  public
    procedure AddValue(sDEA, sWert: string);
    property GeraeteTyp: string read FGeraeteTyp write FGeraeteTyp;
    property Hersteller: string read FHersteller write FHersteller;
    property FabrikNummer: string read FFabrikNummer write FFabrikNummer;
    property ShapeGridMouseDown: TShapeGridMouseDown
      read FShapeGridMouseDown write FShapeGridMouseDown;
  end;

implementation

{------------------------- TDSfGInstanzParameterShape -------------------------}

{----------------------------------------------------}
constructor TDSfGInstanzParameterShape.Create(anOwner: TComponent);
{----------------------------------------------------}
begin
  inherited Create(anOwner);

  Self.OnDragDrop := DEADragDrop;
  Self.OnDragOver := DEADragOver;
  Self.StringGrid.Options :=
    StringGrid.Options + [goRowSelect, goColSizing] - [goRangeSelect];
  Self.StringGrid.OnMouseDown := GridMouseDown;
  Self.StringGrid.DragMode := dmManual;
  Self.StringGrid.OnDragDrop := DEADragDrop;
  Self.StringGrid.OnDragOver := DEADragOver;
  Self.StringGrid.OnStartDrag := GridStartDrag;

  StringGrid.BorderStyle := bsNone;
  StringGrid.Options := StringGrid.Options + [goColSizing];

  FGeraeteTyp := 'unbekannt';
  FHersteller := 'unbekannt';
  FFabrikNummer := 'unbekannt';

  IsBitMap := True;
  IsGrid := True;
  IsButton := True;
end;

{----------------------------------------------------}
destructor TDSfGInstanzParameterShape.Destroy;
{----------------------------------------------------}
var
  p : TControl;
begin
  // Alle übergeordneten Komponenten von Freigabe benachrichtigen
  p := Self.Parent;
  while (Assigned(p)) do begin
    FreeNotification(p);
    p := p.Parent;
  end;

  inherited Destroy;
end;

{----------------------------------------------------}
procedure TDSfGInstanzParameterShape.Paint;
{----------------------------------------------------}
var
  i             : integer;
  X, Y, W, H, S : Integer;
  sTestText     : string;
begin
// 'Paint'-Inhalt von TMyCustomShape
  if (Self.Width < 15) then Self.Width := 15;
  if (Self.Height < 15) then Self.Height := 15;

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
    if Shape in [stSquare, stRoundSquare, stCircle] then
    begin

      Inc(X, (W - S) div 2);
      Inc(Y, (H - S) div 2);
      W := S;
      H := S;
    end;
    case Shape of
      stRectangle, stSquare:
        Rectangle(X, Y, X + W, Y + H);
      stRoundRect, stRoundSquare:
        RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
      stCircle, stEllipse:
        Ellipse(X, Y, X + W, Y + H);
    end;
  end;

  i := Height div 4;
  if (i < 34) then i := 34;

  if (InstanzAdresse in ['A'..'_']) then begin
    Canvas.Font.Height := i-8;
    Canvas.Font.Color := (not Canvas.Brush.Color);
    Canvas.TextOut(((Self.Width div 3) - Canvas.TextWidth(InstanzAdresse)) div 2,
      4, InstanzAdresse);
  end;

  sTestText := FGeraeteTyp;
  if (Canvas.TextWidth(sTestText) < Canvas.TextWidth(FHersteller))
    then sTestText := FHersteller;
  if (Canvas.TextWidth(sTestText) < Canvas.TextWidth(FFabrikNummer))
    then sTestText := FFabrikNummer;
  if (sTestText <> '') then begin
    i := (i-10) div 3;
    Canvas.Font.Height := 8;
    while (Canvas.TextWidth(sTestText) <= (Self.Width div 3)) and
          (Canvas.TextHeight(sTestText) <= i) do
      Canvas.Font.Height := Canvas.Font.Height + 1;
    Canvas.Font.Height := Canvas.Font.Height - 1;

    Canvas.TextOut(
      (Self.Width div 3) + ((Self.Width div 3) - Canvas.TextWidth(FGeraeteTyp)) div 2,
      4, FGeraeteTyp);
    Canvas.TextOut(
      (Self.Width div 3) + ((Self.Width div 3) - Canvas.TextWidth(FHersteller)) div 2,
      i + 8, FHersteller);
    Canvas.TextOut(
      (Self.Width div 3) + ((Self.Width div 3) - Canvas.TextWidth(FFabrikNummer)) div 2,
      2*i + 12, FFabrikNummer);
  end;

  if (IsBitmap) then
  begin
    Canvas.StretchDraw(Rect(
      ((2*Self.Width) div 3) + 1, 1, Self.Width -1, (Self.Height div 3) - 2),
      Bitmap);
  end;

  if (IsButton) then begin
    SpeedBtn.Top := 2;
    SpeedBtn.Left := 2*Self.Width div 3 + 1;
    SpeedBtn.Width := (Self.Width div 3) - 4;
    SpeedBtn.Height := (Self.Height div 3) - 4;
  end;

  if (IsGrid) then begin
    StringGrid.Left := 8;
    StringGrid.Top := Self.Height div 3;
    StringGrid.Width := Self.Width - 16;
    StringGrid.Height := Self.Height - StringGrid.Top - 8;

    if (StringGrid.VisibleRowCount+StringGrid.FixedRows = StringGrid.RowCount)
    then StringGrid.DefaultColWidth := (StringGrid.Width-6) div 2
    else StringGrid.DefaultColWidth :=
      (StringGrid.Width-6-GetSystemMetrics(SM_CYHSCROLL)) div 2;
  end;
end;

{ Fügt Wert zu Liste hinzu                           }
{ Parameter: DEA, Wert zu DEA                        }
{----------------------------------------------------}
procedure TDSfGInstanzParameterShape.AddValue(sDEA, sWert: string);
{----------------------------------------------------}
begin
  with StringGrid do begin
    if ((RowCount > 2) or (Cells[0, 1] <> '')) then RowCount := RowCount + 1;
    Cells[0, RowCount-1] := sDEA;
    Cells[1, RowCount-1] := sWert;
  end;
end;

{ Ereignisbehandlung für Akzepieren von D&D-Objekten }
{----------------------------------------------------}
procedure TDSfGInstanzParameterShape.DEADragOver(
    Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
{----------------------------------------------------}
begin
  Accept := ((Source is TTreeView) and
             (Assigned(TTreeView(Source).Selected)));
end;

{ Ereignisbehandlung für Ablegen von D&D-Objekten    }
{----------------------------------------------------}
procedure TDSfGInstanzParameterShape.DEADragDrop(Sender, Source: TObject; X, Y: Integer);
{----------------------------------------------------}
begin
// Einfügen über 'EndDrag' des aufrufenden Fensters gelöst
end;

{ Ereignisbehandlung für Starten von D&D-Objekten    }
{----------------------------------------------------}
procedure TDSfGInstanzParameterShape.GridStartDrag(
  Sender: TObject; var DragObject: TDragObject);
{----------------------------------------------------}
begin
  if (Sender is TStringGrid) and
     ((TStringGrid(Sender).RowCount = 2) and
      (TStringGrid(Sender).Cols[0][1] = '')) then Abort;
end;

{ Ereignisbehandlung für MouseDown beim StringGrid   }
{----------------------------------------------------}
procedure TDSfGInstanzParameterShape.GridMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{----------------------------------------------------}
var
  sDEA, sWert : string;
  iCol, iRow  : integer;
  i           : integer;
begin
  StringGrid.MouseToCell(X, Y, iCol, iRow);
  if (iRow > 0) then StringGrid.Row := iRow;

  if (Button = mbLeft) then StringGrid.BeginDrag(True);

  if (Assigned(FShapeGridMouseDown)) then
    if (iRow > 0) then begin
      sDEA := StringGrid.Cells[0, iRow];
      i := Pos(' - ', sDEA);
      if (i > 0) then sDEA := Copy(sDEA, 1, i-1);
      sWert := StringGrid.Cells[1, iRow];
      if (sDEA <> '') then
         FShapeGridMouseDown(Button, Self.InstanzAdresse, sDEA, sWert,
         StringGrid.ClientToScreen(Point(X, Y)).X,
         StringGrid.ClientToScreen(Point(X, Y)).Y);
    end;
end;

{----------------------------------------------------}
procedure TDSfGInstanzParameterShape.SetColor(Value: TColor);
{----------------------------------------------------}
begin
  inherited;

  if (Assigned(StringGrid)) then begin
    if (Parent is TForm) then StringGrid.Color := TForm(Parent).Color
    else if (Parent is TPanel) then StringGrid.Color := TPanel(Parent).Color;
  end;
end;

end.
