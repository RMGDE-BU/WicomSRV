{------------------------------------------------------------------------------}
{ Objekt zur Anzeige von variierenden Balken (Lasten bei LAKS)                 }
{------------------------------------------------------------------------------}
unit oLastAnz;

interface

uses
  ExtCtrls, Classes, Controls, Graphics, Forms, SysUtils, tbLaksP, LaksUtil,
  Windows;

type
  TLastDiagramm = class(TPanel)
    constructor Create(anOwner: TComponent; aParent: TWinControl;
                       aLeft, aRight: integer; aSl: TStringList); reintroduce;
    destructor Destroy; override;
    procedure DiagrammResize(Sender: TObject);
  private
    FTitle          : string;
    FLeftMargin     : integer;
    FRightMargin    : integer;
    FTopMargin      : integer;
    FBottomMargin   : integer;
    FZeitfenster1   : double;
    FZeitfenster2   : double;
    FZeitfenster3   : double;
    FZeitfenster4   : double;
    FXMax           : integer;
    FImage          : TImage;
    FAnOwner        : TForm;
    FLastenString   : string;
    FLastenListe    : TStringList;
    procedure InitDiagramm;
    procedure DrawZeitfenster(X1, X2, Y1, Y2: integer);
    procedure DrawValues;
    procedure SetLasten(aSl: TStringList);
    procedure AddLLZWert(aLast: string; aLfdNr: integer; aZustand: boolean);
  public
    procedure DrawLastDiagramm;
    procedure SetZeitFenster(Z1, Z2, Z3, Z4: double);
    procedure AddValues(X: double; Y: integer);
    property LastenListe: TStringList read FLastenListe write SetLasten;
    property XMax: integer read FXmax write FXMax;
  end;

implementation

resourcestring
 S_Lastzustaende = 'Lastzustände';


{--------------------------- Allgemeine Funktionen ----------------------------}

type
  PLLZKanalRec = ^TLLZKanalRec;
  TLLZKanalRec = record
    LfdNr      : integer;
    Zustand    : boolean;
  end;

{ Leert eine mit PLLZKanalRec-Objekten gefüllte Liste }
{---------------------------------------------------}
procedure ClearLLZListe(var aSl: TStringlist);
{---------------------------------------------------}
var
  i        : integer;
begin
  try
    for i:= 0 to aSl.Count-1 do
      Dispose(PLLZKanalRec(aSl.Objects[i]));
  except
  end;
  aSl.Clear;
end;

{ Leert eine mit Stringlist-Objekten gefüllte Liste }
{---------------------------------------------------}
procedure ClearLastenListe(var aSl: TStringlist);
{---------------------------------------------------}
var
  i        : integer;
  sl       : TStringList;
begin
  try
    for i:= 0 to aSl.Count-1 do begin
      sl:= TStringList(aSl.Objects[i]);
      ClearLLZListe(sl);
      sl.Free;
    end;
  except
  end;
  aSl.Clear;
end;

{------------------------------ TLastDiagramm ---------------------------------}

{----------------------------------------------------}
constructor TLastDiagramm.Create(anOwner: TComponent; aParent: TWinControl;
                                 aLeft, aRight: integer; aSl: TStringList);
{----------------------------------------------------}
var
  i         : integer;
begin
  FAnOwner:= TForm(anOwner);
  FLeftMargin:= aLeft;
  FRightMargin:= aRight;
  FTopMargin:= 5;
  FBottomMargin:= 5;
  FZeitfenster1:= 0;
  FZeitfenster2:= 0;
  FZeitfenster3:= 0;
  FZeitfenster4:= 0;
  FXMax:= 15;

  FTitle:= S_Lastzustaende;
  FLastenString:= '';
  FLastenListe:= TStringList.Create;
  if (aSl <> nil) then for i:= 0 to aSl.Count-1 do FLastenListe.Add(aSl[i]);

  inherited Create(anOwner);

  self.Parent:= aParent;
  self.Align:= alClient;

  FImage:= TImage.Create(self);
  FImage.Parent:= self;
  FImage.Align:= alClient;

  self.onResize:= DiagrammResize;
end;

{----------------------------------------------------}
destructor TLastDiagramm.Destroy;
{----------------------------------------------------}
begin
  FLastenListe.Free;
  FImage.Free;
  inherited Destroy;
end;

{----------------------------------------------------}
procedure TLastDiagramm.DiagrammResize(Sender: TObject);
{----------------------------------------------------}
begin
  InitDiagramm;
end;

{ Zeichnet das Diagramm nach den Vorgaben            }
{----------------------------------------------------}
procedure TLastDiagramm.InitDiagramm;
{----------------------------------------------------}
var
  X1, Y1, X2, Y2 : integer;
  dX, dY         : double;
  i              : integer;

  {--------------------------}
  procedure SetTitle(aString: string);
  {--------------------------}
  var
    X, Y     : integer;
  begin
    Y:= trunc(Height/100 * FTopMargin);    { oberer Rand }
    if Y < 1 then Y:= 1;
    X:= (Width - FImage.Canvas.TextWidth(aString)) div 2;
    FImage.Canvas.Font.Color:= clBlue;
    FImage.Canvas.TextOut(X, Y, aString);
  end;

begin
  { Graphics auf nil setzen, damit Canvas-Größe angepaßt wird  !!! }
  FImage.Picture.Graphic:= nil;
  Application.ProcessMessages;
  { Canvas leeren }
  FImage.Canvas.Brush.Color:= clBtnFace;
  FImage.Canvas.FillRect(ClientRect);
  { Ränder festlegen }
  SetTitle(FTitle);
  FImage.Canvas.Font.Color:= clBlack;
  X1:= trunc(Width/100 * FLeftMargin);    { linker Rand }
  if X1 < 1 then X1:= 1;
  X2:= Width - trunc(Width/100 * FRightMargin);   { rechter Rand }
  if X2 < 1 then X2:= 1;
  Y1:= trunc(Height/100 * FTopMargin);    { oberer Rand }
  if Y1 < 1 then Y1:= 1;
  Y1:= Y1 + FImage.Canvas.TextHeight('X') + 2;
  Y2:= Height - trunc(Height/100 * FBottomMargin) - FImage.Canvas.TextHeight('X') - 2; { unterer Rand }
  if Y2 < 1 then Y2:= 1;
  { Nummern eintragen }
  if FLastenListe.Count > 0 then begin
    dY:= (Y2 - Y1) / FLastenListe.Count;
    if dY > 12 then dY:= 12;
    for i:= 0 to FLastenListe.Count-1 do begin
      FImage.Canvas.TextOut(X1, (Y1 + Round(i*dY + dY/2)), FLastenListe[i]);
    end;
    X1:= X1 + FImage.Canvas.TextWidth('XX');
  end;
  { Begrenzungslinien zeichnen }
  FImage.Canvas.Pen.Width:= 2;
  FImage.Canvas.Pen.Style:= psSolid;
  FImage.Canvas.Pen.Color:= clBlack;
  FImage.Canvas.Polygon([Point(X1, Y1), Point(X1, Y2), Point(X2, Y2),
                         Point(X2, Y1), Point(X1, Y1)]);
  { X-Achse beschriften }
  if FXMax > 0 then begin
    dX:= (X2 - X1) / FXMax;
    FImage.Canvas.Pen.Width:= 1;
    FImage.Canvas.Pen.Style:= psDot;
    FImage.Canvas.Pen.Color:= clBlack;
    for i:= 1 to FXMax do begin
      FImage.Canvas.TextOut(X1 + Round(i*dX) - FImage.Canvas.TextWidth(IntToStr(i)),
                            Y2+2, IntToStr(i));
      FImage.Canvas.MoveTo(X1 + Round(i*dX), Y2);
      FImage.Canvas.LineTo(X1 + Round(i*dX), Y1);
    end;
  end;
  { Zeitfenster einzeichnen }
  DrawZeitfenster(X1, X2, Y1, Y2);
end;

{ Zeitfenster einzeichnen                            }
{----------------------------------------------------}
procedure TLastDiagramm.DrawZeitfenster(X1, X2, Y1, Y2: integer);
{----------------------------------------------------}
var
  dX, xh1, x, y  : double;
begin
  dX:= (X2 - X1) / FXMax;
  FImage.Canvas.Pen.Width:= 1;
  FImage.Canvas.Pen.Style:= psSolid;
  FImage.Canvas.Pen.Color:= clBlue;
  FImage.Canvas.Font.Color:= clBlue;
  xh1:= 0;
  y:= Self.ClientHeight - FImage.Canvas.TextHeight('X') - 2;
  if (FZeitFenster1 <> 0) then begin
    FImage.Canvas.MoveTo(X1 + Round(FZeitFenster1*dX), Y2);
    FImage.Canvas.LineTo(X1 + Round(FZeitFenster1*dX), Y1);
    x:= X1 + xh1 + ((Round(FZeitFenster1*dX) - xh1) / 2);
    xh1:= Round(FZeitFenster1*dX);
    FImage.Canvas.TextOut(Round(x), Round(y), '1');
  end;
  if (FZeitFenster2 <> 0) then begin
    FImage.Canvas.MoveTo(X1 + Round(FZeitFenster2*dX), Y2);
    FImage.Canvas.LineTo(X1 + Round(FZeitFenster2*dX), Y1);
    x:= X1 + xh1 + ((Round(FZeitFenster2*dX) - xh1) / 2);
    xh1:= Round(FZeitFenster2*dX);
    FImage.Canvas.TextOut(Round(x), Round(y), '2');
  end;
  if (FZeitFenster3 <> 0) then begin
    FImage.Canvas.MoveTo(X1 + Round(FZeitFenster3*dX), Y2);
    FImage.Canvas.LineTo(X1 + Round(FZeitFenster3*dX), Y1);
    x:= X1 + xh1 + ((Round(FZeitFenster3*dX) - xh1) / 2);
    xh1:= Round(FZeitFenster3*dX);
    FImage.Canvas.TextOut(Round(x), Round(y), '3');
  end;
  if (FZeitFenster4 <> 0) then begin
    FImage.Canvas.MoveTo(X1 + Round(FZeitFenster4*dX), Y2);
    FImage.Canvas.LineTo(X1 + Round(FZeitFenster4*dX), Y1);
    x:= X1 + xh1 + ((Round(FZeitFenster4*dX) - xh1) / 2);
    xh1:= Round(FZeitFenster4*dX);
    FImage.Canvas.TextOut(Round(x), Round(y), '4');
  end;
  if (FZeitFenster4 <> FXMax) and (xh1 <> 0) then begin
    x:= X1 + xh1 + ((Round(FXMax*dX) - xh1) / 2);
    FImage.Canvas.TextOut(Round(x), Round(y), '5');
  end;
end;

{ Zeichnet die Lastzustände aus der Kanalliste      }
{----------------------------------------------------}
procedure TLastDiagramm.DrawValues;
{----------------------------------------------------}
var
  X1, Y1, X2, Y2    : integer;
  dX, dY            : double;
  i, j, k, l, m, n  : integer;
  sl                : TStringList;
begin
  { Ränder festlegen }
  X1:= trunc(Width/100 * FLeftMargin);    { linker Rand }
  if X1 < 1 then X1:= 1;
  X1:= X1 + FImage.Canvas.TextWidth('XX');
  X2:= Width - trunc(Width/100 * FRightMargin);   { rechter Rand }
  if X2 < 1 then X2:= 1;
  Y1:= trunc(Height/100 * FTopMargin);    { oberer Rand }
  if Y1 < 1 then Y1:= 1;
  Y2:= Height - trunc(Height/100 * FBottomMargin) - FImage.Canvas.TextHeight('X') - 2; { unterer Rand }
  if Y2 < 1 then Y2:= 1;

  { Schleife über alle Lasten }
  if  (FLastenListe.Count > 0) and (FXMax > 0) then begin
    dX:= (X2 - X1) / (FXMax * 10);
    dY:= (Y2 - Y1) / FLastenListe.Count;
    if dY > 12 then dY:= 12;
  { Schleife über die Werte eines Kanals }
    for i:= 0 to FLastenListe.Count-1 do begin
      j:= X1;   { Flag für aktuelle X-Position }
      sl:= TStringList(FLastenListe.Objects[i]);
      for k:= 0 to sl.Count-1 do begin
        l:= X1 + Round(PLLZKanalRec(sl.Objects[k])^.LfdNr * dX);
        if (PLLZKanalRec(sl.Objects[k])^.Zustand) then begin
          m:= 2;
          FImage.Canvas.Brush.Color:= clGreen;
        end
        else begin
          if (dY-2 > 10) then m:= 10 else m:= Round(dY-2);
          if (m < 2) then m:= 2;
          FImage.Canvas.Brush.Color:= clRed;
        end;
        n:= Round(Y1 + (i+1.5)*dY + FImage.Canvas.TextHeight('X'));
        m:= n - m;
        FImage.Canvas.FillRect(Rect(j, m, l, n));
        j:= l;
      end;
    end;
  end;
end;

{ Übergibt eine Stringliste mit neuen Lasten         }
{ ACHTUNG: nur komplette Übergabe möglich !!!        }
{----------------------------------------------------}
procedure TLastDiagramm.SetLasten(aSl: TStringList);
{----------------------------------------------------}
var
  i, j      : integer;
  sl        : TStringList;
begin
  if aSl <> nil then begin
    ClearLastenListe(FLastenListe);
    FLastenString:= '';
    for i:= 0 to aSl.Count-1 do begin
      FLastenstring:= FLastenString + ' ' + aSl[i];
      sl:= TStringList.Create;
      if (FLastenListe.Count > 0) then begin
        if (StrToIntDef(aSl[i], 0) >= StrToIntDef(FLastenListe[FLastenListe.Count-1], 0))
        then FLastenListe.AddObject(aSl[i], sl)
        else begin
          j:= FLastenListe.Count-1;
          while (j > 0) and
                (StrToIntDef(aSl[i], 0) < StrToIntDef(FLastenListe[j-1], 0))
          do begin
            dec(j);
          end;
          FLastenListe.InsertObject(j, aSl[i], sl);
        end
      end
      else begin
        FLastenListe.AddObject(aSl[i], sl)
      end;
    end;
    InitDiagramm;
  end;
end;

{ Fügt neuen Wert in LLZ-Liste ein                   }
{ Parameter: Nummer/Name der Last;                   }
{            Lfd. Nr. des Wertes;                    }
{            Zustand ein/aus                         }
{----------------------------------------------------}
procedure TLastDiagramm.AddLLZWert(aLast: string; aLfdNr: integer; aZustand: boolean);
{----------------------------------------------------}
var
  i, j      : integer;
  sl        : TStringList;
  p         : PLLZKanalRec;
begin
  i:= FLastenListe.IndexOf(aLast);
  if (i > -1) then begin
    sl:= TStringList(FLastenListe.Objects[i]);
    j:= sl.IndexOf(IntToStr(aLfdNr));
    if (j < 0) then begin
      New(p);
      p^.LfdNr:= aLfdNr;
      p^.Zustand:= aZustand;
      sl.AddObject(IntToStr(aLfdNr), TObject(p));
    end;
  end;
end;

{ Übergibt neue Werte                                }
{ Parameter: Stringliste mit PLaksLastenRec-Objekten }
{----------------------------------------------------}
procedure TLastDiagramm.AddValues(X: double; Y: integer);
{----------------------------------------------------}
var
  i         : integer;
  m         : LongInt;

  {---------------------------------}
  function IsLastZustand(i: integer): boolean;
  {---------------------------------}
  begin
    if Pos(IntToStr(i), FLastenString)> 0 then result:= True else result:= False;
  end;

begin
  m:= 1;
  { Bitweises Überprüfen des Lastzustandes }
  for i:= 1 to C_Laks_MaxLasten do begin
  { Nur aktive Lasten behandeln }
    if IsLastZustand(i) then begin
      if ((Y and m) <> 0) then AddLLZWert(IntToStr(i), Round(X*10), False)
        else AddLLZWert(IntToStr(i), Round(X*10), True);
    end;
    m:=m shl 1;
  end;
end;

{ Zeichnet das Diagramm neu                          }
{----------------------------------------------------}
procedure TLastDiagramm.DrawLastDiagramm;
{----------------------------------------------------}
begin
  LockWindowUpdate((FAnOwner as TForm).Handle);
  InitDiagramm;
  DrawValues;
  LockWindowUpdate(0);
  (FAnOwner as TForm).Invalidate;
end;

{ Stellt die Zeitfenster ein                         }
{----------------------------------------------------}
procedure TLastDiagramm.SetZeitFenster(Z1, Z2, Z3, Z4: double);
{----------------------------------------------------}
begin
  FZeitfenster1:= Z1;
  FZeitfenster2:= Z2;
  FZeitfenster3:= Z3;
  FZeitfenster4:= Z4;
end;

end.
