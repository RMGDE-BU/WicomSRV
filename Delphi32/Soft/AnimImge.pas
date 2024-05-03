{------------------------------------------------------------------------------}
{ Objekt für Animiertes Wieser-Logo                                            }
{                                                                              }
{ 12.07.2000  GD    Neu                                                        }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000                                          }
{------------------------------------------------------------------------------}
unit AnimImge;

interface

uses
  Windows, Forms, Controls, Classes, ExtCtrls, Dialogs, SysUtils, Graphics;

type
  TAnimatedAlign = (anmlTop, anmlMiddle, anmlBottom);

  TCustomAnimatedImage = class(TCustomControl)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
  private
    { Private-Deklarationen }
    FDoPlay : boolean;
    FHandle : integer;
    FBackImage : TImage;
    FAnimatedImage : TImage;
    procedure InsertSelfToList(state: boolean);
    procedure SetStretch(Value: boolean);
  protected
    procedure InitImages(state: boolean); virtual;
    procedure AnimateImage; virtual;
    property IsPlaying: boolean read FDoPlay write FDoPlay;
  public
    procedure Play; virtual;
    procedure Stop; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Stretch: boolean write SetStretch;
  end;

  TWieserAnimatedImage = class(TCustomAnimatedImage)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
  private
    { Private-Deklarationen }
    FIsInPlayingLoop : boolean;
  protected
    procedure InitImages(state: boolean); override;
    procedure AnimateImage; override;
  end;

  TAnimatedBirdImage = class(TCustomAnimatedImage)
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
  private
    { Private-Deklarationen }
    FIsInPlayingLoop : boolean;
    FAnimatedAlign   : TAnimatedAlign;
    FText            : string;
  protected
    procedure AnimateImage; override;
    procedure InitImages(state: boolean); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property AnimatedAlign: TAnimatedAlign
      read FAnimatedAlign write FAnimatedAlign;
    property Text: string read Ftext write FText;
  end;

implementation

{$R *.RES}

type
  TSpeedyBmps = (pic1, pic2, pic3, pic4, pic5, pic6, pic7, pic8, pic9, pic10,
    pic11, pic12, pic13, pic14);

  TBirdBmps = (Bird1, Bird2, Bird3, Bird4, Bird5, Bird6, Bird7, Bird8, Bird9, Bird10);

const
  CSpeedyIconSet : array [TSpeedyBmps] of string[7] =
    ('SPEEDY1', 'SPEEDY2', 'SPEEDY3', 'SPEEDY4', 'SPEEDY5', 'SPEEDY6', 'SPEEDY7',
     'SPEEDY7', 'SPEEDY6', 'SPEEDY5', 'SPEEDY4', 'SPEEDY3', 'SPEEDY2', 'SPEEDY1');

  CBirdIconSet : array [TBirdBmps] of string[6] = ('BIRD1', 'Bird2', 'BIRD3',
    'BIRD4', 'Bird5', 'BIRD6','BIRD7', 'Bird8', 'BIRD9', 'Bird10');
  CBirdIconSetLeft : array [Bird1..Bird3] of string[6] = ('BIRD1', 'Bird2', 'BIRD3');
  CBirdIconSetLeftToRight : array [Bird4..Bird5] of string[6] = ('BIRD4', 'Bird5');
  CBirdIconSetRightToLeft : array [Bird6..Bird7] of string[6] = ('BIRD6', 'Bird6');
  CBirdIconSetRight : array [Bird8..Bird10] of string[6] = ('BIRD8', 'Bird9', 'BIRD10');

  CWieserRes = 'WIESER';

var
  FAnimatedObjectList : TStrings;
  FTimerId : UINT;

{ Timer-Callback                         }
{----------------------------------------}
procedure ThisTimerProc(
  MyHandle: HWND; MyMsg, MyEvent: UINT; MyTime: DWord); stdcall;
{----------------------------------------}
var
  i : integer;
begin
  if (Assigned(FAnimatedObjectList)) then
    for i := 0 to FAnimatedObjectList.Count-1 do begin
      if (FAnimatedObjectList.Objects[i] is TWieserAnimatedImage) then
        TWieserAnimatedImage(FAnimatedObjectList.Objects[i]).AnimateImage
      else if (FAnimatedObjectList.Objects[i] is TCustomAnimatedImage) then
        TCustomAnimatedImage(FAnimatedObjectList.Objects[i]).AnimateImage;
    end;
end;

{----------------------------- TCustomAnimatedImage ---------------------------}

{----------------------------------------}
constructor TCustomAnimatedImage.Create(anOwner: TComponent);
{----------------------------------------}
begin
  inherited Create(anOwner);

  Width := 32;
  Height := 32;
  if (Owner is TWinControl) then Parent := TWinControl(Owner);
  Left := 0;
  Top := 0;

  InitImages(True);
  FHandle := Self.Handle;
  InsertSelfToList(True);
  FDoPlay := False;
end;

{----------------------------------------}
destructor TCustomAnimatedImage.Destroy;
{----------------------------------------}
begin
  InsertSelfToList(False);
  inherited;
end;

{ Trägt SELF in FAnimatedObjectList ein  }
{ Parameter: True-Eintr., False-Austr.   }
{----------------------------------------}
procedure TCustomAnimatedImage.InsertSelfToList(state: boolean);
{----------------------------------------}
var
  i : integer;
begin
  if (State) then begin
    if (not Assigned(FAnimatedObjectList)) then begin
      FAnimatedObjectList := TStringList.Create;
      FTimerId := Windows.SetTimer(0, 1, 200, @ThisTimerProc);
    end;
    i := FAnimatedObjectList.IndexOf(IntToStr(FHandle));
    if (i < 0) then FAnimatedObjectList.AddObject(IntToStr(FHandle), Self);
  end
  else begin
    i := FAnimatedObjectList.IndexOf(IntToStr(FHandle));
    if (i >= 0) then FAnimatedObjectList.Delete(i);
    if (FAnimatedObjectList.Count = 0) then begin
      KillTimer(0, FTimerId);
      FAnimatedObjectList.Free;
      FAnimatedObjectList := nil;
    end;
  end;
end;

{ Erzeugt/Gibt frei die Images           }
{ Parameter: True-Erzeugen, False-freig. }
{----------------------------------------}
procedure TCustomAnimatedImage.InitImages(state: boolean);
{----------------------------------------}

  procedure InitThisImage(var pImage: TImage);
  begin
    pImage := TImage.Create(Self);
    pImage.Parent := Self;
    pImage.Visible := True;
    pImage.Top := 0;
    pImage.left := 0;
    pImage.Width := 32;
    pImage.Height := 32;
  end;

begin
  if (state) then begin
    InitThisImage(FBackImage);
    InitThisImage(FAnimatedImage);
  end
  else begin
    if (Assigned(FBackImage)) then FBackImage.Free;
    if (Assigned(FAnimatedImage)) then FAnimatedImage.Free;
  end;
end;

{ Erweitert die Width/Height-Methoden    }
{----------------------------------------}
procedure TCustomAnimatedImage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
{----------------------------------------}
begin
  inherited;
  if (Assigned(FBackImage)) then begin
    FBackImage.Width := AWidth;
    FBackImage.Height := AHeight;
  end;
  if (Assigned(FAnimatedImage)) then begin
    FAnimatedImage.Width := AWidth;
    FAnimatedImage.Height := AHeight;
  end;
end;

{ Bitmaps den Dimensionen anpassen       }
{ Parameter: Anpassen Ja/Nein            }
{----------------------------------------}
procedure TCustomAnimatedImage.SetStretch(Value: boolean);
{----------------------------------------}
begin
  FBackImage.Stretch := Value;
  FAnimatedImage.Stretch := Value;
end;

{ Animiertes Bild starten - beenden      }
{----------------------------------------}
procedure TCustomAnimatedImage.AnimateImage;
{----------------------------------------}
begin
//  if (FDoPlay = state) then Exit;
end;

{ Startet die Animation                  }
{----------------------------------------}
procedure TCustomAnimatedImage.Play;
{----------------------------------------}
begin
  if (not IsPlaying) then IsPlaying := True;
end;

{ Beendet die Animation                  }
{----------------------------------------}
procedure TCustomAnimatedImage.Stop;
{----------------------------------------}
begin
  if (IsPlaying) then IsPlaying := False;
end;

{----------------------------- TWieserAnimatedImage ---------------------------}

{----------------------------------------}
constructor TWieserAnimatedImage.Create(anOwner: TComponent);
{----------------------------------------}
begin
  inherited Create(anOwner);
  FIsInPlayingLoop := False;
end;

{----------------------------------------}
destructor TWieserAnimatedImage.Destroy;
{----------------------------------------}
begin
  while (FIsInPlayingLoop) do begin
    IsPlaying := False;
    sleep(10);
    Application.ProcessMessages;
  end;
  inherited;
end;

{ Erzeugt/Gibt frei die Images           }
{ Parameter: True-Erzeugen, False-freig. }
{----------------------------------------}
procedure TWieserAnimatedImage.InitImages(state: boolean);
{----------------------------------------}
begin
  inherited;

  if (state) then begin
    FBackImage.Picture.Bitmap.LoadFromResourceName(HINSTANCE, CWieserRes);
    FAnimatedImage.Picture.Bitmap.LoadFromResourceName(HINSTANCE, CSpeedyIconSet[pic1]);
    FAnimatedImage.BringToFront;
    FAnimatedImage.Transparent := True;
  end;
end;

{ Animiertes Bild starten - beenden      }
{----------------------------------------}
procedure TWieserAnimatedImage.AnimateImage;
{----------------------------------------}
const
  i : TSpeedyBmps = pic1;
  IsUp : boolean = False;
begin
  inherited;
  FAnimatedImage.Visible := IsPlaying;
  if (IsPlaying) then begin
    if (i = Low(TSpeedyBmps)) or (i = High(TSpeedyBmps)) then IsUp := not IsUp;
    if (IsUp) then Inc(i) else Dec(i);
    FAnimatedImage.Picture.Bitmap.LoadFromResourceName(HINSTANCE, CSpeedyIconSet[i]);
    Application.ProcessMessages;
  end;
end;

{----------------------------- TAnimatedBirdImage ---------------------------}

{----------------------------------------}
constructor TAnimatedBirdImage.Create(anOwner: TComponent);
{----------------------------------------}
begin
  inherited Create(anOwner);
  FIsInPlayingLoop := False;
  FAnimatedAlign := anmlMiddle;
  FText := '';
end;

{----------------------------------------}
destructor TAnimatedBirdImage.Destroy;
{----------------------------------------}
begin
  while (FIsInPlayingLoop) do begin
    IsPlaying := False;
    sleep(10);
    Application.ProcessMessages;
  end;
  Parent := nil;

  inherited;
end;

{ Erzeugt/Gibt frei die Images           }
{ Parameter: True-Erzeugen, False-freig. }
{----------------------------------------}
procedure TAnimatedBirdImage.InitImages(state: boolean);
{----------------------------------------}
begin
  inherited;

  if (state) then begin
    Parent := Parent;

    FAnimatedImage.Picture.Bitmap.LoadFromResourceName(HINSTANCE, CSpeedyIconSet[pic1]);
    FAnimatedImage.BringToFront;
    Stretch := True;
    FBackImage.Transparent := True;
    FAnimatedImage.Transparent := True;
  end;
end;

{ Animiertes Bild starten - beenden      }
{----------------------------------------}
procedure TAnimatedBirdImage.AnimateImage;
{----------------------------------------}
const
  MyBmp : TBirdBmps = Bird1;
begin
  inherited;

  FAnimatedImage.Visible := IsPlaying;
  if (IsPlaying) then begin

//    FAnimatedImage.BringToFront;

    if (MyBmp in [Bird1..Bird3]) then begin
      Inc(MyBmp);
    end

    else if (MyBmp in [Bird6..Bird8]) then begin
      Inc(MyBmp);
    end

    else if (MyBmp in [Bird4..Bird5]) then begin
      FAnimatedImage.Left := FAnimatedImage.Left + (FAnimatedImage.Width div 3);
      if ((FAnimatedImage.Left + FAnimatedImage.Width) > Self.Width) then
      begin
        MyBmp := Bird6;
      end
      else begin
        if (MyBmp = Bird4) then Inc(MyBmp) else Dec(MyBmp);
      end;
    end

    else if (MyBmp in [Bird9..Bird10]) then begin
      FAnimatedImage.Left := FAnimatedImage.Left - (FAnimatedImage.Width div 3);
      if (FAnimatedImage.Left <= 0) then
      begin
        MyBmp := Bird1;
      end
      else begin
        if (MyBmp = Bird9) then Inc(MyBmp) else Dec(MyBmp);
      end;
    end

    else Exit;

    FAnimatedImage.Picture.Bitmap.LoadFromResourceName(HINSTANCE, CBirdIconSet[MyBmp]);
    Invalidate;
    Application.ProcessMessages;
  end;
end;

{ Erweitert die Width/Height-Methoden    }
{----------------------------------------}
procedure TAnimatedBirdImage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
{----------------------------------------}
var
  i : integer;
begin
  inherited;
  if (Assigned(FBackImage)) then begin
    FBackImage.Width := AWidth;
    FBackImage.Height := AHeight;
  end;
  if (Assigned(FAnimatedImage)) then begin
    if (AHeight < AWidth) then i := AHeight div 2 else i := AWidth div 2;
    FAnimatedImage.Width := i;
    FAnimatedImage.Height := i;
    if (FAnimatedAlign = anmlMiddle) then
      FAnimatedImage.Top := (Self.Height - FAnimatedImage.Height) div 2
    else if (FAnimatedAlign = anmlBottom) then
      FAnimatedImage.Top := Self.Height - FAnimatedImage.Height
    else FAnimatedImage.Top := 0;
  end;
end;

{ Überschriebene Methode zu Paint        }
{----------------------------------------}
procedure TAnimatedBirdImage.Paint;
{----------------------------------------}
const
  oldX      : integer = 0;
  isForward : boolean = True;
var
  x1, x2, y1, y2, l, h : integer;
  oldColor  : TColor;
begin
  inherited;

  if (FAnimatedImage.Left > oldX) then begin
    isForward := True;
  end
  else if (FAnimatedImage.Left < oldX) then begin
    isForward := False;
  end;
  oldX := FAnimatedImage.Left;

  if (FText <>  '') then begin
    Application.Hint := FText;
    l := Canvas.TextWidth(FText) + 2;
    h := Canvas.TextHeight(FText) + 2;
    if (not isForward)
      then x1 := FAnimatedImage.Left + FAnimatedImage.Width + 1
      else x1 := FAnimatedImage.Left - l - 1;
    y1 := FAnimatedImage.Top + ((FAnimatedImage.Width - h) div 2);
    x2 := x1 + l;
    y2 := y1 + h;

    oldColor := Canvas.Brush.Color;
    Canvas.Brush.Color := clRed;
    Canvas.FillRect(Rect(0, y1, Width, y2));
    Canvas.Brush.Color := oldColor;

    Canvas.Rectangle(x1, y1, x2, y2);
    Canvas.TextOut(x1+1, y1+1, FText);
  end;
end;

{ Überschriebene Methode zu SetParent    }
{----------------------------------------}
procedure TAnimatedBirdImage.SetParent(AParent: TWinControl);
{----------------------------------------}
begin
  inherited;

{  if (Assigned(AParent)) then begin
    if (Assigned(FAnimatedImage)) then FAnimatedImage.Parent := AParent
    else if (Assigned(FAnimatedImage)) then FAnimatedImage.Parent := Self;
  end;  }
end;

end.
