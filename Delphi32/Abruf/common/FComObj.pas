{------------------------------------------------------------------------------}
{ Übersichtpanel für COM-Server                                                }
{                                                                              }
{ 02.07.2002  GD  Neu                                                          }
{                                                                              }
{ (C) Karl Wieser GmbH 2002                                                    }
{------------------------------------------------------------------------------}
unit FComObj;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ShellApi, ComObj, ComObjIni, Variants;

type
  TFormComObjectControl = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    bbtnOk: TBitBtn;
    ScrollBox: TScrollBox;
    procedure pnBottomResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    FImageList : TList;
    FComList   : TList;
    FIni       : TComObjectIni;
    FComArray  : array of OleVariant;
    procedure LoadImages;
    procedure LeftClick(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

{-------------------------------------------}
procedure TFormComObjectControl.FormCreate(Sender: TObject);
{-------------------------------------------}
begin
  FImageList := TList.Create;
  FComList := TList.Create;
  FIni := TComObjectIni.Create;
  LoadImages;
end;

{-------------------------------------------}
procedure TFormComObjectControl.FormDestroy(Sender: TObject);
{-------------------------------------------}
var
  i : integer;
begin
  for i := FImageList.Count-1 downto 0 do TObject(FImageList[i]).Free;
  FImageList.Free;
  FComList.Free;
  FIni.Free;
end;

{-------------------------------------------}
procedure TFormComObjectControl.pnBottomResize(Sender: TObject);
{-------------------------------------------}
begin
  if (not (csDestroying in Self.ComponentState)) then begin
    bbtnOk.Left := (pnBottom.Width - bbtnOk.Width) div 2;
  end;
end;

{-------------------------------------------}
procedure TFormComObjectControl.LoadImages;
{-------------------------------------------}
const
  C_Border = 30;
  C_Diff   = 20;
var
  pSl   : TStrings;
  i, x, y : integer;
  p     : TImage;
begin
  pSl := TStringList.Create;
  try
    // In INI definierte COM-Objecte holen
    FIni.GetComServerList(pSl);
    SetLength(FComArray, pSl.Count);

    // Positionen initialisieren
    x := C_Border;
    y := C_Border;

    // ComObjekte als Images ablegen
    for i := 0 to pSl.Count-1 do begin
      p := TImage.Create(ScrollBox);
      p.AutoSize := True;
      p.Tag := i;
      p.Parent := ScrollBox;
      p.OnMouseDown := Self.LeftClick;
      p.Left := x;
      p.Top := y;
      p.ShowHint := True;
      p.Hint := pSl[i];
      p.Picture.Icon.Handle := ExtractIcon(HInstance, PChar(pSl[i]), 0);
      Application.ProcessMessages;
      x := x + p.Width + C_Diff;
      FImageList.Add(p);
    end;
  finally
    pSl.Free;
  end;

end;

{-------------------------------------------}
procedure TFormComObjectControl.LeftClick(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{-------------------------------------------}
var
  p         : TImage;
  oldCursor : TCursor;
begin
  if (Sender is TImage) and (Button = mbRight) then begin
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      p := TImage(Sender);
      if (VarType(FComArray[p.Tag]) = varEmpty) then
        FComArray[p.Tag] := CreateOleObject(FIni.ComClassName[p.Hint]);
    finally
      Screen.Cursor := oldCursor;
    end;
    Application.ProcessMessages;
    FComArray[p.Tag].PopUp;
  end;
end;

end.
