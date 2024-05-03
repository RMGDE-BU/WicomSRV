{------------------------------------------------------------------------------}
{ Auswahllisten-Formular                                                       }
{                                                                              }
{ 07.11.2001  GD  Neu                                                          }
{ 17.12.2001  GD  Tastaturbedienung                                            }
{ 05.03.2003  GD  onResize                                                     }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2003                                    }
{------------------------------------------------------------------------------}
unit FListAuswahlDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TFormListenDialog = class(TForm)
    pnBottom: TPanel;
    pnClient: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    lbSource: TListBox;
    pnSplitter: TPanel;
    Splitter: TSplitter;
    lbSink: TListBox;
    pnButtons: TPanel;
    sbtnAllIn: TSpeedButton;
    sbtnIn: TSpeedButton;
    sbtnOut: TSpeedButton;
    sbtnAllOut: TSpeedButton;
    procedure sbtnAllInClick(Sender: TObject);
    procedure sbtnInClick(Sender: TObject);
    procedure sbtnOutClick(Sender: TObject);
    procedure sbtnAllOutClick(Sender: TObject);
    procedure ListboxDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListboxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure lbSinkKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbSourceKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure EnableSpdBtns;
    procedure InsertList(iIndex: integer; pSl: TStrings);
    function GetList(iIndex: integer): TStrings;
  protected
    { Protected-Deklarationen }
    procedure SelectString; virtual;
    procedure SelectAllStrings; virtual;
    procedure DeselectString; virtual;
    procedure DeselectAllStrings; virtual;
  public
    { Public-Deklarationen }
    property AuswahlListe: TStrings index 1 read GetList write InsertList;
    property GewaehltListe: TStrings index 2 read GetList write InsertList;
  end;

implementation

{$R *.DFM}

{------------------------------------------------------}
procedure TFormListenDialog.FormCreate(Sender: TObject);
{------------------------------------------------------}
begin
  EnableSpdBtns;
end;

{------------------------------------------------------}
procedure TFormListenDialog.FormResize(Sender: TObject);
{------------------------------------------------------}
var
  i : integer;
begin
  if (not (csDestroying in ComponentState)) then begin
    i := pnClient.ClientWidth - 2*pnClient.BorderWidth - pnSplitter.Width;
    lbSource.Width := i div 2;
    i := pnSplitter.ClientHeight - pnButtons.Height;
    pnButtons.Top := i div 2;
  end;
end;

{------------------------------------------------------}
procedure TFormListenDialog.sbtnAllInClick(Sender: TObject);
{------------------------------------------------------}
begin
  SelectAllStrings;
end;

{------------------------------------------------------}
procedure TFormListenDialog.sbtnInClick(Sender: TObject);
{------------------------------------------------------}
begin
  SelectString;
end;

{------------------------------------------------------}
procedure TFormListenDialog.sbtnOutClick(Sender: TObject);
{------------------------------------------------------}
begin
  DeselectString;
end;

{------------------------------------------------------}
procedure TFormListenDialog.sbtnAllOutClick(Sender: TObject);
{------------------------------------------------------}
begin
  DeselectAllStrings;
end;

{ Fügt Liste zur Auswahl ein                           }
{ Parameter: Index (1-Quelle,2-Ziel), Liste            }
{------------------------------------------------------}
procedure TFormListenDialog.InsertList(iIndex: integer; pSl: TStrings);
{------------------------------------------------------}
var
  i, j : integer;
begin
  if (iIndex = 1) then lbSource.Items.Assign(pSl)
  else if (iIndex = 2) then lbSink.Items.Assign(pSl);

  for i := 0 to lbSink.Items.Count-1 do begin
    j := lbSource.Items.IndexOf(lbSink.Items[i]);
    if (j >= 0) then lbSource.Items.Delete(j);
  end;

  if (lbSource.Items.Count > 0) then lbSource.ItemIndex := 0;
  if (lbSink.Items.Count > 0) then lbSink.ItemIndex := 0;
  EnableSpdBtns;
end;

{ Gibt Liste zurück                                    }
{ Parameter: Index (1-Quelle,2-Ziel)                   }
{ Rückgabe: Liste                                      }
{------------------------------------------------------}
function TFormListenDialog.GetList(iIndex: integer): TStrings;
{------------------------------------------------------}
begin
  Result := TStringList.Create;

  if (iIndex = 1) then Result.Assign(lbSource.Items)
  else if (iIndex = 2) then Result.Assign(lbSink.Items);
end;

{ En-/Disabled Auswahlschalter                         }
{------------------------------------------------------}
procedure TFormListenDialog.EnableSpdBtns;
{------------------------------------------------------}
begin
  sbtnIn.Enabled := (lbSource.Items.Count > 0);
  sbtnAllIn.Enabled := sbtnIn.Enabled;
  sbtnOut.Enabled := (lbSink.Items.Count > 0);
  sbtnAllOut.Enabled := sbtnOut.Enabled;
  bbtnOk.Enabled := (lbSink.Items.Count > 0);
end;

{ Wählt einzelnen string aus                           }
{------------------------------------------------------}
procedure TFormListenDialog.SelectString;
{------------------------------------------------------}
var
  s : string;
  i : integer;
begin
  if (lbSource.ItemIndex >= 0) then begin
    s := lbSource.Items[lbSource.ItemIndex];
    if (lbSink.Items.IndexOf(s) < 0) then begin
      lbSink.Items.Add(s);
      i := lbSource.ItemIndex;
      lbSource.Items.Delete(i);
      if (lbSource.Items.Count > i) then lbSource.ItemIndex := i
      else if (lbSource.Items.Count > 0) then lbSource.ItemIndex := i-1
      else if (lbSink.Items.Count > 0) then begin
        lbSink.SetFocus;
        lbSink.ItemIndex := 0
      end;
    end;
    EnableSpdBtns;
  end;
end;

{ Wählt alle strings aus                              }
{------------------------------------------------------}
procedure TFormListenDialog.SelectAllStrings;
{------------------------------------------------------}
var
  i : integer;
  s : string;
begin
  for i := 0 to lbSource.Items.Count-1 do begin
    s := lbSource.Items[i];
    if (lbSink.Items.IndexOf(s) < 0) then lbSink.Items.Add(s);
  end;
  lbSource.Clear;
  EnableSpdBtns;
end;

{ Wählt einzelnen string ab                            }
{------------------------------------------------------}
procedure TFormListenDialog.DeselectString;
{------------------------------------------------------}
var
  s : string;
  i : integer;
begin
  if (lbSink.ItemIndex >= 0) then begin
    s := lbSink.Items[lbSink.ItemIndex];
    if (lbSource.Items.IndexOf(s) < 0) then begin
      lbSource.Items.Add(s);
      i := lbSink.ItemIndex;
      lbSink.Items.Delete(i);
      if (lbSink.Items.Count > i) then lbSink.ItemIndex := i
      else if (lbSink.Items.Count > 0) then lbSink.ItemIndex := i-1
      else if (lbSource.Items.Count > 0) then begin
        lbSource.SetFocus;
        lbSource.ItemIndex := 0
      end;
    end;
    EnableSpdBtns;
  end;
end;

{ Wählt alle strings ab                                }
{------------------------------------------------------}
procedure TFormListenDialog.DeselectAllStrings;
{------------------------------------------------------}
var
  i : integer;
  s : string;
begin
  for i := 0 to lbSink.Items.Count-1 do begin
    s := lbSink.Items[i];
    if (lbSource.Items.IndexOf(s) < 0) then lbSource.Items.Add(s);
  end;
  lbSink.Clear;
  EnableSpdBtns;
end;

{------------------------------------------------------}
procedure TFormListenDialog.ListboxDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
{------------------------------------------------------}
begin
  Accept := (Sender is TListBox) and (Source is TListBox) and
    (TListBox(Source).ItemIndex >= 0) and (Sender <> Source);
end;

{------------------------------------------------------}
procedure TFormListenDialog.ListboxDragDrop(Sender, Source: TObject; X,
  Y: Integer);
{------------------------------------------------------}
begin
  if (TListBox(Source).Name = lbSource.Name) then SelectString
  else if (TListBox(Source).Name = lbSink.Name) then DeselectString;
end;

{------------------------------------------------------}
procedure TFormListenDialog.lbSinkKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{------------------------------------------------------}
begin
  if (Key in [VK_Left]) then begin
    DeselectString;
    Key := Ord('0');
  end;
end;

{------------------------------------------------------}
procedure TFormListenDialog.lbSourceKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{------------------------------------------------------}
begin
  if (Key in [VK_RIGHT]) then begin
    SelectString;
    Key := Ord('0');
  end;
end;

end.
