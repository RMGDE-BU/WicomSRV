{------------------------------------------------------------------------------}
{ Grid mit Checkboxen bei Überschriften                                        }
{                                                                              }
{ 08.08.2001  GD  Neu                                                          }
{ 11.07.2002  GD  InsertCol                                                    }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, 2002                                    }
{------------------------------------------------------------------------------}
unit GD_Grid;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ComCtrls, ExtCtrls, Forms,
  Menus, dialogs, Variants,
  GD_Utils;

type
  TCheckClicked = procedure(iIndex: integer) of object;

  TCheckGrid = class;

  TMyCheckList = class(TList)
    constructor Create(pListView: TCheckGrid);
  private
    FListView : TCheckGrid;
    procedure SetCheckVisible(iIndex: integer; bState: boolean);
    procedure SetChecked(iIndex: integer; bState: boolean);
    function GetChecked(iIndex: integer): boolean;
  protected
  public
    function GetCheckedCount: integer;
    property CheckVisible [iIndex: integer]: boolean write SetCheckVisible;
    property Checked [iIndex: integer]: boolean read GetChecked write SetChecked;
  end;

  TCheckGrid = class(TListview)
    constructor Create(pOwner: TComponent); override;
    destructor Destroy; override;
  private
    FPopUp      : TPopupMenu;
    FCheckList  : TMyCheckList;
    FCheckClicked : TCheckClicked;
    FUpdating     : boolean;
    procedure ResizeGrid(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure SetCheckVisible(iIndex: integer; bState: boolean);
    procedure SetChecked(iIndex: integer; bState: boolean);
    function GetChecked(iIndex: integer): boolean;
  protected
  public
    procedure Add(sCaption: string);
    procedure Insert(iIndex: Integer; sCaption: string);
    procedure AddCol(sCaption: string; pSlCol: TStrings);
    procedure InsertCol(iIndex: Integer; sCaption: string; pSlCol: TStrings);
    procedure DeleteCol(iIndex: integer); overload;
    procedure Append;
    procedure AppendRecord(const pValues: array of const);
    function GetList(sSeparator: char = #9; bChecked: boolean = True): TStrings;
    property CheckVisible [iIndex: integer]: boolean write SetCheckVisible;
    property Checked [iIndex: integer]: boolean read GetChecked write SetChecked;
  published
    property CheckClicked: TCheckClicked read FCheckClicked write FCheckClicked;
  end;

implementation

const
  C_State_NoCheck = 0;
  C_State_UnChecked = 1;
  C_State_Checked = 2;


{---------------------------------- TMyCheckList ------------------------------}

{------------------------------------------------------}
constructor TMyCheckList.Create(pListView: TCheckGrid);
{------------------------------------------------------}
begin
  inherited Create;

  FListView := pListView;
end;

{------------------------------------------------------}
procedure TMyCheckList.SetCheckVisible(iIndex: integer; bState: boolean);
{------------------------------------------------------}
begin
  if (bState)
  then Items[iIndex] := Pointer(C_State_Checked)
  else Items[iIndex] := Pointer(C_State_NoCheck);
end;

{------------------------------------------------------}
procedure TMyCheckList.SetChecked(iIndex: integer; bState: boolean);
{------------------------------------------------------}
begin
  if (Integer(Items[iIndex]) <> C_State_NoCheck) then
    Items[iIndex] := Pointer(Integer(bState)+1);
  FListView.ResizeGrid(nil);
end;

{------------------------------------------------------}
function TMyCheckList.GetChecked(iIndex: integer): boolean;
{------------------------------------------------------}
begin
  if (Integer(Items[iIndex]) <> C_State_NoCheck)
  then Result := Boolean(Integer(Items[iIndex])-1)
  else Result := True;
end;

{------------------------------------------------------}
function TMyCheckList.GetCheckedCount: integer;
{------------------------------------------------------}
var
  i : integer;
begin
  Result := 0;
  for i := 0 to Count-1 do if (Checked[i]) then Inc(Result);
end;

{----------------------------------- TCheckGrid -------------------------------}

{------------------------------------------------------}
constructor TCheckGrid.Create(pOwner: TComponent);
{------------------------------------------------------}
begin
  inherited;

  if (pOwner is TWinControl) then begin
    Parent := TWinControl(pOwner);
    Align := alClient;
  end;

  Self.BorderStyle := bsNone;
  ViewStyle := vsReport;
  GridLines := True;
//  OnCustomDrawSubItem := CustomDrawSubItem;
  HideSelection := False;
  RowSelect := True;

  OnResize := ResizeGrid;
  FPopUp := TPopupMenu.Create(Self);
  FPopUp.AutoPopup := True;
  PopupMenu := FPopUp;
  FCheckList := TMyCheckList.Create(Self);
  FCheckClicked := nil;
  FUpdating := False;
end;

{------------------------------------------------------}
destructor TCheckGrid.Destroy;
{------------------------------------------------------}
begin
  FPopUp.Free;
  FCheckList.Free;

  inherited;
end;

{ Ereignis beim Ändern des Grids                       }
{ Parameter: Aufrufendes Objekt                        }
{------------------------------------------------------}
procedure TCheckGrid.ResizeGrid(Sender: TObject);
{------------------------------------------------------}
var
  i, j, iMinWidth, iWidth : integer;
begin
  if (FUpdating) or (csDestroying in ComponentState) then Exit;
  iWidth := (ClientWidth - GetSystemMetrics(SM_CXVSCROLL)) div
    FCheckList.GetCheckedCount;
  for i := 0 to Columns.Count-1 do
    if (Checked[i]) then begin
      iMinWidth := EstimatedTextLengthOf(Columns[i].Caption, Font);
      if (iMinWidth > iWidth) then j := iMinWidth else j := iWidth;
      Columns[i].Width := j;
    end
    else Columns[i].Width := 0;
end;

{ Fügt neue Spalte ein                                 }
{ Parameter: Spaltenüberschrift                        }
{------------------------------------------------------}
procedure TCheckGrid.Add(sCaption: string);
{------------------------------------------------------}
var
  i, j : integer;
  pCol : TListColumn;
begin
  FUpdating := True;
  Items.BeginUpdate;
  Columns.BeginUpdate;
  try
    pCol := Columns.Add;
    pCol.Caption := sCaption;

    Application.ProcessMessages;
    for i := 0 to Items.Count-1 do
      for j := Items[i].SubItems.Count to Columns.Count-2 do
        Items[i].SubItems.Add('');

    FPopUp.Items.Add(
      NewItem(sCaption, 0, True, True, MenuClick, 0, 'mi' + IntToStr(Columns.Count)));
    with FPopUp.Items[FPopUp.Items.Count-1] do Tag := Columns.Count;

    FCheckList.Add(Pointer(C_State_Checked));

    Application.ProcessMessages;
  finally
    Items.EndUpdate;
    Columns.EndUpdate;
    FUpdating := False;
  end;
  ResizeGrid(nil);
end;

{ Löscht vorhandene Spalte                             }
{ Parameter: Index der Spalte (<> 0)                   }
{------------------------------------------------------}
procedure TCheckGrid.DeleteCol(iIndex: integer);
{------------------------------------------------------}
var
  i : integer;
begin
  Items.BeginUpdate;
  Columns.BeginUpdate;
  try
    for i := 0 to Items.Count-1 do Items[i].SubItems.Delete(iIndex-1);
    Columns.Delete(iIndex);
    FPopUp.Items.Delete(iIndex);
    FCheckList.Delete(iIndex);
  finally
    Items.EndUpdate;
    Columns.EndUpdate;
  end;
end;

{ Fügt neue Spalte ein                                 }
{ Parameter: Spaltenüberschrift                        }
{------------------------------------------------------}
procedure TCheckGrid.Insert(iIndex: integer; sCaption: string);
{------------------------------------------------------}
var
  i, j : integer;
begin
  if (iIndex = 0) then Exit;

  FUpdating := True;
  Items.BeginUpdate;
  Columns.BeginUpdate;
  try
    // Neue Spalte am Ende
    Columns.Add;
    for i := 0 to Items.Count-1 do
      for j := Items[i].SubItems.Count to Columns.Count-2 do
        Items[i].SubItems.Add('');

    // Inhalt der Spalten bis iIndex verschieben
    for i := Columns.Count-1 downto iIndex+1 do begin
      Columns[i].Caption := Columns[i-1].Caption;
      for j := 0 to Items.Count-1 do
        Items[j].SubItems[i-1] := Items[j].SubItems[i-2];
    end;

    // Spalte iIndex mit Überschrift versehen und leeren
    Columns[iIndex].Caption := sCaption;
    for i := 0 to Items.Count-1 do
      Items[i].SubItems[iIndex-1] := '';

    Application.ProcessMessages;

    FPopUp.Items.Insert(iIndex,
      NewItem(sCaption, 0, True, True, MenuClick, 0, 'mi' + IntToStr(Columns.Count)));
    with FPopUp.Items[FPopUp.Items.Count-1] do Tag := Columns.Count;

    FCheckList.Insert(iIndex, Pointer(C_State_Checked));

    Application.ProcessMessages;
  finally
    Items.EndUpdate;
    Columns.EndUpdate;
    FUpdating := False;
  end;
  ResizeGrid(nil);
end;

{ Gibt Liste mit Tabelleninhalt zurück                 }
{ Parameter: Trennzeichen, nur 'Checked'               }
{ Rückgabe: Stringliste mit Inhalten                   }
{------------------------------------------------------}
function TCheckGrid.GetList(
  sSeparator: char = #9; bChecked: boolean = True): TStrings;
{------------------------------------------------------}
var
  i, j : integer;
  s    : string;
begin
 Result := TStringlist.Create;

 // Spaltenüberschriften in Liste eintragen
 s := '';
 for i := 0 to Columns.Count-1 do
   if (not bChecked) or (Checked[i]) then
     s := s + Columns[i].Caption + sSeparator;
 if (s <> '') then System.Delete(s, Length(s), 1);
 Result.Add(s);

 // Zeilen in Liste eintragen
 for i := 0 to Items.Count-1 do begin
   s := Items[i].Caption + sSeparator;
   for j := 0 to Items[i].SubItems.Count-1 do
     if (not bChecked) or (Checked[j+1]) then
       s := s + Items[i].SubItems[j] + sSeparator;
   if (s <> '') then System.Delete(s, Length(s), 1);
   Result.Add(s);
 end;
end;

{------------------------------------------------------}
procedure TCheckGrid.MenuClick(Sender: TObject);
{------------------------------------------------------}
begin
  if (Sender is TMenuItem) and (TMenuItem(Sender).Tag > 0) then begin
    TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
    Checked[TMenuItem(Sender).Tag-1] := TMenuItem(Sender).Checked;
    if (Assigned(FCheckClicked)) then FCheckClicked(TMenuItem(Sender).Tag-1);
  end;
end;

{------------------------------------------------------}
procedure TCheckGrid.SetCheckVisible(iIndex: integer; bState: boolean);
{------------------------------------------------------}
begin
  FCheckList.CheckVisible[iIndex] := bState;
  FPopUp.Items[iIndex].Enabled := bState;
end;

{------------------------------------------------------}
procedure TCheckGrid.SetChecked(iIndex: integer; bState: boolean);
{------------------------------------------------------}
begin
  FCheckList.Checked[iIndex] := bState;
end;

{------------------------------------------------------}
function TCheckGrid.GetChecked(iIndex: integer): boolean;
{------------------------------------------------------}
begin
  Result := FCheckList.Checked[iIndex];
end;

{ Fügt neue Zeile in Gitter ein                        }
{ Parameter: Array mit Werten                          }
{------------------------------------------------------}
procedure TCheckGrid.AppendRecord(const pValues: array of const);
{------------------------------------------------------}
const
  BoolChars: array[Boolean] of Char = ('F', 'T');
var
  i : integer;
  s : string;
  p : TListItem;
begin
  Items.BeginUpdate;
  Columns.BeginUpdate;
  try
    p := Items.Add;
    for i := Low(pValues) to High(pValues) do begin
      s := '';
      case pValues[i].VType of
        vtInteger:    s := IntToStr(pValues[i].VInteger);
        vtBoolean:    s := BoolChars[pValues[i].VBoolean];
        vtChar:       s := pValues[i].VChar;
        vtExtended:   s := FloatToStr(pValues[i].VExtended^);
        vtString:     s := pValues[i].VString^;
        vtPChar:      s := pValues[i].VPChar;
        vtObject:     s := pValues[i].VObject.ClassName;
        vtClass:      s := pValues[i].VClass.ClassName;
        vtAnsiString: s := string(pValues[i].VAnsiString);
        vtCurrency:   s := CurrToStr(pValues[i].VCurrency^);
        vtVariant:    if (pValues[i].VVariant^ <> NULL) then
          s := string(pValues[i].VVariant^);
        vtInt64:      s := IntToStr(pValues[i].VInt64^);
      end;

      if ((i-Low(pValues)) = 0) then p.Caption := s else p.SubItems.Add(s);
    end;

    // restliche Spalten auffüllen
    for i := (High(pValues) - Low(pValues) + 1) to Columns.Count-1 do
      if (i = 0) then p.Caption := '' else p.SubItems.Add('');
  finally
    Items.EndUpdate;
    Columns.EndUpdate;
  end;
end;

{ Fügt neue Spalte mit Werten ein                      }
{ Parameter: Spaltenüberschrift, Werteliste            }
{------------------------------------------------------}
procedure TCheckGrid.AddCol(sCaption: string; pSlCol: TStrings);
{------------------------------------------------------}
var
  i, j : integer;
begin
  Items.BeginUpdate;
  Columns.BeginUpdate;
  try
    Add(sCaption);
    j := pSlCol.Count;

    // Eintragen, solange es Zeilen bereits gibt
    for i := 0 to pSlCol.Count-1 do
      if (i >= Items.Count ) then begin
        j := i;
        Break;
      end
      else Items[i].SubItems[Columns.Count-2] := pSlCol[i];

    // Ggf. weitere Zeilen eintragen
    for i := j to pSlCol.Count-1 do begin
      Append;
      Items[i].SubItems[Columns.Count-2] := pSlCol[i];
    end;
  finally
    Items.EndUpdate;
    Columns.EndUpdate;
  end;
end;

{ Fügt neue Spalte mit Werten ein                      }
{ Parameter: Spaltenüberschrift, Werteliste            }
{------------------------------------------------------}
procedure TCheckGrid.InsertCol(
  iIndex: Integer; sCaption: string; pSlCol: TStrings);
{------------------------------------------------------}
var
  i, j : integer;
begin
  Items.BeginUpdate;
  Columns.BeginUpdate;
  try
    if (iIndex = 0) then Exit;

    Insert(iIndex, sCaption);
    j := pSlCol.Count;

    // Eintragen, solange es Zeilen bereits gibt
    for i := 0 to pSlCol.Count-1 do
      if (i >= Items.Count ) then begin
        j := i;
        Break;
      end
      else Items[i].SubItems[iIndex-1] := pSlCol[i];

    // Ggf. weitere Zeilen eintragen
    for i := j to pSlCol.Count-1 do begin
      Append;
      Items[i].SubItems[iIndex-1] := pSlCol[i];
    end;
  finally
    Items.EndUpdate;
    Columns.EndUpdate;
  end;
end;

{ Fügt neue Zeile ohne Werte in Gitter ein             }
{------------------------------------------------------}
procedure TCheckGrid.Append;
{------------------------------------------------------}
var
  i : integer;
begin
  Items.BeginUpdate;
  Columns.BeginUpdate;
  try
    with Items.Add do begin
      Caption := '';
      // restliche Spalten auffüllen
      for i := 1 to Columns.Count-1 do SubItems.Add('');
    end;
  finally
    Items.EndUpdate;
    Columns.EndUpdate;
  end;
end;

end.
