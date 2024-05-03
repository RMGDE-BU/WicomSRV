{------------------------------------------------------------------------------}
{ Erweiterte zeilenweise Dialogeingabe um Checkboxes                           }
{                                                                              }
{ 11.05.2002  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2002                                          }
{------------------------------------------------------------------------------}
unit FLineChbDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  FZeilenDialog;

type
  TFormLineChbDialog = class(TFormZeilenDialog)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
    FCheckedList: TList;
    procedure CheckBoxChange(Sender: TObject);
    function GetChecked(iIndex: integer): boolean;
    procedure SetChecked(iIndex: integer; bValue: boolean);
  public
    { Public-Deklarationen }
    procedure SetEingabeList(pSlEingabe, pSlResult: TStrings); override;
    property IsChecked [iIndex: integer]: boolean
      read GetChecked write SetChecked;
  end;

implementation

{$R *.DFM}

const
  C_ChB_Left    = 110;
  C_Chb_Top     = 20;
  C_Chb_Width   = 15;

  C_Line_Diff       = 30;

{----------------------------------------------}
procedure TFormLineChbDialog.FormCreate(Sender: TObject);
{----------------------------------------------}
begin
  inherited;

  FCheckedList := TList.Create;
end;

{----------------------------------------------}
procedure TFormLineChbDialog.FormDestroy(Sender: TObject);
{----------------------------------------------}
begin
  FCheckedList.Free;

  inherited;
end;

{----------------------------------------------}
procedure TFormLineChbDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
{----------------------------------------------}
begin
  inherited;

  Action := caHide;
end;

{----------------------------------------------}
function TFormLineChbDialog.GetChecked(iIndex: integer): boolean;
{----------------------------------------------}
var
  i : integer;
begin
  if (iIndex >= 0) then begin
    for i := FCheckedList.Count to iIndex do FCheckedList.Add(TObject(0));
    Result := Boolean(Integer(FCheckedList[iIndex]));
  end
  else Result := False;
end;

{----------------------------------------------}
procedure TFormLineChbDialog.SetChecked(iIndex: integer; bValue: boolean);
{----------------------------------------------}
var
  i : integer;
begin
  if (iIndex >= 0) then begin
    for i := FCheckedList.Count to iIndex do FCheckedList.Add(TObject(0));
    FCheckedList[iIndex] := TObject(Integer(bValue));
  end;
end;

{----------------------------------------------}
procedure TFormLineChbDialog.SetEingabeList(pSlEingabe, pSlResult: TStrings);
{----------------------------------------------}
var
  i         : integer;
  pCheckBox : TCheckBox;
begin
  inherited;

  for i := 0 to pSlEingabe.Count-1 do begin
    if (pSlResult.Count <= i) then pSlResult.Add('');

    pCheckBox := TCheckBox.Create(Self);
    pCheckBox.Parent := pnClient;
    pCheckBox.Left := C_ChB_Left;
    pCheckBox.Top := C_Chb_Top + (i * C_Line_Diff);
    pCheckBox.Width := C_Chb_Width;
    pCheckBox.Caption := '';
    pCheckBox.Tag := i;
    pCheckBox.Checked := True;
    pCheckBox.OnClick := CheckBoxChange;
    IsChecked[i] := True;
    ComponentList.Add(pCheckBox);
  end;
end;

{----------------------------------------------}
procedure TFormLineChbDialog.CheckBoxChange(Sender: TObject);
{----------------------------------------------}
begin
  if (Sender is TCheckBox) then
    IsChecked[TCheckBox(Sender).Tag] := TCheckBox(Sender).Checked;
end;

end.
