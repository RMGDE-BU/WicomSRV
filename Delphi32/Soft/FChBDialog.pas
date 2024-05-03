{------------------------------------------------------------------------------}
{ Zeilenweise Dialogeingabe                                                    }
{                                                                              }
{ 19.04.2002  GD  Neu                                                          }
{ 09.03.2015  GD  Action auf caHide geändert, sonst Freigabe nach Schalter     }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2002                                          }
{------------------------------------------------------------------------------}
unit FChBDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, CheckLst;

type
  TFormCheckBoxDialog = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    CheckListBox: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
    function GetChecked(iIndex: integer): boolean;
    procedure SetChecked(iIndex: integer; bValue: boolean);
  protected
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure SetEingabeList(const sLines: string); virtual;
    property IsChecked [iIndex: integer]: boolean
      read GetChecked write SetChecked;
  end;

implementation

{$R *.DFM}

const
  C_Label_Left      = 8;
  C_Label_Top       = 20;

  C_Edit_Left       = 128;
  C_Edit_Top        = 16;
  C_Edit_Width      = 240;

  C_Line_Diff       = 30;

{----------------------------------------------}
procedure TFormCheckBoxDialog.FormCreate(Sender: TObject);
{----------------------------------------------}
begin
//
end;

{----------------------------------------------}
procedure TFormCheckBoxDialog.FormDestroy(Sender: TObject);
{----------------------------------------------}
begin
//
end;

{----------------------------------------------}
procedure TFormCheckBoxDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
{----------------------------------------------}
begin
  Action := caHide;  // 09.03.2015
end;

{----------------------------------------------}
procedure TFormCheckBoxDialog.SetEingabeList(const sLines: string);
{----------------------------------------------}
begin
  CheckListBox.Items.CommaText := sLines;
end;

{----------------------------------------------}
function TFormCheckBoxDialog.GetChecked(iIndex: integer): boolean;
{----------------------------------------------}
begin
  if (iIndex >= 0) and (iIndex < CheckListBox.Count)
  then Result := CheckListBox.Checked[iIndex]
  else Result := False;
end;

{----------------------------------------------}
procedure TFormCheckBoxDialog.SetChecked(iIndex: integer; bValue: boolean);
{----------------------------------------------}
begin
  if (iIndex >= 0) and (iIndex < CheckListBox.Count) then
    CheckListBox.Checked[iIndex] := bValue;
end;

end.
