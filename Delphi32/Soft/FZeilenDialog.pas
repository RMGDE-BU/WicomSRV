{------------------------------------------------------------------------------}
{ Zeilenweise Dialogeingabe                                                    }
{                                                                              }
{ 19.04.2002  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2002                                          }
{------------------------------------------------------------------------------}
unit FZeilenDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFormZeilenDialog = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
    FComponentList : TList;
    FResultList    : TStrings;
    procedure EditChange(Sender: TObject);
  protected
    { Private-Deklarationen }
    property ComponentList: TList read FComponentList;
    property ResultList: TStrings read FResultList;
  public
    { Public-Deklarationen }
    procedure SetEingabeList(pSlEingabe, pSlResult: TStrings); virtual;
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
procedure TFormZeilenDialog.FormCreate(Sender: TObject);
{----------------------------------------------}
begin
  FComponentList := TList.Create;
end;

{----------------------------------------------}
procedure TFormZeilenDialog.FormDestroy(Sender: TObject);
{----------------------------------------------}
var
  i : integer;
begin
  for i := 0 to FComponentList.Count-1 do TObject(FComponentList[i]).Free;
  FComponentList.Free;
end;

{----------------------------------------------}
procedure TFormZeilenDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
{----------------------------------------------}
begin
  Action := caFree;
end;

{----------------------------------------------}
procedure TFormZeilenDialog.EditChange(Sender: TObject);
{----------------------------------------------}
begin
  if (Sender is TEdit) then
    if (FResultList.Count > TEdit(Sender).Tag) then
      FResultList[TEdit(Sender).Tag] := TEdit(Sender).Text;
end;

{----------------------------------------------}
procedure TFormZeilenDialog.SetEingabeList(pSlEingabe, pSlResult: TStrings);
{----------------------------------------------}
var
  i      : integer;
  pLabel : TLabel;
  pEdit  : TEdit;
begin
  FResultList := pSlResult;
  for i := 0 to pSlEingabe.Count-1 do begin
    if (pSlResult.Count <= i) then pSlResult.Add('');

    pLabel := TLabel.Create(Self);
    pLabel.Parent := pnClient;
    pLabel.Left := C_Label_Left;
    pLabel.Top := C_Label_Top + (i * C_Line_Diff);
    pLabel.Caption := pSlEingabe[i];
    FComponentList.Add(pLabel);

    pEdit := TEdit.Create(Self);
    pEdit.Parent := pnClient;
    pEdit.Left := C_Edit_Left;
    pEdit.Top := C_Edit_Top + (i * C_Line_Diff);
    pEdit.Width := C_Edit_Width;
    pEdit.Text := pSlResult[i];
    pEdit.Tag := i;
    pEdit.OnChange := EditChange;
    FComponentList.Add(pEdit);
  end;

  Self.ClientHeight :=
    pnBottom.Height + (C_Label_Top) + (pSlEingabe.Count * C_Line_Diff);
end;

end.
