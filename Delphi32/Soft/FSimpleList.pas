{------------------------------------------------------------------------------}
{ Einfacher Listendialog                                                       }
{                                                                              }
{ 09.12.2002  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2002                                          }
{------------------------------------------------------------------------------}
unit FSimpleList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFormSimpleListDlg = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    ListBox: TListBox;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
  private
    { Private-Deklarationen }
    function GetAuswahl: string;
  public
    { Public-Deklarationen }
    procedure SetAuswahl(pList: TStrings);
    property Auswahl: string read GetAuswahl;
  end;

implementation

{$R *.DFM}

{-----------------------------------}
function TFormSimpleListDlg.GetAuswahl: string;
{-----------------------------------}
begin
  if (ListBox.ItemIndex >= 0)
  then Result := ListBox.Items[ListBox.ItemIndex]
  else Result := '';
end;

{-----------------------------------}
procedure TFormSimpleListDlg.SetAuswahl(pList: TStrings);
{-----------------------------------}
begin
  ListBox.Items.Assign(pList);
  if (pList.Count > 0) then ListBox.ItemIndex := 0;
end;

end.
