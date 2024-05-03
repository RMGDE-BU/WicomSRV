{******************************************************************************}
{* Unit: Einfacher COM-Dialog mit Anzeige-Panel und -Liste                    *}
{* 18.11.2009  WW                                                             *}
{******************************************************************************}
unit F_COMListDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, F_COMPanelDlg, StdCtrls, Buttons, ExtCtrls, Menus, Clipbrd;

type
  TFormCOMListDlg = class(TFormCOMPanelDlg)
    lbDaten: TListBox;
    lDaten: TLabel;
    pmDaten: TPopupMenu;
    miClipboard: TMenuItem;
    procedure miClipboardClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FClipboard: TClipboard;
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

{----------------------------------------------------------}
procedure TFormCOMListDlg.miClipboardClick(Sender: TObject);
{----------------------------------------------------------}
begin
  FClipboard:=Clipboard;
  FClipboard.AsText:=lbDaten.Items.Text;
end;

end.
