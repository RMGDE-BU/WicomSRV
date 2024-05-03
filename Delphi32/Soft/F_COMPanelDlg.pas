{******************************************************************************}
{* Unit: Einfacher COM-Dialog mit Anzeige-Panel                               *}
{* 17.11.2009  WW                                                             *}
{******************************************************************************}
unit F_COMPanelDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, F_COMDlg, StdCtrls, Buttons, ExtCtrls;

type
  TFormCOMPanelDlg = class(TFormCOMDlg)
    lStatus: TLabel;
    pStatus: TPanel;
  private
    { Private-Deklarationen }
    procedure SetStatusPanel (const AText: string);
  public
    { Public-Deklarationen }
    property Status: string write SetStatusPanel;
  end;

implementation

{$R *.dfm}

{--------------------------------------------------------------}
procedure TFormCOMPanelDlg.SetStatusPanel (const AText: string);
{--------------------------------------------------------------}
begin
  pStatus.Caption:=AText;
end;

end.
 