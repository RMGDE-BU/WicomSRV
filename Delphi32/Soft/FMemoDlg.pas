{------------------------------------------------------------------------------}
{ Einfacher Memodialog                                                         }
{                                                                              }
{ 11.06.2007  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) RMG Messtechnik GmbH 2007                                      }
{------------------------------------------------------------------------------}
unit FMemoDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFormMemoDlg = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    Memo: TMemo;
  private
    { Private-Deklarationen }
    function GetAuswahl: string;
    procedure SetAuswahl(const sCommaText: string);
  public
    { Public-Deklarationen }
    property Auswahl: string read GetAuswahl write SetAuswahl;
  end;

implementation

{$R *.DFM}

{-----------------------------------}
function TFormMemoDlg.GetAuswahl: string;
{-----------------------------------}
begin
  Result := Memo.Lines.CommaText;
end;

{-----------------------------------}
procedure TFormMemoDlg.SetAuswahl(const sCommaText: string);
{-----------------------------------}
begin
  Memo.Lines.CommaText := sCommaText;
end;

end.
