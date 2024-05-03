{------------------------------------------------------------------------------}
{ Allgemeiner Auswahldialog                                                    }
{                                                                              }
{ 12.04.2001  GD  Neu                                                          }
{ 02.12.2011  GD  Stay on top                                                  }
{ 14.11.2022  WW  AuswahlList veröffentlicht                                   }
{ 19.10.2023  WW  Erweitert für Combobox-Listenobjekte (SetAuswahlListWith-    }
{                 Objects), Fenster breiter, DropDownCount erhöht auf 16       }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, RMG Messtechnik GmbH 2023               }
{------------------------------------------------------------------------------}
unit FAuswahlDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TFormAuswahlDlg = class(TForm)
    pnBottom: TPanel;
    pnClient: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    cbAuswahl: TComboBox;
    lAuswahl: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
    function GetAuswCaption(iIndex: byte): string;
    procedure SetAuswCaption(iIndex: byte; sCaption: string);
    function GetAuswahl: string;
    procedure SetAuswahl(sAuswahl: string);
    function GetAuswahlList: TStrings;
    procedure SetAuswahlList(pSl: TStrings);
    procedure SetDropDownStyle(Value: TComboBoxStyle);
  public
    { Public-Deklarationen }
    procedure SetAuswahlListWithObjects(pSl: TStrings);
    property DropDownStyle: TComboBoxStyle write SetDropDownStyle;
    property AuswahlCaption [iIndex: byte]: string
      read GetAuswCaption write SetAuswCaption;
    property Auswahl: string read GetAuswahl write SetAuswahl;
    property AuswahlList: TStrings read GetAuswahlList write SetAuswahlList;
  end;

implementation

{$R *.DFM}

{------------------------------------}
procedure TFormAuswahlDlg.FormShow(Sender: TObject);
{------------------------------------}
begin
  cbAuswahl.SetFocus;
end;

{------------------------------------}
function TFormAuswahlDlg.GetAuswCaption(iIndex: byte): string;
{------------------------------------}
begin
  case iIndex of
    1 : Result := Self.Caption;
    2 : Result := lAuswahl.Caption;
    else Result := '';
  end;
end;

{------------------------------------}
procedure TFormAuswahlDlg.SetAuswCaption(iIndex: byte; sCaption: string);
{------------------------------------}
begin
  case iIndex of
    1 : Self.Caption := sCaption;
    2 : lAuswahl.Caption := sCaption;
  end;
end;

{------------------------------------}
function TFormAuswahlDlg.GetAuswahl: string;
{------------------------------------}
begin
  Result := cbAuswahl.Text;
end;

{------------------------------------}
procedure TFormAuswahlDlg.SetAuswahl(sAuswahl: string);
{------------------------------------}
begin
  cbAuswahl.ItemIndex := cbAuswahl.Items.IndexOf(sAuswahl);
  if (cbAuswahl.ItemIndex < 0) then cbAuswahl.Text := sAuswahl;
end;

// 14.11.2022, WW
{------------------------------------}
procedure TFormAuswahlDlg.SetDropDownStyle(Value: TComboBoxStyle);
{------------------------------------}
begin
  if (cbAuswahl.Style <> Value) then cbAuswahl.Style := Value;
end;

{------------------------------------}
function TFormAuswahlDlg.GetAuswahlList: TStrings;
{------------------------------------}
begin
  Result := cbAuswahl.Items;
end;

{------------------------------------}
procedure TFormAuswahlDlg.SetAuswahlList(pSl: TStrings);
{------------------------------------}
begin
  cbAuswahl.Items.CommaText := pSl.CommaText;
  if (cbAuswahl.Items.Count > 0) then cbAuswahl.ItemIndex := 0;
end;

// 19.10.2023, WW
{------------------------------------}
procedure TFormAuswahlDlg.SetAuswahlListWithObjects(pSl: TStrings);
{------------------------------------}
begin
  cbAuswahl.Items.Assign (pSl);
  if (cbAuswahl.Items.Count > 0) then cbAuswahl.ItemIndex := 0;
end;

end.
