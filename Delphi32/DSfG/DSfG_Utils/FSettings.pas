{------------------------------------------------------------------------------}
{ Einstellungen (Registry) für DSfG-ActiveX-Komponenten                        }
{                                                                              }
{ 13.07.2000  GD    Neu                                                        }
{ 24.07.2000  GD    Erweiterung um Zugangscode2 für Parametrierung             }
{ 31.10.2000  GD    Erweiterung um Zugangscode1 für Parametrierung             }
{ 27.11.2000  GD    Erweiterung um Einstellung der Busadresse                  }
{ 30.01.2001  GD    Zugangscodes herausgenommen                                }
{ 28.10.2003  GD    Auswahlliste um höhere Baudraten ergänzt (dfm)             }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, 2003                                    }
{------------------------------------------------------------------------------}
unit FSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, MyIni, Spin;

type
  TFormEinstellungen = class(TForm)
    PageControl: TPageControl;
    pnButtons: TPanel;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    tsheetEinstellungen: TTabSheet;
    pnEinstellungen: TPanel;
    cbBaudRate: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    speTimeOut: TSpinEdit;
    Label5: TLabel;
    eCardAddress: TEdit;
    Label3: TLabel;
    seCom: TSpinEdit;
    procedure bbtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure LoadSettings;
    procedure SaveSettings;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

{----------------------------------------------------------------------------}
procedure TFormEinstellungen.FormCreate(Sender: TObject);
{----------------------------------------------------------------------------}
begin
  PageControl.ActivePageIndex := 0;
  LoadSettings;
end;

{----------------------------------------------------------------------------}
procedure TFormEinstellungen.bbtnOkClick(Sender: TObject);
{----------------------------------------------------------------------------}
begin
  SaveSettings;
end;

{----------------------------------------------------------------------------}
procedure TFormEinstellungen.LoadSettings;
{----------------------------------------------------------------------------}
begin
  cbBaudRate.ItemIndex :=
    cbBaudRate.Items.IndexOf(IntToStr(DSfGActiveXIniFile.BaudRate));
  speTimeOut.Value := DSfGActiveXIniFile.TimeOut div 1000;
  eCardAddress.Text := DSfGActiveXIniFile.CardAddress;
  seCom.Value := DSfGActiveXIniFile.DPAComPort;
end;

{----------------------------------------------------------------------------}
procedure TFormEinstellungen.SaveSettings;
{----------------------------------------------------------------------------}
begin
  if (cbBaudRate.ItemIndex >= 0) then
    DSfGActiveXIniFile.BaudRate :=
      StrToInt(cbBaudRate.Items[cbBaudRate.ItemIndex]);
  DSfGActiveXIniFile.TimeOut := speTimeOut.Value * 1000;
  if (Length(eCardAddress.Text) = 0) or
     (not (eCardAddress.Text[1] in ['A'..'_'])) then eCardAddress.Text := '0';
  DSfGActiveXIniFile.CardAddress := eCardAddress.Text[1];
  DSfGActiveXIniFile.DPAComPort := seCom.Value;
end;

end.
