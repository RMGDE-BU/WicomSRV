{******************************************************************************}
{* Unit: Eingabe-Dialog für GSM-Modeminitialisierung                          *}
{* 04.12.2009  WW                                                             *}
{******************************************************************************}
unit F_GSMModemInitDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, F_COMDlg, StdCtrls, Buttons, ExtCtrls;

type
  TFormGSMModemInitDlg = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    bbtnAbbruch: TBitBtn;
    bbtnOK: TBitBtn;
    lInfo: TLabel;
    gbModem: TGroupBox;
    lReset: TLabel;
    eReset: TEdit;
    eInit: TEdit;
    lInit: TLabel;
    lPIN: TLabel;
    ePIN: TEdit;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

end.
 