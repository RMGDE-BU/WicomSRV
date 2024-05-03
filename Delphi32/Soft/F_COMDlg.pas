{******************************************************************************}
{* Unit: Einfacher COM-Dialog                                                 *}
{* 17.11.2009  WW                                                             *}
{******************************************************************************}
unit F_COMDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Serial;

resourcestring
  SInitSchnittstelle = 'Schnittstelle initialisieren...';
  SFehlerInitSchnittstelle = 'Schnittstelle konnte nicht initialisiert werden';
  SFehlerSendReceive = 'Fehler beim Senden/Empfangen aufgetreten';

type
  TFormCOMDlg = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    bbtnBeenden: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
  private
    { Private-Deklarationen }
    FCOM: integer;
    FBaudrate: integer;
    FDataBits: TDataBits;
    FParityBit: TParityBit;
    FStopBits: TStopBits;
    procedure SetCOM (const ACOM: integer);
    procedure SetBaudrate (const ABaudrate: integer);
    procedure SetDataBits (const ADataBits: TDataBits);
    procedure SetParityBit (const AParityBit: TParityBit);
    procedure SetStopBits (const AStopBits: TStopBits);
  public
    { Public-Deklarationen }
    property COM: integer read FCOM write SetCOM;
    property Baudrate: integer read FBaudrate write SetBaudrate;
    property DataBits: TDataBits read FDataBits write SetDataBits;
    property ParityBit: TParityBit read FParityBit write SetParityBit;
    property StopBits: TStopBits read FStopBits write SetStopBits;
  end;

implementation

{$R *.DFM}

{------------------------------------------------}
procedure TFormCOMDlg.FormCreate(Sender: TObject);
{------------------------------------------------}
begin
  FCOM:=1;
  FBaudrate:=9600;
  FDataBits:=db_8;
  FParityBit:=none;
  FStopBits:=sb_1;

  bbtnBeenden.Enabled:=false;
end;

{--------------------------------------------------}
procedure TFormCOMDlg.FormActivate(Sender: TObject);
{--------------------------------------------------}
begin
  bbtnBeenden.Enabled:=true;
end;

{---------------------------------------------------------------------------}
procedure TFormCOMDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
{---------------------------------------------------------------------------}
begin
  CanClose:=bbtnBeenden.Enabled;
end;

{-------------------------------------------------}
procedure TFormCOMDlg.SetCOM (const ACOM: integer);
{-------------------------------------------------}
begin
  FCOM:=ACOM;
end;

{-----------------------------------------------------------}
procedure TFormCOMDlg.SetBaudrate (const ABaudrate: integer);
{-----------------------------------------------------------}
begin
  FBaudrate:=ABaudrate;
end;

{-------------------------------------------------------------}
procedure TFormCOMDlg.SetDataBits (const ADataBits: TDataBits);
{-------------------------------------------------------------}
begin
  FDataBits:=ADataBits;
end;

{----------------------------------------------------------------}
procedure TFormCOMDlg.SetParityBit (const AParityBit: TParityBit);
{----------------------------------------------------------------}
begin
  FParityBit:=AParityBit;
end;

{-------------------------------------------------------------}
procedure TFormCOMDlg.SetStopBits (const AStopBits: TStopBits);
{-------------------------------------------------------------}
begin
  FStopBits:=AStopBits;
end;

end.
