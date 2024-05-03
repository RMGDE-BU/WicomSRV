unit FZeitbereichDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FDlgDef, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TFormZeitbereichDialog = class(TFormDialogDefault)
    pnBottomBtnCancel: TPanel;
    bbtnCancel: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    dtpDatumVon: TDateTimePicker;
    dtpZeitVon: TDateTimePicker;
    dtpDatumBis: TDateTimePicker;
    dtpZeitBis: TDateTimePicker;
  private
    { Private-Deklarationen }
    procedure SetDatumVon(dtVon: TDateTime);
    function GetDatumVon: TDateTime;
    procedure SetDatumBis(dtBis: TDateTime);
    function GetDatumBis: TDateTime;
  public
    { Public-Deklarationen }
    property DatumVon: TDateTime read GetDatumVon write SetDatumVon;
    property DatumBis: TDateTime read GetDatumBis write SetDatumBis;
  end;

implementation

{$R *.dfm}

{------------------------------------------}
procedure TFormZeitbereichDialog.SetDatumVon(dtVon: TDateTime);
{------------------------------------------}
begin
  dtpDatumVon.DateTime := dtVon;
  dtpZeitVon.DateTime := dtVon;
end;

{------------------------------------------}
function TFormZeitbereichDialog.GetDatumVon: TDateTime;
{------------------------------------------}
begin
  Result := Trunc(dtpDatumVon.Date) + Frac(dtpZeitVon.Time);
end;

{------------------------------------------}
procedure TFormZeitbereichDialog.SetDatumBis(dtBis: TDateTime);
{------------------------------------------}
begin
  dtpDatumBis.DateTime := dtBis;
  dtpZeitBis.DateTime := dtBis;
end;

{------------------------------------------}
function TFormZeitbereichDialog.GetDatumBis: TDateTime;
{------------------------------------------}
begin
  Result := Trunc(dtpDatumBis.Date) + Frac(dtpZeitBis.Time);
end;

end.
