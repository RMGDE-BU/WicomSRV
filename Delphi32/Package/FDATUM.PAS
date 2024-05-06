unit Fdatum;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Grids, Calendar, ExtCtrls, Spin, SysUtils;

type
  TFormDatum = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    Calendar: TCalendar;
    Label1: TLabel;
    Label2: TLabel;
    CBMonat: TComboBox;
    Label3: TLabel;
    SEJahr: TSpinEdit;
    BtnPCDatum: TBitBtn;
    procedure CBDatumChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnPCDatumClick(Sender: TObject);
  private
    CanDatumChange: Boolean;
  public
  end;

var
  FormDatum: TFormDatum;

implementation

{$R *.DFM}

{-------------------------------------------------}
procedure TFormDatum.CBDatumChange(Sender: TObject);
{-------------------------------------------------}
begin
  if CanDatumChange then
  begin
    Calendar.Day := 1;
    Calendar.Month := CBMonat.ItemIndex + 1;
    Calendar.Year := SEJahr.Value;
  end;
end;

{----------------------------------------------}
procedure TFormDatum.FormCreate(Sender: TObject);
{----------------------------------------------}
begin
  CBMonat.ItemIndex := Calendar.Month - 1;
  SEJahr.Value := Calendar.Year;
  CanDatumChange := True;
end;

{---------------------------------------------------}
procedure TFormDatum.BtnPCDatumClick(Sender: TObject);
{---------------------------------------------------}
var
  Year, Month, Day: Word;
begin
  DecodeDate (Date, Year, Month, Day);
  SEJahr.Value := Year;
  CBMonat.ItemIndex := Month - 1;
  Calendar.CalendarDate := Date;
end;

end.




