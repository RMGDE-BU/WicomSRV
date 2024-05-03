unit Flogin;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TFormLogIn = class(TForm)
    BTOk: TBitBtn;
    Label1: TLabel;
    BitBtn2: TBitBtn;
    EdPasswort: TEdit;
    procedure EdPasswortChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    function GetPasswort: string;
  end;

var
  FormLogIn: TFormLogIn;

implementation

{$R *.DFM}

procedure TFormLogIn.EdPasswortChange(Sender: TObject);
begin
  BTOk.Enabled := Length (EdPasswort.Text) > 0;
end;

function TFormLogin.GetPasswort: string;
begin
  Result := EdPasswort.text;
end;

procedure TFormLogIn.FormShow(Sender: TObject);
begin
  EdPasswort.text := '';
  EdPasswort.SetFocus;
end;

end.
