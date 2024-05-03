unit Fpwedit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TFormPWEdit = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    EdPasswort2: TEdit;
    BTOk: TBitBtn;
    BTCancel: TBitBtn;
    EdPasswort1: TEdit;
    procedure EdPasswortChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure UpdateBTOk;
  public
    function GetPasswort: string;
  end;

var
  FormPWEdit: TFormPWEdit;

implementation

{$R *.DFM}

{------------------------------------------------------}
procedure TFormPWEdit.EdPasswortChange(Sender: TObject);
{------------------------------------------------------}
begin
  UpdateBTOk;
end;

{-------------------------------}
procedure TFormPWEdit.UpdateBTOk;
{-------------------------------}
begin
  BTOk.Enabled :=
    (Length (EDPasswort1.Text) > 0) and
    (Length (EDPasswort2.Text) > 0) and
    (CompareText (EDPasswort1.Text, EdPasswort2.Text) = 0);
end;

{---------------------------------------}
function TFormPWEdit.GetPasswort: string;
{---------------------------------------}
begin
  Result := EDPasswort1.Text;
end;

procedure TFormPWEdit.FormActivate(Sender: TObject);
begin
  EdPasswort1.SetFocus;
end;

end.
