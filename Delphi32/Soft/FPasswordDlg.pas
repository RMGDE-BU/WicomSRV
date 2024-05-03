unit FPasswordDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FDlgDef, StdCtrls, Buttons, ExtCtrls;

type
  TFormPassworteingabe = class(TFormDialogDefault)
    Panel1: TPanel;
    bbtnCancel: TBitBtn;
    ePassword: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ePasswordChange(Sender: TObject);
  private
    { Private-Deklarationen }
    FLength : integer;
    procedure SetChars(cChar: char);
    procedure SetMaxLength(iLength: integer);
    procedure SetLength(iLength: integer);
    procedure SetPassword(const sPW: string);
    function GetPassword: string;
  public
    { Public-Deklarationen }
    property Chars: char write SetChars;
    property MaxLength: integer write SetMaxLength;
    property MustLength: integer write SetLength;
    property Password: string read GetPassword write SetPassword;
  end;

implementation

{$R *.dfm}

procedure TFormPassworteingabe.FormCreate(Sender: TObject);
begin
  ePassword.PasswordChar := '*';
  FLength := 0;
  ePassword.Text := ''
end;

procedure TFormPassworteingabe.SetChars(cChar: char);
begin
  ePassword.PasswordChar := cChar;
end;

procedure TFormPassworteingabe.SetMaxLength(iLength: integer);
begin
  if (iLength > 0) then ePassword.MaxLength := iLength;
end;

procedure TFormPassworteingabe.SetLength(iLength: integer);
begin
  FLength := iLength;
  if (FLength > 0) then ePassword.MaxLength := FLength;
  bbtnOk.Enabled := (FLength = 0) or (Length(ePassword.Text) = FLength);
end;

procedure TFormPassworteingabe.SetPassword(const sPW: string);
begin
  ePassword.Text := sPW;
end;

function TFormPassworteingabe.GetPassword: string;
begin
  Result := ePassword.Text;
end;

procedure TFormPassworteingabe.ePasswordChange(Sender: TObject);
begin
   bbtnOk.Enabled := (FLength = 0) or (Length(ePassword.Text) = FLength);
end;

end.
