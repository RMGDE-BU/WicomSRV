unit Passwort;

interface

uses
  Wintypes, Forms, IniFiles, SysUtils, FLogIn, FPWEdit, Dialogs;

function InitPasswort (const WieserIni: TFileName): Boolean;
function DonePasswort: Boolean;
function PasswortLogged: Boolean;
function LoginPasswort: Boolean;
function LogoutPasswort: Boolean;
function ChangePasswort: Boolean;

implementation

type

  TPasswort = class (TObject)
  private
    FWieserIni: TIniFile;
    FLogged: Boolean;
    function EncodePW (const PW: string): string;
    function DecodePW (const PWCode: string): string;
  protected
    function GetPWCode: string;
    procedure SetPWCode (const PWCode: string);
    function GetPasswort: string;
  public
    constructor Create (const WieserIni: TFileName);
    destructor Destroy; override;
    function LogIn (const PW: string): Boolean;
    procedure LogOut;
    function ChangePW (const NewPW: string): Boolean;
  end;

const
  PW: TPasswort = nil;

  CSectionPasswort = 'Passwort';
  CIdentPasswort   = 'PW';
  CPWDefault = '00006050080801504011000040401100003';

  CEPWError = 'Passwortfehler';

resourcestring
  S_PWEingabe = 'Sie haben ein falsches Paßwort eingegeben !' + #13 +
                'Geben Sie das richtige Paßwort ein oder brechen Sie ab.'


{ TPasswort }

{-----------------------------------------------------}
function TPasswort.EncodePW (const PW: string): string;
{-----------------------------------------------------}
var
  TempCode: string;
  i: LongInt;
  PWLen: LongInt;
  PWInt: LongInt;
  PWCur: LongInt;
begin
  Result := '';

  PWLen := Length (PW);
  TempCode := Format ('%0.3d', [PWLen]);
  for i := 1 to PWLen do
  begin
    PWInt := LongInt (PW [i]);
    TempCode := TempCode + Format ('%0.3d', [PWInt]);
  end;

  PWLen := Length (TempCode);
  PWCur := 0;
  for i := 1 to PWLen do
  begin
    PWInt := StrToInt (TempCode [i]);
    if PWInt > PWCur then
      Result := Result + IntToStr (PWInt - PWCur)
    else
      Result := Result + '0' + IntToStr (PWCur - PWInt);
    PWCur := PWInt;
  end;
end;

{---------------------------------------------------------}
function TPasswort.DecodePW (const PWCode: string): string;
{---------------------------------------------------------}
var
  TempCode: string;
  i: LongInt;
  PWLen: LongInt;
  PWInt: LongInt;
  PWCur: LongInt;
begin
  TempCode := '';
  PWLen := Length (PWCode);
  PWCur := 0;
  i := 0;
  while i < PWLen do
  begin
    Inc (i);
    PWInt := StrToInt (PWCode [i]);
    if PWInt = 0 then
    begin
      if (i < PWLen) then
      begin
        Inc (i);
        PWInt := StrToInt (PWCode [i]);
        PWCur := PWCur - PWInt;
      end
      else
        raise EAbort.Create (CEPWError);
    end
    else
      PWCur := PWCur + PWInt;
    if (PWCur >= 0) and (PWCur <= 9) then
      TempCode := TempCode + IntToStr (PWCur)
    else
      raise EAbort.Create (CEPWError);
  end;

  Result := '';
  PWLen := 0;
  i := 0;
  while (i < (Length (TempCode) - 2)) do
  begin
    PWInt := StrToInt (Copy (TempCode, i + 1, 3));
    if (i = 0) then
      PWLen := PWInt
    else
    begin
      if (PWInt >= 0) and (PWInt <= 255) then
        Result := Result + Char (PWInt)
      else
        raise EAbort.Create (CEPWError);
    end;
    Inc (i, 3);
  end;
  if PWLen <> Length (Result) then
    raise EAbort.Create (CEPWError);
end;

{-----------------------------------}
function TPasswort.GetPWCode: string;
{-----------------------------------}
begin
  Result := FWieserIni.ReadString (CSectionPasswort,
    CIdentPasswort, CPWDefault);
end;

{---------------------------------------------------}
procedure TPasswort.SetPWCode (const PWCode: string);
{---------------------------------------------------}
begin
  FWieserIni.WriteString (CSectionPasswort, CIdentPasswort, PWCode);
end;

{-------------------------------------}
function TPasswort.GetPasswort: string;
{-------------------------------------}
begin
  Result := DecodePW (GetPWCode);
end;

{--------------------------------------------------------}
constructor TPasswort.Create (const WieserIni: TFileName);
{--------------------------------------------------------}
begin
  inherited Create;
  FWieserIni := TIniFile.Create (WieserIni);
  FLogged := False;
end;

{---------------------------}
destructor TPasswort.Destroy;
{---------------------------}
begin
  FWieserIni.Free;
  inherited Destroy;
end;

{---------------------------------------------------}
function TPasswort.LogIn (const PW: string): Boolean;
{---------------------------------------------------}
begin
  try
    Result := CompareText (PW, GetPasswort) = 0;
    if Result then
      FLogged := True;
  except
    on Exception do
      Result := False;
  end;
end;

{-------------------------}
procedure TPasswort.LogOut;
{-------------------------}
begin
  FLogged := False;
end;

{---------------------------------------------------------}
function TPasswort.ChangePW (const NewPW: string): Boolean;
{---------------------------------------------------------}
begin
  Result := False;
  if FLogged then
  begin
    try
      SetPWCode (EncodePW (NewPW));
      Result := True;
    except
      on Exception do;
    end;
  end;
end;

{----------------------------------------------------------}
function InitPasswort (const WieserIni: TFileName): Boolean;
{----------------------------------------------------------}
begin
  if not Assigned (PW) then
  begin
    PW := TPasswort.Create (WieserIni);
    Result := True;
  end
  else
    Result := False;
end;

{-----------------------------}
function DonePasswort: Boolean;
{-----------------------------}
begin
  if Assigned (PW) then
  begin
    PW.Free;
    PW := nil;
    Result := True;
  end
  else
    Result := False;
end;

{-------------------------------}
function PasswortLogged: Boolean;
{-------------------------------}
begin
  Result := False;
  if Assigned (PW) then
    Result := PW.FLogged;
end;

{------------------------------}
function LoginPasswort: Boolean;
{------------------------------}
var
  FormLogIn: TFormLogIn;
begin
  Result := False;
  if Assigned (PW) then
  begin
    FormLogIn := TFormLogIn.Create (Application);
    try
      while not Result and (FormLogIn.ShowModal = idOk) do
      begin
        PW.LogOut;
        Result := PW.Login (FormLogin.GetPasswort);
        if not Result then
          MessageDlg(S_PWEingabe, mtError,[mbOk],0);
      end;
    finally
      FormLogIn.Free;
    end;
  end;
end;

{-------------------------------}
function LogoutPasswort: Boolean;
{-------------------------------}
begin
  Result := False;
  if Assigned (PW) then
  begin
    PW.LogOut;
    Result := True;
  end;
end;

{-------------------------------}
function ChangePasswort: Boolean;
{-------------------------------}
var
  FormPWEdit: TFormPWEdit;
begin
  Result := False;
  if Assigned (PW) then
  begin
    if PW.FLogged then
    begin
      FormPWEdit := TFormPWEdit.Create (Application);
      try
        if FormPWEdit.ShowModal = idOk then
        begin
          Result := PW.ChangePW (FormPWEdit.GetPasswort);
        end;
      finally
        FormPWEdit.Free;
      end;
    end;
  end;
end;

end.
