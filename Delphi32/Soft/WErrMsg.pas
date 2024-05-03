{******************************************************************************}
{* Hilfsunit zur Ausgabe von System-Fehlermeldungen                           *}
{* 05.02.2001  WW                                                             *}
{******************************************************************************}
unit WErrMsg;

interface

uses
  Windows, Dialogs, SysUtils;

function ExceptionErrorMsgStr (ExceptObject: TObject): string;
function LastErrorStr: string;
procedure LastErrorMsg;

implementation

{------------------------------------------------------------}
function ExceptionErrorMsgStr (ExceptObject: TObject): string;
{------------------------------------------------------------}
{ liefert Exception-Messagestring mit:
  - Name des EXE in dem die Exception aufgetreten ist
  - Exception-Klassenname
  - Exception-Message
  - Adresse, an der die Exception ausgelöst wurde }
var
  Buffer: array[0..1023] of Char;
begin
  ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, SizeOf(Buffer));
  Result:=string(Buffer);
end;

{----------------------------}
function LastErrorStr: string;  // 18.08.2009, WW
{----------------------------}
{ gibt String mit 'LastError' aus }
var
  Buf: array [Byte] of Char;
  ErrorCode: cardinal;
begin
  ErrorCode := GetLastError;
  if ErrorCode <> 0 then begin
    if FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
                     ErrorCode, LOCALE_USER_DEFAULT,
                     Buf, sizeof(Buf), nil) <> 0 then
        Result:='Error ' + IntToStr(ErrorCode) + ': '+ string(Buf)
    else
      Result:='Error FormatMessage';
  end else
    Result:='';
end;

{---------------------}
procedure LastErrorMsg;
{---------------------}
{ gibt Dialog mit 'LastError' aus }
var
  S: string;
begin
  S:=LastErrorStr;
  if length (S) > 0 then
    Showmessage (S);
end;

end.
