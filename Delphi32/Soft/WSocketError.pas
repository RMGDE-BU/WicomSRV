{******************************************************************************}
{* Unit: Socket-Fehlertexte (aus: http://www.delphipraxis.net)                *}
{*       -> genauere Fehlercodebeschreibung siehe WSocketError.txt            *}
{* 06.03.2006  WW                                                             *}
{******************************************************************************}
unit WSocketError;

interface

uses
  ScktComp;

function SocketErrorEventToString(ErrorEvent: TErrorEvent): string;
function SocketErrorCodeToString(ErrorCode: integer): string;

implementation

{-----------------------------------------------------------------}
function SocketErrorEventToString(ErrorEvent: TErrorEvent): string;
{-----------------------------------------------------------------}
{ liefert Text zu einem Socket-ErrorEvent }
begin
  case ErrorEvent of
    eeGeneral:    Result:='eeGeneral';
    eeSend:       Result:='eeSend';
    eeReceive:    Result:='eeReceive';
    eeConnect:    Result:='eeConnect';
    eeDisconnect: Result:='eeDisconnect';
    eeAccept:     Result:='eeAccept';
    eeLookup:     Result:='eeLookup';
  else
    Result:='unknown socket event';
  end;
end;

{-----------------------------------------------------------}
function SocketErrorCodeToString(ErrorCode: integer): string;
{-----------------------------------------------------------}
{ liefert Fehlertext zu einem Socket-Errorcode }
begin
  case ErrorCode of
    10004: Result := 'interrupted function call';
    10013: Result := 'permission denied';
    10014: Result := 'bad address';
    10022: Result := 'invalid argument';
    10024: Result := 'too many open files';
    10035: Result := 'resource temporarily unavailable';
    10036: Result := 'operation now in progress';
    10037: Result := 'operation already in progress';
    10038: Result := 'socket operation on non-socket';
    10039: Result := 'destination address required';
    10040: Result := 'message too long';
    10041: Result := 'protocol wrong type for socket';
    10042: Result := 'bad protocol option';
    10043: Result := 'protocol not supported';
    10044: Result := 'socket type not supported';
    10045: Result := 'operation not supported';
    10046: Result := 'protocol family not supported';
    10047: Result := 'address family not supported by protocol family';
    10048: Result := 'address already in use';
    10049: Result := 'cannot assign requested address';
    10050: Result := 'network is down';
    10051: Result := 'network is unreachable';
    10052: Result := 'network dropped connection on reset';
    10053: Result := 'software caused connection abort';
    10054: Result := 'connection reset by peer';
    10055: Result := 'no buffer space available';
    10056: Result := 'socket is already connected';
    10057: Result := 'socket is not connected';
    10058: Result := 'cannot send after socket shutdown';
    10060: Result := 'connection timed out';
    10061: Result := 'connection refused';
    10064: Result := 'host is down';
    10065: Result := 'no route to host';
    10067: Result := 'too many processes';
    10091: Result := 'network subsystem is unavailable';
    10092: Result := 'winsock.dll version out of range';
    10093: Result := 'successful wsastartup not yet performed';
    10094: Result := 'graceful shutdown in progress';
    11001: Result := 'host not found';
    11002: Result := 'non-authoritative host not found';
    11003: Result := 'this is a non-recoverable error';
    11004: Result := 'valid name, no data record of requested type';
  else
    Result := 'unknown socket error';
  end;
end;

end.
