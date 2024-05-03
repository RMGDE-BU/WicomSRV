{******************************************************************************}
{* Unit: GPRS-Serversocket-Objekt                                             *}
{* 07.01.2009 WW                                                              *}
{******************************************************************************}
unit O_GPRSServerSocket;

interface

uses
  Windows, Messages, SysUtils, Classes, ScktComp, WSocketError;

type
  { Callback-Prozedurtypen zur Anzeige in Fremdmodulen }

  TCBBooleanStringProc = procedure (b: boolean; S: string) of object;
  TCBStringSocketProc = procedure (S: string; Socket: TCustomWinSocket) of object;
  TCBStringProc = procedure (S: string) of object;

  { Objekt für GPRS-Serversocket }

  TGPRSServerSocketObj = class(TObject)
  private
    { Private-Deklarationen }
    FServerSocket: TServerSocket;
    FServerPortStatus: TCBBooleanStringProc;
    FClientConnect: TCBStringSocketProc;
    FClientDisconnect: TCBStringSocketProc;
    FClientRead: TSocketNotifyEvent;
    FClientError: TCBStringProc;
    procedure ServerSocketClientConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocketClientDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocketClientRead(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocketClientError(Sender: TObject;
      Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
      var ErrorCode: Integer);
    procedure Set_StatusServerPort (bActive: boolean; Port: integer);
    function GetServerActive: boolean;
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
    function OpenServer (Port: integer; var ErrMsg: string): boolean;
    procedure CloseServer;
    property ServerSocket: TServerSocket read FServerSocket;
    property ServerActive: boolean read GetServerActive;
    property CBServerPortStatus: TCBBooleanStringProc read FServerPortStatus
      write FServerPortStatus;
    property CBClientConnect: TCBStringSocketProc read FClientConnect
      write FClientConnect;
    property CBClientDisconnect: TCBStringSocketProc read FClientDisconnect
      write FClientDisconnect;
    property CBClientRead: TSocketNotifyEvent read FClientRead
      write FClientRead;
    property CBClientError: TCBStringProc read FClientError
      write FClientError;
  end;

implementation

resourcestring
  SMsgPortOpened = 'Port %d geöffnet, GPRS-Server ist aktiv';
  SMsgPortClosed = 'Port %d geschlossen, GPRS-Server ist beendet';


{--------------------------------------}
constructor TGPRSServerSocketObj.Create;
{--------------------------------------}
begin
  inherited Create;

  FServerSocket:=TServerSocket.Create (nil);
  FServerSocket.ServerType:=stNonBlocking;
  FServerSocket.OnClientConnect:=ServerSocketClientConnect;
  FServerSocket.OnClientDisconnect:=ServerSocketClientDisconnect;
  FServerSocket.OnClientRead:=ServerSocketClientRead;
  FServerSocket.OnClientError:=ServerSocketClientError;
end;

{--------------------------------------}
destructor TGPRSServerSocketObj.Destroy;
{--------------------------------------}
begin
  FServerSocket.Free;

  inherited Destroy;
end;

{---------------------------- Anzeige -----------------------------------------}

{------------------------------------------------------------------------------------}
procedure TGPRSServerSocketObj.Set_StatusServerPort (bActive: boolean; Port: integer);
{------------------------------------------------------------------------------------}
{ Status Server-Port geöffnet/geschlossen anzeigen;
  Übergabe: Flag 'bActive' (true: geöffnet, false: geschlossen)
            Port }
var
  S: string;

begin
  if bActive then
    S:=Format (SMsgPortOpened, [Port]) { OK, Port geöffnet }
  else
    S:=Format (SMsgPortClosed, [Port]); { Fehler, Port nicht geöffnet }

  if Assigned (CBServerPortStatus) then
    CBServerPortStatus (bActive, S);
end;

{-------------------------- Aktionen ------------------------------------------}

{------------------------------------------------------------------------------------}
function TGPRSServerSocketObj.OpenServer (Port: integer; var ErrMsg: string): boolean;
{------------------------------------------------------------------------------------}
{ GPRS-Serversocket öffnen;
  Übergabe: Port
  Rückagbe: Fehlertext
  Ergebnis: true, wenn GPRS-Serversocket geöffnet werden konnte }
begin
  Result:=false;
  ErrMsg:='';

  FServerSocket.Port:=Port;
  try
    FServerSocket.Open;
    { Anzeige Server-Port: }
    Set_StatusServerPort (FServerSocket.Active, FServerSocket.Port);
    Result:=true;
  except
    on E: Exception do
      ErrMsg:=E.Message;
  end;
end;

{-----------------------------------------}
procedure TGPRSServerSocketObj.CloseServer;
{-----------------------------------------}
{ Serversocket schließen }
begin
  if FServerSocket <> nil then begin
    if FServerSocket.Active then begin
      FServerSocket.Close;
      { Anzeige Server-Port: }
      Set_StatusServerPort (FServerSocket.Active, FServerSocket.Port);
    end;
  end;
end;

{-----------------------------------------------------}
function TGPRSServerSocketObj.GetServerActive: boolean;
{-----------------------------------------------------}
{ Ergebnis: true, wenn Serversocket geöffnet ist }
begin
  if FServerSocket <> nil then
    Result:=FServerSocket.Active
  else
    Result:=false;
end;

{-------------------------- Socket-Kommunikation ------------------------------}

{-----------------------------------------------------------------------}
procedure TGPRSServerSocketObj.ServerSocketClientConnect(Sender: TObject;
  Socket: TCustomWinSocket);
{-----------------------------------------------------------------------}
{ eine Verbindung wurde geöffnet }
var
  S: string;

begin
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 +
     'Open' + #9 + Socket.RemoteAddress + ':' + IntToStr(Socket.RemotePort);

  if Assigned (CBClientConnect) then
    CBClientConnect (S, Socket);
end;

{--------------------------------------------------------------------------}
procedure TGPRSServerSocketObj.ServerSocketClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
{--------------------------------------------------------------------------}
{ eine Verbindung wurde geschlossen }
var
  S: string;

begin
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 +
     'Close' + #9 + Socket.RemoteAddress + ':' + IntToStr(Socket.RemotePort);

  if Assigned (CBClientDisconnect) then
    CBClientDisconnect (S, Socket);
end;

{--------------------------------------------------------------------}
procedure TGPRSServerSocketObj.ServerSocketClientRead(Sender: TObject;
  Socket: TCustomWinSocket);
{--------------------------------------------------------------------}
{ es wurden Zeichen empfangen }
begin
  if Assigned (CBClientRead) then
    CBClientRead (Sender, Socket);
end;

{---------------------------------------------------------------------------}
procedure TGPRSServerSocketObj.ServerSocketClientError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
{---------------------------------------------------------------------------}
{ es trat ein Socket-Fehler auf }
var
  S: string;
begin
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 +
     'ErrorEvent: ' + SocketErrorEventToString (ErrorEvent) +
     ' (' + IntToStr (Integer(ErrorEvent)) + '), ' +
     'ErrorCode: ' + SocketErrorCodeToString (Errorcode) +
     ' (' + IntToStr (ErrorCode) + ')';

  if Assigned (CBClientError) then
    CBClientError (S);

  ErrorCode:=0;  // Exception-Ausgabe unterdrücken
end;

end.


