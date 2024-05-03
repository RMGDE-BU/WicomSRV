{******************************************************************************}
{* Unit: IEC-Kommunikation �ber TCP/IP f�r Leitwartenkopplung                 *}
{* 31.05.2007 WW                                                              *}
{******************************************************************************}
unit TCPIP_Iec;

interface

uses
  Windows, SysUtils, Forms, Classes, ExtCtrls, ScktComp, WStrUtils, IecConst,
  IecLog, LogFile, T_Tools;

type
  { Client-Verbindungsstati }
  TStatusClientConnection = (scc_connecting,     // Verbindung wird ge�ffnet
                             scc_connected,      // Verbindung ist offen
                             scc_disconnecting,  // Verbindung wird geschlossen
                             scc_disconnected    // Verbindung ist geschlossen
                            );

  { Callback-Prozedurtypen zur Anzeige in Fremdmodulen }

  TCBStringProc = procedure (S: string) of object;
  TCBBooleanStringProc = procedure (b: boolean; S: string) of object;
  TCBStatusStringProc = procedure (Status: TStatusClientConnection; S: string) of object;

  { Basis-Objekt f�r IEC-Kommunikation �ber TCP/IP }

  TCustomSocketIec = class(TObject)
  private
    { Private-Deklarationen }
    Empfangspuffer: string;  { Puffer f�r empfangene Zeichen }
    Status: TStatusClientConnection;
    FMonitor: TCBStringProc;
    FClientConnection: TCBStatusStringProc;
    FIecLogfile: TIecLogFile;
    procedure EventConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure EventDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure EventError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure Set_StatusClientConnection (AStatus: TStatusClientConnection;
      ARemoteAddress: string; ARemotePort: integer);
    procedure ClearEmpfangspuffer;
  public
    { Public-Deklarationen }
    VerbindungSocket: TCustomWinSocket;  // Socket der Verbindung zur Gegenstelle
    constructor Create (AIecLogfile: TIecLogFile);
    procedure Send (ABefehl: string);
    function Received (var RecStr: string): boolean;
    property CBMonitor: TCBStringProc read FMonitor write FMonitor;
    property CBClientConnection: TCBStatusStringProc read FClientConnection
      write FClientConnection;
    property IecLogfile: TIecLogfile read FIecLogfile write FIecLogfile;  // 10.03.2015, WW
  end;


  { Objekt f�r IEC-Kommunikation �ber TCP/IP als Server }

  TServerSocketIec = class(TCustomSocketIec)
  private
    { Private-Deklarationen }
    FServerPortStatus: TCBBooleanStringProc;
    ServerSocket: TServerSocket;  // wir sind Station
    procedure Set_StatusServerPort (bActive: boolean; IPAdr: string; Port: integer);
  public
    { Public-Deklarationen }
    constructor Create (AIecLogfile: TIecLogFile);
    destructor Destroy; override;
    function OpenServer (Port: integer; var ErrMsg: string): boolean;
    procedure CloseServer;
    property CBServerPortStatus: TCBBooleanStringProc read FServerPortStatus
      write FServerPortStatus;
  end;


  { Objekt f�r IEC-Kommunikation �ber TCP/IP als Client }

  TClientSocketIec = class(TCustomSocketIec)
  private
    { Private-Deklarationen }
    ClientSocket: TClientSocket;  // wir sind Zentrale
  public
    { Public-Deklarationen }
    constructor Create (AIecLogfile: TIecLogFile);
    destructor Destroy; override;
    function OpenClient (Adresse: string; Port: integer; var ErrMsg: string): boolean;
    function CloseClient (var ErrMsg: string): boolean;
  end;

implementation

resourcestring
  SMsgClientConnecting    = 'IEC-Client: Verbinde mit %s:%d';
  SMsgClientConnected     = 'IEC-Client: %s:%d';
  SMsgClientDisconnecting = 'IEC-Client: Beende Verbindung';
  SMsgClientDisconnected  = 'IEC-Client: Keine Verbindung';
  SMsgUnknownConnectState = 'IEC-Client: Undef. Verbindungsstatus';

  SMsgAdrPort    = 'IEC-Server: %s:%d';
  SMsgPortOpened = 'IEC-Server: Port %d ge�ffnet';
  SMsgPortClosed = 'IEC-Server: Port %d geschlossen';

  SMsgErrOpenIECServer = 'IEC-Server konnte nicht ge�ffnet werden: %s';
  SMsgErrOpenIECClient = 'IEC-Client konnte nicht ge�ffnet werden: %s';
  SMsgErrCloseIECClient = 'IEC-Client konnte nicht geschlossen werden: %s';


{ TCustomSocketIec }

{-------------------------------------------------------------}
constructor TCustomSocketIec.Create (AIecLogfile: TIecLogFile);
{-------------------------------------------------------------}
begin
  inherited Create;
  IecLogfile:=AIecLogFile;

  VerbindungSocket:=nil;  // es besteht keine Verbindung zur Gegenstelle
  Empfangspuffer:='';
  Status:=scc_disconnected;
end;

{---------------------------- Anzeige -----------------------------------------}

{--------------------------------------------------------------------------------------}
procedure TCustomSocketIec.Set_StatusClientConnection (AStatus: TStatusClientConnection;
  ARemoteAddress: string; ARemotePort: integer);
{--------------------------------------------------------------------------------------}
{ Status der Client-Verbindung setzen und anzeigen }
var
  S: string;

begin
  Status:=AStatus;
  case AStatus of
    scc_connecting:    S:=Format (SMsgClientConnecting, [ARemoteAddress, ARemotePort]);
    scc_connected:     S:=Format (SMsgClientConnected, [ARemoteAddress, ARemotePort]);
    scc_disconnecting: S:=Format (SMsgClientDisconnecting, [ARemoteAddress, ARemotePort]);
    scc_disconnected:  S:=SMsgClientDisconnected;
  else
    S:=SMsgUnknownConnectState;
  end;

  if Assigned (CBClientConnection) then
    CBClientConnection (AStatus, S);

  { Logfile-Protokollierung: }
  if IECLogFile <> nil then
    IECLogFile.Write (S, '', '');
end;

{-------------------------- Ereignisse ----------------------------------------}

{---------------------------------------------------------------------------------}
procedure TCustomSocketIec.EventConnect(Sender: TObject; Socket: TCustomWinSocket);
{---------------------------------------------------------------------------------}
{ eine Verbindung wurde ge�ffnet }
var
  S: string;

begin
  { Monitor-Anzeige: }
  if Assigned (CBMonitor) then begin
    S:='Open' + #9 + Socket.RemoteAddress + ':' + IntToStr(Socket.RemotePort);
    CBMonitor (S);
  end;

  { nur eine Client-Verbindung wird akzeptiert: }
  if not Assigned (VerbindungSocket) then begin
    VerbindungSocket:=Socket;  // Zeiger der Client-Verbindung merken
    { Status/Anzeige Client-Verbindung: }
    Set_StatusClientConnection (scc_connected, Socket.RemoteAddress, Socket.RemotePort);
  end else
    Socket.Close;  { alle weiteren Verbindungen gleich wieder schlie�en }
end;

{------------------------------------------------------------------------------------}
procedure TCustomSocketIec.EventDisconnect(Sender: TObject; Socket: TCustomWinSocket);
{------------------------------------------------------------------------------------}
{ eine Verbindung wurde geschlossen }
var
  S: string;

begin
  { Monitor-Anzeige: }
  if Assigned (CBMonitor) then begin
    S:='Close' + #9 + Socket.RemoteAddress + ':' + IntToStr(Socket.RemotePort);
    CBMonitor (S);
  end;

  { Client-Verbindung wurde beendet: }
  if Assigned (VerbindungSocket) then begin
    if VerbindungSocket = Socket then begin  // die akzeptierte Verbindung wurde beendet
      VerbindungSocket:=nil;  // gemerkten Zeiger der Client-Verbindung l�schen
      { Status/Anzeige Client-Verbindung: }
      Set_StatusClientConnection (scc_disconnected, Socket.RemoteAddress, Socket.RemotePort);
    end;
  end;
end;

{------------------------------------------------------------------------------}
procedure TCustomSocketIec.EventError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
{------------------------------------------------------------------------------}
{ ein Socket-Fehler ist aufgetreten }
var
  S: string;
begin
  { Logfile-Protokollierung: }
  if IecLogFile <> nil then begin
    case ErrorEvent of
      eeGeneral:    S:='Allgemeiner Fehler aufgetreten';
      eeConnect:    S:='Fehler bei der Verbindungsanforderung aufgetreten';
      eeDisconnect: S:='Fehler beim Schlie�en der Verbindung aufgetreten';
      eeReceive:    S:='Fehler beim Empfangen von Daten aufgetreten';
      eeSend:       S:='Fehler beim Senden von Daten aufgetreten';
      eeAccept:     S:='Fehler beim Annehmen einer Client-Verbindungsanforderung aufgetreten';
      eeLookup:     S:='Hostname konnte nicht gefunden werden';
    else
      S:='Undefinierter Fehler aufgetreten';
    end;

    IecLogFile.Write ('SOCKET-FEHLER: ' + S +
                      ' (ErrorEvent: ' + IntToStr (Integer(ErrorEvent)) + ', ' +
                      'ErrorCode: ' + IntToStr (ErrorCode) + ')', '', '', lt_Error);
  end;

  ErrorCode:=0;  // Exception-Ausgabe unterdr�cken

  { Status/Anzeige Client-Verbindung: }
  case Status of
    scc_connecting,
    scc_disconnecting:
      begin  // Fehler beim �ffnen/Schlie�en einer Client-Verbindung
        Set_StatusClientConnection (scc_disconnected, '', -1);  // keine Verbindung !
      end;
  end;
end;

{-------------------------- Aktionen ------------------------------------------}

{------------------------------------------------}
procedure TCustomSocketIec.Send (ABefehl: string);
{------------------------------------------------}
{ Daten an Client senden }
begin
  if Assigned (VerbindungSocket) then
    VerbindungSocket.SendText (ABefehl)
  else begin
    { Logfile-Protokollierung: }
    if IECLogFile <> nil then
      IECLogFile.Write ('FEHLER TCustomSocketIec.Send: Socket = nil', '', '', lt_Error);
  end;
end;

{---------------------------------------------------------------}
function TCustomSocketIec.Received (var RecStr: string): boolean;
{---------------------------------------------------------------}
{ Daten vom Client empfangen;
  R�ckgabe: von der Schnittstelle empfangene Zeichen
  Ergebnis: true, wenn Antwort vollst�ndig empfangen }
var
  L: byte;
  S: string;

begin
  Result:=false;
  RecStr:='';  // Vorbelegung R�ckgabe

  if Assigned (VerbindungSocket) then
    S:=VerbindungSocket.ReceiveText
  else begin
    { Logfile-Protokollierung: }
    if IECLogFile <> nil then
      IECLogFile.Write ('FEHLER TCustomSocketIec.Received: Socket = nil', '', '', lt_Error);
    exit;
  end;
  Empfangspuffer:=Empfangspuffer + S;

  { Ergebnis: true, wenn eine Telegramm vollst�ndig empfangen wurde }
  if length (Empfangspuffer) >= 2 then begin
    if byte (Empfangspuffer [1]) = C_START_VAR then begin  // 1. Zeichen mu� Startzeichen sein
      L:=byte (Empfangspuffer [2]);  // 2. Zeichen: L�nge der APDU (Soll-L�nge f�r Telegramm)
      if length (Empfangspuffer) >= (L + 2) then begin
        RecStr:=Copy (Empfangspuffer, 1, L + 2);
        Empfangspuffer:=Copy (Empfangspuffer, L + 3, length (Empfangspuffer));
        Result:=true;
      end;
    end else
      ClearEmpfangspuffer;
  end;
end;

{---------------------------------------------}
procedure TCustomSocketIec.ClearEmpfangspuffer;
{---------------------------------------------}
{  Empfangspuffer leeren }
begin
  Empfangspuffer:='';
end;


{ TServerSocketIec }

{-------------------------------------------------------------}
constructor TServerSocketIec.Create (AIecLogfile: TIecLogFile);
{-------------------------------------------------------------}
begin
  inherited Create (AIecLogFile);

  ServerSocket:=TServerSocket.Create (nil);
  ServerSocket.ServerType:=stNonBlocking;
  ServerSocket.OnClientConnect:=EventConnect;
  ServerSocket.OnClientDisconnect:=EventDisconnect;
  ServerSocket.OnClientError:=EventError;
end;

{----------------------------------}
destructor TServerSocketIec.Destroy;
{----------------------------------}
begin
  ServerSocket.Free;

  inherited Destroy;
end;

{---------------------------- Anzeige -----------------------------------------}

{-------------------------------------------------------------------------------}
procedure TServerSocketIec.Set_StatusServerPort (bActive: boolean; IPAdr: string;
  Port: integer);
{-------------------------------------------------------------------------------}
{ Status Server-Port ge�ffnet/geschlossen anzeigen }
var
  S: string;
  sAdrPort: string;

begin
  sAdrPort:=Format (SMsgAdrPort, [IPAdr, Port]);

  if Assigned (CBServerPortStatus) then
    CBServerPortStatus (bActive, sAdrPort);

  { Logfile-Protokollierung: }
  if IECLogFile <> nil then begin
    if bActive then begin
      IECLogFile.Write (sAdrPort, '', '');

      S:=Format (SMsgPortOpened, [Port])   { OK, Port ge�ffnet }
    end else
      S:=Format (SMsgPortClosed, [Port]);  { Fehler, Port nicht ge�ffnet }

    IECLogFile.Write (S, '', '');
  end;
end;

{-------------------------- Aktionen ------------------------------------------}

{--------------------------------------------------------------------------------}
function TServerSocketIec.OpenServer (Port: integer; var ErrMsg: string): boolean;
{--------------------------------------------------------------------------------}
{ Serversocket �ffnen }
var
  sIPAdr: string;

begin
  Result:=false;
  ErrMsg:='';

  ServerSocket.Port:=Port;
  try
    ServerSocket.Open;
    { Anzeige Server IP-Adresse/Port: }
    sIPAdr:=WInAddrToStr (ServerSocket.Socket.LookupName (''));  // 10.03.2015, WW
    Set_StatusServerPort (ServerSocket.Active, sIPAdr, ServerSocket.Port);
    Result:=true;
  except
    on E: Exception do
      ErrMsg:=Format (SMsgErrOpenIECServer, [E.Message]);
  end;
end;

{-------------------------------------}
procedure TServerSocketIec.CloseServer;
{-------------------------------------}
{ Serversocket schlie�en }
begin
  if ServerSocket <> nil then begin
    if ServerSocket.Active then begin
      ServerSocket.Close;

      { Anzeige Server-Port: }
      Set_StatusServerPort (ServerSocket.Active, '', ServerSocket.Port);
    end;
  end;
end;


{ TClientSocketIec }

{-------------------------------------------------------------}
constructor TClientSocketIec.Create (AIecLogfile: TIecLogFile);
{-------------------------------------------------------------}
begin
  inherited Create (AIecLogFile);

  ClientSocket:=TClientSocket.Create (nil);
  ClientSocket.ClientType:=ctNonBlocking;
  ClientSocket.OnConnect:=EventConnect;
  ClientSocket.OnDisconnect:=EventDisconnect;
  ClientSocket.OnError:=EventError;
end;

{----------------------------------}
destructor TClientSocketIec.Destroy;
{----------------------------------}
begin
  ClientSocket.Free;

  inherited Destroy;
end;

{-------------------------------------------------------------------}
function TClientSocketIec.OpenClient (Adresse: string; Port: integer;
  var ErrMsg: string): boolean;
{-------------------------------------------------------------------}
{ Client-Socketverbindung �ffnen }
begin
  Result:=false;
  ErrMsg:='';

  // f�hrende Nullen aus IP-Adresse ausfiltern (wird von TClientSocket als 0 interpretiert !):
  ClientSocket.Address:=WTrimIPAddress (Adresse);
  ClientSocket.Port:=Port;
  { Status/Anzeige Client-Verbindung: }
  Set_StatusClientConnection (scc_connecting, ClientSocket.Address, ClientSocket.Port);
  try
    ClientSocket.Open;
    Result:=true;
  except
    on E: Exception do
      ErrMsg:=Format (SMsgErrOpenIECClient, [E.Message]);
  end;
end;

{------------------------------------------------------------------}
function TClientSocketIec.CloseClient (var ErrMsg: string): boolean;
{------------------------------------------------------------------}
{ Client-Socketverbindung schlie�en }
begin
  Result:=true;
  ErrMsg:='';

  if ClientSocket.Active then begin
    Result:=false;
    { Status/Anzeige Client-Verbindung: }
    Set_StatusClientConnection (scc_disconnecting, ClientSocket.Address, ClientSocket.Port);
    try
      ClientSocket.Close;
      Result:=true;
    except
      on E: Exception do
        ErrMsg:=Format (SMsgErrCloseIECClient, [E.Message]);
    end;
  end;
end;

end.

