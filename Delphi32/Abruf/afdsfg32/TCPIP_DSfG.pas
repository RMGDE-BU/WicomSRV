{******************************************************************************}
{* Unit: DSfG-Kommunikation über TCP/IP                                       *}
{* 24.03.2003 WW                                                              *}
{* 04.01.2012 WW Bugfix Connect bei IP-Adresse mit führenden Nullen (in       *}
{*            TDSfGClientSocketCommObj)                                       *}
{* 09.10.2013 WW TDSfGClientSocketCommObj.Destroy mit Disconnect              *}
{* 25.02.2020 WW Erweiterung TDSfGClientSocketCommObj für Verbindung per DNS  *}
{*               Hostname                                                     *}
{******************************************************************************}
unit TCPIP_DSfG;

interface

uses
  Windows, SysUtils, Forms, Classes, ExtCtrls, scktcomp, WinSock, T_Zeit,
  ErrConst, LogCom, WStrUtils, WComm, O_Comm, GPRSTelegrList, GPRSVerbList,
  CommUtil;

type
  { Objekt für DSfG-Kommunikation über Socket }

  TDSfGCustomSocketCommObj = class(TDSfGCommObj)
  private
    { Private-Deklarationen }
    FSocket: TCustomWinSocket;
    SocketErrorEvent: TErrorEvent;  // zur Fehlerüberwachung
    procedure SocketError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    function GetConnected: boolean;
  protected
    function Comm_SendData (sData: string): boolean; override;
    function Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal): string; override;
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; override;
    procedure Comm_ClearRecBuf; override;
    procedure Comm_SetFehlerRueckgabe (NoCarrier: boolean;
      var Rueckgabe: TRueckgabe); override;
  public
    { Public-Deklarationen }
    constructor Create (ASocket: TCustomWinSocket; AWorkPath: TFileName;
      AComLogFile: TComLogFile);
    function Disconnect: boolean;
    property Connected: boolean read GetConnected;
  end;


  { Objekt für DSfG-Kommunikation über GPRS }

  TDSfGGPRSCommObj = class(TDSfGCustomSocketCommObj)
  private
    { Private-Deklarationen }
    FGPRSVerbindungenListe: TGPRSVerbList;
    FGPRSTelegrammListe: TGPRSTelegrList;
  protected
    function Comm_SendData (sData: string): boolean; override;
    function Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal): string; override;
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; override;
  public
    { Public-Deklarationen }
    constructor Create (ASocket: TCustomWinSocket; AWorkPath: TFileName;
      AComLogFile: TComLogFile; AGPRSVerbindungenListe: TGPRSVerbList;
      AGPRSTelegrammListe: TGPRSTelegrList);
    procedure AddPushTelegramToList (ATelegram: string; AGeraetetyp: integer); override;
    property GPRSRemoteAddress: string read FGPRSRemoteAddress;
  end;


  { Objekt für DSfG-Kommunikation über Client-Socket }

  TDSfGClientSocketCommObj = class(TDSfGCustomSocketCommObj)
  private
    FClientSocket: TClientSocket;
  public
    constructor Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
    destructor Destroy; override;
    function Connect (AAddress_Host: string; APort: integer; ATimeout: integer;
      var AFehlergruppe: integer; var AFehlercode: integer): boolean;
    function Disconnect (ATimeout: integer;
      var AFehlergruppe: integer; var AFehlercode: integer): boolean;
    property ClientSocket: TClientSocket read FClientSocket;
  end;

implementation

{ TDSfGCustomSocketCommObj }

{---------------------------------------------------------------------}
constructor TDSfGCustomSocketCommObj.Create (ASocket: TCustomWinSocket;
  AWorkPath: TFileName; AComLogFile: TComLogFile);
{---------------------------------------------------------------------}
begin
  inherited Create (AWorkPath, AComLogFile);

  FSocket:=ASocket;

  SocketErrorEvent:=eeGeneral;
  if Assigned (FSocket) then
    FSocket.OnErrorEvent:=SocketError;    { Fehler-Ereignisbehandlungsroutine zuweisen }

  FPrefix_RMGDfueBefehl:='YW';  // Prefix für RMG-DSfG-DFÜ-Befehle bei TCP/IP-Verbindung }
end;


{---------------------------------------------------------------------------}
procedure TDSfGCustomSocketCommObj.SocketError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
{---------------------------------------------------------------------------}
begin
  FCommError:=1;  // Socketfehler aufgetreten
  SocketErrorEvent:=ErrorEvent;
  { Rückgabe: }
  ErrorCode:=0;  // Exception-Ausgabe unterdrücken
end;

{------------------------------------------------------}
function TDSfGCustomSocketCommObj.GetConnected: boolean;
{------------------------------------------------------}
var
  Dummy: TRueckgabe;

begin
  Result:=Comm_Connection (Dummy);
end;

{----------------------------------------------------}
function TDSfGCustomSocketCommObj.Disconnect: boolean;
{----------------------------------------------------}
begin
  try
    if Assigned (FSocket) then begin
      FSocket.Close;
      Result:=true;
    end else
      Result:=false;
  except
    Result:=false;
  end;
end;


{------------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
function TDSfGCustomSocketCommObj.Comm_SendData (sData: string): boolean;
{-----------------------------------------------------------------------}
begin
  try
    if Assigned (FSocket) then
      Result:=FSocket.SendText (sData) <> SOCKET_ERROR  { Daten über Socket versenden }
    else
      Result:=false;
  except
    Result:=false;  // 08.01.2018, WW
  end;
end;

{---------------------------------------------------}
function TDSfGCustomSocketCommObj.Comm_ReceiveData (
  AnzCharsToReceive, ChToRec_Left: cardinal): string;
{---------------------------------------------------}
var
  n: integer;
  S: string;

begin
  try
    if Assigned (FSocket) then begin
      n:=FSocket.ReceiveLength;  // Anzahl der Zeichen im Empfangspuffer

      if AnzCharsToReceive > 0 then     { wenn feste Anzahl an zu empfangenden Zeichen übergeben wurde }
        if n > integer(ChToRec_Left) then n:=ChToRec_Left;     { ...Anzahl begrenzen; 12.09.2008 WW }

      if n > 0 then begin
        SetLength(S, n);  // String-Puffer bereitstellen; 12.09.2008 WW
        SetLength(S, FSocket.ReceiveBuf(Pointer(S)^, Length(S)));  // Zeichen aus Empfangspuffer lesen
      end else
        S:='';
    end else
      S:='';
  except
    S:='';  // 08.01.2018, WW
  end;

  Result:=S;
end;

{-------------------------------------------------}
function TDSfGCustomSocketCommObj.Comm_Connection (
  var Rueckgabe: TRueckgabe): boolean;
{-------------------------------------------------}
begin
  { Überwachung, ob Socket-Verbindung noch besteht: }
  try
    if Assigned (FSocket) then
      Result:=FSocket.Connected  { Socket-Verbindung ist nicht aktiv -> Verbindung unterbrochen }
    else
      Result:=false;
  except
    Result:=false;  // 08.01.2018, WW
  end;

  if not Result then begin
    with Rueckgabe do begin
      Fehlergruppe:=COM_TCPIP_ERROR;
      Fehlercode:=TCPIP_ERR_CONNECTION_INACTIVE;
      Antwort:='';
    end;
  end;
end;

{--------------------------------------------------}
procedure TDSfGCustomSocketCommObj.Comm_ClearRecBuf;
{--------------------------------------------------}
begin
  // Empfangspuffer wird nicht geleert
end;

{-----------------------------------------------------------------------------}
procedure TDSfGCustomSocketCommObj.Comm_SetFehlerRueckgabe (NoCarrier: boolean;
  var Rueckgabe: TRueckgabe);
{-----------------------------------------------------------------------------}
begin
  { Rueckgabe belegen mit Socketfehler:
   -> Rückgabe "Verbindung unterbrochen" nicht mit SocketError überschreiben }
  if not NoCarrier then begin
    with Rueckgabe do begin
      SocketErrorEventToFehlerGruppeCode (SocketErrorEvent, Fehlergruppe, Fehlercode);
      Antwort:='';
    end;
  end;
end;


{ TDSfGGPRSCommObj }

{-------------------------------------------------------------}
constructor TDSfGGPRSCommObj.Create (ASocket: TCustomWinSocket;
  AWorkPath: TFileName; AComLogFile: TComLogFile;
  AGPRSVerbindungenListe: TGPRSVerbList;
  AGPRSTelegrammListe: TGPRSTelegrList);
{-------------------------------------------------------------}
begin
  inherited Create (ASocket, AWorkPath, AComLogFile);
  FGPRSVerbindungenListe:=AGPRSVerbindungenListe;
  FGPRSTelegrammListe:=AGPRSTelegrammListe;

  if Assigned (FSocket) then begin
    FGPRSRemoteAddress:=FSocket.RemoteAddress;
    FGPRSRemotePort:=FSocket.RemotePort;
  end;
end;

{---------------------------------------------------------------}
function TDSfGGPRSCommObj.Comm_SendData (sData: string): boolean;
{---------------------------------------------------------------}
var
  AnzSendBytes: integer;

begin
  Result:=inherited Comm_SendData (sData);
  if Result AND Assigned (FGPRSVerbindungenListe) then begin
    AnzSendBytes:=length (sData);
    FGPRSVerbindungenListe.IncrementSendCounts (FGPRSRemoteAddress, 1, AnzSendBytes);
  end;
end;

{---------------------------------------------------}
function TDSfGGPRSCommObj.Comm_ReceiveData (
  AnzCharsToReceive, ChToRec_Left: cardinal): string;
{---------------------------------------------------}
var
  AnzRecBytes: integer;

begin
  Result:=inherited Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left);

  AnzRecBytes:=length (Result);
  if (AnzRecBytes > 0) AND Assigned (FGPRSVerbindungenListe) then
    FGPRSVerbindungenListe.IncrementRecCounts (FGPRSRemoteAddress, 0, 1, AnzRecBytes);
end;

{-----------------------------------------------------------------------------}
function TDSfGGPRSCommObj.Comm_Connection (var Rueckgabe: TRueckgabe): boolean;
{-----------------------------------------------------------------------------}
begin
  Result:=inherited Comm_Connection (Rueckgabe);

  if not Result AND Assigned (FGPRSVerbindungenListe) then
    if FGPRSVerbindungenListe.GetAktiv (FGPRSRemoteAddress) then  // nur wenn Verbindung aktiv
      FGPRSVerbindungenListe.IncrementVerbCount (false, FGPRSRemoteAddress, -1);
end;

{------------------------------------------------------------------}
procedure TDSfGGPRSCommObj.AddPushTelegramToList (ATelegram: string;
  AGeraetetyp: integer);
{------------------------------------------------------------------}
{ Telegramm in GPRS-Push-Telegrammliste anhängen;
  Übergabe: Telegramm
            Gerätetyp }
begin
  if Assigned (FGPRSTelegrammListe) then
    FGPRSTelegrammListe.SetTelegramm (ATelegram, AGeraetetyp, FGPRSRemoteAddress,
                                      FSocket);
end;


{ TDSfGClientSocketCommObj }

{----------------------------------------------------------------}
constructor TDSfGClientSocketCommObj.Create (AWorkPath: TFileName;
  AComLogFile: TComLogFile);
{----------------------------------------------------------------}
begin
  FClientSocket:=TClientSocket.Create(nil);
  inherited Create (FClientSocket.Socket, AWorkPath, AComLogFile);
end;

{------------------------------------------}
destructor TDSfGClientSocketCommObj.Destroy;
{------------------------------------------}
var
  AFehlergruppe: integer;
  AFehlercode: integer;

begin
  Disconnect (5000, AFehlergruppe, AFehlercode);  // 09.10.2013, WW

  inherited Destroy;
  FClientSocket.Free;
end;

{----------------------------------------------------------------------------------}
function TDSfGClientSocketCommObj.Connect (AAddress_Host: string; APort: integer;
  ATimeout: integer; var AFehlergruppe: integer; var AFehlercode: integer): boolean;
{----------------------------------------------------------------------------------}
{ Socketverbindung öffnen;
  Übergabe: Socket-Adresse (IPv4) oder Socket-Hostname
            Socket-Port
            Timeout für Öffnen der Verbindung
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Socketverbindung geöffnet werden konnte }
begin
  Result:=false;

  if WIsIPv4Address(AAddress_Host) then begin  // 25.02.2020, WW
    // führende Nullen aus IP-Adresse ausfiltern (wird von TClientSocket als 0
    // interpretiert !): 04.01.2012, WW
    FClientSocket.Address:=WTrimIPAddress (AAddress_Host);
    FClientSocket.Host:='';
  end
  else begin
    FClientSocket.Address:='';
    FClientSocket.Host:=AAddress_Host;  // 25.02.2020, WW
  end;

  FClientSocket.Port:=APort;
  Timeout:=ATimeout;

  { Vorbelegung: OK }
  AFehlergruppe:=0;
  AFehlercode:=0;
  FCommError:=0;

  TimeoutCount:=0;
  TimerTimeout.Enabled:=true;                   { Timeout-Überwachung starten }
  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');
  Application.ProcessMessages;

  { Socket-Verbindung öffnen: }
  if ComLogFile <> nil then
    ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung öffnen...',
      FClientSocket.Host,  // 25.02.2020, WW
      FClientSocket.Address, FClientSocket.Port);
  try
    FClientSocket.Open;
  except
    on E:Exception do begin    // 12.07.2010
      AFehlergruppe:=COM_TCPIP_ERROR;
      AFehlercode:=TCPIP_ERR_CONNECT;
      if (ComLogFile <> nil) then
        ComLogFile.WriteTCPIP_Msg('TCP/IP-Verbindung konnte nicht geöffnet werden: ' + E.Message,
          FClientSocket.Host,  // 25.02.2020, WW
          FClientSocket.Address, FClientSocket.Port);
      exit;
    end;
  end;
  { warten bis Socket-Verbindung geöffnet ist oder Fehler aufgetreten ist: }
  while true do begin
    Sleep (1);
    Application.ProcessMessages;
    if FClientSocket.Active then begin
      { Socket-Verbindung ist jetzt geöffnet }
      if ComLogFile <> nil then
        ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung ist geöffnet',
          FClientSocket.Host,  // 25.02.2020, WW
          FClientSocket.Address, FClientSocket.Port);
      Result:=true;
      Break;
    end;
    if FCommError <> 0 then Break;             { Socket-Fehler ist aufgetreten }

    if TimeoutCount >= Timeout then begin    { Timeout beim Verbindung-Öffnen }
      AFehlergruppe:=COM_TCPIP_ERROR;
      AFehlercode:=TCPIP_ERR_TIMEOUTCONNECT;
      Break;
    end;
  end;  { while }
  if (not Result) and (ComLogFile <> nil) then  // 12.07.2010
    ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung konnte nicht geöffnet werden',
      FClientSocket.Host,  // 25.02.2020, WW
      FClientSocket.Address, FClientSocket.Port);

  TimerTimeout.Enabled:=false;                  { Timeout-Überwachung beenden }
  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  Application.ProcessMessages;

  if FCommError <> 0 then
    SocketErrorEventToFehlerGruppeCode (SocketErrorEvent, AFehlergruppe, AFehlercode);
end;

{---------------------------------------------------------------}
function TDSfGClientSocketCommObj.Disconnect (ATimeout: integer;
  var AFehlergruppe: integer; var AFehlercode: integer): boolean;
{---------------------------------------------------------------}
{ Socketverbindung schließen;
  Übergabe: Timeout für Schließen der Verbindung
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Socketverbindung geschlossen werden konnte }
begin
  Result:=false;
  Timeout:=ATimeout;

  { Vorbelegung: OK }
  AFehlergruppe:=0;
  AFehlercode:=0;
  FCommError:=0;

  if not FClientSocket.Active then begin         { Socketverbindung ist bereits geschlossen }
    Result:=true;
    exit;
  end;

  TimeoutCount:=0;
  TimerTimeout.Enabled:=true;                   { Timeout-Überwachung starten }
  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');
  Application.ProcessMessages;

  { Socket-Verbindung schließen: }
  if ComLogFile <> nil then
    ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung schließen...',
      FClientSocket.Host,  // 25.02.2020, WW
      FClientSocket.Address, FClientSocket.Port);
  try
    FClientSocket.Close;
  except
    AFehlergruppe:=COM_TCPIP_ERROR;
    AFehlercode:=TCPIP_ERR_DISCONNECT;
    exit;
  end;
  { warten bis Socket-Verbindung geschlossen ist: }
  while true do begin
    Sleep (1);
    Application.ProcessMessages;
    if not FClientSocket.Active then begin
      { Socket-Verbindung ist jetzt geschlossen }
      if ComLogFile <> nil then
        ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung ist geschlossen',
          FClientSocket.Host,  // 25.02.2020, WW
          FClientSocket.Address, FClientSocket.Port);
      Result:=true;
      Break;
    end;

    if TimeoutCount >= Timeout then begin    { Timeout beim Verbindung-Schließen }
      AFehlergruppe:=COM_TCPIP_ERROR;
      AFehlercode:=TCPIP_ERR_TIMEOUTDISCONNECT;
      Break;
    end;
  end;  { while }

  TimerTimeout.Enabled:=false;                  { Timeout-Überwachung beenden }
  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  Application.ProcessMessages;

  if FCommError <> 0 then begin
    SocketErrorEventToFehlerGruppeCode (SocketErrorEvent, AFehlergruppe, AFehlercode);
    Result:=false;
  end;
end;

end.

