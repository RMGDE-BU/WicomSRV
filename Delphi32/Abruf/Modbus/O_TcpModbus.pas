//------------------------------------------------------------------------------
// Modbusanbindung über TCP (analog zu serieller Anbindung von WW)
//
// 04.08.2010  GD  Neu
//
// Copyright (C) RMG Messtechnik GmbH 2010
//------------------------------------------------------------------------------
unit O_TcpModbus;

interface

uses
  SysUtils, Windows, Classes, ErrConst, ScktComp, WinSock,
  LogCom, T_Zeit, WComm, O_ModbusComm, CommUtil;

type
  TModbusTcpCommObj = class(TModbusCustomCommObj)
    constructor Create; override;
    destructor Destroy; override;
  private
    { Private-Deklarationen }
    FSocket      : TClientSocket;
    // Konfiguration
    FIpPort      : integer;
    FIpAddress   : string;
    // Hilfsvariablen zur Fehlerüberwachung
    FSocketError : boolean;
    FSocketErrorEvent : TErrorEvent;
  protected
    function Comm_SendData(sData: string): boolean; override;
    function Comm_ReceiveData: string; override;
    function Comm_Connection(var pRueckgabe: TRueckgabe): boolean; override;
    procedure Comm_ClearRecBuf; override;
    procedure Comm_SetFehlerRueckgabe(var pRueckgabe: TRueckgabe); override;
    procedure OnClientSocketError(pSender: TObject; Socket: TCustomWinSocket;
      iErrorEvent: TErrorEvent; var iErrorCode: Integer);
  public
    { Public-Deklarationen }
    procedure SetIpSettings(const sIpAddress: string; iIpPort: integer);
    function Connect: integer; override;
    procedure Disconnect;
    property Socket: TClientSocket read FSocket;
  end;

implementation

const
  C_IpTimeOut = 5000;  // TCP/IP-Timeout 5 sec  (hoch wg. GPRS)

//------------------------------ TModbusTcpCommObj -----------------------------

//----------------------------------------------
constructor TModbusTcpCommObj.Create;
//----------------------------------------------
begin
  inherited Create;

  FSocket := TClientSocket.Create(nil);
  FIpPort := 0;
  FIpAddress := '0.0.0.0';
  FSocketErrorEvent := eeGeneral;
  FSocketError := False;
  FSocket.OnError := OnClientSocketError;    { Fehler-Eregnisbehandlungsroutine zuweisen }
end;

//----------------------------------------------
destructor TModbusTcpCommObj.Destroy;
//----------------------------------------------
begin
  Disconnect;
  FreeAndNil(FSocket);

  inherited Destroy;
end;

// IP-Einstellungen übergeben
// Parameter: IP-Port, IP-Adresse
//----------------------------------------------
procedure TModbusTcpCommObj.SetIpSettings(
  const sIpAddress: string; iIpPort: integer);
//----------------------------------------------
begin
  FIpPort := iIpPort;
  FIpAddress := sIpAddress;
end;

// IP-Verbindung öffnen
// Rückgabe 0 = Verbindung geöffnet; -1 = Verbindung nicht geöffnet (Timeout);
//         -2 = Verbindung nicht geöffnet (Fehler)
//----------------------------------------------
function TModbusTcpCommObj.Connect: integer;
//----------------------------------------------
var
  iTimeOut : Cardinal;
begin
  try
    FSocketError := False;
    Delay(1);

    // 11.03.2015
    FSocket.Active := False;
    FSocket.Socket.Close;
    FSocket.Close;

    FSocket.Port := FIpPort;
    FSocket.Address := FIpAddress;
    FSocket.Open;
    iTimeOut := GetTickCount + C_IpTimeOut;
    while (GetTickCount < iTimeOut) do begin
      Delay(1);
      if (FSocket.Active) then begin
        Break;
      end;
    end;
    if (FSocket.Active) then Result := 0 else Result := -1;
  except
    Result := -2;
  end;
end;

//----------------------------------------------
procedure TModbusTcpCommObj.Disconnect;
//----------------------------------------------
var
  iTimeOut : Cardinal;
begin
  try
    FSocketError := False;
    if (FSocket.Active) then begin
      // 11.03.2015
      FSocket.Active := False;
      FSocket.Socket.Close;
      FSocket.Close;
      iTimeOut := GetTickCount + C_IpTimeOut;
      while (GetTickCount < iTimeOut) and (FSocket.Active) do
        Delay(1);
    end;
  except
  end;
end;

//----------------------------------------------
function TModbusTcpCommObj.Comm_SendData(sData: string): boolean;
//----------------------------------------------
begin
  try
    FSocketError := False;
    if (not FSocket.Active) then Connect;  // 11.03.2015

    if (FSocket.Active) then begin
      Result := (FSocket.Socket.SendText(sData) > 0);
      if (FSocketError) then begin
        Disconnect;
        Connect;
        Result := (FSocket.Socket.SendText(sData) > 0);
      end;
    end
    else Result := False;
  except
    Result := False;
  end;
end;

//----------------------------------------------
function TModbusTcpCommObj.Comm_ReceiveData: string;
//----------------------------------------------
var
  iReceived   : integer;
  sReceiveBuf : string;
begin
  try
    if (FSocket.Active) then begin
      SetLength(sReceiveBuf, 500);
      try
        iReceived := FSocket.Socket.ReceiveBuf(Pointer(sReceiveBuf)^, 500);
        if (iReceived > 0) then begin
          Result := Copy(sReceiveBuf, 1, iReceived);
        end
        else begin
          Result := '';
        end;
      finally
        sReceiveBuf := '';
      end;
    end
    else begin
      FSocketError := True;
      FSocketErrorEvent := eeReceive;
      Result := '';
    end;
  except
    Result := '';
  end;
end;

//----------------------------------------------
function TModbusTcpCommObj.Comm_Connection(var pRueckgabe: TRueckgabe): boolean;
//----------------------------------------------
begin
  Result := FSocket.Active;
end;

//----------------------------------------------
procedure TModbusTcpCommObj.Comm_ClearRecBuf;
//----------------------------------------------
begin
  Delay(10);
  while (FSocket.Active) and (FSocket.Socket.ReceiveLength > 0) do begin
    Comm_ReceiveData;
    Delay(10);
  end;
end;

//----------------------------------------------
procedure TModbusTcpCommObj.Comm_SetFehlerRueckgabe(var pRueckgabe: TRueckgabe);
//----------------------------------------------
begin
  with pRueckgabe do
    SocketErrorEventToFehlerGruppeCode(
      Self.FSocketErrorEvent, Fehlergruppe, Fehlercode);
end;

//----------------------------------------------
procedure TModbusTcpCommObj.OnClientSocketError(pSender: TObject;
  Socket: TCustomWinSocket; iErrorEvent: TErrorEvent; var iErrorCode: integer);
//----------------------------------------------
begin
  FSocketError := True;
  if (iErrorCode = 10054) then FSocket.Active := False;
  FSocketErrorEvent := iErrorEvent;
  // Rückgabe:
  iErrorCode := 0;  // Exception-Ausgabe unterdrücken
end;

end.
