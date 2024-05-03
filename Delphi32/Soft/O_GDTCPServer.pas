//------------------------------------------------------------------------------
// TCP Server für RM
//
// 06.02.2012  GD  Neu für Displayinterface
// 27.02.2014  GD  Allgemeines Objekt ausgegliedert in eigene Basisunit
//
// Copyright (C) RMG Messtechnik GmbH 2012, 2014
//------------------------------------------------------------------------------
unit O_GDTCPServer;

interface

uses
  Windows, SysUtils, Classes, ScktComp, ActiveX, ComObj, Forms,
  O_GDThread, GD_Utils, T_Zeit;

type
  TGDTcpServerThread = class(TGDThread)
  private
    FServerSocket : TServerSocket;
    FClientSocket : TCustomWinSocket;
    FCbStatus     : TNotifyString;
    FCbRx         : TNotifyString;
    FCbTx         : TNotifyString;
    FReceived     : string;
    FRequest      : string;
    FResponse     : string;
    FStatusText   : string;
    FRxText       : string;
    FTxText       : string;
  protected
    procedure InitializeThisThread(bState: boolean); override;
    procedure SetPort(iPort: integer);
    function MyExecuteAction: boolean; override;
    procedure HandleStatus(
      const sStatusText: string; iState: integer = 0); override;

    procedure HandleRx(const sRxText: string); virtual;
    procedure HandleTx(const sTxText: string); virtual;

    procedure SyncStatus; virtual;
    procedure SyncRx; virtual;
    procedure SyncTx; virtual;

    function Connect: boolean; virtual;
    function Disconnect: boolean; virtual;
    function IsCompleteRequest(var sRequest: string): boolean; virtual;
    procedure ServerSocketClientConnect(
      pSender: TObject; pSocket: TCustomWinSocket); virtual;
    procedure ServerSocketClientDisconnect(
      pSender: TObject; pSocket: TCustomWinSocket); virtual;
    procedure ServerSocketClientWrite(
      pSender: TObject; pSocket: TCustomWinSocket); virtual;

    property ServerSocket: TServerSocket read FServerSocket;
    property ClientSocket: TCustomWinSocket
      read FClientSocket write FClientSocket;
    property RxText: string read FRxText write FRxText;
    property TxText: string read FTxText write FTxText;
    property Request: string read FRequest write FRequest;
  public
    property CbStatus: TNotifyString write FCbStatus;
    property CbRx: TNotifyString read FCbRx write FCbRx;
    property CbTx: TNotifyString read FCbTx write FCbTx;
    property Port: integer write SetPort;
  end;

implementation

//------------------------------ TGDTcpServerThread ----------------------------

//---------------------------------------------
procedure TGDTcpServerThread.InitializeThisThread(bState: boolean);
//---------------------------------------------
begin
  inherited;

  if (bState) then begin
    TerminateOnFault := False;
    CoInitialize(nil);
    FClientSocket := nil;
    FCbStatus := nil;
    FCbRx := nil;
    FCbTx := nil;
    FRequest := '';
    FResponse := '';

    FServerSocket := TServerSocket.Create(nil);

    FServerSocket.Active := False;
    FServerSocket.Socket.Close;
    FServerSocket.Close;

    FServerSocket.ServerType := stNonBlocking;
    FServerSocket.Port := 9100;

    FServerSocket.OnClientConnect := Self.ServerSocketClientConnect;
    FServerSocket.OnClientDisconnect := Self.ServerSocketClientDisconnect;
  end
  else begin
    FClientSocket := nil;
    FCbRx := nil;
    FCbTx := nil;

    FServerSocket.Active := False;
    FServerSocket.Socket.Close;
    FServerSocket.Close;
    FreeAndNil(FServerSocket);

    FCbStatus := nil;
    CoUnInitialize;
  end;
end;

// Setzen des Ports für die Kommunikation
//---------------------------------------------
procedure TGDTcpServerThread.SetPort(iPort: integer);
//---------------------------------------------
begin
  if (Assigned(FServerSocket)) and (FServerSocket.Port <> iPort) then
    FServerSocket.Port := iPort; 
end;

//---------------------------------------------
function TGDTcpServerThread.MyExecuteAction: boolean;
//---------------------------------------------
begin
  try
    // Grundsätzlich Zugriff auf Socket?
    Result := (Assigned(FServerSocket)) and
      ((FServerSocket.Active) or (not FServerSocket.Active));
    if (Result) then
    try
      if (not FServerSocket.Active) then Connect;
    except
      // Kein Grund für Abbruch
      on E:Exception do begin
        HandleError(E.Message);
      end;
    end;

    Application.ProcessMessages;

    if (FServerSocket.Active) and (Assigned(FClientSocket)) then
    try
      ServerSocketClientWrite(FServerSocket, FClientSocket);
    except
      FClientSocket := nil;
      try
        Disconnect;
        Connect;
      except
      end;
    end;
  except
    // Abbruch
    on E:Exception do begin
      HandleError(E.Message);
      Result := False;
    end;
  end;
end;

//---------------------------------------------
procedure TGDTcpServerThread.HandleStatus(
  const sStatusText: string; iState: integer = 0);
//---------------------------------------------
begin
  inherited;

  FStatusText := sStatusText;
  if (Assigned(FCbStatus)) then Synchronize(SyncStatus);
end;

//---------------------------------------------
procedure TGDTcpServerThread.HandleRx(const sRxText: string);
//---------------------------------------------
begin
  if (Assigned(FCbRx)) then begin
    FRxText := sRxText;
    Synchronize(SyncRx);
  end;
end;

//---------------------------------------------
procedure TGDTcpServerThread.HandleTx(const sTxText: string);
//---------------------------------------------
begin
  if (Assigned(FCbTx)) then begin
    FTxText := sTxText;
    Synchronize(SyncTx);
  end;
end;

//---------------------------------------------
procedure TGDTcpServerThread.SyncStatus;
//---------------------------------------------
begin
  if (Assigned(FCbStatus)) then FCbStatus(0, FStatusText);
end;

//---------------------------------------------
procedure TGDTcpServerThread.SyncRx;
//---------------------------------------------
begin
  if (Assigned(FCbRx)) then FCbRx(1, FRxText);
end;

//---------------------------------------------
procedure TGDTcpServerThread.SyncTx;
//---------------------------------------------
begin
  if (Assigned(FCbTx)) then FCbTx(2, FTxText);
end;

//---------------------------------------------
function TGDTcpServerThread.Connect: boolean;
//---------------------------------------------
var
  iTo : Cardinal;
begin
  try
    FServerSocket.Active := False;
    FServerSocket.Socket.Close;
    FServerSocket.Close;
    FServerSocket.Open;

    iTO := GetTickCount + 10000;
    while (iTo > GetTickCount) and (not FServerSocket.Active) do Delay(1);

    Result := (FServerSocket.Active);
    HandleStatus('Server open (Port: ' + IntToStr(FServerSocket.Port) + '): ' +
      BoolToStr(Result, True));
  except
    try
      FServerSocket.Active := False;
      FServerSocket.Socket.Close;
      FServerSocket.Close;
    except
    end;
    Result := False;
  end;
end;

//---------------------------------------------
function TGDTcpServerThread.Disconnect: boolean;
//---------------------------------------------
var
  iTo : Cardinal;
begin
  try
    FServerSocket.Active := False;
    FServerSocket.Socket.Close;
    FServerSocket.Close;

    iTO := GetTickCount + 10000;
    while (iTo > GetTickCount) and (FServerSocket.Active) do Delay(1);

    Result := (not FServerSocket.Active);
    HandleStatus('Server closed: ' + BoolToStr(Result, True));
  except
    Result := False;
  end;
end;

//---------------------------------------------
function TGDTcpServerThread.IsCompleteRequest(var sRequest: string): boolean;
//---------------------------------------------
begin
  Result := False;
end;

//---------------------------------------------
procedure TGDTcpServerThread.ServerSocketClientConnect(
  pSender: TObject; pSocket: TCustomWinSocket);
//---------------------------------------------
var
  s : string;
begin
  FClientSocket := pSocket;
  try
    s := 'Connect:'#9 +
      FClientSocket.RemoteAddress + #9 + IntToStr(FClientSocket.RemotePort);
    HandleStatus(s);
    FReceived := '';
  except
    FClientSocket := nil;
  end;
end;

//---------------------------------------------
procedure TGDTcpServerThread.ServerSocketClientDisconnect(
  pSender: TObject; pSocket: TCustomWinSocket);
//---------------------------------------------
var
  s : string;
begin
  try
    s := 'Disconnect:'#9 +
      pSocket.RemoteAddress + #9 + IntToStr(pSocket.RemotePort);
  except
    s := 'Disconnect';
    FReceived := '';
  end;
  HandleStatus(s);
  FClientSocket := nil;
end;

//---------------------------------------------
procedure TGDTcpServerThread.ServerSocketClientWrite(pSender: TObject;
  pSocket: TCustomWinSocket);
//---------------------------------------------
var
  s : string;
begin
  FClientSocket := pSocket;
  try
    s := 'Request:'#9;

    FReceived := FReceived + pSocket.ReceiveText;
    if (IsCompleteRequest(FReceived)) then begin
      FRequest := FReceived;
      FReceived := '';
      HandleRx(s + FRequest);
    end;
  except
    FRequest := '';
    FClientSocket := nil;
  end;
end;

end.
