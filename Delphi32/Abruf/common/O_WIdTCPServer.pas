{******************************************************************************}
{* Erweiterte TIdTCPServer-Klasse (Indy 10)                                   *}
{* 03.06.2020 WW  Neu  Workarounds, Zusatzfunktion GetClientsConnectedCount   *}
{* 03.11.2020 WW  Timeout in SetActive auf 30s erhöht                         *}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2020                                      *}
{******************************************************************************}
unit O_WIdTCPServer;

interface

uses
  Forms, SysUtils, Classes, IdTCPServer, IdContext;

type

  { Erweiterte TIdTCPServer-Klasse }

  TWIdTCPServer = class(TIdTCPServer)
  private
    FLastExecute: TDateTime;
    function GetClientsConnectedCount: integer;
  protected
    function DoExecute(AContext: TIdContext): Boolean; override;
    procedure SetActive(AValue: Boolean); override;
  public
    constructor Create; overload;
    constructor Create(AOwner: TComponent); overload;
  end;

implementation

{ TWIdTCPServer }

{-------------------------------}
constructor TWIdTCPServer.Create;
{-------------------------------}
begin
  inherited Create;

  FLastExecute:=-1;
end;

{---------------------------------------------------}
constructor TWIdTCPServer.Create(AOwner: TComponent);
{---------------------------------------------------}
begin
  inherited Create(AOwner);

  FLastExecute:=-1;
end;

{--------------------------------------------------------------}
function TWIdTCPServer.DoExecute(AContext: TIdContext): Boolean;
{--------------------------------------------------------------}
begin
  // Zeitpunkt des letzten DoExecute-Aufrufs merken (für Schließen des TCP-Servers):
  FLastExecute:=Now;

  Result:=inherited DoExecute(AContext);
end;

{-------------------------------------------------}
procedure TWIdTCPServer.SetActive(AValue: Boolean);
{-------------------------------------------------}
begin
  if not AValue AND (FActive <> AValue) then begin
    // Prüfen, ob noch Clients verbunden sind; 03.06.2020, WW
    // Anm.: IdTCPServer.Active auf false setzen führt zum Hänger, wenn noch
    // Client-Verbindungen bestehen !
    while GetClientsConnectedCount > 0 do begin
      Application.ProcessMessages;

      if Now > (FLastExecute + EncodeTime (0, 0, 30, 0)) then
        Break;  // Timeout 30s nach letztem Execute-Aufruf gegen Endlosschleife;
                // 03.11.2020, WW
    end;
  end;

  inherited SetActive (AValue);
end;      

{-------------------------------------------------------}
function TWIdTCPServer.GetClientsConnectedCount: integer;
{-------------------------------------------------------}
{ Liefert die Anzahl der Clients, welche mit dem TCP-Server verbunden sind }
begin
  Result:=0;

  if Assigned (Self.Contexts) then begin
    with Self.Contexts.LockList do
    try
      Result := Count;
    finally
      Self.Contexts.UnlockList;
    end;
  end;
end;

end.

