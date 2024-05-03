{------------------------------------------------------------------------------}
{ Hilfsroutinen für den Umgang mit NT-Diensten                                 }
{                                                                              }
{ 09.10.2004  GD  Neu                                                          }
{ 23.05.2005  GD  Nicht aktiver Service heißt: S_SERVICE_NoInfo                }
{ 07.12.2005  GD  Änderung in "WStopService"                                   }
{ 16.02.2006  GD  Zugriffsrechte herabgesetzt                                  }
{ 08.03.2024  WW  WAktServiceStateConfig                                       }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2004, 2024                                    }
{------------------------------------------------------------------------------}
unit Service_Utils;

interface

uses           
  Windows, WinSvc, Forms, SysUtils;

resourcestring
  S_SERVICE_NoInfo        = 'Keine Information zu Dienst';
  S_SERVICE_STOPPED       = 'Dienst ist gestoppt';
  S_SERVICE_START_PENDING = 'Dienst wird gestartet';
  S_SERVICE_STOP_PENDING  = 'Dienst wird gestoppt';
  S_SERVICE_RUNNING	  = 'Dienst ist gestartet';
  S_SERVICE_CONTINUE_PENDING = 'Dienst wird fortgesetzt';
  S_SERVICE_PAUSE_PENDING = 'Dienst wird angehalten';
  S_SERVICE_PAUSED        = 'Dienst ist angehalten';
  S_SERVICE_DISABLED      = 'Dienst ist deaktiviert';

(* Zugehörige Stati und Start Types in WinSvc
SERVICE_STOPPED	The service is not running.
SERVICE_START_PENDING	The service is starting.
SERVICE_STOP_PENDING	The service is stopping.
SERVICE_RUNNING	The service is running.
SERVICE_CONTINUE_PENDING	The service continue is pending.
SERVICE_PAUSE_PENDING	The service pause is pending.
SERVICE_PAUSED	The service is paused.
SERVICE_DISABLED The service is disabled.
*)

type
  TWServiceConfig = record
    ServiceType: cardinal;
    StartType: cardinal;
    ErrorControl: cardinal;
    BinaryPathName: string;
    LoadOrderGroup: string;
    TagId: cardinal;
    Dependencies: string;
    ServiceStartName: string;
    DisplayName: string;
  end;

function WOpenSCManager(
  iAccess: DWord = SC_MANAGER_CONNECT+SC_MANAGER_ENUMERATE_SERVICE): SC_HANDLE;
function WOpenService(iSCHandle: SC_HANDLE; sServiceName: string;
  iAccess: DWord = SERVICE_START+SERVICE_STOP+SERVICE_QUERY_STATUS): SC_HANDLE;
function WCloseServiceHandle(iSCHandle: SC_HANDLE): boolean;
function WQueryServiceStatus(iSrvHdl: SC_HANDLE): TServiceStatus;

function GetWindowServerHandle(sClassName: string): HWnd;
function WStartService(sServiceName: string): boolean;
function WStopService(sServiceName: string; iTimeOut: Cardinal): boolean;
function WAktServiceState(sServiceName: string): DWord;
function WAktServiceStateConfig(sServiceName: string;
  var WSC: TWServiceConfig): DWord;

implementation


{----------------------------- Lokale Funktionen ------------------------------}

{--------------------------------------------}
function WOpenSCManager(
  iAccess: DWord = SC_MANAGER_CONNECT+SC_MANAGER_ENUMERATE_SERVICE): SC_HANDLE;
{--------------------------------------------}
begin
  Result := OpenSCManager(nil, nil, iAccess);
end;

{--------------------------------------------}
function WOpenService(iSCHandle: SC_HANDLE; sServiceName: string;
  iAccess: DWord = SERVICE_START+SERVICE_STOP+SERVICE_QUERY_STATUS): SC_HANDLE;
{--------------------------------------------}
begin
  Result := OpenService(iSCHandle, PChar(sServiceName), iAccess);
end;

{--------------------------------------------}
function WCloseServiceHandle(iSCHandle: SC_HANDLE): boolean;
{--------------------------------------------}
begin
  Result := CloseServiceHandle(iSCHandle);
end;

{--------------------------------------------}
function WQueryServiceStatus(iSrvHdl: SC_HANDLE): TServiceStatus;
{--------------------------------------------}
begin
  FillChar(Result, SizeOf(TServiceStatus), 0);
  QueryServiceStatus(iSrvHdl, Result);
end;

// 08.03.2024, WW
{--------------------------------------------}
function WQueryServiceConfig(iSrvHdl: SC_HANDLE): TWServiceConfig;
{--------------------------------------------}
var
  pConfig: Pointer;
  iSize: DWord;

begin
  with Result do begin  // Vorbelegung
    ServiceType:=0;
    StartType:=0;
    ErrorControl:=0;
    BinaryPathName:='';
    LoadOrderGroup:='';
    TagId:=0;
    Dependencies:='';
    ServiceStartName:='';
    DisplayName:='';
  end;

  QueryServiceConfig(iSrvHdl, nil, 0, iSize);
  pConfig:=AllocMem(iSize);
  try
    if QueryServiceConfig(iSrvHdl, pConfig, iSize, iSize) then
      with Result do begin
        ServiceType:=PQueryServiceConfig(pConfig)^.dwServiceType;
        StartType:=PQueryServiceConfig(pConfig)^.dwStartType;
        ErrorControl:=PQueryServiceConfig(pConfig)^.dwErrorControl;
        BinaryPathName:=PQueryServiceConfig(pConfig)^.lpBinaryPathName;
        LoadOrderGroup:=PQueryServiceConfig(pConfig)^.lpLoadOrderGroup;
        TagId:=PQueryServiceConfig(pConfig)^.dwTagId;
        Dependencies:=PQueryServiceConfig(pConfig)^.lpDependencies;
        ServiceStartName:=PQueryServiceConfig(pConfig)^.lpServiceStartName;
        DisplayName:=PQueryServiceConfig(pConfig)^.lpDisplayName;
      end;
  finally
    Dispose(pConfig);
  end;
end;

{------------------------ Veröffentlichte Funktionen --------------------------}

{ Gibt Handle eines Fensters zurück                     }
{ Rückgabe: Handle oder 0                               }
{-------------------------------------------------------}
function GetWindowServerHandle(sClassName: string): HWnd;
{-------------------------------------------------------}
var
  pc   : PChar;
  iWnd : HWnd;
begin
  Result := 0;

  // Fensterhandle suchen und speichern
  GetMem(pc, 100);
  try
    iWnd:= FindWindow(PChar(sClassName), nil);
    if (iWnd <> 0) then Result := iWnd;
  finally
    FreeMem(pc, 100);
  end;
end;

{--------------------------------------------}
function WStartService(sServiceName: string): boolean;
{--------------------------------------------}
var
  iSCM, iSrvHdl  : SC_HANDLE;
  pArgv          : PChar;
  pServiceStatus : TServiceStatus;
begin
  Result := False;

  iSCM := WOpenSCManager;
  if (iSCM > 0) then
  try
    iSrvHdl := WOpenService(iSCM, sServiceName);
    if (iSrvHdl > 0) then
    try
      // Aktuellen Status abfragen
      pServiceStatus := WQueryServiceStatus(iSrvHdl);
      if (pServiceStatus.dwServiceType <> 0) then begin
        if ((pServiceStatus.dwCurrentState = SERVICE_RUNNING) or
            (pServiceStatus.dwCurrentState = SERVICE_START_PENDING))
        then Result := True
        else Result := (Winsvc.StartService(iSrvHdl, 0, pArgv));
      end;
    finally
      WCloseServiceHandle(iSrvHdl);
    end;
  finally
    WCloseServiceHandle(iSCM);
  end;
end;

{--------------------------------------------}
function WStopService(sServiceName: string; iTimeOut: Cardinal): boolean;
{--------------------------------------------}
var
  iSCM, iSrvHdl  : SC_HANDLE;
  pServiceStatus : TServiceStatus;
  iStop          : Cardinal;
begin
  Result := False;

  iSCM := WOpenSCManager;
  if (iSCM > 0) then
  try
    iSrvHdl := WOpenService(iSCM, sServiceName);
    if (iSrvHdl > 0) then
    try
      // Aktuellen Status abfragen
      pServiceStatus := WQueryServiceStatus(iSrvHdl);
      if (pServiceStatus.dwServiceType <> 0) then begin
        if ((pServiceStatus.dwCurrentState <> SERVICE_STOPPED) and
            (pServiceStatus.dwCurrentState <> SERVICE_STOP_PENDING)) then
        begin
          ControlService(iSrvHdl, SERVICE_CONTROL_STOP, pServiceStatus);
        end;

        // Warten, ob der Dienst innerhalb der Totzeit beendet wurde
        iStop := GetTickCount + iTimeOut;
        while ((GetTickCount < iStop) and
               (pServiceStatus.dwCurrentState <> SERVICE_STOPPED)) do
        begin
          Sleep(100);
          Application.ProcessMessages;
          pServiceStatus := WQueryServiceStatus(iSrvHdl);
          if (not (pServiceStatus.dwCurrentState in
            [SERVICE_STOPPED, SERVICE_STOP_PENDING]))
          then ControlService(iSrvHdl, SERVICE_CONTROL_STOP, pServiceStatus);
        end;

        Result := (pServiceStatus.dwCurrentState = SERVICE_STOPPED);
      end;
    finally
      WCloseServiceHandle(iSrvHdl);
    end;
  finally
    WCloseServiceHandle(iSCM);
  end;
end;

{--------------------------------------------}
function WAktServiceState(sServiceName: string): DWord;
{--------------------------------------------}
var
  iSCM, iSrvHdl  : SC_HANDLE;

begin
  Result := 0;

  iSCM := WOpenSCManager;
  if (iSCM > 0) then
  try
    iSrvHdl := WOpenService(iSCM, sServiceName);
    if (iSrvHdl > 0) then
    try
      // Aktuellen Status abfragen
      Result := WQueryServiceStatus(iSrvHdl).dwCurrentState;
    finally
      WCloseServiceHandle(iSrvHdl);
    end;
  finally
    WCloseServiceHandle(iSCM);
  end;
end;

// 08.03.2024, WW
{--------------------------------------------}
function WAktServiceStateConfig(sServiceName: string;
  var WSC: TWServiceConfig): DWord;
{--------------------------------------------}
var
  iSCM, iSrvHdl  : SC_HANDLE;

begin
  Result := 0;

  iSCM := WOpenSCManager;
  if (iSCM > 0) then
  try
    iSrvHdl := WOpenService(iSCM, sServiceName, SERVICE_START + SERVICE_STOP +
                            SERVICE_QUERY_STATUS + SERVICE_QUERY_CONFIG);
    if (iSrvHdl > 0) then
    try
      // Aktuellen Status abfragen
      Result := WQueryServiceStatus(iSrvHdl).dwCurrentState;
      // Aktuelle Konfiguration abfragen
      WSC := WQueryServiceConfig(iSrvHdl);
    finally
      WCloseServiceHandle(iSrvHdl);
    end;
  finally
    WCloseServiceHandle(iSCM);
  end;
end;

end.
