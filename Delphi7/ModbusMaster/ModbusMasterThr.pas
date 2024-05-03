{******************************************************************************}
{* Unit: Thread für serielle Kommunikation im Modbus-Master                   *}
{* 15.02.2010  WW                                                             *}
{* 25.11.2014  WN Austausch Memo -> Listbbox (Verbesserung Performance)       *}
{* 28.06.2017  WW Callback-Funktionen für Sende-/Empfangsdaten ersetzt durch  *}
{*                Funktion für Monitordaten; Anpassungen für Funktionscode;   *}
{*                mit Log-Datei für Modbus-Schreibvorgänge; Schreib-/Lesevor- *}
{*                gänge optional täglich zu fester Uhrzeit                    *}
{******************************************************************************}
unit ModbusMasterThr;

interface

uses
  Windows, Forms, Classes, SysUtils, StdCtrls,
  Serial, T_BinMask, DateUtils, PathIni,
  LogFile, WErrMsg, ModbusUtil, ModbusMasterConst, ModbusCmdExec,
  ModbusMasterRes;

type
  { COM-Thread für Modbus-Master }
  TModbusMasterCOMThread = class(TThread)
  private
    FTerminated: boolean;
    FCOMNr: integer;
    FCallingFormHandle: HWND;
    FLogFile: TCustomLogFile;
    FErrLogfile: TCustomLogFile;
    FWriteLogfile: TCustomLogFile;
    FSlaveListe: TSlaveList;
    FMCE: TModbusCmdExec;
    FCBCOM_Init: TCBModbusExecCOM_InitProc;
    FCBCOM_Open: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
    FCBCOM_Request: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
    FCBCOM_MonitorData: TCBModbusExecCOM_MonitorProc;  // 28.06.2017, WW
    FCBCOM_Fehler: TCBModbusExecCOM_StringProc;
    { Variablen für Synchronize-Aufruf von COM_InitProc: }
    FBaudrate: integer;
    FDatabits: TDataBits;
    FParityBit: TParityBit;
    FStopBits: TStopBits;
    FIpAddress : string;
    FIpPort    : integer;
    FModbusModus: TModbusModus;
    { Variablen für Synchronize-Aufruf von COM_OpenProc: }
    FFehlergruppe_COM_Open: integer;
    FFehlercode_COM_Open: integer;
    { Variablen für Synchronize-Aufruf von COM_RequestProc: }
    FFehlergruppe_COM_Request: integer;
    FFehlercode_COM_Request: integer;
    FFktCode_COM_Request: integer;  // 28.06.2017, WW
    { Variable für Synchronize-Aufruf von COM_MonitorDataProc: }
    FMonitorData: string;
    FMonitorDataType: TMonitorDataType;
    { Variable für Synchronize-Aufruf von COM_FehlerProc: }
    FFehler: string;
    procedure COM_InitProc;
    procedure COM_Init (ACOMNr: integer; ABaudrate: integer; ADatabits: TDataBits;
      AParityBit: TParityBit; AStopBits: TStopBits; const sIpAddress: string;
      iIpPort: integer; AModbusModus: TModbusModus);
    procedure COM_OpenProc;
    procedure COM_Open (ACOMNr, AFehlergruppe, AFehlercode: integer; AFktCode: byte);
    procedure COM_RequestProc;
    procedure COM_Request (ACOMNr, AFehlergruppe, AFehlercode: integer; AFktCode: byte);
    procedure COM_MonitorDataProc;
    procedure COM_MonitorData (ACOMNr: integer; AData: string;
      AMonitorDataType: TMonitorDataType);
    procedure COM_FehlerProc;
    procedure COM_Fehler (ACOMNr: integer; AFehler: string);
  protected
    procedure Execute; override;
  public
    constructor CreateIt (ACOMNr: integer;
      ADebugThreadProtokoll, ADebugCOMProtokoll, ADebugFehlerProtokoll,
      ADebugWriteProtokoll: boolean;
      ACallingFormHandle: HWND;
      ACBCOM_Init: TCBModbusExecCOM_InitProc;
      ACBCOM_Open: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
      ACBCOM_Request: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
      ACBCOM_MonitorData: TCBModbusExecCOM_MonitorProc;
      ACBCOM_Fehler: TCBModbusExecCOM_StringProc);
    destructor Destroy; override;
    procedure WaitForTermination;
  end;

  { Struktur für Modbus-COM-Steuerung }
  // 25.11.2014  WN
  TModbusCOMControl = record
    Thread: TModbusMasterCOMThread;  // der COM-Thread selbst
    MonitorMemo: TListBox;  // Listbox für Monitor-Anzeige
    ErrorMemo: TListBox;  // Memo für Fehler-Anzeige
  end;

  { Feld für Zugriff auf COM-Threads (Index = COM-Nr. }
//  TModbusCOMControls = array [1..MaxPossiblePort] of TModbusCOMControl;
  TModbusCOMControls = array [1..999] of TModbusCOMControl;

implementation

resourcestring
  SSlaveKonfigUngueltig = 'Slave-Konfigurationsdatei enthält ungültige Daten';
  SRegisterKonfigUngueltig = 'Register-Konfigurationsdatei enthält ungültige Daten';
  SUndefinierterKonfigFehler = 'Undefinierter Fehler in den Konfigurationsdaten';


{ TModbusMasterCOMThread }

{--------------------------------------------------------------------------}
constructor TModbusMasterCOMThread.CreateIt (ACOMNr: integer;
  ADebugThreadProtokoll, ADebugCOMProtokoll, ADebugFehlerProtokoll,
  ADebugWriteProtokoll: boolean;
  ACallingFormHandle: HWND;
  ACBCOM_Init: TCBModbusExecCOM_InitProc;
  ACBCOM_Open: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
  ACBCOM_Request: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
  ACBCOM_MonitorData: TCBModbusExecCOM_MonitorProc;
  ACBCOM_Fehler: TCBModbusExecCOM_StringProc);
{--------------------------------------------------------------------------}
var
  LogFilename: string;

begin
  FTerminated:=false;

  inherited Create (true); // Thread createn und gleich wieder anhalten
  FreeOnTerminate:=true;  // Thread soll sich beim Beenden selbst freigeben
  Priority:=tpNormal;     // Thread soll mit normaler Priorität ausgeführt werden

  FCOMNr:=ACOMNr;
  FCallingFormHandle:=ACallingFormHandle;
  FCBCOM_Init:=ACBCOM_Init;
  FCBCOM_Open:=ACBCOM_Open;
  FCBCOM_Request:=ACBCOM_Request;
  FCBCOM_MonitorData:=ACBCOM_MonitorData;
  FCBCOM_Fehler:=ACBCOM_Fehler;

  { Logfile für Protokollierung des Threadablaufs initialisieren, wenn lt. INI-Konfiguration
    eingestellt: }
  if ADebugThreadProtokoll then begin
    LogFilename:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_THR_' + Format ('%.3d', [FCOMNr]);
    FLogFile:=TCustomLogFile.Create (PathServer.Pathname[WLogDir], LogFilename, false);  // 28.06.2017, WW
  end else
    FLogFile:=nil;

  { Logfile für Schnittstellen-Fehlerprotokollierung initialisieren: }
  if ADebugFehlerProtokoll then begin
    LogFilename:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_ERR_' + Format ('%.3d', [FCOMNr]);
    FErrLogFile:=TCustomLogFile.Create (PathServer.Pathname[WLogDir], LogFilename, false);  // 28.06.2017, WW
  end else
    FErrLogFile:=nil;

  { Logfile für Protokollierung von Modbus-Schreibvorgängen initialisieren:
    28.06.2017, WW }
  if ADebugWriteProtokoll then begin
    LogFilename:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_WRITE_' + Format ('%.3d', [FCOMNr]);
    FWriteLogFile:=TCustomLogFile.Create (PathServer.Pathname[WLogDir], LogFilename, false);
  end else
    FWriteLogFile:=nil;

  { Slaveliste createn: }
  FSlaveListe:=TSlaveList.Create;

  { Objekt zur Ausführung der Modbus-Abfrage initialisieren: }
  FMCE:=TModbusCmdExec.Create (FCOMNr, ADebugCOMProtokoll, FErrLogfile, FWriteLogFile,
                               FCallingFormHandle, COM_Init, COM_Open, COM_Request,
                               COM_MonitorData, COM_Fehler);

  if FLogFile <> nil then
    FLogFile.Write ('Thread started (Program version: ' + CVersion_ModbusMaster + ')');  { Logfile-Protokollierung }
  Suspended:=false;         // Thread jetzt fortsetzen
end;

{-------------------------------------}
destructor TModbusMasterCOMThread.Destroy;
{-------------------------------------}
begin
  FMCE.Free;
  FSlaveListe.Free;
  FErrLogFile.Free;
  FWriteLogFile.Free;

  if FLogFile <> nil then
    FLogFile.Write ('Thread stopped (Program version: ' + CVersion_ModbusMaster + ')');    { Logfile-Protokollierung }
  FLogFile.Free;

  inherited Destroy;

  FTerminated:=true;
end;

{---------------------------------------}
procedure TModbusMasterCOMThread.Execute;
{---------------------------------------}

  // Gibt ausgehend von einer vorgegebenen Zeit den nächsten Zeitpunkt aus,
  // wobei basierend auf dem Stundenwechsel ein Intervall verwendet wird
  // Parameter: Letzter Zeitpunkt, Intervall
  // Rückgabe: Nächster Zeitpunkt
  //------------------------------------
  function SetToNextHourInterval(
    dtCall: TDateTime; iInterval_ms : integer): TDateTime;
  //------------------------------------
  var
    dt          : TDateTime;
    h, n, s, ms : word;
    iInterval : integer;

  begin
    if (iInterval_ms mod 1000) = 0 then begin  // volle Sekunden; 04.11.2011, WW
      iInterval:=iInterval_ms div 1000;

      if (dtCall = 0) then dtCall := IncSecond(Now, -iInterval);
      if (IncSecond(dtCall, iInterval) > Now) then dt := dtCall else dt := Now;
      // Auf Stunden normieren
      DecodeTime(dt, h, n, s, ms);
      if (iInterval <= 60)
      then dtCall := EncodeTime(h, n, 0, 0)
      else dtCall := EncodeTime(h, 0, 0, 0);
      dtCall := Trunc(dt) + dtCall;
      dt := IncSecond(dt);
      while (dtCall <= dt) do dtCall := IncSecond(dtCall, iInterval);
      Result := dtCall;
    end
    else begin  // krumme Sekunden; 04.11.2011, WW
      if (dtCall = 0) then
        dtCall := Now;
      Result:=IncMilliSecond (dtCall, iInterval_ms);
    end;
  end;


  //------------------------------------
  function SetToNextTime(dtCall: TDateTime; dtTime: TDateTime): TDateTime;
  //------------------------------------
  begin
    if dtCall > 0 then begin
      Result:=IncDay(dtCall);
    end
    else begin  // Init
      if Time > Frac(dtTime) then begin
        Result:=Date + Frac(dtTime);
        Result:=IncDay(Result);  // Morgen zur definierten Uhrzeit
      end else
        Result:=Date + Frac(dtTime);  // Heute zur definierten Uhrzeit
    end;
  end;     

{ Thread-Ausführung }
var
  Erg: integer;
  S: string;
  AbfrageAktiv: boolean;
  i: integer;
  SlaveData: TSlaveData;

begin
  { Über die COM abzufragende Modbus-Slaves und Modbus-Register in Liste laden: }
  Erg:=GetSlaveListe (FCOMNr, FSlaveListe);
  if Erg <> 0 then begin  // Fehler beim Lesen der Konfigurationsdaten
    if Assigned (FCBCOM_Request) OR Assigned (FErrLogFile) then begin
      case Erg of
        -1: S:=SSlaveKonfigUngueltig;
        -2: S:=SRegisterKonfigUngueltig;
      else
        S:=SUndefinierterKonfigFehler;
      end;

      COM_Fehler (FCOMNr, S);  // Ausgabe: Fehler

      if Assigned (FErrLogFile) then
        FErrLogFile.Write (S, true, lt_Error);  // Fehler protokollieren in Err-File
    end;
    AbfrageAktiv:=false;
  end else
    AbfrageAktiv:=true;

  { Abrufzeitpunkt für jeden Slave initialisieren: }
  for i:=0 to FSlaveListe.Count - 1 do begin
    with TSlaveDataObj (FSlaveListe [i]).Data do begin
      if Uhrzeit > -1 then  // wenn fester Zeitpunkt definiert; 28.06.2017, WW
        NextCallTime := SetToNextTime(0, Uhrzeit)
      else
        NextCallTime := SetToNextHourInterval(0, PollingZyklus);  // 04.11.2011, WW
    end;
  end;

  while true do begin
    try
      if (not FMCE.AbfrageAktiv) then begin
        FMCE.OpenModbus;
        Sleep(10000);
      end;

      if (AbfrageAktiv) and (FMCE.AbfrageAktiv) then begin
        // Überwachung der Zykluszeit jedes Slaves:
        for i:=0 to FSlaveListe.Count - 1 do begin
          Application.ProcessMessages;

          with TSlaveDataObj (FSlaveListe [i]).Data do begin
            if (Now >= NextCallTime) then begin
              if Uhrzeit > -1 then  // wenn fester Zeitpunkt definiert; 28.06.2017, WW
                NextCallTime := SetToNextTime(NextCallTime, Uhrzeit)
              else
                NextCallTime := SetToNextHourInterval(NextCallTime, PollingZyklus);  // 04.11.2011, WW

              SlaveData:=TSlaveDataObj (FSlaveListe [i]).Data;
              FMCE.SlaveAbfragen (SlaveData);  { Modbus-Abfrage eines Slave }
            end;
          end;
        end;  { for i }
      end;  { if AbfrageAktiv }

      if Terminated then
        Break  // Thread beendet sich
      else
        Sleep (1);  { Prozessorauslastung niedrig halten }
    except
      on E: Exception do begin
        if FLogFile <> nil then
          FLogFile.Write ('!!! ' + ExceptionErrorMsgStr (E) + ' !!!', true, lt_Error);  { Logfile-Protokollierung }
      end;
    end;
  end;  { while true }
end;

{--------------------------------------------------}
procedure TModbusMasterCOMThread.WaitForTermination;
{--------------------------------------------------}
{ Warten bis Thread beendet ist;
  -> "Trial-and-Error": TThread.WaitFor läßt in diesem Projekt keine Timer-Aufrufe
     mehr in Untermodulen zu ! Die Anwendung hängt dadurch in der Timeout-
     Überwachung der seriellen Modbus-Kommunikation. Daher hier ersatzweise eine
     einfache Abfrage des eingeführten FTerminated-Flags. Info: In WicomSrv tritt
     das Problem nicht auf (Grund unbekannt, evtl. liegts am zusätzlich vorhandenen
     Thread des Socketservers ?) }
begin
  while not FTerminated do begin
    Application.ProcessMessages;
    Sleep (1);
  end;
end;

{--------------------------------------------}
procedure TModbusMasterCOMThread.COM_InitProc;
{--------------------------------------------}
begin
  if Assigned (FCBCOM_Init) then FCBCOM_Init(FCOMNr, FBaudrate, FDatabits,
    FParityBit, FStopBits, FIpAddress, FIpPort, FModbusModus);
end;

{-----------------------------------------------------------------------------}
procedure TModbusMasterCOMThread.COM_Init (ACOMNr: integer; ABaudrate: integer;
  ADatabits: TDataBits; AParityBit: TParityBit; AStopBits: TStopBits;
  const sIpAddress: string; iIpPort: integer; AModbusModus: TModbusModus);
{-----------------------------------------------------------------------------}
begin
  FBaudrate:=ABaudrate;
  FDatabits:=ADatabits;
  FParityBit:=AParityBit;
  FStopBits:=AStopBits;
  FModbusModus:=AModbusModus;
  FIpAddress := sIpAddress;
  FIpPort := iIpPort;

  Synchronize (COM_InitProc);
end;

{--------------------------------------------}
procedure TModbusMasterCOMThread.COM_OpenProc;
{--------------------------------------------}
begin
  if Assigned (FCBCOM_Open) then
    FCBCOM_Open (FCOMNr, FFehlergruppe_COM_Open, FFehlercode_COM_Open, 0);
end;

{--------------------------------------------------------------------------------------}
procedure TModbusMasterCOMThread.COM_Open (ACOMNr, AFehlergruppe, AFehlercode: integer;
  AFktCode: byte);
{--------------------------------------------------------------------------------------}
begin
  FFehlergruppe_COM_Open:=AFehlergruppe;
  FFehlercode_COM_Open:=AFehlercode;

  Synchronize (COM_OpenProc);
end;

{-----------------------------------------------}
procedure TModbusMasterCOMThread.COM_RequestProc;
{-----------------------------------------------}
begin
  if Assigned (FCBCOM_Request) then
    FCBCOM_Request (FCOMNr, FFehlergruppe_COM_Request, FFehlercode_COM_Request,
      FFktCode_COM_Request);
end;

{----------------------------------------------------------------------------------------}
procedure TModbusMasterCOMThread.COM_Request (ACOMNr, AFehlergruppe, AFehlercode: integer;
  AFktCode: byte);
{----------------------------------------------------------------------------------------}
begin
  FFehlergruppe_COM_Request:=AFehlergruppe;
  FFehlercode_COM_Request:=AFehlercode;
  FFktCode_COM_Request:=AFktCode;

  Synchronize (COM_RequestProc);
end;

{---------------------------------------------------}
procedure TModbusMasterCOMThread.COM_MonitorDataProc;
{---------------------------------------------------}
begin
  if Assigned (FCBCOM_MonitorData) then
    FCBCOM_MonitorData (FCOMNr, FMonitorData, FMonitorDatatype);
end;

{-------------------------------------------------------------------------------}
procedure TModbusMasterCOMThread.COM_MonitorData (ACOMNr: integer; AData: string;
  AMonitorDataType: TMonitorDataType);
{-------------------------------------------------------------------------------}
begin
  FMonitorData:=AData;
  FMonitorDataType:=AMonitorDataType;

  Synchronize (COM_MonitorDataProc);
end;

{----------------------------------------------}
procedure TModbusMasterCOMThread.COM_FehlerProc;
{----------------------------------------------}
begin
  if Assigned (FCBCOM_Fehler) then
    FCBCOM_Fehler (FCOMNr, FFehler);
end;

{-----------------------------------------------------------------------------}
procedure TModbusMasterCOMThread.COM_Fehler (ACOMNr: integer; AFehler: string);
{-----------------------------------------------------------------------------}
begin
  FFehler:=AFehler;

  Synchronize (COM_FehlerProc);
end;

end.
