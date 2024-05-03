{******************************************************************************}
{* Unit: Klasse für Ausführung der Modbus-Abfrage                             *}
{* 16.02.2010  WW                                                             *}
{* 18.11.2014  GD Erweiterung Debug-Einträge                                  *}
{* 28.06.2017  WW mit Modbusfunktionen 01 (Read Coil Status), 02 (Read Input  *}
{*                Status), 04 (Read Input Registers), 06 (Preset Single Regis-*}
{*                ter), 16 (Preset Multiple Registers)                        *}
{* 04.06.2018  WW Timeout für Versenden der Windows-Nachrichten erhöht        *}
{*                von 100 auf 1000 ms                                         *}
{* 16.12.2019  WW Transaktions-ID für Modbus TCP/IP                           *}
{* 20.03.2024  WW Bugfix IP900 erlaubt; Fehlertexte für IP-Verbindung         *}
{******************************************************************************}
unit ModbusCmdExec;

interface

uses
  Windows, Classes, Forms, SysUtils,
  Serial, SerialConst, T_BinMask, WChars, PathIni,
  LogCom, LogFile, O_SerialModbus, ModbusUtil, WComm, ModbusMasterIniFile,
  ModbusMasterRes, MBM_WriteData, O_ModbusComm, O_TcpModbus,
  ErrConst, ErrTxt, WSysCon, ModbusMasterConst, ModbusMasterUtil;

type
  { Callback-Prozedurtypen zur Anzeige in Fremdmodulen }
  TCBModbusExecCOM_InitProc = procedure (ACOMNr, ABaudrate: integer;
    ADatabits: TDataBits; AParityBit: TParityBit; AStopBits: TStopBits;
    const sIpAddress: string; iIpPort: integer;
    AModbusModus: TModbusModus) of object;

  TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc = procedure (
    ACOMNr, AFehlergruppe, AFehlercode: integer; AFktCode: byte) of object;

  TCBModbusExecCOM_StringProc = procedure (ACOMNr: integer; AStr: string) of object;

  TCBModbusExecCOM_MonitorProc = procedure (ACOMNr: integer; AStr: string;
    AMonitorDataType: TMonitorDataType) of object;

  { Objekt zur Ausführung der Modbus-Abfrage }
  TModbusCmdExec = class(TObject)
  private
    FCOMNr: integer;
    FComLogFile: TComLogFile;
    FErrLogfile: TCustomLogFile;
    FWriteLogfile: TCustomLogFile;
    FModbusCommObj: TModbusCustomCommObj;
    FBaudrate: integer;
    FDatabits: TDataBits;
    FParityBit: TParityBit;
    FStopBits: TStopBits;
    FIpPort     : integer;
    FIpAddress  : string;
    FModbusModus: TModbusModus;
    FTimeoutAntwort: integer;
    FTimeoutZeichen: integer;
    FAusgabeEmpfaengerWM: string;
    FNoCarrier: boolean;
    FLastFehlergruppe: integer;
    FLastFehlercode: integer;
    FLastFunktionscode: byte;
    FAbfrageAktiv: boolean;
    FCallingFormHandle: HWND;
    FReceiverFormHandle: HWND;
    FModbusTID: word;  // Transaktionsnummer für Modbus TCP/IP; 16.12.2019, WW

    FCBCOM_Init: TCBModbusExecCOM_InitProc;
    FCBCOM_Open: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
    FCBCOM_Request: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
    FCBCOM_MonitorData: TCBModbusExecCOM_MonitorProc;
    FCBCOM_Fehler: TCBModbusExecCOM_StringProc;
    procedure Init_FehlerGruppeCode (var Fehlergruppe: integer;
                                     var Fehlercode: integer);
    procedure GetIniEinstellungen;
    function SendRequest (SlaveData: TSlaveData;
      RegisterRequestData: TRegisterRequestData): boolean;
    function FormatMonitorData (sRohData: string): string;
    procedure Write_WriteLogfile (bResult: boolean;
      SlaveAdresse, Funktionscode: byte; StartAdresse, Anzahl: word;
      WertTyp_Einstellen, Wert_Einstellen_Def, Wert_Einstellen_BinData,
      InfoWert_Einstellen_Aktuell: string);
    procedure SendData_WindowsMessage(SlaveAdresse: byte;
      RegisterKonvListe: TRegisterKonvList);
  public
    constructor Create (ACOMNr: integer;
      ADebugCOMProtokoll: boolean; AErrLogfile, AWriteLogfile: TCustomLogFile;
      ACallingFormHandle: HWND;
      ACBCOM_Init: TCBModbusExecCOM_InitProc;
      ACBCOM_Open: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
      ACBCOM_Request: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
      ACBCOM_MonitorData: TCBModbusExecCOM_MonitorProc;
      ACBCOM_Fehler: TCBModbusExecCOM_StringProc);
    destructor Destroy; override;
    function OpenModbus: boolean;
    procedure SlaveAbfragen (SlaveData: TSlaveData);

    property AbfrageAktiv: boolean read FAbfrageAktiv;
  end;

implementation

{ TModbusCmdExec }

{---------------------------------------------------------}
constructor TModbusCmdExec.Create (ACOMNr: integer;
  ADebugCOMProtokoll: boolean; AErrLogfile, AWriteLogfile: TCustomLogFile;
  ACallingFormHandle: HWND;
  ACBCOM_Init: TCBModbusExecCOM_InitProc;
  ACBCOM_Open: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
  ACBCOM_Request: TCBModbusExecCOM_FehlerGruppeCode_FktCodeProc;
  ACBCOM_MonitorData: TCBModbusExecCOM_MonitorProc;
  ACBCOM_Fehler: TCBModbusExecCOM_StringProc);
{---------------------------------------------------------}
begin
  inherited Create;
  FCOMNr:=ACOMNr;
  FErrLogfile:=AErrLogfile;
  FWriteLogfile:=AWriteLogfile;
  FCallingFormHandle:=ACallingFormHandle;
  FReceiverFormHandle := 0;

  FCBCOM_Init:=ACBCOM_Init;
  FCBCOM_Open:=ACBCOM_Open;
  FCBCOM_Request:=ACBCOM_Request;
  FCBCOM_MonitorData:=ACBCOM_MonitorData;
  FCBCOM_Fehler:=ACBCOM_Fehler;

  FAbfrageAktiv:=false;
  FNoCarrier:=true;
  { Vorbelegung für letzte gemerkte Fehlergruppe/-code/Funktionscode: unbekannt }
  FLastFehlergruppe:=-1;
  FLastFehlercode:=-1;
  FLastFunktionscode:=0;

  { Modbus }
  FModbusTID:=InitTID_ModbusTCPIP;  // Transaktionsnummer für Modbus TCP/IP initialisieren; 16.12.2019 WW

  { Logfile für Schnittstellen-Protokollierung initialisieren: }
  if ADebugCOMProtokoll then
    FComLogFile:=TComLogFile.Create (PathServer.Pathname[WLogDir],  // 28.06.2017, WW
                                     FCOMNr, false,
                                     ChangeFileExt (ExtractFileName (ParamStr(0)), ''))
  else
    FComLogFile:=nil;

  GetIniEinstellungen;  // INI-Einstellungen für Modbus-Kommunikation lesen

  if Assigned (FCBCOM_Init) then
    FCBCOM_Init (FCOMNr, FBaudrate, FDatabits, FParityBit, FStopBits,
      FIpAddress, FIpPort, FModbusModus);  // Ausgabe: COM-Initialisierung

  { Objekt für serielle Modbus-Kommunikation initialisieren: }
  if (FCOMNr <= MaxPossiblePort) then
    FModbusCommObj:=TModbusSerialCommObj.Create
  else if (FCOMNr >= CComTCP_IP) then  // Bugfix IP900 erlaubt; 20.03.2024, WW
    FModbusCommObj := TModbusTcpCommObj.Create;

  FModbusCommObj.SetVersuche (1);  // im Pollingbetrieb reicht 1 Versuch
  { Modbus-Modus im Kommunikationsobjekt zuweisen: }
  FModbusCommObj.Modus:=FModbusModus;
  { Zeiger auf COM-Logfile im Kommunikationsobjekt zuweisen: }
  if Assigned (FModbusCommObj) then
    FModbusCommObj.ComLogfile:=FComLogfile;

  if (not OpenModbus) then exit;  { Modbus-Schnittstelle öffnen }
end;

{--------------------------------}
destructor TModbusCmdExec.Destroy;
{--------------------------------}
begin
  FModbusCommObj.Free;
  FComLogFile.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------}
procedure TModbusCmdExec.Init_FehlerGruppeCode (var Fehlergruppe: integer;
                                                var Fehlercode: integer);
{------------------------------------------------------------------------}
{ Fehlergruppe und Fehlercode mit "OK" vorbelegen;
  Übergabe/Rückgabe: AFehlergruppe
                     AFehlercode }
begin
  Fehlergruppe:=0;
  Fehlercode:=0;
end;

{-------------------------------------------}
procedure TModbusCmdExec.GetIniEinstellungen;
{-------------------------------------------}
{ INI-Einstellungen für Modbus-Kommunikation lesen }
var
  MI: TModbusMasterIni;

begin
  MI:=TModbusMasterIni.Create;
  try
    FBaudrate:=MI.Baud [FCOMNr];
    FDatabits:=MI.Databits [FCOMNr];
    FParityBit:=MI.Parity [FCOMNr];
    FStopBits:=MI.Stopbits [FCOMNr];
    FIpPort := MI.IPPort[FCOMNr];
    FIpAddress := MI.IPAddress[FCOMNr];
    FTimeoutAntwort:=MI.TOAntwort [FCOMNr];
    FTimeoutZeichen:=MI.TOZeichen [FCOMNr];
    FModbusModus:=MI.ModbusModus [FCOMNr];
    FAusgabeEmpfaengerWM:=MI.AusgabeEmpfaengerWM;
    FReceiverFormHandle := MI.AusgabeEmpfaengerHWND;
    if (Assigned(FCBCOM_Fehler)) then begin  // 18.11.2014
      FCBCOM_Fehler(Self.FCOMNr, 'INFO - Receiver window: ' +
        FAusgabeEmpfaengerWM);
      FCBCOM_Fehler(Self.FCOMNr, 'INFO - Receiver window handle: ' +
        IntToStr(FReceiverFormHandle));
    end;
  finally
    MI.Free;
  end;
end;

{------------------------------------------}
function TModbusCmdExec.OpenModbus: boolean;
{------------------------------------------}
{ Modbus-Schnittstelle öffnen;
  Ergebnis: true, wenn Öffnen der Schnittstelle erfolgreich }
var
  Erg: integer;
  AFehlergruppe: integer;
  AFehlercode: integer;

begin
  Result:=false;
  Init_FehlerGruppeCode (AFehlergruppe, AFehlercode);     // Vorbelegung für Fehlergruppe/-code: OK

  try
    if (FModbusCommObj is TModbusSerialCommObj) then
      TModbusSerialCommObj(FModbusCommObj).SetComSettings(
        FCOMNr, FBaudrate, FDatabits, FParityBit, FStopBits)
    else if (FModbusCommObj is TModbusTcpCommObj) then
      TModbusTcpCommObj(FModbusCommObj).SetIpSettings(
        FIpAddress, FIpPort);
    Erg := FModbusCommObj.Connect;
    case Erg of
      -1: begin
            if (FModbusCommObj is TModbusSerialCommObj) then begin
              AFehlergruppe:=COM_PORTERROR;
              AFehlercode:=COMPORTERR_NICHTVORHANDEN;
            end
            else if (FModbusCommObj is TModbusTcpCommObj) then begin  // 20.03.2024, WW
              AFehlergruppe:=COM_TCPIP_ERROR;
              AFehlercode:=TCPIP_ERR_TIMEOUTCONNECT;
            end;
            exit;
          end;
      -2: begin
            if (FModbusCommObj is TModbusSerialCommObj) then begin
              AFehlergruppe:=COM_PORTERROR;
              AFehlercode:=COMPORTERR_OEFFNEN;
            end
            else if (FModbusCommObj is TModbusTcpCommObj) then begin  // 20.03.2024, WW
              AFehlergruppe:=COM_TCPIP_ERROR;
              AFehlercode:=TCPIP_ERR_CONNECT;
            end;
            exit;
          end;
    end;
    FNoCarrier:=false;
    Result:=true;
  finally
    FAbfrageAktiv := Result;
    if Assigned (FCBCOM_Open) then
      FCBCOM_Open (FCOMNr, AFehlergruppe, AFehlercode, 0);  // Ausgabe: COM-Öffnen
  end;
end;

{-------------------------------------------------------------}
procedure TModbusCmdExec.SlaveAbfragen (SlaveData: TSlaveData);
{-------------------------------------------------------------}
{ Modbus-Kommandos für einen Slave versenden;
  Übergabe: Slave-Daten }
var
  RegisterRequestData: TRegisterRequestData;
  i: integer;

begin
  if not FAbfrageAktiv then exit;

  if Assigned (SlaveData.RegisterRequestListe) then begin
    // alle für den Slave definierten Register abfragen:
    for i:=0 to SlaveData.RegisterRequestListe.Count - 1 do begin
      Application.ProcessMessages;

      RegisterRequestData:=
        TRegisterRequestDataObj (SlaveData.RegisterRequestListe [i]).Data;

      // Modbus-Request versenden, Response empfangen:
      if not SendRequest (SlaveData, RegisterRequestData) then;
    end;  { for i }
  end;
end;

{---------------------------------------------------------}
function TModbusCmdExec.SendRequest (SlaveData: TSlaveData;
  RegisterRequestData: TRegisterRequestData): boolean;
{---------------------------------------------------------}
{ Request an Slave senden, Antwort empfangen, konvertieren und anzeigen;
  Übergaben: Slave-Daten
             Register-Requestdaten
  Ergebnis: true, wenn Senden des Requests erfolgreich }
var
  sQuery: string;
  iStartAdresse_Index0: word;
  R: TRueckgabe;
  AFehlergruppe: integer;
  AFehlercode: integer;
  S: string;
  sErr: string;
  sRequestBinData: string;
  bPreset: boolean;
  sInfoWert_Aktuell: string;
  iNoOfPoints: word;
  sDummy: string;

begin
  Result:=false;
  Init_FehlerGruppeCode (AFehlergruppe, AFehlercode);  // Vorbelegung für Fehlergruppe/-code: OK
  sErr:='';
  try
    // Indizierte Startadresse, bei 0 beginnend:
    iStartAdresse_Index0:=Get_Modbus_StartAdresse_Index0 (RegisterRequestData.StartAdresse);

    // Anzahl Register oder Stati ("No. of Points"):
    iNoOfPoints:=Get_Modbus_NoOfPoints(RegisterRequestData);

    // Query bilden:
    // 16.12.2019, WW: mit TID
    if not Build_Modbus_Query(FModbusModus, SlaveData, RegisterRequestData,
                              iStartAdresse_Index0, iNoOfPoints, sQuery,
                              sRequestBinData, sInfoWert_Aktuell, sErr, FModbusTID) then exit;

    with RegisterRequestData do begin  // 08.03.2019, WW
      with SlaveData do begin  // 08.03.2019, WW
        if Assigned (FCBCOM_MonitorData) then begin
          S:=FormatMonitorData (sQuery);
          FCBCOM_MonitorData (FCOMNr, S, mdt_Tx);  // Ausgabe: Sendedaten (Query)
        end;

        // Query versenden, Antwort empfangen:
        if not FModbusCommObj.SendQuery (sQuery, FTimeoutAntwort, FTimeoutZeichen,
                                         R, FNoCarrier) then begin
          AFehlergruppe:=R.Fehlergruppe;
          AFehlercode:=R.Fehlercode;
          exit;
        end;

        if Assigned (FCBCOM_MonitorData) then begin
          S:=FormatMonitorData (R.Antwort);
          FCBCOM_MonitorData (FCOMNr, S, mdt_Rx);  // Ausgabe: Empfangsdaten (Response)
        end;

        // Response auf Gültigkeit prüfen:
        // 16.12.2019, WW: mit Soll-TID
        if not Valid_Modbus_Response (SlaveAdresse, FktCode, FModbusTID, FModbusModus,
                                      R.Antwort, AFehlergruppe, AFehlercode) then begin
          sErr:=Format ('Invalid modbus response (function %.2d): ', [FktCode]);
          exit;
        end;

        // Response auswerten und konvertieren:
        if not Konv_Modbus_Response (FModbusModus, SlaveData, RegisterRequestData,
                                     iStartAdresse_Index0, iNoOfPoints,
                                     R.Antwort, sRequestBinData, bPreset, sDummy,
                                     AFehlergruppe, AFehlercode, sErr) then exit;

        // Bei Einstell-Vorgang das Ergebnis in Write-Logdatei schreiben:
        case FktCode of
          6: begin  // 28.06.2017, WW
               Write_WriteLogfile (bPreset, SlaveAdresse, FktCode, StartAdresse,
                 iNoOfPoints, Typ, Wert_Einstellen, sRequestBinData,
                 sInfoWert_Aktuell);
             end;

         16: begin  // 28.06.2017, WW
               Write_WriteLogfile (bPreset, SlaveAdresse, FktCode, StartAdresse,
                 iNoOfPoints, Typ, Wert_Einstellen, sRequestBinData,
                 sInfoWert_Aktuell);
             end;
        end;

        { Gelesene Registerwert(e) weiterleiten per Windows-Message (eingestellte nicht): }
        if not ((FktCode = 6) OR (FktCode = 16)) then
          SendData_WindowsMessage (SlaveAdresse, RegisterKonvListe);
      end;  // with SlaveData
    end;  // with RegisterRequestData
    Result:=true;
  finally
    if Assigned (FCBCOM_Fehler) OR Assigned (FErrLogFile) then begin
      S:=sErr;
      if not FehlerGruppeCode_OK (AFehlergruppe, AFehlercode) then  // Fehler bei Request
        S:=S + GetStatusText (AFehlergruppe) + ' - ' +
           GetErrorText (AFehlergruppe, AFehlercode);

      if S <> '' then begin
        if Assigned (FCBCOM_Fehler) then
          FCBCOM_Fehler (FCOMNr, S);  // Ausgabe: Fehler

        if Assigned (FErrLogFile) then
          FErrLogFile.Write (S, true, lt_Error);  // Fehler protokollieren in Err-File
      end;
    end;

    // Aktuellen Status neu anzeigen, wenn sich Fehlergruppe/-code geändert
    // haben (Statuswechsel):
    if not ((AFehlergruppe = FLastFehlergruppe) AND
            (AFehlercode = FLastFehlercode) AND
            (RegisterRequestData.FktCode = FLastFunktionscode)) then begin
      if Assigned (FCBCOM_Request) then
        FCBCOM_Request (FCOMNr, AFehlergruppe, AFehlercode,
          RegisterRequestData.FktCode);  // Ausgabe: COM-Abfrage

      // Fehlergruppe/-code merken:
      FLastFehlergruppe:=AFehlergruppe;
      FLastFehlercode:=AFehlercode;
      FLastFunktionscode:=RegisterRequestData.FktCode;  // 08.03.2019, WW 
    end;
  end;
end;

{-------------------------------------------------------------------}
function TModbusCmdExec.FormatMonitorData (sRohData: string): string;
{-------------------------------------------------------------------}
begin
  if (FModbusModus in  [modbus_RTU, modbus_TCPIP]) then
    Result:=Bin2Hex (sRohData, true)  // Hex mit Space als Trenner
  else
    Result:=SonderzeichenString (sRohData);
end;

{-----------------------------------------------------------------}
procedure TModbusCmdExec.Write_WriteLogfile (bResult: boolean;
  SlaveAdresse, Funktionscode: byte; StartAdresse, Anzahl: word;
  WertTyp_Einstellen, Wert_Einstellen_Def, Wert_Einstellen_BinData,
  InfoWert_Einstellen_Aktuell: string);
{-----------------------------------------------------------------}
{ Schreibt Ergebnis eines Modbus-Schreibvorgangs in Write-Logfile und
  MonitorData-Ausgabe; 28.06.2017, WW
  Übergaben: Ergebnis des Schreibvorgangs (erfolgreich/nicht erfolgreich)
             Adresse des Modbus-Slave
             Funktionscode
             Startadresse (eines Registers oder eines Status)
             Anzahl ("No. of Points" für Register oder Stati)
             Typ des geschriebenen Wertes
             Definierter Schreib-Wert (typabhängiges Format)
             Tatsächlich geschriebener Wert im Binär-Format
             Tatsächlich geschriebener Wert (lesbar, zur Info) }
var
  LogType: TLogType;
  S: string;

begin
  if Assigned (FWriteLogFile) then begin
    if bResult then begin  // Write-Vorgang erfolgreich
      S:='Write result: Successful';
      LogType:=lt_Info;
    end
    else begin
      S:='Write result: Not successful';
      LogType:=lt_Error;
    end;

    S:=S + ' (Line: ' + IntToStr(FCOMNr) +
         ', Slave: ' + IntToStr(SlaveAdresse) +
         ', Function: ' + IntToStr(Funktionscode) +
         ', Start addr.: ' + IntToStr(StartAdresse) +
         ', No. of points: ' + IntToStr(Anzahl) +
         ', Value type: ' + WertTyp_Einstellen +
         ', Defined value: ' + Wert_Einstellen_Def +
         ', Actual value: ' + InfoWert_Einstellen_Aktuell +
         ', BinData: ' + FormatMonitorData (Wert_Einstellen_BinData) +
         ')';
    FWriteLogFile.Write (S, true, LogType);  // in Write-Logfile schreiben

    if Assigned (FCBCOM_MonitorData) then
      FCBCOM_MonitorData (FCOMNr, S, mdt_None);  // Monitor-Ausgabe
  end;
end;

{-------------------------------------------------------------------}
procedure TModbusCmdExec.SendData_WindowsMessage (SlaveAdresse: byte;
  RegisterKonvListe: TRegisterKonvList);
{-------------------------------------------------------------------}
{ Gelesene Registerwert(e) weiterleiten per Windows-Message;
  Übergaben: Slave-Adresse
             Register-Konvertierungsliste }
var
  RegDataWM: TRegisterData_WindowsMessage;
  i: integer;
  S: string;

begin
  { Gelesene Registerwert(e) weiterleiten per Windows-Message: }
  for i:=0 to RegisterKonvListe.Count - 1 do begin
    RegDataWM.Linie:=FCOMNr;
    RegDataWM.SlaveAdr:=SlaveAdresse;
    RegDataWM.RegisterAdr:=TRegisterKonvDataObj (RegisterKonvListe [i]).Data.StartAdresse;
    RegDataWM.WertTyp:=TRegisterKonvDataObj (RegisterKonvListe [i]).Data.Typ;
    RegDataWM.Wert:=TRegisterKonvDataObj (RegisterKonvListe [i]).Data.Wert;
    RegDataWM.FktCode:=TRegisterKonvDataObj (RegisterKonvListe [i]).Data.FktCode;  // 28.06.2017, WW

    // Timeout für Versenden erhöht von 100 auf 1000 ms; 04.06.2018
    if (FReceiverFormHandle <= 0) then // Kein Empfänger-Fensterhandle definiert ?
      FReceiverFormHandle := WriteRegisterData_WindowsMessage(
        FCallingFormHandle, FAusgabeEmpfaengerWM, RegDataWM, 1000)
    else
      FReceiverFormHandle := WriteRegisterData_WindowsMessage(
        FCallingFormHandle, FReceiverFormHandle, RegDataWM, 1000);

    if (FAusgabeEmpfaengerWM <> '') and (FReceiverFormHandle = 0) then begin
      S:='Invalid receiver window: ' + FAusgabeEmpfaengerWM;

      if Assigned (FCBCOM_Fehler) then
        FCBCOM_Fehler (FCOMNr, S);  // Ausgabe: Fehler

      if (Assigned (FErrLogFile)) then  // 18.11.2014
        FErrLogFile.Write (S, true, lt_Error);
    end;

    if (FAusgabeEmpfaengerWM <> '') and (Assigned (FCBCOM_MonitorData)) then begin
      S := 'Send to receiver: ' + FAusgabeEmpfaengerWM +
           ', Handle: ' + IntToStr(FReceiverFormHandle) +
           ', Line: ' + IntToStr(RegDataWM.Linie) +
           ', Slave: ' + IntToStr(RegDataWM.SlaveAdr) +
           ', Function: ' + IntToStr(RegDataWM.FktCode) +
           ', Start addr.: ' + IntToStr(RegDataWM.RegisterAdr) +
           ', Value: ' + RegDataWM.Wert;  // 18.11.2014
      FCBCOM_MonitorData (FCOMNr, S, mdt_None);  // Monitor-Ausgabe: Windows-Message
    end;
  end;  { for i }
end;

end.
