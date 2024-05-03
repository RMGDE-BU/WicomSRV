{******************************************************************************}
{* Unit: Modbus-Kommunikation ASCII/RTU über serielle Schnittstelle           *}
{* 23.06.2009 WW                                                              *}
{* 04.08.2010 GD  An ModbusMaster angepasst                                   *}
{* 01.03.2013 GD  Durch Überladung mit ModbusMaster UND Dialog900 lauffähig   *}
{******************************************************************************}
unit O_SerialModbus;

interface

uses
  Windows, Classes, Serial, T_Zeit, ErrConst, LogCom, WComm, O_ModbusComm,
  CommUtil;

type
  { Objekt für Modbus-Kommunikation über die serielle Schnittstelle }

  TModbusSerialCommObj = class(TModbusCustomCommObj)
  private
    { Private-Deklarationen }
    FSerial    : TSerial;
    FComPort   : integer;
    FBaudrate  : integer;
    FDatabits  : TDataBits;
    FParityBit : TParityBit;
    FStopBits  : TStopBits;
  protected
    function Comm_SendData (sData: string): boolean; override;
    function Comm_ReceiveData: string; override;
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; override;
    procedure Comm_ClearRecBuf; override;
    procedure Comm_SetFehlerRueckgabe (var Rueckgabe: TRueckgabe); override;
  public
    { Public-Deklarationen }
    constructor Create; overload; override;
    constructor Create (AOwner: TComponent); reintroduce; overload; // 01.03.2013
    destructor Destroy; override;
    function Connect: integer; overload; override;
    function Connect (AComPort, ABaudrate: integer; ADatabits: TDataBits; // 01.03.2013
      AParityBit: TParityBit; AStopBits: TStopBits): integer; reintroduce; overload;
    procedure SetComSettings(iComPort, iBaudrate: integer; iDatabits: TDataBits;
      iParityBit: TParityBit; iStopBits: TStopBits);
    procedure Disconnect;
    property Serial: TSerial read FSerial;
  end;

implementation

{ TModbusSerialCommObj }

{-----------------------------------------------------------}
constructor TModbusSerialCommObj.Create;  // 04.08.2010
{-----------------------------------------------------------}
begin
  inherited Create;

  FComPort := 1;
  FBaudrate := 9600;
  FDatabits := db_8;
  FParityBit := none;
  FStopBits := sb_1;

  FSerial:=TSerial.Create(nil);
  with FSerial do begin
    { Puffergröße für Modbus-Kommunikation: }
    BufSizeTrm:=1024;                             { Sendepuffergröße }
    BufSizeRec:=1024;                             { Empfangspuffergröße }
    { RTS/CTS-Handshake ausschalten: }
    HandshakeRTSCTS:=false;
    { DTR- und RTS-Leitung setzen: }
    DTRActive:=true;
    RTSActive:=true;
  end;
end;

{-----------------------------------------------------------}
constructor TModbusSerialCommObj.Create (AOwner: TComponent);
{-----------------------------------------------------------}
begin
  Create;
end;

{--------------------------------------}
destructor TModbusSerialCommObj.Destroy;
{--------------------------------------}
begin
  CloseCOM (FSerial, FComLogFile);
  FSerial.Free;
  inherited Destroy;
end;

// Setzen der Schnittstellenparameter für serielle Schnittstelle
// Parameter: COM-Port, Baudrate, Datenbits, Parity, Stopbits
//---------------------------------------
procedure TModbusSerialCommObj.SetComSettings(iComPort, iBaudrate: integer;
  iDatabits: TDataBits; iParityBit: TParityBit; iStopBits: TStopBits); // 04.08.2010
//---------------------------------------
begin
  FComPort := iComPort;
  FBaudrate := iBaudrate;
  FDatabits := iDatabits;
  FParityBit := iParityBit;
  FStopBits := iStopBits;
end;

{-----------------------------------------------------------------------------}
function TModbusSerialCommObj.Connect: integer; // 04.08.2010
{-----------------------------------------------------------------------------}
{ Verfügbarkeit der seriellen Schnittstelle prüfen, Schnittstellenparameter setzen und
  Schnittstelle öffnen;
  Ergebnis:  0 = Schnittstelle konnte korrekt geöffnet werden
            -1 = Schnittstelle ist nicht vorhanden
            -2 = Schnittstelle konnte nicht geöffnet werden }
begin
  Result:=ConnectCOM (FSerial, FComLogFile, FComPort, FBaudrate, FDatabits,
                      FParityBit, FStopBits);
end;

{-----------------------------------------------------------------------------}
function TModbusSerialCommObj.Connect (AComPort, ABaudrate: integer;
  ADatabits: TDataBits; AParityBit: TParityBit; AStopBits: TStopBits): integer;
{-----------------------------------------------------------------------------}
{ Verfügbarkeit der seriellen Schnittstelle prüfen, Schnittstellenparameter setzen und
  Schnittstelle öffnen;
  Ergebnis:  0 = Schnittstelle konnte korrekt geöffnet werden
            -1 = Schnittstelle ist nicht vorhanden
            -2 = Schnittstelle konnte nicht geöffnet werden }
begin
  SetComSettings(AComPort, ABaudrate, ADatabits, AParityBit, AStopBits);
  Result := Connect;
end;

{----------------------------------------}
procedure TModbusSerialCommObj.Disconnect;
{----------------------------------------}
{ Serielle Schnittstelle schließen }
begin
  CloseCOM (FSerial, FComLogFile);
end;

{-------------------------------------------------------------------}
function TModbusSerialCommObj.Comm_SendData (sData: string): boolean;
{-------------------------------------------------------------------}
begin
  FSerial.TransmittText(sData);              { Daten in Sendepuffer schreiben }
  Result:=true;
end;

{-----------------------------------------------------}
function TModbusSerialCommObj.Comm_ReceiveData: string;
{-----------------------------------------------------}
var
  ComStat: TComStat;
  n: cardinal;
  S: string;

begin
  with FSerial do begin
    if ClearCommError(SerHandle,CommError,@ComStat) then              { Schnittstellenstatus abfragen }
      n:=ComStat.cbInQue  // Anzahl der Zeichen im Empfangspuffer
    else
      n:=0;

    if n > 0 then begin
      SetLength(S, n);  // String-Puffer bereitstellen
      SetLength(S, ReceiveData(Pointer(S)^, Length(S)));  // Zeichen aus Empfangspuffer lesen
    end else
      S:='';
  end;
  Result:=S;
end;

{---------------------------------------------------------------------------------}
function TModbusSerialCommObj.Comm_Connection (var Rueckgabe: TRueckgabe): boolean;
{---------------------------------------------------------------------------------}
begin
  Result:=true;  // Keine Verbindungsprüfung bei serieller Kommunikation
end;

{----------------------------------------------}
procedure TModbusSerialCommObj.Comm_ClearRecBuf;
{----------------------------------------------}
var
  ComStat: TComStat;

begin
  with FSerial do begin
    { Empfangspuffer leeren, solange noch Zeichen einlaufen: }
    while ClearCommError(SerHandle,CommError,@ComStat) do begin
      Delay (50);
      if ComStat.cbInQue > 0 then
        PurgeComm (SerHandle, PURGE_RXCLEAR)          { Empfangspuffer leeren }
      else
        Break;
    end;
  end;
end;

{-------------------------------------------------------------------------}
procedure TModbusSerialCommObj.Comm_SetFehlerRueckgabe (
  var Rueckgabe: TRueckgabe);
{-------------------------------------------------------------------------}
begin
  { Rueckgabe belegen mit Schnittstellenfehler: }
  with Rueckgabe do begin
    Fehlergruppe:=COM_ERROR;
    Fehlercode:=CommError;
    Antwort:='';
  end;
end;

end.

