{******************************************************************************}
{* Unit: Basisroutinen für serielle und Socket-Kommunikation                  *}
{* 23.06.2009  WW                                                             *}
{* 02.08.2011  WW  Erweiterung Modem-Kommunikation: ValidModemConnectAnswer   *}
{******************************************************************************}
unit CommUtil;

interface

uses
  SysUtils, scktcomp, WStrUtils, Serial, LogCom, ErrConst;


function OpenCOM (ASerial: TSerial; AComLogfile: TComLogFile): boolean;

procedure CloseCOM (ASerial: TSerial; AComLogfile: TComLogFile);

function ConnectCOM (ASerial: TSerial; AComLogfile: TComLogFile;
  AComPort: integer; ABaudrate: integer; ADatabits: TDataBits;
  AParityBit: TParityBit; AStopBits: TStopBits): integer;

procedure SocketErrorEventToFehlerGruppeCode (AErrorEvent: TErrorEvent;
  var AFehlergruppe: integer; var AFehlercode: integer);

function ValidModemConnectAnswer (ModemAnswer: string; var AFehlergruppe: integer;
  var AFehlercode: integer): boolean;

implementation

{------------------------- Serielle Kommunikation -----------------------------}

{---------------------------------------------------------------------}
function OpenCOM (ASerial: TSerial; AComLogfile: TComLogFile): boolean;
{---------------------------------------------------------------------}
{ Serielle Schnittstelle öffnen;
  Übergabe: Zeiger auf serielle Komponente
            Zeiger auf COM-Logfile }
begin
  Result:=true;
  if AComLogFile <> nil then
    AComLogFile.WriteMsg ('COM öffnen...');  { Logfileeintrag }

  if not ASerial.OpenComm then begin
    Result:=false;
    exit;
  end;

  if AComLogFile <> nil then
    AComLogFile.WriteMsg ('COM ist geöffnet');  { Logfileeintrag }
end;

{--------------------------------------------------------------}
procedure CloseCOM (ASerial: TSerial; AComLogfile: TComLogFile);
{--------------------------------------------------------------}
{ Serielle Schnittstelle schließen;
  Übergabe: Zeiger auf serielle Komponente
            Zeiger auf COM-Logfile }
begin
  if ASerial.Active then begin  // 13.02.2012, WW
    if AComLogFile <> nil then
      AComLogFile.WriteMsg ('COM schließen...');  { Logfileeintrag }

    ASerial.CloseComm;

    if AComLogFile <> nil then
      AComLogFile.WriteMsg ('COM ist geschlossen');  { Logfileeintrag }
  end;
end;

{--------------------------------------------------------------}
function ConnectCOM (ASerial: TSerial; AComLogfile: TComLogFile;
  AComPort: integer; ABaudrate: integer; ADatabits: TDataBits;
  AParityBit: TParityBit; AStopBits: TStopBits): integer;
{--------------------------------------------------------------}
{ Verfügbarkeit der seriellen Schnittstelle prüfen, Schnittstellenparameter setzen und
  Schnittstelle öffnen;
  Übergabe: Zeiger auf serielle Komponente
            Zeiger auf COM-Logfile
            AComPort
            ABaudrate
            ADataBits
            AParityBit
            AStopBit
  Ergebnis:  0 = Schnittstelle konnte korrekt geöffnet werden
            -1 = Schnittstelle ist nicht vorhanden
            -2 = Schnittstelle konnte nicht geöffnet werden }
var
  i: integer;
  PortOK: boolean;

begin
  Result:=0;

  { prüfen, ob COM vorhanden ist: }
  ASerial.AktuOnePort (AComPort);  // 05.10.2011, WW
  PortOK:=false;
  for i:=0 to ASerial.Ports.Count-1 do begin
    PortOK:=ASerial.Ports [i] = 'COM' + IntToStr (ACOMPort);
    if PortOK then Break;
  end;
  if not PortOK then begin
    if AComLogFile <> nil then
      AComLogFile.WriteMsg ('COM nicht vorhanden');  { Logfileeintrag }

    Result:=-1;
    exit;
  end;

  { Schnittstellen-Parameter belegen: }
  with ASerial do begin
    COMPort:=AComPort;
    Baudrate:=KonvertBaudrate (ABaudrate);
    DataBits:=ADataBits;
    ParityBit:=AParityBit;
    StopBits:=AStopBits;

    { Schnittstelle öffnen: }
    if not OpenCOM (ASerial, AComLogFile) then begin
      Result:=-2;
      exit;
    end;
  end;
end;


{----------------------- Socket-Kommunikation ---------------------------------}

{---------------------------------------------------------------------}
procedure SocketErrorEventToFehlerGruppeCode (AErrorEvent: TErrorEvent;
  var AFehlergruppe: integer; var AFehlercode: integer);
{---------------------------------------------------------------------}
{ Umsetzung Socket-ErrorEvent -> Fehlergruppe, Fehlercode }
begin
  AFehlergruppe:=COM_TCPIP_ERROR;

  case AErrorEvent of
    eeGeneral:    AFehlercode:=TCPIP_ERR_GENERAL;
    eeConnect:    AFehlercode:=TCPIP_ERR_CONNECT;
    eeDisconnect: AFehlercode:=TCPIP_ERR_DISCONNECT;
    eeReceive:    AFehlercode:=TCPIP_ERR_RECEIVE;
    eeSend:       AFehlercode:=TCPIP_ERR_SEND;
    eeAccept:     AFehlercode:=TCPIP_ERR_ACCEPT;
    eeLookup:     AFehlercode:=TCPIP_ERR_LOOKUP;  // 10.01.2007, WW
  else
    AFehlercode:=TCPIP_ERR_UNDEFINIERT;  // 10.01.2007, WW
  end;
end;


{------------------------ Modem-Kommunikation ---------------------------------}

{--------------------------------------------------------------------------------}
function ValidModemConnectAnswer (ModemAnswer: string; var AFehlergruppe: integer;
  var AFehlercode: integer): boolean;
{--------------------------------------------------------------------------------}
{ Modemantwort auf Verbindungsaufbau- bzw. Anrufannahme-Befehl prüfen;
  Übergabe: Modem-Antwort
  Rückgabe: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn CONNECT erfolgt ist }
var
  S: string;

begin
  { Vorbelegung Rückgabe: OK }
  AFehlergruppe:=0;
  AFehlercode:=0;

  { für Auswertung der Modemantwort: Leerzeichen raus und in Großbuchstaben wandeln }
  S:=AnsiUpperCase (StrFilter (ModemAnswer, ' '));

  { Verbindung steht }
  if Pos('CONNECT', S) <> 0 then begin
    Result:=true;
    exit;
  end;

  Result:=false;
  AFehlergruppe:=COM_MODEMERROR;

  { besetzt }
  if Pos('BUSY', S) <> 0 then begin
    AFehlercode:=CME_BUSY;
    exit;
  end;

  { kein Freizeichen }
  if Pos('NODIALTONE', S) <> 0 then begin
    AFehlercode:=CME_NODIALTONE;
    exit;
  end;

  { kein Antwortton }
  if Pos('NOANSWER', S) <> 0 then begin
    AFehlercode:=CME_NOANSWER;
    exit;
  end;

  { kein Träger vorhanden }
  if Pos('NOCARRIER', S) <> 0 then begin
    AFehlercode:=CME_NOCARRIER;
    exit;
  end;

  { Rufverzögerung }
  if Pos('DELAYED', S) <> 0 then begin
    AFehlercode:=CME_DELAYED;
    exit;
  end;

  { Wählfunktion gesperrt }
  if Pos('DIALLOCKED', S) <> 0 then begin
    AFehlercode:=CME_DIALLOCKED;
    exit;
  end;

  { "Schwarze Liste" }
  if Pos('BLACKLISTED', S) <> 0 then begin
    AFehlercode:=CME_BLACKLISTED;
    exit;
  end;

  { Abbruch }
  if Pos('ABORT', S) <> 0 then begin
    AFehlercode:=CME_ABORT;
    exit;
  end;

  { Error }
  if Pos('ERROR', S) <> 0 then begin
    AFehlercode:=CME_ERROR;
    exit;
  end;

  { Ring }
  if Pos('RING', S) <> 0 then begin
    AFehlercode:=CME_RING;
    exit;
  end;

  { OK (das ist an dieser Stelle wirklich ein Fehler !) }
  if Pos('OK', S) <> 0 then begin
    AFehlercode:=CME_OK;
    exit;
  end;

  { Sonstiges }
  AFehlercode:=CME_SONST;
end;

end.

