{******************************************************************************}
{* Unit: Basisklasse für Modbus-Kommunikation ASCII/RTU/TCP-IP                *}
{* 23.06.2009  WW                                                             *}
{* 03.08.2010  GD  Erweitert um TCP/IP                                        *}
{******************************************************************************}
unit O_ModbusComm;

interface

uses
  Windows, SysUtils, Forms, Classes, ExtCtrls, LogCom, WComm, WChars, WStrUtils,
  AbrufTimeoutConst, ErrConst, T_BinMask, ModbusUtil, Serial;

type
  { Callback-Prozedurtypen zur Anzeige in Fremdmodulen }

  TCBDataProc = procedure (Data: string; Bytes: string) of object;
  TCBStatusMsgProc = procedure (Msg: string) of object;

  { Basis-Objekt für Modbus-Kommunikation }

  TModbusCustomCommObj = class(TObject)
  private
    { Private-Deklarationen }
    FTxD: TCBDataProc;
    FRxD: TCBDataProc;
    FTimeout: TCBStatusMsgProc;
    Versuche_LRC_CRC: integer;   { max. Anzahl an Versuchen bei falschem LRC (ASCII) bzw. CRC (RTU) }
    Versuche: byte;
    procedure SetStandardVersuche;
    function EndOfLRC_CRCTransmission (var Rueckgabe: TRueckgabe): boolean;
  protected
    TimerTimeout: TTimer;
    Timeout: integer;
    TimeoutCount: integer;
    FComLogFile: TComLogFile;
    FModus: TModbusModus;
    Query: string;
    CommError: cardinal;
    procedure TimerTimeoutTimer(Sender: TObject); virtual;
    function Comm_SendData (sData: string): boolean; virtual; abstract;
    function Comm_ReceiveData: string; virtual; abstract;
    function Comm_Connection (
      var Rueckgabe: TRueckgabe): boolean; virtual; abstract;
    procedure Comm_ClearRecBuf; virtual; abstract;
    procedure Comm_SetFehlerRueckgabe (
      var Rueckgabe: TRueckgabe); virtual; abstract;
    procedure QuerySenden;
  public
    { Public-Deklarationen }
    constructor Create; virtual;
    destructor Destroy; override;
    function Connect: integer; virtual; abstract;
    procedure SetVersuche (AVersuche_LRC_CRC: integer);
    function SendQuery (AQuery: string; ATimeoutAntwort, ATimeoutZeichen: integer;
                        var Rueckgabe: TRueckgabe;
                        var NoCarrier: boolean): boolean; virtual;
    property ComLogfile: TComLogFile read FComLogFile write FComLogFile;
    property Modus: TModbusModus read FModus write FModus;
    property CBTxD: TCBDataProc read FTxD write FTxD;
    property CBRxD: TCBDataProc read FRxD write FRxD;
    property CBTimeout: TCBStatusMsgProc read FTimeout write FTimeout;
  end;

implementation

{ TModbusCustomCommObj }

{--------------------------------------}
constructor TModbusCustomCommObj.Create;
{--------------------------------------}
begin
  inherited Create;

  FComLogfile:=nil;
  FModus:=modbus_RTU;
  Query:='';
  Timeout:=0;
  TimeoutCount:=0;

  SetStandardVersuche;                             { Standard-Versuche setzen }

  Versuche:=0;
  CommError:=0;

  { Timer für Timeout-Überwachung beim Empfangen der Daten: }
  TimerTimeout:=TTimer.Create (nil);
  TimerTimeout.Enabled:=false;
  TimerTimeout.Interval:=500;     { 0,5 s }
  TimerTimeout.OnTimer:=TimerTimeoutTimer;
end;

{--------------------------------------}
destructor TModbusCustomCommObj.Destroy;
{--------------------------------------}
begin
  TimerTimeout.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------}
procedure TModbusCustomCommObj.TimerTimeoutTimer (Sender: TObject);
{-----------------------------------------------------------------}
begin
  TimeoutCount:=TimeoutCount + integer(TimerTimeout.Interval);
  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    if (TimeoutCount MOD 1000) = 0 then
      CBTimeout (IntToStr((Timeout-TimeoutCount) DIV 1000) + ' s');
end;


{-------------------------------------------------}
procedure TModbusCustomCommObj.SetStandardVersuche;
{-------------------------------------------------}
{ Standard-Versuche setzen }
begin
  Versuche_LRC_CRC:=CModbus_LRC_CRCVersuche;
end;

{----------------------------------------------------------------------}
procedure TModbusCustomCommObj.SetVersuche (AVersuche_LRC_CRC: integer);
{----------------------------------------------------------------------}
{ Standard-Versuche mit übergebenen Werten belegen }
begin
  Versuche_LRC_CRC:=AVersuche_LRC_CRC;
end;

{-----------------------------------------}
procedure TModbusCustomCommObj.QuerySenden;
{-----------------------------------------}
begin
  TimeoutCount:=0;

  { TxD/RxD-Anzeige: }
  if Assigned (CBTxD) then
    CBTxD (SonderzeichenString (Query), IntToStr (length (Query)) + ' Byte');
  if Assigned (CBRxD) then
    CBRxD ('', '');
  Application.ProcessMessages;

  if Comm_SendData (Query) then  // Befehl senden
    if (length (Query) > 0) AND (FComLogFile <> nil) then
      FComLogFile.Write ('S', Query, -1, true);  { Logfileeintrag Hex mit Kopf "Sendedaten" }

  TimerTimeout.Enabled:=true;                   { Timeout-Überwachung starten }

  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');
end;

{---------------------------------------------------------------------}
function TModbusCustomCommObj.SendQuery (AQuery: string;
  ATimeoutAntwort, ATimeoutZeichen: integer; var Rueckgabe: TRueckgabe;
  var NoCarrier: boolean): boolean;
{---------------------------------------------------------------------}
{ Query senden und Response empfangen;
  Übergabe: Query-String
            Antwort-Timeout (ASCII: max. Wartezeit in ms auf jedes empfangene Zeichen
                             RTU: max. Wartezeit in ms auf erstes empfangenes Zeichen)
            AZeichenzeitTimeout (max. Wartezeit in ms zwischen 2 empfangenen
              Zeichen; nur für RTU)
  Rückgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (Response)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Response korrekt gelesen werden konnte }
var
  S: string;
  ResponseEmpfangen: boolean;
  First: boolean;
  TickCount: cardinal;
  i: integer;

begin
  Timeout:=ATimeoutAntwort;
  Versuche:=1;

  Query:=AQuery;
  QuerySenden;                                           { Query abschicken }

  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:=''
  end;

  { Antwort auslesen: }
  ResponseEmpfangen:=false;
  TickCount:=0;
  CommError:=0;
  First:=true;
  while not ResponseEmpfangen AND (CommError = 0) do begin
    Sleep (1);
    Application.ProcessMessages;

    { Verbindung prüfen: }
    if not Comm_Connection (Rueckgabe) then begin
      NoCarrier:=true;
      with Rueckgabe do begin                           { Rueckgabe initalisieren }
        Fehlergruppe := COM_KOMMERROR;
        Fehlercode := KOMMERR_VERB_UNTERBROCHEN;
        Antwort := '';
      end;
      Break;
    end;

    { Antwort lesen: }
    s:=Comm_ReceiveData;
    if length (s) > 0 then begin
      if (FModus in [modbus_RTU, modbus_TCPIP]) then  // nur RTU
        TickCount:=GetTickCount;  { zum Ermitteln der Zeichenzeit }
      TimeoutCount:=0;
      { Timeout-Anzeige: }
      if Assigned (CBTimeout) then
        CBTimeout ('');

      if First then begin                   { die ersten ausgelesenen Zeichen }
        First:=false;
        if FComLogFile <> nil then
          FComLogFile.Write ('E', s, -1, true);    { Logfileeintrag Hex mit Kopf "Empfangsdaten" }
      end  { if First }
      else begin
        if FComLogFile <> nil then
          FComLogFile.Write ('D', s, -1, true);    { Logfileeintrag Hex nur Daten }
      end;
      Rueckgabe.Antwort:=Rueckgabe.Antwort + s;

      { RxD-Anzeige: }
      if Assigned (CBRxD) then
        CBRxD (SonderzeichenString (Rueckgabe.Antwort),
               IntToStr (length (Rueckgabe.Antwort)) + ' Byte');

      if FModus = modbus_ASCII then begin
        for i:=1 to length (s) do begin
          if s[i] = LF then begin      { Endezeichen LF in Puffer enthalten }
            ResponseEmpfangen:=EndOfLRC_CRCTransmission (Rueckgabe);
            First:=true;
          end;
        end;
      end;
    end   { length (s) > 0 }
    else if (FModus in [modbus_RTU, modbus_TCPIP]) then begin
      if TickCount > 0 then begin
        { Die Response kann als vollständig empfangen gelten, wenn
          nach Ablauf der Zeichenzeit keine neuen Zeichen mehr kommen (nur RTU): }
        if (cardinal (GetTickCount) - TickCount) > cardinal (ATimeoutZeichen) then begin
          ResponseEmpfangen:=EndOfLRC_CRCTransmission (Rueckgabe);
          TickCount:=0;
          First:=true;
        end;
      end;
    end;

    if TimeoutCount >= Timeout then begin       { Timeout beim Datenempfang }
      with Rueckgabe do begin
        Fehlergruppe:=COM_KOMMERROR;
        Fehlercode:=KOMMERR_TIMEOUT;
        { bei Timeout Antwort nicht löschen }
      end;
      Break;
    end;
  end; { while }

  TimerTimeout.Enabled:=false;                  { Timeout-Überwachung beenden }
  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  { TxD/RxD-Anzeige: }
  if Assigned (CBRxD) then
    CBRxD (SonderzeichenString (Rueckgabe.Antwort), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
  Application.ProcessMessages;

  if CommError <> 0 then begin              { Kommunikationsfehler ist aufgetreten }
    { Rückgabe-Record mit Kommunikationsfehler belegen: }
    Comm_SetFehlerRueckgabe (Rueckgabe);
    { Empfangspuffer leeren: }
    Comm_ClearRecBuf;
  end;

  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{------------------------------------------------------------------------------------------}
function TModbusCustomCommObj.EndOfLRC_CRCTransmission (var Rueckgabe: TRueckgabe): boolean;
{------------------------------------------------------------------------------------------}
{ LRC (ASCII) bzw. CRC (RTU) in empfangenen Daten prüfen;
  Rückgabe: Rueckgabe-Record
  Ergebnis: true, wenn Datenübertragung abgeschlossen (keine Wiederholung) }
var
  S: string;
  sBin: string;
  CRC_LRC: word;
  CRC_LRC_Calc: word;

begin
  if FModus = modbus_ASCII then begin
    S:=ExtractString (Rueckgabe.Antwort, ':', CR, 0);  // Datenteil zwischen Start und Ende (ASCII hex)
    sBin:=Hex2Bin (S);  // Hex-String in Binär-String wandeln

    S:=Copy (sBin, length (sBin), 1);  // Binär-LRC: letztes Zeichen
    CRC_LRC:=Bin2Byte (S);
    S:=Copy (sBin, 1, length (sBin) - 1);  // Binär-Datenteil, über den LRC gebildet wird
    CRC_LRC_Calc:=Get_Modbus_LRC (S);
  end
  else if (FModus in [modbus_RTU]) then begin  // RTU
    S:=Copy (Rueckgabe.Antwort, length (Rueckgabe.Antwort) - 1, 2);  // CRC: die letzten 2 Zeichen
    CRC_LRC:=Bin2Word (S, bo_BigEndian);  // CRC immer Big-Endian
    S:=Copy (Rueckgabe.Antwort, 1, length (Rueckgabe.Antwort) - 2);  // Datenteil, über den CRC gebildet wird
    CRC_LRC_Calc:=Get_Modbus_CRC (S);
  end
  else if (FModus in [modbus_TCPIP]) then begin // TC/IP
    Result := True;
    Exit;
  end
  else begin
    Result := False; 
    Exit;
  end;

  if CRC_LRC <> CRC_LRC_Calc then begin         { LRC/CRC-Fehler in der Response }
    if FComLogFile <> nil then begin
      if FModus = modbus_ASCII then
        S:='ERROR: Wrong LRC !'
      else  // RTU
        S:='ERROR: Wrong CRC !';
      FComLogFile.WriteMsg (S);  { Logfileeintrag }
    end;

    Rueckgabe.Antwort:='';
    if length (Query) > 0 then begin    { bei leerem Query macht eine Wiederholung keinen Sinn }
      inc (Versuche);
      if Versuche <= Versuche_LRC_CRC then begin
        QuerySenden;                        { nochmal gleiche Query senden }
        Result:=false;
        exit;
      end;
    end;

    { erst nach dem letzten Versuch Rueckgabe mit BCC-Fehler belegen: }
    with Rueckgabe do begin
      Fehlergruppe:=COM_KOMMERROR;
      if FModus = modbus_ASCII then
        Fehlercode:=KOMMERR_LRC
      else  // RTU
        Fehlercode:=KOMMERR_CRC;
    end;
  end;
  Result:=true;
end;

end.

