{**************************************************************************************}
{* Unit: DSfG-Kommunikation �ber serielle Schnittstelle                               *}
{* 22.03.2000 WW                                                                      *}
{* 06.03.2002 WW  mit SendModemCommand-Methode                                        *}
{* 03.02.2003 WW  Timeout- und Versuche-Variablen in Klasse integriert (bisher global)*}
{* 28.05.2013 WW  R�ckgabe Fehlercode 'GetLastError', wenn ClearCommError fehlschl�gt *}
{* 03.01.2022 WW  mit ComTracelog                                                     *}
{* 19.12.2022 WW  Timeout-�berwachung: Timer durch TickCount ersetzt                  *}
{**************************************************************************************}
unit SerDSfG;

interface

uses
  Windows, SysUtils, Forms, Classes, Serial, T_Zeit, ErrConst,
  LogCom, WChars, WStrUtils, WComm, AbrufTimeoutConst, O_Comm, CommUtil;

type
  { Basis-Objekt f�r DSfG-Kommunikation �ber die serielle Schnittstelle }

  TDSfGSerialCustomCommObj = class(TDSfGCommObj)
  private
    { Private-Deklarationen }
    FSerial: TSerial;
    Timeout_ModemAntwort: integer;  { Timeout beim Warten auf Modem-Antwort }
    procedure SetStandardTimeouts;
  protected
    function Comm_SendData (sData: string): boolean; override;
    function Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal): string; override;
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; override;
    procedure Comm_ClearRecBuf; override;
    procedure Comm_SetFehlerRueckgabe (NoCarrier: boolean; var Rueckgabe: TRueckgabe); override;
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; AWorkPath: TFileName; AComLogFile: TComLogFile);
    destructor Destroy; override;
    procedure SetTimeouts_Versuche (ATimeout_ModemAntwort: integer;
                                    AVersuche_BCC: integer);
    function Connect (AComPort: integer; ABaudrate: integer; ADatabits: TDataBits;
                      AParityBit: TParityBit; AStopBits: TStopBits): integer;
    function SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                          AEndezeichenAnzahl: integer;
                          ATimeout: integer; AAnswerDest: TAnswerDest;
                          var Rueckgabe: TRueckgabe; var NoCarrier: boolean;
                          AnzCharsToReceive: cardinal = 0): boolean; override;
    property Serial: TSerial read FSerial;
  end;


  { Objekt f�r DSfG-Kommunikation �ber die serielle Schnittstelle }

  TDSfGSerialCommObj = class(TDSfGSerialCustomCommObj);


  { Objekt f�r DSfG-Kommunikation mit Modem �ber die serielle Schnittstelle }

  TDSfGModemCommObj = class(TDSfGSerialCustomCommObj)
  private
    { Private-Deklarationen }
    FModemstatus: TCBStatusMsgProc;
    DCDCheck: boolean;                   { Flag, ob DCD-Signal �berwacht wird }
  protected
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; override;
    procedure Comm_SetFehlerRueckgabe (NoCarrier: boolean; var Rueckgabe: TRueckgabe); override;
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; AWorkPath: TFileName; AComLogFile: TComLogFile);
    procedure SetDCDCheck (OnOff: boolean);
    function SendModemCommand (ABefehl: string; ATimeout: integer;
                               var Rueckgabe: TRueckgabe;
                               WaitFor_OK_ERROR: boolean = false): boolean;
    function Rufabfrage (var Ruf_angekommen: boolean;
                         var Fehlergruppe: integer; var Fehlercode: integer): boolean;
    property CBModemstatus: TCBStatusMsgProc read FModemstatus write FModemstatus;
  end;


implementation

{ TDSfGSerialCustomCommObj }

{------------------------------------------------------------------------------------}
constructor TDSfGSerialCustomCommObj.Create (AOwner: TComponent; AWorkPath: TFileName;
                                             AComLogFile: TComLogFile);
{------------------------------------------------------------------------------------}
begin
  inherited Create (AWorkPath, AComLogFile);

  FSerial:=TSerial.Create (AOwner);
  with FSerial do begin
    { Puffergr��e f�r DSfG-Kommunikation: }
    BufSizeTrm:=8192;                             { Sendepuffergr��e }
    BufSizeRec:=8192;                             { Empfangspuffergr��e }
    { ohne RTS/CTS-Handshake: }
    HandshakeRTSCTS:=false;
    { DTR- und RTS-Leitung setzen: }
    DTRActive:=true;
    RTSActive:=true;
  end;

  SetStandardTimeouts;
end;

{------------------------------------------}
destructor TDSfGSerialCustomCommObj.Destroy;
{------------------------------------------}
begin
  CloseCOM (FSerial, FComLogFile);
  FSerial.Free;
  inherited Destroy;
end;

{-----------------------------------------------------}
procedure TDSfGSerialCustomCommObj.SetStandardTimeouts;
{-----------------------------------------------------}
{ Standard-Timeouts setzen }
begin
  Timeout_ModemAntwort:=CTimeout_ModemAntwort;
end;

{--------------------------------------------------------------------------------------}
procedure TDSfGSerialCustomCommObj.SetTimeouts_Versuche (ATimeout_ModemAntwort: integer;
                                                         AVersuche_BCC: integer);
{--------------------------------------------------------------------------------------}
{ Standard-Timeouts und -Versuche mit �bergebenen Werten belegen }
begin
  Timeout_ModemAntwort:=ATimeout_ModemAntwort;
  SetVersuche (AVersuche_BCC);
end;

{--------------------------------------------------------------------------------------}
function TDSfGSerialCustomCommObj.Connect (AComPort: integer; ABaudrate: integer;
                                           ADatabits: TDataBits; AParityBit: TParityBit;
                                           AStopBits: TStopBits): integer;
{--------------------------------------------------------------------------------------}
{ Verf�gbarkeit der seriellen Schnittstelle pr�fen, Schnittstellenparameter setzen und
  Schnittstelle �ffnen;
  �bergabe: AComPort
            ABaudrate
            ADataBits
            AParityBit
            AStopBit
  Ergebnis:  0 = Schnittstelle konnte korrekt ge�ffnet werden
            -1 = Schnittstelle ist nicht vorhanden
            -2 = Schnittstelle konnte nicht ge�ffnet werden }
begin
  Result:=ConnectCOM (FSerial, FComLogFile, AComPort, ABaudrate, ADatabits,
                      AParityBit, AStopBits);
end;

{--------------------------------------------------------------------------------------------}
function TDSfGSerialCustomCommObj.SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                                               AEndezeichenAnzahl: integer;
                                               ATimeout: integer;
                                               AAnswerDest: TAnswerDest;
                                               var Rueckgabe: TRueckgabe;
                                               var NoCarrier: boolean;
                                               AnzCharsToReceive: cardinal = 0): boolean;
{--------------------------------------------------------------------------------------------}
{ Befehl senden und Antwort empfangen;
  �bergabe: Befehl-String
            in der Antwort zu erwartende Endezeichen
            Anzahl, der in der Antwort zu erwartenden Endezeichen
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist
            AAnswerDest (DestString: Empfangsdaten -> Antwort-String
                         Destfile: Empfangsdaten -> Rohfile
            AnzCharsToReceive feste Anzahl an zu empfangenen Zeichen (wenn > 0 wird
                              nach den �bergebenen n Zeichen der Zeichenempfang mit
                              OK beendet, unabh�ngig vom �bergebenen Endezeichen)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AnswerDest = DestString: Empfangsdaten
                          AnswerDest = Destfile: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
begin
  { Empfangsbereitschaft des Modem pr�fen (eigentlich m��te man auch das DSR-Signal
    pr�fen, aber das wird standardm��ig nicht immer unterst�tzt, z.B. ke LOGEM9600): }
  if FSerial.HandshakeRTSCTS AND not FSerial.CheckCTSSignal then begin
    { CTS-Signal ist aus -> Modem nicht angeschlossen }
    with Rueckgabe do begin
      Fehlergruppe:=COM_MODEMERROR;
      Fehlercode:=CME_CTS;
      Antwort:='';
    end;
    NoCarrier:=true;            { erweitert 23.05.2003, WW }
    Result:=false;
    exit;
  end;

  Result:=inherited SendCommand (ABefehl, AEndezeichen, AEndezeichenAnzahl,
                                 ATimeout, AAnswerDest, Rueckgabe, NoCarrier,
                                 AnzCharsToReceive);
end;

{------------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
function TDSfGSerialCustomCommObj.Comm_SendData (sData: string): boolean;
{-----------------------------------------------------------------------}
begin
  Delay (30);       { sonst funktioniert das ke LOGEM LGM9600 nicht richtig ! }
  FSerial.TransmittText(sData);             { Befehl in Sendepuffer schreiben }
  Result:=true;
end;

{-----------------------------------------------------------------------------------------------------}
function TDSfGSerialCustomCommObj.Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal): string;
{-----------------------------------------------------------------------------------------------------}
var
  ComStat: TComStat;
  n: cardinal;
  S: string;

begin
  with FSerial do begin
    if ClearCommError(SerHandle,FCommError,@ComStat) then              { Schnittstellenstatus abfragen }
      n:=ComStat.cbInQue  // Anzahl der Zeichen im Empfangspuffer
    else begin
      n:=0;

      // Errorcode aus 'GetLastError': 28.05.2013, WW
      FCommError:=GetLastError;
      FCommError:=FCommError + ERR_OFFSET_GETLASTERROR;  // Kennzeichnung 'GetLastError'
    end;

    if AnzCharsToReceive > 0 then     { wenn feste Anzahl an zu empfangenden Zeichen �bergeben wurde }
      if n > ChToRec_Left then n:=ChToRec_Left;     { ...Anzahl begrenzen; 12.09.2008 WW }

    if n > 0 then begin
      SetLength(S, n);  // String-Puffer bereitstellen; 12.09.2008 WW
      SetLength(S, ReceiveData(Pointer(S)^, Length(S)));  // Zeichen aus Empfangspuffer lesen
    end else
      S:='';
  end;
  Result:=S;
end;

{-------------------------------------------------------------------------------------}
function TDSfGSerialCustomCommObj.Comm_Connection (var Rueckgabe: TRueckgabe): boolean;
{-------------------------------------------------------------------------------------}
begin
  Result:=true;  // Verbindung immer vorhanden
end;

{--------------------------------------------------}
procedure TDSfGSerialCustomCommObj.Comm_ClearRecBuf;
{--------------------------------------------------}
var
  ComStat: TComStat;
  CommError: cardinal;

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

{-------------------------------------------------------------------------------------}
procedure TDSfGSerialCustomCommObj.Comm_SetFehlerRueckgabe (NoCarrier: boolean;
                                                            var Rueckgabe: TRueckgabe);
{-------------------------------------------------------------------------------------}
begin
  { Rueckgabe belegen mit Schnittstellenfehler: }
  with Rueckgabe do begin
    Fehlergruppe:=COM_ERROR;
    Fehlercode:=FCommError;
    Antwort:='';
  end;
end;


{ TDSfGSerialCommObj }



{ TDSfGModemCommObj }

{-----------------------------------------------------------------------------}
constructor TDSfGModemCommObj.Create (AOwner: TComponent; AWorkPath: TFileName;
                                      AComLogFile: TComLogFile);
{-----------------------------------------------------------------------------}
begin
  inherited Create (AOwner, AWorkPath, AComLogFile);

  FSerial.HandshakeRTSCTS:=true;  { mit RTS/CTS-Handshake }
  SetDCDCheck (false);

  FPrefix_RMGDfueBefehl:='YW';  // Prefix f�r RMG-DSfG-DF�-Befehle bei DF�-Verbindung }
end;

{-------------------------------------------------------}
procedure TDSfGModemCommObj.SetDCDCheck (OnOff: boolean);
{-------------------------------------------------------}
{ DCD-�berwachung einschalten (OnOff = true) bzw. ausschalten (OnOff = false) }
begin
  DCDCheck:=OnOff;
end;

{---------------------------------------------------------------------------------------}
function TDSfGModemCommObj.SendModemCommand (ABefehl: string; ATimeout: integer;
                                             var Rueckgabe: TRueckgabe;
                                             WaitFor_OK_ERROR: boolean = false): boolean;
{---------------------------------------------------------------------------------------}
{ Modem-Befehl senden und Antwort empfangen;
  �bergabe: Befehl-String
            ATimeout (Zeit, die maximal gewartet wird bis das erste Zeichen empfangen wird
            Flag 'WaitFor_OK_ERROR' (wenn true, wird auf OK oder ERROR in der Antwort gewartet)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (Empfangsdaten)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  S: string;
  AntwortEmpfangen: boolean;
  First: boolean;
  TickCount: cardinal;

begin
  Timeout:=ATimeout;
  AnswerDest:=ad_String;

  { Empfangsbereitschaft des Modem pr�fen (eigentlich m��te man auch das DSR-Signal
    pr�fen, aber das wird standardm��ig nicht immer unterst�tzt, z.B. ke LOGEM9600): }
  if FSerial.HandshakeRTSCTS AND not FSerial.CheckCTSSignal then begin
    { CTS-Signal ist aus -> Modem nicht angeschlossen }
    with Rueckgabe do begin
      Fehlergruppe:=COM_MODEMERROR;
      Fehlercode:=CME_CTS;
      Antwort:='';
    end;
    Result:=false;
    exit;
  end;

  Befehl:=ABefehl;
  BefehlSenden;                                           { Befehl abschicken }

  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:=''
  end;

  { Antwort auslesen: }
  AntwortEmpfangen:=false;
  TickCount:=0;
  FCommError:=0;
  First:=true;
  while not AntwortEmpfangen AND (FCommError = 0) do begin
    Sleep (1);
    Application.ProcessMessages;
    Timeout_Elapsed;  // Verstrichene Zeit messen f�r Timeout-�berwachung; 19.12.2022, WW

    s:=Comm_ReceiveData (0, 0);  // 28.05.2013, WW
    if length (s) > 0 then begin
      TickCount:=GetTickCount;
      TimeoutCount:=0;
      { Timeout-Anzeige: }
      if Assigned (CBTimeout) then
        CBTimeout ('');

      if First then begin                   { die ersten ausgelesenen Zeichen }
        First:=false;
        if FComLogFile <> nil then
          FComLogFile.Write ('E', s);    { Logfileeintrag mit Kopf "Empfangsdaten" }
        if FComTracelog <> nil then
          FComTracelog.Write ('E', s);  { Tracelogeintrag mit Kopf "Empfangsdaten"; 03.01.2022, WW }
      end  { if First }
      else begin
        if FComLogFile <> nil then
          FComLogFile.Write ('D', s);    { Logfileeintrag nur Daten }
        if FComTraceLog <> nil then
          FComTraceLog.Write ('D', s);    { Tracelogeintrag nur Daten; 03.01.2022, WW }
      end;
      Rueckgabe.Antwort:=Rueckgabe.Antwort + s;

      { RxD-Anzeige: }
      if Assigned (CBRxD) then
        CBRxD (SonderzeichenString (Rueckgabe.Antwort),
               IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
    end   { length (s) > 0 }
    else begin
      { die Modem-Antwort kann als vollst�ndig empfangen gelten, wenn
        innerhalb einer kurzen Zeitspanne keine neuen Zeichen mehr kommen: }
      if TickCount > 0 then begin
        if WaitFor_OK_ERROR then   // wenn auf OK oder ERROR in der Antwort gewartet werden soll
          if (Pos ('OK', Rueckgabe.Antwort) = 0) AND
             (Pos ('ERROR', Rueckgabe.Antwort) = 0) then
            TickCount:=GetTickCount;   // TickCount updaten, damit nicht vorzeitig abgebrochen wird

        AntwortEmpfangen:=(cardinal (GetTickCount) - TickCount) > cardinal (Timeout_ModemAntwort);
      end;

      if TimeoutCount >= Timeout then begin       { Timeout beim Datenempfang }
        with Rueckgabe do begin
          Fehlergruppe:=COM_KOMMERROR;
          Fehlercode:=KOMMERR_TIMEOUT;
          { bei Timeout Antwort nicht l�schen }
        end;
        Break;
      end;
    end;
  end; { while }

  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  { RxD-Anzeige: }
  if Assigned (CBRxD) then
    CBRxD (SonderzeichenString (Rueckgabe.Antwort), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
  Application.ProcessMessages;

  if FCommError <> 0 then begin         { Schnittstellenfehler ist aufgetreten }
    with Rueckgabe do begin    { Rueckgabe belegen mit Schnittstellenfehler }
      Fehlergruppe:=COM_ERROR;
      Fehlercode:=FCommError;
      Antwort:='';
    end;

    { Empfangspuffer leeren: }
    Comm_ClearRecBuf;
  end;
  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{-----------------------------------------------------------------------}
function TDSfGModemCommObj.Rufabfrage (var Ruf_angekommen: boolean;
                                       var Fehlergruppe: integer;
                                       var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------}
{ Schnittstelle auf ankommenden Ruf pr�fen;
  Anmerkung: Ein ankommender Ruf k�nnte auch �ber das RI-Signal erkannt werden.
             Es kann dann jedoch nicht die Anzahl der Rings gez�hlt werden, da
             manche Modems bei ankommendem Ruf das RI-Signal st�ndig anstehen lassen.
  R�ckgabe: Ruf_angekommen (true, wenn Ruf anliegt)
            Fehlergruppe/-code
  Ergebnis: true, wenn Rufabfrage ok }
Const
  CTimeout_ModemRing = 1;   { auf einen schon anstehenden RING mu� nicht lang gewartet werden }
var
  R: TRueckgabe;
  dummy: boolean;
  ModemAntwort: string;
begin
  Result:=false;
  Ruf_angekommen:=false;
  Fehlergruppe:=0;
  Fehlercode:=0;

  if SendCommand ('', [LF], 2, CTimeout_ModemRing, ad_String, R, dummy) then begin
    { Antworten: RING = Ruf steht an
                 alle anderen sind unerwartete Antworten }
    ModemAntwort:=ExtractString (R.Antwort, LF, CR, 0);
    if ModemAntwort = 'RING' then begin
      Ruf_angekommen:=true;
      Result:=true;
    end;
  end
  else if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) then
    Result:=true      { keine Antwort -> es steht kein Ruf an; Fehlergruppe/-code: OK }
  else begin    { Fehler ist aufgetreten }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
  end;
end;


{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
function TDSfGModemCommObj.Comm_Connection (var Rueckgabe: TRueckgabe): boolean;
{------------------------------------------------------------------------------}
begin
  { DCD-�berwachung (RLSD-Signal): }
  if DCDCheck then begin
    Result:=FSerial.CheckRLSDSignal;  { false: DCD-Signal ist aus -> Verbindung unterbrochen }
    if not Result then begin
      with Rueckgabe do begin
        Fehlergruppe:=COM_MODEMERROR;
        Fehlercode:=CME_DCD;
        Antwort:='';
      end;
      { Modemstatus-Anzeige: }
      if Assigned (CBModemstatus) then
        CBModemstatus ('');
    end;
  end else
    Result:=true;
end;

{------------------------------------------------------------------------------}
procedure TDSfGModemCommObj.Comm_SetFehlerRueckgabe (NoCarrier: boolean;
                                                     var Rueckgabe: TRueckgabe);
{------------------------------------------------------------------------------}
begin
  { Rueckgabe belegen mit Schnittstellenfehler:
    -> R�ckgabe "Verbindung unterbrochen" nicht �berschreiben, wenn mit DCDCheck }
  if not (DCDCheck AND NoCarrier) then
    inherited Comm_SetFehlerRueckgabe (NoCarrier, Rueckgabe);
end;

end.

