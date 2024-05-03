{******************************************************************************}
{* Unit: MRG-Kommunikation mit FUP über serielle Schnittstelle                *}
{* 05.12.2000 WW                                                              *}
{* 11.03.2003 WW Timeout-Variablen in Klasse integriert (bisher global)       *}
{* 04.10.2006 WW FUP-Handshake ein-/ausschaltbar                              *}
{* 24.01.2008 WW Empfangsdaten blockweise in Rohfile/Logfile schreiben        *}
{* 28.05.2013 WW Rückgabe Fehlercode 'GetLastError', wenn ClearCommError      *}
{*               fehlschlägt                                                  *}
{* 19.12.2022 WW Timeout-Überwachung: Timer durch TickCount ersetzt           *}
{******************************************************************************}
unit SerMrgFup;

interface

uses
  Windows, Classes, SysUtils, Forms, Serial, O_Comm, ErrPrc32, ErrConst, T_Zeit,
  T_Tools, LogCom, WChars, WStrUtils, WComm, AbrufTimeoutConst, CommUtil;

type

  { Objekt für MRG-Kommunikation mit FUP über die serielle Schnittstelle }

  TMRGFupCommObj = class(TMRGCommObj)
  private
    { Private-Deklarationen }
    FSerial: TSerial;
    Timeout_FupAntwort: integer;
    WithFupHandshake: boolean;    { Flag, ob FUP-Kommunikation mit RTS-Handshake erfolgt }
    TimeoutSleep: boolean;
    procedure SetStandardTimeout_FupAntwort;
    procedure HandShake;
  protected
    procedure Comm_Reset; override;
    function Comm_SendData (sData: string): boolean; override;
    function Comm_SendChar (c: char): boolean; override;
  public
    { Public-Deklarationen }
    isFup1200: boolean;             { true, wenn ein alter FUP 1200 dranhängt }
    constructor Create (AOwner: TComponent; AWorkPath: TFileName;
                        AComLogFile: TComLogFile);
    destructor Destroy; override;
    procedure SetTimeout_FupAntwort (ATimeout_FupAntwort: integer);
    procedure SetWithFupHandshake (OnOff: boolean);
    function Connect (AComPort: integer; ABaudrate: integer; ADatabits: TDataBits;
                      AParityBit: TParityBit; AStopBits: TStopBits): integer;
    function Reconnect: boolean;
    function SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet; AEndezeichenAnzahl: integer;
                          ATimeout: integer; AAnswerDest: TAnswerDest;
                          var Rueckgabe: TRueckgabe;
                          var NoCarrier: boolean;
                          AnzCharsToReceive: cardinal = 0): boolean; override;
    function Rufabfrage (var Ruf_angekommen: boolean;
                         var Fehlergruppe: integer; var Fehlercode: integer): boolean; override;
    property Serial: TSerial read FSerial;
  end;

implementation

{ TMRGFupCommObj }

{--------------------------------------------------------------------------}
constructor TMRGFupCommObj.Create (AOwner: TComponent; AWorkPath: TFileName;
                                   AComLogFile: TComLogFile);
{--------------------------------------------------------------------------}
begin
  FSerial:=TSerial.Create (AOwner);
  with FSerial do begin
    { Puffergrößen für MRG-Kommunikation: }
    BufSizeTrm:=2048;                                        { Sendepuffergröße }
    BufSizeRec:=8192;                                     { Empfangspuffergröße }
    { kein RTS/CTS-Handshake: }
    HandshakeRTSCTS:=false;
    { DTR- und RTS-Leitung setzen: }
    DTRActive:=true;
    RTSActive:=true;
  end;

  inherited Create(AWorkPath, AComLogFile);

  SetStandardTimeout_FupAntwort;
  SetWithFupHandshake (true);  // Standard: mit FUP-Handshake
  TimeoutSleep:=true;
  isFup1200:=false;
end;

{--------------------------------}
destructor TMRGFupCommObj.Destroy;
{--------------------------------}
begin
  CloseCOM (FSerial, ComLogFile);
  FSerial.Free;
  inherited Destroy;
end;

{-----------------------------------------------------}
procedure TMRGFupCommObj.SetStandardTimeout_FupAntwort;
{-----------------------------------------------------}
{ Standard-Timeouts setzen }
begin
  Timeout_FupAntwort:=CMRG_Timeout_FupAntwort;
end;

{----------------------------------------------------------------------------}
procedure TMRGFupCommObj.SetTimeout_FupAntwort (ATimeout_FupAntwort: integer);
{----------------------------------------------------------------------------}
{ Timeouts mit übergebenen Werten belegen }
begin
  Timeout_FupAntwort:=ATimeout_FupAntwort;
end;

{------------------------------------------------------------}
procedure TMRGFupCommObj.SetWithFupHandshake (OnOff: boolean);
{------------------------------------------------------------}
{ FUP-Handshake einschalten (OnOff = true) bzw. ausschalten (OnOff = false) }
begin
  WithFupHandshake:=OnOff;
end;

{-------------------------------------------------------------------------------------------}
function TMRGFupCommObj.Connect (AComPort: integer; ABaudrate: integer; ADatabits: TDataBits;
                                 AParityBit: TParityBit; AStopBits: TStopBits): integer;
{-------------------------------------------------------------------------------------------}
{ Verfügbarkeit der seriellen Schnittstelle prüfen, Schnittstellenparameter setzen und
  Schnittstelle öffnen;
  Übergabe: AComPort
            ABaudrate
            ADataBits
            AParityBit
            AStopBit
  Ergebnis:  0 = Schnittstelle konnte korrekt geöffnet werden
            -1 = Schnittstelle ist nicht vorhanden
            -2 = Schnittstelle konnte nicht geöffnet werden }
begin
  Result:=ConnectCOM (FSerial, ComLogFile, AComPort, ABaudrate, ADatabits,
                      AParityBit, AStopBits);
end;

{-----------------------------------------}
function TMRGFupCommObj.Reconnect: boolean;
{-----------------------------------------}
{ Schnittstelle schließen und wieder öffnen;
  Ergebnis: true = Schnittstelle konnte korrekt wiedergeöffnet werden }
begin
  CloseCOM (FSerial, ComLogFile);
  Delay (1000);
  Result:=OpenCOM (FSerial, ComLogFile);
end;

{---------------------------------}
procedure TMRGFupCommObj.HandShake;
{---------------------------------}
{ Handshake: Sequenz für RTS-Handshake }
Begin
  EscapeCommFunction (Serial.SerHandle, CLRRTS);
  EscapeCommFunction (Serial.SerHandle, SETRTS);
End;

{-------------------------------------------------------------------------------------}
function TMRGFupCommObj.SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                                     AEndezeichenAnzahl: integer;
                                     ATimeout: integer; AAnswerDest: TAnswerDest;
                                     var Rueckgabe: TRueckgabe; var NoCarrier: boolean;
                                     AnzCharsToReceive: cardinal = 0): boolean;
{-------------------------------------------------------------------------------------}
{ Befehl an FUP senden und Antwort empfangen;
  Übergabe: Befehl-String
            in der Antwort zu erwartende Endezeichen
            AEndezeichenAnzahl (Pseudoparameter für formal gleiche Übergabe wie
                                in TMRGModemCommObj.SendCommand, wird nicht verwendet)
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist;
                      Timeout = 0: es werden keine Empfangsdaten gelesen)
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
            AnzCharsToReceive (Dummy für Kompatibilität zu Basisfunktion in TCommObj)
  Rückgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde;
                       Dies kann bei FUP-Kommunikation nur behelfsmäßig erkannt werden !
                       NoCarrier wird auf true gesetzt, wenn auf einen MRG-Befehl STX..ETX
                       eine FUP-Antwort ESC..CR statt einer MRG-Antwort zurückkommt.)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
const
  C_FileDataBlockLen = 1024;  // 1K-Blöcke in Datei schreiben

var
  s: string;
  AntwortEmpfangen: boolean;
  CommError: cardinal;
  n, m: cardinal;
  Buf: pchar;
  First: boolean;
  ComStat: TComStat;
  i: cardinal;
  EndezeichenSet: TEndezeichenSet;
  CR_Detected: boolean;
  FUPAntwort: string;
  sFileDataBlock: string;

begin
  Timeout:=ATimeout;
  AnswerDest:=AAnswerDest;
  Befehl:=ABefehl;

  { Endezeichen der möglichen Antworten auf Befehl definieren: }
  EndezeichenSet:=AEndezeichen;      { grundsätzlich in der Antwort erwartete Endezeichen }
  if not (CR in AEndezeichen) then
    Include (EndezeichenSet, CR);    { FUP-Antwort kann aber immer kommen ! }

  { TxD/RxD-Anzeige: }
  if Assigned (CBTxD) then
    CBTxD (SonderzeichenString (Befehl), IntToStr (length (Befehl)) + ' Byte');
  if Assigned (CBRxD) then
    CBRxD ('', '');
  Application.ProcessMessages;

  BefehlSenden;                                           { Befehl abschicken }

  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;

  if Timeout > 0 then begin               { bei Timeout > 0: Antwort auslesen }
    sFileDataBlock:='';
    CR_Detected:=false;
    AntwortEmpfangen:=false;
    CommError:=0;
    First:=true;
    GetMem (Buf, Serial.BufSizeRec+1);
    try
      while not AntwortEmpfangen AND (CommError = 0) do begin
        Application.ProcessMessages;
        Timeout_Elapsed;  // Verstrichene Zeit messen für Timeout-Überwachung; 19.12.2022, WW

        if ClearCommError(Serial.SerHandle,CommError,@ComStat) then { Schnittstellenstatus abfragen }
          n:=ComStat.cbInQue
        else begin
          n:=0;

          // Errorcode aus 'GetLastError': 28.05.2013, WW
          CommError:=GetLastError;
          CommError:=CommError + ERR_OFFSET_GETLASTERROR;  // Kennzeichnung 'GetLastError'
        end;

        if n > 0 then begin
          if n > Serial.BufSizeRec then n:=Serial.BufSizeRec;
          Buf^:=#0;
          m:=Serial.ReceiveData (Buf^, n);            { Daten aus Empfangspuffer lesen }
          if WithFupHandshake then
            HandShake;
         { Buf zeichenweise nach s kopieren (kann am Ende ein NUL-Zeichen als BCC enthalten,
           deshalb StrLCopy nicht verwenden !): }
          s:='';
          if m > 0 then
            for i:=0 to m-1 do
              s:=s + Buf[i];
        end else
          s:='';

        if length (s) > 0 then begin
          TimeoutCount:=0;
          { Timeout-Anzeige: }
          if Assigned (CBTimeout) then
            CBTimeout ('');

          Rueckgabe.Antwort:=Rueckgabe.Antwort + s;

          { RxD-Anzeige: }
          if Assigned (CBRxD) then
            CBRxD (SonderzeichenString (s), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');

          if First then begin  { die ersten ausgelesenen Zeichen }
            First:=false;
            if ComLogFile <> nil then
              ComLogFile.Write ('E', s);       { Logfileeintrag mit Kopf "Empfangsdaten" }
          end;

          { Empfangsdaten blockweise in Rohfile/Logfile schreiben, um Anzahl der
            Schreibzugriffe zu reduzieren (FUP-Puffer Überlauf, wenn Schreiben in
            Datei zu lange dauert !); 24.01.2008 WW: }
          sFileDataBlock:=sFileDataBlock + s;
          if length (sFileDataBlock) >= C_FileDataBlockLen then begin
            if not First AND (ComLogFile <> nil) then
              ComLogFile.Write ('D', sFileDataBlock);  { Logfileeintrag nur Daten }

            if AnswerDest = ad_File then begin  { Rohfile schreiben }
              if not WriteRohfile (RohfileName, sFileDataBlock) then begin
                Rueckgabe.Fehlergruppe:=ST_FILEERROR;
                Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
                sFileDataBlock:='';
                Break;
              end;
            end;
            sFileDataBlock:='';
          end;

          for i:=1 to length (s) do
            if s[i] in EndezeichenSet then begin  { Endezeichen in Puffer enthalten }
              AntwortEmpfangen:=true;
              if s[i] = CR then
                CR_Detected:=true;
            end;

          { nicht bei jedem Schleifendurchlauf 'Sleep' aufrufen, sonst wird bei
            2400 Baud wegen des zeichenweisen Handshakes zu langsam von der
            Schnittstelle gelesen und der Puffer im FUP läuft über !  WW  17.08.2001 }
          if (length (Rueckgabe.Antwort) MOD 5) = 0 then
            Sleep (1);
          TimeoutSleep:=false;
        end   { length (s) > 0 }
        else begin
          if TimeoutCount > 250 then  // Halbe Intervallzeit in ms des bisher verwendeten Timers; 19.12.2022, WW
            TimeoutSleep:=true;

          if TimeoutSleep then
            Sleep (1);

          if TimeoutCount >= Timeout then begin       { Timeout beim Datenempfang }
            with Rueckgabe do begin
              Fehlergruppe:=COM_KOMMERROR;
              Fehlercode:=KOMMERR_TIMEOUT;
              Antwort:='';
            end;
            Break;
          end;
        end;
      end; { while }
    finally
      FreeMem (Buf, Serial.BufSizeRec+1);
    end;

    { Rest der Empfangsdaten in Rohfile/Logfile schreiben; 24.01.2008 WW }
    if length (sFileDataBlock) > 0 then begin
      if not First AND (ComLogFile <> nil) then
        ComLogFile.Write ('D', sFileDataBlock);  { Logfileeintrag nur Daten }

      if AnswerDest = ad_File then begin  { Rohfile schreiben }
        if not WriteRohfile (RohfileName, sFileDataBlock) then begin
          Rueckgabe.Fehlergruppe:=ST_FILEERROR;
          Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
        end;
      end;
      sFileDataBlock:='';
    end;

    { Timeout-Anzeige: }
    if Assigned (CBTimeout) then
      CBTimeout ('');

    { TxD/RxD-Anzeige: }
    if Assigned (CBRxD) then
      CBRxD (SonderzeichenString (Rueckgabe.Antwort), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
    Application.ProcessMessages;

    if CommError <> 0 then begin         { Schnittstellenfehler ist aufgetreten }
      with Rueckgabe do begin    { Rueckgabe belegen mit Schnittstellenfehler }
        Fehlergruppe:=COM_ERROR;
        Fehlercode:=CommError;
        Antwort:='';
      end;

      { Empfangspuffer leeren, solange noch Zeichen einlaufen: }
      while ClearCommError(Serial.SerHandle,CommError,@ComStat) do begin
        Delay (50);
        if ComStat.cbInQue > 0 then
          PurgeComm (Serial.SerHandle, PURGE_RXCLEAR)          { Empfangspuffer leeren }
        else
          Break;
      end;
    end;

    { kein FUP-Befehl, sondern z.B. MRG-Befehl STX..ETX gesendet, es kommt aber
      FUP-Antwort ESC..CR -> Fehler }
    if not (CR in AEndezeichen) AND CR_Detected then begin
      FupAntwort:=ExtractString (Rueckgabe.Antwort, ESC, CR, 0);
      with Rueckgabe do begin         { Rueckgabe belegen mit FUP-Fehlermeldung }
        Fehlergruppe:=COM_FUPERROR;
        Fehlercode:=GetFupErrorcode (FupAntwort);
        Antwort:='';
      end;
      NoCarrier:=true;
    end;

    if AnswerDest = ad_File then begin
      if Rueckgabe.Fehlergruppe <> 0 then
        DeleteFile (RohFileName)                 { Rohfile löschen bei Fehler }
      else
        Rueckgabe.Antwort:=RohFileName;   { Rückgabe: Antwort = Rohfilename }
    end;
  end;  { if Timeout > 0 }
  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{-----------------------------------------------------------------------------------------------}
function TMRGFupCommObj.Rufabfrage (var Ruf_angekommen: boolean;
                                    var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------------------------}
{ Schnittstelle auf ankommenden Ruf prüfen;
  Rückgabe: Ruf_angekommen (true, wenn Ruf anliegt)
            Fehlergruppe/-code
  Ergebnis: true, wenn Rufabfrage ok }
var
  R: TRueckgabe;
  dummy: boolean;
  FupAntwort: string;
begin
  Result:=false;
  Ruf_angekommen:=false;
  Fehlergruppe:=0;
  Fehlercode:=0;

  if SendCommand (ESC+'?'+CR, [CR], 1, Timeout_FupAntwort, ad_String, R, dummy) then begin
    { Antworten: FN = es steht kein Ruf an
                 FZ = Ruf steht an
                 alle anderen sind unerwartete Antworten }
    FupAntwort:=ExtractString (R.Antwort, ESC, CR, 0);
    if FupAntwort = 'FZ' then begin
      Ruf_angekommen:=true;
      Result:=true;
    end
    else if FupAntwort = 'FN' then
      Result:=true;
  end
  else begin      { Fehler ist aufgetreten }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
  end;
end;

{------------------------------------------------------------------------------}

{----------------------------------}
procedure TMRGFupCommObj.Comm_Reset;
{----------------------------------}
begin
  FSerial.ResetComm;  // COM-Reset (Schnittstellenfehler löschen, Sende- und Empfangspuffer leeren)
end;

{-------------------------------------------------------------}
function TMRGFupCommObj.Comm_SendData (sData: string): boolean;
{-------------------------------------------------------------}
begin
  FSerial.TransmittText(sData, false, false);  // Befehl senden ohne COM-Reset
  Result:=true;
end;

{-------------------------------------------------------}
function TMRGFupCommObj.Comm_SendChar (c: char): boolean;
{-------------------------------------------------------}
begin
  FSerial.TransmittChar(c);  // Zeichen senden
  Result:=true;
end;

end.

