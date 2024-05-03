{******************************************************************************}
{* Basis-Klasse für Kommunikation mit Socketserver                            *}
{* 15.11.2011 WW                                                              *}
{* 02.02.2012 GD/WW  Open und Close wg. Fehler 10055 abgesichert              *}
{* 12.02.2012 WW  Empfangspuffer leeren vor Versenden des Befehls             *}
{* 03.06.2020 WW  Fehlercode SRV_ERR_TIMEOUTRECEIVE; Umstellung auf Indy-10   *}
{*                TCP-Client (Vs. 10.0.52)                                    *}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2011, 2020                                *}
{******************************************************************************}
unit O_TCPIP_CustomSrv;

interface

uses
  Windows, Forms, SysUtils, Classes, ExtCtrls, T_Tools, T_Zeit, ErrConst,
  WChars, WStrUtils, WComm, IdTCPConnection, IdTCPClient;


type
(*  { Callback-Prozedurtypen zur Anzeige in Fremdmodulen }

  TCBDataProc = procedure (Data: string; Bytes: string) of object;
  TCBStatusMsgProc = procedure (Msg: string) of object;*)

  { Basis-Klasse für Kommunikation mit Socketserver }

  TClientSocketCustomSrv = class(TObject)
  private
    { Private-Deklarationen }
    FTCPClient: TIdTCPClient;  // 03.06.2020, WW
    TimerTimeout: TTimer;
    EndeZeichenEmpfangen: boolean;
    Befehl: string;
    Timeout: integer;
    TimeoutCount: integer;
    AnswerDest: TAnswerDest;
    WorkPath: TFileName;
    RohFileName: string;
    FFehlergruppe: integer;
    FRecTerminator: string;
//    FTxD: TCBDataProc;
//    FRxD: TCBDataProc;
//    FTimeout: TCBStatusMsgProc;
    { Hilfsvariablen zur Fehlerüberwachung: }
    FCommError: cardinal;
    function GetConnected: boolean;
    procedure BefehlSenden;
    function ReceiveData: string;
    procedure TimerTimeoutTimer(Sender: TObject);
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; AWorkPath: TFileName;
      AFehlergruppe: integer); reintroduce;
    destructor Destroy; override;
    procedure SetRecTerminator (ATerminator: string);
    function Connect (AHost: string; AAddress: string; APort: integer;
      ATimeout: integer; var AFehlergruppe: integer;
      var AFehlercode: integer): boolean;
    function Disconnect (ATimeout: integer;
      var AFehlergruppe: integer; var AFehlercode: integer): boolean;
    function SendCommand (ABefehl: string; ATimeout: integer;
      AAnswerDest: TAnswerDest; var Rueckgabe: TRueckgabe): boolean;
    property Active: boolean read GetConnected;
//    property CBTxD: TCBDataProc read FTxD write FTxD;
//    property CBRxD: TCBDataProc read FRxD write FRxD;
//    property CBTimeout: TCBStatusMsgProc read FTimeout write FTimeout;
  end;

implementation


{ TClientSocketCustomSrv }

{------------------------------------------------------------}
constructor TClientSocketCustomSrv.Create (AOwner: TComponent;
  AWorkPath: TFileName; AFehlergruppe: integer);
{------------------------------------------------------------}
begin
  inherited Create;

  WorkPath:=AWorkPath;
  FFehlergruppe:=AFehlergruppe;

  FTCPClient:=TIdTCPClient.Create(nil);

  Befehl:='';
  EndeZeichenEmpfangen:=false;
  Timeout:=0;
  TimeoutCount:=0;
  AnswerDest:=ad_String;
  RohFileName:='';
  FRecTerminator:=ETX;

  { Timer für Timeout-Überwachung beim Empfangen der Daten: }
  TimerTimeout:=TTimer.Create (nil);
  TimerTimeout.Enabled:=false;
  TimerTimeout.Interval:=1000;     { 1 s }
  TimerTimeout.OnTimer:=TimerTimeoutTimer;

  FCommError:=0;
end;

{----------------------------------------}
destructor TClientSocketCustomSrv.Destroy;
{----------------------------------------}
begin
  TimerTimeout.Free;
  FTCPClient.Free;

  inherited Destroy;
end;

{----------------------------------------------------------------------}
procedure TClientSocketCustomSrv.SetRecTerminator (ATerminator: string);
{----------------------------------------------------------------------}
{ Setzt Endezeichen-String für Datenempfang;
  Übergabe: Endezeichen-String }
begin
  FRecTerminator:=ATerminator;
end;

{----------------------------------------------------}
function TClientSocketCustomSrv.GetConnected: boolean;
{----------------------------------------------------}
{ Ergebnis: True, wenn Verbindung besteht }
begin
  try
    Result:=FTCPClient.Connected;
  except
    Result:=false;
  end;
end;

{-----------------------------------------------------------------------}
function TClientSocketCustomSrv.Connect (AHost: string; AAddress: string;
  APort: integer; ATimeout: integer; var AFehlergruppe: integer;
  var AFehlercode: integer): boolean;
{-----------------------------------------------------------------------}
{ Socketverbindung öffnen;
  Übergabe: Server-Hostname
            Server-Adresse (wird nur verwendet, wenn Server-Hostname leer)
            Server-Port
            Timeout für Öffnen der Verbindung
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Socketverbindung geöffnet werden konnte }
begin
  Result:=false;

  // Vorher evtl. vorhandene Verbindung schließen; 02.02.2012
  try
    FTCPClient.Disconnect;
  except
    //
  end;

  if AHost <> '' then
    FTCPClient.Host:=AHost
  else
    // führende Nullen aus IP-Adresse ausfiltern (wird von TClientSocket als 0
    // interpretiert !): 04.01.2012, WW
    FTCPClient.Host:=WTrimIPAddress (AAddress);
    
  FTCPClient.Port:=APort;
  Timeout:=ATimeout;

  { Vorbelegung: OK }
  AFehlergruppe:=0;
  AFehlercode:=0;

  TimeoutCount:=0;
  TimerTimeout.Enabled:=true;                   { Timeout-Überwachung starten }

(*  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');
  Application.ProcessMessages;*)

  { Socket-Verbindung öffnen: }
(*  if ComLogFile <> nil then
    ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung öffnen...', Address, Port);*)
  try
    FTCPClient.Connect;
  except
    AFehlergruppe:=FFehlergruppe;
    AFehlercode:=SRV_ERR_CONNECT;
    try           // 02.02.2012
      FTCPClient.Disconnect;
    except
      //
    end;
    exit;
  end;

  { warten bis Socket-Verbindung geöffnet ist: }
  while true do begin
    Sleep (1);
    Application.ProcessMessages;
    if GetConnected then begin
      { Socket-Verbindung ist jetzt geöffnet }
(*      if ComLogFile <> nil then
        ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung ist geöffnet', Address, Port);*)
      Result:=true;
      Break;
    end;

    if TimeoutCount >= Timeout then begin    { Timeout beim Verbindung-Öffnen }
      AFehlergruppe:=FFehlergruppe;
      AFehlercode:=SRV_ERR_TIMEOUTCONNECT;
      Break;
    end;
  end;  { while }

  TimerTimeout.Enabled:=false;                  { Timeout-Überwachung beenden }
(*  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  Application.ProcessMessages;*)
end;

{---------------------------------------------------------------}
function TClientSocketCustomSrv.Disconnect (ATimeout: integer;
  var AFehlergruppe: integer; var AFehlercode: integer): boolean;
{---------------------------------------------------------------}
{ Socketverbindung schließen;
  Übergabe: Timeout für Schließen der Verbindung
  Rückgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Socketverbindung geschlossen werden konnte }
begin
  Result:=false;
  Timeout:=ATimeout;

  { Vorbelegung: OK }
  AFehlergruppe:=0;
  AFehlercode:=0;

  if not GetConnected then begin         { Socketverbindung ist bereits geschlossen }
    Result:=true;
    exit;
  end;

  TimeoutCount:=0;
  TimerTimeout.Enabled:=true;                   { Timeout-Überwachung starten }
(*  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');
  Application.ProcessMessages;*)

  { Socket-Verbindung schließen: }
(*  if ComLogFile <> nil then
    ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung schließen...', Address, Port);*)
  try
    FTCPClient.Disconnect;
  except
    AFehlergruppe:=FFehlergruppe;
    AFehlercode:=SRV_ERR_DISCONNECT;
    exit;
  end;

  { warten bis Socket-Verbindung geschlossen ist: }
  while true do begin
    Sleep (1);
    Application.ProcessMessages;
    if not GetConnected then begin
      { Socket-Verbindung ist jetzt geschlossen }
(*      if ComLogFile <> nil then
        ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung ist geschlossen', Address, Port);*)
      Result:=true;
      Break;
    end;

    if TimeoutCount >= Timeout then begin    { Timeout beim Verbindung-Schließen }
      AFehlergruppe:=FFehlergruppe;
      AFehlercode:=SRV_ERR_TIMEOUTDISCONNECT;
      Break;
    end;
  end;  { while }

  TimerTimeout.Enabled:=false;                  { Timeout-Überwachung beenden }
(*  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  Application.ProcessMessages;*)
end;

{--------------------------------------------}
procedure TClientSocketCustomSrv.BefehlSenden;
{--------------------------------------------}
begin
  EndeZeichenEmpfangen:=false;
  TimeoutCount:=0;

(*  { TxD/RxD-Anzeige: }
  if Assigned (CBTxD) then
    CBTxD (SonderzeichenString (Befehl), IntToStr (length (Befehl)) + ' Byte');
  if Assigned (CBRxD) then
    CBRxD ('', '');
  Application.ProcessMessages;*)

  // mit zusätzlicher Connected-Prüfung; 03.06.2020, WW
  if (length (Befehl) > 0) AND GetConnected then begin
(*    if ComLogFile <> nil then
      ComLogFile.Write ('S', Befehl);     { Logfileeintrag mit Kopf "Sendedaten" }*)

    ReceiveData;  { Empfangspuffer vor dem Senden leeren; 13.02.2012, WW }
    try
      FTCPClient.IOHandler.Write (Befehl);                 { Befehl versenden }
    except
      //
    end;
  end;

  if AnswerDest = ad_File then
    RohFileName:=CreateTempRohFile (WorkPath, prefix_SrvData);
  TimerTimeout.Enabled:=true;                   { Timeout-Überwachung starten }

(*  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');*)
end;

{--------------------------------------------------}
function TClientSocketCustomSrv.ReceiveData: string;
{--------------------------------------------------}
{ Liest Empfangspuffer der Socket-Verbindung aus;
  Ergebnis: Gelesene Empfangspuffer-Zeichen }
var
  n: integer;
  S: string;

begin
  if GetConnected then begin
    try
      n:=FTCPClient.IOHandler.InputBuffer.Size;  // Anzahl der Zeichen im Empfangspuffer
      if n > 0 then
        S:=FTCPClient.IOHandler.ReadString(n)  // Zeichen aus Empfangspuffer lesen
      else
        S:='';
    except
      on E:Exception do begin
        FCommError:=SRV_ERR_RECEIVE;  // Fehler beim Empfangen aufgetreten; 03.06.2020, WW
(*        if (ComLogFile <> nil) then
          ComLogFile.WriteMsg('Fehler beim Empfangen: ' + E.Message);  // 03.06.2020, WW*)
        S:='';
      end;
    end;
  end else
    S:='';

  Result:=S;
end;

{------------------------------------------------------------------------------}
function TClientSocketCustomSrv.SendCommand (ABefehl: string; ATimeout: integer;
  AAnswerDest: TAnswerDest; var Rueckgabe: TRueckgabe): boolean;
{------------------------------------------------------------------------------}
{ Befehl senden und Antwort empfangen;
  Übergabe: Befehl-String
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist
            AAnswerDest (DestString: Empfangsdaten -> Antwort-String
                         Destfile: Empfangsdaten -> Rohfile
  Rückgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AnswerDest = DestString: Empfangsdaten
                          AnswerDest = Destfile: Name des Rohfiles (kompletter Pfad)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  S: string;
  AntwortEmpfangen: boolean;
  First: boolean;

begin
  Timeout:=ATimeout;
  AnswerDest:=AAnswerDest;
  Befehl:=ABefehl;
  BefehlSenden;                                           { Befehl abschicken }

  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:=''
  end;

  if Timeout > 0 then begin               { bei Timeout > 0: Antwort auslesen }
    AntwortEmpfangen:=false;
    FCommError:=0;
    First:=true;
    while not AntwortEmpfangen AND (FCommError = 0) do begin
      Sleep (1);
      Application.ProcessMessages;

      { Überwachung, ob Socket-Verbindung noch besteht: }
      if not GetConnected then begin         { Socket-Verbindung ist nicht mehr aktiv }
        with Rueckgabe do begin
          Fehlergruppe:=FFehlergruppe;
          Fehlercode:=SRV_ERR_CONNECTION_INACTIVE;
          Antwort:='';
        end;
        Break;
      end;

      s:=ReceiveData;
      if length (s) > 0 then begin
        TimeoutCount:=0;
  (*      { Timeout-Anzeige: }
        if Assigned (CBTimeout) then
          CBTimeout ('');*)

        if First then begin                     { die ersten ausgelesenen Zeichen }
          First:=false;
  (*        if ComLogFile <> nil then
            ComLogFile.Write ('E', S);       { Logfileeintrag mit Kopf "Empfangsdaten" }

          { RxD-Anzeige: }
          if Assigned (CBRxD) then
            CBRxD (SonderzeichenString (s), IntToStr (length (s)) + ' Byte');           *)
        end  { if First }
        else begin
  (*        if ComLogFile <> nil then
            ComLogFile.Write ('D', s);               { Logfileeintrag nur Daten }

          { RxD-Anzeige: }
          if Assigned (CBRxD) then
            CBRxD (SonderzeichenString (s), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');*)
        end;

        if AnswerDest = ad_File then begin
          if not WriteRohFile (RohfileName, s) then begin
            Rueckgabe.Fehlergruppe:=ST_FILEERROR;
            Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
            Break;
          end;

          // In Antwort-Rückgabe immer nur die für die Prüfung auf abgeschlossenen
          // Empfang nötigen, letzten Empfangsdaten merken (bisher wurden die
          // kompletten Empfangsdaten im Antwort-Rückgabe-String mitgeführt (ver-
          // ursacht evtl. Speicherprobleme !); 20.06.2014, WW
          Rueckgabe.Antwort:=Copy (Rueckgabe.Antwort,
            length (Rueckgabe.Antwort) - length (FRecTerminator) + 1,
            length (FRecTerminator));
        end;
        Rueckgabe.Antwort:=Rueckgabe.Antwort + s;

        { Prüfung: Empfang abgeschlossen ? }
        s:=Copy (Rueckgabe.Antwort,
                 length (Rueckgabe.Antwort) - length (FRecTerminator) + 1,
                 length (FRecTerminator));
        AntwortEmpfangen:=s = FRecTerminator;
      end   { length (s) > 0 }
      else begin
        if TimeoutCount >= Timeout then begin       { Timeout beim Datenempfang }
(* Debug (Datahub_WicomSrv):
          WriteDebugLog (ExtractFilePath (ParamStr(0)), 'DH_WicomSrv_SendCommand_Timeout.log',
            CR + LF + 'Befehl: ' + Befehl + CR + LF + CR + LF +
            'Antwort: ' + Rueckgabe.Antwort, true, lt_Debug);*)

          with Rueckgabe do begin
            Fehlergruppe:=FFehlergruppe;
            Fehlercode:=SRV_ERR_TIMEOUTRECEIVE;  // 03.06.2020, WW
            { bei Timeout Antwort nicht löschen }
          end;
          Break;
        end;
      end;

    end; { while }

    TimerTimeout.Enabled:=false;                  { Timeout-Überwachung beenden }
  (*  { Timeout-Anzeige: }
    if Assigned (CBTimeout) then
      CBTimeout ('');

    { TxD/RxD-Anzeige: }
    if Assigned (CBRxD) then
      CBRxD (SonderzeichenString (Rueckgabe.Antwort), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
    Application.ProcessMessages;*)

    if AnswerDest = ad_File then
      Rueckgabe.Antwort:=RohFileName;         { Rückgabe: Antwort = Rohfilename }

    if FCommError <> 0 then begin              { Socket-Fehler ist aufgetreten }
      { Rückgabe "Verbindung nicht mehr aktiv" nicht mit TCP-CommError überschreiben: }
      if GetConnected then begin
        with Rueckgabe do begin              { Rueckgabe belegen mit Socketfehler }
          Fehlergruppe:=FFehlergruppe;
          Fehlercode:=FCommError;
          Antwort:='';
        end;
      end;
    end;

    if (Rueckgabe.Fehlergruppe <> 0) AND (AnswerDest = ad_File) then  { Rohfile löschen bei Fehler }
      DeleteFile (RohFileName);
  end;  { if Timeout > 0 }
  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{-------------------------------------------------------------------}
procedure TClientSocketCustomSrv.TimerTimeoutTimer (Sender: TObject);
{-------------------------------------------------------------------}
begin
  TimeoutCount:=TimeoutCount + integer(TimerTimeout.Interval);
(*  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr((Timeout-TimeoutCount) DIV 1000) + ' s');*)
end;

end.

