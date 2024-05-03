{******************************************************************************}
{* Basis-Klasse für Kommunikation mit Socketserver                            *}
{* 15.11.2011 WW                                                              *}
{* 02.02.2012 GD/WW  Open und Close wg. Fehler 10055 abgesichert              *}
{* 12.02.2012 WW  Empfangspuffer leeren vor Versenden des Befehls             *}
{* 03.06.2020 WW  Fehlercode SRV_ERR_TIMEOUTRECEIVE                           *}
{*                                                                            *}
{* Copyright © RMG Messtechnik GmbH 2011, 2020                                *}
{******************************************************************************}
unit O_TCPIP_CustomSrv;

interface

uses
  Windows, Forms, SysUtils, Classes, ExtCtrls, scktcomp, T_Tools, T_Zeit, ErrConst,
  WChars, WStrUtils, WComm;


type
(*  { Callback-Prozedurtypen zur Anzeige in Fremdmodulen }

  TCBDataProc = procedure (Data: string; Bytes: string) of object;
  TCBStatusMsgProc = procedure (Msg: string) of object;*)

  { Basis-Klasse für Kommunikation mit Socketserver }

  TClientSocketCustomSrv = class(TClientSocket)
  private
    { Private-Deklarationen }
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
    SocketError: boolean;
    SocketErrorEvent: TErrorEvent;
    procedure BefehlSenden;
    procedure ErrorEventToFehlerGruppeCode (AErrorEvent: TErrorEvent;
      var AFehlergruppe: integer; var AFehlercode: integer);
    procedure ClientSocketError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
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
  inherited Create(nil);
  WorkPath:=AWorkPath;
  FFehlergruppe:=AFehlergruppe;

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

  SocketError:=false;
  SocketErrorEvent:=eeGeneral;
  OnError:=ClientSocketError;    { Fehler-Eregnisbehandlungsroutine zuweisen }
end;

{----------------------------------------}
destructor TClientSocketCustomSrv.Destroy;
{----------------------------------------}
begin
  TimerTimeout.Free;
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

  Active := False;   // 02.02.2012
  Socket.Close;
  Close;

  Host:=AHost;
  // führende Nullen aus IP-Adresse ausfiltern (wird von TClientSocket als 0
  // interpretiert !): 04.01.2012, WW
  Address:=WTrimIPAddress (AAddress);
  Port:=APort;
  Timeout:=ATimeout;

  { Vorbelegung: OK }
  AFehlergruppe:=0;
  AFehlercode:=0;
  SocketError:=false;

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
    Open;
  except
    AFehlergruppe:=FFehlergruppe;
    AFehlercode:=SRV_ERR_CONNECT;
    try           // 02.02.2012
      Active := False;
      Socket.Close;
      Close;
    except
    end;
    exit;
  end;
  { warten bis Socket-Verbindung geöffnet ist oder Fehler aufgetreten ist: }
  while true do begin
    Sleep (1);
    Application.ProcessMessages;
    if Active then begin
      { Socket-Verbindung ist jetzt geöffnet }
(*      if ComLogFile <> nil then
        ComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung ist geöffnet', Address, Port);*)
      Result:=true;
      Break;
    end;
    if SocketError then Break;                { Socket-Fehler ist aufgetreten }

    if TimeoutCount >= Timeout then begin    { Timeout beim Verbindung-Öffnen }
      AFehlergruppe:=FFehlergruppe;
      AFehlercode:=SRV_ERR_TIMEOUTCONNECT;
      Break;
    end;                                          end;  { while }

  TimerTimeout.Enabled:=false;                  { Timeout-Überwachung beenden }
(*  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  Application.ProcessMessages;*)

  if SocketError then
    ErrorEventToFehlerGruppeCode (SocketErrorEvent, AFehlergruppe, AFehlercode);
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
  SocketError:=false;

  if not Active then begin         { Socketverbindung ist bereits geschlossen }
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
    Active := False;  // 02.02.2012
    Socket.Close;     // 02.02.2012
    Close;
  except
    AFehlergruppe:=FFehlergruppe;
    AFehlercode:=SRV_ERR_DISCONNECT;
    exit;
  end;
  { warten bis Socket-Verbindung geschlossen ist: }
  while true do begin
    Sleep (1);
    Application.ProcessMessages;
    if not Active then begin
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

  if SocketError then begin
    ErrorEventToFehlerGruppeCode (SocketErrorEvent, AFehlergruppe, AFehlercode);
    Result:=false;
  end;
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

  if length (Befehl) > 0 then begin
(*    if ComLogFile <> nil then
      ComLogFile.Write ('S', Befehl);     { Logfileeintrag mit Kopf "Sendedaten" }*)
    Socket.ReceiveText;  { Empfangspuffer vor dem Senden leeren; 13.02.2012, WW }
    Socket.SendText (Befehl);                             { Befehl versenden }
  end;

  if AnswerDest = ad_File then
    RohFileName:=CreateTempRohFile (WorkPath, prefix_SrvData);
  TimerTimeout.Enabled:=true;                   { Timeout-Überwachung starten }

(*  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');*)
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
    SocketError:=false;
    First:=true;
    while not AntwortEmpfangen AND not SocketError do begin
      Sleep (1);
      Application.ProcessMessages;

      s:=Socket.ReceiveText;
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

      { Überwachung, ob Socket-Verbindung noch besteht: }
      if not Active then begin         { Socket-Verbindung ist nicht mehr aktiv }
        with Rueckgabe do begin
          Fehlergruppe:=FFehlergruppe;
          Fehlercode:=SRV_ERR_CONNECTION_INACTIVE;
          Antwort:='';
        end;
        Break;
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

    if SocketError then begin              { Socket-Fehler ist aufgetreten }
      { Rückgabe "Verbindung nicht mehr aktiv" nicht mit SocketError überschreiben: }
      if Active then begin
        with Rueckgabe do begin              { Rueckgabe belegen mit Socketfehler }
          ErrorEventToFehlerGruppeCode (SocketErrorEvent, Fehlergruppe, Fehlercode);
          Antwort:='';
        end;
      end;
    end;

    if (Rueckgabe.Fehlergruppe <> 0) AND (AnswerDest = ad_File) then  { Rohfile löschen bei Fehler }
      DeleteFile (RohFileName);
  end;  { if Timeout > 0 }
  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{--------------------------------------------------------------------------------}
procedure TClientSocketCustomSrv.ErrorEventToFehlerGruppeCode (
  AErrorEvent: TErrorEvent; var AFehlergruppe: integer; var AFehlercode: integer);
{--------------------------------------------------------------------------------}
{ Umsetzung Socket-ErrorEvent -> Fehlergruppe, Fehlercode }
begin
  AFehlergruppe:=FFehlergruppe;

  case AErrorEvent of
    eeGeneral:    AFehlercode:=SRV_ERR_GENERAL;
    eeConnect:    AFehlercode:=SRV_ERR_CONNECT;
    eeDisconnect: AFehlercode:=SRV_ERR_DISCONNECT;
    eeReceive:    AFehlercode:=SRV_ERR_RECEIVE;
    eeSend:       AFehlercode:=SRV_ERR_SEND;
    eeAccept:     AFehlercode:=SRV_ERR_ACCEPT;
    eeLookup:     AFehlercode:=SRV_ERR_LOOKUP;  // 10.01.2007, WW
  else
    AFehlercode:=SRV_ERR_UNDEFINIERT;  // 10.01.2007, WW
  end;
end;

{---------------------------------------------------------------------------}
procedure TClientSocketCustomSrv.ClientSocketError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
{---------------------------------------------------------------------------}
begin
  SocketError:=true;
  SocketErrorEvent:=ErrorEvent;
  { Rückgabe: }
  ErrorCode:=0;  // Exception-Ausgabe unterdrücken
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

