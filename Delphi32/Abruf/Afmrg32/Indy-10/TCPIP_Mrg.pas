{******************************************************************************}
{* Unit: MRG-Kommunikation �ber TCP/IP                                        *}
{* 10.03.2009 WW                                                              *}
{* 04.01.2012 WW Bugfix Connect bei IP-Adresse mit f�hrenden Nullen (in       *}
{*            TMRGClientSocketCommObj)                                        *}
{* 09.10.2013 WW TMRGClientSocketCommObj.Destroy mit Disconnect               *}
{* 16.12.2019 WW mit Modbus TCP/IP                                            *}
{* 25.02.2020 WW Erweiterung TDSfGClientSocketCommObj f�r Verbindung per DNS  *}
{*               Hostname                                                     *}
{* 03.06.2020 WW Umstellung auf Indy-10 TCP-Client (Vs. 10.0.52)              *}
{* 03.01.2022 WW mit ComTracelog                                              *}
{* 04.10.2022 WW Modbus TCP/IP als Default eliminiert                         *}
{* 19.12.2022 WW Timeout-�berwachung: Timer durch TickCount ersetzt           *}
{******************************************************************************}
unit TCPIP_Mrg;

interface

uses
  Windows, SysUtils, Forms, Classes, T_Zeit,
  ErrConst, LogCom, WStrUtils, WComm, SerMrgModem, O_Comm, GPRSTelegrList,
  GPRSVerbList, ModbusUtil, IdTCPConnection, IdTCPClient;

type
  { Objekt f�r MRG-Kommunikation �ber Socket }

  TMRGCustomSocketCommObj = class(TMRGCustomCommObj)
  private
    { Private-Deklarationen }
    FConnection: TIdTCPConnection;  // 03.06.2020, WW
    function GetConnected: boolean;
  protected
    procedure Comm_Reset; override;
    function Comm_SendData (sData: string): boolean; override;
    function Comm_SendChar (c: char): boolean; override;
    function Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal;
                               bSaveFirstCommError: boolean): string; override;
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; override;
    procedure Comm_ClearRecBuf; override;
    procedure Comm_SetFehlerRueckgabe (NoCarrier: boolean; var Rueckgabe: TRueckgabe); override;
  public
    { Public-Deklarationen }
    constructor Create (AConnection: TIdTCPConnection; AWorkPath: TFileName;
                        AComLogFile: TComLogFile);
    function Rufabfrage (var Ruf_angekommen: boolean;
                         var Fehlergruppe: integer; var Fehlercode: integer): boolean; override;
  end;


  { Objekt f�r MRG-Kommunikation �ber GPRS }

  TMRGGPRSCommObj = class(TMRGCustomSocketCommObj)
  private
    { Private-Deklarationen }
    FGPRSVerbindungenListe: TGPRSVerbList;
    FGPRSTelegrammListe: TGPRSTelegrList;
  protected
    function Comm_SendData (sData: string): boolean; override;
    function Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal;
                               bSaveFirstCommError: boolean): string; override;
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; override;
    procedure AddPushTelegramToList (ATelegram: string; AGeraetetyp: integer); override;
  public
    { Public-Deklarationen }
    constructor Create (AConnection: TIdTCPConnection; AWorkPath: TFileName;
                        AComLogFile: TComLogFile; AGPRSVerbindungenListe: TGPRSVerbList;
                        AGPRSTelegrammListe: TGPRSTelegrList);
    property GPRSRemoteAddress: string read FGPRSRemoteAddress;
  end;


  { Objekt f�r MRG-Kommunikation �ber Client-Socket }

  TMRGClientSocketCommObj = class(TMRGCustomSocketCommObj)
  private
    FTCPClient: TIdTCPClient;  // 03.06.2020, WW
  public
    constructor Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
    destructor Destroy; override;
    function Connect (AAddress_Host: string; APort: integer; ATimeout: integer;
                      var AFehlergruppe: integer; var AFehlercode: integer): boolean;
    function Disconnect (ATimeout: integer;
                         var AFehlergruppe: integer; var AFehlercode: integer): boolean;
  end;

implementation

{ TMRGCustomSocketCommObj }

{------------------------------------------------------------------------}
constructor TMRGCustomSocketCommObj.Create (AConnection: TIdTCPConnection;
  AWorkPath: TFileName; AComLogFile: TComLogFile);
{------------------------------------------------------------------------}
begin
  inherited Create (AWorkPath, AComLogFile);
  FConnection:=AConnection;
end;

{-----------------------------------------------------}
function TMRGCustomSocketCommObj.GetConnected: boolean;
{-----------------------------------------------------}
var
  Dummy: TRueckgabe;

begin
  Result:=Comm_Connection (Dummy);
end;

{-----------------------------------------------------------------------------}
function TMRGCustomSocketCommObj.Rufabfrage (var Ruf_angekommen: boolean;
                                             var Fehlergruppe: integer;
                                             var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------}
begin
  // Rufabfrage wird �ber Sockets nicht unterst�tzt, daher R�ckgabe "Kein Ruf":
  Ruf_angekommen:=false;
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;
end;

{------------------------------------------------------------------------------}

{-------------------------------------------}
procedure TMRGCustomSocketCommObj.Comm_Reset;
{-------------------------------------------}
begin
  // Kein "Socket-Reset" (Schnittstellenfehler l�schen entf�llt, Sende- und
  // Empfangspuffer werden nicht geleert)
end;

{----------------------------------------------------------------------}
function TMRGCustomSocketCommObj.Comm_SendData (sData: string): boolean;
{----------------------------------------------------------------------}
var
  sLog: string;
  
begin
  // mit zus�tzlicher Connected-Pr�fung; 03.06.2020, WW
  if Assigned (FConnection) AND GetConnected then begin
    try
      FConnection.IOHandler.Write (sData);   { Daten �ber Socket versenden }
      Result:=true;
    except
      on E:Exception do begin
        sLog:='Fehler beim Senden: ' + E.Message;
        if (FComLogFile <> nil) then
          FComLogFile.WriteMsg(sLog);  // 03.06.2020, WW
        if (FComTraceLog <> nil) then
          FComTraceLog.WriteMsg(sLog);  // 03.01.2022, WW
        Result:=false;  // 03.06.2020, WW
      end;
    end;
  end else
    Result:=false;
end;

{----------------------------------------------------------------}
function TMRGCustomSocketCommObj.Comm_SendChar (c: char): boolean;
{----------------------------------------------------------------}
begin
  Result:=Comm_SendData (c);
end;

{---------------------------------------------------------------------------------}
function TMRGCustomSocketCommObj.Comm_ReceiveData (
  AnzCharsToReceive, ChToRec_Left: cardinal; bSaveFirstCommError: boolean): string;
{---------------------------------------------------------------------------------}
var
  n: integer;
  S: string;
  sLog: string;

begin
  if Assigned (FConnection) AND GetConnected then begin
    try
      n:=FConnection.IOHandler.InputBuffer.Size;  // Anzahl der Zeichen im Empfangspuffer

      if AnzCharsToReceive > 0 then     { wenn feste Anzahl an zu empfangenden Zeichen �bergeben wurde }
        if n > integer(ChToRec_Left) then n:=ChToRec_Left;     { ...Anzahl begrenzen; 12.09.2008 WW }

      if n > 0 then
        S:=FConnection.IOHandler.ReadString(n)  // Zeichen aus Empfangspuffer lesen
      else
        S:='';
    except
      on E:Exception do begin
        FCommError:=TCPIP_ERR_RECEIVE;  // Fehler beim Empfangen aufgetreten; 03.06.2020, WW
        sLog:='Fehler beim Empfangen: ' + E.Message;
        if (FComLogFile <> nil) then
          FComLogFile.WriteMsg(sLog);  // 03.06.2020, WW
        if (FComTraceLog <> nil) then
          FComTraceLog.WriteMsg(sLog);  // 03.01.2022, WW
        S:='';  // 08.01.2018, WW
      end;
    end;
  end else
    S:='';

  Result:=S;
end;

{------------------------------------------------------------------------------------}
function TMRGCustomSocketCommObj.Comm_Connection (var Rueckgabe: TRueckgabe): boolean;
{------------------------------------------------------------------------------------}
begin
  { �berwachung, ob Socket-Verbindung noch besteht: }
  try
    if Assigned (FConnection) then
      Result:=FConnection.Connected  { Socket-Verbindung ist nicht aktiv -> Verbindung unterbrochen }
    else
      Result:=false;
  except
    Result:=false;  // 03.06.2020, WW
  end;

  if not Result then begin
    with Rueckgabe do begin
      Fehlergruppe:=COM_TCPIP_ERROR;
      Fehlercode:=TCPIP_ERR_CONNECTION_INACTIVE;
      Antwort:='';
    end;
  end;
end;

{-------------------------------------------------}
procedure TMRGCustomSocketCommObj.Comm_ClearRecBuf;
{-------------------------------------------------}
begin
  // Empfangspuffer wird nicht geleert
end;

{------------------------------------------------------------------------------------}
procedure TMRGCustomSocketCommObj.Comm_SetFehlerRueckgabe (NoCarrier: boolean;
                                                           var Rueckgabe: TRueckgabe);
{------------------------------------------------------------------------------------}
begin
  { Rueckgabe belegen mit TCP-CommError:
   -> R�ckgabe "Verbindung unterbrochen" nicht mit TCP-CommError �berschreiben }
  if not NoCarrier then begin
    with Rueckgabe do begin
      Fehlergruppe:=COM_TCPIP_ERROR;
      Fehlercode:=FCommError;
      Antwort:='';
    end;
  end;
end;


{ TMRGGPRSCommObj }

{--------------------------------------------------------------------------------------}
constructor TMRGGPRSCommObj.Create (AConnection: TIdTCPConnection; AWorkPath: TFileName;
                                    AComLogFile: TComLogFile;
                                    AGPRSVerbindungenListe: TGPRSVerbList;
                                    AGPRSTelegrammListe: TGPRSTelegrList);
{--------------------------------------------------------------------------------------}
begin
  inherited Create (AConnection, AWorkPath, AComLogFile);
  FGPRSVerbindungenListe:=AGPRSVerbindungenListe;
  FGPRSTelegrammListe:=AGPRSTelegrammListe;

  if Assigned (FConnection) then begin
    FGPRSRemoteAddress:=FConnection.Socket.Binding.PeerIP;
    FGPRSRemotePort:=FConnection.Socket.Binding.PeerPort;
  end;
end;

{--------------------------------------------------------------}
function TMRGGPRSCommObj.Comm_SendData (sData: string): boolean;
{--------------------------------------------------------------}
var
  AnzSendBytes: integer;

begin
  Result:=inherited Comm_SendData (sData);
  if Result AND Assigned (FGPRSVerbindungenListe) then begin
    AnzSendBytes:=length (sData);
    FGPRSVerbindungenListe.IncrementSendCounts (FGPRSRemoteAddress, 1, AnzSendBytes);
  end;
end;

{-----------------------------------------------------------------------------------}
function TMRGGPRSCommObj.Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal;
                                           bSaveFirstCommError: boolean): string;
{-----------------------------------------------------------------------------------}
var
  AnzRecBytes: integer;

begin
  Result:=inherited Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left, bSaveFirstCommError);

  AnzRecBytes:=length (Result);
  if (AnzRecBytes > 0) AND Assigned (FGPRSVerbindungenListe) then
    FGPRSVerbindungenListe.IncrementRecCounts (FGPRSRemoteAddress, 0, 1, AnzRecBytes);
end;

{----------------------------------------------------------------------------}
function TMRGGPRSCommObj.Comm_Connection (var Rueckgabe: TRueckgabe): boolean;
{----------------------------------------------------------------------------}
begin
  Result:=inherited Comm_Connection (Rueckgabe);

  if not Result AND Assigned (FGPRSVerbindungenListe) then
    if FGPRSVerbindungenListe.GetAktiv (FGPRSRemoteAddress) then  // nur wenn Verbindung aktiv
      FGPRSVerbindungenListe.IncrementVerbCount (false, FGPRSRemoteAddress, -1);
end;

{----------------------------------------------------------------------------------------}
procedure TMRGGPRSCommObj.AddPushTelegramToList (ATelegram: string; AGeraetetyp: integer);
{----------------------------------------------------------------------------------------}
{ Telegramm in GPRS-Push-Telegrammliste anh�ngen;
  �bergabe: Telegramm
            Ger�tetyp }
begin
  if Assigned (FGPRSTelegrammListe) then
    FGPRSTelegrammListe.SetTelegramm (ATelegram, AGeraetetyp, FGPRSRemoteAddress,
                                      FConnection);
end;


{ TMRGClientSocketCommObj }

{--------------------------------------------------------------------}
constructor TMRGClientSocketCommObj.Create (AWorkPath: TFileName;
                                            AComLogFile: TComLogFile);
{--------------------------------------------------------------------}
begin
  FTCPClient:=TIdTCPClient.Create(nil);

  inherited Create (FTCPClient, AWorkPath, AComLogFile);
end;

{-----------------------------------------}
destructor TMRGClientSocketCommObj.Destroy;
{-----------------------------------------}
var
  AFehlergruppe: integer;
  AFehlercode: integer;

begin
  Disconnect (5000, AFehlergruppe, AFehlercode);  // 09.10.2013, WW

  inherited Destroy;
  FTCPClient.Free;
end;

{----------------------------------------------------------------------------------}
function TMRGClientSocketCommObj.Connect (AAddress_Host: string; APort: integer;
  ATimeout: integer; var AFehlergruppe: integer; var AFehlercode: integer): boolean;
{----------------------------------------------------------------------------------}
{ Socketverbindung �ffnen;
  �bergabe: Socket-Adresse (IPv4) oder Socket-Hostname
            Socket-Port
            Timeout f�r �ffnen der Verbindung
  R�ckgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Socketverbindung ge�ffnet werden konnte }
var
  sLog: string;
  
begin
  Result:=false;

  if WIsIPv4Address(AAddress_Host) then begin  // 25.02.2020, WW
    // f�hrende Nullen aus IP-Adresse ausfiltern (wird von TClientSocket als 0
    // interpretiert !): 04.01.2012, WW
    FTCPClient.Host:=WTrimIPAddress (AAddress_Host);
  end
  else begin
    FTCPClient.Host:=AAddress_Host;  // 25.02.2020, WW
  end;

  FTCPClient.Port:=APort;
  Timeout:=ATimeout;

  { Vorbelegung: OK }
  AFehlergruppe:=0;
  AFehlercode:=0;

  Timeout_Init;  // Timeout-�berwachung initialisieren; 19.12.2022, WW

  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');
  Application.ProcessMessages;

  { Socket-Verbindung �ffnen: }
  sLog:='TCP/IP-Verbindung �ffnen...';
  if FComLogFile <> nil then
    FComLogFile.WriteTCPIP_Msg (sLog, FTCPClient.Host,  // 25.02.2020, WW
      '', FTCPClient.Port);
  if FComTraceLog <> nil then
    FComTraceLog.WriteTCPIP_Msg (sLog, FTCPClient.Host, '', FTCPClient.Port);  // 03.01.2022, WW
  try
    FTCPClient.Connect;
  except
    on E:Exception do begin    // 12.07.2010
      AFehlergruppe:=COM_TCPIP_ERROR;
      AFehlercode:=TCPIP_ERR_CONNECT;
      sLog:='TCP/IP-Verbindung konnte nicht ge�ffnet werden: ' + E.Message;
      if (FComLogFile <> nil) then
        FComLogFile.WriteTCPIP_Msg(sLog, FTCPClient.Host,  // 25.02.2020, WW
          '', FTCPClient.Port);
      if (FComTraceLog <> nil) then
        FComTraceLog.WriteTCPIP_Msg(sLog, FTCPClient.Host, '', FTCPClient.Port);  // 03.01.2022, WW
      exit;
    end;
  end;

  { warten bis Socket-Verbindung ge�ffnet ist: }
  while true do begin
    Sleep (1);
    Application.ProcessMessages;
    Timeout_Elapsed;  // Verstrichene Zeit messen f�r Timeout-�berwachung; 19.12.2022, WW

    if GetConnected then begin
      { Socket-Verbindung ist jetzt ge�ffnet }
      sLog:='TCP/IP-Verbindung ist ge�ffnet';
      if FComLogFile <> nil then
        FComLogFile.WriteTCPIP_Msg (sLog, FTCPClient.Host,  // 25.02.2020, WW
          '', FTCPClient.Port);
      if FComTraceLog <> nil then
        FComTraceLog.WriteTCPIP_Msg (sLog, FTCPClient.Host, '', FTCPClient.Port);  // 03.01.2022, WW
      Result:=true;
      Break;
    end;

    if TimeoutCount >= Timeout then begin    { Timeout beim Verbindung-�ffnen }
      AFehlergruppe:=COM_TCPIP_ERROR;
      AFehlercode:=TCPIP_ERR_TIMEOUTCONNECT;
      Break;
    end;
  end;  { while }

  if (not Result) then begin  // 12.07.2010
    sLog:='TCP/IP-Verbindung konnte nicht ge�ffnet werden';
    if FComLogFile <> nil then
      FComLogFile.WriteTCPIP_Msg (sLog, FTCPClient.Host,  // 25.02.2020, WW
        '', FTCPClient.Port);
    if FComTraceLog <> nil then
      FComTraceLog.WriteTCPIP_Msg (sLog, FTCPClient.Host, '', FTCPClient.Port);  // 03.01.2022, WW
  end;

  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  Application.ProcessMessages;
end;

{---------------------------------------------------------------}
function TMRGClientSocketCommObj.Disconnect (ATimeout: integer;
  var AFehlergruppe: integer; var AFehlercode: integer): boolean;
{---------------------------------------------------------------}
{ Socketverbindung schlie�en;
  �bergabe: Timeout f�r Schlie�en der Verbindung
  R�ckgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Socketverbindung geschlossen werden konnte }
var
  sLog: string;

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

  Timeout_Init;  // Timeout-�berwachung initialisieren; 19.12.2022, WW

  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');
  Application.ProcessMessages;

  { Socket-Verbindung schlie�en: }
  sLog:='TCP/IP-Verbindung schlie�en...';
  if FComLogFile <> nil then
    FComLogFile.WriteTCPIP_Msg (sLog, FTCPClient.Host,  // 25.02.2020, WW
      '', FTCPClient.Port);
  if FComTraceLog <> nil then
    FComTraceLog.WriteTCPIP_Msg (sLog, FTCPClient.Host, '', FTCPClient.Port);  // 03.01.2022, WW
  try
    FTCPClient.Disconnect;
  except
    on E:Exception do begin    // 03.06.2020
      AFehlergruppe:=COM_TCPIP_ERROR;
      AFehlercode:=TCPIP_ERR_DISCONNECT;
      sLog:='TCP/IP-Verbindung konnte nicht geschlossen werden: ' + E.Message;
      if (FComLogFile <> nil) then
        FComLogFile.WriteTCPIP_Msg(sLog, FTCPClient.Host, '', FTCPClient.Port);
      if (FComTraceLog <> nil) then
        FComTraceLog.WriteTCPIP_Msg(sLog, FTCPClient.Host, '', FTCPClient.Port);  // 03.01.2022, WW
      exit;
    end;
  end;

  { warten bis Socket-Verbindung geschlossen ist: }
  while true do begin
    Sleep (1);
    Application.ProcessMessages;
    Timeout_Elapsed;  // Verstrichene Zeit messen f�r Timeout-�berwachung; 19.12.2022, WW

    if not GetConnected then begin
      { Socket-Verbindung ist jetzt geschlossen }
      sLog:='TCP/IP-Verbindung ist geschlossen';
      if FComLogFile <> nil then
        FComLogFile.WriteTCPIP_Msg (sLog, FTCPClient.Host,  // 25.02.2020, WW
          '', FTCPClient.Port);
      if FComTraceLog <> nil then
        FComTraceLog.WriteTCPIP_Msg (sLog, FTCPClient.Host, '', FTCPClient.Port);  // 03.01.2022, WW
      Result:=true;
      Break;
    end;

    if TimeoutCount >= Timeout then begin    { Timeout beim Verbindung-Schlie�en }
      AFehlergruppe:=COM_TCPIP_ERROR;
      AFehlercode:=TCPIP_ERR_TIMEOUTDISCONNECT;
      Break;
    end;
  end;  { while }

  if (not Result) then begin  // 03.06.2020, WW
    sLog:='TCP/IP-Verbindung konnte nicht geschlossen werden';
    if FComLogFile <> nil then
      FComLogFile.WriteTCPIP_Msg (sLog, FTCPClient.Host, '', FTCPClient.Port);
    if FComTraceLog <> nil then
      FComTraceLog.WriteTCPIP_Msg (sLog, FTCPClient.Host, '', FTCPClient.Port);  // 03.01.2022, WW
  end;

  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  Application.ProcessMessages;
end;

end.

