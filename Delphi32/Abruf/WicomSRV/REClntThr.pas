{******************************************************************************}
{* Unit: Thread für TCP/IP-Rufentgegennahme                                   *}
{* 08.01.2018  WW                                                             *}
{* 03.06.2020 WW Umstellung auf Indy-10 TCP-Connection (Vs. 10.0.52)          *}
{* 14.03.2024 WW Bugfix Speicherleck; Automatische Thread-Terminierung ent-   *}
{*               fernt (Synchronisationsproblem mit aufrufendem Thread)       *}
{******************************************************************************}
unit REClntThr;

interface

uses
  Windows, Forms, Classes, SysUtils,
  PathIni, WErrMsg, ErrConst, T_Tools, TCPIP_DSfG, O_Comm, WComm, WChars,
  REClntCmdList, LogFile, LogCom, AbrufConst, SrvCfgIni, IdTCPConnection;

type
  { TCP/IP-Rufentgegennahme-Thread }

  TREClntThread = class(TThread)
  private
    FDSfGSocketCommObj: TDSfGCustomSocketCommObj;  { DSfG-Kommunikations-Objekt für Client (TCP/IP) }
    FRemoteAddress: string;
    FRemotePort: integer;

    FLogFile: TCustomLogFile;
    FComLogFile: TComLogFile;
    FSrvCfgIni: TSrvCfg32Ini;

    FIndex: integer;  { Index des Eintrags in der RE-Verbindungsliste, unter dem der Anruf läuft }
    FCallingFormHandle: HWND;  { Handle des Formulars, welches den Thread aufruft }
    FKommandoListe: TREClntKommandoListe; { Kommandoliste für Thread }
    FNoCarrier: boolean;   // Verbindungsstatus zum DSfG-Client
    FDebugCOMProtokoll: boolean;
    FDebugRundpufferMaxBytes: integer;

    function Kommando_Ausfuehren (AKommandoObj: TREClntCmdObj): boolean;
    procedure WriteLogFile_Kommando (AKommandoObj: TREClntCmdObj);
    procedure Init_ComLogFile;
    procedure DisconnectClient;
  protected
    procedure Execute; override;
  public
    constructor CreateIt (AConnection: TIdTCPConnection;
                          AIndex: integer;
                          AKommandoListe: TREClntKommandoListe;
                          ACallingFormHandle: HWND);
    destructor Destroy; override;
  end;


  { Objekt für DSfG-Kommunikation über TCP/IP-Rufentgegennahme-Thread }

  TDSfGREClntThreadCommObj = class(TDSfGCommObj)
    FKommandoListe: TREClntKommandoListe;
    FLogFile: TCustomLogFile;
  private
    function WaitForKommandoErledigt (ATimeout: integer): TREClntCmdObj;
    procedure WriteLogFile_TimeoutAntwort (sMethodName: string; iTimeout: integer);
  public
    constructor Create (pKommandoListe: TREClntKommandoListe;
      ALogFile: TCustomLogFile);
    function SetExtensionmode (Value: byte): byte; override;
    function SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                          AEndezeichenAnzahl: integer;
                          ATimeout: integer; AAnswerDest: TAnswerDest;
                          var Rueckgabe: TRueckgabe; var NoCarrier: boolean;
                          AAnzCharsToReceive: cardinal = 0): boolean; override;
    function Disconnect: boolean;
    // Bei den folgenden Methoden wird auf eine Umsetzung verzichtet:
    //  - SetVersuche -> Standard-Anzahl für Versuche bislang immer ausreichend
    //  - GetMaxExtensionmode -> Liefert statische Information, Umsetzung nicht erforderlich
    //  - InitializeSysTimePW -> Wird bei RE nicht verwendet
   end;

implementation

const
  C_TOKommandoErledigt = 5000;  // 5 s sollten für Aktionen die "sofort" erfolgen ausreichen


{ TREClntThread }

{-----------------------------------------------------------------------}
constructor TREClntThread.CreateIt (AConnection: TIdTCPConnection;
                                    AIndex: integer;
                                    AKommandoListe: TREClntKommandoListe;
                                    ACallingFormHandle: HWND);
{-----------------------------------------------------------------------}
{ TCP/IP-Rufentgegennahme-Thread erzeugen;
  Übergabe: Socket-Verbindung zum Client
            Zugeordneter Thread-Index
            Kommandoliste für Thread
            Handle des aufrufenden Formulars }
var
  LogFilename: string;
  S: string;

begin
  inherited Create (true); // Thread createn und gleich wieder anhalten
  FreeOnTerminate:=true;  // Thread soll sich beim Beenden selbst freigeben
  Priority:=tpNormal;     // Thread soll mit normaler Priorität ausgeführt werden

  FIndex:=AIndex;
  FKommandoListe:=AKommandoListe;
  FCallingFormHandle:=ACallingFormHandle;

  FDSfGSocketCommObj:=TDSfGCustomSocketCommObj.Create (AConnection,
    PathServer.PathName [WWorkDir], nil);  // DSfG-Kommunikations-Objekt für Client-Verbindung anlegen

  FRemoteAddress:=AConnection.Socket.Binding.PeerIP;
  FRemotePort:=AConnection.Socket.Binding.PeerPort;

  FSrvCfgIni:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);

  // Logfile für Protokollierung des Anrufablaufs initialisieren, wenn lt.
  // INI-Konfiguration eingestellt:
  if FSrvCfgIni.DebAbrufProtokoll then begin
    // Logfilename mit Rufentgegennahme-Thread-Index:
    LogFilename:='RTHR_IP_' + Format ('%.3d', [AIndex]);
    FLogFile:=TCustomLogFile.Create (PathServer.Pathname[WLogDir], LogFilename, false);
  end;

  FDebugCOMProtokoll:=FSrvCfgIni.DebCOMProtokoll;
  if FSrvCfgIni.DebRundpuffer then
    FDebugRundpufferMaxBytes:=FSrvCfgIni.DebRundpuffergroesse_kB * 1000
  else
    FDebugRundpufferMaxBytes:=0;  // kein Rundpuffer

  // Rundpuffer auf Anruf-Logfile ausführen:
  if FDebugRundpufferMaxBytes > 0 then begin
    if Assigned (FLogFile) then
      FLogFile.DoRundpuffer (FDebugRundpufferMaxBytes);
  end;

  // Connect loggen:
  if FLogFile <> nil then begin
    S:='Connected ' + FRemoteAddress + ':' + IntToStr(FRemotePort);
    FLogFile.Write (S);    { Logfile-Protokollierung }
  end;

  FComLogFile:=nil;  // Default, wird erst in Execute-Methode angelegt
  FNoCarrier:=false;  // IP-Verbindung steht bereits

  if FLogFile <> nil then
    FLogFile.Write ('Thread gestartet (Programmversion: ' + CVersion_WicomSrv + ')');  { Logfile-Protokollierung }
  Suspended:=false;         // Thread jetzt fortsetzen
end;

{-------------------------------}
destructor TREClntThread.Destroy;
{-------------------------------}
begin
  FDSfGSocketCommObj.Free; // Bugfix Speicherleck (Aufruf jetzt ohne Absturz); 14.03.2024, WW
  FSrvCfgIni.Free;
  FComLogFile.Free;

  if FLogFile <> nil then
    FLogFile.Write ('Thread beendet (Programmversion: ' + CVersion_WicomSrv + ')');    { Logfile-Protokollierung }
  FLogFile.Free;

  inherited destroy;
  { Botschaft an das aufrufende Fenster schicken: Thread ist beendet }
  PostMessage (FCallingFormHandle, wm_REClntThreadDone, FIndex, 0);
end;

{------------------------------}
procedure TREClntThread.Execute;
{------------------------------}
{ Thread-Ausführung }
var
  Anruf_beendet: boolean;
  KommandoObj: TREClntCmdObj;

begin
  // 14.03.2024, WW: Execute wird erst verlassen, wenn der Thread von außen
  // terminiert wird: 
  Anruf_beendet:=false;
  while true do begin
    try
      KommandoObj:=FKommandoListe.GetKommandoObj (false); // noch nicht abgearbeitetes Kommando-Objekt aus Liste lesen
      if Assigned (KommandoObj) AND not Anruf_beendet then begin  // wenn Kommando vorhanden
        WriteLogFile_Kommando (KommandoObj);  // Aus Kommandoliste gelesenes Kommando im Logfile protokollieren
        Init_ComLogFile;  // Logfile für Schnittstellen-Protokollierung initialisieren

        Anruf_beendet:=Kommando_Ausfuehren (KommandoObj);  // Kommando abarbeiten
      end
      else begin  // kein Kommando vorhanden oder Anruf (Verbindung) beendet; 14.03.2024, WW
        if Terminated then begin  // Thread wird von außen beendet
          if FLogFile <> nil then
            FLogFile.Write ('Thread terminated');  { Logfile-Protokollierung }
          Break;
        end else
          Sleep (1);  // Prozessorauslastung niedrig halten
      end;
    except
      on E: Exception do begin
        if FLogFile <> nil then
          FLogFile.Write ('!!! ' + ExceptionErrorMsgStr (E) + ' !!!', true, lt_Error);  { Logfile-Protokollierung }
        // Thread-Ausführung beenden
        // -> Weitermachen macht keinen Sinn, Exceptions können evtl. dauerhaft auftreten !
        Break;
      end;
    end;
  end;  { while true }

  while not Terminated do  // Warten auf das Beenden von außen; 14.03.2024, WW
    Sleep (1);  // Prozessorauslastung niedrig halten
end;

{--------------------------------------------------------------------------------}
function TREClntThread.Kommando_Ausfuehren (AKommandoObj: TREClntCmdObj): boolean;   
{--------------------------------------------------------------------------------}
{ Kommando ausführen;
  Übergabe: Kommando-Objekt
  Ergebnis: true, wenn ein laufender Anruf mit diesem Kommando abgeschlossen ist }
var
  Cmd_SetExtensionmode: TREClntCmd_SetExtensionmode;
  Cmd_SendCommand: TREClntCmd_SendCommand;
  ARueckgabe: TRueckgabe;
  iErgebnis: integer;
  bErgebnis: boolean;

begin
  Result:=false;
  if Assigned (AKommandoObj) then begin
    // Kommando ausführen...
    if AKommandoObj is TREClntCmdObj_SetExtensionmode then begin
      // Kommando 'SetExtensionmode'
      Cmd_SetExtensionmode:=TREClntCmdObj_SetExtensionmode (AKommandoObj).Cmd;
      with Cmd_SetExtensionmode do
        iErgebnis:=FDSfGSocketCommObj.SetExtensionmode (ExtMode);

      FKommandoListe.SetKommandoObj_SetExtensionmode_Completed (iErgebnis);
    end

    else if AKommandoObj is TREClntCmdObj_SendCommand then begin
      // Kommando 'SendCommand'
      Cmd_SendCommand:=TREClntCmdObj_SendCommand (AKommandoObj).Cmd;
      with Cmd_SendCommand do
        bErgebnis:=FDSfGSocketCommObj.SendCommand (Befehl, Endezeichen,
          EndezeichenAnzahl, Timeout, AnswerDest, ARueckgabe, FNoCarrier,
          AnzCharsToReceive);

      FKommandoListe.SetKommandoObj_SendCommand_Completed (ARueckgabe,
        FNoCarrier, bErgebnis);
    end

    else if AKommandoObj is TREClntCmdObj_Disconnect then begin
      // Kommando 'Disconnect'
      DisconnectClient;  // Verbindung zum Client beenden

      FKommandoListe.SetKommandoObj_Completed;
      Result:=true;  // Anruf fertig
    end;
  end;
end;

{--------------------------------------------------------------------------}
procedure TREClntThread.WriteLogFile_Kommando (AKommandoObj: TREClntCmdObj);
{--------------------------------------------------------------------------}
{ Aus Kommandoliste gelesenes Kommando im Logfile protokollieren;
  Übergabe: Kommando-Objekt }
var
  S: string;

begin
  if (FLogFile <> nil) AND (AKommandoObj <> nil) then begin
    if AKommandoObj is TREClntCmdObj_SendCommand then
      S:='SendCommand ' +
        SonderzeichenString (TREClntCmdObj_SendCommand (AKommandoObj).Cmd.Befehl)
    else if AKommandoObj is TREClntCmdObj_SetExtensionmode then
      S:='SetExtensionmode ' +
        IntToStr (TREClntCmdObj_SetExtensionmode (AKommandoObj).Cmd.ExtMode)
    else if AKommandoObj is TREClntCmdObj_Disconnect then
      S:='Disconnect'
    else
      S:='';

    if S <> '' then
      FLogFile.Write ('-> Kommando gelesen: ' + S);  { Logfile-Protokollierung }
  end;
end;

{--------------------------------------}
procedure TREClntThread.Init_ComLogFile;
{--------------------------------------}
{ Logfile für Schnittstellen-Protokollierung initialisieren;
  -> Erst aufrufen, wenn ein Kommando vorliegt (erst dann ist die tatsächlich
     verwendete COM-Nummer in der Kommandoliste verfügbar!) }
begin
  if FDebugCOMProtokoll AND not Assigned(FComLogFile) AND
     (FKommandoListe.COMNr <> 0) then begin
    FComLogFile:=TComLogFile.Create (PathServer.Pathname[WLogDir],
                                     FKommandoListe.COMNr, false, 'Ruf');
    FDSfGSocketCommObj.ComLogFile:=FComLogFile;

    // Rundpuffer auf COM-Logfile ausführen:
    if FDebugRundpufferMaxBytes > 0 then
        FComLogFile.DoRundpuffer (FDebugRundpufferMaxBytes);

    // Connect nachträglich loggen:
    FComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung ist geöffnet',
      '', FRemoteAddress, FRemotePort);
  end;
end;

{---------------------------------------}
procedure TREClntThread.DisconnectClient;
{---------------------------------------}
{ Verbindung zum Client beenden }
var
  bErg: boolean;
  S: string;

begin
  if FDSfGSocketCommObj.Connected then begin
    if FComLogFile <> nil then  { COM-Logfile-Protokollierung }
      FComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung schließen',
        '', FRemoteAddress, FRemotePort);

    // Socket-Verbindung beenden (warten auf Wirksamwerden des Verbindungs-
    // endes nicht erforderlich):
    bErg:=FDSfGSocketCommObj.Disconnect;

    if FLogFile <> nil then begin  { Logfile-Protokollierung }
      S:='Disconnect ' + FRemoteAddress + ':' + IntToStr (FRemotePort) + ' ';
      if bErg then begin
        S:=S + 'OK';
        FLogFile.Write (S);
      end
      else begin
        S:=S + 'Fehler';
        FLogFile.Write (S, true, lt_Error);
      end;
    end;
  end
  else begin  // Verbindung ist bereits geschlossen
    if FComLogFile <> nil then  { COM-Logfile-Protokollierung }
      FComLogFile.WriteTCPIP_Msg ('TCP/IP-Verbindung ist geschlossen',
        '', FRemoteAddress, FRemotePort);
  end;
end;


{ TDSfGREClntThreadCommObj }

{-------------------------------------------------------------------------------}
constructor TDSfGREClntThreadCommObj.Create (pKommandoListe: TREClntKommandoListe;
  ALogFile: TCustomLogFile);
{-------------------------------------------------------------------------------}
{ Objekt für DSfG-Kommunikation über TCP/IP-Rufentgegennahme-Thread erzeugen;
  Übergabe: Kommandoliste für Thread
            Zeiger auf Logfile }
begin
  inherited Create ('', nil);  // WorkPath und Com-Logfile nicht erforderlich

  FKommandoListe:=pKommandoListe;
  FLogFile:=ALogFile;
end;

{---------------------------------------------------------}
function TDSfGREClntThreadCommObj.WaitForKommandoErledigt (
  ATimeout: integer): TREClntCmdObj;
{---------------------------------------------------------}
{ Warten bis das in die Kommandoliste gestellte Kommando vom Thread bearbeitet
  wurde;
  Übergabe: Timeout für Warten auf bearbeitetes Kommando
  Ergebnis: Bearbeitetes Kommando-Objekt (nil bei Timeout) }
var
  KommandoObj: TREClntCmdObj;
  KommandoErledigtWaitTime: integer;
  LastTickCount: cardinal;
  ms: cardinal;

begin
  KommandoErledigtWaitTime:=0;
  LastTickCount:=GetTickCount;
  KommandoObj:=nil;
  while true do begin
    // Antwort auf Kommando aus Kommandoliste lesen
    KommandoObj:=FKommandoListe.GetKommandoObj (true); // abgearbeitetes Kommando-Objekt aus Liste lesen
    if Assigned (KommandoObj) then  // Antwort liegt vor
      Break;

    // Wenn Antwort noch nicht vorliegt:
    if KommandoErledigtWaitTime >= ATimeout then begin  // Wartezeit auf Antwort überschritten
      Break;  // Warten auf Antwort wird beendet wegen Timeout
    end
    else begin
      Sleep (1);                   { Prozessorauslastung niedrig halten }
      ms:=TicksElapsed (LastTickCount);
      inc (KommandoErledigtWaitTime, ms);
    end;
  end;  // while true
  Result:=KommandoObj;
end;

{--------------------------------------------------------------}
procedure TDSfGREClntThreadCommObj.WriteLogFile_TimeoutAntwort (
  sMethodName: string; iTimeout: integer);
{--------------------------------------------------------------}
{ Aufgetretenen Timeout beim Warten auf Kommando-Antwort im Logfile protokollieren;
  Übergabe: Name der Klassen-Methode
            Timeout in ms }
begin
  if FLogFile <> nil then                     { Logfile-Protokollierung }
    FLogFile.Write ('TDSfGREClntThreadCommObj.' + sMethodName +
      ': Timeout beim Warten auf Antwort (' + IntToStr (iTimeout) + ' ms)', true, lt_Error);
end;
    
{---------------------------------------------------------------------}
function TDSfGREClntThreadCommObj.SetExtensionmode (Value: byte): byte;
{---------------------------------------------------------------------}
{ DSfG-Kommunikations-Kommando 'SetExtensionmode' ausführen;
  Übergaben/Rückgaben/Ergebnis: Wie vererbte Methode }
var
  Kommando: TREClntCmd_SetExtensionmode;
  KommandoObj: TREClntCmdObj;
  TimeoutKommandoErledigt: integer;

begin
  // Kommandoparameter-Record belegen
  with Kommando do begin
    ExtMode:=Value;
  end;

  // Kommando in Kommandoliste für Thread stellen
  FKommandoListe.SetKommandoObj_SetExtensionmode (Kommando);
  try
    // Warten bis das Kommando vom Thread als 'erledigt' zurückgemeldet wird
    TimeoutKommandoErledigt:=C_TOKommandoErledigt;
    KommandoObj:=WaitForKommandoErledigt (TimeoutKommandoErledigt);

    // Auswertung der Kommando-Rückgaben
    if Assigned (KommandoObj) then begin  // Wenn Antwort vorliegt
      // Ergebnis belegen:
      Result:=TREClntCmdObj_SetExtensionmode (KommandoObj).Cmd.Ergebnis;
    end
    else begin
      WriteLogFile_TimeoutAntwort ('SetExtensionmode', TimeoutKommandoErledigt);
      Result:=0;     
    end;
  finally
    // Kommando aus Kommandoliste löschen, ist erfolgreich oder innerhalb Wartezeit
    // nicht erfolgreich abgearbeitet worden.
    FKommandoListe.DeleteKommandoObj;
  end;
end;

{------------------------------------------------------------------------------}
function TDSfGREClntThreadCommObj.SendCommand (ABefehl: string;
  AEndezeichen: TEndezeichenSet; AEndezeichenAnzahl: integer; ATimeout: integer;
  AAnswerDest: TAnswerDest; var Rueckgabe: TRueckgabe; var NoCarrier: boolean;
  AAnzCharsToReceive: cardinal = 0): boolean;
{------------------------------------------------------------------------------}
{ DSfG-Kommunikations-Kommando 'SendCommand' ausführen;
  Übergaben/Rückgaben/Ergebnis: Wie vererbte Methode }
var
  Kommando: TREClntCmd_SendCommand;
  KommandoObj: TREClntCmdObj;
  TimeoutKommandoErledigt: integer;

begin
  // Kommandoparameter-Record belegen
  with Kommando do begin
    // nur Übergabeparameter belegen (reicht)
    Befehl:=ABefehl;
    Endezeichen:=AEndezeichen;
    EndezeichenAnzahl:=AEndezeichenAnzahl;
    Timeout:=ATimeout;
    AnswerDest:=AAnswerDest;
    AnzCharsToReceive:=AAnzCharsToReceive;
  end;

  // Kommando in Kommandoliste für Thread stellen
  FKommandoListe.SetKommandoObj_SendCommand (Kommando);
  try
    // Warten bis das Kommando vom Thread als 'erledigt' zurückgemeldet wird
    TimeoutKommandoErledigt:=ATimeout + C_TOKommandoErledigt;
    KommandoObj:=WaitForKommandoErledigt (TimeoutKommandoErledigt);

    // Auswertung der Kommando-Rückgaben
    if Assigned (KommandoObj) then begin  // Wenn Antwort vorliegt
      // Rückgabeparameter und Ergebnis belegen:
      Rueckgabe:=TREClntCmdObj_SendCommand (KommandoObj).Cmd.Rueckgabe;
      NoCarrier:=TREClntCmdObj_SendCommand (KommandoObj).Cmd.NoCarrier;
      Result:=TREClntCmdObj_SendCommand (KommandoObj).Cmd.Ergebnis;
    end
    else begin
      WriteLogFile_TimeoutAntwort ('SendCommand', TimeoutKommandoErledigt);

      with Rueckgabe do begin
        Antwort:='';
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_TIMEOUT;
      end;
      // NoCarrier bleibt wie es war

      Result:=false;           
    end;
  finally
    // Kommando aus Kommandoliste löschen, ist erfolgreich oder innerhalb Wartezeit
    // nicht erfolgreich abgearbeitet worden.
    FKommandoListe.DeleteKommandoObj;
  end;
end;

{----------------------------------------------------}
function TDSfGREClntThreadCommObj.Disconnect: boolean;
{----------------------------------------------------}
{ Kommunikations-Kommando 'Disconnect' ausführen }
var
  Kommando: TREClntCmd_Disconnect;
  KommandoObj: TREClntCmdObj;
  TimeoutKommandoErledigt: integer;

begin
  // Kommando in Kommandoliste für Thread stellen
  FKommandoListe.SetKommandoObj_Disconnect (Kommando);
  try
    // Warten bis das Kommando vom Thread als 'erledigt' zurückgemeldet wird
    TimeoutKommandoErledigt:=C_TOKommandoErledigt;
    KommandoObj:=WaitForKommandoErledigt (TimeoutKommandoErledigt);

    // Auswertung der Kommando-Rückgaben
    if Assigned (KommandoObj) then begin  // Wenn Antwort vorliegt
      // Ergebnis belegen:
      Result:=TREClntCmdObj_Disconnect (KommandoObj).Cmd.Ergebnis;
    end
    else begin
      WriteLogFile_TimeoutAntwort ('Disconnect', TimeoutKommandoErledigt);
      Result:=false;                
    end;
  finally
    // Kommando aus Kommandoliste löschen, ist erfolgreich oder innerhalb Wartezeit
    // nicht erfolgreich abgearbeitet worden.
    FKommandoListe.DeleteKommandoObj;
  end;
end;

end.
