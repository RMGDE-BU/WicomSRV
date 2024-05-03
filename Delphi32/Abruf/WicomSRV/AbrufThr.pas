{******************************************************************************}
{* Unit: Thread für MRG/DSfG-Abruf                                            *}
{* 12.12.2002  WW                                                             *}
{* 05.11.2010  WN  manuelles Nachholen von zur Wiederholung anstehenden SMS   *}
{* 03.12.2013  WW  Auf geänderte Einstellungen der Schnittstellen-Konfigura-  *}
{*                 tion während Laufzeit prüfen: COM-Logfile, Abruf-Logfile,  *}
{*                 Rundpuffer auf Logfiles                                    *}
{* 03.06.2020  WW  Umstellung auf Indy-10 TCP-Connection (Vs. 10.0.52); GPRS- *}
{*                 Thread auskommentiert (nicht mehr unterstützt)             *}
{* 14.03.2024  WW  Bugfix Anruf nicht als beendet kennzeichnen bei nicht      *}
{*                 erfolgreicher Ausführung des Rufannahme-Kommandos          *}
{******************************************************************************}
unit AbrufThr;

interface

uses
  Windows, Forms, Classes, SysUtils, scktcomp, syncobjs,
  PathIni, AbrufCmdList, AbrufCmd, AbrufCmdExec, AbrufAnsw, ErrConst,
  AbrufCmdPlausib, SerialConst, AbrufConst, LogFile, SrvCfgIni, Lizenz32, WErrMsg,
  GPRSVerbList, GPRSTelegrList, {GPRSCmdExec,} AusgabeDirList, GPRS_Util, WSysCon,
  REClntVerbList, IdTCPConnection, T_Zeit, O_ResFilesList;

type
  { Server-Basis-Thread }
  TServerCustomThread = class(TThread)
  private
    FLizenzList: TLizenzList;  // 31.01.2023, WW
    FLogFile: TCustomLogFile;
    FSrvCfgIni: TSrvCfg32Ini;
    FResourceFilesList: TResourceFilesList;

    FCOMNr: integer;  { Schnittstellennummer, auf der der Abruf läuft }
    CallingFormHandle: HWND;
    KommandoListe: TAbrufKommandoListe;
    GPRSServerSocket: TServerSocket;
    GPRSVerbindungenListe: TGPRSVerbList;
    GPRSTelegrammListe: TGPRSTelegrList;
    FREClntVerbList: TREClntVerbList;
    FDoDebugServiceIO: boolean;

    ClientConnection_Ruf_SMS_GPRSEmpfang: TIdTCPConnection;
    procedure Create_Logfile_CmdExec; virtual; abstract;
    function Kommando_Ausfuehren (AKommandoObj: TKommandoObj): boolean;
    procedure SendAnswerToClient (AClientConnection: TIdTCPConnection;
                                  AAntwort: string);
    function KommandoExec (AKTyp: TKommandoTyp; AKommandoObj: TKommandoObj;
                           var Antwort: string): boolean; virtual; abstract;
  public
    constructor CreateIt (ACOMNr: integer;
                          AKommandoListe: TAbrufKommandoListe;
                          AGPRSServerSocket: TServerSocket;
                          AGPRSVerbindungenListe: TGPRSVerbList;
                          AGPRSTelegrammListe: TGPRSTelegrList;
                          AREClntVerbList: TREClntVerbList;
                          AResourceFilesList: TResourceFilesList;
                          ALizenzList: TLizenzList;
                          ACallingFormHandle: HWND;
                          ADoDebugServiceIO: boolean);
    destructor Destroy; override;
    procedure SendAnswerToRuf_SMS_GPRSEmpfangClient (AAntwort: string);
  end;


  { Server-Thread für MRG- und DSfG-Abrufe }
  TServerAbrufThread = class(TServerCustomThread)
  private
    FCOMNr_Kommando: integer;  { Schnittstellennummer aus Kommando }
    ACE: TAbrufCmdExec;
    Ruf_SMS_GPRS_PollingWaitTime: integer;
    GPRSData_AusgabeProc: TCBGPRSDataProc;
    FFileName_GetSavedSMS: String;  // 05.11.2010  WN
    procedure Create_Logfile_CmdExec; override;
    procedure SetLogfile (Value: boolean);
    procedure CheckForNewSrvCfgIniSettings;
    function KommandoExec (AKTyp: TKommandoTyp; AKommandoObj: TKommandoObj;
                           var Antwort: string): boolean; override;
    function Ruf_SMSEmpfang_Abfragen: byte;
    function SMS_manuell_Konvertieren: byte;  // 05.11.2010  WN/GD
  protected
    procedure Execute; override;
  public
    constructor CreateIt (ACOMNr: integer;
                          ACOMNr_Kommando: integer;
                          AKommandoListe: TAbrufKommandoListe;
                          AGPRSServerSocket: TServerSocket;
                          AGPRSVerbindungenListe: TGPRSVerbList;
                          AGPRSTelegrammListe: TGPRSTelegrList;
                          AREClntVerbList: TREClntVerbList;
                          AResourceFilesList: TResourceFilesList;
                          ALizenzList: TLizenzList;
                          ACallingFormHandle: HWND;
                          ADoDebugServiceIO: boolean;
                          AGPRSData_AusgabeProc: TCBGPRSDataProc);
    destructor Destroy; override;
  end;

 (* auskommentiert, GPRS nicht mehr unterstützt; 03.06.2020, WW
  { Server-Thread für empfangene GPRS-Daten (push) }
  TServerGPRSThread = class(TServerCustomThread)
  private
    GCE: TGPRSCmdExec;
    FAusgabeDirList: TAusgabeDirList;
    procedure Create_Logfile_CmdExec; override;
    function KommandoExec (AKTyp: TKommandoTyp; AKommandoObj: TKommandoObj;
                           var Antwort: string): boolean; override;
    procedure GPRSEmpfang_Abfragen;
  protected
    procedure Execute; override;
  public
    constructor CreateIt (ACOMNr: integer;
                          AKommandoListe: TAbrufKommandoListe;
                          AGPRSServerSocket: TServerSocket;
                          AGPRSVerbindungenListe: TGPRSVerbList;
                          AGPRSTelegrammListe: TGPRSTelegrList;
                          ACallingFormHandle: HWND;
                          ADoDebugServiceIO: boolean;
                          AAusgabeDirList: TAusgabeDirList);
    destructor Destroy; override;
  end;      *)


  { Struktur für Abruf-Steuerung }
  TAbrufControl = record
    Thread: TServerCustomThread;    // der Thread selbst
    ThreadRunning: boolean;  // Flag für externe Überwachung "Thread aktiv/inaktiv"
    KommandoListe: TAbrufKommandoListe;
    // für Multi-Client-Betrieb: 21.04.2015, WW
    ProzessID_Kommando: string;
    COMNr_Kommando: integer;  
  end;

  { Feld für Zugriff und Überwachung der Abruf-Threads und des GPRS-Threads
    -> Index = 0: GPRS-Thread
    -> Index > 0: COM-Nr.
    -> Index < 0: IP-Nr. }
  TAbrufControls = array [CMaxIPLinien*(-1)..MaxPossiblePort] of TAbrufControl;


procedure InitAbrufControls (var AbrufControls: TAbrufControls);
procedure CloseAbrufControls (var AbrufControls: TAbrufControls);
function GetAbrufControls_IPLinienNr_intern (var AbrufControls: TAbrufControls;
  iCOMNr: integer; sProzessID: string; var iIPLinienNr_intern: integer): boolean;

implementation

Const
  C_TimeoutKommando  = 120000;  { Wartezeit in ms auf neues Kommando in Kommandoliste }

  C_PollingWaitTime    =   1000;
  C_Ruf_PollingZyklus  =   1000;  { Ruf-Pollingzyklus in ms }
  C_SMS_PollingZyklus  =  10000;  { SMS-Pollingzyklus in ms }


{--------------------------------------------------------------}
procedure InitAbrufControls (var AbrufControls: TAbrufControls);
{--------------------------------------------------------------}
{ AbrufControls initialisieren }
var
  i: integer;
begin
  { Abruf-Kontrolle initialisieren: }
  for i:=Low (AbrufControls) to High (AbrufControls) do begin
    AbrufControls [i].Thread:=nil;
    AbrufControls [i].ThreadRunning:=false;

    AbrufControls [i].KommandoListe:=TAbrufKommandoListe.Create;  // Kommandoliste erzeugen
    AbrufControls [i].KommandoListe.Duplicates:=dupAccept;  // mehrfache, gleiche Kommandos erlaubt
    // 21.04.2015, WW
    AbrufControls [i].ProzessID_Kommando:='';
    AbrufControls [i].COMNr_Kommando:=i;  // Default: Schnittstellennummer aus Kommando gleich interner
  end;
end;

{---------------------------------------------------------------}
procedure CloseAbrufControls (var AbrufControls: TAbrufControls);
{---------------------------------------------------------------}
{ AbrufControls beenden und freigeben }
var
  i: integer;
begin
  { Beenden aller noch laufenden AbrufControls einleiten: }
  for i:=Low (AbrufControls) to High (AbrufControls) do begin
    if (AbrufControls [i].Thread <> nil) AND (AbrufControls [i].ThreadRunning = true) then begin
      AbrufControls [i].Thread.Resume;    // Thread fortsetzen, falls er unterbrochen ist (sonst reagiert er nicht)
      AbrufControls [i].Thread.Terminate; // Thread-Beenden einleiten
    end;
  end;
  { auf das Ende jedes AbrufControls warten: }
  for i:=Low (AbrufControls) to High (AbrufControls) do begin
    if (AbrufControls [i].Thread <> nil) AND (AbrufControls [i].ThreadRunning = true) then begin
      try
        AbrufControls [i].Thread.WaitFor;   // warten bis Thread beendet ist
      except
        // Ungültiges Handle unterdrücken; 23.01.2007 WW
      end;
    end;
  end;
  { Kommandolisten freigeben: }
  for i:=Low (AbrufControls) to High (AbrufControls) do
    AbrufControls [i].KommandoListe.Free;
end;

{-------------------------------------------------------------------------------}
function GetAbrufControls_IPLinienNr_intern (var AbrufControls: TAbrufControls;
  iCOMNr: integer; sProzessID: string; var iIPLinienNr_intern: integer): boolean;
{-------------------------------------------------------------------------------}
{ Ermittelt aus der Kombination IP-Linien-Nr./Prozess-ID (aus Kommando) die für
  den IP-Abruf tatsächlich verwendete IP-Linien-Nr. (Multi-Client-Fähigkeit:
  ermöglicht gleichzeitige Verwendung der gleichen IP-Linien-Nr. durch mehrere
  Clients)
  Übergaben: Zeiger auf AbrufControls
             Schnittstellennummer aus Kommando
             Prozess-ID aus Kommando
  Rückgabe: Ermittelte interne Schnittstellennummer
  Ergebnis: true, wenn interne Schnittstellennummer ermittelt werden konnte }
var
  i: integer;
begin
  Result:=false;
  iIPLinienNr_intern:=0;  // Default Rückgabe

  if (iCOMNr >= Low (AbrufControls)) AND (iCOMNr <= High (AbrufControls)) then begin
    // 1. Prüfung: Standardmäßig vorgesehene IP-Linien-Nr. frei ?
    if (AbrufControls [iCOMNr].ProzessID_Kommando = '') then begin
      iIPLinienNr_intern:=iCOMNr;
      // In AbrufControls vermerken:
      AbrufControls [iCOMNr].ProzessID_Kommando:=sProzessID;
      AbrufControls [iCOMNr].COMNr_Kommando:=iCOMNr;  // Zusatzinfo für spätere Verwendung im Programm
      Result:=true;  // Ist frei, wird verwendet
    end
    // 2. Prüfung: Standardmäßig vorgesehene IP-Linien-Nr. mit gleicher Prozess-ID belegt ?
    else if (AbrufControls [iCOMNr].ProzessID_Kommando = sProzessID) then begin
      iIPLinienNr_intern:=iCOMNr;
      Result:=true;  // Ist mit gleicher Prozess-ID belegt, wird verwendet
    end
    else begin
      // mit Prozess-ID belegte IP-Linien-Nr. suchen:
      for i:=-1 downto Low (AbrufControls) do begin  // nur die IP-Linien-Nummern
        if (AbrufControls [i].ProzessID_Kommando = sProzessID) then begin
          iIPLinienNr_intern:=i;
          Result:=true;  // gefunden, wird verwendet
          exit;  // fertig
        end;
      end;
      // wenn nicht gefunden, die erste freie interne IP-Linien-Nr. suchen:
      for i:=-1 downto Low (AbrufControls) do begin  // nur die IP-Linien-Nummern
        if (AbrufControls [i].ProzessID_Kommando = '') then begin
          iIPLinienNr_intern:=i;
          // In AbrufControls vermerken:
          AbrufControls [i].ProzessID_Kommando:=sProzessID;
          AbrufControls [i].COMNr_Kommando:=iCOMNr;  // Zusatzinfo für spätere Verwendung im Programm
          Result:=true;  // freie gefunden, wird verwendet
          Break;
        end;
      end;
    end;
  end;
end;


{ TServerCustomThread }

{------------------------------------------------------------------------------}
constructor TServerCustomThread.CreateIt (ACOMNr: integer;
                                          AKommandoListe: TAbrufKommandoListe;
                                          AGPRSServerSocket: TServerSocket;
                                          AGPRSVerbindungenListe: TGPRSVerbList;
                                          AGPRSTelegrammListe: TGPRSTelegrList;
                                          AREClntVerbList: TREClntVerbList;
                                          AResourceFilesList: TResourceFilesList;
                                          ALizenzList: TLizenzList;
                                          ACallingFormHandle: HWND;
                                          ADoDebugServiceIO: boolean);
{------------------------------------------------------------------------------}
begin
  // Für Thread-Sicherheit: Kopie der Lizenzliste anlegen; 31.01.2023, WW
  FLizenzList:=TLizenzList.Create;
  FLizenzList.LoadFromLizenzlist (ALizenzList);

  inherited Create (true); // Thread createn und gleich wieder anhalten
  FreeOnTerminate:=true;  // Thread soll sich beim Beenden selbst freigeben
  Priority:=tpNormal;     // Thread soll mit normaler Priorität ausgeführt werden

  FCOMNr:=ACOMNr;
  KommandoListe:=AKommandoListe;
  GPRSServerSocket:=AGPRSServerSocket;
  GPRSVerbindungenListe:=AGPRSVerbindungenListe;
  GPRSTelegrammListe:=AGPRSTelegrammListe;
  FREClntVerbList:=AREClntVerbList;
  FResourceFilesList:=AResourceFilesList;  // 06.08.2021, WW

  CallingFormHandle:=ACallingFormHandle;  { Handle des Formulars, welches den Thread aufruft }
  FDoDebugServiceIO:=ADoDebugServiceIO;

  ClientConnection_Ruf_SMS_GPRSEmpfang:=nil;
  FLogFile:=nil;

  FSrvCfgIni:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);

  { Logfile und Objekt zur Kommando-Ausführung initialisieren: }
  Create_Logfile_CmdExec;

  if FLogFile <> nil then
    FLogFile.Write ('Thread gestartet (Programmversion: ' + CVersion_WicomSrv + ')');  { Logfile-Protokollierung }
  Suspended:=false;         // Thread jetzt fortsetzen
end;

{-------------------------------------}
destructor TServerCustomThread.Destroy;
{-------------------------------------}
begin
  FSrvCfgIni.Free;
  if FLogFile <> nil then
    FLogFile.Write ('Thread beendet (Programmversion: ' + CVersion_WicomSrv + ')');    { Logfile-Protokollierung }
  FLogFile.Free;
  FLizenzList.Free;  // 31.01.2023, WW

  inherited destroy;
  { Botschaft an das aufrufende Fenster schicken: Thread ist beendet }
  PostMessage(CallingFormHandle, wm_AbrufThreadDone, FCOMNr, 0);
end;

{-------------------------------------------------------------------------------------}
function TServerCustomThread.Kommando_Ausfuehren (AKommandoObj: TKommandoObj): boolean;
{-------------------------------------------------------------------------------------}
{ Client-Kommando ausführen;
  Übergabe: Kommando-Objekt
  Ergebnis: true, wenn ein laufender Abruf mit diesem Kommando abgeschlossen ist }
var
  KTyp: TKommandoTyp;
  dummy: string;
  ExeName: string;
  Antwort: string;
  bAllow_UnbegrenzteLaufzeit: boolean;

begin
  Result:=false;
  { lizenzierte Programm-Laufzeit prüfen: }
  ExeName:=ExtractFileName(Application.ExeName);
{$IFDEF GAS-X}
  // für Gas-X-Version: unbegrenzte Laufzeit nicht gültig; 04.06.2014, WW
  bAllow_UnbegrenzteLaufzeit:=false;
{$ELSE}
  bAllow_UnbegrenzteLaufzeit:=true;
{$ENDIF}
  if not FLizenzList.GetExeLaufzeit (ExeName, bAllow_UnbegrenzteLaufzeit) then begin
    // Lizenz ist abgelaufen !
    Antwort:=GetClnt_XMLAntwort (SYS_LICENCEERROR, LICENCEERR_LAUFZEIT, AKommandoObj.Kommando,
                                 '', '', FDoDebugServiceIO, PathServer.PathName [WWorkDir]);
   { Logfile-Protokollierung von Fehlergruppe/-code: }
   if FLogFile <> nil then
     FLogFile.Write ('Ergebnis: ' + IntToStr (SYS_LICENCEERROR) + ', ' + IntToStr (LICENCEERR_LAUFZEIT));
  end
  else begin
    { Lizenz OK ! }
    KTyp:=GetAbrufKommandoTyp (AKommandoObj.Kommando, dummy);     // Kommandotyp aus Abruf-Kommando ermitteln
    if KTyp = kt_unbekannt then begin
      Antwort:=GetClnt_XMLAntwort (SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT, AKommandoObj.Kommando,
                                   '', '', FDoDebugServiceIO, PathServer.PathName [WWorkDir]);
      { Logfile-Protokollierung von Fehlergruppe/-code: }
      if FLogFile <> nil then
        FLogFile.Write ('Ergebnis: ' + IntToStr (SYS_ABRUFERROR) + ', ' + IntToStr (SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT));
    end
    else begin
      if not AbrufKommando_plausibel (KTyp, AKommandoObj.Kommando) then begin  { Abruf-Kommando auf Plausibilität prüfen }
        Antwort:=GetClnt_XMLAntwort (SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTPLAUSIBEL, AKommandoObj.Kommando,
                                     '', '', FDoDebugServiceIO, PathServer.PathName [WWorkDir]);
        { Logfile-Protokollierung von Fehlergruppe/-code: }
        if FLogFile <> nil then
          FLogFile.Write ('Ergebnis: ' + IntToStr (SYS_ABRUFERROR) + ', ' + IntToStr (SYSABRFERR_KOMMANDONICHTPLAUSIBEL));
      end else  { Kommando ist OK ! }
        Result:=KommandoExec (KTyp, AKommandoObj, Antwort);
    end;
  end;

  // Antwort auf Kommando an Client senden:
  SendAnswerToClient (AKommandoObj.ClientConnection, Antwort);
end;

{-------------------------------------------------------------------------------------}
procedure TServerCustomThread.SendAnswerToRuf_SMS_GPRSEmpfangClient (AAntwort: string);
{-------------------------------------------------------------------------------------}
{ Antwort an Rufentgegennahme-Client senden;
  Übergabe: Antwort-String }
begin
  SendAnswerToClient (ClientConnection_Ruf_SMS_GPRSEmpfang, AAntwort);
end;

{--------------------------------------------------------------------------------}
procedure TServerCustomThread.SendAnswerToClient (AClientConnection: TIdTCPConnection;
                                                  AAntwort: string);
{--------------------------------------------------------------------------------}
{ Antwort an Client senden;
  Übergabe: Zeiger auf Client-Connection, an den Antwort gesendet werden soll
            Antwort-String }
begin
  if length (AAntwort) > 0 then begin
    if Assigned (AClientConnection) then begin
      if Assigned (AClientConnection.IOHandler) then begin
        try
          if AClientConnection.Connected then begin
            { Logfile-Protokollierung: }
            if FLogFile <> nil then
              FLogFile.Write ('<- Antwort an Client >' + AClientConnection.Socket.Binding.PeerIP +
                              ':' + IntToStr (AClientConnection.Socket.Binding.PeerPort) + '<');
            AClientConnection.IOHandler.Write(AAntwort);
            if FLogFile <> nil then
              FLogFile.Write ('Antwort an Client gesendet');
          end
          else begin
            { Logfile-Protokollierung: }
            if FLogFile <> nil then
              FLogFile.Write ('Client ist nicht verbunden ' +
                              '(Request von >' + AClientConnection.Socket.Binding.PeerIP +
                              ':' + IntToStr (AClientConnection.Socket.Binding.PeerPort) + '<');
          end;
        except
          on E: Exception do begin
            { Logfile-Protokollierung bei Client-Socket-Fehler: }
            if FLogFile <> nil then
              FLogFile.Write ('Client-Connection ist nicht verfügbar: ' + E.Message);  // 03.06.2020, WW
          end;
        end;
      end
      else begin
        { Logfile-Protokollierung: }
        if FLogFile <> nil then
          FLogFile.Write ('Client-Connection-IOHandler ist NIL');  // 03.06.2020, WW
      end;
    end
    else begin
      { Logfile-Protokollierung: }
      if FLogFile <> nil then
        FLogFile.Write ('Client-Connection ist NIL');
    end;
  end;
end;


{ TServerAbrufThread }

{-------------------------------------------------------------------------------}
constructor TServerAbrufThread.CreateIt (ACOMNr: integer;
                                         ACOMNr_Kommando: integer;
                                         AKommandoListe: TAbrufKommandoListe;
                                         AGPRSServerSocket: TServerSocket;
                                         AGPRSVerbindungenListe: TGPRSVerbList;
                                         AGPRSTelegrammListe: TGPRSTelegrList;
                                         AREClntVerbList: TREClntVerbList;
                                         AResourceFilesList: TResourceFilesList;
                                         ALizenzList: TLizenzList;
                                         ACallingFormHandle: HWND;
                                         ADoDebugServiceIO: boolean;
                                         AGPRSData_AusgabeProc: TCBGPRSDataProc);
{-------------------------------------------------------------------------------}
begin
  ACE:=nil;
  Ruf_SMS_GPRS_PollingWaitTime:=0;
  GPRSData_AusgabeProc:=AGPRSData_AusgabeProc;
  FCOMNr_Kommando:=ACOMNr_Kommando;

  // 05.11.2010  WN
  // Dateiname für Nachholen gespeicherter SMS ermitteln, in NetProgDir suchen
  FFileName_GetSavedSMS := IncludeTrailingBackslash(PathServer.Pathname[WNetProgDir]);
  FFileName_GetSavedSMS := FFileName_GetSavedSMS + C_Filename_GetSavedSMS;
  FFileName_GetSavedSMS := ExpandUNCFileName(FFileName_GetSavedSMS);

  inherited CreateIt (ACOMNr, AKommandoListe, AGPRSServerSocket,
                      AGPRSVerbindungenListe, AGPRSTelegrammListe,
                      AREClntVerbList, AResourceFilesList, ALizenzList,
                      ACallingFormHandle, ADoDebugServiceIO);
end;

{------------------------------------}
destructor TServerAbrufThread.Destroy;
{------------------------------------}
begin
  ACE.Free;
  inherited destroy;
end;

{-----------------------------------}
procedure TServerAbrufThread.Execute;
{-----------------------------------}
{ Thread-Ausführung }
var
  Abruf_beendet: boolean;
  KommandoObj: TKommandoObj;
  Ruf_SMS_Erg: byte;
  LastTickCount: cardinal;
  ms: cardinal;
  PollingZyklus: integer;
  KommandoWaitTime: integer;
  VerbHaltenWaitTime: integer;
  // 05.11.2010  WN
  sKommando: String;
  bReGestartet: Boolean;

  {-----------------------}
  procedure ResetWaitTimes;
  {-----------------------}
  begin
    KommandoWaitTime:=0;
    VerbHaltenWaitTime:=0;
    LastTickCount:=GetTickCount;  // LastTickCount initialisieren; 02.05.2013, WW
  end;

begin
  Abruf_beendet:=false;
  ResetWaitTimes;  // 02.05.2013, WW
  while true do begin
    try
      KommandoObj:=KommandoListe.GetKommandoObj; // Kommando-Objekt aus Liste lesen
      if Assigned (KommandoObj) then begin  // wenn Kommando vorhanden
        try
          if FLogFile <> nil then
            FLogFile.Write ('-> Kommando gelesen: >'+ KommandoObj.Kommando + '<');  { Logfile-Protokollierung }

          Abruf_beendet:=Kommando_Ausfuehren (KommandoObj);    // Kommando abarbeiten
          ResetWaitTimes;  // 02.05.2013, WW
        finally
          KommandoObj.Free;
        end;
      end
      else begin   // kein Kommando vorhanden
        if Abruf_beendet then begin     // laufender Abruf ist beendet
          // nach beendetem Abruf:
          // -> Ruf/SMS-Empfang abfragen, wenn Rufentgegennahme aktiv
          // -> warten auf nächstes Kommando, wenn COM offen gehalten bleibt,
          //    ansonsten Thread beenden
          if ACE.Rufentgegennahme_SMSEmpfang_aktiviert then begin
            Sleep (C_PollingWaitTime);   // kurze Wartezeit von 1 s
            inc (Ruf_SMS_GPRS_PollingWaitTime, C_PollingWaitTime);

            if ACE.RufTyp = re_SMS then
              PollingZyklus:=C_SMS_PollingZyklus    // Zyklus für SMS-Polling
            else
              PollingZyklus:=C_Ruf_PollingZyklus;   // Zyklus für Rufentgegennahme FUP/Modem/IP

            if Ruf_SMS_GPRS_PollingWaitTime >= PollingZyklus then begin
              Ruf_SMS_GPRS_PollingWaitTime:=0;  // Polling-Zähler zurücksetzen

              // Auf Änderung der Schnittstellenkonfiguration prüfen; 03.12.2012, WW
              CheckForNewSrvCfgIniSettings;

              Ruf_SMS_Erg:=Ruf_SMSEmpfang_Abfragen;  // Ruf bzw. SMS von der Schnittstelle abfragen
              if Ruf_SMS_Erg = 1 then begin          // wenn ein MRG/DSfG-Ruf ansteht
                Abruf_beendet:=false;
                ResetWaitTimes;  // 20.12.2013, WW
                // Flag 'Abruf_beendet' wird auf false gesetzt, damit bei anstehendem Ruf die
                // Timeoutüberwachung für das nun vom Client zu sendende Rufannahme-Kommando
                // aktiv wird (nicht bei empfangener SMS und abgeschlossener Rufentgegennahme,
                // da hier keine Rufannahme folgt !)
              end;
            end;

            // 05.11.2010  WN
            if (ACE.RufTyp = re_SMS) and FileExists(FFileName_GetSavedSMS) then
            begin
              if FLogFile <> nil then
                FLogFile.Write ('SMS-Konvertierung manuell init');  { Logfile-Protokollierung }
              SMS_manuell_Konvertieren;

              DeleteFile(FFileName_GetSavedSMS);  // Datei wieder löschen
              ACE.SMS_WdhDatum := 0;              // Prüfdatum zurücksetzen
              sKommando := BuildRufSrvCmd(0, 0, C_CmdRufSMS_Wdh);
              ACE.Rufentgegennahme(sKommando, bReGestartet);
              if FLogFile <> nil then
                FLogFile.Write ('SMS-Konvertierung manuell gestartet');  { Logfile-Protokollierung }
            end;
          end
          else if ACE.Keep_COM_Open then
            Sleep (1)                    { Prozessorauslastung niedrig halten }
          else begin
            if FLogFile <> nil then
              FLogFile.Write ('Abruf beendet, keine weiteren Kommandos vorhanden');  { Logfile-Protokollierung }
            Break;
          end;
        end
        else begin                     // aktueller Abruf läuft noch
          // warten auf neues Kommando:
          if KommandoWaitTime >= C_TimeoutKommando then begin  // Wartezeit auf Kommando überschritten
            if FLogFile <> nil then
              FLogFile.Write ('Kommando-Timeout, keine weiteren Kommandos vorhanden');  { Logfile-Protokollierung }

            Abruf_beendet:=true;
            // Thread wird beendet wegen Kommando-Timeout, wenn keine Rufentgegennahme
            // aktiviert ist und die Schnittstelle nicht offen gehalten bleiben muß:
            if not ACE.Rufentgegennahme_SMSEmpfang_aktiviert AND not ACE.Keep_COM_Open then
              Break;
          end
          else begin
            Sleep (1);                   { Prozessorauslastung niedrig halten }

            { Tatsächlich verstrichene Zeit messen für Timeoutüberwachung: }
            ms:=F_GetTickCountDiff(LastTickCount);
            inc (KommandoWaitTime, ms);

            if ACE.VerbindungHalten_Cfg > 0 then begin  // Verbindung soll gehalten werden; 02.05.2013, WW
              inc (VerbHaltenWaitTime, ms);
              if VerbHaltenWaitTime >= ACE.VerbindungHalten_Cfg then begin
                ACE.VerbHalten;  // Aktion zum Halten der Verbindung zur Station ausführen
                VerbHaltenWaitTime:=0;
              end;
            end;
          end;
        end;
      end;  { if Assigned (KommandoObj) ... else }

      if Terminated AND Abruf_beendet then begin
        if FLogFile <> nil then
          FLogFile.Write ('Thread terminated, Abruf beendet');  { Logfile-Protokollierung }
        Break;  // Thread von außen erst beenden, wenn kein Abruf mehr läuft
      end;
    except
      on E: Exception do begin
        if FLogFile <> nil then
          FLogFile.Write ('!!! ' + ExceptionErrorMsgStr (E) + ' !!!', true, lt_Error);  { Logfile-Protokollierung }
      end;
    end;
  end;  { while true }
end;

{--------------------------------------------------}
procedure TServerAbrufThread.Create_Logfile_CmdExec;
{--------------------------------------------------}
var
  iDebRundpufferMaxBytes: integer;

begin
  { Logfile für Protokollierung des Abrufablaufs initialisieren, wenn lt.
    INI-Konfiguration eingestellt: }
  if FSrvCfgIni.DebAbrufProtokoll then
    SetLogfile (true);

  { Objekt zur Abrufkommando-Ausführung initialisieren: }
  if FSrvCfgIni.DebRundpuffer then  // 03.12.2013, WW
    iDebRundpufferMaxBytes:=FSrvCfgIni.DebRundpuffergroesse_kB * 1000
  else
    iDebRundpufferMaxBytes:=0;  // kein Rundpuffer

  ACE:=TAbrufCmdExec.Create (FCOMNr, FCOMNr_Kommando,
                             GPRSServerSocket, GPRSVerbindungenListe,
                             GPRSTelegrammListe, FREClntVerbList,
                             FResourceFilesList,
                             FLogFile,
                             FSrvCfgIni.DebCOMProtokoll,
                             FDoDebugServiceIO,
                             FSrvCfgIni.DebRohdaten,
                             iDebRundpufferMaxBytes,
                             FLizenzList,
                             GPRSData_AusgabeProc);
  ACE.CBSendAnswerToRuf_SMSEmpfangClient:=SendAnswerToRuf_SMS_GPRSEmpfangClient;
end;


{-------------------------------------------------------}
procedure TServerAbrufThread.SetLogfile (Value: boolean);
{-------------------------------------------------------}
{ Logfile für Protokollierung des Abrufablaufs aktivieren/deaktivieren;
  Übergabe: true = aktivieren }
var
  LogFilename: string;

begin
  if Value then begin  // Logfile ein
    if not Assigned (FLogFile) then begin
      if FCOMNr > 0 then  { COM-Port }
        LogFilename:='ATHR_' + Format ('%.3d', [FCOMNr])
      else begin              { IP-Abrufnummer }
        if FCOMNr = FCOMNr_Kommando then
          LogFilename:='ATHR_IP_' + Format ('%.3d', [Abs (FCOMNr)])
        else
          // Logfilename mit IP-Linien-Nr. aus Kommando und intern verwendeter:
          // ATHR_IP_<IP-Linie Kommando>_<IP-Linie intern>.log
          LogFilename:='ATHR_IP_' + Format ('%.3d', [Abs (FCOMNr_Kommando)]) +
                       '_' + Format ('%.3d', [Abs (FCOMNr)]);  // 21.04.2015, WW
      end;
      FLogFile:=TCustomLogFile.Create (PathServer.Pathname[WLogDir], LogFilename, false);
    end;
  end
  else begin  // Logfile aus
    if Assigned (FLogFile) then
      FreeAndNil (FLogFile);
  end;
  if Assigned (ACE) then
    ACE.AbrufLogFile:=FLogFile;
end;

{--------------------------------------------------------}
procedure TServerAbrufThread.CheckForNewSrvCfgIniSettings;
{--------------------------------------------------------}
{ Auf Änderung der INI-Datei der Schnittstellenkonfiguration prüfen. Wenn
  geändert, aktuelle Schnittstellen-Einstellungen übernehmen für:
    - COM-Logdatei ein/aus
    - Abruf-Logfile ein/aus
    - Rundpuffer für Logdateien ein/aus
    - Rundpuffergröße
    - ServiceIO ein/aus
    - Rohdaten ein/aus }
begin
  if FSrvCfgIni.FileDateTimeChanged then begin  // Datei-Datum hat sich geändert
    if FLogFile <> nil then
      FLogFile.Write (CMsgSrvCfgIniChanged_Reread);  { Logfile-Protokollierung; 28.09.2020, WW }   

    ACE.DebugCOMProtokoll:=FSrvCfgIni.DebCOMProtokoll;
    SetLogfile (FSrvCfgIni.DebAbrufProtokoll);
    if FSrvCfgIni.DebRundpuffer then  // 03.12.2013, WW
      ACE.DebugRundpufferMaxBytes:=FSrvCfgIni.DebRundpuffergroesse_kB * 1000
    else
      ACE.DebugRundpufferMaxBytes:=0;  // kein Rundpuffer

    FDoDebugServiceIO:=FSrvCfgIni.DebServiceIO;  // 28.09.2020, WW
    ACE.DebugServiceIO:=FDoDebugServiceIO;

    ACE.RohdatenLoeschen:=not FSrvCfgIni.DebRohdaten;  // 28.09.2020, WW
  end;
end;                                                    

{----------------------------------------------------------------------}
function TServerAbrufThread.KommandoExec (AKTyp: TKommandoTyp;
                                          AKommandoObj: TKommandoObj;
                                          var Antwort: string): boolean;
{----------------------------------------------------------------------}
var
  Verbindung_OK: boolean;
  RE_gestartet: boolean;

begin
  Result:=false;
  case AKTyp of
    kt_VerbAufbau:
      begin
        // Auf Änderung der Schnittstellenkonfiguration prüfen; 03.12.2012, WW
        CheckForNewSrvCfgIniSettings;

        if FLogFile <> nil then
          FLogFile.Write ('v-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.VerbAufbau (AKommandoObj.Kommando, Verbindung_OK);  // Verbindungsaufbau-Kommando abarbeiten
        Result:=not Verbindung_OK;    // Abruf als beendet kennzeichnen, wenn Verbindungsaufbau nicht erfolgreich war
      end;

    kt_VerbAbbau:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('e-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.VerbAbbau (AKommandoObj.Kommando);   // Verbindungsabbau-Kommando abarbeiten
        Result:=true;   // Abruf als beendet kennzeichnen
      end;

    kt_MessAbruf:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('E-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.MessAbruf (AKommandoObj.Kommando,
                                AKommandoObj.StaKanalKonvDataList);  // Messwertabruf-Kommando abarbeiten
      end;

    kt_MeldAbruf:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('M-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.MeldAbruf (AKommandoObj.Kommando);  // Meldungsabruf-Kommando abarbeiten
      end;

    kt_ParaAbruf:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('B-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.ParaAbruf (AKommandoObj.Kommando);  // Parameterabruf-Kommando abarbeiten
      end;

    kt_PruefAbruf:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('X-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.PruefAbruf (AKommandoObj.Kommando);  // Prüfsatzabruf-Kommando abarbeiten
      end;

    kt_ZeitSync:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('Z-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.ZeitSynchronisation (AKommandoObj.Kommando);  // Zeitsynchronisations-Kommando abarbeiten
      end;

    kt_DSfGUmschalt:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('}-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.DSfGUmschaltung (AKommandoObj.Kommando);  // DSfG-Slave-Umschaltung-Kommando abarbeiten
      end;

    kt_DSfGBusanalyse:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('I-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.DSfGBusAnalyseAbruf (AKommandoObj.Kommando);  // DSfG-Busanalyse-Kommando abarbeiten
      end;

    kt_RpReset:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('U-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.RundpufferReset (AKommandoObj.Kommando);  // Rundpufferreset-Kommando abarbeiten
      end;

    kt_Parametrieren:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('C-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.Parametrieren (AKommandoObj.Kommando);  // Parametrier-Kommando abarbeiten
      end;

    kt_Transparent:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('T-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.Transparentbefehl (AKommandoObj.Kommando);  // Transparent-Kommando abarbeiten
      end;

    kt_Ruf:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('R-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.Rufentgegennahme (AKommandoObj.Kommando, RE_gestartet);  // Rufentgegennahme-Kommando abarbeiten
        { wenn die Rufentgegennahme aktiviert wurde:
          Client-Socket merken für spätere Rufalarmierung des Client }
        if RE_gestartet then begin
          ClientConnection_Ruf_SMS_GPRSEmpfang:=AKommandoObj.ClientConnection;
          Ruf_SMS_GPRS_PollingWaitTime:=0;  // Polling-Zähler zurücksetzen
        end;
        Result:=true;   // "Abruf" als beendet kennzeichnen (Kommando ist ja ausgeführt)
      end;

    kt_Rufannahme:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('a-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.Rufannahme (AKommandoObj.Kommando);  // Rufannahme-Kommando abarbeiten
        // Anruf nicht als beendet kennzeichnen, wenn Rufannahme nicht erfolgeich
        // war! Es muß auf das Verbindungsabbau-Kommando des Client gewartet werden
        // bevor der nächste anstehende Anruf bearbeitet wird; 14.03.2024, WW
      end;

    kt_Rufliste:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('#-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.Rufliste (AKommandoObj.Kommando);  // Ruflistenabfrage-Kommando abarbeiten
      end;

    kt_SlaveRufQuitt:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('Q-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.SlaveRufQuittierung (AKommandoObj.Kommando);  // Slave-Rufquittierung-Kommando abarbeiten
      end;

    kt_Rueckruf:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('A-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.Rueckruf (AKommandoObj.Kommando);  // Rückruf-Kommando abarbeiten
      end;

    kt_ZeitAbruf:  // 17.08.2010
      begin
        if FLogFile <> nil then
          FLogFile.Write ('t-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=ACE.ZeitAbruf (AKommandoObj.Kommando);  // Rückruf-Kommando abarbeiten
      end;
  else
    Antwort:=GetClnt_XMLAntwort (SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT,
                                 AKommandoObj.Kommando,
                                 '', '', FDoDebugServiceIO, PathServer.PathName [WWorkDir])
  end;  { case }
end;

{--------------------------------------------------------}
function TServerAbrufThread.Ruf_SMSEmpfang_Abfragen: byte;
{--------------------------------------------------------}
{ auf eingegangenen Ruf/SMS-Empfang prüfen;
  Ergebnis: true, wenn Ruf ansteht bzw. SMS empfangen wurde }
var
  Ruf_SMS_steht_an: byte;
  Antwort: string;

begin
  Antwort:=ACE.Ruf_SMS_Abfragen (Ruf_SMS_steht_an);
  if length (Antwort) > 0 then begin  // bei anstehendem Ruf/SMS oder Störung
    // Rufpolling-Antwort an Client senden, welcher das Ruf/SMS-Polling-Start-Kommando
    // gesendet hat:
    SendAnswerToRuf_SMS_GPRSEmpfangClient (Antwort);
  end;
  Result:=Ruf_SMS_steht_an;
end;

{ 05.11.2010  WN/GD                                      }
{--------------------------------------------------------}
function TServerAbrufThread.SMS_manuell_Konvertieren: byte;
{--------------------------------------------------------}
{ auf eingegangenen Ruf/SMS-Empfang prüfen;
  Ergebnis: true, wenn Ruf ansteht bzw. SMS empfangen wurde }
var
  Ruf_SMS_steht_an: byte;
  Antwort: string;

begin
  ACE.SendeSMSFiles := True;
  ACE.LetztSMSFilename := '';
  ACE.SMS_WdhDatum := 0;
  Antwort:=ACE.Ruf_SMS_Abfragen (Ruf_SMS_steht_an);
  if length (Antwort) > 0 then begin  // bei anstehendem Ruf/SMS oder Störung
    // Rufpolling-Antwort an Client senden, welcher das Ruf/SMS-Polling-Start-Kommando
    // gesendet hat:
    SendAnswerToRuf_SMS_GPRSEmpfangClient (Antwort);
  end;
  Result:=Ruf_SMS_steht_an;
end;

(* auskommentiert, GPRS nicht mehr unterstützt; 03.06.2020, WW
{ TServerGPRSThread }

{----------------------------------------------------------------------------}
constructor TServerGPRSThread.CreateIt (ACOMNr: integer;
                                        AKommandoListe: TAbrufKommandoListe;
                                        AGPRSServerSocket: TServerSocket;
                                        AGPRSVerbindungenListe: TGPRSVerbList;
                                        AGPRSTelegrammListe: TGPRSTelegrList;
                                        ACallingFormHandle: HWND;
                                        ADoDebugServiceIO: boolean;
                                        AAusgabeDirList: TAusgabeDirList);
{----------------------------------------------------------------------------}
begin
  FAusgabeDirList:=AAusgabeDirList;
  inherited CreateIt (ACOMNr, AKommandoListe, AGPRSServerSocket,
                      AGPRSVerbindungenListe, AGPRSTelegrammListe, nil,
                      ACallingFormHandle, ADoDebugServiceIO);
end;

{-----------------------------------}
destructor TServerGPRSThread.Destroy;
{-----------------------------------}
begin
  GCE.Free;
  inherited destroy;
end;

{----------------------------------}
procedure TServerGPRSThread.Execute;
{----------------------------------}
{ Thread-Ausführung }
var
  KommandoObj: TKommandoObj;

begin
  while true do begin
    try
      KommandoObj:=KommandoListe.GetKommandoObj; // Kommando-Objekt aus Liste lesen
      if Assigned (KommandoObj) then begin  // wenn Kommando vorhanden
        try
          if FLogFile <> nil then
            FLogFile.Write ('-> Kommando gelesen: >'+ KommandoObj.Kommando + '<');  { Logfile-Protokollierung }

          Kommando_Ausfuehren (KommandoObj);    // Kommando abarbeiten
        finally
          KommandoObj.Free;
        end;
      end
      else begin   // kein Kommando vorhanden
        Sleep (1);                   { Prozessorauslastung niedrig halten }
        GPRSEmpfang_Abfragen;  // GPRS-Telegramme verarbeiten

        if Terminated then begin
          if FLogFile <> nil then
            FLogFile.Write ('GPRS-Thread terminated');  { Logfile-Protokollierung }
          Break;  // Thread von außen erst beenden, wenn kein Abruf mehr läuft
        end;
      end;  { if Assigned (KommandoObj) ... else }
    except
      on E: Exception do begin
        if FLogFile <> nil then
          FLogFile.Write ('!!! ' + ExceptionErrorMsgStr (E) + ' !!!', true, lt_Error);  { Logfile-Protokollierung }
      end;
    end;
  end;  { while true }
end;

{-------------------------------------------------}
procedure TServerGPRSThread.Create_Logfile_CmdExec;
{-------------------------------------------------}
var
  LogFilename: string;

begin
  { Logfile für Protokollierung des GPRS-Ablaufs initialisieren, wenn lt. INI-Konfiguration
    eingestellt: }
  if FSrvCfgIni.DebAbrufProtokoll then begin
    LogFilename:='GTHR_' + Format ('%.3d', [FCOMNr]);
    FLogFile:=TCustomLogFile.Create (PathServer.Pathname[WLogDir], LogFilename, false);
  end else
    FLogFile:=nil;

  { Objekt zur GPRS-Kommando-Ausführung initialisieren: }
  GCE:=TGPRSCmdExec.Create (GPRSServerSocket, GPRSVerbindungenListe,
                            GPRSTelegrammListe, FAusgabeDirList, FLogFile,
                            FDoDebugServiceIO,
                            FSrvCfgIni.DebRohdaten,
                            FWLizenz32,
                            CallingFormHandle);
end;

{---------------------------------------------------------------------}
function TServerGPRSThread.KommandoExec (AKTyp: TKommandoTyp;
                                         AKommandoObj: TKommandoObj;
                                         var Antwort: string): boolean;
{---------------------------------------------------------------------}
var
  GPRS_gestartet: boolean;

begin
  Result:=false;
  case AKTyp of
    kt_Ruf:
      begin
        if FLogFile <> nil then
          FLogFile.Write ('R-Kommando ausführen');  { Logfile-Protokollierung }
        Antwort:=GCE.Rufentgegennahme (AKommandoObj.Kommando, GPRS_gestartet);  // Rufentgegennahme-Kommando abarbeiten
        { wenn der GPRS-Empfang aktiviert wurde:
          Client-Socket merken für das Senden der GPRS-Telegrammdaten an Client }
        if GPRS_gestartet then begin
          ClientConnection_Ruf_SMS_GPRSEmpfang:=AKommandoObj.ClientConnection;
        end;
        Result:=true;   // "Abruf" als beendet kennzeichnen (Kommando ist ja ausgeführt)
      end;
  else
    Antwort:=GetClnt_XMLAntwort (SYS_ABRUFERROR, SYSABRFERR_KOMMANDONICHTIMPLEMENTIERT,
                                 AKommandoObj.Kommando,
                                 '', '', doDebugServiceIO, PathServer.PathName [WWorkDir])
  end;  { case }
end;

{-----------------------------------------------}
procedure TServerGPRSThread.GPRSEmpfang_Abfragen;
{-----------------------------------------------}
{ auf eingegangene GPRS-Telegramme prüfen, GPRS-Zeitsynchronisations-
  Startzeitpunkt prüfen }
var
  Antwort: string;
  bSendWieserMsg_NKD: boolean;

begin
  { eingegangene GPRS-Telegramme prüfen/verarbeiten: }
  Antwort:=GCE.GPRS_Abfragen (bSendWieserMsg_NKD);
  if bSendWieserMsg_NKD then
    { Botschaft an das aufrufende Fenster schicken: Wieser-Benachrichtigung
      für neue Kurzzeitwerte verschicken }
     PostMessage(CallingFormHandle, WM_SendWieserMsg_NKD, 0, 0);

  if length (Antwort) > 0 then begin  // bei eingegangenem GPRS-Telegramm
    // Antwort an Client senden, welcher das Ruf/SMS-Polling-Start-Kommando
    // gesendet hat:
    SendAnswerToRuf_SMS_GPRSEmpfangClient (Antwort);
  end;

  { GPRS-Zeitsynchronisations-Startzeitpunkt prüfen/Zeitsynchronisation starten: }
  GCE.GPRS_ZeitSync;
end;
*)
end.
