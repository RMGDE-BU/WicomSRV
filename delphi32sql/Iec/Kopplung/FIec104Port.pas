{******************************************************************************}
{* Unit: Monitor mit TCP/IP-Schnittstellenroutinen für IEC-Kopplung (32-Bit), *}
{*       IEC 870-5-104                                                        *}
{* 31.05.2007  WW                                                             *}
{******************************************************************************}
unit FIec104Port;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, Buttons, ScktComp, DateUtils,
  TCPIP_Iec, IecLog, IecConst, IecIniFile, FIecCustomPort, FIecCustomTelegr,
  FIec104Telegr, VerifyFlags, LogFile, O_IecRemoteMonitor;

type
  { Formular mit TCP/IP-Schnittstellenhandling und Monitor }

  TFormPortIec104 = class(TFormCustomPortIec)
    TimerTimeout_t0: TTimer;
    TimerTimeout_t1: TTimer;
    TimerTimeout_t2: TTimer;
    TimerTimeout_t3: TTimer;
    lTimeout_t0: TLabel;
    eTimeout_t0: TEdit;
    TimerRedundanzVerbUnterbrechung: TTimer;
    procedure TimerTimeout_t0Timer(Sender: TObject);
    procedure TimerTimeout_t1Timer(Sender: TObject);
    procedure TimerTimeout_t2Timer(Sender: TObject);
    procedure TimerTimeout_t3Timer(Sender: TObject);
    procedure TimerRedundanzVerbUnterbrechungTimer(Sender: TObject);
  private
    { Private-Deklarationen }
    SocketIec: TCustomSocketIec;
    Port: integer;
    IPAdresse: string;
    FTimeout_t1: TCBStringProc;
    FTimeout_t2: TCBStringProc;
    FTimeout_t3: TCBStringProc;
    TO_Count_t0: integer;
    TO_Count_t1: integer;
    TO_Count_t2: integer;
    TO_Count_t3: integer;
    Timeout_t0: integer;
    Timeout_t1: integer;
    Timeout_t2: integer;
    Timeout_t3: integer;
    Timeout_RedundanzVerbUnterbrechung: integer;
    procedure GetIniPort (bCheckIniChanged: boolean);
    function CheckIniPort: boolean;
    procedure ShowServerPortStatus (bActive: boolean; S: string);
    procedure ShowClientConnection (Status: TStatusClientConnection; S: string);
    function StartTimer_t0: boolean;
    procedure StopTimer_t0;
    procedure RestartTimer_t1;
    procedure StartTimer_t3;
    procedure ResetTimer_t3;
    procedure StopTimer_t3;
    procedure Fill_RemoteMonitorControlList;
  protected
    procedure UpdateMonitor (S: string); override;
    procedure Port_Action; override;
    function GetConnected: boolean; override;
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; ANetProgDir: string;
                        AFunktion: TIEC870_Funktion; AIecLogfile: TIecLogFile;
                        ARemoteMonitorObj: TRemoteMonitorObj;
                        var bOK: boolean); reintroduce;
    destructor Destroy; override;
    procedure Set_IecLogfile (AIecLogfile: TIecLogFile); override;
    procedure ActiveCloseConnection;
    procedure StopTimer_t1;
    procedure StartTimer_t2;
    procedure StopTimer_t2;
    property CBTimeout_t1: TCBStringProc read FTimeout_t1 write FTimeout_t1;
    property CBTimeout_t2: TCBStringProc read FTimeout_t2 write FTimeout_t2;
    property CBTimeout_t3: TCBStringProc read FTimeout_t3 write FTimeout_t3;
  end;

var
  FormPortIec104: TFormPortIec104;

implementation

{$R *.dfm}

{--------------------------------------------------------------------------}
constructor TFormPortIec104.Create (AOwner: TComponent; ANetProgDir: string;
                                    AFunktion: TIEC870_Funktion;
                                    AIecLogfile: TIecLogFile;
                                    ARemoteMonitorObj: TRemoteMonitorObj;
                                    var bOK: boolean);
{--------------------------------------------------------------------------}
var
  ErrMsg: string;

begin
  bOK:=true;  // Default: Create OK
  inherited Create (AOwner, ANetProgDir, AIecLogfile, ARemoteMonitorObj);

  Fill_RemoteMonitorControlList;  // 10.03.2015, WW

  TO_Count_t0:=0;
  TO_Count_t1:=0;
  TO_Count_t2:=0;
  TO_Count_t3:=0;

  GetIniPort (false);  { liest Ini-Einstellungen für Socket }

  if Assigned (FormTelegrIec104) then begin
    // Timeout-Ausgaben t1, t2, t3 im 104-Telegramm-Formular:
    CBTimeout_t1:=FormTelegrIec104.Update_Timeout_t1Anzeige;
    CBTimeout_t2:=FormTelegrIec104.Update_Timeout_t2Anzeige;
    CBTimeout_t3:=FormTelegrIec104.Update_Timeout_t3Anzeige;
  end;

  if AFunktion = fkt_iec870_Station then begin  // wir sind Station (Server)
    // Server-Socket erzeugen:
    SocketIec:=TServerSocketIec.Create (IecLogfile);
    SocketIec.CBMonitor:=ShowText;
    SocketIec.CBClientConnection:=ShowClientConnection;
    TServerSocketIec (SocketIec).CBServerPortStatus:=ShowServerPortStatus;
    // Server-Socket öffnen:
    if not TServerSocketIec (SocketIec).OpenServer (Port, ErrMsg) then begin
      if Assigned (IECLogFile) then
        IECLogFile.Write (ErrMsg, '', '', lt_Error);
      MessageBoxWhenExe (0, pchar(ErrMsg), pchar (Application.Title), MB_ICONERROR + MB_OK);
      bOK:=false;  // Create Fehler
      exit;
    end;
  end
  else if AFunktion = fkt_iec870_Zentrale then begin  // wir sind Zentrale (Client)
    // Anzeigeelemente für Timeout t0 sichtbar machen:
    lTimeout_t0.Visible:=true;
    eTimeout_t0.Visible:=true;
    // Client-Socket erzeugen:
    SocketIec:=TClientSocketIec.Create (IecLogfile);
    SocketIec.CBMonitor:=ShowText;
    SocketIec.CBClientConnection:=ShowClientConnection;
  end;

  { t3-Timeout-Anzeige: }
  if Assigned (CBTimeout_t3) then
    CBTimeout_t3 (IntToStr(Timeout_t3 DIV 1000) + ' s');

  // Default: Timer-Überwachung für Redundanz-Verbindungsunterbrechung aus
  TimerRedundanzVerbUnterbrechung.Enabled:=false;
  TimerRedundanzVerbUnterbrechung.Interval:=Timeout_RedundanzVerbUnterbrechung;

  // INI-Einstellungen für 104-Port prüfen:
  if not CheckIniPort then begin  // 10.03.2015, WW
    bOK:=false;  // Create Fehler
    exit;
  end;

  TimerUebertragung.Enabled:=true;
end;

{---------------------------------}
destructor TFormPortIec104.Destroy;
{---------------------------------}
var
  ErrMsg: string;
begin
  if (SocketIec is TServerSocketIec) then
    TServerSocketIec (SocketIec).CloseServer  // Server-Socket schließen
  else if (SocketIec is TClientSocketIec) then
    TClientSocketIec (SocketIec).CloseClient (ErrMsg);  // bestehende Client-Verbindung zur Station (Server) beenden

  SocketIec.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------}
procedure TFormPortIec104.Set_IecLogfile (AIecLogfile: TIecLogFile);  // 10.03.2015, WW
{------------------------------------------------------------------}
begin
  inherited Set_IecLogfile (AIecLogfile);

  if Assigned (SocketIec) then
    SocketIec.IecLogfile:=AIecLogfile;
end;                                  

{---------------------------------------------------------------}
Procedure TFormPortIec104.GetIniPort (bCheckIniChanged: boolean);
{---------------------------------------------------------------}
{ INI-Konfiguration für 104-Port lesen;
  Übergabe: Flag 'bCheckIniChanged': wenn true, wird nur bei geänderter INI-
              Konfiguration gelesen }
var
  Iec32Ini: TIec32Ini;
begin
  Iec32Ini:=TIec32Ini.Create (FNetProgDir);
  try
    // INI-Einstellungen, welche nur zum Programmstart gelesen werden (können):
    if (not bCheckIniChanged) then begin
      Port:=Iec32Ini.Port;
      IPAdresse:=Iec32Ini.IPAdresse;
      Timeout_t0:=Iec32Ini.TO_t0;
      Timeout_t1:=Iec32Ini.TO_t1;
      Timeout_t2:=Iec32Ini.TO_t2;
      Timeout_t3:=Iec32Ini.TO_t3;
    end;

    // INI-Einstellungen, welche zur Laufzeit neu gelesen werden (können):
    if (not bCheckIniChanged) OR
       (bCheckIniChanged AND
        not (SameDateTime (Iec32Ini.FileDate, FIniDate))) then begin  
      FIniDate:=Iec32Ini.FileDate;  // Zeitstempel der INI-Datei merken; 10.03.2015, WW

      Timeout_RedundanzVerbUnterbrechung:=Iec32Ini.RedundanzTO_VerbUnterbrechung;
    end;
  finally
    Iec32Ini.Free;
  end;
end;

{---------------------------------------------}
function TFormPortIec104.CheckIniPort: boolean;
{---------------------------------------------}
var
  S: string;

begin
  Result:=true;
  { Prüfung Timeout-Einstellungen t1, t2, t3: }
  if Timeout_t2 >= Timeout_t1 then begin
    S:='Prüfung der INI-Einstellungen für Timeout t1 und t2 (IEC 870-5-104):' + #13 +
       '[104] t1 = ' + IntToStr (Timeout_t1) + ', t2 = ' + IntToStr (Timeout_t2) + #13#13 +
       't2 muß kleiner t1 sein !' + #13 +
       'Standardwerte in ms: t1 = 15000, t2 = 10000';
    WriteProgramLog (S, lt_Error);
    MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=false;
  end;

  if (Timeout_t3 > 0) AND (Timeout_t3 <= Timeout_t1) then begin
    S:='Prüfung der INI-Einstellungen für Timeout t1 und t3 (IEC 870-5-104):' + #13 +
       '[104] t1 = ' + IntToStr (Timeout_t1) + ', t3 = ' + IntToStr (Timeout_t3) + #13#13 +
       't3 muß größer t1 sein !' + #13 +
       'Standardwerte in ms: t1 = 15000, t3 = 20000';
    WriteProgramLog (S, lt_Error);
    MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=false;
  end;
end;

{--------------------------------------------------}
procedure TFormPortIec104.UpdateMonitor (S: string);
{--------------------------------------------------}
begin
  inherited UpdateMonitor(S);

  // 104-Port-Daten an alle verbundenen Remote-Monitor-Clients senden: 10.03.2015, WW
  if Assigned (FRemoteMonitorObj) then
    FRemoteMonitorObj.SendPortData (C_RM_104, S, nil);  // ohne Kontrollelemente-Daten
end;

{---------------------------------------------------------------------------}
procedure TFormPortIec104.ShowServerPortStatus (bActive: boolean; S: string);
{---------------------------------------------------------------------------}
{ Status Server-Port geöffnet/geschlossen anzeigen }
begin
  lServer.Caption:=S;  { Ausgabe im Server-Textpanel }
  if bActive then
    lServer.Font.Color:=clgreen
  else
    lServer.Font.Color:=clRed;
end;

{------------------------------------------------------------------------------}
procedure TFormPortIec104.ShowClientConnection (Status: TStatusClientConnection;
  S: string);
{------------------------------------------------------------------------------}
{ Status Client-Verbindung anzeigen }
begin
  lClient.Caption:=S;  { Ausgabe im Client-Textpanel }
  case Status of
    scc_connected:     lClient.Font.Color:=clGreen;
    scc_disconnected:  lClient.Font.Color:=clRed;
  else
    lClient.Font.Color:=clBlack;
  end;

  if Status = scc_connected then begin  { wenn Client-Verbindung geöffnet wurde }
    FormTelegrIec104.Init_Uebertragung;  { Telegramm-Übertragung initialisieren }

    if (SocketIec is TClientSocketIec) then  { wenn wir Zentrale sind (Client) }
      FormTelegrIec104.Set_Telegramm_U (C_FKT_STARTDT_ACT);  // Datenübertragung in Unterstation aktivieren: STARTDT: act schicken

    TimerRedundanzVerbUnterbrechung.Enabled:=false;  // Timer-Überwachung für Verbindungsunterbrechung stoppen
  end
  else if Status = scc_disconnected then begin  { wenn Client-Verbindung geschlossen wurde }
    { -> Info: wird nicht beim Beenden des Serversockets mit offener Client-Verbindung
               aufgerufen (OK !) }
    if Redundanzbetrieb <> C_Redundanz_Aus then begin  // nur im Redundanzbetrieb
      { INI neu lesen, wenn geändert; 10.03.2015, WW }
      GetIniPort (true);

      TimerRedundanzVerbUnterbrechung.Enabled:=true;  // Timer-Überwachung für Verbindungsunterbrechung starten
    end;
  end;
end;

{---------------------------------------------}
function TFormPortIec104.GetConnected: boolean;
{---------------------------------------------}
begin
  Result:=Assigned (SocketIec.VerbindungSocket);
end;

{----------------------------------------------}
procedure TFormPortIec104.ActiveCloseConnection;
{----------------------------------------------}
{ Verbindung zum Client aktiv schließen (nur, wenn wir Server sind. Verbindung
  wurde von Client aufgebaut) }
begin
  if IECLogFile <> nil then
    IECLogFile.Write ('active close', '', '');

  if Assigned (SocketIec.VerbindungSocket) then
    SocketIec.VerbindungSocket.Close
  else begin
    { Logfile-Protokollierung: }
    if IECLogFile <> nil then
      IECLogFile.Write ('FEHLER TFormPortIec104.ActiveCloseConnection: Socket = nil', '', '', lt_Error);
  end;

  // nil könnte Grund für Zugriffsverletzung bei Dauertest sein ! Test: verbindung unterbrechen !
  SocketIec.VerbindungSocket:=nil;  // Zeiger der Verbindung zur Gegenstelle löschen
end;

{------------------------------------}
procedure TFormPortIec104.Port_Action;
{------------------------------------}
var
  S: string;
  AllesEmpfangen: boolean;
  TelegrammTyp: TTelegrammTyp;
  ErrMsg: string;

begin
  if Assigned (SocketIec.VerbindungSocket) then begin  { es besteht eine Verbindung zur Gegenstelle }
    { Timeout-Überwachung t0 beenden, falls noch aktiv }
    if SocketIec is TClientSocketIec then
      StopTimer_t0;

    { Timeout-Überwachung t3 starten, wenn t3 > 0 }
    if Timeout_t3 > 0 then
      StartTimer_t3;

    { Telegramm an LW senden: }
    if FormTelegrIec104.Get_SendTelegramm (S, TelegrammTyp) then begin
      SocketIec.Send (S);
      ShowSendeDaten (S);

      // Bereit-Flag und Versendet-Flag des zu versendeten Telegramm rücksetzen:
      FormTelegrIec104.Set_SendTelegrammVersendet;
      if TelegrammTyp = tt_104_Quitt_Con then  // Quittierungs-/Bestätigungstelegramm
        FormTelegrIec104.Set_SendTelegrammAngekommen  { eben versendetes Telegramm aus Sende-Telegrammliste löschen }
      else begin
        RestartTimer_t1;  { t1-Timeout-Überwachung neustarten, wir erwarten eine Quittierung/Bestätigung der Gegenstelle) }
        // wenn ein Daten-Telegramm versendet wurde (104: I-Format):
        if TelegrammTyp in [tt_Data_MRG, tt_Data_DSfG, tt_Data_DSfG_KZW, tt_Data_intern] then begin
          FormTelegrIec104.Set_SendTelegrammUnquittiert_I;  // versendetes Telegramm in Unquittiert-Liste stellen
          FormTelegrIec104.Inc_SendefolgeZS_VS;  // Sendefolgezählerstand hochzählen
          FormTelegrIec104.Inc_kZS;  // k-Zählerstand hochzählen
        end;
      end;

      { t3-Timeout-Zähler rücksetzen, wenn Telegramm gesendet wird: }
      if Timeout_t3 > 0 then
        ResetTimer_t3;
    end;

    { Telegramm der LW empfangen: }
    AllesEmpfangen:=SocketIec.Received (S);
    if AllesEmpfangen then begin   { Telegramm wurde vollständig empfangen }
      ShowEmpfangsdaten (S);                    { Empfangsdaten anzeigen }
      FormTelegrIec104.Set_EmpfTelegramm(S);
      FormTelegrIec104.Uebertragung (ics_Receive);  { Aktion nach empfangenem Telegramm }

      { t3-Timeout-Zähler rücksetzen, wenn Telegramm empfangen wird: }
      if Timeout_t3 > 0 then
        ResetTimer_t3;
    end;

    { Timeout-Behandlung:
      ab 21.08.2015: Immer ausführen (bisher nur, wenn nicht alles empfangen.
                     -> Führte dazu, daß Timeout_t2-Aktion nicht ausgeführt wurde,
                     wenn viele Generalabfrage-Telegramme auf Einzelstation
                     hintereinander empfangen wurden. t2-Countdown lief ins
                     Negative (Siemens Leitwarte bei STGW) }
    if TO_Count_t1 >= Timeout_t1 then begin  { Timeout t1 }
      StopTimer_t1;
      if IECLogFile <> nil then
        IECLogFile.Write ('Timeout t1', '', '');
      ActiveCloseConnection;  { active close }
    end
    else if TO_Count_t2 >= Timeout_t2 then begin { Timeout t2 }
      StopTimer_t2;
      FormTelegrIec104.Uebertragung (ics_Timeout_t2);  { Aktion nach Timeout t2 }
    end
    else if (Timeout_t3 > 0) AND (TO_Count_t3 >= Timeout_t3) then begin { Timeout t3 }
      ResetTimer_t3;
      FormTelegrIec104.Uebertragung (ics_Timeout_t3);  { Aktion nach Timeout t3 }
    end else  { eigene Aktion }
      FormTelegrIec104.Uebertragung (ics_Idle);
  end
  else begin  { es besteht keine Verbindung zur Gegenstelle }
    { Timeout-Überwachungen t1, t2, t3 stoppen: }
    StopTimer_t1;
    StopTimer_t2;
    StopTimer_t3;

    if (SocketIec is TClientSocketIec) then begin  { wenn wir Zentrale sind (Client) }
      if StartTimer_t0 then begin
        if not FormTelegrIec104.IsClosing then begin
          { Client-Verbindung zur Station (Server) herstellen: }
          if not TClientSocketIec (SocketIec).OpenClient(IPAdresse, Port, ErrMsg) then begin
            if IECLogFile <> nil then
              IECLogFile.Write (ErrMsg, '', '', lt_Error);
          end;
        end;  
      end;
    end;
  end;

  { Timer t0 überwacht Verbindungsaufbau: }
  if (SocketIec is TClientSocketIec) AND (TO_Count_t0 >= Timeout_t0) then begin  { Timeout t0 }
    StopTimer_t0;
    if IECLogFile <> nil then
      IECLogFile.Write ('Timeout t0', '', '');
  end;
end;

{-------------------------- Timerroutinen -------------------------------------}

{----------------------------------------------}
function TFormPortIec104.StartTimer_t0: boolean;
{----------------------------------------------}
{ Timer t0 starten;
  Ergebnis: false, wenn Timer t0 bereits enabled ist }
begin
  if not TimerTimeout_t0.Enabled then begin
    TO_Count_t0:=0;
    TimerTimeout_t0.Enabled:=true;
    { t0-Timeout-Anzeige: }
    eTimeout_t0.Text:=IntToStr(Timeout_t0 DIV 1000) + ' s';
    Application.ProcessMessages;
    Result:=true;
  end else
    Result:=false;
end;

{-------------------------------------}
procedure TFormPortIec104.StopTimer_t0;
{-------------------------------------}
begin
  if TimerTimeout_t0.Enabled then begin
    TimerTimeout_t0.Enabled:=false;
    TO_Count_t0:=0;
    { t0-Timeout-Anzeige: }
    eTimeout_t0.Text:='';
    Application.ProcessMessages;
  end;
end;

{----------------------------------------}
procedure TFormPortIec104.RestartTimer_t1;
{----------------------------------------}
begin
  if not TimerTimeout_t1.Enabled then
    TimerTimeout_t1.Enabled:=true;

  TO_Count_t1:=0;
  { t1-Timeout-Anzeige: }
  if Assigned (CBTimeout_t1) then
    CBTimeout_t1 (IntToStr(Timeout_t1 DIV 1000) + ' s');
end;

{-------------------------------------}
procedure TFormPortIec104.StopTimer_t1;
{-------------------------------------}
begin
  if TimerTimeout_t1.Enabled then begin
    TimerTimeout_t1.Enabled:=false;
    TO_Count_t1:=0;
    { t1-Timeout-Anzeige: }
    if Assigned (CBTimeout_t1) then
      CBTimeout_t1 ('');
  end;
end;

{--------------------------------------}
procedure TFormPortIec104.StartTimer_t2;
{--------------------------------------}
begin
  if not TimerTimeout_t2.Enabled then begin
    TO_Count_t2:=0;
    TimerTimeout_t2.Enabled:=true;
    { t2-Timeout-Anzeige: }
    if Assigned (CBTimeout_t2) then
      CBTimeout_t2 (IntToStr(Timeout_t2 DIV 1000) + ' s');
  end;
end;

{-------------------------------------}
procedure TFormPortIec104.StopTimer_t2;
{-------------------------------------}
begin
  if TimerTimeout_t2.Enabled then begin
    TimerTimeout_t2.Enabled:=false;
    TO_Count_t2:=0;
    { t2-Timeout-Anzeige: }
    if Assigned (CBTimeout_t2) then
      CBTimeout_t2 ('');
  end;
end;

{--------------------------------------}
procedure TFormPortIec104.StartTimer_t3;
{--------------------------------------}
begin
  if not TimerTimeout_t3.Enabled then begin
    TO_Count_t3:=0;
    TimerTimeout_t3.Enabled:=true;
    { t3-Timeout-Anzeige: }
    if Assigned (CBTimeout_t3) then
      CBTimeout_t3 (IntToStr(Timeout_t3 DIV 1000) + ' s');
  end;
end;

{--------------------------------------}
procedure TFormPortIec104.ResetTimer_t3;
{--------------------------------------}
begin
  TO_Count_t3:=0;  // Zähler beginnt von vorn
  { t3-Timeout-Anzeige: }
  if Assigned (CBTimeout_t3) then
    CBTimeout_t3 (IntToStr(Timeout_t3 DIV 1000) + ' s');
end;

{-------------------------------------}
procedure TFormPortIec104.StopTimer_t3;
{-------------------------------------}
begin
  if TimerTimeout_t3.Enabled then begin
    TimerTimeout_t3.Enabled:=false;
    TO_Count_t3:=0;
    { t3-Timeout-Anzeige: }
    if Assigned (CBTimeout_t3) then
      CBTimeout_t3 ('');
  end;
end;

{--------------------------------------------------------------}
procedure TFormPortIec104.TimerTimeout_t0Timer(Sender: TObject);
{--------------------------------------------------------------}
{ Timeout t0 }
begin
  TO_Count_t0:=TO_Count_t0 + integer (TimerTimeout_t0.Interval);
  { t1-Timeout-Anzeige: }
  eTimeout_t0.Text:=IntToStr((Timeout_t0 - TO_Count_t0) DIV 1000) + ' s';
  Application.ProcessMessages;
end;

{--------------------------------------------------------------}
procedure TFormPortIec104.TimerTimeout_t1Timer(Sender: TObject);
{--------------------------------------------------------------}
{ Timeout t1 }
begin
  TO_Count_t1:=TO_Count_t1 + integer (TimerTimeout_t1.Interval);
  { t1-Timeout-Anzeige: }
  if Assigned (CBTimeout_t1) then
    CBTimeout_t1 (IntToStr((Timeout_t1 - TO_Count_t1) DIV 1000) + ' s');
end;

{--------------------------------------------------------------}
procedure TFormPortIec104.TimerTimeout_t2Timer(Sender: TObject);
{--------------------------------------------------------------}
{ Timeout t2 }
begin
  TO_Count_t2:=TO_Count_t2 + integer (TimerTimeout_t2.Interval);
  { t2-Timeout-Anzeige: }
  if Assigned (CBTimeout_t2) then
    CBTimeout_t2 (IntToStr((Timeout_t2 - TO_Count_t2) DIV 1000) + ' s');
end;

{--------------------------------------------------------------}
procedure TFormPortIec104.TimerTimeout_t3Timer(Sender: TObject);
{--------------------------------------------------------------}
{ Timeout t3 }
begin
  TO_Count_t3:=TO_Count_t3 + integer (TimerTimeout_t3.Interval);
  { t3-Timeout-Anzeige: }
  if Assigned (CBTimeout_t3) then
    CBTimeout_t3 (IntToStr((Timeout_t3 - TO_Count_t3) DIV 1000) + ' s');
end;

{------------------------------------------------------------------------------}
procedure TFormPortIec104.TimerRedundanzVerbUnterbrechungTimer(Sender: TObject);
{------------------------------------------------------------------------------}
begin
  if Assigned (VerifyFlagObject) then
    VerifyFlagObject.WriteTerminalErrorFlag;  { zentrale Systemüberwachung: Fehler ! }
end;

{-------------------------- Remote-Monitor ------------------------------------}

{------------------------------------------------------}
procedure TFormPortIec104.Fill_RemoteMonitorControlList;
{------------------------------------------------------}
{ Füllt Liste mit allen Kontrollelementen, deren Daten an Remote-Clients gesendet
  werden sollen }
begin
  FRemoteMonitorControlList.Add (lServer);
  FRemoteMonitorControlList.Add (lClient);
  FRemoteMonitorControlList.Add (eTimeout_t0);
end;

end.
