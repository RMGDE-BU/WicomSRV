{******************************************************************************}
{* Unit: V24-Monitor mit Schnittstellenroutinen für IEC-Kopplung (32-Bit)     *}
{* 26.03.2003  WW                                                             *}
{******************************************************************************}
unit FIec101Port;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, Buttons, DateUtils, ScktComp,
  Serial, SerIec, IecConst, IecIniFile, FIecCustomPort, FIecCustomTelegr,
  FIec101Telegr, IecLog, ErrConst, ErrTxt, LogFile, O_IecRemoteMonitor;

type
  { Formular mit V24-Schnittstellenhandling und Monitor }

  TFormPortIec101 = class(TFormCustomPortIec)
    TimerTimeout_EmpfSekundaer: TTimer;
    procedure TimerTimeout_EmpfSekundaerTimer(Sender: TObject);
  private
    { Private-Deklarationen }
    COMNr: integer;
    Baud: integer;
    Datenbits: integer;
    Paritaet: TParityBit;
    Stopbits: TStopBits;
    FSerialIec: TSerialIec;
    FTimeout_EmpfSekundaer: TCBStringProc;
    TO_Count_EmpfSekundaer: integer;
    Timeout_EmpfSekundaer: integer;
    FLastCommError: cardinal;
    FLastCommErrorTime: TDateTime;
    procedure GetIniPort (bCheckIniChanged: boolean);
    function ConnectFehler (Fehler: integer; bMsgBox: boolean): boolean; { false -> COM öffnen OK }
    procedure CheckCommError;
    procedure Fill_RemoteMonitorControlList;
  protected
    procedure UpdateMonitor (S: string); override;
    procedure Port_Action; override;
    function GetConnected: boolean; override;
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; ANetProgDir: string;
      AIecLogfile: TIecLogFile; ARemoteMonitorObj: TRemoteMonitorObj;
      var bOK: boolean); reintroduce;
    destructor Destroy; override;
    procedure ReConnect;
    property LastCommErrorTime: TDateTime read FLastCommErrorTime;
    property CBTimeout_EmpfSekundaer: TCBStringProc
      read FTimeout_EmpfSekundaer write FTimeout_EmpfSekundaer;
  end;

var
  FormPortIec101: TFormPortIec101;

implementation

{$R *.dfm}

resourcestring
  SMsgErrorCOM_nicht_vorhanden  = 'COM%d nicht vorhanden';
  SMsgErrorCOM_nicht_geoeffnet  = 'COM%d kann nicht geöffnet werden';
  SMsgError_unbekannt           = 'Unbekannter Fehler %d';
  SMsgCOMGeoeffnet              = 'COM%d ist geöffnet';
  SMsgCOMReconnect              = 'COM%d erneut öffnen...';


{--------------------------------------------------------------------------}
constructor TFormPortIec101.Create (AOwner: TComponent; ANetProgDir: string;
  AIecLogfile: TIecLogFile; ARemoteMonitorObj: TRemoteMonitorObj;
  var bOK: boolean);
{--------------------------------------------------------------------------}
var
  Erg: integer;

begin
  bOK:=true;  // Default: Create OK
  inherited Create (AOwner, ANetProgDir, AIecLogfile, ARemoteMonitorObj);

  Fill_RemoteMonitorControlList;  // 06.07.2015, WW

  TO_Count_EmpfSekundaer:=0;
  FLastCommError:=0;  // Default: Kein Fehler auf der Schnittstelle
  FLastCommErrorTime:=0;  // Default: Letzter aufgetretener CommError ist ewig her...

  GetIniPort (false);  { liest Ini-Einstellungen für ComPort }

  if Assigned (FormTelegrIec101) then
    CBTimeout_EmpfSekundaer:=FormTelegrIec101.Update_TimeoutAnzeige;  // Timeout-Ausgabe im 101-Telegramm-Formular

  FSerialIec:=TSerialIec.Create (self);
  Erg:=FSerialIec.Connect (COMNr, Baud, Datenbits, Paritaet, Stopbits);
  { Prüft Erg;
    - bOK 'true' bei Connect-Fehler, ohne MsgBox-Ausgabe; 06.07.2015, WW }
  ConnectFehler (Erg, false);

  TimerUebertragung.Enabled:=true;
end;

{---------------------------------}
destructor TFormPortIec101.Destroy;
{---------------------------------}
begin
  if FSerialIec <> nil then
    FSerialIec.Disconnect;
  FSerialIec.Free;
  inherited Destroy;
end;

{---------------------------------------------------------------}
Procedure TFormPortIec101.GetIniPort (bCheckIniChanged: boolean);
{---------------------------------------------------------------}
{ INI-Konfiguration für 101-Port lesen;
  Übergabe: Flag 'bCheckIniChanged': wenn true, wird nur bei geänderter INI-
              Konfiguration gelesen }
var
  Iec32Ini: TIec32Ini;
begin
  Iec32Ini:=TIec32Ini.Create (FNetProgDir);
  try
    // INI-Einstellungen, welche nur zum Programmstart gelesen werden (können):
    if (not bCheckIniChanged) then begin
      COMNr:=Iec32Ini.COM;
      Baud:=Iec32Ini.Baud;
      Datenbits:=Iec32Ini.Databits;
      Paritaet:=Iec32Ini.Parity;
      Stopbits:=Iec32Ini.Stopbits;
    end;

    // INI-Einstellungen, welche zur Laufzeit neu gelesen werden (können):
    if (not bCheckIniChanged) OR
       (bCheckIniChanged AND
        not (SameDateTime (Iec32Ini.FileDate, FIniDate))) then begin
      FIniDate:=Iec32Ini.FileDate;  // Zeitstempel der INI-Datei merken; 10.03.2015, WW

      Timeout_EmpfSekundaer:=Iec32Ini.TO_EmpfSekundaer;
    end;
  finally
    Iec32Ini.Free;
  end;
end;

{----------------------------------------------------------------------------------}
function TFormPortIec101.ConnectFehler (Fehler: integer; bMsgBox: boolean): boolean;
{----------------------------------------------------------------------------------}
var
  S: string;
begin
  case Fehler of
    -1: S:=Format (SMsgErrorCOM_nicht_vorhanden, [COMNr]);
    -2: S:=Format (SMsgErrorCOM_nicht_geoeffnet, [COMNr]);
  else
    S:=Format (SMsgError_unbekannt, [Fehler]);
  end;

  if Fehler < 0 then begin
    if Assigned (IECLogFile) then
      IECLogFile.Write (S, '', '', lt_Error);
    if bMsgBox then
      MessageBoxWhenExe (0, pchar(S), pchar (Application.Title), MB_ICONERROR + MB_OK);
    Result:=true;
  end
  else begin
    S:=Format (SMsgCOMGeoeffnet, [COMNr]);
    if Assigned (IECLogFile) then
      IECLogFile.Write (S, '', '', lt_Info);
    Result:=false;
  end;

  // Ausgabe im Server-Textpanel; 06.07.2015, WW
  lServer.Caption:=S;
  if not Result then
    lServer.Font.Color:=clgreen
  else
    lServer.Font.Color:=clRed;
end;

{---------------------------------------------}
function TFormPortIec101.GetConnected: boolean;  // 06.07.2015, WW
{---------------------------------------------}
begin
  Result:=FSerialIec.Active;
end;

{---------------------------------------}
procedure TFormPortIec101.CheckCommError;
{---------------------------------------}
{ Fehlerzustand der seriellen Schnittstellen prüfen, bei aufgetretenem
  Schnittstellenfehler diesen protokollieren und Zeit merken }
var
  sStatus: string;
  sError: string;

begin
  if (FSerialIec.CommError <> 0) then begin
    FLastCommErrorTime:=Now;

    if (IECLogFile <> nil) then begin
      // Log-Eintrag nur bei Wechsel des Fehlers, sonst Dauerschreiben des Logs:
      if (FSerialIec.CommError <> FLastCommError) then begin
        sStatus := GetStatusText (COM_ERROR);
        sError := GetErrorText (COM_ERROR, FSerialIec.CommError);
        IECLogFile.Write (sStatus + ': ' + sError, '', '', lt_Error);
      end;
    end;
  end;
  FLastCommError:=FSerialIec.CommError;
end;

{----------------------------------}
procedure TFormPortIec101.ReConnect;
{----------------------------------}
{ Serielle Schnittstelle erneut öffnen }
var
  Erg: integer;

begin
  // nur, wenn aktuell Schnittstellenfehler vorliegt oder Schnittstelle nicht
  // geöffnet ist:
  if (FSerialIec.CommError <> 0) OR
     (not Connected) then begin  // 06.07.2015, WW
    if Assigned (IECLogFile) then
      IECLogFile.Write (Format (SMsgCOMReconnect, [COMNr]), '', '', lt_Warning);

    FSerialIec.Disconnect;
    Erg:=FSerialIec.Connect (COMNr, Baud, Datenbits, Paritaet, Stopbits);
    ConnectFehler (Erg, false);  { prüft Erg }
  end;
end;

{--------------------------------------------------}
procedure TFormPortIec101.UpdateMonitor (S: string);
{--------------------------------------------------}
begin
  inherited UpdateMonitor(S);

  // 101-Port-Daten an alle verbundenen Remote-Monitor-Clients senden: 10.03.2015, WW
  if Assigned (FRemoteMonitorObj) then
    FRemoteMonitorObj.SendPortData (C_RM_101, S, nil);  // ohne Kontrollelemente-Daten
end;

{------------------------------------}
procedure TFormPortIec101.Port_Action;
{------------------------------------}
var
  S: string;
  AllesEmpfangen: boolean;
  isPrm: boolean;
  isSek: boolean;
  dummy: boolean;

begin
  { Telegramm an LW senden: }
  if FormTelegrIec101.Get_SendTelegramm (S, isPrm) then begin
    FSerialIec.Send (S);
    ShowSendeDaten (S);
    FormTelegrIec101.Set_SendTelegrammVersendet (isPrm);  // Bereit-Flag für zu versendendes Telegramm rücksetzen

    if isPrm then begin  // es wurde ein Primärstation-Telegramm versendet
      { INI neu lesen, wenn geändert; 10.03.2015, WW }
      GetIniPort (true);

      TO_Count_EmpfSekundaer:=0;  // Timeout-Zähler rücksetzen
      TimerTimeout_EmpfSekundaer.Enabled:=true;  // Timeout-Überwachung starten für Antwort der LW
      { Timeout-Anzeige: }
      if Assigned (CBTimeout_EmpfSekundaer) then
        CBTimeout_EmpfSekundaer (IntToStr(Timeout_EmpfSekundaer DIV 1000) + ' s');
    end;
  end;

  { Telegramm der LW empfangen: }
  AllesEmpfangen:=FSerialIec.Received (S, true);
  CheckCommError;  // 05.03.2015, WW

  if AllesEmpfangen then begin   { Telegramm wurde vollständig empfangen }
    FSerialIec.ClearEmpfangspuffer;
    ShowEmpfangsdaten (S);                    { Empfangsdaten anzeigen }
    FormTelegrIec101.Set_EmpfTelegramm(S);
    FormTelegrIec101.Uebertragung (ics_Receive, isSek);  { Aktion nach empfangenem Telegramm }
    if isSek then begin
      TimerTimeout_EmpfSekundaer.Enabled:=false;  // Timeout-Überwachung beenden nach empfangenem Sekundärstation-Telegramm
      { Timeout-Anzeige: }
      if Assigned (CBTimeout_EmpfSekundaer) then
        CBTimeout_EmpfSekundaer ('');
    end;
  end
  else begin
    if TO_Count_EmpfSekundaer >= Timeout_EmpfSekundaer then begin
      TimerTimeout_EmpfSekundaer.Enabled:=false;  // Timeout-Überwachung beenden nach Timeout
      { Timeout-Anzeige: }
      if Assigned (CBTimeout_EmpfSekundaer) then
        CBTimeout_EmpfSekundaer ('');

      FormTelegrIec101.Uebertragung (ics_Timeout_EmpfSek, dummy);  { Aktion nach Timeout }
    end
    else begin
      { eigene Aktion bei symmetrischem Betrieb:
        - wir werden selber aktiv (Wechsel 'Senden von Antworten' -> 'Senden von Anfragen'
          bei Status s_SndLinkStatus)
        - bei verzögertem Senden }
      if FormTelegrIec101.Uebertragungsprozedur = tp_symmetrisch then begin
        if (FormTelegrIec101.SymmStatus = iss_SndLinkStatus) OR
           (FormTelegrIec101.SymmStatus = iss_SndLinkReady) then
          FormTelegrIec101.Uebertragung (ics_Idle, dummy);
      end;
    end;
  end;
end;

{-------------------------- Timerroutinen -------------------------------------}

{-------------------------------------------------------------------------}
procedure TFormPortIec101.TimerTimeout_EmpfSekundaerTimer(Sender: TObject);
{-------------------------------------------------------------------------}
{ Timeout für Empfang eines Sekundär-Telegramms }
begin
  TO_Count_EmpfSekundaer:=TO_Count_EmpfSekundaer + integer (TimerTimeout_EmpfSekundaer.Interval);
  { Timeout-Anzeige: }
  if Assigned (CBTimeout_EmpfSekundaer) then
    CBTimeout_EmpfSekundaer (IntToStr((Timeout_EmpfSekundaer - TO_Count_EmpfSekundaer) DIV 1000) + ' s');
end;

{-------------------------- Remote-Monitor ------------------------------------}

{------------------------------------------------------}
procedure TFormPortIec101.Fill_RemoteMonitorControlList;
{------------------------------------------------------}
{ Füllt Liste mit allen Kontrollelementen, deren Daten an Remote-Clients gesendet
  werden sollen }
begin
  FRemoteMonitorControlList.Add (lServer);
end;

end.
