{******************************************************************************}
{* Unit: Routinen f�r Ausf�hrung der Abruf-Kommandos                          *}
{* 16.12.2002  WW                                                             *}
{* 05.11.2010  WN  manuelles Nachholen von zur Wiederholung anstehenden SMS   *}
{* 13.01.2011  GD  t-Befehl f�r GasX                                          *}
{* 24.03.2011  GD  Bugfix: t-Befehl f�r GasX                                  *}
{* 28.05.2013  WW  Rufentgegenname: Schnittstelle neu initialisieren nach     *}
{*                 erfolgloser Modeminitialisierung/FUP-Reset                 *}
{* 19.11.2013  WW  Gas-X-Version: Rufnummer aus XML-Verbindungsaufbau-Kommando*}
{*                 um COM-spezifische Vorwahl aus SrvCfgIni-Konfiguration     *}
{*                 erweitern                                                  *}
{* 03.12.2013  WW  mit Rundpuffer auf COM-Logfile und Abruf-Logfile           *}
{* 07.04.2014  WW  Signatur-Lizenz pr�fen; INI-Schalter f�r Rohdaten in XML-  *}
{*                 Response                                                   *}
{* 08.01.2018  WW  mit TCP/IP-Rufentgegennahme f�r DSfG                       *}
{* 13.03.2018  WW  mit freiem Text f�r erweiterte Ergebnis-R�ckgabe (Gas-X)   *}
{* 03.06.2020  WW  Aufschalten auf GPRS-Verbindung auskommentiert (nicht mehr *}
{*                 unterst�tzt)                                               *}
{* 06.08.2021  WW  Ressourcedaten aus Liste statt Datei lesen                 *}
{* 03.01.2022  WW  Optionales COM-Tracelog erweitert f�r alle GAS-X-relevanten*}
{*                 Responses                                                  *}
{* 14.04.2023  WW  Verbindung halten erweitert f�r MRG und DSfG-IP-Abrufe     *}
{* 14.03.2024  WW  mit Terminierung des TCP/IP-Rufentgegennahme-Threads;      *}
{*                 Bugfix freigeben des Abruf-Objekts nur bei nicht erfolg-   *}
{*                 reichem Entgegennehmen eines Anrufs                        *}
{******************************************************************************}
unit AbrufCmdExec;
                                                                                              
interface

uses
  Windows, Classes, SysUtils, ScktComp, DateUtils, AbrufCmd, AbrufAnsw, Serial,
  SerMrgFup, SerMrgModem, SerDSfG, AbrufSrvIniFile, SrvCfgIni, ModemIni,
  AbrufConst, LogCom, ErrConst, FupModemInit, AbrufObj, AbrufObjMrg, AbrufObjDSfG,
  MResMrg, MObjMeld, MObjPara, DDELList, WSysCon, DListen, LogFile, PathIni, T_Zeit,
  Lizenz32, RespConst, MLGZKonvList, GSMModemFkt, SMSList, MSMSKonv, MFileNam,
  TCPIP_DSfG, DDfueParaList, GD_Utils, WChars, SMSDecode, DSMSKonv, WicomSrvUtil,
  WComm, GPRSVerbList, GPRSTelegrList, O_Comm, TCPIP_Mrg, GPRS_Util, WStrUtils,
  REClntCmdList, REClntVerbList, REClntThr, O_ResFilesList;

type
  { Callback-Prozedurtypen }
  TCBStringProc = procedure (S: string) of object;

  { Objekt zur Abrufkommando-Ausf�hrung }
  TAbrufCmdExec = class(TObject)
  private
    FCOMNr: integer;  { Schnittstellennummer, auf der der Abruf l�uft }
    FCOMNr_Kommando: integer;  { Schnittstellennummer aus Kommando }
    GPRSServerSocket: TServerSocket;
    GPRSVerbindungenListe: TGPRSVerbList;
    GPRSTelegrammListe: TGPRSTelegrList;
    FAbrufLogFile: TCustomLogFile;
    FDebugServiceIO: boolean;
    FDebugCOMProtokoll: boolean;
    FRohdatenLoeschen: boolean;
    FDebugRundpufferMaxBytes: integer;
    FLizenzList: TLizenzList;  // 31.01.2023, WW
    FResourceFilesList: TResourceFilesList;  // 06.08.2021, WW

    FSendAnswerToRuf_SMSEmpfangClient: TCBStringProc;

    ComLogFile: TComLogFile;
    CommObj: TCommObj;  { Kommunikations-Objekt (seriell oder TCP/IP) }
    Abruf: TAbruf;
    FUP_F2_Count: integer;

    ProzessId_VerbAufbau: string;   { Prozess-Id im Verbindungsaufbau- bzw. Rufannahme-Kommando }
    DSfG_TransparentModus_VerbAufbau: boolean; { DSfG-TransparentModus im Verbindungsaufbau-Kommando }
    TraceLog_VerbAufbau: integer; { Schalter f�r Tracelog-Ausgabe im Verbindungsaufbau-Kommando }
    GeraeteTyp_Cmd: integer; { aktueller Ger�te-Typ aus Verbindungsaufbau-, DSfG-Umschalt- bzw.
                               Rufannahme-Kommando f�r Antwort-R�ckgabe an Client }
    StationsKennung: string;
    { f�r Rufentgegennahme: }
    Neustart_Rufpolling: boolean;      { Flag kennzeichnet Zustand, wenn Rufpolling
                                         nach Programmstart oder Abruf neu beginnt (true) }
    FUP_Reset_fuer_Rufpolling_OK: boolean;  { Flag kennzeichnet Zustand, ob bei Rufpolling-
                                              Neustart ein FUP-Reset erfolgen mu� (false) oder
                                              ein FUP-Init ausreicht (true) }
    Kommando_Rufentgegennahme: string; { zuletzt gesendetes Rufentgegennahme-Kommando }
    RingCount: integer;
    TickCount_LastRing: cardinal;
    FNextRundpufferDatumZeit: TDateTime;

    FREClntVerbList: TREClntVerbList;  // Liste mit TCP/IP-Rufentgegennahme-Verbindungen
    FAktREClntVerbDataObj: TREClntVerbDataObj;  // Objekt der aktuellen TCP/IP-Rufentgegennahme-Verbindung
    FAktREClntVerbDataIndex: integer;  // Listenindex des aktuellen TCP/IP-Rufentgegennahme-Verbindungsobjekts (0-basiert)

    { f�r SMS-Empfang: }
    FSendeSMSFiles: boolean;    // 05.11.2010  WN
    FLetztSMSFilename: string;  // 05.11.2010  WN
    FSMS_WdhDatum: integer;     // 05.11.2010  WN
    { aus Schnittstellenkonfiguration-Ini-File: }
    Device: string;               { an COM angeschlossenes Ger�t (FUP, Modem) }
    ModemName: string;
    MaxRings: integer;  { f�r Rufentgegennahme per Modem}
    FupHandshake: boolean;
    Vorwahl_GasX: string;  { nur f�r Gas-X-Version }
    { aus Modem-Ini-File: }
    MaxBaudrate: integer;
    Modem_Databits_Parity_Stopbits: string;
    { aus Programm-Ini-File: }
    MRGTimeouts: TMRGTimeouts;
    MRGModemVersuche: TMRGModemVersuche;
    DSfGTimeouts: TDSfGTimeouts;
    DSfG_BCCVersuche: integer;
    ZeitSyncAbweichungMin_Cfg: integer;
    ZeitSyncAbweichungMax_Cfg: integer;
    ZeitSyncKorrekturMax_Cfg: integer;
    GasX_AIX_kompatibel_MRG_Analog_Cfg: boolean;
    KennungExt_Cfg: string;
    SMS_BackupDir_Cfg: string;
    SMS_ImportDir_Cfg: string;
    ISO646_Cfg: boolean;
    FFirmwareBinFilePath: string;
    FXMLResponseEncodeRohdaten: integer;  // 07.04.2014, WW
    { Signatur-Lizenz: }
    FSignatur_freigeschaltet: boolean;  // 07.04.2014, WW

    GPRSData_AusgabeProc: TCBGPRSDataProc;
    procedure SetDebugCOMProtokoll (Value: boolean);
    procedure Init_FehlerGruppeCode (var Fehlergruppe: integer; var Fehlercode: integer);
    procedure Pruefe_auf_FUP_F2_Antwort (AFehlergruppe, AFehlercode: integer);
    function GetGPRSConnection (Kennung: string;
                                var Fehlergruppe: integer; var Fehlercode: integer): TCustomWinSocket;
    function Init_MRG_CommObject (overIP: boolean; sGPRS_Kennung: string; doFUP_Reset: boolean;
                                  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
    function Init_DSfG_CommObject (overIP: boolean; sGPRS_Kennung: string;
                                   bREClntThread: boolean;
                                   var Fehlergruppe: integer; var Fehlercode: integer): boolean;
    function Init_MRG_Abruf (overIP: boolean; sGPRS_Kennung: string;
                             var Fehlergruppe: integer; var Fehlercode: integer): boolean;
    function Init_DSfG_Abruf (overIP: boolean; sGPRS_Kennung: string; bREClntThread: boolean;
                              var Fehlergruppe: integer; var Fehlercode: integer): boolean;
    procedure Free_CommObject;
    procedure Free_Abruf;
    procedure ReadSrvCfg_ModemIni;
    procedure ReadProgramIni_MRG;
    procedure ReadProgramIni_DSfG;

    function KonvSMSToXMLFile (AGeraeteTyp: integer; ASMS_Data: string;
      ASMS_DatumZeit, ADatumZeit_DatenlueckePruefung_bis: TDateTime): boolean;

    function Check_NeueREClntVerbindung: boolean;
    procedure Terminate_AktREClntVerbindung_Thread (bWaitFor: boolean);
  public
    Rufentgegennahme_SMSEmpfang_aktiviert: boolean;  // Flag: true, wenn Rufentgegennahme auf der COM aktiv ist
    RufTyp: integer;    // Rufentgegennahme/SMS-Datenempfang-Modus (aus Schnittstellenkonfiguration-Ini-File)
    Keep_COM_Open: boolean;    // Flag: true, wenn COM nach dem ersten Abruf weiter offen gehalten bleiben soll (bei FUP)
    VerbindungHalten_Cfg: integer;  // Zykluszeit f�r Halten der Verbindung zur Station
    constructor Create (ACOMNr: integer;
                        ACOMNr_Kommando: integer;
                        AGPRSServerSocket: TServerSocket;
                        AGPRSVerbindungenListe: TGPRSVerbList;
                        AGPRSTelegrammListe: TGPRSTelegrList;
                        AREClntVerbList: TREClntVerbList;
                        AResourceFilesList: TResourceFilesList;
                        AAbrufLogFile: TCustomLogFile;
                        ADebugCOMProtokoll: boolean;
                        ADebugServiceIO: boolean; ADebugRohdaten: boolean;
                        ADebugRundpufferMaxBytes: integer;
                        ALizenzList: TLizenzList;
                        AGPRSData_AusgabeProc: TCBGPRSDataProc);
    destructor Destroy; override;
    function VerbAufbau (Kommando: string; var Verbindung_OK: boolean): string;
    function VerbAbbau (Kommando: string): string;
    function MessAbruf (Kommando: string; StaKanalKonvDataList: TStaKanalKonvDataList): string;
    function MeldAbruf (Kommando: string): string;
    function ParaAbruf (Kommando: string): string;
    function PruefAbruf (Kommando: string): string;
    function ZeitSynchronisation (Kommando: string): string;
    function DSfGUmschaltung (Kommando: string): string;
    function DSfGBusAnalyseAbruf (Kommando: string): string;
    function RundpufferReset (Kommando: string): string;
    function Parametrieren (Kommando: string): string;
    function Transparentbefehl (Kommando: string): string;

    function Rufentgegennahme (Kommando: string; var RE_gestartet: boolean): string;
    function Ruf_SMS_Abfragen (var Ruf_SMS_steht_an: byte): string;
    function Rufannahme (Kommando: string): string;
    function Rufliste (Kommando: string): string;
    function SlaveRufQuittierung (Kommando: string): string;
    function Rueckruf (Kommando: string): string;
    function ZeitAbruf (Kommando: string): string;  // 17.08.2010
    procedure VerbHalten;

    property CBSendAnswerToRuf_SMSEmpfangClient: TCBStringProc
      read FSendAnswerToRuf_SMSEmpfangClient write FSendAnswerToRuf_SMSEmpfangClient;
    property DebugCOMProtokoll: boolean write SetDebugCOMProtokoll;
    property AbrufLogFile: TCustomLogFile read FAbrufLogFile
      write FAbrufLogFile;  // 03.12.2013, WW
    property DebugRundpufferMaxBytes: integer write FDebugRundpufferMaxBytes;  // 03.12.2013, WW
    property DebugServiceIO: boolean write FDebugServiceIO;  // 28.09.2020, WW
    property RohdatenLoeschen: boolean write FRohdatenLoeschen;  // 28.09.2020, WW

    // 05.11.2010  WN
    property SendeSMSFiles: boolean read FSendeSMSFiles write FSendeSMSFiles;
    property LetztSMSFilename: string read FLetztSMSFilename write FLetztSMSFilename;
    property SMS_WdhDatum: Integer read FSMS_WdhDatum write FSMS_WdhDatum;
  end;

implementation

{ TAbrufCmdExec }

{------------------------------------------------------------------------}
constructor TAbrufCmdExec.Create (ACOMNr: integer;
                                  ACOMNr_Kommando: integer;
                                  AGPRSServerSocket: TServerSocket;
                                  AGPRSVerbindungenListe: TGPRSVerbList;
                                  AGPRSTelegrammListe: TGPRSTelegrList;
                                  AREClntVerbList: TREClntVerbList;
                                  AResourceFilesList: TResourceFilesList;
                                  AAbrufLogFile: TCustomLogFile;
                                  ADebugCOMProtokoll: boolean;
                                  ADebugServiceIO: boolean;
                                  ADebugRohdaten: boolean;
                                  ADebugRundpufferMaxBytes: integer;
                                  ALizenzList: TLizenzList;
                                  AGPRSData_AusgabeProc: TCBGPRSDataProc);
{------------------------------------------------------------------------}
begin
  inherited Create;
  FCOMNr:=ACOMNr;
  FCOMNr_Kommando:=ACOMNr_Kommando;
  GPRSServerSocket:=AGPRSServerSocket;
  GPRSVerbindungenListe:=AGPRSVerbindungenListe;
  GPRSTelegrammListe:=AGPRSTelegrammListe;
  FREClntVerbList:=AREClntVerbList;
  FResourceFilesList:=AResourceFilesList;
  FAbrufLogFile:=AAbrufLogFile;
  FDebugCOMProtokoll:=ADebugCOMProtokoll;
  FDebugServiceIO:=ADebugServiceIO;
  FRohdatenLoeschen:=not ADebugRohdaten;
  FDebugRundpufferMaxBytes:=ADebugRundpufferMaxBytes;
  FLizenzList:=ALizenzList;

  GPRSData_AusgabeProc:=AGPRSData_AusgabeProc;

  // f�r RE: N�chsten Rundpuffer in einem Tag ausf�hren; 03.12.2013, WW
  FNextRundpufferDatumZeit:=IncDay (Now, 1);

  { Logfile f�r Schnittstellen-Protokollierung initialisieren: }
  if FDebugCOMProtokoll then
    ComLogFile:=TComLogFile.Create (PathServer.Pathname[WLogDir],
                                    FCOMNr, false, '', FCOMNr_Kommando)  // 21.04.2015, WW
  else
    ComLogFile:=nil;

  CommObj:=nil;
  Abruf:=nil;
  FUP_F2_Count:=0;

  Device:='';
  ModemName:='';
  RufTyp:=-1;
  MaxRings:=0;
  MaxBaudrate:=0;
  Modem_Databits_Parity_Stopbits:='';
  FupHandshake:=true;
  Vorwahl_GasX:='';

  ZeitSyncAbweichungMin_Cfg:=0;
  ZeitSyncAbweichungMax_Cfg:=0;
  ZeitSyncKorrekturMax_Cfg:=0;
  GasX_AIX_kompatibel_MRG_Analog_Cfg:=false;   { Standard: keine Analogwert-Normierung }
  KennungExt_Cfg:='';  { Standard: Bilden einer Ersatzkennung ohne Erweiterung
                                   ist bei Kennungsvergleich ausgeschaltet }
  SMS_BackupDir_Cfg:='';
  SMS_ImportDir_Cfg:='';
  ISO646_Cfg:=true;  { Standard: Zeichen-Konvertierung ASCII/ISO 646 ein }
  FFirmwareBinFilePath:='';
  VerbindungHalten_Cfg:=0;  { Standard: Verbindung nicht halten }
  FXMLResponseEncodeRohdaten:=0;  { Standard: Keine Rohdaten in XML-Response }

  Neustart_Rufpolling:=true;
  Kommando_Rufentgegennahme:='';
  RingCount:=0;
  TickCount_LastRing:=0;
  FAktREClntVerbDataObj:=nil;
  FAktREClntVerbDataIndex:=-1;

  SendeSMSFiles:=true;  // nach Programmstart sollen evtl. noch vorhandene SMS-Files gesendet werden
  LetztSMSFilename:='';
  SMS_WdhDatum:=Round (Date);  // aktuelles Datum

  ProzessId_VerbAufbau:='';
  GeraeteTyp_Cmd:=-1;
  DSfG_TransparentModus_VerbAufbau:=true;
  TraceLog_VerbAufbau:=-1;

  StationsKennung:='';

  FillChar (MRGTimeouts, SizeOf (MRGTimeouts), 0);
  FillChar (MRGModemVersuche, SizeOf (MRGModemVersuche), 0);
  FillChar (DSfGTimeouts, SizeOf (DSfGTimeouts), 0);
  DSfG_BCCVersuche:=0;

  Rufentgegennahme_SMSEmpfang_aktiviert:=false;
  FUP_Reset_fuer_Rufpolling_OK:=false;  // FUP-Reset f�r RE noch nicht erfolgt
  Keep_COM_Open:=false;

  FSignatur_freigeschaltet:=
    FLizenzList.ReadGlobaleFunktionFromLizenzFile (FNExt_Signatur);  // 07.04.2014, WW
end;

{-------------------------------}
destructor TAbrufCmdExec.Destroy;
{-------------------------------}
begin
  // Thread der aktuellen Rufentgegennahme-Verbindung beenden und Warten bis
  // Thread beendet ist:
  Terminate_AktREClntVerbindung_Thread (true);  // 14.03.2024, WW

  Free_Abruf;
  Free_CommObject;
  ComLogFile.Free;
  inherited Destroy;
end;

{------------------------------------------------------------}
procedure TAbrufCmdExec.SetDebugCOMProtokoll (Value: boolean);
{------------------------------------------------------------}
{ COM-Protokoll aktivieren/deaktivieren;
  �bergabe: true = aktivieren }
begin
  FDebugCOMProtokoll:=Value;

  if FDebugCOMProtokoll then begin  // COM-Logfile ein
    if not Assigned (ComLogFile) then
      ComLogFile:=TComLogFile.Create (PathServer.Pathname[WLogDir],
                                      FCOMNr, false, '', FCOMNr_Kommando);  // 21.04.2015, WW
  end
  else begin  // COM-Logfile aus
    if Assigned (ComLogFile) then
      FreeAndNil (ComLogFile);
  end;
  if Assigned (CommObj) then  // 28.05.2013, WW
    CommObj.ComLogFile:=ComLogFile;
end;

{-----------------------------------------------------------------------}
procedure TAbrufCmdExec.Init_FehlerGruppeCode (var Fehlergruppe: integer;
                                               var Fehlercode: integer);
{-----------------------------------------------------------------------}
{ Fehlergruppe und Fehlercode mit "OK" vorbelegen;
  �bergabe/R�ckgabe: AFehlergruppe
                     AFehlercode }
begin
  Fehlergruppe:=0;
  Fehlercode:=0;
end;

{--------------------------------------------------------------------------------------}
procedure TAbrufCmdExec.Pruefe_auf_FUP_F2_Antwort (AFehlergruppe, AFehlercode: integer);
{--------------------------------------------------------------------------------------}
{ pr�ft auf F2-Antwort des FUP und inkrementiert, wenn der Fall, den F2-Z�hler }
begin
  if (AFehlergruppe = COM_FUPERROR) AND (AFehlercode = FUPERR_F2) then
    inc (FUP_F2_Count)
  else
    FUP_F2_Count:=0;
end;

{-----------------------------------------------------------------------------------}
function TAbrufCmdExec.GetGPRSConnection (Kennung: string;
                                          var Fehlergruppe: integer;
                                          var Fehlercode: integer): TCustomWinSocket;
{-----------------------------------------------------------------------------------}
{ Sucht GPRS-Verbindung f�r Kennung;
  �bergabe: Kennung
  R�ckgabe: Fehlergruppe
            Fehlercode
  Ergebnis: Zeiger auf aktive GPRS-Verbindung zu Kennung (nil, wenn keine Verbindung
            besteht) }
var
  Erg: integer;
  GPRSClientSocket: TCustomWinSocket;
  sGPRS_IPAdr: string;
  i: integer;

begin
  Result:=nil;

  if GPRSVerbindungenListe <> nil then begin
    { IP-Adresse der GPRS-Verbindung zu Kennung ermitteln: }
    Erg:=GPRSVerbindungenListe.GetAktiveVerbindungByKennung (Kennung, sGPRS_IPAdr);
    case Erg of
      -1: begin  // Mehrere aktive GPRS-Verbindungen f�r Kennung vorhanden, kein Datenabruf
            Fehlergruppe:=COM_GPRS_ERROR;
            Fehlercode:=GPRSERR_KENNUNG_MEHRFACH_KEIN_ABRUF;
            exit;
          end;
      -2: begin  // Keine GPRS-Verbindung vorhanden
            Fehlergruppe:=COM_GPRS_ERROR;
            Fehlercode:=GPRSERR_KEINE_VERBINDUNG;
            exit;
          end;
    end;

    GPRSClientSocket:=nil;
    if length (sGPRS_IPAdr) > 0 then begin
      { Socket der Verbindung zu GPRS-IP-Adresse ermitteln: }
      if GPRSServerSocket <> nil then begin
        if GPRSServerSocket.Socket.ActiveConnections > 0 then begin
          for i:=GPRSServerSocket.Socket.ActiveConnections-1 downto 0 do begin
            GPRSClientSocket:=GPRSServerSocket.Socket.Connections [i];
            if GPRSClientSocket <> nil then
              if (GPRSClientSocket.RemoteAddress = sGPRS_IPAdr) then
                Break;  { GPRS-Connection gefunden }
            GPRSClientSocket:=nil;
          end; { for }
        end;
      end;
    end;

    if Assigned (GPRSClientSocket) then
      if GPRSClientSocket.Connected then
        Result:=GPRSClientSocket;  // GPRS-Verbindung vorhanden
  end;

  if not Assigned (Result) then begin  // Keine GPRS-Verbindung vorhanden
    Fehlergruppe:=COM_GPRS_ERROR;
    Fehlercode:=GPRSERR_KEINE_VERBINDUNG;
  end;
end;

{---------------------------------------------------------}
function TAbrufCmdExec.Check_NeueREClntVerbindung: boolean;
{---------------------------------------------------------}
{ Pr�fen auf neue TCP/IP-Rufentgegennahme-Verbindung;
  Ergebnis: true, wenn neue Verbindung vorhanden }
var
  REClntVerbDataObj: TREClntVerbDataObj;

begin
  Result:=false;
  // Thread der aktuellen Rufentgegennahme-Verbindung beenden:
  Terminate_AktREClntVerbindung_Thread (false);  // 14.03.2024, WW

  if Assigned (FREClntVerbList) then begin
    // Liegt eine neue RE-Verbindung vor ?
    REClntVerbDataObj:=FREClntVerbList.GetNeueVerbindung (FCOMNr);
    if Assigned (REClntVerbDataObj) then begin  // neue RE-Verbindung liegt vor
      // Objekt und Listenindex der neuen RE-Verbindung den Klassen-internen
      // Variablen zuweisen:
      FAktREClntVerbDataObj:=REClntVerbDataObj;
      FAktREClntVerbDataIndex:=FAktREClntVerbDataObj.Data.ListIndex;  // 14.03.2024, WW
      Result:=true;
    end;
  end;
end;

// 14.03.2024, WW
{-------------------------------------------------------------------------------}
procedure TAbrufCmdExec.Terminate_AktREClntVerbindung_Thread (bWaitFor: boolean);
{-------------------------------------------------------------------------------}
{ Thread der aktuellen Rufentgegennahme-Verbindung beenden;
  �bergabe: Flag 'Warten bis Thread beendet ist' ja/nein }
begin
  // Liegt eine aktuell in Bearbeitung befindliche RE-Verbindung vor ?
  if FAktREClntVerbDataIndex > -1 then begin
    // Den zugeh�rigen Rufentgegennahme-Thread der RE-Verbindung beenden:
    if Assigned (FAktREClntVerbDataObj) then begin
      with FAktREClntVerbDataObj.Data.Thread do begin
        Resume;     // Thread fortsetzen, falls er unterbrochen ist (sonst reagiert er nicht)
        Terminate;  // Thread-Beenden einleiten
        try
          if bWaitFor then
            WaitFor;  // Warten bis Thread beendet ist
        except
          // Ung�ltiges Handle unterdr�cken
        end;
      end;

      if FAbrufLogFile <> nil then
        FAbrufLogFile.Write ('Ruf-Thread ' + IntToStr (FAktREClntVerbDataIndex + 1) +
                             ': Terminate');  { Logfile-Protokollierung }
    end else
      if FAbrufLogFile <> nil then
        FAbrufLogFile.Write ('Ruf-Thread ' + IntToStr (FAktREClntVerbDataIndex + 1) +
                             ': Not assigned (Terminate)', true, lt_Error);  { Logfile-Protokollierung }
    FAktREClntVerbDataIndex:=-1;  // Listenindex der aktuellen RE-Verbindung l�schen
  end;
end;

{----------------------------------------------------------------------------}
function TAbrufCmdExec.Init_MRG_CommObject (overIP: boolean;
                                            sGPRS_Kennung: string;
                                            doFUP_Reset: boolean;
                                            var Fehlergruppe: integer;
                                            var Fehlercode: integer): boolean;
{----------------------------------------------------------------------------}
{ serielle Schnittstelle f�r MRG-Kommunikation initialisieren und �ffnen;
  �bergabe: Flag 'overIP' (true: TCP/IP; false: seriell)
            GPRS-Kennung (nicht leer: auf GPRS-Verbindung aufschalten, falls vorhanden)
            Flag 'doFUP_Reset' (true: FUP-Reset durchf�hren)
  R�ckgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn OK }
var
  Erg: integer;
  MRGModemTimeouts: TMRGModemTimeouts;
  GPRSSocket: TCustomWinSocket;

begin
  if Assigned (CommObj) then begin
    { Pr�fen, ob aktuell verwendetes Kommunikations-Objekt g�ltig ist: }
    if overIP then begin
      if length (sGPRS_Kennung) > 0 then begin  // Aufschalten auf GPRS-Verbindung
        // keinerlei Kommunikationsobjekt wiederverwendbar (vom Typ nicht passend bzw.
        // TMRGGPRSCommObj tr�gt Verbindung zu einer Gegenstelle)
        Free_CommObject;
      end
      else begin
        if not (CommObj is TMRGClientSocketCommObj) then  // nur TMRGClientSocketCommObj wiederverwendbar
          Free_CommObject;
      end;
    end
    else begin
      { Pr�fen, ob aktuell verwendetes Kommunikations-Objekt g�ltig ist: }
      if UpperCase (Device) = UpperCase (Devices [devFUP]) then begin
        if not (CommObj is TMRGFupCommObj) then
          Free_CommObject;  // nur TMRGFupCommObj wiederverwendbar
      end
      else if UpperCase (Device) = UpperCase (Devices [devModem]) then begin
        if not (CommObj is TMRGModemCommObj) then
          Free_CommObject;  // nur TMRGModemCommObj wiederverwendbar
      end
      else begin
        if not (CommObj is TMRGSerialCommObj) then
          Free_CommObject;  // nur TMRGSerialCommObj wiederverwendbar
      end;
    end;
  end;

  if not Assigned (CommObj) then begin
    Result:=false;
    if overIP then begin
      if length (sGPRS_Kennung) > 0 then begin  // Aufschalten auf GPRS-Verbindung
        { GPRS-Verbindung f�r Kennung suchen: }
        GPRSSocket:=GetGPRSConnection (sGPRS_Kennung, Fehlergruppe, Fehlercode);
        if not Assigned (GPRSSocket) then exit;  // Keine GPRS-Verbindung vorhanden

        exit;  // GPRS nicht mehr unterst�tzt; 03.06.2020, WW
(*      auskommentiert, GPRS nicht mehr unterst�tzt
        { MRG-Socketobjekt mit GPRS-Verbindung initialisieren: }
        CommObj:=TMRGGPRSCommObj.Create (GPRSSocket,
                                         PathServer.PathName [WWorkDir],
                                         ComLogFile,
                                         GPRSVerbindungenListe,
                                         GPRSTelegrammListe);
        TMRGGPRSCommObj (CommObj).CBGPRSData_Ausgabe:=GPRSData_AusgabeProc;
        if ComLogFile <> nil then
          ComLogFile.WriteTCPIP_Msg ('GPRS-Verbindung ist ge�ffnet',
                                      GPRSSocket.RemoteHost,  // 25.02.2020, WW
                                      GPRSSocket.RemoteAddress, GPRSSocket.RemotePort);

        { "Pull-Aktiv"-Flag in GPRS-Verbindungenliste setzen: }
        if Assigned (GPRSVerbindungenListe) then
          GPRSVerbindungenListe.SetPull_Aktiv (GPRSSocket.RemoteAddress, true); *)
      end
      else begin
        { MRG-Clientsocketobjekt f�r aktiven Verbindungsaufbau initialisieren: }
        CommObj:=TMRGClientSocketCommObj.Create (PathServer.PathName [WWorkDir],
                                                 ComLogFile);
      end;

      { aus INI gelesene Timeout- und Versuche-Einstellungen an MRG-IP-
        Kommunikationsobjekt �bergeben: }
      with MRGModemTimeouts do begin
        ModemAntwort:=MRGTimeouts.ModemAntwort;
        CRCCheck:=MRGTimeouts.CRCCheck;
        ACK01ProtMeldung:=MRGTimeouts.ACK01_ProtMeldung;
      end;
      TMRGCustomCommObj (CommObj).SetTimeouts (MRGModemTimeouts);
      TMRGCustomCommObj (CommObj).SetVersuche (MRGModemVersuche);
    end
    else begin
      { Schnittstelle initialisieren: }
      if UpperCase (Device) = UpperCase (Devices [devFUP]) then begin  // f�r FUP-Abruf
        CommObj:=TMRGFupCommObj.Create (nil, PathServer.PathName [WWorkDir], ComLogFile);
        { aus INI gelesene Timeout-Einstellungen an MRG-FUP-Kommunikationsobjekt
          �bergeben: }
        TMRGFupCommObj (CommObj).SetTimeout_FupAntwort (MRGTimeouts.FupAntwort);
        { FUP-Kommunikation mit/ohne FUP-Handshake: }
        TMRGFupCommObj (CommObj).SetWithFupHandshake (FupHandshake);  // 05.10.2006
        { Schnittstelle �ffnen: }
        Erg:=TMRGFupCommObj (CommObj).Connect (FCOMNr, C_BaudMRGStandard, db_8, none, sb_1);
      end
      else if UpperCase (Device) = UpperCase (Devices [devModem]) then begin  // f�r Modem-Abruf
        CommObj:=TMRGModemCommObj.Create (nil, PathServer.PathName [WWorkDir], ComLogFile);
        { aus INI gelesene Timeout- und Versuche-Einstellungen an MRG-Modem-
          Kommunikationsobjekt �bergeben: }
        with MRGModemTimeouts do begin
          ModemAntwort:=MRGTimeouts.ModemAntwort;
          CRCCheck:=MRGTimeouts.CRCCheck;
          ACK01ProtMeldung:=MRGTimeouts.ACK01_ProtMeldung;
        end;
        TMRGCustomCommObj (CommObj).SetTimeouts (MRGModemTimeouts);
        TMRGCustomCommObj (CommObj).SetVersuche (MRGModemVersuche);
        TMRGModemCommObj (CommObj).MaxModemBaudrate:=
          TMRGModemCommObj (CommObj).Serial.KonvertBaudrate (MaxBaudrate);
        { Schnittstelle �ffnen: }
        Erg:=TMRGModemCommObj (CommObj).Connect (FCOMNr, C_BaudMRGStandard, db_7, even, sb_1);
      end
      else begin  // f�r serielle Auslesung
        CommObj:=TMRGSerialCommObj.Create (nil, PathServer.PathName [WWorkDir], ComLogFile);
        { aus INI gelesene Timeout- und Versuche-Einstellungen an MRG-Modem-
          Kommunikationsobjekt �bergeben: }
        with MRGModemTimeouts do begin
          ModemAntwort:=0;   // nicht verwendet
          CRCCheck:=MRGTimeouts.CRCCheck;
          ACK01ProtMeldung:=MRGTimeouts.ACK01_ProtMeldung;
        end;
        TMRGCustomCommObj (CommObj).SetTimeouts (MRGModemTimeouts);
        TMRGCustomCommObj (CommObj).SetVersuche (MRGModemVersuche);
        { Schnittstelle �ffnen: }
        Erg:=TMRGSerialCommObj (CommObj).Connect (FCOMNr, C_BaudMRGStandard, db_7, even, sb_1);
      end;

      case Erg of
        -1: begin
              Fehlergruppe:=COM_PORTERROR;
              Fehlercode:=COMPORTERR_NICHTVORHANDEN;
              exit;
            end;
        -2: begin
              Fehlergruppe:=COM_PORTERROR;
              Fehlercode:=COMPORTERR_OEFFNEN;
              exit;
            end;
      end;

      { FUP-Reset nach dem �ffnen der Schnittstelle durchf�hren: }
      if (UpperCase (Device) = UpperCase (Devices [devFup])) AND doFUP_Reset then begin
        if not Fup_Reset (TMRGFupCommObj (CommObj), PathServer.Pathname [WNetProgDir],
                          MRGTimeouts.FupReset, MRGTimeouts.FupAntwort, false,
                          Fehlergruppe, Fehlercode) then exit;
      end;
    end;
  end;  { if not Assigned (CommObj) }
  Result:=true;
end;

{-----------------------------------------------------------------------------}
function TAbrufCmdExec.Init_DSfG_CommObject (overIP: boolean;
                                             sGPRS_Kennung: string;
                                             bREClntThread: boolean;
                                             var Fehlergruppe: integer;
                                             var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------}
{ DSfG-Kommunikations-Objekt (f�r serielle Schnittstelle oder TCP/IP)
  initialisieren und �ffnen;
  �bergabe: Flag 'overIP' (true: TCP/IP; false: seriell)
            GPRS-Kennung (nicht leer: auf GPRS-Verbindung aufschalten, falls vorhanden)
            Flag 'bREClntThread' (true: TCP/IP-Rufentgegennahme �ber Client-Thread)
  R�ckgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn OK }
var
  Erg: integer;
  GPRSSocket: TCustomWinSocket;

begin
  if Assigned (CommObj) then begin
    { Pr�fen, ob aktuell verwendetes Kommunikations-Objekt g�ltig ist: }
    if overIP then begin
      if length (sGPRS_Kennung) > 0 then begin  // Aufschalten auf GPRS-Verbindung
        // keinerlei Kommunikationsobjekt wiederverwendbar (vom Typ nicht passend bzw.
        // TDSfGGPRSCommObj tr�gt Verbindung zu einer Gegenstelle)
        Free_CommObject;
      end
      else begin
        if bREClntThread then begin  // 08.01.2018, WW
          // keinerlei Kommunikationsobjekt wiederverwendbar (vom Typ nicht passend bzw.
          // TDSfGREClntThreadCommObj tr�gt noch Zeiger auf Rufentgegennahme-Thread-Kommandoliste)
          Free_CommObject;      
        end
        else begin
          if not (CommObj is TDSfGClientSocketCommObj) then  // nur TDSfGClientSocketCommObj wiederverwendbar
            Free_CommObject;
        end;
      end;
    end
    else begin
      if not (CommObj is TDSfGModemCommObj) then  // nur TDSfGModemCommObj wiederverwendbar
        Free_CommObject;
    end;
  end;

  if not Assigned (CommObj) then begin
    Result:=false;
    if overIP then begin
      if length (sGPRS_Kennung) > 0 then begin  // Aufschalten auf GPRS-Verbindung
        { GPRS-Verbindung f�r Kennung suchen: }
        GPRSSocket:=GetGPRSConnection (sGPRS_Kennung, Fehlergruppe, Fehlercode);
        if not Assigned (GPRSSocket) then exit;  // Keine GPRS-Verbindung vorhanden

        exit;  // GPRS nicht mehr unterst�tzt; 03.06.2020, WW
(*      auskommentiert, GPRS nicht mehr unterst�tzt
        { DSfG-Socketobjekt mit GPRS-Verbindung initialisieren, GPRS-Telegrammliste
          f�r empfangene Push-Telegramme �bergeben: }
        CommObj:=TDSfGGPRSCommObj.Create (GPRSSocket,
                                          PathServer.PathName [WWorkDir],
                                          ComLogFile, GPRSVerbindungenListe,
                                          GPRSTelegrammListe);
        TDSfGGPRSCommObj (CommObj).CBGPRSData_Ausgabe:=GPRSData_AusgabeProc;
        if ComLogFile <> nil then
          ComLogFile.WriteTCPIP_Msg ('GPRS-Verbindung ist ge�ffnet',
                                      GPRSSocket.RemoteHost,  // 25.02.2020, WW
                                      GPRSSocket.RemoteAddress, GPRSSocket.RemotePort);

        { "Pull-Aktiv"-Flag in GPRS-Verbindungenliste setzen: }
        if Assigned (GPRSVerbindungenListe) then
          GPRSVerbindungenListe.SetPull_Aktiv (GPRSSocket.RemoteAddress, true); *)
      end
      else begin
        if bREClntThread then begin  // 08.01.2018, WW
          { DSfG-Kommunikationsobjekt f�r Rufentgegennahme-Thread mit
            Kommandoliste des angenommenen Anrufs initialisieren: 08.01.2018, WW }
          CommObj:=TDSfGREClntThreadCommObj.Create (FAktREClntVerbDataObj.Data.KommandoListe,
                                                    AbrufLogFile);
        end
        else begin
          { DSfG-Clientsocketobjekt f�r aktiven Verbindungsaufbau initialisieren: }
          CommObj:=TDSfGClientSocketCommObj.Create (PathServer.PathName [WWorkDir],
                                                    ComLogFile);
        end;
      end;

      { aus INI gelesene Versuche-Einstellungen f�r DSfG-Kommunikation
        an DSfG-Socketobjekt �bergeben: }
      TDSfGCommObj (CommObj).SetVersuche (DSfG_BCCVersuche);
    end
    else begin
      { Schnittstelle initialisieren: }
      CommObj:=TDSfGModemCommObj.Create (nil, PathServer.PathName [WWorkDir], ComLogFile);
      { aus INI gelesene Timeout- und Versuche-Einstellungen f�r DSfG-Kommunikation
        an SerialDSfG �bergeben: }
      TDSfGModemCommObj (CommObj).SetTimeouts_Versuche (DSfGTimeouts.ModemAntwort, DSfG_BCCVersuche);
      { Schnittstelle �ffnen: }
      Erg:=TDSfGModemCommObj (CommObj).Connect (FCOMNr, MaxBaudrate, db_8, none, sb_1);
      case Erg of
        -1: begin
              Fehlergruppe:=COM_PORTERROR;
              Fehlercode:=COMPORTERR_NICHTVORHANDEN;
              exit;
            end;
        -2: begin
              Fehlergruppe:=COM_PORTERROR;
              Fehlercode:=COMPORTERR_OEFFNEN;
              exit;
            end;
      end;
    end;
  end;  { if not Assigned (CommObj) }
  Result:=true;
end;

{----------------------------------------------------------------------------}
function TAbrufCmdExec.Init_MRG_Abruf (overIP: boolean; sGPRS_Kennung: string;
                                       var Fehlergruppe: integer;
                                       var Fehlercode: integer): boolean;
{----------------------------------------------------------------------------}
{ MRG-Abruf initialisieren (MRG-relevante CFG lesen, COM �ffnen etc.);
  �bergabe: Flag 'overIP' (true: TCP/IP; false: seriell)
            GPRS-Kennung (nicht leer: auf GPRS-Verbindung aufschalten, falls vorhanden)
  R�ckgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Initialisieren OK }
begin
  Result:=false;
  { INI-Konfigurationen lesen: }
  ReadSrvCfg_ModemIni;
  ReadProgramIni_MRG;

  { Schnittstellen- bzw. IP-Kommunikations-Objekt initialisieren und �ffnen,
    wenn nicht bereits erfolgt: }
  if not Init_MRG_CommObject (overIP, sGPRS_Kennung, true, Fehlergruppe, Fehlercode) then exit;

  Free_Abruf;            { Abruf-Objekt und -Variablen freigeben }
  { neues Abruf-Objekt createn: }
  Abruf:=TMRGAbruf.Create (FCOMNr, FCOMNr_Kommando,
                           TMRGCommObj (CommObj), MRGTimeouts,
                           ModemName, Modem_Databits_Parity_Stopbits, KennungExt_Cfg,
                           FDebugCOMProtokoll, FRohdatenLoeschen,
                           PathServer [WStammDir], PathServer [WWorkDir],
                           PathServer [WNetProgDir], PathServer [WLogDir],
                           FSignatur_freigeschaltet, FXMLResponseEncodeRohdaten,
                           FResourceFilesList);
  Result:=true;
end;

{-----------------------------------------------------------------------------}
function TAbrufCmdExec.Init_DSfG_Abruf (overIP: boolean; sGPRS_Kennung: string;
                                        bREClntThread: boolean;
                                        var Fehlergruppe: integer;
                                        var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------}
{ DSfG-Abruf initialisieren (DSfG-relevante CFG lesen, COM �ffnen etc.);
  �bergabe: Flag 'overIP' (true: TCP/IP; false: seriell)
            GPRS-Kennung (nicht leer: auf GPRS-Verbindung aufschalten, falls vorhanden)
            Flag 'bREClntThread' (true: TCP/IP-Rufentgegennahme �ber Client-Thread)
  R�ckgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Initialisieren OK }
begin
  Result:=false;
  { INI-Konfigurationen lesen: }
  ReadSrvCfg_ModemIni;
  ReadProgramIni_DSfG;

  { DSfG-Abruf �ber serielle Schnittstelle nur mit Modem m�glich: }
  if not overIP then begin
    if UpperCase (Device) <> UpperCase (Devices [devModem]) then begin
      Fehlergruppe:=COM_PORTERROR;
      Fehlercode:= COMPORTERR_DUE_GERAET_FALSCH;
      exit;
    end;
  end;

  { Schnittstellen- bzw. IP-Kommunikations-Objekt initialisieren und �ffnen,
    wenn nicht bereits erfolgt: }
  if not Init_DSfG_CommObject (overIP, sGPRS_Kennung, bREClntThread,
                               Fehlergruppe, Fehlercode) then exit;

  Free_Abruf;            { Abruf-Objekt und -Variablen freigeben }
  { neues Abruf-Objekt createn: }
  Abruf:=TDSfGAbruf.Create (FCOMNr, FCOMNr_Kommando,
                            TDSfGCommObj (CommObj), DSfGTimeouts,
                            ModemName, FDebugCOMProtokoll, FRohdatenLoeschen,
                            PathServer [WStammDir], PathServer [WWorkDir],
                            PathServer [WNetProgDir], PathServer [WLogDir],
                            FFirmwareBinFilePath, FSignatur_freigeschaltet,
                            FXMLResponseEncodeRohdaten, FResourceFilesList, nil);
  Result:=true;
end;

{--------------------------------------}
procedure TAbrufCmdExec.Free_CommObject;
{--------------------------------------}
{ Kommunikations-Objekt freigeben }
begin
  if Assigned (CommObj) then begin
    { GPRS-Verbindung: "Pull-Aktiv"-Flag in GPRS-Verbindungenliste r�cksetzen }
    if Assigned (GPRSVerbindungenListe) then begin
      if CommObj is TMRGGPRSCommObj then      
        GPRSVerbindungenListe.SetPull_Aktiv (TMRGGPRSCommObj (CommObj).GPRSRemoteAddress, false)
      else if CommObj is TDSfGGPRSCommObj then
        GPRSVerbindungenListe.SetPull_Aktiv (TDSfGGPRSCommObj (CommObj).GPRSRemoteAddress, false);
    end;

    FreeAndNil (CommObj);  // serielle Komponente: COM wird mit dem Freigeben auch geschlossen
  end;
end;

{---------------------------------}
procedure TAbrufCmdExec.Free_Abruf;
{---------------------------------}
{ Objekte freigeben, Objekt-Variablen initialisieren }
begin
  if Abruf <> nil then
    FreeAndNil (Abruf);

  ProzessId_VerbAufbau:='';
  GeraeteTyp_Cmd:=-1;
  StationsKennung:='';
  DSfG_TransparentModus_VerbAufbau:=true;
  TraceLog_VerbAufbau:=-1;
end;

{---------------------------------------------------------------------------------------}
function TAbrufCmdExec.VerbAufbau (Kommando: string; var Verbindung_OK: boolean): string;
{---------------------------------------------------------------------------------------}
{ Verbindungsaufbau-Kommando ausf�hren;
  �bergabe: Kommando-String
  R�ckgabe: Flag "Verbindung_OK": true, wenn Verbindungsaufbau erfolgreich
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe: integer;
  Fehlercode: integer;
  VerbAufbauCmdData: TVerbAufbauCmdData;
  DSfGAufmTelegrammList: TAufmTelegrammList;
  DSfGDfueKonfigData: TDSfGDfueKonfigData;
  DSfGZeitSyncInfoData: TZeitSyncInfoData;
  DSfG_ZS_Fehlergruppe: integer;
  DSfG_ZS_Fehlercode: integer;
  isMRGVerbAufbau: boolean;
  FUP_Reset_vor_Abruf: boolean;
  overIP: boolean;
  sGPRS_Kennung: string;
  DSfGResponseLogList: TResponseLogList;
  GerTypCmd_Buf: integer;
  FwUpdateInfoData: TFwUpdateInfoData;
  VerbAutoDetectData: TVerbAutoDetectData;
  VerbInfoData: TVerbInfoData;
  sRetFreierText: string;
  ResponseTraceLogList: TDSfGDataList;
{$IFDEF GAS-X}
  MrgTyp_Wieser: integer;
  bResourceOK: boolean;
{$ENDIF}

begin
  if Abruf <> nil then begin     { es besteht noch eine Verbindung }
    Free_CommObject;   { Kommunikation wird beendet, um bestehende Verbindung zu beenden }
    Delay (2000);
  end;

  // Rundpuffer auf COM-Logfile und Abruf-Logfile ausf�hren; 03.12.2013, WW
  if FDebugRundpufferMaxBytes > 0 then begin
    if Assigned (ComLogFile) then
      ComLogFile.DoRundpuffer (FDebugRundpufferMaxBytes);
    if Assigned (FAbrufLogFile) then
      FAbrufLogFile.DoRundpuffer (FDebugRundpufferMaxBytes);
  end;

  // es besteht jetzt keine Verbindung mehr, neue Verbindung kann aufgebaut werden:
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);     // Vorbelegung f�r Fehlergruppe/-code: OK
  StationsKennung:='';
  Verbindung_OK:=false;
  sRetFreierText:='';

  DSfGDfueKonfigData.EAdr_Dfue:=NUL;  // Kennzeichnung, da� keine DSfG-DF�-Konfigdaten f�r R�ckgabe vorhanden sind
  DSfG_ZS_Fehlergruppe:=-1;           // Kennzeichnung, da� kein DSfG-ZeitSync-Ergebnis (Fehlergruppe/-code)
  DSfG_ZS_Fehlercode:=-1;             // f�r R�ckgabe vorhanden ist
  with DSfGZeitSyncInfoData do begin  // Kennzeichnung, da� keine DSfG-ZeitSync-Infodaten f�r R�ckgabe vorhanden sind
    DZ_Server:=0;
    DZ_Geraet:=0;
  end;
  with FwUpdateInfoData do begin  // Kennzeichnung, da� keine Firmware-Update-Infodaten f�r R�ckgabe vorhanden sind
    Version_neu:='';
    Build_neu:='';
  end;
  with VerbAutoDetectData do begin
    ModemTyp:='';  // Kennzeichnung, da� kein automatisch ermittelter Modemtyp vorhanden ist
    PasswortNr:=-1;  // Kennzeichnung, da� keine automatisch ermittelte Passwortnummer vorhanden ist
  end;
  with VerbInfoData do begin
    DZ_VerbindungSteht:=0;  // Kennzeichnung, da� kein 'Verbindung hergestellt'-Zeitpunkt vorhanden ist
    DZ_Login:=0;  // Kennzeichnung, da� kein 'Login erfolgt'-Zeitpunkt vorhanden ist
  end;

  VerbAufbauCmdData:=GetVerbAufbauCmdData (Kommando);

  DSfGAufmTelegrammList:=TAufmTelegrammList.Create;
  try
    DSfGResponseLogList:=TResponseLogList.Create;
    try
      // Bei aktiviertem Tracelog, die zugeh�rige Responseliste anlegen; 03.01.2022, WW
      if VerbAufbauCmdData.TraceLog = C_CmdTraceLog_Aktiviert then
        ResponseTraceLogList:=TDSfGDataList.Create
      else
        ResponseTraceLogList:=nil;
      try
        GerTypCmd_Buf:=VerbAufbauCmdData.GeraeteTyp;   // Ger�tetyp aus Client-Kommando
  {$IFDEF GAS-X}
        { die im Gas-X verwendete MRG-Typnummer in die entsprechende Wieser MRG-Typnummer
          umwandeln: }
        if not ((VerbAufbauCmdData.GeraeteTyp = C_GeraeteTypDSfG) OR (VerbAufbauCmdData.GeraeteTyp < 0)) then begin
          bResourceOK:=false;
          if Assigned (FResourceFilesList) then
            bResourceOK:=FResourceFilesList.GetMrgTypGasX (VerbAufbauCmdData.GeraeteTyp,
                                                           MrgTyp_Wieser);  // 06.08.2021, WW
          if bResourceOK then
            VerbAufbauCmdData.GeraeteTyp:=MrgTyp_Wieser
          else begin
            Fehlergruppe:=ST_KONFIGERROR;
            Fehlercode:=KFERR_KONFIGDATANOTFOUND;
          end;
        end;
  {$ENDIF}

        if (Fehlergruppe = 0) AND (Fehlercode = 0) then begin
          overIP:=FCOMNr < 0;  // IP-Abruf, wenn COMNr negativ
          if VerbAufbauCmdData.ModemTyp = srv_GPRS then  // GPRS-Abruf
            sGPRS_Kennung:=VerbAufbauCmdData.Kennung  // Kennung
          else
            sGPRS_Kennung:='';

          // MRG oder DSfG-Abruf starten (abh�ngig vom Ger�tetyp im Verbindungsaufbau-Kommando):
          if (VerbAufbauCmdData.GeraeteTyp = C_GeraeteTypDSfG) OR
             (VerbAufbauCmdData.GeraeteTyp < 0) then begin  // DSfG
            { DSfG-Ger�tetyp-Lizenz pr�fen: }
            if FLizenzList.GetLizenzMrgTyp (C_GeraeteTypDSfG) <> 0 then begin
              if Init_DSfG_Abruf (overIP, sGPRS_Kennung, false, Fehlergruppe, Fehlercode) then begin
  {$IFDEF GAS-X}
                // Rufnummer um COM-spezifische Vorwahl aus SrvCfgIni-Konfiguration
                // erweitern; 21.11.2013 WW
                if not overIP then
                  VerbAufbauCmdData.Rufnummer:=
                    Concat_Vorwahl_Rufnummer (Vorwahl_GasX, VerbAufbauCmdData.Rufnummer);
  {$ENDIF}
                // merken als "Soll-Prozess-Id" f�r alle weiteren Kommandos des DSfG-Abrufs:
                ProzessId_VerbAufbau:=GetAbrufKommandoProzessID (Kommando);
                GeraeteTyp_Cmd:=GerTypCmd_Buf;   // merken f�r XML-Antwort-R�ckgaben
                // merken f�r Abruf/-Konvertierung von DSfG-Daten (Datenelemente von
                // DSfG-Instanzen oder DSfG-DF�-Momentanwerten):
                DSfG_TransparentModus_VerbAufbau:=
                  VerbAufbauCmdData.DSfG_TransparentModus in [dtm_Ja, dtm_Immer];  // 11.07.2019, WW
                // merken f�r Tracelog-Ausgabe in XML-Antworten
                TraceLog_VerbAufbau:=VerbAufbauCmdData.TraceLog;  // 24.02.2020, WW

                // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
                if Assigned (ResponseTraceLogList) then
                  Abruf.CreateCOMTraceLog
                else
                  Abruf.FreeCOMTraceLog;
                try
                  { DSfG-Verbindung aufbauen:
                    -> mit optionaler Zeitsynchronisation der Login-DSfG-DF� }
                  Verbindung_OK:=TDSfGAbruf (Abruf).VerbAufbau (VerbAufbauCmdData,
                                                                DSfGAufmTelegrammList,
                                                                DSfGResponseLogList,
                                                                DSfGDfueKonfigData,
                                                                StationsKennung,
                                                                DSfGZeitSyncInfoData,
                                                                DSfG_ZS_Fehlergruppe,
                                                                DSfG_ZS_Fehlercode,
                                                                FwUpdateInfoData,
                                                                VerbInfoData);
                  Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);  // 27.02.2013, WW
                  sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW

                  if not Verbindung_OK then begin
                    { Fehler beim Verbindungsaufbau (Verbindung steht): }
                    if not Abruf.NoCarrier then
                      TDSfGAbruf (Abruf).VerbAbbau;
                  end;
                  // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW
                  Abruf.AddCOMTraceLogToList (ResponseTraceLogList);
                finally
                  Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
                end;
              end; { if Init_DSfG_Abruf }
            end
            else begin
              // DSfG-Ger�tetyp nicht lizenziert:
              Fehlergruppe:=SYS_LICENCEERROR;
              Fehlercode:=LICENCEERR_GERAETETYP;
            end;
          end
          else begin  // MRG
            { MRG-Ger�tetyp-Lizenz pr�fen: }
            if FLizenzList.GetLizenzMrgTyp (VerbAufbauCmdData.GeraeteTyp) <> 0 then begin
              if Init_MRG_Abruf (overIP, sGPRS_Kennung, Fehlergruppe, Fehlercode) then begin
  {$IFDEF GAS-X}
                // Rufnummer um COM-spezifische Vorwahl aus SrvCfgIni-Konfiguration
                // erweitern; 21.11.2013 WW
                if not overIP then
                  VerbAufbauCmdData.Rufnummer:=
                    Concat_Vorwahl_Rufnummer (Vorwahl_GasX, VerbAufbauCmdData.Rufnummer);
  {$ENDIF}
                // merken als "Soll-Prozess-Id" f�r alle weiteren Kommandos des MRG-Abrufs:
                ProzessId_VerbAufbau:=GetAbrufKommandoProzessID (Kommando);
                GeraeteTyp_Cmd:=GerTypCmd_Buf;   // merken f�r XML-Antwort-R�ckgaben
                // Transparentmodus eigtl. nur f�r DSfG relevant, trotzdem auch hier
                // der Vollst�ndigkeit halber zuweisen:
                DSfG_TransparentModus_VerbAufbau:=
                  VerbAufbauCmdData.DSfG_TransparentModus in [dtm_Ja, dtm_Immer];  // 11.07.2019, WW
                // merken f�r Tracelog-Ausgabe in XML-Antworten
                TraceLog_VerbAufbau:=VerbAufbauCmdData.TraceLog;  // 24.02.2020, WW

                // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
                if Assigned (ResponseTraceLogList) then
                  Abruf.CreateCOMTraceLog
                else
                  Abruf.FreeCOMTraceLog;
                try
                  { Standardm��ig wird der FUP vor einem Abruf nur neu initialisiert.
                    Nach 2 aufeinanderfolgenden Abrufen mit FUP-Meldung F2 wird jedoch
                    sicherheitshalber ein FUP-Reset gemacht, sonst h�ngt der (9600er) FUP
                    eventuell und baut keine Verbindungen mehr auf ! }
                  if FUP_F2_Count >= 2 then begin
                    FUP_Reset_vor_Abruf:=true;
                    FUP_F2_Count:=0;
                  end else
                    FUP_Reset_vor_Abruf:=false;

                  { MRG-Verbindung aufbauen: }
                  Verbindung_OK:=TMRGAbruf (Abruf).VerbAufbau (VerbAufbauCmdData,
                                                               FUP_Reset_vor_Abruf,
                                                               StationsKennung,
                                                               VerbAutoDetectData,
                                                               VerbInfoData);
                  Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
                  sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW

                  { Ergebnis des Verbindungsaufbaus auf FUP-F2-Antwort pr�fen: }
                  Pruefe_auf_FUP_F2_Antwort (Fehlergruppe, Fehlercode);

                  if not Verbindung_OK then begin
                    { Fehler beim Verbindungsaufbau (Verbindung steht) bzw. immer bei FUP-Abruf: }
                    if not Abruf.NoCarrier OR TMRGAbruf (Abruf).isFupAbruf then
                      TMRGAbruf (Abruf).VerbAbbau;
                  end;

                  Neustart_Rufpolling:=true;  { Status: Start eines MRG-Abruf kennzeichnet den
                                                        Neustart des optionalen Ruf-Pollings
                                                        mit FUP/Modem-Initialisierung nach dem Abruf }
                  // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW
                  Abruf.AddCOMTraceLogToList (ResponseTraceLogList);
                finally
                  Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
                end;
              end;  { if Init_MRG_Abruf }
            end
            else begin
              // MRG-Ger�tetyp nicht lizenziert:
              Fehlergruppe:=SYS_LICENCEERROR;
              Fehlercode:=LICENCEERR_GERAETETYP;
            end;
          end;
        end;  { if (Fehlergruppe = 0) AND (Fehlercode = 0) then }

        { Logfile-Protokollierung von Fehlergruppe/-code: }
        if FAbrufLogFile <> nil then
          FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

        { Antwort f�r Client: }
        isMRGVerbAufbau:=true;
        if Abruf <> nil then begin
          if Abruf is TDSfGAbruf then
            isMRGVerbAufbau:=false;
        end;

        if isMRGVerbAufbau then
          Result:=GetClnt_XMLAntwort_VerbAufbauMRG (Fehlergruppe, Fehlercode, Kommando,
                                                    StationsKennung, GeraeteTyp_Cmd,
                                                    ResponseTraceLogList,
                                                    VerbAutoDetectData,
                                                    VerbInfoData,
                                                    sRetFreierText,
                                                    FDebugServiceIO, PathServer.PathName [WWorkDir])
        else
          { DSfG-Verbindungsaufbau mit R�ckgabe der DSfG-DF�-Konfigurationsdaten: }
          Result:=GetClnt_XMLAntwort_VerbAufbauDSfG (Fehlergruppe, Fehlercode, Kommando,
                                                     StationsKennung, DSfGAufmTelegrammList,
                                                     DSfGDfueKonfigData, DSfGZeitSyncInfoData,
                                                     DSfG_ZS_Fehlergruppe, DSfG_ZS_Fehlercode,
                                                     DSfGResponseLogList,
                                                     ResponseTraceLogList,
                                                     FwUpdateInfoData,
                                                     VerbInfoData,
                                                     sRetFreierText,
                                                     FDebugServiceIO, PathServer.PathName [WWorkDir]);
      finally
        ResponseTraceLogList.Free;
      end;
    finally
      DSfGResponseLogList.Free;
    end;
  finally
    DSfGAufmTelegrammList.Free;
  end;

  { Abruf-Objekt freigeben, wenn keine Verbindung zustande kam: }
  if not Verbindung_OK then
    Free_Abruf;
end;

{----------------------------------------------------------}
function TAbrufCmdExec.VerbAbbau (Kommando: string): string;
{----------------------------------------------------------}
{ Verbindungsabbau-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ProzessId: string;
  sRetFreierText: string;
  ResponseTraceLogList: TDSfGDataList;

begin
  sRetFreierText:='';

  // Bei aktiviertem Tracelog, die zugeh�rige Responseliste anlegen; 03.01.2022, WW
  if TraceLog_VerbAufbau = C_CmdTraceLog_Aktiviert then
    ResponseTraceLogList:=TDSfGDataList.Create
  else
    ResponseTraceLogList:=nil;
  try
    if Abruf <> nil then begin
      ProzessId:=GetAbrufKommandoProzessID (Kommando);

      // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
      if (ProzessId = ProzessId_VerbAufbau) OR
         (ProzessId_VerbAufbau = '') then begin  // f�r entgegengenommenen Ruf; 14.03.2024, WW
        Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);     // Vorbelegung f�r Fehlergruppe/-code: OK

        // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
        if Assigned (ResponseTraceLogList) then
          Abruf.CreateCOMTraceLog
        else
          Abruf.FreeCOMTraceLog;
        try
          if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
            if not TMRGAbruf (Abruf).VerbAbbau then
              Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
          end
          else begin                                                 { DSfG-Abruf }
            if not TDSfGAbruf (Abruf).VerbAbbau then
              Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
          end;
          sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
          // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW
          Abruf.AddCOMTraceLogToList (ResponseTraceLogList);
        finally
          Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
        end;
      end
      else begin
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
      end;
    end
    else begin
      // R�ckgabe: OK, Verbindung beendet
      Fehlergruppe:=0;
      Fehlercode:=0;
    end;

    { Logfile-Protokollierung von Fehlergruppe/-code: }
    if FAbrufLogFile <> nil then
      FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

    { Antwort f�r Client: }
    Result:=GetClnt_XMLAntwort_VerbAbbau (Fehlergruppe, Fehlercode, Kommando,
                                          StationsKennung,
                                          ResponseTraceLogList,
                                          sRetFreierText,
                                          FDebugServiceIO, PathServer.PathName [WWorkDir]);  // 03.01.2022, WW
  finally
    ResponseTraceLogList.Free;
  end;

  { Abruf-Objekt freigeben nach Beendigung des Abrufs: }
  Free_Abruf;
end;

{-------------------------------------------------------------------------------------}
function TAbrufCmdExec.MessAbruf (Kommando: string;
                                  StaKanalKonvDataList: TStaKanalKonvDataList): string;
{-------------------------------------------------------------------------------------}
{ Messwertabruf-Kommando ausf�hren;
  �bergabe: Kommando-String
            Liste mit Sta-Kanaldaten
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  MessAbrufCmdData: TMessMeldPruefAbrufCmdData;
  MessFilenameListe: TStringList;
  TagFilenameListe: TStringList;
  MaxMessKanal: integer;
  MaxTagKanal: integer;
  MrgDefData: TMrgDefData;
  DSfGArchivdatenListe: TDSfGDataList;
  isMRGAntwort: boolean;
  Filename: string;
  i: integer;
  ProzessId: string;
  DSfGResponseLogList: TResponseLogList;
  Tagesende: integer;
  LGZnorm_Daten: boolean;
  DSfGResponseRohdatenList: TDSfGDataList;
  iMaxMessKanal_Abruf: integer;
  sRetFreierText: string;
  ResponseTraceLogList: TDSfGDataList;
  bResourceOK: boolean;
  iMinMessKanalExt: integer;
  iMaxMessKanalExt: integer;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);   // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  { Vorbelegungen f�r MRG-Messwertabruf }
  MaxMessKanal:=0;
  MaxTagKanal:=0;
  iMinMessKanalExt:=-1;
  iMaxMessKanalExt:=-1;
  Tagesende:=CTagesende;  // Default-Tagesende

  MessFilenameListe:=TStringList.Create;
  try
    TagFilenameListe:=TStringList.Create;
    try
      DSfGArchivdatenListe:=TDSfGDataList.Create;
      try
        DSfGResponseLogList:=TResponseLogList.Create;
        try
          DSfGResponseRohdatenList:=TDSfGDataList.Create;
          try
            // Bei aktiviertem Tracelog, die zugeh�rige Responseliste anlegen; 24.02.2020, WW
            if TraceLog_VerbAufbau = C_CmdTraceLog_Aktiviert then
              ResponseTraceLogList:=TDSfGDataList.Create
            else
              ResponseTraceLogList:=nil;
            try
              if Abruf <> nil then begin
                MessAbrufCmdData:=GetMessMeldPruefAbrufCmdData (Kommando);
                ProzessId:=GetAbrufKommandoProzessID (Kommando);

                // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
                if ProzessId = ProzessId_VerbAufbau then begin
                  // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
                  if Assigned (ResponseTraceLogList) then
                    Abruf.CreateCOMTraceLog
                  else
                    Abruf.FreeCOMTraceLog;
                  try
                    if Abruf is TMRGAbruf then begin                          { MRG-Abruf }
                      // MRG-Messwerte und -Tagess�tze abrufen:
                      TMRGAbruf (Abruf).AbrufMessTagWerte (MessAbrufCmdData,
                                                           StaKanalKonvDataList,
                                                           GasX_AIX_kompatibel_MRG_Analog_Cfg,
                                                           Tagesende,
                                                           MessFilenameListe,
                                                           TagFilenameListe,
                                                           iMaxMessKanal_Abruf,
                                                           iMinMessKanalExt,
                                                           iMaxMessKanalExt);
                      Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
                      sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
                      // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW }
                      Abruf.AddCOMTraceLogToList (ResponseTraceLogList);

                      // ger�tespezifische Kanalzahlen f�r Messwert- und Z�hlerstandsdaten aus Konfiguration lesen
                      bResourceOK:=false;
                      if Assigned (FResourceFilesList) then
                        bResourceOK:=FResourceFilesList.GetMrgDefData (
                          TMRGAbruf (Abruf).Geraetetyp, MrgDefData);  // 06.08.2021, WW
                      if bResourceOK then begin
                        // Sofern verf�gbar, �bersteuert die dynamisch per Abruf/Konvertierung
                        // ermittelte Anzahl von Messwert-Kan�len die in der
                        // Ressource hinterlegte, statische Information:
                        if iMaxMessKanal_Abruf > -1 then
                          MaxMessKanal:=iMaxMessKanal_Abruf  // 21.02.2017, WW
                        else
                          MaxMessKanal:=MrgDefData.AnzahlKanaele;
                        MaxTagKanal:=MrgDefData.AnzahlZaehlerkanaele;
                      end
                      else begin
                        Fehlergruppe:=ST_KONFIGERROR;
                        Fehlercode:=KFERR_KONFIGDATANOTFOUND;
                      end;
                    end
                    else begin                                               { DSfG-Abruf }
                      // DSfG-Archive abrufen:
                      if not TDSfGAbruf (Abruf).AbrufDaten (C_IsArchive,
                                                            MessAbrufCmdData.vonDZ, MessAbrufCmdData.bisDZ,
                                                            MessAbrufCmdData.vonOrdNr, MessAbrufCmdData.bisOrdNr,
                                                            MessAbrufCmdData.DSfG_AdresslistData,
                                                            DSfGArchivdatenListe, nil,
                                                            DSfGResponseLogList,
                                                            DSfGResponseRohdatenList,
                                                            ResponseTraceLogList,
                                                            -1) then
                        Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

                      sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
                    end;
                  finally
                    Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
                  end;
                end
                else begin
                  Fehlergruppe:=SYS_ABRUFERROR;
                  Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
                end;
              end
              else begin
                Fehlergruppe:=COM_KOMMERROR;
                Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
              end;

              { Logfile-Protokollierung von Fehlergruppe/-code: }
              if FAbrufLogFile <> nil then
                FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

              { XML-Antwort f�r Client: }
              isMRGAntwort:=true;
              if Abruf <> nil then begin
                if Abruf is TDSfGAbruf then
                  isMRGAntwort:=false;
              end;

              if isMRGAntwort then begin                { Antwort mit MRG-Daten liefern }
                { ob normierte LGZ-Daten oder Originalwerte zur�ckgeliefert werden
                  sollen, steht im Messwert-Abrufkommando (nur f�r Wieser-Abruf): }
                LGZnorm_Daten:=MessAbrufCmdData.ArchivdatenTyp = C_CmdArchDatenTyp_LGZnorm;
                Result:=GetClnt_XMLAntwort_MWTA (Fehlergruppe, Fehlercode, Kommando,
                                                 StationsKennung, GeraeteTyp_Cmd,
                                                 MessFilenameListe, TagFilenameListe,
                                                 MaxMessKanal, MaxTagKanal,
                                                 iMinMessKanalExt, iMaxMessKanalExt,
                                                 Tagesende,
                                                 ResponseTraceLogList,
                                                 MessAbrufCmdData.vonDZ, MessAbrufCmdData.bisDZ,
                                                 LGZnorm_Daten, sRetFreierText,
                                                 FDebugServiceIO, PathServer.PathName [WWorkDir]);

                { MRG-Datenfiles k�nnen jetzt gel�scht werden: }
                for i:=0 to MessFilenameListe.Count-1 do begin  { alle Messwert-Dateien }
                  Filename:=MessFilenameListe [i];
                  if length (Filename) > 0 then
                    DeleteFile (Filename);
                end;
                for i:=0 to TagFilenameListe.Count-1 do begin   { alle Tagessatz-Dateien }
                  Filename:=TagFilenameListe [i];
                  if length (Filename) > 0 then
                    DeleteFile (Filename);
                end;
              end
              else begin                               { Antwort mit DSfG-Daten liefern }
                Result:=GetClnt_XMLAntwort_AR (Fehlergruppe, Fehlercode, Kommando,
                                               StationsKennung, DSfGArchivdatenListe,
                                               DSfGResponseLogList, DSfGResponseRohdatenList,
                                               ResponseTraceLogList,
                                               MessAbrufCmdData.vonDZ, MessAbrufCmdData.bisDZ,
                                               false, -1,
                                               sRetFreierText,
                                               FDebugServiceIO, PathServer.PathName [WWorkDir],
                                               ISO646_Cfg);

                { DSfG-Datenfiles k�nnen jetzt gel�scht werden: }
                for i:=0 to DSfGArchivdatenListe.Count-1 do begin
                  Filename:=DSfGArchivdatenListe [i];
                  if length (Filename) > 0 then
                    DeleteFile (Filename);
                end;

                { Dateien mit kodierten Rohdaten k�nnen jetzt gel�scht werden: 07.04.2014, WW }
                if FRohdatenLoeschen then begin
                  for i:=0 to DSfGResponseRohdatenList.Count-1 do begin
                    Filename:=DSfGResponseRohdatenList [i];
                    if length (Filename) > 0 then
                      DeleteFile (Filename);
                  end;
                end;
              end;
            finally
              ResponseTraceLogList.Free;
            end;
          finally
            DSfGResponseRohdatenList.Free;
          end;
        finally
          DSfGResponseLogList.Free;
        end;
      finally
        DSfGArchivdatenListe.Free;
      end;
    finally
      TagFilenameListe.Free;
    end;
  finally
    MessFilenameListe.Free;
  end;
end;

{----------------------------------------------------------}
function TAbrufCmdExec.MeldAbruf (Kommando: string): string;
{----------------------------------------------------------}
{ Meldungsabruf-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  MeldAbrufCmdData: TMessMeldPruefAbrufCmdData;
  MRGMeldungsListe: TMeldungsListe;
  DSfGLogbuchdatenListe: TDSfGDataList;
  isMRGAntwort: boolean;
  DSfGFilename: string;
  i: integer;
  ProzessId: string;
  DSfGResponseLogList: TResponseLogList;
  DSfGResponseRohdatenList: TDSfGDataList;
  sRetFreierText: string;
  ResponseTraceLogList: TDSfGDataList;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);   // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  MRGMeldungsListe:=TMeldungsListe.Create (PathServer [WStammDir]);
  try
    DSfGLogbuchdatenListe:=TDSfGDataList.Create;
    try
      DSfGResponseLogList:=TResponseLogList.Create;
      try
        DSfGResponseRohdatenList:=TDSfGDataList.Create;
        try
          // Bei aktiviertem Tracelog, die zugeh�rige Responseliste anlegen; 03.01.2022, WW
          if TraceLog_VerbAufbau = C_CmdTraceLog_Aktiviert then
            ResponseTraceLogList:=TDSfGDataList.Create
          else
            ResponseTraceLogList:=nil;
          try
            if Abruf <> nil then begin
              MeldAbrufCmdData:=GetMessMeldPruefAbrufCmdData (Kommando);
              ProzessId:=GetAbrufKommandoProzessID (Kommando);

              // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
              if ProzessId = ProzessId_VerbAufbau then begin
                // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
                if Assigned (ResponseTraceLogList) then
                  Abruf.CreateCOMTraceLog
                else
                  Abruf.FreeCOMTraceLog;
                try
                  if Abruf is TMRGAbruf then begin                          { MRG-Abruf }
                    // MRG-Meldungen abrufen:
                    TMRGAbruf (Abruf).AbrufMeldungen (MeldAbrufCmdData,
                                                      MRGMeldungsListe);
                    Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
                    // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW
                    Abruf.AddCOMTraceLogToList (ResponseTraceLogList);
                  end
                  else begin                                               { DSfG-Abruf }
                    // DSfG-Logb�cher abrufen:
                    if not TDSfGAbruf (Abruf).AbrufDaten (C_IsLogbuecher,
                                                          MeldAbrufCmdData.vonDZ, MeldAbrufCmdData.bisDZ,
                                                          MeldAbrufCmdData.vonOrdNr, MeldAbrufCmdData.bisOrdNr,
                                                          MeldAbrufCmdData.DSfG_AdresslistData,
                                                          DSfGLogbuchdatenListe, nil,
                                                          DSfGResponseLogList,
                                                          DSfGResponseRohdatenList,
                                                          ResponseTraceLogList,
                                                          -1) then
                      Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
                  end;

                  sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
                finally
                  Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
                end;
              end
              else begin
                Fehlergruppe:=SYS_ABRUFERROR;
                Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
              end;
            end
            else begin
              Fehlergruppe:=COM_KOMMERROR;
              Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
            end;

            { Logfile-Protokollierung von Fehlergruppe/-code: }
            if FAbrufLogFile <> nil then
              FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

            { XML-Antwort f�r Client: }
            isMRGAntwort:=true;
            if Abruf <> nil then
              if Abruf is TDSfGAbruf then
                isMRGAntwort:=false;

            if isMRGAntwort then begin              { Antwort mit MRG-Daten liefern }
              Result:=GetClnt_XMLAntwort_ME (Fehlergruppe, Fehlercode, Kommando,
                                             StationsKennung, GeraeteTyp_Cmd,
                                             MRGMeldungsListe,
                                             ResponseTraceLogList,
                                             MeldAbrufCmdData.vonDZ, MeldAbrufCmdData.bisDZ,
                                             sRetFreierText,
                                             FDebugServiceIO, PathServer.PathName [WWorkDir]);
            end
            else begin                             { Antwort mit DSfG-Daten liefern }
              Result:=GetClnt_XMLAntwort_LB (Fehlergruppe, Fehlercode, Kommando,
                                             StationsKennung,
                                             DSfGLogbuchdatenListe, DSfGResponseLogList,
                                             DSfGResponseRohdatenList,
                                             ResponseTraceLogList,
                                             MeldAbrufCmdData.vonDZ, MeldAbrufCmdData.bisDZ,
                                             false, -1,
                                             sRetFreierText,
                                             FDebugServiceIO, PathServer.PathName [WWorkDir]);

              { DSfG-Datenfiles k�nnen jetzt gel�scht werden: }
              for i:=0 to DSfGLogbuchdatenListe.Count-1 do begin
                DSfGFilename:=DSfGLogbuchdatenListe [i];
                if length (DSfGFilename) > 0 then
                  DeleteFile (DSfGFilename);
              end;

              { Dateien mit kodierten Rohdaten k�nnen jetzt gel�scht werden: 07.04.2014, WW }
              if FRohdatenLoeschen then begin
                for i:=0 to DSfGResponseRohdatenList.Count-1 do begin
                  DSfGFilename:=DSfGResponseRohdatenList [i];
                  if length (DSfGFilename) > 0 then
                    DeleteFile (DSfGFilename);
                end;
              end;
            end;
          finally
            ResponseTraceLogList.Free;
          end;
        finally
          DSfGResponseRohdatenList.Free;
        end;
      finally
        DSfGResponseLogList.Free;
      end;
    finally
      DSfGLogbuchdatenListe.Free;
    end;
  finally
    MRGMeldungsListe.Free;
  end;
end;

{----------------------------------------------------------}
function TAbrufCmdExec.ParaAbruf (Kommando: string): string;
{----------------------------------------------------------}
{ Parameterabruf-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ParaAbrufCmdData: TParaAbrufCmdData;
  ParameterListe: TParameterListe;
  DELList: TDELList;
  isMRGAntwort: boolean;
  ProzessId: string;
  DSfGResponseLogList: TResponseLogList;
  DSfGDfueParaList: TDfueParaList;
  DSfGDfue_EAdr: string;
  DSfG_DE_abrufen: boolean;
  DSfGResponseRohdatenList: TDSfGDataList;
  i: integer;
  DSfGFilename: string;
  sRetFreierText: string;
  ResponseTraceLogList: TDSfGDataList;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);   // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  ParameterListe:=TParameterListe.Create (PathServer [WStammDir]);  { f�r MRG-Parameter }
  try
    { 22.06.2022, WW: Kein �berschreiben von Datenelementwerten (Workaround f�r
      Ger�teproblem ERZ2000-NG in Standardabfrage-Antwort-DE's bei Abfrage 'a-z'
      (z.T. leere Werte) bzw. konzeptionelles DE-Adressenproblem bei Umwertern
      (gleiche DE-Adresse mit verschiedenen Bedeutungen, z.B. bhfc in Standard-
      abfragen als aktueller Wert oder aufgezeichnet beim Intervallende/Alarm) }
    DELList:=TDELList.Create (false);  { f�r DSfG-Datenelemente }
    try
      DSfGResponseLogList:=TResponseLogList.Create;
      try
        DSfGDfueParaList:=TDfueParaList.Create;
        try
          DSfGResponseRohdatenList:=TDSfGDataList.Create;
          try
            // Bei aktiviertem Tracelog, die zugeh�rige Responseliste anlegen; 03.01.2022, WW
            if TraceLog_VerbAufbau = C_CmdTraceLog_Aktiviert then
              ResponseTraceLogList:=TDSfGDataList.Create
            else
              ResponseTraceLogList:=nil;
            try
              { wenn beim DSfG-Verbindungsaufbau transparent geschaltet wird, werden
                beim Parameterabruf DSfG-Datenelemente geholt, ansonsten DSfG-DF�-Momentanwerte: }
              DSfG_DE_abrufen:=DSfG_TransparentModus_VerbAufbau;
              DSfGDfue_EAdr:='';   // Vorbelegung DSfG-DF�-Busadresse f�r R�ckgabe der DSfG-DF�-Momentanwerte

              if Abruf <> nil then begin
                ParaAbrufCmdData:=GetParaAbrufCmdData (Kommando);
                ProzessId:=GetAbrufKommandoProzessID (Kommando);

                // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
                if ProzessId = ProzessId_VerbAufbau then begin
                  // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
                  if Assigned (ResponseTraceLogList) then
                    Abruf.CreateCOMTraceLog
                  else
                    Abruf.FreeCOMTraceLog;
                  try
                    if Abruf is TMRGAbruf then begin                    { MRG-Abruf }
                      // MRG-Parameter abrufen:
                      TMRGAbruf (Abruf).AbrufParameter (ParaAbrufCmdData.PNrAllg, nil,
                                                        ParameterListe,
                                                        ParaAbrufCmdData.ArchivdatenTyp,
                                                        ParaAbrufCmdData.Dateiname);
                      Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
                      // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW
                      Abruf.AddCOMTraceLogToList (ResponseTraceLogList);
                    end
                    else begin                                         { DSfG-Abruf }
                      if DSfG_DE_abrufen then begin  { DSfG-Datenelemente abrufen }
                        if not TDSfGAbruf (Abruf).AbrufDaten (C_IsDatenelemente, -1, -1, -1, -1,
                                                              ParaAbrufCmdData.DSfG_AdresslistData,
                                                              nil, DELList,
                                                              DSfGResponseLogList,
                                                              DSfGResponseRohdatenList,
                                                              ResponseTraceLogList,
                                                              ParaAbrufCmdData.Timeout  // 16.08.2012, WW
                                                              ) then
                          Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
                      end
                      else begin                     { DSfG-DF�-Momentanwerte abrufen }
                        if not TDSfGAbruf (Abruf).AbrufDfueMomentanwerte (ParaAbrufCmdData.DSfG_AdresslistData,
                                                                          DSfGDfueParaList,
                                                                          DSfGDfue_EAdr) then
                          Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
                        // COM-Tracelog f�r Busadresse der DF�-Instanz in TraceLog-Liste eintragen; 03.01.2022, WW }
                        Abruf.AddCOMTraceLogToList (ResponseTraceLogList, DSfGDfue_EAdr);
                      end;
                    end;

                    sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
                  finally
                    Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
                  end;
                end
                else begin
                  Fehlergruppe:=SYS_ABRUFERROR;
                  Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
                end;
              end
              else begin
                Fehlergruppe:=COM_KOMMERROR;
                Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
              end;

              { Logfile-Protokollierung von Fehlergruppe/-code: }
              if FAbrufLogFile <> nil then
                FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

              { XML-Antwort f�r Client: }
              isMRGAntwort:=true;
              if Abruf <> nil then
                if Abruf is TDSfGAbruf then
                  isMRGAntwort:=false;

              if isMRGAntwort then      { MRG-Abruf: Antwort mit MRG-Parametern liefern }
                Result:=GetClnt_XMLAntwort_PA (Fehlergruppe, Fehlercode, Kommando,
                                               StationsKennung, GeraeteTyp_Cmd,
                                               ParameterListe,
                                               ResponseTraceLogList,
                                               sRetFreierText,
                                               FDebugServiceIO, PathServer.PathName [WWorkDir])
              else begin                { DSfG-Abruf }
                if DSfG_DE_abrufen then begin  { Antwort mit DSfG-Datenelementen liefern }
                  Result:=GetClnt_XMLAntwort_DE (Fehlergruppe, Fehlercode, Kommando,
                                                 StationsKennung,
                                                 DELList, DSfGResponseLogList,
                                                 DSfGResponseRohdatenList,
                                                 ResponseTraceLogList,
                                                 sRetFreierText,
                                                 FDebugServiceIO, PathServer.PathName [WWorkDir],
                                                 ISO646_Cfg);

                  { Dateien mit kodierten Rohdaten k�nnen jetzt gel�scht werden: 07.04.2014, WW }
                  if FRohdatenLoeschen then begin
                    for i:=0 to DSfGResponseRohdatenList.Count-1 do begin
                      DSfGFilename:=DSfGResponseRohdatenList [i];
                      if length (DSfGFilename) > 0 then
                        DeleteFile (DSfGFilename);
                    end;
                  end;
                end else   { Antwort mit DSfG-DF�-Momentanwerten liefern }
                  Result:=GetClnt_XMLAntwort_DSfGDfue_Mom (Fehlergruppe, Fehlercode, Kommando,
                                                           StationsKennung, DSfGDfue_EAdr,
                                                           DSfGDfueParaList,
                                                           ResponseTraceLogList,
                                                           sRetFreierText,
                                                           FDebugServiceIO, PathServer.PathName [WWorkDir]);
              end;
            finally
              ResponseTraceLogList.Free;
            end;
          finally
            DSfGResponseRohdatenList.Free;
          end;
        finally
          DSfGDfueParaList.Free;
        end;
      finally
        DSfGResponseLogList.Free;
      end;
    finally
      DELList.Free;
    end;
  finally
    ParameterListe.Free;
  end;
end;

{-----------------------------------------------------------}
function TAbrufCmdExec.PruefAbruf (Kommando: string): string;
{-----------------------------------------------------------}
{ Pr�fsatzabruf-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  PruefAbrufCmdData: TMessMeldPruefAbrufCmdData;
  PruefFilename: string;
  ProzessId: string;
  sRetFreierText: string;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  PruefFilename:='';                                 // Vorbelegung: es wurden keine Daten abgerufen

  if Abruf <> nil then begin
    PruefAbrufCmdData:=GetMessMeldPruefAbrufCmdData (Kommando);
    ProzessId:=GetAbrufKommandoProzessID (Kommando);

    // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
    if ProzessId = ProzessId_VerbAufbau then begin
      if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
        // MRG-Pr�fungss�tze abrufen:
        if not TMRGAbruf (Abruf).AbrufPruefwerte (PruefAbrufCmdData, PruefFilename) then
          Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

        sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
      end
      else begin                                                 { DSfG-Abruf }
        { Pr�fungss�tze gibts bei DSfG nicht }
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
      end;
    end
    else begin
      Fehlergruppe:=SYS_ABRUFERROR;
      Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
    end;
  end
  else begin
    Fehlergruppe:=COM_KOMMERROR;
    Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if FAbrufLogFile <> nil then
    FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

  { XML-Antwort f�r Client: }
  Result:=GetClnt_XMLAntwort_PR (Fehlergruppe, Fehlercode, Kommando,
                                 StationsKennung, GeraeteTyp_Cmd,
                                 PruefFilename,
                                 PruefAbrufCmdData.vonDZ, PruefAbrufCmdData.bisDZ,
                                 sRetFreierText,
                                 FDebugServiceIO, PathServer.PathName [WWorkDir]);

  { Datenfile kann jetzt gel�scht werden: }
  if length (PruefFilename) > 0 then
    DeleteFile (PruefFilename);
end;

{--------------------------------------------------------------------}
function TAbrufCmdExec.ZeitSynchronisation (Kommando: string): string;
{--------------------------------------------------------------------}
{ Zeitsynchronisations-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ZeitSyncCmdData: TZeitSyncCmdData;
  ProzessId: string;
  ZeitSyncInfoData: TZeitSyncInfoData;
  sRetFreierText: string;
  ResponseTraceLogList: TDSfGDataList;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  with ZeitSyncInfoData do begin  // Vorbelegung: keine ZeitSync-Infodaten vorhanden
    DZ_Server:=0;
    DZ_Geraet:=0;
  end;

  // Bei aktiviertem Tracelog, die zugeh�rige Responseliste anlegen; 24.02.2020, WW
  if TraceLog_VerbAufbau = C_CmdTraceLog_Aktiviert then
    ResponseTraceLogList:=TDSfGDataList.Create
  else
    ResponseTraceLogList:=nil;
  try
    if Abruf <> nil then begin
      ZeitSyncCmdData:=GetZeitSyncCmdData (Kommando);
      ProzessId:=GetAbrufKommandoProzessID (Kommando);

      // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
      if ProzessId = ProzessId_VerbAufbau then begin
        // wenn Min/Max-Abweichung im Kommando nicht enthalten ist, dann Standardwert
        // aus Program-Ini nehmen:
        if ZeitSyncCmdData.Abweichung_min = C_CmdZSyncAbweich_Standard then
          ZeitSyncCmdData.Abweichung_min:=ZeitSyncAbweichungMin_Cfg;
        if ZeitSyncCmdData.Abweichung_max = C_CmdZSyncAbweich_Standard then
          ZeitSyncCmdData.Abweichung_max:=ZeitSyncAbweichungMax_Cfg;

        // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
        if Assigned (ResponseTraceLogList) then
          Abruf.CreateCOMTraceLog
        else
          Abruf.FreeCOMTraceLog;
        try
          if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
            // Zeitsynchronisation im MRG durchf�hren:
            TMRGAbruf (Abruf).ZeitSynchronisation (ZeitSyncCmdData,
                                                   ZeitSyncKorrekturMax_Cfg,
                                                   ZeitSyncInfoData);
            Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
          end
          else begin                                                 { DSfG-Abruf }
            // Zeitsynchronisation in der DSfG-Station durchf�hren:
            TDSfGAbruf (Abruf).ZeitSynchronisation (ZeitSyncCmdData,
                                                    ZeitSyncInfoData);
            Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
          end;

          sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
          // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW
          Abruf.AddCOMTraceLogToList (ResponseTraceLogList);
        finally
          Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
        end;
      end
      else begin
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
      end;
    end
    else begin
      Fehlergruppe:=COM_KOMMERROR;
      Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
    end;

    { Logfile-Protokollierung von Fehlergruppe/-code: }
    if FAbrufLogFile <> nil then
      FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

    { XML-Antwort f�r Client: }
    Result:=GetClnt_XMLAntwort_ZeitSync (Fehlergruppe, Fehlercode, Kommando,
                                         StationsKennung, GeraeteTyp_Cmd,
                                         ResponseTraceLogList,
                                         ZeitSyncInfoData,
                                         sRetFreierText,
                                         FDebugServiceIO, PathServer.PathName [WWorkDir]);
  finally
    ResponseTraceLogList.Free;
  end;
end;

{----------------------------------------------------------------}
function TAbrufCmdExec.DSfGUmschaltung (Kommando: string): string;
{----------------------------------------------------------------}
{ DSfG-Slave-Umschaltung-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  DSfGUmschaltCmdData: TDSfGUmschaltCmdData;
  ProzessId: string;
  StationsKennung_vor_Umschaltung: string;
  sRetFreierText: string;
  ResponseTraceLogList: TDSfGDataList;
{$IFDEF GAS-X}
  MrgTyp_Wieser: integer;
  bResourceOK: boolean;
{$ENDIF}

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  StationsKennung_vor_Umschaltung:=StationsKennung;  // aktuelle Stationskennung (vor der
                                                     // Umschaltung) f�r Antwort merken
  StationsKennung:='';  // 26.03.2008, WW

  // Bei aktiviertem Tracelog, die zugeh�rige Responseliste anlegen; 03.01.2022, WW
  if TraceLog_VerbAufbau = C_CmdTraceLog_Aktiviert then
    ResponseTraceLogList:=TDSfGDataList.Create
  else
    ResponseTraceLogList:=nil;
  try
    if Abruf <> nil then begin
      DSfGUmschaltCmdData:=GetDSfGUmschaltCmdData (Kommando);
      GeraeteTyp_Cmd:=DSfGUmschaltCmdData.GeraeteTyp;   // merken f�r XML-Antwort-R�ckgaben
      ProzessId:=GetAbrufKommandoProzessID (Kommando);

      // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
      if ProzessId = ProzessId_VerbAufbau then begin
        if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
  {$IFDEF GAS-X}
          { die im Gas-X verwendete MRG-Typnummer in die entsprechende Wieser MRG-Typnummer
            umwandeln: }
          bResourceOK:=false;
          if Assigned (FResourceFilesList) then
            bResourceOK:=FResourceFilesList.GetMrgTypGasX (DSfGUmschaltCmdData.GeraeteTyp,
                                                           MrgTyp_Wieser);  // 06.08.2021, WW
          if bResourceOK then
            DSfGUmschaltCmdData.GeraeteTyp:=MrgTyp_Wieser
          else begin
            Fehlergruppe:=ST_KONFIGERROR;
            Fehlercode:=KFERR_KONFIGDATANOTFOUND;
          end;
  {$ENDIF}

          if (Fehlergruppe = 0) AND (Fehlercode = 0) then begin
            // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
            if Assigned (ResponseTraceLogList) then
              Abruf.CreateCOMTraceLog
            else
              Abruf.FreeCOMTraceLog;
            try
              // Slave-Umschaltung durchf�hren (R�ckgabe: Kennung des Slave, auf den
              // umgeschaltet werden soll)
              TMRGAbruf (Abruf).DSfGUmschaltung (DSfGUmschaltCmdData,
                                                 StationsKennung);
              Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

              sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
              // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW
              Abruf.AddCOMTraceLogToList (ResponseTraceLogList);
            finally
              Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
            end;
          end;
        end
        else begin                                                 { DSfG-Abruf }
          { Slave-Umschaltung gibts bei DSfG nicht }
          Fehlergruppe:=SYS_ABRUFERROR;
          Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
        end;
      end
      else begin
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
      end;
    end
    else begin
      Fehlergruppe:=COM_KOMMERROR;
      Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
    end;

    { Logfile-Protokollierung von Fehlergruppe/-code: }
    if FAbrufLogFile <> nil then
      FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

   { XML-Antwort f�r Client: }
    Result:=GetClnt_XMLAntwort_DSfGUmschalt (Fehlergruppe, Fehlercode, Kommando,
                                             ResponseTraceLogList,
                                             StationsKennung_vor_Umschaltung,
                                             StationsKennung,
                                             sRetFreierText,
                                             FDebugServiceIO, PathServer.PathName [WWorkDir]);
  finally
    ResponseTraceLogList.Free;
  end;
end;

{--------------------------------------------------------------------}
function TAbrufCmdExec.DSfGBusAnalyseAbruf (Kommando: string): string;
{--------------------------------------------------------------------}
{ DSfG-Busanalyseabruf-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ProzessId: string;
  DELList: TDELList;
  sRetFreierText: string;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  DELList:=TDELList.Create;
  try
    if Abruf <> nil then begin
      ProzessId:=GetAbrufKommandoProzessID (Kommando);

      // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
      if ProzessId = ProzessId_VerbAufbau then begin
        if Abruf is TDSfGAbruf then begin                        { DSfG-Abruf }
          // DSfG-Busanalysedaten abrufen:
          if not TDSfGAbruf (Abruf).AbrufKonfiguration (DELList) then
            Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

          sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
        end
        else begin                                                { MRG-Abruf }
          { DSfG-Busanalysedaten gibts bei MRGs nicht }
          Fehlergruppe:=SYS_ABRUFERROR;
          Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
        end;
      end
      else begin
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
      end;
    end
    else begin
      Fehlergruppe:=COM_KOMMERROR;
      Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
    end;

    { Logfile-Protokollierung von Fehlergruppe/-code: }
    if FAbrufLogFile <> nil then
      FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

    { XML-Antwort f�r Client: }
    Result:=GetClnt_XMLAntwort_DSfG_Busanalyse (Fehlergruppe, Fehlercode, Kommando,
                                                StationsKennung, DELList,
                                                sRetFreierText,
                                                FDebugServiceIO, PathServer.PathName [WWorkDir],
                                                ISO646_Cfg);
  finally
    DELList.Free;
  end;
end;

{----------------------------------------------------------------}
function TAbrufCmdExec.RundpufferReset (Kommando: string): string;
{----------------------------------------------------------------}
{ Rundpufferreset-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ProzessId: string;
  RpTyp: string;
  sRetFreierText: string;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  if Abruf <> nil then begin
    RpTyp:=GetRpResetCmdData (Kommando);
    ProzessId:=GetAbrufKommandoProzessID (Kommando);

    // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
    if ProzessId = ProzessId_VerbAufbau then begin
      if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
        if RpTyp = C_CmdRpTyp_ME then begin
          // Rundpufferreset f�r Meldungen im MRG durchf�hren:
          if not TMRGAbruf (Abruf).ResetRundpuffer_Meld then
            Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
        end
        else if RpTyp = C_CmdRpTyp_MW then begin
          // Rundpufferreset f�r Messwerte im MRG durchf�hren:
          if not TMRGAbruf (Abruf).ResetRundpuffer_Mess then
            Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
        end
        else begin
          // Rundpufferreset f�r Pr�fungss�tze im MRG durchf�hren:
          if not TMRGAbruf (Abruf).ResetRundpuffer_Pruef then
            Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
        end;

        sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
      end
      else begin                                                 { DSfG-Abruf }
        { Rundpufferreset gibts bei DSfG nicht }
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
      end;
    end
    else begin
      Fehlergruppe:=SYS_ABRUFERROR;
      Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
    end;
  end
  else begin
    Fehlergruppe:=COM_KOMMERROR;
    Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if FAbrufLogFile <> nil then
    FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

  { XML-Antwort f�r Client: }
  Result:=GetClnt_XMLAntwort (Fehlergruppe, Fehlercode, Kommando,
                              StationsKennung,
                              sRetFreierText,
                              FDebugServiceIO, PathServer.PathName [WWorkDir]);
end;

{--------------------------------------------------------------}
function TAbrufCmdExec.Parametrieren (Kommando: string): string;
{--------------------------------------------------------------}
{ Parametrier-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ProzessId: string;
  ParaEinstellCmdData: TParaEinstellCmdData;
  ParaEinstellResultData: TParaEinstellResultData;
  DSfGDfue_EAdr: string;
  sRetFreierText: string;
  ResponseTraceLogList: TDSfGDataList;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  with ParaEinstellResultData do begin  // Vorbelegung: keine Parametrier-Ergebnisdaten
    ParaTyp:='';
    BAdr:='';
    ParaAdr:='';
    WertAlt:='';
    WertNeu:='';
  end;
  DSfGDfue_EAdr:='';   // Vorbelegung DSfG-DF�-Busadresse f�r R�ckgabe Parametrier-Ergebnis

  // Bei aktiviertem Tracelog, die zugeh�rige Responseliste anlegen; 03.01.2022, WW
  if TraceLog_VerbAufbau = C_CmdTraceLog_Aktiviert then
    ResponseTraceLogList:=TDSfGDataList.Create
  else
    ResponseTraceLogList:=nil;
  try
    if Abruf <> nil then begin
      ParaEinstellCmdData:=GetParaEinstellCmdData (Kommando);
      ProzessId:=GetAbrufKommandoProzessID (Kommando);

      // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
      if ProzessId = ProzessId_VerbAufbau then begin
        // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
        if Assigned (ResponseTraceLogList) then
          Abruf.CreateCOMTraceLog
        else
          Abruf.FreeCOMTraceLog;
        try
          if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
            if ParaEinstellCmdData.ParaTyp = C_CmdParatyp_MRG then begin
              // pr�fen, ob im Lizenzfile die system-globale Funktionalit�t 'Parametrierung MRG'
              // freigeschaltet ist:
              if FLizenzList.ReadGlobaleFunktionFromLizenzFile (FNExt_ParametrierungMRG) then begin
                // Parametrierung im MRG durchf�hren:
                if not TMRGAbruf (Abruf).UebertragungParameter (ParaEinstellCmdData,
                                                                ParaEinstellResultData) then
                  Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

                sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
                // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW
                Abruf.AddCOMTraceLogToList (ResponseTraceLogList);  // 03.01.2022, WW
              end
              else begin
                // Programmfunktion nicht lizenziert:
                Fehlergruppe:=SYS_LICENCEERROR;
                Fehlercode:=LICENCEERR_PROGFUNKTION;
              end;
            end
            else begin
              Fehlergruppe:=SYS_ABRUFERROR;
              Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
            end;
          end
          else begin                                                 { DSfG-Abruf }
            if (ParaEinstellCmdData.ParaTyp = C_CmdParatyp_DSfG) then begin
              // pr�fen, ob im Lizenzfile die system-globale Funktionalit�t 'Parametrierung DSfG'
              // freigeschaltet ist:
              if FLizenzList.ReadGlobaleFunktionFromLizenzFile (FNExt_ParametrierungDSfG) then begin
                if ISO646_Cfg then  // Zeichen-Konvertierung ISO 646 -> ASCII; 03.03.2010, WW
                  if length (ParaEinstellCmdData.ParaWertNeu) > 1 then  // zur Vermeidung, da� DSfG-Busadressen konvertiert werden
                    ParaEinstellCmdData.ParaWertNeu:=
                      WISO646_DeToAscii (ParaEinstellCmdData.ParaWertNeu);

                // DSfG-Parametrierung (Datenelemente) durchf�hren:
                if not TDSfGAbruf (Abruf).UebertragungDatenelement (ParaEinstellCmdData,
                                                                    ParaEinstellResultData) then
                  Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

                sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
                // COM-Tracelog f�r Busadresse der Instanz in TraceLog-Liste eintragen; 03.01.2022, WW
                Abruf.AddCOMTraceLogToList (ResponseTraceLogList, ParaEinstellCmdData.BAdr);  // 03.01.2022, WW
              end
              else begin
                // Programmfunktion nicht lizenziert:
                Fehlergruppe:=SYS_LICENCEERROR;
                Fehlercode:=LICENCEERR_PROGFUNKTION;
              end;
            end

            else if (ParaEinstellCmdData.ParaTyp = C_CmdParatyp_DSfGDfue) then begin
              // pr�fen, ob im Lizenzfile die system-globale Funktionalit�t 'Parametrierung DSfG-DF�'
              // freigeschaltet ist:
              if FLizenzList.ReadGlobaleFunktionFromLizenzFile (FNExt_ParametrierungDSfGDfue) then begin
                // DSfG-DF�-Parametrierung durchf�hren:
                if not TDSfGAbruf (Abruf).UebertragungDfueParameter (ParaEinstellCmdData,
                                                                     ParaEinstellResultData,
                                                                     DSfGDfue_EAdr) then
                  Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

                sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
                // COM-Tracelog f�r Busadresse der DF�-Instanz in TraceLog-Liste eintragen; 03.01.2022, WW
                Abruf.AddCOMTraceLogToList (ResponseTraceLogList, DSfGDfue_EAdr);  // 03.01.2022, WW
              end
              else begin
                // Programmfunktion nicht lizenziert:
                Fehlergruppe:=SYS_LICENCEERROR;
                Fehlercode:=LICENCEERR_PROGFUNKTION;
              end;
            end

            else begin
              Fehlergruppe:=SYS_ABRUFERROR;
              Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
            end;
          end;
        finally
          Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
        end;
      end
      else begin
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
      end;
    end
    else begin
      Fehlergruppe:=COM_KOMMERROR;
      Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
    end;

    { Logfile-Protokollierung von Fehlergruppe/-code: }
    if FAbrufLogFile <> nil then
      FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

    { XML-Antwort f�r Client: }
    Result:=GetClnt_XMLAntwort_Parametrieren (Fehlergruppe, Fehlercode, Kommando,
                                              StationsKennung, GeraeteTyp_Cmd,
                                              DSfGDfue_EAdr,
                                              ResponseTraceLogList,
                                              ParaEinstellResultData,
                                              sRetFreierText,
                                              FDebugServiceIO, PathServer.PathName [WWorkDir],
                                              ISO646_Cfg);
  finally
    ResponseTraceLogList.Free;
  end;
end;

{------------------------------------------------------------------}
function TAbrufCmdExec.Transparentbefehl (Kommando: string): string;
{------------------------------------------------------------------}
{ Transparent-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ProzessId: string;
  TransparentCmdData: TTransparentCmdData;
  TransparentAntw: string;
  sRetFreierText: string;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  TransparentAntw:='';  // Vorbelegung: Antwort auf Transparentbefehl leer

  if Abruf <> nil then begin
    TransparentCmdData:=GetTransparentCmdData (Kommando);
    ProzessId:=GetAbrufKommandoProzessID (Kommando);

    // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
    if ProzessId = ProzessId_VerbAufbau then begin
      if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
        // Transparentbefehl f�r MRG durchf�hren:
        if not TMRGAbruf (Abruf).AbrufTransparent (TransparentCmdData,
                                                   TransparentAntw) then
          Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
      end
      else begin                                                 { DSfG-Abruf }
        // Transparentbefehl f�r DSfG durchf�hren:
        if not TDSfGAbruf (Abruf).AbrufTransparent (TransparentCmdData,
                                                    TransparentAntw) then
          Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
      end;

      sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
    end
    else begin
      Fehlergruppe:=SYS_ABRUFERROR;
      Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
    end;
  end
  else begin
    Fehlergruppe:=COM_KOMMERROR;
    Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if FAbrufLogFile <> nil then
    FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

  { XML-Antwort f�r Client: }
  Result:=GetClnt_XMLAntwort_Transparent (Fehlergruppe, Fehlercode, Kommando,
                                          StationsKennung, TransparentAntw,
                                          sRetFreierText,
                                          FDebugServiceIO, PathServer.PathName [WWorkDir]);
end;

{--------------------------------------------------------------------------------------------}
function TAbrufCmdExec.Rufentgegennahme (Kommando: string; var RE_gestartet: boolean): string;
{--------------------------------------------------------------------------------------------}
{ Rufentgegennahme-Kommando ausf�hren;
  �bergabe: Kommando-String
  R�ckgabe: Flag RE_gestartet (true, wenn Rufentgegenahme mit dem Kommando gestartet wurde)
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  RufModus: integer;
  Lic_OK: boolean;
  sFilename: string;
  sXML: string;
  iErg: integer;
  iFileAttr: integer;
  iDate: integer;
  bOK: boolean;
  dtDummy: TDateTime;

begin
  RE_gestartet:=false;
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);     // Vorbelegung f�r Fehlergruppe/-code: OK

  ReadSrvCfg_ModemIni;             { Schnittstellen/Modem-Konfiguration lesen }
  RufModus:=GetRufCmdData (Kommando);

  if RufModus = C_CmdRufStart then begin           { Rufentgegennahme starten }
    // pr�fen, ob im Lizenzfile die system-globale Funktionalit�t 'Rufentgegennahme'
    // MRG/DSfG' oder 'SMS-Empfang' freigeschaltet ist:
    if RufTyp = re_SMS then
      Lic_OK:=FLizenzList.ReadGlobaleFunktionFromLizenzFile (FNExt_SMS_Datenempfang)
    else if RufTyp = re_DSfG then
      Lic_OK:=FLizenzList.ReadGlobaleFunktionFromLizenzFile (FNExt_RufentgegennahmeDSfG)
    else
      Lic_OK:=FLizenzList.ReadGlobaleFunktionFromLizenzFile (FNExt_RufentgegennahmeMRG);
    if Lic_OK then begin
      Kommando_Rufentgegennahme:=Kommando;     { Kommando merken f�r Antwort, wenn MRG/DSfG-Ruf ansteht }

      RE_gestartet:=true;  // RE wurde mit dem Kommando gestartet
      Rufentgegennahme_SMSEmpfang_aktiviert:=true;
      FUP_Reset_fuer_Rufpolling_OK:=false;  { f�r RE mit FUP: Reset noch nicht erfolgt ! }
    end
    else begin
      // Programmfunktion nicht lizenziert:
      Fehlergruppe:=SYS_LICENCEERROR;
      Fehlercode:=LICENCEERR_PROGFUNKTION;
    end;
  end
  else if RufModus = C_CmdRufEnde then begin       { Rufentgegennahme beenden }
    Rufentgegennahme_SMSEmpfang_aktiviert:=false;
    { -> mit dem False-Setzen des Flags wird im �bergeordneten Objekt (TServerAbrufThread)
         das Objekt TAbrufCmdExec freigegeben, womit u.a. auch die COM geschlossen wird }
  end
  else if (RufModus = C_CmdRufSMS_OK) OR
          (RufModus = C_CmdRufSMS_Wdh) then begin  { SMS-Best�tigung vom Client }
    if FAbrufLogFile <> nil then
      FAbrufLogFile.Write ('SMS-Datei quittiert: ' + IntToStr (RufModus));  { Logfile-Protokollierung }

    { wenn die letzte SMS vom Client erfolgreich empfangen/verarbeitet wurde, kann
      die SMS-Datei jetzt gel�scht werden: }
    if RufModus = C_CmdRufSMS_OK then begin
      if length (LetztSMSFilename) > 0 then
        DeleteFile (LetztSMSFilename);
    end
    else begin  // nicht erfolgreich verarbeitet, zu sp�terem Zeitpunkt nochmal senden
      if length (SMS_BackupDir_Cfg) > 0 then begin  // SMS-Backup aktiviert
        iErg:=BackupSMSXMLFile (LetztSMSFilename, SMS_BackupDir_Cfg);
        if FAbrufLogFile <> nil then                     { Logfile-Protokollierung }
          FAbrufLogFile.Write ('SMS-Backup: ' + IntToStr (iErg));
      end;

      // Archivbit l�schen:
      // -> SMX-XML-Dateien mit gel�schtem Archivbit werden nicht (mehr) gebackupt
      //    und nur einmal am Tag erneut an den Client versendet
      if length (LetztSMSFilename) > 0 then begin
        iFileAttr:=FileGetAttr (LetztSMSFilename);
        if (iFileAttr and faArchive) > 0 then begin // Archiv-Bit gesetzt
          bOK:=FileSetAttr (LetztSMSFilename, iFileAttr AND not faArchive) = 0;  { Archiv-Bit l�schen }
          if FAbrufLogFile <> nil then begin                    { Logfile-Protokollierung }
            if bOK then
              FAbrufLogFile.Write ('SMS-Datei: Archivbit gel�scht')
            else
              FAbrufLogFile.Write ('SMS-Datei: Fehler beim Archivbit l�schen', true, lt_Error);
          end;
        end;
      end;
    end;

    // ...und die n�chste SMS-Datei kann gesendet werden, falls vorhanden:
    // Client-SMS-Antwort aus Datei lesen, Dateiname wird mit zur�ckgegeben:
    iDate:=Round (Date);
    if iDate <> SMS_WdhDatum then begin
      SMS_WdhDatum:=iDate;
      // einmal am Tag auch die zur Wiederholung anstehenden SMS-Dateien
      bOK:=SetArchivbitForAllSMSTempFiles (PathServer.PathName [WWorkDir], prefix_SMS_XML);
      if FAbrufLogFile <> nil then begin                    { Logfile-Protokollierung }
        if bOK then
          FAbrufLogFile.Write ('Alle SMS-Dateien: Archivbit gesetzt (Rufentgegennahme)')
        else
          FAbrufLogFile.Write ('Alle SMS-Dateien: Fehler beim Archivbit setzen (Rufentgegennahme)', true, lt_Error);
      end;
    end;

    sXML:=GetNextSMSFromTempFile (PathServer.PathName [WWorkDir], prefix_SMS_XML,
                                  fas_NurArchivfaehige, sFileName, dtDummy);
    if length (sFileName) > 0 then begin  // SMS-Datei ist vorhanden
      if FAbrufLogFile <> nil then
        FAbrufLogFile.Write ('SMS-Datei gelesen (Rufentgegennahme)');  { Logfile-Protokollierung }
      { SMS-Dateiname merken (mu� nach positiver Client-R�ckmeldung gel�scht werden !): }
      LetztSMSFilename:=sFileName;
      Result:=sXML;  { Antwort f�r Client }
    end
    else begin  // keine weitere SMS-Datei vorhanden
      LetztSMSFilename:='';   // SMS-Merk-Dateiname zur�cksetzen
      Result:='';  { keine Antwort f�r Client ! }
    end;
    exit;
  end
  else begin
    Fehlergruppe:=SYS_ABRUFERROR;
    Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if FAbrufLogFile <> nil then
    FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

  { Antwort f�r Client: }
  Result:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode, Kommando,
                                  '', rc_Ruf_steht_nicht_an,
                                  '', // unbenutzt
                                  FDebugServiceIO, PathServer.PathName [WWorkDir]);
end;

{---------------------------------------------------------------------------}
function TAbrufCmdExec.Ruf_SMS_Abfragen (var Ruf_SMS_steht_an: byte): string;
{---------------------------------------------------------------------------}
{ Rufabfrage ausf�hren;
  R�ckgabe: 0 = Ruf/SMS steht nicht an
            1 = Ruf steht an (MRG oder DSfG), weiter mit Rufannahme durch Client
            2 = Ruf/SMS steht an und wurde abgewickelt, kein Folge-Kommando durch Client
  Ergebnis: XML-Antwort-String, wenn Ruf/SMS ansteht, St�rung beim Ruf/SMS-Polling
            aufgetreten ist oder Polling nach St�rung wieder OK ist, ansonsten Leer-String }
const
  CTimeout_RingCheck = 10000;  //  Timeout in ms f�r �berwachung eingehender "Rings"
                               //  -> Rings laufen im Abstand von ca. 4 s ein, d.h
                               //     nach Ablauf des Timeouts kann man davon
                               //     ausgehen, da� kein Ring des selben Abrufs
                               //     mehr nachkommt.

var
  Fehlergruppe, Fehlercode: integer;
  Ruf_angekommen: boolean;
  OK: boolean;
  isRufentgegennahmeDSfG: boolean;
  SMS_Rohdaten: string;
  SMSListe: TSMSList;
  SMSListObj: TSMSListObj;
  i: integer;
  SMS_GeraeteTyp: integer;
  RE_Antwort: string;
  TickCount: cardinal;
  GSM_WaitAfterLogin: boolean;
  sFilename: string;
  sXML: string;
  bReadSMS: boolean;
  NewSMS_Count: integer;
  MRGMeldungsliste: TMeldungsliste;
  Ruf_Geraetetyp: integer;
  iDate: integer;
  bOK_ArchBit: boolean;
  dtDatenlueckePruefung_bis: TDateTime;
  dtFileDate: TDateTime;
  dtDummy: TDateTime;
  VerbInfoData: TVerbInfoData;
  overIP: boolean;
  sRetFreierText: string;
  Verbindung_OK: boolean;

begin
  { Vorbelegung: kein Ruf angekommen, keine St�rung des Ruf-Polling }
  Result:='';
  Ruf_SMS_steht_an:=0;  // es steht weder Ruf noch SMS an
  StationsKennung:='';
  if Neustart_Rufpolling then begin
    RingCount:=0;
  end;
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);     // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  // Rundpuffer auf COM-Logfile zus�tzlich einmal t�glich ausf�hren (bei RE �ber
  // FUP wird in COM-Logfile auch ohne Anruf geschrieben); 03.12.2013, WW
  if Now > FNextRundpufferDatumZeit then begin
    FNextRundpufferDatumZeit:=IncDay (FNextRundpufferDatumZeit, 1);

    if FDebugRundpufferMaxBytes > 0 then begin
      if Assigned (ComLogFile) then
        ComLogFile.DoRundpuffer (FDebugRundpufferMaxBytes);
    end;
  end;

  overIP:=FCOMNr < 0;  // IP-Abruf, wenn COMNr negativ

  { Pr�fen, ob aktuell verwendetes Kommunikations-Objekt f�r RE g�ltig ist: }
  if RufTyp = re_DSfG then begin  { Rufentgegennahme f�r DSfG }
    if not overIP then begin  // f�r IP-Rufentgegennahme Kommunikations-Objekt hier nicht erforderlich; 08.01.2018, WW
      if (not Assigned (CommObj)) OR  // 28.05.2013, WW
         (not (CommObj is TDSfGModemCommObj)) then begin  // nur TDSfGModemCommObj wiederverwendbar
        Free_CommObject;
        ReadProgramIni_DSfG;
        Init_DSfG_CommObject (false, '', false, Fehlergruppe, Fehlercode);  // RE nicht �ber GPRS
      end;
    end;

    isRufentgegennahmeDSfG:=true;  // 08.01.2018, WW
  end
  else begin                      { Rufentgegennahme f�r MRG und SMS-Datenempfang }
    if (not Assigned (CommObj)) OR  // 28.05.2013, WW
       (not ((CommObj is TMRGFupCommObj) OR (CommObj is TMRGModemCommObj))) then begin
      // nur TMRGFupCommObj oder TMRGModemCommObj wiederverwendbar
      Free_CommObject;
      ReadProgramIni_MRG;
      Init_MRG_CommObject (false, '', false, Fehlergruppe, Fehlercode);  // RE nicht �ber IP, GPRS und ohne FUP-Reset
    end;

    isRufentgegennahmeDSfG:=false;  // 08.01.2018, WW
  end;

  if overIP then begin
    { Pr�fen auf neue TCP/IP-Rufentgegennahme-Verbindung: }
    Ruf_angekommen:=Check_NeueREClntVerbindung;  // 08.01.2018, WW
    OK:=true;
  end
  else begin  // nicht �ber TCP/IP
    if CommObj is TDSfGModemCommObj then begin  { Rufentgegennahme DSfG mit Modem }
      { wenn das Ruf-Polling neu beginnt: zuvor Modem neu initialisieren }
      OK:=true;
      if Neustart_Rufpolling then begin
        OK:=Modem_Initialisieren_Wico (CommObj, false, ModemName,
                                       PathServer.Pathname [WNetProgDir],
                                       DSfGTimeouts.ModemInit, DSfGTimeouts.GSMModem,
                                       FCOMNr, false, FAbrufLogFile, Fehlergruppe, Fehlercode);
        { wenn Login in GSM-Modem nicht m�glich ist, wird Ruf-/SMS-Polling gestoppt:
            - bei falscher PIN w�rde bei weiteren Login-Versuchen die SIM-Karte gesperrt werde.
            - es macht es keinen Sinn weiterzupollen, wenn PUK-Eingabe erforderlich
            ist (Login �ber PUK ist nicht vorgesehen) oder wenn PIN-Sperre aktiv ist. }
        if ((Fehlergruppe = COM_MODEMERROR) AND ((Fehlercode = CME_GSM_PIN_WRONG) OR
            (Fehlercode = CME_GSM_PIN_LOGINIMPOSSIBLE_PUK) OR
            (Fehlercode = CME_GSM_PIN_LOGINIMPOSSIBLE_UNKNOWN))) OR
           ((Fehlergruppe = COM_DEVICEINITERROR) AND (Fehlercode = DEVINITERR_PIN_LOCK)) then
          Rufentgegennahme_SMSEmpfang_aktiviert:=false;

        if not OK then  // Modem-Initialisierung ist fehlgeschlagen
          Free_CommObject;  // damit Schnittstellen-Objekt beim n�chsten Rufpolling
                            // neu initialisiert wird; 28.05.2013, WW
      end;

      if OK then
        { Schnittstelle auf DSfG-Ruf pr�fen: }
        OK:=TDSfGModemCommObj (CommObj).Rufabfrage (Ruf_angekommen, Fehlergruppe, Fehlercode);
    end
    else if (CommObj is TMRGFupCommObj) OR
            (CommObj is TMRGModemCommObj) then begin  { Rufentgegennahme MRG/SMS-Datenempfang mit FUP/Modem }
      { wenn das Ruf/SMS-Polling neu beginnt: zuvor Modem/FUP neu initialisieren (incl.
        PIN-Abfrage/Login bei GSM-Modem), bei SMS-Polling zus�tzlich SMS-Format setzen }
      OK:=true;
      if Neustart_Rufpolling then begin
        if UpperCase (Device) = UpperCase (Devices [devFUP]) then begin
          if FUP_Reset_fuer_Rufpolling_OK then begin
            { FUP-Init reicht (sonst dauert es auch zu lange bis der n�chste Abruf
              durchgef�hrt werden kann) }
            if not Fup_Init (TMRGFupCommObj (CommObj), PathServer.Pathname [WNetProgDir],
                             MRGTimeouts.FupAntwort, Fehlergruppe, Fehlercode) then begin
              OK:=Fup_Reset (TMRGFupCommObj (CommObj), PathServer.Pathname [WNetProgDir],
                             MRGTimeouts.FupReset, MRGTimeouts.FupAntwort, true,
                             Fehlergruppe, Fehlercode);
              FUP_Reset_fuer_Rufpolling_OK:=OK;
              if not OK then  // Fup-Reset ist fehlgeschlagen
                Free_CommObject;  // damit Schnittstellen-Objekt beim n�chsten Rufpolling
                                  // neu initialisiert wird; 28.05.2013, WW
            end;
          end
          else begin
            { FUP-Reset erneut versuchen }
            OK:=Fup_Reset (TMRGFupCommObj (CommObj), PathServer.Pathname [WNetProgDir],
                           MRGTimeouts.FupReset, MRGTimeouts.FupAntwort, true,
                           Fehlergruppe, Fehlercode);
            FUP_Reset_fuer_Rufpolling_OK:=OK;
            if not OK then  // Fup-Reset ist fehlgeschlagen
              Free_CommObject;  // damit Schnittstellen-Objekt beim n�chsten Rufpolling
                                // neu initialisiert wird; 28.05.2013, WW
          end;
        end
        else begin
          { Baudrate, Schnittstellen-Parameter und Datenprotokoll zur Modem-Initialisierung
            aktivieren: }
          if RufTyp = re_MRG_FUP then
            { Rufentgegennahme f�r FUP-Ger�te: mit fester Standard-Baudrate 9600 }
            TMRGModemCommObj (CommObj).Serial.Baudrate:=
              TMRGModemCommObj (CommObj).Serial.KonvertBaudrate (C_BaudMRGStandard)
          else
            { Rufentgegennahme f�r Modemger�te und SMS-Datenempfang: mit max. m�glicher Baudrate wie
              in Schnittstellen-Konfiguration eingestellt }
            TMRGModemCommObj (CommObj).Serial.Baudrate:=TMRGModemCommObj (CommObj).MaxModemBaudrate;
          TMRGCustomCommObj (CommObj).SetAbrufgruppe (0, Modem_Databits_Parity_Stopbits);

          GSM_WaitAfterLogin:=RufTyp = re_SMS;  // Zugriff auf SMS-Liste des GSM-Modems
                                                // ist nach einem Login erst nach gewisser Wartezeit m�glich
          OK:=Modem_Initialisieren_Wico (CommObj, false, ModemName,
                                         PathServer.Pathname [WNetProgDir],
                                         MRGTimeouts.ModemInit, MRGTimeouts.GSMModem,
                                         FCOMNr, GSM_WaitAfterLogin, FAbrufLogFile,
                                         Fehlergruppe, Fehlercode);
          { wenn Login in GSM-Modem nicht m�glich ist, wird Ruf-/SMS-Polling gestoppt:
              - bei falscher PIN w�rde bei weiteren Login-Versuchen die SIM-Karte gesperrt werde.
              - es macht es keinen Sinn weiterzupollen, wenn PUK-Eingabe erforderlich
              ist (Login �ber PUK ist nicht vorgesehen) oder wenn PIN-Sperre aktiv ist. }
          if ((Fehlergruppe = COM_MODEMERROR) AND ((Fehlercode = CME_GSM_PIN_WRONG) OR
              (Fehlercode = CME_GSM_PIN_LOGINIMPOSSIBLE_PUK) OR
              (Fehlercode = CME_GSM_PIN_LOGINIMPOSSIBLE_UNKNOWN))) OR
             ((Fehlergruppe = COM_DEVICEINITERROR) AND (Fehlercode = DEVINITERR_PIN_LOCK)) then
            Rufentgegennahme_SMSEmpfang_aktiviert:=false;

          if not OK then  // Modem-Initialisierung ist fehlgeschlagen
            Free_CommObject;  // damit Schnittstellen-Objekt beim n�chsten Rufpolling
                              // neu initialisiert wird; 28.05.2013, WW

          { bei SMS-Datenempfang zus�tzlich: SMS-Format setzen }
          if OK AND (RufTyp = re_SMS) then
            OK:=GSM_Set_SMS_Format (CommObj, MRGTimeouts.GSMModem,
                                    smsf_Text, Fehlergruppe, Fehlercode);   { SMS-Format "Text" setzen }
        end;
      end;

      if OK AND (RufTyp <> re_SMS) then    { nicht bei SMS-Datenempfang }
        { Schnittstelle auf MRG-Ruf pr�fen: }
        OK:=TMRGCommObj (CommObj).Rufabfrage (Ruf_angekommen, Fehlergruppe, Fehlercode);
    end else
      exit;
  end;

  Neustart_Rufpolling:=false;                    { Neustart-Flag zur�cksetzen }
  if OK then begin
    if RufTyp = re_SMS then begin     { SMS-Datenempfang }
      SMSListe:=TSMSList.Create;
      try
        bReadSMS:=true;
        while bReadSMS do begin
          { alle SMS aus GSM-Modem auslesen:
            -> Im GSM-Modem haben max. 20 SMS Platz. F�r das Nachladen nach dem
               1. Durchlauf stehen ca. 33 s zur Verf�gung (20 SMS l�schen: ca. 23 s;
               SMS-Leszyklus: 10 s). Nachladezeit sollte ausreichend sein.
          }
          if GSM_Readlist_SMS (CommObj, MRGTimeouts.GSMModem, 'ALL',
                               SMS_Rohdaten, Fehlergruppe, Fehlercode) then begin
            { in Rohantwort enthaltene SMS in SMS-Liste laden und in SMS-Logfile
              protokollieren: }
            NewSMS_Count:=SMSListe.LoadFromModemSMSList_Rohstring (SMS_Rohdaten,
              PathServer.Pathname[WLogDir], true, FCOMNr);
            if NewSMS_Count > 0 then begin       { es sind neue SMS vorhanden }
              if FAbrufLogFile <> nil then
                FAbrufLogFile.Write (IntToStr (NewSMS_Count) + ' SMS empfangen');  { Logfile-Protokollierung }

              { alle neu gelesene SMSen im GSM-Modem l�schen: }
              for i:=0 to SMSListe.Count - 1 do begin
                SMSListObj:=TSMSListObj (SMSListe [i]);
                if not SMSListObj.Daten.geloescht then begin
                  if not GSM_Delete_SMS (CommObj, MRGTimeouts.GSMModem,
                                         SMSListObj.Daten.Index,
                                         Fehlergruppe, Fehlercode) then begin
                    Neustart_Rufpolling:=true;
                    { wenn ein Fehler beim L�schen der SMS aufgetreten ist, mu� durch
                      Neustart des SMS-Pollings versucht werden, das Problem zu l�sen
                      (das Modem kann z.B. eine erneute Eingabe der PIN erwarten) }

                    { Logfile-Protokollierung von Fehlergruppe/-code: }
                    if FAbrufLogFile <> nil then
                      FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

                    { Antwort f�r Client: }
                    Result:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode, Kommando_Rufentgegennahme,
                                                    StationsKennung, rc_Ruf_steht_nicht_an,
                                                    sRetFreierText,
                                                    FDebugServiceIO, PathServer.PathName [WWorkDir]);
                    bReadSMS:=false;  { Fehler SMS l�schen, SMS-Auslesen beenden }
                    Break;
                  end;
                  SMSListObj.Daten.geloescht:=true;  // gel�scht-Flag setzen
                end;  { if not SMSListObj.Daten.geloescht }
              end;  { for i }
            end else
              bReadSMS:=false;  { keine weiteren SMS mehr vorhanden, SMS-Auslesen beenden }
          end
          else begin   { Fehler beim SMS-Polling aufgetreten }
            Neustart_Rufpolling:=true;
            { wenn ein Fehler beim Abfragen der SMS aufgetreten ist, mu� durch
              Neustart des SMS-Pollings versucht werden, das Problem zu l�sen
              (das Modem kann z.B. eine erneute Eingabe der PIN erwarten) }

            { Logfile-Protokollierung von Fehlergruppe/-code: }
            if FAbrufLogFile <> nil then
              FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

            { Antwort f�r Client: }
            Result:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode, Kommando_Rufentgegennahme,
                                            StationsKennung, rc_Ruf_steht_nicht_an,
                                            sRetFreierText,
                                            FDebugServiceIO, PathServer.PathName [WWorkDir]);
            bReadSMS:=false;  { Fehler SMS lesen, SMS lesen beenden }
          end;
        end;  { while bReadSMS }

        { eine SMS-Schnittstelle zum Importieren/Testen von SMS-Rohdaten �ber
          Datei; 20.01.2009, WW }
        if length (SMS_ImportDir_Cfg) > 0 then begin  // SMS-Import aktiviert
          SMS_Rohdaten:=GetNextSMSFromTempFile (SMS_ImportDir_Cfg, prefix_SMS_Import,
                                                fas_Alle, sFileName, dtDummy);
          NewSMS_Count:=0;
          while length (sFileName) > 0 do begin  // SMS-Import-Rohdatei ist vorhanden
            { in Importdatei enthaltene SMS in SMS-Liste laden und in SMS-Logfile
              protokollieren: }
            i:=SMSListe.LoadFromModemSMSList_Rohstring (SMS_Rohdaten,
              PathServer.Pathname[WLogDir], true, FCOMNr);
            inc (NewSMS_Count, i);
            DeleteFile (sFilename);  // Importdatei l�schen

            SMS_Rohdaten:=GetNextSMSFromTempFile (SMS_ImportDir_Cfg, prefix_SMS_Import,
                                                  fas_Alle, sFileName, dtDummy);
          end;  { while length (sFileName) > 0 }
          if NewSMS_Count > 0 then  { es sind neue Import-SMS vorhanden }
            if FAbrufLogFile <> nil then
              FAbrufLogFile.Write (IntToStr (NewSMS_Count) + ' SMS-Dateien importiert');  { Logfile-Protokollierung }
        end;  { if length (SMS_ImportDir_Cfg) > 0 }

        if SMSListe.Count > 0 then begin       { es sind SMS in der Liste vorhanden }
          Ruf_SMS_steht_an:=2;         // SMS steht an
          SendeSMSFiles:=true;  // neue SMS-Files wurden erzeugt und k�nnen versendet werden

          for i:=0 to SMSListe.Count - 1 do begin     { alle SMS abarbeiten }
            SMSListObj:=TSMSListObj (SMSListe [i]);
            { pr�fen, von welchem Ger�tetyp die SMS stammt:
              -> Lizenzfreischaltung f�r SMS: global �ber Programm-Funktion "SMS-Datenempfang",
                 keine ger�tetypspezifische Freischaltung (notwendig) }
            SMS_GeraeteTyp:=CheckSMSData_Geraetetyp (SMSListObj.Daten.SMS_Data);

            // SMS nach max. 24 h Stunden trotz Datenl�cke konvertieren:
            dtDatenlueckePruefung_bis:=IncHour (SMSListObj.Daten.DatumZeit, 24);
            // SMS in XML-Struktur konvertieren:
            if not KonvSMSToXMLFile (SMS_GeraeteTyp, SMSListObj.Daten.SMS_Data,
                                     SMSListObj.Daten.DatumZeit,
                                     dtDatenlueckePruefung_bis) then begin
              { bei Datenl�cke SMS-Rohdaten in Datei sichern f�r sp�tere erneute
                Konvertierungsversuche: 20.01.2009, WW }
              SaveSMSToTempFile (PathServer.PathName [WWorkDir], prefix_SMS_Roh,
                                 SMSListObj.Daten.SMS_Data, SMSListObj.Daten.DatumZeit);
              if FAbrufLogFile <> nil then
                FAbrufLogFile.Write ('SMS in Rohdatei gesichert');  { Logfile-Protokollierung }
            end;
          end;  { for i }

          { alle in Rohdateien gesicherte SMS nachkonvertieren: }
          if not SetArchivbitForAllSMSTempFiles (PathServer.PathName [WWorkDir],
                                                 prefix_SMS_Roh) then begin
            if FAbrufLogFile <> nil then  { Logfile-Protokollierung }
              FAbrufLogFile.Write ('Alle SMS-Rohdateien: Fehler beim Archivbit setzen', true, lt_Error);
          end;

          SMS_Rohdaten:=GetNextSMSFromTempFile (PathServer.PathName [WWorkDir],
                                                prefix_SMS_Roh, fas_NurArchivfaehige,
                                                sFileName, dtFileDate);
          while length (sFileName) > 0 do begin  // SMS-Rohdatei ist vorhanden
            if FAbrufLogFile <> nil then
              FAbrufLogFile.Write ('SMS-Rohdatei gelesen');  { Logfile-Protokollierung }

            SMS_GeraeteTyp:=CheckSMSData_Geraetetyp (SMS_Rohdaten);

            // SMS nach max. 24 h Stunden trotz Datenl�cke konvertieren:
            dtDatenlueckePruefung_bis:=IncHour (dtFileDate, 24);
            // SMS in XML-Struktur konvertieren:
            if not KonvSMSToXMLFile (SMS_GeraeteTyp, SMS_Rohdaten,
                                     dtFileDate, dtDatenlueckePruefung_bis) then begin
              { bei Datenl�cke Archiv-Bit der Rohdatei l�schen: }
              if not ClearFileArchivbit (sFileName) then begin
                if FAbrufLogFile <> nil then  { Logfile-Protokollierung }
                  FAbrufLogFile.Write ('SMS-Rohdatei: Fehler beim Archivbit l�schen', true, lt_Error);
              end;
            end else
              DeleteFile (sFilename);  // Konvertierung erfolgt, SMS-Rohdatei kann gel�scht werden

            SMS_Rohdaten:=GetNextSMSFromTempFile (PathServer.PathName [WWorkDir],
                                                  prefix_SMS_Roh, fas_NurArchivfaehige,
                                                  sFileName, dtFileDate);
          end;  { while length (sFileName) > 0 }
        end;  { if SMSListe.Count > 0 }
      finally
        SMSListe.Free;
      end;

      { SMS-Datei versenden:
        - wenn kein Fehler beim SMS-Lesen oder L�schen aufgetreten ist (Result ist leer)
        - SendeSMSFiles-Flag gesetzt ist und die letzte versendete SMS-Datei
          bereits quittiert ist }
      if (length (Result) = 0) AND
          SendeSMSFiles AND (length (LetztSMSFilename) = 0) then begin
        SendeSMSFiles:=false;
        { Client-SMS-Antwort aus Datei lesen, Dateiname wird mit zur�ckgegeben: }
        iDate:=Round (Date);
        if iDate <> SMS_WdhDatum then begin
          SMS_WdhDatum:=iDate;
          // einmal am Tag auch die zur Wiederholung anstehenden SMS-Dateien
          bOK_ArchBit:=SetArchivbitForAllSMSTempFiles (PathServer.PathName [WWorkDir],
                                                       prefix_SMS_XML);
          if FAbrufLogFile <> nil then begin                    { Logfile-Protokollierung }
            if bOK_ArchBit then
              FAbrufLogFile.Write ('Alle SMS-Dateien: Archivbit gesetzt (Ruf_SMS_Abfragen)')
            else
              FAbrufLogFile.Write ('Alle SMS-Dateien: Fehler beim Archivbit setzen (Ruf_SMS_Abfragen)', true, lt_Error);
          end;
        end;
        sXML:=GetNextSMSFromTempFile (PathServer.PathName [WWorkDir], prefix_SMS_XML,
                                      fas_NurArchivfaehige, sFileName, dtDummy);
        if length (sFileName) > 0 then begin  // SMS-Datei ist vorhanden
          if FAbrufLogFile <> nil then
            FAbrufLogFile.Write ('SMS-Datei gelesen (Ruf_SMS_Abfragen)');  { Logfile-Protokollierung }

          { SMS-Dateiname merken (mu� nach positiver Client-R�ckmeldung gel�scht werden !): }
          LetztSMSFilename:=sFileName;
          { Antwort f�r Client: }
          Result:=sXML;
        end;
      end;
    end

    else begin                        { MRG/DSfG-Rufentgegennahme }
      if Ruf_angekommen then begin
        inc (RingCount);
        { TickCount des letzten Rings merken f�r Ruf-Abbruch-�berwachung}
        TickCount_LastRing:=GetTickCount;

        { bei "Ring" gleich Ruf-Client benachrichtigen: }
        if FAbrufLogFile <> nil then
         FAbrufLogFile.Write ('Ruf steht an');  { Logfile-Protokollierung }

        if Assigned (CBSendAnswerToRuf_SMSEmpfangClient) then begin
          { Antwort f�r RE-Client: }
          RE_Antwort:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode, Kommando_Rufentgegennahme,
                                              '', rc_Ruf_steht_an,
                                              sRetFreierText,
                                              FDebugServiceIO, PathServer.PathName [WWorkDir]);
          CBSendAnswerToRuf_SMSEmpfangClient (RE_Antwort);
        end;

        { angekommenen Ruf erst nach MaxRings entgegennehmen: }
        if RingCount >= MaxRings then begin
          if RufTyp = re_MRG_FTL_SR then
            Ruf_SMS_steht_an:=2   // Ruf steht an, kein Folge-Kommando durch Client
          else
            Ruf_SMS_steht_an:=1;  // Ruf steht an, weiter mit Rufannahme durch Client

          if FAbrufLogFile <> nil then
           FAbrufLogFile.Write ('Ruf wird angenommen');  { Logfile-Protokollierung }

          Verbindung_OK:=false;
          try
            ProzessId_VerbAufbau:='';  // Vorbelegung: nicht verwendet/unbekannt; 14.03.2024, WW
            Ruf_Geraetetyp:=-1;  // Vorbelegung: nicht verwendet/unbekannt
            with VerbInfoData do begin
              DZ_VerbindungSteht:=0;  // Kennzeichnung, da� kein 'Verbindung hergestellt'-Zeitpunkt vorhanden ist
              DZ_Login:=0;  // Kennzeichnung, da� kein 'Login erfolgt'-Zeitpunkt vorhanden ist
            end;

            MRGMeldungsliste:=nil;
            try
              if isRufentgegennahmeDSfG then begin      { Rufentgegennahme DSfG }
                if Init_DSfG_Abruf (overIP, '', true, Fehlergruppe, Fehlercode) then begin  // TCP/IP-Rufentgegennahme �ber Client-Thread
                  Verbindung_OK:=TDSfGAbruf (Abruf).RufEntgegennahme (StationsKennung,
                                                                      VerbInfoData);
                  if not Verbindung_OK then begin
                    Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
                    sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW

                    { Fehler bei der Rufannahme (Verbindung steht): }
                    if not Abruf.NoCarrier then
                      TDSfGAbruf (Abruf).VerbAbbau;
                  end else
                    sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW

                  Neustart_Rufpolling:=true;  { Status: DSfG-Anrufannahme kennzeichnet den
                                                        Neustart des optionalen Ruf-Pollings
                                                        mit Modem-Initialisierung nach der Rufannahme }
                end;
              end
              else begin                                { Rufentgegennahme MRG }
                if RufTyp = re_MRG_FTL_SR then  // Anruf Tritschler Stationsrechner: liefert Meldungen
                  MRGMeldungsliste:=TMeldungsListe.Create (PathServer [WStammDir]);

                if Init_MRG_Abruf (false, '', Fehlergruppe, Fehlercode) then begin  // keine TCP/IP-Rufentgegennahme
                  Verbindung_OK:=TMRGAbruf (Abruf).RufEntgegennahme (RufTyp,
                                                                     StationsKennung,
                                                                     Ruf_Geraetetyp,
                                                                     MRGMeldungsliste,
                                                                     VerbInfoData);
                  if not Verbindung_OK then begin
                    Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
                    sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW

                    { Fehler bei der Rufannahme (Verbindung steht) bzw. immer bei FUP-Abruf: }
                    if not Abruf.NoCarrier OR TMRGAbruf (Abruf).isFupAbruf then
                      TMRGAbruf (Abruf).VerbAbbau;
                  end else
                    sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW

                  Neustart_Rufpolling:=true;  { Status: MRG-Anrufannahme kennzeichnet den
                                                        Neustart des optionalen Ruf-Pollings
                                                        mit FUP/Modem-Initialisierung nach der Rufannahme }
                end;
              end;

              { Logfile-Protokollierung von Fehlergruppe/-code: }
              if FAbrufLogFile <> nil then
                FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

              { Antwort f�r Client: }
              if Assigned (MRGMeldungsliste) then  // mit Meldungen
                Result:=GetClnt_XMLAntwort_Ruf_ME_VI (Fehlergruppe, Fehlercode, Kommando_Rufentgegennahme,
                                                      StationsKennung, rc_Ruf_angenommen_SMS_GPRS_MRG,
                                                      Ruf_Geraetetyp, MRGMeldungsliste,
                                                      VerbInfoData,
                                                      sRetFreierText,
                                                      FDebugServiceIO, PathServer.PathName [WWorkDir])
              else  // Standard: ohne Meldungen
                Result:=GetClnt_XMLAntwort_Ruf_VI (Fehlergruppe, Fehlercode, Kommando_Rufentgegennahme,
                                                   StationsKennung, rc_Ruf_angenommen_SMS_GPRS_MRG,
                                                   VerbInfoData,
                                                   sRetFreierText,
                                                   FDebugServiceIO, PathServer.PathName [WWorkDir]);
            finally
              MRGMeldungsliste.Free;
            end;
          finally
            { Abruf-Objekt freigeben, wenn das Entgegennehmen des Rufs nicht erfolgreich war: }
            if not Verbindung_OK then  // 14.03.2024, WW
              Free_Abruf;
            // Anm.: Das Abruf-Objekt mu� bei erfolgreichem Entgegennehmen des
            //       Rufs erhalten bleiben, damit ein ggf. nachfolgendes
            //       Verbindungsabbau-Kommando ausgef�hrt werden kann !
          end;
        end;  { if RingCount >= MaxRings }
      end  { if Ruf_angekommen }
      else begin
        { �berwachung Ruf-Abbruch:
          -> wenn die anrufende Station nach dem 1. "Ring" wieder aufgelegt hat
             und der Ruf bis dahin noch nicht angenommen wurde: }
        if RingCount > 0 then begin
          TickCount:=GetTickCount;
          if ((TickCount - TickCount_LastRing) > CTimeout_RingCheck) OR
              ((TickCount < TickCount_LastRing) AND (TickCount > CTimeout_RingCheck)) then begin
            RingCount:=0;
            if FAbrufLogFile <> nil then
              FAbrufLogFile.Write ('Ruf abgebrochen');  { Logfile-Protokollierung }

              { Antwort f�r Client: }
              Result:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode, Kommando_Rufentgegennahme,
                                              '', rc_Ruf_abgebrochen,
                                              sRetFreierText,
                                              FDebugServiceIO, PathServer.PathName [WWorkDir]);
          end;
        end;
      end;
    end;  { MRG/DSfG-Rufentgegennahme }
  end
  else begin   { Fehler beim Ruf-Polling aufgetreten }
    if FAbrufLogFile <> nil then                     { Logfile-Protokollierung }
      FAbrufLogFile.Write ('Fehler Ruf/SMS-Polling: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode),
                           true, lt_Error);

    { Antwort f�r Client: }
    Result:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode, Kommando_Rufentgegennahme,
                                    '', rc_Ruf_steht_nicht_an,
                                    sRetFreierText,
                                    FDebugServiceIO, PathServer.PathName [WWorkDir]);

    { durch Neustart wird versucht, die Rufpolling-St�rung zu beheben (Modem/FUP
      initialisieren bzw. reseten): }
    Neustart_Rufpolling:=true;
  end;
end;

{-----------------------------------------------------------}
function TAbrufCmdExec.Rufannahme (Kommando: string): string;
{-----------------------------------------------------------}
{ Rufannahme-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe: integer;
  Fehlercode: integer;
  RufannahmeCmdData: TRufannahmeCmdData;
  DSfGDfue_EAdr: string;
  DSfGAufmTelegrammList: TAufmTelegrammList;
  DSfG_RDeakt_Fehlergruppe: integer;
  DSfG_RDeakt_Fehlercode: integer;
  DSfG_RDeakt_RufNrZentrale_Alt: string;
  isRufannahmeDSfG: boolean;
  Lic_OK: boolean;
  GerTypCmd_Buf: integer;
  VerbInfoData: TVerbInfoData;
  overIP: boolean;
  sRetFreierText: string;
  Verbindung_OK: boolean;
{$IFDEF GAS-X}
  MrgTyp_Wieser: integer;
  bResourceOK: boolean;
{$ENDIF}

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);     // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  Verbindung_OK:=true;

  DSfGDfue_EAdr:='';   // Vorbelegung DSfG-DF�-Busadresse f�r R�ckgabe der DSfG-Rufannahmedaten
  DSfG_RDeakt_Fehlergruppe:=-1;       // Kennzeichnung, da� kein Rufdeaktivierungs-Ergebnis
  DSfG_RDeakt_Fehlercode:=-1;         // (Fehlergruppe/-code) f�r R�ckgabe vorhanden ist
  DSfG_RDeakt_RufNrZentrale_Alt:='';  // Kennzeichnung, da� keine Rufdeaktivierungs-Infodaten f�r R�ckgabe vorhanden sind
  with VerbInfoData do begin
    DZ_VerbindungSteht:=0;  // Kennzeichnung, da� kein 'Verbindung hergestellt'-Zeitpunkt vorhanden ist
    DZ_Login:=0;  // Kennzeichnung, da� kein 'Login erfolgt'-Zeitpunkt vorhanden ist
  end;

  // Rundpuffer auf COM-Logfile und Abruf-Logfile ausf�hren; 03.12.2013, WW
  if FDebugRundpufferMaxBytes > 0 then begin
    if Assigned (ComLogFile) then
      ComLogFile.DoRundpuffer (FDebugRundpufferMaxBytes);
    if Assigned (FAbrufLogFile) then
      FAbrufLogFile.DoRundpuffer (FDebugRundpufferMaxBytes);
  end;

  DSfGAufmTelegrammList:=TAufmTelegrammList.Create;
  try
    RufannahmeCmdData:=GetRufannahmeCmdData (Kommando);
    GerTypCmd_Buf:=RufannahmeCmdData.GeraeteTyp;   // Ger�tetyp aus Client-Kommando
    isRufannahmeDSfG:=(RufannahmeCmdData.GeraeteTyp = C_GeraeteTypDSfG) OR (RufannahmeCmdData.GeraeteTyp < 0);
{$IFDEF GAS-X}
    { die im Gas-X verwendete MRG-Typnummer in die entsprechende Wieser MRG-Typnummer
      umwandeln: }
    if not isRufannahmeDSfG then begin
      bResourceOK:=false;
      if Assigned (FResourceFilesList) then
        bResourceOK:=FResourceFilesList.GetMrgTypGasX (RufannahmeCmdData.GeraeteTyp,
                                                       MrgTyp_Wieser);  // 06.08.2021, WW
      if bResourceOK then
        RufannahmeCmdData.GeraeteTyp:=MrgTyp_Wieser
      else begin
        Fehlergruppe:=ST_KONFIGERROR;
        Fehlercode:=KFERR_KONFIGDATANOTFOUND;
      end;
    end;
{$ENDIF}

    if (Fehlergruppe = 0) AND (Fehlercode = 0) then begin
      // pr�fen, ob im Lizenzfile die system-globale Funktionalit�t 'Rufentgegennahme'
      // f�r MRG bzw. DSfG freigeschaltet ist:
      if isRufannahmeDSfG then
        Lic_OK:=FLizenzList.ReadGlobaleFunktionFromLizenzFile (FNExt_RufentgegennahmeDSfG)
      else
        Lic_OK:=FLizenzList.ReadGlobaleFunktionFromLizenzFile (FNExt_RufentgegennahmeMRG);
      if Lic_OK then begin
        // MRG oder DSfG-Ruf annehmen (abh�ngig vom Ger�tetyp im Rufannahme-Kommando):
        overIP:=FCOMNr < 0;  // IP-Abruf, wenn COMNr negativ

        if isRufannahmeDSfG then begin
          if Init_DSfG_Abruf (overIP, '', true, Fehlergruppe, Fehlercode) then begin  // TCP/IP-Rufentgegennahme �ber Client-Thread
            // merken als "Soll-Prozess-Id" f�r alle weiteren Kommandos des DSfG-Abrufs:
            ProzessId_VerbAufbau:=GetAbrufKommandoProzessID (Kommando);
            GeraeteTyp_Cmd:=GerTypCmd_Buf;   // merken f�r XML-Antwort-R�ckgaben
            // merken f�r Daten-R�ckgabe:
            StationsKennung:=RufannahmeCmdData.Kennung;
            // Transparentmodus-Flag setzen f�r Abruf/-Konvertierung von DSfG-Daten
            // bei DSfG-Rufentgegennahme (immer transparent schalten, da Datenelemente
            // von DSfG-Instanzen abgerufen werden, nie DSfG-DF�-Momentanwerte):
            DSfG_TransparentModus_VerbAufbau:=true;
            // Tracelog-Ausgabe in XML-Antworten setzen (deaktiviert, da f�r
            // Rufannahme DSfG derzeit nicht vorgesehen)
            TraceLog_VerbAufbau:=C_CmdTraceLog_Deaktiviert;

            { DSfG-Ruf annehmen (Login durchf�hren): }
            Verbindung_OK:=TDSfGAbruf (Abruf).Rufannahme (RufannahmeCmdData,
                                                          DSfGAufmTelegrammList,
                                                          DSfG_RDeakt_RufNrZentrale_Alt,
                                                          DSfG_RDeakt_Fehlergruppe,
                                                          DSfG_RDeakt_Fehlercode,
                                                          DSfGDfue_EAdr,
                                                          VerbInfoData);
            sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW

            if not Verbindung_OK then begin
              Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
              { Fehler bei der Rufannahme (Verbindung steht): }
              if not Abruf.NoCarrier then
                TDSfGAbruf (Abruf).VerbAbbau;
            end;
          end; { if Init_DSfG_Abruf }
        end
        else begin
          if Init_MRG_Abruf (false, '', Fehlergruppe, Fehlercode) then begin  // keine TCP/IP-Rufentgegennahme
            { MRG-Ger�tetyp-Lizenz pr�fen: }
            if FLizenzList.GetLizenzMrgTyp (RufannahmeCmdData.GeraeteTyp) <> 0 then begin
              // merken als "Soll-Prozess-Id" f�r alle weiteren Kommandos des MRG-Abrufs:
              ProzessId_VerbAufbau:=GetAbrufKommandoProzessID (Kommando);
              GeraeteTyp_Cmd:=GerTypCmd_Buf;   // merken f�r XML-Antwort-R�ckgaben
              // merken f�r Daten-R�ckgabe:
              StationsKennung:=RufannahmeCmdData.Kennung;
              // Transparentmodus eigtl. nur bei DSfG wichtig, trotzdem auch hier
              // der Vollst�ndigkeit halber zuweisen:
              DSfG_TransparentModus_VerbAufbau:=true;
              // Tracelog-Ausgabe in XML-Antworten setzen (deaktiviert, da f�r
              // Rufannahme MRG derzeit nicht vorgesehen)
              TraceLog_VerbAufbau:=C_CmdTraceLog_Deaktiviert;

              { MRG-Ruf annehmen (Login durchf�hren): }
              Verbindung_OK:=TMRGAbruf (Abruf).Rufannahme (RufannahmeCmdData,
                                                           VerbInfoData);
              Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
              sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW

              if not Verbindung_OK then begin
                { Fehler bei der Rufannahme (Verbindung steht) bzw. immer bei FUP-Abruf: }
                if not Abruf.NoCarrier OR TMRGAbruf (Abruf).isFupAbruf then
                  TMRGAbruf (Abruf).VerbAbbau;
              end;
            end
            else begin
              // MRG-Ger�tetyp nicht lizenziert:
              Fehlergruppe:=SYS_LICENCEERROR;
              Fehlercode:=LICENCEERR_GERAETETYP;
              // bestehende Verbindung trennen
              TMRGAbruf (Abruf).VerbAbbau;
              Verbindung_OK:=false;
            end;
          end;  { if Init_MRG_Abruf }
        end;
      end
      else begin
        // Programmfunktion nicht lizenziert:
        Fehlergruppe:=SYS_LICENCEERROR;
        Fehlercode:=LICENCEERR_PROGFUNKTION;
      end;
    end;  { if (Fehlergruppe = 0) AND (Fehlercode = 0) then }

    { Logfile-Protokollierung von Fehlergruppe/-code: }
    if FAbrufLogFile <> nil then
      FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

    { Antwort f�r Client: }
    if isRufannahmeDSfG then
      Result:=GetClnt_XMLAntwort_RufannahmeDSfG (Fehlergruppe, Fehlercode, Kommando,
                                                 StationsKennung,
                                                 DSfGDfue_EAdr,
                                                 DSfGAufmTelegrammList,
                                                 DSfG_RDeakt_RufNrZentrale_Alt,
                                                 DSfG_RDeakt_Fehlergruppe,
                                                 DSfG_RDeakt_Fehlercode,
                                                 VerbInfoData,
                                                 sRetFreierText,
                                                 FDebugServiceIO, PathServer.PathName [WWorkDir])
    else
      Result:=GetClnt_XMLAntwort_RufannahmeMRG (Fehlergruppe, Fehlercode, Kommando,
                                                StationsKennung,
                                                VerbInfoData,
                                                sRetFreierText,
                                                FDebugServiceIO, PathServer.PathName [WWorkDir]);
  finally
    DSfGAufmTelegrammList.Free;
  end;

  { Abruf-Objekt freigeben, wenn das Annehmen des Rufs nicht erfolgreich war: }
  if not Verbindung_OK then
    Free_Abruf;
end;

{---------------------------------------------------------}
function TAbrufCmdExec.Rufliste (Kommando: string): string;
{---------------------------------------------------------}
{ MRG-Ruflistenabfrage-Kommando ausf�hren (bei Wieser-DSfG-Umleitung);
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ProzessId: string;
  Rufliste: string;
  sRetFreierText: string;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  Rufliste:='';  // Vorbelegung: Rufliste leer

  if Abruf <> nil then begin
    ProzessId:=GetAbrufKommandoProzessID (Kommando);

    // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
    if ProzessId = ProzessId_VerbAufbau then begin
      if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
        // MRG-Rufliste abfragen:
        if not TMRGAbruf (Abruf).RuflisteAbfragen (Rufliste) then
          Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

        sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
      end
      else begin                                                 { DSfG-Abruf }
        { Ruflistenabfrage gibts bei DSfG nicht }
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
      end;
    end
    else begin
      Fehlergruppe:=SYS_ABRUFERROR;
      Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
    end;
  end
  else begin
    Fehlergruppe:=COM_KOMMERROR;
    Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if FAbrufLogFile <> nil then
    FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

  { XML-Antwort f�r Client: }
  Result:=GetClnt_XMLAntwort_Rufliste (Fehlergruppe, Fehlercode, Kommando,
                                       StationsKennung, GeraeteTyp_Cmd,
                                       Rufliste,
                                       sRetFreierText,
                                       FDebugServiceIO, PathServer.PathName [WWorkDir]);
end;

{--------------------------------------------------------------------}
function TAbrufCmdExec.SlaveRufQuittierung (Kommando: string): string;
{--------------------------------------------------------------------}
{ Slave-Rufquittierung-Kommando ausf�hren (bei Wieser-DSfG-Umleitung);
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ProzessId: string;
  Adresse: string;
  sRetFreierText: string;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  if Abruf <> nil then begin
    Adresse:=GetSlaveRufQuittCmdData (Kommando);
    ProzessId:=GetAbrufKommandoProzessID (Kommando);

    // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
    if ProzessId = ProzessId_VerbAufbau then begin
      if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
        // Slave-Ruf quittieren:
        if not TMRGAbruf (Abruf).SlaveRufQuittieren (Adresse) then
          Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

        sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
      end
      else begin                                                 { DSfG-Abruf }
        { Slave-Rufquittierung gibts bei DSfG nicht }
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
      end;
    end
    else begin
      Fehlergruppe:=SYS_ABRUFERROR;
      Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
    end;
  end
  else begin
    Fehlergruppe:=COM_KOMMERROR;
    Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if FAbrufLogFile <> nil then
    FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

  { XML-Antwort f�r Client: }
  Result:=GetClnt_XMLAntwort (Fehlergruppe, Fehlercode, Kommando,
                              StationsKennung,
                              sRetFreierText,
                              FDebugServiceIO, PathServer.PathName [WWorkDir]);
end;

{---------------------------------------------------------}
function TAbrufCmdExec.Rueckruf (Kommando: string): string;
{---------------------------------------------------------}
{ R�ckruf-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  Fehlergruppe, Fehlercode: integer;
  ProzessId: string;
  Zentrale: integer;
  sRetFreierText: string;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  if Abruf <> nil then begin
    Zentrale:=GetRueckrufCmdData (Kommando);
    ProzessId:=GetAbrufKommandoProzessID (Kommando);

    // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
    if ProzessId = ProzessId_VerbAufbau then begin
      if Abruf is TMRGAbruf then begin                            { MRG-Abruf }
        // R�ckruf im MRG ausl�sen:
        if not TMRGAbruf (Abruf).RueckrufAusloesung (Zentrale) then
          Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);

        sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
      end
      else begin                                                 { DSfG-Abruf }
        { R�ckruf-Ausl�sung gibts bei DSfG nicht }
        Fehlergruppe:=SYS_ABRUFERROR;
        Fehlercode:=SYSABRFERR_KOMMANDOUNGUELTIG;
      end;
    end
    else begin
      Fehlergruppe:=SYS_ABRUFERROR;
      Fehlercode:=SYSABRFERR_KOMMANDOSOLLPROZESSID;
    end;
  end
  else begin
    Fehlergruppe:=COM_KOMMERROR;
    Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if FAbrufLogFile <> nil then
    FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));

  { XML-Antwort f�r Client: }
  Result:=GetClnt_XMLAntwort (Fehlergruppe, Fehlercode, Kommando,
                              StationsKennung,
                              sRetFreierText,
                              FDebugServiceIO, PathServer.PathName [WWorkDir]);
end;

{---------------------------------------------------------}
function TAbrufCmdExec.ZeitAbruf (Kommando: string): string;  // 17.08.2010  // 13.01.2011 
{---------------------------------------------------------}
{ Zeitabruf-Kommando ausf�hren;
  �bergabe: Kommando-String
  Ergebnis: XML-Antwort-String auf Kommando }
var
  iFehlergruppe, iFehlercode      : integer;
  sKennung, sProzessId            : string;
  sUnixTime, sTimeBias, sTimeInfo : string;
  sRetFreierText: string;
  ResponseTraceLogList: TDSfGDataList;
  cEAdr: char;

begin
  Init_FehlerGruppeCode(iFehlergruppe, iFehlercode);  // Vorbelegung f�r Fehlergruppe/-code: OK
  sRetFreierText:='';

  // Bei aktiviertem Tracelog, die zugeh�rige Responseliste anlegen; 03.01.2022, WW
  if TraceLog_VerbAufbau = C_CmdTraceLog_Aktiviert then
    ResponseTraceLogList:=TDSfGDataList.Create
  else
    ResponseTraceLogList:=nil;
  try
    if (Abruf <> nil) then begin
      sProzessId := GetAbrufKommandoProzessID(Kommando);
      sKennung := GetAbrufKommandoKennung(Kommando);     // 24.03.2011

      // pr�fen, ob ProzessId aus Kommando mit "Soll-ProzessId" aus Verbindungsaufbau-Kommando �bereinstimmt:
      if (sProzessId = ProzessId_VerbAufbau) then begin
        // COM-Tracelog createn, wenn Response-Tracelog-Liste vorliegt; 03.01.2022, WW
        if Assigned (ResponseTraceLogList) then
          Abruf.CreateCOMTraceLog
        else
          Abruf.FreeCOMTraceLog;
        try
          if (Abruf is TMRGAbruf) then begin          { MRG-Abruf }
            // Kennung ist f�r Weitergabe Kennung<#9>Typ
            sKennung := StationsKennung;                   // 24.03.2011
            sKennung := sKennung + #9 + IntToStr(GeraeteTyp_Cmd);
            { Zeitabruf f�r MRG durchf�hren }
            if (not TMRGAbruf(Abruf).ZeitAbruf(sUnixTime, sTimeBias, sTimeInfo)) then
              Abruf.GetFehlerGruppeCode(iFehlergruppe, iFehlercode);

            sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
            // COM-Tracelog in TraceLog-Liste eintragen; 03.01.2022, WW
            Abruf.AddCOMTraceLogToList (ResponseTraceLogList);
          end
          else if (Abruf is TDSfGAbruf) and (Length(sKennung) = 1) then begin    { DSfG-Abruf }
            sTimeInfo := 'MEZ-1MESZ-2';  // fix bei DSfG
            cEAdr:=sKennung[1];  // Busadresse
            // Zeitabruf f�r DSfG durchf�hren
            if (not TDSfGAbruf(Abruf).ZeitAbruf(cEAdr, sUnixTime, sTimeBias)) then
              Abruf.GetFehlerGruppeCode(iFehlergruppe, iFehlercode);

            sRetFreierText:=Abruf.RetFreierText;  // 13.03.2018, WW
            // COM-Tracelog f�r Busadresse der Instanz in TraceLog-Liste eintragen; 03.01.2022, WW
            Abruf.AddCOMTraceLogToList (ResponseTraceLogList, cEAdr);
          end
          else begin                                  // Sonstiges ...
            iFehlergruppe := SYS_ABRUFERROR;
            iFehlercode := SYSABRFERR_KOMMANDOUNGUELTIG;
          end;
        finally
          Abruf.FreeCOMTraceLog;  // COM-Tracelog freigeben
        end;
      end
      else begin
        iFehlergruppe := SYS_ABRUFERROR;
        iFehlercode := SYSABRFERR_KOMMANDOUNGUELTIG;
      end;
    end
    else begin
      iFehlergruppe := COM_KOMMERROR;
      iFehlercode := KOMMERR_VERB_UNTERBROCHEN;
    end;

    { Logfile-Protokollierung von Fehlergruppe/-code: }
    if (FAbrufLogFile <> nil) then
      FAbrufLogFile.Write(
        'Ergebnis: ' + IntToStr (iFehlergruppe) + ', ' + IntToStr (iFehlercode));

    { XML-Antwort f�r Client: }
    Result := GetClnt_XMLAntwort_ZeitAbruf(iFehlergruppe, iFehlercode, Kommando,
      StationsKennung, sKennung, sUnixTime, sTimeBias, sTimeInfo,
      ResponseTraceLogList,
      sRetFreierText, PathServer.PathName [WWorkDir], FDebugServiceIO);
  finally
    ResponseTraceLogList.Free;
  end;
end;

{---------------------------------}
procedure TAbrufCmdExec.VerbHalten;
{---------------------------------}
{ Aktion zum Halten der Verbindung zur Station ausf�hren }
var
  Fehlergruppe, Fehlercode: integer;

begin
  Init_FehlerGruppeCode (Fehlergruppe, Fehlercode);   // Vorbelegung f�r Fehlergruppe/-code: OK

  if Abruf <> nil then begin
    if Assigned (CommObj) then begin
      if Abruf is TMRGAbruf then begin                    { MRG-Abruf }
        if (CommObj is TMRGModemCommObj) OR   { bei Modem-Verbindung; 14.04.2023, WW }
           (CommObj is TMRGClientSocketCommObj) then begin  { bei IP-Verbindung; 14.04.2023, WW }
            if FAbrufLogFile <> nil then
              FAbrufLogFile.Write ('Verbindung halten');  { Logfile-Protokollierung }

            if not TMRGAbruf (Abruf).VerbHalten then
              Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
        end else
          exit;  // nicht bei serieller Verbindung
      end
      else begin                                         { DSfG-Abruf }
        if (CommObj is TDSfGModemCommObj) OR   { bei Modem-Verbindung; 27.03.2013, WW }
           (CommObj is TDSfGClientSocketCommObj) then begin  { bei IP-Verbindung; 14.04.2023, WW }
          if DSfG_TransparentModus_VerbAufbau then begin  { nur, wenn DSfG-Bus transparent geschaltet ist }
            if FAbrufLogFile <> nil then
              FAbrufLogFile.Write ('Verbindung halten');  { Logfile-Protokollierung }

            if not TDSfGAbruf (Abruf).VerbHalten then
              Abruf.GetFehlerGruppeCode (Fehlergruppe, Fehlercode);
          end else
            exit;  // nicht, wenn DSfG-Bus nicht transparent geschaltet ist
        end else
          exit;  // nicht bei serieller Verbindung
      end;
    end
    else begin
      if FAbrufLogFile <> nil then
        FAbrufLogFile.Write ('Verbindung halten, CommObj = nil');  { Logfile-Protokollierung }

      Fehlergruppe:=COM_KOMMERROR;
      Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
    end;
  end
  else begin
    if FAbrufLogFile <> nil then
      FAbrufLogFile.Write ('Verbindung halten, Abruf = nil');  { Logfile-Protokollierung }

    Fehlergruppe:=COM_KOMMERROR;
    Fehlercode:=KOMMERR_VERB_UNTERBROCHEN;
  end;

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if FAbrufLogFile <> nil then
    FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode));
end;


{------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------}
function TAbrufCmdExec.KonvSMSToXMLFile (AGeraeteTyp: integer; ASMS_Data: string;
  ASMS_DatumZeit, ADatumZeit_DatenlueckePruefung_bis: TDateTime): boolean;
{-------------------------------------------------------------------------------}
{ Konvertiert Rohdaten einer SMS in Datei mit XML-Format (Client-Antwort);
  �bergabe: Ger�tetyp
            SMS-Rohdaten
            SMS-Zeitstempel
            Zeitpunkt, bis zu dem die SMS-Konvertierung bei einer auftretenden
              Datenl�cke nicht durchgef�hrt wird
  Ergebnis: false, wenn Konvertierung aufgrund einer Datenl�cke nicht durchgef�hrt
            wurde }
var
  SMSKonvMRGRec: TSMSKonvMRG;
  SMSKonvDSfGRec: TSMSKonvDSfG;
  MaxMessKanal: integer;
  MaxTagKanal: integer;
  SMSKonvErg: integer;
  SMS_Kennung: string;
  MrgDefData: TMrgDefData;
  Fehlergruppe, Fehlercode: integer;
  sXML: string;
  Datentyp: integer;
  j: integer;
  Filename: string;
  bDatenlueckePruefungOK: boolean;
  bResourceOK: boolean;

begin
  Result:=true;
  Init_FehlergruppeCode (Fehlergruppe, Fehlercode);

  case AGeraeteTyp of
    mrgtyp_Veribox_Mini:
      begin  // Ger�te mit MRG-SMS-Daten
        { MRG-SMS-Konvertierungsrecord belegen: }
        SMSKonvMRGRec.SMS_Data:=ASMS_Data;  { Datenteil einer SMS }
        SMSKonvMRGRec.GeraeteTyp:=AGeraeteTyp;
        { Messwert-Zielfile mit eindeutigem Namen wird hier bereits erzeugt: }
        SMSKonvMRGRec.StdZieldateiName:=CreateZwischenDateiNameTemp (dd_Mess);
        try
          { Tagessatz-Zielfile mit eindeutigem Namen wird hier bereits erzeugt: }
          SMSKonvMRGRec.TagZieldateiName:=CreateZwischenDateiNameTemp (dd_Tags);
          try
            SMSKonvMRGRec.WSystemPfad:=ExtractFilePath (ParamStr(0));
            SMSKonvMRGRec.dtDatenlueckePruefung_bis:=ADatumZeit_DatenlueckePruefung_bis;

            MaxMessKanal:=-1;  { Vorbelegung: keine SMS-Daten f�r R�ckgabe an Client vorhanden }
            MaxTagKanal:=-1;
            { SMS in MRG-Datenstruktur konvertieren: Kennung wird zur�ckgegeben }
            SMSKonvErg:=0;  // OK
            if KonvSMS_MRG (SMSKonvMRGRec, SMS_Kennung, bDatenlueckePruefungOK) then begin
              if bDatenlueckePruefungOK then begin  // Konvertierung OK, keine Datenl�cke
                { ger�tespezifische Kanalzahlen f�r Messwert- und Z�hlerstandsdaten
                  aus Konfiguration lesen }
                bResourceOK:=false;
                if Assigned (FResourceFilesList) then
                  bResourceOK:=FResourceFilesList.GetMrgDefData (AGeraeteTyp, MrgDefData);  // 06.08.2021, WW
                if bResourceOK then begin
                  MaxMessKanal:=MrgDefData.AnzahlKanaele;
                  MaxTagKanal:=MrgDefData.AnzahlZaehlerKanaele;
                end
                else begin
                  Fehlergruppe:=ST_KONFIGERROR;
                  Fehlercode:=KFERR_KONFIGDATANOTFOUND;
                end;
              end
              else begin  // Konvertierung nicht durchgef�hrt, da sonst Datenl�cke
                Result:=false;
                exit;
              end;
            end
            else begin   { SMS enth�lt keine g�ltigen MRG-Daten }
              Fehlergruppe:=ST_DATACHECK;
              Fehlercode:=DCH_MWINVALID;
            end;

            { Client-SMS-Antwort bilden: }
            if (MaxMessKanal > -1) AND (MaxTagKanal > -1) then  { Ruf-Antwort (MRG-SMS) mit MRG-Daten }
              sXML:=GetClnt_XMLAntwort_SMS_MWTA (Fehlergruppe, Fehlercode,
                                                 Kommando_Rufentgegennahme,
                                                 SMS_Kennung, rc_Ruf_angenommen_SMS_GPRS_MRG,
                                                 AGeraetetyp,
                                                 SMSKonvMRGRec.StdZieldateiName,
                                                 SMSKonvMRGRec.TagZieldateiName,
                                                 MaxMessKanal, MaxTagKanal,
                                                 -1,  // Umrechnung Gastag -> physik. Tag deaktiviert (TA-Daten
                                                      // aus Veribox tragen das physikalische Datum)
                                                 '',  // unbenutzt
                                                 FDebugServiceIO,
                                                 PathServer.PathName [WWorkDir])
            else  { Ruf-Antwort (MRG-SMS) ohne Daten: }
              sXML:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode,
                                            Kommando_Rufentgegennahme,
                                            SMS_Kennung, rc_Ruf_angenommen_SMS_GPRS_MRG,
                                            '',  // unbenutzt
                                            FDebugServiceIO,
                                            PathServer.PathName [WWorkDir]);
            { Client-SMS-Antwort in Datei sichern: }
            SaveSMSToTempFile (PathServer.PathName [WWorkDir], prefix_SMS_XML, sXML, ASMS_DatumZeit);

            { Messwert/Tagessatz-Files k�nnen jetzt gel�scht werden: }
          finally
            if length (SMSKonvMRGRec.TagZieldateiName) > 0 then
              DeleteFile (SMSKonvMRGRec.TagZieldateiName);
          end;
        finally
          if length (SMSKonvMRGRec.StdZieldateiName) > 0 then
            DeleteFile (SMSKonvMRGRec.StdZieldateiName);
        end;
      end;

    mrgtyp_MRG910:
      begin  // Ger�te mit DSfG-SMS-Daten
        { DSfG-SMS-Konvertierungsrecord belegen: }
        SMSKonvDSfGRec.SMS_Data:=ASMS_Data;  { Datenteil einer SMS }
        SMSKonvDSfGRec.GeraeteTyp:=AGeraeteTyp;
        SMSKonvDSfGRec.ZielPfad:=PathServer.PathName [WWorkDir];
        SMSKonvDSfGRec.ArLbDatenListe:=TDSfGDataList.Create;  { Archiv/Logbuch-Datenliste }
        try
          { SMS in DSfG-Datenstruktur konvertieren: Kennung wird zur�ckgegeben }
          if not KonvSMS_DSfG (SMSKonvDSfGRec, SMS_Kennung, Datentyp,
                               SMSKonvErg) then begin
            { SMS enth�lt keine g�ltigen DSfG-Daten }
            Fehlergruppe:=ST_DATACHECK;
            if Datentyp = C_IsArchive then
              Fehlercode:=DCH_ARINVALID
            else if Datentyp = C_IsLogbuecher then
              Fehlercode:=DCH_LBINVALID
            else
              Fehlercode:=DCH_INVALID;
          end;

          { Client-SMS-Antwort bilden: }
          if Datentyp = C_IsArchive then  { Ruf-Antwort (DSfG-SMS) mit Archivkanal-Daten }
            sXML:=GetClnt_XMLAntwort_AR (Fehlergruppe, Fehlercode,
                                         Kommando_Rufentgegennahme,
                                         SMS_Kennung,
                                         SMSKonvDSfGRec.ArLbDatenListe, nil, nil, nil,
                                         -1, -1, true, rc_Ruf_SMS_GPRS_DSfG,
                                         '',  // unbenutzt
                                         FDebugServiceIO, PathServer.PathName [WWorkDir],
                                         ISO646_Cfg)
          else if Datentyp = C_IsLogbuecher then  { Ruf-Antwort (DSfG-SMS) mit Logbuch-Daten }
            sXML:=GetClnt_XMLAntwort_LB (Fehlergruppe, Fehlercode,
                                         Kommando_Rufentgegennahme,
                                         SMS_Kennung,
                                         SMSKonvDSfGRec.ArLbDatenListe, nil, nil, nil,
                                         -1, -1, true, rc_Ruf_SMS_GPRS_DSfG,
                                         '',  // unbenutzt
                                         FDebugServiceIO, PathServer.PathName [WWorkDir])
          else  { Ruf-Antwort (DSfG-SMS) ohne Daten: }
            sXML:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode,
                                          Kommando_Rufentgegennahme,
                                          SMS_Kennung, rc_Ruf_SMS_GPRS_DSfG,
                                          '',  // unbenutzt
                                          FDebugServiceIO,
                                          PathServer.PathName [WWorkDir]);
          { Client-SMS-Antwort in Datei sichern: }
          SaveSMSToTempFile (PathServer.PathName [WWorkDir], prefix_SMS_XML, sXML, ASMS_DatumZeit);

          { DSfG-Datenfiles k�nnen jetzt gel�scht werden: }
          for j:=0 to SMSKonvDSfGRec.ArLbDatenListe.Count-1 do begin
            Filename:=SMSKonvDSfGRec.ArLbDatenListe [j];
            if length (Filename) > 0 then
              DeleteFile (Filename);
          end;
        finally
          SMSKonvDSfGRec.ArLbDatenListe.Free;
        end;
      end;
  else  // unbekannter Ger�tetyp
    SMSKonvErg:=0;  // OK
    SMS_Kennung:='';  // damit auch Kennung unbekannt
    { SMS enth�lt keine g�ltigen Daten }
    Fehlergruppe:=ST_DATACHECK;
    Fehlercode:=DCH_INVALID;

    { Client-SMS-Antwort bilden (Ruf-Antwort (unbekannte SMS) ohne
      Daten): }
    sXML:=GetClnt_XMLAntwort_Ruf (Fehlergruppe, Fehlercode,
                                  Kommando_Rufentgegennahme,
                                  SMS_Kennung, rc_SMS_GPRS_unbekannt,
                                  '',  // unbenutzt
                                  FDebugServiceIO,
                                  PathServer.PathName [WWorkDir]);
    { Client-SMS-Antwort in Datei sichern: }
    SaveSMSToTempFile (PathServer.PathName [WWorkDir], prefix_SMS_XML, sXML, ASMS_DatumZeit);
  end;  { case AGeraeteTyp }

  { Logfile-Protokollierung von Fehlergruppe/-code: }
  if FAbrufLogFile <> nil then
    FAbrufLogFile.Write ('Ergebnis: ' + IntToStr (Fehlergruppe) + ', ' + IntToStr (Fehlercode) +
                        ', ' + IntToStr (SMSKonvErg));
end;

{------------------------------------------------------------------------------}

{------------------------------------------}
procedure TAbrufCmdExec.ReadSrvCfg_ModemIni;
{------------------------------------------}
{ SrvCfgIni/ModemIni-Konfigurationen f�r Abruf lesen }
var
  MI: TModemIni;
  SI: TSrvCfg32Ini;

begin
  MI:=TModemIni.Create (PathServer.Pathname [WNetProgDir]);
  try
    SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
    try
      Device:=SI.Device [FCOMNr];
      if UpperCase (Device) = UpperCase (Devices [devFUP]) then begin
        MaxRings:=0;                     { bei FUP-Ruf: Ruf sofort annehmen }
        Keep_COM_Open:=true;  // wenn ein FUP dranh�ngt: COM offen halten, sonst
                              // macht der FUP bei jedem �ffnen der COM einen Reset
                              // und das dauert recht lange !
      end else
        MaxRings:=SI.RingCount [FCOMNr];  { bei Modem-Ruf: Ruf nach SI.RingCount annehmen }

      if UpperCase (Device) = UpperCase (Devices [devSeriell]) then begin  // 09.08.2011, WW
        ModemName:='';  // Modem nicht verwendet
        MaxBaudrate:=0;  // Max. Modem-Baudrate nicht verwendet
        Modem_Databits_Parity_Stopbits:='';  // Spezifische Modem-Schnittstellenparameter nicht verwendet
      end
      else begin
        ModemName:=SI.Modemname [FCOMNr];
        MaxBaudrate:=MI.GetMaxBaud(ModemName);
        Modem_Databits_Parity_Stopbits:=MI.GetDPS (ModemName);
      end;

      if FCOMNr < 0 then begin  // IP-Abruf, wenn COMNr negativ; 08.01.2018, WW
        if FCOMNr < COffsetIPLinien_Ruf then  // wenn IP-Rufentgegennahme-Linie
          RufTyp:=re_DSfG  // bislang nur DSfG-Rufentgegennahme
        else
          RufTyp:=re_Aus;
      end else
        RufTyp:=SI.Ruf [FCOMNr];

      FupHandshake:=SI.FupHandshake [FCOMNr];
      Vorwahl_GasX:=SI.GetVorwahl_DefaultBlank (FCOMNr);  // Vorwahl f�r Gas-X-Version; 21.11.2013
    finally
      SI.Free;
    end;
  finally
    MI.Free;
  end;
end;

{-----------------------------------------}
procedure TAbrufCmdExec.ReadProgramIni_MRG;
{-----------------------------------------}
{ MRG-relevante Programm-Ini-Einstellungen lesen }
var
  AI: TAbrufSrvIni;

begin
  AI:=TAbrufSrvIni.Create;
  try
    { f�r MRG-Zeitsynchronisation: }
    ZeitSyncAbweichungMin_Cfg:=AI.ZeitSyncAbweichungMin;
    ZeitSyncAbweichungMax_Cfg:=AI.ZeitSyncAbweichungMax;
    ZeitSyncKorrekturMax_Cfg:=AI.ZeitSyncKorrekturMax;

{$IFDEF GAS-X}
    { f�r MRG-Analogmesswerte-R�ckgabe an Gas-X: }
    GasX_AIX_kompatibel_MRG_Analog_Cfg:=AI.GasX_AIX_kompatibel_MRG_Analog;
{$ENDIF}

    { f�r Bilden einer Ersatz-Kennung beim Kennungsvergleich (STGW): }
    KennungExt_Cfg:=AI.MRG_KennungExt;

    { f�r SMS-Empfang: }
    SMS_BackupDir_Cfg:=AI.SMS_BackupDir;
    SMS_ImportDir_Cfg:=AI.SMS_ImportDir;

    { f�r Halten der Verbindung zur Station: }
    VerbindungHalten_Cfg:=AI.VerbindungHalten;

    with MRGTimeouts do begin                        { Timeouts f�r MRG-Abruf }
      ModemAntwort:=AI.TOModemAntwort;
      ModemInit:=AI.TOModemInit;
      GSMModem:=AI.TOGSMModem;
      FupAntwort:=AI.MRG_TOFupAntwort;
      FupReset:=AI.MRG_TOFupReset;
      CRCCheck:=AI.MRG_TOCRCCheck;
      ACK01_ProtMeldung:=AI.MRG_TOACK01ProtMeldung;
      Verbindungsaufbau:=AI.MRG_TOVerbindungsaufbau;
      Verbindungsabbau:=AI.MRG_TOVerbindungsabbau;
      RufAnnahmeModem:=AI.MRG_TORufAnnahmeModem;
      Kennung:=AI.MRG_TOKennung;
      Login:=AI.MRG_TOLogin;
      Parameter:=AI.MRG_TOParameter;
      Meldungen:=AI.MRG_TOMeldungen;
      Messwerte:=AI.MRG_TOMesswerte;
      Tagessaetze:=AI.MRG_TOTagessaetze;
      Pruefsaetze:=AI.MRG_TOPruefsaetze;
      Binaerdatei:=AI.MRG_TOBinaerdatei;
      RundpufferReset:=AI.MRG_TORundpufferReset;
      Parametrieren:=AI.MRG_TOParametrieren;
      Rufausloesung:=AI.MRG_TORufausloesung;
      DSfGUmschaltung:=AI.MRG_TODSfGUmschaltung;
      DSfGRufliste:=AI.MRG_TODSfGRufliste;
      DSfGRufQuittung:=AI.MRG_TODSfGRufQuittung;
      IEC1107Telegr:=AI.MRG_TOIEC1107Telegr;
      TritschlerIECProt:=AI.MRG_TOTritschlerIECProt;
      TritschlerFTLProt:=AI.MRG_TOTritschlerFTLProt;
      CorusSAMProt:=AI.MRG_TOCorusSAMProt;
      ElsterDS100Prot:=AI.MRG_TOElsterDS100Prot;
      ModbusProt:=AI.MRG_TOModbusProt;  // 08.03.2019, WW
    end;  { with }

    with MRGModemVersuche do begin       { Fehler-Versuche f�r MRG-Modemabruf }
      CRC:=AI.MRG_CRCVersuche;
      BCC:=AI.MRG_BCCVersuche;
      ACK01Prot:=AI.MRG_ACK01ProtVersuche;
      FTLProt:=AI.MRG_FTLProtVersuche;
      ModbusProtLRC_CRC:=AI.MRG_ModbusProtLRC_CRCVersuche;  // 08.03.2019, WW
    end;  { with }
  finally
    AI.Free;
  end;
end;

{------------------------------------------}
procedure TAbrufCmdExec.ReadProgramIni_DSfG;
{------------------------------------------}
{ DSfG-relevante Programm-Ini-Einstellungen lesen }
var
  AI: TAbrufSrvIni;
begin
  AI:=TAbrufSrvIni.Create;
  try
    with DSfGTimeouts do begin                      { Timeouts f�r DSfG-Abruf }
      ModemAntwort:=AI.TOModemAntwort;
      ModemInit:=AI.TOModemInit;
      GSMModem:=AI.TOGSMModem;
      Verbindungsaufbau:=AI.DSfG_TOVerbindungsaufbau;
      Verbindungsabbau:=AI.DSfG_TOVerbindungsabbau;
      RufAnnahme:=AI.DSfG_TORufAnnahme;
      Login:=AI.DSfG_TOLogin;
      DFUETransparent:=AI.DSfG_TODFUETransparent;
      Archive:=AI.DSfG_TOArchive;
      Logbuecher:=AI.DSfG_TOLogbuecher;
      Datenelemente:=AI.DSfG_TODatenelemente;
      Einstellen:=AI.DSfG_TOEinstellen;
      DFUEParameter:=AI.DSfG_TODFUEParameter;
      Binaerdatei:=AI.DSfG_TOBinaerdatei;
    end;  { with }

    DSfG_BCCVersuche:=AI.DSfG_BCCVersuche;   { Fehler-Versuche f�r DSfG-Abruf }

    { Zeichen-Konvertierung ASCII -> ISO 646 ein/aus: }
    ISO646_Cfg:=AI.DSfG_ISO646;  // 03.03.2010, WW

    { Pfad zu Firmware-Bin�rdatendateien: }
    FFirmwareBinFilePath:=AI.FwUpd_FwDir;

    { f�r Halten der Verbindung zur Station: }
    VerbindungHalten_Cfg:=AI.VerbindungHalten;

    { Kodierverfahren f�r Rohdaten in XML-Response: }
    FXMLResponseEncodeRohdaten:=AI.XMLResponseEncodeRohdaten;
  finally
    AI.Free;
  end;
end;

end.
