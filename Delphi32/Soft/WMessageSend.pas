{******************************************************************************}
{* Unit: Wieser-Benachrichtigung über TCP/IP versenden                        *}
{* 14.10.2004  WW                                                             *}
{* 24.11.2004  GD  Erweitert für Export                                       *}
{* 27.01.2005  WW  umstrukturiert, Erweiterungen für Wical-Daten              *}
{* 04.05.2005  GD  Um Absender erweitert                                      *}
{* 07.08.2006  WW  Nachricht "Archivdaten abgerufen"                          *}
{* 11.12.2006  GD  Umstellung der Datum-/Zeit-Übergabe auf Klartext           *}
{* 07.07.2010  NW  Parameter-Export                                           *}
{* 29.08.2011  NW  Message "neue Archivdaten MRG" mit Angabe Zeitpunkt        *}
{* 14.10.2011  GD  Open und Close wg. Fehler 10055 abgesichert                *}
{* 21.04.2015  WW  Log-Pfad optional übergeben                                *}
{* 03.01.2023  WW  Public-Deklaration statt Published in TMessageThread       *}
{* 09.02.2023  WW  Benachrichtigung über Stationsereignis senden              *}
{******************************************************************************}
unit WMessageSend;

interface

uses
  Windows, Forms, Classes, SysUtils, contnrs, scktcomp, WChars, WMessageReg,
  T_Zeit, LogFile, WSysCon;

const
  // Allgemeiner Aufbau der Wieser-IP-Nachrichten:
  // STX + <DatumZeit: yyyymmddhhnnss> + GS + <Absender: Uppercase(Dateiname)> +
  // GS + <Nachrichtentyp: Event, Command> + GS + <Nachrichten-Spezifikation> +
  // GS + <Datenkennung: MW, AK usw.> + GS + <spezifischer Datenteil> + ETX

  { Konstanten für Nachrichten }
  CWMsg_FormatDT = 'yyyymmddhhnnss';  { Datum/Zeit-Format in Benachrichtigung }

  { Ereignis-Benachrichtigungen }
  CWMsg_Event_NAD     = 'NAD';  { Ereignis-Nachricht: neue Daten vorhanden (Abrufdaten, Auswertedaten...) }
  CWMsg_Event_ADA     = 'ADA';  { Ereignis-Nachricht: Daten wurden abgerufen }
  CWMsg_Event_CmdDone = 'CDN';  { Ereignis-Nachricht: Kommando wurde ausgeführt }
  CWMsg_Cmd           = 'CMD';  { Kommando-Nachricht }
  CWMsg_Event_NKD     = 'NKD';  { Ereignis-Nachricht: neue Kurzzeitdaten vorhanden (GPRS-Datenempfang) }
  CWMsg_Event_STN     = 'STN';  { Ereignis-Nachricht zu einer Station (z.B. nicht erreicht) }

  { Kommandotypen }
  CWMsg_Cmd_CExpD   = 'CExpD';     { Kommando Exportdaten-Erzeugung }
  CWMsg_Cmd_CExpDel = 'CExpDel';   { Kommando Exportdaten-Erzeugung abbrechen }
  CWMsg_Cmd_CWiCalD = 'CWiCalD';   { Kommando Auswertedaten-Erzeugung }

  { Info für Ereignis-Nachricht "neue Daten vorhanden": }
  { Datenarchive }
  CWMsg_MRG   = 'MRG';    { MRG-Abrufdaten }
  CWMsg_DSfG  = 'DSfG';   { DSfG-Abrufdaten }
  CWMsg_WiCal = 'WiCal';  { Auswertedaten }

  { Datentypen }
  CWMsg_MW = 'MW';  { MRG-Messwerte und Tagessäetze }
  CWMsg_ME = 'ME';  { MRG-Meldungen }
  CWMsg_AK = 'AK';  { DSfG-Archivkanaldaten }
  CWMsg_LB = 'LB';  { DSfG-Logbuchdaten }

  { Info für Kommando-Nachricht "Auswertedaten-Erzeugung" }
  CWMsg_WiCal_Gruppe = 'G';  { Auswerte-Gruppe }
  CWMsg_WiCal_Klasse = 'C';  { Auswerte-Klasse }
  CWMsg_WiCal_Mst    = 'M';  { Auswerte-Messstelle }
  CWMsg_WiCal_Kanal  = 'K';  { Auswerte-Kanal }

  { Info für Kommando-Nachricht "Exportdaten-Erzeugung" }
  CWMsg_ExportData_Std     = 'S'; // Exportdaten: MRG-Stundenwerte (1. Stelle)
  CWMsg_ExportData_Tag     = 'T'; // Exportdaten: MRG-Tageswerte        "
  CWMsg_ExportData_Meldung = 'M'; // Exportdaten: MRG-Meldungen         "
  CWMsg_ExportData_DArchiv = 'A'; // Exportdaten: DSfG-Archivdaten      "
  CWMsg_ExportData_DLogb   = 'L'; // Exportdaten: DSfG-Logbuchdaten     "
  CWMsg_ExportData_WICAL   = 'W'; // Exportdaten: Wical-Archivdaten     "
  CWMsg_ExportData_Params  = 'P'; // Exportdaten: Parameter             "  // 07.07.2010  WN
  CWMsg_ExportData_Zeit    = '1'; // Exportdaten: Export nach Zeit (2. Stelle)
  CWMsg_ExportData_ZeitInt = 1;   // Exportdaten: Export nach Zeit      "
  CWMsg_ExportData_RefNr   = '3'; // Exportdaten: Export nach Referenznummer "
  CWMsg_ExportData_RefNrInt = 3;  // Exportdaten: Export nach Referenznummer "

  { Kommunikations-Arten bei Datenabruf }
  CWMsg_CommType_Abruf      = 'A';  { Abruf (aktive Anforderung ) }
  CWMsg_CommType_SMS        = 'S';  { Datenempfang per SMS (spontan, passiv) }
  CWMsg_CommType_GPRS       = 'G';  { Datenempfang per GPRS (spontan, passiv) }
  CWMsg_CommType_eMail      = 'M';  { Datenempfang per e-Mail (spontan, passiv) }
  CWMsg_CommType_FileImport = 'F';  { Datei-Import (spontan, passiv) }
  CWMsg_CommType_MDE        = 'D';  { Import von MDE-Daten (spontan, passiv) }
  CWMsg_CommType_IEC        = 'I';  { Daten-Import per IEC-Kopplung (spontan, passiv) }
  CWMsg_CommType_OPC        = 'O';  { Daten-Import per OPC-DB-Interface (spontan, passiv) }


type
  TWMsg_ExportType = (etAscii, etDatabase);  { Export-Ziel }

  { Objekt mit Informationen zur Nachricht 'neue MRG-Messwerte/Tagessätze', 'neue
    MRG-Meldungen' }
  TWMsgDataObj_MRG = class (TObject)
    MrgId: integer;
  public
    procedure SetData (AMrgId: integer);
  end;

  { Objekt mit Informationen zur Nachricht 'neue DSfG-Archivkanaldaten' }
  TWMsgDataObj_DSfG_AK = class (TObject)
    StationId: integer;
    InstanzId: integer;
    Archivgruppe: integer;
    Archivkanal: integer;
  public
    procedure SetData (AStationId, AInstanzId, AArchivgruppe, AArchivkanal: integer);
  end;

  { Objekt mit Informationen zur Nachricht 'neue DSfG-Logbuchdaten' }
  TWMsgDataObj_DSfG_LB = class (TObject)
    StationId: integer;
    InstanzId: integer;
    LogbuchNr: integer;
  public
    procedure SetData (AStationId, AInstanzId, ALogbuchNr: integer);
  end;

  { Objekt mit Informationen zur Nachricht 'MRG-Messwerte/Tagessätze abgerufen',
    'MRG-Meldungen abgerufen' }
  TWMsgDataObj_MRG_ADA = class (TWMsgDataObj_MRG)
    Fehlergruppe: integer;
    Fehlercode: integer;
    CommType: string;
  public
    procedure SetData (AMrgId, AFehlergruppe, AFehlercode: integer;
                       ACommType: string);
  end;

  { Objekt mit Informationen zur Nachricht 'DSfG-Archivkanaldaten abgerufen' }
  TWMsgDataObj_DSfG_AK_ADA = class (TWMsgDataObj_DSfG_AK)
    Fehlergruppe: integer;
    Fehlercode: integer;
    CommType: string;
  public
    procedure SetData (AStationId, AInstanzId, AArchivgruppe, AArchivkanal,
                       AFehlergruppe, AFehlercode: integer; ACommType: string);
  end;

  { Objekt mit Informationen zur Nachricht 'DSfG-Logbuchdaten abgerufen' }
  TWMsgDataObj_DSfG_LB_ADA = class (TWMsgDataObj_DSfG_LB)
    Fehlergruppe: integer;
    Fehlercode: integer;
    CommType: string;
  public
    procedure SetData (AStationId, AInstanzId, ALogbuchNr,
                       AFehlergruppe, AFehlercode: integer; ACommType: string);
  end;

  { Objekt mit Informationen zur Nachricht 'neue WiCal-Daten' }
  TWMsgDataObj_NewWiCal = class (TObject)
    KlasseId: integer;
    MstId: integer;
    KanalId: integer;
    Kanaltyp: string;
    Kanalinfo: string;
  public
    procedure SetData (AKlasseId, AMstId, AKanalId: integer; AKanaltyp, AKanalinfo: string);
  end;

  { Objekt mit Informationen zur Nachricht 'neue Export-Daten' }
  TWMsgDataObj_NewExport = class (TObject)
    ExportTyp     : TWMsg_ExportType;
    DatenAbrufTyp : string;
    AutoDaten     : boolean;
    GeraeteArt    : string;
    GeraeteId     : integer;
    DatenSubId    : integer;
    Kanaele       : string;
    DateiPrefix   : string;
    DatenVon      : double;
    DatenBis      : double;
    AusgabePfad   : TFileName;
    DefDatei      : TFileName;
    ExcelAusgabe  : boolean;
    CancelTask    : boolean;
    Debug         : boolean;
    constructor SetData (iExportTyp: TWMsg_ExportType; sDatenAbrufTyp,
      sGeraeteArt, sKanaele, sDateiPrefix: string; sAusgabePfad,
      sDefDatei: TFileName; bAutoDaten, bExcelAusgabe, bCancelTask,
      bDebug: boolean; iGeraeteId, iDatenSubId: integer;
      fDatenVon, fDatenBis: double);
  end;

  { Objekt mit Informationen zur Nachricht 'Erzeuge WiCal-Daten' }
  TWMsgDataObj_CreateWiCal = class (TObject)
    Kennzeichen: string;
    Nr: integer;
  public
    procedure SetData (AKennzeichen: string; ANr: integer);
  end;

  { Liste zur Aufnahme von TWMsgDataObj_MRG-Objekten }
  TWMsgDataList_MRG = class(TObjectList)
    constructor Create;
    procedure Add_WMsgData (AMrgId: integer);
  end;

  { Liste zur Aufnahme von TWMsgDataObj_DSfG_AK-Objekten }
  TWMsgDataList_DSfG_AK = class(TObjectList)
    constructor Create;
    procedure Add_WMsgData (AStationId, AInstanzId, AArchivgruppe, AArchivkanal: integer);
  end;

  { Liste zur Aufnahme von TWMsgDataObj_DSfG_LB-Objekten }
  TWMsgDataList_DSfG_LB = class(TObjectList)
    constructor Create;
    procedure Add_WMsgData (AStationId, AInstanzId, ALogbuchNr: integer);
  end;

  { Liste zur Aufnahme von TWMsgDataObj_MRG_ADA-Objekten }
  TWMsgDataList_MRG_ADA = class(TObjectList)
    constructor Create;
    procedure Add_WMsgData (AMrgId, AFehlergruppe, AFehlercode: integer;
                            ACommType: string);
  end;

  { Liste zur Aufnahme von TWMsgDataObj_DSfG_AK_ADA-Objekten }
  TWMsgDataList_DSfG_AK_ADA = class(TObjectList)
    constructor Create;
    procedure Add_WMsgData (AStationId, AInstanzId, AArchivgruppe, AArchivkanal,
                            AFehlergruppe, AFehlercode: integer; ACommType: string);
  end;

  { Liste zur Aufnahme von TWMsgDataObj_DSfG_LB_ADA-Objekten }
  TWMsgDataList_DSfG_LB_ADA = class(TObjectList)
    constructor Create;
    procedure Add_WMsgData (AStationId, AInstanzId, ALogbuchNr,
                            AFehlergruppe, AFehlercode: integer; ACommType: string);
  end;

  { Liste zur Aufnahme von TWMsgDataObj_NewWiCal-Objekten }
  TWMsgDataList_NewWiCal = class(TObjectList)
    constructor Create;
    procedure Add_WMsgData (AKlasseId, AMstId, AKanalId: integer; AKanaltyp, AKanalinfo: string);
  end;

  { Liste zur Aufnahme von TWMsgDataObj_CreateWiCal-Objekten }
  TWMsgDataList_CreateWiCal = class(TObjectList)
    constructor Create;
    procedure Add_WMsgData (AKennzeichen: string; ANr: integer);
  end;


procedure SendWieserMsg (AHost, AAddress: string; APort: integer;
                         AMessage, AWSystemPath: string;
                         ADebugLog: boolean = False; ALogPath: string = '');

procedure SendWieserMsg_NeueArchivDaten_MRG_MW (RegList: TObjectList; AWSystemPath: string;
                                                AMrgId: integer; ADebugLog: boolean = false;
                                                dtNeueDatenAb: TDateTime = 0;  // 29.08.2011  WN
                                                ALogPath: string = '');
procedure SendWieserMsg_NeueArchivDaten_MRGList_MW (RegList: TObjectList; AWSystemPath: string;
                                                    WMsgDataList_MRG: TWMsgDataList_MRG;
                                                    ADebugLog: boolean = false;
                                                    ALogPath: string = '');
procedure SendWieserMsg_NeueArchivDaten_MRG_ME (RegList: TObjectList; AWSystemPath: string;
                                                AMrgId: integer; ADebugLog: boolean = false;
                                                ALogPath: string = '');
procedure SendWieserMsg_NeueArchivDaten_MRGList_ME (RegList: TObjectList; AWSystemPath: string;
                                                    WMsgDataList_MRG: TWMsgDataList_MRG;
                                                    ADebugLog: boolean = false;
                                                    ALogPath: string = '');
procedure SendWieserMsg_NeueArchivDaten_DSfG_AK (RegList: TObjectList; AWSystemPath: string;
                                                 WMsgDataList_DSfG_AK: TWMsgDataList_DSfG_AK;
                                                 ADebugLog: boolean = false;
                                                 ALogPath: string = '');
procedure SendWieserMsg_NeueArchivDaten_DSfG_LB (RegList: TObjectList; AWSystemPath: string;
                                                 WMsgDataList_DSfG_LB: TWMsgDataList_DSfG_LB;
                                                 ADebugLog: boolean = false;
                                                 ALogPath: string = '');
procedure SendWieserMsg_NeueKurzzeitDaten (RegList: TObjectList; AWSystemPath: string;
                                           ADebugLog: boolean = false; ALogPath: string = '');
procedure SendWieserMsg_ArchivDatenAbgerufen_MRG_MW (RegList: TObjectList; AWSystemPath: string;
                                                     WMsgDataList_MRG_ADA: TWMsgDataList_MRG_ADA;
                                                     ADebugLog: boolean = false;
                                                     ALogPath: string = '');
procedure SendWieserMsg_ArchivDatenAbgerufen_MRG_ME (RegList: TObjectList; AWSystemPath: string;
                                                     WMsgDataList_MRG_ADA: TWMsgDataList_MRG_ADA;
                                                     ADebugLog: boolean = false;
                                                     ALogPath: string = '');
procedure SendWieserMsg_ArchivDatenAbgerufen_DSfG_AK (RegList: TObjectList; AWSystemPath: string;
                                                      WMsgDataList_DSfG_AK_ADA: TWMsgDataList_DSfG_AK_ADA;
                                                      ADebugLog: boolean = false;
                                                      ALogPath: string = '');
procedure SendWieserMsg_ArchivDatenAbgerufen_DSfG_LB (RegList: TObjectList; AWSystemPath: string;
                                                      WMsgDataList_DSfG_LB_ADA: TWMsgDataList_DSfG_LB_ADA;
                                                      ADebugLog: boolean = false;
                                                      ALogPath: string = '');
procedure SendWieserMsg_NeueArchivDaten_WiCal (RegList: TObjectList; AWSystemPath: string;
                                               WMsgDataList_NewWiCal: TWMsgDataList_NewWiCal;
                                               ADebugLog: boolean = false;
                                               ALogPath: string = '');
procedure SendWieserMsg_ErzeugeWiCalData (RegList: TObjectList; AWSystemPath: string;
                                          WMsgDataList_CreateWiCal: TWMsgDataList_CreateWiCal;
                                          ADebugLog: boolean = false;
                                          ALogPath: string = '');
procedure SendWieserMsg_Custom (AWSystemPath: string;
                                AWMsgType: TWMsgType; sMessages: string;
                                ADebugLog: boolean = False;
                                ALogPath: string = '');

procedure SendWieserMsg_ExportAbrufData(sWSystemPath: TFileName;
  pExportType: TWMsg_ExportType; sExportData: string; bAuto: boolean;
  sGerArt: string; iGerId, iSubId: integer; sKanaele, sExportPrefix: string;
  dtVon, dtBis: TDateTime; sFilePath: TFilename = ''; sDefFile: TFilename = '';
  bExcel: boolean = False; bCancelAbruf: boolean = False;
  ADebugLog: boolean = false;
  ALogPath: string = ''); overload;

procedure SendWieserMsg_ExportAbrufData(
  sWSystemPath: TFileName; pDataList: TObjectList); overload;

procedure SendWieserMsg_StationEvent(sWSystemPath: TFileName;
  sGeraeteArt: string; iStationId, iEventNr: integer;
  ADebugLog: boolean = false; ALogPath: string = '');
  
implementation

type
  { Klasse zum Versenden von Nachrichten über TCP/IP }

  TClientSocketMessage = class(TClientSocket)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create; reintroduce;
    function Connect (AHost: string; AAddress: string; APort: integer;
                      var ErrMsg: string): boolean;
    function Disconnect: boolean;
    function SendMessage (AMessage: string; var BytesSent: integer;
                          var ErrMsg: string): boolean;
  end;

  { Thread zum Versenden einer Nachricht }

  TMessageThread = class(TThread)
  private
    Host: string;
    Address: string;
    Port: integer;
    sMessage: string;
    WSystemPath: string;
    DebugLog: boolean;
    FLogPath: string;
  protected
    procedure SendMessage; virtual;
    procedure Execute; override;
  public
    constructor CreateIt (AHost, AAddress: string; APort: integer;
                          AMessage, AWSystemPath: string;
                          ADebugLog: boolean; ALogPath: string);
  end;


{ TClientSocketMessage }

{--------------------------------------}
constructor TClientSocketMessage.Create;
{--------------------------------------}
begin
  inherited Create(nil);
  ClientType:=ctBlocking;  // synchrone Socket-Verbindung
end;

{-------------------------------------------------------------------------------------}
function TClientSocketMessage.Connect (AHost: string; AAddress: string; APort: integer;
                                       var ErrMsg: string): boolean;
{-------------------------------------------------------------------------------------}
{ Socketverbindung öffnen;
  Übergabe: Server-Hostname
            Server-Adresse (wird nur verwendet, wenn Server-Hostname leer)
            Server-Port
  Rückgabe: Exception-Message
  Ergebnis: true, wenn Socketverbindung geöffnet werden konnte }
begin
  Host:=AHost;
  Address:=AAddress;
  Port:=APort;
  try
    Open;
    ErrMsg:='';
    Result:=true;    { Socket-Verbindung ist jetzt geöffnet }
  except
    on E:Exception do begin
      ErrMsg:=E.Message;  // mit Rückgabe der Exception-Message; 30.07.2007 WW
      Result:=false;
      try       // 14.10.2011
        Active := False;
        Socket.Close;
        Close;
      except
      end;
    end;
  end;
end;

{---------------------------------------}
function TClientSocketMessage.Disconnect;
{---------------------------------------}
{ Socketverbindung schließen;
  Ergebnis: true, wenn Socketverbindung geschlossen werden konnte }
begin
  if not Active then begin         { Socketverbindung ist bereits geschlossen }
    Result:=true;
    exit;
  end;

  try
    Close;
    Result:=true;        { Socket-Verbindung ist jetzt geschlossen }
  except
    Result:=false;
  end;
end;

{----------------------------------------------------------------------}
function TClientSocketMessage.SendMessage (AMessage: string;
                                           var BytesSent: integer;
                                           var ErrMsg: string): boolean;
{----------------------------------------------------------------------}
{ Nachricht versenden (Rückantwort wird nicht erwartet);
  Übergabe: Nachricht-String
  Rückgabe: Anzahl der tatsächlich versendeten Zeichen
            Exception-Message
  Ergebnis: true, wenn Nachricht korrekt versendet wurde }
var
  BytesToSend: integer;

begin
  Result:=true;
  BytesSent:=0;  // Vorbelegung Rückgabe

  try
    ErrMsg:='';
    BytesToSend:=length (AMessage);
    if BytesToSend > 0 then begin
      BytesSent:=Socket.SendText (AMessage);             { Nachricht versenden }
      if BytesSent <> BytesToSend then begin
        ErrMsg:='Message nicht oder unvollständig versendet';  // mit Rückgabe der Message; 30.07.2007 WW
        Result:=false;
      end;
    end;
  except
    on E:Exception do begin
      ErrMsg:=E.Message;  // mit Rückgabe der Exception-Message; 30.07.2007 WW
      Result:=false;
    end;
  end;
end;


{ TMessageThread }

{ -> evtl. ist noch ein Mechanismus nötig, der vor dem Beenden des Sende-Programms
     solange wartet bis alle Nachrichten-Threads fertig sind (Resume, Terminate,
     WaitFor, siehe z.B. CloseAbrufControls des Abrufdienstes) !! }

{---------------------------------------------------------------------------}
constructor TMessageThread.CreateIt (AHost, AAddress: string; APort: integer;
                                     AMessage, AWSystemPath: string;
                                     ADebugLog: boolean; ALogPath: string);
{---------------------------------------------------------------------------}
{ Übergabe: Hostname
            Adresse
            Port-Id
            Nachrichten-String
            Pfad zur Nachrichten-Registrierungsdatei
              (-> Leerstring = Es erfolgt keine automatische Abmeldung aus der
              Nachrichten-Registrierungsdatei, falls Empfänger nicht erreichbar ist)
            Flag 'Log ein/aus'
            Log-Pfad }

begin
  inherited Create(true); // Thread createn und gleich wieder anhalten
  FreeOnTerminate:=true;  // Thread soll sich beim Beenden selbst freigeben
  Priority:=tpNormal;     // Thread soll mit normaler Priorität ausgeführt werden

  Host:=AHost;
  Address:=AAddress;
  Port:=APort;
  sMessage:=AMessage;
  WSystemPath:=AWSystemPath;
  DebugLog:=ADebugLog;
  FLogPath:=ALogPath;  // 21.04.2015, WW
  if FLogPath = '' then
    FLogPath:=ExtractFilePath (ParamStr(0));

  Suspended:=false;       // Thread jetzt fortsetzen
end;

{-----------------------------------}
procedure TMessageThread.SendMessage;
{-----------------------------------}
{ Messaging }
const
  CMaxVersuche = 10;  { maximale Anzahl an Versuchen, den Empfänger zu erreichen }
var
  ClientSocketMessage: TClientSocketMessage;
  Connected: boolean;
  WMessageReg: TWMessageReg;
  i: integer;
  BytesSent: integer;
  DebugLogFilename: string;
  ErrMsg: string;
  OK: boolean;

begin
  DebugLogFilename:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_WMsgSend';
  try
    ClientSocketMessage:=TClientSocketMessage.Create;
    try
      Connected:=false;
      i:=1;
      while (not Connected) AND (i <= CMaxVersuche) do begin
        Connected:=ClientSocketMessage.Connect (Host, Address, Port, ErrMsg);
        if Connected then begin
          if not ClientSocketMessage.SendMessage (sMessage, BytesSent, ErrMsg) then begin
            if DebugLog then
              WriteDebugLog (FLogPath, DebugLogFilename, 'Fehler ''SendMessage''' +
                             '  Host: ' + Host + '  Port: ' + IntToStr (Port) +
                             '  Bytes soll: ' + IntToStr (length (sMessage)) +
                             '  Bytes gesendet: ' + IntToStr (BytesSent) +
                             '  Message: >' + sMessage + '<' +
                             '  ' + ErrMsg, true, lt_Error);   // Error-Log
          end;
        end
        else begin
          if DebugLog then
            WriteDebugLog (FLogPath, DebugLogFilename, 'Fehler ''Connect''' +
                           '  Host: ' + Host + '  Port: ' + IntToStr (Port) + '  ' +
                           ErrMsg);   // Error-Log
        end;
        ClientSocketMessage.Disconnect;

        if not Connected then begin              { Empfänger nicht erreichbar }
          if i >= CMaxVersuche then begin  { Empfänger nach max. Versuchen nicht erreicht }
            if length (WSystemPath) > 0 then begin
              { aus Nachrichten-Registrierungsdatei löschen: }
              WMessageReg:=TWMessageReg.Create (WSystemPath);
              try
                // Eintrag als deaktiviert markieren (nicht rauslöschen); 07.09.2006 WW
                OK:=WMessageReg.DeactivateMessaging (Host, Port);
                if DebugLog then begin
                  if OK then  // Deaktivierung war erfolgreich
                    WriteDebugLog (FLogPath, DebugLogFilename, 'DeactivateMessaging' +
                                   '  Host: ' + Host + '  Port: ' + IntToStr (Port))   // Error-Log; 30.07.2007 WW
                  else
                    WriteDebugLog (FLogPath, DebugLogFilename, 'Fehler ''DeactivateMessaging''' +
                                   '  Host: ' + Host + '  Port: ' + IntToStr (Port), true, lt_Error);   // Error-Log
                end;
              finally
                WMessageReg.Free;
              end;
            end
            else begin
              if DebugLog then
                WriteDebugLog (FLogPath, DebugLogFilename, 'WSystemPath ist nicht zugewiesen',
                               true, lt_Error);   // Error-Log; 03.07.2009 WW
            end;
          end else
            Sleep (2000);  // kurz warten vor dem nächsten Versuch
        end;
        inc(i);
      end;  { while }
    finally
      ClientSocketMessage.Free;
    end;
  except
    on E: Exception do begin
      if DebugLog then
        WriteDebugLog (FLogPath, DebugLogFilename, 'Fehler ''Except''' +
                       '  Host: ' + Host + '  Port: ' + IntToStr (Port) +
                       '  Message: >' + sMessage + '<', true, lt_Error);   // Error-Log
    end;
  end;
end;


{-------------------------------}
procedure TMessageThread.Execute;
{-------------------------------}
{ Thread-Ausführung }
begin
  // Inhalt in "SendMessage" ausgelagert

  SendMessage;
end;

{-------------- Listenobjekte für Nachrichteninformationen --------------------}

{ TWMsgDataObj_MRG }

{---------------------------------------------------}
procedure TWMsgDataObj_MRG.SetData (AMrgId: integer);
{---------------------------------------------------}
begin
  MrgId:=AMrgId;
end;

{ TWMsgDataList_MRG }

{-----------------------------------}
constructor TWMsgDataList_MRG.Create;
{-----------------------------------}
begin
  inherited Create (true);
end;

{---------------------------------------------------------}
procedure TWMsgDataList_MRG.Add_WMsgData (AMrgId: integer);
{---------------------------------------------------------}
{ MRG-Listenobjekt createn und in Liste einfügen }
var
  ListObj: TWMsgDataObj_MRG;
begin
  ListObj:=TWMsgDataObj_MRG.Create;
  ListObj.SetData (AMrgId);
  Add (ListObj);
end;

{ TWMsgDataObj_DSfG_AK }

{----------------------------------------------------------------------------}
procedure TWMsgDataObj_DSfG_AK.SetData (AStationId, AInstanzId, AArchivgruppe,
                                        AArchivkanal: integer);
{----------------------------------------------------------------------------}
begin
  StationId:=AStationId;
  InstanzId:=AInstanzId;
  Archivgruppe:=AArchivgruppe;
  Archivkanal:=AArchivkanal;
end;

{ TWMsgDataList_DSfG_AK }

{---------------------------------------}
constructor TWMsgDataList_DSfG_AK.Create;
{---------------------------------------}
begin
  inherited Create (true);
end;

{----------------------------------------------------------------------------------}
procedure TWMsgDataList_DSfG_AK.Add_WMsgData (AStationId, AInstanzId, AArchivgruppe,
                                              AArchivkanal: integer);
{----------------------------------------------------------------------------------}
{ Archivkanal-Listenobjekt createn und in Liste einfügen }
var
  ListObj: TWMsgDataObj_DSfG_AK;
begin
  ListObj:=TWMsgDataObj_DSfG_AK.Create;
  ListObj.SetData (AStationId, AInstanzId, AArchivgruppe, AArchivkanal);
  Add (ListObj);
end;

{ TWMsgDataObj_DSfG_LB }

{-----------------------------------------------------------------------------------}
procedure TWMsgDataObj_DSfG_LB.SetData (AStationId, AInstanzId, ALogbuchNr: integer);
{-----------------------------------------------------------------------------------}
begin
  StationId:=AStationId;
  InstanzId:=AInstanzId;
  LogbuchNr:=ALogbuchNr;
end;

{ TWMsgDataList_DSfG_LB }

{---------------------------------------}
constructor TWMsgDataList_DSfG_LB.Create;
{---------------------------------------}
begin
  inherited Create (true);
end;

{-----------------------------------------------------------------------------------------}
procedure TWMsgDataList_DSfG_LB.Add_WMsgData (AStationId, AInstanzId, ALogbuchNr: integer);
{-----------------------------------------------------------------------------------------}
{ Logbuch-Listenobjekt createn und in Liste einfügen }
var
  ListObj: TWMsgDataObj_DSfG_LB;
begin
  ListObj:=TWMsgDataObj_DSfG_LB.Create;
  ListObj.SetData (AStationId, AInstanzId, ALogbuchNr);
  Add (ListObj);
end;

{ TWMsgDataObj_MRG_ADA }

{----------------------------------------------------------------------------------}
procedure TWMsgDataObj_MRG_ADA.SetData (AMrgId, AFehlergruppe, AFehlercode: integer;
                                        ACommType: string);
{----------------------------------------------------------------------------------}
begin
  MrgId:=AMrgId;
  Fehlergruppe:=AFehlergruppe;
  Fehlercode:=AFehlercode;
  CommType:=ACommType;
end;

{ TWMsgDataObj_DSfG_AK_ADA }

{--------------------------------------------------------------------------------------------}
procedure TWMsgDataObj_DSfG_AK_ADA.SetData (AStationId, AInstanzId, AArchivgruppe,
                                            AArchivkanal, AFehlergruppe, AFehlercode: integer;
                                            ACommType: string);
{--------------------------------------------------------------------------------------------}
begin
  StationId:=AStationId;
  InstanzId:=AInstanzId;
  Archivgruppe:=AArchivgruppe;
  Archivkanal:=AArchivkanal;
  Fehlergruppe:=AFehlergruppe;
  Fehlercode:=AFehlercode;
  CommType:=ACommType;
end;

{ TWMsgDataList_MRG_ADA }

{---------------------------------------}
constructor TWMsgDataList_MRG_ADA.Create;
{---------------------------------------}
begin
  inherited Create (true);
end;

{----------------------------------------------------------------------------------------}
procedure TWMsgDataList_MRG_ADA.Add_WMsgData (AMrgId, AFehlergruppe, AFehlercode: integer;
                                              ACommType: string);
{----------------------------------------------------------------------------------------}
{ MRG-Listenobjekt createn und in Liste einfügen }
var
  ListObj: TWMsgDataObj_MRG_ADA;
begin
  ListObj:=TWMsgDataObj_MRG_ADA.Create;
  ListObj.SetData (AMrgId, AFehlergruppe, AFehlercode, ACommType);
  Add (ListObj);
end;

{ TWMsgDataList_DSfG_AK_ADA }

{-------------------------------------------}
constructor TWMsgDataList_DSfG_AK_ADA.Create;
{-------------------------------------------}
begin
  inherited Create (true);
end;

{--------------------------------------------------------------------------------------------------}
procedure TWMsgDataList_DSfG_AK_ADA.Add_WMsgData (AStationId, AInstanzId, AArchivgruppe,
                                                  AArchivkanal, AFehlergruppe, AFehlercode: integer;
                                                  ACommType: string);
{--------------------------------------------------------------------------------------------------}
{ Archivkanal-Listenobjekt createn und in Liste einfügen }
var
  ListObj: TWMsgDataObj_DSfG_AK_ADA;
begin
  ListObj:=TWMsgDataObj_DSfG_AK_ADA.Create;
  ListObj.SetData (AStationId, AInstanzId, AArchivgruppe, AArchivkanal,
                   AFehlergruppe, AFehlercode, ACommType);
  Add (ListObj);
end;

{ TWMsgDataObj_DSfG_LB_ADA }

{------------------------------------------------------------------------------}
procedure TWMsgDataObj_DSfG_LB_ADA.SetData (AStationId, AInstanzId, ALogbuchNr,
                                            AFehlergruppe, AFehlercode: integer;
                                            ACommType: string);
{------------------------------------------------------------------------------}
begin
  StationId:=AStationId;
  InstanzId:=AInstanzId;
  LogbuchNr:=ALogbuchNr;
  Fehlergruppe:=AFehlergruppe;
  Fehlercode:=AFehlercode;
  CommType:=ACommType;
end;

{ TWMsgDataList_DSfG_LB_ADA }

{-------------------------------------------}
constructor TWMsgDataList_DSfG_LB_ADA.Create;
{-------------------------------------------}
begin
  inherited Create (true);
end;

{------------------------------------------------------------------------------------}
procedure TWMsgDataList_DSfG_LB_ADA.Add_WMsgData (AStationId, AInstanzId, ALogbuchNr,
                                                  AFehlergruppe, AFehlercode: integer;
                                                  ACommType: string);
{------------------------------------------------------------------------------------}
{ Logbuch-Listenobjekt createn und in Liste einfügen }
var
  ListObj: TWMsgDataObj_DSfG_LB_ADA;
begin
  ListObj:=TWMsgDataObj_DSfG_LB_ADA.Create;
  ListObj.SetData (AStationId, AInstanzId, ALogbuchNr, AFehlergruppe, AFehlercode,
                   ACommType);
  Add (ListObj);
end;

{ TWMsgDataObj_NewExport }

{-----------------------------------------------------}
constructor TWMsgDataObj_NewExport.SetData (iExportTyp: TWMsg_ExportType;
  sDatenAbrufTyp, sGeraeteArt, sKanaele, sDateiPrefix: string; sAusgabePfad,
  sDefDatei: TFileName; bAutoDaten, bExcelAusgabe, bCancelTask, bDebug: boolean;
  iGeraeteId, iDatenSubId: integer; fDatenVon, fDatenBis: double);
{-----------------------------------------------------}
begin
  inherited Create;

  ExportTyp     := iExportTyp;
  DatenAbrufTyp := sDatenAbrufTyp;
  AutoDaten     := bAutoDaten;
  GeraeteArt    := sGeraeteArt;
  GeraeteId     := iGeraeteId;
  DatenSubId    := iDatenSubId;
  Kanaele       := sKanaele;
  DateiPrefix   := sDateiPrefix;
  DatenVon      := fDatenVon;
  DatenBis      := fDatenBis;
  AusgabePfad   := sAusgabePfad;
  DefDatei      := sDefDatei;
  ExcelAusgabe  := bExcelAusgabe;
  CancelTask    := bCancelTask;
  Debug         := bDebug;
end;

{ TWMsgDataObj_NewWiCal }

{----------------------------------------------------------------------------}
procedure TWMsgDataObj_NewWiCal.SetData (AKlasseId, AMstId, AKanalId: integer;
                                         AKanaltyp, AKanalinfo: string);
{----------------------------------------------------------------------------}
begin
  KlasseId:=AKlasseId;
  MstId:=AMstId;
  KanalId:=AKanalId;
  Kanaltyp:=AKanaltyp;
  Kanalinfo:=AKanalinfo;
end;

{ TWMsgDataList_NewWiCal }

{----------------------------------------}
constructor TWMsgDataList_NewWiCal.Create;
{----------------------------------------}
begin
  inherited Create (true);
end;

{----------------------------------------------------------------------------------}
procedure TWMsgDataList_NewWiCal.Add_WMsgData (AKlasseId, AMstId, AKanalId: integer;
                                               AKanaltyp, AKanalinfo: string);
{----------------------------------------------------------------------------------}
{ NewWiCal-Listenobjekt createn und in Liste einfügen }
var
  ListObj: TWMsgDataObj_NewWiCal;
begin
  ListObj:=TWMsgDataObj_NewWiCal.Create;
  ListObj.SetData (AKlasseId, AMstId, AKanalId, AKanaltyp, AKanalinfo);
  Add (ListObj);
end;

{ TWMsgDataObj_CreateWiCal }

{------------------------------------------------------------------------------}
procedure TWMsgDataObj_CreateWiCal.SetData (AKennzeichen: string; ANr: integer);
{------------------------------------------------------------------------------}
begin
  Kennzeichen:=AKennzeichen;
  Nr:=ANr;
end;

{ TWMsgDataList_CreateWiCal }

{-------------------------------------------}
constructor TWMsgDataList_CreateWiCal.Create;
{-------------------------------------------}
begin
  inherited Create (true);
end;

{------------------------------------------------------------------------------------}
procedure TWMsgDataList_CreateWiCal.Add_WMsgData (AKennzeichen: string; ANr: integer);
{------------------------------------------------------------------------------------}
{ CreateWiCal-Listenobjekt createn und in Liste einfügen }
var
  ListObj: TWMsgDataObj_CreateWiCal;
begin
  ListObj:=TWMsgDataObj_CreateWiCal.Create;
  ListObj.SetData (AKennzeichen, ANr);
  Add (ListObj);
end;


{--------------- Routinen zum Versenden von Benachrichtigungen ----------------}

{---------------------------------------------------------------}
procedure SendWieserMsg (AHost, AAddress: string; APort: integer;
                         AMessage, AWSystemPath: string;
                         ADebugLog: boolean = false;
                         ALogPath: string = '');
{---------------------------------------------------------------}
{ universelle Benachrichtigungs-Routine;
  Übergabe: Hostname des Empfängers
            Adresse des Empfängers
            Port des Empfängers
            Benachrichtigungs-String
            Pfad zur Nachrichten-Registrierungsdatei
              (-> Leerstring = Es erfolgt keine automatische Abmeldung aus der
              Nachrichten-Registrierungsdatei, falls Empfänger nicht erreichbar ist)
            Flag 'Log ein/aus'
            Log-Pfad }
begin
  with TMessageThread.CreateIt (AHost, AAddress, APort, AMessage, AWSystemPath,
                                ADebugLog, ALogPath) do;
end;


{ Senderoutinen für Ereignis-Benachrichtigungen }

{-------------------------------------------------------------------------------------------}
procedure SendWieserMsg_NeueArchivDaten_MRG_MW (RegList: TObjectList; AWSystemPath: string;
                                                AMrgId: integer; ADebugLog: boolean = false;
                                                dtNeueDatenAb: TDateTime = 0;  // 29.08.2011  WN
                                                ALogPath: string = '');
{-------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "neue MRG-Archivdaten, Messwerte";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            MRG-StationId
            Debug-Log ein/aus
            Zeitpunkt neue Daten ab }
var
  Msg: string;
  i: integer;

begin
  Msg:=STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
       UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
       CWMsg_Event_NAD + GS + CWMsg_MRG + GS + CWMsg_MW + GS + IntToStr (AMrgId);
  // 29.08.2011  WN
  if (dtNeueDatenAb > 0) then
    Msg := Msg + GS + FormatDateTime(CWMsg_FormatDT, dtNeueDatenAb);

  Msg := Msg + ETX;
  for i:=0 to RegList.Count - 1 do
    SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                   TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                   ADebugLog, ALogPath);
end;

{---------------------------------------------------------------------------------------------}
procedure SendWieserMsg_NeueArchivDaten_MRGList_MW (RegList: TObjectList; AWSystemPath: string;
                                                    WMsgDataList_MRG: TWMsgDataList_MRG;
                                                    ADebugLog: boolean = false;
                                                    ALogPath: string = '');
{---------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "neue MRG-Archivdaten, Messwerte" (über Liste);
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit MRG-Informationen (MrgId) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_MRG) then exit;
  if WMsgDataList_MRG.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_MRG.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Event_NAD + GS + CWMsg_MRG + GS + CWMsg_MW + GS +
           IntToStr (TWMsgDataObj_MRG (WMsgDataList_MRG [i]).MrgId) + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{-------------------------------------------------------------------------------------------}
procedure SendWieserMsg_NeueArchivDaten_MRG_ME (RegList: TObjectList; AWSystemPath: string;
                                                AMrgId: integer; ADebugLog: boolean = false;
                                                ALogPath: string = '');
{-------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "neue MRG-Archivdaten, Meldungen";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            MRG-StationId }
var
  Msg: string;
  i: integer;

begin
  Msg:=STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
       UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
       CWMsg_Event_NAD + GS + CWMsg_MRG + GS + CWMsg_ME + GS + IntToStr (AMrgId) + ETX;
  for i:=0 to RegList.Count - 1 do
    SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                   TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                   ADebugLog, ALogPath);
end;

{---------------------------------------------------------------------------------------------}
procedure SendWieserMsg_NeueArchivDaten_MRGList_ME (RegList: TObjectList; AWSystemPath: string;
                                                    WMsgDataList_MRG: TWMsgDataList_MRG;
                                                    ADebugLog: boolean = false;
                                                    ALogPath: string = '');
{---------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "neue MRG-Archivdaten, Meldungen" (über Liste);
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit MRG-Informationen (MrgId) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_MRG) then exit;
  if WMsgDataList_MRG.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_MRG.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Event_NAD + GS + CWMsg_MRG + GS + CWMsg_ME + GS +
           IntToStr (TWMsgDataObj_MRG (WMsgDataList_MRG [i]).MrgId) + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{------------------------------------------------------------------------------------------}
procedure SendWieserMsg_NeueArchivDaten_DSfG_AK (RegList: TObjectList; AWSystemPath: string;
                                                 WMsgDataList_DSfG_AK: TWMsgDataList_DSfG_AK;
                                                 ADebugLog: boolean = false;
                                                 ALogPath: string = '');
{------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "neue DSfG-Archivdaten, Archivkanal";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit AK-Informationen (DSfG-StationId, InstanzId, Archivgruppen-Nummer,
                                        Archivkanal-Nummer) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_DSfG_AK) then exit;
  if WMsgDataList_DSfG_AK.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_DSfG_AK.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Event_NAD + GS + CWMsg_DSfG + GS + CWMsg_AK + GS +
           IntToStr (TWMsgDataObj_DSfG_AK (WMsgDataList_DSfG_AK [i]).StationId) + US +
           IntToStr (TWMsgDataObj_DSfG_AK (WMsgDataList_DSfG_AK [i]).InstanzId) + US +
           IntToStr (TWMsgDataObj_DSfG_AK (WMsgDataList_DSfG_AK [i]).Archivgruppe) + US +
           IntToStr (TWMsgDataObj_DSfG_AK (WMsgDataList_DSfG_AK [i]).Archivkanal) + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{--------------------------------------------------------------------------------------------}
procedure SendWieserMsg_NeueArchivDaten_DSfG_LB (RegList: TObjectList; AWSystemPath: string;
                                                 WMsgDataList_DSfG_LB: TWMsgDataList_DSfG_LB;
                                                 ADebugLog: boolean = false;
                                                 ALogPath: string = '');
{--------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "neue DSfG-Archivdaten, Logbuch";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit LB-Informationen (DSfG-StationId, InstanzId, Logbuch-Nummer) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_DSfG_LB) then exit;
  if WMsgDataList_DSfG_LB.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_DSfG_LB.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Event_NAD + GS + CWMsg_DSfG + GS + CWMsg_LB + GS +
           IntToStr (TWMsgDataObj_DSfG_LB (WMsgDataList_DSfG_LB [i]).StationId) + US +
           IntToStr (TWMsgDataObj_DSfG_LB (WMsgDataList_DSfG_LB [i]).InstanzId) + US +
           IntToStr (TWMsgDataObj_DSfG_LB (WMsgDataList_DSfG_LB [i]).LogbuchNr) + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{------------------------------------------------------------------------------------}
procedure SendWieserMsg_NeueKurzzeitDaten (RegList: TObjectList; AWSystemPath: string;
                                           ADebugLog: boolean = false;
                                           ALogPath: string = '');
{------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "neue Kurzzeitdaten";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei }
var
  Msg: string;
  i: integer;

begin
  Msg:=STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
       UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
       CWMsg_Event_NKD + ETX;

  for i:=0 to RegList.Count - 1 do
    SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                   TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                   ADebugLog, ALogPath);
end;

{-----------------------------------------------------------------------------------------------}
procedure SendWieserMsg_ArchivDatenAbgerufen_MRG_MW (RegList: TObjectList; AWSystemPath: string;
                                                     WMsgDataList_MRG_ADA: TWMsgDataList_MRG_ADA;
                                                     ADebugLog: boolean = false;
                                                     ALogPath: string = '');
{-----------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "MRG-Archivdaten abgerufen, Messwerte";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit MRG-Messwertabruf-Informationen (MRG-Id, Fehlergrupppe-/code) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_MRG_ADA) then exit;
  if WMsgDataList_MRG_ADA.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_MRG_ADA.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Event_ADA + GS + CWMsg_MRG + GS + CWMsg_MW + GS +
           IntToStr (TWMsgDataObj_MRG_ADA (WMsgDataList_MRG_ADA [i]).MrgId) + GS +
           IntToStr (TWMsgDataObj_MRG_ADA (WMsgDataList_MRG_ADA [i]).Fehlergruppe) + GS +
           IntToStr (TWMsgDataObj_MRG_ADA (WMsgDataList_MRG_ADA [i]).Fehlercode) + GS +
           TWMsgDataObj_MRG_ADA (WMsgDataList_MRG_ADA [i]).CommType + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{-----------------------------------------------------------------------------------------------}
procedure SendWieserMsg_ArchivDatenAbgerufen_MRG_ME (RegList: TObjectList; AWSystemPath: string;
                                                     WMsgDataList_MRG_ADA: TWMsgDataList_MRG_ADA;
                                                     ADebugLog: boolean = false;
                                                     ALogPath: string = '');
{-----------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "MRG-Archivdaten abgerufen, Meldungen";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit MRG-Meldungsabruf-Informationen (MRG-Id, Fehlergrupppe-/code) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_MRG_ADA) then exit;
  if WMsgDataList_MRG_ADA.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_MRG_ADA.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Event_ADA + GS + CWMsg_MRG + GS + CWMsg_ME + GS +
           IntToStr (TWMsgDataObj_MRG_ADA (WMsgDataList_MRG_ADA [i]).MrgId) + GS +
           IntToStr (TWMsgDataObj_MRG_ADA (WMsgDataList_MRG_ADA [i]).Fehlergruppe) + GS +
           IntToStr (TWMsgDataObj_MRG_ADA (WMsgDataList_MRG_ADA [i]).Fehlercode) + GS +
           TWMsgDataObj_MRG_ADA (WMsgDataList_MRG_ADA [i]).CommType + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{--------------------------------------------------------------------------------------------------------}
procedure SendWieserMsg_ArchivDatenAbgerufen_DSfG_AK (RegList: TObjectList; AWSystemPath: string;
                                                      WMsgDataList_DSfG_AK_ADA: TWMsgDataList_DSfG_AK_ADA;
                                                      ADebugLog: boolean = false;
                                                      ALogPath: string = '');
{--------------------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "DSfG-Archivdaten abgerufen, Archivkanal";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit AK-Informationen (DSfG-StationId, InstanzId, Archivgruppen-Nummer,
                                        Archivkanal-Nummer, Fehlergrupppe-/code) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_DSfG_AK_ADA) then exit;
  if WMsgDataList_DSfG_AK_ADA.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_DSfG_AK_ADA.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Event_ADA + GS + CWMsg_DSfG + GS + CWMsg_AK + GS +
           IntToStr (TWMsgDataObj_DSfG_AK_ADA (WMsgDataList_DSfG_AK_ADA [i]).StationId) + US +
           IntToStr (TWMsgDataObj_DSfG_AK_ADA (WMsgDataList_DSfG_AK_ADA [i]).InstanzId) + US +
           IntToStr (TWMsgDataObj_DSfG_AK_ADA (WMsgDataList_DSfG_AK_ADA [i]).Archivgruppe) + US +
           IntToStr (TWMsgDataObj_DSfG_AK_ADA (WMsgDataList_DSfG_AK_ADA [i]).Archivkanal) + US +
           IntToStr (TWMsgDataObj_DSfG_AK_ADA (WMsgDataList_DSfG_AK_ADA [i]).Fehlergruppe) + US +
           IntToStr (TWMsgDataObj_DSfG_AK_ADA (WMsgDataList_DSfG_AK_ADA [i]).Fehlercode) + US +
           TWMsgDataObj_DSfG_AK_ADA (WMsgDataList_DSfG_AK_ADA [i]).CommType + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{--------------------------------------------------------------------------------------------------------}
procedure SendWieserMsg_ArchivDatenAbgerufen_DSfG_LB (RegList: TObjectList; AWSystemPath: string;
                                                      WMsgDataList_DSfG_LB_ADA: TWMsgDataList_DSfG_LB_ADA;
                                                      ADebugLog: boolean = false;
                                                      ALogPath: string = '');
{--------------------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "DSfG-Archivdaten abgerufen, Logbuch";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit LB-Informationen (DSfG-StationId, InstanzId, Logbuch-Nummer,
                                        Fehelrgruppe-/code) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_DSfG_LB_ADA) then exit;
  if WMsgDataList_DSfG_LB_ADA.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_DSfG_LB_ADA.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Event_ADA + GS + CWMsg_DSfG + GS + CWMsg_LB + GS +
           IntToStr (TWMsgDataObj_DSfG_LB_ADA (WMsgDataList_DSfG_LB_ADA [i]).StationId) + US +
           IntToStr (TWMsgDataObj_DSfG_LB_ADA (WMsgDataList_DSfG_LB_ADA [i]).InstanzId) + US +
           IntToStr (TWMsgDataObj_DSfG_LB_ADA (WMsgDataList_DSfG_LB_ADA [i]).LogbuchNr) + US +
           IntToStr (TWMsgDataObj_DSfG_LB_ADA (WMsgDataList_DSfG_LB_ADA [i]).Fehlergruppe) + US +
           IntToStr (TWMsgDataObj_DSfG_LB_ADA (WMsgDataList_DSfG_LB_ADA [i]).Fehlercode) + US +
           TWMsgDataObj_DSfG_LB_ADA (WMsgDataList_DSfG_LB_ADA [i]).CommType + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{-------------------------------------------------------------------------------------------}
procedure SendWieserMsg_NeueArchivDaten_WiCal (RegList: TObjectList; AWSystemPath: string;
                                               WMsgDataList_NewWiCal: TWMsgDataList_NewWiCal;
                                               ADebugLog: boolean = false;
                                               ALogPath: string = '');
{-------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "neue Auswertedaten";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit Neue-WiCal-Daten-Informationen (Klassen-Id, Messstellen-Id,
                                                      Kanal-Id, Auswerte-Kanaltyp,
                                                      Auswerte-Kanalinfo) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_NewWiCal) then exit;
  if WMsgDataList_NewWiCal.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_NewWiCal.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Event_NAD + GS + CWMsg_WiCal + GS +
           IntToStr (TWMsgDataObj_NewWiCal (WMsgDataList_NewWiCal [i]).KlasseId) + US +
           IntToStr (TWMsgDataObj_NewWiCal (WMsgDataList_NewWiCal [i]).MstId) + US +
           IntToStr (TWMsgDataObj_NewWiCal (WMsgDataList_NewWiCal [i]).KanalId) + US +
           TWMsgDataObj_NewWiCal (WMsgDataList_NewWiCal [i]).Kanaltyp + US +
           TWMsgDataObj_NewWiCal (WMsgDataList_NewWiCal [i]).Kanalinfo + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{--------------------------------------------------------------------------------------------}
procedure SendWieserMsg_ErzeugeWiCalData (RegList: TObjectList; AWSystemPath: string;
                                          WMsgDataList_CreateWiCal: TWMsgDataList_CreateWiCal;
                                          ADebugLog: boolean = false;
                                          ALogPath: string = '');
{--------------------------------------------------------------------------------------------}
{ Benachrichtigungs-Routine "Erzeuge Auswertedaten";
  Übergabe: Liste mit Host und Adresse aller Empfänger
            Pfad zur Nachrichten-Registrierungsdatei
            Liste mit Erzeuge-WiCal-Daten-Informationen (Kennzeichen für Gruppe,
            Klasse, Messstelle oder Kanal; Nummer der Gruppe, Klasse, Messstelle
            oder des Kanals) }
var
  Msg: string;
  i: integer;

begin
  if not Assigned (WMsgDataList_CreateWiCal) then exit;
  if WMsgDataList_CreateWiCal.Count > 0 then begin
    Msg:='';
    for i:=0 to WMsgDataList_CreateWiCal.Count - 1 do
      Msg:=Msg +
           STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
           UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
           CWMsg_Cmd + GS + CWMsg_Cmd_CWiCalD + GS +
           TWMsgDataObj_CreateWiCal (WMsgDataList_CreateWiCal [i]).Kennzeichen + US +
           IntToStr (TWMsgDataObj_CreateWiCal (WMsgDataList_CreateWiCal [i]).Nr) + ETX;

    for i:=0 to RegList.Count - 1 do
      SendWieserMsg (TWMsgRegObj (RegList[i]).Data.Host, '',
                     TWMsgRegObj (RegList[i]).Data.Port, Msg, AWSystemPath,
                     ADebugLog, ALogPath);
  end;
end;

{----------------------------------------------------------------------}
procedure SendWieserMsg_Custom (AWSystemPath: string;
                                AWMsgType: TWMsgType; sMessages: string;
                                ADebugLog: boolean = False;
                                ALogPath: string = '');
{----------------------------------------------------------------------}
{ Standard-Benachrichtigungs-Routine (String mit einer oder mehreren Wieser-Messages
  muß extern gebildet und übergeben werden);
  Übergabe: Pfad zur Nachrichten-Registrierungsdatei
            Nachrichtentyp
            zu übertragender Nachrichten-String }
var
  pRegList   : TObjectList;
  i          : integer;

begin
  if length (sMessages) > 0 then begin
    pRegList := TObjectList.Create(True);
    try
      with TWMessageReg.Create(AWSystemPath) do
      try
        if (GetMsgRegistrationList(AWMsgType, pRegList)) then begin
          // Schleife über alle eingetragenen Empfänger
          for i := 0 to pRegList.Count-1 do
            SendWieserMsg (TWMsgRegObj (pRegList[i]).Data.Host, '',
              TWMsgRegObj(pRegList[i]).Data.Port, sMessages, AWSystemPath,
              ADebugLog, ALogPath);
        end;
      finally
        Free;
      end;
    finally
      pRegList.Free;
    end;
  end;
end;


{ Benachrichtigt den Exportserver über neue Aufträge          }
{ Parameter: Exporttyp (Ascii, Datenbank), Geräteart, -Id,    }
{            optionaler Datei-Prefix, Zeitbereich von-bis,    }
{            Ausgabepfad, Definitionsdatei, Flag Excelausgabe }
{-------------------------------------------------------------}
procedure SendWieserMsg_ExportAbrufData(sWSystemPath: TFileName;
  pExportType: TWMsg_ExportType; sExportData: string; bAuto: boolean;
  sGerArt: string; iGerId, iSubId: integer; sKanaele, sExportPrefix: string;
  dtVon, dtBis: TDateTime; sFilePath: TFilename = '';
  sDefFile: TFilename = ''; bExcel: boolean = False;
  bCancelAbruf: boolean = False; ADebugLog: boolean = False;
  ALogPath: string = '');
{-------------------------------------------------------------}
var
  pRegList   : TObjectList;
  i          : integer;
  sMsg, sCmd : string;
begin
  if (bCancelAbruf) then sCmd := CWMsg_Cmd_CExpDel else sCmd := CWMsg_Cmd_CExpD;

  pRegList := TObjectList.Create(True);
  with TWMessageReg.Create(sWSystemPath) do
  try
    if (GetMsgRegistrationList(wmt_CmdCreateExportData, pRegList)) then
    begin
      // Botschaft zusammenstellen: Daten und Envelope
      sMsg := IntToStr(Integer(bAuto)) + US + sGerArt + US + IntToStr(iGerId)
        + US + IntToStr(iSubId) + US + sKanaele + US + sExportData
        + US + sExportPrefix + US + FormatDateTime(C_FormatDateTimeWithMs, dtVon)
        + US + FormatDateTime(C_FormatDateTimeWithMs, dtBis)
        + US + sFilePath + US + sDefFile + US + IntToStr(Integer(bExcel));
      sMsg := STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
        UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
        CWMsg_Cmd + GS + sCmd + GS + IntToStr(Byte(pExportType)) + GS + sMsg +
        ETX;

      // Schleife über alle eingetragenen Exportserver
      for i := 0 to pRegList.Count-1 do
        SendWieserMsg (TWMsgRegObj (pRegList[i]).Data.Host, '',
          TWMsgRegObj(pRegList[i]).Data.Port, sMsg, sWSystemPath,
          ADebugLog, ALogPath);
    end;
  finally
    Free;
    pRegList.Free;
  end;
end;

{ Benachrichtigt den Exportserver über neue Aufträge          }
{ Parameter: Pfad zur Nachrichten-Registrierungsdatei,        }
{            Daten-Liste                                      }
{-------------------------------------------------------------}
procedure SendWieserMsg_ExportAbrufData(
  sWSystemPath: TFileName; pDataList: TObjectList);
{-------------------------------------------------------------}
var
  pRegList     : TObjectList;
  pWMessageReg : TWMessageReg;
  i            : integer;
  bDebug       : boolean;
  sMsg, sCmd, sMsgList : string;
begin

  pRegList := TObjectList.Create(True);
  pWMessageReg := TWMessageReg.Create(sWSystemPath);
  try
    if (pWMessageReg.GetMsgRegistrationList(
      wmt_CmdCreateExportData, pRegList)) then
    begin
      sMsgList := '';
      bDebug := False;

      for i := 0 to pDataList.Count-1 do
        with TWMsgDataObj_NewExport(pDataList[i]) do begin
          // Botschaft zusammenstellen: Daten und Envelope
          if (CancelTask)
          then sCmd := CWMsg_Cmd_CExpDel
          else sCmd := CWMsg_Cmd_CExpD;
          sMsg := IntToStr(Integer(AutoDaten)) + US + GeraeteArt + US +
            IntToStr(GeraeteId) + US + IntToStr(DatenSubId) + US +
            Kanaele + US + DatenAbrufTyp + US + DateiPrefix + US +
            FormatDateTime(C_FormatDateTimeWithMs, DatenVon) + US +
            FormatDateTime(C_FormatDateTimeWithMs, DatenBis) + US +
            AusgabePfad + US + DefDatei + US + IntToStr(Integer(ExcelAusgabe));
          sMsg := STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
            UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
            CWMsg_Cmd + GS + sCmd + GS + IntToStr(Byte(ExportTyp)) + GS + sMsg +
            ETX;
          bDebug := Debug;
          sMsgList := sMsgList + sMsg;
        end;

      // Schleife über alle eingetragenen Exportserver
      for i := 0 to pRegList.Count-1 do
        SendWieserMsg (TWMsgRegObj (pRegList[i]).Data.Host, '',
          TWMsgRegObj(pRegList[i]).Data.Port, sMsgList, sWSystemPath, bDebug);
    end;
  finally
    pWMessageReg.Free;
    pRegList.Free;
  end;
end;

// 15.03.2023, WW
{-----------------------------------------------------------}
procedure SendWieserMsg_StationEvent(sWSystemPath: TFileName;
  sGeraeteArt: string; iStationId, iEventNr: integer;
  ADebugLog: boolean = false; ALogPath: string = '');
{-----------------------------------------------------------}
{ Benachrichtigung über Stationsereignis
  Übergaben: Pfad zur Nachrichten-Registrierungsdatei
             Geräteart
             Station-Id
             Ereignis-Nummer }
var
  sMsg: string;

begin
  sMsg:=STX + FormatDateTime (CWMsg_FormatDT, Now) + GS +
        UpperCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) + GS +
        CWMsg_Event_STN + GS +
        sGeraeteArt + US +
        IntToStr (iStationId) + US +
        IntToStr (iEventNr) + ETX;

  SendWieserMsg_Custom (sWSystemPath, wmt_StationEvent, sMsg, ADebugLog, ALogPath);
end;

end.

