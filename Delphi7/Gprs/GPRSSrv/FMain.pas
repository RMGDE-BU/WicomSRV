{******************************************************************************}
{* GPRS-Server                                                                *}
{* Version 1.0: 01.03.2006  WW  für MRG 910 (Messe-Version)                   *}
{*         2.0: 16.11.2006  WW  für MRG 900 (neue SMS-Formate)                *}
{*           - Empfang von Datentelegrammen Typ v, p, r; kein aktives Senden  *}
{*             von Anforderungstelegrammen an Client-Geräte                   *}
{*           - Abspeichern von Messperioden- und Meldungs-Telegrammenen (v, r)*}
{*             in DSfG-Archivdatenbank incl. Journaleintrag                   *}
{*           - Abspeichern von Kurzzeitwerten (p) als ASCII-File für IEC-Leit-*}
{*             wartenkopplung (ohne Journaleintrag)                           *}
{*         2.01: 31.01.2007 WW  Löschen der Zwischendateien aus Datenkonver-  *}
{*            tierung überarbeitet; zusätzliches Logfile GPRSSrv_KonvData.log *}
{*            mit lesbaren Gerätedaten                                        *}
{*         2.02: 02.03.2007 WW  DB-Singlethread; Telegrammnummer sofort in INI*}
{*            speichern, zusätzliche Debugfiles (Testversion)                 *}
{*         2.03: 29.03.2007 WW  zusätzliche Debugfiles wieder raus (Ausliefer-*}
{*            version)                                                        *}
{*         2.04: 13.06.2007 WW  Daten-Logfile Excel-auswertbar                *}
{*         2.1:  26.07.2007 WW  mit Telegrammtyp q (Kurzzeitwerte im Sekunden-*}
{*            raster); Anpassung Konvertierung Telegrammtyp v für Kanaldeakti-*}
{*            vierung; INI erweitert: Ausgabepfade für Kurzzeitwerte einstell-*}
{*            bar (Standard wie bisher: PathServer.NetWorkDir), Client-Verbin-*}
{*            dungsdatei in Ausgabepfade schreiben                            *}
{*         2.11:  04.06.2008 WW  Debugdatei für Kurzzeitwerte eines Kanals    *}
{*         2.12:  01.07.2008 WW  Debugdatei für Kurzzeitwerte eines Kanals für*}
{*            mehrere Ausgabepfade (INI-Eintrag Datenformat)                  *}
{*         2.13:  30.07.2008 WW  Kurzzeitwerte im Standard-ASCII-Format       *}
{*         2.14:  15.05.2009 WW  IP-Adresse alleiniges Schlüsselfeld in GPRS- *}
{*            Verbindungsliste                                                *}
{*         2.2:   23.05.2011 WW  Journaltabelle mit Stationsname und Gerätetyp*}
{******************************************************************************}
unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ScktComp, Buttons, ComCtrls, ImgList, Contnrs,
  PathIni, WChars, LogFile, WSocketError, Lizenz32, EndeWin, T_Zeit,
  GPRSVerbList, GPRS_Util, GPRSKonvThr, GPRSSrvIniFile, GPRSConst,
  WMessageReg, WMessageSend, GPRSTelegrList, AusgabeDirList, WSysCon;

type
  TFormGPRSServerMain = class(TForm)
    ServerSocket: TServerSocket;
    Timer: TTimer;
    PageControl: TPageControl;
    tsheetKomm: TTabSheet;
    pLeft: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pFehler: TPanel;
    pErrors: TPanel;
    pKommunikation: TPanel;
    pReceive: TPanel;
    memoReceive: TMemo;
    memoError: TMemo;
    pVerbindungen: TPanel;
    Panel2: TPanel;
    pRight: TPanel;
    pStatistik: TPanel;
    Panel6: TPanel;
    lSrvLaufzeit: TLabel;
    lAnzVerbGeoeffnet: TLabel;
    lAnzVerbGeschlossen: TLabel;
    lAnzRecTelegramme: TLabel;
    lAnzRecBytes: TLabel;
    lAnzIPFehler: TLabel;
    lAnzReadEvents: TLabel;
    lAnzRecBloecke: TLabel;
    eSrvLaufzeit: TEdit;
    eAnzVerbGeoeffnet: TEdit;
    eAnzVerbGeschlossen: TEdit;
    eAnzRecTelegramme: TEdit;
    eAnzRecBytes: TEdit;
    eAnzIPFehler: TEdit;
    eAnzReadEvents: TEdit;
    eAnzRecBloecke: TEdit;
    lvVerbindungen: TListView;
    VerbImageList: TImageList;
    pVerbAktiv: TPanel;
    pVerbInaktiv: TPanel;
    pPort: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ServerSocketClientConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocketClientDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocketClientRead(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocketClientError(Sender: TObject;
      Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
      var ErrorCode: Integer);
    procedure TimerTimer(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgramIni: TGPRSSrvIni;
    WieserIniFileName: string;
    FNetProgDir: string;
    FNetWorkDir: string;
    FAusgabeDirList: TAusgabeDirList;
    WLizenz32: TWLizenz32;
    AnzGesamtReadEvent: integer;
    AnzGesamtRecTelegramme: integer;
    AnzGesamtRecBlock: integer;
    AnzGesamtRecBytes: integer;
    AnzGesamtSendTelegramme: integer;
    AnzGesamtSendBytes: integer;
    AnzGesamtVerbGeoffnet: integer;
    AnzGesamtVerbGeschlossen: integer;
    AnzGesamtIPFehler: integer;
    GPRSVerbindungenListe: TGPRSVerbList;
    GPRSTelegrammListe: TGPRSTelegrList;
    GPRSKonvThread: TGPRSKonvThread;
    dtServerStart: TDateTime;
    Tage_Merk: integer;
    MRGTelegramme: string;
    bAktuIPVerbAnzeige: boolean;
    bSendWieserMsg_NKD: boolean;
    FormDestroying: boolean;
    bAnzeigeRohdaten: boolean;
    bDebugDatenProtokoll: boolean;
    bDebugFehlerProtokoll: boolean;
    bDebugStatistikProtokoll: boolean;
    bDebugRohdaten: boolean;
    bAusgabeKurzzeitwerte: boolean;
    procedure SetFensterTitel;
    procedure InitIPCommData;
    procedure SaveTelegrammNr (AGPRSTelegrNr: integer);
    procedure CloseServerPort;
    procedure CloseKonvThread;
    procedure ShowPortStatus;
    procedure WriteProgramErrorLog (S: string);
    procedure AktuIPVerbindungen;
    procedure AddMemoReceive (S: string);
    procedure AddMemoError (S: string);
    procedure CutMemoReceive;
    procedure CutMemoError;
    procedure Benachrichtigen_NKD;
    procedure WriteDataLog (S: string);
    procedure WriteErrorLog (S: string);
    procedure WMAktuIPVerbindungen (var AMessage : TMessage); message wm_AktuIPVerbindungen;
    procedure WMSendWieserMsg_NKD (var AMessage: TMessage); message wm_SendWieserMsg_NKD;
  public
    { Public-Deklarationen }
  end;

var
  FormGPRSServerMain: TFormGPRSServerMain;

implementation

{$R *.DFM}

resourcestring
  SMsgClientAddress      = 'Adresse:';
  SMsgClientPort         = 'Port:';
  SMsgClientConnected    = '<Client-Verbindung ist geöffnet>';
  SMsgClientDisconnected = '<Client-Verbindung ist geschlossen>';
  SMsgEmptyDirName       = 'Die INI-Konfiguration enthält leeren Ausgabe-Verzeichnisnamen !';
  SMsgMultipleDirName    = 'In der INI-Konfiguration der Ausgabe-Verzeichnisse sind gleiche Verzeichnisnamen mehrfach vorhanden !';
  SMsgUndefDirErr        = 'Undefinierter Fehler in der INI-Konfiguration der Ausgabe-Verzeichnisse !';
  SMsgErrDirCreate       = 'Verzeichnis %s kann nicht erstellt werden !';


{--------------------------------------------------------}
procedure TFormGPRSServerMain.FormCreate(Sender: TObject);
{--------------------------------------------------------}
var
  ExeName: string;
  S: string;
  pi: TProgramIni;
  ps: TPathServer;
  Erg: integer;
  i: integer;
  DirName: string;

begin
  SetFensterTitel;
  PageControl.ActivePage:=tsheetKomm;

  GPRSVerbindungenListe:=TGPRSVerbList.Create ([af_Aktiv, af_IPAdresse, af_Port,
    af_AnzRecTelegr, af_AnzRecBloecke, af_AnzRecBytes,
    af_AnzVerbOpen, af_AnzVerbClose,
    af_Kennung, af_GerTypName,
    af_LetzteAktion]);

  InitIPCommData;
  MRGTelegramme:='';
  bAktuIPVerbAnzeige:=false;
  bSendWieserMsg_NKD:=false;
  Tage_Merk:=-1;  // für tägliche Laufzeit-Lizenzprüfung
  GPRSKonvThread:=nil;

  pi:=TProgramIni.Create;
  try
    WieserIniFileName:=pi.WieserIniFile;
    try
      // lokalen PathServer initialisieren:
      ps:=TPathServer.Create(pi.WieserIniFile,
                             [WNetProgDir,
                              WStammDir,
                              WWorkDir,
                              WNetWorkDir,
                              AsciiDir,
                              WStammDb,
                              AutoDb,
                              ManuDb
                             ], false, true); // keine eigene Session, Zugriff prüfen; 01.03.2007, WW
    except
      pPort.Font.Color:=clred;
      S:=SMsgSrvInactive + SMsgPathServerCreateError;
      pPort.Caption:=S;          { Fehler-Ausgabe im Server-Fenster }
      WriteProgramErrorLog (S);  { Fehler-Ausgabe in Programm-Error-Logfile }
      MessageDlg (S, mtError, [mbOk], 0);
      Application.Terminate;
      exit;
    end;
  finally
    pi.Free;
  end;

  try
    try
      ps.Check;
    except
      pPort.Font.Color:=clred;
      S:=SMsgSrvInactive + SMsgPathsDBNotFound;
      pPort.Caption:=S;          { Fehler-Ausgabe im Server-Fenster }
      WriteProgramErrorLog (S);  { Fehler-Ausgabe in Programm-Error-Logfile }
      MessageDlg (S, mtError, [mbOk], 0);
      Application.Terminate;
      exit;
    end;

    FNetProgDir:=ps.Pathname[WNetProgDir];
    FNetWorkDir:=ps.Pathname[WNetWorkDir];
  finally
    ps.Free;
  end;

  // ProgramIni initialisieren:
  ProgramIni:=TGPRSSrvIni.Create (FNetProgDir);
  FAusgabeDirList:=TAusgabeDirList.Create (true);

  bAnzeigeRohdaten:=ProgramIni.AnzeigeRohdaten;
  bDebugDatenProtokoll:=ProgramIni.DebugDatenProtokoll;
  bDebugFehlerProtokoll:=ProgramIni.DebugFehlerProtokoll;
  bDebugStatistikProtokoll:=ProgramIni.DebugStatistikProtokoll;
  bDebugRohdaten:=ProgramIni.DebugRohdaten;
  bAusgabeKurzzeitwerte:=ProgramIni.AusgabeKurzzeitwerte;
  // Ausgabe-Verzeichnisse aus INI lesen:
  Erg:=ProgramIni.GetAusgabeDirList (FAusgabeDirList, FNetWorkDir);
  if Erg < 0 then begin
    case Erg of
      -1: S:=SMsgEmptyDirName;     // fehlender Verzeichnisname (leer)
      -2: S:=SMsgMultipleDirName;  // gleicher Verzeichnisname mehrfach vorhanden
    else
      S:=SMsgUndefDirErr;  // undefinierter Fehler in Verzeichnis-Einstellungen
    end;
    pPort.Font.Color:=clred;
    pPort.Caption:=S;          { Fehler-Ausgabe im Server-Fenster }
    WriteProgramErrorLog (S);  { Fehler-Ausgabe in Programm-Error-Logfile }
    MessageDlg (S, mtError, [mbOk], 0);
    Application.Terminate;
    exit;
  end;
  // Ausgabe-Verzeichnisse anlegen, wenn nicht vorhanden:
  for i:=0 to (FAusgabeDirList.Count-1) do begin
    DirName:=TAusgabeDirDataObj (FAusgabeDirList [i]).Daten.DirName;
    if not DirectoryExists (DirName) then begin
      if not ForceDirectories(DirName) then begin
        // Kurzzeitwerte-Pfad konnte nicht angelegt werden !
        pPort.Font.Color:=clred;
        S:=Format (SMsgErrDirCreate, [DirName]);
        pPort.Caption:=S;          { Fehler-Ausgabe im Server-Fenster }
        WriteProgramErrorLog (S);  { Fehler-Ausgabe in Programm-Error-Logfile }
        MessageDlg (S, mtError, [mbOk], 0);
        Application.Terminate;
        exit;
      end;
    end;
  end;

  { Lizenz prüfen: }
  WLizenz32:=TWLizenz32.Create (FNetProgDir + C_Lizenz_Filename, true);
  ExeName:=ExtractFileName(Application.ExeName);
  if not WLizenz32.GetLizenzExe (ExeName) then begin
    // Programm ist nicht lizenziert !
    pPort.Font.Color:=clred;
    S:=SMsgSrvInactive + SMsgLicExe;
    pPort.Caption:=S;          { Fehler-Ausgabe im Server-Fenster }
    WriteProgramErrorLog (S);  { Fehler-Ausgabe in Programm-Error-Logfile }
    MessageDlg (S, mtError, [mbOk], 0);
    Application.Terminate;
    exit;
  end;

  { Anm.: Laufzeit-Lizenz wird bei Programmstart und täglich geprüft (in Timer-Methode) }

  if not WLizenz32.GetLizenzPC ('') then begin
    // Programm ist für Rechner nicht lizenziert !
    pPort.Font.Color:=clred;
    S:=SMsgSrvInactive + SMsgLicPC;
    pPort.Caption:=S;
    WriteProgramErrorLog (S);  { Fehler-Ausgabe in Programm-Error-Logfile }
    MessageDlg (S, mtError, [mbOk], 0);
    Application.Terminate;
    exit;
  end;

  GPRSTelegrammListe:=TGPRSTelegrList.Create;
  GPRSTelegrammListe.Duplicates:=dupAccept;  // mehrfache, gleiche Einträge erlaubt
  GPRSTelegrammListe.TelegrNr:=ProgramIni.LetztTelegrammNr;  // letzte vergebene GPRS-Telegrammnummer einlesen
  GPRSTelegrammListe.CBSaveTelegrNr:=SaveTelegrammNr;  // Callback-Prozedur zum Speichern der GPRS-Telegrammnummer

  ServerSocket.Port:=ProgramIni.ServerPort;    { Port aus Konfiguration lesen }
  ServerSocket.Open;
  { Port-Anzeige: }
  ShowPortStatus;

  // Start-Eintrag in Daten-Logfile schreiben:
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 + 'Start';
  WriteDataLog (S);

  Timer.Enabled:=true;
  FormDestroying:=false;

  WindowState:=wsMaximized;
end;

{---------------------------------------------------------}
procedure TFormGPRSServerMain.FormDestroy(Sender: TObject);
{---------------------------------------------------------}
begin
  WLizenz32.Free;
  FAusgabeDirList.Free;
  ProgramIni.Free;
  GPRSTelegrammListe.Free;
  GPRSVerbindungenListe.Free;
end;

{------------------------------------------------------}
procedure TFormGPRSServerMain.FormClose(Sender: TObject;
  var Action: TCloseAction);
{------------------------------------------------------}
var
  S: string;
begin
  EndeFensterZeigen (Application.Icon, Application.Title,
                     'GPRS-Server wird beendet. Bitte warten...');

  FormDestroying:=true;
  Timer.Enabled:=false;

  // Stop-Eintrag und Gesamt-Statistikdaten in Daten-Logfile schreiben:
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 + 'Stop' + CR + LF +
     lSrvLaufzeit.Caption + ' ' + eSrvLaufzeit.Text + CR + LF +
     lAnzVerbGeoeffnet.Caption + ' ' + eAnzVerbGeoeffnet.Text + CR + LF +
     lAnzVerbGeschlossen.Caption + ' ' + eAnzVerbGeschlossen.Text + CR + LF +
     lAnzReadEvents.Caption + ' ' + eAnzReadEvents.Text + CR + LF +
     lAnzRecTelegramme.Caption + ' ' + eAnzRecTelegramme.Text + CR + LF +
     lAnzRecBloecke.Caption + ' ' + eAnzRecBloecke.Text + CR + LF +
     lAnzRecBytes.Caption + ' ' + eAnzRecBytes.Text + CR + LF +
     lAnzIPFehler.Caption + ' ' + eAnzIPFehler.Text;
  WriteDataLog (S);

  // Statistikdaten der einzelnen Verbindungen in Statistik-Ascii-Datei schreiben:
  if bDebugStatistikProtokoll then
   GPRSVerbindungenListe.WriteStatistikFile (dtServerStart, eSrvLaufzeit.Text);

  CloseServerPort;  // Port schließen
  CloseKonvThread;  // Konvertierungsthread beenden
end;

{--------------------------------------------}
procedure TFormGPRSServerMain.SetFensterTitel;
{--------------------------------------------}
begin
  Caption:=Application.Title + ' - Vs. ' + CVersion_GPRSSrv + ', ' +
           FormatDateTime('dd.mm.yyyy', (FileDateToDateTime (FileAge (Application.ExeName)))) +
           ', Copyright © 2006-2008 RMG Messtechnik GmbH';
end;

{-------------------------------------------}
procedure TFormGPRSServerMain.InitIPCommData;
{-------------------------------------------}
begin
  // Statistikwerte und zugehörige Anzeigefelder initialisieren:
  dtServerStart:=Now;
  AnzGesamtVerbGeoffnet:=0;
  AnzGesamtVerbGeschlossen:=0;
  AnzGesamtIPFehler:=0;
  AnzGesamtReadEvent:=0;
  AnzGesamtRecTelegramme:=0;
  AnzGesamtRecBlock:=0;
  AnzGesamtRecBytes:=0;
  AnzGesamtSendTelegramme:=0;
  AnzGesamtSendBytes:=0;

  eSrvLaufzeit.Text:='';
  eAnzVerbGeoeffnet.Text:='0';
  eAnzVerbGeschlossen.Text:='0';
  eAnzIPFehler.Text:='0';
  eAnzReadEvents.Text:='0';
  eAnzRecTelegramme.Text:='0';
  eAnzRecBloecke.Text:='0';
  eAnzRecBytes.Text:='0';
end;

{---------------------------------------------------------------------}
procedure TFormGPRSServerMain.SaveTelegrammNr (AGPRSTelegrNr: integer);
{---------------------------------------------------------------------}
{ Telegramm-Nummer abspeichern }
begin
  ProgramIni.LetztTelegrammNr:=AGPRSTelegrNr;
end;

{--------------------------------------------}
procedure TFormGPRSServerMain.CloseServerPort;
{--------------------------------------------}
{ Port schließen }
begin
  if ServerSocket.Active then begin
    ServerSocket.Close;
    ShowPortStatus;

    // Verbindungsliste leeren
    GPRSVerbindungenListe.Clear;
    AktuIPVerbindungen;
  end;
end;

{--------------------------------------------}
procedure TFormGPRSServerMain.CloseKonvThread;
{--------------------------------------------}
{ GPRS-Konvertierungsthread beenden }
begin
  if Assigned (GPRSKonvThread) then begin
    try
      GPRSKonvThread.Resume;    // Thread fortsetzen, falls er unterbrochen ist (sonst reagiert er nicht)
      GPRSKonvThread.Terminate; // Thread-Beenden einleiten
      GPRSKonvThread.WaitFor;   // warten bis Thread beendet ist
    except
      // Ungültiges Handle unterdrücken
    end;
  end;
end;


{-------------------------- Socket-Kommunikation ------------------------------}

// KEINE Application.ProcessMessages-Aurfufe in den Socket-Ereignisroutinen !
// Der Ablauf der Ereignisroutine wird sonst evtl. durch ein zeitgleiches neues
// Ereignis unterbrochen und erst zu einem späteren Zeitpunkt fortgeführt !
// Probleme bislang keine bekannt, aber die Reihenfolge der Programmschritte
// ändert sich !

{----------------------------------------------------------------------}
procedure TFormGPRSServerMain.ServerSocketClientConnect(Sender: TObject;
  Socket: TCustomWinSocket);
{----------------------------------------------------------------------}
{ eine Verbindung wurde geöffnet }
var
  S: string;

begin
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 +
     'Open' + #9 + Socket.RemoteAddress + ':' + IntToStr(Socket.RemotePort);
  WriteDataLog (S);
  AddMemoReceive (S);

  // Verbindungsliste aktualisieren
  GPRSVerbindungenListe.IncrementVerbCount (true, Socket.RemoteAddress, Socket.RemotePort);
  bAktuIPVerbAnzeige:=true;

  inc (AnzGesamtVerbGeoffnet);
  eAnzVerbGeoeffnet.Text:=IntToStr (AnzGesamtVerbGeoffnet);
end;

{-------------------------------------------------------------------------}
procedure TFormGPRSServerMain.ServerSocketClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
{-------------------------------------------------------------------------}
{ eine Verbindung wurde geschlossen }
var
  S: string;

begin
  S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 +
     'Close' + #9 + Socket.RemoteAddress + ':' + IntToStr(Socket.RemotePort);
  WriteDataLog (S);
  AddMemoReceive (S);

  // Verbindungsliste aktualisieren
  GPRSVerbindungenListe.IncrementVerbCount (false, Socket.RemoteAddress, Socket.RemotePort);
  bAktuIPVerbAnzeige:=true;

  inc (AnzGesamtVerbGeschlossen);
  eAnzVerbGeschlossen.Text:=IntToStr (AnzGesamtVerbGeschlossen);
end;

{-------------------------------------------------------------------}
procedure TFormGPRSServerMain.ServerSocketClientRead(Sender: TObject;
  Socket: TCustomWinSocket);
{-------------------------------------------------------------------}
{ es wurden Zeichen empfangen }
var
  S, RecBuf: string;
  sData: string;
  AnzTelegr: integer;
  AnzBytes: integer;
  OneMRGTelegramm: string;

begin
  inc (AnzGesamtReadEvent);

  RecBuf:=Socket.ReceiveText;
  MRGTelegramme:=MRGTelegramme + RecBuf;
  while length (RecBuf) > 0 do begin  // bis alle Zeichen empfangen wurden
    // Anzahl der empfangenen Blöcke
    inc (AnzGesamtRecBlock);  // Gesamt
    // Anzahl der empfangenen Bytes
    AnzBytes:=length (RecBuf);
    inc (AnzGesamtRecBytes, AnzBytes);  // Gesamt

    sData:=SonderzeichenString (RecBuf);
    S:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 +
       'Read' + #9 + Socket.RemoteAddress + ':' + IntToStr(Socket.RemotePort) + #9 +
        IntToStr (AnzBytes) + #9 + 'Bytes';
    WriteDataLog (S + #9 + sData);  // Datenprotokolldatei immer mit Rohdaten
    if bAnzeigeRohdaten then
      S:=S + #9 + sData;
    AddMemoReceive (S);

    AnzTelegr:=0;
    if FIsCompletePushTelegramm (MRGTelegramme, mrgtyp_MRG910) then begin
      // MRGTelegramm kann eines oder mehrere MRG-Datentelegramme enthalten !
      // Konvertierung des Datentelegramms in eigenen Thread auslagern.
      // Grund: Konvertierung kann länger dauern und somit den Empfang weiterer
      // Telegramme blockieren !

      { alle MRG-Telegramme einzeln abarbeiten: }
      while length (MRGTelegramme) > 0 do begin
        inc (AnzTelegr);  // Anzahl der enthaltenen MRG-Telegramme hochzählen

        OneMRGTelegramm:=FCutOnePushTelegramm (MRGTelegramme, mrgtyp_MRG910);  { ein MRG-Telegramm ausschneiden }
        { Telegramm in Telegrammliste eintragen: }
        GPRSTelegrammListe.SetTelegramm (OneMRGTelegramm, mrgtyp_MRG910,
                                         Socket.RemoteAddress,
                                         nil);  // ClientSocket wird nicht benötigt
        { Konvertierungs-Thread starten, wenn noch nicht erfolgt: }
        if not Assigned (GPRSKonvThread) then
          GPRSKonvThread:=TGPRSKonvThread.CreateIt (GPRSTelegrammListe,
            not bDebugRohdaten, bDebugFehlerProtokoll, bDebugDatenProtokoll,
            GPRSVerbindungenListe, Handle, WieserIniFileName, FAusgabeDirList,
            bAusgabeKurzzeitwerte);
      end;  { while length (MRGTelegramme) > 0 }
      MRGTelegramme:='';

      inc (AnzGesamtRecTelegramme, AnzTelegr);  // Gesamt
    end;

    // Verbindungsliste aktualisieren
    GPRSVerbindungenListe.IncrementRecCounts (Socket.RemoteAddress,
                                              AnzTelegr, 1, AnzBytes);

    RecBuf:=Socket.ReceiveText;
    MRGTelegramme:=MRGTelegramme + RecBuf;
  end;

  bAktuIPVerbAnzeige:=true;

  eAnzReadEvents.Text:=IntToStr (AnzGesamtReadEvent);
  eAnzRecTelegramme.Text:=IntToStr (AnzGesamtRecTelegramme);
  eAnzRecBloecke.Text:=IntToStr (AnzGesamtRecBlock);
  eAnzRecBytes.Text:=IntToStr (AnzGesamtRecBytes);
end;

{---------------------------------------------------------------------------}
procedure TFormGPRSServerMain.ServerSocketClientError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
{---------------------------------------------------------------------------}
{ es trat ein Socket-Fehler auf }
var
  S: string;
  sErrorEvent: string;
begin
  inc (AnzGesamtIPFehler);
  eAnzIPFehler.Text:=IntToStr (AnzGesamtIPFehler);

  case ErrorEvent of
    eeGeneral: sErrorEvent:='eeGeneral';
    eeSend: sErrorEvent:='eeSend';
    eeReceive: sErrorEvent:='eeReceive';
    eeConnect: sErrorEvent:='eeConnect';
    eeDisconnect: sErrorEvent:='eeDisconnect';
    eeAccept: sErrorEvent:='eeAccept';
    eeLookup: sErrorEvent:='eeLookup';
  else
    sErrorEvent:='unbekannt';
  end;

  S:='ErrorEvent: ' +  sErrorEvent + ' (' +
     IntToStr (Integer(ErrorEvent)) + '), ErrorCode: ' + SocketErrorCodeToString (Errorcode) +
     ' (' + IntToStr (ErrorCode) + ')';
  WriteErrorLog (S);
  AddMemoError (S);

  ErrorCode:=0;  // Exception-Ausgabe unterdrücken
end;


{-------------------------------- Anzeige -------------------------------------}

{-------------------------------------------}
procedure TFormGPRSServerMain.ShowPortStatus;
{-------------------------------------------}
{ Status Port geöffnet/geschlossen anzeigen }
resourcestring
  SMsgPortOpened = ' Port %d geöffnet, Server ist aktiv';
  SMsgPortClosed = ' Port %d geschlossen, Server ist beendet';
var
  S: string;

begin
  if ServerSocket.Active then begin
    pPort.Font.Color:=clgreen;
    S:=Format (SMsgPortOpened, [ServerSocket.Port]); { OK, Port geöffnet }
  end
  else begin
    pPort.Font.Color:=clred;
    S:=Format (SMsgPortClosed, [ServerSocket.Port]); { Fehler, Port nicht geöffnet }
  end;
  pPort.Caption:=S;          { Ausgabe im Server-Fenster }
  WriteProgramErrorLog (S);  { Ausgabe in Programm-Error-Logfile }
end;

{-----------------------------------------------}
procedure TFormGPRSServerMain.AktuIPVerbindungen;
{-----------------------------------------------}
{ aktuelle Info zu Client-Verbindungen anzeigen/schreiben }
var
  i: integer;
  DirName: string;
  FName: string;

begin
  // Anzeige für aktuelle Verbindungen aktualisieren:
  GPRSVerbindungenListe.VerbindungenAnzeigen (lvVerbindungen, pVerbAktiv, pVerbInaktiv);

  // Verbindungsinfo-Übergabedatei für Anzeige-Client schreiben:
  for i:=0 to (FAusgabeDirList.Count-1) do begin
    if TAusgabeDirDataObj (FAusgabeDirList [i]).Daten.ClientVerbindungen then begin
      DirName:=TAusgabeDirDataObj (FAusgabeDirList [i]).Daten.DirName;
      FName:=GetFilename_GPRS_Verbindungen (DirName);
      GPRSVerbindungenListe.WriteClientInfoFile (FName);
    end;
  end;
end;

{-------------------------------------------------------}
procedure TFormGPRSServerMain.AddMemoReceive (S: string);
{-------------------------------------------------------}
{ neuen MemoReceive-Eintrag anhängen }
begin
  memoReceive.Lines.BeginUpdate;
  try
    memoReceive.Lines.Add (S);
    CutMemoReceive;
  finally
    memoReceive.Lines.EndUpdate;
  end;
  // ans Ende scrollen:
  memoReceive.Perform (EM_LineScroll, 0, memoReceive.Lines.Count-1);
end;

{-------------------------------------------}
procedure TFormGPRSServerMain.CutMemoReceive;
{-------------------------------------------}
{ Anzahl der MemoReceive-Einträge begrenzen (zur Sicherheit, wer weiß nach wievielen
  Einträgen es kracht !?) }
begin
  while memoReceive.Lines.Count > 1000 do      { max. 1000 Zeilen darstellen }
    memoReceive.Lines.Delete (0);
end;

{-----------------------------------------------------}
procedure TFormGPRSServerMain.AddMemoError (S: string);
{-----------------------------------------------------}
{ neuen MemoError-Eintrag anhängen }
begin
  memoError.Lines.BeginUpdate;
  try
    memoError.Lines.Add (FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + #9 + S);
    CutMemoError;
  finally
    memoError.Lines.EndUpdate;
  end;
  // ans Ende scrollen:
  memoError.Perform (EM_LineScroll, 0, memoError.Lines.Count-1);
end;

{-----------------------------------------}
procedure TFormGPRSServerMain.CutMemoError;
{-----------------------------------------}
{ Anzahl der MemoError-Einträge begrenzen (zur Sicherheit, wer weiß nach wievielen
  Einträgen es kracht !?) }
begin
  while memoError.Lines.Count > 1000 do      { max. 1000 Zeilen darstellen }
    memoError.Lines.Delete (0);
end;


{------------------------ Wieser Benachrichtigung -----------------------------}

{------------------------------------------------}
procedure TFormGPRSServerMain.Benachrichtigen_NKD;
{------------------------------------------------}
{ Benachrichtigung "Kurzzeitwerte vorhanden" versenden }
var
  WMsgRegList: TObjectList;
  WMessageReg: TWMessageReg;
  bDebug: boolean;

begin
  bDebug:=true;  // Flag für Debugging der Wieser-Benachrichtigung

  WMsgRegList:=TObjectList.Create (true);
  try
    WMessageReg:=TWMessageReg.Create (FNetProgDir);
    try
      { alle für "neue Kurzzeitdaten vorhanden" registrierten Nachrichten-Empfänger
        in Liste laden: }
      WMessageReg.GetMsgRegistrationList (wmt_NeueKurzzeitDaten, WMsgRegList);
      { Benachrichtigung "neue Kurzzeitdaten vorhanden": }
      SendWieserMsg_NeueKurzzeitDaten (WMsgRegList, FNetProgDir, bDebug);
    finally
      WMessageReg.Free;
    end;
  finally
    WMsgRegList.Free;
  end;
end;


{------------------------- Logging und Statistik ------------------------------}

{-------------------------------------------------------------}
procedure TFormGPRSServerMain.WriteProgramErrorLog (S: string);
{-------------------------------------------------------------}
{ Eintrag in Programm-Error-Logfile schreiben }
var
  ErrLogFile: TCustomLogFile;
  FileName: string;

begin
  FileName:=ChangeFileExt (ExtractFileName (Application.ExeName), '');
  ErrLogFile:=TCustomLogFile.Create (ExtractFilePath (ParamStr(0)), FileName, false);
  try
    ErrLogFile.Write (S);
  finally
    ErrLogFile.Free;
  end;
end;

{-----------------------------------------------------}
procedure TFormGPRSServerMain.WriteDataLog (S: string);
{-----------------------------------------------------}
{ Eintrag in IP-Daten-Logdatei schreiben (IP-Ereignisse/Empfangsdaten);
  Übergabe: Logdatei-Eintrag }
var
  FName: string;
begin
  if bDebugDatenProtokoll then begin
    FName:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_Data';
    WriteDebugLog (ExtractFilePath (ParamStr(0)), FName, S, false);
    // Data-Logfile ohne Header für Excel-Auswertung; 13.06.2007, WW
  end;
end;

{------------------------------------------------------}
procedure TFormGPRSServerMain.WriteErrorLog (S: string);
{------------------------------------------------------}
{ Eintrag in IP-Fehler-Logdatei schreiben;
  Übergabe: Logdatei-Eintrag }
var
  FName: string;
begin
  if bDebugFehlerProtokoll then begin
    FName:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_Error';
    WriteDebugLog (ExtractFilePath (ParamStr(0)), FName, '***' + CR + LF + S + CR + LF);
  end;
end;


{------------------------------- Timer ----------------------------------------}

{--------------------------------------------------------}
procedure TFormGPRSServerMain.TimerTimer(Sender: TObject);
{--------------------------------------------------------}
var
  dtServerLaufzeit: TDateTime;
  d: integer;
  h, m, sec, ms: word;
  ExeName: string;
  S: string;

begin
  Timer.Enabled:=false;
  try
    // Laufzeit seit Port geöffnet wurde anzeigen:
    dtServerLaufzeit:=Now - dtServerStart;
    d:=Trunc (dtServerLaufzeit);  // Tage
    DecodeTime (dtServerLaufzeit, h, m, sec, ms);
    eSrvLaufzeit.Text:=Format ('%d t  %d h  %d min  %d s', [d, h, m, sec]);

    // beim Tageswechsel:
    if d <> Tage_Merk then begin
      Tage_Merk:=d;
      // Laufzeit-Lizenz prüfen:
      if Assigned (WLizenz32) then begin
        ExeName:=ExtractFileName(Application.ExeName);
        if not WLizenz32.GetExeLaufzeit (ExeName) then begin
          // Programm-Lizenz ist abgelaufen !
          pPort.Font.Color:=clred;
          S:=SMsgSrvInactive + SMsgLicExeLaufzeit;
          pPort.Caption:=S;
          WriteProgramErrorLog (S);  { Fehler-Ausgabe in Programm-Error-Logfile }

          CloseServerPort;  // Port schließen
          FormDestroying:=true;  // keine neuen Aktionen zulassen
        end;
      end;
    end;

    // IP-Verbindungen aktualisieren, wenn erforderlich:
    if bAktuIPVerbAnzeige then begin
      AktuIPVerbindungen;
      bAktuIPVerbAnzeige:=false;
    end;

    // Wieser-Benachrichtigung für neue Kurzzeitwerte verschicken, wenn erforderlich;
    if bSendWieserMsg_NKD then begin
      Benachrichtigen_NKD;
      bSendWieserMsg_NKD:=false;
    end;
  finally
    if not FormDestroying then
      Timer.Enabled:=true;
  end;
end;


{------------------------- Message-Behandlungsmethoden ------------------------}

{--------------------------------------------------------------------------}
procedure TFormGPRSServerMain.WMAktuIPVerbindungen (var AMessage: TMessage);
{--------------------------------------------------------------------------}
{ Message-Behandlungsroutine wird aufgerufen, wenn IP-Verbindungsanzeige
  aktualisiert werden soll (Threads) }
begin
  bAktuIPVerbAnzeige:=true;
end;

{-------------------------------------------------------------------------}
procedure TFormGPRSServerMain.WMSendWieserMsg_NKD (var AMessage: TMessage);
{-------------------------------------------------------------------------}
{ Message-Behandlungsroutine wird aufgerufen, wenn Wieser-Benachrichtigung für
  neue Kurzzeitwerte verschickt werden soll (Threads) }
begin
  bSendWieserMsg_NKD:=true;
end;

end.


