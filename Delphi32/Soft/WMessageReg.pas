{******************************************************************************}
{* Unit: Registrierung für Wieser-Benachrichtigung über TCP/IP                *}
{* 14.10.2004  WW                                                             *}
{* 24.11.2004  GD  Erweitert für Export                                       *}
{* 29.06.2005  GD  Erweitert um Abruf-Statusabfrage und Löschen eines Typs    *}
{* 29.10.2008  GD  Bugfix: Suspenden des MsgThreads vor Freigabe              *}
{* 09.02.2023  WW  mit Nachrichtentyp Stationsereignis                        *}
{******************************************************************************}
unit WMessageReg;

interface

uses
  Windows, Forms, Classes, SysUtils, contnrs, ScktComp, SyncObjs, WinSock,
  novell, PathIni, WChars, T_Zeit;

const
  { Stati der DSfG-Abrufe }
  C_Status_Waiting        = 0;
  C_Status_Abruf          = 1;
  C_Status_Konfiguration  = 2;
  C_Status_Momentanwerte  = 3;
  C_Status_MomentanDfue   = 4;
  C_Status_RufAbfragen    = 5;
  C_Status_FirmwareUpdate = 6;

  C_WMsgType_Deaktiviert = $80000000;  // MSB in TWMsgTyp ist reserviert für:
                                       // "TWMsgReg-Eintrag ist deaktiviert"
  C_WMsgType_Aktivieren  = $7FFFFFFF;  // Konstante für Aktivierung

type
  { Nachrichtentypen
    -> Achtung: neue Typen immer am Ende anhängen, sonst ändert sich der für jeden
       Typ intern verwendete Aufzählungswert ! Typen werden programmübergreifend
       verwendet ! }
  TWMsgType = (wmt_NeueArchivDaten,      { Ereignis: neue MRG/DSfG-Archivdaten vorhanden (aus Abruf) }
               wmt_CmdCreateExportData,  { Kommando: Exportdaten erzeugen }
               wmt_NeueWicalDaten,       { Ereignis: neue Auswertedaten vorhanden }
               wmt_CmdCreateWicalData,   { Kommando: WiCal-Daten erzeugen }
               wmt_ExportCmdDone,        { Ereignis: Export-Kommando ausgeführt }
               wmt_AbrufStatus,          { Abfrage : Abrufstatus }
               wmt_ArchivDatenAbgerufen, { Ereignis: MRG/DSfG-Archivdaten wurden abgerufen }
               wmt_NeueKurzzeitDaten,    { Ereignis: neue Kurzzeitdaten vorhanden (aus GPRS-Empfang) }
               wmt_StationEvent          { Ereignis zu einer Station (z.B. nicht erreicht); 15.03.2023, WW }
              );

  TWMsgTypes = set of TWMsgType;

  { Record für Nachrichten-Registrierung }

  TWMsgReg = packed record
{$H-}
    Host: string [100];
{$H+}
    Port: integer;
    MsgTypes: integer;
  end;

  // Thread für Nachrichtenempfang - hat durchzulaufen
  TWMsgSrvProc = procedure(sMsg: string) of object;
  TWMsgSvrThread = class(TThread)
    constructor Create(
      sNetProgDir: TFileName; pWMsgTypes: TWMsgTypes; pCallBack: TWMsgSrvProc);
    destructor Destroy; override;
  private
    FMsgSrvSocket : TServerSocket;
    FCritSection  : TCriticalSection;
    FCallBack     : TWMsgSrvProc;
    FNetProgDir   : TFileName;
    FPort         : integer;
    FReceivedText : string;
    FSendText     : string;
    procedure OnClientRead(pSender: TObject; pSocket: TCustomWinSocket);
  protected
    procedure FormatText(sText: string); virtual;
    procedure SendText; virtual;
    procedure Execute; override;
  public
    property Port: integer read FPort;
  end;

  { Objekt für Nachrichten-Registrierung }

  TWMsgRegObj = class (TObject)
    Data: TWMsgReg;
  public
    procedure SetData (AWMsgReg: TWMsgReg);
  end;

  { Klasse für Nachrichten-Registrierung }

  TWMessageReg = class(TObject)
  private
    Pfad: string;
    function GetMsgRegFilename: string;
    function GetMinPortId (Hostname: string): integer;
    function GetLocalComputername: string;
    function EncodeMsgType (AWMsgTypes: TWMsgTypes): integer;
  public
    constructor Create (APfad: string);
    function RegisterMessaging (AWMsgTypes: TWMsgTypes; var PortId: integer): boolean;
    function UnregisterMessaging (AHostname: string; APortId: integer): boolean;
    function GetMsgRegistrationList (AWMsgType: TWMsgType; RegList: TObjectList;
                                     AllWMsgTypes: boolean = false): boolean;
    function UnregisterMessageTypes(pWMsgTypes: TWMsgTypes): integer;
    function DeactivateMessaging (AHostname: string; APortId: integer): boolean;
    function VerifyMessaging (iPortId: integer): boolean;
    function CheckRegistration (APortId: integer): boolean;
  end;

function RegisterWieserMessaging (ANetProgPath: string; AWMsgTypes: TWMsgTypes;
                                  var PortId: integer): boolean;
function UnregisterWieserMessaging (ANetProgPath: string; PortId: integer): boolean;

function CheckRegistrationWieserMessaging (ANetProgPath: string; APortId: integer): boolean;

function VerifyWieserMessaging(ANetProgPath: string; APortId: integer): boolean;

implementation

const
  C_WMsgReg_Filename = 'Wnet_msg.dat';

{-------------------------------- TWMsgSvrThread ------------------------------}

{----------------------------------}
constructor TWMsgSvrThread.Create(
  sNetProgDir: TFileName; pWMsgTypes: TWMsgTypes; pCallBack: TWMsgSrvProc);
{----------------------------------}
begin
  inherited Create(True);   // Thread createn und anhalten lassen

  FCallBack := pCallBack;   // Callback für empfangene Texte
  FNetProgDir := sNetProgDir;
  FReceivedText := '';      // Empfangspuffer

  // Benachrichtigungs-Server createn und öffnen
  if (not RegisterWieserMessaging (FNetProgDir, pWMsgTypes, FPort))
  then Self.Terminate
  else begin
    FMsgSrvSocket := TServerSocket.Create(nil);
    FMsgSrvSocket.Active := False;
    FMsgSrvSocket.Port := FPort;
    FMsgSrvSocket.ServerType := stNonBlocking;
//    FMsgSrvSocket.ServerType := stThreadBlocking;  // Dann geht gar nix
    FMsgSrvSocket.OnClientRead := Self.OnClientRead;
    FMsgSrvSocket.Open;
    if (not FMsgSrvSocket.Active) then Self.Terminate;
  end;

  // CriticalSection createn
  FCritSection := TCriticalSection.Create();

  FreeOnTerminate := True;  // Thread soll sich beim Beenden selbst freigeben
  Priority := tpNormal;     // Thread hat normale Priorität
  Suspended := False;       // Thread fortsetzen
end;

{----------------------------------}
destructor TWMsgSvrThread.Destroy;
{----------------------------------}
begin
  Suspended := True;  // Thread anhalten // 29.10.2008

  UnRegisterWieserMessaging(FNetProgDir, FPort);
  if (Assigned(FMsgSrvSocket)) then begin
    FMsgSrvSocket.OnClientRead := nil;
    FMsgSrvSocket.Close;
    FMsgSrvSocket.Free;
  end;

  FCritSection.Free;

  inherited Destroy;
end;

{----------------------------------}
procedure TWMsgSvrThread.FormatText(sText: string);
{----------------------------------}
var
  iPe, iPs : integer;
  s        : string;
begin
  s := FReceivedText + sText;

  iPs := Pos(stx, s);
  iPe := Pos(etx, s);
  while (ips > 0) and (iPe > 0) do begin
    FSendText := Copy(s, iPs, (iPe-iPs+1));
    Synchronize(SendText);

    System.Delete(s, 1, iPe);
    iPs := Pos(stx, s);
    iPe := Pos(etx, s);
  end;

  FReceivedText := s;
end;

{----------------------------------}
procedure TWMsgSvrThread.SendText;
{----------------------------------}
begin
  if (Assigned(FCallBack)) then FCallBack(FSendText);
end;

{----------------------------------}
procedure TWMsgSvrThread.OnClientRead(
  pSender: TObject; pSocket: TCustomWinSocket);
{----------------------------------}
var
  s, sText : string;
begin
  FCritSection.Enter;
  try
    s := pSocket.ReceiveText;
    while (s <> '') do begin
      sText := sText + s;
      Sleep(1);
      s := pSocket.ReceiveText;
    end;
    FormatText(sText);
  finally
    FCritSection.Leave;
  end;
end;

{----------------------------------}
procedure TWMsgSvrThread.Execute;
{----------------------------------}
begin
  while (not Terminated) and (FMsgSrvSocket.Active) do delay(1);
end;

{ TWMsgRegObj }

{-------------------------------------------------}
procedure TWMsgRegObj.SetData (AWMsgReg: TWMsgReg);
{-------------------------------------------------}
begin
  Data:=AWMsgReg;
end;


{ TWMessageReg }

{----------------------------------------------}
constructor TWMessageReg.Create (APfad: string);
{----------------------------------------------}
begin
  inherited Create;
  Pfad:=APfad;
end;

{----------------------------------------------}
function TWMessageReg.GetMsgRegFilename: string;
{----------------------------------------------}
{ Liefert vollständigen Namen der Wieser-Message-Registrierdatei zurück }
begin
  Result:= IncludeTrailingBackslash(Pfad) + C_WMsgReg_Filename;
end;

{-------------------------------------------------------------}
function TWMessageReg.GetMinPortId (Hostname: string): integer;
{-------------------------------------------------------------}
{ liefert die lt. Wieser.ini dem lokalen Rechner zugewiesene kleinste freigegebene PortId }
var
  pi: TProgramIni;
  ps: TPathserver;

begin
  pi:=TProgramIni.Create;
  try
    ps:=TPathserver.Create(pi.WieserIniFile, [WNetProgDir]);
    try
      Result:=ps.IPPortNr_ab [Hostname];
    finally
      ps.Free;
    end;
  finally
    pi.Free;
  end;
end;

{-------------------------------------------------}
function TWMessageReg.GetLocalComputername: string;
{-------------------------------------------------}
{ Liefert lokalen Rechnernamen, unter dem die Anwendung läuft }
var
  LocalMachine: array [0..100] of char;
  Size: DWORD;

begin
  Size:=SizeOf (LocalMachine);
  GetComputerName (LocalMachine, Size);
  Result:=string (LocalMachine);
end;

{--------------------------------------------------------------------}
function TWMessageReg.EncodeMsgType (AWMsgTypes: TWMsgTypes): integer;
{--------------------------------------------------------------------}
{ wandelt Nachrichtentypen vom Typ TWMsgType in Bit-codierten Integerwert }
var
  i: TWMsgType;

begin
  Result:=0;
  for i:=Low (TWMsgType) to High (TWMsgType) do begin
    if (i in AWMsgTypes) then
      Result:=Result OR (1 SHL integer (i));
  end;
end;

{---------------------------------------------------------------------}
function TWMessageReg.RegisterMessaging (AWMsgTypes: TWMsgTypes;
                                         var PortId: integer): boolean;
{---------------------------------------------------------------------}
{ Meldet Applikation in der Nachrichten-Registrierungsdatei an und gibt eine freie
  IPPort-Nummer zurück, über die die Benachrichtigung erfolgt;
  Übergabe: Nachrichtentypen
  Rückgabe: IPPort-Nummer
  Ergebnis: true, wenn Anmeldung erfolgreich }
var
  Filename: string;
  FS: TFileOfRecStreamExt;
  isOK: boolean;
  FRecCount: integer;
  WMsgReg: TWMsgReg;
  MinPortId: integer;
  sLocalMachine: string;

begin
  Result:=false;
  PortId:=-1;
  Filename:=GetMsgRegFilename;
  if FileExists (FileName) then
    FS:=TFileOfRecStreamExt.Create (Filename, fmOpenReadWrite OR fmShareDenyWrite,
                                    SizeOf (WMsgReg), isOK)
  else
    FS:=TFileOfRecStreamExt.Create (Filename, fmCreate, SizeOf (WMsgReg), isOK);
  try
    if isOK then begin
      { lokalen Rechnernamen ermitteln: }
      sLocalMachine:=GetLocalComputername;

      { höchste für den Rechnernamen vergebene PortId ermitteln: dazu ganze Datei durchlesen }
      FRecCount:=FS.RecCount;
      while FS.RecPosition < FRecCount do begin
        Application.ProcessMessages;
        FS.ReadRec (WMsgReg);
        if (WMsgReg.Host = sLocalMachine) AND (WMsgReg.Port > PortId) then
          PortId:=WMsgReg.Port;
      end;

      { nächste freie PortId ermitteln: }
      MinPortId:=GetMinPortId (sLocalMachine);   { kleinste freigegebene PortId }
      if PortId < 0 then     { noch keine Registrier-Einträge vorhanden }
        PortId:=MinPortId
      else begin
        if PortId < MinPortId then   { höchste vergebene PortId ist kleiner als die kleinste freigegebene }
          PortId:=MinPortId
        else
          PortId:=PortId + 1;
      end;

      { Registrier-Eintrag in Datei schreiben: }
      WMsgReg.Host:=sLocalMachine;
      WMsgReg.Port:=PortId;
      WMsgReg.MsgTypes:=EncodeMsgType (AWMsgTypes);
      FS.WriteRec (WMsgReg);
      Result:=true;
    end;
  finally
    FS.Free;
  end;
end;

{---------------------------------------------------------------------------------------}
function TWMessageReg.UnregisterMessaging (AHostname: string; APortId: integer): boolean;
{---------------------------------------------------------------------------------------}
{ Meldet die unter dem übergebenen Hostnamen/IPPort-Nummer angemeldete Applikation aus der
  Nachrichten-Registrierungsdatei ab;
  Übergabe: Hostname
            IPPort-Nummer
  Ergebnis: true, wenn Abmeldung erfolgreich }
var
  Filename: string;
  FS: TFileOfRecStreamExt;
  isOK: boolean;
  FRecCount: integer;
  WMsgReg: TWMsgReg;
  gefunden: boolean;

begin
  Result:=false;
  Filename:=GetMsgRegFilename;
  if FileExists (FileName) then begin
    FS:=TFileOfRecStreamExt.Create (Filename, fmOpenReadWrite OR fmShareDenyWrite,
                                      SizeOf (WMsgReg), isOK);
    try
      if isOK then begin
        { Eintrag für Rechnername und PortId suchen: }
        gefunden:=false;
        FRecCount:=FS.RecCount;
        while FS.RecPosition < FRecCount do begin
          Application.ProcessMessages;
          FS.ReadRec (WMsgReg);
          if (WMsgReg.Host = AHostname) AND (WMsgReg.Port = APortId) then begin
            gefunden:=true;
            Break;
          end;
        end;

        if gefunden then begin
          { Eintrag aus Datei löschen: nachfolgende Einträge um eins nach vorne verschieben }
          while FS.RecPosition < FRecCount do begin
            Application.ProcessMessages;
            FS.ReadRec (WMsgReg);
            FS.RecPosition:=FS.RecPosition - 2;
            FS.WriteRec (WMsgReg);
            FS.RecPosition:=FS.RecPosition + 1;
          end;
          FS.RecCount:=FRecCount - 1;  { File um einen Eintrag kürzen }
        end;
        Result:=true;
      end;
    finally
      FS.Free;
    end;
  end;  { if FileExists }
end;

{---------------------------------------------------------------------------------------}
function TWMessageReg.DeactivateMessaging (AHostname: string; APortId: integer): boolean;
{---------------------------------------------------------------------------------------}
{ Markiert die unter dem übergebenen Hostnamen/IPPort-Nummer angemeldete Applikation in der
  Nachrichten-Registrierungsdatei als deaktiviert.
  Übergabe: Hostname
            IPPort-Nummer
  Ergebnis: true, wenn Deaktivierung erfolgreich }
var
  Filename: string;
  FS: TFileOfRecStreamExt;
  isOK: boolean;
  FRecCount: integer;
  WMsgReg: TWMsgReg;

begin
  Result:=false;
  Filename:=GetMsgRegFilename;
  if FileExists (FileName) then begin
    FS:=TFileOfRecStreamExt.Create (Filename, fmOpenReadWrite OR fmShareDenyWrite,
                                    SizeOf (WMsgReg), isOK);
    try
      if isOK then begin
        { Eintrag für Rechnername und PortId suchen: }
        FRecCount:=FS.RecCount;
        while FS.RecPosition < FRecCount do begin
          Application.ProcessMessages;
          FS.ReadRec (WMsgReg);
          if (WMsgReg.Host = AHostname) AND (WMsgReg.Port = APortId) then begin
            // Eintrag gefunden: in MsgTypes als deaktiviert markieren
            WMsgReg.MsgTypes:=WMsgReg.MsgTypes OR integer (C_WMsgType_Deaktiviert);
            // Eintrag überschreiben:
            FS.RecPosition:=FS.RecPosition - 1;
            FS.WriteRec (WMsgReg);
            // Kein Break. Sicherheitshalber ganze Datei durchsuchen, falls mehrere
            // Einträge vorhanden sein sollten; 03.07.2009, WW
          end;
        end;
        // wenn Eintrag nicht gefunden wurde: auch OK, dann wurde er bereits endgültig gelöscht
        Result:=true;
      end;
    finally
      FS.Free;
    end;
  end;  { if FileExists }
end;

{ Löscht Einträge mit den übergebenen Typen aus der Registrierungsdatei      }
{ Parameter: Zu löschende Typen                                              }
{ Rückgabe: Anzahl der durchgeführten Löschungen                             }
{----------------------------------------------------------------------------}
function TWMessageReg.UnregisterMessageTypes(pWMsgTypes: TWMsgTypes): integer;
{----------------------------------------------------------------------------}
var
  sFilename : TFileName;
  pFS       : TFileOfRecStreamExt;
  bOK       : boolean;
  iRecCount : integer;
  pWMsgReg  : TWMsgReg;
  iType     : TWMsgType;
  iTemp     : integer;
begin
  Result := 0;
  sFilename := GetMsgRegFilename;
  if (FileExists(sFileName)) then begin
    pFS := TFileOfRecStreamExt.Create(sFilename,
      (fmOpenReadWrite OR fmShareDenyWrite), SizeOf(pWMsgReg), bOK);
    try
      if (bOK) then begin
        { Eintrag für Rechnername und PortId suchen: }
        iRecCount := pFS.RecCount;
        while (pFS.RecPosition < iRecCount) do begin
          Application.ProcessMessages;
          pFS.ReadRec(pWMsgReg);
          iTemp := pWMsgReg.MsgTypes;
          for iType := Low(TWMsgType) to High(TWMsgType) do
            if (iType in pWMsgTypes) then
              iTemp := iTemp and (not (1 shl Integer(iType)));
          if (iTemp = 0) then begin  // Kein verbliebener Typ => löschen
            while (pFS.RecPosition < iRecCount) do begin
              Application.ProcessMessages;
              pFS.ReadRec(pWMsgReg);
              pFS.RecPosition := pFS.RecPosition - 2;
              pFS.WriteRec(pWMsgReg);
              pFS.RecPosition := pFS.RecPosition + 1;
            end;
            Dec(iRecCount);  { File um einen Eintrag kürzen }
            pFS.RecCount := iRecCount;
            Inc(Result);
          end
          else if (iTemp <> pWMsgReg.MsgTypes) then begin  // Änderung des Typs => ersetzen
            pWMsgReg.MsgTypes := iTemp;
            pFS.RecPosition := pFS.RecPosition - 1;
            pFS.WriteRec(pWMsgReg);
            Inc(Result);
          end;
        end;
      end;
    finally
      pFS.Free;
    end;
  end;  { if FileExists }
end;

{-------------------------------------------------------------------------------------------------}
function TWMessageReg.GetMsgRegistrationList (AWMsgType: TWMsgType; RegList: TObjectList;
                                              AllWMsgTypes: boolean = false): boolean;
{-------------------------------------------------------------------------------------------------}
{ liefert Liste mit allen Registrierungsdatei-Einträgen, welche den übergebenen
  Nachrichtentyp enthalten;
  Übergabe: Nachrichtentyp
            Flag 'alle Nachrichtentypen'
  Rückgabe: Liste (Struktur: TWMsgRegObj)
  Ergebnis: true, wenn Zugriff auf Nachrichten-Registrierungsdatei OK }
var
  Filename: string;
  FS: TFileOfRecStreamExt;
  isOK: boolean;
  FRecCount: integer;
  WMsgReg: TWMsgReg;
  WMsgRegObj: TWMsgRegObj;

begin
  Result:=true;
  if RegList = nil then exit;
  RegList.Clear;  // Liste vorher leeren; 03.08.2006 WW

  Filename:=GetMsgRegFilename;
  if FileExists (FileName) then begin
    FS:=TFileOfRecStreamExt.Create (Filename, fmOpenRead OR fmShareDenyWrite,
                                    SizeOf (WMsgReg), isOK);
    try
      if isOK then begin
        { alle Einträge mit übergebenem Nachrichtentyp suchen: }
        FRecCount:=FS.RecCount;
        while FS.RecPosition < FRecCount do begin
          Application.ProcessMessages;
          FS.ReadRec (WMsgReg);

          // nur, wenn Eintrag nicht deaktiviert ist; 07.09.2006, WW
          if (((WMsgReg.MsgTypes AND EncodeMsgType ([AWMsgType])) <> 0) AND
              ((WMsgReg.MsgTypes AND C_WMsgType_Deaktiviert) = 0)) OR
             AllWMsgTypes then begin
            { Listenobjekt createn und in Liste einfügen: }
            WMsgRegObj:=TWMsgRegObj.Create;
            WMsgRegObj.SetData (WMsgReg);
            RegList.Add (WMsgRegObj);
          end;
        end;
      end else
        Result:=false;
    finally
      FS.Free;
    end;
  end;  { if FileExists }
end;

{------------------------------------------------------------------}
function TWMessageReg.CheckRegistration (APortId: integer): boolean;
{------------------------------------------------------------------}
{ Prüft, ob Port-Nummer in Nachrichten-Registrierungsdatei eingetragen ist;
  Übergabe: IPPort-Nummer
  Ergebnis: true, wenn Port-Nummer registriert ist }
var
  Filename: string;
  FS: TFileOfRecStreamExt;
  isOK: boolean;
  FRecCount: integer;
  WMsgReg: TWMsgReg;
  sLocalMachine: string;

begin
  Result:=false;
  Filename:=GetMsgRegFilename;
  if not FileExists (FileName) then exit;  { Registrierungsdatei fehlt, dann auch kein Registrierung }

  FS:=TFileOfRecStreamExt.Create (Filename, fmOpenRead OR fmShareDenyWrite,
                                  SizeOf (WMsgReg), isOK);
  try
    if isOK then begin
      { lokalen Rechnernamen ermitteln: }
      sLocalMachine:=GetLocalComputername;

      { Eintrag für Rechnernamen und PortId suchen: }
      FRecCount:=FS.RecCount;
      while FS.RecPosition < FRecCount do begin
        Application.ProcessMessages;
        FS.ReadRec (WMsgReg);
        // nur, wenn Eintrag nicht deaktiviert ist; 07.09.2006, WW
        if (WMsgReg.Host = sLocalMachine) AND (WMsgReg.Port = APortId) AND
           ((WMsgReg.MsgTypes AND C_WMsgType_Deaktiviert) = 0) then begin
          Result:=true;
          Break;
        end;
      end;
    end;
  finally
    FS.Free;
  end;
end;

{------------------------------------------------------------------}
function TWMessageReg.VerifyMessaging(iPortId: integer): boolean;
{------------------------------------------------------------------}
var
  Filename: string;
  FS: TFileOfRecStreamExt;
  isOK: boolean;
  FRecCount: integer;
  WMsgReg: TWMsgReg;
  sLocalMachine: string;
begin
  try
    Result := False;
    Filename := GetMsgRegFilename;
    if (not FileExists (FileName)) then exit;  { Registrierungsdatei fehlt, dann auch kein Registrierung }

    FS:=TFileOfRecStreamExt.Create (Filename, fmOpenReadWrite OR fmShareDenyWrite,
                                    SizeOf (WMsgReg), isOK);
    try
      if isOK then begin
        { lokalen Rechnernamen ermitteln: }
        sLocalMachine:=GetLocalComputername;

        { Eintrag für Rechnernamen und PortId suchen: }
        FRecCount:=FS.RecCount;
        while (FS.RecPosition < FRecCount) do begin
          Application.ProcessMessages;
          FS.ReadRec (WMsgReg);
          if (WMsgReg.Host = sLocalMachine) AND (WMsgReg.Port = iPortId) then
          begin
            // Eintrag gefunden: Wert deaktiviert ?
            if ((WMsgReg.MsgTypes AND C_WMsgType_Deaktiviert) > 0) then begin
              WMsgReg.MsgTypes :=
                WMsgReg.MsgTypes AND integer (C_WMsgType_Aktivieren);
              // Eintrag überschreiben:
              FS.RecPosition:=FS.RecPosition - 1;
              FS.WriteRec (WMsgReg);
            end;
            Result := True;
            Break;
          end;
        end;
      end;
    finally
      FS.Free;
    end;
  except
    Result := False;
  end;
end;

{---------- Routinen zum An- und Abmelden im Benachrichtigungssystem ----------}

{-----------------------------------------------------------------------------}
function RegisterWieserMessaging (ANetProgPath: string; AWMsgTypes: TWMsgTypes;
                                  var PortId: integer): boolean;
{-----------------------------------------------------------------------------}
{ Meldet Applikation in der Nachrichten-Registrierungsdatei an und gibt eine freie
  IPPort-Nummer zurück, über die die Benachrichtigung erfolgt;
  Übergabe: Netz-Programmpfad (Pfad der Nachrichten-Registrierungsdatei)
            Nachrichtentypen
  Rückgabe: IPPort-Nummer
  Ergebnis: true, wenn Anmeldung erfolgreich }
var
  WMessageReg: TWMessageReg;
begin
  WMessageReg:=TWMessageReg.Create (ANetProgPath);
  try
    Result:=WMessageReg.RegisterMessaging (AWMsgTypes, PortId);
  finally
    WMessageReg.Free;
  end;
end;

{----------------------------------------------------------------------------------}
function UnregisterWieserMessaging (ANetProgPath: string; PortId: integer): boolean;
{----------------------------------------------------------------------------------}
{ Meldet die unter der übergebenen IPPort-Nummer angemeldete, laufende Applikation
  aus der Nachrichten-Registrierungsdatei ab;
  Übergabe: Netz-Programmpfad (Pfad der Nachrichten-Registrierungsdatei)
            IPPort-Nummer
  Ergebnis: true, wenn Abmeldung erfolgreich }
var
  WMessageReg: TWMessageReg;
  sLocalMachine: string;

begin
  WMessageReg:=TWMessageReg.Create (ANetProgPath);
  try
    sLocalMachine:=WMessageReg.GetLocalComputername;     { lokalen Rechnernamen ermitteln }
    Result:=WMessageReg.UnregisterMessaging (sLocalMachine, PortId);
  finally
    WMessageReg.Free;
  end;
end;

{--------------------------------------------------------------------}
function CheckRegistrationWieserMessaging (ANetProgPath: string;
                                           APortId: integer): boolean;
{--------------------------------------------------------------------}
{ Prüft, ob Port-Nummer in Nachrichten-Registrierungsdatei eingetragen ist;
  Übergabe: Netz-Programmpfad (Pfad der Nachrichten-Registrierungsdatei)
            IPPort-Nummer
  Ergebnis: true, wenn Port-Nummer registriert ist }
var
  WMessageReg: TWMessageReg;
begin
  WMessageReg:=TWMessageReg.Create (ANetProgPath);
  try
    Result:=WMessageReg.CheckRegistration (APortId);
  finally
    WMessageReg.Free;
  end;
end;

{--------------------------------------------------------------------}
function VerifyWieserMessaging(ANetProgPath: string; APortId: integer): boolean;
{--------------------------------------------------------------------}
{ Prüft, ob Port-Nummer in Nachrichten-Registrierungsdatei eingetragen ist;
  Übergabe: Netz-Programmpfad (Pfad der Nachrichten-Registrierungsdatei)
            IPPort-Nummer
  Ergebnis: true, wenn Port-Nummer registriert ist }
var
  WMessageReg: TWMessageReg;
begin
  WMessageReg:=TWMessageReg.Create (ANetProgPath);
  try
    Result := WMessageReg.VerifyMessaging(APortId);
  finally
    WMessageReg.Free;
  end;
end;

end.

