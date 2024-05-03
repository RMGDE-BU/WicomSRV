{------------------------------------------------------------------------------}
{ Objekt für DSfG-Abrufe via DPA2200 (lokale Anbindung)                        }
{                                                                              }
{ 18.02.2003  GD  Neu                                                          }
{ 25.03.2004  GD  Anpassung an geänderten Befehlssatz                          }
{ 03.12.2004  GD  Anpassung an Quittung für gesendete Telegramme               }
{ 13.01.2005  GD  Änderung der Quittungsabfrage für gesendete Telegramme       }
{ 14.07.2005  GD  Änderung des Opened-Status, Fehlerbeseitigung bei SetAdresse }
{ 19.10.2005  GD  Erweitert um "FlashDpaSoftware" 	                   }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003, 2005                                    }
{------------------------------------------------------------------------------}
unit DDpaConnect;

interface

uses
  Windows, Forms, SysUtils, Classes, Serial, ExtCtrls, Dialogs, Controls,
  DConnect, T_Zeit, GD_Utils, DSG_Utils, DMomLists, DListen, WSysCon, COM_Utils,
  DArchivDll, DStaDll;

type
  TDSfGDPAConnection = class(TCustomDSfGConnection)
  private
    FSerial        : TSerial;
    FComPort       : byte;
    FBusy          : boolean;
    FEmpfang       : string;
    FComBaudRate   : integer;
    FDSfGBaudRate  : integer;
    FNotify        : TNotifyString;
    FTerminated    : boolean;
    FFlashAdresse  : boolean;
    FMonitorMode   : boolean;
    FRundsendeMode : boolean;
    FMonitorTimer  : TTimer;
    FMonitorFile   : TFileStream;
    FSendTeleView  : TSetCaptionEvent;
    FReceiveTeleView : TSetCaptionEvent;
    FParametrierung : boolean;
    FAbrufInstId   : integer;
    FAbrufArchGrp  : integer;
    FAbrufLogbNr   : integer;
    FTeilnehmer    : string;  // Speichert zuletzt abgerufene Teilnehmerliste intern
    FAbruf         : boolean;
    FSendResult    : boolean;
    FLogFile       : TFileName;
    function GetDaten: string;
    procedure SetDaten(sDaten: string);
    function SendReceiveCommand(sCommand: string;
      iTimeOut: Cardinal = 10000; cEndeChar: char = Chr(cr)): string;
    function ReceiveCommand(
      iTimeOut: Cardinal = 10000; cEndeChar: char = Chr(cr)): string;
    procedure SetComPort(iPort: byte);
    function GetComBaudRate: integer;
    procedure SetComBaudRate(iBaudRate: integer);
    procedure SetDSfGBaudRate(iBaudRate: integer);
    function GetDSfGBaudRate: integer;
    function GetAdresse: char;
    function GetMonitorMode: boolean;
    procedure SetMonitorMode(bValue: boolean);
    function GetRundsendeMode: boolean;
    procedure SetRundsendeMode(bValue: boolean);
    function GetFirmwareVersion: string;
    function GetLastRundsendeTelegramm: string;
    procedure MonitorTimerTimer(Sender: TObject);
    function GetZeitangaben: TZeitangaben;
    function GetArchivByONummer(cInstAdr: char; iAGNr, iAKNr: byte;
      iONrVon, iONrBis: integer): string;
    function GetArchivByTime(cInstAdr: char; iAGNr, iAKNr: byte;
      dtVon, dtBis: TDateTime): string;
    function GetLogbuchByONummer(cInstAdr, cQuellAdresse: char;
      iONrVon, iONrBis: integer): string;
    function GetLogbuchByTime(cInstAdr, cQuellAdresse: char;
      dtVon, dtBis: TDateTime): string;
    function CheckArchivTelegramm(sTelegramm: string; iOVon: integer;
      dtVon: TDateTime): boolean;
  protected
    procedure InitComponents(aState: boolean); override;
    procedure SetAdresse(Value: char); override;
  public
    function FlashDpaSoftware(sHexFile: TFileName): boolean;  // 19.10.2005
    procedure SetAdresseNULL;
    function HasData: boolean;
    procedure SendTelegramm(sTelegramm: string); override;
    function ReceiveTelegramm: string; override;
    function SendReceiveTelegramm(sTelegramm: string;
      bFolgetelegramme: boolean = True): string; override;
    function StartConnectionTime: integer; override;
    procedure InitConnection(aState: boolean; iId: integer = -1); override;
    procedure InitParameter(aState: boolean); override;
    function IsTeilnehmer: Boolean; override;
    function GetTeilnehmer: string; override;
    function InitAutoDatenAbruf(iStaId: integer): boolean; override;
    function InitManuDatenAbruf(iStaId: integer; pAbrufListe: TArchivAbrufList;
      dtVon, dtBis: TDateTime): boolean; override;
    function ReadParameter(cIAdr: char; sDEAVon: string;
      sDEABis: string = ''): string; override;
    function WriteParameter(cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string;
      iType: byte = 0): string; virtual;
    function ReadStammdaten(iState: byte): TStrings; override;
    property Busy: boolean read FBusy;
    property ComPort: byte read FComPort write SetComPort;
    property ComBaudRate: integer read GetComBaudRate write SetComBaudRate;
    property DSfGBaudRate: integer read FDSfGBaudRate write SetDSfGBaudRate;
    property NotifyString: TNotifyString write FNotify;
    property Terminated: boolean read FTerminated write FTerminated;
    property MonitorMode: boolean read GetMonitorMode write SetMonitorMode;
    property RundsendeMode: boolean
      read GetRundsendeMode write SetRundsendeMode;
    property Daten: string read GetDaten write SetDaten;
    property FirmwareVersion: string read GetFirmwareVersion;
    property LastRundsendeTelegramm: string read GetLastRundsendeTelegramm;
    property MonitorFile: TFileStream write FMonitorFile;
    property SendTeleView: TSetCaptionEvent write FSendTeleView;
    property ReceiveTeleView: TSetCaptionEvent write FReceiveTeleView;
    property AbrufInstId: integer write FAbrufInstId;
    property AbrufArchGrp: integer write FAbrufArchGrp;
    property AbrufLogbNr: integer write FAbrufLogbNr;
    property AbrufAktiv: boolean read FAbruf write FAbruf;
    property IsParametrierung: boolean
      read FParametrierung write FParametrierung;
    property LogFile: TFileName write FLogFile;
  end;

  TDSfGDPAMomConnection = class(TDSfGDPAConnection)
  private
    FMomList     : TDSfGMomDefList;
    FNewDataList : TStrings;
    FMomCallBack : TNeueMomentanwerteCB;
    FTempTimer   : TTimer;
    FNewData     : boolean;
    FEnableTempTimer : boolean;
    procedure HoleNeueDaten;
  protected
    procedure InitComponents(aState: boolean); override;
    function GetNewData: boolean; virtual;
  public
    procedure TempTimerTimer(Sender: TObject);
    procedure InsertMomData(cIAdr: char; sDEAVon, sDEABis: string);
    procedure DeleteMomData(cIAdr: char; sDEAVon, sDEABis: string);
    function GetAnzeigeDaten: TDSfGMomValueList; virtual;
    procedure CallMomData;
    procedure ClearMomData;
    property MomList: TDSfGMomDefList read FMomList;
    property NewData: boolean read GetNewData;
    property NeueMomentanwerteCB: TNeueMomentanwerteCB
      read FMomCallBack write FMomCallBack;
    property EnableTempTimer: boolean write FEnableTempTimer;
  end;

implementation

resourcestring
  S_Com_Error = 'Schnittstelle/Modem nicht verfügbar !';
  S_Generalpolling = 'Bitte Generalpolling auslösen/abwarten !';
  S_ReadCommonKonfig = 'Hole Typenschildinformationen von Instanz %s';
  S_ReadArchiveKonfig = 'Hole Archivinformationen von Instanz %s';

const
  C_BufSize         = 8192;
  C_MaxBlockLaenge  = 256;
  C_MonitorInterval = 100;

  C_TO_ACK          = 2000;  // Timeout für Quittierung
  C_Wdh_OnNAK       = 10;    // Wiederholungen im Fall von NAK

{---------------------------- TDSfGDPAConnection ------------------------------}

{---------------------------------------------------------}
procedure TDSfGDPAConnection.InitComponents(aState: boolean);
{---------------------------------------------------------}
begin
  if (aState) then begin
    FNotify := nil;           // Callback für Anzeige
    FEmpfang := '';           // Objekt - Globaler Empfangspuffer
    FBusy := False;           // Flag, ob aktuell Auftrag bearbeitet wird
    FComPort := 0;            // ComPort inaktiv
    FComBaudRate := 9600;     // Baudrate der Schnittstelle
    FDSfGBaudRate := 9600;    // Baudrate des DSfG-Busses
    FTerminated := False;     // Flag, ob Programm beendet wird;
    FFlashAdresse := True;    // Flag, ob Adresse auch in DPA geflashed wird
    FMonitorMode := False;    // Monitormodus ist aus
    FMonitorFile := nil;      // Datei für Speicherung des Monitordatenverkehrs
    FSendTeleView  := nil;    // Callback für Sendeanzeige
    FReceiveTeleView := nil;  // Callback für Empfangsanzeige
    FTeilnehmer := '';  // Speichert zuletzt abgerufene Teilnehmerliste intern
    FAbrufInstId := 0;        // Ggf. InstanzId für selektiven Datenabruf
    FAbrufArchGrp := 0;       // Ggf. ArchGrp für selektiven Datenabruf
    FAbrufLogbNr := 0;        // Ggf. Logbuch für selektiven Datenabruf
    FAbruf := False;          // Flag, ob gerade ein Abruf aktiv ist
    FSendResult := False;     // Letzte Sendequittierung
    FLogFile := '';           // Logfile für Datenmitschnitt

    FMonitorTimer := TTimer.Create(nil);  // Timer für Monitordaten
    FMonitorTimer.Enabled := False;
    FMonitorTimer.Interval := C_MonitorInterval;
    FMonitorTimer.OnTimer := MonitorTimerTimer;

    FRundsendeMode := False;  // Rundsendemodus ist aus
    Adresse := '_';           // Dummy für "Start" (DPA kann nicht Master sein)
    Opened := False;
    TimeOut := 30000;
    if (not Assigned(FSerial)) then FSerial := TSerial.Create(nil);
    FSerial.DataBits := db_7;
    FSerial.StopBits := sb_1;
    FSerial.ParityBit := even;
  end
  else begin
    if (Assigned(FSerial)) then begin
      Opened := False;
      FSerial.Active := False;
      FreeAndNil(FSerial);
      FreeAndNil(FMonitorTimer);
      FBusy := False;           // Flag, ob aktuell Auftrag bearbeitet wird
    end;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.GetAdresse: char;
{---------------------------------------------------------}
var
  s : string;
begin
  s := SendReceiveCommand(Chr(esc) + 'A' + Chr(cr));
  if (Length(s) > 3) then Result := s[3] else Result := '0';
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.GetDSfGBaudRate: integer;
{---------------------------------------------------------}
var
  s : string;
begin
  s := SendReceiveCommand(Chr(esc) + 'D' + Chr(cr));
  if (Length(s) >= 9)
  then Result := StrToIntDef(Copy(s, 3, 6), 0)
  else Result := 0;
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.SetAdresseNULL;
{---------------------------------------------------------}
var
  s      : string;
begin
  if (Assigned(FSerial)) and (FSerial.Active) then begin

    s := SendReceiveCommand(Chr(esc) + 'A0' + Chr(cr));
    Delay(10);
    if (Length(s) >= 3) and (s[2] = 'A') then begin
      FFlashAdresse := False;  // Nur eintragen
      Adresse := s[3];
    end;
    FFlashAdresse := True;  // Im Normalfall wird geflashed !
  end;
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.SetAdresse(Value: char);
{---------------------------------------------------------}
var
  i      : byte;
  s      : string;
begin
  try
    if (Assigned(FSerial)) and (FSerial.Active) and
       ((Value <> Adresse) or (Value = '0')) and (Value in ['0', 'A'..'_']) then
    begin

      if (FFlashAdresse) and (Value <> '_') then begin
        if (Value = '0') then begin
          s := GetTeilnehmer;
          for i := 1 to Length(s) do
            if (s[i] = '.') then begin
              Value := Chr(Ord('A') - 1 + i);
              Break;
            end;
        end;

        s := SendReceiveCommand(Chr(esc) + 'A' + Value + Chr(cr));
        Delay(10);
        if (Length(s) >= 3) and (s[2] = 'A') then inherited SetAdresse(s[3]);
      end
      else inherited;
    end
    else if (Value = '_') then inherited;   // Basiseinstellung
  finally
    FFlashAdresse := True;
  end;
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.SetComPort(iPort: byte);
{---------------------------------------------------------}
begin
  if (iPort <> FComPort) then begin
    FComPort := iPort;
    FSerial.Active := False;
    FSerial.COMPort := iPort;
    if (Opened) then begin
      FSerial.Active := True;
      FFlashAdresse := False;  // Adresse eintragen, aber nicht schreiben
      Adresse := GetAdresse;
      Opened := FSerial.Active;
    end;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.GetComBaudRate: integer;
{---------------------------------------------------------}
const
  CMyBaudRates : array [0..6] of integer =
    (0, 9600, 14400, 19200, 38400, 57600, 115200);
var
  sAnswer : string;
  bOpened : boolean;
  i       : integer;
begin
  // Ggf. Monitormodus beenden
  if (FSerial.Active) then begin
    SetDaten(chr(esc) + 'M0' + chr(cr));
    Delay(100);
  end;

  Result := 0;  // Default: Nix aktiv
  bOpened := FSerial.Active;
  try
    CMyBaudRates[0] := FComBaudRate;  // Zunächst mit der eingestellten ...
    for i := 0 to 6 do begin
      FSerial.Active := False;
      FSerial.Baudrate := FSerial.KonvertBaudrate(CMyBaudRates[i]);
      FSerial.Active := True;
      Delay(100);
      SetDaten(Chr(esc) + 'M0' + Chr(cr));
      Delay(100);
      sAnswer := GetDaten;
      if (Pos(Chr(esc), sAnswer) > 0) and ((Pos(Chr(cr), sAnswer) > 0) ) then
      begin
        SetDaten(Chr(esc) + 'P' + Chr(cr));
        Delay(100);
        sAnswer := GetDaten;
        if (Pos(Chr(esc), sAnswer) > 0) and ((Pos(Chr(cr), sAnswer) > 0) ) then
        begin
          Result := StrToIntDef(Copy(sAnswer, 3, 6), 0);
          if (Result <> CMyBaudRates[i]) then Result := 0;
          FComBaudRate := Result;
          Break;
        end;
      end;
    end;
  finally
    FSerial.Active := bOpened;
  end;
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.SetComBaudRate(iBaudRate: integer);
{---------------------------------------------------------}
var
  s : string;
begin
  if (iBaudRate <> FComBaudRate) then begin
    FComBaudRate := iBaudRate;
    if (FSerial.Active) then begin
      SetDaten(chr(esc) + 'M0' + chr(cr));
      Delay(100);

      s := SendReceiveCommand(
        Chr(esc) + 'P' + Format('%.6d', [iBaudRate]) + Chr(cr));
      Delay(10);
      if (Length(s) >= 8) and (s[2] = 'P') then
        FComBaudRate := StrToInt(Copy(s, 3, 6));

      FSerial.Active := False;
      FSerial.Baudrate := FSerial.KonvertBaudrate(FComBaudRate);
      FSerial.Active := True;
      Delay(100);
    end;
  end;
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.SetDSfGBaudRate(iBaudRate: integer);
{---------------------------------------------------------}
var
  s : string;
begin
  if (iBaudRate <> FDSfGBaudRate) then begin
    FDSfGBaudRate := iBaudRate;
    if (FSerial.Active) then begin
      SetDaten(chr(esc) + 'M0' + chr(cr));
      Delay(100);
      if (FComBaudRate < FDSfGBaudRate) then ComBaudRate := FDSfGBaudRate;

      s := SendReceiveCommand(
        Chr(esc) + 'D' + Format('%.6d', [iBaudRate]) + Chr(cr));
      Delay(10);
      if (Length(s) >= 8) and (s[2] = 'D') then
        FDSfGBaudRate := StrToInt(Copy(s, 3, 6));
    end;
  end;
end;

{ Daten über serielle Schnittstelle senden                }
{ Parameter: string mit Daten                             }
{---------------------------------------------------------}
procedure TDSfGDPAConnection.SetDaten(sDaten: string);
{---------------------------------------------------------}
var
  p : PChar;
begin
  if (FSerial.Active) then begin
    if (Assigned(FNotify)) then FNotify(0, '');
    if (Assigned(FNotify)) then FNotify(1, CharstrToString(sDaten));
    if (Assigned(FNotify)) then FNotify(2, '');

    GetMem(p, Length(sDaten) + 1);
    try
      StrPCopy(p, sDaten);
      FSerial.TransmittData(p^, StrLen(p));
    finally
      FreeMem(p, Length(sDaten) + 1);
    end;
  end
  else if (Opened) then begin
    Opened := False;
    raise Exception.Create(S_Com_Error);
  end;
end;

{ Wurden neue Daten über serielle Schnittstelle empfangen?}
{ Rückgabe: ja/nein                                       }
{---------------------------------------------------------}
function TDSfGDPAConnection.HasData: boolean;
{---------------------------------------------------------}
begin
  Result := (FSerial.BufRec > 0);
end;

{ Daten über serielle Schnittstelle empfangen             }
{ Rückgabe: string mit Daten                              }
{---------------------------------------------------------}
function TDSfGDPAConnection.GetDaten: string;
{---------------------------------------------------------}
var
  p       : PChar;
  i, n, m : integer;
begin
  Result := '';

  if (FSerial.Active) then begin
    n := FSerial.BufRec;

    if (n > 0) then begin
      GetMem(p, C_BufSize + 1);
      try
        m := FSerial.ReceiveData(p^, n);
        for i := 0 to m-1 do Result := Result + p[i];
      finally
        FreeMem(p, C_BufSize + 1);
      end;
    end;

  end
  else if (Opened) then begin
    Opened := False;
    raise Exception.Create(S_Com_Error);
  end;
end;

{ Telegramm senden                                        }
{ Parameter: Telegramm                                    }
{---------------------------------------------------------}
procedure TDSfGDPAConnection.SendTelegramm(sTelegramm: string);
{---------------------------------------------------------}
var
  s, sHeader   : string;
  i, j, i1, i2 : integer;
begin
  FSendResult := False;
if (FLogFile <> '') then StringToFile(DateTimeToStr(Now) + ': Sendeauftrag >' +
  CharstrToString(sTelegramm) + '<', FLogFile, False);
  if (not Opened) or (FBusy) then Exit;
  FBusy := True;
  try
    Delay(10);
    sTelegramm := sTelegramm + GetBCC_Chars (GetBCC (0, sTelegramm));
    if (Length(sTelegramm) <= C_MaxBlockLaenge) then begin
      SetDaten(sTelegramm);

if (FLogFile <> '') then
  StringToFile('  ' + TimeToStr(Now) + ' Daten gesendet', FLogFile, False);
      // Prüfung der Quittierung
      s := ReceiveCommand(C_TO_ACK);
if (FLogFile <> '') then StringToFile('  ' + TimeToStr(Now) + ' Quittierung >' +
  CharstrToString(s) + '<', FLogFile, False);
      if (Pos('S1', s) = 0) then begin
        if (Pos('S2_NAK', s) > 0) then begin  // Ggf. Wiederholungen
          for i := 1 to C_Wdh_OnNAK do begin
            SetDaten(sTelegramm);
            s := ReceiveCommand(C_TO_ACK);
            if (Pos('S1', s) > 0) then Break;
          end;
          if (Pos('S1', s) = 0) then Exit;
        end
        else Exit;
      end;

      if (Assigned(FSendTeleView)) then begin
        FSendTeleView(sTelegramm);
        FReceiveTeleView('');
      end;
    end
    else begin     // Blockung beherrscht der DPA z.Zt noch nicht - trotzdem ...
      // BNO gegen xx, BNR gegen yy austauschen
      j := 0;  // Anzahl us
      s := '';
      i1 := 0;
      i2 := 0;
      for i := 1 to Length(sTelegramm) do
        if (sTelegramm[i] = Chr(us)) then begin
          Inc(j);
          if (j = 3) then i1 := i
          else if (j = 5) then begin
            i2 := i;
            Break;
          end;
        end;
      s := Copy(sTelegramm, 1, i1) + 'xx' + Chr(us) + 'yy' +
        Copy(sTelegramm, i2, Length(sTelegramm));
      // Header für Folgetelegrammblöcke
      sHeader := Chr(stx) + DSG_Utils.GetDSfGInstAdr(s) + Chr(us) + '15' +
        Chr(us) + GetDSfGDatenelement(s, 2) + Chr(us) + 'xx' + Chr(us) + 'yy' +
        Chr(us) + GetDSfGAbsAdr(s) + Chr(us);

      with TStringList.Create do
      try
        Add(Copy(s, 1, C_MaxBlockLaenge-1) + Chr(etb));
        System.Delete(s, 1, C_MaxBlockLaenge-1);
        while ((Length(s) + Length(sHeader)) > C_MaxBlockLaenge) do begin
          Add(sHeader + Copy(s, 1, C_MaxBlockLaenge-Length(sHeader)-1) + Chr(etb));
          System.Delete(s, 1, C_MaxBlockLaenge-Length(sHeader)-1);
        end;
        if (s <> '') then Add(sHeader + s);

        for i := 0 to Count-1 do begin
          s := StringReplace(Strings[i], 'xx', IntToStr(Count), []);  // BLO
          s := StringReplace(s, 'yy', IntToStr(i+1), []);             // BNR
          s := s + GetBCC_Chars(GetBCC (0, s));                       // BCC
          SetDaten(s);
          if (Pos('S1', ReceiveCommand(C_TO_ACK)) = 0) then Exit;
          if (Assigned(FSendTeleView)) then FSendTeleView(s);
          Delay(10);
        end;
        if (Count > 0) and (Assigned(FSendTeleView)) then FSendTeleView('');
      finally
        Free;
      end;
    end;

    Delay(100);
    FSendResult := True;
  finally
    FBusy := False;
  end;
end;

{ Antwort-(Teil-)Telegramm empfangen                      }
{ Rückgabe: string mit Daten                              }
{---------------------------------------------------------}
function TDSfGDPAConnection.ReceiveTelegramm: string;
{---------------------------------------------------------}
const
  bWaitETX : boolean = True;
var
  iStop : Cardinal;
  s     : string;
  i     : integer;
  bTele : boolean;
  iBLO, iBNR : integer;
  cAdresse   : char;
begin
  Result := '';
  if (not Opened) then Exit;

  s := '';
  bTele := False;
  iStop := GetTickCount + Cardinal(TimeOut);
  while (not FTerminated) and (GetTickCount <= iStop) do begin

    s := GetDaten;
if (FLogFile <> '') and (s <> '') then StringToFile(DateTimeToStr(Now) +
  ': Empfange >' + CharstrToString(s) + '<', FLogFile, False);
    FEmpfang := FEmpfang + s;

    if (Assigned(FNotify)) then
      FNotify(0, IntToStr(((iStop-GetTickCount) div 1000)) + ' s');
    if (Assigned(FNotify)) then FNotify(2, CharstrToString(FEmpfang));

    if (not bTele) then begin
      i := Pos(Chr(stx), FEmpfang);  // Prüfung auf STX
      if (i > 0) then begin
        bTele := True;
        System.Delete(FEmpfang, 1, i-1);
      end;
    end;

    if (bTele) then begin
      i := Pos(Chr(etb), FEmpfang);  // Prüfung auf ETB
      if (i = 0) then i := Pos(Chr(etx), FEmpfang);  // Prüfung auf ETX
      if (i > 0) then begin
        Result := Result + Copy(FEmpfang, 1, i);
        System.Delete(FEmpfang, 1, i);
        if (Length(Result) > 5) then begin
          // Prüfen, ob ETB BCC-Zeichen hinter ETX ist
          if (Pos(Chr(etb), Result) > 0) and (Pos(Chr(etx), Result) > 0) and
             (Pos(Chr(etb), Result) > Pos(Chr(etx), Result))
          then System.Delete(Result, Pos(Chr(etx), Result)+1, 2);
          if (Assigned(FReceiveTeleView)) then FReceiveTeleView(Result);
          Delay(10);
          Break;
        end
        else begin  // ... war irgend ein Steuerzeichen im BCC
          Result := '';
          bTele := False;
        end;
      end
      else begin
        Result := Result + FEmpfang;
        FEmpfang := '';
      end;
    end;

    if (Result <> '') and (Result[Length(Result)] in [Chr(etx), Chr(etb)]) then
    begin
      if (Assigned(FReceiveTeleView)) then FReceiveTeleView(Result);
      Delay(10);
      Break;
    end;

    Delay(1);
  end;

  if (Assigned(FNotify)) then FNotify(0, '');

  // Auf Blockung untersuchen und ggf. Folgeblöcke holen
  if (bWaitETX) and (Result <> '') and (Result[Length(Result)] = Chr(etb)) then
  try

    bWaitETX := False;
    cAdresse := GetDSfGAbsAdr(Result);  // Wer soll antworten ?
    with TStringList.Create do
    try

      iBLO := StrToIntDef(GetDSfGDatenelement(Result, 3), 0);
      iBNR := StrToIntDef(GetDSfGDatenelement(Result, 4), 0);
      Add(Result);
      Result := '';
      iStop := GetTickCount + Cardinal(TimeOut);

      while (not FTerminated) and (iBNR < iBLO) and (GetTickCount <= iStop) do
      begin

        s := ReceiveTelegramm;  // Telegramm-Antwort holen
        if (s <> '') then begin
          if (s[1] = Chr(stx)) and (GetDSfGAbsAdr(s) = cAdresse) then  // vollständig/richtiger Absender
          begin
            Add(s);
            // Prüfung auf Blockung
            iBLO := StrToIntDef(GetDSfGDatenelement(s, 3), 0);
            iBNR := StrToIntDef(GetDSfGDatenelement(s, 4), 0);

            if (iBLO = 0) or (iBNR = 0) or (iBNR <> Count)
            then Exit
            else iStop := GetTickCount + Cardinal(TimeOut);
          end;
        end
        else Exit;

      end;

      if (Count = iBLO) then begin
        Result := GetHDCL(Strings[0]);          // Telegramm-Header
        for i := 0 to Count-1 do Result := Result + GetDatenTeil(Strings[i]);

        // Telegramm abschliessen
//        Result[Length(Result)] := Chr(fs);
        Result := Result + Chr(fs) + Chr(etx)
      end;

    finally
      if (Assigned(FNotify)) then FNotify(0, '');
      Free;
    end;

  finally
if (FLogFile <> '') then StringToFile(DateTimeToStr(Now) + ': Empfangsergebnis >' +
  CharstrToString(Result) + '<', FLogFile, False);
    if (Result <> '') and (Assigned(FReceiveTeleView)) then
      FReceiveTeleView('');
    bWaitETX := True;
  end;
end;

{ Telegramm senden, Antwort empfangen                     }
{ Parameter: Telegramm                                    }
{ Rückgabe: string mit Daten                              }
{---------------------------------------------------------}
function TDSfGDPAConnection.SendReceiveTelegramm(sTelegramm: string;
  bFolgetelegramme: boolean = True): string;
{---------------------------------------------------------}
var
  s          : string;
  cAdresse   : char;
  iStop      : Cardinal;
  pSl        : TStrings;
  i, j       : integer;
begin
  Result := '';  // Default
  FEmpfang := '';  // Empfangspuffer löschen
  if (not Opened) or ((Busy) and (bFolgetelegramme)) then Exit;
  FBusy := True;
  try

    cAdresse := GetDSfGInstAdr(sTelegramm);  // Wer soll antworten ?

    FBusy := False;
    SendTelegramm(sTelegramm);
    FBusy := True;
    if (not FSendResult) then Exit;

    iStop := GetTickCount + Cardinal(TimeOut);
    s := ReceiveTelegramm;
    while (not FTerminated) and (s <> '') and (GetDSfGAbsAdr(s) <> cAdresse) and
          (GetTickCount <= iStop)
    do s := ReceiveTelegramm;

    if (s <> '') and (GetDSfGAbsAdr(s) = cAdresse) then Result := s;

    s := Result;
    if (bFolgetelegramme) and (
        (GetDSfGDEB(sTelegramm) = 'V') or (GetDSfGDEB(sTelegramm) = 'O')  or
        (GetDSfGDEB(sTelegramm) = 'Z'))
    then

      while (GetDSfGNTY(s) = 'U') do begin

        if (not Opened) then Exit;


    // Bei Bereichsabfragen
        if (GetDSfGDEB(sTelegramm) = 'V') then
          sTelegramm := ModifyDSfGParamTelegramm(sTelegramm, s)
      // Bei Ordnungsnummernabfragen
        else if (GetDSfGDEB(sTelegramm) = 'O') then
          sTelegramm := ModifyDSfGONrTelegramm(sTelegramm, s)
      // Bei Zeitbereichsabfragen
        else if (GetDSfGDEB(sTelegramm) = 'Z') then
          sTelegramm := ModifyDSfGZeitBereichsTelegramm(sTelegramm, s)
      // sonst abbrechen
        else sTelegramm := '';

        if (sTelegramm <> '') then begin
          s := SendReceiveTelegramm(sTelegramm, False);

          if (Length(s) > 8) then begin
      // Telegramme zusammensetzen
            // Datenteil bisheriges Ergebnis
            pSl := GetDatenListe(Result);
            try
              j := pSl.Count;
              Result := '';
              for i := 0 to pSl.Count-1 do Result := Result + pSl[i] + Chr(gs);
            finally
              pSl.Free;
            end;
            // Datenteil neu hinzugekommenes Ergebnis
            pSl := GetDatenListe(s);
            try
              j := j + pSl.Count;
              for i := 0 to pSl.Count-2 do Result := Result + pSl[i] + Chr(gs);
              Result := Result + pSl[pSl.Count-1] + Chr(fs) + Chr(etx);
            finally
              pSl.Free;
            end;
            // Deklarationsteil erstellen
            Result := IntToStr(j) + Chr(us) + Result;  // ZAE
            for i := 9 downto 1 do Result := GetStringPart(s, i) + Chr(us) + Result;
          end;
        end;
      end;

  finally
    FBusy := False;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.StartConnectionTime: integer;
{---------------------------------------------------------}
begin
  Result := 10000;
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.InitConnection(aState: boolean; iId: integer = -1);
{---------------------------------------------------------}
begin
  if (aState) then begin
    FSerial.Active := False;
    if (FComPort > 0) then begin
      FSerial.COMPort := FComPort;
      FSerial.Baudrate := FSerial.KonvertBaudrate(FComBaudRate);
      if (FSerial.OpenComm) then begin
        GetComBaudRate;
        MonitorMode := False;
        RundsendeMode := False;
        FDSfGBaudRate := GetDSfGBaudRate;
        FFlashAdresse := False;  // Adresse eintragen, aber nicht flashen
        Adresse := GetAdresse;
      end;
      Opened := FSerial.Active;
    end
    else Opened := False;
  end
  else begin
    Opened := False;
    MonitorMode := False;
    RundsendeMode := False;
    FSerial.Active := False;
    FBusy := False;
  end;
  FEmpfang := '';
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.InitParameter(aState: boolean);
{---------------------------------------------------------}
begin
  FParametrierung := aState;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.IsTeilnehmer: Boolean;
{---------------------------------------------------------}
// Am besten über Monitor-Mode
begin
  Result := False;
  if (not Opened) or (Busy) then Exit;
  FBusy := True;
  try
    Result := (Pos(Adresse, GetTeilnehmer) > 0);
  finally
    FBusy := False;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.GetTeilnehmer: string;
{---------------------------------------------------------}
const
  C_TN_TimeOut = 30000;
var
  c     : char;
  s     : string;
begin
  Result := '';
  for c := 'A' to '_' do Result := Result + '.';
  if (not Opened) or (Busy) then Exit;
  FBusy := True;
  try
    if (FSerial.Active) then begin
      SetDaten(Chr(esc) + 'T' + Chr(cr));
      Delay(1000);
      s := GetDaten;
      if (Pos(Chr(esc), s) > 0) and ((Pos(Chr(cr), s) > 0) )
      then Result := Copy(s, 3, 31);
      FTeilnehmer := Result;
    end
    else begin
      Opened := False;
      raise Exception.Create(S_Com_Error);
    end;
  finally
    FBusy := False;
  end;
end;

{ Gibt Zeitangaben für Station zurück     }
{ Rückgabe: Zeitangabenrecord             }
{-----------------------------------------}
function TDSfGDPAConnection.GetZeitangaben: TZeitangaben;
{-----------------------------------------}
var
  i        : integer;
  s, sWert : string;
  tzi      : TIME_ZONE_INFORMATION;
begin
  FillChar(Result, SizeOf(Result), 0);  // Default

  // Schleife über alle aktuellen Teilnehmer (setzt GetTeilnehmer vorraus)
  for i := 1 to Length(FTeilnehmer) do
    if (FTeilnehmer[i] in ['A'..'_']) then begin
      // Telegramm zusammenstellen und abrufen
      s := C_DEA_DatumZeit + chr(gs) + C_DEA_Zeitzone + chr(gs) +
        C_DEA_LetztVerstZz;
      s := GetMultiDEDSfGTelegramm(Adresse, FTeilnehmer[i], s, 3);
      s := SendReceiveTelegramm(s);
      // Antwortdaten auswerten
      sWert := GetWertFromTelegramm(s, C_DEA_Zeitzone);
      if (Trim(sWert) <> '') then begin
        Result.EAdr := FTeilnehmer[i];
        Result.Zeitzone := Trim(sWert[1]);
        Result.DatumZeit := UnixToDateTime(StrToIntDef(
          '$' + GetWertFromTelegramm(s, C_DEA_DatumZeit), 0));
        Result.LetztVerstZZ := UnixToDateTime(StrToIntDef(
          '$' + GetWertFromTelegramm(s, C_DEA_LetztVerstZz), 0));
        Result.vom_PC := False;

        Break;
      end;
    end;

  // Als Ersatz PC-Zeit und PC-Zeitzone, falls Gerät keine Zeitangaben liefert
  if Result.DatumZeit = 0 then begin
    Result.DatumZeit:=Now;
    if (GetTimeZoneInformation (tzi) = TIME_ZONE_ID_DAYLIGHT)
    then Result.Zeitzone := CMESZ
    else Result.Zeitzone := CMEZ;
    Result.vom_PC:=true;
  end;

end;

{ Archivkanal über Ordungsnummer abfragen }
{ Parameter: Instanzadresse, Gruppe, Kanal}
{            Ordnungsnummer von-bis       }
{ Rückgabe: Antworttelegramm              }
{-----------------------------------------}
function TDSfGDPAConnection.GetArchivByONummer(cInstAdr: char;
  iAGNr, iAKNr: byte; iONrVon, iONrBis: integer): string;
{-----------------------------------------}
var
  s : string;
begin
  s := GetDeaArchivkanal(iAGNr, iAKNr) + 'd';  // Archivdaten
  s := GetDatenabfrageONrTelegramm(Adresse, cInstAdr, s, iONrVon, iONrBis);
  Result := SendReceiveTelegramm(s);
end;

{ Archivkanal über Zeitbereich abfragen   }
{ Parameter: Instanzadresse, Gruppe, Kanal}
{            Zeitbereich von-bis          }
{ Rückgabe: Antworttelegramm              }
{-----------------------------------------}
function TDSfGDPAConnection.GetArchivByTime(cInstAdr: char; iAGNr, iAKNr: byte;
  dtVon, dtBis: TDateTime): string;
{-----------------------------------------}
var
  s : string;
begin
  s := GetDeaArchivkanal(iAGNr, iAKNr) + 'd';  // Archivdaten
  s := GetDatenabfrageZeitbereichTelegramm(Adresse, cInstAdr, s,
    IntToHex(DateTimeToUnix(dtVon), 8), IntToHex(DateTimeToUnix(dtBis), 8));
  Result := SendReceiveTelegramm(s);
end;

{ Archivkanal über Ordungsnummer abfragen }
{ Parameter: Instanzadresse, Gruppe, Kanal}
{            Ordnungsnummer von-bis       }
{ Rückgabe: Antworttelegramm              }
{-----------------------------------------}
function TDSfGDPAConnection.GetLogbuchByONummer(
  cInstAdr, cQuellAdresse: char; iONrVon, iONrBis: integer): string;
{-----------------------------------------}
var
  s : string;
begin
  s := GetDeaLogbuch(cQuellAdresse) + 'd';  // Archivdaten
  s := GetDatenabfrageONrTelegramm(Adresse, cInstAdr, s, iONrVon, iONrBis);
  Result := SendReceiveTelegramm(s);
end;

{ Archivkanal über Zeitbereich abfragen   }
{ Parameter: Instanzadresse, Gruppe, Kanal}
{            Zeitbereich von-bis          }
{ Rückgabe: Antworttelegramm              }
{-----------------------------------------}
function TDSfGDPAConnection.GetLogbuchByTime(
  cInstAdr, cQuellAdresse: char; dtVon, dtBis: TDateTime): string;
{-----------------------------------------}
var
  s : string;
begin
  s := GetDeaLogbuch(cQuellAdresse) + 'd';  // Archivdaten
  s := GetDatenabfrageZeitbereichTelegramm(Adresse, cInstAdr, s,
    IntToHex(DateTimeToUnix(dtVon), 8), IntToHex(DateTimeToUnix(dtBis), 8));
  Result := SendReceiveTelegramm(s);
end;

{ Prüft Telegramm formal und auf Daten    }
{ Parameter: Telegramm, OrdNr von, Zeit v.}
{ Rückgabe: OK ja/nein                    }
{-----------------------------------------}
function TDSfGDPAConnection.CheckArchivTelegramm(
  sTelegramm: string; iOVon: integer; dtVon: TDateTime): boolean;
{-----------------------------------------}
var
   i, iZAE     : integer;
   iONr        : integer;
   sDtVon      : string;
begin
  Result := False;

  // HDCL prüfen, Anzahl der Datenelemente ermitteln
  if (CheckDSfGHeader(sTelegramm, iZAE) <> 0) then Exit;

  // Wenn Ordnungsnummer oder Zeit = 0, dann Prüfung beenden
  if (iOVon = 0) or (dtVon = 0) then begin
    Result := True;
    Exit;
  end;

  sDtVon := IntToHex(DateTimeToUnix(dtVon), 8);  // DatumZeit in DSfG-Unix-String

  with GetDatenListe(sTelegramm) do
  try
    for i := 0 to Count-1 do begin
      iONr := StrToInt(GetStringPart(Strings[i], 4));
      if (iONr = iOVon) then begin
        Result := (sDtVon = GetStringPart(Strings[i], 3)); // Datumsvergleich
        Break;
      end;
    end;
  finally
    Free;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.InitAutoDatenAbruf(iStaId: integer): boolean;
{---------------------------------------------------------}
var
  pArchivAbrufListe    : TAbrufList;
  pLogbuchAbrufListe   : TAbrufList;
  s, sTeilnehmer       : string;
  pOrdNrList           : TStrings;
  i, iOVon, iOBis, iIx : integer;
  dtVon, dtBis         : TDateTime;
  pAbrufListObj        : TAbrufListObj;
  pArchivDatenZugriff  : TArchivDatenZugriff;
begin
  FAbruf := True;
  Result := False;

  // Ggf. aktuelle Verbindung beenden
  if (Opened) then begin
    InitConnection(False);
    Delay(3000);
  end;

  pArchivAbrufListe := TAbrufList.Create;
  pLogbuchAbrufListe := TAbrufList.Create;
  pArchivDatenZugriff := TArchivDatenZugriff.Create;
  pOrdNrList := TStringList.Create;
  try
    // Verbindung zur angeschlossenen Station herstellen
    InitConnection(True);
    sTeilnehmer := StringReplace(GetTeilnehmer, '.', '', [rfReplaceAll]);
    if (sTeilnehmer = '') then Exit;

    // Abruflisten füllen
    if (not Assigned(ClientStammdaten)) then
      ClientStammdaten := TClientStammdaten.Create();
    if (not ClientStammdaten.GetDatenAbrufliste(
      iStaId, C_IsArchive, pArchivAbrufListe, True)) then Exit;
    if (not ClientStammdaten.GetDatenAbrufliste(
      iStaId, C_IsLogbuecher, pLogbuchAbrufListe, True)) then Exit;
    if (FAbrufInstId > 0) then begin  // Ggf. Einschräkung der Abrufdaten
      if (FAbrufArchGrp > 0) then begin   // Einschränkung der Archivabrufe
        for i := pArchivAbrufListe.Count-1 downto 0 do
          with TAbrufListObj(pArchivAbrufListe.Items[i]) do
            if (InstanzId <> FAbrufInstId) or (Gruppe <> FAbrufArchGrp) then
              pArchivAbrufListe.Delete(i);
      end
      else pArchivAbrufListe.Clear;       // Keine Archivabrufe durchführen
      if (FAbrufLogbNr > 0) then begin    // Einschränkung der Logbuchabrufe
        for i := pLogbuchAbrufListe.Count-1 downto 0 do
          with TAbrufListObj(pLogbuchAbrufListe.Items[i]) do
            if (InstanzId <> FAbrufInstId) or (Gruppe <> FAbrufLogbNr) then
              pLogbuchAbrufListe.Delete(i);
      end
      else pLogbuchAbrufListe.Clear;      // Keine Logbuchabrufe durchführen
    end;

    pArchivDatenZugriff.NewArchive;  // Abrufliste für Konvertierung initial.

    // Archivabrufe durchführen
    for i := 0 to pArchivAbrufListe.Count-1 do begin
      pAbrufListObj := TAbrufListObj(pArchivAbrufListe.Items[i]);
      if (Pos(pAbrufListObj.EAdr, sTeilnehmer) = 0) or
         (not (pAbrufListObj.BusAdr_Quelle[1] in ['A'..'_']))
      then Continue;

      // Aktuellen Archivfüllstand holen
      dtBis := 0;
      iOBis := 0;
      if (iOBis = 1) then ;  // Dummy zur Hinweisvermeidung
      s := GetDeaArchivgruppe(pAbrufListObj.Gruppe);
      iIx := pOrdNrList.IndexOf(pAbrufListObj.EAdr[1] + s);
      if (iIx >= 0) then iOBis := Integer(pOrdNrList.Objects[iIx])
      else begin;
        iIx := pOrdNrList.Add(pAbrufListObj.EAdr[1] + s);
        s := s + 'd';  // Füllstand
        s := GetOneDEDSfGTelegramm(Adresse, pAbrufListObj.EAdr[1], s);
        s := SendReceiveTelegramm(s);
        s := Trim(GetWertFromTelegramm(s, 0));
        if (s = '') then Continue else iOBis := StrToIntDef(s, 0);
        pOrdNrList.Objects[iIx] := TObject(iOBis);
      end;
      if (iOBis = 0) then Continue;

      // Bisherige Archiveckwerte holen
      if (not pArchivDatenZugriff.GetLetztWertOrdNr_DatumZeit(iStaId,
        pAbrufListObj.InstanzId, pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.Kanaltyp, iOVon, dtVon))
      then Break;  // Schwerer Fehler

      // Parameter 'ab Ordnungsnummer' ermitteln
      if (iOVon = 0) then iOVon := iOBis - 24;  // noch keine Daten vorhanden
      if (iOVon < 1) then iOVon := 1;
      if (iOVon > iOBis) then dtBis := Now + 1; // Daten gelöscht -> Zeitabruf

      // Archivdaten holen
      if (dtBis > 0) then s := GetArchivByTime(pAbrufListObj.EAdr[1],
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal, dtVon, dtBis)
      else begin
        s := GetArchivByONummer(pAbrufListObj.EAdr[1],
          pAbrufListObj.Gruppe, pAbrufListObj.Kanal, iOVon, iOBis);
        if (not CheckArchivTelegramm(s, iOVon, dtVon)) then
          s := GetArchivByTime(pAbrufListObj.EAdr[1],
            pAbrufListObj.Gruppe, pAbrufListObj.Kanal, dtVon, Now+1)
      end;
      pArchivDatenZugriff.InsertArchivTelegramm(pAbrufListObj.InstanzId,
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.InstanzId_Quelle, pAbrufListObj.GerTypNr_Quelle, s);
    end;

    // Logbuchabrufe durchführen
    for i := 0 to pLogbuchAbrufListe.Count-1 do begin
      pAbrufListObj := TAbrufListObj(pLogbuchAbrufListe.Items[i]);
      if (Pos(pAbrufListObj.EAdr, s) = 0) or
         (not (pAbrufListObj.BusAdr_Quelle[1] in ['A'..'_']))
      then Continue;

      // Bisherige Logbucheckwerte holen
      if (not pArchivDatenZugriff.GetLetztDSfGMeldungOrdNr_DatumZeit(
        C_AbrArtAuto, pAbrufListObj.InstanzId, pAbrufListObj.Gruppe, iOVon, dtVon))
      then Break;  // Schwerer Fehler

      // Aktuellen Archivfüllstand holen
      dtBis := 0;
      iOBis := 0;
      if (iOBis = 1) then;  // Dummy zur Hinweisvermeidung
      s := GetDeaLogbuch(pAbrufListObj.BusAdr_Quelle[1]) + 'b';  // Füllstand
      s := GetOneDEDSfGTelegramm(Adresse, pAbrufListObj.EAdr[1], s);
      s := SendReceiveTelegramm(s);
      s := Trim(GetWertFromTelegramm(s, 0));
      if (s = '') then Break else iOBis := StrToInt(s);

      // Parameter 'ab Ordnungsnummer' ermitteln
      if (iOVon = 0) then iOVon := iOBis - 50;  // noch keine Daten vorhanden
      if (iOVon < 1) then iOVon := 1;
      if (iOVon > iOBis) then dtBis := Now + 1; // Daten gelöscht -> Zeitabruf

      // Logbuchdaten holen
      if (dtBis > 0) then s := GetLogbuchByTime(pAbrufListObj.EAdr[1],
        pAbrufListObj.BusAdr_Quelle[1], dtVon, dtBis)
      else begin
        s := GetLogbuchByONummer(pAbrufListObj.EAdr[1],
          pAbrufListObj.BusAdr_Quelle[1], iOVon, iOBis);
        if (not CheckArchivTelegramm(s, iOVon, dtVon)) then
          s := GetLogbuchByTime(pAbrufListObj.EAdr[1],
            pAbrufListObj.BusAdr_Quelle[1], dtVon, Now+1)
      end;
      pArchivDatenZugriff.InsertArchivTelegramm(pAbrufListObj.InstanzId,
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.InstanzId_Quelle, pAbrufListObj.GerTypNr_Quelle, s);
    end;

    Result := pArchivDatenZugriff.KonvertiereArchive(
      C_AbrArtAuto, GetZeitangaben, iStaId, 1000, -1, True);
  finally
    pOrdNrList.Free;
    pArchivAbrufListe.Free;
    pLogbuchAbrufListe.Free;
    pArchivDatenZugriff.Free;
    InitConnection(False);
    FAbruf := False;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.InitManuDatenAbruf(iStaId: integer;
  pAbrufListe: TArchivAbrufList; dtVon, dtBis: TDateTime): boolean;
{---------------------------------------------------------}
var
  pArchivAbrufListe   : TAbrufList;
  pLogbuchAbrufListe  : TAbrufList;
  s, sTeilnehmer      : string;
  i, j                : integer;
  b                   : boolean;
  pAbrufListObj       : TAbrufListObj;
  pArchivDatenZugriff : TArchivDatenZugriff;
begin
  FAbruf := True;
  Result := False;

  // Ggf. aktuelle Verbindung beenden
  if (Opened) then begin
    InitConnection(False);
    Delay(3000);
  end;

  pArchivAbrufListe := TAbrufList.Create;
  pLogbuchAbrufListe := TAbrufList.Create;
  pArchivDatenZugriff := TArchivDatenZugriff.Create(False);
  try
    // Bisherige Archivdaten löschen
    if (not pArchivDatenZugriff.DeleteStationsDaten(iStaId)) then Exit;  // Schwerer Fehler

    // Verbindung zur angeschlossenen Station herstellen
    InitConnection(True);
    sTeilnehmer := StringReplace(GetTeilnehmer, '.', '', [rfReplaceAll]);
    if (sTeilnehmer = '') then Exit;

    // Stimmt die Station ?
    s := '';  //  Zwischenspeicher für Registrierinstanzen
    if (not Assigned(ClientStammdaten)) then
      ClientStammdaten := TClientStammdaten.Create();
    with ClientStammdaten.FillDInstanzListe(ClientStammdaten.DStationsName(iStaId))
    do begin
      for i := 1 to Length(sTeilnehmer) do begin
        if (Self.Adresse = sTeilnehmer[i]) then Continue; // Eigene Adresse ...
        
        b := True;  // Teilnehmer stimmt mit Stammdaten überein
        for j := 0 to Count-1 do
          if (InstanzAdresse[j] = sTeilnehmer[i]) then begin
            // Prüfen von Instanztyp, Gerätetyp und Fabriknummer
            if (Pos(InstanzTyp[j], GetWertFromTelegramm(ReadParameter(
              sTeilnehmer[i], C_DEA_InstTyp), C_DEA_InstTyp)) = 0) or
               (Pos(InstanzGeraetetyp[j], GetWertFromTelegramm(ReadParameter(
              sTeilnehmer[i], C_DEA_GerTyp), C_DEA_GerTyp)) = 0) or
               (InstanzFabriknummer[j] <> Trim(GetWertFromTelegramm(ReadParameter(
              sTeilnehmer[i], C_DEA_FabrikNr), C_DEA_FabrikNr)))
            then begin
              b := False;
              Break;
            end
            else begin
              b := True;
              if (InstanzTyp[j] = C_D_Instanztyp_Reg) then
                s := s + sTeilnehmer[i];
              Break;
            end;
          end;
        if (not (b)) then Exit;
      end;
    end;
    if (Length(s) = 0) then Exit else sTeilnehmer := s;  // Reg.Inst. vorhanden ?

    // Abruflisten füllen
    if (not ClientStammdaten.GetDatenAbrufliste(
      iStaId, C_IsArchive, pArchivAbrufListe, False)) then Exit;
    if (not ClientStammdaten.GetDatenAbrufliste(
      iStaId, C_IsLogbuecher, pLogbuchAbrufListe, False)) then Exit;


    pArchivDatenZugriff.NewArchive;  // Abrufliste für Konvertierung initial.

    // Archivabrufe durchführen
    for i := 0 to pArchivAbrufListe.Count-1 do begin
      pAbrufListObj := TAbrufListObj(pArchivAbrufListe.Items[i]);
      if (Pos(pAbrufListObj.EAdr, sTeilnehmer) = 0) or
         (not (pAbrufListObj.BusAdr_Quelle[1] in ['A'..'_'])) or
         (not pAbrufListe.HasArchivKanal(pAbrufListObj.InstanzId,
           pAbrufListObj.Gruppe, pAbrufListObj.Kanal))
      then Continue;

      // Ggf. Zeitraum modifizieren
      if (dtBis = 0) then dtBis := Now + 1;

      // Archivdaten holen
      s := GetArchivByTime(pAbrufListObj.EAdr[1],
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal, dtVon, dtBis);
      pArchivDatenZugriff.InsertArchivTelegramm(pAbrufListObj.InstanzId,
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.InstanzId_Quelle, pAbrufListObj.GerTypNr_Quelle, s);
    end;

    // Logbuchabrufe durchführen
    for i := 0 to pLogbuchAbrufListe.Count-1 do begin
      pAbrufListObj := TAbrufListObj(pLogbuchAbrufListe.Items[i]);
      if (Pos(pAbrufListObj.EAdr, sTeilnehmer) = 0) or
         (not (pAbrufListObj.BusAdr_Quelle[1] in ['A'..'_'])) or
         (not pAbrufListe.HasArchivKanal(pAbrufListObj.InstanzId,
           pAbrufListObj.Gruppe, 0))
      then Continue;

      // Ggf. Zeitraum modifizieren
      if (dtBis = 0) then dtBis := Now + 1;

      // Logbuchdaten holen
      s := GetLogbuchByTime(pAbrufListObj.EAdr[1],
        pAbrufListObj.BusAdr_Quelle[1], dtVon, dtBis);
      pArchivDatenZugriff.InsertArchivTelegramm(pAbrufListObj.InstanzId,
        pAbrufListObj.Gruppe, pAbrufListObj.Kanal,
        pAbrufListObj.InstanzId_Quelle, pAbrufListObj.GerTypNr_Quelle, s);
    end;

    Result := pArchivDatenZugriff.KonvertiereArchive(
      C_AbrArtManu, GetZeitangaben, iStaId, 1000, -1, True);
  finally
    pArchivAbrufListe.Free;
    pLogbuchAbrufListe.Free;
    pArchivDatenZugriff.Free;
    InitConnection(False);
    FAbruf := False;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.ReadParameter(cIAdr: char; sDEAVon: string;
  sDEABis: string = ''): string;
{---------------------------------------------------------}
var
  s : string;
begin
  if (Opened) and (cIAdr in ['A'..'_']) and (sDEAVon <> '') then begin
  // Telegramm zusammenstellen
    if (sDEABis = '')
    then s := GetOneDEDSfGTelegramm(Adresse, cIAdr, sDEAVon)
    else s := GetBereichsabfrageDSfGTelegramm(Adresse, cIAdr, sDEAVon, sDEABis);

    Result := SendReceiveTelegramm(s);
  end
  else Result := '';
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.WriteParameter(
  cIAdr: char; sZC1, sZC2, sDEAdr, sValue: string; iType: byte = 0): string;
{---------------------------------------------------------}

  function GetReceiveValue(var sTelegram: string): string;
  begin
    Result := '';
    if (sTelegram = '') or (GetDSfGDatenTeil(sTelegram) = '?' + Chr(fs)) then
    begin                             // Wert einzeln abfragen
      sTelegram := ReadParameter(cIAdr, sDEAdr);
      if (sTelegram = '')
      then Exit
      else with GetDatenListe(sTelegram) do
      try
        if (Count = 1) then Result := GetStringPart(Strings[0], 2);
      finally
        Free;
      end;
    end
    else with GetDatenListe(sTelegram) do  // Sonst Wert aus Antwort
    try
      if (Count > 0) then Result := GetStringPart(Strings[Count-1], 2);
    finally
      Free;
    end;
  end;

var
  s, sDSfGCommand : string;
  pRec            : PDDEARec;
  iStellen        : integer;
begin
  Result := '';  // Default;

  sDSfGCommand :=
    GetEinstDSfGTelegramm(Adresse, cIAdr, sZC1, sZC2, sDEAdr, sValue);
  s := SendReceiveTelegramm(sDSfGCommand);
  Result := GetReceiveValue(s);

  if (s <> '') and (Trim(Result) <> Trim(sValue)) then begin  // Wert falsch
    // Falls Typ nicht definiert: Typ suchen
    if (iType = 0) then begin
      if (not Assigned(ClientStammdaten)) then
        ClientStammdaten := TClientStammdaten.Create();
      pRec := ClientStammdaten.GetDeaDefRecord(sDEAdr);
      if (Assigned(pRec)) then iType := StrToIntDef(pRec^.DEATyp, 0);
      if (iType = 0) then iType := 1;
    end;

    // Falls Typ = string => Stellenanzahl ermitteln
    if (iType = StrToInt(C_DEA_Typ_Zeichenkette)) then begin
      s := ReadParameter(cIAdr, sDEAdr);
      if (s = '')
      then Exit
      else with GetDatenListe(s) do
      try
        if (Count = 1)
        then iStellen := Length(GetStringPart(Strings[0], 2))
        else Exit;
        if (iStellen = 0) and (Pos(sDEAdr, Strings[0]) = 0) then Exit;
      finally
        Free;
      end;

      // Neuen Wert formatieren ...
      while (Length(sValue) < iStellen) do sValue := sValue + ' ';
      // ... und senden
      sDSfGCommand :=
        GetEinstDSfGTelegramm(Adresse, cIAdr, sZC1, sZC2, sDEAdr, sValue);
      s := SendReceiveTelegramm(sDSfGCommand);
      Result := GetReceiveValue(s);
    end;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.ReadStammdaten(iState: byte): TStrings;
{---------------------------------------------------------}
var
  sTeilnehmer       : string;
  sDeas, sTelegramm : string;
  i, iOldTimeOut    : integer;
  iTimeOut          : Cardinal;
  pSl               : TStrings;

  function GetInstTyp(s: string): char;
  var
    x : integer;
  begin
    Result := 'X';  // Default

    with GetDatenListe(s) do
    try
      for x := 0 to Count-1 do
        if (GetStringPart(Strings[x], 1) = C_DEA_InstTyp) then begin
          if (Length(GetStringPart(Strings[x], 2)) > 0) then
            Result := GetStringPart(Strings[x], 2)[1];
          Break;
        end;
    finally
      Free;
    end;
  end;

begin
  if (FSerial.Active) and (Adresse in ['A'..Chr(Ord('_')-1)]) then begin
    iOldTimeOut := TimeOut;
    try
      Result := TStringList.Create;

      // Erreichbare Busteilnehmer holen
      sTeilnehmer := GetTeilnehmer;
      // Gültiger DSfG-Bus und DPA angemeldet ?
      if (Pos('_', sTeilnehmer) > 0) then begin
        if (Pos(Adresse, sTeilnehmer) = 0) then begin
          with TComInfoWindow.CreateInfoWindow('') do
          try
            SetInfoText(S_Generalpolling);
            Show;
            iTimeOut := GetTickCount + Cardinal(TimeOut);
            while (Pos(Adresse, sTeilnehmer) = 0) and (GetTickCount <= iTimeOut) do
            begin
              SetInfoCaption(IntToStr((iTimeOut - GetTickCount) div 1000));
              Sleep(100);
              sTeilnehmer := GetTeilnehmer;
            end;
            if (iTimeOut <= GetTickCount) then Exit;
          finally
            Free;
          end;
        end;
      end
      else Exit;

      // Schleife über alle Teilnehmer
      for i := 1 to Length(sTeilnehmer) do
        if (sTeilnehmer[i] in ['A'..'_']) and (sTeilnehmer[i] <> Adresse) then
        begin
          TimeOut := 10000;
          if (Assigned(FNotify)) then
            FNotify(3, Format(S_ReadCommonKonfig, [sTeilnehmer[i]]));
          // Allgemeine Datenelemente holen
          sDeas := C_DEA_InstTyp + Chr(gs) + C_DEA_Hersteller + Chr(gs) +
            C_DEA_GerTyp + Chr(gs) + C_DEA_FabrikNr + Chr(gs) + C_DEA_Baujahr +
            Chr(gs) + C_DEA_GerSwVs + Chr(gs) + C_DEA_Inbetriebnahme;
          sTelegramm :=
            GetMultiDEDSfGTelegramm(Adresse, sTeilnehmer[i], sDeas, 7 );

          if (not Opened) then Exit;

          sTelegramm := SendReceiveTelegramm(sTelegramm);
          if (not Opened) then Exit;
          // Ggf. wiederholen
          if (sTelegramm = '') and (sTeilnehmer[i] <> '_') then begin
            sTelegramm :=
              GetMultiDEDSfGTelegramm(Adresse, sTeilnehmer[i], sDeas, 7 );
            sTelegramm := SendReceiveTelegramm(sTelegramm);
            if (not Opened) then Exit;
          end;

          // Gültiges Telegramm (z.B. keine außerplanmäßige Antwort) ?
          pSl := GetDatenListe(sTelegramm);
          if (not ((Assigned(pSl)) and (pSl.Count > 0) and
                   (Pos('aaa', pSl[0]) > 0)))
          then sTelegramm := '';

          if (sTelegramm = '') then Continue
          else begin
            Result.Add(sTeilnehmer[i] + ' ' + sTelegramm);
            // Datenelemente der Registrierinstanz holen
            if (iState = 2) and (GetInstTyp(sTelegramm) = C_D_Instanztyp_Reg) then
            begin
              TimeOut := 60000;
              if (Assigned(FNotify)) then
                FNotify(3, Format(S_ReadArchiveKonfig, [sTeilnehmer[i]]));
              sTelegramm := GetBereichsabfrageDSfGTelegramm(
                Adresse, sTeilnehmer[i], 'c', 'cbdbz');

              if (not Opened) then Exit;

              sTelegramm := SendReceiveTelegramm(sTelegramm);
              // Ggf. wiederholen
              if (sTelegramm = '') and (sTeilnehmer[i] <> '_') then begin
                sTelegramm := GetBereichsabfrageDSfGTelegramm(
                  Adresse, sTeilnehmer[i], 'c', 'cbdbz');
                sTelegramm := SendReceiveTelegramm(sTelegramm);
                if (not Opened) then Exit;
              end;

              if (not Opened) then Exit;

              if (sTelegramm = '')
              then Continue
              else Result.Add(sTeilnehmer[i] + 'R' + sTelegramm);
            end;

          end;
        end;
    finally
      TimeOut := iOldTimeOut;
    end;
  end
  else Result := TStringList.Create;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.ReceiveCommand(
  iTimeOut: Cardinal = 10000; cEndeChar: char = Chr(cr)): string;
{---------------------------------------------------------}
var
  i, j  : integer;
  iStop : Cardinal;
begin
  Result := '';
  if (not FSerial.Active) then Exit;
  iStop := GetTickCount + iTimeOut;

  while (not FTerminated) and (GetTickCount <= iStop) and
        (Length(FEmpfang) < 100000) do
  begin
    Delay(1);
    FEmpfang := FEmpfang + GetDaten;
    // Fälschlich erkannte Zeichen eleminieren
    i := 0;
    if (i = 1) then;  // Dummy zur Hinweisvermeidung
    j := Pos(cEndeChar, FEmpfang);
    if (j > 0) then begin
      if (cEndeChar = Chr(cr)) then i := Pos(Chr(esc), FEmpfang)
      else if (cEndeChar = Chr(etx)) then i := Pos(Chr(stx), FEmpfang)
      else Exit;
      if (i = 0) or (i >= j-1) then System.Delete(FEmpfang, 1, j);
    end;

    if ((cEndeChar = Chr(cr)) and
        (Pos(Chr(esc), FEmpfang) > 0) and (Pos(Chr(cr), FEmpfang) > 0) and
        (Pos(Chr(cr), FEmpfang) > (Pos(Chr(esc), FEmpfang)+1))) or
       ((cEndeChar = Chr(etx)) and
        (Pos(Chr(stx), FEmpfang) > 0) and (Pos(Chr(etx), FEmpfang) > 0) and
        (Pos(Chr(etx), FEmpfang) > (Pos(Chr(stx), FEmpfang)+1)))
    then Break;
  end;

  if (cEndeChar = Chr(cr)) then begin
    i := Pos(Chr(esc), FEmpfang);
    j := Pos(Chr(cr), FEmpfang);
    if (i > 0) and (j > i+1) then begin
      Result := Copy(FEmpfang, i, (j-i+1));
      System.Delete(FEmpfang, 1, j);
      if (Assigned(FNotify)) then FNotify(2, CharstrToString(Result));
    end;
  end
  else begin
    i := Pos(Chr(stx), FEmpfang);
    j := Pos(Chr(etx), FEmpfang);
    if (i > 0) and (j > i+1) then begin
      Result := Copy(FEmpfang, i, (j-i+1));
      System.Delete(FEmpfang, 1, j);
      if (Assigned(FNotify)) then FNotify(2, CharstrToString(Result));
    end;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.SendReceiveCommand(sCommand: string;
  iTimeOut: Cardinal = 10000; cEndeChar: char = Chr(cr)): string;
{---------------------------------------------------------}
begin
  Result := '';
  if (not FSerial.Active) or (Busy) then Exit;
  FBusy := True;
  try
    FEmpfang := '';
    SetDaten(sCommand);

    Result := ReceiveCommand(iTimeOut, cEndeChar);
  finally
    FBusy := False;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.GetMonitorMode: boolean;
{---------------------------------------------------------}
begin
  Result := FMonitorMode;
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.SetMonitorMode(bValue: boolean);
{---------------------------------------------------------}
var
  sOnOff : string;
begin
  if (FSerial.Active) then begin
    if (bValue) then sOnOff := 'M1' else sOnOff := 'M0';
    SetDaten(chr(esc) + sOnOff + chr(cr));
    Delay(100);
    FMonitorMode := bValue;
    FMonitorTimer.Enabled := FMonitorMode;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.GetRundsendeMode: boolean;
{---------------------------------------------------------}
begin
  Result := FRundsendeMode;
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.SetRundsendeMode(bValue: boolean);
{---------------------------------------------------------}
var
  s, sOnOff : string;
  b         : boolean;
begin
  if (FSerial.Active) then begin
    if (bValue) then sOnOff := 'R1' else sOnOff := 'R0';
    SetDaten(chr(esc) + sOnOff + chr(cr));
    Delay(100);
    FRundsendeMode := bValue;

    if (not bValue) then begin
      b := FBusy;
      try
        FBusy := False;
        s := SendReceiveCommand(Chr(esc) + 'RH' + Chr(cr), 1000, Chr(etx));
        while (Pos(Chr(stx), s) > 0) do
          s := SendReceiveCommand(Chr(esc) + 'RH' + Chr(cr), 1000, Chr(etx));
      finally
        FBusy := b;
      end;
    end;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.GetLastRundsendeTelegramm: string;
{---------------------------------------------------------}
var
  s : string;
begin
  if (FSerial.Active) and (FRundsendeMode) then begin
    Delay(10);
    s := SendReceiveCommand(Chr(esc) + 'RH' + Chr(cr), 10000, Chr(etx));
    if (Pos(Chr(stx), s) > 0) then begin
      Result := s;
      if (Assigned(FReceiveTeleView)) then begin
        FReceiveTeleView(Result);
        FReceiveTeleView('');
      end;
    end;
    Delay(10);
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.GetFirmwareVersion: string;
{---------------------------------------------------------}
var
  s : string;
begin
  if (FSerial.Active) then begin
    s := SendReceiveCommand(Chr(esc) + 'V' + Chr(cr));
    if (Length(s) >= 3) then Result := Copy(s, 3, Length(s)-3);
  end;
end;

{---------------------------------------------------------}
procedure TDSfGDPAConnection.MonitorTimerTimer(Sender: TObject);
{---------------------------------------------------------}
var
  sDaten : string;
  i      : integer;
begin
  FMonitorTimer.Enabled := False;
  try
    if Assigned(FMonitorFile) then begin
      sDaten := GetDaten;
//      sDaten := CharstrToString(sDaten);
      for i := 1 to Length(sDaten) do FMonitorFile.WriteBuffer(sDaten[i], 1);
      Application.ProcessMessages;
    end;
  finally
    FMonitorTimer.Enabled := FMonitorMode;
  end;
end;

{---------------------------------------------------------}
function TDSfGDPAConnection.FlashDpaSoftware(sHexFile: TFileName): boolean;
{---------------------------------------------------------}
var
  s : string;
  f : file of char;
  c : char;
  b : boolean;
  i, iSize : integer;
  fInfo : TComInfoWindow;
begin
  Result := False;
  if (not FileExists(sHexFile)) then Exit;

  b := FSerial.Active;
  try
    FSerial.Active := False;
    with TSerial.Create(nil) do
    try
      COMPort := Self.ComPort;
      HandshakeRtsCts := False;
      HandshakeDtrDsr := False;
      HandshakeXOnXOff := False;
      Baudrate := br_009600;
      DataBits := db_7;
      ParityBit := even;
      StopBits := sb_1;
      OpenComm;
      if (Active) then
      try
        if (MessageDlg('Verbinden Sie bitte den DPA2200 über'#13#10 +
          'den mitgelieferten Flash-Stecker.', mtConfirmation, [mbOk, mbCancel], 0)
          = mrCancel)
        then Exit;
        if (MessageDlg('Bitte machen Sie den DPA2200'#13#10 +
          'für ca. 5 sec. stromlos.', mtConfirmation, [mbOk, mbCancel], 0)
          = mrCancel)
        then Exit;
        s := ReceiveText;
        if (MessageDlg('Meldung vom DPA:'#13#10 + s, mtConfirmation,
          [mbOk, mbCancel], 0) = mrCancel)
        then Exit;
        if (MessageDlg('Das bisherige Betriebsprogramm wird gelöscht.',
          mtConfirmation, [mbOk, mbCancel], 0) = mrCancel)
        then Exit;
        TransmittText('E'#13);
        s := '00000';
        if (not InputQuery('Bitte geben Sie eine Fabriknummer ein',
          'Fabriknummer (5-stellig)', s))
        then Exit;
        TransmittText('N'#13 + Copy(s, 1, 5));
        if (MessageDlg('Das neue Betriebsprogramm wird im Anschluss geflashed.',
          mtConfirmation, [mbOk, mbCancel], 0) = mrCancel)
        then Exit;

        TransmittText('L'#13);

        fInfo := TComInfoWindow.CreateInfoWindow('Flashen DPA2200');
        try
          fInfo.EnableStop := False;
          fInfo.Show;

          AssignFile(f, sHexFile);
          Reset(f);
          try
            iSize := FileSize(f);
            i := 0;
            while (not Eof(f)) do begin
              Read(f, c);
              TransmittData(c, 1);
              Inc(i);
              fInfo.SetInfoText(
                Format('%.d von %.d Zeichen übertragen', [i, iSize]));
            end;
          finally
            CloseFile(f);
          end;
        finally
          fInfo.Free;
        end;

        TransmittText('X'#13 + Copy(s, 1, 5));
        MessageDlg(
          'Der Flashvorgang wurde abgeschlossen.'#13#10#13#10'Bitte entfernen' +
          ' Sie den Flash-Stecker und machen den DPA2200 für ca. 5 sec. stromlos.',
          mtInformation, [mbOk], 0);
        Result := True;
      finally
        CloseComm;
      end;
    finally
      Free;
    end;
  finally
    if (b) then FSerial.Active := True;
  end;
end;

{---------------------------- TDSfGDpaMomConnection ------------------------------}

{ Initialisieren/Freigeben v. Komponenten }
{ Parameter: T-Init.; F-Freigeben         }
{-----------------------------------------}
procedure TDSfGDpaMomConnection.InitComponents(aState: boolean);
{-----------------------------------------}
begin
  inherited;

  if (aState) then begin
    FNewDataList := TStringList.Create;

    FMomList := TDSfGMomDefList.Create;
    FTempTimer := TTimer.Create(nil);
    FTempTimer.Enabled := False;
    FTempTimer.Interval := 2000;
    FTempTimer.OnTimer := TempTimerTimer;
    FTempTimer.Enabled := True;
    FEnableTempTimer := True; // Flag für internere Timer
  end
  else begin
    FTempTimer.Enabled := False;
    FMomCallBack := nil;
    if (Assigned(FTempTimer)) then FTempTimer.Free;
    if (Assigned(FMomList)) then FMomList.Free;
    if (Assigned(FNewDataList)) then FNewDataList.Free;
  end;
end;

{ Fragt die definierten Momentanwerte ab  }
{-----------------------------------------}
procedure TDSfGDpaMomConnection.CallMomData;
{-----------------------------------------}
begin
  if (Assigned(FMomCallBack)) then FMomCallBack(GetAnzeigeDaten);
end;

{ Fügt Momentanwerte in Abrufliste ein    }
{ Parameter: InstanzAdr, DEA von, DEA bis }
{-----------------------------------------}
procedure TDSfGDpaMomConnection.InsertMomData(cIAdr: char; sDEAVon, sDEABis: string);
{-----------------------------------------}
begin
  if (not Opened) then Exit;

  FMomList.InsertMomDefValue(cIAdr, sDEAVon, sDEABis);
  FTempTimer.Enabled := True;
end;

{ Löscht Daten aus der Abrufdef.-Liste    }
{ Parameter: InstanzAdr, DEA von, DEA bis }
{-----------------------------------------}
procedure TDSfGDpaMomConnection.DeleteMomData(cIAdr: char; sDEAVon, sDEABis: string);
{-----------------------------------------}
var
  i : integer;
  c1, c2 : char;
begin
  if (not Opened) then Exit;

  i := FMomList.Indexof(cIAdr + sDEAVon);
  if (i >= 0) then FMomList.Delete(i);

  if (sDEABis <> '') then begin
    c1 := sDEAVon[1];
    c2 := sDEABis[1];
  // DE-Bereich löschen
    for i := FMomList.Count-1 downto 0 do begin
      if (PDMDInsertRec(FMomList.Objects[i])^.cIAdr = cIAdr) and
         (PDMDInsertRec(FMomList.Objects[i])^.sDEAVon[1] in [c1..c2]) then
      begin
        FMomList.Delete(i);
      end;
    end;
  end;

//  FTempTimer.Enabled := True;
end;

{ Leert die Abruf-Definitions-Liste       }
{-----------------------------------------}
procedure TDSfGDpaMomConnection.ClearMomData;
{-----------------------------------------}
begin
  FMomList.Clear;
end;

{ Holt die eingestellten Parameter        }
{ Rückgabe: Liste mit Abruf-Ergebnis      }
{-----------------------------------------}
function TDSfGDpaMomConnection.GetAnzeigeDaten: TDSfGMomValueList;
{-----------------------------------------}
var
  i : integer;
  p : PDMomValueRec;
begin
  Result := TDSfGMomValueList.Create;

  for i := 0 to FNewDataList.Count-1 do begin
    New(p);
    p^.cIAdr := GetStringPart(FNewDataList[i], 1);
    p^.sDEA := GetStringPart(FNewDataList[i], 2);
    p^.sWert := GetStringPart(FNewDataList[i], 3);
    p^.iStellen := StrToIntDef(GetStringPart(FNewDataList[i], 4), 0);
    Result.AddObject(p^.cIAdr + p^.sDEA, TObject(p));
  end;
end;

{ Holt die eingestellten Parameter        }
{-----------------------------------------}
procedure TDSfGDpaMomConnection.HoleNeueDaten;
{-----------------------------------------}
var
  s    : string;
  i, j : integer;
  c    : char;
  pSl  : TStrings;

  procedure InsertToResult;
  var
    iCount : integer;
  begin
    if (Length(s) > 0) then begin
      Delete(s, Length(s), 1);    // Letztes <gs> löschen
      s := GetMultiDEDSfGTelegramm(Adresse, c, s, j);

      if (not Opened) then Exit;

      s := SendReceiveTelegramm(s);

      if (not Opened) then Exit;

      if (s <> '') then
        with GetDatenListe(s) do  // DEAs in Liste eintragen
        try
          for iCount := 0 to Count-1 do begin
            pSl.Add(c + Chr(us) + GetStringPart(Strings[iCount], 1) + Chr(us) +
              GetStringPart(Strings[iCount], 2) + Chr(us) +
              IntToStr(Length(GetStringPart(Strings[iCount], 2))));
          end;
        finally
          Free;
        end;
    end;
  end;

begin
  pSl := TStringList.Create;
  with TDSfGMomDefList.Create do
  try
    // Inhalt der aktuellen Momentanliste übernehmen
    for i := 0 to FMomList.Count-1 do
      if (i < FMomList.Count)
      then InsertMomDefValue(
        FMomList.InstanzAdresse[i], FMomList.DeaVon[i], FMomList.DeaBis[i])
      else Break;

    // Telegramm für abzurufende Daten zusammenstellen
    Sort;  // Sortieren, damit zusammengehörende DEAs zusammenstehen

    s := '';        // DEAs für ein Telegramm
    j := 0;         // Anzahl der DEAs in dem Telegramm
    for i := 0 to Count-1 do begin

      if (not Opened) or (IsParametrierung) then Exit;

      if (i = 0) then c := Strings[i][1];  // 1. Instanzadresse
      if (c = Strings[i][1])
      then begin
        s := s + DeaVon[i] + Chr(gs);
        Inc(j);
      end
      else begin
        InsertToResult;
        if (Count <= i) then Exit; // Liste wurde extern gelöscht !
        c := Strings[i][1];
        s := DeaVon[i] + Chr(gs);
        j := 1;
      end;
    end;
    InsertToResult;

    FNewDataList.Assign(pSl);
  finally
    Free;
    pSl.Free;
  end;
end;

{ Fragt ab, ob neue Werte da sind         }
{ Rückgabe: Neue Werte Ja/Nein            }
{-----------------------------------------}
function TDSfGDpaMomConnection.GetNewData: boolean;
{-----------------------------------------}
begin
  Result := FNewData;
  FNewData := False;
end;

{ Ereignis für TempTimer                  }
{-----------------------------------------}
procedure TDSfGDpaMomConnection.TempTimerTimer(Sender: TObject);
{-----------------------------------------}
begin
  FTempTimer.Enabled := False;
  if (not IsParametrierung) then begin
    HoleNeueDaten;
    FNewData := True;
  end;
  FTempTimer.Enabled := ((Opened) and (FEnableTempTimer));
end;

end.
