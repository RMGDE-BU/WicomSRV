{******************************************************************************}
{* Unit: Basisklassen für MRG/DSfG-Kommunikation                              *}
{* 04.03.2009 WW                                                              *}
{* 14.02.2012 WW  Property ComLogFile                                         *}
{* 22.02.2012 WW  DSfG mit Systemzeit-Passwort                                *}
{* 20.07.2012 WW  mit DSfG-DFÜ-Erweiterungsgrad 2; DSfG-DFÜ-Befehl 's' ohne   *}
{*                SOH nach dem Systemzeit-Passwort                            *}
{* 24.02.2020 WW  Property ComTracelog                                        *}
{* 18.02.2021 WW  Unterbrochene Verbindung loggen                             *}
{* 03.01.2022 WW  Erweiterung ComTracelog für MRG                             *}
{* 19.12.2022 WW  Timeout-Überwachung: Timer durch TickCount ersetzt          *}
{******************************************************************************}
unit O_Comm;

interface

uses
  Windows, SysUtils, Forms, Classes, DateUtils, LogCom, WComm, WChars,
  T_Zeit, T_Tools, WStrUtils, AbrufTimeoutConst, DSG_Utils, ErrConst, CRC16,
  WSysCon, GPRS_Util, DValidAnswer, UnixDT, T_MUSysTime;

Const
  { MRG-Standard-Baudrate für:
      - alle Abrufe und Rufentgegennahme mit Thema 'MRG-FUP'
      - Rufentgegennahme mit Thema 'MRG-Modem' von FUP-Geräten (ACK01-Protokoll) }
  C_BaudMRGStandard = 9600;

type
  TEndezeichenSet = set of char;

  TAntwortTelegrammTyp = (at_unbekannt,         // unbekanntes Antworttelegramm
                          at_PushData_MRG900,   // Push-Datentelegramm, MRG 905/910 (GPRS)
                          at_sonstige);         // alle anderen Antworttelegrammtypen

  { Callback-Prozedurtypen zur Anzeige in Fremdmodulen }

  TCBDataProc = procedure (Data: string; Bytes: string) of object;
  TCBStatusMsgProc = procedure (Msg: string) of object;

  { Basis-Objekt für Kommunikation }

  TCommObj = class(TObject)
  private
    { Private-Deklarationen }
    FTxD: TCBDataProc;
    FRxD: TCBDataProc;
    FTimeout: TCBStatusMsgProc;
    FLastTickCount: cardinal;  { für Timeout-Überwachung per TickCount }
    { für RMG-Systemzeit-Passwort: }
    FSysTimeDiff: Int64;  { Systemzeit-Differenz zwischen Gerät und PC (Unix) }
    FWithSysTimePW: boolean;  { Flag zur Steuerung Befehl mit/ohne Systemzeit-Passwort }
    procedure CalcSysTimeDiff (sSysTime_Geraet: string);
    function CalcSysTimePW: string;
  protected
    Timeout: integer;
    TimeoutCount: integer;
    FComLogFile: TComLogFile;
    FComTraceLog: TComLogFile;  // 24.02.2020, WW
    AnswerDest: TAnswerDest;
    Befehl: string;
    WorkPath: TFileName;
    RohFileName: string;
    procedure Timeout_Init;  // 19.12.2022, WW
    procedure Timeout_Elapsed;  // 19.12.2022, WW
    procedure AddPushTelegramToList (ATelegram: string; AGeraetetyp: integer); virtual;
    function InitializeSysTimePW (ATimeout: integer; var Rueckgabe: TRueckgabe;
      var NoCarrier: boolean): boolean; virtual; abstract;
  public
    { Public-Deklarationen }
    constructor Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
    function CheckForPushData (AData: string): TAntwortTelegrammTyp;
    function SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                          AEndezeichenAnzahl: integer;
                          ATimeout: integer; AAnswerDest: TAnswerDest;
                          var Rueckgabe: TRueckgabe; var NoCarrier: boolean;
                          AnzCharsToReceive: cardinal = 0): boolean; virtual; abstract;
    function SendCommandWithSysTimePW (ABefehl: string;
                                       AEndezeichen: TEndezeichenSet;
                                       AEndezeichenAnzahl: integer;
                                       ATimeout: integer; AAnswerDest: TAnswerDest;
                                       var Rueckgabe: TRueckgabe;
                                       var NoCarrier: boolean): boolean;
    function SendFwBinData (ABefehl: string; ATimeout: integer;
      var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
    function SendFwEndCommand (ATimeout: integer;
      var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
    property CBTxD: TCBDataProc read FTxD write FTxD;
    property CBRxD: TCBDataProc read FRxD write FRxD;
    property CBTimeout: TCBStatusMsgProc read FTimeout write FTimeout;
    property ComLogFile: TComLogFile read FComLogFile  // 13.02.2012, WW
      write FComLogFile;  // 21.06.2012, WW
    property ComTraceLog: TComLogFile read FComTraceLog
     write FComTraceLog;  // 24.02.2020, WW
  end;


  { Basis-Objekt für DSfG-Kommunikation }

  TDSfGCommObj = class(TCommObj)
  private
    { Private-Deklarationen }
    Versuche_BCC: integer;        { max. Anzahl an Versuchen bei falschem BCC }
    EndeZeichenEmpfangen: boolean;
    BCC: byte;
    BCCDSfG: string [2];
    Versuche: byte;
    Extensionmode: byte;  { Erweiterungsgrad, mit dem die DSfG-DFÜ-Kommunikation abläuft }
    { für Push-Daten, MRG 910: }
    CRCMRG: string;
    { für GPRS-Kommunikation in abgeleiteten Klassen: }
    FGPRSData_Ausgabe: TCBGPRSDataProc;
    { Empfangene DSfG-Quittierung im Klartext ausgeben (für Gas-X): }
    FQuittung: TCBStatusMsgProc;
    procedure SetStandardVersuche;
    function EndOfBCCTransmission (var Rueckgabe: TRueckgabe): boolean;
  protected
    FCommError: cardinal;
    FPrefix_RMGDfueBefehl: string;    { Prefix für RMG-DSfG-DFÜ-Befehle je nach Verbindungsart }
    { für GPRS-Kommunikation in abgeleiteten Klassen: }
    FGPRSRemoteAddress: string;
    FGPRSRemotePort: integer;
    function Comm_SendData (sData: string): boolean; virtual; abstract;
    function Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal): string; virtual; abstract;
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; virtual; abstract;
    procedure Comm_ClearRecBuf; virtual; abstract;
    procedure Comm_SetFehlerRueckgabe (NoCarrier: boolean; var Rueckgabe: TRueckgabe); virtual; abstract;
    procedure BefehlSenden;
  public
    { Public-Deklarationen }
    constructor Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
    procedure SetVersuche (AVersuche_BCC: integer);
    function GetMaxExtensionmode: byte;
    function SetExtensionmode (Value: byte): byte; virtual;
    function SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                          AEndezeichenAnzahl: integer;
                          ATimeout: integer; AAnswerDest: TAnswerDest;
                          var Rueckgabe: TRueckgabe; var NoCarrier: boolean;
                          AnzCharsToReceive: cardinal = 0): boolean; override;
    function InitializeSysTimePW (ATimeout: integer; var Rueckgabe: TRueckgabe;
      var NoCarrier: boolean): boolean; override;
    property CBGPRSData_Ausgabe: TCBGPRSDataProc read FGPRSData_Ausgabe write
      FGPRSData_Ausgabe;
    property CBQuittung: TCBStatusMsgProc read FQuittung write FQuittung;
  end;


  { Basis-Objekt für MRG-Kommunikation }

  TMRGCommObj = class(TCommObj)
  private
    { Private-Deklarationen }
  protected
    DelayCommand: integer;
    DelayCommandChar: integer;        { Wartezeit zwischen den Befehlszeichen }
    Reset_COM_Before_Sending: boolean;  { Flag: mit/ohne COM-Reset vor dem ersten zu versendenen Zeichen }
    procedure Comm_Reset; virtual; abstract;
    function Comm_SendData (sData: string): boolean; virtual; abstract;
    function Comm_SendChar (c: char): boolean; virtual; abstract;
    procedure BefehlSenden (ATempBefehl: string = '';
                            AComLogHexDaten: boolean = false); virtual;
  public
    { Public-Deklarationen }
    constructor Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
    function SendCommandList (ABefehlListe: TBefehlList;
                              AStartIndex_Befehlliste: integer;
                              AEndezeichen: TEndezeichenSet;
                              AEndezeichenAnzahl: integer;
                              ATimeout: integer; AAnswerDest: TAnswerDest;
                              var Rueckgabe: TRueckgabe;
                              var NoCarrier: boolean;
                              var AktIndex_Befehlliste: integer;
                              bTeilantwort_bei_Fehler: boolean = false): boolean;
    function Rufabfrage (var Ruf_angekommen: boolean;
                         var Fehlergruppe: integer; var Fehlercode: integer): boolean; virtual; abstract;
  end;

implementation

const
  CMaxDSfGExtensionmode = 2;   { höchster DSfG-Erweiterungsgrad, der vom Programm unterstützt wird
                                 ab 20.07.2012: Erweiterungsgrad 2 }

resourcestring
  SEmpfangeneQuittung = 'Empfangene Quittung: %s';
  SKeine = 'Keine';


{ TCommObj }

{---------------------------------------------------------------------------}
constructor TCommObj.Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
{---------------------------------------------------------------------------}
begin
  inherited Create;
  WorkPath:=AWorkPath;
  FComLogFile:=AComLogFile;

  FComTraceLog:=nil;  // Default: Ohne Tracelog
  Befehl:='';
  Timeout:=0;
  TimeoutCount:=0;
  FLastTickCount:=0;
  AnswerDest:=ad_String;
  RohFileName:='';
  FSysTimeDiff:=0;
  FWithSysTimePW:=false;  { Default: Befehl ohne Systemzeit-Passwort }
end;

{------------------------------}
procedure TCommObj.Timeout_Init;
{------------------------------}
{ Timeout-Überwachung initialisieren }
begin
  TimeoutCount:=0;  // Timeoutzähler rücksetzen
  FLastTickCount:=GetTickCount;  // TickCount aktualisieren
end;

{---------------------------------}
procedure TCommObj.Timeout_Elapsed;
{---------------------------------}
{ Verstrichene Zeit per TickCount messen für Timeout-Überwachung }
var
  ms: cardinal;

begin
  ms:=F_GetTickCountDiff (FLastTickCount);
  TimeoutCount:=TimeoutCount + integer(ms);

  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr((Timeout - TimeoutCount) DIV 1000) + ' s');
end;

{-----------------------------------------------------------------------}
function TCommObj.CheckForPushData (AData: string): TAntwortTelegrammTyp;
{-----------------------------------------------------------------------}
{ Prüfung auf Push-Daten (GPRS);
  Übergabe: Rohdaten
  Ergebnis: Antwort-Telegrammtyp }
begin
  Result:=at_unbekannt;  // unbekanntes Antworttelegramm
  if length (AData) >= 2 then begin
    if (AData [1] = STX) AND (AData [2] in ['p', 'q', 'r', 'v']) then
      Result:=at_PushData_MRG900  // Push-Datentelegramm, MRG 905/910 (GPRS)
    else
      Result:=at_sonstige;  // alle anderen Antworttelegrammtypen
  end;
end;

{---------------------------------------------------------------------------------}
procedure TCommObj.AddPushTelegramToList (ATelegram: string; AGeraetetyp: integer);
{---------------------------------------------------------------------------------}
{ Telegramm in Push-Telegrammliste anhängen;
  Übergabe: Telegramm
            Gerätetyp }
begin
  // Default: Keine Aktion
end;

{-----------------------------------------------------------}
procedure TCommObj.CalcSysTimeDiff (sSysTime_Geraet: string);
{-----------------------------------------------------------}
{ Systemzeit-Differenz zwischen Gerät und PC berechnen;
  Übergabe: Geräte-Systemzeit (Unix-Format) }
var
  iSysTime: cardinal;

begin
  GetUnixSekundenFromUnixTimeStr (sSysTime_Geraet, iSysTime);
  FSysTimeDiff:=Int64 (iSysTime) - GetUnixSekundenFromDateTime (Now);
end;

{--------------------------------------}
function TCommObj.CalcSysTimePW: string;
{--------------------------------------}
{ RMG-Systemzeit-Passwort berechnen;
  Ergebnis: Berechnetes Systemzeit-Passwort }
var
  dt: TDateTime;
  S: string;

begin
  dt:=IncSecond (Now, FSysTimeDiff + 5);    // Zeitfenster-Offset: 1..9 s, wir nehmen die Mitte: 5
  DateTimeToUnixTimeStr (dt, S);
  Result:=EncodeSysTimeStr (S);
end;

{--------------------------------------------------------------------------------------}
function TCommObj.SendCommandWithSysTimePW (ABefehl: string;
  AEndezeichen: TEndezeichenSet; AEndezeichenAnzahl: integer; ATimeout: integer;
  AAnswerDest: TAnswerDest; var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
{--------------------------------------------------------------------------------------}
{ Befehl mit Systemzeit-Passwort senden und Antwort empfangen;
  Übergabe: Befehl-String
            in der Antwort zu erwartende Endezeichen
            Anzahl, der in der Antwort zu erwartenden Endezeichen
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist
            AAnswerDest (DestString: Empfangsdaten -> Antwort-String
                         Destfile: Empfangsdaten -> Rohfile
  Rückgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AnswerDest = DestString: Empfangsdaten
                          AnswerDest = Destfile: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
begin
  FWithSysTimePW:=true;  { Flag setzen: Befehl mit Systemzeit-Passwort }
  Result:=SendCommand (ABefehl, AEndezeichen, AEndezeichenAnzahl, ATimeout,
                       AAnswerDest, Rueckgabe, NoCarrier);
  FWithSysTimePW:=false;  { Flag löschen: Nächste Befehle ohne Systemzeit-Passwort }

  if Result then begin
    { Antwort auswerten: }
    if not ValidDSfGDfueAntwort (true, Rueckgabe.Antwort, Rueckgabe.Fehlergruppe,
                                 Rueckgabe.Fehlercode) then begin
      if (Rueckgabe.Fehlergruppe = COM_DSFGDFUERROR) AND
         (Rueckgabe.Fehlercode = DSFGDFUERR_WIESER_VERBOTEN) then begin
        { Wenn das Systempasswort vom Gerät abgelehnt wird, Gerätesystemzeit holen
          und Befehl nochmal probieren: 08.08.2012, WW }
        if not InitializeSysTimePW (ATimeout, Rueckgabe, NoCarrier) then begin
          Result:=false;
          exit;
        end;

        FWithSysTimePW:=true;  { Flag setzen: Befehl mit Systemzeit-Passwort }
        Result:=SendCommand (ABefehl, AEndezeichen, AEndezeichenAnzahl, ATimeout,
                             AAnswerDest, Rueckgabe, NoCarrier);
        FWithSysTimePW:=false;  { Flag löschen: Nächste Befehle ohne Systemzeit-Passwort }
      end;
    end;
  end;
end;

{----------------------------------------------------------------------}
function TCommObj.SendFwBinData (ABefehl: string; ATimeout: integer;
      var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
{----------------------------------------------------------------------}
{ Firmware-Binärdaten senden und Antwort empfangen;
  Übergabe: Befehl-String
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist
  Rückgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (Empfangsdaten)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  bStop: boolean;
  Wdh: integer;

begin
  Result:=false;

  bStop:=false;
  Wdh:=0;
  while not bStop AND not NoCarrier do begin
    { Befehl senden: }
    if not SendCommand (ABefehl, [ACK, NAK, CAN, ENQ, 'X'], 1, ATimeout,
                        ad_String, Rueckgabe, NoCarrier) then exit;

    if Rueckgabe.Antwort = ACK then begin  // OK
      Break;
    end
    else if Rueckgabe.Antwort = NAK then begin  // Falsches XOR, Wiederholung
      inc (Wdh);
      { begrenzte Anzahl von Versuchen: }
      if Wdh >= 3 then begin
        Rueckgabe.Fehlergruppe:=EST_FIRMWAREUPDATEERROR;
        Rueckgabe.Fehlercode:=FIRMWUPDERR_BINDATA_TRANSFER_NAK;
        exit;
      end;
    end
    else if Rueckgabe.Antwort = CAN then begin  // Adressfehler
      Rueckgabe.Fehlergruppe:=EST_FIRMWAREUPDATEERROR;
      Rueckgabe.Fehlercode:=FIRMWUPDERR_BINDATA_TRANSFER_CAN;
      exit;
    end
    else if Rueckgabe.Antwort = ENQ then begin  // Datensatz ohne vorherigen Adresssatz gesendet
      Rueckgabe.Fehlergruppe:=EST_FIRMWAREUPDATEERROR;
      Rueckgabe.Fehlercode:=FIRMWUPDERR_BINDATA_TRANSFER_ENQ;
      exit;
    end
    else if Rueckgabe.Antwort = 'X' then begin  // Fehler bei der Übertragung; 31.07.2013, WW
      Rueckgabe.Fehlergruppe:=EST_FIRMWAREUPDATEERROR;
      Rueckgabe.Fehlercode:=FIRMWUPDERR_BINDATA_TRANSFER_X;
      exit;
    end
    else begin  // Undefinierter Fehler
      Rueckgabe.Fehlergruppe:=EST_FIRMWAREUPDATEERROR;
      Rueckgabe.Fehlercode:=FIRMWUPDERR_BINDATA_TRANSFER_UNDEF;
      exit;
    end;
  end;  { while not bStop }

  Result:=true;
end;

{------------------------------------------------------------}
function TCommObj.SendFwEndCommand (ATimeout: integer;
  var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
{------------------------------------------------------------}
{ Firmware-END-Kommando senden und Antwortzeichen empfangen, während Gerät die
  neue Firmware in den Flash schreibt;
  Übergabe: ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist
  Rückgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (Empfangsdaten)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  sTerminator: string;

begin
  Result:=false;
  if not SendCommand ('END' + CR, ['r', 'E', 'P'], 1, ATimeout, ad_String,
                      Rueckgabe, NoCarrier) then exit;

  sTerminator:=Copy (Rueckgabe.Antwort, length (Rueckgabe.Antwort), 1);  // das letzte Zeichen in der Antwort
  if sTerminator = 'r' then begin  // OK, Flashen ist fertig
    Result:=true;
  end
  else if sTerminator = 'E' then begin  // Fehler beim Löschen eines Flash-Blocks
    Rueckgabe.Fehlergruppe:=EST_FIRMWAREUPDATEERROR;
    Rueckgabe.Fehlercode:=FIRMWUPDERR_FLASH_ERASE;
  end
  else if sTerminator = 'P' then begin  // Fehler beim Programmieren eines Flash-Blocks
    Rueckgabe.Fehlergruppe:=EST_FIRMWAREUPDATEERROR;
    Rueckgabe.Fehlercode:=FIRMWUPDERR_FLASH_PROG;
  end
  else begin  // Undefinierter Fehler
    Rueckgabe.Fehlergruppe:=EST_FIRMWAREUPDATEERROR;
    Rueckgabe.Fehlercode:=FIRMWUPDERR_FLASH_UNDEF;
  end;
end;


{ TDSfGCommObj }

{-------------------------------------------------------------------------------}
constructor TDSfGCommObj.Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
{-------------------------------------------------------------------------------}
begin
  inherited Create (AWorkPath, AComLogFile);

  SetStandardVersuche;                             { Standard-Versuche setzen }

  EndeZeichenEmpfangen:=false;
  BCC:=0;
  BCCDSfG:='';
  Versuche:=0;
  Extensionmode:=0;
  FCommError:=0;

  CRCMRG:='';

  FGPRSRemoteAddress:='';
  FGPRSRemotePort:=0;
  FGPRSData_Ausgabe:=nil;

  FPrefix_RMGDfueBefehl:='';  // Default: Für Befehle per serieller Verbindung kein Prefix

  FQuittung:=nil;
end;

{-----------------------------------------}
procedure TDSfGCommObj.SetStandardVersuche;
{-----------------------------------------}
{ Standard-Versuche setzen }
begin
  Versuche_BCC:=CDSfG_BCCVersuche;
end;

{----------------------------------------------------------}
procedure TDSfGCommObj.SetVersuche (AVersuche_BCC: integer);
{----------------------------------------------------------}
{ Standard-Versuche mit übergebenen Werten belegen }
begin
  Versuche_BCC:=AVersuche_BCC;
end;

{----------------------------------------------}
function TDSfGCommObj.GetMaxExtensionmode: byte;
{----------------------------------------------}
{ höchsten Erweiterungsgrad, den das Modul bedienen kann, zurückgeben }
begin
  Result:=CMaxDSfGExtensionmode;
end;

{---------------------------------------------------------}
function TDSfGCommObj.SetExtensionmode (Value: byte): byte;
{---------------------------------------------------------}
{ DSfG-DFÜ-Erweiterungsgrad setzen, mit dem der Abruf erfolgen soll;
  Ergebnis: gesetzter Erweiterungsgrad }
begin
  if Value > CMaxDSfGExtensionmode then
    Extensionmode:=CMaxDSfGExtensionmode
  else
    Extensionmode:=Value;
   Result:=Extensionmode;
end;

{----------------------------------}
procedure TDSfGCommObj.BefehlSenden;
{----------------------------------}
begin
  EndeZeichenEmpfangen:=false;
  BCC:=0;
  BCCDSfG:='';

  { TxD/RxD-Anzeige: }
  if Assigned (CBTxD) then
    CBTxD (SonderzeichenString (Befehl), IntToStr (length (Befehl)) + ' Byte');
  if Assigned (CBRxD) then
    CBRxD ('', '');
  Application.ProcessMessages;

  if Comm_SendData (Befehl) then  // Befehl senden
    if (length (Befehl) > 0) then begin
      if (FComLogFile <> nil) then
        FComLogFile.Write ('S', Befehl);  // Logfileeintrag mit Kopf "Sendedaten"
      if (FComTraceLog <> nil) then
        FComTraceLog.Write ('S', Befehl);  // Tracelogeintrag mit Kopf "Sendedaten"; 24.02.2020, WW
    end;

  if AnswerDest = ad_File then
    RohFileName:=CreateTempRohFile (WorkPath, prefix_DSfG_Roh);

  Timeout_Init;  // Timeout-Überwachung initialisieren; 19.12.2022, WW

  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout (IntToStr(Timeout DIV 1000) + ' s');
end;

{--------------------------------------------------------------------------------}
function TDSfGCommObj.SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                                   AEndezeichenAnzahl: integer;
                                   ATimeout: integer;
                                   AAnswerDest: TAnswerDest;
                                   var Rueckgabe: TRueckgabe;
                                   var NoCarrier: boolean;
                                   AnzCharsToReceive: cardinal = 0): boolean;
{--------------------------------------------------------------------------------}
{ Befehl senden und Antwort empfangen;
  Übergabe: Befehl-String
            in der Antwort zu erwartende Endezeichen
            Anzahl, der in der Antwort zu erwartenden Endezeichen
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist
            AAnswerDest (DestString: Empfangsdaten -> Antwort-String
                         Destfile: Empfangsdaten -> Rohfile
            optional:
            AnzCharsToReceive feste Anzahl an zu empfangenen Zeichen (wenn > 0 wird
                              nach den übergebenen n Zeichen der Zeichenempfang mit
                              OK beendet, unabhängig vom übergebenen Endezeichen)
  Rückgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AnswerDest = DestString: Empfangsdaten
                          AnswerDest = Destfile: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  S: string;
  AntwortEmpfangen: boolean;
  EndeZeichenPos: integer;
  First: boolean;
  EndezeichenCount: integer;
  i: integer;
  cQuittung: char;
  ChToRec_Left: cardinal;
  AntwortTelegrammTyp: TAntwortTelegrammTyp;
  PushTelegramme: string;  // Push-Daten (GPRS)
  OnePushTelegramm: string;
  sBuf: string;
  sSysTimePW: string;
  iLenPrefix_RMGDfueBefehl: integer;
  sBefBeforePW: string;
  sBefAfterPW: string;
  ohneSOH: boolean;
  sLog: string;

  {--------------------------------------}
  function ReInit_AntwortEmpfang: boolean;
  {--------------------------------------}
  { Empfang für Antwortdaten reinitialisieren }
  begin
    Result:=true;
    Rueckgabe.Antwort:='';
    if AnswerDest = ad_File then begin
      if not ClearRohfile (RohfileName) then begin
        Rueckgabe.Fehlergruppe:=ST_FILEERROR;
        Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
        Result:=false;
      end;
    end;
    EndeZeichenEmpfangen:=false;
    EndezeichenCount:=0;
    AntwortTelegrammTyp:=at_unbekannt;
    First:=true;
  end;

begin
  Timeout:=ATimeout;
  AnswerDest:=AAnswerDest;
  Versuche:=1;
  EndezeichenCount:=0;

  // Default für Quittungszeichen: leer; 13.03.2018, WW
  if Assigned (CBQuittung) then begin
    if ExtensionMode >= 1 then
      CBQuittung (Format (SEmpfangeneQuittung, [SKeine]))
    else
      CBQuittung ('');
  end;

  S:=ABefehl;
  { ab Erweiterungsgrad 1: alle Befehle werden in 'STX Befehl ETX BCC' gepackt }
  if (ExtensionMode >= 1) AND (length (S) > 0) then begin  // 20.07.2012, WW
    { ...außer Quittierungen einer emulierten DSfG-DFÜ an Zentrale; 05.08.2011, WW }
    if (S <> ACK) AND (S <> ENQ) AND (S <> CAN) AND (S <> NAK) then begin
      S:=STX + S + ETX;
      S:=S + GetBCC_Chars (GetBCC (0, S));
    end;
  end
  else if (ExtensionMode = 0) AND (length (S) > 0) then begin
    { für DSfG-DFÜ-Befehle mit Systemzeit-Passwort (NG); 22.02.2012, WW }
    if FWithSysTimePW then begin
      sBuf:=ExtractString (S, STX, ETX, 0);  { Befehlszeichen zwischen STX und ETX }
      sSysTimePW:=CalcSysTimePW;  { Systemzeit-Passwort berechnen }
      iLenPrefix_RMGDfueBefehl:=length (FPrefix_RMGDfueBefehl);

      sBefBeforePW:=Copy (sBuf, 1, iLenPrefix_RMGDfueBefehl + 1);
      sBefAfterPW:=Copy (sBuf, iLenPrefix_RMGDfueBefehl + 2, length (sBuf));
      // s bzw. YWs-Befehl OHNE SOH bilden: 17.07.2012, WW
      ohneSOH:=Copy (sBefBeforePW, length (sBefBeforePW), 1) = 's';
      if ohneSOH then
        S:=STX + sBefBeforePW + sSysTimePW + sBefAfterPW + ETX
      else
        S:=STX + sBefBeforePW + sSysTimePW + SOH + sBefAfterPW + ETX;
    end;
  end;

  Befehl:=S;
  BefehlSenden;                                           { Befehl abschicken }

  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:=''
  end;

  { Antwort auslesen: }
  if Timeout > 0 then begin               { bei Timeout > 0: Antwort auslesen }
    PushTelegramme:='';
    AntwortTelegrammTyp:=at_unbekannt;
    cQuittung:=NUL;
    ChToRec_Left:=AnzCharsToReceive;
    AntwortEmpfangen:=false;
    FCommError:=0;
    First:=true;
    while not AntwortEmpfangen AND (FCommError = 0) do begin
      Sleep (1);
      Application.ProcessMessages;
      Timeout_Elapsed;  // Verstrichene Zeit messen für Timeout-Überwachung; 19.12.2022, WW

      { Verbindung prüfen: }
      if not Comm_Connection (Rueckgabe) then begin
        // Unterbrochene Verbindung loggen; 18.02.2021, WW
        sLog:='Verbindung unterbrochen';
        if (Rueckgabe.Fehlergruppe = COM_MODEMERROR) AND
           (Rueckgabe.Fehlercode = CME_DCD) then
          sLog:=sLog + ' (kein DCD-Signal)';
          
        if FComLogFile <> nil then
          FComLogFile.WriteMsg (sLog);  // Logfileeintrag
        if FComTraceLog <> nil then
          FComTraceLog.WriteMsg (sLog);  // Tracelogeintrag

        NoCarrier:=true;
        Break;
      end;

      { Antwort lesen: }
      s:=Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left);
      if length (s) > 0 then begin
        { Anzahl der zu empfangenden Zeichen updaten: }
        if ChToRec_Left >= cardinal(length (s)) then
          ChToRec_Left:=ChToRec_Left - cardinal(length (s))
        else
          ChToRec_Left:=0;

        if First then begin                     { die ersten ausgelesenen Zeichen }
          First:=false;
          if FComLogFile <> nil then
            FComLogFile.Write ('E', S);  // Logfileeintrag mit Kopf "Empfangsdaten"
          if FComTraceLog <> nil then
            FComTraceLog.Write ('E', S);  // Tracelogeintrag mit Kopf "Empfangsdaten"; 24.02.2020, WW

          { RxD-Anzeige: }
          if Assigned (CBRxD) then
            CBRxD (SonderzeichenString (s), IntToStr (length (s)) + ' Byte');

          { Überprüfung der Quittierung, wenn Befehl ab Erweiterungsgrad 1 gesendet wurde: }
          if (Extensionmode >= 1) AND (length (Befehl) > 0) then begin  // 20.07.2012, WW
            cQuittung:=s[1];

            // Quittungszeichen im Klartext; 13.03.2018, WW
            if Assigned (CBQuittung) then
              CBQuittung (Format (SEmpfangeneQuittung, [SonderzeichenString (cQuittung)]));

            case cQuittung of
              ACK: begin                       { positive Quittung: Telegramm ist richtig übertragen und versendet worden }
                     Delete (s, 1, 1);      { Quittierungszeichen für Antwort rauslöschen }
                   end;

              ENQ: begin                     { negative Quittung: Telegramm kann nicht versendet werden, da formal falsch }
                     with Rueckgabe do begin
                       Fehlergruppe:=ST_DSFGERROR;
                       Fehlercode:=DSFGERR_ENQ;
                       Antwort:='';
                     end;
                     Break;
                   end;

              CAN: begin     { negative Quittung: Telegramm kann nicht versendet werden, da Zielteinehmer nicht vorhanden }
                     with Rueckgabe do begin
                       Fehlergruppe:=ST_DSFGERROR;
                       Fehlercode:=DSFGERR_CAN;
                       Antwort:='';
                     end;
                     Break;
                   end;

              NAK: begin  { negative Quittung: Telegramm kann nicht versendet werden, da nicht innerhalb TS übermittelbar }
                     with Rueckgabe do begin
                       Fehlergruppe:=ST_DSFGERROR;
                       Fehlercode:=DSFGERR_NAK;
                       Antwort:='';
                     end;
                     Break;
                   end;
            end;  { case }
          end;  { if Extensionmode }
        end  { if First }
        else begin
          if FComLogFile <> nil then
            FComLogFile.Write ('D', s);  // Logfileeintrag nur Daten
          if FComTraceLog <> nil then
            FComTraceLog.Write ('D', s);  // Tracelogeintrag nur Daten; 24.02.2020, WW

          { RxD-Anzeige: }
          if Assigned (CBRxD) then
            CBRxD (SonderzeichenString (s), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
        end;

        Rueckgabe.Antwort:=Rueckgabe.Antwort + s;
        if AnswerDest = ad_File then begin
          if not WriteRohfile (RohfileName, s) then begin
            Rueckgabe.Fehlergruppe:=ST_FILEERROR;
            Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
            Break;
          end;
        end;

        { Prüfung auf Push-Daten (GPRS): }
        if AntwortTelegrammTyp = at_unbekannt then begin  { nur wenn Antwort noch nicht identifiziert ist }
          AntwortTelegrammTyp:=CheckForPushData (Rueckgabe.Antwort);
          if AntwortTelegrammTyp = at_sonstige then begin
            if (Extensionmode >= 1) AND (length (Befehl) > 0) AND (cQuittung <> ACK) then begin
              // 20.07.2012, WW: ab Erweiterungsgrad 1
              with Rueckgabe do begin
                Fehlergruppe:=ST_DSFGERROR;
                Fehlercode:=DSFGERR_QUITTUNGUNDEFINIERT;  { unbekanntes Quittierungszeichen }
                Antwort:='';
              end;
              Break;
            end;
          end;
        end;

        { Timeout-Zähler nicht bei einlaufenden Push-Daten rücksetzen: }
        if AntwortTelegrammTyp <> at_PushData_MRG900 then begin
          TimeoutCount:=0;
          { Timeout-Anzeige: }
          if Assigned (CBTimeout) then
            CBTimeout ('');
        end;

        if AnzCharsToReceive > 0 then begin  { wenn feste Anzahl an zu empfangenden Zeichen übergeben wurde }
          if ChToRec_Left <= 0 then      { ...und alle Zeichen empfangen wurden }
            AntwortEmpfangen:=true;    { ...fertig ! }
        end
        else begin
          if AntwortTelegrammTyp = at_PushData_MRG900 then begin
            // es wurde ein Push-Telegramm eines MRG 905/910 erkannt
            // -> Push-Telegramm kommt bei DSfG-Abruf immer OHNE CRC (ist halt so):
            if not EndeZeichenEmpfangen then begin
              EndeZeichenPos:=Pos (ETX, s);
              if EndeZeichenPos <> 0 then  { Endezeichen in Puffer enthalten }
                EndeZeichenEmpfangen:=true;
            end;

            if EndeZeichenEmpfangen then begin
              if Assigned (CBGPRSData_Ausgabe) then  // Ausgabe GPRS-Daten
                CBGPRSData_Ausgabe (Rueckgabe.Antwort, FGPRSRemoteAddress, FGPRSRemotePort);

              while length (Rueckgabe.Antwort) > 0 do begin
                OnePushTelegramm:=FCutOnePushTelegramm (Rueckgabe.Antwort, mrgtyp_MRG910);  { ein Push-Telegramm ausschneiden }
                { Push-Telegramm in Telegrammliste eintragen: }
                AddPushTelegramToList (OnePushTelegramm, mrgtyp_MRG910);
              end;  { while length (Rueckgabe.Antwort) > 0 }

              { Empfang für weitere Antwortdaten reinitialisieren: }
              if not ReInit_AntwortEmpfang then Break;
            end;
          end

          else if (Extensionmode >= 1) AND (AEndezeichenAnzahl = 1) then begin  { mit BCC-Prüfung; 17.09.2008 WW }
            // 20.07.2012, WW: ab Erweiterungsgrad 1
            if not EndeZeichenEmpfangen then begin
              BCC:=GetBCC (BCC, s);                          { BCC berechnen }
              EndeZeichenPos:=Pos (ETX, s);
              if EndeZeichenPos <> 0 then begin { Endezeichen in Puffer enthalten }
                EndeZeichenEmpfangen:=true;
                if (length (s) >= EndeZeichenPos + 1) then begin     { und mind. eines der beiden BCC-Zeichen auch }
                  BCCDSfG:=Copy (s, EndeZeichenPos + 1, 2);
                  if length (BCCDSfG) >= 2 then begin                { alle beiden BCC-Zeichen sind enthalten }
                    AntwortEmpfangen:=EndOfBCCTransmission (Rueckgabe);
                    First:=true;
                  end;
                end;
              end;
            end
            else begin   { s enthält ein oder beide BCC-Zeichen, evtl. auch weitere spontan gesendete Zeichen }
              BCCDSfG:=BCCDSfG + s;
              BCCDSfG:=Copy (BCCDSfG, 1, 2);
              if length (BCCDSfG) >= 2 then begin                { alle beiden BCC-Zeichen sind enthalten }
                AntwortEmpfangen:=EndOfBCCTransmission (Rueckgabe);
                First:=true;
              end;
            end;

            { evtl. anhängendes Push-Telegramm aus Antwort kopieren: }
            if AntwortEmpfangen then begin
              PushTelegramme:=ExtractString (Rueckgabe.Antwort, ETX, NUL, 0);   { ab ETX }
              PushTelegramme:=Copy (PushTelegramme, 3, length (PushTelegramme));  { und ohne 2 BCC-Zeichen }

              { falls Push-Telegramm vorliegt, Rohfile mit erwarteter Antwort
                neu schreiben (es enthält am Ende die Push-Daten !): }
              if (AnswerDest = ad_File) AND (length (PushTelegramme) > 0) then begin
                sBuf:=Copy (Rueckgabe.Antwort, 1,
                            length (Rueckgabe.Antwort) - length (PushTelegramme));
                if not WriteRohfile (RohfileName, sBuf, true) then begin
                  Rueckgabe.Fehlergruppe:=ST_FILEERROR;
                  Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
                  Break;
                end;
              end;
            end;
          end
          else begin                                           { ohne BCC-Prüfung }
            for i:=1 to length (s) do
              if s[i] in AEndezeichen then       { Endezeichen in Puffer enthalten }
                inc (EndezeichenCount);
            AntwortEmpfangen:=EndezeichenCount >= AEndezeichenAnzahl;

            { evtl. anhängendes Push-Telegramm aus Antwort kopieren: }
            if AntwortEmpfangen then begin
              PushTelegramme:=ExtractString (Rueckgabe.Antwort, ETX, NUL, AEndezeichenAnzahl - 1);  // ab n-tem ETX

              { falls Push-Telegramm vorliegt, Rohfile mit erwarteter Antwort
                neu schreiben (es enthält am Ende die Push-Daten !): }
              if (AnswerDest = ad_File) AND (length (PushTelegramme) > 0) then begin
                sBuf:=Copy (Rueckgabe.Antwort, 1,
                            length (Rueckgabe.Antwort) - length (PushTelegramme));
                if not WriteRohfile (RohfileName, sBuf, true) then begin
                  Rueckgabe.Fehlergruppe:=ST_FILEERROR;
                  Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
                  Break;
                end;
              end;
            end;
          end;
        end;
      end   { length (s) > 0 }
      else begin
        if TimeoutCount >= Timeout then begin       { Timeout beim Datenempfang }
          with Rueckgabe do begin
            Fehlergruppe:=COM_KOMMERROR;
            Fehlercode:=KOMMERR_TIMEOUT;
            { bei Timeout Antwort nicht löschen }
          end;
          Break;
        end;
      end;
    end; { while }

    { Timeout-Anzeige: }
    if Assigned (CBTimeout) then
      CBTimeout ('');

    { TxD/RxD-Anzeige: }
    if Assigned (CBRxD) then
      CBRxD (SonderzeichenString (Rueckgabe.Antwort), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
    Application.ProcessMessages;

    if AnswerDest = ad_File then
      Rueckgabe.Antwort:=RohFileName;         { Rückgabe: Antwort = Rohfilename }

    if FCommError <> 0 then begin              { Kommunikationsfehler ist aufgetreten }
      { Rückgabe-Record mit Kommunikationsfehler belegen: }
      Comm_SetFehlerRueckgabe (NoCarrier, Rueckgabe);
      { Empfangspuffer leeren: }
      Comm_ClearRecBuf;
    end
    else begin  // Kommunikation OK
      { Prüfung auf evtl. anhängende Push-Telegramme: }
      if length (PushTelegramme) > 0 then begin
        if CheckForPushData (PushTelegramme) = at_PushData_MRG900 then begin
          if Assigned (CBGPRSData_Ausgabe) then  // Ausgabe GPRS-Daten
            CBGPRSData_Ausgabe (PushTelegramme, FGPRSRemoteAddress, FGPRSRemotePort);

          while length (PushTelegramme) > 0 do begin
            OnePushTelegramm:=FCutOnePushTelegramm (PushTelegramme, mrgtyp_MRG910);  { ein Push-Telegramm ausschneiden }
            { Push-Telegramm in Telegrammliste eintragen: }
            AddPushTelegramToList (OnePushTelegramm, mrgtyp_MRG910);
          end;  { while length (PushTelegramme) > 0 }
        end;
      end;
    end;

    if (Rueckgabe.Fehlergruppe <> 0) AND (AnswerDest = ad_File) then  { Rohfile löschen bei Fehler }
      DeleteFile (RohFileName);
  end;  { if Timeout > 0 }
  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{------------------------------------------------------------------------------}
function TDSfGCommObj.EndOfBCCTransmission (var Rueckgabe: TRueckgabe): boolean;
{------------------------------------------------------------------------------}
{ Empfangene Daten prüfen;
  Rückgabe: Rueckgabe-Record
  Ergebnis: true, wenn Datenübertragung abgeschlossen (keine Wiederholung) }
var
  sLog: string;

begin
  if GetBCC_Chars (BCC) <> BCCDSfG then begin         { BCC-Fehler in der Antwort }
    sLog:='BCC falsch !';
    if FComLogFile <> nil then
      FComLogFile.WriteMsg (sLog);  // Logfileeintrag
    if FComTraceLog <> nil then
      FComTraceLog.WriteMsg (sLog);  // Tracelogeintrag; 24.02.2020, WW

    Rueckgabe.Antwort:='';
    if AnswerDest = ad_File then
      DeleteFile (RohFileName);

    if length (Befehl) > 0 then begin    { bei Leer-Befehl macht eine Wiederholung keinen Sinn }
      inc (Versuche);
      if Versuche <= Versuche_BCC then begin
        BefehlSenden;                        { nochmal gleichen Befehl senden }
        Result:=false;
        exit;
      end;
    end;

    { erst nach dem letzten Versuch Rueckgabe mit BCC-Fehler belegen (23.05.2003, WW): }
    with Rueckgabe do begin
      Fehlergruppe:=COM_KOMMERROR;
      Fehlercode:=KOMMERR_BCC;
    end;
  end;
  Result:=true;
end;

{------------------------------------------------------------}
function TDSfGCommObj.InitializeSysTimePW (ATimeout: integer;
  var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
{------------------------------------------------------------}
{ Systemzeit-Passwort initialisieren: Ermitteln der Differenz
  zwischen Geräte- und PC-Zeit in Sekunden
  Übergabe: ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
  Rückgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Systemzeit-Passwort initialisiert werden konnte }
var
  sBefehl: string;
  sSysTime: string;

begin
  Result:=false;
  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;

  sBefehl:=FPrefix_RMGDfueBefehl + '1B00';
  sBefehl:=STX + sBefehl + ETX;

  { Kommando zum Auslesen der Systemzeit senden }
  if not SendCommand (sBefehl, [ETX], 1, ATimeout, ad_String, Rueckgabe, NoCarrier) then exit;

  { Antwort auf Systemzeitlese-Kommando auswerten: }
  if not ValidDSfGDfueAntwort (true, Rueckgabe.Antwort, Rueckgabe.Fehlergruppe,
                               Rueckgabe.Fehlercode) then exit;

  sSysTime:=ExtractString (Rueckgabe.Antwort, STX, ETX, 0);
  sSysTime:=Copy (sSysTime, 5, length (sSysTime));  // 1B00 wegschneiden
  sSysTime:=ExtractString (sSysTime, NUL, US, 0);  // Systemzeit bis zum US
  CalcSysTimeDiff (sSysTime);  { Systemzeit-Differenz zwischen Gerät und PC berechnen }

  Result:=true;
end;


{ TMRGCommObj }

{------------------------------------------------------------------------------}
constructor TMRGCommObj.Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
{------------------------------------------------------------------------------}
begin
  inherited Create(AWorkPath, AComLogFile);

  DelayCommand:=0;       { Standard: keine Wartezeit vor einem Befehl }
  DelayCommandChar:=0;   { Standard: keine Wartezeit zwischen den Befehlszeichen }
  Reset_COM_Before_Sending:=true;  { Standard: mit COM-Reset vor jedem Sendebefehl }
end;

{--------------------------------------------------------------------}
procedure TMRGCommObj.BefehlSenden (ATempBefehl: string = '';
                                    AComLogHexDaten: boolean = false);
{--------------------------------------------------------------------}
{ Befehl senden;
  Übergabe: Temporärer Befehl (wenn nicht <leer> wird der temporäre Befehl statt
                               dem im Objekt hinterlegten Befehl gesendet)
            Flag: COM-Daten als Hex loggen ja/nein }
var
  i: integer;
  ABefehl: string;

begin
  { RxD-Anzeige: }
  if Assigned (CBRxD) then
    CBRxD ('', '');

  if length (ATempBefehl) = 0 then
    ABefehl:=Befehl  { den im Objekt abgelegten Befehl senden (Standard) }
  else
    ABefehl:=ATempBefehl;  { den übergebenen temporären Befehl senden; 26.09.2008, WW }

  { Befehl in Sendepuffer schreiben: }
  if length (ABefehl) > 0 then begin
    if DelayCommand > 0 then                         { Wartezeit vor dem Befehl }
      Delay (DelayCommand);

    if Reset_COM_Before_Sending then
      Comm_Reset;  // Reset der Schnittstelle

    if DelayCommandChar > 0 then begin  { mit Wartezeit zwischen den Zeichen }
      for i:=1 to length (ABefehl) do begin
        Comm_SendChar (ABefehl[i]);  // einzelnes Befehlszeichen senden
        if i < length (Befehl) then
          Delay (DelayCommandChar);
      end;
    end
    else  { ohne Wartezeit zwischen den Zeichen }
      Comm_SendData (ABefehl);  // Befehl senden

    { TxD-Anzeige: }
    if Assigned (CBTxD) then
      CBTxD (SonderzeichenString (ABefehl), IntToStr (length (ABefehl)) + ' Byte');

    if FComLogFile <> nil then
      FComLogFile.Write ('S', ABefehl, -1, AComLogHexDaten);  { Logfileeintrag mit Kopf "Sendedaten",
                                                                Hex-Daten per Flag; 08.03.2019, WW }
    if FComTraceLog <> nil then
      FComTraceLog.Write ('S', ABefehl, -1, AComLogHexDaten);  { Tracelogeintrag mit Kopf "Sendedaten",
                                                                 Hex-Daten per Flag; 03.01.2022, WW }
  end
  else begin
    { TxD-Anzeige: }
    if Assigned (CBTxD) then
      CBTxD ('', '');
  end;

  { Wenn ein temporärer Befehl gesendet wird, werden keine Aktionen eingeleitet,
    um eine Antwort zu verarbeiten: }
  if length (ATempBefehl) = 0 then begin
    if AnswerDest = ad_File then
      RohFileName:=CreateTempRohFile (WorkPath, prefix_MRG_Roh);

    if Timeout > 0 then begin
      { Timeout-Anzeige: }
      if Assigned (CBTimeout) then
        CBTimeout (IntToStr(Timeout DIV 1000) + ' s');
    end;
  end;

  Timeout_Init;  // Timeout-Überwachung initialisieren; 19.12.2022, WW

  Application.ProcessMessages;
end;

{-----------------------------------------------------------------------------------------}
function TMRGCommObj.SendCommandList (ABefehlListe: TBefehlList;
                                      AStartIndex_Befehlliste: integer;
                                      AEndezeichen: TEndezeichenSet;
                                      AEndezeichenAnzahl: integer;
                                      ATimeout: integer; AAnswerDest: TAnswerDest;
                                      var Rueckgabe: TRueckgabe;
                                      var NoCarrier: boolean;
                                      var AktIndex_Befehlliste: integer;
                                      bTeilantwort_bei_Fehler: boolean = false): boolean;
{-----------------------------------------------------------------------------------------}
{ Liste von Befehlen nacheinander senden und Antworten empfangen;
  -> alle Antworten auf die einzelnen Befehle werden aneinandergehängt
  Übergabe: Befehlsliste
            Index des ersten zu sendenden Befehls der Befehlsliste
            in den Antworten zu erwartendes Endezeichen
            Anzahl, der in jeder Antworten zu erwartenden Endezeichen
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
            Flag 'bTeilantwort_bei_Fehler': wenn false, werden bei einem Abruffehler
              alle bereits erfolgreich gelesenen Teilantworten gelöscht
  Rückgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
            Index des zuletzt gesendeten Befehls der Befehlsliste
  Ergebnis: true, wenn alle Antworten korrekt gelesen werden konnten }
var
  i: integer;
  GesamtAntwort: string;
  ABefehl: string;
  ABefehlInfo: string;

begin
  Result:=false;
  Rueckgabe.Fehlergruppe:=0;
  Rueckgabe.Fehlercode:=0;
  AktIndex_Befehlliste:=AStartIndex_Befehlliste;

  GesamtAntwort:='';
  if AAnswerDest = ad_File then
    RohFileName:=CreateTempRohFile (WorkPath, prefix_MRG_Roh);

  { alle in der Liste enthaltenen Befehle abrufen: }
  if AStartIndex_Befehlliste >= 0 then begin
    for i:=AStartIndex_Befehlliste to ABefehlListe.Count - 1 do begin
      ABefehl:=TBefehlDataObj (ABefehlListe [i]).Daten.sBefehl;
      AktIndex_Befehlliste:=i;  // aktueller Befehlslistenindex für Rückgabe
      if not SendCommand (ABefehl, AEndezeichen, AEndezeichenAnzahl, ATimeout, ad_String,
                          Rueckgabe, NoCarrier) then begin
        if bTeilantwort_bei_Fehler then begin
          if AAnswerDest = ad_File then
            Rueckgabe.Antwort:=RohfileName
          else
            Rueckgabe.Antwort:=GesamtAntwort;
        end
        else begin
          if AAnswerDest = ad_File then
            DeleteFile (RohFileName);
        end;
        exit;
      end;

      { Prüfen, ob eine Zusatzinfo zum Befehl vorhanden ist: }
      ABefehlInfo:=TBefehlDataObj (ABefehlListe [i]).Daten.sInfo;
      if length (ABefehlInfo) > 0 then
        Rueckgabe.Antwort:=ABefehlInfo + Rueckgabe.Antwort;   // Zusatzinfo der Antwort voranstellen

      if AAnswerDest = ad_File then begin
        if not WriteRohfile (RohfileName, Rueckgabe.Antwort) then begin
          DeleteFile (RohFileName);
          Rueckgabe.Fehlergruppe:=ST_FILEERROR;
          Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
          exit;
        end;
      end else
        GesamtAntwort:=GesamtAntwort + Rueckgabe.Antwort;
    end;  { for }
  end;  { if AStartIndex_Befehlliste >= 0 }

  if AAnswerDest = ad_File then
    Rueckgabe.Antwort:=RohfileName
  else
    Rueckgabe.Antwort:=GesamtAntwort;
  Result:=true;
end;

end.

