{**************************************************************************************}
{* Unit: MRG-Kommunikation mit Modem �ber serielle Schnittstelle                      *}
{* 05.12.2000 WW                                                                      *}
{* 06.03.2002 WW  mit SendModemCommand-Methode                                        *}
{* 06.09.2002: CRC-Pr�fung verfeinert wg. Abrufproblem EC694  GD/SM/WW                *}
{* 11.03.2003 WW  Timeout- und Versuche-Variablen in Klasse integriert (bisher global)*}
{* 23.06.2003 WW  Erweiterungen f�r Abruf von KE-Anlagen                              *}
{* 13.01.2011 GD  BCC-Fehler bei VC3 umgangen                                         *}
{* 09.08.2011 WW  neue Klasse TMRGSerialCommObj                                       *}
{* 09.11.2011 WW  BCC-Fehler, wenn Startzeichen STX, SOH nicht vorhanden (EK260, RWE) *}
{* 30.04.2012 WW  Zus�tzliche Rohdaten-Plausibilisierung in SendIEC1107DatenTelegramm *}
{*                f�r Elster IEC1107-Datenabfrage                                     *}
{* 23.05.2012 WW  Nochmaliger Bugfix BCC-Fehler bei VC3 (Schmutzzeichen "Space" nach  *}
{*                dem BCC-Zeichen)                                                    *}
{* 19.06.2012 WW  SendMRGCommand: Variablen f�r Empfang der Antwort reinitialisiert   *}
{*                nach CRC-Fehler und Timeout                                         *}
{* 07.11.2012 WW  Bugfix Rohdatenplausibilisierung bei Fehlertelegramm in SendIEC1107-*}
{*                DatenTelegramm                                                      *}
{* 28.05.2013 WW  R�ckgabe Fehlercode 'GetLastError', wenn ClearCommError fehlschl�gt *}
{* 13.12.2017 WW  SendMRGCommand, Abrufgruppe 11: Korrektur 8. Bit in den 7-Bit-      *}
{*                Ger�tedaten l�schen (f�r UNIGAS 300)                                *}
{* 25.02.2019 WW  SendMRGCommand, Abrufgruppe 8: Korrektur 8. Bit in den 7-Bit-       *}
{*                Ger�tedaten l�schen (f�r Tritschler IEC-Ger�te (MC2, VC3 etc.) mit  *}
{*                neuem Ethernet-Modem                                                *}
{* 08.03.2019 WW  mit Modbus-Protokoll, Abrufgruppe 14 (Primus)                       *}
{* 30.04.2019 WW  mit FTL-intern-Protokoll, Abrufgruppe 15 (Parametrierung VCx)       *}
{* 18.02.2021 WW  Unterbrochene Verbindung loggen                                     *}
{* 03.01.2022 WW  mit ComTracelog                                                     *}
{* 19.12.2022 WW  Timeout-�berwachung: Timer durch TickCount ersetzt                  *}
{**************************************************************************************}
unit SerMrgModem;

interface

uses
  Windows, SysUtils, Forms, Classes, O_Comm, ErrConst, T_Zeit, T_Tools,
  LogCom, WChars, WStrUtils, Serial, CRC16, MValidAnswer, MDatensatzList, WComm,
  CrcSealink, AbrufTimeoutConst, WSysCon, T_BinMask, GPRS_Util, CommUtil, MrgBefehl,
  ModbusUtil;

const
  { Standard-L�nge der Parameternummer f�r Wieser MRG (C-Befehl): }
  C_DefaultParaNrLen_MRG = 3;

type
  { Timeouts f�r MRG-Modem-Kommunikation }

  TMRGModemTimeouts = record
    ModemAntwort: integer;
    CRCCheck: integer;
    ACK01ProtMeldung: integer;
  end;

  { Fehler-Versuche f�r MRG-Modem-Kommunikation }

  TMRGModemVersuche = record
    CRC: integer;
    BCC: integer;
    ACK01Prot: integer;
    FTLProt: integer;  // Tritschler FTL-Protokoll
    ModbusProtLRC_CRC: integer;  // Modbus-Protokoll: LRC (ASCII), CRC (RTU) 
  end;

  { ACK01-Protokollstati }

  TACK01ProtokollStatus = (
    ps_ACK,                       { ACK 0/1 empfangen }
    ps_NAK,                       { NAK empfangen }
    ps_ENQ,                       { ENQ empfangen }
    ps_EOT,                       { EOT empfangen }
    ps_DLE_EOT,                   { DLE EOT empfangen }
    ps_SOH,                       { SOH empfangen (MRG-Daten) }
    ps_ETB,                       { ein Datenblock (von mehreren) gesendet }
    ps_ETX,                       { letzter Datenblock gesendet }
    ps_BCCError,                  { Blockcheck-Fehler in Empfangsdaten }
    ps_UNGUELTIG,                 { ung�ltige Daten empfangen }
    ps_UNVOLLSTAENDIG,            { unvollst�ndige Daten empfangen }
    ps_UNDEFINIERT);              { Vorbelegung }

  { Checksummen }

  TChecksum = (
    cs_Off,        { ohne Checksumme }
    cs_BCC,        { mit BCC }
    cs_CRC_Corus   { mit CRC f�r Actaris Corus }
  );


  { Basis-Objekt f�r MRG-Kommunikation �ber Modem, TCP/IP }

  TMRGCustomCommObj = class(TMRGCommObj)
  private
    { Private-Deklarationen }
    Timeouts: TMRGModemTimeouts;
    Versuche: TMRGModemVersuche;
    EndeZeichenEmpfangen: boolean;
    FAbrufgruppe: integer;
    DelayWakeCommand: integer;

    WithCRC: boolean;       { Flag, ob Senden und Empfangen mit CRC erfolgt (nur Wieser MRG) }
    WithPasswort: boolean;  { Flag, ob Passwort in MRG-Befehlen enthalten ist }
    WithVersuche: boolean;  { Flag, ob das Senden von MRG-Befehlen bei Fehler
                              mehrmals versucht werden soll }
    SendWithChecksum: TChecksum; { Flag, ob Befehl mit Pr�fsumme versendet wird (Fremdger�te) }
    ReceiveWithChecksum: TChecksum;  { Flag, ob Antwort mit Pr�fsumme erwartet wird (Fremdger�te) }
    WithDatasizeCheck: boolean;  { Flag, ob in Antwort enthaltene Information
                                   �ber Datengr��e ausgewertet werden soll (Fremdger�te) }
    FParaNrLen: integer;  { L�nge der MRG-Parameternummer (f�r C-Befehl) }
    Passwort: string;
    CRC: word;
    CRCMRG: string;
    CRCLen: byte;  { L�nge des CRC-Strings }
    Versuch: byte;
    FReInitProtokoll: boolean;

    ACK01CommModus: TACK01CommModus;   { ACK01-Kommunikationsmodus: MRG oder KE-Anlage }
    ACK01ProtokollStatus: TACK01ProtokollStatus;
    ACKAlternate: string [2];  { ACK 0/1: Best�tigung f�r g�ltigen Datenblock }

    SealinkLogfileName: string;
    { f�r GPRS-Kommunikation in abgeleiteten Klassen: }
    FGPRSData_Ausgabe: TCBGPRSDataProc;

    procedure SetStandardTimeouts_Versuche;
    procedure InitEinstellungen;
    function Befehl_nochmal (MaxVersuche: integer; var Rueckgabe: TRueckgabe): boolean;
    function EndOfCRCTransmission (var Rueckgabe: TRueckgabe): boolean;
    function CheckMRGAntwort (var Rueckgabe: TRueckgabe): boolean;
    function GetBefehl (ABefehl: string): string;
    procedure WakeUpMRG_ByCommand;
    procedure WakeUpMRG_ByBreak; virtual;
    function SendACK01Command (ABefehl: string;
                               ATimeout: integer; AAnswerDest: TAnswerDest;
                               var Rueckgabe: TRueckgabe;
                               var NoCarrier: boolean): boolean;
    function SendProtokoll (ABefehl: string; ATimeout: integer; AAnswerDest: TAnswerDest;
                            var Rueckgabe: TRueckgabe): boolean; virtual;
    function ReceiveProtokoll (var Rueckgabe: TRueckgabe;
                               var NoCarrier: boolean;
                               MaxAnzCharsToReceive: cardinal = 0): boolean;
    procedure FilterProtokollDaten (var Daten: string);
    function ValidBlockCheck (Daten: string): boolean;
    procedure SetACK01ProtokollStatus (Antwort: string);
    function GetACK01ProtokollStatus (AStatus: TACK01ProtokollStatus): boolean;
    procedure ACK01ProtokollStatusToFehlerGruppeCode (var Fehlergruppe: integer;
                                                      var Fehlercode: integer);

    function Check_BCC (var Rueckgabe: TRueckgabe; iBlankCharCount: word): boolean; // 13.01.2011
    function GetBCC (Start: byte; s: string): byte;

    function CheckKE_Datenheader_Ruhrgastyp (HeaderStr: string;
                                             var DataSize: word;
                                             var CheckSum: word): boolean;

    function CheckFTLBlocksum (Antwort: string; PosK23: integer): boolean;

    function GetFTL_internBlocksum (S: string): string;
    function CheckFTL_internBlocksum (Antwort: string): boolean;

    function Antwort_quittierenDS100 (cQuittung: char; bAntwort_erwartet: boolean;
      ATimeout: integer; var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
    function GetMOD256Checksum (S: string): string;

    function Check_ModbusLRC_CRC (var Rueckgabe: TRueckgabe): boolean;
    function EndOfModbusResponse (sResponse: string): boolean;

    procedure CorrectAscii8Bit (var S: string);
  protected
    FCommError: cardinal;
    { f�r GPRS-Kommunikation in abgeleiteten Klassen: }
    FGPRSRemoteAddress: string;
    FGPRSRemotePort: integer;
    { Modbus: }
    FModbusModus: TModbusModus;

    function Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal;
                               bSaveFirstCommError: boolean): string; virtual; abstract;
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; virtual; abstract;
    procedure Comm_ClearRecBuf; virtual; abstract;
    procedure Comm_SetFehlerRueckgabe (NoCarrier: boolean; var Rueckgabe: TRueckgabe); virtual; abstract;
    procedure BefehlSenden (ATempBefehl: string = '';
                            AComLogHexDaten: boolean = false); override;
  public
    { Public-Deklarationen }
    constructor Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
    procedure SetTimeouts (ATimeouts: TMRGModemTimeouts);
    procedure SetVersuche (AVersuche: TMRGModemVersuche);
    procedure SetAbrufgruppe (Gruppe: integer; sDataFormat: string = ''); virtual;
    procedure SetWithCRC (OnOff: boolean);
    function GetWithCRC: boolean;
    procedure SetWithPasswort (OnOff: boolean);
    function GetWithPasswort: boolean;
    procedure SetPasswort (APasswort: string);
    procedure SetWithVersuche (OnOff: boolean);
    procedure SetSendWithChecksum (AChecksum: TChecksum);
    procedure SetReceiveWithChecksum (AChecksum: TChecksum);
    function GetReceiveWithChecksum: TChecksum;
    procedure SetWithDatasizeCheck (OnOff: boolean);
    procedure SetLaengeParaNr (AParaNrLen: integer);  // 29.02.2012, WW

    function MRG_CRCKonfig_Check (var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
    function SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                          AEndezeichenAnzahl: integer;
                          ATimeout: integer; AAnswerDest: TAnswerDest;
                          var Rueckgabe: TRueckgabe;
                          var NoCarrier: boolean;
                          AnzCharsToReceive: cardinal = 0): boolean; override;
    function SendMRGCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                             AEndezeichenAnzahl: integer;
                             ATimeout: integer; AAnswerDest: TAnswerDest;
                             var Rueckgabe: TRueckgabe;
                             var NoCarrier: boolean;
                             AnzCharsToReceive: cardinal = 0;
                             ATimeoutFirstRec: integer = 0): boolean; virtual;
    function SendIEC1107DatenTelegramm (ABefehl: string;
                                        AEndezeichen: TEndezeichenSet;
                                        AAnswerDest: TAnswerDest;
                                        ATimeout: integer; AChecksum: TChecksum;
                                        var Rueckgabe: TRueckgabe;
                                        var NoCarrier: boolean;
                                        bRohdatenPlausibilisieren: boolean): boolean;
    function SendTritschlerIECCommand (ABefehl: string; AAnswerDest: TAnswerDest;
                                       ATimeout: integer; AMrgTyp: integer;
                                       bMitWecken: boolean;
                                       var Rueckgabe: TRueckgabe;
                                       var NoCarrier: boolean): boolean;
    function SendTritschlerFTLCommand (ABefehl: string; ATimeout: integer;
                                       bPosQuittierung: boolean;
                                       var Rueckgabe: TRueckgabe;
                                       var NoCarrier: boolean): boolean;
    function SendTritschlerFTLFunktionCommand (ABefehl: string;
                                               AAnswerDest: TAnswerDest;
                                               ATimeout: integer;
                                               AVonDatum: TDateTime;
                                               var Rueckgabe: TRueckgabe;
                                               var NoCarrier: boolean): boolean;
    function SendTritschlerFTL_internCommand (ABefehl: string;
                                              ATimeout: integer;
                                              var Rueckgabe: TRueckgabe;
                                              var NoCarrier: boolean): boolean;
    function SendKEDatentelegramm_S (ABefehl: string; AAnswerDest: TAnswerDest;
                                     ATimeout: integer;
                                     var Rueckgabe: TRueckgabe;
                                     var NoCarrier: boolean): boolean;
    function SendKEDatentelegramm_T (ABefehl: string; AAnswerDest: TAnswerDest;
                                     ATimeout: integer;
                                     var Rueckgabe: TRueckgabe;
                                     var NoCarrier: boolean): boolean;
    function SendElsterDS100Command (ABefehl: string; AAnswerDest: TAnswerDest;
                                     ATimeout: integer;
                                     var Rueckgabe: TRueckgabe;
                                     var NoCarrier: boolean): boolean;
    function SetDS100Parameter(cBefehlscode: char; sWert: string;
      ATimeout: integer; var bErfolgreich: boolean; var Rueckgabe: TRueckgabe;
      var NoCarrier: boolean): boolean;
    function SendModbusQuery (ABefehl: string; ATimeout: integer;
      var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;

    property Abrufgruppe: integer read FAbrufgruppe;
    property ModbusModus: TModbusModus read FModbusModus write FModbusModus;
    property ReInitProtokoll: boolean write FReInitProtokoll;
    property CBGPRSData_Ausgabe: TCBGPRSDataProc read FGPRSData_Ausgabe write
      FGPRSData_Ausgabe;
  end;


  { Basis-Objekt f�r MRG-Kommunikation �ber die serielle Schnittstelle }

  TMRGSerialCustomCommObj = class(TMRGCustomCommObj)
  private
    { Private-Deklarationen }
    FSerial: TSerial;
    procedure WakeUpMRG_ByBreak; override;
  protected
    procedure Comm_Reset; override;
    function Comm_SendData (sData: string): boolean; override;
    function Comm_SendChar (c: char): boolean; override;
    function Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal;
                               bSaveFirstCommError: boolean): string; override;
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; override;
    procedure Comm_ClearRecBuf; override;
    procedure Comm_SetFehlerRueckgabe (NoCarrier: boolean; var Rueckgabe: TRueckgabe); override;
  public
    { Public-Deklarationen }
    constructor Create (AOwner: TComponent; AWorkPath: TFileName;
                        AComLogFile: TComLogFile);
    destructor Destroy; override;
    function Connect (AComPort: integer; ABaudrate: integer; ADatabits: TDataBits;
                      AParityBit: TParityBit; AStopBits: TStopBits): integer;
    procedure SetAbrufgruppe (Gruppe: integer; sDataFormat: string = ''); override;
    function Rufabfrage (var Ruf_angekommen: boolean;
                         var Fehlergruppe: integer; var Fehlercode: integer): boolean; override;
    property Serial: TSerial read FSerial;
  end;


  { Objekt f�r MRG-Kommunikation �ber die serielle Schnittstelle }

  TMRGSerialCommObj = class(TMRGSerialCustomCommObj);


  { Objekt f�r MRG-Kommunikation mit Modem �ber die serielle Schnittstelle }

  TMRGModemCommObj = class(TMRGSerialCustomCommObj)
  private
    { Private-Deklarationen }
    FModemstatus: TCBStatusMsgProc;
    DCDCheck: boolean;                   { Flag, ob DCD-Signal �berwacht wird }
    function SendProtokoll (ABefehl: string; ATimeout: integer; AAnswerDest: TAnswerDest;
                            var Rueckgabe: TRueckgabe): boolean; override;
  protected
    function Comm_Connection (var Rueckgabe: TRueckgabe): boolean; override;
    procedure Comm_SetFehlerRueckgabe (NoCarrier: boolean; var Rueckgabe: TRueckgabe); override;
  public
    { Public-Deklarationen }
    MaxModemBaudrate: TBaudrate;  { max. Baudrate PC <-> Modem; das Modem sollte so
                                    initialisiert sein, da� die telefonseitige Baudrate
                                    sich an die rechnerseitige anpa�t }
    constructor Create (AOwner: TComponent; AWorkPath: TFileName;
                        AComLogFile: TComLogFile);
    procedure SetDCDCheck (OnOff: boolean);
    function SendModemCommand (ABefehl: string; ATimeout: integer;
                               var Rueckgabe: TRueckgabe;
                               WaitFor_OK_ERROR: boolean = false): boolean;
    function SendMRGCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                             AEndezeichenAnzahl: integer;
                             ATimeout: integer; AAnswerDest: TAnswerDest;
                             var Rueckgabe: TRueckgabe;
                             var NoCarrier: boolean;
                             AnzCharsToReceive: cardinal = 0;
                             ATimeoutFirstRec: integer = 0): boolean; override;
    function Rufabfrage (var Ruf_angekommen: boolean;
                         var Fehlergruppe: integer; var Fehlercode: integer): boolean; override;
    property CBModemstatus: TCBStatusMsgProc read FModemstatus write FModemstatus;
  end;

implementation

uses
  DateUtils;

Const
{ F�r den Modem-Abruf eines MRG sind folgende Abrufgruppen in der Tabelle
  'MRGDEF.DB', Feld 'ModemAbrufgruppe' definiert:

  Abrufgruppe 1: mit ACK01-Protokoll (MRGs, die mit FUP abgerufen werden k�nnen
                 und mit 2400DX laufen, z.B. MRG 2100V1); 7, e, 1
  Abrufgruppe 2: normales Protokoll ohne CRC o.�. (MRG 800, MRG 2001); 7, e, 1
  Abrufgruppe 3: normales Protokoll mit/ohne CRC, mit/ohne Passwort im Befehl,
                 Wartezeit zwischen den Zeichen des Sendebefehls etc. (MRG 800 PTB);
                 8, n, 1
  Abrufgruppe 4: wie Abrufgruppe 3, jedoch ohne Befehls-Wartezeiten, kein Break
                 zum Wecken (EC 694, MRG 910); 8, n, 1
  Abrufgruppe 5: IEC 1107-Protokoll (Elster DL240, EK260); 7, e, 1
  Abrufgruppe 6: KE-Anlagen (modifiziertes ACK01-Protokoll: Sende-/Empfangsdaten ohne
                 vorangestellten Stern, andere BCC-Bildung etc. ); 8, n, 1
  Abrufgruppe 7: Datacon FWU mit vereinfachtem IEC 1107-Protokoll; 8, n, 1
  Abrufgruppe 8: Tritschler-Ger�te mit IEC-Protokoll (VC2, TTG, TDS, SSU); 7, e, 1
  Abrufgruppe 9: Tritschler-Ger�te mit FTL-Protokoll (TTG); 7, n, 2
  Abrufgruppe 10: Actaris Corus (SAM); 8, n, 1
  Abrufgruppe 11: IEC 1107-Protokoll (Actaris Sparklog, Kamstrup UNIGAS 300); 7, e, 1
  Abrufgruppe 12: M900-Protokoll (EC 900); �hnlich Abrufgruppe 4, jedoch: Befehle
                  immer mit Passwort und mit SOH statt Space als Passwort-Trenner; 8, n, 1
  Abrufgruppe 13: Elster DS-100-Protokoll; 8, n, 1
  Abrufgruppe 14: Modbus RTU-Protokoll (RMG Primus/Prilog 400, TME400, RSM200); 8, n, 1
  Abrufgruppe 15: Standard 8, n, 1 (ohne sonstigen Schnickschnack); z.B. f�r
                  FTL-intern-Protokoll zur Parametrierung von Tritschler VCx

  Die in der Klasse verwendete Abrufgruppe 0 wird f�r die Modem-Kommunikation mit
  at-Befehlen verwendet (Modem-Initialisierung, Verbindungsaufbau, Rufentgegennahme etc.). }

  CMaxAbrufgruppe = 15;

  CDelayAllgCommand = 30;      { Standard-Wartezeit in ms vor jedem Befehl (sonst funktioniert
                                 das ke LOGEM LGM9600 nicht richtig !) }
  CDelayWakeCommand_Modem   =  200;  { Wartezeit nach Senden des Weckbefehls bei Modemabruf }
  CDelayWakeCommand_Seriell = 2000;  { Wartezeit nach Senden des Weckbefehls bei seriellem Abruf }
    { -> ca. 1 s bis Antwort auf Weckbefehl vom Ger�t kommt plus ca. 1 s bis Ger�t
         nach gesendeter Antwort empfangsbereit ist ! }

  { spezielle Konstanten f�r MRG 800PTB }

  CDelayPTBCommandChar = 130;  { Wartezeit in ms zwischen den Befehlszeichen }
  CDelayPTBCommand     = 400;  { Wartezeit in ms vor jedem Befehl, um die Mindestzeit
                                 zwischen dem letzten Antwortzeichen und
                                 dem 1. Zeichen des n�chsten Befehls einzuhalten }

  { spezielle Konstanten f�r Protokoll-Abruf: }

  CDelayACK01CommandChar   = 100;  { Wartezeit in ms zwischen den Befehlszeichen }
  CDelayACK01Command       = 100;  { Wartezeit in ms vor jedem MRG-Befehl }
  CACK01Blocklaenge        = 256;  { max. L�nge eines zu sendenden Datenblocks (STX Datenblock ETX) }
  CACK01ProtokollLen_MRG   =  13;  { Anzahl der Protokoll-Zeichen vor STX und nach ETX (MRG) }
  CACK01ProtokollLen_KE    =  12;  { ein Zeichen weniger bei KE (vorangestellter Stern fehlt) }
  CACK01MaxQuittungLen_MRG =   4;  { maximale Anzahl der Quittungs-Zeichen (MRG) }
  CACK01MaxQuittungLen_KE  =   3;  { ein Zeichen weniger bei KE (vorangestellter Stern fehlt) }

  CLenKE_Datenheader_Ruhrgastyp = 6;     { L�nge des Datenkopfes (Ruhrgas-Datenformat) }

  ACK0 = ACK + '0';
  ACK1 = ACK + '1';


{ TMRGCustomCommObj }

{------------------------------------------------------------------------------------}
constructor TMRGCustomCommObj.Create (AWorkPath: TFileName; AComLogFile: TComLogFile);
{------------------------------------------------------------------------------------}
begin
  inherited Create(AWorkPath, AComLogFile);

  SetStandardTimeouts_Versuche;      { Standard-Timeouts und -Versuche setzen }
  EndeZeichenEmpfangen:=false;
  CRC:=0;
  CRCMRG:='';
  CRCLen:=4;  // Standard: L�nge des CRC-Strings wie in Wieser-Ger�ten (4-stellig Hex)
  Versuch:=0;
  Passwort:='';

  SetAbrufgruppe (0);                      { Vorbelegung: Modem-Kommunikation }

  ACK01CommModus:=cm_MRG;   { Standard-ACK01-Kommunikationsmodus: mit MRG }
  ACK01ProtokollStatus:=ps_UNDEFINIERT;
  ACKAlternate:=ACK0;

  FCommError:=0;
  SealinkLogfileName:='';

  FGPRSRemoteAddress:='';
  FGPRSRemotePort:=0;
  FGPRSData_Ausgabe:=nil;

  DelayWakeCommand:=0;

  FModbusModus:=modbus_RTU;  // 08.03.2019, WW
  FReInitProtokoll:=false;  // 30.04.2019, WW
end;

{-------------------------------------------------------}
procedure TMRGCustomCommObj.SetStandardTimeouts_Versuche;
{-------------------------------------------------------}
{ Standard-Timeouts und -Versuche setzen }
begin
  Timeouts.ModemAntwort:=CTimeout_ModemAntwort;
  Timeouts.CRCCheck:=CMRG_Timeout_CRCCheck;
  Timeouts.ACK01ProtMeldung:=CMRG_Timeout_ACK01ProtMeldung;

  Versuche.CRC:=CMRG_CRCVersuche;
  Versuche.BCC:=CMRG_BCCVersuche;
  Versuche.ACK01Prot:=CMRG_ACK01ProtVersuche;
  Versuche.FTLProt:=CMRG_FTLProtVersuche;
  Versuche.ModbusProtLRC_CRC:=CModbus_LRC_CRCVersuche;  // 08.03.2019, WW
end;

{---------------------------------------------------------------------}
procedure TMRGCustomCommObj.SetTimeouts (ATimeouts: TMRGModemTimeouts);
{---------------------------------------------------------------------}
{ Timeouts mit �bergebenen Werten belegen }
begin
  Timeouts.ModemAntwort:=ATimeouts.ModemAntwort;
  Timeouts.CRCCheck:=ATimeouts.CRCCheck;
  Timeouts.ACK01ProtMeldung:=ATimeouts.ACK01ProtMeldung;
end;

{---------------------------------------------------------------------}
procedure TMRGCustomCommObj.SetVersuche (AVersuche: TMRGModemVersuche);
{---------------------------------------------------------------------}
{ Versuche mit �bergebenen Werten belegen }
begin
  Versuche.CRC:=AVersuche.CRC;
  Versuche.BCC:=AVersuche.BCC;
  Versuche.ACK01Prot:=AVersuche.ACK01Prot;
  Versuche.FTLProt:=AVersuche.FTLProt;
  Versuche.ModbusProtLRC_CRC:=AVersuche.ModbusProtLRC_CRC;
end;

{-------------------------------------------------------------------------------------}
procedure TMRGCustomCommObj.SetAbrufgruppe (Gruppe: integer; sDataFormat: string = '');
{-------------------------------------------------------------------------------------}
{ Abrufgruppe setzen, f�r die der Abruf erfolgen soll;
  �bergaben: Abrufgruppe }
begin
  if (Gruppe >= 0) AND (Gruppe <= CMaxAbrufgruppe) then
    FAbrufgruppe:=Gruppe
  else
    FAbrufgruppe:=0;

  { Standard-Belegungen setzen: }
  InitEinstellungen;

  { spezielle Belegungen je nach Abrufgruppe: }
  case FAbrufgruppe of
    1: begin  { "FUP-Ger�te" (ACK01-Protokoll) }
         DelayCommand:=CDelayACK01Command;
         DelayCommandChar:=CDelayACK01CommandChar;
         ACK01CommModus:=cm_MRG;  { ACK01-Kommunikation mit MRG }
       end;
    3: begin  { MRG 800PTB }
         DelayCommand:=CDelayPTBCommand;
         DelayCommandChar:=CDelayPTBCommandChar;
       end;
    6: begin  { KE-Anlagen }
         ACK01CommModus:=cm_KE;  { Modus: ACK01-Kommunikation mit KE-Anlage }
       end;
    7: begin  { Datacon FWU }
         SetReceiveWithChecksum (cs_BCC);  { Empfangsdaten mit BCC }
       end;
    8: begin  { Tritschler-Ger�te mit IEC-Protokoll (VC2, TTG) }
         SetReceiveWithChecksum (cs_BCC);  { Empfangsdaten mit BCC }
       end;
   10: begin  { Actaris Corus }
         CRCLen:=2;  // CRC-String 2-stellig (Low-Byte, High-Byte)
       end;
  end;
end;

{--------------------------------------------}
procedure TMRGCustomCommObj.InitEinstellungen;
{--------------------------------------------}
{ Standard-Einstellungen f�r Kommunikation mit einem MRG setzen }
begin
  DelayCommand:=CDelayAllgCommand;
  DelayCommandChar:=0;
  SetWithCRC (false);
  SetWithPasswort (false);
  SetPasswort ('');
  SetWithVersuche (false);
  SetSendWithChecksum(cs_Off);
  SetReceiveWithChecksum(cs_Off);
  SetWithDatasizeCheck (false);
  ACKAlternate:=ACK0;
  SetLaengeParaNr (C_DefaultParaNrLen_MRG);  // 29.12.2012, WW
end;

{------------------------------------------------------}
procedure TMRGCustomCommObj.SetWithCRC (OnOff: boolean);
{------------------------------------------------------}
{ CRC-Behandlung in Befehl/Antwort einschalten (OnOff = true) bzw. ausschalten (OnOff = false) }
begin
  WithCRC:=OnOff;
end;

{---------------------------------------------}
function TMRGCustomCommObj.GetWithCRC: boolean;
{---------------------------------------------}
begin
  Result:=WithCRC;
end;

{-----------------------------------------------------------}
procedure TMRGCustomCommObj.SetWithPasswort (OnOff: boolean);
{-----------------------------------------------------------}
{ Passwort in Befehl einf�gen (OnOff = true) bzw. nicht einf�gen (OnOff = false) }
begin
  WithPasswort:=OnOff;
end;

{--------------------------------------------------}
function TMRGCustomCommObj.GetWithPasswort: boolean;
{--------------------------------------------------}
begin
  Result:=WithPasswort;
end;

{----------------------------------------------------------}
procedure TMRGCustomCommObj.SetPasswort (APasswort: string);
{----------------------------------------------------------}
{ Ger�te-Passwort �bergeben }
begin
  Passwort:=APasswort;
end;

{-----------------------------------------------------------}
procedure TMRGCustomCommObj.SetWithVersuche (OnOff: boolean);
{-----------------------------------------------------------}
{ Senden von MRG-Befehlen bei Fehler wiederholen (OnOff = true, max. Anzahl = CRCVersuche)
  bzw. nicht wiederholen (OnOff = false) }
begin
  WithVersuche:=OnOff;
end;

{---------------------------------------------------------------------}
procedure TMRGCustomCommObj.SetSendWithChecksum (AChecksum: TChecksum);
{---------------------------------------------------------------------}
{ Befehl mit Checksumme (BCC, CRC) bzw. ohne versenden }
begin
  SendWithChecksum:=AChecksum;
end;

{------------------------------------------------------------------------}
procedure TMRGCustomCommObj.SetReceiveWithChecksum (AChecksum: TChecksum);
{------------------------------------------------------------------------}
{ Antwort mit Checksumme (BCC, CRC) bzw. ohne erwarten }
begin
  ReceiveWithChecksum:=AChecksum;
end;

{-----------------------------------------------------------}
function TMRGCustomCommObj.GetReceiveWithChecksum: TChecksum;
{-----------------------------------------------------------}
{ ReceiveWithChecksum-Flag zur�ckgeben }
begin
  Result:=ReceiveWithChecksum;
end;

{----------------------------------------------------------------}
procedure TMRGCustomCommObj.SetWithDatasizeCheck (OnOff: boolean);
{----------------------------------------------------------------}
{ in Antwort enthaltene Information �ber Datengr��e auswerten (OnOff = true) }
begin
  WithDatasizeCheck:=OnOff;
end;

{----------------------------------------------------------------}
procedure TMRGCustomCommObj.SetLaengeParaNr (AParaNrLen: integer);  // 29.02.2012, WW
{----------------------------------------------------------------}
{ L�nge der Parameternummer setzen (f�r C-Befehl) }
begin
  FParaNrLen:=AParaNrLen;
end;

{--------------------------------------------------------------------------}
procedure TMRGCustomCommObj.BefehlSenden (ATempBefehl: string = '';
                                          AComLogHexDaten: boolean = false);
{--------------------------------------------------------------------------}
begin
  EndeZeichenEmpfangen:=false;
  CRC:=0;
  CRCMRG:='';
  inherited BefehlSenden (ATempBefehl, AComLogHexDaten);
end;

{----------------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                                        AEndezeichenAnzahl: integer;
                                        ATimeout: integer; AAnswerDest: TAnswerDest;
                                        var Rueckgabe: TRueckgabe; var NoCarrier: boolean;
                                        AnzCharsToReceive: cardinal = 0): boolean;
{----------------------------------------------------------------------------------------}
begin
  case FAbrufgruppe of
    { wenn Abrufgruppe 0 definiert ist (z.B. f�r at-Befehle ans Modem):
      Befehle senden/empfangen wie f�r Abrufgruppe 2 (ohne CRC und sonstigen Schnickschnack)
      -> ab 06.03.2002 eigene SendModemCommand-Methode eingef�hrt }
    0, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 15:
      Result:=SendMRGCommand (ABefehl, AEndezeichen, AEndezeichenAnzahl,
                              ATimeout, AAnswerDest, Rueckgabe, NoCarrier,
                              AnzCharsToReceive);
    1, 6:
      Result:=SendACK01Command (ABefehl, ATimeout, AAnswerDest, Rueckgabe, NoCarrier);

    13:
      Result:=SendElsterDS100Command (ABefehl, AAnswerDest, ATimeout, Rueckgabe, NoCarrier);
  else
    Result:=false;
  end;
end;

{----------------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendMRGCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                                           AEndezeichenAnzahl: integer;
                                           ATimeout: integer; AAnswerDest: TAnswerDest;
                                           var Rueckgabe: TRueckgabe;
                                           var NoCarrier: boolean;
                                           AnzCharsToReceive: cardinal = 0;
                                           ATimeoutFirstRec: integer = 0): boolean;
{----------------------------------------------------------------------------------------}
{ MRG-Befehl senden und Antwort empfangen;
  �bergabe: Befehl-String
            in der Antwort zu erwartendes Endezeichen
            Anzahl, der in der Antwort zu erwartenden Endezeichen
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
            optional:
            AnzCharsToReceive feste Anzahl an zu empfangenen Zeichen (wenn > 0 wird
                              nach den �bergebenen n Zeichen der Zeichenempfang mit
                              OK beendet, unabh�ngig vom �bergebenen Endezeichen);
                              ab 24.06.2003 WW
            ATimeoutFirstRec (max. Wartezeit bis zum ersten empfangenen Zeichen)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  S: string;
  AntwortEmpfangen: boolean;
  EndeZeichenPos: integer;
  First: boolean;
  EndezeichenCount: integer;
  i: cardinal;
  SleepCount: integer;
  ChToRec_Left: cardinal;
  StartZeichenPos: integer;
  Datenlaenge: integer;
  EndezeichenPos_Soll: integer;
  AnzZeichenGelesen: integer;
  AntwortTelegrammTyp: TAntwortTelegrammTyp;
  PushTelegramme: string;  // Push-Daten (GPRS)
  OnePushTelegramm: string;
  sBuf: string;
  bComLogHexDaten: boolean;
  sLog: string;

  {----------------------------------------------------}
  function ReInit_AntwortEmpfang_AfterPushData: boolean;
  {----------------------------------------------------}
  { Empfang f�r Antwortdaten reinitialisieren nach Pushdaten-Empfang }
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

    EndezeichenCount:=0;
    AntwortTelegrammTyp:=at_unbekannt;
    First:=true;

    EndeZeichenEmpfangen:=false;
    CRC:=0;
    CRCMRG:='';
  end;

  {------------------------------}
  procedure ReInit_AntwortEmpfang;  // 19.06.2012, WW
  {------------------------------}
  { Empfang f�r Antwortdaten reinitialisieren }
  begin
    EndezeichenCount:=0;
    AntwortTelegrammTyp:=at_unbekannt;
    PushTelegramme:='';
    ChToRec_Left:=AnzCharsToReceive;
    StartZeichenPos:=0;
    EndezeichenPos_Soll:=0;
    AnzZeichenGelesen:=0;
    First:=true;
  end;

begin
  if ATimeoutFirstRec > 0 then
    Timeout:=ATimeoutFirstRec  { Timeout f�r erstes empfangenes Zeichen }
  else
    Timeout:=ATimeout;  { allg. Empfangsdaten-Timeout }

  AnswerDest:=AAnswerDest;
  Versuch:=1;

  bComLogHexDaten:=FAbrufgruppe in [14];  // Modbus-Daten als Hex loggen; 08.03.2019, WW

  Befehl:=GetBefehl (ABefehl);                                { Befehl bilden }
  BefehlSenden ('', bComLogHexDaten);                     { Befehl abschicken }

  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;

  if ATimeout > 0 then begin  { bei Empfangsdaten-Timeout > 0: Antwort auslesen }
    ReInit_AntwortEmpfang;

    AntwortEmpfangen:=false;
    FCommError:=0;
    SleepCount:=0;
    while not AntwortEmpfangen AND (FCommError = 0) do begin
      inc (SleepCount);
      { Sleep verz�gert Senden des n�chsten Befehls, daher nur bei jedem 10. Durchlauf aufrufen !
        -> wirkt sich v.a. bei beim Datentelegramm-Abruf mit IEC 1107-Protokoll
           (st�ndiges Best�tigen mit ACK) aus }
      if (SleepCount MOD 10) = 0 then begin
        Sleep (1);
        SleepCount:=0;
      end;
      Application.ProcessMessages;
      Timeout_Elapsed;  // Verstrichene Zeit messen f�r Timeout-�berwachung; 19.12.2022, WW

      { Verbindung pr�fen; 03.06.2020, WW }
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
      s:=Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left, true);
      if length (s) > 0 then begin
        Timeout:=ATimeout;  { wenn Zeichen empfangen wurden, wird der allg. Timeout f�r Empfangsdaten �berwacht }

        if (FAbrufgruppe = 8) OR     // f�r Tritschler IEC-Ger�te (MC2, VC3 etc.) mit neuem Ethernet-Modem; 25.02.2019, WW
           (FAbrufgruppe = 11) then  // f�r UNIGAS 300; 13.12.2017, WW
          CorrectAscii8Bit (s);  { Korrektur: 8. Bit in den 7-Bit-Ger�tedaten l�schen

        { Anzahl der zu empfangenden Zeichen updaten: }
        if ChToRec_Left >= cardinal(length (s)) then
          ChToRec_Left:=ChToRec_Left - cardinal(length (s))
        else
          ChToRec_Left:=0;

        if First then begin                   { die ersten ausgelesenen Zeichen }
          First:=false;
          if FComLogFile <> nil then
            FComLogFile.Write ('E', s, -1, bComLogHexDaten);  { Logfileeintrag mit Kopf "Empfangsdaten",
                                                                Hex-Daten per Flag; 08.03.2019, WW }
          if FComTraceLog <> nil then
            FComTraceLog.Write ('E', s, -1, bComLogHexDaten);  { Tracelogeintrag mit Kopf "Empfangsdaten",
                                                                 Hex-Daten per Flag; 03.01.2022, WW }
          { RxD-Anzeige: }
          if Assigned (CBRxD) then
            CBRxD (SonderzeichenString (s), IntToStr (length (s)) + ' Byte');
        end  { if First }
        else begin
          if FComLogFile <> nil then
            FComLogFile.Write ('D', s, -1, bComLogHexDaten);  { Logfileeintrag nur Daten,
                                                                Hex-Daten per Flag; 08.03.2019, WW }
          if FComTraceLog <> nil then
            FComTraceLog.Write ('D', s, -1, bComLogHexDaten);  { Tracelogeintrag nur Daten,
                                                                 Hex-Daten per Flag; 03.01.2022, WW }
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

        { Pr�fung auf Push-Daten (GPRS) bei Abrufgruppe 4, 12 (MRG 910, EC 900 etc.): }
        if (FAbrufgruppe = 4) OR (FAbrufgruppe = 12) then
          if AntwortTelegrammTyp = at_unbekannt then  { nur wenn Antwort noch nicht identifiziert ist }
            AntwortTelegrammTyp:=CheckForPushData (Rueckgabe.Antwort);

        { Timeout-Z�hler nicht bei einlaufenden Push-Daten r�cksetzen: }
        if AntwortTelegrammTyp <> at_PushData_MRG900 then begin
          TimeoutCount:=0;
          { Timeout-Anzeige: }
          if Assigned (CBTimeout) then
            CBTimeout ('');
        end;

        if AnzCharsToReceive > 0 then begin  { wenn feste Anzahl an zu empfangenden Zeichen �bergeben wurde }
          if ChToRec_Left <= 0 then      { ...und alle Zeichen empfangen wurden }
            AntwortEmpfangen:=true;    { ...fertig ! }
        end
        else if WithCRC OR (ReceiveWithChecksum = cs_CRC_Corus) then begin  { mit CRC-Pr�fung }
          if not EndeZeichenEmpfangen then begin

            if WithDatasizeCheck then begin  { in Antwort enthaltene Datengr��e-Info auswerten }
              case FAbrufgruppe of
                10: begin  { Actaris Corus }
                      // Position des Startzeichens in der Antwort ermitteln, wenn noch nicht erfolgt:
                      if StartzeichenPos = 0 then
                        StartzeichenPos:=Pos (SOH, Rueckgabe.Antwort);

                      // Datenl�ngen-Byte auswerten, wenn noch nicht erfolgt:
                      if (StartzeichenPos > 0) AND (EndezeichenPos_Soll = 0) AND
                         (length (Rueckgabe.Antwort) > StartZeichenPos) then begin
                        Datenlaenge:=Ord(Rueckgabe.Antwort[StartZeichenPos + 1]);
                        EndezeichenPos_Soll:=StartzeichenPos + Datenlaenge + 2;
                      end;
                    end;
              end;  { case }
            end;

            for i:=1 to length (s) do begin
              { CRC berechnen: }
              case CRCLen of
                2: CRC:=scrc16_Corus (s[i], CRC);  // CRC-Berechnung Actaris Corus
                4, 12: CRC:=scrc16 (s[i], CRC);  // CRC-Berechnung Wieser/RMG MRG
              end;

              if s[i] in AEndezeichen then begin  { Endezeichen in Puffer enthalten }
                case FAbrufgruppe of
                  10: begin  { Actaris Corus }
                        { NAK-Antwort erfolgt immer ohne CRC }
                        if (s[i] = NAK) AND (Pos (NAK, Rueckgabe.Antwort) = 1) then begin
                          AntwortEmpfangen:=true;
                          Break;
                        end;
                      end;
                end;  { case }

                { wenn Soll-Position des Endezeichens ermittelt wurde: mit
                  Ist-Position vergleichen, m�ssen �bereinstimmen ! }
                EndeZeichenPos:=i;
                if (EndezeichenPos_Soll = 0) OR
                   ((EndezeichenPos_Soll > 0) AND
                    ((AnzZeichenGelesen + EndezeichenPos) >= EndezeichenPos_Soll)) then begin
                  EndeZeichenEmpfangen:=true;
                  if (length (s) >= EndeZeichenPos + 1) then begin  { und mind. eines der CRC-Zeichen auch }
                    CRCMRG:=Copy (s, EndeZeichenPos + 1, CRCLen);
                    if length (CRCMRG) >= CRCLen then begin  { alle CRC-Zeichen sind enthalten }
                      if AntwortTelegrammTyp = at_PushData_MRG900 then begin
                        if Assigned (CBGPRSData_Ausgabe) then  // Ausgabe GPRS-Daten
                          CBGPRSData_Ausgabe (Rueckgabe.Antwort, FGPRSRemoteAddress, FGPRSRemotePort);

                        while length (Rueckgabe.Antwort) > 0 do begin
                          OnePushTelegramm:=FCutOnePushTelegramm (Rueckgabe.Antwort, mrgtyp_MRG910);  { ein Push-Telegramm ausschneiden }
                          { Push-Telegramm in Telegrammliste eintragen: }
                          AddPushTelegramToList (OnePushTelegramm, mrgtyp_MRG910);
                        end;  { while length (Rueckgabe.Antwort) > 0 }

                        { Empfang f�r weitere Antwortdaten reinitialisieren: }
                        if not ReInit_AntwortEmpfang_AfterPushData then Break;
                      end
                      else begin
                        AntwortEmpfangen:=EndOfCRCTransmission (Rueckgabe);
                        if not AntwortEmpfangen then
                          ReInit_AntwortEmpfang  // 19.06.2012, WW
                      end;
                    end;
                  end;

                  Break;
                end;
              end;
            end;  { for i }
          end
          else begin                   { s enth�lt ein oder mehrere CRC-Zeichen }
            CRCMRG:=CRCMRG + s;
            if length (CRCMRG) >= CRCLen then begin  { alle CRC-Zeichen sind enthalten }
              if AntwortTelegrammTyp = at_PushData_MRG900 then begin
                if Assigned (CBGPRSData_Ausgabe) then  // Ausgabe GPRS-Daten
                  CBGPRSData_Ausgabe (Rueckgabe.Antwort, FGPRSRemoteAddress, FGPRSRemotePort);

                while length (Rueckgabe.Antwort) > 0 do begin
                  OnePushTelegramm:=FCutOnePushTelegramm (Rueckgabe.Antwort, mrgtyp_MRG910);  { ein Push-Telegramm ausschneiden }
                  { Push-Telegramm in Telegrammliste eintragen: }
                  AddPushTelegramToList (OnePushTelegramm, mrgtyp_MRG910);
                end;  { while length (Rueckgabe.Antwort) > 0 }

                { Empfang f�r weitere Antwortdaten reinitialisieren: }
                if not ReInit_AntwortEmpfang_AfterPushData then Break;
              end
              else begin
                AntwortEmpfangen:=EndOfCRCTransmission (Rueckgabe);
                if not AntwortEmpfangen then
                  ReInit_AntwortEmpfang  // 19.06.2012, WW
              end;
            end;
          end;

          { evtl. anh�ngendes Push-Telegramm aus Antwort kopieren: }
          if ((FAbrufgruppe = 4) OR (FAbrufgruppe = 12)) AND AntwortEmpfangen then begin
            PushTelegramme:=ExtractString (Rueckgabe.Antwort, ETX, NUL, 0);   { ab ETX }
            PushTelegramme:=Copy (PushTelegramme, CRCLen + 1, length (PushTelegramme));  { und ohne die CRC-Zeichen }

            { falls Push-Telegramm vorliegt, Rohfile mit erwarteter Antwort
              neu schreiben (es enth�lt am Ende die Push-Daten !): }
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

        else if (ReceiveWithChecksum = cs_BCC) then begin  { BCC pr�fen in Antwort }
          if not EndeZeichenEmpfangen then begin
            EndeZeichenPos:=0;
            for i:=1 to length (s) do begin
              if s[i] in AEndezeichen then begin  { Endezeichen in Puffer enthalten }
                EndeZeichenPos:=i;
                Break;
              end;
            end;

            if EndeZeichenPos <> 0 then begin  { Endezeichen in Puffer enthalten }
              EndeZeichenEmpfangen:=true;
              if (length (s) > EndeZeichenPos) then begin  { und das BCC-Zeichen auch }
                AntwortEmpfangen:=true;
                Check_BCC (     { BCC pr�fen, wenn BCC falsch keine Wdh ! }
                  Rueckgabe, Length(s)-(EndeZeichenPos+1)); // 13.01.2011
              end;
            end;
          end
          else begin  { s enth�lt nur das BCC-Zeichen und evtl. nachfolgende Schmutzzeichen }
            AntwortEmpfangen:=true;
            Check_BCC (     { BCC pr�fen, wenn BCC falsch keine Wdh ! }
              Rueckgabe, Length(s)-1);  // 23.05.2012, WW
          end;
        end

        else begin  { ohne CRC- oder BCC-Pr�fung }
          for i:=1 to length (s) do
            if s[i] in AEndezeichen then  { Endezeichen in Puffer enthalten }
              inc (EndezeichenCount);

          if EndezeichenCount >= AEndezeichenAnzahl then begin
            if AntwortTelegrammTyp = at_PushData_MRG900 then begin
              if Assigned (CBGPRSData_Ausgabe) then  // Ausgabe GPRS-Daten
                CBGPRSData_Ausgabe (Rueckgabe.Antwort, FGPRSRemoteAddress, FGPRSRemotePort);

              while length (Rueckgabe.Antwort) > 0 do begin
                OnePushTelegramm:=FCutOnePushTelegramm (Rueckgabe.Antwort, mrgtyp_MRG910);  { ein Push-Telegramm ausschneiden }
                { Push-Telegramm in Telegrammliste eintragen: }
                AddPushTelegramToList (OnePushTelegramm, mrgtyp_MRG910);
              end;  { while length (Rueckgabe.Antwort) > 0 }

              { Empfang f�r weitere Antwortdaten reinitialisieren: }
              if not ReInit_AntwortEmpfang_AfterPushData then Break;
            end
            else begin
              if FAbrufgruppe = 14 then  // Modbus-Abrufgruppen; 08.03.2019, WW
                AntwortEmpfangen:=EndOfModbusResponse (Rueckgabe.Antwort)
              else
                AntwortEmpfangen:=true;

              { evtl. anh�ngendes Push-Telegramm aus Antwort kopieren: }
              if (FAbrufgruppe = 4) OR (FAbrufgruppe = 12) then begin
                PushTelegramme:=ExtractString (Rueckgabe.Antwort, ETX, NUL, AEndezeichenAnzahl - 1);

                { falls Push-Telegramm vorliegt, Rohfile mit erwarteter Antwort
                  neu schreiben (es enth�lt am Ende die Push-Daten !): }
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
        end;

        { m�gliches Problem bei Abrufgruppe 3, 4, 12:
          Zeichen trotz Delay beim Senden zu schnell hintereinander im MRG eingetroffen (nicht zu beeinflussen !)
          Auswirkung: MRG interpretiert Befehl als komplett empfangen (aber unbekannt) oder
                      MRG interpretiert Befehl als nicht komplett empfangen (keine Antwort)
          Abhilfe: Wiederholungen bei "Kommando unbekannt" und Timeout }
        if AntwortEmpfangen AND WithVersuche then begin
          case FAbrufgruppe of
            3, 4, 12:
              begin
                AntwortEmpfangen:=CheckMRGAntwort (Rueckgabe);
                if not AntwortEmpfangen then
                  ReInit_AntwortEmpfang;  // 19.06.2012, WW
              end;
          end;
        end;

        inc (AnzZeichenGelesen, length (s));
      end   { length (s) > 0 }
      else begin
        if TimeoutCount >= Timeout then begin  { Timeout beim Datenempfang }
          with Rueckgabe do begin
            Fehlergruppe:=COM_KOMMERROR;
            Fehlercode:=KOMMERR_TIMEOUT;
            Antwort:='';
          end;

          if WithVersuche then begin
            case FAbrufgruppe of
              3, 4, 12:
                begin
                  { MRG aufwecken: }
                  if FAbrufgruppe = 3 then
                    WakeUpMRG_ByBreak
                  else if (FAbrufgruppe = 4) OR (FAbrufgruppe = 12) then
                    WakeUpMRG_ByCommand;
                  { letzten Befehl wiederholen: }
                  if Befehl_nochmal (Versuche.CRC, Rueckgabe) then
                    ReInit_AntwortEmpfang  // 19.06.2012, WW
                  else
                    Break;
                end;
            else
              Break;
            end;
          end else
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
      Rueckgabe.Antwort:=RohFileName;  { R�ckgabe: Antwort = Rohfilename }

    if FCommError <> 0 then begin  { Schnittstellenfehler ist aufgetreten }
      { R�ckgabe-Record mit Kommunikationsfehler belegen: }
      Comm_SetFehlerRueckgabe (NoCarrier, Rueckgabe);
      { Empfangspuffer leeren: }
      Comm_ClearRecBuf;
    end
    else begin  // Kommunikation OK
      { Pr�fung auf evtl. anh�ngende Push-Telegramme: }
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

    if (Rueckgabe.Fehlergruppe <> 0) AND (AnswerDest = ad_File) then  { Rohfile l�schen bei Fehler }
      DeleteFile (RohFileName);
  end;  { if Timeout > 0 }
  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{-----------------------------------------------------------------------------}
function TMRGCustomCommObj.Befehl_nochmal (MaxVersuche: integer;
                                           var Rueckgabe: TRueckgabe): boolean;
{-----------------------------------------------------------------------------}
{ letzten Befehl nochmal senden, wenn max. Anzahl der Versuche noch nicht erreicht;
  �bergabe: max. Anzahl von Versuchen
  Ergebnis: true, wenn Befehl nochmal gesendet wird }
begin
  inc (Versuch);
  if Versuch <= MaxVersuche then begin
    with Rueckgabe do begin                         { Rueckgabe initalisieren }
      Fehlergruppe:=0;
      Fehlercode:=0;
      Antwort:='';
    end;
    if AnswerDest = ad_File then
      DeleteFile (RohFileName);

    BefehlSenden;                            { nochmal gleichen Befehl senden }
    Result:=true;
  end else
    Result:=false;
end;

{-----------------------------------------------------------------------------------}
function TMRGCustomCommObj.EndOfCRCTransmission (var Rueckgabe: TRueckgabe): boolean;
{-----------------------------------------------------------------------------------}
{ Empfangene Daten pr�fen und CRC-Vergleich durchf�hren. Bei Abweichung letzten
  Befehl nochmal senden;
  R�ckgabe: Rueckgabe-Record
  Ergebnis: true, wenn Daten�bertragung abgeschlossen (keine Wiederholung) }
var
  ACRC: string;
  sLog: string;
  
begin
  case CRCLen of
    2: ACRC:=GetCRC16_Chars_LoHi (CRC);  // CRC-Berechnung Actaris Corus
    4, 12: ACRC:=GetCRC16_Chars_Hex (CRC);  // CRC-Berechnung Wieser/RMG MRG
  else
    ACRC:='';
  end;

  if UpperCase(ACRC) <> UpperCase (CRCMRG) then begin      { CRC-Fehler in der Antwort }
    sLog:='CRC falsch !';
    if FComLogFile <> nil then
      FComLogFile.WriteMsg (sLog);  { Logfileeintrag }
    if FComTraceLog <> nil then
      FComTraceLog.WriteMsg (sLog);  { Tracelogeintrag; 03.01.2022, WW }

    with Rueckgabe do begin
      Fehlergruppe:=COM_KOMMERROR;
      Fehlercode:=KOMMERR_CRC;
      Antwort:='';
    end;
    { nochmal gleichen Befehl senden, wenn max. Anzahl der Versuche noch nicht
      erreicht: }
    if Befehl_nochmal (Versuche.CRC, Rueckgabe) then begin
      Result:=false;
      exit;
    end;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------}
function TMRGCustomCommObj.CheckMRGAntwort (var Rueckgabe: TRueckgabe): boolean;
{------------------------------------------------------------------------------}
{ bei Ger�ten der Abrufgruppe 3, 4, 12 empfangene MRG-Daten pr�fen. Bei MRG-Fehlerantwort
  "Kommando unbekannt" letzten Befehl nochmal senden;
  R�ckgabe: Rueckgabe-Record
  Ergebnis: true, wenn MRG-Antwort ok (keine Wiederholung) }
var
  Antwort: string;
  AFehlergruppe: integer;
  AFehlercode: integer;
begin
  Antwort:=Rueckgabe.Antwort;
  if not ValidMRGAntwort ('', FAbrufgruppe, Antwort, AFehlergruppe, AFehlercode) then begin
    if ((AFehlergruppe = COM_MRGERROR) AND (AFehlercode = MRGERR_KOMMANDOUNBEKANNT)) OR
       ((AFehlergruppe = COM_MRGERROR) AND (AFehlercode = MRGERR_ANTWORTUNERWARTET)) then begin
      { nochmal gleichen Befehl senden, wenn max. Anzahl der Versuche noch nicht
        erreicht: }
      if (AFehlergruppe = COM_MRGERROR) AND (AFehlercode = MRGERR_ANTWORTUNERWARTET) then  // Ger�t ist aufgewacht; 10.08.2011, WW
        Delay (DelayWakeCommand);  // Warten, damit Ger�t auch empfangsbereit ist; 10.08.2011, WW

      if Befehl_nochmal (Versuche.CRC, Rueckgabe) then begin
        Result:=false;
        exit;
      end;
    end;
  end;
  Result:=true;
end;

{-------------------------------------------------------------}
function TMRGCustomCommObj.GetBefehl (ABefehl: string): string;
{-------------------------------------------------------------}
{ Befehl je nach Abrufgruppe modifizieren;
  �bergaben: Befehl }
var
  S: string;

begin
  S:=ABefehl;
  case FAbrufgruppe of
    3, 4:
      begin
        if length (S) > 0 then begin
          if Copy (S, 1, 2) = (STX + 'C') then begin
            { bei C-Befehl: PW immer nach Befehlsbuchstabe und Parameternummer
              -> EC 694, EC 900 haben 4-stellige Parameternummern ! }
            S:=Copy (S, 1, 2 + FParaNrLen) + Copy (Passwort, 1, 8) + ' ' +
               Copy (S, 2 + FParaNrLen + 1, length (S));  // 19.06.2008, WW
          end
          else begin
            { allen �brigen Befehlen mit PW: PW nach Befehlsbuchstabe }
            if WithPasswort then
              S:=Copy (S, 1, 2) + Copy (Passwort, 1, 8) + ' ' +
                 Copy (S, 3, length (S));
          end;
        end;

        if WithCRC then
          S:=S + GetCRC16_Chars_Hex (scrc16 (S, 0));  { an Befehl wird CRC angeh�ngt }
      end;

    5, 8, 9, 11:
      begin  { Abrufgruppen 8 und 9 wegen Break-Befehl an Multiplexer SSU }
        if SendWithChecksum = cs_BCC then
          S:=S + char (GetBCC (0, S));  { an Befehl wird BCC angeh�ngt }
      end;

    10: begin
          if SendWithChecksum = cs_CRC_Corus then
            S:=S + GetCRC16_Chars_LoHi (scrc16_Corus (S, 0));  { an Befehl wird CRC angeh�ngt }
        end;

    12:
      begin  // EC 900
        if length (S) > 0 then begin
          { bei allen Befehlen mit PW: PW nach Befehlsbuchstabe }
          if (Copy (S, 1, 2) = (STX + 'C')) OR WithPasswort then
            S:=Copy (S, 1, 2) + Copy (Passwort, 1, 8) + SOH +
               Copy (S, 3, length (S));
        end;

        if WithCRC then
          S:=S + GetCRC16_Chars_Hex (scrc16 (S, 0));  { an Befehl wird CRC angeh�ngt }
      end;
  end;
  Result:=S;
end;

{----------------------------------------------}
procedure TMRGCustomCommObj.WakeUpMRG_ByCommand;
{----------------------------------------------}
{ Batterie-Ger�t mit Dummy-Befehl aufwecken;
 -> funktioniert bei allen (bekannten) EC694 und bei manchen MRG 800 PTB }
begin
  BefehlSenden ('Y');  // 26.09.2008, WW
  Delay (DelayWakeCommand);
end;

{--------------------------------------------}
procedure TMRGCustomCommObj.WakeUpMRG_ByBreak;
{--------------------------------------------}
begin
  // Default: nichts tun (Wecken mit Break ist nur f�r serielle Kommunikation vorgesehen)
end;

{-------------------------------------------------------------------------------}
function TMRGCustomCommObj.MRG_CRCKonfig_Check (var Rueckgabe: TRueckgabe;
                                                var NoCarrier: boolean): boolean;
{-------------------------------------------------------------------------------}
{ Pr�fen, ob MRG-Kommunikation mit CRC erfolgen mu�;
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (Empfangsdaten des Pr�f-Befehls)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn CRC-Check ok }
var
  CRC_CheckBefehl: string;
  Checked: boolean;
  CRC_CheckVersuche: integer;
  P: integer;
begin
  WakeUpMRG_ByCommand;
  CRC_CheckBefehl:=STX+'Y'+ETX;                                 { Pr�f-Befehl }
  Checked:=false;
  CRC_CheckVersuche:=1;
  while not Checked AND not NoCarrier do begin
    SetWithCRC (false);
    SetWithPasswort (false);
    if not SendMRGCommand (CRC_CheckBefehl, [ETX], 1, Timeouts.CRCCheck, ad_String,
                           Rueckgabe, NoCarrier) then begin
      { bei Timeout CRC nachschicken: }
      if (Rueckgabe.Fehlergruppe = COM_KOMMERROR) AND
         (Rueckgabe.Fehlercode = KOMMERR_TIMEOUT) then begin
        if not SendMRGCommand (GetCRC16_Chars_Hex (scrc16 (CRC_CheckBefehl, 0)),
                               [ETX], 1, Timeouts.CRCCheck, ad_String,
                               Rueckgabe, NoCarrier) then begin
          { bei erneutem Timeout: }
          if (Rueckgabe.Fehlergruppe = COM_KOMMERROR) AND
             (Rueckgabe.Fehlercode = KOMMERR_TIMEOUT) then begin
            if FAbrufgruppe = 3 then
              WakeUpMRG_ByBreak;
            inc (CRC_CheckVersuche);
            if CRC_CheckVersuche > Versuche.CRC then Break;
          end else
            Break;                           { Schnittstellen-Problem -> Stop }
        end
        else begin
          if not ValidMRGAntwort ('Y', FAbrufgruppe, Rueckgabe.Antwort,
                                  Rueckgabe.Fehlergruppe, Rueckgabe.Fehlercode) then begin
            { MRG-Fehlerantwort: Check wiederholen, wenn max. Anzahl der CRC_CheckVersuche
              noch nicht erreicht }
            inc (CRC_CheckVersuche);
            if CRC_CheckVersuche > Versuche.CRC then Break;
          end
          else begin
            SetWithCRC (true);
            Checked:=true;
          end;
        end;
      end else
        Break;                               { Schnittstellen-Problem -> Stop }
    end
    else begin
      if not ValidMRGAntwort ('Y', FAbrufgruppe, Rueckgabe.Antwort,
                              Rueckgabe.Fehlergruppe, Rueckgabe.Fehlercode) then begin
        { MRG-Fehlerantwort: Check wiederholen, wenn max. Anzahl der CRC_CheckVersuche
          noch nicht erreicht }
        inc (CRC_CheckVersuche);
        if CRC_CheckVersuche > Versuche.CRC then Break;
      end
      else begin
        Checked:=true;
        { wenn auf Befehl ohne CRC eine Antwort mit CRC erfolgt
         (erweitert 19.11.2001, WW; nochmal ge�ndert GD/SM/WW 06.09.2002): }
        if length (Rueckgabe.Antwort) > 0 then begin
          P:=Pos (ETX, Rueckgabe.Antwort);
          if (P > 0) AND (P < length (Rueckgabe.Antwort)) then
            SetWithCRC (true);
        end;
      end;
    end;
  end;  { while not Checked }

  SetWithVersuche (true);    { ab jetzt alle MRG-Befehle wiederholen, falls n�tig }
  Result:=Checked;
end;


{-------------------- Methoden f�r ACK01-Protokoll ----------------------------}

{---------------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendACK01Command (ABefehl: string;
                                             ATimeout: integer; AAnswerDest: TAnswerDest;
                                             var Rueckgabe: TRueckgabe;
                                             var NoCarrier: boolean): boolean;
{---------------------------------------------------------------------------------------}
{ Protokoll-Befehl senden und Antwort empfangen;
  �bergabe: Befehl-String
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  DatensatzList: TDatensatzList;
  Datensatz: string;
  Wdh: integer;
  isMRGBefehl: boolean;
  AntwortBuf: string;
  AlleBloeckeEmpfangen: boolean;
  BCCWdh: integer;
  AntwortVorbelegung: string;
  ErsterBlock: boolean;
  SQuittung: string;
  AnzQuitChars: integer;
  KE_AntwCode: integer;
  dummy: integer;

begin
  Result:=false;
  with Rueckgabe do begin
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;

  isMRGBefehl:=Pos (STX, ABefehl) <> 0;  { wenn ein STX enthalten ist, ist ABefehl ein MRG-Befehl }
  DatensatzList:=TDatensatzList.Create (ACK01CommModus, CACK01BlockLaenge);
  try
    { Befehl mu� evtl. in mehrere Bl�cke aufgeteilt werden: }
    DatensatzList.InsertDaten (ABefehl, isMRGBefehl);
    while DatensatzList.GetDatensatz (Datensatz, Wdh) do begin
      { bei MRG-Kommunikation Wartezeit, damit Befehl nicht zu schnell auf letzte Antwortbest�tigung folgt
        (sonst DLE EOT von Gegenstelle): }
      if ACK01CommModus = cm_MRG then
        Delay (200);                     
      { Befehl senden: }
      if not SendProtokoll (Datensatz, Timeouts.ACK01ProtMeldung, ad_String, Rueckgabe) then exit;
      { Rueckgabe f�r ReceiveProtokoll vorbelegen: }
      with Rueckgabe do begin
        Fehlergruppe:=0;
        Fehlercode:=0;
        Antwort:='';
      end;
      { Quittung empfangen (Anzahl der zu lesenden Zeichen begrenzen, um nicht nachfolgende
        Daten mitauszulesen): }
      if ACK01CommModus = cm_MRG then
        AnzQuitChars:=CACK01MaxQuittungLen_MRG
      else
        AnzQuitChars:=CACK01MaxQuittungLen_KE;
      if ReceiveProtokoll (Rueckgabe, NoCarrier, AnzQuitChars) then begin
        if GetACK01ProtokollStatus (ps_ACK) then                  { ACK -> OK }
          inc (DatensatzList.BlockCount)        { Listenz�hler inkrementieren }
        else if GetACK01ProtokollStatus (ps_NAK) OR              { NAK -> Wdh }
                GetACK01ProtokollStatus (ps_ENQ) then begin      { ENQ -> Wdh }
          if Wdh >= (Versuche.ACK01Prot - 1) then begin
            ACK01ProtokollStatusToFehlerGruppeCode (Rueckgabe.Fehlergruppe,
                                                    Rueckgabe.Fehlercode);
            exit;
          end;
        end
        else begin                    { alle anderen Protokollstati -> Fehler }
          ACK01ProtokollStatusToFehlerGruppeCode (Rueckgabe.Fehlergruppe,
                                                  Rueckgabe.Fehlercode);
          exit;
        end;
      end else
        exit;
    end;  { while GetDatensatz }
  finally
    DatensatzList.Free;
  end;

  if not isMRGBefehl then begin              { wenn kein MRG-Befehl -> fertig }
    Result:=true;
    exit;
  end;

  { in der letzten Antwort (Quittierung) k�nnen schon die ersten Zeichen der
    nachfolgenden Antwort (MRG-Daten) enthalten sein: }
  AntwortVorbelegung:=ExtractString (Rueckgabe.Antwort, CR, NUL, 0);

  { Dummy-Sendeaufruf: nichts senden, Timeout und AnswerDest setzen, Timeout-Timer starten }
  if not SendProtokoll ('', ATimeout, AAnswerDest, Rueckgabe) then exit;
  { MRG-Daten empfangen: }
  AntwortBuf:='';                  { Vorbelegung f�r gesamte MRG-Antwortdaten }
  BCCWdh:=1;                     { Wiederholungen bei falschem BCC vorbelegen }
  AlleBloeckeEmpfangen:=false;
  ErsterBlock:=true;
  while not AlleBloeckeEmpfangen do begin              { evtl. mehrere Bl�cke }
    { Rueckgabe f�r ReceiveProtokoll vorbelegen: }
    with Rueckgabe do begin
      Fehlergruppe:=0;
      Fehlercode:=0;
      if ErsterBlock then begin
        ErsterBlock:=false;
        Antwort:=AntwortVorbelegung;
      end else
        Antwort:='';
    end;
    { Block empfangen: }
    if ReceiveProtokoll (Rueckgabe, NoCarrier) then begin
      if GetACK01ProtokollStatus (ps_SOH) then begin              { SOH -> OK }
        { MRG-Daten aus der Antwort filtern und BCC pr�fen: }
        FilterProtokollDaten (Rueckgabe.Antwort);

        { ETB oder ETX: OK (MRG-Daten empfangen) }
        if GetACK01ProtokollStatus (ps_ETB) OR
           GetACK01ProtokollStatus (ps_ETX) then begin
          { Empfangsblock in AntwortBuf bzw. Rohfile anh�ngen: }
          AntwortBuf:=AntwortBuf + Rueckgabe.Antwort;
          if AAnswerDest = ad_File then begin
            if not WriteRohfile (RohfileName, Rueckgabe.Antwort) then begin
              Rueckgabe.Fehlergruppe:=ST_FILEERROR;
              Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
              Break;
            end;
          end;

          SQuittung:=ACKAlternate + CR;
          if ACK01CommModus = cm_MRG then
            SQuittung:='*' + SQuittung;         { mit * bei MRG-Kommunikation }

          if GetACK01ProtokollStatus (ps_ETX) then begin                { ETX }
            if ACK01CommModus = cm_MRG then begin  { MRG-Kommunikation: nach ETX kommen keine weiteren Bl�cke mehr }
              AlleBloeckeEmpfangen:=true;
              { positive Quittierung senden; Timeout = 0, da nach gesendetem ACK0/1
                nichts mehr empfangen wird }
              if not SendProtokoll (SQuittung, 0, ad_String, Rueckgabe) then Break;
            end
            else begin { KE-Kommunikation: solange Antwort "Warten auf Daten" empfangen wird, weiter empfangen }
              ValidKEAntwort (Rueckgabe.Antwort, KE_AntwCode, dummy, dummy);
              if KE_AntwCode <> kec_Warten_auf_Daten then begin
                AlleBloeckeEmpfangen:=true;
                { positive Quittierung senden; Timeout = 0, da nach gesendetem ACK0/1
                  nichts mehr empfangen wird }
                if not SendProtokoll (SQuittung, 0, ad_String, Rueckgabe) then Break;
              end
              else begin     { warten auf KE-Daten... }
                { positive Quittierung senden; Timeout wieder setzen, da nach gesendetem ACK0/1
                  der n�chste Datenblock empfangen wird }
                if not SendProtokoll (SQuittung, ATimeout, ad_String, Rueckgabe) then Break;
              end;
            end;
          end
          else begin                                                    { ETB }
            { positive Quittierung senden; Timeout wieder setzen, da nach gesendetem ACK0/1
             der n�chste Datenblock empfangen wird }
            if not SendProtokoll (SQuittung, ATimeout, ad_String, Rueckgabe) then Break;
          end;

          { Wechsel der ACK 0/1-Best�tigung: }
          if AckAlternate = ACK0 then
            AckAlternate:=ACK1
          else
            AckAlternate:=ACK0;
          BCCWdh:=1;             { Wiederholungen bei falschem BCC vorbelegen }
        end

        { BCC-Fehler -> negative Best�tigung mit NAK (Wiederholung): }
        else if GetACK01ProtokollStatus (ps_BCCError) then begin
          { negative Quittierung senden: }
          SQuittung:=NAK + CR;
          if ACK01CommModus = cm_MRG then
            SQuittung:='*' + SQuittung;         { mit * bei MRG-Kommunikation }
          if not SendProtokoll (SQuittung, 0, ad_String, Rueckgabe) then Break;
          inc (BCCWdh);
          if BCCWdh > Versuche.BCC then begin
            ACK01ProtokollStatusToFehlerGruppeCode (Rueckgabe.Fehlergruppe,
                                                    Rueckgabe.Fehlercode);
            Break;
          end;
        end

       { alle �brigen Protokollstati -> Fehler }
        else begin
          ACK01ProtokollStatusToFehlerGruppeCode (Rueckgabe.Fehlergruppe,
                                                  Rueckgabe.Fehlercode);
          Break;
        end;
      end

      { kein SOH in den Empfangsdaten -> Fehler }
      else begin
        ACK01ProtokollStatusToFehlerGruppeCode (Rueckgabe.Fehlergruppe,
                                                Rueckgabe.Fehlercode);
        Break;
      end;
    end else
      Break;
  end;  { while }

  if Rueckgabe.Fehlergruppe = 0 then begin                  { Datenempfang OK }
    if AAnswerDest = ad_String then
      Rueckgabe.Antwort:=AntwortBuf              { R�ckgabe: Antwort = Daten }
    else
      Rueckgabe.Antwort:=RohFileName;       { R�ckgabe: Antwort = Rohfilename }
  end
  else begin
    Rueckgabe.Antwort:='';
    if AAnswerDest = ad_File then
      DeleteFile (RohFileName);                  { Rohfile l�schen bei Fehler }
  end;
  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{----------------------------------------------------------------------------}
function TMRGCustomCommObj.SendProtokoll (ABefehl: string; ATimeout: integer;
                                          AAnswerDest: TAnswerDest;
                                          var Rueckgabe: TRueckgabe): boolean;
{----------------------------------------------------------------------------}
{ Protokoll-Befehl senden;
  �bergabe: Befehl-String
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
  Ergebnis: true, wenn Befehl gesendet werden konnte }
begin
  Timeout:=ATimeout;
  AnswerDest:=AAnswerDest;

  Befehl:=ABefehl;
  BefehlSenden;                                           { Befehl abschicken }
  Result:=true;
end;

{---------------------------------------------------------------------------------------}
function TMRGCustomCommObj.ReceiveProtokoll (var Rueckgabe: TRueckgabe;
                                             var NoCarrier: boolean;
                                             MaxAnzCharsToReceive: cardinal = 0): boolean;
{----------------------------------------------------------------------------------------}
{ Protokoll-Antwort empfangen;
  �bergabe: maximale Anzahl der zu empfangenen Zeichen (optional)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort: Empfangsdaten
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  S: string;
  AntwortEmpfangen: boolean;
  First: boolean;
  i: cardinal;
  ChToRec_Left: cardinal;
  Dummy: TRueckgabe;
  sLog: string;

begin
 { Rueckgabe mu� vor dem Funktionsaufruf initialisiert werden ! }

  { Antwort auslesen: }
  ChToRec_Left:=MaxAnzCharsToReceive;
  AntwortEmpfangen:=false;
  FCommError:=0;
  First:=true;
  while not AntwortEmpfangen AND (FCommError = 0) do begin
    Application.ProcessMessages;
    Sleep (1);
    Timeout_Elapsed;  // Verstrichene Zeit messen f�r Timeout-�berwachung; 19.12.2022, WW

    { Verbindung pr�fen; 03.06.2020, WW }
    if not Comm_Connection (Dummy) then begin
      // Unterbrochene Verbindung loggen; 18.02.2021, WW
      sLog:='Verbindung unterbrochen';
      if (Dummy.Fehlergruppe = COM_MODEMERROR) AND
         (Dummy.Fehlercode = CME_DCD) then
        sLog:=sLog + ' (kein DCD-Signal)';
        
      if FComLogFile <> nil then
        FComLogFile.WriteMsg (sLog);  // Logfileeintrag
      if FComTraceLog <> nil then
        FComTraceLog.WriteMsg (sLog);  // Tracelogeintrag

      NoCarrier:=true;
      { R�ckgabe wird erst am Funktionsende belegt }
      Break;
    end;

    { Antwort lesen: }
    s:=Comm_ReceiveData (MaxAnzCharsToReceive, ChToRec_Left, false);
    if length (s) > 0 then begin
      { Anzahl der zu empfangenden Zeichen updaten: }
      if ChToRec_Left >= cardinal(length (s)) then
        ChToRec_Left:=ChToRec_Left - cardinal(length (s))
      else
        ChToRec_Left:=0;

      TimeoutCount:=0;
      { Timeout-Anzeige: }
      if Assigned (CBTimeout) then
        CBTimeout ('');

      if First then begin                   { die ersten ausgelesenen Zeichen }
        First:=false;
        if FComLogFile <> nil then
          FComLogFile.Write ('E', S);       { Logfileeintrag mit Kopf "Empfangsdaten" }
        if FComTraceLog <> nil then
          FComTraceLog.Write ('E', S);      { Tracelogeintrag mit Kopf "Empfangsdaten"; 03.01.2022, WW }
        { RxD-Anzeige: }
        if Assigned (CBRxD) then
          CBRxD (SonderzeichenString (s), IntToStr (length (s)) + ' Byte');
      end  { if First }
      else begin
        if FComLogFile <> nil then
          FComLogFile.Write ('D', s);       { Logfileeintrag nur Daten }
        if FComTraceLog <> nil then
          FComTraceLog.Write ('D', s);      { Tracelogeintrag nur Daten; 03.01.2022, WW }
        { RxD-Anzeige: }
        if Assigned (CBRxD) then
          CBRxD (SonderzeichenString (s), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
      end;

      Rueckgabe.Antwort:=Rueckgabe.Antwort + s;
      for i:=1 to length (s) do
        if s[i] = CR then                   { Endezeichen in Puffer enthalten }
          AntwortEmpfangen:=true;
    end   { length (s) > 0 }
    else begin
      if TimeoutCount >= Timeout then begin       { Timeout beim Datenempfang }
        with Rueckgabe do begin
          Fehlergruppe:=COM_KOMMERROR;
          Fehlercode:=KOMMERR_TIMEOUT;
          Antwort:='';
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

  if FCommError <> 0 then begin         { Schnittstellenfehler ist aufgetreten }
    { R�ckgabe-Record mit Kommunikationsfehler belegen: }
    Comm_SetFehlerRueckgabe (NoCarrier, Rueckgabe);
    { Empfangspuffer leeren: }
    Comm_ClearRecBuf;
  end;

  { Protokoll-Antwort pr�fen und entsprechenden ACK01-Protokollstatus setzen: }
  ACK01ProtokollStatus:=ps_UNDEFINIERT;
  if Rueckgabe.Fehlergruppe = 0 then
    SetACK01ProtokollStatus (Rueckgabe.Antwort);

  { Verbindung weg: DLE EOT nicht mit CME_DCD �berschreiben }
  if NoCarrier AND not GetACK01ProtokollStatus (ps_DLE_EOT) then begin
    with Rueckgabe do begin
      Fehlergruppe:=COM_MODEMERROR;
      Fehlercode:=CME_DCD;
      Antwort:='';
    end;
  end;

  { bei DLE EOT oder EOT wird der Abruf abgebrochen und es werden keine weiteren
    Daten mehr geholt: }
  if GetACK01ProtokollStatus (ps_DLE_EOT) OR
     GetACK01ProtokollStatus (ps_EOT) then
    NoCarrier:=true;

  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{-------------------------------------------------------------------}
procedure TMRGCustomCommObj.FilterProtokollDaten (var Daten: string);
{-------------------------------------------------------------------}
{ filtert MRG-Datenteil aus Protokoll-Antwort und setzt entsprechenden Protokollstatus;
  R�ckgabe: gefilterter MRG-Datenteil }
var
  Buf: string;
  SatzNummer: integer;
  ProtLen: integer;

begin
  if ACK01CommModus = cm_MRG then
    ProtLen:=CACK01ProtokollLen_MRG
  else
    ProtLen:=CACK01ProtokollLen_KE;

  if length (Daten) > ProtLen then begin
    if ACK01CommModus = cm_MRG then
      SatzNummer:=StrToInt (Copy (Daten, 3, 3))  { MRG: Stern und  SOH stehen voran }
    else
      SatzNummer:=StrToInt (Copy (Daten, 2, 3)); { KE: nur SOH steht voran }
    { Buf f�r Blockcheck-Pr�fung:  }
    Buf:=ExtractString (Daten, SOH, CR, 0);    { alles zwischen SOH und CR rauskopieren }
    if ACK01CommModus = cm_KE then
      Buf:=SOH + Buf;                { KE: mit SOH }
    if ValidBlockCheck (Buf) then begin
      Delete (Buf, length (Buf) - 1, 2);                     { ohne BCC1 BCC2 }
      if length (Buf) > 0 then begin
        if Buf [length (Buf)] = ETX then
          ACK01ProtokollStatus:=ps_ETX                   { letzter Datenblock }
        else begin
          ACK01ProtokollStatus:=ps_ETB;          { weitere Datenbl�cke folgen }
          Delete (Buf, length (Buf), 1);                           { ohne ETB }
        end;
      end else
        ACK01ProtokollStatus:=ps_UNVOLLSTAENDIG;        { Daten unvollst�ndig }

      Daten:=ExtractString (Buf, STX, NUL, 0);  { alles bis einschlie�lich STX wegschneiden }
      if (SatzNummer = 1) OR (ACK01CommModus = cm_KE) then
        Daten:=STX + Daten;
    end;
  end else
    ACK01ProtokollStatus:=ps_UNVOLLSTAENDIG;            { Daten unvollst�ndig }
end;

{------------------------------------------------------------------}
function TMRGCustomCommObj.ValidBlockCheck (Daten: string): boolean;
{------------------------------------------------------------------}
{ �berpr�ft Blockcheck der R�ckantwort des MRG's;
  �bergabe: Antwortteil ab SOH bis einschlie�lich ETX;
  Ergebnis: true, wenn Pr�fung OK }
var
  BCCIst: byte;      { errechneter BlockCheck }
  BCCSoll: byte;     { �bertragener BlockCheck }
  i: integer;
  BCCSollStr: string [2];

begin
  Result:=False;
  BCCSollStr:=Copy (Daten, length (Daten) - 1, 2);                { BCC1 BCC2 }
  if length (BCCSollStr) = 2 then begin
    BCCSoll:=((byte (BCCSollStr [1]) AND $0F) SHL 4) +
              (byte (BCCSollStr [2]) AND $0F);
    BCCIst:=0;
    for i:=1 to length (Daten) - 2 do begin
      if ACK01CommModus = cm_MRG then
        BCCIst:=BCCIst XOR byte (Daten [i])        { BCC-Bildung bei MRG-Kommunikation }
      else
        BCCIst:=BCCIst + (byte (Daten [i]) MOD 2); { BCC-Bildung bei KE-Kommunikation }
    end;
    if BCCIst = BCCSoll Then
      Result:=true
    else
      ACK01ProtokollStatus:=ps_BCCError;
  end;
End;

{--------------------------------------------------------------------}
procedure TMRGCustomCommObj.SetACK01ProtokollStatus (Antwort: string);
{--------------------------------------------------------------------}
{ pr�ft Protokoll-Antwort und setzt den entsprechenden ACK01-Protokoll-Status;
  �bergabe: ACK01-Protokoll-Antwort }
var
  QPos: integer;
begin
  if length (Antwort) >= 3 then begin
    { Das erste Zeichen wird nicht gepr�ft, da au�er dem normalerweise erwarteten
      Stern auch andere Zeichen kommen k�nnen (z.B. T). }
    if ACK01CommModus = cm_MRG then
      QPos:=2        { MRG: Quittungszeichen an 2. Stelle }
    else
      QPos:=1;       { KE: Quittungszeichen an 1. Stelle }
    case Antwort[QPos] of
      ACK: ACK01ProtokollStatus:=ps_ACK;              { ACK wurde empfangen }
      NAK: ACK01ProtokollStatus:=ps_NAK;              { NAK wurde empfangen }
      ENQ: ACK01ProtokollStatus:=ps_ENQ;              { ENQ wurde empfangen }
      EOT: ACK01ProtokollStatus:=ps_EOT;              { EOT wurde empfangen }
      DLE: ACK01ProtokollStatus:=ps_DLE_EOT;          { DLE wurde empfangen }
      SOH: ACK01ProtokollStatus:=ps_SOH;              { SOH wurde empfangen }
    else
      ACK01ProtokollStatus:=ps_UNGUELTIG;
    end;
  end else
    ACK01ProtokollStatus:=ps_UNVOLLSTAENDIG;
end;

{-------------------------------------------------------------------------------------------}
function TMRGCustomCommObj.GetACK01ProtokollStatus (AStatus: TACK01ProtokollStatus): boolean;
{-------------------------------------------------------------------------------------------}
{ aktuellen ACK 0/1-Protokollstatus pr�fen;
  Ergebnis: true, wenn mit AStatus �bereinstimmt }
begin
  Result:=AStatus = ACK01ProtokollStatus;
end;

{--------------------------------------------------------------------------------------------}
procedure TMRGCustomCommObj.ACK01ProtokollStatusToFehlerGruppeCode (var Fehlergruppe: integer;
                                                                    var Fehlercode: integer);
{--------------------------------------------------------------------------------------------}
begin
  if GetACK01ProtokollStatus (ps_NAK) then begin
    Fehlergruppe:=COM_MODEMPROTERROR;
    Fehlercode:=CMPE_NAK;
  end
  else if GetACK01ProtokollStatus (ps_ENQ) then begin
    Fehlergruppe:=COM_MODEMPROTERROR;
    Fehlercode:=CMPE_ENQ;
  end
  else if GetACK01ProtokollStatus (ps_EOT) then begin
    Fehlergruppe:=COM_MODEMPROTERROR;
    Fehlercode:=CMPE_EOT;
  end
  else if GetACK01ProtokollStatus (ps_DLE_EOT) then begin
    Fehlergruppe:=COM_MODEMPROTERROR;
    Fehlercode:=CMPE_DLE_EOT;
  end
  else if GetACK01ProtokollStatus (ps_UNGUELTIG) then begin
    Fehlergruppe:=COM_MODEMPROTERROR;
    Fehlercode:=CMPE_UNGUELTIG;
  end
  else if GetACK01ProtokollStatus (ps_UNVOLLSTAENDIG) then begin
    Fehlergruppe:=COM_MODEMPROTERROR;
    Fehlercode:=CMPE_UNVOLLSTAENDIG;
  end
  else if GetACK01ProtokollStatus (ps_BCCERROR) then begin
    Fehlergruppe:=COM_KOMMERROR;
    Fehlercode:=KOMMERR_BCC;
  end
  else begin
    Fehlergruppe:=COM_MODEMPROTERROR;
    Fehlercode:=CMPE_UNBESTIMMT;
  end;
end;


{------------------------ Methoden f�r IEC-Protokoll --------------------------}

{-------------------------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendIEC1107DatenTelegramm (ABefehl: string;
                                                      AEndezeichen: TEndezeichenSet;
                                                      AAnswerDest: TAnswerDest;
                                                      ATimeout: integer; AChecksum: TChecksum;
                                                      var Rueckgabe: TRueckgabe;
                                                      var NoCarrier: boolean;
                                                      bRohdatenPlausibilisieren: boolean): boolean;
{-------------------------------------------------------------------------------------------------}
{ IEC 1107-Datentelegramm an Ger�t senden, empfangene Daten in Rohfile schreiben;
  Die Antwort kann aus einem Datensatz (STX..ETX BCC) oder aus mehreren Teils�tzen
  bestehen (STX..EOT BCC .. STX..EOT BCC STX..ETX BCC). Jeder Teilsatz mu�
  quittiert werden: mit ACK, wenn er OK ist, ansonsten mit NAK (dann wird der
  letzte Satz nochmal gesendet)
  �bergabe: Befehl-String
            in der Antwort zu erwartende Endezeichen
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            Pr�fsummentyp
            Flag 'bRohdatenPlausibilisieren': wenn true, erfolgt eine Plausibilisierung
              der Empfangsdaten
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort auf Datentelegramm korrekt gelesen werden konnte }
var
  Stop: boolean;
  Wdh: integer;
  AntwBuf: string;
  P: integer;
  iVersuche: integer;
  cEnd: char;
  S: string;
  iCount: integer;

begin
  Result:=false;
  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;
  AntwBuf:='';
  if AAnswerDest = ad_File then
    RohFileName:=CreateTempRohFile (WorkPath, prefix_MRG_Roh);

  { "Datenstrom-Schleife" starten: }
  SetSendWithChecksum (AChecksum);       { Flag setzen, es folgt Befehl mit Checksumme }
  SetReceiveWithChecksum (AChecksum);    { Flag setzen, es folgt Antwort mit Checksumme }
  if not SendCommand (ABefehl, AEndezeichen, 1, ATimeout, ad_String, Rueckgabe, NoCarrier) then begin
    if not ((Rueckgabe.Fehlergruppe = COM_KOMMERROR) AND
            ((Rueckgabe.Fehlercode = KOMMERR_BCC) OR (Rueckgabe.Fehlercode = KOMMERR_CRC))) then begin
      if AAnswerDest = ad_File then              { Rohfile l�schen bei Fehler }
        DeleteFile (RohFileName);
      exit;                         { bei allen Fehlern au�er BCC-Fehler raus }
    end;
  end;

  case AChecksum of
    cs_BCC:       iVersuche:=Versuche.BCC;
    cs_CRC_Corus: iVersuche:=Versuche.CRC;
  else
    iVersuche:=Versuche.BCC;
  end;
  Stop:=false;
  Wdh:=0;
  SetSendWithChecksum (cs_Off);  { Flag r�cksetzen, es folgen ab jetzt Befehle ohne Checksumme }
  while not Stop AND not NoCarrier do begin
    { negative Quittung bei falschem BCC senden: }
    if (Rueckgabe.Fehlergruppe = COM_KOMMERROR) AND
       ((Rueckgabe.Fehlercode = KOMMERR_BCC) OR (Rueckgabe.Fehlercode = KOMMERR_CRC)) then begin
      inc(Wdh);
      if not SendCommand (NAK, AEndezeichen, 1, ATimeout, ad_String, Rueckgabe, NoCarrier) then begin
        if not ((Rueckgabe.Fehlergruppe = COM_KOMMERROR) AND
                ((Rueckgabe.Fehlercode = KOMMERR_BCC) OR (Rueckgabe.Fehlercode = KOMMERR_CRC))) then begin
          if AAnswerDest = ad_File then          { Rohfile l�schen bei Fehler }
            DeleteFile (RohFileName);
          exit;                     { bei allen Fehlern au�er BCC-Fehler raus }
        end;
      end;
      { begrenzte Anzahl von Versuchen: }
      if Wdh >= (iVersuche - 1) then begin
        if AAnswerDest = ad_File then            { Rohfile l�schen bei Fehler }
          DeleteFile (RohFileName);
        exit;
      end;
      Continue;  { bei negativer Quittierung kommt der gleiche Satz nochmal }
    end;
    Wdh:=0;

    { letzten Teil-Datensatz erhalten ? }
    case AChecksum of
      cs_BCC:  // IEC1107-Protokoll
        begin
          { -> Pr�fung ge�ndert 09.04.2003 WW: nur wenn ETX als Endezeichen in der
               Antwort enthalten ist, wurde der letzte Datensatz empfangen (ETX
               kann auch als BCC angeh�ngt sein !) }
          P:=Pos (ETX, Rueckgabe.Antwort);
          if (P > 0) AND (P < length (Rueckgabe.Antwort)) then begin
            Stop:=true;

            if bRohdatenPlausibilisieren then begin  // 27.03.2013, WW
              { Zus�tzliche Rohdaten-Plausibilisierung f�r Elster IEC1107-Archivdaten:
                -> Es m�ssen mind. 3 durch ( und ) begrenzte Werte im Rohsatz
                   enthalten sein. Der Zeitstempel steht als 3. Wert im Rohsatz von
                   DL240/220/210 und EK260; 30.04.2012, WW }
              S:=ExtractString (Rueckgabe.Antwort, STX, ETX, 0);   { Rohdaten zwischen STX und ETX }
              if not (Pos ('(#', S) = 1) then begin  // nicht bei Fehlertelegramm (#nnnn); 07.11.2012, WW
                iCount:=F_TotalChars (S, '(');  // Anzahl der (-Zeichen
                if iCount < 3 then begin
                  { Fehler: Ung�ltige Rohdaten }
                  Rueckgabe.Fehlergruppe:=ST_DATACHECK;
                  Rueckgabe.Fehlercode:=DCH_INVALID;

                  { Bislang erfolgreich empfangene Teils�tze werden zur�ckgegeben: }
                  if AAnswerDest = ad_File then
                    Rueckgabe.Antwort:=RohFileName          { R�ckgabe: Antwort = Rohfilename }
                  else
                    Rueckgabe.Antwort:=AntwBuf;
                  Result:=true;
                  exit;
                end;
              end;
            end;  { if bRohdatenPlausibilisieren }
          end else
            Stop:=false;
        end;

      cs_CRC_Corus:  // Actaris Corus
        begin
          if length (Rueckgabe.Antwort) >= 4 then begin
            cEnd:=Rueckgabe.Antwort [4];  // auf letzten Teildatensatz pr�fen
            Stop:=(Ord (cEnd) AND $80) > 0;
          end else
            Stop:=true;
        end;
    else
      Stop:=true;
    end;

    if AAnswerDest = ad_File then begin
      if not WriteRohfile (RohfileName, Rueckgabe.Antwort) then begin
        DeleteFile (RohFileName);
        Rueckgabe.Fehlergruppe:=ST_FILEERROR;
        Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
        exit;
      end;
    end else
      AntwBuf:=AntwBuf + Rueckgabe.Antwort;

    if not Stop then begin
      { positive Quittung senden: }
      if not SendCommand (ACK, AEndezeichen, 1, ATimeout, ad_String, Rueckgabe, NoCarrier) then begin
        if not ((Rueckgabe.Fehlergruppe = COM_KOMMERROR) AND
                ((Rueckgabe.Fehlercode = KOMMERR_BCC) OR (Rueckgabe.Fehlercode = KOMMERR_CRC))) then begin
          if AAnswerDest = ad_File then          { Rohfile l�schen bei Fehler }
            DeleteFile (RohFileName);
          exit;                     { bei allen Fehlern au�er BCC-Fehler raus }
        end;
      end;
    end;
  end;  { while Stop }

  if NoCarrier then begin                           { Verbindung unterbrochen }
    if AAnswerDest = ad_File then                { Rohfile l�schen bei Fehler }
      DeleteFile (RohFileName);
    exit;
  end;

  if AAnswerDest = ad_File then
    Rueckgabe.Antwort:=RohFileName          { R�ckgabe: Antwort = Rohfilename }
  else
    Rueckgabe.Antwort:=AntwBuf;
  Result:=true;
end;

{---------------------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendTritschlerIECCommand (ABefehl: string; AAnswerDest: TAnswerDest;
                                                     ATimeout: integer; AMrgTyp: integer;
                                                     bMitWecken: boolean;
                                                     var Rueckgabe: TRueckgabe;
                                                     var NoCarrier: boolean): boolean;
{---------------------------------------------------------------------------------------------}
{ Tritschler IEC-Befehl senden und Antwort empfangen; vor dem eigentlichen Befehl
  wird optional ein Befehl zum Wecken des Ger�ts gesendet (bei Multiplexer-Betrieb
  nicht erforderlich);
  �bergabe: Befehl-String
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            Ger�tetyp-Nummer
            Flag 'bMitWecken' (true �bergeben, wenn Wecksequenz vor dem Abfragebefehl
                               gesendet werden soll)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }

  {-------------------------------------}
  function GetIECKommando_Wecken: string;
  {-------------------------------------}
  { liefert Weck-Kommando f�r IEC-Protokoll }
  var
    S: string;
    i: integer;
    AnzWeckZeichen: integer;
  begin
    // Anzahl der NUL-Weckzeichen:
    // -> VC2 und TTG: "trial and error" anhand der zahlreichen STGW-Ger�te. Einige VC2 und TTG
    // lassen sich mit der Standard-Weckzeichenanzahl des Tritschler-Abrufprogramms
    // MoTe zusammen mit dem Wieser-Modem DF22 nicht erfolgreich wecken.  30.04.2008, WW
    // -> �brige Ger�tetypen: Standard-Weckzeichenanzahl des Tritschler-Abrufprogramms
    // MoTe (Abrufe funktionieren damit in der Praxis bislang zufriedenstellend)
    if (AMrgTyp = mrgtyp_VC2) OR (AMrgTyp = mrgtyp_TTG_IEC) then  { VC2, TTG (IEC) }
      AnzWeckZeichen:=240
    else if (AMrgTyp = mrgtyp_VC3) OR (AMrgTyp = mrgtyp_VC3_VC2komp) OR
            (AMrgTyp = mrgtyp_VCC) then  { VC3, VCC }
      AnzWeckZeichen:=120
    else  { TDS }
      AnzWeckZeichen:=80;

    S:='';
    for i:=1 to AnzWeckZeichen do
      S:=S + NUL;
    Result:=S;
  end;

Const
  CTimeout_WakeUpTritschlerIEC = 5000;  { Timeout in ms f�r Wecken der Tritschler IEC-Ger�te }

  { Konstanten f�r IEC-Protokoll-Reinitialisierung: }
  CDelayFix_ReInitProtTritschlerIEC = 10000;  { Fixe Verz�gerung in ms }
  CDelayWdh_ReInitProtTritschlerIEC =  2000;  { Verz�gerung nach jeder Wiederholung in ms }
  CTimeout_ReInitProtTritschlerIEC  = 60000;  { Timeout in ms (incl. fixer Verz�gerung}

var
  Stop: boolean;
  Wdh: integer;
  WeckBefehl: string;
  dummy: TRueckgabe;
  dtStart: TDateTime;
  Dauer_ms: Int64;

begin
  Result:=false;
  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;

  // Wenn Protokoll-Reinitialisierungsphase l�uft, erst mal pauschal warten
  // (Protokollerkennung startet im Ger�t erst nach gewisser Zeit). Erfolgreiches
  // Empfangen einer Antwort (mit dem IEC-Protokoll) wird dann �ber die evtl.
  // erforderlichen, nachfolgenden Wiederholungen geregelt; 30.04.2019, WW
  dtStart:=Now;
  if FReInitProtokoll then
    Delay (CDelayFix_ReInitProtTritschlerIEC);

  WeckBefehl:=GetIECKommando_Wecken;
  Stop:=false;
  Wdh:=0;
  while not Stop AND not NoCarrier do begin
    if bMitWecken then begin
      SendCommand (WeckBefehl, [], 0, 0, ad_String, dummy, NoCarrier);  // Ger�t wecken
      // Wartezeit nach der Wecksequenz:
      // -> VC2 und TTG: "trial and error" anhand der zahlreichen STGW-Ger�te. Einige VC2 und TTG
      // lassen sich mit der Standard-Wartezeit des Tritschler-Abrufprogramms
      // MoTe zusammen mit dem Wieser-Modem DF22 nicht erfolgreich wecken.  30.04.2008, WW
      // -> �brige Ger�tetypen: Standard-Wartezeit des Tritschler-Abrufprogramms
      // MoTe (Abrufe funktionieren damit in der Praxis bislang zufriedenstellend)
      if (AMrgTyp = mrgtyp_VC2) OR (AMrgTyp = mrgtyp_TTG_IEC) then  { VC2, TTG (IEC) }
        Delay (1500)
      else if (AMrgTyp = mrgtyp_VC3) OR (AMrgTyp = mrgtyp_VC3_VC2komp) OR
              (AMrgTyp = mrgtyp_VCC) then  { VC3, VCC }
        Delay (500)
      else  { TDS }
        Delay (600);
    end;

    { Befehl senden: }
    Result:=SendMRGCommand (ABefehl, [ETX], 1, ATimeout, AAnswerDest, Rueckgabe, NoCarrier,
                            0, CTimeout_WakeUpTritschlerIEC);  // mit Aufweck-Timeout; 22.09.2009, WW
    if not Result then begin
      { Wenn IEC-Protokoll-Reinitialisierung l�uft (nach Kommunikation mit internem
        FTL-Protokoll): Wiederholungen f�r begrenzte Zeit; 30.04.2019, WW
        -> Testergebnisse f�r R�ckschaltung von Protokoll FTL-intern auf IEC (lokale IR-Schnittstelle):
             VC2: Dauer ca. 27 s (reagiert bis dahin mit Timeout)
             VC3: Dauer ca. 18 s (reagiert bis dahin mit Parit�tsfehler) }
      if FReInitProtokoll then begin
        Dauer_ms:=MilliSecondsBetween (dtStart, Now);  // ms zwischen Start der Zeitmessung und der aktuellen Zeit

        if (Rueckgabe.Fehlergruppe = COM_KOMMERROR) AND
           (Rueckgabe.Fehlercode = KOMMERR_TIMEOUT) then begin  // Timeout
          if (Dauer_ms >= CTimeout_ReInitProtTritschlerIEC) OR (Dauer_ms < 0) then exit;
        end
        else if (Rueckgabe.Fehlergruppe = COM_ERROR) AND
                (Rueckgabe.Fehlercode <> 0) then begin  // COM-Fehler (z.B. Parit�t)
          if (Dauer_ms >= CTimeout_ReInitProtTritschlerIEC) OR (Dauer_ms < 0) then
            exit
          else
            Delay (CDelayWdh_ReInitProtTritschlerIEC);  // Kurz Warten vor dem n�chsten Versuch
        end else
          exit;
      end
      else begin  // Es l�uft keine IEC-Protokoll-Reinitialisierung (Normalfall)
        { Begrenzte Anzahl von weiteren Versuchen bei BCC- und Timeout-Fehler: }
        if (Rueckgabe.Fehlergruppe = COM_KOMMERROR) AND
           ((Rueckgabe.Fehlercode = KOMMERR_BCC) OR (Rueckgabe.Fehlercode = KOMMERR_TIMEOUT)) then begin
          inc (Wdh);
          if Wdh >= Versuche.BCC then exit;  
        end else
          exit;
      end;
    end
    else begin  // OK
      Stop:=true;
      // Flag f�r Protokoll-Reinitialisierungsphase zur�cksetzen; 30.04.2019, WW
      FReInitProtokoll:=false;
    end;
  end;  { while not Stop }
end;

{------------------------------------------------------------------------}
function TMRGCustomCommObj.Check_BCC (
  var Rueckgabe: TRueckgabe; iBlankCharCount: word): boolean;
{------------------------------------------------------------------------}
{ Empfangene Daten pr�fen und BCC-Vergleich durchf�hren (IEC-Protokoll);
  �bergabe: Rueckgabe.Antwort
            Anzahl der Schmutzzeichen am Ende der Antwort (Zeichen nach dem BCC)
  R�ckgabe: Rueckgabe-Record
  Ergebnis: true, wenn BCC-Pr�fung OK }
var
  BCC: byte;
  pruefen: boolean;
  i: integer;
  S: string;
  cBCC: char;
  sLog: string;

begin
  S:=Rueckgabe.Antwort;
  pruefen:=false;
  BCC:=0;
  for i := 1 to (Length(S)-1-iBlankCharCount) do  // 13.01.2011
  begin
    if pruefen then
      BCC:=BCC XOR ord (S [i]);
    if (S [i] = SOH) OR (S [i] = STX) then   { Startzeichen: STX oder SOH }
      pruefen:=true;
  end;

  cBCC:=S [length(S)-iBlankCharCount];  // BCC-Zeichen in der Antwort
  if ((Ord(cBCC) <> BCC)) OR  // 13.01.2011
     not pruefen then begin  // wenn kein Startzeichen enthalten: BCC-Fehler ausgeben; 09.11.2011, WW
    sLog:='BCC falsch !';
    if FComLogFile <> nil then
      FComLogFile.WriteMsg (sLog);  { Logfileeintrag }
    if FComTraceLog <> nil then
      FComTraceLog.WriteMsg (sLog);  { Tracelogeintrag; 03.01.2022, WW }

    with Rueckgabe do begin
      Fehlergruppe:=COM_KOMMERROR;
      Fehlercode:=KOMMERR_BCC;
      Antwort:='';
    end;
    Result:=false;
  end else
    Result:=true;
end;

{---------------------------------------------------------------}
function TMRGCustomCommObj.GetBCC (Start: byte; s: string): byte;
{---------------------------------------------------------------}
{ BCC berechnen/updaten;
  �bergabe: BCC-Startwert
            Daten-String
  Ergebnis: neues BCC }
var
  i: integer;
begin
  Result:=Start;
  for i:=2 to length(s) do begin           { ohne Startzeichen: STX oder SOH }
    Result:=Result XOR Byte (s[i]);
    if s[i] = ETX then Break;
  end;
end;


{---------------- Methoden f�r Datenabruf von KE-Anlagen ----------------------}

{--------------------------------------------------------------------------------------}
function TMRGCustomCommObj.CheckKE_Datenheader_Ruhrgastyp (HeaderStr: string;
                                                           var DataSize: word;
                                                           var CheckSum: word): boolean;
{--------------------------------------------------------------------------------------}
{ Datenheader pr�fen und enthaltene Datengr��e zur�ckgeben;
  Ergebnis: true, wenn Header OK }
var
  S: string;
  NegDataSize: word;

begin
  Result:=false;
  if length (HeaderStr) <> CLenKE_Datenheader_Ruhrgastyp then exit;
  { Datengr��e }
  S:=Copy (HeaderStr, 1, 2);
  DataSize:=Bin2Word (S);
  { negierte Datengr��e }
  S:=Copy (HeaderStr, 3, 2);
  NegDataSize:=Bin2Word (S);

  { Pr�fung: Datengr��e, negierte Datengr��e }
  if DataSize <> not NegDataSize then exit;

  { Checksumme }
  S:=Copy (HeaderStr, 5, 2);
  CheckSum:=Bin2Word (S);
  Result:=true;
end;

{-------------------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendKEDatentelegramm_S (ABefehl: string; AAnswerDest: TAnswerDest;
                                                   ATimeout: integer;
                                                   var Rueckgabe: TRueckgabe;
                                                   var NoCarrier: boolean): boolean;
{-------------------------------------------------------------------------------------------}
{ KE-Datenabfrage-Telegramm (Ruhrgas-Typ) an Ger�t senden, empfangene Daten in Rohfile schreiben;
  Ergebnis: true, wenn Antwort auf Datentelegramm korrekt gelesen werden konnte }

  {---------------------------------------------}
  function CalcCheckSum (Daten: string): integer;
  {---------------------------------------------}
  { Checksumme �ber KE-daten bilden }
  var
    i: integer;
    sum: integer;
  begin
    sum:=0;
    for i:=1 to length (Daten) do
      sum:=sum + byte (Daten [i]);
    Result:=sum;
  end;

var
  DataSize: word;
  Stop: boolean;
  AntwBuf: string;
  STX_Count: integer;
  S: string;
  AntwCode: integer;
  cQuittung: char;
  CheckSum_Header: word;
  Wdh: integer;
  OK: boolean;
  sLog: string;

begin
  Result:=false;
  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;
  AntwBuf:='';
  if AAnswerDest = ad_File then
    RohFileName:=CreateTempRohFile (WorkPath, prefix_MRG_Roh);

  try
    { Datenauslese-Befehl senden, mit ACK01-Protokoll alle "Warten auf Daten"-Datenbl�cke
      solange empfangen bis der "Daten folgen"-Block oder "keine Daten vorhanden"-Block
      empfangen wird: }
    if not SendACK01Command (ABefehl, ATimeout, ad_String, Rueckgabe, NoCarrier) then exit;

    { Antwortcode des letzten Block pr�fen: }
    STX_Count:=F_TotalChars (Rueckgabe.Antwort, STX);
    S:=ExtractString (Rueckgabe.Antwort, STX, ETX, STX_Count - 1);
    S:=STX + S + ETX;
    if not ValidKEAntwort (S, AntwCode, Rueckgabe.Fehlergruppe, Rueckgabe.Fehlercode) then exit;
    if AntwCode = kec_Keine_Daten_vorhanden then begin
      { keine Daten vorhanden, dann mit OK raus;
        -> Antwort: Leerstring bzw. Leerfile }
      Result:=true;
      exit;
    end;

    { ACK empfangen:
      -> wenn kein ACK empfangen wird, konnte keine Synchronisation mit Gegenstelle erfolgen (Fehler) }
    if not SendMRGCommand ('', [ACK], 1, Timeouts.ACK01ProtMeldung, ad_String, Rueckgabe, NoCarrier) then exit;                         { bei Fehler raus }

    Stop:=false;
    cQuittung:=ACK;
    Wdh:=0;
    while not Stop AND not NoCarrier do begin
      { ACK/NAK senden, ACK/DLE empfangen: }
      if not SendMRGCommand (cQuittung, [ACK, DLE], 1, Timeouts.ACK01ProtMeldung, ad_String, Rueckgabe, NoCarrier) then exit;                         { bei Fehler raus }
      { der letzte Datenblock wurde empfangen, wenn vom Ger�t kein ACK quittiert wird: }
      if Copy (Rueckgabe.Antwort, 1, 1) <> ACK then
        Break;  { es kommt zwar noch irgendwas Undefiniertes nach, ist aber uninteressant... }

      { ACK senden, Datenkopf empfangen (feste Anzahl an Zeichen): }
      if not SendMRGCommand (ACK, [], 0, Timeouts.ACK01ProtMeldung, ad_String, Rueckgabe, NoCarrier,
                             CLenKE_Datenheader_Ruhrgastyp) then exit;

      { Anzahl der jetzt nachfolgenden Daten-Zeichen steht im Header: }
      OK:=CheckKE_Datenheader_Ruhrgastyp (Rueckgabe.Antwort, DataSize, CheckSum_Header);
      if not OK then begin
        inc (Wdh);
        { begrenzte Anzahl von Versuchen: }
        if Wdh >= Versuche.ACK01Prot then begin
          Delay (7000);
          { raus mit KE-Headerfehler }
          with Rueckgabe do begin
            Fehlergruppe:=COM_KE_ERROR;
            Fehlercode:=KEERR_HEADER;
          end;
        end;
          exit;

        cQuittung:=NAK;
        Continue;
      end;

      { ACK senden, Daten empfangen (feste Anzahl an Zeichen lt. Header -> DataSize): }
      if not SendMRGCommand (ACK, [], 0, Timeouts.ACK01ProtMeldung, ad_String, Rueckgabe, NoCarrier,
                             DataSize) then exit;
      { Checksumme pr�fen }
      OK:=CalcCheckSum (Rueckgabe.Antwort) = CheckSum_Header;
      if not OK then begin
        sLog:='Checksumme falsch !';
        if FComLogFile <> nil then
          FComLogFile.WriteMsg (sLog);  { Logfileeintrag }
        if FComTraceLog <> nil then
          FComTraceLog.WriteMsg (sLog);  { Tracelogeintrag; 03.01.2022, WW }

        inc (Wdh);
        { begrenzte Anzahl von Versuchen: }
        if Wdh >= Versuche.ACK01Prot then begin
          Delay (7000);
          { raus mit Checksummenfehler }
          with Rueckgabe do begin
            Fehlergruppe:=COM_KOMMERROR;
            Fehlercode:=KOMMERR_BCC;
          end;
          exit;
        end;

        cQuittung:=NAK;
        Continue;
      end;
      cQuittung:=ACK;
      Wdh:=0;

      if AAnswerDest = ad_File then begin
        if not WriteRohfile (RohfileName, Rueckgabe.Antwort) then begin
          Rueckgabe.Fehlergruppe:=ST_FILEERROR;
          Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
          exit;
        end;
      end else
        AntwBuf:=AntwBuf + Rueckgabe.Antwort;
    end;  { while not Ende }

    if NoCarrier then exit;  { Verbindung unterbrochen }
    Result:=true;
  finally
    { Rohfile l�schen bei Fehler oder Verbindungsunterbrechung: }
    if (AAnswerDest = ad_File) AND ((not Result) OR NoCarrier) then
      DeleteFile (RohFileName);
  end;

  if AAnswerDest = ad_File then
    Rueckgabe.Antwort:=RohFileName          { R�ckgabe: Antwort = Rohfilename }
  else
    Rueckgabe.Antwort:=AntwBuf;
end;


{-------------------------- Sealink -------------------------------------------}

{-------------------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendKEDatentelegramm_T (ABefehl: string; AAnswerDest: TAnswerDest;
                                                   ATimeout: integer;
                                                   var Rueckgabe: TRueckgabe;
                                                   var NoCarrier: boolean): boolean;
{-------------------------------------------------------------------------------------------}
{ KE-Datenabfrage-Telegramm (PPN-Typ) an Ger�t senden, empfangene Daten in Rohfile schreiben;
  Ergebnis: true, wenn Antwort auf Datentelegramm korrekt gelesen werden konnte }

{$R-,H-}  // Bereichspr�fung mu� f�r Sealink-Protokoll (CRC-Berechnung) ausgeschaltet sein.

{--------------------------------------}
PROCEDURE RxSeaLink(overdrive: BOOLEAN);
{--------------------------------------}
{ Daten empfangen mit Sealink-Protokoll;
  Quelle: BBS Archives (http://archives.thebbs.org/ra90c.htm -> TPSLINK.ZIP }

TYPE
   zeros = RECORD
              flen,
              fstamp: LONGINT;
              fnam: ARRAY[0..16] OF BYTE;
              prog: ARRAY[0..14] OF BYTE;
              noacks: BYTE;
              fill: ARRAY[0..86] OF BYTE
           END;
   secbuf = ARRAY[0..127] OF BYTE;

VAR
   ackless: INTEGER;
   chktec, toterr: INTEGER;

  {---------------------------------------------------}
  PROCEDURE WriteSealinkLogfile(s: STRING; n: INTEGER);
  {---------------------------------------------------}
  { Sealink-Protokolldatei schreiben }
  var
    SBuf: string;
    FS: TFileStream;
  BEGIN
     if FComLogFile <> nil then begin { nur, wenn auch Standard-COM-Logfile geschrieben wird }
       try
         if length (SealinkLogFilename) > 0 then begin
           if not FileExists (SealinkLogFilename) then
             FS:=TFileStream.Create (SealinkLogFilename, fmCreate)
           else
             FS:=TFileStream.Create (SealinkLogFilename, fmOpenReadWrite OR fmShareDenyWrite);
           try
             FS.Seek (0, soFromEnd);
             SBuf:=FormatDateTime ('dd.mm.yyyy hh:nn:ss,zzz', Now) + '  ' +
                   F_RightPad (s, ' ', 25);
             IF (n >= 0) THEN
                SBuf:=SBuf + ' [' + Format ('%.3d', [n]) + ']';
             SBuf:=SBuf + CR + LF;
             FS.Write (SBuf [1], length (SBuf));
           finally
             FS.Free;
           end;
         end;
       except
       end;
     end;
  END;

  {---------------------------------}
  FUNCTION FromAsciiZ(VAR a): STRING;
  {---------------------------------}
  VAR
     s: STRING;
     ar: ARRAY[0..255] OF CHAR ABSOLUTE a;
     p: WORD;

  BEGIN
     p := 0;
     WHILE (ar[p] <> #0) AND (p <= 255) DO
     BEGIN
        s[p+1] := ar[p];
        Inc(p)
     END;
     s[0] := Chr(p);
     FromAsciiZ := s
  END;

  {------------------------------------------}
  FUNCTION TimerSet(tenths: INTEGER): LONGINT;
  {------------------------------------------}
  { �bergabe: Zehntel-Sekunden
    R�ckgabe: Zehntel-Sekunden seit Tagesbeginn plus �bergebene }
  VAR
     h,m,s,ms: WORD;

  BEGIN
     DecodeTime (Time,h,m,s,ms);
     TimerSet := (LONGINT(h)*36000)+(LONGINT(m)*600)+(LONGINT(s)*10)+(LONGINT(ms) DIV 100)+
                 (LONGINT(tenths));
  END;

  {---------------------------------------}
  FUNCTION TimeUp(atime: LONGINT): BOOLEAN;
  {---------------------------------------}
  BEGIN
     TimeUp := (TimerSet(0) >= atime)
  END;

  {------------------------------------------}
  FUNCTION Com_GetC(tenths: INTEGER): INTEGER;
  {------------------------------------------}
  VAR
     ms: integer;
     RBuf: TRueckgabe;
  BEGIN
    Com_GetC:=-1;
    ms:=tenths * 100;   { Zehntelsekunden -> Millisekunden }
    { Lese 1 Zeichen: }
    if not SendMRGCommand ('', [NUL], 0, ms, ad_String, RBuf, NoCarrier, 1) then exit;
    { hier keine Fehlerbelegung in 'Rueckgabe', wird durch nakblock etc. gehandlet ! }
    if length (RBuf.Antwort) > 0 then
      Com_GetC:=Byte (RBuf.Antwort [1]);
  END;

  {-----------------------------------------}
  FUNCTION GetBlock(VAR buf: secbuf): STRING;
  {-----------------------------------------}
  VAR
     ourcrc, hiscrc: smallint;
     c, n: INTEGER;
     RBuf: TRueckgabe;

  BEGIN
     ourcrc := 0;
    { Lese 128-Byte-Block: }
    if not SendMRGCommand ('', [NUL], 0, 1000, ad_String, RBuf, NoCarrier, 128) then begin
      if Rueckgabe.Fehlergruppe = 0 then     { ersten aufgetretenen Fehler merken }
        Rueckgabe:=RBuf;
      GetBlock := 'Short';
      exit;
    end;
    if length (RBuf.Antwort) <> 128 then begin
      GetBlock := 'Short';
      exit;
    end;
    for n:=1 to length (RBuf.Antwort) do begin
      c:=Ord (RBuf.Antwort[n]);
      IF (chktec <> 0) THEN
         ourcrc := UpdCrc_Sealink(c,ourcrc)
      ELSE
         ourcrc := ourcrc + c;
      buf[n-1] := Byte(c);
    end;

     IF (chktec <> 0) THEN
     BEGIN
        ourcrc := UpdCrc_Sealink(0,ourcrc);
        ourcrc := UpdCrc_Sealink(0,ourcrc);
        c := Com_GetC(100);
        hiscrc := c SHL 8;
        c := Com_GetC(100);
        hiscrc := hiscrc + c;
        IF (hiscrc = ourcrc) THEN
           GetBlock := ''
        ELSE
           GetBlock := 'CRC';
        Exit
     END;
     ourcrc := ourcrc AND $FF;
     hiscrc := Com_GetC(100) AND $FF;
     IF (hiscrc = ourcrc) THEN
        GetBlock := ''
     ELSE
        GetBlock := 'Check'
  END;

  {-----------------------------------------}
  PROCEDURE SendAck(acknak, blknum: INTEGER);
  {-----------------------------------------}
  var
    RBuf: TRueckgabe;
    S: string;

  BEGIN
     IF (acknak <> 0) THEN
        S:=Chr(6)
     ELSE IF (chktec <> 0) THEN
        S:='C'
     ELSE BEGIN
        S:=Chr(21);
     END;
     S:=S + Chr(BYTE(blknum)) + Chr(BYTE(blknum XOR $FF));

    if not SendMRGCommand (S, [NUL], 0, 0, ad_String, RBuf, NoCarrier) then begin
      if Rueckgabe.Fehlergruppe = 0 then     { ersten aufgetretenen Fehler merken }
        Rueckgabe:=RBuf;
    end;
  END;


LABEL
   nakblock, ackblock, nextblock, blockstart, endrcv, abort;

VAR
   c, tries, blknum, inblk, endblk: INTEGER;
   t1, left: LONGINT;
   zero: zeros;
   name, pname, stat, why: STRING;
   buff: secbuf;
   RBuf: TRueckgabe;

BEGIN
   stat := 'Init';
   blknum := 1;
   tries := -10;
   chktec := 1;
   toterr := 0;
   endblk := 0;
   ackless := 0;
   FillChar(zero,128,0);
   { Das im Original-Sealink-Protokoll vorgesehene Pr�fen auf ein SOH-Zeichen, ohne
     es aus dem Empfangspuffer auszulesen, kann entfallen: }
//   IF (Com_Peek = 1) THEN
//      GOTO nextblock;

nakblock:
   IF (blknum > 1) THEN
      Inc(toterr);
   Inc(tries);
   IF (tries > 10) THEN
   BEGIN
      WriteSealinkLogfile('Too many errors',-1);
      GOTO abort
   END;
   IF (tries = 0) THEN
      chktec := 0;
   SendAck(0,blknum);
   WriteSealinkLogfile('NAK '+stat,blknum);
   IF (ackless <> 0) AND (toterr > 20) THEN
   BEGIN
      ackless := 0;
      WriteSealinkLogfile('Overdrive disengaged',-1)
   END;
   GOTO nextblock;

ackblock:
   IF (ackless = 0) THEN
      WriteSealinkLogfile('ACK',blknum-1)
   ELSE IF ((blknum MOD 10) = 0) THEN
      WriteSealinkLogfile('Got block',blknum);

nextblock:
   stat := '';
   IF NoCarrier THEN    { Tr�ger verloren }
   BEGIN
      WriteSealinkLogfile('Lost carrier',-1);
      GOTO abort
   END;

{  hier kann bei Bedarf ein Abbruch erfolgen:
   IF Abbruch THEN BEGIN
       WriteSealinkLogfile('Aborted by operator',-1);
       GOTO abort
   END; }

   t1 := timerset(100);
   WHILE (NOT (TimeUp(t1))) DO
   BEGIN
      c := Com_GetC(1);
      IF (c = 4) AND ((endblk = 0) OR (endblk = blknum)) THEN
         GOTO endrcv;
      IF (c = 1) THEN
      BEGIN
         inblk := Com_GetC(100);
         IF (Com_GetC(100) = (inblk XOR $FF)) THEN
            GOTO blockstart
      END
   END;
   stat := 'Time';
   GOTO nakblock;

blockstart:
   c := blknum AND $FF;
   IF (inblk = 0) AND (blknum <= 1) THEN
   BEGIN
      why := GetBlock(buff);
      IF (why = '') THEN
      BEGIN
         SendAck(1,inblk);
         Move(buff,zero,128);
         left := zero.flen;
         name := FromAsciiZ(zero.fnam);
         pname := FromAsciiZ(zero.prog);
         ackless := (zero.noacks) AND (BYTE(overdrive));
         IF (left > 0) THEN
            endblk := (left + 127) DIV 128 + 1;
         IF (zero.noacks <> 0) THEN
            WriteSealinkLogfile('Overdrive engaged',-1)
         ELSE
            WriteSealinkLogfile('Overdrive disengaged',-1);
         IF (endblk <> 0) THEN
         BEGIN
            WriteSealinkLogfile('Receiving '+ IntToStr(endblk-1) + ' blocks of ' + name +' from ' + pname, 0);
         END;
         blknum := 1;
         GOTO ackblock
      END
      ELSE
      BEGIN
         stat := why;
         GOTO nakblock
      END
   END
   ELSE IF (inblk = c) THEN
   BEGIN
      why := GetBlock(buff);
      IF (why = '') THEN
      BEGIN
         IF (ackless = 0) THEN
            SendAck(1,inblk);
         WriteRohfileBuf (RohfileName, buff, 128);
//         left := left - 128;
         tries := 0;
         Inc(blknum);
         GOTO ackblock
      END
      ELSE
      BEGIN
         stat := why;
         GOTO nakblock
      END
   END
   ELSE IF (inblk < c) OR (inblk > c + 100) THEN
   BEGIN
      why := GetBlock(buff);
      SendAck(1,inblk);
      stat := 'Dup';
      GOTO ackblock
   END
   ELSE
      GOTO nextblock;

endrcv:
   SendAck(0,blknum);
   WriteSealinkLogfile('NAK EOT',-1);
   IF (Com_GetC(100) <> 4) THEN
      GOTO nakblock;
   SendAck(1,blknum);
   WriteSealinkLogfile('ACK EOT',-1);
   SendMRGCommand (CAN + CAN + CAN, [NUL], 0, 0, ad_String, RBuf, NoCarrier);
   Delay (1000);
   SendMRGCommand (ACK, [NUL], 0, 0, ad_String, RBuf, NoCarrier);

abort:
   IF (blknum = 0) THEN
      WriteSealinkLogfile('No file received',-1);
END;


{$R+,H+}
var
  AntwBuf: string;
  STX_Count: integer;
  S: string;
  AntwCode: integer;
  Merk: boolean;

begin
  Result:=false;
  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;
  AntwBuf:='';
  if AAnswerDest = ad_File then
    RohFileName:=CreateTempRohFile (WorkPath, prefix_MRG_Roh);

  try
    { Datenauslese-Befehl senden, mit ACK01-Protokoll alle "Warten auf Daten"-Datenbl�cke
      solange empfangen bis der "Daten folgen"-Block oder "keine Daten vorhanden"-Block
      empfangen wird: }
    if not SendACK01Command (ABefehl, ATimeout, ad_String, Rueckgabe, NoCarrier) then exit;

    { Antwortcode des letzten Block pr�fen: }
    STX_Count:=F_TotalChars (Rueckgabe.Antwort, STX);
    S:=ExtractString (Rueckgabe.Antwort, STX, ETX, STX_Count - 1);
    S:=STX + S + ETX;
    if not ValidKEAntwort (S, AntwCode, Rueckgabe.Fehlergruppe, Rueckgabe.Fehlercode) then exit;
    if AntwCode = kec_Keine_Daten_vorhanden then begin
      { keine Daten vorhanden, dann mit OK raus;
        -> Antwort: Leerstring bzw. Leerfile }
      Result:=true;
      exit;
    end;

    Merk:=Reset_COM_Before_Sending;    { Flag-Wert merken }
    { f�r Sealink-Protokoll: KEIN COM-Reset vor dem Versenden von Zeichen }
    Reset_COM_Before_Sending:=false;
    try
      RxSeaLink (true);        { Daten empfangen mit Sealink-Protokoll }
    finally
      Reset_COM_Before_Sending:=Merk;  { urspr�nglichen Flag-Wert wiederherstellen }
    end;

    { Fehler beim Empfang mit Sealink-Protokoll aufgetreten oder Verbindung unterbrochen: }
    if (Rueckgabe.Fehlercode <> 0) OR NoCarrier then exit;
    Result:=true;
  finally
    { Rohfile l�schen bei Fehler oder Verbindungsunterbrechung: }
    if (AAnswerDest = ad_File) AND ((not Result) OR NoCarrier) then
      DeleteFile (RohFileName);
  end;

  if AAnswerDest = ad_File then
    Rueckgabe.Antwort:=RohFileName          { R�ckgabe: Antwort = Rohfilename }
  else
    Rueckgabe.Antwort:=AntwBuf;
end;


{------------------ Methoden f�r Titschler FTL-Protokoll ----------------------}

{------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendTritschlerFTLCommand (ABefehl: string;
  ATimeout: integer; bPosQuittierung: boolean; var Rueckgabe: TRueckgabe;
  var NoCarrier: boolean): boolean;
{------------------------------------------------------------------------------}
{ FTL-Befehl senden, Quittierung empfangen und auswerten;
  �bergabe: Befehl-String
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            Flag 'bPosQuittierung' (auf false setzen, wenn keine positive Quittierung
                                    erwartet wird (z.B. bei �bertragungsende-Befehl A)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort: Empfangsdaten
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen wurden }
var
  Wdh: integer;
  FunktionNr_Soll: string;
begin
  Result:=false;
  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;

  if Copy (ABefehl, 1, 1) = 'F' then  { Befehl ist Funktionsaufruf Fnnnn }
    FunktionNr_Soll:=ExtractString (ABefehl, 'F', CR, 0)   { Funktionsnummer }
  else
    FunktionNr_Soll:='';  { kein Soll/Ist-Vergleich der Funktionsnummer in Befehl und Antwort }

  Wdh:=0;
  while Wdh < Versuche.FTLProt do begin
    { FTL-Befehl senden: }
    if not SendMRGCommand (ABefehl, [CR], 1, ATimeout, ad_String, Rueckgabe, NoCarrier) then begin
      if (Rueckgabe.Fehlergruppe = COM_KOMMERROR) AND
         (Rueckgabe.Fehlercode = KOMMERR_TIMEOUT) then begin
        if bPosQuittierung then begin  // es wird eine positive Quittierung erwartet
          inc (Wdh);
          Continue;
        end
        else begin
          Result:=true;  // Timeout ist in diesem Fall OK !
          exit;
        end;
      end else
        exit;
    end;
    { Antwort auswerten: }
    if ValidFTLAntwort (FunktionNr_Soll, Rueckgabe.Antwort,
                        Rueckgabe.Fehlergruppe, Rueckgabe.Fehlercode) then begin
      Result:=true;
      exit;
    end;
    inc (Wdh);
  end;
end;

{------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendTritschlerFTLFunktionCommand (ABefehl: string;
  AAnswerDest: TAnswerDest; ATimeout: integer; AVonDatum: TDateTime;
  var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
{------------------------------------------------------------------------------}
{ FTL-Funktionsbefehl (Fnnnn) senden, empfangene Daten pr�fen (Blocksummenpr�fung
  K23-Block) und in Rohfile schreiben; Die Antwort kann aus einem Datensatz
  (..K23..1) oder aus mehreren Teils�tzen bestehen (..K23..0 .. K23..1). Jeder
  Teilsatz mu� quittiert werden: mit '*', wenn er OK ist, ansonsten mit '?nn'
  (nn = Fehlercode, z.B. 02), dann wird der letzte Satz nochmal gesendet);
  -> Die FTL-Funktionsbefehle sehen keinen von/bis-Zeitraum vor ! Der Datenempfang
     mu� gestoppt werden, wenn alle gew�nschten Daten empfangen wurden (nicht mehr
     quittieren).
  �bergabe: Funktionsbefehl-String
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            AVonDatum ( < 0: alle Daten lesen
                       >= 0: Daten ab von-Datum lesen)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwortdaten korrekt gelesen wurden }
var
  Stop: boolean;
  PosK23: integer;
  PosK15: integer;
  S: string;
  Wdh: integer;
  AntwBuf: string;
  sDatum: string;
  dtDaten: TDateTime;
  sLog: string;

begin
  Result:=false;
  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;
  AntwBuf:='';
  if AAnswerDest = ad_File then
    RohFileName:=CreateTempRohFile (WorkPath, prefix_MRG_Roh);

  { "Datenstrom-Schleife" starten: }
  if not SendTritschlerFTLCommand (ABefehl, ATimeout, true, Rueckgabe, NoCarrier) then begin
    if AAnswerDest = ad_File then              { Rohfile l�schen bei Fehler }
      DeleteFile (RohFileName);
    exit;
  end;

  Stop:=false;
  Wdh:=0;
  while not Stop do begin
    PosK23:=Pos ('K23', Rueckgabe.Antwort);  { Position des K23-Blocks (Blockende mit Pr�fsumme) }
    if PosK23 <= 0 then begin  { kein K23-Block vorhanden }
      Rueckgabe.Fehlergruppe:=COM_FTL_ERROR;
      Rueckgabe.Fehlercode:=FTLERR_ANTWORTUNVOLLSTAENDIG;

      if AAnswerDest = ad_File then  { Rohfile l�schen bei Fehler }
        DeleteFile (RohFileName);
      exit;
    end;

    { Blocksumme pr�fen: }
    if not CheckFTLBlocksum (Rueckgabe.Antwort, PosK23) then begin
      sLog:='Blocksumme falsch !';
      if FComLogFile <> nil then
        FComLogFile.WriteMsg (sLog);  { Logfileeintrag }
      if FComTraceLog <> nil then
        FComTraceLog.WriteMsg (sLog);  { Tracelogeintrag; 03.01.2022, WW }

      Rueckgabe.Fehlergruppe:=COM_KOMMERROR;
      Rueckgabe.Fehlercode:=KOMMERR_FTL_BLOCKSICHERUNG;

      inc(Wdh);
      { begrenzte Anzahl von Versuchen: }
      if Wdh >= Versuche.FTLProt then begin
        if AAnswerDest = ad_File then  { Rohfile l�schen bei Fehler }
          DeleteFile (RohFileName);
        exit;
      end;
      { negative Quittung 'Blocksicherung falsch' senden: }
      if not SendTritschlerFTLCommand ('?06' + CR, ATimeout, true, Rueckgabe, NoCarrier) then begin
        if AAnswerDest = ad_File then  { Rohfile l�schen bei Fehler }
          DeleteFile (RohFileName);
        exit;
      end;
      Continue;  { bei negativer Quittierung kommt der gleiche Satz nochmal }
    end;
    Wdh:=0;

    S:=Copy (Rueckgabe.Antwort, 1, PosK23-1);  { Daten: alles bis zum K23 }
    if AAnswerDest = ad_File then begin
      if not WriteRohfile (RohfileName, S) then begin
        DeleteFile (RohFileName);
        Rueckgabe.Fehlergruppe:=ST_FILEERROR;
        Rueckgabe.Fehlercode:=FILEERR_COULDNOTWRITE;
        exit;
      end;
    end else
      AntwBuf:=AntwBuf + S;

    { Der Datenempfang ist beendet, wenn
        - das Datenende-Byte '1' im K23-Block enthalten ist (Ger�t hat alle vorhandenen Daten gesendet)
        - die Daten des gew�nschten Zeitraums (von-Datum !) empfangen wurden
        - das Datum in den Empfangsdaten '000000' ist (Speicherende im Ger�t ist erreicht) }
    Stop:=Copy (Rueckgabe.Antwort, PosK23+11, 1) <> '0';  { 1. Pr�fung: Datenende-Byte }
    if not Stop then begin
      PosK15:=Pos ('K15', Rueckgabe.Antwort);  { Position des K15-Blocks bei Messperiodenwerten (Befehle F0002, F0012) }
      if PosK15 > 0 then begin  { K15-Block ist vorhanden }
        if Copy (Rueckgabe.Antwort, PosK15+3, 3) = '   ' then begin
          { ein K15-Block mit Datum ist vorhanden (K15-Folgebl�cke enthalten kein Datum !) }
          sDatum:=Copy (Rueckgabe.Antwort, PosK15+6, 6);  { Datum im Format DDMMYY }
          Stop:=sDatum = '000000';  { 2. Pr�fung: Speicherende erreicht }
          if not Stop AND (AVonDatum >= 0) then begin  { 3. Pr�fung: von-Datum erreicht ? }
            if EncodeDateStr (sDatum, 'DDMMYY', dtDaten) then
              Stop:=CmpDateTime (dtDaten, AVonDatum) <= 0;
          end;
        end;
      end;
    end;

    if not Stop then begin
      { positive Quittung senden: }
      if not SendTritschlerFTLCommand ('*' + CR, ATimeout, true, Rueckgabe, NoCarrier) then begin
        if AAnswerDest = ad_File then  { Rohfile l�schen bei Fehler }
          DeleteFile (RohFileName);
        exit;
      end;
    end;
  end;  { while Stop }

  if NoCarrier then begin                           { Verbindung unterbrochen }
    if AAnswerDest = ad_File then                { Rohfile l�schen bei Fehler }
      DeleteFile (RohFileName);
    exit;
  end;

  if AAnswerDest = ad_File then
    Rueckgabe.Antwort:=RohFileName          { R�ckgabe: Antwort = Rohfilename }
  else
    Rueckgabe.Antwort:=AntwBuf;
  Result:=true;
end;

{--------------------------------------------------------------------------------------}
function TMRGCustomCommObj.CheckFTLBlocksum (Antwort: string; PosK23: integer): boolean;
{--------------------------------------------------------------------------------------}
{ Blocksummen-Pr�fung f�r FTL-Protokoll;
  �bergabe: Rohdaten-String
            Position, an welcher der K23-Block beginnt
  Ergebnis: true, wenn Blocksummen-Pr�fung OK }
var
  BlocksummeStr: string;
  BS: integer;
  i: integer;
  S: string;

begin
  BlocksummeStr:=Copy (Antwort, PosK23+5, 5);

  S:=Copy (Antwort, 1, length (Antwort)-1);   { CR abschneiden, geht nicht in Berechnung mit ein  }
  S:=F_RightTrunc (S, ' ');  { abschlie�ende Space abschneiden, gehen nicht in Berechnung mit ein }
  BS:=0;
  for i:=1 to length (S) do
    BS:=BS + ord (S [i]);

  { in S enthaltene Blocksummen-Zeichen f�r Berechnung wie Leerzeichen behandeln: }
  for i:=1 to length (BlocksummeStr) do
    BS:=BS - ord (BlocksummeStr [i]) + ord (' ');

  Result:=Format ('%.5d', [BS]) = BlocksummeStr;
end;


{--------- Methoden f�r Titschler FTL-intern-Protokoll (Parametrierung)--------}

{-------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendTritschlerFTL_internCommand (ABefehl: string;
  ATimeout: integer; var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
{-------------------------------------------------------------------------------}
{ FTL-intern-Befehl senden und Antwort empfangen;
  �bergabe: Befehl-String (ohne Blocksumme, mit abschlie�endem CR)
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort: Empfangsdaten
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen wurden }
var
  sBlockNr_Befehl: string;
  sBefehl_mit_Blocksumme: string;
  S: string;
  Stop: boolean;
  Wdh: integer;

begin
  Result:=false;
  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;

  // Blocknummer im Befehl:
  sBlockNr_Befehl:=Copy (ABefehl, 1, 3);  // Befehl beginnt immer mit 3-stelliger Blocknummer

  // Befehl um Blocksumme erweitern:
  S:=ABefehl;
  sBefehl_mit_Blocksumme:=F_Zerlegen (S, CR);  // CR wegschneiden
  sBefehl_mit_Blocksumme:=sBefehl_mit_Blocksumme +
    GetFTL_internBlocksum (ABefehl) + CR;

  Stop:=false;
  Wdh:=0;
  while not Stop AND not NoCarrier do begin
    // Befehl senden:
    if not SendMRGCommand (sBefehl_mit_Blocksumme, [CR], 1, ATimeout, ad_String,
                           Rueckgabe, NoCarrier) then exit;

    // Blocksumme der Antwort pr�fen:
    if not CheckFTL_internBlocksum (Rueckgabe.Antwort) then begin
      // begrenzte Anzahl von weiteren Versuchen bei Blocksummen-Fehler:
      inc (Wdh);
      if Wdh >= Versuche.FTLProt then begin
        // Rueckgabe: Blocksummen-Fehler }
        with Rueckgabe do begin 
          Fehlergruppe:=COM_KOMMERROR;
          Fehlercode:=KOMMERR_FTL_BLOCKSICHERUNG;
        end;
        exit;
      end;
    end
    else begin
      Stop:=true;
      // Antwort auswerten:
      if ValidFTL_internAntwort (sBlockNr_Befehl, Rueckgabe.Antwort,
                                 Rueckgabe.Fehlergruppe,
                                 Rueckgabe.Fehlercode) then
        Result:=true;
    end;
  end;  // while not Stop
end;

{-------------------------------------------------------------------}
function TMRGCustomCommObj.GetFTL_internBlocksum (S: string): string;
{-------------------------------------------------------------------}
{ Blocksumme f�r internes FTL-Protokoll bilden;
  �bergabe: String, �ber den Blocksumme gebildet werden soll (einschlie�lich des
              abschlie�enden CR)
  Ergebnis: Blocksumme als String }
var
  BS: integer;
  i: integer;

begin
  BS:=0;
  for i:=1 to length (S) do
    BS:=BS + ord (S [i]);

  Result:=Format ('%.5d', [BS]);
end;

{----------------------------------------------------------------------------}
function TMRGCustomCommObj.CheckFTL_internBlocksum (Antwort: string): boolean;
{----------------------------------------------------------------------------}
{ Blocksummen-Pr�fung f�r internes FTL-Protokoll;
  �bergabe: Rohdaten-String
  Ergebnis: true, wenn Blocksummen-Pr�fung OK }
var
  iPosCR: integer;
  BlocksummeStr: string;
  S: string;

begin
  iPosCR:=Pos (CR, Antwort);
  if iPosCR > 0 then begin
    { Antwort bis einschlie�lich des CR (CR geht in Berechnung mit ein): }
    S:=Copy (Antwort, 1, iPosCR);
    { Blocksumme in der Antwort (5 Zeichen vor dem CR): }
    BlocksummeStr:=Copy (S, iPosCR - 5, 5);
    { Die in der Antwort enthaltene Blocksumme entfernen: }
    System.Delete (S, iPosCR - 5, 5);

    Result:=GetFTL_internBlocksum (S) = BlocksummeStr;
  end else
    Result:=false;
end;


{------------------ Methoden f�r Elster DS-100-Protokoll ----------------------}

{-------------------------------------------------------------------------------------------}
function TMRGCustomCommObj.SendElsterDS100Command (ABefehl: string; AAnswerDest: TAnswerDest;
                                                   ATimeout: integer;
                                                   var Rueckgabe: TRueckgabe;
                                                   var NoCarrier: boolean): boolean;
{-------------------------------------------------------------------------------------------}
{ Elster-DS-100-Befehl senden und Antwort empfangen;
  �bergabe: Befehl-String
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  Stop: boolean;
  Wdh: integer;
  iPos: integer;
  sChecksumme: string;
  sChecksummeCalc: string;
  S: string;
  R: TRueckgabe;
  bQuittierung_mit_Antwort: boolean;

begin
  Result:=false;

  // Befehl zum Auslesen des DS-100-Datenspeichers ('?x'): Nach Quittierung durch
  // Zentrale schickt das Ger�t eine Quittierung zur�ck:
  bQuittierung_mit_Antwort:=ABefehl = GetDS100_LeseBefehl ('x');

  Stop:=false;
  Wdh:=0;
  while not Stop AND not NoCarrier do begin
    Stop:=true;
    with Rueckgabe do begin                        { Rueckgabe initalisieren }
      Fehlergruppe:=0;
      Fehlercode:=0;
      Antwort:='';
    end;

    { Befehl senden:
      -> Terminatorset: LF  = Standard-Terminator bei positiver Antwort
                        '-' = Terminator bei negativer Quittung vom Ger�t }
    if not SendMRGCommand (ABefehl, [LF, '-'], 1, ATimeout, ad_String, Rueckgabe, NoCarrier) then exit;

    if Copy (Rueckgabe.Antwort, 1, 1) = '-' then begin  // Antwort enth�lt negative Quittung
      Rueckgabe.Fehlergruppe:=COM_DS100_ERROR;
      Rueckgabe.Fehlercode:=DS100ERR_QUITTUNGNEGATIV;
      exit;
    end
    else begin  // OK-Antwort mit Daten
      // Checksumme pr�fen
      iPos:=Pos('%', Rueckgabe.Antwort);  // Position von % in Antwort
      if iPos > 0 then begin  // % ist in Antwort enthalten
        // Checksummen-String aus Antwort rauskopieren:
        sChecksumme:=Copy (Rueckgabe.Antwort, iPos + 1, 2);
        // Checksumme pr�fen:
        S:=Copy (Rueckgabe.Antwort, 2, iPos - 2);  // ohne f�hrendes Kommandozeichen
        sChecksummeCalc:=GetMOD256Checksum (S);
        if sChecksumme <> sChecksummeCalc then begin  // Checksumme falsch !
          Rueckgabe.Fehlergruppe:=COM_KOMMERROR;
          Rueckgabe.Fehlercode:=KOMMERR_CRC;

          // Negative Quittierung senden:
          if not Antwort_quittierenDS100 ('-', bQuittierung_mit_Antwort, ATimeout, R, NoCarrier) then begin
            Rueckgabe.Fehlergruppe:=R.Fehlergruppe;
            Rueckgabe.Fehlercode:=R.Fehlercode;
            exit;
          end;

          { begrenzte Anzahl von weiteren Versuchen bei Checksummen-Fehler: }
          inc (Wdh);
          if Wdh >= Versuche.CRC then exit;    // CRC-Fehler
          Stop:=false;
        end
        else begin  // Checksumme OK
          // Positive Quittierung senden:
          if not Antwort_quittierenDS100 ('+', bQuittierung_mit_Antwort, ATimeout, R, NoCarrier) then begin
            Rueckgabe.Fehlergruppe:=R.Fehlergruppe;
            Rueckgabe.Fehlercode:=R.Fehlercode;
            exit;
          end;
          Result:=true;
        end;
      end
      else begin
        Rueckgabe.Fehlergruppe:=COM_MRGERROR;
        Rueckgabe.Fehlercode:=MRGERR_ANTWORTUNVOLLSTAENDIG;
        exit;
      end;
    end;
  end;  { while not Stop }
end;

{------------------------------------------------------------------}
function TMRGCustomCommObj.Antwort_quittierenDS100 (cQuittung: char;
  bAntwort_erwartet: boolean; ATimeout: integer;
  var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
{------------------------------------------------------------------}
{ Quittierungszeichen an DS-100 senden;
  �bergabe:	Quittierungszeichen
            Flag "Antwort auf Quittierung erwartet ja/nein"
            Timeout
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Quittierung erfolgreich }
begin
  Result:=false;
  with Rueckgabe do begin                        { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;

  { Quittung senden: }
  if not SendMRGCommand (cQuittung, [], 0, 0, ad_String, Rueckgabe, NoCarrier) then exit;

	if bAntwort_erwartet then begin
    { Antwort auf Quittung empfangen: }
    if not SendMRGCommand ('', [cQuittung], 1, ATimeout, ad_String, Rueckgabe, NoCarrier) then exit;
    { Antwort auf Quittung auswerten: }
		if Rueckgabe.Antwort <> cQuittung then begin
      Rueckgabe.Fehlergruppe:=COM_DS100_ERROR;
      Rueckgabe.Fehlercode:=DS100ERR_QUITTUNGUNERWARTET;  // Unerwartete Quittung
      exit;
    end;
  end else
    Delay (20);  // damit Quittierung sicher versendet wird, wenn anschlie�end
                 // erneut ein Befehl versendet wird
  Result:=true;
end;

{------------------------------------------------------------------------------}
function TMRGCustomCommObj.SetDS100Parameter (cBefehlscode: char; sWert: string;
  ATimeout: integer; var bErfolgreich: boolean; var Rueckgabe: TRueckgabe;
  var NoCarrier: boolean): boolean;
{------------------------------------------------------------------------------}
{ Parameter im DS-100 �ndern;
  -> Dient auch zum Senden des Zutrittscodes. Der Zutrittscode wird zum Verstellen
     von DS-100-Parametern ben�tigt.
  �bergabe: neuer Parameterwert bzw. Zutrittscode
  R�ckgabe: Flag "erfolgreich" ja/nein
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis:	true, wenn Parameter �ndern ohne Fehler }
var
  ABefehl: string;


begin
  Result:=false;

	bErfolgreich:=false;	// Vorbelegung R�ckgabe
  with Rueckgabe do begin                        { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:='';
  end;

  { Befehl mit Befehlscode senden:
    -> Terminatorset: LF  = Standard-Terminator bei positiver Antwort
                      '-' = Terminator bei negativer Quittung vom Ger�t }
  ABefehl:=GetDS100_SchreibBefehl (cBefehlscode);
  if not SendMRGCommand (ABefehl, [cBefehlscode], 1, ATimeout, ad_String, Rueckgabe, NoCarrier) then exit;
  if Copy (Rueckgabe.Antwort, 1, 1) <> cBefehlscode then begin  // Antwort mu� gesendeter Befehlscode sein
    Rueckgabe.Fehlergruppe:=COM_MRGERROR;
    Rueckgabe.Fehlercode:=MRGERR_ANTWORTUNERWARTET;
    exit;
  end;

  { Wert senden: }
  ABefehl:=sWert + '%' + GetMOD256Checksum (sWert) + CR + LF;
  if not SendMRGCommand (ABefehl, ['+', '-'], 1, ATimeout, ad_String, Rueckgabe, NoCarrier) then exit;
  if Copy (Rueckgabe.Antwort, 1, 1) = '+' then  // Parameter�nderung/Zutrittscode senden OK
		bErfolgreich:=true;

	Result:=true;
end;

{---------------------------------------------------------------}
function TMRGCustomCommObj.GetMOD256Checksum (S: string): string;
{---------------------------------------------------------------}
{ MOD256-Checksumme bilden;
  �bergabe: String, �ber den Checksumme gebildet werden soll
  Ergebnis: Checksumme als String }
var
  L: integer;
  i: integer;

begin
  L:=0;
  for i:=1 to length (S) do
    L:=L + Ord (S[i]);
  L:=L MOD 256;
  Result:=IntToHex (L, 2);
end;


{---------------------- Methoden f�r Modbus-Protokoll -------------------------}

{-----------------------------------------------------------------------------}
function TMRGCustomCommObj.SendModbusQuery (ABefehl: string; ATimeout: integer;
  var Rueckgabe: TRueckgabe; var NoCarrier: boolean): boolean;
{-----------------------------------------------------------------------------}
{ Modbus-Befehl senden und Antwort empfangen;
  �bergabe: Befehl-String (Query)
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (Reponse)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  Stop: boolean;
  Wdh: integer;
  EndezeichenSet: TEndezeichenSet;
  EndezeichenAnzahl: integer;

begin
  Result:=false;

  { Terminatorset: LF = Terminator bei Modbus ASCII
                   Kein Terminator bei Modbus RTU und TCP ! }
   if FModbusModus = modbus_ASCII then begin
     EndezeichenSet:=[LF];
     EndezeichenAnzahl:=1;
   end
   else begin
     EndezeichenSet:=[];
     EndezeichenAnzahl:=0;
   end;

  Stop:=false;
  Wdh:=0;
  while not Stop AND not NoCarrier do begin
    Stop:=true;
    { Befehl senden: }
    if not SendMRGCommand (ABefehl, EndezeichenSet, EndezeichenAnzahl, ATimeout,
                           ad_String, Rueckgabe, NoCarrier) then exit;
    { Checksumme pr�fen: }
    if not Check_ModbusLRC_CRC (Rueckgabe) then begin
      { begrenzte Anzahl von weiteren Versuchen bei Checksummen-Fehler: }
      inc (Wdh);
      if Wdh >= Versuche.ModbusProtLRC_CRC then exit;  // LRC/CRC-Fehler
      Stop:=false;
    end else
      Result:=true;
  end;  { while not Stop }
end;

{----------------------------------------------------------------------------------}
function TMRGCustomCommObj.Check_ModbusLRC_CRC (var Rueckgabe: TRueckgabe): boolean;
{----------------------------------------------------------------------------------}
{ LRC (ASCII) bzw. CRC (RTU) in empfangenen Modbus-Daten pr�fen;
  R�ckgabe: Rueckgabe-Record
  Ergebnis: true, wenn Pr�fsumme OK }
var
  S: string;
  sBin: string;
  CRC_LRC: word;
  CRC_LRC_Calc: word;
  sLog: string;

begin
  Result:=true;
  // Checksumme pr�fen bei Modbus RTU und ASCII (bei Modbus TCP gibts keine Pr�fsumme)
  if FModbusModus = modbus_ASCII then begin  // ASCII
    S:=ExtractString (Rueckgabe.Antwort, ':', CR, 0);  // Datenteil zwischen Start und Ende (ASCII hex)
    sBin:=Hex2Bin (S);  // Hex-String in Bin�r-String wandeln

    S:=Copy (sBin, length (sBin), 1);  // Bin�r-LRC: letztes Zeichen
    CRC_LRC:=Bin2Byte (S);
    S:=Copy (sBin, 1, length (sBin) - 1);  // Bin�r-Datenteil, �ber den LRC gebildet wird
    CRC_LRC_Calc:=Get_Modbus_LRC (S);

    if CRC_LRC <> CRC_LRC_Calc then begin         { LRC-Fehler in der Response }
      sLog:='LRC falsch !';
      if FComLogFile <> nil then
        FComLogFile.WriteMsg (sLog);  { Logfileeintrag }
      if FComTraceLog <> nil then
        FComTraceLog.WriteMsg (sLog);  { Tracelogeintrag; 03.01.2022, WW }

      with Rueckgabe do begin
        Fehlergruppe:=COM_KOMMERROR;
        Fehlercode:=KOMMERR_LRC;
        Antwort:='';
      end;
      Result:=false;
    end;
  end
  else if FModbusModus = modbus_RTU then begin  // RTU
    S:=Copy (Rueckgabe.Antwort, length (Rueckgabe.Antwort) - 1, 2);  // CRC: die letzten 2 Zeichen
    CRC_LRC:=Bin2Word (S, bo_BigEndian);  // CRC immer Big-Endian
    S:=Copy (Rueckgabe.Antwort, 1, length (Rueckgabe.Antwort) - 2);  // Datenteil, �ber den CRC gebildet wird
    CRC_LRC_Calc:=Get_Modbus_CRC (S);

    if CRC_LRC <> CRC_LRC_Calc then begin         { CRC-Fehler in der Response }
      sLog:='CRC falsch !';
      if FComLogFile <> nil then
        FComLogFile.WriteMsg (sLog);  { Logfileeintrag }
      if FComTraceLog <> nil then
        FComTraceLog.WriteMsg (sLog);  { Tracelogeintrag; 03.01.2022, WW }

      with Rueckgabe do begin
        Fehlergruppe:=COM_KOMMERROR;
        Fehlercode:=KOMMERR_CRC;
        Antwort:='';
      end;
      Result:=false;
    end;
  end;
end;

{--------------------------------------------------------------------------}
function TMRGCustomCommObj.EndOfModbusResponse (sResponse: string): boolean;
{--------------------------------------------------------------------------}
{ Pr�ft auf vollst�ndige Modbus-Response (RTU, TCP);
  �bergabe: Response
  Ergebnis: true, wenn Response vollst�ndig }
var
  iFktCode: byte;
  iByteCount: integer;
  iLenResp_Soll: integer;
  sResp: string;
  S: string;
  iLenCRC: integer;

begin
  Result:=false;

  if FModbusModus in [modbus_RTU, modbus_TCPIP] then begin
    if FModbusModus = modbus_RTU then begin
      sResp:=sResponse;
      iLenCRC:=2;  // 2 Bytes CRC
    end
    else begin  // TCP
      // MBAP-Header: F�r die Pr�fung die ersten 6 Bytes wegschneiden (Trans-
      // aktionsnummer, Protokollnummer, Anzahl der Zeichen)
      sResp:=Copy (sResponse, 7, length (sResponse));
      iLenCRC:=0;  // Ohne CRC
    end;

    // Funktionscode enthalten ?
    if length (sResp) < 2 then exit;
    iFktCode:=Bin2Byte (sResp [2]);

    if (iFktCode AND $80) <> 0 then  { Bit 7 = 1: Exception Response; 11.02.2020, WW }
      iLenResp_Soll:=3 + iLenCRC  // fixe L�nge
    else begin
      // Abh�ngig von Funktionscode: Fixe Response-L�nge oder variabel ?
      case iFktCode of
         // Funktionscodes mit fixer L�nge
         7: iLenResp_Soll:=3 + iLenCRC;

         5, 6, 8, 11, 15, 16:
            iLenResp_Soll:=6 + iLenCRC;

        22: iLenResp_Soll:=8 + iLenCRC;
      else
        iLenResp_Soll:=-1;
      end;

      if iLenResp_Soll <= -1 then begin
        case iFktCode of
          // Funktionscodes mit variabler L�nge: Byte Count auswerten
          1, 2, 3, 4, 12, 17, 20, 21, 23:
            begin
              if length (sResp) < 3 then exit;
              iByteCount:=Bin2Byte (sResp [3]);
              iLenResp_Soll:=3 + iByteCount + iLenCRC;
            end;

          24:
            begin
              if length (sResp) < 4 then exit;
              S:=Copy (sResp, 3,2);
              iByteCount:=Bin2Word (S, bo_BigEndian);  // Byte Count immer Big-Endian
              iLenResp_Soll:=4 + iByteCount + iLenCRC;
            end;
        end;
      end;
    end;

    if iLenResp_Soll > -1 then begin
      if length (sResp) >= iLenResp_Soll then  // vollst�ndige RTU/TCP-Response
        Result:=true;  //
    end else  // unbekannter Funktionscode
      Result:=false;  // ge�ndert auf false (sonst erfolgt CRC-Pr�fung); 11.02.2020, WW
  end
  else if FModbusModus in [modbus_ASCII] then  // 11.02.2020, WW
    Result:=true
  else  // unbekannter Modbus-Modus
    Result:=false;  // ge�ndert auf false (sonst erfolgt CRC-Pr�fung); 11.02.2020, WW
end;


{----------------------------- Byte-Korrektur ---------------------------------}

{-----------------------------------------------------------}
procedure TMRGCustomCommObj.CorrectAscii8Bit (var S: string);
{-----------------------------------------------------------}
{ 8. Bit in 7-Bit-Daten l�schen (Korrektur f�r UNIGAS 300 lt. Vorgabe der Fa.
  Wigersma & Sikkema)
  �bergabe: Empfangsdaten des Ger�ts
  R�ckgabe: Korrigierte Empfangsdaten }
var
  i: integer;

begin
  for i:=1 to length(S) do begin
    if S[i] >= #128 then
      S[i]:=Chr (Ord (S[i]) - 128);
  end;
end;



{ TMRGSerialCustomCommObj }

{-----------------------------------------------------------------------------------}
constructor TMRGSerialCustomCommObj.Create (AOwner: TComponent; AWorkPath: TFileName;
                                            AComLogFile: TComLogFile);
{-----------------------------------------------------------------------------------}
begin
  FSerial:=TSerial.Create (AOwner);
  with FSerial do begin
    { Puffergr��en f�r MRG-Kommunikation: }
    BufSizeTrm:=2048;                                        { Sendepuffergr��e }
    BufSizeRec:=8192;                                     { Empfangspuffergr��e }
    { DTR- und RTS-Leitung setzen: }
    DTRActive:=true;
    RTSActive:=true;
    { ohne RTS/CTS-Handshake: }
    HandshakeRTSCTS:=false;
  end;

  inherited Create(AWorkPath, AComLogFile);

  DelayWakeCommand:=CDelayWakeCommand_Seriell;
end;

{-----------------------------------------}
destructor TMRGSerialCustomCommObj.Destroy;
{-----------------------------------------}
begin
  CloseCOM (FSerial, FComLogFile);
  FSerial.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------------}
function TMRGSerialCustomCommObj.Connect (AComPort: integer; ABaudrate: integer;
                                          ADatabits: TDataBits; AParityBit: TParityBit;
                                          AStopBits: TStopBits): integer;
{-------------------------------------------------------------------------------------}
{ Verf�gbarkeit der seriellen Schnittstelle pr�fen, Schnittstellenparameter setzen und
  Schnittstelle �ffnen;
  �bergabe: AComPort
            ABaudrate
            ADataBits
            AParityBit
            AStopBit
  Ergebnis:  0 = Schnittstelle konnte korrekt ge�ffnet werden
            -1 = Schnittstelle ist nicht vorhanden
            -2 = Schnittstelle konnte nicht ge�ffnet werden }
begin
  Result:=ConnectCOM (FSerial, FComLogFile, AComPort, ABaudrate, ADatabits,
                      AParityBit, AStopBits);
end;

{-------------------------------------------------------------------------------------------}
procedure TMRGSerialCustomCommObj.SetAbrufgruppe (Gruppe: integer; sDataFormat: string = '');
{-------------------------------------------------------------------------------------------}
{ Abrufgruppe setzen, f�r die der Abruf erfolgen soll und optionale Einstellung
  des seriellen Datenformats (Datenbits, Parit�t, Stopbits);
  -> Das Datenformat ist bei Modem-Befehlen (at) abh�ngig vom verwendeten PC-seitigen
     Modem, bei MRG-Befehlen abh�ngig vom Modem auf der Ger�teseite !
  �bergaben: Abrufgruppe
             Datenformat (z.B. 8N1, 7E1) }
var
  newDataBits: TDataBits;
  newParity: TParityBit;
  newStopBits: TStopBits;
  dbBuf: TDataBits;
  parBuf: TParityBit;
  sbBuf: TStopBits;

begin
  inherited SetAbrufgruppe (Gruppe, sDataFormat);

  { spezielle Belegungen der seriellen Schnittstellenparameter f�r Modem-Abruf
   je nach Abrufgruppe: }
  case FAbrufgruppe of
    0,  { Standardeinstellung f�r Modembefehle und Rufentgegennahme }
    1,  { "FUP-Ger�te" (ACK01-Protokoll) }
    2,  { MRG 800, 2001 }
    5,  { Elster DL 240, EK260 }
    8,  { Tritschler-Ger�te mit IEC-Protokoll (VC2, TTG) }
   11:  { Actaris Sparklog }
     begin
       newDataBits:=db_7;
       newParity:=even;
       newStopBits:=sb_1;
     end;

    9:  { Tritschler-Ger�te mit FTL-Protokoll (TTG) }
      begin
        newDataBits:=db_7;
        newParity:=none;
        newStopBits:=sb_2;
      end;

    3,  { MRG 800PTB }
    4,  { EC 694, MRG 910 }
    6,  { KE-Anlagen }
    7,  { Datacon FWU }
   10,  { Actaris Corus }
   12,  { EC 900 }
   13,  { DS-100 }
   14,  { Primus/Prilog 400, TME400, RSM200 }
   15:  { Standard 8N1 (z.B. FTL-intern-Protokoll }
     begin
       newDataBits:=db_8;
       newParity:=none;
       newStopBits:=sb_1;
     end;
  else  // Standard: wie Abrufgruppe 0
    newDataBits:=db_7;
    newParity:=even;
    newStopBits:=sb_1;
  end;

  // wenn ein bekanntes Datenformat �bergeben wurde, �bersteuert es die Standard-
  // Datenformate der einzelnen Abrufgruppen:
  if length (sDataFormat) > 0 then
    if GetSerialDataFormat (sDataFormat, dbBuf, parBuf, sbBuf) then begin
      newDataBits:=dbBuf;
      newParity:=parBuf;
      newStopBits:=sbBuf;
    end;

  // serielle Schnittstellen-Parameter setzen:
  FSerial.SetCommParameter (FSerial.Baudrate, newDataBits, newParity, newStopBits);
end;

{--------------------------------------------------}
procedure TMRGSerialCustomCommObj.WakeUpMRG_ByBreak;
{--------------------------------------------------}
{ Batterie-Ger�t mit Break aufwecken;
 -> bei manchen MRG 800 PTB die einzige M�glichkeit, das Ger�t zu wecken
 Hinweis: Das Wecken mit Break sollte als allerletzte M�glichkeit verwendet werden,
          ein Ger�t zu wecken (zuvor mit Dummy-Befehl versuchen !). Das Break kann
          bei Funkmodem-Verbindungen die Daten�bertragung zum Ger�t lahmlegen
          (festgestellt 01/2002 mit MRG 910 im 800er-Modus) ! }
begin
(*  Delay (1000);      { Wartezeit, um zu verhindern, da� das Break nicht vor dem
                       CONNECT im MRG ankommt }
                       -> nicht mehr n�tig; Methode wird jetzt nach Timeout beim
                          ersten Ger�tebefehl aufgerufen } *)
  FSerial.SetBreakZustand;
  Delay (300);
  FSerial.ClearBreakZustand;
  Delay (1500);      { Wartezeit, um zu verhindern, da� evtl. vom MRG gesendete
                       Daten nicht den nachfolgenden Sendebefehl st�ren }
end;

{-----------------------------------------------------------------------------}
function TMRGSerialCustomCommObj.Rufabfrage (var Ruf_angekommen: boolean;
                                             var Fehlergruppe: integer;
                                             var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------}
{ Schnittstelle auf ankommenden Ruf pr�fen;
  R�ckgabe: Ruf_angekommen (true, wenn Ruf anliegt)
            Fehlergruppe/-code
  Ergebnis: true, wenn Rufabfrage ok }
begin
  Result:=true;
  Ruf_angekommen:=false;  // Serielle Rufabfrage inaktiv
  Fehlergruppe:=0;
  Fehlercode:=0;
end;


{------------------------------------------------------------------------------}

{-------------------------------------------}
procedure TMRGSerialCustomCommObj.Comm_Reset;
{-------------------------------------------}
begin
  FSerial.ResetComm;  // COM-Reset (Schnittstellenfehler l�schen, Sende- und Empfangspuffer leeren)
end;

{----------------------------------------------------------------------}
function TMRGSerialCustomCommObj.Comm_SendData (sData: string): boolean;
{----------------------------------------------------------------------}
begin
  FSerial.TransmittText(sData, false, false);  // Befehl senden ohne COM-Reset
  Result:=true;
end;

{----------------------------------------------------------------}
function TMRGSerialCustomCommObj.Comm_SendChar (c: char): boolean;
{----------------------------------------------------------------}
begin
  FSerial.TransmittChar(c);  // Zeichen senden
  Result:=true;
end;

{-------------------------------------------------------------------------------------------}
function TMRGSerialCustomCommObj.Comm_ReceiveData (AnzCharsToReceive, ChToRec_Left: cardinal;
                                                   bSaveFirstCommError: boolean): string;
{-------------------------------------------------------------------------------------------}
var
  CommErrBuf: cardinal;
  ComStat: TComStat;
  n: cardinal;
  S: string;

begin
  with FSerial do begin
    if ClearCommError(SerHandle,CommErrBuf,@ComStat) then begin  { Schnittstellenstatus abfragen }
      n:=ComStat.cbInQue;  // Anzahl der Zeichen im Eingangspuffer
      // von ClearCommError erkanntes Break nicht als Fehler behandeln:
      // wegen Elster EK260 mit h�ufigem Break beim Datenempfang, 11.04.2003, WW }
      CommErrBuf:=CommErrBuf AND not ce_Break;
    end
    else begin
      n:=0;

      // Errorcode aus 'GetLastError': 28.05.2013, WW
      CommErrBuf:=GetLastError;             
      CommErrBuf:=CommErrBuf + ERR_OFFSET_GETLASTERROR;  // Kennzeichnung 'GetLastError'
    end;

    // evtl. nur ersten aufgetretenen Schnittstellenfehler merken f�r R�ckgabe
    if (FCommError = 0) OR not bSaveFirstCommError then
      FCommError:=CommErrBuf;

    if AnzCharsToReceive > 0 then     { wenn feste Anzahl an zu empfangenden Zeichen �bergeben wurde }
      if n > ChToRec_Left then n:=ChToRec_Left;     { ...Anzahl begrenzen }

    if n > 0 then begin
      SetLength(S, n);  // String-Puffer bereitstellen; 12.09.2008 WW
      SetLength(S, ReceiveData(Pointer(S)^, Length(S)));  // Zeichen aus Empfangspuffer lesen
    end else
      S:='';
  end;
  Result:=S;
end;

{------------------------------------------------------------------------------------}
function TMRGSerialCustomCommObj.Comm_Connection (var Rueckgabe: TRueckgabe): boolean;
{------------------------------------------------------------------------------------}
begin
  Result:=true;  // Verbindung immer vorhanden
end;

{-------------------------------------------------}
procedure TMRGSerialCustomCommObj.Comm_ClearRecBuf;
{-------------------------------------------------}
var
  ComStat: TComStat;
  CommError: cardinal;

begin
  with FSerial do begin
    { Empfangspuffer leeren, solange noch Zeichen einlaufen: }
    while ClearCommError(SerHandle,CommError,@ComStat) do begin
      Delay (50);
      if ComStat.cbInQue > 0 then
        PurgeComm (SerHandle, PURGE_RXCLEAR)          { Empfangspuffer leeren }
      else
        Break;
    end;
  end;
end;

{------------------------------------------------------------------------------------}
procedure TMRGSerialCustomCommObj.Comm_SetFehlerRueckgabe (NoCarrier: boolean;
                                                           var Rueckgabe: TRueckgabe);
{------------------------------------------------------------------------------------}
begin
  { Rueckgabe belegen mit Schnittstellenfehler: }
  with Rueckgabe do begin
    Fehlergruppe:=COM_ERROR;
    Fehlercode:=FCommError;
    Antwort:='';
  end;
end;


{ TMRGModemCommObj }

{----------------------------------------------------------------------------}
constructor TMRGModemCommObj.Create (AOwner: TComponent; AWorkPath: TFileName;
                                     AComLogFile: TComLogFile);
{----------------------------------------------------------------------------}
begin
  inherited Create(AOwner, AWorkPath, AComLogFile);

  DelayWakeCommand:=CDelayWakeCommand_Modem;
  FSerial.HandshakeRTSCTS:=true;  { mit RTS/CTS-Handshake }

  MaxModemBaudrate:=br_057600;
  SetDCDCheck (false);

  { Logfile f�r Sealink-Protokoll: }
  if AComLogFile <> nil then
    SealinkLogFilename:=AComLogfile.Path + 'COM_' + Format ('%.3d_Sealink', [FSerial.ComPort]) + '.LOG';
end;

{------------------------------------------------------}
procedure TMRGModemCommObj.SetDCDCheck (OnOff: boolean);
{------------------------------------------------------}
{ DCD-�berwachung einschalten (OnOff = true) bzw. ausschalten (OnOff = false) }
begin
  DCDCheck:=OnOff;
end;

{--------------------------------------------------------------------------------------}
function TMRGModemCommObj.SendModemCommand (ABefehl: string; ATimeout: integer;
                                            var Rueckgabe: TRueckgabe;
                                            WaitFor_OK_ERROR: boolean = false): boolean;
{--------------------------------------------------------------------------------------}
{ Modem-Befehl senden und Antwort empfangen;
  �bergabe: Befehl-String
            ATimeout (Zeit, die maximal gewartet wird bis das erste Zeichen empfangen wird)
            Flag 'WaitFor_OK_ERROR' (wenn true, wird auf OK oder ERROR in der Antwort gewartet)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (Empfangsdaten)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
var
  S: string;
  AntwortEmpfangen: boolean;
  First: boolean;
  TickCount: cardinal;

begin
  Timeout:=ATimeout;
  AnswerDest:=ad_String;

  { Empfangsbereitschaft des Modem pr�fen (eigentlich m��te man auch das DSR-Signal
    pr�fen, aber das wird standardm��ig nicht immer unterst�tzt, z.B. ke LOGEM9600): }
  if FSerial.HandshakeRTSCTS AND not FSerial.CheckCTSSignal then begin
    { CTS-Signal ist aus -> Modem nicht angeschlossen }
    with Rueckgabe do begin
      Fehlergruppe:=COM_MODEMERROR;
      Fehlercode:=CME_CTS;
      Antwort:='';
    end;
    Result:=false;
    exit;
  end;

  Befehl:=ABefehl;
  BefehlSenden;                                           { Befehl abschicken }

  with Rueckgabe do begin                           { Rueckgabe initalisieren }
    Fehlergruppe:=0;
    Fehlercode:=0;
    Antwort:=''
  end;

  { Antwort auslesen: }
  AntwortEmpfangen:=false;
  TickCount:=0;
  FCommError:=0;
  First:=true;
  while not AntwortEmpfangen AND (FCommError = 0) do begin
    Sleep (1);
    Application.ProcessMessages;
    Timeout_Elapsed;  // Verstrichene Zeit messen f�r Timeout-�berwachung; 19.12.2022, WW

    { Antwort lesen: }
    s:=Comm_ReceiveData (0, 0, false);
    if length (s) > 0 then begin
      TickCount:=GetTickCount;
      TimeoutCount:=0;
      { Timeout-Anzeige: }
      if Assigned (CBTimeout) then
        CBTimeout ('');

      if First then begin                   { die ersten ausgelesenen Zeichen }
        First:=false;
        if FComLogFile <> nil then
          FComLogFile.Write ('E', s);    { Logfileeintrag mit Kopf "Empfangsdaten" }
        if FComTraceLog <> nil then
          FComTraceLog.Write ('E', s);    { Tracelogeintrag mit Kopf "Empfangsdaten"; 03.01.2022, WW }
      end  { if First }
      else begin
        if FComLogFile <> nil then
          FComLogFile.Write ('D', s);    { Logfileeintrag nur Daten }
        if FComTraceLog <> nil then
          FComTraceLog.Write ('D', s);    { Tracelogeintrag nur Daten; 03.01.2022, WW }
      end;
      Rueckgabe.Antwort:=Rueckgabe.Antwort + s;

      { RxD-Anzeige: }
      if Assigned (CBRxD) then
        CBRxD (SonderzeichenString (Rueckgabe.Antwort),
               IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
    end   { length (s) > 0 }
    else begin
      { die Modem-Antwort kann als vollst�ndig empfangen gelten, wenn
        innerhalb einer kurzen Zeitspanne keine neuen Zeichen mehr kommen: }
      if TickCount > 0 then begin
        if WaitFor_OK_ERROR then   // wenn auf OK oder ERROR in der Antwort gewartet werden soll
          if (Pos ('OK', Rueckgabe.Antwort) = 0) AND
             (Pos ('ERROR', Rueckgabe.Antwort) = 0) then
            TickCount:=GetTickCount;   // TickCount updaten, damit nicht vorzeitig abgebrochen wird

        AntwortEmpfangen:=(cardinal (GetTickCount) - TickCount) > cardinal (Timeouts.ModemAntwort);
      end;

      if TimeoutCount >= Timeout then begin       { Timeout beim Datenempfang }
        with Rueckgabe do begin
          Fehlergruppe:=COM_KOMMERROR;
          Fehlercode:=KOMMERR_TIMEOUT;
          { bei Timeout Antwort nicht l�schen }
        end;
        Break;
      end;
    end;
  end; { while }

  { Timeout-Anzeige: }
  if Assigned (CBTimeout) then
    CBTimeout ('');
  { RxD-Anzeige: }
  if Assigned (CBRxD) then
    CBRxD (SonderzeichenString (Rueckgabe.Antwort), IntToStr (length (Rueckgabe.Antwort)) + ' Byte');
  Application.ProcessMessages;

  if FCommError <> 0 then begin         { Schnittstellenfehler ist aufgetreten }
    with Rueckgabe do begin    { Rueckgabe belegen mit Schnittstellenfehler }
      Fehlergruppe:=COM_ERROR;
      Fehlercode:=FCommError;
      Antwort:='';
    end;

    { Empfangspuffer leeren: }
    Comm_ClearRecBuf;
  end;
  Result:=Rueckgabe.Fehlergruppe = 0;
end;

{---------------------------------------------------------------------------------------}
function TMRGModemCommObj.SendMRGCommand (ABefehl: string; AEndezeichen: TEndezeichenSet;
                                          AEndezeichenAnzahl: integer;
                                          ATimeout: integer; AAnswerDest: TAnswerDest;
                                          var Rueckgabe: TRueckgabe;
                                          var NoCarrier: boolean;
                                          AnzCharsToReceive: cardinal = 0;
                                          ATimeoutFirstRec: integer = 0): boolean;
{---------------------------------------------------------------------------------------}
{ MRG-Befehl senden und Antwort empfangen;
  �bergabe: Befehl-String
            in der Antwort zu erwartendes Endezeichen
            Anzahl, der in der Antwort zu erwartenden Endezeichen
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
            optional:
            AnzCharsToReceive feste Anzahl an zu empfangenen Zeichen (wenn > 0 wird
                              nach den �bergebenen n Zeichen der Zeichenempfang mit
                              OK beendet, unabh�ngig vom �bergebenen Endezeichen);
                              ab 24.06.2003 WW
            ATimeoutFirstRec (max. Wartezeit bis zu den ersten empfangenen Zeichen)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
            NoCarrier (true, wenn Verbindung unterbrochen wurde)
  Ergebnis: true, wenn Antwort korrekt gelesen werden konnte }
begin
  { Empfangsbereitschaft des Modem pr�fen (eigentlich m��te man auch das DSR-Signal
    pr�fen, aber das wird standardm��ig nicht immer unterst�tzt, z.B. ke LOGEM9600): }
  if FSerial.HandshakeRTSCTS AND not FSerial.CheckCTSSignal then begin
    { CTS-Signal ist aus -> Modem nicht angeschlossen }
    with Rueckgabe do begin
      Fehlergruppe:=COM_MODEMERROR;
      Fehlercode:=CME_CTS;
      Antwort:='';
    end;
    Result:=false;
    exit;
  end;

  Result:=inherited SendMRGCommand (ABefehl, AEndezeichen, AEndezeichenAnzahl,
                                    ATimeout, AAnswerDest, Rueckgabe,  NoCarrier,
                                    AnzCharsToReceive, ATimeoutFirstRec);
end;

{----------------------------------------------------------------------------------------------------}
function TMRGModemCommObj.SendProtokoll (ABefehl: string; ATimeout: integer; AAnswerDest: TAnswerDest;
                                         var Rueckgabe: TRueckgabe): boolean;
{----------------------------------------------------------------------------------------------------}
{ Protokoll-Befehl senden;
  �bergabe: Befehl-String
            ATimeout (Zeit, nach der nicht mehr mit Empfangsdaten zu rechnen ist)
            AAnswerDest (ad_String: Empfangsdaten -> Antwort-String
                         ad_File: Empfangsdaten -> Rohfile)
  R�ckgabe: Rueckgabe-Record
              -> Fehlergruppe
              -> Fehlercode
              -> Antwort (AAnswerDest = ad_String: Empfangsdaten
                          AAnswerDest = ad_File: Name des Rohfiles (kompletter Pfad)
  Ergebnis: true, wenn Befehl gesendet werden konnte }
begin
  { Empfangsbereitschaft des Modem pr�fen (eigentlich m��te man auch das DSR-Signal
    pr�fen, aber das wird standardm��ig nicht immer unterst�tzt, z.B. ke LOGEM9600): }
  if FSerial.HandshakeRTSCTS AND not FSerial.CheckCTSSignal then begin
    { CTS-Signal ist aus -> Modem nicht angeschlossen }
    with Rueckgabe do begin
      Fehlergruppe:=COM_MODEMERROR;
      Fehlercode:=CME_CTS;
      Antwort:='';
    end;
    Result:=false;
    exit;
  end;

  Result:=inherited SendProtokoll (ABefehl, ATimeout, AAnswerDest, Rueckgabe);
end;

{-------------------------------------------------------------------------------------------------}
function TMRGModemCommObj.Rufabfrage (var Ruf_angekommen: boolean;
                                      var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-------------------------------------------------------------------------------------------------}
{ Schnittstelle auf ankommenden Ruf pr�fen;
  Anmerkung: 1. Eine erfolgreiche Rufentgegennahme kann nur f�r solche MRGs durchgef�hrt
             werden, deren Datenformat (Parit�t, Datenbits, Stopbits) mit dem
             �bereinstimmt, welches bei der Modem-Initialisierung verwendet wird.
             2. Ein ankommender Ruf k�nnte auch �ber das RI-Signal erkannt werden.
             Es kann dann jedoch nicht die Anzahl der Rings gez�hlt werden, da
             manche Modems bei ankommendem Ruf das RI-Signal st�ndig anstehen lassen.
  R�ckgabe: Ruf_angekommen (true, wenn Ruf anliegt)
            Fehlergruppe/-code
  Ergebnis: true, wenn Rufabfrage ok }
Const
  CTimeout_ModemRing = 1;   { auf einen schon anstehenden RING mu� nicht lang gewartet werden }
var
  R: TRueckgabe;
  dummy: boolean;
  ModemAntwort: string;
begin
  Result:=false;
  Ruf_angekommen:=false;
  Fehlergruppe:=0;
  Fehlercode:=0;

  if SendCommand ('', [LF], 2, CTimeout_ModemRing, ad_String, R, dummy) then begin
    { Antworten: RING = Ruf steht an
                 alle anderen sind unerwartete Antworten }
    ModemAntwort:=ExtractString (R.Antwort, LF, CR, 0);
    if ModemAntwort = 'RING' then begin
      Ruf_angekommen:=true;
      Result:=true;
    end;
  end
  else if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) then
    Result:=true      { keine Antwort -> es steht kein Ruf an; Fehlergruppe/-code: OK }
  else begin    { Fehler ist aufgetreten }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
  end;
end;


{------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
function TMRGModemCommObj.Comm_Connection (var Rueckgabe: TRueckgabe): boolean;
{-----------------------------------------------------------------------------}
begin
  { DCD-�berwachung (RLSD-Signal): }
  if DCDCheck then begin
    Result:=FSerial.CheckRLSDSignal;  { false: DCD-Signal ist aus -> Verbindung unterbrochen }
    if not Result then begin
      with Rueckgabe do begin
        Fehlergruppe:=COM_MODEMERROR;
        Fehlercode:=CME_DCD;
        Antwort:='';
      end;
      { Modemstatus-Anzeige: }
      if Assigned (CBModemstatus) then
        CBModemstatus ('');
    end;
  end else
    Result:=true;
end;

{-----------------------------------------------------------------------------}
procedure TMRGModemCommObj.Comm_SetFehlerRueckgabe (NoCarrier: boolean;
                                                    var Rueckgabe: TRueckgabe);
{-----------------------------------------------------------------------------}
begin
  { Rueckgabe belegen mit Schnittstellenfehler:
    -> R�ckgabe "Verbindung unterbrochen" nicht �berschreiben, wenn mit DCDCheck }
  if not (DCDCheck AND NoCarrier) then
    inherited Comm_SetFehlerRueckgabe (NoCarrier, Rueckgabe);
end;

end.

