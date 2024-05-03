{******************************************************************************}
{* Client-Klasse f�r digitale Signatur                                        *}
{* 16.11.2011 WW  Kommunikation mit Signaturserver, Verifizierung von signier-*}
{*                ten Daten                                                   *}
{* 27.01.2012 WW  zus�tzliche Informationen in Verify-Logdatei; eine Verify   *}
{*                -Logdatei f�r alle Abruflinien; Entfall Err-Logdatei        *}
{* 13.02.2012 WW  Signaturserver-Clientsocket: Empfangspuffer l�schen vor Ver-*}
{*                senden des Kommandos; mit Zeitmessung in Verify-Logdatei    *}
{* 06.03.2012 WW  Schl�ssel erzeugen und Signieren                            *}
{* 21.04.2015 WW  Log-Pfad im Konstruktor �bergeben                           *}
{* 30.04.2019 WW  Logging abschaltbar                                         *}
{* 17.03.2021 WW  Timeout f�r Warten auf Signaturserver-Antwort erh�ht auf 30s*}
{* 06.08.2021 WW  Timeout f�r Warten auf Signaturserver-Antwort erh�ht auf 90s*}
{*                                                                            *}
{* Copyright � RMG Messtechnik GmbH 2011, 2021                                *}
{******************************************************************************}
unit O_Signatur;

interface

uses
  Windows, SysUtils, DateUtils, WStrUtils, GD_Utils, WChars, LogFile, ErrPrc32,
  ErrConst, WPorts, WComm, O_TCPIP_CustomSrv, WSysCon;

const
  { Codes f�r Signaturverfahren }
  C_SigCode_RIPEMD160_ECDSAp192r1 = '0';  // RIPEMD160 + ECDSAp192r1
  C_SigCode_SHA256_ECDSAp192r1    = '1';  // SHA256 + ECDSAp192r1

  { Codes f�r Schl�sselerzeugungsverfahren }
  C_SigCode_ECDSAp192r1 = '0';  // ECDSAp192r1

type
  { Record mit Daten f�r Verifizierungs-Kommando }
  TVerifyCmdData = record
    SigCode: string;  // Code f�r Signaturverfahren
    Daten: string;    // Signierte Daten (hex)
    Sig_r: string;    // Signatur r (hex)
    Sig_s: string;    // Signatur s (hex)
    Qx: string;       // �ffentlicher Schl�ssel Qx (hex)
    Qy: string;       // �ffentlicher Schl�ssel Qy (hex)
  end;

  { Record mit Daten f�r Signierungs-Kommando }
  TSignCmdData = record
    SigCode: string;  // Code f�r Signaturverfahren
    Daten: string;    // Zu signierende Daten (hex)
    Qx: string;       // �ffentlicher Schl�ssel Qx (hex)
    Qy: string;       // �ffentlicher Schl�ssel Qy (hex)
  end;

  { Klasse f�r digitale Signatur }
  TSignaturClient = class (TObject)
  private
    FClientSocketSignaturSrv: TClientSocketCustomSrv;
    FHost: string;
    FIPAddress: string;
    FPort: integer;
    FLogPfad: string;
    FLogFilename: string;  // Logdatei Signatur (Leer: Es wird nicht geloggt)
    FPublicKey_x: string;
    FPublicKey_y: string;
    FLogInfo_Kennung: string;
    FLogInfo_AbrufLinie: string;  // COMn bzw. IPn
    FFehlergruppe: integer;
    FFehlercode: integer;
    procedure FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer);
    function GetLogInfo_Kennung: string;
    procedure SetLogInfo_Kennung (sKennung: string);

    function Connect_SignaturServer (var Fehlergruppe: integer;
      var Fehlercode: integer): boolean;
    function Disconnect_SignaturServer (var Fehlergruppe: integer;
      var Fehlercode: integer): boolean;

    function BuildSrvCommand_Verify (TId: integer; CmdData: TVerifyCmdData): string;
    function BuildSrvCommand_Version (TId: integer): string;
    function BuildSrvCommand_GenerateKey (TId: integer;  sSigCode: string): string;
    function BuildSrvCommand_Sign (TId: integer; CmdData: TSignCmdData): string;

    procedure ValidSrvAnswer (SrvAntwort: string; Kommando_Soll: string;
      TId_Soll: integer; var Fehlergruppe: integer; var Fehlercode: integer);
    procedure WriteLog_Verify (Fehlergruppe, Fehlercode: integer; sSigCode, sData,
      sSignature_r, sSignature_s, sLogInfo: string; TId: integer;
      sDSfGBusAdr, sDSfGDEL: string; dtStart: TDateTime);
    procedure WriteLog_GenerateKey (Fehlergruppe, Fehlercode: integer;
      sSigCode: string; TId: integer; dtStart: TDateTime;
      sPubKey_x, sPubKey_y: string);
    procedure WriteLog_Sign (Fehlergruppe, Fehlercode: integer;
      sSigCode, sData, sSignature_r, sSignature_s: string; TId: integer;
      dtStart: TDateTime);
    procedure WriteLog_Custom (sAktion: string; Fehlergruppe, Fehlercode: integer;
      sSigCode, sData, sSignature_r, sSignature_s, sLogInfo: string;
      TId: integer; sDSfGKennung, sDSfGBusAdr, sDSfGDEL: string; dtStart: TDateTime;
      sPubKey_x, sPubKey_y: string);
  public
    constructor Create (ALogPath, ALogInfo_AbrufLinie: string;
      AIPAddress: string; APort: integer; bLog: boolean = true);
    destructor Destroy; override;
    procedure GetFehlerGruppeCode (var AFehlergruppe: integer;
      var AFehlercode: integer);
    procedure SetPublicKey (sQx, sQy: string);
    function Verify (TId: integer; sSigCode, sData, sSignatur_r, sSignatur_s,
      sLogInfo, sDSfGBusAdr, sDSfGDEL: string; bHexData: boolean = false): integer;
    function GenerateKey (TId: integer; sSigCode: string; var sPubKey_x: string;
      var sPubKey_y: string): boolean;
    function Sign (TId: integer; sSigCode, sData: string;
      var sSignatur_r: string; var sSignatur_s: string;
      bHexData: boolean = false): boolean;
    function GetVersionCryptoDll (TId: integer; var sVersion: string): boolean;
    property LogInfo_Kennung: string read GetLogInfo_Kennung write SetLogInfo_Kennung;
  end;

procedure ReadPublicKeyFromFile (var sQx: string; var sQy: string;
  var sIPAdr_SigServer: string);

implementation

const
  C_Timeout_ServerVerbindungOeffnen = 30000;  { Timeout f�r �ffnen der Signaturserver-Verbindung }
  C_Timeout_ServerVerbindungBeenden = 30000;  { Timeout f�r Beenden der Signaturserver-Verbindung }
  C_Timeout_ServerReceive           = 90000;  { Timeout f�r Warten auf Signaturserver-Antwort
                                                -> Erh�ht von 10 auf 30s (f�r GAS-X 2.0); 17.03.2021, WW 
                                                -> Nochmal erh�ht auf 90s (f�r GAS-X 2.0, OGE); 06.08.2021, WW }

  C_CmdSeparator = ';';   { Trennzeichen in Signaturserver-Kommando/Antwort }


{----------------------------------------------------------------}
procedure ReadPublicKeyFromFile (var sQx: string; var sQy: string;
  var sIPAdr_SigServer: string);
{----------------------------------------------------------------}
{ Liest �ffentlichen Schl�ssel Qx, Qy und IP-Adresse zum Signaturserver aus Datei;
  R�ckgabe: Qx, Qy, IP-Adresse }
var
  Filename: string;
  S: string;

begin
  Filename:=ExtractFilePath (ParamStr(0)) + '\Signatur_PublicKey.txt';
  S:=StringFromFile (Filename);  // Public key aus File in String einlesen;
                                 // String enth�lt Qx, Qy durch CR LF getrennt
  sQx:=ExtractString (S, NUL, CR, 0);
  sQy:=ExtractString (S, LF, CR, 0);
  sIPAdr_SigServer:=ExtractString (S, LF, CR, 1);
end;


{ TSignaturClient }

{------------------------------------------------------------------------}
constructor TSignaturClient.Create (ALogPath, ALogInfo_AbrufLinie: string;
  AIPAddress: string; APort: integer; bLog: boolean = true);
{------------------------------------------------------------------------}
{ �bergaben: Pfad f�r Log-Dateien
             Abruflinie (f�r Protokollierung)
             IP-Adresse f�r Verbindung zum Signaturserver (leer = localhost)
             Port f�r Verbindung zum Signaturserver
             Flag, optional: Logging ja/nein }
begin
  inherited Create;

  FClientSocketSignaturSrv:=TClientSocketCustomSrv.Create (nil, '', COM_SIGNSRV_ERROR);

  FLogInfo_AbrufLinie:=ALogInfo_AbrufLinie;
  FIPAddress:=AIPAddress;
  if length (AIPAddress) = 0 then
    FHost:='localhost'
  else
    FHost:='';
  FPort:=APort;

  FLogPfad:=ALogPath;  // 21.04.2015, WW
  if bLog then
    FLogFilename:=ChangeFileExt (ExtractFileName (ParamStr(0)), '') + '_Signatur'
  else
    FLogFilename:='';  // Logging abgeschaltet; 30.04.2019, WW

  FPublicKey_x:='';
  FPublicKey_y:='';

  FLogInfo_Kennung:='';

  { Initialisieren der Fehlerstati }
  FFehlergruppe:=0;
  FFehlercode:=0;
end;

{---------------------------------}
destructor TSignaturClient.Destroy;
{---------------------------------}
  { Timeouts f�r Server-Socketverbindung (k�nnen per INI-File angepasst werden) }
var
  AFehlergruppe: integer;
  AFehlercode: integer;

begin
  Disconnect_SignaturServer (AFehlergruppe, AFehlercode);  { Socket-Verbindung zum Signaturserver schlie�en }

  FClientSocketSignaturSrv.Free;
  inherited Destroy;
end;

{--------------------------------------------------------------}
procedure TSignaturClient.FehlerGruppeCodeUpdate (AFehlergruppe,
  AFehlercode: integer);
{--------------------------------------------------------------}
{ setzt Fehlergruppe und Fehlercode;
  �bergabe: AFehlergruppe
            AFehlercode }
begin
  FFehlergruppe:=AFehlergruppe;
  FFehlercode:=AFehlercode;
end;

{------------------------------------------------------------------------}
procedure TSignaturClient.GetFehlerGruppeCode (var AFehlergruppe: integer;
  var AFehlercode: integer);
{------------------------------------------------------------------------}
{ global in der Klasse verwendete Fehlergruppe und Fehlercode zur�ckgeben;
  R�ckgaben: AFehlergruppe
             AFehlercode }
begin
  AFehlergruppe:=FFehlergruppe;
  AFehlercode:=FFehlercode;
end;

{--------------------------------------------------------}
procedure TSignaturClient.SetPublicKey (sQx, sQy: string);
{--------------------------------------------------------}
{ Setzt �ffentlichen Schl�ssel;
  �bergabe: Qx, Qy (hex) }
begin
  FPublicKey_x:=sQx;
  FPublicKey_y:=sQy;
end;

{--------------------------------------------------}
function TSignaturClient.GetLogInfo_Kennung: string;
{--------------------------------------------------}
{ Gibt Log-Info "Kennung" zur�ck;
  Ergebnis: Kennung }
begin
  Result:=FLogInfo_Kennung;
end;

{--------------------------------------------------------------}
procedure TSignaturClient.SetLogInfo_Kennung (sKennung: string);
{--------------------------------------------------------------}
{ Setzt LogInfo "Kennung";
  �bergabe: Kenung }
begin
  FLogInfo_Kennung:=sKennung;
end;

{-------------------------------------------------------------------------}
function TSignaturClient.Connect_SignaturServer (var Fehlergruppe: integer;
  var Fehlercode: integer): boolean;
{-------------------------------------------------------------------------}
{ Socket-Verbindung zum Signaturserver �ffnen;
  R�ckgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Verbindung ge�ffnet werden konnte }
begin
  // Vorbelegung R�ckgaben: OK
  Fehlergruppe:=0;
  Fehlercode:=0;

  Result:=FClientSocketSignaturSrv.Connect (FHost, FIPAddress, FPort,
                                            C_Timeout_ServerVerbindungOeffnen,
                                            Fehlergruppe, Fehlercode);
end;

{----------------------------------------------------------------------------}
function TSignaturClient.Disconnect_SignaturServer (var Fehlergruppe: integer;
  var Fehlercode: integer): boolean;
{----------------------------------------------------------------------------}
{ Verbindung zum Signaturserver schlie�en;
  R�ckgabe: Fehlergruppe
            Fehlercode
  Ergebnis: true, wenn Verbindung geschlossen werden konnte }
begin
  // Vorbelegung R�ckgaben: OK
  Fehlergruppe:=0;
  Fehlercode:=0;

  Result:=FClientSocketSignaturSrv.Disconnect (C_Timeout_ServerVerbindungBeenden,
                                               Fehlergruppe, Fehlercode);
end;

{--------------------------------------------------------------------------}
function TSignaturClient.Verify (TId: integer; sSigCode, sData, sSignatur_r,
  sSignatur_s, sLogInfo, sDSfGBusAdr, sDSfGDEL: string;
  bHexData: boolean = false): integer;
{--------------------------------------------------------------------------}
{ Signierte Daten verifizieren;
  �bergaben: Transaktions-Id
             Code f�r verwendetes Signaturverfahren
             zu verifizierender Datenstring
             Signaturen r, s
             Log-Information
             DSfG-Busadresse, von der Datenstring stammt
             DSfG-Datenelementadresse, von der Datenstring stammt
             optional:
               Flag 'bHexData': auf true setzen, wenn sData in Hex-Darstellung
                                �bergeben wird (Default: false = sData in ASCII)
  Ergebnis: Signatur-Verifizierungsstatus }
var
  Befehl: string;
  R: TRueckgabe;
  CmdData: TVerifyCmdData;
  i: integer;
  AFehlergruppe: integer;
  AFehlercode: integer;
  dtStart: TDateTime;

begin
  Result:=C_SigVerifyState_NotVerified;  // Vorbelegung Ergebnis: Signatur konnte nicht verifiziert werden
  FehlerGruppeCodeUpdate (0, 0);  // Vorbelegung Fehlergruppe/-code: OK

  { Signaturserver-Kommando f�r Verifizierung bilden: }
  with CmdData do begin
    SigCode:=sSigCode;
    if bHexData then
      Daten:=sData  // sData liegt bereits in Hex-Darstellung vor
    else begin
      { Rohdaten in Hex-Darstellung wandeln: }
      Daten:='';
      for i:=1 to length (sData) do
        Daten:=Daten + IntToHex (Ord (sData [i]), 2);
    end;
    Sig_r:=sSignatur_r;
    Sig_s:=sSignatur_s;
    Qx:=FPublicKey_x;
    Qy:=FPublicKey_y;
  end;

  { Start Zeitmessung: 13.02.2012, WW }
  dtStart:=Now;
  { Socket-Verbindung zum Signaturserver �ffnen, wenn noch nicht erfolgt: }
  if not FClientSocketSignaturSrv.Active then begin
    if not Connect_SignaturServer (AFehlergruppe, AFehlercode) then begin
      FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
      WriteLog_Verify (AFehlergruppe, AFehlercode, sSigCode, CmdData.Daten, sSignatur_r,
        sSignatur_s, sLogInfo, TId, sDSfGBusAdr, sDSfGDEL, dtStart);
      exit;
    end;
  end;

  Befehl:=BuildSrvCommand_Verify (TId, CmdData);

  { Verifizierungskommando an Signaturserver senden }
  if not FClientSocketSignaturSrv.SendCommand (Befehl, C_Timeout_ServerReceive,
                                               ad_String, R) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_Verify (R.Fehlergruppe, R.Fehlercode, sSigCode, CmdData.Daten, sSignatur_r,
      sSignatur_s, sLogInfo, TId, sDSfGBusAdr, sDSfGDEL, dtStart);
    exit;
  end;

  { Antwort auf Verifizierungskommando auswerten: }
  ValidSrvAnswer (R.Antwort, 'v', TId, R.Fehlergruppe, R.Fehlercode);
  if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_Verify (R.Fehlergruppe, R.Fehlercode, sSigCode, CmdData.Daten, sSignatur_r,
      sSignatur_s, sLogInfo, TId, sDSfGBusAdr, sDSfGDEL, dtStart);

    if (R.Fehlergruppe = SYS_SIGNSRV_ERROR) AND
       (R.Fehlercode = SIGNSRVERR_SIGNINVALID) then
      Result:=C_SigVerifyState_Invalid;  // Signatur ung�ltig
    exit;
  end;

  WriteLog_Verify (R.Fehlergruppe, R.Fehlercode, sSigCode, CmdData.Daten, sSignatur_r,
    sSignatur_s, sLogInfo, TId, sDSfGBusAdr, sDSfGDEL, dtStart);
  Result:=C_SigVerifyState_Valid;  // OK, Signatur g�ltig
end;

{---------------------------------------------------------}
function TSignaturClient.GetVersionCryptoDll (TId: integer;
  var sVersion: string): boolean;
{---------------------------------------------------------}
{ Version der vom Signaturserver verwendeten Crypto-DLL lesen;
  �bergabe: Transaktions-Id
  R�ckgabe: Version
  Ergebnis: true, wenn Lesen der Version ohne Fehler }
var
  Befehl: string;
  R: TRueckgabe;
  sAnswer: string;
  AFehlergruppe: integer;
  AFehlercode: integer;

begin
  Result:=false;
  sVersion:='';  // Vorbelegung R�ckgabe
  FehlerGruppeCodeUpdate (0, 0);  // Vorbelegung Fehlergruppe/-code: OK

  { Socket-Verbindung zum Signaturserver �ffnen, wenn noch nicht erfolgt: }
  if not FClientSocketSignaturSrv.Active then begin
    if not Connect_SignaturServer (AFehlergruppe, AFehlercode) then begin
      FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
      exit;
    end;
  end;

  { Signaturserver-Kommando f�r Versionlesen bilden: }
  Befehl:=BuildSrvCommand_Version (TId);

  { Versionslesekommando an Signaturserver senden }
  if not FClientSocketSignaturSrv.SendCommand (Befehl, C_Timeout_ServerReceive,
                                               ad_String, R) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Versionslesekommando auswerten: }
  ValidSrvAnswer (R.Antwort, 'z', TId, R.Fehlergruppe, R.Fehlercode);
  if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwortteil zwischen STX und ETX: }
  sAnswer:=ExtractString (R.Antwort, STX, ETX, 0);
  { Version extrahieren: }
  sVersion:=ExtractString (sAnswer, C_CmdSeparator, C_CmdSeparator, 1);   { Version }

  Result:=true;
end;

{-------------------------------------------------------------------}
function TSignaturClient.GenerateKey (TId: integer; sSigCode: string;
  var sPubKey_x: string; var sPubKey_y: string): boolean;
{-------------------------------------------------------------------}
{ Schl�ssel erzeugen;
  �bergaben: Transaktions-Id
             Code f�r verwendetes Signaturverfahren
               -> 0 = ECDSAp192r1
  R�ckgabe: �ffentlicher Schl�ssel Qx, Qy
  Ergebnis: true, wenn Schl�ssel erzeugen ohne Fehler }
var
  Befehl: string;
  R: TRueckgabe;
  sAnswer: string;
  AFehlergruppe: integer;
  AFehlercode: integer;
  dtStart: TDateTime;

begin
  Result:=false;
 { Vorbelegung R�ckgaben: }
  sPubKey_x:='';
  sPubKey_y:='';

  FehlerGruppeCodeUpdate (0, 0);  // Vorbelegung Fehlergruppe/-code: OK

  { Start Zeitmessung: }
  dtStart:=Now;
  { Socket-Verbindung zum Signaturserver �ffnen, wenn noch nicht erfolgt: }
  if not FClientSocketSignaturSrv.Active then begin
    if not Connect_SignaturServer (AFehlergruppe, AFehlercode) then begin
      FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
      WriteLog_GenerateKey (AFehlergruppe, AFehlercode, sSigCode, TId, dtStart,
        sPubKey_x, sPubKey_y);
      exit;
    end;
  end;

  { Signaturserver-Kommando f�r Schl�sselerzeugung bilden: }
  Befehl:=BuildSrvCommand_GenerateKey (TId, sSigCode);

  { Schl�sselerzeugungskommando an Signaturserver senden }
  if not FClientSocketSignaturSrv.SendCommand (Befehl, C_Timeout_ServerReceive,
                                               ad_String, R) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_GenerateKey (R.Fehlergruppe, R.Fehlercode, sSigCode, TId, dtStart,
      sPubKey_x, sPubKey_y);
    exit;
  end;

  { Antwort auf Schl�sselerzeugungskommando auswerten: }
  ValidSrvAnswer (R.Antwort, 'g', TId, R.Fehlergruppe, R.Fehlercode);
  if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_GenerateKey (R.Fehlergruppe, R.Fehlercode, sSigCode, TId, dtStart,
      sPubKey_x, sPubKey_y);
    exit;
  end;

  { Antwortteil zwischen STX und ETX: }
  sAnswer:=ExtractString (R.Antwort, STX, ETX, 0);
  { �ffentlichen Schl�ssel Qx, Qy extrahieren: }
  sPubKey_x:=ExtractString (sAnswer, C_CmdSeparator, C_CmdSeparator, 2);   { Qx }
  sPubKey_y:=ExtractString (sAnswer, C_CmdSeparator, C_CmdSeparator, 3);   { Qy }

  WriteLog_GenerateKey (R.Fehlergruppe, R.Fehlercode, sSigCode, TId, dtStart,
    sPubKey_x, sPubKey_y);
  Result:=true;
end;

{-------------------------------------------------------------------}
function TSignaturClient.Sign (TId: integer; sSigCode, sData: string;
  var sSignatur_r: string; var sSignatur_s: string;
  bHexData: boolean = false): boolean;
{-------------------------------------------------------------------}
{ Daten signieren;
  �bergaben: Transaktions-Id
             Code f�r zu verwendendes Signaturverfahren
             zu signierender Datenstring
             optional:
               Flag 'bHexData': auf true setzen, wenn sData in Hex-Darstellung
                                �bergeben wird (Default: false = sData in ASCII)
  Ergebnis: true, wenn Signierungsvorgang ohne Fehler }
var
  Befehl: string;
  R: TRueckgabe;
  CmdData: TSignCmdData;
  i: integer;
  AFehlergruppe: integer;
  AFehlercode: integer;
  dtStart: TDateTime;
  sAnswer: string;

begin
  Result:=false;
 { Vorbelegung R�ckgaben: }
  sSignatur_r:='';
  sSignatur_s:='';

  FehlerGruppeCodeUpdate (0, 0);  // Vorbelegung Fehlergruppe/-code: OK

  { Signaturserver-Kommando f�r Signierung bilden: }
  with CmdData do begin
    SigCode:=sSigCode;
    if bHexData then
      Daten:=sData  // sData liegt bereits in Hex-Darstellung vor
    else begin
      { Rohdaten in Hex-Darstellung wandeln: }
      Daten:='';
      for i:=1 to length (sData) do
        Daten:=Daten + IntToHex (Ord (sData [i]), 2);
    end;
    Qx:=FPublicKey_x;
    Qy:=FPublicKey_y;
  end;

  { Start Zeitmessung: }
  dtStart:=Now;
  { Socket-Verbindung zum Signaturserver �ffnen, wenn noch nicht erfolgt: }
  if not FClientSocketSignaturSrv.Active then begin
    if not Connect_SignaturServer (AFehlergruppe, AFehlercode) then begin
      FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
      WriteLog_Sign (AFehlergruppe, AFehlercode, sSigCode, CmdData.Daten,
        sSignatur_r, sSignatur_s, TId, dtStart);
      exit;
    end;
  end;

  Befehl:=BuildSrvCommand_Sign (TId, CmdData);

  { Signierungskommando an Signaturserver senden }
  if not FClientSocketSignaturSrv.SendCommand (Befehl, C_Timeout_ServerReceive,
                                               ad_String, R) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_Sign (R.Fehlergruppe, R.Fehlercode, sSigCode, CmdData.Daten,
      sSignatur_r, sSignatur_s, TId, dtStart);
    exit;
  end;

  { Antwort auf Signierungskommando auswerten: }
  ValidSrvAnswer (R.Antwort, 's', TId, R.Fehlergruppe, R.Fehlercode);
  if not FehlerGruppeCode_OK (R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    WriteLog_Sign (R.Fehlergruppe, R.Fehlercode, sSigCode, CmdData.Daten,
      sSignatur_r, sSignatur_s, TId, dtStart);
    exit;
  end;

  { Antwortteil zwischen STX und ETX: }
  sAnswer:=ExtractString (R.Antwort, STX, ETX, 0);
  { Signaturen r, s extrahieren: }
  sSignatur_r:=ExtractString (sAnswer, C_CmdSeparator, C_CmdSeparator, 2);   { Signatur r }
  sSignatur_s:=ExtractString (sAnswer, C_CmdSeparator, C_CmdSeparator, 3);   { Signatur s }

  WriteLog_Sign (R.Fehlergruppe, R.Fehlercode, sSigCode, CmdData.Daten,
    sSignatur_r, sSignatur_s, TId, dtStart);
  Result:=true;
end;


{------------------------ Signaturserver-Befehle bilden -----------------------}

{------------------------------------------------------------}
function TSignaturClient.BuildSrvCommand_Verify (TId: integer;
  CmdData: TVerifyCmdData): string;
{------------------------------------------------------------}
{ Signaturserver-Kommandostring f�r Verifizierung zusammensetzen;
  �bergabe: Transaktions-ID
            Verifizierungs-Kommandorecord
  Ergebnis: Kommando-String }
var
  S: string;

begin
  with CmdData do begin
    S:='v' + C_CmdSeparator +             // Kommandozeichen
       IntToStr (TId) + C_CmdSeparator +  // Transaktions-ID
       SigCode + C_CmdSeparator +         // Code f�r Signaturverfahren
       Daten + C_CmdSeparator +           // Signierte Daten
       Sig_r + C_CmdSeparator +           // Signatur r
       Sig_s + C_CmdSeparator +           // Signatur s
       Qx + C_CmdSeparator +              // �ffentlicher Schl�ssel Qx
       Qy + C_CmdSeparator;               // �ffentlicher Schl�ssel Qy
  end;
  Result:=STX + S + ETX;
end;

{----------------------------------------------------------------------}
function TSignaturClient.BuildSrvCommand_Version (TId: integer): string;
{----------------------------------------------------------------------}
{ Signaturserver-Kommandostring f�r Lesen der Version der Crypto-Dll zusammensetzen;
  �bergabe: Transaktions-ID
  Ergebnis: Kommando-String }
var
  S: string;

begin
  S:='z' + C_CmdSeparator +            // Kommandozeichen
     IntToStr (TId) + C_CmdSeparator;  // Transaktions-ID
  Result:=STX + S + ETX;
end;

{-----------------------------------------------------------------}
function TSignaturClient.BuildSrvCommand_GenerateKey (TId: integer;
  sSigCode: string): string;
{-----------------------------------------------------------------}
{ Signaturserver-Kommandostring f�r Schl�sselerzeugung zusammensetzen;
  �bergaben: Transaktions-Id
             Code f�r verwendetes Signaturverfahren
  Ergebnis: Kommando-String }
var
  S: string;

begin
  S:='g' + C_CmdSeparator +             // Kommandozeichen
     IntToStr (TId) + C_CmdSeparator +  // Transaktions-ID
     sSigCode + C_CmdSeparator;         // Code f�r Signaturverfahren
  Result:=STX + S + ETX;
end;

{------------------------------------------------------------}
function TSignaturClient.BuildSrvCommand_Sign (TId: integer;
  CmdData: TSignCmdData): string;
{------------------------------------------------------------}
{ Signaturserver-Kommandostring f�r Signierung zusammensetzen;
  �bergabe: Transaktions-ID
            Signierungs-Kommandorecord
  Ergebnis: Kommando-String }
var
  S: string;

begin
  with CmdData do begin
    S:='s' + C_CmdSeparator +             // Kommandozeichen
       IntToStr (TId) + C_CmdSeparator +  // Transaktions-ID
       SigCode + C_CmdSeparator +         // Code f�r Signaturverfahren
       Daten + C_CmdSeparator +           // Signierte Daten
       Qx + C_CmdSeparator +              // �ffentlicher Schl�ssel Qx
       Qy + C_CmdSeparator;               // �ffentlicher Schl�ssel Qy
  end;
  Result:=STX + S + ETX;
end;


{------------------------ Signaturserver-Antwort pr�fen -----------------------}

{----------------------------------------------------------------------------------}
procedure TSignaturClient.ValidSrvAnswer (SrvAntwort: string; Kommando_Soll: string;
  TId_Soll: integer; var Fehlergruppe: integer; var Fehlercode: integer);
{----------------------------------------------------------------------------------}
{ Pr�ft Signaturserver-Antwort auf Plausibilit�t und liefert enthaltenen
  Fehlerstatus zur�ck;
  �bergabe: Serverantwort als String
            in Antwort erwartetes Kommandozeichen
            in Antwort erwartete Transaktions-ID
  R�ckgabe: Fehlergruppe
            Fehlercode }
var
  S: string;
  sAnswer: string;
  TId: integer;
  Kommando_Answer: string;
  AnzSeparator_Soll: integer;

begin
  { Vorbelegung: Signaturserver-Antwort nicht plausibel }
  Fehlergruppe:=SYS_SIGNSRVSYSERROR;
  Fehlercode:=SIGNSRVSYSERR_ANTWORTNICHTPLAUSIBEL;

  { Antwortteil zwischen STX und ETX: }
  sAnswer:=ExtractString (SrvAntwort, STX, ETX, 0);

  { Kommandozeichen in Antwort mit Soll vergleichen: }
  Kommando_Answer:=ExtractString (sAnswer, NUL, C_CmdSeparator, 0);   { Kommandozeichen }
  if Kommando_Answer <> Kommando_Soll then begin
    Fehlergruppe:=SYS_SIGNSRVSYSERROR;
    Fehlercode:=SIGNSRVSYSERR_ANTWORTUNERWARTET_CMD;
    exit;
  end;

  { Antwort auf Plausibilit�t pr�fen (Anzahl der Trennzeichen): }
  if Kommando_Answer = 's' then
    AnzSeparator_Soll:=5
  else if Kommando_Answer = 'g' then
    AnzSeparator_Soll:=5
  else
    AnzSeparator_Soll:=3;
  if F_TotalChars (sAnswer, C_CmdSeparator) <> AnzSeparator_Soll then exit;

  { Transaktions-Id in Antwort mit Soll vergleichen: }
  S:=ExtractString (sAnswer, C_CmdSeparator, C_CmdSeparator, 0);   { Transaktions-Id }
  try
    TId:=StrToInt (S);
  except
    Fehlergruppe:=SYS_SIGNSRVSYSERROR;
    Fehlercode:=SIGNSRVSYSERR_ANTWORTNICHTPLAUSIBEL;
    exit;
  end;
  if TId <> TId_Soll then begin
    Fehlergruppe:=SYS_SIGNSRVSYSERROR;
    Fehlercode:=SIGNSRVSYSERR_ANTWORTUNERWARTET_TID;
    exit;
  end;

  if (Kommando_Answer = 's') OR
     (Kommando_Answer = 'g') OR
     (Kommando_Answer = 'v') then begin
    { Status extrahieren: }
    S:=ExtractString (sAnswer, C_CmdSeparator, C_CmdSeparator, 1);   { Status }
    try
      { Vom Signaturserver gelieferter Status wird in Fehlercode �bernommen: }
      Fehlercode:=StrToInt (S);
      if Fehlercode = 0 then  // OK
        Fehlergruppe:=0
      else
        Fehlergruppe:=SYS_SIGNSRV_ERROR;
    except
      Fehlergruppe:=SYS_SIGNSRVSYSERROR;
      Fehlercode:=SIGNSRVSYSERR_ANTWORTNICHTPLAUSIBEL;
      exit;
    end;
  end
  else begin
    // OK
    Fehlergruppe:=0;
    Fehlercode:=0;
  end;
end;


{------------------------------- Logging --------------------------------------}

{----------------------------------------------------------------------------}
procedure TSignaturClient.WriteLog_Verify (Fehlergruppe, Fehlercode: integer;
  sSigCode, sData, sSignature_r, sSignature_s, sLogInfo: string; TId: integer;
  sDSfGBusAdr, sDSfGDEL: string; dtStart: TDateTime);
{----------------------------------------------------------------------------}
{ Signatur-Logdatei schreiben f�r Verifizieren }
begin
  WriteLog_Custom ('Verifizieren', Fehlergruppe, Fehlercode, sSigCode, sData,
    sSignature_r, sSignature_s, sLogInfo, TId,
    FLogInfo_Kennung, sDSfGBusAdr, sDSfGDEL, dtStart,
    FPublicKey_x, FPublicKey_y);
end;

{--------------------------------------------------------------------------------}
procedure TSignaturClient.WriteLog_GenerateKey (Fehlergruppe, Fehlercode: integer;
  sSigCode: string; TId: integer; dtStart: TDateTime;
  sPubKey_x, sPubKey_y: string);
{--------------------------------------------------------------------------------}
{ Signatur-Logdatei schreiben f�r Schl�ssel erzeugen }
begin
  WriteLog_Custom ('Schl�ssel erzeugen', Fehlergruppe, Fehlercode, sSigCode, '',
    '', '', '', TId, '', '', '', dtStart, sPubKey_x, sPubKey_y);
end;

{----------------------------------------------------------------------------}
procedure TSignaturClient.WriteLog_Sign (Fehlergruppe, Fehlercode: integer;
  sSigCode, sData, sSignature_r, sSignature_s: string; TId: integer;
  dtStart: TDateTime);
{----------------------------------------------------------------------------}
{ Signatur-Logdatei schreiben f�r Signieren }
begin
  WriteLog_Custom ('Signieren', Fehlergruppe, Fehlercode, sSigCode, sData,
    sSignature_r, sSignature_s, '', TId, '', '', '', dtStart,
    FPublicKey_x, FPublicKey_y);
end;

{------------------------------------------------------------------------------}
procedure TSignaturClient.WriteLog_Custom (sAktion: string;
  Fehlergruppe, Fehlercode: integer;
  sSigCode, sData, sSignature_r, sSignature_s, sLogInfo: string;
  TId: integer; sDSfGKennung, sDSfGBusAdr, sDSfGDEL: string; dtStart: TDateTime;
  sPubKey_x, sPubKey_y: string);
{------------------------------------------------------------------------------}
{ Signatur-Logdatei schreiben (Basis-Funktion) }
var
  S: string;
  dt: TDateTime;
  sErgebnis: string;
  sFehler_spez: string;
  sVerbSigServer: string;
  Dauer_ms: Int64;

begin
  if FLogFilename = '' then exit;  // Pr�fen, ob Logging abgeschaltet ist; 30.04.2019, WW

  { Ende Zeitmessung: 13.02.2012, WW }
  Dauer_ms:=MilliSecondsBetween (dtStart, Now);  // ms zwischen Start und Ende der Zeitmessung

  if not FileExists (FLogPfad + FLogFilename + '.log') then begin
    { 1. Eintrag im Logfile enth�lt �berschriften: }
    S:='Datum' + #9 + 'Zeit' + #9 + 'Aktion' + #9 + 'Ergebnis' + #9 + 'Kenner' + #9 +
       'Daten (hex)' + #9 + 'Signatur r (hex)' + #9 + 'Signatur s (hex)' + #9 +
       'Qx (hex)' + #9 + 'Qy (hex)' + #9 + 'Rohdatendatei' + #9 +
       'Fehler spezifisch' + #9 + 'Verbindung Sig.server' + #9 +  // 15.01.2012, WW
       'TransaktionsId' + #9 + 'DSfG-Kennung' + #9 + 'DSfG-Busadr.' + #9 +
       'DSfG-DEL' + #9 + 'Abruflinie' + #9 + 'Dauer (ms)';

    WriteDebugLog (FLogPfad, FLogFilename, S, false);
  end;

  { OK/Fehler: }
  if not FehlerGruppeCode_OK (Fehlergruppe, Fehlercode) then begin
    sErgebnis:='Fehler';
    sFehler_spez:=GetStatusText (Fehlergruppe) + ': ' +
                  GetErrorText (Fehlergruppe, Fehlercode);
  end
  else begin
    sErgebnis:='OK';
    sFehler_spez:='';
  end;

  { Verbindungsdaten Signaturserver: }
  if length (FHost) > 0 then
    sVerbSigServer:=FHost + ':' + IntToStr (FPort)
  else
    sVerbSigServer:=FIPAddress + ':' + IntToStr (FPort);

  dt:=Now;
  S:=FormatDateTime ('dd.mm.yyyy', dt) + #9 +
     FormatDateTime ('hh:nn:ss', dt) + #9 +
     sAktion + #9 +  // 06.03.2012, WW
     sErgebnis + #9 + sSigCode + #9 + sData + #9 +
     sSignature_r + #9 + sSignature_s + #9 +
     sPubKey_x + #9 + sPubKey_y + #9 + sLogInfo + #9 +
     sFehler_spez + #9 + sVerbSigServer + #9 + IntToStr (TId) + #9 +  // 15.01.2012, WW
     sDSfGKennung + #9 + sDSfGBusAdr + #9 + sDSfGDEL + #9 +
     FLogInfo_AbrufLinie + #9 + IntToStr (Dauer_ms);

  WriteDebugLog (FLogPfad, FLogFilename, S, false);
end;

end.
