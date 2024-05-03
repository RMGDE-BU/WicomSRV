{******************************************************************************}
{* Unit: FUP/Modem initialisieren f�r MRG-Abruf                               *}
{* 30.12.2002  WW                                                             *}
{* 19.11.2013  WW  Standardm��ige Wartezeit nach PIN-Login (f�r Sierra Wire-  *}
{*                 less Modem                                                 *}
{******************************************************************************}
unit FupModemInit;

interface

uses
  Windows, SysUtils, Serial, O_Comm, SerMrgModem, SerMrgFup, SerDSfG, SrvCfgIni,
  ModemIni, WStrUtils, WChars, ErrConst, WComm, GSMModemFkt, LogFile, VerifyFlags;

type
  { Modem-Initialisierungsarten }
  TModemInitMode = (mim_DSfG,       // f�r Abruf DSfG
                    mim_MRG_Allg,   // f�r Abruf MRG (au�er MRG800PTB)
                    mim_MRG800PTB   // f�r Abruf MRG800PTB
                   );

function Fup_Reset (MRGFupCommObj: TMRGFupCommObj; SrvCfgPfad: TFileName;
  Timeout_FupReset: integer; Timeout_FupAntwort: integer; doReconnectCOM: boolean;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;

function Fup_Init (MRGFupCommObj: TMRGFupCommObj; SrvCfgPfad: TFileName;
  Timeout_FupAntwort: integer;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;

function Modem_Initialisieren (CommObj: TCommObj; ResetStr, InitStr: string;
  isGSM_Modem: boolean; PIN: string; PIN_Lock: boolean;
  Timeout_ModemInit, Timeout_GSMModem: integer; COMNr: integer;
  GSM_WaitAfterLogin: boolean; LogFile: TCustomLogFile;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;

function Modem_Initialisieren_Wico (CommObj: TCommObj; MRG800PTB: boolean;
  ModemName: string; NetProgPfad: TFileName;
  Timeout_ModemInit, Timeout_GSMModem: integer; COMNr: integer;
  GSM_WaitAfterLogin: boolean; LogFile: TCustomLogFile;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;

implementation

{--------------------------------------------------------------------------------}
function Fup_Reset (MRGFupCommObj: TMRGFupCommObj; SrvCfgPfad: TFileName;
  Timeout_FupReset: integer; Timeout_FupAntwort: integer; doReconnectCOM: boolean;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{--------------------------------------------------------------------------------}
{ FUP zur�cksetzen und initialisieren;
  �bergaben: Zeiger auf Kommunikationsobjekt TMRGFupCommObj
             Pfad der SRVCFGINI-32-Konfiguration
             Timeout f�r FUP-Reset
             Timeout f�r FUP-Antwort
             Flag doReconnectCOM (wenn true, wird vor dem FUP-Reset die COM
                                  geschlossen und wieder ge�ffnet)
  R�ckgaben: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn FUP-Reset OK }
var
  R: TRueckgabe;
  dummy: boolean;
  Version: string;

begin
  Result:=false;

  { Schnittstelle schlie�en und neu �ffnen (wg. Problem mit FUP-Betrieb an
    ComServer-Schnittstellen): 31.10.2006, WW }
  if doReconnectCOM then begin
    if not MRGFupCommObj.Reconnect then begin
      Fehlergruppe:=COM_PORTERROR;
      Fehlercode:=COMPORTERR_OEFFNEN;
      exit;
    end;
  end;

  { FUP zur�cksetzen (Break, DTR-Pegel f�r verschiedene (?) FUP-Versionen): }
  MRGFupCommObj.Serial.SetBreakZustand;                  // WW  25.09.2001
  Sleep (500);
  MRGFupCommObj.Serial.ClearBreakZustand;
  Sleep (500);

  MRGFupCommObj.Serial.ClearDTRSignal;
  Sleep (500);
  MRGFupCommObj.Serial.SetDTRSignal;
  Sleep (500);

  MRGFupCommObj.Serial.ClearRTSSignal;                   // H.-P.R. 17.09.02 wg GVT
  Sleep (500);
  MRGFupCommObj.Serial.SetRTSSignal;

  Sleep (1000);    { kurze Wartezeit, sonst wird der erste Befehl vom FUP evtl.
                     nicht erkannt (WW  25.09.2001) }
  { Wahlinformation abfragen als Funktionstest: }
  if not MRGFupCommObj.SendCommand (ESC+'w'+CR, [CR], 1, Timeout_FupReset, ad_String, R, dummy) then begin
    Fehlergruppe:=COM_DEVICEINITERROR;
    Fehlercode:=DEVINITERR_FUPRESET;
    exit;
  end;
  if ExtractString (R.Antwort, ESC, CR, 0) <> 'W' then begin      { FUP-Antwort nicht ok }
    Fehlergruppe:=COM_DEVICEINITERROR;
    Fehlercode:=DEVINITERR_FUPRESET;
    exit;
  end;

  { FUP-Version abfragen: }
  if not MRGFupCommObj.SendCommand (ESC+'V'+CR, [CR], 1, Timeout_FupAntwort, ad_String, R, dummy) then begin
    { falls keine Antwort kommt, h�ngt ein alter Fup-1200 dran, der kennt keinen V-Befehl ! }
    if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) then
      Version:='FUP-1200'
    else begin
      Fehlergruppe:=COM_DEVICEINITERROR;
      Fehlercode:=DEVINITERR_FUPRESET;
      exit;
    end;
  end else
    Version:=ExtractString (R.Antwort, ESC, CR, 0);
  MRGFupCommObj.isFup1200:=Pos ('1200', Version) <> 0;

  { FUP initialisieren: }
  Result:=Fup_Init (MRGFupCommObj, SrvCfgPfad, Timeout_FupAntwort, Fehlergruppe, Fehlercode);
end;

{----------------------------------------------------------------------}
function Fup_Init (MRGFupCommObj: TMRGFupCommObj; SrvCfgPfad: TFileName;
  Timeout_FupAntwort: integer;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{----------------------------------------------------------------------}
{ FUP initialisieren;
  �bergaben: Zeiger auf Kommunikationsobjekt TMRGFupCommObj
             Pfad der SRVCFG32-INI-Konfiguration
             Timeout f�r FUP-Antwort
  R�ckgaben: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn FUP-Initialisierung OK }
var
  SI: TSrvCfg32Ini;
  S, FupInitStr, InitStr: string;
  Befehl: string;
  BefehlChar: char;
  R: TRueckgabe;
  dummy: boolean;

begin
  Result:=false;

  { FUP-Initialisierungsstring lesen: }
  SI:=TSrvCfg32Ini.Create (SrvCfgPfad);
  try
    FupInitStr:=SI.FupInit [MRGFupCommObj.Serial.COMPort];
  finally
    SI.Free;
  end;

  { FUP initialisieren, InitStr enth�lt durch Trennzeichen abgegrenzte Teil-Initialisierungen: }
  InitStr:=FupInitStr;
  while length (InitStr) > 0 do begin
    S:=F_Zerlegen (InitStr, CFupModemInit_Trenner);
    if length (S) > 0 then begin
      BefehlChar:=S[1];
      if IsCharUpper (BefehlChar) then begin
        if MRGFupCommObj.isFup1200 AND (BefehlChar in ['S', 'M']) then
          Continue;              { S- und M-Sequenz gibts im FUP-1200 nicht }
        Befehl:=ESC+S+CR+ESC+LowerCase (BefehlChar)+CR;
        if not MRGFupCommObj.SendCommand (Befehl, [CR], 1, Timeout_FupAntwort, ad_String, R, dummy) then begin
          Fehlergruppe:=COM_DEVICEINITERROR;
          Fehlercode:=DEVINITERR_FUPINIT;
          exit;
        end;
        if ExtractString (R.Antwort, ESC, CR, 0) <> S then begin     { FUP-Antwort nicht ok }
          Fehlergruppe:=COM_DEVICEINITERROR;
          Fehlercode:=DEVINITERR_FUPINIT;
          exit;
        end;
      end;
    end;
  end;  { while length (InitStr) }
  Result:=true;
end;

{---------------------------------------}
procedure ResetRTS_DTR (Serial: TSerial);
{---------------------------------------}
{ R�cksetzen und Setzen des RTS/DTR-Signals }
begin
  Serial.ClearRTSSignal;
  Serial.ClearDTRSignal;
  Sleep (1000);
  Serial.SetDTRSignal;
  Serial.SetRTSSignal;
end;

{--------------------------------------------------------------------------}
function Modem_Initialisieren (CommObj: TCommObj; ResetStr, InitStr: string;
  isGSM_Modem: boolean; PIN: string; PIN_Lock: boolean;
  Timeout_ModemInit, Timeout_GSMModem: integer; COMNr: integer;
  GSM_WaitAfterLogin: boolean; LogFile: TCustomLogFile;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{--------------------------------------------------------------------------}
{ Modem initialisieren;
  �bergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (f�r MRG-Modemabruf)
               oder TDSfGModemCommObj (f�r DSfG-Abruf)
             ResetStr (f�r Werkseinstellung laden)
             InitStr (f�r spezielle Modemeinstellungen)
             Flag 'isGSM_Modem': true �bergeben, wenn GSM-Modem initialisiert werden soll
             PIN (f�r GSM-Modem)
             Flag 'PIN_Lock' (f�r GSM-Modem): true �bergeben, wenn Initialisierung sicherheitshalber
               abgebrochen werden soll, falls PIN-Eingabe erforderlich ist
             Timeout f�r Modem-Initialisierung
             Timeout f�r GSM-Modembefehle
             Flag 'GSM_WaitAfterLogin': wenn true �bergeben wird, erfolgt nach dem
                                        Login eine Wartezeit von ca. 20s (f�r
                                        anschlie�ende Zugriffe auf GSM-Modemspeicher
                                        (z.B. SMS-Liste, Telefonbuch etc.)
             Zeiger auf Logfile (wenn NIL �bergeben wird, erfolgen keine Logfile-Eintr�ge)
  R�ckgaben: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Modem-Initialisierung OK }
Const
  CModemAntwortOK  = 'OK';
  CMaxResetInitVersuche = 3;    { max. Anzahl von Reset/Init-Versuchen }
  CMaxInitVersuche_GSM  = 20;   { max. Anzahl von Init-Versuchen bei GSM-Modem
                                  -> ergibt Init-Versuche f�r maximal ca. 30 s
                                     (mit 500 ms Warten auf Modemantwort + Sleep-
                                     Wartezeit 1000 ms). Das sollte ausreichend sein. }
var
  Initialisiert: boolean;
  R: TRueckgabe;
  ResetInitVersuche: byte;
  OK: boolean;
  InitVersuche_GSM: byte;
  PIN_Status: integer;
  PIN_OK: boolean;
  ASerial: TSerial;

begin
  Result:=false;

  { vor der Modem-Initialisierung m�ssen die Schnittstellen-Parameter auf die
    Standardwerte 9600,7,e,1 gesetzt sein, um:
    1. das Modem �berhaupt ansprechen zu k�nnen (andere Datenformate k�nnen vom
       Modem evtl. nicht automatisch erkannt werden)
    2. einen ankommenden Ruf (RING) im definierten Datenformat erkennen zu k�nnen
    3. bei der Rufentgegennahme die MRG-Daten lesen zu k�nnen (es k�nnen nur Rufe
       von MRGs, die ihre Daten mit 7,e,1 senden ("FUP-Ger�te"), entgegengenommen werden !)
    4. Modem-Initialisierung mit 7,e,1 funktioniert auch, wenn eine Modem-Antwort
       mit 8,n,1 kommt (wie z.B. beim GSM-Modem) }
  if CommObj is TMRGModemCommObj then
    TMRGModemCommObj (CommObj).SetDCDCheck (false)
  else if CommObj is TDSfGModemCommObj then
    TDSfGModemCommObj (CommObj).SetDCDCheck (false);

  OK:=false;
  ASerial:=nil;
  Initialisiert:=false;
  ResetInitVersuche:=1;
  while not Initialisiert AND (ResetInitVersuche <= CMaxResetInitVersuche) do begin
    inc (ResetInitVersuche);
    { "Reset": }
    if length (ResetStr) > 0 then begin
      if CommObj is TMRGModemCommObj then begin
        OK:=TMRGModemCommObj (CommObj).SendModemCommand (ResetStr+CR, Timeout_ModemInit, R);
        ASerial:=TMRGModemCommObj (CommObj).Serial;
      end
      else if CommObj is TDSfGModemCommObj then begin
        OK:=TDSfGModemCommObj (CommObj).SendModemCommand (ResetStr+CR, Timeout_ModemInit, R);
        ASerial:=TDSfGModemCommObj (CommObj).Serial;
      end else
        Break;

      if not OK then begin
        ResetRTS_DTR (ASerial);
        Continue;
      end;
      if Pos (CModemAntwortOK, R.Antwort) = 0 then begin
        ResetRTS_DTR (ASerial);
        Continue;
      end;
    end;

    InitVersuche_GSM:=1;
    while not Initialisiert AND (InitVersuche_GSM <= CMaxInitVersuche_GSM) do begin
      inc (InitVersuche_GSM);
      { "Init": }
      if length (InitStr) > 0 then begin
        if CommObj is TMRGModemCommObj then begin
          OK:=TMRGModemCommObj (CommObj).SendModemCommand (InitStr+CR, Timeout_ModemInit, R);
          ASerial:=TMRGModemCommObj (CommObj).Serial;
        end
        else if CommObj is TDSfGModemCommObj then begin
          OK:=TDSfGModemCommObj (CommObj).SendModemCommand (InitStr+CR, Timeout_ModemInit, R);
          ASerial:=TDSfGModemCommObj (CommObj).Serial;
        end else
          Break;

        { bei einem GSM-Modem mehrmals 'Init' probieren ohne erneutes Reset !
          Grund: Nach OK-Antwort auf Reset-Befehl ist das GSM-Modem evtl. noch
                 nicht bereit den anschlie�enden Init-Befehl zu verarbeiten und
                 liefert dann eine unvollst�ndige Antwort. }
        if not OK then begin
          if isGSM_Modem then begin  { GSM-Modem }
            Sleep (1000);
            Continue;  { weiter mit erneutem Init-Versuch }
          end
          else begin   { Analog-Modem }
            ResetRTS_DTR (ASerial);
            Break;  { weiter mit erneutem Reset-Versuch }
          end;
        end;
        if Pos (CModemAntwortOK, R.Antwort) = 0 then begin
          if isGSM_Modem then begin  { GSM-Modem }
            Sleep (1000);
            Continue;  { weiter mit erneutem Init-Versuch }
          end
          else begin   { Analog-Modem }
            ResetRTS_DTR (ASerial);
            Break;  { weiter mit erneutem Reset-Versuch }
          end;
        end;
      end;

      Initialisiert:=true;
    end;  { while Init }
  end;  { while Reset }

  if Initialisiert then begin
    { bei GSM-Modem zus�tzlich: PIN abfragen. Wenn nicht eingeloggt, PIN eingeben }
    if isGSM_Modem then begin  { GSM-Modem }
      OK:=GSM_PIN_Check (CommObj, Timeout_GSMModem, PIN_Status,
                         Fehlergruppe, Fehlercode);  { PIN abfragen }
      if OK then begin
        case PIN_Status of
          0: begin  { GSM-Modem bereit, PIN-Eingabe nicht notwendig }
               if LogFile <> nil then
                 LogFile.Write ('GSM-Modem bereit, PIN-Eingabe nicht notwendig');  { Logfile-Protokollierung }
             end;
          1: begin  { PIN mu� eingeben werden }
               if PIN_Lock then begin
                 // PIN-Login ist in Schnittstellen-Konfiguration gesperrt
                 Fehlergruppe:=COM_DEVICEINITERROR;
                 Fehlercode:=DEVINITERR_PIN_LOCK;
                 OK:=false;
               end
               else begin
                 OK:=GSM_PIN_Login (CommObj, Timeout_GSMModem, PIN, PIN_OK,
                                    Fehlergruppe, Fehlercode);   { PIN eingeben }
                 if OK then begin
                   if PIN_OK then begin
                     if LogFile <> nil then
                       LogFile.Write ('GSM-Modem nicht bereit, PIN-Eingabe: ' + PIN);  { Logfile-Protokollierung }
                     if GSM_WaitAfterLogin then
                       Sleep (20000)  // L�ngere Wartezeit nach PIN-Eingabe (Zugriffe auf GSM-Modemspeicher
                                      // sind erst nach ca. 10..20 s m�glich)
                     else
                       Sleep (3000);  // Standard: Kurze Wartezeit 3s
                                      // -> N�tig f�r Sierra Wireless: bricht sonst
                                      //    W�hlversuch gleich mit NoCarrier ab; 19.11.2013 WW
                   end
                   else begin
                     { Login in GSM-Modem nicht m�glich: PIN ist falsch. }
                     Fehlergruppe:=COM_MODEMERROR;
                     Fehlercode:=CME_GSM_PIN_WRONG;
                     OK:=false;
                   end;
                 end;
               end;
             end;
          2: begin  { PUK mu� eingegeben werden }
               if PIN_Lock then begin
                 // PIN-Login ist in Schnittstellen-Konfiguration gesperrt
                 Fehlergruppe:=COM_DEVICEINITERROR;
                 Fehlercode:=DEVINITERR_PIN_LOCK;
                 OK:=false;
               end
               else begin
                 { Login in GSM-Modem �ber PIN nicht m�glich: PUK-Eingabe ist erforderlich. }
                 Fehlergruppe:=COM_MODEMERROR;
                 Fehlercode:=CME_GSM_PIN_LOGINIMPOSSIBLE_PUK;
                 OK:=false;
               end;
             end;
        else  { sonstige (unbekannte) PIN-Stati }
          { Login in GSM-Modem �ber PIN nicht m�glich }
          Fehlergruppe:=COM_MODEMERROR;
          Fehlercode:=CME_GSM_PIN_LOGINIMPOSSIBLE_UNKNOWN;
          OK:=false;
        end;
      end;

      if OK then
        Result:=true;
    end else
      Result:=true;
  end
  else begin
    Fehlergruppe:=COM_DEVICEINITERROR;
    Fehlercode:=DEVINITERR_MODEMINIT;
  end;
end;

{------------------------------------------------------------------------}
function Modem_Initialisieren_Wico (CommObj: TCommObj; MRG800PTB: boolean;
  ModemName: string; NetProgPfad: TFileName;
  Timeout_ModemInit, Timeout_GSMModem: integer; COMNr: integer;
  GSM_WaitAfterLogin: boolean; LogFile: TCustomLogFile;
  var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{------------------------------------------------------------------------}
{ Modem initialisieren innerhalb Wico-Umgebung;
  �bergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (f�r MRG-Modemabruf)
               oder TDSfGModemCommObj (f�r DSfG-Abruf)
             Flag "Initialisierung f�r MRG 800 PTB" ja/nein
             Modemname
             Pfad der MODEM-INI-Konfiguration
             Timeout f�r Modem-Initialisierung
             Timeout f�r GSM-Modembefehle
             COM-Nummer
             Flag 'GSM_WaitAfterLogin': wenn true �bergeben wird, erfolgt nach dem
                                        Login eine Wartezeit von ca. 20s (f�r
                                        anschlie�ende Zugriffe auf GSM-Modemspeicher
                                        (z.B. SMS-Liste, Telefonbuch etc.)
             Zeiger auf Logfile (wenn NIL �bergeben wird, erfolgen keine Logfile-Eintr�ge)
  R�ckgaben: Fehlergruppe, Fehlercode
  Ergebnis: true, wenn Modem-Initialisierung OK }
var
  S, ResetStr, InitStr: string;
  MI: TModemIni;
  SI: TSrvCfg32Ini;
  isGSM_Modem: boolean;

begin
  Result:=false;

  { Initialisierungsstrings lesen: }
  MI:=TModemIni.Create (NetProgPfad);
  try
    isGSM_Modem:=MI.GetGSM (ModemName);
    if CommObj is TMRGModemCommObj then begin   { Modem-Initialisierung f�r MRG durchf�hren }
      if MRG800PTB then
        S:=MI.GetMRG800PTBInitString(ModemName)
      else
        S:=MI.GetMRGInitString(ModemName);
    end else                                     { Modem-Initialisierung f�r DSfG durchf�hren }
      S:=MI.GetDSfGInitString(ModemName);
  finally
    MI.Free;
  end;

  if S = CModemInitDefault then begin  { keine Initialisierung definiert: Modem f�r Stationsabruf nicht verwendbar }
    Fehlergruppe:=COM_PORTERROR;
    Fehlercode:= COMPORTERR_DUE_GERAET_FALSCH;
    exit;
  end;

  ResetStr:=F_Zerlegen (S, CFupModemInit_Trenner); { "Reset"-String (f�r Werkseinstellung laden) }
  InitStr:=S;                                      { "Init"-String (f�r spezielle Modemeinstellungen) }

  SI:=TSrvCfg32Ini.Create (NetProgPfad);
  try
    if not Modem_Initialisieren (CommObj, ResetStr, InitStr, isGSM_Modem,
      SI.PIN [COMNr], SI.PIN_Lock [COMNr], Timeout_ModemInit, Timeout_GSMModem,
      COMNr, GSM_WaitAfterLogin, LogFile, Fehlergruppe, Fehlercode) then begin
      if (Fehlergruppe = COM_MODEMERROR) AND (Fehlercode = CME_GSM_PIN_WRONG) then
        { Login in GSM-Modem nicht m�glich: PIN ist falsch.
          -> PIN-Login f�r Modem wird in Schnittstellen-Konfiguration gesperrt,
             damit die SIM-Karte nicht durch weitere Fehl-Versuchen gesperrt wird ! }
        SI.PIN_Lock [COMNr]:=true
      else if (Fehlergruppe = COM_MODEMERROR) AND (Fehlercode = CME_GSM_PIN_LOGINIMPOSSIBLE_PUK) then
        { Login in GSM-Modem �ber PIN nicht m�glich: PUK-Eingabe ist erforderlich.
          -> PIN-Login f�r Modem wird in Schnittstellen-Konfiguration gesperrt,
             da weitere Login-Versuche �ber PIN ohnehin zwecklos sind ! }
        SI.PIN_Lock [COMNr]:=true
      else if (Fehlergruppe = COM_DEVICEINITERROR) AND (Fehlercode = DEVINITERR_MODEMINIT) then begin
        if Assigned (VerifyFlagObject) then
          VerifyFlagObject.WriteTerminalErrorFlag;  { zentrale Wico-System�berwachung: Fehler ! }
      end;
      exit;
    end;
  finally
    SI.Free;
  end;
  Result:=true;
end;

end.
