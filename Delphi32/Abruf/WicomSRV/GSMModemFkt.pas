{******************************************************************************}
{* Unit: Funktionen für Zugriff auf GSM-Modem                                 *}
{* 05.11.2004  WW                                                             *}
{* 19.11.2013  WW  Auswertung der Antwort auf PIN-Abfrage angepaßt für Sierra *}
{*                 Wireless Modem                                             *}
{******************************************************************************}
unit GSMModemFkt;

interface

uses
  Windows, SysUtils, O_Comm, SerMrgModem, SerDSfG, WChars, ErrConst, WComm,
  WStrUtils;

type
  { SMS-Formate }
  TSMS_Format = (smsf_PDU, smsf_Text);


function GSM_PIN_Check (CommObj: TCommObj; Timeout: integer; var PIN_Status: integer;
                        var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function GSM_PIN_Login (CommObj: TCommObj; Timeout: integer; PIN: string;
                        var PIN_OK: boolean;
                        var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function GSM_Set_SMS_Format (CommObj: TCommObj; Timeout: integer; SMS_Format: TSMS_Format;
                             var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function GSM_Readlist_SMS (CommObj: TCommObj; Timeout: integer; Lesestatus: string;
                           var SMS_Rohantwort: string;
                           var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function GSM_Delete_SMS (CommObj: TCommObj; Timeout: integer; SMS_Index: integer;
                         var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function GSM_Send_SMS (CommObj: TCommObj; Timeout: integer; Rufnummer, SMS_Text: string;
                       var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function GSM_Read_Signalquality (CommObj: TCommObj; Timeout: integer;
                                 var iSQ: integer; var iBFR: integer;
                                 var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function GSM_Read_Operator (CommObj: TCommObj; Timeout: integer; var sOp: string;
                            var Fehlergruppe: integer; var Fehlercode: integer): boolean;
function GSM_Read_Registration (CommObj: TCommObj; Timeout: integer; var sReg: string;
                                var Fehlergruppe: integer; var Fehlercode: integer): boolean;

implementation

Const
  { GSM-Modem-Antworten }
  C_OK      = 'OK';
  C_ERROR   = 'ERROR';
  C_READY   = 'READY';
  C_SIM_PIN = 'SIM PIN';
  C_SIM_PUK = 'SIM PUK';
  C_PROMPT  = '>';


{-----------------------------------------------------------------------------------}
function GSM_PIN_Check (CommObj: TCommObj; Timeout: integer; var PIN_Status: integer;
                        var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------------}
{ PIN abfragen von GSM-Modem;
  Übergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (für MRG-Modemabruf)
               oder TDSfGModemCommObj (für DSfG-Abruf)
             Timeout für PIN-Check
  Rückgaben: PIN_Status (-1: undefiniert, PIN-Status konnte nicht ermittelt werden
                          0: PIN ist eingegeben, Modem ist bereit (READY)
                          1: zum Login ist PIN einzugeben, Modem ist nicht bereit (SIM PIN)
                          2: zum Login ist PUK einzugeben, Modem ist nicht bereit (SIM PUK)
                          3: unbekannter PIN-Status, Modem ist nicht bereit
             Fehlergruppe, Fehlercode
  Ergebnis: false, wenn Fehler beim Abfragen der PIN aufgetreten ist }
var
  Befehl: string;
  R: TRueckgabe;
  OK: boolean;

begin
  Result:=false;

  { PIN-Abfrage-Befehl: }
  Befehl:='at+cpin?' + CR;

  { PIN-Abfrage-Befehl senden: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, false)  // 19.11.2013, WW
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, false) // 19.11.2013, WW
  else begin
    OK:=false;
  end;
  if not OK then begin                        { Fehler auf der Schnittstelle }
    PIN_Status:=-1;
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auf PIN-Abfrage-Befehl auswerten: }
  if Pos (C_OK, R.Antwort) = 0 then begin   { Fehler: kein OK in Antwort enthalten }
    { Fehlerbedingung für Sierra Wireless Modem erweitert (schickt kein OK in der
      Antwort): ERROR in Antwort enthalten; 19.11.2013, WW }
    if Pos (C_ERROR, R.Antwort) > 0 then begin
      PIN_Status:=-1;
      Fehlergruppe:=COM_MODEMERROR;
      Fehlercode:=CME_GSM_PIN_CHECK;
      exit;
    end;
  end;

  { Antwort ist OK: }
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;

  { PIN-Status prüfen: }
  if Pos (C_READY, R.Antwort) <> 0 then           { Bereit }
    PIN_Status:=0
  else if Pos (C_SIM_PIN, R.Antwort) <> 0 then    { PIN eingeben }
    PIN_Status:=1
  else if Pos (C_SIM_PUK, R.Antwort) <> 0 then    { PUK eingeben }
    PIN_Status:=2
  else                                            { sonstiges }
    PIN_Status:=3;
end;


{-----------------------------------------------------------------------------------}
function GSM_PIN_Login (CommObj: TCommObj; Timeout: integer; PIN: string;
                        var PIN_OK: boolean;
                        var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-----------------------------------------------------------------------------------}
{ PIN in GSM-Modem eingeben;
  Übergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (für MRG-Modemabruf)
               oder TDSfGModemCommObj (für DSfG-Abruf)
             Timeout für PIN-Login
             PIN
  Rückgaben: Flag PIN_OK (true, wenn PIN vom Modem akzeptiert wurde)
             Fehlergruppe, Fehlercode
  Ergebnis: false, wenn Fehler beim Eingeben der PIN aufgetreten ist }
var
  Befehl: string;
  R: TRueckgabe;
  OK: boolean;

begin
  Result:=false;

  { PIN-Eingabe-Befehl: }
  Befehl:='at+cpin=' + PIN + CR;

  { PIN-Eingabe-Befehl senden: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else begin
    OK:=false;
  end;
  if not OK then begin                        { Fehler auf der Schnittstelle }
    PIN_OK:=false;
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort ist OK: }
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;

  { Antwort auf PIN-Eingabe-Befehl auswerten: }
  if Pos (C_OK, R.Antwort) = 0 then
    PIN_OK:=false    { bei falscher PIN kommt ERROR als Antwort }
  else
    PIN_OK:=true;    { bei richtiger PIN kommt OK als Antwort }
end;


{----------------------------------------------------------------------------------------}
function GSM_Set_SMS_Format (CommObj: TCommObj; Timeout: integer; SMS_Format: TSMS_Format;
                             var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{----------------------------------------------------------------------------------------}
{ SMS-Format im GSM-Modem setzen;
  Übergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (für MRG-Modemabruf)
               oder TDSfGModemCommObj (für DSfG-Abruf)
             Timeout für Setzen des SMS-Formats
             SMS-Format
  Rückgaben: Fehlergruppe, Fehlercode
  Ergebnis: false, wenn Fehler beim Setzen des SMS-Formats aufgetreten ist }
var
  Befehl: string;
  R: TRueckgabe;
  OK: boolean;
  FmtMode: char;

begin
  Result:=false;

  { SMS-Format-Befehl: }
  if SMS_Format = smsf_PDU then
    FmtMode:='0'
  else
    FmtMode:='1';
  Befehl:='at+cmgf=' + FmtMode + CR;

  { SMS-Format-Befehl senden: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else begin
    OK:=false;
  end;
  if not OK then begin                      { Fehler auf der Schnittstelle }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auf SMS-Format-Befehl auswerten: }
  if Pos (C_OK, R.Antwort) = 0 then begin   { Fehler: kein OK in Antwort enthalten }
    Fehlergruppe:=COM_MODEMERROR;
    Fehlercode:=CME_GSM_SMS_FORMAT_WRITE;
    exit;
  end;

  { SMS-Format wurde eingestellt }
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;
end;

{--------------------------------------------------------------------------------------}
function GSM_Readlist_SMS (CommObj: TCommObj; Timeout: integer; Lesestatus: string;
                           var SMS_Rohantwort: string;
                           var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{--------------------------------------------------------------------------------------}
{ SMS-Liste aus GSM-Modem auslesen;
  Übergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (für MRG-Modemabruf)
               oder TDSfGModemCommObj (für DSfG-Abruf)
             Timeout für Auslesen der SMS
             Status für Auslesen der SMS je nach eingestelltem SMS-Format (Text bzw. PDU)
                 Text         PDU
               'REC UNREAD' /  0  -> alle empfangenen, ungelesenen SMS
               'REC READ'   /  1  -> alle empfangenen, gelesenen SMS
               'STO UNSENT' /  2  -> alle gespeicherten, ungesendeten SMS
               'STO SENT'   /  3  -> alle gespeicherten, gesendeten SMS
               'ALL'        /  4  -> alle SMS
  Rückgaben: SMS-Liste als Rohstring
             Fehlergruppe, Fehlercode
  Ergebnis: false, wenn Fehler beim Auslesen der SMS aufgetreten ist }
var
  Befehl: string;
  R: TRueckgabe;
  OK: boolean;

begin
  Result:=false;
  SMS_Rohantwort:='';

  { SMS-Lese-Befehl: }
  Befehl:='at+cmgl=' + Lesestatus + CR;

  { SMS-Lese-Befehl senden: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else begin
    OK:=false;
  end;
  if not OK then begin                        { Fehler auf der Schnittstelle }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auf SMS-Lese-Befehl auswerten: }
  if Pos (C_OK, R.Antwort) = 0 then begin   { Fehler: kein OK in Antwort enthalten }
    Fehlergruppe:=COM_MODEMERROR;
    Fehlercode:=CME_GSM_SMS_READ;
    exit;
  end;

  { Antwort ist OK: }
  SMS_Rohantwort:=R.Antwort;
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;
end;

{------------------------------------------------------------------------------------}
function GSM_Delete_SMS (CommObj: TCommObj; Timeout: integer; SMS_Index: integer;
                         var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{------------------------------------------------------------------------------------}
{ SMS im GSM-Modem löschen;
  Übergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (für MRG-Modemabruf)
               oder TDSfGModemCommObj (für DSfG-Abruf)
             Timeout für Löschen der SMS
             SMS-Index
  Rückgaben: Fehlergruppe, Fehlercode
  Ergebnis: false, wenn Fehler beim Löschen der SMS aufgetreten ist }
var
  Befehl: string;
  R: TRueckgabe;
  OK: boolean;

begin
  Result:=false;

  { SMS-Lösch-Befehl: }
  Befehl:='at+cmgd=' + IntToStr (SMS_Index) + CR;

  { SMS-Format-Befehl senden: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else begin
    OK:=false;
  end;
  if not OK then begin                      { Fehler auf der Schnittstelle }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auf SMS-Lösch-Befehl auswerten: }
  if Pos (C_OK, R.Antwort) = 0 then begin   { Fehler: kein OK in Antwort enthalten }
    Fehlergruppe:=COM_MODEMERROR;
    Fehlercode:=CME_GSM_SMS_DELETE;
    exit;
  end;

  { SMS wurde gelöscht }
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;
end;

{--------------------------------------------------------------------------------------}
function GSM_Send_SMS (CommObj: TCommObj; Timeout: integer; Rufnummer, SMS_Text: string;
                       var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{--------------------------------------------------------------------------------------}
{ SMS vom GSM-Modem aus versenden;
  Übergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (für MRG-Modemabruf)
               oder TDSfGModemCommObj (für DSfG-Abruf)
             Timeout für Senden der SMS
             Rufnummer, an die die SMS versendet werden soll
             SMS-Text
  Rückgaben: Fehlergruppe, Fehlercode
  Ergebnis: false, wenn Fehler beim Senden der SMS aufgetreten ist }
var
  Befehl: string;
  R: TRueckgabe;
  OK: boolean;

begin
  Result:=false;

  Befehl:='at+cmgs="' + Rufnummer + '"' + CR;

  { SMS-Sende-Befehl mit Rufnummer senden, warten auf Eingabeaufforderung: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, false)
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, false)
  else begin
    OK:=false;
  end;
  if not OK then begin                      { Fehler auf der Schnittstelle }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auswerten: }
  if Pos (C_PROMPT, R.Antwort) = 0 then begin   { Fehler: keine Eingabeaufforderung in Antwort enthalten }
    Fehlergruppe:=COM_MODEMERROR;
    Fehlercode:=CME_GSM_SMS_SEND;
    exit;
  end;

  Befehl:=SMS_Text + SUB;

  { SMS-Text senden: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else begin
    OK:=false;
  end;
  if not OK then begin                      { Fehler auf der Schnittstelle }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auswerten: }
  if Pos (C_OK, R.Antwort) = 0 then begin   { Fehler: kein OK in Antwort enthalten }
    Fehlergruppe:=COM_MODEMERROR;
    Fehlercode:=CME_GSM_SMS_SEND;
    exit;
  end;

  { SMS wurde gesendet }
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;
end;

{--------------------------------------------------------------------------------------------}
function GSM_Read_Signalquality (CommObj: TCommObj; Timeout: integer;
                                 var iSQ: integer; var iBFR: integer;
                                 var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{--------------------------------------------------------------------------------------------}
{ Signalqualität aus GSM-Modem lesen;
  Übergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (für MRG-Modemabruf)
               oder TDSfGModemCommObj (für DSfG-Abruf)
             Timeout für Lesen der Signalqualität
  Rückgaben: Signalqualität ( 0..10 = schlecht
                             11..22 = normal
                             23..31 = sehr gut
                                 99 = nicht feststellbar)
             Bitfehlerrate (0..7 während Verbindung, sonst 99)
             Fehlergruppe, Fehlercode
  Ergebnis: false, wenn Fehler beim Lesen der Signalqualität aufgetreten ist }
var
  Befehl: string;
  R: TRueckgabe;
  OK: boolean;
  S: string;

begin
  Result:=false;
  iSQ:=-1;  // Vorbelegung: Wert nicht vorhanden
  iBFR:=-1;  // Vorbelegung: Wert nicht vorhanden

  { Befehl zum Abfragen der GSM-Signalqualität: }
  Befehl:='at+csq' + CR;

  { GSM-Signalqualitäts-Lesebefehl senden: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else begin
    OK:=false;
  end;
  if not OK then begin                      { Fehler auf der Schnittstelle }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auf GSM-Signalqualitäts-Lesebefehl auswerten: }
  if (Pos (C_OK, R.Antwort) = 0) then begin  { Fehler: kein OK in Antwort enthalten }
    Fehlergruppe:=COM_MODEMERROR;
    Fehlercode:=CME_GSM_CSQ_READ;
    exit;
  end;

  { Antwort ist OK: }
  S:=ExtractString (R.Antwort, ' ' , ',', 0);
  iSQ:=StrToIntDef (S, -1);  // Signalqualität
  S:=ExtractString (R.Antwort, ',' , CR, 0);
  iBFR:=StrToIntDef (S, -1);  // Bitfehlerrate
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;
end;

{---------------------------------------------------------------------------------------}
function GSM_Read_Operator (CommObj: TCommObj; Timeout: integer; var sOp: string;
                            var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{---------------------------------------------------------------------------------------}
{ gewählten Netzbetreiber aus GSM-Modem lesen;
  Übergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (für MRG-Modemabruf)
               oder TDSfGModemCommObj (für DSfG-Abruf)
             Timeout für Lesen des Netzbetreibers
  Rückgaben: Rohstring im Format <n>,[,<format>,<oper>]
             Fehlergruppe, Fehlercode
  Ergebnis: false, wenn Fehler beim Lesen des Netzbetreibers aufgetreten ist }
var
  Befehl: string;
  R: TRueckgabe;
  OK: boolean;

begin
  Result:=false;
  sOp:='';  // Vorbelegung: Wert nicht vorhanden

  { Befehl zum Abfragen der GSM-Signalqualität: }
  Befehl:='at+cops?' + CR;

  { Netzbetreiber-Lesebefehl senden: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else begin
    OK:=false;
  end;
  if not OK then begin                      { Fehler auf der Schnittstelle }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auf Netzbetreiber-Befehl auswerten: }
  if Pos (C_OK, R.Antwort) = 0 then begin   { Fehler: kein OK in Antwort enthalten }
    Fehlergruppe:=COM_MODEMERROR;
    Fehlercode:=CME_GSM_COPS_READ;
    exit;
  end;

  { Antwort ist OK: }
  sOp:=ExtractString (R.Antwort, ' ' , CR, 0);
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;
end;

{-------------------------------------------------------------------------------------------}
function GSM_Read_Registration (CommObj: TCommObj; Timeout: integer; var sReg: string;
                                var Fehlergruppe: integer; var Fehlercode: integer): boolean;
{-------------------------------------------------------------------------------------------}
{ Einbuchzustand aus GSM-Modem lesen (Status im GSM-Netz);
  Übergaben: Zeiger auf Kommunikationsobjekt TMRGModemCommObj (für MRG-Modemabruf)
               oder TDSfGModemCommObj (für DSfG-Abruf)
             Timeout für Lesen des Einbuchzustands
  Rückgaben: Rohstring im Format <n>,<stat>
             Fehlergruppe, Fehlercode
  Ergebnis: false, wenn Fehler beim Lesen des Einbuchzustands aufgetreten ist }
var
  Befehl: string;
  R: TRueckgabe;
  OK: boolean;

begin
  Result:=false;
  sReg:='';  // Vorbelegung: Wert nicht vorhanden

  { Befehl zum Abfragen des Einbuchzustands: }
  Befehl:='at+creg?' + CR;

  { Einbuchzustand-Lesebefehl senden: }
  if CommObj is TMRGModemCommObj then
    OK:=TMRGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else if CommObj is TDSfGModemCommObj then
    OK:=TDSfGModemCommObj (CommObj).SendModemCommand (Befehl, Timeout, R, true)
  else begin
    OK:=false;
  end;
  if not OK then begin                      { Fehler auf der Schnittstelle }
    Fehlergruppe:=R.Fehlergruppe;
    Fehlercode:=R.Fehlercode;
    exit;
  end;

  { Antwort auf Einbuchzustand-Befehl auswerten: }
  if Pos (C_OK, R.Antwort) = 0 then begin   { Fehler: kein OK in Antwort enthalten }
    Fehlergruppe:=COM_MODEMERROR;
    Fehlercode:=CME_GSM_REG_READ;
    exit;
  end;

  { Antwort ist OK: }
  sReg:=ExtractString (R.Antwort, ' ' , CR, 0);
  Fehlergruppe:=0;
  Fehlercode:=0;
  Result:=true;
end;

end.
