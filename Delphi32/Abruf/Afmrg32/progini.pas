{******************************************************************************}
{* Unit: INI-Datei des MRG-Abrufmodul lesen/schreiben                         *}
{* Version: 17.11.98  WW                                                      *}
{******************************************************************************}
unit ProgIni;

interface

uses
  PathIni, AbrufTimeoutConst, MSysCon;

type

  { Objekt für AFMRG32.INI }
  TAfMRG32Ini = class (TProgramIni)
  protected
    function GetTOModemAntwort: integer;
    function GetTOFup: integer;
    function GetTOFupReset: integer;
    function GetTOModemInit: integer;
    function GetTOCRCCheck: integer;
    function GetTOProtMeldung: integer;
    function GetTOIEC1107Telegr: integer;
    function GetTOVerbindungsaufbau: integer;
    function GetTOVerbindungsabbau: integer;
    function GetTORufAnnahmeModem: integer;
    function GetTOKennung: integer;
    function GetTOLogin: integer;
    function GetTOParameter: integer;
    function GetTOMeldungen: integer;
    function GetTOMesswerte: integer;
    function GetTOTagessaetze: integer;
    function GetTOPruefsaetze: integer;
    function GetTOBinaerdatei: integer;
    function GetTORundpufferReset: integer;
    function GetTOParametrieren: integer;
    function GetTORufausloesung: integer;
    function GetTODSfGUmschaltung: integer;
    function GetTODSfGRufliste: integer;
    function GetTODSfGRufQuittung: integer;
    function GetCRCVersuche: integer;
    function GetBCCVersuche: integer;
    function GetProtVersuche: integer;
  public
    property TOModemAntwort: integer read GetTOModemAntwort;           { Timeout Modem-Antwort }
    property TOFup: integer read GetTOFup;                             { Timeout FUP-Befehl }
    property TOFupReset: integer read GetTOFupReset;                   { Timeout FUP-Reset }
    property TOModemInit: integer read GetTOModemInit;                 { Timeout Modeminitialisierung }
    property TOCRCCheck: integer read GetTOCRCCheck;                   { Timeout CRC-Check }
    property TOProtMeldung: integer read GetTOProtMeldung;             { Timeout ACK0/1-Protokollmeldung }
    property TOIEC1107Telegr: integer read GetTOIEC1107Telegr;         { Timeout IEC1107-Telegramm }
    property TOVerbindungsaufbau: integer read GetTOVerbindungsaufbau; { Timeout Verbindungsaufbau }
    property TOVerbindungsabbau: integer read GetTOVerbindungsabbau;   { Timeout Verbindungsabbau }
    property TORufAnnahmeModem: integer read GetTORufAnnahmeModem;     { Timeout Rufannahme mit Modem }
    property TOKennung: integer read GetTOKennung;                     { Timeout Kennung lesen }
    property TOLogin: integer read GetTOLogin;                         { Timeout DFÜ-Login }
    property TOParameter: integer read GetTOParameter;                 { Timeout Parameter lesen }
    property TOMeldungen: integer read GetTOMeldungen;                 { Timeout Meldungen lesen }
    property TOMesswerte: integer read GetTOMesswerte;                 { Timeout Meßwerte lesen }
    property TOTagessaetze: integer read GetTOTagessaetze;             { Timeout Tagessätze lesen }
    property TOPruefsaetze: integer read GetTOPruefsaetze;             { Timeout Prüfungssätze lesen }
    property TOBinaerdatei: integer read GetTOBinaerdatei;             { Timeout Binärdatei-Befehl übertragen }
    property TORundpufferReset: integer read GetTORundpufferReset;     { Timeout Rundpufferreset durchführen }
    property TOParametrieren: integer read GetTOParametrieren;         { Timeout Parameter übertragen }
    property TORufausloesung: integer read GetTORufausloesung;         { Timeout Ruf im MRG auslösen }
    property TODSfGUmschaltung: integer read GetTODSfGUmschaltung;     { Timeout bei DSfG-Umleitung: umschalten auf Slave }
    property TODSfGRufliste: integer read GetTODSfGRufliste;           { Timeout bei DSfG-Umleitung: Rufliste abfragen }
    property TODSfGRufQuittung: integer read GetTODSfGRufQuittung;     { Timeout bei DSfG-Umleitung: Ruf quittieren }
    property CRCVersuche: integer read GetCRCVersuche;                 { max. Versuche bei falschem CRC }
    property BCCVersuche: integer read GetBCCVersuche;                 { max. Versuche bei falschem BCC }
    property ProtVersuche: integer read GetProtVersuche;               { max. Versuche bei falscher ACK0/1-Protokollantwort }
  end;

var
  AfMRG32Ini: TAfMRG32Ini;

implementation

const
  CSectionProgram = 'AfMRG32';

  CIdentTOModemAntwort      = 'TOModemAntwort';
  CIdentTOFup               = 'TOFup';
  CIdentTOFupReset          = 'TOFupReset';
  CIdentTOModemInit         = 'TOModemInit';
  CIdentTOCRCCheck          = 'TOCRCCheck';
  CIdentTOProtMeldung       = 'TOProtMeldung';
  CIdentTOIEC1107Telegr     = 'TOIEC1107Telegr';
  CIdentTOVerbindungsaufbau = 'TOVerbindungsaufbau';
  CIdentTOVerbindungsabbau  = 'TOVerbindungsabbau';
  CIdentTORufAnnahmeModem   = 'TORufAnnahmeModem';
  CIdentTOKennung           = 'TOKennung';
  CIdentTOLogin             = 'TOLogin';
  CIdentTOParameter         = 'TOParameter';
  CIdentTOMeldungen         = 'TOMeldungen';
  CIdentTOMesswerte         = 'TOMesswerte';
  CIdentTOTagessaetze       = 'TOTagessaetze';
  CIdentTOPruefsaetze       = 'TOPruefsaetze';
  CIdentTOBinaerdatei       = 'TOBinaerdatei';
  CIdentTORundpufferReset   = 'TORundpufferReset';
  CIdentTOParametrieren     = 'TOParametrieren';
  CIdentTORufausloesung     = 'TORufausloesung';
  CIdentTODSfGUmschaltung   = 'TODSfGUmschaltung';
  CIdentTODSfGRufliste      = 'TODSfGRufliste';
  CIdentTODSfGRufQuittung   = 'TODSfGRufQuittung';

  CIdentCRCVersuche         = 'CRCVersuche';
  CIdentBCCVersuche         = 'BCCVersuche';
  CIdentProtVersuche        = 'ProtVersuche';


{ TAfMRG32Ini }

{----------------------------------------------}
function TAfMRG32Ini.GetTOModemAntwort: integer;
{----------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOModemAntwort, CTimeout_ModemAntwort);  { ms }
end;

{-------------------------------------}
function TAfMRG32Ini.GetTOFup: integer;
{-------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOFup, CMRG_Timeout_FupAntwort);      { ms }
end;

{------------------------------------------}
function TAfMRG32Ini.GetTOFupReset: integer;
{------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOFupReset, CMRG_Timeout_FupReset);      { ms }
end;

{-------------------------------------------}
function TAfMRG32Ini.GetTOModemInit: integer;
{-------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOModemInit, CTimeout_ModemInit);      { ms }
end;

{------------------------------------------}
function TAfMRG32Ini.GetTOCRCCheck: integer;
{------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOCRCCheck, CMRG_Timeout_CRCCheck);      { ms }
end;

{---------------------------------------------}
function TAfMRG32Ini.GetTOProtMeldung: integer;
{---------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOProtMeldung, CMRG_Timeout_ACK01ProtMeldung);      { ms }
end;

{-----------------------------------------------}
function TAfMRG32Ini.GetTOIEC1107Telegr: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOIEC1107Telegr, CMRG_Timeout_IEC1107Telegr);      { ms }
end;

{---------------------------------------------------}
function TAfMRG32Ini.GetTOVerbindungsaufbau: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOVerbindungsaufbau, CTimeout_Verbindungsaufbau);      { ms }
end;

{--------------------------------------------------}
function TAfMRG32Ini.GetTOVerbindungsabbau: integer;
{--------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOVerbindungsabbau, CTimeout_Verbindungsabbau);      { ms }
end;

{-------------------------------------------------}
function TAfMRG32Ini.GetTORufAnnahmeModem: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTORufAnnahmeModem, CTimeout_RufAnnahmeModem);      { ms }
end;

{-----------------------------------------}
function TAfMRG32Ini.GetTOKennung: integer;
{-----------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOKennung, CTimeout_Kennung);      { ms }
end;

{---------------------------------------}
function TAfMRG32Ini.GetTOLogin: integer;
{---------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOLogin, CTimeout_Login);      { ms }
end;

{-------------------------------------------}
function TAfMRG32Ini.GetTOParameter: integer;
{-------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOParameter, CTimeout_Parameter);      { ms }
end;

{-------------------------------------------}
function TAfMRG32Ini.GetTOMeldungen: integer;
{-------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOMeldungen, CTimeout_Meldungen);      { ms }
end;

{-------------------------------------------}
function TAfMRG32Ini.GetTOMesswerte: integer;
{-------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOMesswerte, CTimeout_Messwerte);      { ms }
end;

{---------------------------------------------}
function TAfMRG32Ini.GetTOTagessaetze: integer;
{---------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOTagessaetze, CTimeout_Tagessaetze);      { ms }
end;

{---------------------------------------------}
function TAfMRG32Ini.GetTOPruefsaetze: integer;
{---------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOPruefsaetze, CTimeout_Pruefsaetze);      { ms }
end;

{--------------------------------------------}
function TAfMRG32Ini.GetTOBinaerdatei: integer;
{--------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOBinaerdatei, CTimeout_Binaerdatei);      { ms }
end;

{-------------------------------------------------}
function TAfMRG32Ini.GetTORundpufferReset: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTORundpufferReset, CTimeout_RundpufferReset);      { ms }
end;

{-----------------------------------------------}
function TAfMRG32Ini.GetTOParametrieren: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOParametrieren, CTimeout_Parametrieren);      { ms }
end;

{-----------------------------------------------}
function TAfMRG32Ini.GetTORufausloesung: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTORufausloesung, CTimeout_Rufausloesung);      { ms }
end;

{-------------------------------------------------}
function TAfMRG32Ini.GetTODSfGUmschaltung: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTODSfGUmschaltung, CTimeout_DSfGUmschaltung);      { ms }
end;

{----------------------------------------------}
function TAfMRG32Ini.GetTODSfGRufliste: integer;
{----------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTODSfGRufliste, CTimeout_DSfGRufliste);      { ms }
end;

{-------------------------------------------------}
function TAfMRG32Ini.GetTODSfGRufQuittung: integer;
{-------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTODSfGRufQuittung, CTimeout_DSfGRufQuittung);      { ms }
end;

{-------------------------------------------}
function TAfMRG32Ini.GetCRCVersuche: integer;
{-------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentCRCVersuche, CMRG_CRCVersuche);
end;

{-------------------------------------------}
function TAfMRG32Ini.GetBCCVersuche: integer;
{-------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentBCCVersuche, CMRG_BCCVersuche);
end;

{--------------------------------------------}
function TAfMRG32Ini.GetProtVersuche: integer;
{--------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentProtVersuche, CMRG_ACK01ProtVersuche);
end;

end.
