{******************************************************************************}
{* Unit: INI-Datei des DSfG-Abrufmodul lesen/schreiben                        *}
{* Version: 17.11.99  WW                                                      *}
{*          15.07.03  GD  Erweitert um "Delay" bei Momentanwertabruf          *}
{******************************************************************************}
unit ProgIni;

interface

uses
  PathIni, AbrufTimeoutConst, DSysCon, WSysCon;

type

  { Objekt für AFDSFG32.INI }
  TAfDSfG32Ini = class (TProgramIni)
  protected
    function GetTOModemAntwort: integer;
    function GetTOModemInit: integer;
    function GetTOVerbindungsaufbau: integer;
    function GetTOVerbindungsabbau: integer;
    function GetTORufAnnahme: integer;
    function GetTOLogin: integer;
    function GetTODFUETransparent: integer;
    function GetTOArchive: integer;
    function GetTOLogbuecher: integer;
    function GetTODatenelemente: integer;
    function GetTOKonfLesen: integer;
    function GetTOEinstellen: integer;
    function GetTODFUEParameter: integer;
    function GetTOBinaerdatei: integer;
    function GetBCCVersuche: integer;
    function GetAutomatikZeitabruf: boolean;
    function GetAutomatikErsteDatenTage: integer;
    function GetFW_Zeitfenster: integer;
    function GetMomDelay: integer;   // 15.07.2003
  public
    property TOModemAntwort: integer read GetTOModemAntwort;            { Timeout Modem-Antwort }
    property TOModemInit: integer read GetTOModemInit;                  { Timeout Modeminitialisierung }
    property TOVerbindungsaufbau: integer read GetTOVerbindungsaufbau;  { Timeout Verbindungsaufbau }
    property TOVerbindungsabbau: integer read GetTOVerbindungsabbau;    { Timeout Verbindungsabbau }
    property TORufAnnahme: integer read GetTORufAnnahme;                { Timeout Rufannahme }
    property TOLogin: integer read GetTOLogin;                          { Timeout DFÜ-Login }
    property TODFUETransparent: integer read GetTODFUETransparent;      { Timeout DFÜ transparentschalten }
    property TOArchive: integer read GetTOArchive;                      { Timeout Archive lesen }
    property TOLogbuecher: integer read GetTOLogbuecher;                { Timeout Logbücher lesen }
    property TODatenelemente: integer read GetTODatenelemente;          { Timeout Datenelemente lesen }
    property TOKonfLesen: integer read GetTOKonfLesen;                  { Timeout Konfiguration lesen }
    property TOEinstellen: integer read GetTOEinstellen;                { Timeout Datenelemente einstellen }
    property TODFUEParameter: integer read GetTODFUEParameter;          { Timeout DSfG-DFÜ-Parameter lesen/einstellen }
    property TOBinaerdatei: integer read GetTOBinaerdatei;              { Timeout Binärdateibefehl senden }
    property BCCVersuche: integer read GetBCCVersuche;                  { max. Versuche bei falschem BCC }
    property AutomatikZeitabruf: boolean read GetAutomatikZeitabruf;    { Automatikabruf über Zeit ein/aus }
    property AutomatikErsteDatenTage: integer read
      GetAutomatikErsteDatenTage;    { Anzahl der Tage zurück bei erstem Automatikabruf über Zeit }
    property MomDelay: integer read GetMomDelay;
    property FW_Zeitfenster: integer read GetFW_Zeitfenster;            { Fahrweg-Toleranzbereich in s (für kavernenbezogene
                                                                          Datenkonvertierung) }
  end;

var
  AfDSfG32Ini: TAfDSfG32Ini;

implementation

const
  CSectionProgram   = 'AfDSfG32';
  CSectionAutomatik = 'Automatik';

  CIdentTOModemAntwort      = 'TOModemAntwort';
  CIdentTOModemInit         = 'TOModemInit';
  CIdentTOVerbindungsaufbau = 'TOVerbindungsaufbau';
  CIdentTOVerbindungsabbau  = 'TOVerbindungsabbau';
  CIdentTORufAnnahme        = 'TORufannahme';
  CIdentTOLogin             = 'TOLogin';
  CIdentTODFUETransparent   = 'TODFUETransparent';
  CIdentTOArchive           = 'TOArchive';
  CIdentTOLogbuecher        = 'TOLogbuecher';
  CIdentTODatenelemente     = 'TODatenelemente';
  CIdentTOKonfLesen         = 'TOKonfLesen';
  CIdentTOEinstellen        = 'TOEinstellen';
  CIdentTODFUEParameter     = 'TODFUEParameter';
  CIdentTOBinaerdatei       = 'TOBinaerdatei';
  CIdentDMomDelay           = 'DMOMDELAY';

  CIdentBCCVersuche         = 'BCCVersuche';

  CIdentAutoZeitabruf       = 'Zeitabruf';
  CIdentAutoErsteDatenTage  = 'ErsteDatenTage';

  CIdentFW_Zeitfenster      = 'FahrwegZeitfenster';


{ TAfDSfG32Ini }

{-----------------------------------------------}
function TAfDSfG32Ini.GetMomDelay: integer;   // 15.07.2003
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentDMomDelay, 2000);  { ms }
end;

{-----------------------------------------------}
function TAfDSfG32Ini.GetTOModemAntwort: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOModemAntwort, CTimeout_ModemAntwort);  { ms }
end;

{--------------------------------------------}
function TAfDSfG32Ini.GetTOModemInit: integer;
{--------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOModemInit, CTimeout_ModemInit);      { ms }
end;

{----------------------------------------------------}
function TAfDSfG32Ini.GetTOVerbindungsaufbau: integer;
{----------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOVerbindungsaufbau, CTimeout_Verbindungsaufbau);      { ms }
end;

{---------------------------------------------------}
function TAfDSfG32Ini.GetTOVerbindungsabbau: integer;
{---------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOVerbindungsabbau, CTimeout_Verbindungsabbau);      { ms }
end;

{---------------------------------------------}
function TAfDSfG32Ini.GetTORufAnnahme: integer;
{---------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTORufAnnahme, CTimeout_RufAnnahme);      { ms }
end;

{----------------------------------------}
function TAfDSfG32Ini.GetTOLogin: integer;
{----------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOLogin, CTimeout_Login);              { ms }
end;

{--------------------------------------------------}
function TAfDSfG32Ini.GetTODFUETransparent: integer;
{--------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTODFUETransparent, CTimeout_DFUETransparent);              { ms }
end;

{------------------------------------------}
function TAfDSfG32Ini.GetTOArchive: integer;
{------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOArchive, CTimeout_Archive);              { ms }
end;

{---------------------------------------------}
function TAfDSfG32Ini.GetTOLogbuecher: integer;
{---------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOLogbuecher, CTimeout_Logbuecher);              { ms }
end;

{------------------------------------------------}
function TAfDSfG32Ini.GetTODatenelemente: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTODatenelemente, CTimeout_Datenelemente);              { ms }
end;

{--------------------------------------------}
function TAfDSfG32Ini.GetTOKonfLesen: integer;
{--------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOKonfLesen, CTimeout_KonfLesen);              { ms }
end;

{---------------------------------------------}
function TAfDSfG32Ini.GetTOEinstellen: integer;
{---------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOEinstellen, CTimeout_Einstellen);              { ms }
end;

{------------------------------------------------}
function TAfDSfG32Ini.GetTODFUEParameter: integer;
{------------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTODFUEParameter, CTimeout_DFUEParameter);        { ms }
end;

{----------------------------------------------}
function TAfDSfG32Ini.GetTOBinaerdatei: integer;
{----------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentTOBinaerdatei, CTimeout_Binaerdatei);        { ms }
end;

{--------------------------------------------}
function TAfDSfG32Ini.GetBCCVersuche: integer;
{--------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentBCCVersuche, CDSfG_BCCVersuche);
end;

{---------------------------------------------------}
function TAfDSfG32Ini.GetAutomatikZeitabruf: boolean;
{---------------------------------------------------}
begin
  Result := ReadBool (CSectionAutomatik, CIdentAutoZeitabruf, CAutomatikZeitabruf);
end;

{--------------------------------------------------------}
function TAfDSfG32Ini.GetAutomatikErsteDatenTage: integer;
{--------------------------------------------------------}
begin
  Result := ReadInteger (CSectionAutomatik, CIdentAutoErsteDatenTage, CAutomatikErsteDatenTage);
end;

{-----------------------------------------------}
function TAfDSfG32Ini.GetFW_Zeitfenster: integer;
{-----------------------------------------------}
begin
  Result := ReadInteger (CSectionProgram, CIdentFW_Zeitfenster, C_FW_Zeitfenster);
end;

end.
