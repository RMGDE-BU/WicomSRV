{******************************************************************************}
{* Unit: Zugriff auf INI-Datei des Modbus-Master                              *}
{* 15.02.2010  WW Neu                                                         *}
{* 20.03.2024  WW COM-Plausibilitätsprüfung bis MaxPossiblePort               *}
{******************************************************************************}
unit ModbusMasterIniFile;

interface

uses
  Windows, Classes, SysUtils, PathIni, Serial, SerialConst, ModbusUtil, WSysCon;

type
  { INI-Objekt für Modbus-Master }

  TModbusMasterIni = class (TProgramIni)
  protected
    function GetDebugThreadProtokoll: boolean;
    function GetDebugCOMProtokoll: boolean;
    function GetDebugFehlerProtokoll: boolean;
    function GetDebugWriteProtokoll: boolean;
    function GetBaud (COMNr: integer): integer;
    function GetDatabits (COMNr: integer): TDataBits;
    function GetParity (COMNr: integer): TParityBit;
    function GetStopbits (COMNr: integer): TStopBits;
    function GetTOAntwort (COMNr: integer): integer;
    function GetTOZeichen (COMNr: integer): integer;
    function GetIpPort(iCOMNr: integer): integer;
    function GetIpAddress(iCOMNr: integer): string;
    function GetModbusModus (COMNr: integer): TModbusModus;
    function GetAusgabeEmpfaengerWM: string;
    function GetAusgabeEmpfaengerHWND: HWND;
  public
    constructor Create (sFilename: TFilename = '');
    function GetCOMList (Sl: TStrings): boolean;
    function GetIPList (Sl: TStrings): boolean;  // 04.08.2010 GD
    { Debug: }
    property DebugThreadProtokoll: boolean read GetDebugThreadProtokoll;
    property DebugCOMProtokoll: boolean read GetDebugCOMProtokoll;
    property DebugFehlerProtokoll: boolean read GetDebugFehlerProtokoll;
    property DebugWriteProtokoll: boolean read GetDebugWriteProtokoll;  // 28.06.2017, WW
    { COM: }
    property Baud [COMNr: integer]: integer read GetBaud;
    property Databits [COMNr: integer]: TDataBits read GetDatabits;
    property Parity [COMNr: integer]: TParityBit read GetParity;
    property Stopbits [COMNr: integer]: TStopBits read GetStopbits;
    property TOAntwort [COMNr: integer]: integer read GetTOAntwort;
    property TOZeichen [COMNr: integer]: integer read GetTOZeichen;
    property ModbusModus [COMNr: integer]: TModbusModus read GetModbusModus;
    property IPPort [iCOMNr: integer]: integer read GetIpPort;
    property IPAddress [iCOMNr: integer]: string read GetIpAddress;
    { Ausgabe: }
    property AusgabeEmpfaengerWM: string read GetAusgabeEmpfaengerWM;
    property AusgabeEmpfaengerHWND: HWND read GetAusgabeEmpfaengerHWND;
  end;

implementation

const
  { Sections }
  CSectionDebug = 'Debug';
  CSectionCOM   = 'COM';
  CSectionCOMn  = 'COM%d';
  CSectionIP    = 'IP';
  CSectionIPn   = 'IP%d';
  CSectionAusgabe = 'Ausgabe';

  { Idents }
  CIdentThreadProtokoll = 'ThreadProtokoll';
  CIdentCOMProtokoll    = 'COMProtokoll';
  CIdentFehlerProtokoll = 'FehlerProtokoll';
  CIdentWriteProtokoll  = 'WriteProtokoll';

  CIdentBaud          = 'Baud';
  CIdentDatabits      = 'Databits';
  CIdentParity        = 'Parity';
  CIdentStopbits      = 'Stopbits';
  CIdentTOAntwort     = 'TOAntwort';
  CIdentTOZeichen     = 'TOZeichen';
  CIdentModbusModus   = 'ModbusModus';
  CIdentIPPort        = 'IPPORT';
  CIdentIpAddress     = 'IPADDRESS';

  CIdentEmpfaengerWM = 'EmpfaengerWM';
  CIdentEmpfaengerHWND = 'EmpfaengerHWND';

// Gibt abhängig von COM-Numer richtige Sektion (COM oder IP) zurück
// Parameter: Portnummer
// Rückgabe: Sektion
//-------------------------------------------------
function GetSectionName(iPort: word): string;
//-------------------------------------------------
begin
  if (iPort >= CComTCP_IP)
  then Result := Format(CSectionIPn, [iPort])
  else Result := Format(CSectionCOMn, [iPort]);
end;

{ TModbusMaster }

{--------------------------------------------------------------}
constructor TModbusMasterIni.Create (sFilename: TFilename = '');
{--------------------------------------------------------------}
begin
  inherited Create (true, false, sFilename);  // kein benutzerdefinierter Zugriff
                                              // Filename optional; 04.06.2018, WW
end;

{---------------------------------------------------------}
function TModbusMasterIni.GetDebugThreadProtokoll: boolean;
{---------------------------------------------------------}
begin
  Result:=ReadBool (CSectionDebug, CIdentThreadProtokoll, false);
end;

{------------------------------------------------------}
function TModbusMasterIni.GetDebugCOMProtokoll: boolean;
{------------------------------------------------------}
begin
  Result:=ReadBool (CSectionDebug, CIdentCOMProtokoll, false);
end;

{---------------------------------------------------------}
function TModbusMasterIni.GetDebugFehlerProtokoll: boolean;
{---------------------------------------------------------}
begin
  Result:=ReadBool (CSectionDebug, CIdentFehlerProtokoll, false);
end;

{--------------------------------------------------------}
function TModbusMasterIni.GetDebugWriteProtokoll: boolean;
{--------------------------------------------------------}
begin
  Result:=ReadBool (CSectionDebug, CIdentWriteProtokoll, false);
end;

{-----------------------------------------------------------}
function TModbusMasterIni.GetCOMList (Sl: TStrings): boolean;
{-----------------------------------------------------------}
{ liest alle COM-Sections und gibt die COM-Nummern in Sl zurück;
  Ergebnis: true, wenn gültige COM-Nummern konfiguriert sind }
var
  SectionList: TStringList;
  i: integer;
  Section: string;
  sCOMNr: string;
  iCOMNr: integer;
  OK: boolean;

begin
  Result:=true;
  if Sl = nil then exit;
  Sl.Clear;

  SectionList:=TStringList.Create;
  try
    ReadSections (SectionList);
    for i:=0 to SectionList.Count-1 do begin
      Section:=SectionList[i];
      if Pos (CSectionCOM, Section) = 1 then begin   { nur Sections, die mit COM... beginnen }
        sCOMNr:=Copy (Section, length (CSectionCOM) + 1, length (Section));
        iCOMNr:=StrToIntDef (sCOMNr, -1);
        OK:=(iCOMNr >= 1) AND (iCOMNr <= MaxPossiblePort);  // 20.03.2024, WW
        { gültige COM-Nummer in Liste laden: }
        if OK then
          Sl.Add (sCOMNr)
        else
          Result:=false;
      end;
    end;  { for i }
  finally
    SectionList.Free;
  end;
end;

{-----------------------------------------------------------}
function TModbusMasterIni.GetIPList (Sl: TStrings): boolean;  // 04.08.2010 GD
{-----------------------------------------------------------}
{ liest alle IP-Sections und gibt die IP-Nummern in Sl zurück;
  Ergebnis: true, wenn gültige IP-Nummern konfiguriert sind }
var
  SectionList: TStringList;
  i: integer;
  Section: string;
  sCOMNr: string;
  iCOMNr: integer;
  OK: boolean;

begin
  Result:=true;
  if Sl = nil then exit;
  Sl.Clear;

  SectionList:=TStringList.Create;
  try
    ReadSections (SectionList);
    for i:=0 to SectionList.Count-1 do begin
      Section:=SectionList[i];
      if Pos (CSectionIP, Section) = 1 then begin   { nur Sections, die mit IP... beginnen }
        sCOMNr:=Copy (Section, length (CSectionIP) + 1, length (Section));
        iCOMNr:=StrToIntDef (sCOMNr, -1);
        OK:=(iCOMNr >= CComTCP_IP) AND (iCOMNr < (CComTCP_IP + 100));  { Virtuelle COM-Nummern }
        { gültige COM-Nummer in Liste laden: }
        if OK then
          Sl.Add (sCOMNr)
        else
          Result:=false;
      end;
    end;  { for i }
  finally
    SectionList.Free;
  end;
end;

{----------------------------------------------------------}
function TModbusMasterIni.GetBaud (COMNr: integer): integer;
{----------------------------------------------------------}
begin
  Result:=ReadInteger (GetSectionName(COMNr), CIdentBaud, 9600);
end;

{----------------------------------------------------------------}
function TModbusMasterIni.GetDatabits (COMNr: integer): TDataBits;
{----------------------------------------------------------------}
var
  i: integer;
begin
  i:=ReadInteger (GetSectionName(COMNr), CIdentDatabits, 8);
  case i of
    4: Result:=db_4;
    5: Result:=db_5;
    6: Result:=db_6;
    7: Result:=db_7;
    8: Result:=db_8;
  else
    Result:=db_8;
  end;
end;

{---------------------------------------------------------------}
function TModbusMasterIni.GetParity (COMNr: integer): TParityBit;
{---------------------------------------------------------------}
var
  S: string;

begin
  S:=UpperCase (ReadString(GetSectionName(COMNr), CIdentParity, 'NONE'));
  case S[1] of
    'N': Result:=none;
    'O': Result:=odd;
    'E': Result:=even;
    'M': Result:=mark;
    'S': Result:=space;
  else
    Result:=none;
  end;
end;

{----------------------------------------------------------------}
function TModbusMasterIni.GetStopbits (COMNr: integer): TStopBits;
{----------------------------------------------------------------}
var
  S: string;

begin
  S:=ReadString (GetSectionName(COMNr), CIdentStopbits, '1');
  if S = '1' then
    Result:=sb_1
  else if (S = '1.5') OR (S = '1,5') then
    Result:=sb_15
  else if S = '2' then
    Result:=sb_2
  else
    Result:=sb_1;
end;

{---------------------------------------------------------------}
function TModbusMasterIni.GetTOAntwort (COMNr: integer): integer;
{---------------------------------------------------------------}
begin
  Result:=ReadInteger (GetSectionName(COMNr), CIdentTOAntwort, 2000);
end;

{---------------------------------------------------------------}
function TModbusMasterIni.GetTOZeichen (COMNr: integer): integer;
{---------------------------------------------------------------}
begin
  Result:=ReadInteger (GetSectionName(COMNr), CIdentTOZeichen, 100);
end;

{---------------------------------------------------------------}
function TModbusMasterIni.GetIpPort(iCOMNr: integer): integer;
{---------------------------------------------------------------}
begin
  Result := ReadInteger(GetSectionName(iCOMNr), CIdentIPPort, 502);
end;

{---------------------------------------------------------------}
function TModbusMasterIni.GetIpAddress(iCOMNr: integer): string;
{---------------------------------------------------------------}
begin
  Result :=
    ReadString(GetSectionName(iCOMNr), CIdentIpAddress, '0.0.0.0');
end;

{----------------------------------------------------------------------}
function TModbusMasterIni.GetModbusModus (COMNr: integer): TModbusModus;
{----------------------------------------------------------------------}
var
  i : integer;
  s : string;
begin
  if (COMNr >= CComTCP_IP)
  then s := Format (CSectionIPn, [COMNr])
  else s := GetSectionName(COMNr);
  i := ReadInteger(s, CIdentModbusModus, 0);
  case i of
    0: Result:=modbus_RTU;
    1: Result:=modbus_ASCII;
    2: Result := modbus_TCPIP;
  else
    Result:=modbus_RTU;
  end;
end;

{-------------------------------------------------------}
function TModbusMasterIni.GetAusgabeEmpfaengerWM: string;
{-------------------------------------------------------}
begin
  Result:=ReadString (CSectionAusgabe, CIdentEmpfaengerWM, '');
end;

{-------------------------------------------------------}
function TModbusMasterIni.GetAusgabeEmpfaengerHWND: HWND;
{-------------------------------------------------------}
begin
  Result := ReadInteger(CSectionAusgabe, CIdentEmpfaengerHWND, 0);
end;

end.
