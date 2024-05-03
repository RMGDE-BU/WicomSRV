{******************************************************************************}
{* Unit: Zugriffe auf SRVCFG32.INI                                            *}
{*       enthält alle Schnittstellen-Einstellungen für das Abrufsystem        *}
{* Version: 28.11.2000   WW                                                   *}
{******************************************************************************}
unit SrvCfgIni;

interface

uses
  Classes, IniFiles, SysUtils;

const
  { unterstützte physikalische serielle Schnittstellen }
  MaxCOMName = 16;
  COMName : array [0..MaxCOMName-1] of string =
    ('COM1',  'COM2',  'COM3',  'COM4',  'COM5',  'COM6',  'COM7',  'COM8',
     'COM9',  'COM10', 'COM11', 'COM12', 'COM13', 'COM14', 'COM15', 'COM16');

  { Themen für serielle Schnittstellen }
  maxThemen = 4;
  Themen: array [1..maxThemen] of string = ('MRG-FUP', 'MRG-Modem', 'DSfG', 'LAKS');

  thMRGFUP   = 1;
  thMRGModem = 2;
  thDSfG     = 3;
  thLAKS     = 4;

  { weitere Themen }
  sthTCP_IP = 'TCP/IP';     { Kommunikation über TCP/IP }

  { Gerätegruppen für MRG-Modem-Rufentgegennahme }
  mr_Modem = 0;
  mr_FUP   = 1;

  { Einstellwerte im Ini-File }
  CUnbenutzt = 'unbenutzt';
  CKeinModem = 'LAKS seriell';

  { Zuordnung logische -> physikalische Schnittstellen
    Index: logische; Wert: physikalische (Vorbelegung: Sn = COMn) }
  LogPhysComZuordnung: array [1..MaxCOMName] of integer =
    (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);

  CInitTrenner = ';';     { Trennzeichen zwischen den Initialisierungsstrings }

type


  { Objekt für SRVCFG32.INI }

  TSrvCfg32Ini = class (TIniFile)
  protected
    function GetThema(Com: integer): string;
    procedure SetThema(Com: integer; Value: string);
    function GetFupInit(Com: integer): string;
    procedure SetFupInit(Com: integer; Value: string);
    function GetFup1200(Com: integer): string;
    procedure SetFup1200(Com: integer; Value: string);
    function GetFup2400(Com: integer): string;
    procedure SetFup2400(Com: integer; Value: string);
    function GetModem(Com: integer): string;
    procedure SetModem(Com: integer; Value: string);
    function GetLAKSModem(Com: integer): string;
    procedure SetLAKSModem(Com: integer; Value: string);
    function GetRingCount(Com: integer): integer;
    procedure SetRingCount(Com: integer; Value: integer);
    function GetModemRuf(Com: integer): integer;
    procedure SetModemRuf(Com: integer; Value: integer);
    function GetLogSchnittstelle(i: integer): string;
    procedure SetLogSchnittstelle(i: integer; Value: string);
    function GetProtokoll: boolean;
    procedure SetProtokoll(Value: boolean);
    function GetTCP_IP_DSfG: boolean;
    procedure SetTCP_IP_DSfG(Value: boolean);
  public
    constructor Create(Path: TFilename);
    property Thema[Com: integer]: string read GetThema write SetThema;
    property FupInit[Com: integer]: string read GetFupInit write SetFupInit;
    property Fup1200[Com: integer]: string read GetFup1200 write SetFup1200;
    property Fup2400[Com: integer]: string read GetFup2400 write SetFup2400;
    property Modem[Com: integer]: string read GetModem write SetModem;
    property LAKSModem[Com: integer]: string read GetLAKSModem write SetLAKSModem;
    property RingCount[Com: integer]: integer read GetRingcount write SetRingcount;
    property ModemRuf[Com: integer]: integer read GetModemRuf write SetModemRuf;
    property LogSchnittstelle[i: integer]: string read GetLogSchnittstelle
                                               write SetLogSchnittstelle;
    property Protokoll: boolean read GetProtokoll write SetProtokoll;
    property TCP_IP_DSfG: boolean read GetTCP_IP_DSfG write SetTCP_IP_DSfG;
    procedure ReadLogPhysCom_Zuordnung;
  end;

implementation

Const
  CSrvCfg32Ini = 'SRVCFG32.INI';

  { Sections }
  CSectionSchnittstelle = 'Schnittstelle';
  CSectionZuordnung     = 'Zuordnung';
  CSectionTCP_IP        = 'TCP/IP';

  { Idents }
  CIdentThema       = 'Thema';
  CIdentFupInit     = 'FupInit';
  CIdentFup1200     = 'Fup1200';
  CIdentFup2400     = 'Fup2400';
  CIdentModem       = 'Modem';
  CIdentRingCount   = 'RingCount';
  CIdentProtokoll   = 'Protokoll';
  CIdentMRGModemRuf = 'MRGModemRuf';
  CIdentDSfG        = 'DSfG';

  { Einstellwerte }
  CFupInit = 'D99'+CInitTrenner+'I10'+CInitTrenner+'S2';
  CFup1200 = 'S2';
  CFup2400 = 'S6';
  CModemAllgemein = '(Allgemein)';
  CEin = 'Ein';
  CAus = 'Aus';

  { logische Schnittstellen }
  KeyNameZuordnung: array[1..MaxCOMName] of string =
    ('S1',  'S2',  'S3',  'S4',  'S5',  'S6',  'S7',  'S8',
     'S9',  'S10', 'S11', 'S12', 'S13', 'S14', 'S15', 'S16');


{ TSrvCfg32Ini }

{-----------------------------------------------}
constructor TSrvCfg32Ini.Create(Path: TFilename);
{-----------------------------------------------}
begin
  inherited Create(Path + CSrvCfg32Ini);
end;

{------------------------------------------------}
function TSrvCfg32Ini.GetThema(Com: integer): string;
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    Result:= ReadString(ComName[Com-1], CIdentThema, CUnbenutzt)
  else
    Result:= CUnbenutzt;
end;

{------------------------------------------------}
procedure TSrvCfg32Ini.SetThema(Com: integer; Value: string);
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    WriteString(ComName[Com-1], CIdentThema, Value);
end;

{------------------------------------------------}
function TSrvCfg32Ini.GetFupInit(Com: integer): string;
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    Result:= ReadString(ComName[Com-1], CIdentFupInit, CFupInit)
  else
    Result:= CFupInit;
end;

{------------------------------------------------}
procedure TSrvCfg32Ini.SetFupInit(Com: integer; Value: string);
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    WriteString(ComName[Com-1], CIdentFupInit, Value);
end;

{------------------------------------------------}
function TSrvCfg32Ini.GetFup1200(Com: integer): string;
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    Result:= ReadString(ComName[Com-1], CIdentFup1200, CFup1200)
  else
    Result:= CFup1200;
end;

{------------------------------------------------}
procedure TSrvCfg32Ini.SetFup1200(Com: integer; Value: string);
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    WriteString(ComName[Com-1], CIdentFup1200, Value);
end;

{------------------------------------------------}
function TSrvCfg32Ini.GetFup2400(Com: integer): string;
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    Result:= ReadString(ComName[Com-1], CIdentFup2400, CFup2400)
  else
    Result:= CFup2400;
end;

{------------------------------------------------}
procedure TSrvCfg32Ini.SetFup2400(Com: integer; Value: string);
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    WriteString(ComName[Com-1], CIdentFup2400, Value);
end;

{------------------------------------------------}
function TSrvCfg32Ini.GetModem(Com: integer): string;
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    Result:= ReadString(ComName[Com-1], CIdentModem, CModemAllgemein)
  else
    Result:= CModemAllgemein;
end;

{------------------------------------------------}
procedure TSrvCfg32Ini.SetModem(Com: integer; Value: string);
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    WriteString(ComName[Com-1], CIdentModem, Value);
end;

{------------------------------------------------}
function TSrvCfg32Ini.GetLAKSModem(Com: integer): string;
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then begin
    Result:= ReadString(ComName[Com-1], CIdentModem, CKeinModem);
    if Result = '' then
      Result:=CKeinModem;
  end else
    Result:= CKeinModem;
end;

{------------------------------------------------}
procedure TSrvCfg32Ini.SetLAKSModem(Com: integer; Value: string);
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    WriteString(ComName[Com-1], CIdentModem, Value);
end;

{------------------------------------------------}
function TSrvCfg32Ini.GetRingCount(Com: integer): integer;
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    Result:= ReadInteger(ComName[Com-1], CIdentRingCount, 1)
  else
    Result:= 1;
end;

{------------------------------------------------}
procedure TSrvCfg32Ini.SetRingCount(Com: integer; Value: integer);
{------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    WriteInteger(ComName[Com-1], CIdentRingCount, Value);
end;

{----------------------------------------------------}
function TSrvCfg32Ini.GetModemRuf(Com: integer): integer;
{----------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    Result:= ReadInteger(ComName[Com-1], CIdentMRGModemRuf, mr_Modem)
  else
    Result:= 1;
end;

{------------------------------------------------------------}
procedure TSrvCfg32Ini.SetModemRuf(Com: integer; Value: integer);
{------------------------------------------------------------}
begin
  if (Com >=1) AND (Com <= MaxComName) then
    WriteInteger(ComName[Com-1], CIdentMRGModemRuf, Value);
end;

{---------------------------------------------------------}
function TSrvCfg32Ini.GetLogSchnittstelle(i: integer): string;
{---------------------------------------------------------}
begin
  if (i >= 1) AND (i <= MaxCOMName) then
    Result:= ReadString (CSectionZuordnung, KeyNameZuordnung[i], COMName[i-1])
  else
    Result:=COMName[0];
end;

{------------------------------------------------}
procedure TSrvCfg32Ini.SetLogSchnittstelle(i: integer; Value: string);
{------------------------------------------------}
begin
  if (i >= Low(KeyNameZuordnung)) AND (i <= High(KeyNameZuordnung)) then
    WriteString(CSectionZuordnung, KeyNameZuordnung[i], Value);
end;

{------------------------------------------}
function TSrvCfg32Ini.GetProtokoll: boolean;
{------------------------------------------}
begin
  if ReadString (CSectionSchnittstelle, CIdentProtokoll, CAus) = CEin then
    Result:= true
  else
    Result:= false;
end;

{------------------------------------------------}
procedure TSrvCfg32Ini.SetProtokoll(Value: boolean);
{------------------------------------------------}
begin
  if Value then
    WriteString(CSectionSchnittstelle, CIdentProtokoll, CEin)
  else
    WriteString(CSectionSchnittstelle, CIdentProtokoll, CAus);
end;

{--------------------------------------------}
function TSrvCfg32Ini.GetTCP_IP_DSfG: boolean;
{--------------------------------------------}
begin
  if ReadString (CSectionTCP_IP, CIdentDSfG, CAus) = CEin then
    Result:= true
  else
    Result:= false;
end;

{----------------------------------------------------}
procedure TSrvCfg32Ini.SetTCP_IP_DSfG(Value: boolean);
{----------------------------------------------------}
begin
  if Value then
    WriteString(CSectionTCP_IP, CIdentDSfG, CEin)
  else
    WriteString(CSectionTCP_IP, CIdentDSfG, CAus);
end;

{----------------------------------------------}
procedure TSrvCfg32Ini.ReadLogPhysCom_Zuordnung;
{----------------------------------------------}
{ Schnittstellenzuordnungen "logisch" -> "physikalisch"  lesen und
  nach LogPhysComZuordnung (global) schreiben }
var
  S: string;
  l, p: integer;

begin
  for l:=Low (KeyNameZuordnung) to High (KeyNameZuordnung) do begin
    S:=GetLogSchnittstelle (l);
    for p:=Low (COMName) to High (COMName) do begin
      if UpperCase(S) = COMName[p] then begin
        LogPhysComZuordnung[l]:=p + 1;
        Break;
      end;
    end;
  end;
end;

end.
