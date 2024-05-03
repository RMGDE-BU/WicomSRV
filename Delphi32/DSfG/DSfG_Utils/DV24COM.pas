{------------------------------------------------------------------------------}
{ Objekt für DSfG-V24-Zugriff über COM (TCustomDSfGV24COMObject)               }
{                                                                              }
{ -> Benötigt DSta.DLL (Falls beim Parametrieren Typ nicht übergeben wird)     }
{                                                                              }
{ 07.04.2001  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001                                          }
{------------------------------------------------------------------------------}
unit DV24COM;

interface

uses
  Windows, SysUtils, ComObj, Classes, T_Zeit, PathIni, SrvCfgIni, DStaDll,
  DMomLists, Forms;

type
  TCustomDSfGV24COMObject = class(TObject)
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FCOMObject : OleVariant;
    FOpened    : boolean;
    function GetComPort: SmallInt;
    procedure SetComPort(iPort: SmallInt);
    function GetOpened: boolean;
    function GetTimeOut: integer;
    procedure SetTimeOut(iTimeOut: integer);
    function GetAdresse: char;
    procedure SetAdresse(cAdresse: char);
    function GetStoreTelegrams: boolean;
    procedure SetStoreTelegrams(bState: boolean);
  protected
    procedure InitComponents(aState: boolean); virtual;
  public
    procedure InitConnection(bState: boolean; iStaId: integer = -1);
    function GetTeilnehmer: string;
    function IsTeilnehmer: boolean;
    procedure SendTelegramm(sTelegramm: string); virtual;
    function ReceiveTelegramm: string; virtual;
    function SendReceiveTelegramm(sTelegramm: string): string; virtual;
    function ReadParameter(
      cInstAdr: char; sDeaVon: string; sDeaBis: string = ''): string;
    function WriteParameter(
      cInstAdr: char; sZC1, sZC2, sDea, sWert: string; iType: byte = 0): string;
    function ReadStammdaten: TStrings;
    property TimeOut: integer read GetTimeOut write SetTimeOut;
    property ComPort: SmallInt read GetComPort write SetComPort;
    property Opened: boolean read GetOpened;
    property StoreTelegrams: boolean
      read GetStoreTelegrams write SetStoreTelegrams;
    property Adresse: char read GetAdresse write SetAdresse;
  end;

implementation

const
  C_V24COMObject = 'DSfGV24MomCOM.DSfGV24MomObject';
  C_DFUCOMObject = 'DDfuCOM.DDfuComObject';

  C_ComModeDfu   = 1;
  C_ComModeV24   = 2;

  C_Section_Kommunikation = 'KOMMUNIKATION';
  C_Ident_Dll = 'DLL';

{------------------------- Allgemeine Funktionen ------------------------------}

{ Holt Wieser-Hauptpfad                       }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function GetWieserPath: string;
{---------------------------------------------}
begin
  with TProgramIni.Create do
  try
    Result := ExtractFilePath(WieserIniFile);
  finally
    Free;
  end;
end;

{ Gibt Verbindungsmodus zurück                }
{ Rückgabe: 1-DFÜ; 2-V24                      }
{---------------------------------------------}
function GetMomDllModus: byte;
{---------------------------------------------}
begin
  with TProgramIni.Create do
  try
    Result := ReadInteger(C_Section_Kommunikation, C_Ident_Dll, C_ComModeDfu);
  finally
    Free;
  end;
end;

{ Gibt Dateinamen des COM-Servers zurück      }
{---------------------------------------------}
function GetComName: string;
{---------------------------------------------}
begin
  case GetMomDllModus of
    1: Result := C_DFUCOMObject;
    2: Result := C_V24COMObject;
    else Result := C_DFUCOMObject;
  end;
end;

{ Holt COM aus SrvCfg.INI (DSfG-Allgemein)}
{ Rückgabe: COM-Nummer (Default: 1)       }
{-----------------------------------------}
function GetCom: byte;
{-----------------------------------------}
var
  i : byte;
begin
  Result := 1;  // Default

  with TSrvCfg32Ini.Create(ExtractFilePath(ParamStr(0))) do
  try
    for i := 1 to MaxCOMName do
      if (Thema[i] = Themen[thDSfG]) and (Modem[i] = '(Allgemein)') then begin
        Result := i;
        Break;
      end;
  finally
  end;
end;

{-------------------------- TCustomDSfGV24COMObject ---------------------------}

{------------------------------------}
constructor TCustomDSfGV24COMObject.Create;
{------------------------------------}
begin
  inherited Create;

  FCOMObject := CreateOleObject(GetComName);

  if (GetMomDllModus = C_ComModeV24) then begin
    FComObject.ComPort := GetCom;
  end;
  Delay(100);
  InitComponents(True);
end;

{------------------------------------}
destructor TCustomDSfGV24COMObject.Destroy;
{------------------------------------}
begin
  InitConnection(False);
  Delay(1000);
  InitComponents(False);

  inherited Destroy;
end;

{------------------------------------}
procedure TCustomDSfGV24COMObject.InitComponents(aState: boolean);
{------------------------------------}
begin
end;

{------------------------------------}
function TCustomDSfGV24COMObject.GetComPort: SmallInt;
{------------------------------------}
begin
  Result := FCOMObject.ComPort;
end;

{------------------------------------}
procedure TCustomDSfGV24COMObject.SetComPort(iPort: SmallInt);
{------------------------------------}
begin
  FCOMObject.ComPort := iPort;
end;

{------------------------------------}
function TCustomDSfGV24COMObject.GetOpened: boolean;
{------------------------------------}
begin
  Result := FCOMObject.Opened;
end;

{------------------------------------}
function TCustomDSfGV24COMObject.GetTimeOut: integer;
{------------------------------------}
begin
  Result := FCOMObject.TimeOut;
end;

{------------------------------------}
procedure TCustomDSfGV24COMObject.SetTimeOut(iTimeOut: integer);
{------------------------------------}
begin
  FCOMObject.TimeOut := iTimeOut;
end;

{------------------------------------}
function TCustomDSfGV24COMObject.GetAdresse: char;
{------------------------------------}
var
  s : string;
begin
  s := FCOMObject.Adresse;
  if (Length(s) > 0) then Result := s[1] else Result := '0';
end;

{------------------------------------}
procedure TCustomDSfGV24COMObject.SetAdresse(cAdresse: char);
{------------------------------------}
begin
  FCOMObject.Adresse := cAdresse;
end;

{------------------------------------}
procedure TCustomDSfGV24COMObject.InitConnection(
  bState: boolean; iStaId: integer = -1);
{------------------------------------}
begin
  FCOMObject.InitConnection(bState, iStaId);
  FOpened := bState;
end;

{------------------------------------}
procedure TCustomDSfGV24COMObject.SendTelegramm(sTelegramm: string);
{------------------------------------}
begin
  FCOMObject.SendTelegramm(sTelegramm);
end;

{------------------------------------}
function TCustomDSfGV24COMObject.ReceiveTelegramm: string;
{------------------------------------}
begin
  Result := FCOMObject.ReceiveTelegramm;
end;

{------------------------------------}
function TCustomDSfGV24COMObject.GetTeilnehmer: string;
{------------------------------------}
begin
  Result := FCOMObject.GetTeilnehmer;
end;

{------------------------------------}
function TCustomDSfGV24COMObject.IsTeilnehmer: boolean;
{------------------------------------}
begin
  Result := FCOMObject.IsTeilnehmer;
end;

{------------------------------------}
function TCustomDSfGV24COMObject.SendReceiveTelegramm(sTelegramm: string): string;
{------------------------------------}
begin
  Result := FCOMObject.SendReceiveTelegramm(sTelegramm);
end;

{-----------------------------------------}
function TCustomDSfGV24COMObject.ReadParameter(
  cInstAdr: char; sDeaVon: string; sDeaBis: string = ''): string;
{-----------------------------------------}
begin
  if (sDeaBis = '')
  then Result := FCOMObject.ReadParameter(cInstAdr, sDeaVon, sDeaBis)
  else begin
    StoreTelegrams := True;
    FCOMObject.ReadParameter(cInstAdr, sDeaVon, sDeaBis);
    while (FOpened) and (FCOMObject.StoreTelegrams) do
      Delay(100);
    if (FOpened) then Result := FCOMObject.ReceiveTelegramm else Result := '';
  end;
end;

{-----------------------------------------}
function TCustomDSfGV24COMObject.WriteParameter(
  cInstAdr: char; sZC1, sZC2, sDea, sWert: string; iType: byte = 0): string;
{-----------------------------------------}
var
  pRec : PDDEARec;
begin
  // Ggf. DEA-Typ einstellen
  if (iType = 0) then begin
    pRec := ClientStammdaten.DeaList.GetRecord(sDea);
    if (Assigned(pRec)) then iType := StrToIntDef(pRec^.DEATyp, 0);
  end;

  Result := FCOMObject.WriteParameter(
      cInstAdr, sZC1, sZC2, sDea, sWert, iType);
end;

{-----------------------------------------}
function TCustomDSfGV24COMObject.ReadStammdaten: TStrings;
{-----------------------------------------}
var
  s : string;
begin
  Result := TStringList.Create;
  StoreTelegrams := True;
  FCOMObject.ReadStammdaten;
  while (FOpened) and (FCOMObject.StoreTelegrams) do Delay(100);
  if (FOpened) then begin
    s := FCOMObject.ReceiveTelegramm;
    while (s <> '') do begin
      Result.Add(s);
      s := FCOMObject.ReceiveTelegramm;
    end;
  end;
end;

{ Holt Status, ob Telegr. gesp. werden    }
{ Rückgabe: Status                        }
{-----------------------------------------}
function TCustomDSfGV24COMObject.GetStoreTelegrams: boolean;
{-----------------------------------------}
begin
  Result := FCOMObject.StoreTelegrams;
end;

{ Setzt Status, ob Telegr. gesp. werden   }
{ Parameter: Status                       }
{-----------------------------------------}
procedure TCustomDSfGV24COMObject.SetStoreTelegrams(bState: boolean);
{-----------------------------------------}
begin
  FCOMObject.StoreTelegrams := bState;
end;

end.

