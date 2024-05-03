{------------------------------------------------------------------------------}
{ Objekt für DSfG-V24-Zugriff über COM (TCustomDSfGCOMObject)                  }
{                                                                              }
{ -> Benötigt DSta.DLL (Falls beim Parametrieren Typ nicht übergeben wird)     }
{                                                                              }
{ 07.04.2001  GD  Neu                                                          }
{ 03.02.2002  GD  Erweitert                                                    }
{ 11.04.2002  GD  Änderungen für Archivabruf, Schnittstelle für V24            }
{ 11.04.2002  GD  Änderungen für Archivabruf                                   }
{ 16.07.2008  GD  Prüfen des COM-Objekts auf Ansprechbarkeit                   }
{ 29.11.2013  WW  mit Aufruf COM-Objekt-Eigenschaft 'OnlyLAN' bei ComModeWICOM }
{ 10.06.2014  WW  Logging in InitMomConnection und InitDFUConnection           }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2001, RMG Messtechnik GmbH 2013               }
{------------------------------------------------------------------------------}
unit DConnectionCOM;

interface

uses
  Windows, SysUtils, ComObj, Classes, Forms,
  T_Zeit, PathIni, SrvCfgIni, DStaDll, DMomLists, GD_Utils;

const
  C_ComModeWICOM = 1;
  C_ComModeV24   = 2;
  C_ComModeCard  = 3;
  C_ComModeDfu   = 4;

  C_Section_Kommunikation = 'KOMMUNIKATION';
  C_Ident_Dll             = 'DLL';
  C_Ident_V24Port         = 'V24PORT';
  C_Ident_DpaPort     = 'DPAPORT';
  C_Ident_DpaAdr      = 'DPAADR';
  C_Ident_DpaBdRate   = 'DPABDRATE';

type
  TCustomDSfGCOMObject = class(TObject)
    constructor Create (bOnlyLAN: boolean = false);
    destructor Destroy; override;
  private
    FCOMObject  : OleVariant;
    FOpened     : boolean;
    FStationsId : integer;
    function GetComPort: SmallInt;
    procedure SetComPort(iPort: SmallInt);
    function GetOpened: boolean;
    function GetBusy: boolean;
    function GetTimeOut: integer;
    procedure SetTimeOut(iTimeOut: integer);
    function GetAdresse: char;
    procedure SetAdresse(cAdresse: char);
    function GetStoreTelegrams: boolean;
    procedure SetStoreTelegrams(bState: boolean);
  protected
    FOnlyLAN    : boolean;  // 29.11.2013, WW
    procedure InitComponents(aState: boolean); virtual;
    procedure InitSpecialComOptions; virtual;
    property COMObject : OleVariant read FCOMObject;
  public
    function StartConnectionTime: integer;
    procedure InitConnection(bState: boolean; iStaId: integer = -1);
    function GetLastAbrufResult: boolean;
    function InitAutoDatenAbruf(iStaId: integer = -1): boolean;
    function InitManuDatenAbruf(iStaId: integer; pSlKanaele: TStrings;
      dtVon, dtBis: TDateTime; bShow: boolean): boolean;
    procedure InitMomConnection(bState: boolean; iStaId: integer = -1);
    procedure InitDFUConnection(bState: boolean; iStaId: integer = -1);
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
    function GetAbrufStatus: boolean;
    property TimeOut: integer read GetTimeOut write SetTimeOut;
    property ComPort: SmallInt read GetComPort write SetComPort;
    property Opened: boolean read GetOpened;
    property Busy: boolean read GetBusy;
    property StoreTelegrams: boolean
      read GetStoreTelegrams write SetStoreTelegrams;
    property Adresse: char read GetAdresse write SetAdresse;
    property StationsId: integer read FStationsId;
  end;

function GetMomDllModus: byte;

implementation

const
  C_V24COMObject = 'DSfGV24MomCOM.DSfGV24MomObject';
  C_DFUCOMObject = 'DDfuCOM.DDfuComObject';
  C_WICOMCOMObject = 'DWICOMCOM.DWICOMComObject';
  C_CardCOMObject = 'DSfGCSvr.DSfGCardInterface';

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
    Result := ReadInteger(C_Section_Kommunikation, C_Ident_Dll, C_ComModeWICOM);
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
    C_ComModeWICOM: Result := C_WICOMCOMObject;
    C_ComModeV24: Result := C_V24COMObject;
    C_ComModeCard: Result := C_CardCOMObject;
    C_ComModeDfu: Result := C_DFUCOMObject;
    else Result := C_DFUCOMObject;
  end;
end;

{ Holt COM aus INI-Datei                  }
{ Rückgabe: COM-Nummer (Default: 1)       }
{-----------------------------------------}
function GetCom: byte;
{-----------------------------------------}
var
  sIdent : string;
begin
  if (GetMomDllModus = C_ComModeV24) then sIdent := C_Ident_V24Port
  else if (GetMomDllModus = C_ComModeCard) then sIdent := C_Ident_DpaPort
  else begin
    Result := 1;
    Exit;
  end;

  with TProgramIni.Create do  // 11.04.2002
  try
    Result := ReadInteger(C_Section_Kommunikation, sIdent, 1);
  finally
    Free;
  end;
end;

{ Holt DSfG-Baudrate aus INI-Datei        }
{ Rückgabe: DSfG-Baudrate (Default: 9600) }
{-----------------------------------------}
function GetDSfGBaudRate: integer;
{-----------------------------------------}
var
  sIdent : string;
begin
  if (GetMomDllModus = C_ComModeCard) then sIdent := C_Ident_DpaBdRate
  else begin
    Result := 9600;
    Exit;
  end;

  with TProgramIni.Create do  // 11.04.2002
  try
    Result := ReadInteger(C_Section_Kommunikation, sIdent, 9600);
  finally
    Free;
  end;
end;

{ Holt DSfG-Busadresse aus INI-Datei       }
{ Rückgabe: DSfG-Busadresse (Default: "0") }
{-----------------------------------------}
function GetDSfGAdresse: char;
{-----------------------------------------}
var
  s, sIdent : string;
begin
  if (GetMomDllModus = C_ComModeCard) then sIdent := C_Ident_DpaAdr
  else begin
    Result := '0';
    Exit;
  end;

  with TProgramIni.Create do  // 11.04.2002
  try
    s := Trim(ReadString(C_Section_Kommunikation, sIdent, '0'));
    if (s <> '') then Result := s[1] else Result := '0';
  finally
    Free;
  end;
end;

{-------------------------- TCustomDSfGCOMObject ---------------------------}

{ Parameter: Default-Kommunikation über Netzwerk und Modem }
{------------------------------------}
constructor TCustomDSfGCOMObject.Create (bOnlyLAN: boolean = false);
{------------------------------------}
var
  i : integer;
begin
  inherited Create;

  FOnlyLAN:=bOnlyLAN;  // 29.11.2013, WW
  FCOMObject := CreateOleObject(GetComName);

  // Prüfen, ob COM-Object ansprechbar ist  // 16.07.2008
  try
    for i := 1 to 5 do
    try
      if (not FCOMObject.Opened) then Delay(10);
      Break;
    except
      Delay(i*100);
    end;
  except
    Delay(5000);
  end;

  Delay(100);
  InitSpecialComOptions;  // Aufruf NACH COM-Objekt-Prüfung; 29.11.2013, WW
  InitComponents(True);
end;

{------------------------------------}
destructor TCustomDSfGCOMObject.Destroy;
{------------------------------------}
begin
  InitConnection(False);
  Delay(1000);
  InitComponents(False);

  inherited Destroy;
end;

{ Durchführen spezifischer Aktionen  }
{------------------------------------}
procedure TCustomDSfGCOMObject.InitSpecialComOptions;
{------------------------------------}
begin
  if (GetMomDllModus = C_ComModeV24) then begin
    FComObject.ComPort := GetCom;
  end
  else if (GetMomDllModus = C_ComModeCard) then begin
    FComObject.ComPort := GetCom;
    FCOMObject.BaudRate := GetDSfGBaudRate;
    FCOMObject.Adresse := GetDSfGAdresse;
  end
  else if (GetMomDllModus = C_ComModeWICOM) then begin
    FCOMObject.OnlyLAN := FOnlyLAN;  // 29.11.2013, WW
  end;
end;

{------------------------------------}
procedure TCustomDSfGCOMObject.InitComponents(aState: boolean);
{------------------------------------}
begin
end;

{------------------------------------}
function TCustomDSfGCOMObject.GetComPort: SmallInt;
{------------------------------------}
begin
  Result := FCOMObject.ComPort;
end;

{------------------------------------}
procedure TCustomDSfGCOMObject.SetComPort(iPort: SmallInt);
{------------------------------------}
begin
  FCOMObject.ComPort := iPort;
end;

{------------------------------------}
function TCustomDSfGCOMObject.GetOpened: boolean;
{------------------------------------}
begin
  try
    Result := FCOMObject.Opened;
  except
    Result := False;
  end;
end;

{------------------------------------}
function TCustomDSfGCOMObject.GetBusy: boolean;
{------------------------------------}
begin
  try
    Result := FCOMObject.Busy;
  except
    Result := False;
  end;
end;

{------------------------------------}
function TCustomDSfGCOMObject.GetTimeOut: integer;
{------------------------------------}
begin
  Result := FCOMObject.TimeOut;
end;

{------------------------------------}
procedure TCustomDSfGCOMObject.SetTimeOut(iTimeOut: integer);
{------------------------------------}
begin
  FCOMObject.TimeOut := iTimeOut;
end;

{------------------------------------}
function TCustomDSfGCOMObject.GetAdresse: char;
{------------------------------------}
var
  s : string;
begin
  s := FCOMObject.Adresse;
  if (Length(s) > 0) then Result := s[1] else Result := '0';
end;

{------------------------------------}
procedure TCustomDSfGCOMObject.SetAdresse(cAdresse: char);
{------------------------------------}
begin
  FCOMObject.Adresse := cAdresse;
end;

{ Max. Zeit für Verbindungsaufabu    }
{ Rückgabe: Max. Zeit in millisec.   }
{------------------------------------}
function TCustomDSfGCOMObject.StartConnectionTime: integer;
{------------------------------------}
begin
  try
    Result := FCOMObject.StartConnectionTime;
  except
    Result := 5000;  // Default, damit die Systeme etwas machen können ...
  end;
end;

{------------------------------------}
procedure TCustomDSfGCOMObject.InitConnection(
  bState: boolean; iStaId: integer = -1);
{------------------------------------}
begin
  FOpened := False; //  Default

  try
    if (bState) and (iStaId > 0) and
       (GetMomDllModus in [C_ComModeWICOM, C_ComModeDfu]) then
    begin
      with ClientStammdaten.GetRufstammdaten(iStaId) do begin
        if (Busadresse[1] in ['A'..'_']) then Self.Adresse := Busadresse[1];
      end;
    end;

    FCOMObject.InitConnection(bState, iStaId);
    FOpened := Opened;
    if (FOpened) then FStationsId := iStaId else FStationsId := -1;
  except
    // FOpened setzt internen Status (Externer Status abfragbar über Opened)
  end;
end;

{------------------------------------}
function TCustomDSfGCOMObject.GetLastAbrufResult: boolean;
{------------------------------------}
begin
  Result := FCOMObject.AbrufStatus;
end;

{------------------------------------}
function TCustomDSfGCOMObject.InitAutoDatenAbruf(iStaId: integer = -1): boolean;
{------------------------------------}
begin
  Result := FCOMObject.InitAutoDatenAbruf(iStaId);
end;

{------------------------------------}
function TCustomDSfGCOMObject.InitManuDatenAbruf(iStaId: integer;
  pSlKanaele: TStrings; dtVon, dtBis: TDateTime; bShow: boolean): boolean;
{------------------------------------}
begin
  Result := FCOMObject.InitManuDatenAbruf(
    iStaId, pSlKanaele.CommaText, dtVon, dtBis, bShow);
end;

{------------------------------------}
procedure TCustomDSfGCOMObject.InitMomConnection(
  bState: boolean; iStaId: integer = -1);
{------------------------------------}
begin
  FOpened := False; //  Default

  try
    if (bState) and (iStaId > 0) and
       (GetMomDllModus in [C_ComModeWICOM, C_ComModeDfu]) then
    begin
      with ClientStammdaten.GetRufstammdaten(iStaId) do begin
        if (Busadresse[1] in ['A'..'_']) then Self.Adresse := Busadresse[1];
      end;
    end;

    FCOMObject.InitMomConnection(bState, iStaId);
    FOpened := Opened;
    if (FOpened) then FStationsId := iStaId else FStationsId := -1;
  except
    // FOpened setzt internen Status (Externer Status abfragbar über Opened)
    on E: Exception do
      WriteErrorLog ('TCustomDSfGCOMObject.InitMomConnection Exception: ' +
        E.Message, ChangeFileExt (ParamStr(0), '.log'));
  end;
end;

{------------------------------------}
procedure TCustomDSfGCOMObject.InitDFUConnection(
  bState: boolean; iStaId: integer = -1);
{------------------------------------}
begin
  FOpened := False; //  Default

  try
    FCOMObject.InitDFUConnection(bState, iStaId);
    FOpened := Opened;
  except
    // FOpened setzt internen Status (Externer Status abfragbar über Opened)
    on E: Exception do
      WriteErrorLog ('TCustomDSfGCOMObject.InitDFUConnection Exception: ' +
        E.Message, ChangeFileExt (ParamStr(0), '.log'));
  end;
end;

{------------------------------------}
procedure TCustomDSfGCOMObject.SendTelegramm(sTelegramm: string);
{------------------------------------}
begin
  FCOMObject.SendTelegramm(sTelegramm);
end;

{------------------------------------}
function TCustomDSfGCOMObject.ReceiveTelegramm: string;
{------------------------------------}
begin
  Result := FCOMObject.ReceiveTelegramm;
end;

{------------------------------------}
function TCustomDSfGCOMObject.GetTeilnehmer: string;
{------------------------------------}
begin
  Result := FCOMObject.GetTeilnehmer;
end;

{------------------------------------}
function TCustomDSfGCOMObject.IsTeilnehmer: boolean;
{------------------------------------}
begin
  Result := FCOMObject.IsTeilnehmer;
end;

{------------------------------------}
function TCustomDSfGCOMObject.SendReceiveTelegramm(sTelegramm: string): string;
{------------------------------------}
begin
  Result := FCOMObject.SendReceiveTelegramm(sTelegramm);
end;

{-----------------------------------------}
function TCustomDSfGCOMObject.ReadParameter(
  cInstAdr: char; sDeaVon: string; sDeaBis: string = ''): string;
{-----------------------------------------}
begin
  StoreTelegrams := True;
  FCOMObject.ReadParameter(cInstAdr, sDeaVon, sDeaBis);
  while (FOpened) and (FCOMObject.StoreTelegrams) do
    Delay(100);
  if (FOpened) then Result := FCOMObject.ReceiveTelegramm else Result := '';
end;

{-----------------------------------------}
function TCustomDSfGCOMObject.WriteParameter(
  cInstAdr: char; sZC1, sZC2, sDea, sWert: string; iType: byte = 0): string;
{-----------------------------------------}
var
  pRec : PDDEARec;
begin
  // Ggf. DEA-Typ einstellen
  if (iType = 0) then begin
    pRec := ClientStammdaten.GetDeaDefRecord(sDea, iType);
    if (Assigned(pRec)) then iType := StrToIntDef(pRec^.DEATyp, 0);
  end;

  Result := FCOMObject.WriteParameter(
      cInstAdr, sZC1, sZC2, sDea, sWert, iType);
end;

{-----------------------------------------}
function TCustomDSfGCOMObject.ReadStammdaten: TStrings;
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
function TCustomDSfGCOMObject.GetStoreTelegrams: boolean;
{-----------------------------------------}
begin
  Result := FCOMObject.StoreTelegrams;
end;

{ Setzt Status, ob Telegr. gesp. werden   }
{ Parameter: Status                       }
{-----------------------------------------}
procedure TCustomDSfGCOMObject.SetStoreTelegrams(bState: boolean);
{-----------------------------------------}
begin
  FCOMObject.StoreTelegrams := bState;
end;

{ Holt Status, ob Abruf z.Zt. läuft       }
{ Rückgabe: Status                        }
{-----------------------------------------}
function TCustomDSfGCOMObject.GetAbrufStatus: boolean;
{-----------------------------------------}
begin
  Result := FCOMObject.GetAbrufStatus;
end;

end.

