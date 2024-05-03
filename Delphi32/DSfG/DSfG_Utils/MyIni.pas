{------------------------------------------------------------------------------}
{ INI-Einstellungen (Registry) für DSfG-ActiveX-Komponenten                    }
{                                                                              }
{ 13.07.2000  GD    Neu                                                        }
{ 24.07.2000  GD    Erweiterung um Zugangscode2 für Parametrierung             }
{ 07.08.2000  GD    Erweiterung um Instanzadresse der DSfGCard                 }
{ 31.10.2000  GD    Erweiterung um Zugangscode1, Datenbanknamen und Pfade      }
{ 27.11.2000  GD    Erweiterung um Einstellung der Busadresse                  }
{ 11.01.2001  GD    Erweiterung um Datum für Testversion WIVER32               }
{ 21.03.2002  GD    Kartenadresse: Default = 0                                 }
{ 04.01.2005  GD    Logfile                                                    }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, 2005                                    }
{------------------------------------------------------------------------------}
unit MyIni;

interface

uses
  Windows, Registry, SysUtils, GD_Utils;

const
  C_Wieser_Color = $000080FF;
  C_Wieser_AX_Caption = 'Wieser DSfGCard';
  C_Ax_ClassName      = 'DSfGCardX';

type
  TDSfGActiveXIniFile = class(TRegistry)
    constructor Create;
  private
    function GetIsCardParameters: boolean;
    procedure SetIsCardParameters(Value: boolean);
    function GetIsTeilnehmerListe: boolean;
    procedure SetIsTeilnehmerListe(Value: boolean);
    function GetIsFehlerListe: boolean;
    procedure SetIsFehlerListe(Value: boolean);
    function GetIsInstanzTypen: boolean;
    procedure SetIsInstanzTypen(Value: boolean);
    function GetIsGeraeteTypen: boolean;
    procedure SetIsGeraeteTypen(Value: boolean);
    function GetIsDSfGTelegramme: boolean;
    procedure SetIsDSfGTelegramme(Value: boolean);
    function GetBaudRate: integer;
    procedure SetBaudRate(Value: integer);
    function GetTimeOut: integer;
    procedure SetTimeOut(Value: integer);
    function GetTelegrammFilename: TFileName;
    procedure SetTelegrammFilename(Value: TFileName);
    function GetDSfGTelegramm: string;
    procedure SetDSfGTelegramm(Value: string);
    function GetZugangsCode1: string;
    procedure SetZugangsCode1(Value: string);
    function GetZugangsCode2: string;
    procedure SetZugangsCode2(Value: string);
    function GetInstAdr: char;
    procedure SetInstAdr(Value: char);
    function GetStammDb: string;
    procedure SetStammDb(Value: string);
    function GetWorkDb: string;
    procedure SetWorkDb(Value: string);
    function GetStammDir: string;
    procedure SetStammDir(Value: string);
    function GetWorkDir: string;
    procedure SetWorkDir(Value: string);
    function GetCardAddress: char;
    procedure SetCardAddress(Value: char);
    function GetWiverDatum: TDateTime;
    procedure SetWiverDatum(Value: TDateTime);
    function GetComPort: byte;
    procedure SetComPort(iPort: byte); 
    function GetLogFilename: TFileName;
    procedure SetLogFilename(Value: TFileName);
  protected
  public
    property IsCardParameters: boolean
      read GetIsCardParameters write SetIsCardParameters;
    property IsTeilnehmerListe: boolean
      read GetIsTeilnehmerListe write SetIsTeilnehmerListe;
    property IsFehlerListe: boolean
      read GetIsFehlerListe write SetIsFehlerListe;
    property IsInstanzTypen: boolean
      read GetIsInstanzTypen write SetIsInstanzTypen;
    property IsGeraeteTypen: boolean
      read GetIsGeraeteTypen write SetIsGeraeteTypen;
    property IsDSfGTelegramme: boolean
      read GetIsDSfGTelegramme write SetIsDSfGTelegramme;
    property BaudRate: integer read GetBaudRate write SetBaudRate;
    property TimeOut: integer read GetTimeOut write SetTimeOut;
    property TelegrammFilename: TFileName
      read GetTelegrammFilename write SetTelegrammFilename;
    property DSfGTelegramm: string read GetDSfGTelegramm write SetDSfGTelegramm;
    property ZugangsCode1: string read GetZugangsCode1 write SetZugangsCode1;  // 31.10.2000
    property ZugangsCode2: string read GetZugangsCode2 write SetZugangsCode2;  // 24.07.2000
    property InstAdresse: char read GetInstAdr write SetInstAdr;  // 07.08.2000
    property StammDb: string read GetStammDb write SetStammDb;  // 31.10.2000
    property WorkDb: string read GetWorkDb write SetWorkDb;  // 31.10.2000
    property StammDir: string read GetStammDir write SetStammDir;  // 31.10.2000
    property WorkDir: string read GetWorkDir write SetWorkDir;  // 31.10.2000
    property CardAddress: char read GetCardAddress write SetCardAddress;  // 27.11.2000
    property WiverDatum: TDateTime read GetWiverDatum write SetWiverDatum;  // 11.01.2001
    property DPAComPort: byte read GetComPort write SetComPort;
    property LogFileName: TFileName read GetLogFilename write SetLogFilename;
  end;

var
  DSfGActiveXIniFile : TDSfGActiveXIniFile;

implementation

const
  C_Reg_Path              = '\Software\Wieser\DSfGCard\DSfGActiveX\';

  C_Ident_BaudRate        = 'BAUDRATE';
  C_Ident_TimeOut         = 'TIMEOUT';
  C_Ident_TeleFileName    = 'TELEFILENAME';
  C_Ident_CardParameters  = 'CARDPARAMETERS';
  C_Ident_Teilnehmerliste = 'TEILNEHMERLISTE';
  C_Ident_Fehlerliste     = 'FEHLERLISTE';
  C_Ident_InstanzTypen    = 'INSTANZTYPEN';
  C_Ident_GeraeteTypen    = 'GERAETETYPEN';
  C_Ident_DSfGTelegramme  = 'DSFGTELEGRAMME';
  C_Ident_Zugangscode1    = 'ZUGANGSCODE1';    // 31.10.2000
  C_Ident_Zugangscode2    = 'ZUGANGSCODE2';
  C_Ident_InstAdresse     = 'INSTADR';
  C_Ident_StammDb         = 'STAMMDB';         // 31.10.2000
  C_Ident_WorkDb          = 'WORKDB';          // 31.10.2000
  C_Ident_StammDir        = 'STAMMDB';         // 31.10.2000
  C_Ident_WorkDir         = 'WORKDB';          // 31.10.2000
  C_Ident_CardAddress     = 'CARDADDRESS';     // 27.11.2000
  C_IdentWIVERTest        = 'WIVERPW';         // 11.01.2001
  C_Ident_ComPort         = 'COMPORT';         // 26.09.2003
  C_Ident_LogFile         = 'LOGFILE';         // 04.01.2005

{ 2. constructor ohne Dateiübergabe                                          }
{----------------------------------------------------------------------------}
constructor TDSfGActiveXIniFile.Create;
{----------------------------------------------------------------------------}
begin
  inherited Create;
  // HKEY_LOCAL_MACHINE geht nicht bei eingeschränkten Rechten
  RootKey := HKEY_CURRENT_USER;
  if (not OpenKey(C_Reg_Path, True)) then
    raise Exception.Create('Zugriff auf die Registry fehlgeschlagen');
end;

{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetIsCardParameters: boolean;
{----------------------------------------------------------------------------}
begin
  try
    Result := Self.ReadBool(C_Ident_CardParameters);
  except
    Self.WriteBool(C_Ident_CardParameters, False);
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetIsCardParameters(Value: boolean);
{----------------------------------------------------------------------------}
begin
  Self.WriteBool(C_Ident_CardParameters, Value);
end;

{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetIsTeilnehmerListe: boolean;
{----------------------------------------------------------------------------}
begin
  try
    Result := Self.ReadBool(C_Ident_Teilnehmerliste);
  except
    Self.WriteBool(C_Ident_Teilnehmerliste, False);
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetIsTeilnehmerListe(Value: boolean);
{----------------------------------------------------------------------------}
begin
  Self.WriteBool(C_Ident_Teilnehmerliste, Value);
end;

{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetIsFehlerListe: boolean;
{----------------------------------------------------------------------------}
begin
  try
    Result := Self.ReadBool(C_Ident_Fehlerliste);
  except
    Self.WriteBool(C_Ident_Fehlerliste, False);
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetIsFehlerListe(Value: boolean);
{----------------------------------------------------------------------------}
begin
  Self.WriteBool(C_Ident_Fehlerliste, Value);
end;

{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetIsInstanzTypen: boolean;
{----------------------------------------------------------------------------}
begin
  try
    Result := Self.ReadBool(C_Ident_InstanzTypen);
  except
    Self.WriteBool(C_Ident_InstanzTypen, False);
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetIsInstanzTypen(Value: boolean);
{----------------------------------------------------------------------------}
begin
  Self.WriteBool(C_Ident_InstanzTypen, Value);
end;

{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetIsGeraeteTypen: boolean;
{----------------------------------------------------------------------------}
begin
  try
    Result := Self.ReadBool(C_Ident_GeraeteTypen);
  except
    Self.WriteBool(C_Ident_GeraeteTypen, False);
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetIsGeraeteTypen(Value: boolean);
{----------------------------------------------------------------------------}
begin
  Self.WriteBool(C_Ident_GeraeteTypen, Value);
end;

{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetIsDSfGTelegramme: boolean;
{----------------------------------------------------------------------------}
begin
  try
    Result := Self.ReadBool(C_Ident_DSfGTelegramme);
  except
    Self.WriteBool(C_Ident_DSfGTelegramme, False);
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetIsDSfGTelegramme(Value: boolean);
{----------------------------------------------------------------------------}
begin
  Self.WriteBool(C_Ident_DSfGTelegramme, Value);
end;

{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetBaudRate: integer;
{----------------------------------------------------------------------------}
begin
  try
    Result := Self.ReadInteger(C_Ident_BaudRate);
  except
    Self.WriteInteger(C_Ident_BaudRate, 9600);
    Result := 9600;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetBaudRate(Value: integer);
{----------------------------------------------------------------------------}
begin
  Self.WriteInteger(C_Ident_BaudRate, Value);
end;

{ Liest die Totzeit für DSfG-Kommandos                                       }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetTimeOut: integer;
{----------------------------------------------------------------------------}
begin
  try
    Result := Self.ReadInteger(C_Ident_TimeOut);
  except
    Self.WriteInteger(C_Ident_TimeOut, 60000);
    Result := 60000;
  end;
end;

{ Setzt die Totzeit für DSfG-Kommandos                                       }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetTimeOut(Value: integer);
{----------------------------------------------------------------------------}
begin
  Self.WriteInteger(C_Ident_TimeOut, Value);
end;

{ Liest den Dateinamen der gespeicherten DSfG-Telegramme                     }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetTelegrammFilename: TFileName;
{----------------------------------------------------------------------------}
begin
  try
    Result := Self.ReadString(C_Ident_TeleFileName);
  except
    Self.WriteString(C_Ident_TeleFileName, '');
    Result := '';
  end;
  // Überprüfung, ob die angegebene Datei existiert
  if (Result <> '') and (not FileExists(Result)) then begin
    Self.WriteString(C_Ident_TeleFileName, '');
    Result := '';
  end;
end;

{ Schreibt den Dateinamen der gespeicherten DSfG-Telegramme                  }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetTelegrammFilename(Value: TFileName);
{----------------------------------------------------------------------------}
begin
  Self.WriteString(C_Ident_TeleFileName, Value);
end;

{ Liest ein gespeichertes DSfG-Telegramm                                     }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetDSfGTelegramm: string;
{----------------------------------------------------------------------------}
var
  sFileName : TFileName;
  f         : TextFile;
begin
  Result := '';  // default
  sFileName := Self.GetTelegrammFilename;

  if (sFileName <> '') then begin
    AssignFile(f, sFileName);
{$I-}
    Reset(f);
{$I+}
    if (IOResult = 0) then ReadLn(f, Result);
    CloseFile(f);
  end;
end;

{ Schreibt ein DSfG-Telegramm in das in der Registry eingetragene File       }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetDSfGTelegramm(Value: string);
{----------------------------------------------------------------------------}
var
  sFileName : TFileName;
  f         : TextFile;
  p1, p2    : PChar;
begin
  sFileName := Self.GetTelegrammFilename;
  { Sicherstellen, daß ein File eingetragen ist }
  if (sFileName = '') then begin
    GetMem(p1, 100);
    GetMem(p2, 100);
    GetTempPath(100, p1);
    GetTempFileName(p1, 'DSG', 0, p2);
    sFileName := StrPas(p2);
    FreeMem(p1, 100);
    FreeMem(p2, 100);
    SetTelegrammFilename(sFileName);
  end;
  { Text in File schreiben }
  AssignFile(f, sFileName);
{$I-}
  Rewrite(f);
{$I+}
  if (IOResult = 0) then WriteLn(f, Value);
  CloseFile(f);
end;

{ Liest den Zugagscode1 für Parametrierung aus                               }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetZugangsCode1: string;
{----------------------------------------------------------------------------}
begin
  if (not Self.ValueExists(C_Ident_Zugangscode1)) then
    Self.WriteString(C_Ident_Zugangscode1, '1234');
  Result := Self.ReadString(C_Ident_Zugangscode1);
end;

{ Schreibt den Zugagscode1 für Parametrierung                                }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetZugangsCode1(Value: string);
{----------------------------------------------------------------------------}
begin
  Self.WriteString(C_Ident_Zugangscode1, Value);
end;

{ Liest den Zugagscode2 für Parametrierung aus                               }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetZugangsCode2: string;
{----------------------------------------------------------------------------}
begin
  if (not Self.ValueExists(C_Ident_Zugangscode2)) then
    Self.WriteString(C_Ident_Zugangscode2, '');
  Result := Self.ReadString(C_Ident_Zugangscode2);
end;

{ Schreibt den Zugagscode2 für Parametrierung                                }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetZugangsCode2(Value: string);
{----------------------------------------------------------------------------}
begin
  Self.WriteString(C_Ident_Zugangscode2, Value);
end;

{ Liest die einzustellende Busadresse für die DSfGCard                       }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetInstAdr: char;
{----------------------------------------------------------------------------}
begin
  if (not Self.ValueExists(C_Ident_InstAdresse)) then
    Self.WriteString(C_Ident_InstAdresse, 'R');
  Result := Self.ReadString(C_Ident_InstAdresse)[1];
end;

{ Schreibt die einzustellende Busadresse für die DSfGCard                    }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetInstAdr(Value: char);
{----------------------------------------------------------------------------}
begin
  Self.WriteString(C_Ident_InstAdresse, Value);
end;

{ Liest den Datenbanknamen für Stammdaten aus                                }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetStammDb: string;
{----------------------------------------------------------------------------}
begin
  if (not Self.ValueExists(C_Ident_StammDb)) then
    Self.WriteString(C_Ident_StammDb, 'C:\WIESER\STAMMDAT');
  Result := Self.ReadString(C_Ident_StammDb);
end;

{ Schreibt den Datenbanknamen für Stammdaten                                 }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetStammDb(Value: string);
{----------------------------------------------------------------------------}
begin
  Self.WriteString(C_Ident_StammDb, Value);
end;

{ Liest den Datenbanknamen für Workdaten aus                                 }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetWorkDb: string;
{----------------------------------------------------------------------------}
begin
  if (not Self.ValueExists(C_Ident_WorkDb)) then
    Self.WriteString(C_Ident_WorkDb, 'C:\WIESER\WORK');
  Result := Self.ReadString(C_Ident_WorkDb);
end;

{ Schreibt den Datenbanknamen für Workdaten                                  }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetWorkDb(Value: string);
{----------------------------------------------------------------------------}
begin
  Self.WriteString(C_Ident_WorkDb, Value);
end;

{ Liest den Pfad für Stammdaten aus                                          }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetStammDir: string;
{----------------------------------------------------------------------------}
begin
  if (not Self.ValueExists(C_Ident_StammDir)) then
    Self.WriteString(C_Ident_StammDir, 'C:\WIESER\STAMMDAT');
  Result := Self.ReadString(C_Ident_StammDir);
end;

{ Schreibt den Pfad für Stammdaten                                           }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetStammDir(Value: string);
{----------------------------------------------------------------------------}
begin
  Self.WriteString(C_Ident_StammDir, Value);
end;

{ Liest den Pfad für Workdaten aus                                           }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetWorkDir: string;
{----------------------------------------------------------------------------}
begin
  if (not Self.ValueExists(C_Ident_WorkDir)) then
    Self.WriteString(C_Ident_WorkDir, 'C:\WIESER\WORK');
  Result := Self.ReadString(C_Ident_WorkDb);
end;

{ Schreibt den Pfad für Workdaten                                            }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetWorkDir(Value: string);
{----------------------------------------------------------------------------}
begin
  Self.WriteString(C_Ident_WorkDir, Value);
end;

{ Liest die Adresse für die DSfGCard                                         }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetCardAddress: char;
{----------------------------------------------------------------------------}
begin
  if (not Self.ValueExists(C_Ident_CardAddress)) then
    Self.WriteString(C_Ident_CardAddress, '0');  // 21.03.2002
  Result := Self.ReadString(C_Ident_CardAddress)[1];
end;

{ Schreibt die Adresse für die DSfGCard                                      }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetCardAddress(Value: char);
{----------------------------------------------------------------------------}
begin
  Self.WriteString(C_Ident_CardAddress, Value);
end;

{ Gibt Ablaufdatum für laufzeitbegrenzte WIVER32-Version zurück              }
{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetWiverDatum: TDateTime;
{----------------------------------------------------------------------------}

  function DecodeWiverDatum(sCode: string): TDateTime;
  const
    C_Basis = 'Q';
  var
    i : integer;
    s : string;
  begin
    Result := -1; // Default = Fehler
    try
      s := '';
      //  Unixzeit als String aus Code erzeugen
      for i := Length(sCode) downto 1 do begin
        s := s + IntToStr(Ord(C_Basis) - Ord(sCode[i]));
      end;
      Result := UnixToDateTime(StrToInt(s));
    except
    // es wird bereits -1 zurückgegeben
    end;
  end;

begin
  if (not Self.ValueExists(C_IdentWIVERTest))
    then Result := 0
    else Result := DecodeWiverDatum(Self.ReadString(C_IdentWIVERTest));
end;

{ Schreibt Ablaufdatum für laufzeitbegrenzte WIVER32-Version                 }
{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetWiverDatum(Value: TDateTime);
{----------------------------------------------------------------------------}
var
  sCode : string;

  function EncodeWiverDatum(dtCode: TDateTime): string;
  const
    C_Basis = 'Q';
  var
    i : integer;
    s, s1 : string;
  begin
    Result := '0'; // Default = Fehler
    try
      s1 := '';
      s := IntToStr(DateTimeToUnix(Value));
      //  Unixzeit als Code-String  erzeugen
      for i := 1 to Length(s) do begin
        s1 := Chr(Ord(C_Basis) - StrToInt(s[i]))  + s1;
      end;
      Result := s1;
    except
    // es wird bereits '0' zurückgegeben
    end;
  end;

begin
  sCode := EncodeWiverDatum(Value);
  if (sCode = '0')
    then raise Exception.Create('Konnte Trialdatum nicht umsetzen !')
    else Self.WriteString(C_IdentWIVERTest, sCode);
end;

{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetComPort: byte;
{----------------------------------------------------------------------------}
begin
  if (not ValueExists(C_Ident_ComPort)) then WriteInteger(C_Ident_ComPort, 1);
  Result := ReadInteger(C_Ident_ComPort);
end;

{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetComPort(iPort: byte);
{----------------------------------------------------------------------------}
begin
  WriteInteger(C_Ident_ComPort, iPort);
end;

{----------------------------------------------------------------------------}
function TDSfGActiveXIniFile.GetLogFilename: TFileName;  // 04.01.2005
{----------------------------------------------------------------------------}
begin
  if (not ValueExists(C_Ident_LogFile)) then WriteString(C_Ident_LogFile, '');
  Result := ReadString(C_Ident_LogFile);
end;

{----------------------------------------------------------------------------}
procedure TDSfGActiveXIniFile.SetLogFilename(Value: TFileName);  // 04.01.2005
{----------------------------------------------------------------------------}
begin
  WriteString(C_Ident_LogFile, Value);
end;

end.
