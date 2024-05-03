{------------------------------------------------------------------------------}
{ PathIni                                                                      }
{ Pfadinitialisierung aus Wieser.ini                                           }
{ 06.11.1996 DA, Copyright Karl Wieser GmbH                                    }
{ Hinzufügen der Laks-Pfade am 2.4.98 von HK                                   }
{ 21.12.1998 GD; in GetIniPathName wird Laufwerk bei '\\' nicht vorangestellt  }
{ 20.01.1999 GD; neuer Pathtype WProgDir                                       }
{ 21.04.1999 GD; neuer Pathtype WNetProgDir, WNetWorkDir                       }
{ 14.04.2000 WW; WNetSysDir raus                                               }
{ 06.07.2000 WW; FindFirst durch DirectoryExists ersetzt                       }
{ 04.12.2000 WW; WComserve raus                                                }
{ 22.10.2001 WW; neuer Pathtype WVoiceDir                                      }
{ 11.10.2002 GD; Zusätzlich 'echte' Datenbanken (-Namen, -User, -Passwort)     }
{ 16.11.2003 GD; Transisolationlevel für Datenbanken setzen                    }
{ 08.03.2004 GD; ProgramIni für zentralen userdefinierten Zugriff erweitert    }
{ 28.07.2004 GD; PathType WNetUserDir ersetzt lokales WorkDir                  }
{ 02.08.2004 WW; Rechner-spezifische Einstellungen (IPPortNr_ab)               }
{ 27.09.2004 GD; Erweitert um ADOConnections, "LangzeitDir" wieder aufgenommen }
{ 09.10.2004 GD; Netzlaufwerke werden ggf. in lokale umbenannt                 }
{ 05.01.2005 GD; lokale (Work-) Databases                                      }
{ 31.05.2005 GD; Erstellung von ConnectionStrings für ADO                      }
{ 29.08.2005 GD; Zusätzliche (Drucker-)Einstellungen merken in ProgramIni      }
{ 11.10.2005 GD; PrivateDir für jede session (TPathServerSession)              }
{ 24.11.2005 GD; PathTypes des Pathservers veröffentlicht                      }
{ 20.01.2006 WW; resourcestrings                                               }
{ 09.01.2007 GD  CheckAccess                                                   }
{ 20.11.2007 WW; Language-Einstellung                                          }
{ 25.02.2008 GD/WW; CheckDbConnection                                          }
{ 26.05.2008 GD; Schleife über InitPathList (Mehrfachversuch bei Fehler)       }
{ 19.08.2009 GD; PrivateDir nicht zwingend setzen                              }
{ 08.09.2009 GD; CheckDbConnection mit Prüfung der definierten Datenbanken     }
{ 22.03.2010 WN; TProgramIni -> property UserDefined                           }
{                TProgramIni.Create -> Übergabe eines Dateinamens einer *.ini  }
{                Erweiterung um die Wical-Pfade und -Datenbanken               }
{ 01.12.2010 GD; ADO-Editierfunktionen veröffentlicht                          }
{ 27.01.2011 WN; Zählpunktanzeige im Auswahlbaum ein/aus                       }
{ 17.08.2011 GD; ProgramIni greift als Standard auf NetProgDir zu              }
{ 27.12.2012 WW; PathType WAdoOPCDb                                            }
{ 11.01.2013 WW; CheckDbConnection mit Prüfung auf WAdoOPCDb;                  }
{                Property LoginDlg                                             }
{ 05.02.2013 WN; Erweiterung um die Ascii-Reporting-Pfade                      }
{ 01.03.2013 WW; PathType WAdoPIMMSExportDb; IFDEF ONLY_MSACCESS               }
{ 15.07.2013 WW; AdoLogin: Kein Schreiben der Wieser.ini bei FLoginDlg = false }
{ 30.07.2013 WW; Bugfix AdoLogin: Schreiben der Wieser.ini für MS Access       }
{ 22.04.2014 GD; PathType WLogDir                                              }
{ 21.04.2015 WW; function GetPathServerLogDir                                  }
{ 08.10.2015 WN; function GetStammDb                                           }
{ 05.07.2018 WW; PathType WTriggerDir, function GetPathServerTriggerDir        }
{------------------------------------------------------------------------------}
unit Pathini;

interface

uses
  Windows, Classes, Forms, IniFiles, SysUtils, FileCtrl, DbTables, Bde, Db,
  Dblogdlg, BdeConst, AdoDb, ADOConEd, Printers, WTables, WPorts, ReInit, T_Zeit;

resourcestring
  CMsgErrDbConnection = 'Datenbank-Verbindung konnte nicht wiederhergestellt werden.';
  CMsgDbReconnected   = 'Datenbank-Verbindung wurde wiederhergestellt.';

type
  // Datenbankdefinition
  TDatabaseDef = record    // 11.10.2002
    DatabaseName : string; { Datenbankname }
    AliasName    : string; { Aliasname der Database }
    UserName     : string; { Benutzername }
    Password     : string; { Passwort }
    DescString   : string; { Bei ADO: ggf. ConnectionString }
  end;

  { Pfadbezeichner }

  TPathType = (
    WieserIni,       { Verzeichnis, in dem Wieser.ini vorhanden ist }
    AsciiDir,        { Verzeichnis für Ascii-Daten }

    // 05.02.2013  WN
    AsciiRepDefDir,   // Ascii-Reporting: Definitionspfad
    AsciiRepExpDir,   // Ascii-Reporting: Exportpfad

{$IFDEF DELPHI32}
    // Nur noch für Abwärtskompatibilität benötigt
    DArchivDir,      { DSFG-Archivdatenverzeichnis, automatisch }
    DManuDir,        { DSfG-Archivdatenverzeichnis, manuell }
    LangzeitDir,     { Langzeitverzeichnis }
    ManuDir,         { Verzeichnis manuell abgerufener Daten }
{$ENDIF}

{$IFDEF WICOM}
    // 22.03.2010  WN
    LangzeitDir,     // Langzeitverzeichnis
{$ENDIF}

    LArchivDir,      { Laks, Archiv-Verzeichnis }
    LDPDatenDir,     { Laks, Datenverzeichnis für Dialog }
    LSysdatDir,      { Laks, Daten-Systemverzeichnis }
    LExportDir,      { Laks, Exportverzeichnis }
    LAuswStammDir,   { Laks, Auswert-Stammdatenverzeichnis }
    WProgDir,        { Exe-Dateien }
    WNetProgDir,     { Exe-Dateien, Netz }
    WStammDir,       { Mrg-Stammdaten }
    WWorkDir,        { Userspezifisches Arbeitsverzeichnis auf dem Netz }
    WNetWorkDir,     { WinDFÜ-Arbeitsverzeichnis, Netz }
    WVoiceDir,       { Verzeichnis der Voice-Files }

    // 22.03.2010  WN
    WWicalStammDir,  // Pfad auf die Wical-Stammdaen
    WWicalArchivDir, // Pfad auf das Wical-Archiv
    MRG910AsciiDir,  // MRG910-Ascii-Daten
    ExportDir,       // Exportdaten
    WicalIconDir,    // Verzeichnis der Gruppen-Icon
    DArchivDir,      // DSFG-Archivdatenverzeichnis, automatisch

    WLogDir,      // Standardverzeichnis für LOG-Dateien // 22.04.2014
    WTriggerDir,  // Standardverzeichnis für TST-Dateien // 05.07.2018

    WWorkDb,         { Work - Datenbankname für userspezifische Daten }
    WNetWorkDb,      { Work - Datenbankname für systemspezifische Daten }

    WStammDb,        { Alias - Datenbankname für WIESER-Stammdaten }  // 11.10.2002
    AutoDb,          { Alias - Datenbankname für Auto-Daten }  // 11.10.2002
    ManuDb,          { Alias - Datenbankname für Manu-Daten }  // 11.10.2002
    WWicalStammDb,   { Alias - Datenbankname für Wical-Stammdaten }
    WWicalArchivDb,  { Alias - Datenbankname für Wical-Archiv }
    WBDEDb,          { Alias - Allgemein definierte BDE-Verbindung }  // 27.09.2004
    WADODb,          { Alias - Allgemein definierte ADO-Verbindung }  // 27.09.2004
    WAdoOPCDb,       { Alias - ADO-Verbindung zu OPC-Interface-DB }  // 27.12.2012, WW
    WAdoPIMMSExportDb{ Alias - ADO-Verbindung zu PIMMS-Export-DB (Petrom) }  // 01.03.2013, WW
  );

  TPathTypes = set of TPathType;

  TPathItem = class (TObject)
  private
    PathType    : TPathType;
    PathName    : TFileName;
    DbSection   : string;
    FIsDatabase : boolean;           // 11.10.2002
    FIsADOConnection : boolean;      // 27.09.2004
    FDatabaseDef: TDatabaseDef;      // 11.10.2002
    FUserName   : string;            // 28.07.2004
    function CheckForDatabase(pIniFile: TInifile): boolean;
    function CreateDatabase(pIniFile: TIniFile): string;
    function CheckForAdoConnection(pIniFile: TInifile): boolean;
    procedure InitPathName (IniFile: TIniFile);
    procedure Check;
  public
    constructor Create (APathType: TPathType; IniFile: TIniFile);
    property IsDatabase: boolean read FIsDatabase;
    property IsADOConnection: boolean read FIsADOConnection;
    property DatabaseDef: TDatabaseDef read FDatabaseDef;
  end;

  TPathServerSession = class(TSession)
    constructor Create(pOwner: TComponent); override;
    destructor Destroy; override;
  private
  protected
  public
  end;

  TPathServer = class (TObject)
  private
    PathList   : TList;
    FWieserIni : TIniFile;
    FLogoName  : TFileName;
    FBasePath  : TFileName;
    FDbList    : TList;
    FAdoList   : TList;   // 27.09.2004
    FSession   : TSession;
    FOwnSession: boolean;
    FCheckAccess : boolean;
    FNetPath   : string;  // 09.10.2004
    FLocalPath : string;  // 09.10.2004
    FPathTypes : TPathTypes;  // 24.11.2005
    FLoginDlg: boolean;  // 18.01.2013, WW
    function GetDbDirectory(iIndex: TPathType): string;
    function GetAliasName(iIndex: TPathType): string;
    function GetPathName (Index: TPathType): TFileName;
    function ReadDatabase(iIndex: TPathType): TDatabase;
    function ReadAdoConnection(iIndex: TPathType): TAdoConnection;
    procedure SetPathName (Index: TPathType; Value: TFileName);
    procedure InitPathList (const APathTypes: TPathTypes);
    procedure InitLogoName;
    function RMaxKanaele: word;
    function RShowZpkt: Boolean;  // 27.01.2011  WN
    procedure DbLogin(pDatabase: TDatabase; pLoginParams: TStrings); // 11.10.2002
    procedure AdoLogin(Sender:TObject; sUsername, sPassword, sDbSection: string); // 27.09.2004
    procedure CreateSession;
    function GetIPPortNr_ab (Hostname: string): integer;
    function GetPathItem(iIndex: TPathType): TPathItem;
    procedure CheckAccess(pPathItem: TPathItem);
    function RLanguage: LCID;  // 20.11.2007, WW
    function InsertPathItem(iIndex: TPathType): boolean;
  public
    constructor Create (const FileName: TFileName;
                        const APathTypes: TPathTypes;
                        bSessionOwner: boolean = True;
                        bCheckAccess: boolean = True);
    destructor Destroy; override;
    procedure Check;
    function SetNetPath(sFilePath: TFileName): TFileName;
    function SetLocalPath(sFilePath: TFileName): TFileName;
    function GetDatabase(iIndex: TPathType): TDatabase;
    function GetAdoConnection(iIndex: TPathType): TAdoConnection;
    property LogoName: TFileName read FLogoName;
    property MaxKanaele: word read RMaxKanaele;
    property ShowZpkt: Boolean read RShowZpkt;  // 27.01.2011  WN
    property Pathname [Index: TPathType]: TFileName
      read GetPathName write SetPathName; default;
    property Database [Index: TPathType]: TDatabase read ReadDatabase;
    property AdoConnection [Index: TPathType]: TAdoConnection
      read ReadAdoConnection;
    property PathItem [iIndex: TPathType]: TPathItem read GetPathItem;
    property AliasName [iIndex: TPathType]: string read GetAliasName;
    property DbDirectory [iIndex: TPathType]: string read GetDbDirectory;
    property Session: TSession read FSession;
    property IPPortNr_ab [Hostname: string]: integer read GetIPPortNr_ab;
    property PathTypes: TPathTypes read FPathTypes;
    property Language: LCID read RLanguage;
    property LoginDlg: boolean read FLoginDlg write FLoginDlg;  // 18.01.2013, WW
  end;

  TProgramIni = class (TIniFile)
  private
    WieserItem   : TPathItem;
    FUserDefined : boolean;
    FUserName    : string;
    function GetWieserIni: TFileName;
  protected
    procedure ReadFromIni; virtual;
    property UserDefined : boolean read FUserDefined write FUserDefined;  // 22.03.2010  WN
  public
  	constructor Create(bNetIni: boolean = True; bUserDefined: boolean = False;
                       sFilename : TFilename='');  // 22.03.2010  WN, 17.08.2011
    destructor Destroy; override;
    procedure LoadActUserSettings; virtual;
    procedure SaveActUserSettings; virtual;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    function SectionExists(const Section: string): Boolean;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    property WieserIniFile: TFileName read GetWieserIni;
    property UserName: string read FUserName;
  end;

function EncodePW (const PW: string): string;
function DecodePW (const PWCode: string): string;
function GetAdoDBMSName(
  pAdoConnection: TADOConnection; var sDBMSName: string): boolean;
function EditADOConnectionString(
  const sDbIdent, sDbAlias: string; var sConnectionString: string): boolean;
function BuildADOConnectionString(var sConnectionString, sDataSource,
  sInitCatalogue, sUsername, sPassword: string): boolean; // 01.12.2010
function CheckDbConnection: integer;
procedure InitLanguage;
procedure InitializeBDE;
function GetPathServerLogDir (var sLogDir: string; var sErrMsg: string): boolean;
function GetPathServerTriggerDir (var sTriggerDir: string;
  var sErrMsg: string): boolean;
function GetStammDb: TDatabase;  // 08.10.2015  WN

var
  PathServer: TPathServer;

implementation

resourcestring

  S_PfadUngueltig   = 'Kein gültiger Pfad vorhanden !';
  S_KeineDbDateien  = 'Keine Tabellen-Dateien vorhanden !';
  S_KeineADOVerb    = ': Keine ADO-Verbindung definiert !';
  S_DbFehlt         = 'Datenbank %s nicht vorhanden';
  S_PfadFehlt       = 'Pfad %s nicht vorhanden';
  S_PathTypeInitErr = 'TPathServer: PathType nicht initialisiert';
  S_NoAccess        = 'Unzureichende Zugriffsrechte:'#13#10'%s';

type

  TPathDef = record
    PathType: TPathType;   { Bezeichner }
    Section: string;       { Section in Wieser.ini }
    Ident: string;         { Ident in Wieser.ini }
    Default: string;       { Voreinstellung }
  end;

const

  CWieserIni            = 'WIESER.INI';

  C_Sect_Databases      = 'Databases';
  C_Sect_ADOConnections = 'ADOCONNECTIONS';
  C_Sect_Hostname       = 'Hostname:';

  C_Ident_AliasName     = 'ALIASNAME';
  C_Ident_UserName      = 'USERNAME';
  C_Ident_Password      = 'PASSWORD';
  C_Ident_DescString    = 'DESCSTRING';

  C_Ident_IPPortNr_ab   = 'IPPortNr_ab';

  C_Ident_NetPath       = 'NETPATH';
  C_Ident_LocalPath     = 'LOCALPATH';

  C_DbSection           = 'DBSECTION';

  // ADO-ConnectionString-Einzelteile
  C_AdoIdent_ConnectionStr    = 'CONNECTIONSTR';  // ConnectionString
  C_AdoIdent_Provider         = 'Provider';     // Standard: 'MSDASQL.1'
  C_AdoIdent_Password         = 'Password';     // Passwort
  C_AdoIdent_PersistSecInfo   = 'Persist Security Info'; // Standard: 'False'
  C_AdoIdent_UserName         = 'User ID';      // Benutzername
  C_AdoIdent_DataSource       = 'Data Source';  // Data Source (z.B. ODBC-Alias)
  C_AdoIdent_InitialCatalog   = 'Initial Catalog';  // Datenbank-Bezeichnung
  C_AdoIdent_WDbSection       = 'WDbSection';   // Wieser-Bezeichnung

  C_AdoValue_Provider         = 'MSDASQL.1';
  C_AdoValue_PersistSecInfo   = 'False';

  C_AdoProperty_Name_DBMSName = 'DBMS Name';

  C_Sect_UserSettings         = 'USERSETTINGS_';
  C_Ident_UserPrinter         = 'USERPRINTER';


  PathDefs : array [TPathType] of TPathDef = (
// Verzeichnispfade
    (PathType: WieserIni; Section: 'Wieser';
       Ident: 'WieserIni'; Default: '..\'),
    (PathType: AsciiDir; Section: 'Wieser';
       Ident: 'AsciiDir'; Default: 'Ascii\'),
    // 05.02.2013  WN
    (PathType: AsciiRepDefDir; Section: 'Wieser';
       Ident: 'AsciiRepDefDir'; Default: 'Ascii\ReportDef\'),  // Ascii-Reporting: Definitionspfad
    (PathType: AsciiRepExpDir; Section: 'Wieser';
       Ident: 'AsciiRepExpDir'; Default: 'Ascii\ReportExp\'),  // Ascii-Reporting: Exportpfad
{$IFDEF DELPHI32}
// Abwärtskompatibilität
    (PathType: DArchivDir; Section: 'DSFGDfu';
       Ident: 'ArchivDir'; Default: 'DSFGArch\Auto\'),
    (PathType: DManuDir; Section: 'DSfGDfu';
       Ident: 'ManuDir'; Default: 'DSfGArch\Manu\'),
    (PathType: LangzeitDir; Section: 'Wieser';
       Ident: 'LangzeitDir'; Default: 'Langzeit\'),
    (PathType: ManuDir; Section: 'Wieser';
       Ident: 'ManuDir'; Default: 'Manu\'),
{$ENDIF}
// 22.03.2010  WN
{$IFDEF WICOM}
    (PathType: LangzeitDir; Section: 'Wieser';
       Ident: 'LangzeitDir'; Default: 'Langzeit\'),
{$ENDIF}

    (PathType: LArchivDir; Section: 'Laks';
       Ident: 'ArchivDir'; Default: 'Ausw\Archiv\'),
    (PathType: LDPDatenDir; Section: 'Laks';
       Ident: 'LDPDatenDir'; Default: 'Laksdia\Daten\'),
    (PathType: LSysdatDir; Section: 'Laks';
       Ident: 'SysdatDir'; Default: 'Laksdia\Sysdat\'),
    (PathType: LExportDir; Section: 'Laks';
       Ident: 'ExportDir'; Default: 'Export\'),
    (PathType: LAuswStammDir; Section: 'Laks';
       Ident: 'AuswStammDir'; Default: 'Ausw\Stamm\'),
    (PathType: WProgDir; Section: 'Programme';
       Ident: 'WProgDir'; Default: 'Prog'),
    (PathType: WNetProgDir; Section: 'Programme';
       Ident: 'WNetProgDir'; Default: 'Prog'),
    (PathType: WStammDir; Section: 'WinDfu';
       Ident: 'StammDir'; Default: 'Stammdat\'),
    (PathType: WWorkDir; Section: 'WinDfu';
       Ident: 'WNetUserDir'; Default: 'User\'),
    (PathType: WNetWorkDir; Section: 'WinDfu';
       Ident: 'NetWorkDir'; Default: 'Work\'),
    (PathType: WVoiceDir; Section: 'WinDfu';
       Ident: 'VoiceDir'; Default: 'VoiceDat\'),

    // 22.03.2010  WN
// Wical-Verzeichnisse
    (PathType: WWicalStammDir; Section: 'WICAL';
       Ident: 'WWicalStammDir'; Default: 'WicalStammDir'),
    (PathType: WWicalArchivDir; Section: 'WICAL';
       Ident: 'WWicalArchivDir'; Default: 'WicalArchivDir'),
    (PathType: MRG910AsciiDir; Section: 'WICAL';
       Ident: 'MRG910AsciiDir'; Default: ''),
    (PathType: ExportDir; Section: 'WICAL';
       Ident: 'ExportDir'; Default: ''),
    (PathType: WicalIconDir; Section: 'WICAL';
       Ident: 'WicalIconDir'; Default: 'WicalProg'),
    (PathType: DArchivDir; Section: 'DSFGDfu';
       Ident: 'ArchivDir'; Default: 'DSFGArch\Auto\'),

// Log-Dateien  // 22.04.2014       
    (PathType: WLogDir; Section: 'Wieser';
       Ident: 'WLogDir'; Default: 'Log\'),

// Trigger-Dateien  // 05.07.2018  WW
    (PathType: WTriggerDir; Section: 'Wieser';
       Ident: 'WTriggerDir'; Default: 'Trigger\'),

// Work-Datenbanken
    (PathType: WWorkDb; Section: 'WinDfu';
       Ident: 'WNetUserDir'; Default: 'User\'),
    (PathType: WNetWorkDb; Section: 'WinDfu';
       Ident: 'NetWorkDir'; Default: 'Work\'),

// BDE-Datenbanken
    (PathType: WStammDb; Section: C_Sect_Databases;
       Ident: 'WStammDb'; Default: 'WStammDb'),
    (PathType: AutoDb; Section: C_Sect_Databases;
       Ident: 'AutoDb'; Default: 'WAutoDb'),
    (PathType: ManuDb; Section: C_Sect_Databases;
       Ident: 'ManuDb'; Default: 'WManuDb'),
    (PathType: WWicalStammDb; Section: C_Sect_Databases;
       Ident: 'WWicalStammDb'; Default: 'WicalStammDb'),
    (PathType: WWicalArchivDb; Section: C_Sect_Databases;
       Ident: 'WWicalArchivDb'; Default: 'WicalArchivDb'),
    (PathType: WBDEDb; Section: C_Sect_Databases;
       Ident: 'BDEDb'; Default: 'WBDEDb'),

// ADO-Datenbanken
    (PathType: WADODb; Section: C_Sect_ADOConnections;
       Ident: 'ADODb'; Default: 'WADODb'),
    (PathType: WAdoOPCDb; Section: C_Sect_ADOConnections;
       Ident: 'OPCDb'; Default: 'WOPCDb'),  // 27.12.2012, WW
    (PathType: WAdoPIMMSExportDb; Section: C_Sect_ADOConnections;
       Ident: 'PIMMSExportDb'; Default: 'WPIMMSExportDb')  // 01.03.2013, WW
  );


// Allgemeine Funktionen

{----------------------------------------------------}
procedure InitializeBDE;
{----------------------------------------------------}
var
  Status: DBIResult;
  Env: DbiEnv;
  ClientHandle: hDBIObj;
  SetCursor: Boolean;
begin
  SetCursor := (GetCurrentThreadID = MainThreadID) and (DBScreen.Cursor = dcrDefault);
  if SetCursor then
    DBScreen.Cursor := dcrHourGlass;
  try
    FillChar(Env, SizeOf(Env), 0);
    StrPLCopy(Env.szLang, SIDAPILangID, SizeOf(Env.szLang) - 1);
    Status := DbiInit(@Env);
    if (Status <> DBIERR_NONE) and (Status <> DBIERR_MULTIPLEINIT) then
      Check(Status);
    if DbiGetObjFromName(objCLIENT, nil, ClientHandle) = 0 then
      DbiSetProp(ClientHandle, Integer(clSQLRESTRICT), GDAL);
  finally
    if SetCursor and (DBScreen.Cursor = dcrHourGlass) then
      DBScreen.Cursor := dcrDefault;
  end;
end;

{----------------------------------------------------}
function WGetComputerName: string;
{----------------------------------------------------}
var
  p : PChar;
  i : DWord;
begin
  GetMem(p, 100);
  try
    p[0] := #0;
    i := 99;
    GetComputerName(p, i);
    Result := StrPas(p);
  finally
    FreeMem(p, 100);
  end;
end;

{-----------------------------------------------------}
function WGetUserName: string;
{-----------------------------------------------------}
var
  p         : PChar;
  i         : dword;
begin
  // Username auslesen
  try
    GetMem(p, 100);
    try
      p[0] := #0;
      i := 99;
      GetUserName(p, i);
      Result := StrPas(p);
    finally
      FreeMem(p, 100);
    end;
  except
    Result := '';
  end;
end;

{-----------------------------------------------------}
function GetUniqueDatabaseName(sOriDbName: string): string;
{-----------------------------------------------------}
var
  iIx, iExt : integer;
  pSl       : TStrings;
  s : string;
begin
  // Prüfen, ob DatabaseName bereits existiert
  pSl := TStringList.Create;
  try
    Delay(10);
    DbTables.Session.GetAliasNames(pSl);
    s := sOriDbName;
    iExt := 1;
    iIx  := pSl.IndexOf(s);
    // DatabaseName ggf. erweitern
    while (iIx >= 0) do begin
      s := s + IntToStr(iExt);
      Inc(iExt);
      iIx  := pSl.IndexOf(s);
    end;
    Result := s;
  finally
    pSl.Free;
  end;
end;

// Passwort-Funktionen

{-----------------------------------------------------}
function EncodePW (const PW: string): string;
{-----------------------------------------------------}
var
  TempCode: string;
  i: LongInt;
  PWLen: LongInt;
  PWInt: LongInt;
  PWCur: LongInt;
begin
  Result := '';

  PWLen := Length (PW);
  TempCode := Format ('%0.3d', [PWLen]);
  for i := 1 to PWLen do
  begin
    PWInt := LongInt (PW [i]);
    TempCode := TempCode + Format ('%0.3d', [PWInt]);
  end;

  PWLen := Length (TempCode);
  PWCur := 0;
  for i := 1 to PWLen do
  begin
    PWInt := StrToInt (TempCode [i]);
    if PWInt > PWCur then
      Result := Result + IntToStr (PWInt - PWCur)
    else
      Result := Result + '0' + IntToStr (PWCur - PWInt);
    PWCur := PWInt;
  end;
end;

{---------------------------------------------------------}
function DecodePW (const PWCode: string): string;
{---------------------------------------------------------}
resourcestring
  S_PasswordError = 'Ungültiges Passwortformat !';
var
  TempCode: string;
  i: LongInt;
  PWLen: LongInt;
  PWInt: LongInt;
  PWCur: LongInt;
begin
  TempCode := '';
  PWLen := Length (PWCode);
  PWCur := 0;
  i := 0;
  while i < PWLen do
  begin
    Inc (i);
    PWInt := StrToInt (PWCode [i]);
    if PWInt = 0 then
    begin
      if (i < PWLen) then
      begin
        Inc (i);
        PWInt := StrToInt (PWCode [i]);
        PWCur := PWCur - PWInt;
      end
      else
        raise EAbort.Create (S_PasswordError);
    end
    else
      PWCur := PWCur + PWInt;
    if (PWCur >= 0) and (PWCur <= 9) then
      TempCode := TempCode + IntToStr (PWCur)
    else
      raise EAbort.Create (S_PasswordError);
  end;

  Result := '';
  PWLen := 0;
  i := 0;
  while (i < (Length (TempCode) - 2)) do
  begin
    PWInt := StrToInt (Copy (TempCode, i + 1, 3));
    if (i = 0) then
      PWLen := PWInt
    else
    begin
      if (PWInt >= 0) and (PWInt <= 255) then
        Result := Result + Char (PWInt)
      else
        raise EAbort.Create (S_PasswordError);
    end;
    Inc (i, 3);
  end;
  if PWLen <> Length (Result) then
    raise EAbort.Create (S_PasswordError);
end;

// Pfad-Funktionen

{-------------------------------------------------------------------}
function GetPathServerDir (APathType: TPathType; var sDir: string;
  var sErrMsg: string): boolean;
{-------------------------------------------------------------------}
begin
  with TProgramIni.Create do
  try
    with TPathServer.Create (WieserIniFile, [APathType]) do
    try
      try
        Check;
        sDir:=Pathname[APathType];
        sErrMsg:='';
        Result:=true;
      except
        on E:Exception do begin
          sDir:='';
          sErrMsg:=E.Message;
          Result:=false;
        end;
      end;
    finally
      Free;
    end;
  finally
    Free;
  end;
end;

{-----------------------------------------------------}
function GetPathServerLogDir (var sLogDir: string;
  var sErrMsg: string): boolean;
{-----------------------------------------------------}
begin
  Result:=GetPathServerDir (WLogDir, sLogDir, sErrMsg);
end;

{-----------------------------------------------------}
function GetPathServerTriggerDir (var sTriggerDir: string;
  var sErrMsg: string): boolean;
{-----------------------------------------------------}
begin
  Result:=GetPathServerDir (WTriggerDir, sTriggerDir, sErrMsg);
end;


{---------------------------- Datenbank-Funktionen ----------------------------}

{---------------------------------------------------------}
function EditADOConnectionString(
  const sDbIdent, sDbAlias: string; var sConnectionString: string): boolean;
{---------------------------------------------------------}
var
  NewConnStr: WideString;
begin
  with TConnEditForm.Create(Application) do
  try
    Caption := Format('%s %s', [sDbIdent, sDbAlias]);
    NewConnStr := sConnectionString;
    NewConnStr := Edit(NewConnStr);
    Result := (NewConnStr <> sConnectionString) and (NewConnStr <> '');
    if (Result) then sConnectionString := NewConnStr;
  finally
    Free;
  end;
end;

{---------------------------------------------------------}
function BuildADOConnectionString(var sConnectionString, sDataSource,
  sInitCatalogue, sUsername, sPassword: string): boolean;
{---------------------------------------------------------}
var
  s : string;
  i : integer;
begin
  s := sConnectionString;
  with TStringList.Create do
  try
    i := Pos(';', s);
    while (i > 0) do begin
      if (i > 1) then Add(Copy(s, 1, i-1));
      System.Delete(s, 1, i);
      i := Pos(';', s);
    end;
    if (Trim(s) <> '') then Add(Trim(s));
    s := '';

    if (sDataSource = '')
    then sDataSource := Values[C_AdoIdent_DataSource]
    else Values[C_AdoIdent_DataSource] := sDataSource;
    if (sInitCatalogue = '')
    then sInitCatalogue := Values[C_AdoIdent_InitialCatalog]
    else Values[C_AdoIdent_InitialCatalog] := sInitCatalogue;
    if (sUsername = '')
    then sUsername := Values[C_AdoIdent_UserName]
    else Values[C_AdoIdent_UserName] := sUsername;
    if (sPassword = '')
    then sPassword := Values[C_AdoIdent_Password]
    else Values[C_AdoIdent_Password] := sPassword;
    for i := 0 to Count-1 do s := s + Strings[i] + ';';
    sConnectionString := Copy(s, 1, Length(s) - 1);
  finally
    Free;
  end;
  Result := True;
end;

{---------------------------------------------------------}
function GetAdoDBMSName(
  pAdoConnection: TADOConnection; var sDBMSName: string): boolean;
{---------------------------------------------------------}
var
  i : integer;
begin
  sDBMSName := '';
  Result := False;
  try
    for i := 0 to PathServer.AdoConnection[WADODB].Properties.Count-1 do begin
      if (LowerCase(PathServer.AdoConnection[WADODB].Properties.Item[i].Name) =
          LowerCase(C_AdoProperty_Name_DBMSName)) then
      begin
        sDBMSName := PathServer.AdoConnection[WADODB].Properties.Item[i].Value;
        Result := True;
        Break;
      end;
    end;
  except
    Result := False;
  end;
end;

{----------------------------------}
function CheckDbConnection: integer;
{----------------------------------}
{ Prüfung der Datenbank-Verbindung, bei Störung reconnecten;
  Ergebnis: -1 = Datenbankverbindung ist gestört und konnte nicht wiederher-
                 gestellt werden
             0 = Datenbankverbindung ist OK
             1 = Datenbankverbindung ist gestört und konnte wiederhergestellt
                 werden }

  // Prüft, ob Datenbank connected ist
  function CheckViaFieldNames: boolean;
  var
    pSl : TStrings;
  begin
    pSl := TStringList.Create;
    try
      try
        // Hier knallts ggf.
        if (WStammDb in PathServer.PathTypes) then begin  // 08.09.2009
          PathServer.Database[WStammDb].FlushSchemaCache('station');
          PathServer.Database[WStammDb].GetFieldNames('station', pSl);
        end
        else if (WWicalStammDb in PathServer.PathTypes) then begin
          PathServer.Database[WWicalStammDb].FlushSchemaCache('AwMst');
          PathServer.Database[WWicalStammDb].GetFieldNames('AwMst', pSl);
        end;
        // WAdoOPCDb: Prüfung über GetTableNames, da keine statischen Tabellen
        //            vorhanden sind
        if (WAdoOPCDb in PathServer.PathTypes) then  // 11.01.2013, WW
          PathServer.AdoConnection [WAdoOPCDb].GetTableNames (pSl, False);
        // WAdoPIMMSExportDb: Prüfung über GetTableNames, da keine statischen Tabellen
        //            vorhanden sind
        if (WAdoPIMMSExportDb in PathServer.PathTypes) then  // 01.03.2013, WW
          PathServer.AdoConnection [WAdoPIMMSExportDb].GetTableNames (pSl, False);

        Result := True;
      except
        Result := False;
      end;
    finally
      pSl.Free;
    end;
  end;

var
  bCheck: boolean;

begin
  try
    Result:=0;  // Vorbelegung: DB-Verbindung OK

    bCheck := CheckViaFieldNames;  // Prüfen
    if (not bCheck) then begin  // war nix …
      // Disconnecten ...
      if (WStammDb in PathServer.PathTypes) and    // 08.09.2009
        (PathServer.Database[WStammDb].Connected)
      then PathServer.Database[WStammDb].Connected := False;
      if (AutoDb in PathServer.PathTypes) and
        (PathServer.Database[AutoDb].Connected)
      then PathServer.Database[AutoDb].Connected := False;
      if (ManuDb in PathServer.PathTypes) and
        (PathServer.Database[ManuDb].Connected)
      then PathServer.Database[ManuDb].Connected := False;
      if (WWicalStammDb in PathServer.PathTypes) and
        (PathServer.Database[WWicalStammDb].Connected)
      then PathServer.Database[WWicalStammDb].Connected := False;
      if (WWicalArchivDb in PathServer.PathTypes) and
        (PathServer.Database[WWicalArchivDb].Connected)
      then PathServer.Database[WWicalArchivDb].Connected := False;
      if (WAdoOPCDb in PathServer.PathTypes) and  // 11.01.2013, WW
        (PathServer.AdoConnection[WAdoOPCDb].Connected) then
        PathServer.AdoConnection[WAdoOPCDb].Connected := False;
      if (WAdoPIMMSExportDb in PathServer.PathTypes) and  // 01.03.2013, WW
        (PathServer.AdoConnection[WAdoPIMMSExportDb].Connected) then
        PathServer.AdoConnection[WAdoPIMMSExportDb].Connected := False;

      Delay(10000);

      // Erneut connecten
      if (WStammDb in PathServer.PathTypes) then
        PathServer.Database[WStammDb].Connected := True;
      if (AutoDb in PathServer.PathTypes) then
        PathServer.Database[AutoDb].Connected := True;
      if (ManuDb in PathServer.PathTypes) then
        PathServer.Database[ManuDb].Connected := True;
      if (WWicalStammDb in PathServer.PathTypes) then
        PathServer.Database[WWicalStammDb].Connected := True;
      if (WWicalArchivDb in PathServer.PathTypes) then
        PathServer.Database[WWicalArchivDb].Connected := True;
      if (WAdoOPCDb in PathServer.PathTypes) then  // 11.01.2013, WW
        PathServer.AdoConnection[WAdoOPCDb].Connected := True;
      if (WAdoPIMMSExportDb in PathServer.PathTypes) then  // 01.03.2013, WW
        PathServer.AdoConnection[WAdoPIMMSExportDb].Connected := True;

      // Jetzt OK ?
      bCheck := CheckViaFieldNames;
      if (not bCheck) then  // war wieder nix …
        Result:=-1  // DB-Verbindung gestört
      else
        Result:=1;  // DB-Verbindung wiederhergestellt
    end;
  except
    Result:=-1;  // DB-Verbindung gestört
  end;
end;

// 08.10.2015  WN
// -----------------------------------------------------------------------------
// StammDB ermitteln
// -----------------------------------------------------------------------------
function GetStammDb: TDatabase;
// -----------------------------------------------------------------------------
var
  pPathServer: TPathServer;
begin
  with TProgramIni.Create(True, False) do
  try
    pPathServer := TPathServer.Create(WieserIniFile, [WStammDb]);
    try
      try
        pPathServer.Check;
        Result := pPathServer.Database[WStammDb];
      except
        Result := nil;
      end;
    finally
      FreeAndNil(pPathServer);
    end;
  finally
    Free;
  end;
end;

{---------------------------------- Sprache -----------------------------------}

{---------------------}
procedure InitLanguage;
{---------------------}
var
  pi: TProgramIni;
  ps: TPathServer;

begin
  pi:=TProgramIni.Create;
  try
    ps:=TPathServer.Create(pi.WieserIniFile, []);
    try
      ReInit.SwitchLanguage(ps.Language);  // Sprache laden
    finally
      ps.Free;
    end;
  finally
    pi.Free;
  end;
end;

{----------------------------- TPathServerSession -----------------------------}

{---------------------------------------------------------}
constructor TPathServerSession.Create(pOwner: TComponent);
{---------------------------------------------------------}
var
  sDir : string;  // 19.08.2009
begin
  inherited;

  sDir := GetBDEPrivateDir; // 19.08.2009
  if (sDir = '') or (not DirectoryExists(sDir)) then
    PrivateDir := WTablesGetTempDir
  else
    SetCurrentDir(sDir);
end;

{---------------------------------------------------------}
destructor TPathServerSession.Destroy;
{---------------------------------------------------------}
var
  sTempDir : TFileName;
begin
  sTempDir := PrivateDir;

  inherited;

  if (FileExists(sTempDir)) and (not RemoveDir(sTempDir)) then
    WTablesInsertToDeleteFile;  // 13.03.2003
end;

{ TPathItem }

{---------------------------------------------------------}
constructor TPathItem.Create (APathType: TPathType; IniFile: TIniFile);
{---------------------------------------------------------}
begin
  inherited create;

  PathType := APathType;
  PathName := '';

  // Username auslesen
  FUserName := WGetUserName;
  if (FUserName = '') then FUserName := 'NoUser';

  InitPathName (IniFile);
  DbSection := PathName;
  FIsDatabase := CheckForDatabase(IniFile);
  if (not FIsDatabase)
  then FIsADOConnection := CheckForAdoConnection(IniFile)
  else FIsADOConnection := False;

  if (FIsDatabase) then begin
    if (PathType in [WWorkDb, WNetWorkDb]) then begin
      FDatabaseDef.DatabaseName :=
        GetUniqueDatabaseName(DatabaseDef.DatabaseName);
    end
    else begin
      PathName := GetUniqueDatabaseName(PathName);
    end;
  end;
end;

{---------------------------------------------------------}
procedure TPathItem.InitPathName (IniFile: TIniFile);
{---------------------------------------------------------}
var
  i         : TPathType;
  IniPath   : TFileName;
begin
  IniPath := ExtractFilePath (IniFile.FileName);
  for i := Low (TPathType) to High (TPathType) do
  begin
    if PathDefs [i].PathType = PathType then
    begin
      with PathDefs [i] do
      begin
        PathName := IniFile.ReadString (Section, Ident, Default);
        if Length (PathName) = 0 then PathName := IniPath;

        if (Section = C_Sect_Databases) or (Section = C_Sect_AdoConnections)
        then PathName := ChangeFileExt(ExtractFileName(PathName), '')

        else begin
          if PathName [1] = '\' then begin
    { GeDa - für Laufwerke auf anderen Rechnern }
            if (Length (PathName) > 1) and (PathName [2] <> '\') then
              PathName := copy (IniPath, 1, 2) + PathName;
          end
          else if Length (PathName) > 1 then
            if PathName [2] <> ':' then
              PathName := IniPath + PathName;

          { Verzeichnis immer mit Backslash abschließen: }
          if (Length (PathName) > 2) then begin
            if (PathType in [WWorkDir, WWorkDb]) then begin // 28.07.2004
              PathName := IncludeTrailingBackslash(PathName) + FUserName;
              ForceDirectories(PathName);
            end;
            PathName := IncludeTrailingBackslash(PathName);
          end;
        end;
      end;

      Break;
    end;
  end;
end;

{ Speichert Einstellungen für eine Datenbank  }
{ Parameter: WieserIni                        }
{ Ergebnis: Aliasname oder ''                 }
{---------------------------------------------}
function TPathItem.CreateDatabase(pIniFile: TIniFile): string;
{---------------------------------------------}

  function GetPath(iPathType: TPathType): string;
  var
    i             : TPathType;
  begin
    Result := '';
    for i := Low (TPathType) to High (TPathType) do
      if (PathDefs[i].PathType = iPathType) then begin
        Result := pIniFile.ReadString(
          PathDefs[i].Section, PathDefs[i].Ident, PathDefs[i].Default);
        Break;
      end;
    if (Result = '') or (not DirectoryExists(Result))
    then raise Exception.Create(S_PfadUngueltig)
    else Result := ExpandUNCFileName(Result);
  end;

  function GetStandardDriver(s: TFileName): string;
  begin
    s := IncludeTrailingBackslash(s);
    if (FileExists(s + '*.DB')) then Result := szPARADOX

    else if (FileExists(s + '*.DBF')) then begin
      if (FileExists(s + '*.FPT')) or (FileExists(s + '*.CDX'))
      then Result := szFOXPRO
      else Result := szDBASE;
    end

    else raise Exception.Create(S_KeineDbDateien);
  end;

var
  i     : TPathType;
  sPath : string;
  pSl   : TStrings;
begin
  Result := '';
  try
    // Pfad herausfinden
    case PathType of
      WStammDb : sPath := GetPath(WStammDir);
//      AutoDb  : sPath := GetPath(DArchivDir);
//      ManuDb  : sPath := GetPath(DManuDir);
      else Exit;
    end;

    // Alias herausfinden
    for i := Low (TPathType) to High (TPathType) do
      if (PathDefs[i].PathType = PathType) then begin
        Result := PathDefs[i].Default;
        Break;
      end;
    if (Result = '') then Exit;

    // Datenbank in BDE-Konfiguration eintragen
    pSl := TStringList.Create;
    try
      Session.GetAliasNames(pSl);
      if (pSl.IndexOf(Result) < 0) then begin
        Session.AddStandardAlias(Result, sPath, GetStandardDriver(sPath));
        Session.SaveConfigFile;
      end;
    finally
      pSl.Free;
    end;
  except
    Result := '';
  end;
end;

{------------------------------------}
function TPathItem.CheckForDatabase(pIniFile: TInifile): boolean;
{------------------------------------}
var
  i : TPathType;
  s : string;
begin
  Result := False;
  with FDatabaseDef do begin  // Vorbelegung
    DatabaseName := '';
    AliasName := '';
    UserName := '';
    Password := '';
  end;

  if (PathType in [WWorkDb, WNetWorkDb]) then begin
    Result := DirectoryExists(PathName);
    if (Result) then begin
      if (PathType = WWorkDb) then
        FDatabaseDef.DatabaseName := 'WUserDb'
      else if (PathType = WNetWorkDb) then
        FDatabaseDef.DatabaseName := 'WNetWorkDb';
    end;
  end
  else begin
    for i := Low (TPathType) to High (TPathType) do begin
      if (PathDefs [i].PathType = PathType) then begin
        with PathDefs [i] do begin
          if (Section = C_Sect_Databases) then begin
            FDatabaseDef.DatabaseName := PathName;

            // Alias aus INI holen
            s := pIniFile.ReadString(PathName, C_Ident_AliasName, '');
            // Ggf. anlegen
            if (s = '') then begin
              s := CreateDatabase(pIniFile);
              if (s <> '') then begin
                pIniFile.WriteString(Section, Ident, DbSection);
                pIniFile.WriteString(DbSection, C_Ident_AliasName, s);
              end;
            end;

            FDatabaseDef.AliasName := s;
            FDatabaseDef.UserName :=
              pIniFile.ReadString(PathName, C_Ident_UserName, '');
            FDatabaseDef.Password :=
              DecodePW(pIniFile.ReadString(PathName, C_Ident_Password, ''));
            Result := (FDatabaseDef.AliasName <> '');
          end;  // if (Section = C_Sect_Databases) then begin
        end;  // with PathDefs [i] do begin
      end;  // if (PathDefs [i].PathType = PathType) then begin
    end;  // for i := Low (TPathType) to High (TPathType) do begin
  end;
end;

{------------------------------------}
function TPathItem.CheckForAdoConnection(pIniFile: TInifile): boolean;
{------------------------------------}
var
  i : TPathType;
begin
  Result := False;
  with FDatabaseDef do begin  // Vorbelegung
    DatabaseName:='';
    AliasName:='';
    UserName:='';
    Password:='';
  end;

  for i := Low (TPathType) to High (TPathType) do
    if (PathDefs [i].PathType = PathType) then
      with PathDefs [i] do
        if (Section = C_Sect_ADOConnections) then begin
          FDatabaseDef.DatabaseName := PathName;

          // Alias aus INI holen
          FDatabaseDef.AliasName :=
            pIniFile.ReadString(PathName, C_Ident_AliasName, '');
          FDatabaseDef.UserName :=
            pIniFile.ReadString(PathName, C_Ident_UserName, '');
          FDatabaseDef.Password :=
            DecodePW(pIniFile.ReadString(PathName, C_Ident_Password, ''));
          FDatabaseDef.DescString :=
            pIniFile.ReadString(PathName, C_Ident_DescString, '');

          Result :=
            (FDatabaseDef.AliasName <> '') or (FDatabaseDef.DescString <> '');
          if (not Result) then raise Exception.Create(PathName + S_KeineADOVerb);
        end;
end;

{------------------------------------}
procedure TPathItem.Check;
{------------------------------------}
var
  sSection  : string;
  pPathType : TPathType;
begin
  try
    // Section herausfinden
    sSection := '';
    for pPathType := Low (TPathType) to High (TPathType) do
      if (PathDefs [pPathType].PathType = PathType) then begin
        sSection := PathDefs[pPathType].Section;
        Break;
      end;

    // Datenbank bzw. ADO-Connection ?
    if ((sSection = C_Sect_Databases) and (IsDatabase)) or
       ((sSection = C_Sect_ADOConnections) and (IsADOConnection)) then
    try
      if (IsADOConnection) and (DatabaseDef.DescString <> '') then begin
      // Keine Prüfung
      end
      else begin
        try
          if (Session.GetAliasDriverName(DatabaseDef.AliasName) = '') then begin
            if (IsDatabase) then
              raise Exception.Create(Format (S_DbFehlt, [DatabaseDef.AliasName]))
            else begin
            end;
          end;
        except
          if (IsADOConnection) then begin
            // Keine Prüfung
            with TADOConnection.Create(nil) do
            try
            finally
              Free;
            end;
          end;
        end;
      end;
    except
      FIsDatabase := False;
      raise Exception.Create (Format (S_DbFehlt, [DatabaseDef.AliasName]));
    end


    else if not DirectoryExists (PathName) then       { FindFirst ersetzt (kann LW-Wurzel (z.B. gemapptes G:\) nicht prüfen !) }
      raise Exception.Create (Format (S_PfadFehlt ,[PathName]));
  except
    on E:Exception do begin
      raise Exception.Create(
        'PathItem.Check (' + Self.PathName + '): ' + E.Message);
    end;
  end;
end;

{ TPathserver }

{------------------------------------}
constructor TPathServer.Create (const FileName: TFileName;
  const APathTypes: TPathTypes; bSessionOwner: boolean = True;
  bCheckAccess: boolean = True);
{------------------------------------}
const
  C_MaxLoopCount = 10;
var
  iLoopCount     : integer;
  sPCNameSection : string;
  sError         : string;

  function MyInitPathList: boolean;
  var
    i : integer;
  begin
    try
      InitPathList(APathTypes);
      Result := True;
    except
      on E:Exception do begin
        sError := E.Message;
        for i := 0 to PathList.Count-1 do
        try
          TPathItem (PathList [i]).Free;
        except
        end;
        PathList.Clear;

        // BDE entladen und neu laden
        if (Pos('BDE', sError) > 0) or
          (Pos('Borland Database Engine', sError) > 0) then
        begin
          DbiExit;
          Delay(100);
          InitializeBDE;
          Delay(100);
        end;

        Result := False;
      end;
    end;
  end;

begin
  inherited Create;

  Randomize;

  FPathTypes := APathTypes;  // 24.11.2005
  PathList := TList.Create;
  FOwnSession := bSessionOwner;  // Flag für separate Session
  FCheckAccess := bCheckAccess;  // Flag für Zugriffprüfung
  FWieserIni := TIniFile.Create (FileName);
  FBasePath := ExtractFilePath (FileName);
  FLoginDlg := true;  // Default: Login-Dialog anzeigen, wenn keine DB-Verbindung

  // Pfadliste initialisieren (Schleife bei Fehlversuch)
  iLoopCount := 1;
  while (not MyInitPathList) and (iLoopCount <= C_MaxLoopCount) do begin
    Inc(iLoopCount);
    Delay(Random(1000*iLoopCount + 1));
  end;
  if (iLoopCount > C_MaxLoopCount) then
    raise Exception.Create('TPathServer not initialized: ' + sError);

  InitLogoName;
  FDbList := TList.Create;   // 11.10.2002
  FAdoList := TList.Create;  // 27.09.2004
  FSession := nil;           // Session werden automatisch freigegeben !

  // Nachsehen, ob ggf. Computername = lokaler PC-Name ist  // 09.10.2004
  sPCNameSection := WGetComputerName;
  if (sPCNameSection <> '') then begin
    sPCNameSection := C_Sect_Hostname + WGetComputerName;
    FNetPath := FWieserIni.ReadString(sPCNameSection, C_Ident_NetPath, '');
    FLocalPath := FWieserIni.ReadString(sPCNameSection, C_Ident_LocalPath, '');
  end
  else begin
    FNetPath := '';
    FLocalPath := '';
  end;
end;

{------------------------------------}
destructor TPathServer.Destroy;
{------------------------------------}
var
  i: integer;
begin
  FWieserIni.Free;
  // PathItems freigeben
  if (Assigned(PathList)) then begin
    for i:=0 to PathList.Count-1 do
      TPathItem (PathList [i]).Free;
    FreeAndNil(PathList);
  end;
  // Datenbank-Objekte freigeben
  if (Assigned(FDbList)) then begin
    for i := 0 to FDbList.Count-1 do
      with (TDatabase(FDbList[i])) do begin
        Connected := False;
        Free;
      end;
    FreeAndNil(FDbList);
  end;
  // ADOConnection-Objekte freigeben
  if (Assigned(FAdoList)) then begin
    for i := 0 to FAdoList.Count-1 do
      with (TADOConnection(FAdoList[i])) do begin
        Connected := False;
        Free;
      end;
    FreeAndNil(FAdoList);
  end;

  inherited Destroy;
end;

{------------------------------------}
function TPathServer.SetNetPath(sFilePath: TFileName): TFileName;
{------------------------------------}
begin
  Result := sFilePath;
  // Groß-/Kleinschreibung bei Pos-Suche nicht berücksichtigen; 12.07.2005 WW
  if (FNetPath <> '') and (FLocalPath <> '') and
     (Pos(LowerCase(FLocalPath), LowerCase(Result)) = 1)
  then Result := StringReplace(Result, FLocalPath, FNetPath, [rfIgnoreCase]);
end;

{------------------------------------}
function TPathServer.SetLocalPath(sFilePath: TFileName): TFileName;
{------------------------------------}
begin
  Result := sFilePath;
  // Groß-/Kleinschreibung bei Pos-Suche nicht berücksichtigen; 12.07.2005 WW
  if (FNetPath <> '') and (FLocalPath <> '') and
     (Pos(LowerCase(FNetPath), LowerCase(Result)) = 1)
  then Result := StringReplace(Result, FNetPath, FLocalPath, [rfIgnoreCase]);
end;

{------------------------------------}
function TPathServer.GetPathName (Index: TPathType): TFileName;
{------------------------------------}
var
  i: Integer;
begin
  if (not (Index in FPathTypes)) then InsertPathItem(Index);

  for i := 0 to PathList.Count - 1 do
  begin
    if TPathItem (PathList [i]).PathType = Index then
    begin
      Result := TPathItem (PathList [i]).PathName;

      // Groß-/Kleinschreibung bei Pos-Suche nicht berücksichtigen; 12.07.2005 WW
      if (FNetPath <> '') and (Pos(LowerCase(FNetPath), LowerCase(Result)) = 1) then
        Result := StringReplace(Result, FNetPath, FLocalPath, [rfIgnoreCase]);
      Exit;
    end;
  end;
  raise Exception.Create('GetPathName (' +
    IntToStr(Integer(Index)) + ') - ' + S_PathTypeInitErr);
end;

{------------------------------------}
procedure TPathServer.SetPathName (Index: TPathType; Value: TFileName);
{------------------------------------}
var
  i: Integer;
  j: TPathType;
begin
  if (not (Index in FPathTypes)) then InsertPathItem(Index);

  if Value[length(Value)] <> '\' then Value:=Value + '\';
  for i := 0 to PathList.Count - 1 do
  begin
    if TPathItem (PathList [i]).PathType = Index then
    begin
      { PathList akualisieren: }
      TPathItem (PathList [i]).PathName:=Value;
      { WIESER.INI akualisieren: }
      for j := Low (TPathType) to High (TPathType) do
      begin
        if PathDefs [j].PathType = TPathItem (PathList [i]).PathType then
        begin
          if length(Value) > 3 then Value:=copy(Value,1,length(Value)-1);
          with PathDefs [j] do
            FWieserIni.WriteString (Section, Ident, Value);
          exit;
        end;
      end;
    end;
  end;
  raise Exception.Create('SetPathName (' +
    IntToStr(Integer(Index)) + ') - ' + S_PathTypeInitErr);
end;

{------------------------------------}
procedure TPathserver.InitPathList (const APathTypes: TPathTypes);
{------------------------------------}
var
  i: TPathType;
begin
  for i := Low (TPathType) to High (TPathType) do
  try
    if (i in APathTypes) then
      PathList.Add (TPathItem.Create (i, FWieserIni));
  except
    on E: Exception do
      raise Exception.Create('InitPathList ' + S_PathTypeInitErr + ' (' +
        IntToStr(Integer(i)) + '): ' + E.Message);
  end;
end;

{------------------------------------}
procedure TPathServer.InitLogoName;
{------------------------------------}
begin
  FLogoName := FBasePath + FWieserIni.ReadString ('Wieser', 'Logo', 'Logo.BMP');
end;

{------------------------------------}
function TPathServer.RMaxKanaele: word;
{------------------------------------}
begin
  Result:=FWieserIni.ReadInteger('Wieser', 'Kanalzahl', 28);
end;

{------------------------------------}
function TPathServer.RShowZpkt: Boolean;  // 27.01.2011  WN
{------------------------------------}
begin
  Result:=FWieserIni.ReadBool('Wieser', 'ShowZpkt', False);
end;

{-----------------------------------}
function TPathServer.RLanguage: LCID;  // 20.11.2007, WW
{-----------------------------------}
begin
  Result:=FWieserIni.ReadInteger('Wieser', 'Language', GERMAN);
end;

{ Ereignis zum Login der Datenbank            }
{ Parameter: Datenbank, Parameter             }
{---------------------------------------------}
procedure TPathServer.DbLogin(pDatabase: TDatabase; pLoginParams: TStrings);
{---------------------------------------------}
var
  sUserName, sPassWord : string;
begin
  sUserName := pLoginParams.Values[szUSERNAME];
  if (not LoginDialogEx(pDatabase.AliasName, sUserName, sPassword, False))
  then DatabaseErrorFmt(SLoginError, [pDatabase.DatabaseName]);
  pLoginParams.Values[szUSERNAME] := sUserName;
  pLoginParams.Values[szPASSWORD] := sPassword;
  pDatabase.Params.Values[szUSERNAME] := sUserName;
  pDatabase.Params.Values[szPASSWORD] := sPassword;

  FWieserIni.WriteString(
    pDatabase.Params.Values[C_DbSection], C_Ident_UserName, sUserName);
  FWieserIni.WriteString(
    pDatabase.Params.Values[C_DbSection], C_Ident_Password, EncodePW(sPassWord));
end;

{ Ereignis zum Login der ADO-Conection        }
{ Parameter: Datenbank-Objekt, User, Passwort, DB-Section }
{---------------------------------------------}
procedure TPathServer.AdoLogin(Sender: TObject;
  sUsername, sPassword, sDbSection: string);
{---------------------------------------------}
var
  s, sCtn, sDSrc, sICat, sUser, sPw : string;
  i, iPos                                       : integer;
begin
  if (Sender is TADOConnection) then begin
    with (Sender as TADOConnection) do begin
      // Interne Bezeichnung
      s := ConnectionString;
      with TStringList.Create do
      try
        iPos := Pos(';', s);
        while (iPos > 0) do begin
          if (iPos > 1) then Add(Copy(s, 1, iPos-1));
          System.Delete(s, 1, iPos);
          iPos := Pos(';', s);
        end;
        if (Trim(s) <> '') then Add(Trim(s));
        s := '';

        for i := 0 to Count-1 do
          if (Names[i] <>  C_AdoIdent_WDbSection) then
            s := s + Strings[i] + ';';
        sCtn := Copy(s, 1, Length(s) - 1);
      finally
        Free;
      end;

      // sDbSection wird jetzt im Prozedur-Parameter übergeben; 30.07.2013, WW
      sUser := sUsername;
      sPw := sPassword;  // 30.07.2013, WW
      BuildADOConnectionString(sCtn, sDSrc, sICat, sUser, sPw);
      if FLoginDlg then  // 18.01.2013, WW
        EditADOConnectionString(Provider, 'Connection' + ' (' + sDbSection + ')',
                                sCtn);  // mit DbSection-Anzeige; 15.02.2013,WW
      sDSrc := ''; sICat := ''; sUser := ''; sPw := '';
      BuildADOConnectionString(sCtn, sDSrc, sICat, sUser, sPw);

      // Login-Dialog
      if (not FLoginDlg) or  // 18.01.2013, WW
         (not LoginDialog(sDSrc + ' ' + sICat, sUser, sPw))
      then DatabaseErrorFmt(SLoginError, [Provider]);

      if FLoginDlg then begin  // 15.07.2013, WW
        // Eintrag in WIESER.INI
        if length (sDbSection) > 0 then begin  // sonst Absturz in NTDll.dll; 27.12.2012 WW
          FWieserIni.WriteString(sDbSection, C_Ident_AliasName, sDSrc);
          FWieserIni.WriteString(sDbSection, C_Ident_UserName, sUser);
          FWieserIni.WriteString(sDbSection, C_Ident_Password, EncodePW(sPw));
          FWieserIni.WriteString(sDbSection, C_Ident_DescString, sCtn);
        end;

        BuildADOConnectionString(sCtn, sDSrc, sICat, sUser, sPw);
        ConnectionString := sCtn;
      end;
    end;
  end;
end;

{ Prüft Zugriffsrechte               }
{------------------------------------}
procedure TPathServer.CheckAccess(pPathItem: TPathItem);
{------------------------------------}
var
  bRes  : boolean;
  iFile : integer;
  sFile : TFileName;
  cChar : char;
begin
  if (FCheckAccess) and (not (pPathItem.PathType in [])) then   // Hier Pathtypes einfügen, die nicht geprüft werden sollen
  try
    bRes := True;  // alles i.O.
    if (pPathItem.IsDatabase) then begin  // Datenbank
      Delay(10);
      // Tabelle erzeugen, schreiben, löschen
      with TTable.Create(nil) do
      try
        DatabaseName := Self.Database[pPathItem.PathType].DatabaseName;
        SessionName := Self.Database[pPathItem.PathType].SessionName;
        iFile := Integer(Self) +
          StrToInt(FormatdateTime('nsz', Now + (Random(1000)/123456)));
        TableName := 't' + IntToStr(iFile);
        while (Exists) do begin
          Inc(iFile);
          TableName := 't' + IntToStr(iFile);
        end;
        FieldDefs.Add('aint', ftInteger, 0, False);
        CreateTable;
        Application.ProcessMessages;
        Delay(10);
        Open;
        AppendRecord([0]);
        Close;
        Delay(10);
        Application.ProcessMessages;
        Delay(10);
        DeleteTable;
      finally
        Free;
      end;
    end
    else begin
      // Datei erzeugen, schreiben, lesen, löschen
      iFile := Integer(Self) +
        StrToInt(FormatdateTime('nsz', Now + (Random(1000)/123456)));
      sFile := IncludeTrailingBackslash(pPathItem.PathName);
      while (FileExists(sFile + IntToStr(iFile) + '.TST')) do Inc(iFile);
      sFile := sFile + IntToStr(iFile) + '.TST';
      with TFileStream.Create(sFile, fmCreate) do Free;
      if (FileExists(sFile)) then begin
        Delay(10);
        with TFileStream.Create(sFile, fmOpenReadWrite OR fmShareDenyWrite) do
        try
          cChar := 'T';
          Write(cChar, 1);
        finally
          Free;
        end;
        Application.ProcessMessages;
        Delay(10);
        with TFileStream.Create(sFile, fmOpenRead OR fmShareDenyWrite) do
        try
          cChar := ' ';
          Read(cChar, 1);
        finally
          Free;
        end;
        Delay(10);
        bRes := DeleteFile(sFile) and (cChar = 'T');
      end
      else bRes := False;
    end;
    if (not bRes) then raise Exception.Create(
      pPathItem.PathName + ': ' + Format(S_NoAccess, [pPathItem.PathName]));
  except
    on E:Exception do raise Exception.Create(
      pPathItem.PathName + ': ' + Format(S_NoAccess, [E.Message]));
  end;
end;

{------------------------------------}
procedure TPathServer.Check;
{------------------------------------}
var
  i, j : integer;
  s : string;
begin
  for i := 0 to PathList.Count - 1 do
  try
    with TPathItem (PathList [i]) do begin
      Check;
      if (IsDatabase) then begin
        Self.FDbList.Add(TDatabase.Create(nil));
        Self.CreateSession;
        with TDatabase(Self.FDbList[FDbList.Count-1]) do begin
          if (PathType in [WWorkDb, WNetWorkDb]) then begin
            Params.Values[szCFGDBPATH] := PathName;
            DriverName := szCFGDBSTANDARD;
            DatabaseName := DatabaseDef.DatabaseName;
            SessionName := Self.FSession.SessionName;
            TransIsolation := tiDirtyRead;
            Open;
          end
          else begin
            OnLogin := DbLogin;
            DatabaseName := PathName;
            SessionName := Self.FSession.SessionName;
            if (IsSQLBased)
            then TransIsolation := tiReadCommitted
            else TransIsolation := tiDirtyRead;  // 16.11.2003
            AliasName := DatabaseDef.AliasName;
            KeepConnection := True;
            Params.Values[szUSERNAME] := DatabaseDef.Username;
            Params.Values[szPASSWORD] := DatabaseDef.Password;
            Params.Values[C_DbSection] := DbSection;

            LoginPrompt := False;
            try
              Delay(10);
              Open;
            except
              if (not Connected) then begin
                if FLoginDlg then  // 18.01.2013, WW
                  LoginPrompt := True;
                Delay(10);
                Open;
              end;
            end;
          end;
        end;
      end
      else if (IsAdoConnection) then begin
        Self.FAdoList.Add(TADOConnection.Create(nil));
        with TADOConnection(Self.FAdoList[FAdoList.Count-1]) do begin
          Name := PathName;
          try
            // Connectionstring erstellen
            with TStringList.Create do
            try
              // Gespeicherten String decodieren
              if (DatabaseDef.DescString <> '') then begin
                s := DatabaseDef.DescString;
                j := Pos(';', s);
                while (j > 0) do begin
                  if (j > 1) then Add(Copy(s, 1, j-1));
                  System.Delete(s, 1, j);
                  j := Pos(';', s);
                end;
                if (Trim(s) <> '') then Add(Trim(s));
              end
              else begin
                Values[C_AdoIdent_Provider] := C_AdoValue_Provider;
                Values[C_AdoIdent_PersistSecInfo] := C_AdoValue_PersistSecInfo;
                Values[C_AdoIdent_DataSource] := DatabaseDef.AliasName;
              end;
              Values[C_AdoIdent_UserName] := DatabaseDef.Username;
              Values[C_AdoIdent_Password] := DatabaseDef.Password;
              Values[C_AdoIdent_WDbSection] := DbSection;
              s := '';
              for j := 0 to Count-1 do s := s + Strings[j] + ';';
              ConnectionString := Copy(s, 1, Length(s) - 1);
{$IFDEF ONLY_MSACCESS}
              // Hack, damit MS Access DB funktioniert; 07.03.2013, GD/WW
              ConnectionString := DatabaseDef.DescString;
{$ENDIF}
            finally
              Free;
            end;

            LoginPrompt := False;
            Delay(10);
            Open;
          except
            if (not Connected) then begin
              AdoLogin(TADOConnection(Self.FAdoList[FAdoList.Count-1]),
                DatabaseDef.Username, DatabaseDef.Password, DbSection); 
              Delay(10);
              Open;
            end;
          end;
        end;
      end;
    end;
    CheckAccess(TPathItem(PathList [i]));
  except
    on E:Exception do begin
      raise Exception.Create('PathServer.Check (' +
        TPathItem(PathList [i]).PathName + '): ' + E.Message);
    end;
  end;
end;

{------------------------------------}
function TPathServer.InsertPathItem(iIndex: TPathType): boolean;
{------------------------------------}
var
  pItem : TPathItem;
  j : integer;
  s : string;
begin
  try
    pItem := TPathItem.Create(iIndex, FWieserIni);
    PathList.Add(pItem);
    with pItem do begin
      Check;
      if (IsDatabase) then begin
        Self.FDbList.Add(TDatabase.Create(nil));
        Self.CreateSession;
        with TDatabase(Self.FDbList[FDbList.Count-1]) do begin
          if (PathType in [WWorkDb, WNetWorkDb]) then begin
            Params.Values[szCFGDBPATH] := PathName;
            DriverName := szCFGDBSTANDARD;
            DatabaseName := DatabaseDef.DatabaseName;
            SessionName := Self.FSession.SessionName;
            TransIsolation := tiDirtyRead;
            Open;
          end
          else begin
            OnLogin := DbLogin;
            DatabaseName := PathName;
            SessionName := Self.FSession.SessionName;
            if (IsSQLBased)
            then TransIsolation := tiReadCommitted
            else TransIsolation := tiDirtyRead;  // 16.11.2003
            AliasName := DatabaseDef.AliasName;
            KeepConnection := True;
            Params.Values[szUSERNAME] := DatabaseDef.Username;
            Params.Values[szPASSWORD] := DatabaseDef.Password;
            Params.Values[C_DbSection] := DbSection;

            LoginPrompt := False;
            try
              Delay(10);
              Open;
            except
              if (not Connected) then begin
                if FLoginDlg then  // 18.01.2013, WW
                  LoginPrompt := True;
                Delay(10);
                Open;
              end;
            end;
          end;
        end;
      end
      else if (IsAdoConnection) then begin
        Self.FAdoList.Add(TADOConnection.Create(nil));
        with TADOConnection(Self.FAdoList[FAdoList.Count-1]) do begin
          Name := PathName;
          try
            // Connectionstring erstellen
            with TStringList.Create do
            try
              // Gespeicherten String decodieren
              if (DatabaseDef.DescString <> '') then begin
                s := DatabaseDef.DescString;
                j := Pos(';', s);
                while (j > 0) do begin
                  if (j > 1) then Add(Copy(s, 1, j-1));
                  System.Delete(s, 1, j);
                  j := Pos(';', s);
                end;
                if (Trim(s) <> '') then Add(Trim(s));
              end
              else begin
                Values[C_AdoIdent_Provider] := C_AdoValue_Provider;
                Values[C_AdoIdent_PersistSecInfo] := C_AdoValue_PersistSecInfo;
                Values[C_AdoIdent_DataSource] := DatabaseDef.AliasName;
              end;
              Values[C_AdoIdent_UserName] := DatabaseDef.Username;
              Values[C_AdoIdent_Password] := DatabaseDef.Password;
              Values[C_AdoIdent_WDbSection] := DbSection;
              s := '';
              for j := 0 to Count-1 do s := s + Strings[j] + ';';
              ConnectionString := Copy(s, 1, Length(s) - 1);
            finally
              Free;
            end;

            LoginPrompt := False;
            Delay(10);
            Open;
          except
            if (not Connected) then begin
              AdoLogin(TADOConnection(Self.FAdoList[FAdoList.Count-1]),
                DatabaseDef.Username, DatabaseDef.Password, DbSection);
              Delay(10);
              Open;
            end;
          end;
        end;
      end;
    end;
    CheckAccess(pItem);
    Result := True;
  except
    Result := False;
(*
    on E:Exception do begin
      raise Exception.Create('PathServer.InsertPathItem (' +
        pItem.PathName + '): ' + E.Message);
    end;
*)    
  end;
end;

{------------------------------------}
procedure TPathServer.CreateSession;
{------------------------------------}
begin
  if (not Assigned(Self.FSession)) then begin
    if (FOwnSession) then begin  // Session wird bei Programmende freigegeben
      FSession := TPathServerSession.Create(nil);
      FSession.SessionName := 'pssn' + IntToStr(Integer(FSession));
    end
    else FSession := DbTables.Session;
  end;
end;

{ Datenbank wir übergeben            }
{ Parameter: Path-Typ                }
{ Rückgabe: Zeiger auf die Datenbank }
{------------------------------------}
function TPathServer.ReadDatabase(iIndex: TPathType): TDatabase;
{------------------------------------}
var
  i, j : integer;
begin
  Result := nil;

  if (not (iIndex in FPathTypes)) then InsertPathItem(iIndex);

  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        //  Datenbank übernehmen
        for j := 0 to FDbList.Count-1 do
          if (TDatabase(FDbList[j]).DatabaseName = PathName) or
             (TDatabase(FDbList[j]).DatabaseName = DatabaseDef.DatabaseName)
          then begin
            Result := TDatabase(FDbList[j]);
            Exit;
          end;

        if (Assigned(Result)) then Exit else Break;
      end;
    end;
  end;

  raise Exception.Create('ReadDatabase (' +
    IntToStr(Integer(iIndex)) + ') - ' + S_PathTypeInitErr);
end;

{ ADO-Connection wir übergeben       }
{ Parameter: Path-Typ                }
{ Rückgabe: Zeiger auf die Connection}
{------------------------------------}
function TPathServer.ReadAdoConnection(iIndex: TPathType): TADOConnection;
{------------------------------------}
var
  i, j : integer;
begin
  Result := nil;

  if (not (iIndex in FPathTypes)) then InsertPathItem(iIndex);

  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        //  Datenbank übernehmen
        for j := 0 to FAdoList.Count-1 do
          if (TADOConnection(FAdoList[j]).Name = PathName) then begin
            Result := TADOConnection(FAdoList[j]);
            Exit;
          end;

        if (Assigned(Result)) then Exit else Break;
      end;
    end;
  end;

  raise Exception.Create('ReadAdoConnection (' +
    IntToStr(Integer(iIndex)) + ') - ' + S_PathTypeInitErr);
end;

{ Datenbank wir übergeben            }
{ Pathserver-Objekte werden gelöscht }
{------------------------------------}
function TPathServer.GetDatabase(iIndex: TPathType): TDatabase;
{------------------------------------}
var
  i, j : integer;
begin
  Result := nil;

  if (not (iIndex in FPathTypes)) then InsertPathItem(iIndex);

  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        //  Datenbank übernehmen
        for j := 0 to FDbList.Count-1 do
          if (TDatabase(FDbList[j]).DatabaseName = PathName) or
             (TDatabase(FDbList[j]).DatabaseName = DatabaseDef.DatabaseName)
          then begin
            Result := TDatabase(FDbList[j]);
            // Datenbank aus Liste löschen
            FDbList.Delete(j);
            // PathItem freigeben
            Self.PathList.Delete(i);
            // Datenbank mit neuem Namen versehen
            Result.Connected := False;
            Result.DatabaseName := 'db' + IntToStr(Integer(Result));
            Delay(10);
            Result.Connected := True;
            Free;
            Exit;
          end;

        if (Assigned(Result)) then Exit else Break;
      end;
    end;
  end;

  raise Exception.Create('GetDatabase (' +
    IntToStr(Integer(iIndex)) + ') - ' + S_PathTypeInitErr);
end;

{ Datenbank-Conection wir übergeben  }
{ Pathserver-Objekte werden gelöscht }
{------------------------------------}
function TPathServer.GetAdoConnection(iIndex: TPathType): TADOConnection;
{------------------------------------}
var
  i, j : integer;
begin
  Result := nil;

  if (not (iIndex in FPathTypes)) then InsertPathItem(iIndex);

  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        //  ADOConnection übernehmen
        for j := 0 to FAdoList.Count-1 do
          if (TADOConnection(FAdoList[j]).Name = PathName) then begin
            Result := TADOConnection(FAdoList[j]);
            // Datenbank aus Liste löschen
            FAdoList.Delete(j);
            // PathItem freigeben
            Self.PathList.Delete(i);
            Free;
            Exit;
          end;

        if (Assigned(Result)) then Exit else Break;
      end;
    end;
  end;

  raise Exception.Create('GetAdoConnection (' +
    IntToStr(Integer(iIndex)) + ') - ' + S_PathTypeInitErr);
end;

{------------------------------------}
function TPathServer.GetPathItem(iIndex: TPathType): TPathItem;
{------------------------------------}
var
  i : integer;
begin
  if (not (iIndex in FPathTypes)) then InsertPathItem(iIndex);

  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        Result := TPathItem (PathList [i]);
        Exit;
      end;
    end;
  end;

  raise Exception.Create('GetPathItem (' +
    IntToStr(Integer(iIndex)) + ') - ' + S_PathTypeInitErr);
end;

{------------------------------------}
function TPathServer.GetAliasName(iIndex: TPathType): string;
{------------------------------------}
var
  i : integer;
begin
  Result := '';

  if (not (iIndex in FPathTypes)) then InsertPathItem(iIndex);

  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        //  Datenbank übernehmen
        if (PathDefs[iIndex].Section = C_Sect_Databases) or
           (PathDefs[iIndex].Section = C_Sect_AdoConnections) then
        begin
          Result := DatabaseDef.AliasName;
          Exit;
        end;
      end;
    end;
  end;

  raise Exception.Create('GetAliasName (' +
    IntToStr(Integer(iIndex)) + ') - ' + S_PathTypeInitErr);
end;

{------------------------------------}
function TPathServer.GetDbDirectory(iIndex: TPathType): string;
{------------------------------------}
var
  i, j : integer;
  pSl  : TStrings;
begin
  Result := '';

  if (not (iIndex in FPathTypes)) then InsertPathItem(iIndex);

  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        //  Datenbank übernehmen
        for j := 0 to FDbList.Count-1 do
          if (TDatabase(FDbList[j]).DatabaseName = PathName) and
             (not TDatabase(FDbList[j]).IsSQLBased) then
          begin
            pSl := TStringList.Create;
            try
              Session.GetAliasParams(TDatabase(FDbList[j]).AliasName, pSl);
              Result := pSl.Values['PATH'];
            finally
              pSl.Free;
            end;
            Break;
          end;

        Exit;
      end;
    end;
  end;

  raise Exception.Create('GetDbDirectory (' +
    IntToStr(Integer(iIndex)) + ') - ' + S_PathTypeInitErr);
end;

{--------------------------------------------------------------}
function TPathServer.GetIPPortNr_ab (Hostname: string): integer;
{--------------------------------------------------------------}
var
  Section_Host: string;
begin
  Section_Host:=C_Sect_Hostname + Hostname;
  Result:=FWieserIni.ReadInteger(Section_Host, C_Ident_IPPortNr_ab, CIPPortWMsg_ab_Default);
end;


{ TProgramIni }

{------------------------------------}
constructor TProgramIni.Create(bNetIni: boolean = True;
  bUserDefined: boolean = False; sFilename : TFilename='');
{------------------------------------}
var
  pIniFile : TIniFile;
  p        : PChar;
  i        : DWord;
begin
  if (bNetIni) then begin  // INI-File im NetProgDir ?
    // Ort der Wieser.INI ermitteln, WieserItem initialisieren

    if sFilename = '' then   // 22.03.2010  WN
      pIniFile := TIniFile.Create(ChangeFileExt (Application.ExeName, '.INI'))
    else
      pIniFile := TIniFile.Create(sFilename+'.INI');
    try
      WieserItem := TPathItem.Create (WieserIni, pIniFile);
    finally
      pIniFile.Free;
    end;

    // Zugriff auf Wieser.INI für NetProgDir
    pIniFile := TIniFile.Create(WieserItem.PathName + CWieserIni);
    try
      with TPathItem.Create (WNetProgDir, pIniFile) do
      try
        // INI auf Netzlaufwerk initialisieren
        if sFilename = '' then   // 22.03.2010  WN
          inherited Create(
            PathName + (ChangeFileExt(ExtractFileName(ParamStr(0)), '.INI')))
        else
          inherited Create(
            PathName + (ChangeFileExt(ExtractFileName(sFileName), '.INI')));
      finally
        Free;
      end;
    finally
      pIniFile.Free;
    end;

  end
  else begin
    if sFilename = '' then   // 22.03.2010  WN
      inherited Create (ChangeFileExt (Application.ExeName, '.INI'))
    else
      inherited Create (sFileName+'.INI');
    WieserItem := TPathItem.Create (WieserIni, Self);
  end;
  FUserDefined := bUserDefined;
  ReadFromIni;
  // Username auslesen
  try
    GetMem(p, 100);
    try
      p[0] := #0;
      i := 99;
      GetUserName(p, i);
      FUserName := StrPas(p);
    finally
      FreeMem(p, 100);
    end;
  except
    FUserName := '';
  end;
  if (FUserName = '') then FUserName := 'NoUser';
end;

{------------------------------------}
destructor TProgramIni.Destroy;
{------------------------------------}
begin
  WieserItem.Free;
  inherited Destroy;
end;

{------------------------------------}
procedure TProgramIni.ReadFromIni;
{------------------------------------}
begin
end;

{------------------------------------}
function TProgramIni.GetWieserIni: TFileName;
{------------------------------------}
begin
  Result := WieserItem.PathName + CWieserIni;
end;

{------------------------------------}
function TProgramIni.ReadString(const Section, Ident, Default: string): string;
{------------------------------------}
var
  s : string;
begin
  s := Section;
  if (FUserDefined) and (Pos('-' + FUserName, s) = 0) then
    s := s + '-' + FUserName;

  Result := inherited ReadString(s, Ident, Default);
end;

{------------------------------------}
procedure TProgramIni.WriteString(const Section, Ident, Value: String);
{------------------------------------}
var
  s : string;
begin
  s := Section;
  if (FUserDefined) and (Pos('-' + FUserName, s) = 0) then
    s := s + '-' + FUserName;

  inherited WriteString(s, Ident, Value);
end;

{------------------------------------}
procedure TProgramIni.ReadSection(const Section: string; Strings: TStrings);
{------------------------------------}
var
  s : string;
begin
  s := Section;
  if (FUserDefined) and (Pos('-' + FUserName, s) = 0) then
    s := s + '-' + FUserName;

  inherited ReadSection(s, Strings);
  s := IntToStr(Strings.Count);
end;

{------------------------------------}
procedure TProgramIni.ReadSectionValues(const Section: string; Strings: TStrings);
{------------------------------------}
var
  s : string;
begin
  s := Section;
  if (FUserDefined) and (Pos('-' + FUserName, s) = 0) then
    s := s + '-' + FUserName;

  inherited ReadSectionValues(s, Strings);
  s := IntToStr(Strings.Count);
end;

{------------------------------------}
function TProgramIni.SectionExists(const Section: string): Boolean;
{------------------------------------}
var
  s : string;
begin
  s := Section;
  if (FUserDefined) and (Pos('-' + FUserName, s) = 0) then
    s := s + '-' + FUserName;

  Result := inherited SectionExists(s);
end;

{------------------------------------}
procedure TProgramIni.EraseSection(const Section: string);
{------------------------------------}
var
  s : string;
begin
  s := Section;
  if (FUserDefined) and (Pos('-' + FUserName, s) = 0) then
    s := s + '-' + FUserName;

  inherited EraseSection(s);
end;

{------------------------------------}
procedure TProgramIni.DeleteKey(const Section, Ident: String);
{------------------------------------}
var
  s : string;
begin
  s := Section;
  if (FUserDefined) and (Pos('-' + FUserName, s) = 0) then
    s := s + '-' + FUserName;

  inherited DeleteKey(s, Ident);
end;

{------------------------------------}
procedure TProgramIni.LoadActUserSettings;
{------------------------------------}
var
  sPrintername  : string;
  iPrinterIndex : integer;
begin
  // Druckereinstellungen
  sPrintername := inherited
    ReadString(C_Sect_UserSettings + FUserName, C_Ident_UserPrinter, '');
  if (sPrinterName <> '') then begin
    iPrinterIndex := Printer.Printers.IndexOf(sPrintername);
    if (iPrinterIndex >= 0) then Printer.PrinterIndex := iPrinterIndex;
  end;
end;

{------------------------------------}
procedure TProgramIni.SaveActUserSettings;
{------------------------------------}
var
  sPrintername  : string;
begin
  // Druckereinstellungen
  if (Printer.PrinterIndex < 0)
  then sPrintername := ''
  else sPrintername := Printer.Printers[Printer.PrinterIndex];
  inherited WriteString(
    C_Sect_UserSettings + FUserName, C_Ident_UserPrinter, sPrintername);
end;

end.
