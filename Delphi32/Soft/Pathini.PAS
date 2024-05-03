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
{ 08.06.2004 GD; Generierung von Datenbanken verbessert                        }
{ 27.09.2004 GD; Erweitert um ADOConnections                                   }
{ 09.10.2004 GD; Netzlaufwerke werden ggf. in lokale umbenannt                 }
{ 15.08.2004 WW; Rechner-spezifische Einstellungen (IPPortNr_ab)               }
{------------------------------------------------------------------------------}

unit Pathini;

interface

uses
  Windows, Classes, Forms, IniFiles, SysUtils, FileCtrl, DbTables, Bde, Db,
  Dblogdlg, BdeConst, AdoDb, WPorts;

type
  // Datenbankdefinition
  TDatabaseDef = record    // 11.10.2002
    DatabaseName : string; { Datenbankname }
    AliasName    : string; { Aliasname der Database }
    UserName     : string; { Benutzername }
    Password     : string; { Passwort }
  end;

  { Pfadbezeichner }

  TPathType = (
    WieserIni,       { Verzeichnis, in dem Wieser.ini vorhanden ist }
    AsciiDir,        { Verzeichnis für Ascii-Daten }
    LangzeitDir,     { Langzeitverzeichnis }
    ManuDir,         { Verzeichnis manuell abgerufener Daten }
    LArchivDir,      { Laks, Archiv-Verzeichnis }
    LDPDatenDir,     { Laks, Datenverzeichnis für Dialog }
    LSysdatDir,      { Laks, Daten-Systemverzeichnis }
    LExportDir,      { Laks, Exportverzeichnis }
    LAuswStammDir,   { Laks, Auswert-Stammdatenverzeichnis }
    WProgDir,        { Exe-Dateien }
    WNetProgDir,     { Exe-Dateien, Netz }
    WStammDir,       { Mrg-Stammdaten }
    WWorkDir,        { WinDFÜ-Arbeitsverzeichnis, lokal }
    WNetWorkDir,     { WinDFÜ-Arbeitsverzeichnis, Netz }
    WVoiceDir,       { Verzeichnis der Voice-Files }

    WStammDb,        { Alias - Datenbankname für WIESER-Stammdaten }  // 11.10.2002
    AutoDb,          { Alias - Datenbankname für Auto-Daten }  // 11.10.2002
    ManuDb,          { Alias - Datenbankname für Manu-Daten }  // 11.10.2002
    WBDEDb,          { Alias - Allgemein definierte BDE-Verbindung }  // 27.09.2004

    WADODb,           { Alias - Allgemein definierte ADO-Verbindung }  // 27.09.2004

    // Nur noch für Abwärtskompatibilität benötigt
    DArchivDir,      { DSFG-Archivdatenverzeichnis, automatisch }
    DManuDir        { DSfG-Archivdatenverzeichnis, manuell }
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

  TPathServer = class (TObject)
  private
    PathList   : TList;
    FWieserIni : TIniFile;
    FLogoName  : TFileName;
    FBasePath  : TFileName;
    FDbList    : TList;
    FAdoList   : TList;   // 27.09.2004
    FNetPath   : string;  // 09.10.2004
    FLocalPath : string;  // 09.10.2004
    function GetDbDirectory(iIndex: TPathType): string;
    function GetAliasName(iIndex: TPathType): string;
    function GetPathName (Index: TPathType): TFileName;
    function ReadDatabase(iIndex: TPathType): TDatabase;
    function ReadAdoConnection(iIndex: TPathType): TAdoConnection;
    procedure SetPathName (Index: TPathType; Value: TFileName);
    procedure InitPathList (const APathTypes: TPathTypes);
    procedure InitLogoName;
    function RMaxKanaele: word;
    procedure DbLogin(pDatabase: TDatabase; pLoginParams: TStrings); // 11.10.2002
    procedure AdoLogin(Sender:TObject; sUsername, sPassword: string); // 27.09.2004
    function GetIPPortNr_ab (Hostname: string): integer;
    function GetPathItem(iIndex: TPathType): TPathItem;
  public
    constructor Create (const FileName: TFileName;
                        const APathTypes: TPathTypes);
    destructor Destroy; override;
    procedure Check;
    function GetDatabase(iIndex: TPathType): TDatabase;
    property LogoName: TFileName read FLogoName;
    property MaxKanaele: word read RMaxKanaele;
    property Pathname [Index: TPathType]: TFileName
      read GetPathName write SetPathName; default;
    property Database [Index: TPathType]: TDatabase read ReadDatabase;
    property AdoConnection [Index: TPathType]: TAdoConnection
      read ReadAdoConnection;
    property PathItem [iIndex: TPathType]: TPathItem read GetPathItem;
    property AliasName [iIndex: TPathType]: string read GetAliasName;
    property DbDirectory [iIndex: TPathType]: string read GetDbDirectory;
    property IPPortNr_ab [Hostname: string]: integer read GetIPPortNr_ab;
  end;

  TProgramIni = class (TIniFile)
  private
    WieserItem: TPathItem;
    function GetWieserIni: TFileName;
  protected
    procedure ReadFromIni; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property WieserIniFile: TFileName read GetWieserIni;
  end;

var
  PathServer: TPathServer;

implementation

resourcestring
  S_PfadUngueltig       = 'Kein gültiger Pfad vorhanden !';
  S_KeinAliasDefiniert  = ': Kein Alias definiert !';                            
  S_DbFehlt             = 'Datenbank %s nicht vorhanden';
  S_PfadFehlt           = 'Pfad %s nicht vorhanden';
  S_PathTypeInitErr     = 'TPathServer: PathType nicht initialisiert';
  S_ErrDecodeConnection = 'ADO-Verbindungsinformation konnte nicht entschlüsselt werden !';

type

  TPathDef = record
    PathType: TPathType;   { Bezeichner }
    Section: string;       { Section in Wieser.ini }
    Ident: string;         { Ident in Wieser.ini }
    Default: string;       { Voreinstellung }
  end;

const

  CWieserIni       = 'WIESER.INI';

  C_Sect_Databases = 'Databases';
  C_Sect_ADOConnections = 'ADOCONNECTIONS';
  C_Sect_Hostname  = 'Hostname:';

  C_Ident_AliasName = 'ALIASNAME';
  C_Ident_UserName = 'USERNAME';
  C_Ident_Password = 'PASSWORD';

  C_Ident_IPPortNr_ab = 'IPPortNr_ab';

  C_Ident_NetPath   = 'NETPATH';
  C_Ident_LocalPath = 'LOCALPATH';

  C_DbSection      = 'DBSECTION';

  // ADO-ConnectionString-Einzelteile
  C_AdoIdent_Provider   = 'Provider';     // Standard: 'MSDASQL.1'
  C_AdoIdent_Password   = 'Password';     // Passwort
  C_AdoIdent_PersistSecInfo = 'Persist Security Info'; // Standard: 'False'
  C_AdoIdent_UserName   = 'User ID';      // Benutzername
  C_AdoIdent_AliasName  = 'Data Source';  // (ODBC-)Alias
  C_AdoIdent_WDbSection = 'WDbSection';   // Wieser-Bezeichnung

  C_AdoValue_Provider   = 'MSDASQL.1';
  C_AdoValue_PersistSecInfo = 'False';

  PathDefs : array [TPathType] of TPathDef = (
// Verzeichnispfade
    (PathType: WieserIni; Section: 'Wieser';
       Ident: 'WieserIni'; Default: '..\'),
    (PathType: AsciiDir; Section: 'Wieser';
       Ident: 'AsciiDir'; Default: 'Ascii\'),
    (PathType: LangzeitDir; Section: 'Wieser';
       Ident: 'LangzeitDir'; Default: 'Langzeit\'),
    (PathType: ManuDir; Section: 'Wieser';
       Ident: 'ManuDir'; Default: 'Manu\'),
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
       Ident: 'WorkDir'; Default: 'Work\'),
    (PathType: WNetWorkDir; Section: 'WinDfu';
       Ident: 'NetWorkDir'; Default: 'Work\'),
    (PathType: WVoiceDir; Section: 'WinDfu';
       Ident: 'VoiceDir'; Default: 'VoiceDat\'),

// Datenbanken
    (PathType: WStammDb; Section: C_Sect_Databases;
       Ident: 'WStammDb'; Default: 'WStammDb'),
    (PathType: AutoDb; Section: C_Sect_Databases;
       Ident: 'AutoDb'; Default: 'WAutoDb'),
    (PathType: ManuDb; Section: C_Sect_Databases;
       Ident: 'ManuDb'; Default: 'WManuDb'),

    (PathType: WBDEDb; Section: C_Sect_Databases;
       Ident: 'BDEDb'; Default: 'WBDEDb'),
// ADO-Datenbanken
    (PathType: WADODb; Section: C_Sect_ADOConnections;
       Ident: 'ADODb'; Default: 'WADODb'),
       
// Abwärtskompatibilität
    (PathType: DArchivDir; Section: 'DSFGDfu';
       Ident: 'ArchivDir'; Default: 'DSFGArch\Auto\'),
    (PathType: DManuDir; Section: 'DSfGDfu';
       Ident: 'ManuDir'; Default: 'DSfGArch\Manu\')
  );

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

{ TPathItem }

constructor TPathItem.Create (APathType: TPathType; IniFile: TIniFile);
var
  iIx, iExt : integer;
  s         : string;
  pSl       : TStrings;
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
    // Prüfen, ob DatabaseName bereits existiert
    pSl := TStringList.Create;
    try
      Session.GetAliasNames(pSl);
      s := PathName;
      iExt := 1;
      iIx  := pSl.IndexOf(s);
      // DatanbaseName ggf. erweitern
      while (iIx >= 0) do begin
        s := PathName + IntToStr(iExt);
        Inc(iExt);
        iIx  := pSl.IndexOf(s);
      end;
      PathName := s;
    finally
      pSl.Free;
    end;
  end;
end;

procedure TPathItem.InitPathName (IniFile: TIniFile);
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
          if (Length (PathName) > 2) then 
            PathName := IncludeTrailingBackslash(PathName);
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
    i : TPathType;
    s : string;    // Pfad zu WieserIni    // 08.06.2004
  begin
    s := pIniFile.ReadString(PathDefs[WieserIni].Section,
      PathDefs[WieserIni].Ident, PathDefs[WieserIni].Default);
    Result := '';
    for i := Low (TPathType) to High (TPathType) do
      if (PathDefs[i].PathType = iPathType) then begin
        Result := pIniFile.ReadString(
          PathDefs[i].Section, PathDefs[i].Ident, PathDefs[i].Default);
        Break;
      end;
    if (Result = '') or
       (not ((DirectoryExists(Result)) or (DirectoryExists(s + Result))))
    then raise Exception.Create(S_PfadUngueltig)
    else if (DirectoryExists(Result)) then
      Result := ExpandUNCFileName(Result)
    else if (DirectoryExists(s + Result)) then
      Result := ExpandUNCFileName(s + Result);
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

    else Result := szPARADOX; //raise Exception.Create('Keine Tabellen-Dateien vorhanden !');
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
      AutoDb  : sPath := GetPath(DArchivDir);
      ManuDb  : sPath := GetPath(DManuDir);
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
    DatabaseName:='';
    AliasName:='';
    UserName:='';
    Password:='';
  end;

  for i := Low (TPathType) to High (TPathType) do
    if (PathDefs [i].PathType = PathType) then
      with PathDefs [i] do
        if (Section = C_Sect_Databases) then begin
          FDatabaseDef.DatabaseName := PathName;

          // Alias aus INI holen
          s := pIniFile.ReadString(PathName, C_Ident_AliasName, '');
          // Ggf. anlegen
          if (s = '') then begin
            s := CreateDatabase(pIniFile);
            if (s <> '') then begin
              pIniFile.WriteString(Section, Ident, PathName);
              pIniFile.WriteString(PathName, C_Ident_AliasName, s);
            end;
          end;

          FDatabaseDef.AliasName := s;
          FDatabaseDef.UserName :=
            pIniFile.ReadString(PathName, C_Ident_UserName, '');
          FDatabaseDef.Password :=
            DecodePW(pIniFile.ReadString(PathName, C_Ident_Password, ''));
          Result := (FDatabaseDef.AliasName <> '');
        end;
end;

{------------------------------------}
function TPathItem.CheckForAdoConnection(pIniFile: TInifile): boolean;
{------------------------------------}
var
  i : TPathType;
  s : string;
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
          s := pIniFile.ReadString(PathName, C_Ident_AliasName, '');
          // Sollte da sein ...
          if (s <> '') then begin
            FDatabaseDef.AliasName := s;
            FDatabaseDef.UserName :=
              pIniFile.ReadString(PathName, C_Ident_UserName, '');
            FDatabaseDef.Password :=
              DecodePW(pIniFile.ReadString(PathName, C_Ident_Password, ''));
            Result := (FDatabaseDef.AliasName <> '');
          end
          else raise Exception.Create(PathName +  S_KeinAliasDefiniert);
        end;
end;

{------------------------------------}
procedure TPathItem.Check;
{------------------------------------}
var
  sSection : string;
  i        : TPathType;
begin
  // Section herausfinden
  sSection := '';
  for i := Low (TPathType) to High (TPathType) do
    if (PathDefs [i].PathType = PathType) then begin
      sSection := PathDefs[i].Section;
      Break;
    end;

  // Datenbank bzw. ADO-Connection ?
  if ((sSection = C_Sect_Databases) and (IsDatabase)) or
     ((sSection = C_Sect_ADOConnections) and (IsADOConnection)) then
  try
    if (Session.GetAliasDriverName(DatabaseDef.AliasName) = '') then
      raise Exception.Create(Format (S_DbFehlt, [DatabaseDef.AliasName]));
  except
    FIsDatabase := False;
    raise Exception.Create (Format (S_DbFehlt, [DatabaseDef.AliasName]));
  end

  else if not DirectoryExists (PathName) then       { FindFirst ersetzt (kann LW-Wurzel (z.B. gemapptes G:\) nicht prüfen !) }
    raise Exception.Create (Format (S_PfadFehlt ,[PathName]));
end;

{ TPathserver }

{------------------------------------}
constructor TPathServer.Create (
  const FileName: TFileName; const APathTypes: TPathTypes);
{------------------------------------}
var
  sPCNameSection : string;
begin
  inherited Create;

  PathList := TList.Create;
  FWieserIni := TIniFile.Create (FileName);
  FBasePath := ExtractFilePath (FileName);
  InitPathList (APathTypes);
  InitLogoName;
  FDbList := TList.Create;  // 11.10.2002
  FAdoList := TList.Create;  // 27.09.2004

  // Nachsehen, ob ggf. Computername = lokaler PC-Name ist  // 09.10.2004
  sPCNameSection := C_Sect_Hostname + WGetComputerName;
  if (sPCNameSection <> '') then begin
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
  for i:=0 to PathList.Count-1 do
    TPathItem (PathList [i]).Free;
  PathList.Free;
  // Datenbank-Objekte freigeben
  for i := 0 to FDbList.Count-1 do
    with (TDatabase(FDbList[i])) do begin
      Connected := False;
      Free;
    end;
  FDbList.Free;
  // ADOConnection-Objekte freigeben
  for i := 0 to FAdoList.Count-1 do
    with (TADOConnection(FAdoList[i])) do begin
      Connected := False;
      Free;
    end;
  FAdoList.Free;

  inherited Destroy;
end;

{------------------------------------}
function TPathServer.GetPathName (Index: TPathType): TFileName;
{------------------------------------}
var
  i: Integer;
begin
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
  raise Exception.Create (S_PathTypeInitErr);
end;

{------------------------------------}
procedure TPathServer.SetPathName (Index: TPathType; Value: TFileName);
{------------------------------------}
var
  i: Integer;
  j: TPathType;
begin
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
  raise Exception.Create (S_PathTypeInitErr);
end;

{------------------------------------}
procedure TPathserver.InitPathList (const APathTypes: TPathTypes);
{------------------------------------}
var
  i: TPathType;
begin
  for i := Low (TPathType) to High (TPathType) do
  begin
    if (i in APathTypes) then
      PathList.Add (TPathItem.Create (i, FWieserIni));
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
  RMaxKanaele:=FWieserIni.ReadInteger('Wieser', 'Kanalzahl', 28);
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
    pDatabase.DatabaseName, C_Ident_UserName, sUserName);
  FWieserIni.WriteString(
    pDatabase.DatabaseName, C_Ident_Password, EncodePW(sPassWord));
end;

{ Ereignis zum Login der ADO-Conection        }
{ Parameter: Datenbank-Objekt, User, Passwort }
{---------------------------------------------}
procedure TPathServer.AdoLogin(Sender: TObject; sUsername, sPassword: string);
{---------------------------------------------}
var
  s, sAliasName, sDbSection : string;
  i, iPos                   : integer;
begin
  if (Sender is TADOConnection) then
    with (Sender as TADOConnection) do begin
      // Interne Bezeichnung
      s := ConnectionString;
      iPos := Pos(C_AdoIdent_WDbSection + '=', s);
      if (iPos > 0) then begin
        s := Copy (s, iPos + Length(C_AdoIdent_WDbSection + '='), 40);
        iPos := Pos(';', s);
        if (iPos = 0) then sDbSection := s else sDbSection := Copy(s, 1, iPos);
      end;
      // Aliasname
      s := ConnectionString;
      iPos := Pos(C_AdoIdent_AliasName + '=', s);
      if (iPos > 0) then begin
        s := Copy (s, iPos + Length(C_AdoIdent_AliasName + '='), 40);
        iPos := Pos(';', s);
        if (iPos = 0) then sAliasName := s else sAliasName := Copy(s, 1, iPos);

        // Login-Dialog
        if (not LoginDialog(sAliasName, sUserName, sPassword))
        then DatabaseErrorFmt(SLoginError, [sAliasName]);

        // Connectionstring erstellen
        with TStringList.Create do
        try
          Values[C_AdoIdent_Provider] := C_AdoValue_Provider;
          Values[C_AdoIdent_Password] := sPassword;
          Values[C_AdoIdent_PersistSecInfo] := C_AdoValue_PersistSecInfo;
          Values[C_AdoIdent_UserName] := sUserName;
          Values[C_AdoIdent_AliasName] := sAliasName;
          Values[C_AdoIdent_WDbSection] := sDbSection;
          s := '';
          for i := 0 to Count-1 do s := s + Strings[i] + ';';
          ConnectionString := Copy(s, 1, Length(s) - 1);
        finally
          Free;
        end;

        FWieserIni.WriteString(sDbSection, C_Ident_UserName, sUserName);
        FWieserIni.WriteString(
          sDbSection, C_Ident_Password, EncodePW(sPassWord));
      end
      else raise Exception.Create(S_ErrDecodeConnection);  
    end;
end;

{------------------------------------}
procedure TPathServer.Check;
{------------------------------------}
var
  i, j : Integer;
  s    : string;
begin
  for i := 0 to PathList.Count - 1 do
    with TPathItem (PathList [i]) do begin
      Check;
      if (IsDatabase) then begin
        Self.FDbList.Add(TDatabase.Create(nil));
        with TDatabase(Self.FDbList[FDbList.Count-1]) do begin
          OnLogin := DbLogin;
          DatabaseName := PathName;
          if (not IsSQLBased)
          then TransIsolation := tiDirtyRead
          else TransIsolation := tiReadCommitted;
          AliasName := DatabaseDef.AliasName;
          KeepConnection := True;
          Params.Values[szUSERNAME] := DatabaseDef.Username;
          Params.Values[szPASSWORD] := DatabaseDef.Password;
          Params.Values[C_DbSection] := DbSection;

          LoginPrompt := False;
          try
            Open;
          except
            if (not Connected) then begin
              LoginPrompt := True;
              Open;
            end;
          end;
        end;
      end
      else if (IsAdoConnection) then begin
        Self.FAdoList.Add(TADOConnection.Create(nil));
        with TADOConnection(Self.FAdoList[FAdoList.Count-1]) do begin
          Name := PathName;
          // Connectionstring erstellen
          with TStringList.Create do
          try
            Values[C_AdoIdent_Provider] := C_AdoValue_Provider;
            Values[C_AdoIdent_Password] := DatabaseDef.Password;
            Values[C_AdoIdent_PersistSecInfo] := C_AdoValue_PersistSecInfo;
            Values[C_AdoIdent_UserName] := DatabaseDef.Username;
            Values[C_AdoIdent_AliasName] := DatabaseDef.AliasName;
            Values[C_AdoIdent_WDbSection] := DbSection;
            s := '';
            for j := 0 to Count-1 do s := s + Strings[j] + ';';
            ConnectionString := Copy(s, 1, Length(s) - 1);
          finally
            Free;
          end;

          LoginPrompt := False;
          try
            OnLogin := nil;
            Open;
          except
            if (not Connected) then begin
              LoginPrompt := True;
              OnLogin := AdoLogin;
              Open;
            end;
          end;
        end;
      end;
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

  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        //  Datenbank übernehmen
        for j := 0 to FDbList.Count-1 do
          if (TDatabase(FDbList[j]).DatabaseName = PathName) then begin
            Result := TDatabase(FDbList[j]);
            Exit;
          end;

        if (Assigned(Result)) then Exit else Break;
      end;
    end;
  end;

  raise Exception.Create (S_PathTypeInitErr);
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

  raise Exception.Create (S_PathTypeInitErr);
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

  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        //  Datenbank übernehmen
        for j := 0 to FDbList.Count-1 do
          if (TDatabase(FDbList[j]).DatabaseName = PathName) then begin
            Result := TDatabase(FDbList[j]);
            // Datenbank aus Liste löschen
            FDbList.Delete(j);
            // PathItem freigeben
            Self.PathList.Delete(i);
            // Datenbank mit neuem Namen versehen
            Result.Connected := False;
            Result.DatabaseName := 'db' + IntToStr(Integer(Result));
            Result.Connected := True;
            Free;
            Exit;
          end;

        if (Assigned(Result)) then Exit else Break;
      end;
    end;
  end;

  raise Exception.Create (S_PathTypeInitErr);
end;

{------------------------------------}
function TPathServer.GetPathItem(iIndex: TPathType): TPathItem;
{------------------------------------}
var
  i : integer;
begin
  for i := 0 to PathList.Count - 1 do begin
    with TPathItem (PathList [i]) do begin
      if (PathType = iIndex) then begin
        Result := TPathItem (PathList [i]);
        Exit;
      end;
    end;
  end;

  raise Exception.Create (S_PathTypeInitErr);
end;

{------------------------------------}
function TPathServer.GetAliasName(iIndex: TPathType): string;
{------------------------------------}
var
  i: integer;
begin
  Result := '';

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

  raise Exception.Create (S_PathTypeInitErr);
end;

{------------------------------------}
function TPathServer.GetDbDirectory(iIndex: TPathType): string;
{------------------------------------}
var
  i, j : integer;
  pSl  : TStrings;
begin
  Result := '';

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

  raise Exception.Create (S_PathTypeInitErr);
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

constructor TProgramIni.Create;
begin
  inherited Create (ChangeFileExt (Application.ExeName, '.INI'));
  WieserItem := TPathItem.Create (WieserIni, Self);
  ReadFromIni;
end;

destructor TProgramIni.Destroy;
begin
  WieserItem.Free;
  inherited Destroy;
end;

procedure TProgramIni.ReadFromIni;
begin
end;

function TProgramIni.GetWieserIni: TFileName;
begin
  Result := WieserItem.PathName + CWieserIni;
end;

end.

