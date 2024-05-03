//------------------------------------------------------------------------------
// Skalierbare Soeicherung von Programmkonfigurationen
//
// 25.06.2013  GD  Neu
//
// Copyright (C) RMG Messtechnik GmbH 2013
//------------------------------------------------------------------------------
unit O_ProgCfg;

interface

uses
  SysUtils, Inifiles,
  GD_Utils;

type
  // Programm-Konfiguration (nach Installation unveränderlich)
  TModuleConfig = class(TIniFile)
    constructor Create(const sFileName: TFileName = ''); virtual;
  private
    function GetSystemConfigFile: TFileName;
  protected
    property SystemConfigFile: TFileName read GetSystemConfigFile;
  public
  end;

  // System-Konfiguration (nach Installation unveränderlich)
  TSystemConfig = class(TIniFile)
  private
  protected
    function GetModuleIniFilesDir: string;
    function GetNetProgDir: string;
    function GetWicomSrvStartFile: TFileName;
    procedure SetWicomSrvStartFile(const sFile: TFileName);
    function GetWicomSrvStopFile: TFileName;
    procedure SetWicomSrvStopFile(const sFile: TFileName);
  public
    function GetOrgDbConection: string;
    procedure SetOrgDbConection(sConnStr: string);
    function GetArchiveDbConection: string;
    property WicomSrvStartFile: TFileName
      read GetWicomSrvStartFile write SetWicomSrvStartFile;
    property WicomSrvStopFile: TFileName
      read GetWicomSrvStopFile write SetWicomSrvStopFile;
  end;

  // Modul-Konfiguration (veränderlich)
  TModuleIni = class(TIniFile)
  private
  protected
  public
  end;

  // Programm-Konfiguration (gesamt)
  TProgramConfig = class(TModuleConfig)
    constructor Create(const sFileName: TFileName = ''); override;
    destructor Destroy; override;
  private
    FSystemConfig  : TSystemConfig;
    FModuleIni     : TModuleIni;
  protected
    function GetOrgDbConection: string;
    procedure SetOrgDbConection(sConnStr: string);
    function GetNetProgDir: TFileName;
  public
    function ReadModulIniString(
      const sSection, sIdent, sDefault: string): string;
    procedure WriteModulIniString(
      const sSection, sIdent, sValue: string);
    function ReadModulCfgString(
      const sSection, sIdent, sDefault: string): string;
    function ReadSystemCfgString(
      const sSection, sIdent, sDefault: string): string;

    property OrgDbConection: string
      read GetOrgDbConection write SetOrgDbConection;
    property NetProgDir: TFileName read GetNetProgDir;
    property SystemComfig: TSystemConfig read FSystemConfig;
  end;

implementation

const
  C_Default_SystemConfig   = 'WIESER.INI';

  C_Section_ConfigFiles    = 'WIESER';
  C_Section_Directories    = 'DIRECTORIES';
  C_Section_WinDFU         = 'WinDFU';
  C_Section_AdoConnections = 'ADOCONNECTIONS';
  C_Section_Programs       = 'PROGRAMME';

  C_Ident_SystemCfgFile    = 'WIESERINI';
  C_Ident_ModuleIniFiles   = 'MODULEINIFILES';
  C_Ident_WNetUserDir      = 'WNetUserDir';
  C_Ident_ConnStrOrgDb     = 'CONNSTRORGDB';
  C_Ident_ConnStrArchDb    = 'CONNSTRARCHDB';
  C_Ident_NetProgDir       = 'WNETPROGDIR';

//------------------------------- TModuleConfig -------------------------------

// Konstruktor
// Parameter: Dateiname oder ''; bei '' wird der Default verwendet
//--------------------------------------------------
constructor TModuleConfig.Create(const sFileName: TFileName = '');
//--------------------------------------------------
var
  s : TFileName;
begin
  if (sFileName = '')
  then s:= ChangeFileExt(ParamStr(0), '.CFG')
  else s := sFileName;

  inherited Create(s);
end;

// Gibt die zentrale Konfigurationsdatei zurück (bisher: WIESER.INI)
// Rückgabe: Name der zentralen Konfigurationsdatei
//--------------------------------------------------
function TModuleConfig.GetSystemConfigFile: TFileName;
//--------------------------------------------------
var
  s : TFileName;
begin
  s := GDExpandFilePath(
    ReadString(C_Section_ConfigFiles, C_Ident_SystemCfgFile, '..\'));
  if (FileExists(s)) then
    Result := s
  else if (DirectoryExists(s)) then
    Result := IncludeTrailingBackslash(s) + C_Default_SystemConfig
  else Result := GDExpandFilePath('..\') + C_Default_SystemConfig;
end;

//-------------------------------- TSystemConfig -------------------------------

// Gibt den ADO-ConnectionString zur Stammdatenbank zurück
// Rückgabe: ADO-ConnectionString zur Stammdatenbank
//--------------------------------------------------
function TSystemConfig.GetOrgDbConection: string;
//--------------------------------------------------
begin
  //!! Achtung:Passwort nicht geschützt !!!
  Result := ReadString(C_Section_AdoConnections, C_Ident_ConnStrOrgDb, '');
end;

// Setzt den ADO-ConnectionString zur Stammdatenbank
// Parameter: ADO-ConnectionString zur Stammdatenbank
//--------------------------------------------------
procedure TSystemConfig.SetOrgDbConection(sConnStr: string);
//--------------------------------------------------
begin
  //!! Achtung:Passwort nicht geschützt !!!
  WriteString(C_Section_AdoConnections, C_Ident_ConnStrOrgDb, sConnStr);
end;

// Gibt den ADO-ConnectionString zur Archivdatenbank zurück
// Rückgabe: ADO-ConnectionString zur Archivdatenbank
//--------------------------------------------------
function TSystemConfig.GetArchiveDbConection: string;
//--------------------------------------------------
begin
  //!! Achtung:Passwort nicht geschützt !!!
  Result := ReadString(C_Section_AdoConnections, C_Ident_ConnStrArchDb, '');
end;

// Gibt das Verzeichnis zu den änderbaren Modulinformationen zurück
// Rückgabe: Verzeichnis zu den änderbaren Modulinformationen
//--------------------------------------------------
function TSystemConfig.GetModuleIniFilesDir: string;
//--------------------------------------------------
begin
  // Default ist das Windows-Anwender-Verzeichnis ("Eigene Dateien")
  Result := ReadString(
    C_Section_WinDFU, C_Ident_WNetUserDir, '.\USER\');
  if (Pos('.', Result) = 1) then
    Result := ExtractFilePath(Filename) + Result;
  if (not ForceDirectories(Result)) then Result := WGetUserDir;
end;

// Gibt das zentrale Programmverzeichnis der Installation zurück
// Rückgabe: Zentrales Programmverzeichnis der Installation
//--------------------------------------------------
function TSystemConfig.GetNetProgDir: string;
//--------------------------------------------------
begin
  Result := ReadString(
    C_Section_Programs, C_Ident_NetProgDir, '.\Prog\');
  if (Pos('.', Result) = 1) then
    Result := ExtractFilePath(Filename) + Result;

  if (ForceDirectories(Result))
  then Result := IncludeTrailingBackslash(Result)
  else Result := '';
end;

//---------------------------------------------
function TSystemConfig.GetWicomSrvStartFile: TFileName;
//---------------------------------------------
begin
  Result := ReadString('WICOMSRV', 'STARTFILE', '.\STAMMDAT\WICOMSRVSTART.CTL');
  if (Pos('.', Result) = 1) then
    Result := ExtractFilePath(Filename) + Result;
end;

//---------------------------------------------
procedure TSystemConfig.SetWicomSrvStartFile(const sFile: TFileName);
//---------------------------------------------
begin
  WriteString('WICOMSRV', 'STARTFILE', sFile);
end;

//---------------------------------------------
function TSystemConfig.GetWicomSrvStopFile: TFileName;
//---------------------------------------------
begin
  Result := ReadString('WICOMSRV', 'STOPFILE', '.\STAMMDAT\WICOMSRVSTOP.CTL');
  if (Pos('.', Result) = 1) then
    Result := ExtractFilePath(Filename) + Result;
end;

//---------------------------------------------
procedure TSystemConfig.SetWicomSrvStopFile(const sFile: TFileName);
//---------------------------------------------
begin
  WriteString('WICOMSRV', 'STOPFILE', sFile);
end;

//------------------------------- TProgramConfig -------------------------------

// Konstruktor
// Parameter: Dateiname oder ''; bei '' wird der Default verwendet
//--------------------------------------------------
constructor TProgramConfig.Create(const sFileName: TFileName = '');
//--------------------------------------------------
begin
  inherited Create(sFileName);

  // Systemkonfigurationen (für user unveränderlich)
  FSystemConfig := TSystemConfig.Create(SystemConfigFile);
  FModuleIni := TModuleIni.Create(
    IncludeTrailingBackslash(FSystemConfig.GetModuleIniFilesDir) +
    ExtractFileName(ChangeFileExt(ParamStr(0), '.INI')));
end;

// Destruktor
//--------------------------------------------------
destructor TProgramConfig.Destroy;
//--------------------------------------------------
begin
  FreeAndNil(FSystemConfig);  // Unveränderliche Systemkonfigurationen
  FreeAndNil(FModuleIni);     // Veränderliche Modulkonfigurationen
end;

// ZentralesProgrammverzeichnis der Installation zurückgeben
//--------------------------------------------------
function TProgramConfig.GetNetProgDir: TFileName;
//--------------------------------------------------
begin
  Result := FSystemConfig.GetNetProgDir;
end;

// ADO-Connectionstring zu Stammdatenbank
//--------------------------------------------------
function TProgramConfig.GetOrgDbConection: string;
//--------------------------------------------------
begin
  Result := FSystemConfig.GetOrgDbConection;
end;

// ADO-Connectionstring zu Stammdatenbank
//--------------------------------------------------
procedure TProgramConfig.SetOrgDbConection(sConnStr: string);
//--------------------------------------------------
begin
  FSystemConfig.SetOrgDbConection(sConnStr);
end;

// String aus <module>.INI (änderbar) auslesen
//--------------------------------------------------
function TProgramConfig.ReadModulIniString(
  const sSection, sIdent, sDefault: string): string;
//--------------------------------------------------
begin
  Result := FModuleIni.ReadString(sSection, sIdent, sDefault);
end;

// String in <module>.INI (änderbar) schreiben
//--------------------------------------------------
procedure TProgramConfig.WriteModulIniString(
  const sSection, sIdent, sValue: string);
//--------------------------------------------------
begin
  FModuleIni.WriteString(sSection, sIdent, sValue);
end;

// String aus <module>.CFG (nicht änderbar) auslesen
//--------------------------------------------------
function TProgramConfig.ReadModulCfgString(
  const sSection, sIdent, sDefault: string): string;
//--------------------------------------------------
begin
  Result := Self.ReadString(sSection, sIdent, sDefault);
end;

// String aus <system>.CFG (nicht änderbar) auslesen
//--------------------------------------------------
function TProgramConfig.ReadSystemCfgString(
  const sSection, sIdent, sDefault: string): string;
//--------------------------------------------------
begin
  Result := FSystemConfig.ReadString(sSection, sIdent, sDefault);
end;

end.
