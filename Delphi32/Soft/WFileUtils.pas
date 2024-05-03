{------------------------------------------------------------------------------}
{ Kopieren von Dateien                                                         }
{                                                                              }
{ 13.06.2005  GD  Neu                                                          }
{ 13.08.2005  GD  WDeleteDirectory erweitert                                   }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2005, RMG Messtechnik GmbH 2010               }
{------------------------------------------------------------------------------}
unit WFileUtils;

interface

uses
  Windows, SysUtils, Classes, FileCtrl,
  ZipMstr, WDbTables;

function WCreateTempDirectory(const sMainDirectory: TFileName): TFileName;
function WDeleteDirectory(const sDirectory: TFileName;
  bBreakOnFault: boolean = True; bDelDir: boolean = True): boolean;
function WCopyDirectory(const sSrcDir, sSinkDir: TFileName): boolean;
function WCopyFile(const sOriFile, sNewFile: TFileName; bFailIfExists: boolean;
  bDeleteOriFile: boolean = False): boolean;
function WCopyPDoxTable(const sOriDatabaseName, sOriTableName, sNewDatabaseName,
  sNewTableName: string; bFailIfExists: boolean): boolean;
function WZipFiles(const sZipFileName: TFileName; const sFiles: string;
  sZipDllDir: TFileName = ''; sRootPath: TFileName = ''): boolean;
function WUnZipFiles(const sOutDirectory, sZipFileName: TFileName;
  const sFiles: string; sZipDllDir: TFileName = ''): boolean;

implementation

{ Erzeugt (temporäres) Verzeichnis        }
{ Parameter: Übergeordnetes Verzeichnis   }
{ Ergebnis: Erfolg ja/nein                }
{-----------------------------------------}
function WCreateTempDirectory(const sMainDirectory: TFileName): TFileName;
{-----------------------------------------}
var
  i    : integer;
  sDir : TFileName;
begin
  for i := 1 to 1000 do begin
    sDir := IncludeTrailingBackslash(sMainDirectory) + '~tmp' + IntToHex(i, 4);
    if (not DirectoryExists(sDir)) then begin
      if (CreateDir(sDir)) then Break;
    end;
  end;
  if (DirExists(sDir)) then Result := sDir else Result := '';
end;

{ Löscht ein (volles) Verzeichnis         }
{ Parameter: Verzeichnis                  }
{ Ergebnis: Erfolg ja/nein                }
{-----------------------------------------}
function WDeleteDirectory(const sDirectory: TFileName;
  bBreakOnFault: boolean = True; bDelDir: boolean = True): boolean;
{-----------------------------------------}
var
  pSR : TSearchRec;
begin
  Result := True;
  if (DirectoryExists(sDirectory)) then begin
    if (FindFirst(IncludeTrailingBackslash(sDirectory) + '*.*',
      faDirectory, pSR) = 0) then
    try
      if (Pos('.', pSR.Name) <> 1) then begin
        if ((pSR.Attr and faDirectory) > 0) then begin
          if (not WDeleteDirectory(
            IncludeTrailingBackslash(sDirectory) + pSR.Name))
          then Result := False;
        end
        else begin
          if (not DeleteFile(IncludeTrailingBackslash(sDirectory) + pSR.Name))
          then Result := False;
        end;
        Sleep(1);
      end;

      if (Result) or (not bBreakOnFault) then begin
        while (FindNext(pSR) = 0) do begin
          if (Pos('.', pSR.Name) <> 1) then begin
            if ((pSR.Attr and faDirectory) > 0) then begin
              Result := Result and WDeleteDirectory(
                IncludeTrailingBackslash(sDirectory) + pSR.Name);
              if (not Result) and (bBreakOnFault) then Break;
            end
            else begin
              Result := Result and DeleteFile(
                IncludeTrailingBackslash(sDirectory) + pSR.Name);
              if (not Result) and (bBreakOnFault) then Break;
            end;
          end;
          Sleep(1);
        end;
      end;
    finally
      FindClose(pSR);
    end;
    
    if (bDelDir) then Result := RemoveDir(sDirectory);
  end;
end;

{-----------------------------------------}
function WCopyDirectory(const sSrcDir, sSinkDir: TFileName): boolean;
{-----------------------------------------}

  function CopyThisFile(pSR: TSearchRec): boolean;
  begin
    Result := True;
    if (Pos('.', pSR.Name) <> 1) then begin
      if ((pSR.Attr and faDirectory) > 0) then begin
        if (not WCopyDirectory(IncludeTrailingBackslash(sSrcDir) + pSR.Name,
          IncludeTrailingBackslash(sSinkDir) + pSR.Name))
        then Result := False;
      end
      else begin
        if (not WCopyFile(IncludeTrailingBackslash(sSrcDir) + pSR.Name,
          IncludeTrailingBackslash(sSinkDir) + pSR.Name, False, False))
        then Result := False;
      end;
      Sleep(1);
    end;
  end;

var
  pSR  : TSearchRec;
  iNow : integer;
begin
  if (DirectoryExists(sSrcDir)) and (ForceDirectories(sSinkDir)) then begin
    Result := True;

    iNow := DateTimeToFileDate(Now);
    if (FindFirst(IncludeTrailingBackslash(sSrcDir) + '*.*',
      faDirectory, pSR) = 0) then
    try
      if (pSr.Time < iNow) then
        if (not CopyThisFile(pSR)) then Result := False;

      if (Result) then begin
        while (FindNext(pSR) = 0) do
          if (pSr.Time < iNow) then
            if (not CopyThisFile(pSR)) then Result := False;
      end;
    finally
      FindClose(pSR);
    end;
  end
  else Result := False;
end;

{ Kopiert Datei                           }
{ Parameter: Quelldateiname, Zieldatei-,  }
{            name, nicht überschreiben ?, }
{            Originaldatei löschen ?      }
{ Ergebnis: Erfolg ja/nein                }
{-----------------------------------------}
function WCopyFile(const sOriFile, sNewFile: TFileName; bFailIfExists: boolean;
  bDeleteOriFile: boolean = False): boolean;
{-----------------------------------------}

  function CopyThisFile(const sSrcFile, sSinkFile: TFileName): boolean;
  begin
    Result := CopyFile(PChar(sSrcFile), PChar(sSinkFile), bFailIfExists);
    if (Result) and (bDeleteOriFile) then Result := DeleteFile(sSrcFile);
  end;

var
  sExt, sSinkFile : TFileName;
  pSR             : TSearchRec;
begin
  try
    Result := True;

    sExt := ExtractFileExt(sOriFile);
    if (FindFirst(sOriFile, 0, pSR) = 0) then
    try
      if (sExt = '.*')
      then sSinkFile := ChangeFileExt(sNewFile, ExtractFileExt(pSR.Name))
      else sSinkFile := sNewFile;
      if (not CopyThisFile(ExtractFilePath(sOriFile) + pSR.Name, sSinkFile))
      then Result := False;

      while (FindNext(pSR) = 0) do begin
        if (sExt = '.*')
        then sSinkFile := ChangeFileExt(sNewFile, ExtractFileExt(pSR.Name))
        else sSinkFile := sNewFile;
        if (not CopyThisFile(ExtractFilePath(sOriFile) + pSR.Name, sSinkFile))
        then Result := False;
      end;
    finally
      FindClose(pSR);
    end;

  except
    Result := False;
  end;
end;

{ Kopiert Paradox-Tabelle                 }
{ Parameter: Quelldatenbanname, Quell-    }
{            tabellenname, Zieldatenbank- }
{            name, Zieltabellenname,      }
{            nicht überschreiben ?,       }
{ Ergebnis: Erfolg ja/nein                }
{-----------------------------------------}
function WCopyPDoxTable(const sOriDatabaseName, sOriTableName, sNewDatabaseName,
  sNewTableName: string; bFailIfExists: boolean): boolean;
{-----------------------------------------}
var
  sRelDatabaseTabelName : string;
begin
  Result := False;
  
  // Ist Quelltabelle vorhanden ?
  if (not WTableExists(sOriDatabaseName, sOriTableName)) then Exit;
  // Bei "nicht überschreiben": ist Zieltabelle bereits vorhanden ?
  if (bFailIfExists) and (WTableExists(sNewDatabaseName, sNewTableName))
  then Exit;

  // Zieltabelle erzeugen
  Result := WCopyTableStruc(
    sOriDatabaseName, sOriTableName, sNewDatabaseName, sNewTableName);
  // Zieltabelle füllen
  if (Result) then begin
    if (DirectoryExists(sOriDatabaseName))
    then sRelDatabaseTabelName := '"' +
      IncludeTrailingBackslash(sOriDatabaseName) +
      ChangeFileExt(sOriTableName, '.DB') + '"'
    else sRelDatabaseTabelName := '":' + sOriDatabaseName + ':' +
      ChangeFileExt(sOriTableName, '') + '"';
    with TWQuery.Create(nil) do
    try
      DatabaseName := sNewDatabaseName;
      Sql.Add('INSERT INTO ' + ChangeFileExt(sNewTableName, ''));
      Sql.Add('SELECT * FROM ' + sRelDatabaseTabelName);
      try
        ExecSql;
      except
        on E:Exception do begin
          if (Pos(LowerCase(ChangeFileExt(sOriTableName, '.val')),
            LowerCase(E.Message)) > 0) and (DirectoryExists(sOriDatabaseName))
            and (DeleteFile(IncludeTrailingBackslash(sOriDatabaseName) +
            ChangeFileExt(sOriTableName, '.val'))) then
          begin
            ExecSql;
          end;
        end;
      end;
      Result := True;
    finally
      Free;
    end;
  end;
end;

{ Zipped übergebene Dateien               }
{ Parameter: Liste mit Dateinamen, Name   }
{            d. Ausgabe, Pfad zur ZIP-Dll }
{ Ergebnis: Erfolg ja/nein                }
{-----------------------------------------}
function WZipFiles(const sZipFileName: TFileName; const sFiles: string;
  sZipDllDir: TFileName = ''; sRootPath: TFileName = ''): boolean;
{-----------------------------------------}
begin
  if (sZipDllDir = '') then sZipDllDir := ExtractFilePath(ParamStr(0));

  with TZipMaster.Create(nil) do
  try
    DLLDirectory := sZipDllDir;
    TempDir := sZipDllDir;
    try
      if (Load_Zip_Dll <> 0) then
      try
        FSpecArgs.Clear;
        RootDir := sRootPath;
        FSpecArgs.CommaText := sFiles;
        AddOptions := AddOptions + [AddDirNames, AddRecurseDirs];
        ZipFileName := sZipFileName;
        Add;  // zippen
        Result := True;
      finally
        Unload_Zip_Dll;
      end
      else Result := False;
    except
      Result := False;
    end;
  finally
    Free;
  end;
end;

{ Entzipped übergebene Dateien            }
{ Parameter: Ausgabedirectory, Zipdatei-  }
{            name, Liste mit Dateinamen,  }
{            Pfad zur ZIP-Dll             }
{ Ergebnis: Erfolg ja/nein                }
{-----------------------------------------}
function WUnZipFiles(const sOutDirectory, sZipFileName: TFileName;
  const sFiles: string; sZipDllDir: TFileName = ''): boolean;
{-----------------------------------------}
begin
  if (sZipDllDir = '') then sZipDllDir := ExtractFilePath(ParamStr(0));

  with TZipMaster.Create(nil) do
  try
    DLLDirectory := sZipDllDir;
    TempDir := sZipDllDir;
    try
      if (Load_Unz_Dll <> 0) then
      try
        ForceDirectories(sOutDirectory);
        FSpecArgs.Clear;
        FSpecArgs.Text := sFiles;
        ZipFileName := sZipFileName;
        ExtrBaseDir := sOutDirectory;
        ExtrOptions := ExtrOptions + [ExtrOverwrite, ExtrForceDirs, ExtrDirNames];
        Extract;
        Result := True;
      finally
        Unload_Unz_Dll;
      end
      else Result := False;
    except
      Result := False;
    end;
  finally
    Free;
  end;
end;

end.
