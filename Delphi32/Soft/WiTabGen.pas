{------------------------------------------------------------------------------}
{ Object für Tabellenpflege auf Script-Basis                                   }
{                                                                              }
{ 01.07.2001  GD  Erweitert um das Füllen von Tabellen über ASCII-Files        }
{ 23.02.2002  GD  Erweitert um die Rückgabe von Datenbanknamen                 }
{ 21.03.2002  GD  ForceDirectories                                             }
{ 22.02.2006  GD  Erweitert um Wieser-Databases                                }
{ 04.05.2009  GD  Erweitert um SQL-Anweisungen                                 }
{ 19.06.2013  GD  Tabellen erstellen läuft auch bei Fehlern durch              }
{                 Logging erweitert                                            }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, RMG Messtechnik GmbH 2013               }
{------------------------------------------------------------------------------}
unit WiTabGen;

interface

uses
  SysUtils, Classes, DbTables, WiTabScript, WiTabObj, GD_Utils, FileCtrl,
  PathIni;

type
  TWiTabOverrideState = (wtosIgnore, wtosModify, wtosRewrite);
  TWiDatabaseType = (wdbtAll, wdbtDriverList, wdbtAliasList, adbtBDEList);

  TWieserTabellenGenerator = class(TWiTabScript)
    constructor Create(sScript: TFileName = ''); override;
    destructor Destroy; override;
  private
    FDatabaseList : TDatabaseList;
    procedure CreateDatabases;
    procedure CheckDatabasePath(sDatabaseName: string);
    procedure ModifyRelDbPath(sDatabaseName: string);
  protected
    function CreateAllTablesInDb(sDatabaseName: string;
      pState: TWiTabOverrideState = wtosModify; iLog: byte = 0): boolean; virtual;
  public
    function IsTableDiff(pTable: TTable): boolean;
    function CreateTable(sDatabaseName, sTableName: string;
      pState: TWiTabOverrideState = wtosModify): boolean; virtual;
    function FillTable(sDatabaseName, sTableName: string; // 01.07.2001
      pState: TWiTabOverrideState = wtosModify): boolean; virtual;
    function CreateAllTables(
      pState: TWiTabOverrideState = wtosModify; iLog: byte = 0): boolean; virtual;
    function CopyTable(sDatabaseName, sCopyName, sTableName: string;
      pState: TWiTabOverrideState = wtosModify): boolean; virtual;
  end;

implementation

{---------------------------------------------}
constructor TWieserTabellenGenerator.Create(sScript: TFileName = '');
{---------------------------------------------}
begin
  inherited Create(sScript);

  FDatabaseList := TDatabaseList.Create;
  CreateDatabases;
end;

{---------------------------------------------}
destructor TWieserTabellenGenerator.Destroy;
{---------------------------------------------}
begin
  if (Assigned(FDatabaseList)) then FDatabaseList.Free;

  inherited Destroy;
end;

{ Erzeugt die Datenbank-Komponenten           }
{---------------------------------------------}
procedure TWieserTabellenGenerator.CreateDatabases;
{---------------------------------------------}

  function InsertWieserDatabase(iType: TPathType; sPrefix: string): boolean;
  var
    pDB : TDatabase;
  begin
    try
      with TProgramIni.Create() do
      try
        with TPathServer.Create(WieserIniFile, [iType], False) do
        try
          Check;
          pDB := GetDatabase(iType);
        finally
          Free;
        end;
      finally
        Free;
      end;
      pDB.Connected := False;
      pDb.DatabaseName := sPrefix + C_DbSeparator + pDb.DatabaseName;
      pDB.Connected := True;
      Self.FDatabaseList.Add(pDb);
      Result := True;
    except
      Result := False;
    end;
  end;

var
  i   : integer;
  pDB : TDatabase;
  pSl : TStrings;
begin
  with (AliasDatabaseList) do
  try
    for i := 0 to Count-1 do begin
      pDb := TDatabase.Create(nil);
      pDb.DatabaseName := Strings[i];
      pDb.AliasName := GetDbAlias(Strings[i]);
      pDb.LoginPrompt := False;
      pDb.KeepConnection := False;
      pDb.TransIsolation := tiDirtyRead;
      pSl := GetDbParams(Strings[i]);
      try
        pDb.Params.Assign(pSl);
      finally
        pSl.Free;
      end;
      Self.FDatabaseList.Add(pDb);
      ModifyRelDbPath(Strings[i]);
    end;
  finally
    Free;
  end;

  with (WieserDatabaseList) do
  try
    for i := 0 to Count-1 do begin
      if (UpperCase(Strings[i]) = UpperCase(C_WieserDb_Stamm)) then
        InsertWieserDatabase(WStammDb, C_WieserDb_Stamm)
      else if (UpperCase(Strings[i]) = UpperCase(C_WieserDb_Auto)) then
        InsertWieserDatabase(AutoDb, C_WieserDb_Auto)
      else if (UpperCase(Strings[i]) = UpperCase(C_WieserDb_Manu)) then
        InsertWieserDatabase(ManuDb, C_WieserDb_Manu);
    end;
  finally
    Free;
  end;

  with (DriverDatabaseList) do
  try
    for i := 0 to Count-1 do begin
      pDb := TDatabase.Create(nil);
      pDb.DatabaseName := Strings[i];
      pDb.DriverName := GetDbDriver(Strings[i]);
      pDb.LoginPrompt := False;
      pDb.TransIsolation := tiDirtyRead;
      pSl := GetDbParams(Strings[i]);
      try
        pDb.Params.Assign(pSl);
      finally
        pSl.Free;
      end;
      Self.FDatabaseList.Add(pDb);
      ModifyRelDbPath(Strings[i]);
    end;
  finally
    Free;
  end;
end;

{ Legt ggf. Datenbankpfad an                  }
{ Parameter: DatabaseName                     }
{---------------------------------------------}
procedure TWieserTabellenGenerator.CheckDatabasePath(sDatabaseName: string);
{---------------------------------------------}
var
  i : integer;
  s : string;
begin
  if (not Assigned(FDatabaseList.GetDatabase(sDatabaseName))) then
    raise Exception.Create('Datenbank ''' + sDatabaseName + ''' nicht vorhanden !');
    
  with (FDatabaseList.GetDatabase(sDatabaseName).Params) do begin
    for i := 0 to Count-1 do
      if (Pos('PATH=', Strings[i]) > 0) then begin
        s := GetStringPart(Strings[i], 2, '=');
        if (s <> '') then ForceDirectories(s); // 21.03.2002
      end;
  end;
end;

{ Modifiziert ggf. realativen Datenbankpfad   } 
{ Parameter: DatabaseName                     }
{---------------------------------------------}
procedure TWieserTabellenGenerator.ModifyRelDbPath(sDatabaseName: string);
{---------------------------------------------}
var
  i : integer;
  s : string;
begin
  if (not Assigned(FDatabaseList.GetDatabase(sDatabaseName))) then
    raise Exception.Create('Datenbank ''' + sDatabaseName + ''' nicht vorhanden !');
    
  with (FDatabaseList.GetDatabase(sDatabaseName).Params) do begin
    for i := 0 to Count-1 do
      if (Pos('PATH=', Strings[i]) > 0) then begin
        s := GetStringPart(Strings[i], 2, '=');
        if (Pos(':\', s) = 0) then begin
          while (Length(s) > 0) and (s[1] in ['.', '\']) do System.Delete(s, 1, 1);
          if (s <> '') then s := ExtractFilePath(ParamStr(0)) + s;
          Strings[i] := 'PATH=' + s;
        end;
      end;
  end;
end;

{ Sind Tabellenstruktur und Definition gleich }
{ Parameter: Tabelle                          }
{ Rückgabe: T-Unterschiedlich; F-Gleich       }
{---------------------------------------------}
function TWieserTabellenGenerator.IsTableDiff(pTable: TTable): boolean;
{---------------------------------------------}
var
  pFieldDef : TMyFieldDefList;
  pIndexDef : TMyIndexDefList;
  i, j      : integer;
begin
  Result := False;  // Default

  if (Assigned(pTable)) and (pTable.Exists) then begin

    // Indexdefinitionen holen
    pIndexDef := GetIndexDefList(pTable.TableName);
    // Felddefinitionen holen - Prüfung auf existierende Definition
    pFieldDef := GetFieldDefList(pTable.TableName);
    try

      if (pFieldDef.Count = 0) then raise Exception.Create(
        'Fehlende Tabellendefinition ''' + pTable.TableName + ''' !');

    // Prüfen der Felddefinitionen (Name, Typ, Size)
      pTable.FieldDefs.Update;
      for i := 0 to pTable.FieldDefs.Count-1 do begin
        j := pFieldDef.IndexOf(pTable.FieldDefs[i].Name);
        if (j < 0) then begin
          Result := True;
          Break;
        end
        else begin
          if (pFieldDef.FieldDef[j].FieldType = pTable.FieldDefs[i].DataType)
            and (pFieldDef.FieldDef[j].FieldSize = pTable.FieldDefs[i].Size)
          then pFieldDef.Delete(j)
          else begin
            Result := True;
            Break;
          end;
        end;
      end;
      if (not Result) then Result := (pFieldDef.Count > 0);
      if (Result) then Exit;

    // Prüfen der Indexdefinitionen (Name, Felder)
      pTable.IndexDefs.Update;
      for i := 0 to pTable.IndexDefs.Count-1 do begin
        if (pTable.IndexDefs[i].Name = '') then begin
          j := pIndexDef.IndexOf(C_IndexDefault);
          if (j < 0) and (pIndexDef.Count > 0) then j := 0;
        end
        else j := pIndexDef.IndexOf(pTable.IndexDefs[i].Name);
        if (j < 0) then begin
          Result := True;
          Break;
        end
        else begin
          if ((pIndexDef.IndexDef[j].Name = pTable.IndexDefs[i].Name) or
              (pTable.IndexDefs[i].Name = ''))
            and (pIndexDef.IndexDef[j].Fields = pTable.IndexDefs[i].Fields)
          then pIndexDef.Delete(j)
          else begin
            Result := True;
            Break;
          end;
        end;
      end;
      if (not Result) then Result := (pIndexDef.Count > 0);

    finally
      pIndexDef.Free;
      pFieldDef.Free;
    end;

  end
  else Result := True;
end;

{ Legt Tabelle neu an                         }
{ Parameter: Datenbank-, Tabellenname, Status }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function TWieserTabellenGenerator.CreateTable(sDatabaseName, sTableName: string;
  pState: TWiTabOverrideState = wtosModify): boolean;
{---------------------------------------------}
var
  pTable    : TTable;
  pFieldDef : TMyFieldDefList;
  pIndexDef : TMyIndexDefList;
  pTableDef : TMyTableDef;
  i         : integer;
begin
  Result := False;  // Default

  // Prüfung auf existierende Database
  if (not Assigned(FDatabaseList.GetDatabase(sDatabaseName))) then
    raise Exception.Create('Unbekannte Datenbank ''' + sDatabaseName + ''' !');

  // Felddefinitionen holen - Prüfung auf existierende Definition
  pFieldDef := GetFieldDefList(sTableName);
  try
    if (pFieldDef.Count = 0) then
      raise Exception.Create('Fehlende Tabellendefinition ''' + sTableName + ''' !');

  // Indexdefinitionen holen
    pIndexDef := GetIndexDefList(sTableName);
    try

  // Prüfung auf bereits existierende Tabelle
      pTable := TTable.Create(nil);
      try
        pTable.DatabaseName := sDatabaseName;
        pTable.TableName := sTableName;
        CheckDatabasePath(sDatabaseName);

        if (pTable.Exists) then begin
          if (pState = wtosIgnore) then begin
            Result := True;
            Exit;
          end
          else if (pState = wtosModify) and (not IsTableDiff(pTable)) then begin
            Result := True;
            Exit;
          end
          else if (pState = wtosRewrite) and (IsTableDiff(pTable)) then begin
            pTable.DeleteTable;
            pTable.FieldDefs.Clear;
            pTable.IndexDefs.Clear;
          end;
        end;

  // Wenn wir ankommen, kann es endlich losgehen ...
        pTableDef := TMyTableDef.Create;
        try
          pTableDef.DatabaseName := sDatabaseName;
          pTableDef.TableName := sTableName;
          pTableDef.FieldDef := pFieldDef;
          pTableDef.IndexDef := pIndexDef;
          MyRestructureTable(pTable, pTableDef);

          // Ggf. SQL-Anweisungen ausführen  // 04.05.2009
          with GetSqlDefList(sTableName) do
          try
            for i := 0 to Count-1 do
            try
              with TQuery.Create(nil) do
              try
                DatabaseName := sDatabaseName;
                Sql.Text := Strings[i];
                ExecSQL;
              finally
                Free;
              end;
            except
              Break;
            end;
          finally
            Free;
          end;

          Result := True;
        finally
          pTableDef.Free;
        end;

      finally
        pTable.Free;
      end;

    finally
      pIndexDef.Free;
    end;

  finally
    pFieldDef.Free;
  end;
end;

{ Legt alle Tabelle einer Datenbank neu an    }
{ Parameter: Datenbankname, Status            }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function TWieserTabellenGenerator.CreateAllTablesInDb(sDatabaseName: string;
  pState: TWiTabOverrideState = wtosModify; iLog: byte = 0): boolean;
{---------------------------------------------}
var
  i : integer;
begin
  // Prüfung auf existierende Database
  if (not Assigned(FDatabaseList.GetDatabase(sDatabaseName))) then
    raise Exception.Create('Unbekannte Datenbank ''' + sDatabaseName + ''' !');

  Result := True;  // Default

  with (TStringList.Create) do
  try
    CommaText := Self.GetDbTables(sDatabaseName);
    for i := 0 to Count-1 do
    try
      if (not Self.CreateTable(sDatabaseName, Strings[i], pState))
      then Result := False
      else if (iLog = 1) then WriteErrorLog('Table "' + Strings[i] +
        '" successfully modified / passed',
        ExtractFilePath(ParamStr(0)) + 'TABGEN.LOG', elt_Info);
      FillTable(sDatabaseName, Strings[i], pState);  // 01.07.2001
    except
      on E:Exception do begin       // 19.06.2013
        WriteErrorLog('CreateAllTablesInDb (Table "' + Strings[i] + '"): ' +
          E.Message, ExtractFilePath(ParamStr(0)) + 'TABGEN.ERR');
        Result := False;
      end;
    end;
  finally
    Free;
  end;
end;

{ Legt alle Tabelle aus dem Script neu an     }
{ Parameter: Status, Logging                  }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function TWieserTabellenGenerator.CreateAllTables(
      pState: TWiTabOverrideState = wtosModify; iLog: byte = 0): boolean;
{---------------------------------------------}
var
  i : integer;
begin
  Result := True;  // Default

  for i := 0 to FDatabaseList.Count-1 do
    if (not Self.CreateAllTablesInDb(FDatabaseList.Database[i].DatabaseName,
      pState, iLog))
    then Result := False;
end;

{ Legt alle Tabelle aus dem Script neu an     }
{ Parameter: Status                           }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function TWieserTabellenGenerator.CopyTable(sDatabaseName, sCopyName,
  sTableName: string; pState: TWiTabOverrideState = wtosModify): boolean;
{---------------------------------------------}
var
  pTable    : TTable;
  pFieldDef : TMyFieldDefList;
  pIndexDef : TMyIndexDefList;
  pTableDef : TMyTableDef;
begin
  Result := False;  // Default

  // Prüfung auf existierende Database
  if (not Assigned(FDatabaseList.GetDatabase(sDatabaseName))) then
    raise Exception.Create('Unbekannte Datenbank ''' + sDatabaseName + ''' !');

  // Felddefinitionen holen - Prüfung auf existierende Definition
  pFieldDef := GetFieldDefList(sCopyName);
  try
    if (pFieldDef.Count = 0) then
      raise Exception.Create('Fehlende Tabellendefinition ''' + sTableName + ''' !');

  // Indexdefinitionen holen
    pIndexDef := GetIndexDefList(sCopyName);
    try

  // Prüfung auf bereits existierende Tabelle
      pTable := TTable.Create(nil);
      try
        pTable.DatabaseName := sDatabaseName;
        pTable.TableName := sTableName;
        CheckDatabasePath(sDatabaseName);

        if (pTable.Exists) then begin
          if (pState = wtosIgnore) then begin
            Result := True;
            Exit;
          end
          else if (pState = wtosModify) and (not IsTableDiff(pTable)) then begin
            Result := True;
            Exit;
          end
          else if (pState = wtosRewrite) and (IsTableDiff(pTable)) then begin
            pTable.DeleteTable;
            pTable.FieldDefs.Clear;
            pTable.IndexDefs.Clear;
          end;
        end;

  // Wenn wir ankommen, kann es endlich losgehen ...
        pTableDef := TMyTableDef.Create;
        try
          pTableDef.DatabaseName := sDatabaseName;
          pTableDef.TableName := sTableName;
          pTableDef.FieldDef := pFieldDef;
          pTableDef.IndexDef := pIndexDef;
          MyRestructureTable(pTable, pTableDef);
          Result := True;
        finally
          pTableDef.Free;
        end;

      finally
        pTable.Free;
      end;

    finally
      pIndexDef.Free;
    end;

  finally
    pFieldDef.Free;
  end;
end;

{ Legt alle Tabelle aus dem Script neu an     }
{ Parameter: Status                           }
{ Rückgabe: Erfolg Ja/Nein                    }
{---------------------------------------------}
function TWieserTabellenGenerator.FillTable(sDatabaseName, sTableName: string;
  pState: TWiTabOverrideState = wtosModify): boolean;
{---------------------------------------------}
var
  pSl       : TStrings;
  pTb       : TTable;
  s         : string;
  i         : integer;
  sFileName : TFileName;
  f         : Text;
begin
  Result := False;
  sFileName := '';

  if (FileExists(ExtractFilePath(ParamStr(0)) + 'TB_' + sTableName + '.DAT'))
  then sFileName := ExtractFilePath(ParamStr(0)) + 'TB_' + sTableName + '.DAT'
  else begin
    if (DirectoryExists(sDatabaseName)) and
       (FileExists(ExtractFilePath(ParamStr(0) + 'TB_' + sTableName + '.DAT')))
    then begin
      sFileName := sDatabaseName;
      if (sFileName[Length(sFileName)] <> '\') then sFileName := sFileName + '\';
      sFileName := sFileName + 'TB_' + sTableName + '.DAT';
    end;
  end;

  if (sFileName <> '') then
  try

    pSl := TStringList.Create;
    pTb := TTable.Create(nil);
    try
      pTb.DatabaseName := sDatabaseName;
      pTb.TableName := sTableName;
      if (not pTb.Exists) then Exit;
      if (pState = wtosRewrite) then pTb.EmptyTable;
      pTb.Open;
      if (pState = wtosIgnore) and (not pTb.Eof) then begin
        pTb.Close;
        Exit;
      end;

      AssignFile(f, sFileName);
      Reset(f);
      try
        while (not Eof(f)) do begin
          pTb.Append;

          Readln(f, s);
          pSl.CommaText := s;
          for i := 0 to pSl.Count-1 do
            if (pSl[i] <> '') then pTb.Fields[i].asString := pSl[i];

          pTb.Post;
        end;
      finally
        CloseFile(f);
      end;

      if (pTb.Active) then pTb.Close;
      RenameFile(sFileName,
        ExtractFilePath(sFileName) + '_' + ExtractFileName(sFileName));
      Result := True;
    finally
      pTb.Free;
      pSl.Free;
    end;
  except
  // Result ist False
  end;
end;

end.
