{------------------------------------------------------------------------------}
{ Script-File-Object für Tabellenprüf-Programm                                 }
{                                                                              }
{ Bemerkungen:  -> 'Database'-Einträge: Databasenames müssen eindeutig sein !  }
{               -> 'Params' und andere Listen werden als Kommatext gespeichert }
{               -> Format der 'Alias-Database'-Einträge:                       }
{                   <databasename>=<aliasname>;<params>                        }
{               -> Format der 'Driver-Database'-Einträge:                      }
{                   <databasename>=<drivername>;<params>                       }
{                                                                              }
{ 23.11.2000  GD  Neu                                                          }
{ 24.02.2002  GD  Erweitert um Hinzufügen und Löschen von Databases            }
{ 22.02.2006  GD  Erweitert um Wieser-Databases                                }
{ 04.05.2009  GD  Erweitert um SQL-Anweisungen                                 }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, RMG Messtechnik GmbH 2009               }
{------------------------------------------------------------------------------}
unit WiTabScript;

interface

uses
  IniFiles, SysUtils, Classes, GD_Utils, Db, WiTabObj;

const
  // Feldtypen
  C_TT_unknown      = 'unknown';
  C_TT_String       = 'string';
  C_TT_SmallInt     = 'smallint';
  C_TT_Integer      = 'integer';
  C_TT_Boolean      = 'boolean';
  C_TT_Float        = 'float';
  C_TT_Currency     = 'currency';
  C_TT_Date         = 'date';
  C_TT_Time         = 'time';
  C_TT_DateTime     = 'datetime';
  C_TT_AutoInc      = 'autoinc';
  C_TT_Blob         = 'blob';
  C_TT_Memo         = 'memo';

  C_IndexDefault    = '<kein>';

  C_WieserDb_Stamm  = 'StammDb';
  C_WieserDb_Auto   = 'AutoDb';
  C_WieserDb_Manu   = 'ManuDb';

type
  TWiTabScript = class (TIniFile)
    constructor Create(sScript: TFileName = ''); virtual;
  private
    function GetAliasDatabaseList: TStrings;
    function GetDriverDatabaseList: TStrings;
    function GetWieserDatabaseList: TStrings;
  protected
    procedure WriteFieldTypeDefs;
    procedure WriteIndexOptionsDefs;
  public
    function GetFieldType(sName: string): TFieldType;
    function GetFieldTypeName(pType: TFieldType): string;
    function GetTrueFalseBool(sName: string): boolean;
    function GetTrueFalseName(bWert: boolean): string;
    function GetIndexOption(sOption: string): TIndexOption;
    function GetIndexOptions(sOptions: string): TIndexOptions;
    function GetIndexOptionNames(pIndexOptions: TIndexOptions): string;

    function GetDbTables(sDbName: string): string;
    function GetFieldDefList(sTableName: string): TMyFieldDefList;
    procedure SetFieldDefList(pTableDef: TMyFieldDefList);
    function GetIndexDefList(sTableName: string): TMyIndexDefList;
    procedure SetIndexDefList(pTableDef: TMyIndexDefList);
    function GetSqlDefList(sTableName: string): TStrings;  // 04.05.2009

    function GetDbAlias(sDbName: string): string;
    function GetDbDriver(sDbName: string): string;
    function GetDbParams(sDbName: string): TStrings;

    function DatabaseExists(sDbName: string): boolean;
    function GetDatabaseProperties(sDbName: string; var bAlias: boolean;
      var sAliasDriver: string; var pParams: TStrings): boolean;
    procedure InsertDatabase(  // 24.02.2002
      bAlias: boolean; sDbName, sAliasDriver: string; pParams: TStrings);
    procedure DeleteDatabase(sDbName: string); // 24.02.2002

    property AliasDatabaseList: TStrings read GetAliasDatabaseList;
    property DriverDatabaseList: TStrings read GetDriverDatabaseList;
    property WieserDatabaseList: TStrings read GetWieserDatabaseList;
  end;

implementation

const
  C_Sect_FieldTypeDefs   = 'FIELDTYPEDEFS';
  C_Sect_IndexOptionDefs = 'INDEXOPTIONDEFS';
  C_Sect_AliasDatabases  = 'ALIAS_DATABASES';
  C_Sect_DriverDatabases = 'DRIVER_DATABASES';
  C_Sect_WieserDatabases = 'WIESER_DATABASES';

  C_Ident_DatabaseTables = 'DATABASETABLES';

  C_Extension_DataBase   = '_Database'; // Endung von DB-Definitions-Sections
  C_Extension_Index      = '_Index';    // Endung von Tabellen-Index-Schlüsseln
  C_Extension_Sql        = '_SQL';      // Endung von SQL-Anweisungen 

  // Feldtypen
  C_FT_unknown      = ftUnknown;
  C_FT_String       = ftString;
  C_FT_SmallInt     = ftSmallint;
  C_FT_Integer      = ftInteger;
  C_FT_Boolean      = ftBoolean;
  C_FT_Float        = ftFloat;
  C_FT_Currency     = ftCurrency;
  C_FT_Date         = ftDate;
  C_FT_Time         = ftTime;
  C_FT_DateTime     = ftDateTime;
  C_FT_AutoInc      = ftAutoInc;
  C_FT_Blob         = ftBlob;
  C_FT_Memo         = ftMemo;


  // Index-Options
  C_TO_Primary      = 'ixPrimary';
  C_TO_Unique       = 'ixUnique';
  C_TO_Descending   = 'ixDescending';
  C_TO_Expression   = 'ixExpression';
  C_TO_CaseInsensitive = 'ixCaseInsensitive';

  C_IO_Primary      = ixPrimary;
  C_IO_Unique       = ixUnique;
  C_IO_Descending   = ixDescending;
  C_IO_Expression   = ixExpression;
  C_IO_CaseInsensitive = ixCaseInsensitive;


  // Boolsche Textentsprechungen
  C_Text_True       = 'ja';
  C_Text_False      = 'nein';

{---------------------------------- TWiTabScript ---------------------------------}

{---------------------------------------------}
constructor TWiTabScript.Create(sScript: TFileName = '');
{---------------------------------------------}
begin
  if (sScript = '') then sScript := ChangeFileExt(ParamStr(0), '.WTS')
  else if (Pos(':\', sScript) = 0) then
    sScript := ExtractFilePath(ParamStr(0)) + sScript;
  inherited Create(sScript);

  WriteFieldTypeDefs;     // Feldtypen aktualisieren
  WriteIndexOptionsDefs;  // IndexOptions aktualisieren
end;

{ Trägt festgelegte Feldtypen in INI ein       }
{----------------------------------------------}
procedure TWiTabScript.WriteFieldTypeDefs;
{----------------------------------------------}
begin
  if (not SectionExists(C_Sect_FieldTypeDefs)) then
  try
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_unknown, Integer(C_FT_unknown));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_String, Integer(C_FT_String));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_SmallInt, Integer(C_FT_SmallInt));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_Integer, Integer(C_FT_Integer));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_Boolean, Integer(C_FT_Boolean));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_Float, Integer(C_FT_Float));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_Currency, Integer(C_FT_Currency));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_Date, Integer(C_FT_Date));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_Time, Integer(C_FT_Time));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_DateTime, Integer(C_FT_DateTime));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_AutoInc, Integer(C_FT_AutoInc));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_Blob, Integer(C_FT_Blob));
    WriteInteger(C_Sect_FieldTypeDefs, C_TT_Memo, Integer(C_FT_Memo));
  except
    // dann halt nicht ...
  end;
end;

{ Trägt festgelegte IndexOptionen in INI ein   }
{----------------------------------------------}
procedure TWiTabScript.WriteIndexOptionsDefs;
{----------------------------------------------}
begin
  if (not SectionExists(C_Sect_IndexOptionDefs)) then
  try
    WriteInteger(C_Sect_IndexOptionDefs, C_TO_Primary, Integer(C_IO_Primary));
    WriteInteger(C_Sect_IndexOptionDefs, C_TO_Unique, Integer(C_IO_Unique));
    WriteInteger(C_Sect_IndexOptionDefs, C_TO_Descending, Integer(C_IO_Descending));
    WriteInteger(C_Sect_IndexOptionDefs, C_TO_Expression, Integer(C_IO_Expression));
    WriteInteger(C_Sect_IndexOptionDefs, C_TO_CaseInsensitive, Integer(C_IO_CaseInsensitive));
  except
    // dann halt nicht ...
  end;
end;

{ Gibt Feldtyp zurück                         }
{ Parameter: Feltypname (s.o.)                }
{ Rückgabe: Feldtyp                           }
{---------------------------------------------}
function TWiTabScript.GetFieldType(sName: string): TFieldType;
{---------------------------------------------}
begin
  Result := TFieldType(ReadInteger(C_Sect_FieldTypeDefs, sName, 0));
end;

{ Gibt Feldtypnamen zurück                    }
{ Parameter: Feltyp (s.o.)                    }
{ Rückgabe: Feldtypname                       }
{---------------------------------------------}
function TWiTabScript.GetFieldTypeName(pType: TFieldType): string;
{---------------------------------------------}
var
  pSl : TStrings;
  i   : integer;
begin
  Result := C_TT_unknown;  // Default

  pSl := TStringList.Create;
  try
    Self.ReadSection(C_Sect_FieldTypeDefs, pSl);

    for i := 0 to pSl.Count-1 do
      if (ReadInteger(C_Sect_FieldTypeDefs , pSl[i], 0) = Integer(pType)) then
      begin
        Result := pSl[i];
        Break;
      end;
  finally
    pSl.Free;
  end;
end;

{ Gibt Indexoption zurück                     }
{ Parameter: Option als string                }
{ Rückgabe: TIndexOption                      }
{---------------------------------------------}
function TWiTabScript.GetIndexOption(sOption: string): TIndexOption;
{---------------------------------------------}
begin
  Result := TIndexOption(ReadInteger(C_Sect_IndexOptionDefs, sOption, 0));
end;

{ Gibt Indexoptionen zurück                   }
{ Parameter: Optionen als string (mit ',')    }
{ Rückgabe: set of TIndexOption               }
{---------------------------------------------}
function TWiTabScript.GetIndexOptions(sOptions: string): TIndexOptions;
{---------------------------------------------}
var
  i : integer;
begin
  Result := [];

  i := 1;
  while (GetStringPart(sOptions, i, ',') <> '') do begin
    Result := Result + [GetIndexOption(GetStringPart(sOptions, i, ','))];
    Inc(i);
  end;
end;

{ Gibt IndexOption als Text zurück            }
{ Parameter: TIndexOptions                    }
{ Rückgabe: IndexOption als Text              }
{---------------------------------------------}
function TWiTabScript.GetIndexOptionNames(pIndexOptions: TIndexOptions): string;
{---------------------------------------------}
var
  pSl : TStrings;
  i   : integer;
begin
  Result := '';  // Default

  pSl := TStringList.Create;
  try
    Self.ReadSection(C_Sect_IndexOptionDefs, pSl);

    for i := 0 to pSl.Count-1 do
      if (TIndexOption(ReadInteger(C_Sect_IndexOptionDefs, pSl[i], 0)) in pIndexOptions)
      then Result := Result + pSl[i] + ',';

    if (Length(Result) > 0) then Delete(Result, Length(Result), 1);
  finally
    pSl.Free;
  end;
end;

{ Gibt True/False zurück                      }
{ Parameter: Begriff (s.o.)                   }
{ Rückgabe: True/False                        }
{---------------------------------------------}
function TWiTabScript.GetTrueFalseBool(sName: string): boolean;
{---------------------------------------------}
begin
  Result := (LowerCase(sName) = C_Text_True);
end;

{ Gibt Begriff für True/False zurück          }
{ Parameter: True/False                       }
{ Rückgabe: Begriff (s.o.)                    }
{---------------------------------------------}
function TWiTabScript.GetTrueFalseName(bWert: boolean): string;
{---------------------------------------------}
begin
  if (bWert) then Result := C_Text_True else Result := C_Text_False;
end;

{ Gibt Liste aller Databases zurück           }
{ Rückgabe: Liste aller Databases             }
{---------------------------------------------}
function TWiTabScript.GetAliasDatabaseList: TStrings;
{---------------------------------------------}
begin
  Result := TStringList.Create;
  Self.ReadSection(C_Sect_AliasDatabases, Result);
end;

{ Gibt Liste aller Databases zurück           }
{ Rückgabe: Liste aller Databases             }
{---------------------------------------------}
function TWiTabScript.GetDriverDatabaseList: TStrings;
{---------------------------------------------}
begin
  Result := TStringList.Create;
  Self.ReadSection(C_Sect_DriverDatabases, Result);
end;

{ Gibt Liste aller Databases zurück           }
{ Rückgabe: Liste aller Databases             }
{---------------------------------------------}
function TWiTabScript.GetWieserDatabaseList: TStrings;
{---------------------------------------------}
begin
  Result := TStringList.Create;
  Self.ReadSection(C_Sect_WieserDatabases, Result);
end;

{ Gibt den Aliasnamen einer Databases zurück  }
{ Parameter: Databasename                     }
{ Rückgabe: Aliasname                         }
{---------------------------------------------}
function TWiTabScript.GetDbAlias(sDbName: string): string;
{---------------------------------------------}
begin
  if (not (ValueExists(C_Sect_AliasDatabases, sDbName)))
  then raise Exception.Create('Database ''' + sDbName + ''' nicht vorhanden !')
  else Result := GetStringPart(ReadString(C_Sect_AliasDatabases, sDbName, ''), 1, ';')
end;

{ Gibt den Drivername einer Databases zurück  }
{ Parameter: Databasename                     }
{ Rückgabe: Drivername                        }
{---------------------------------------------}
function TWiTabScript.GetDbDriver(sDbName: string): string;
{---------------------------------------------}
begin
  if (not (ValueExists(C_Sect_DriverDatabases, sDbName)))
  then raise Exception.Create('Database ''' + sDbName + ''' nicht vorhanden !')
  else Result := GetStringPart(ReadString(C_Sect_DriverDatabases, sDbName, ''), 1, ';')
end;

{ Gibt die Params für eine Databases zurück   }
{ Parameter: Databasename                     }
{ Rückgabe: Params                            }
{---------------------------------------------}
function TWiTabScript.GetDbParams(sDbName: string): TStrings;
{---------------------------------------------}
begin
  Result := TStringList.Create;

  if (not (ValueExists(C_Sect_AliasDatabases, sDbName))) and
     (not (ValueExists(C_Sect_DriverDatabases, sDbName)))
  then raise Exception.Create('Database ''' + sDbName + ''' nicht vorhanden !');

  if (ValueExists(C_Sect_AliasDatabases, sDbName)) then Result.Commatext :=
    GetStringPart(ReadString(C_Sect_AliasDatabases, sDbName, ''), 2, ';')
  else if (ValueExists(C_Sect_DriverDatabases, sDbName)) then Result.Commatext :=
    GetStringPart(ReadString(C_Sect_DriverDatabases, sDbName, ''), 2, ';');
end;

{ Gibt die Tabellen einer Databases zurück    }
{ Parameter: Databasename                     }
{ Rückgabe: Tabellennamen als Kommatext       }
{---------------------------------------------}
function TWiTabScript.GetDbTables(sDbName: string): string;
{---------------------------------------------}
var
  s : string;
begin
  s := sDbName;
  if (s <> '') and (Pos(C_DbSeparator, s) > 1) then
    s := Copy(s, 1, Pos(C_DbSeparator, s)-1);

  Result := ReadString(s + C_Extension_DataBase, C_Ident_DatabaseTables, '');
end;

{ Gibt Felddefinitionen einer Tabelle zurück  }
{ Parameter: Tabellenname                     }
{ Rückgabe: Liste mit Felddefinitionen        }
{---------------------------------------------}
function TWiTabScript.GetFieldDefList(sTableName: string): TMyFieldDefList;
{---------------------------------------------}
var
  pSl : TStrings;
  i   : integer;
  s   : string;
begin
  Result := TMyFieldDefList.Create(sTableName);

  if (SectionExists(sTableName)) then begin
    pSl := TStringList.Create;
    try
      ReadSection(sTableName, pSl);
      for i := 0 to pSl.Count-1 do begin
        s := ReadString(sTableName, pSl[i], '');
        Result.AddWert(pSl[i], GetFieldType(GetStringPart(s, 2, ';')),
          StrToIntDef(GetStringPart(s, 3, ';'), 0),
          GetTrueFalseBool(GetStringPart(s, 4, ';')));
      end;
    finally
      pSl.Free;
    end;
  end;
end;

{ Trägt Felddefinitionen einer Tabelle ein    }
{ Parameter: Liste mit Felddefinitionen       }
{---------------------------------------------}
procedure TWiTabScript.SetFieldDefList(pTableDef: TMyFieldDefList);
{---------------------------------------------}
var
  i : integer;
  s : string;
begin
  if (not Assigned(pTableDef)) then Exit;

  if (SectionExists(pTableDef.TableName)) then
    Self.EraseSection(pTableDef.TableName);

  for i := 0 to pTableDef.Count-1 do begin
    s := pTableDef.FieldDef[i].FieldName + ';';
    s := s + GetFieldTypeName(pTableDef.FieldDef[i].FieldType) + ';';
    s := s + IntToStr(pTableDef.FieldDef[i].FieldSize) + ';';
    s := s + GetTrueFalseName(pTableDef.FieldDef[i].Required);

    WriteString(pTableDef.Tablename, pTableDef[i], s);
  end;
end;

{ Gibt Indexdefinitionen einer Tabelle zurück }
{ Parameter: Tabellenname                     }
{ Rückgabe: Liste mit Indexdefinitionen       }
{---------------------------------------------}
function TWiTabScript.GetIndexDefList(sTableName: string): TMyIndexDefList;
{---------------------------------------------}
var
  pSl : TStrings;
  i   : integer;
  s   : string;
begin
  Result := TMyIndexDefList.Create(sTableName);
  sTableName := sTableName + C_Extension_Index;

  if (SectionExists(sTableName)) then begin
    pSl := TStringList.Create;
    try
      ReadSection(sTableName, pSl);
      for i := 0 to pSl.Count-1 do begin
        s := ReadString(sTableName, pSl[i], '');
        Result.AddWert(GetStringPart(s, 1, ';'),
          ChangeStringPart(GetStringPart(s, 2, ';'), ',', ';'),
          GetIndexOptions(GetStringPart(s, 3, ';')));
      end;
    finally
      pSl.Free;
    end;
  end;
end;

{ Trägt Indexdefinitionen einer Tabelle ein   }
{ Parameter: Liste mit Indexdefinitionen      }
{---------------------------------------------}
procedure TWiTabScript.SetIndexDefList(pTableDef: TMyIndexDefList);
{---------------------------------------------}
var
  i        : integer;
  s        : string;
  sSection : string;
begin
  if (not Assigned(pTableDef)) then Exit;
  sSection := pTableDef.TableName + C_Extension_Index;

  if (SectionExists(sSection)) then Self.EraseSection(sSection);

  for i := 0 to pTableDef.Count-1 do begin

    s := pTableDef.IndexDef[i].Name + ';';
    s := s + ChangeStringPart(pTableDef.IndexDef[i].Fields, ';', ',') + ';';
    s := s + GetIndexOptionNames(pTableDef.IndexDef[i].Options) + ';';

    if (pTableDef[i] = '')
    then WriteString(sSection, C_IndexDefault, s)
    else WriteString(sSection, pTableDef[i], s);
  end;
end;

{ Gibt ggf. eine SQL-Anweisung zu einer       }
{   Tabelle zurück                            }
{ Parameter: Tabellenname                     }
{ Rückgabe: SQL-Stringliste                   }
{   (Eine Anwisung pro Zeile)                 }
{---------------------------------------------}
function TWiTabScript.GetSqlDefList(sTableName: string): TStrings;
{---------------------------------------------}
var
  i : integer;
  s : string;
begin
  Result := TStringList.Create;
  s := sTableName + C_Extension_Sql;

  if (SectionExists(s)) then begin
    ReadSectionValues(s, Result);
    for i := 0 to Result.Count-1 do
      Result[i] := Result.Values[Result.Names[i]];
  end;
end;

{ Gibt zurück, ob Datenbank existiert         }
{ Parameter: Name der Db                      }
{ Rückgabe: Databasedefinition vorhanden ?    }
{---------------------------------------------}
function TWiTabScript.DatabaseExists(sDbName: string): boolean;
{---------------------------------------------}
var
  s : string;
begin
  Result := (ValueExists(C_Sect_AliasDatabases, sDbName)) or
    (ValueExists(C_Sect_DriverDatabases, sDbName));

  if (not Result) then begin
    s := sDbName;
    if (s <> '') and (Pos(C_DbSeparator, s) > 1) then begin
      s := Copy(s, 1, Pos(C_DbSeparator, s)-1);
      Result := (ValueExists(C_Sect_WieserDatabases, s));
    end;
  end;
end;

{ Gibt Eigenschaften einer Datenbank zurück   }
{ Parameter: Name der Db; T=Alias, F=Driver;  }
{            Name Alias/Driver, Parameterliste}
{ Rückgabe: Databasedefinition vorhanden ?    }
{---------------------------------------------}
function TWiTabScript.GetDatabaseProperties(sDbName: string;
  var bAlias: boolean; var sAliasDriver: string; var pParams: TStrings): boolean;
{---------------------------------------------}
begin
  sAliasDriver := '';
  pParams := nil;

  if (ValueExists(C_Sect_AliasDatabases, sDbName)) then begin
    bAlias := True;
    sAliasDriver := GetDbAlias(sDbName);
    pParams := GetDbParams(sDbName);
  end
  else if (ValueExists(C_Sect_DriverDatabases, sDbName)) then begin
    bAlias := False;
    sAliasDriver := GetDbDriver(sDbName);
    pParams := GetDbParams(sDbName);
  end;

  Result := (Trim(sAliasDriver) <> '');
end;

{ Trägt neue Datenbank in Skriptfile ein      }
{ Parameter: T=Alias, F=Driver; Name der Db;  }
{            Name Alias/Driver, Parameterliste}
{---------------------------------------------}
procedure TWiTabScript.InsertDatabase(
  bAlias: boolean; sDbName, sAliasDriver: string; pParams: TStrings);
{---------------------------------------------}
var
  s, sSection : string;
begin
  if (bAlias) then begin
    sSection := C_Sect_AliasDatabases;
    DeleteKey(C_Sect_DriverDatabases, sDbName);
  end
  else begin
    sSection := C_Sect_DriverDatabases;
    DeleteKey(C_Sect_AliasDatabases, sDbName);
  end;
  s := sAliasDriver + ';' + pParams.CommaText;
  WriteString(sSection, sDbName, s);
end;

{ Löscht Datenbank aus Skriptfile             }
{ Parameter: T=Alias, F=Driver; Name Al./Dr.  }
{---------------------------------------------}
procedure TWiTabScript.DeleteDatabase(sDbName: string);
{---------------------------------------------}
var
  sSection : string;
begin
  if (ValueExists(C_Sect_AliasDatabases, sDbName)) then
    sSection := C_Sect_AliasDatabases
  else if (ValueExists(C_Sect_DriverDatabases, sDbName)) then
    sSection := C_Sect_DriverDatabases
  else Exit;

  DeleteKey(sSection, sDbName);
end;

end.
