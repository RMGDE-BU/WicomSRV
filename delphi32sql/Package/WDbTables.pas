{------------------------------------------------------------------------------}
{ Wieser-Datenbankobjekte                                                      }
{                                                                              }
{ 13.06.2005  GD  Neu                                                          }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2005                                          }
{------------------------------------------------------------------------------}
unit WDbTables;

interface

uses
  SysUtils, Classes, Db, DbTables, AdoDb, FileCtrl;

type
  TWSession = class;
  TWDataBase = class;
  TWDataSet = class;
  TWTable = class;
  TWQuery = class;

  TWSession = class(TObject)
    constructor Create(pOwner: TComponent); virtual;
    destructor Destroy; override;
  private
    FSession : TSession;
  protected
  public
    procedure GetTableNames(const sDatabaseName, sPattern: string;
      bExtensions, bSystemTables: boolean; pList: TStrings);
  published
  end;

  TWDataBase = class(TObject)
    constructor Create(pOwner: TComponent); virtual;
    destructor Destroy; override;
  private
    FDatabase : TDatabase;
  protected
    function GetDatabaseName: string; virtual;
    procedure SetDatabaseName(sDatabaseName: string); virtual;
    function GetActive: boolean; virtual;
    procedure SetActive(bActive: boolean); virtual;
  public
  published
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property Active: boolean read GetActive write SetActive;
  end;

  TWDataSet = class(TObject)
    constructor Create(pOwner: TComponent); virtual;
    destructor Destroy; override;
  private
    FOwner   : TComponent;
    FDataSet : TDataSet;
  protected
    function GetDataSet: TDataSet; virtual;
    procedure SetDataset(pDataSet: TDataSet); virtual;
    function GetDatabaseName: string; virtual; abstract;
    procedure SetDatabaseName(sDatabaseName: string); virtual; abstract;
    function GetActive: boolean; virtual;
    procedure SetActive(bActive: boolean); virtual;
    function GetEof: boolean; virtual;
    function GetBof: boolean; virtual;
    function GetRecordCount: integer; virtual;
    function GetFieldByIndex(iIndex: integer): TField; virtual;
    property Owner: TComponent read FOwner;
  public
    function Open: boolean; virtual;
    function Close: boolean; virtual;
    procedure First; virtual;
    procedure Last; virtual;
    procedure Next; virtual;
    procedure Prior; virtual;
    procedure Edit; virtual;
    procedure Append; virtual;
    procedure Post; virtual;
    procedure Cancel; virtual;
    function FieldByName(const sFieldName: string): TField; virtual;

    property Eof: boolean read GetEof;
    property Bof: boolean read GetBof;
    property RecordCount: integer read GetRecordCount;
    property Fields[iIndex: integer]: TField read GetFieldByIndex;
    property DataSet: TDataSet read GetDataSet;
  published
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property Active: boolean read GetActive write SetActive;
  end;

  TWTable = class(TWDataSet)
  private
    FTable : TTable;
  protected
    procedure SetDataset(pDataSet: TDataSet); override;
    function GetDatabaseName: string; override;
    procedure SetDatabaseName(sDatabaseName: string); override;
    function GetTableName: string; virtual;
    procedure SetTableName(sTableName: string); virtual;
    function GetExists: boolean; virtual;
    function GetFieldDefs: TFieldDefs; virtual;
    function GetIndexDefs: TIndexDefs; virtual;
  public
    function FindKey(const pKeyValues: array of const): boolean; virtual;
    procedure CreateTable; virtual;

    property FieldDefs: TFieldDefs read GetFieldDefs;
    property IndexDefs: TIndexDefs read GetIndexDefs;
    property Exists: boolean read GetExists;
  published
    property TableName: string read GetTableName write SetTableName;
  end;

  TWQuery = class(TWDataSet)
  private
    FQuery : TQuery;
  protected
    procedure SetDataset(pDataSet: TDataSet); override;
    function GetDatabaseName: string; override;
    procedure SetDatabaseName(sDatabaseName: string); override;
    function GetSql: TStrings; virtual;
    procedure SetSql(pSl: TStrings); virtual;
  public
    procedure ExecSql; virtual;
  published
    property Sql: TStrings read GetSql write SetSql;
  end;

function WTableExists(const sDatabaseName, sTablename: string): boolean;
function WDeleteTable(const sDatabaseName, sTablename: string): boolean;
function WCopyTableStruc(pOriTable, pNewTable: TWTable): boolean; overload;
function WCopyTableStruc(const sOriDatabaseName, sOriTablename,
  sNewDatabaseName, sNewTablename: string): boolean; overload;

implementation

uses TypInfo;

{---------------------------- Allgemeine Funktionen ---------------------------}

{ Prüft ab, ob eine Tabelle existiert     }
{ Parameter: Datenbankname, Tabellenname  }
{ Rückgabe: Existiert ja/nein             }
{-----------------------------------------}
function WTableExists(const sDatabaseName, sTablename: string): boolean;
{-----------------------------------------}
begin
  with TWTable.Create(nil) do
  try
    DatabaseName := sDatabaseName;
    TableName := sTablename;
    Result := Exists;
  finally
    Free;
  end;
end;

{ Löscht eine Tabelle                     }
{ Parameter: Datenbankname, Tabellenname  }
{ Rückgabe: Gelöscht ja/nein              }
{-----------------------------------------}
function WDeleteTable(const sDatabaseName, sTablename: string): boolean;
{-----------------------------------------}
begin
  if (WTableExists(sDatabaseName, sTablename)) then begin
    with TWQuery.Create(nil) do
    try
      DatabaseName := sDatabaseName;
      Sql.Text := 'DROP TABLE ' + ChangeFileExt(sTablename, '');
      try
        ExecSql;
        Result := True;
      except
        Result := False;
      end;
    finally
      Free;
    end;
  end
  else Result := True;
end;

{ Erstellt eine Tabelle mit der Struktur  }
{ einer vorgegebenen Tabelle              }
{ Parameter: Quelltabelle, neue Tabelle   }
{ Rückgabe: Erstellt ja/nein              }
{-----------------------------------------}
function WCopyTableStruc(pOriTable, pNewTable: TWTable): boolean;
{-----------------------------------------}
begin
  // Neue Tabelle erzeugen
  pOriTable.FieldDefs.Update;
  pNewTable.FieldDefs.Assign(pOriTable.FieldDefs);
  pOriTable.IndexDefs.Update;
  pNewTable.IndexDefs.Assign(pOriTable.IndexDefs);

  pNewTable.CreateTable;
  Result := True;
end;

{ Erstellt eine Tabelle mit der Struktur  }
{ einer vorgegebenen Tabelle              }
{ Parameter: Quelldatenbankname, Quell-   }
{            tabellenname, neuer Daten-   }
{            bankname, neuer Tabellenname }
{ Rückgabe: Erstellt ja/nein              }
{-----------------------------------------}
function WCopyTableStruc(const sOriDatabaseName, sOriTablename,
  sNewDatabaseName, sNewTablename: string): boolean;
{-----------------------------------------}
var
  pTbOri, pTbNew : TWTable;
begin
  if (WTableExists(sOriDatabaseName, sOriTablename)) and
    (WDeleteTable(sNewDatabaseName, sNewTablename)) then
  begin
    pTbOri := TWTable.Create(nil);
    pTbNew := TWTable.Create(nil);
    try
      pTbOri.DatabaseName := sOriDatabaseName;
      pTbOri.TableName := sOriTableName;
      pTbNew.DatabaseName := sNewDatabaseName;
      pTbNew.TableName := sNewTableName;
      try
        Result := WCopyTableStruc(pTbOri, pTbNew);
      except
        on E:Exception do begin
          // Brutalmaßnahme: ggf. wird VAL-Datei gelöscht
          if (Pos(LowerCase(ChangeFileExt(sOriTableName, '.val')),
            LowerCase(E.Message)) > 0) and (DirectoryExists(sOriDatabaseName))
            and (DeleteFile(IncludeTrailingBackslash(sOriDatabaseName) +
            ChangeFileExt(sOriTableName, '.val'))) then
          begin
            Result := WCopyTableStruc(pTbOri, pTbNew);;
          end
          else Result := False;
        end;
      end;
    finally
      pTbOri.Free;
      pTbNew.Free;
    end;
  end
  else Result := False;
end;

{------------------------------------ TWSession -------------------------------}

{-----------------------------------------}
constructor TWSession.Create(pOwner: TComponent);
{-----------------------------------------}
begin
  inherited Create;

  FSession := Session;
end;

{-----------------------------------------}
destructor TWSession.Destroy;
{-----------------------------------------}
begin

  inherited Destroy;
end;

{-----------------------------------------}
procedure TWSession.GetTableNames(const sDatabaseName, sPattern: string;
  bExtensions, bSystemTables: boolean; pList: TStrings);
{-----------------------------------------}
begin
  FSession.GetTableNames(
    sDatabaseName, sPattern, bExtensions, bSystemTables, pList);
end;

{------------------------------------ TWDataBase ------------------------------}

{-----------------------------------------}
constructor TWDataBase.Create(pOwner: TComponent);
{-----------------------------------------}
begin
  inherited Create;

  FDatabase := TDatabase.Create(pOwner);
end;

{-----------------------------------------}
destructor TWDataBase.Destroy;
{-----------------------------------------}
begin
  if (FDatabase.Connected) then FDatabase.Connected := False;
  FreeAndNil(FDatabase);

  inherited Destroy;
end;

{-----------------------------------------}
function TWDataBase.GetDatabaseName: string;
{-----------------------------------------}
begin
  Result := FDatabase.DatabaseName;
end;

{-----------------------------------------}
procedure TWDataBase.SetDatabaseName(sDatabaseName: string);
{-----------------------------------------}
begin
  if (LowerCase(FDatabase.DatabaseName) <> LowerCase(sDatabaseName)) then begin
    FDatabase.DatabaseName := sDatabaseName;
  end;
end;

{-----------------------------------------}
function TWDataBase.GetActive: boolean;
{-----------------------------------------}
begin
  Result := FDatabase.Connected;
end;

{-----------------------------------------}
procedure TWDataBase.SetActive(bActive: boolean);
{-----------------------------------------}
begin
  if (FDatabase.Connected <> bActive) then begin
    FDatabase.Connected := bActive;
  end;
end;

{------------------------------------ TWDataSet -------------------------------}

{-----------------------------------------}
constructor TWDataSet.Create(pOwner: TComponent);
{-----------------------------------------}
begin
  inherited Create;

  FOwner := pOwner;
  SetDataset(nil);
end;

{-----------------------------------------}
destructor TWDataSet.Destroy;
{-----------------------------------------}
begin
  Active := False;
  FreeAndNil(FDataSet);

  inherited Destroy;
end;

{-----------------------------------------}
function TWDataSet.GetDataSet: TDataSet;
{-----------------------------------------}
begin
  Result := FDataSet;
end;

{-----------------------------------------}
procedure TWDataSet.SetDataset(pDataSet: TDataSet);
{-----------------------------------------}
begin
  if (Assigned(pDataSet)) then FDataSet := pDataSet;
end;

{-----------------------------------------}
function TWDataSet.GetActive: boolean;
{-----------------------------------------}
begin
  Result := FDataSet.Active;
end;

{-----------------------------------------}
procedure TWDataSet.SetActive(bActive: boolean);
{-----------------------------------------}
begin
  if (Active <> bActive) then FDataSet.Active := bActive;
end;

{-----------------------------------------}
function TWDataSet.GetEof: boolean;
{-----------------------------------------}
begin
  Result := FDataSet.Eof;
end;

{-----------------------------------------}
function TWDataSet.GetBof: boolean;
{-----------------------------------------}
begin
  Result := FDataSet.Bof;
end;

{-----------------------------------------}
function TWDataSet.GetRecordCount: integer;
{-----------------------------------------}
begin
  Result := FDataSet.RecordCount;
end;

{-----------------------------------------}
function TWDataSet.GetFieldByIndex(iIndex: integer): TField;
{-----------------------------------------}
begin
  Result := FDataSet.Fields[iIndex];
end;

{-----------------------------------------}
function TWDataSet.Open: boolean;
{-----------------------------------------}
begin
  if (not Active) then Active := True;
  Result := Active;
end;

{-----------------------------------------}
function TWDataSet.Close: boolean;
{-----------------------------------------}
begin
  if (Active) then Active := True;
  Result := not Active;
end;

{-----------------------------------------}
procedure TWDataSet.First;
{-----------------------------------------}
begin
  FDataSet.First;
end;

{-----------------------------------------}
procedure TWDataSet.Last;
{-----------------------------------------}
begin
  FDataSet.Last;
end;

{-----------------------------------------}
procedure TWDataSet.Next;
{-----------------------------------------}
begin
  FDataSet.Next;
end;

{-----------------------------------------}
procedure TWDataSet.Prior;
{-----------------------------------------}
begin
  FDataSet.Prior;
end;

{-----------------------------------------}
procedure TWDataSet.Edit;
{-----------------------------------------}
begin
  FDataSet.Edit;
end;

{-----------------------------------------}
procedure TWDataSet.Append;
{-----------------------------------------}
begin
  FDataSet.Append;
end;

{-----------------------------------------}
procedure TWDataSet.Post;
{-----------------------------------------}
begin
  FDataSet.Post;
end;

{-----------------------------------------}
procedure TWDataSet.Cancel;
{-----------------------------------------}
begin
  FDataSet.Cancel;
end;

{-----------------------------------------}
function TWDataSet.FieldByName(const sFieldName: string): TField;
{-----------------------------------------}
begin
  Result := FDataSet.FieldByName(sFieldName);
end;

{------------------------------------- TWTable --------------------------------}

{-----------------------------------------}
procedure TWTable.SetDataset(pDataSet: TDataSet);
{-----------------------------------------}
begin
  if (not Assigned(pDataSet)) then begin
    FTable := TTable.Create(Owner);
    SetDataset(FTable);
  end
  else inherited;
end;

{-----------------------------------------}
function TWTable.GetDatabaseName: string;
{-----------------------------------------}
begin
  Result := FTable.DatabaseName;
end;

{-----------------------------------------}
procedure TWTable.SetDatabaseName(sDatabaseName: string);
{-----------------------------------------}
begin
  if (LowerCase(FTable.DatabaseName) <> LowerCase(sDatabaseName)) then begin
    FTable.DatabaseName := sDatabaseName;
  end;
end;

{-----------------------------------------}
function TWTable.GetTableName: string;
{-----------------------------------------}
begin
  Result := FTable.TableName;
end;

{-----------------------------------------}
procedure TWTable.SetTableName(sTableName: string);
{-----------------------------------------}
begin
  if (LowerCase(FTable.TableName) <> LowerCase(sTableName)) then begin
    FTable.TableName := sTableName;
  end;
end;

{-----------------------------------------}
function TWTable.GetExists: boolean;
{-----------------------------------------}
begin
  Result := FTable.Exists;
end;

{-----------------------------------------}
function TWTable.FindKey(const pKeyValues: array of const): boolean;
{-----------------------------------------}
begin
  Result := FTable.FindKey(pKeyValues);
end;

{-----------------------------------------}
function TWTable.GetFieldDefs: TFieldDefs;
{-----------------------------------------}
begin
  Result := FTable.FieldDefs;
end;

{-----------------------------------------}
function TWTable.GetIndexDefs: TIndexDefs;
{-----------------------------------------}
begin
  Result := FTable.IndexDefs;
end;

{-----------------------------------------}
procedure TWTable.CreateTable;
{-----------------------------------------}
begin
  FTable.CreateTable;
end;

{------------------------------------- TWQuery --------------------------------}

{-----------------------------------------}
procedure TWQuery.SetDataset(pDataSet: TDataSet);
{-----------------------------------------}
begin
  if (not Assigned(pDataSet)) then begin
    FQuery := TQuery.Create(Owner);
    SetDataset(FQuery);
  end
  else inherited;
end;


{-----------------------------------------}
function TWQuery.GetDatabaseName: string;
{-----------------------------------------}
begin
  Result := FQuery.DatabaseName;
end;

{-----------------------------------------}
procedure TWQuery.SetDatabaseName(sDatabaseName: string);
{-----------------------------------------}
begin
  if (LowerCase(FQuery.DatabaseName) <> LowerCase(sDatabaseName)) then begin
    FQuery.DatabaseName := sDatabaseName;
  end;
end;

{-----------------------------------------}
function TWQuery.GetSql: TStrings;
{-----------------------------------------}
begin
  Result := FQuery.SQL;
end;

{-----------------------------------------}
procedure TWQuery.SetSql(pSl: TStrings);
{-----------------------------------------}
begin
  FQuery.SQL := pSl;
end;

{-----------------------------------------}
procedure TWQuery.ExecSql;
{-----------------------------------------}
begin
  FQuery.ExecSQL;
end;

end.
