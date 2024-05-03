{------------------------------------------------------------------------------}
{ Objecte für Tabellenprüf-Programm                                            }
{                                                                              }
{ 22.02.2006  GD  Erweitert um Wieser-Databases                                }
{ 12.02.2013  GD  Vs. 2.11 Erweitertes Logging                                 }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2000, RMG Messtechnik GmbH 2013               }
{------------------------------------------------------------------------------}
unit WiTabObj;

interface

uses
  SysUtils, Classes, DbTables, DB, DbClient, Provider,
  GD_Utils;

const
  C_DbSeparator = '__';

type
  TMyFieldDef = record
    FieldName  : string;
    FieldType  : TFieldType;
    FieldSize  : byte;
    Required   : boolean;
  end;
  PMyFieldDef = ^TMyFieldDef;

  TMyIndexDef = record
    Name       : string;
    Fields     : string;
    Options    : TIndexOptions;
  end;
  PMyIndexDef = ^TMyIndexDef;  

  TMyFieldDefList = class(TStringList)
    constructor Create(sTableName: string); virtual;
    destructor Destroy; override;
  private
    FTableName : string;
    function GetFieldDef(iIndex: integer): TMyFieldDef;
  protected
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    procedure AddWert(sFieldName: string; pFieldType: TFieldType;
      iFieldSize: byte; bRequired: boolean);
    property TableName: string read FTableName write FTableName;
    property FieldDef [iIndex: integer]: TMyFieldDef read GetFieldDef;
  end;

  TMyIndexDefList = class(TStringList)
    constructor Create(sTableName: string); virtual;
    destructor Destroy; override;
  private
    FTableName : string;
    function GetIndexDef(iIndex: integer): TMyIndexDef;
  protected
  public
    procedure Delete(iIndex: integer); override;
    procedure Clear; override;
    procedure AddWert(sName, sFields: string; pOptions: TIndexOptions);
    property TableName: string read FTableName write FTableName;
    property IndexDef [iIndex: integer]: TMyIndexDef read GetIndexDef;
  end;

  TMyTableDef = class(TObject)
  private
    FTableName    : string;
    FDatabaseName : string;
    FFieldDef     : TMyFieldDefList;
    FIndexDef     : TMyIndexDefList;
  protected
  public
    property TableName: string read FTableName write FTableName;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property FieldDef: TMyFieldDefList read FFieldDef write FFieldDef;
    property IndexDef: TMyIndexDefList read FIndexDef write FIndexDef;
  end;

  TDatabaseList = class(TList)
    destructor Destroy; override;
  private
    function GetMyDatabase(iIndex: integer): TDatabase;
  protected
  public
    function GetIniDatabaseName(const sDatabaseName: string): string;
    function GetDatabase(sDatabaseName: string): TDatabase;
    procedure Clear; override;
    procedure Delete(Index: integer); virtual;
    property Database [iIndex: integer]: TDatabase read GetMyDatabase;
  end;

procedure MyRestructureTable(pTable: TTable; pTableDef: TMyTableDef);

implementation

{--------------------------- Allgemeine Funktionen ----------------------------}

{ Tabelle mit Soll-Struktur neu aufbauen      }
{ Parameter: Tabelle, Tabellendefinition      }
{---------------------------------------------}
procedure MyRestructureTable(pTable: TTable; pTableDef: TMyTableDef);
{---------------------------------------------}
var
  i, iCDS, iTab : integer;
  pCDS          : TClientDataSet;
  pDSP          : TDataSetProvider;
begin
  if (not Assigned(pTableDef)) then Exit;
  pCDS := nil;
  pDSP := nil;

  try

    if (Assigned(pTable)) and (pTable.Exists) then begin
      // Tabellendaten zwischenspeichern, Tabelle löschen
      if (pTable.Active) then pTable.Close;

      pCDS := TClientDataSet.Create(nil);
      pDSP := TDataSetProvider.Create(nil);

      pDSP.DataSet := pTable;
      pCDS.SetProvider(pDSP);
      pCDS.Open;
      pTable.DeleteTable;
      pTable.FieldDefs.Clear;
      pTable.IndexDefs.Clear;
    end;

    // Tabelle neu erstellen
    pTable.DatabaseName := pTableDef.DatabaseName;
    pTable.TableName := pTableDef.TableName;
    for i := 0 to pTableDef.FieldDef.Count-1 do
      pTable.FieldDefs.Add(pTableDef.FieldDef.FieldDef[i].FieldName,
        pTableDef.FieldDef.FieldDef[i].FieldType,
        pTableDef.FieldDef.FieldDef[i].FieldSize,
        pTableDef.FieldDef.FieldDef[i].Required);
    for i := 0 to pTableDef.IndexDef.Count-1 do
      pTable.IndexDefs.Add(pTableDef.IndexDef.IndexDef[i].Name,
        pTableDef.IndexDef.IndexDef[i].Fields,
        pTableDef.IndexDef.IndexDef[i].Options);
    pTable.CreateTable;

    // Ggf. Tabelle füllen
    if (Assigned(pCDS)) then begin
      pTable.Open;
      pCDS.First;
      iCDS := pCDS.RecordCount;
      while (not pCDS.Eof) do begin
        pTable.Append;
        for i := 0 to pTable.FieldDefs.Count-1 do begin
          if (pCDS.FieldDefs.IndexOf(pTable.Fields[i].FieldName) >= 0) then
            pTable.Fields[i].Value := pCDS.FieldByName(pTable.Fields[i].FieldName).Value;
        end;
        pTable.Post;
        pCDS.Next;
      end;
      iTab := pTable.RecordCount;
      if (iCDS <> iTab) then  // 12.02.2013
        WriteErrorLog('MyRestructureTable (' + pTableDef.TableName + '): CDS: '
           + IntToStr(iCDS) + '; Table: ' + IntToStr(iTab),
           ExtractFilePath(ParamStr(0)) + 'TABGEN.ERR');
    end;

  finally
    if (Assigned(pDSP)) then pDSP.Free;
    if (Assigned(pCDS)) then pCDS.Free;
  end;
end;

{------------------------------- TMyFieldDefList ------------------------------}

{ Überschriebene Methode                       }
{----------------------------------------------}
constructor TMyFieldDefList.Create(sTableName: string);
{----------------------------------------------}
begin
  inherited Create;

  FTableName := sTableName;
end;

{ Überschriebene Methode                       }
{----------------------------------------------}
destructor TMyFieldDefList.Destroy;
{----------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
  try
    Dispose(PMyFieldDef(Objects[i]));
  except
  // Fehlermeldung unterdrücken
  end;

  inherited Destroy;
end;

{ Überschriebene Methode                       }
{----------------------------------------------}
procedure TMyFieldDefList.Delete(iIndex: integer);
{----------------------------------------------}
begin
  try
    Dispose(PMyFieldDef(Objects[iIndex]));
  except
  // Fehlermeldung unterdrücken
  end;

  inherited Delete(iIndex);
end;

{ Überschriebene Methode                       }
{----------------------------------------------}
procedure TMyFieldDefList.Clear;
{----------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
  try
    Dispose(PMyFieldDef(Objects[i]));
  except
  // Fehlermeldung unterdrücken
  end;

  inherited Clear;
end;

{ Gibt Felddefinition eines Eintrags zurück    }
{ Parameter: Listenindex                       }
{ Rückgabe: Felddefinition                     }
{----------------------------------------------}
function TMyFieldDefList.GetFieldDef(iIndex: integer): TMyFieldDef;
{----------------------------------------------}
begin
  if (iIndex < 0) or (iIndex >= Count)
  then raise Exception.Create('Index (' + IntToStr(iIndex) + ') außerhalb ' +
    'des gültigen Bereichs (' + IntToStr(Count) + ') !')
  else
  try
    Result.FieldName := PMyFieldDef(Objects[iIndex])^.FieldName;
    Result.FieldType := PMyFieldDef(Objects[iIndex])^.FieldType;
    Result.FieldSize := PMyFieldDef(Objects[iIndex])^.FieldSize;
    Result.Required := PMyFieldDef(Objects[iIndex])^.Required;
  except
    Result.FieldName := '';
    Result.FieldType := TFieldType(0);
    Result.FieldSize := 0;
    Result.Required := False;
  end;
end;

{ Fügt Wert zur Liste hinzu                    }
{ Parameter: Feldname, -typ, -größe, required  }
{----------------------------------------------}
procedure TMyFieldDefList.AddWert(sFieldName: string; pFieldType: TFieldType;
  iFieldSize: byte; bRequired: boolean);
{----------------------------------------------}
var
  p : PMyFieldDef;
begin
  New(p);
  p^.FieldName := sFieldName;
  p^.FieldType := pFieldType;
  p^.FieldSize := iFieldSize;
  p^.Required := bRequired;

  AddObject(sFieldName, TObject(p));
end;

{------------------------------- TMyIndexDefList ------------------------------}

{ Überschriebene Methode                       }
{----------------------------------------------}
constructor TMyIndexDefList.Create(sTableName: string);
{----------------------------------------------}
begin
  inherited Create;

  FTableName := sTableName;
end;

{ Überschriebene Methode                       }
{----------------------------------------------}
destructor TMyIndexDefList.Destroy;
{----------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
  try
    Dispose(PMyIndexDef(Objects[i]));
  except
  // Fehlermeldung unterdrücken
  end;

  inherited Destroy;
end;

{ Überschriebene Methode                       }
{----------------------------------------------}
procedure TMyIndexDefList.Delete(iIndex: integer);
{----------------------------------------------}
begin
  try
    Dispose(PMyIndexDef(Objects[iIndex]));
  except
  // Fehlermeldung unterdrücken
  end;

  inherited Delete(iIndex);
end;

{ Überschriebene Methode                       }
{----------------------------------------------}
procedure TMyIndexDefList.Clear;
{----------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
  try
    Dispose(PMyIndexDef(Objects[i]));
  except
  // Fehlermeldung unterdrücken
  end;

  inherited Clear;
end;

{ Gibt Indexdefinition eines Eintrags zurück   }
{ Parameter: Listenindex                       }
{ Rückgabe: Indexdefinition                     }
{----------------------------------------------}
function TMyIndexDefList.GetIndexDef(iIndex: integer): TMyIndexDef;
{----------------------------------------------}
begin
  if (iIndex < 0) or (iIndex >= Count)
  then raise Exception.Create('Index (' + IntToStr(iIndex) + ') außerhalb ' +
    'des gültigen Bereichs (' + IntToStr(Count) + ') !')
  else
  try
    Result.Name := PMyIndexDef(Objects[iIndex])^.Name;
    Result.Fields := PMyIndexDef(Objects[iIndex])^.Fields;
    Result.Options := PMyIndexDef(Objects[iIndex])^.Options;
  except
    Result.Name := '';
    Result.Fields := '';
    Result.Options := [];
  end;
end;

{ Fügt Wert zur Liste hinzu                    }
{ Parameter: Feldname, -typ, -größe, required  }
{----------------------------------------------}
procedure TMyIndexDefList.AddWert(sName, sFields: string; pOptions: TIndexOptions);
{----------------------------------------------}
var
  p : PMyIndexDef;
begin
  New(p);
  p^.Name := sName;
  p^.Fields := sFields;
  p^.Options := pOptions;

  AddObject(sName, TObject(p));
end;

{-------------------------------- TDatabaseList -------------------------------}

{ Überschriebene Methode                      }
{---------------------------------------------}
destructor TDatabaseList.Destroy;
{---------------------------------------------}
begin
  Clear;

  inherited Destroy;
end;

{ Überschriebene Methode                      }
{---------------------------------------------}
procedure TDatabaseList.Delete(Index: integer);
{---------------------------------------------}
begin
  try
    TDatabase(Items[Index]).Free;
  except
  //
  end;

  inherited Delete(Index);
end;

{ Überschriebene Methode                      }
{---------------------------------------------}
procedure TDatabaseList.Clear;
{---------------------------------------------}
var
  i : integer;
begin
  for i := Count-1 downto 0 do
  try
    TDatabase(Items[i]).Free;
  except
  //
  end;

  inherited Clear;
end;

{---------------------------------------------}
function TDatabaseList.GetIniDatabaseName(const sDatabaseName: string): string;
{---------------------------------------------}
var
  i : integer;
begin
  Result := '';

  // Prüfen auf gleichen Namen
  for i := 0 to Count-1 do
    if (LowerCase(Database[i].DatabaseName) = LowerCase(sDatabaseName)) then
    begin
      Result := sDatabaseName;
      Break;
    end;

  if (Result <> '') and (Pos(C_DbSeparator, Result) > 1) then
    Result := Copy(sDatabaseName, 1, Pos(C_DbSeparator, sDatabaseName)-1);
end;

{ Gibt Datenbank zu Index zurück              }
{ Parameter: Index                            }
{ Rückgabe: Zeiger auf Database oder nil      }
{---------------------------------------------}
function TDatabaseList.GetMyDatabase(iIndex: integer): TDatabase;
{---------------------------------------------}
begin
  Result := nil;  // Vorgabe

  try
    Result := TDatabase(Items[iIndex]);
  except
  // Fehlermeldung unterdrücken
  end;
end;

{ Gibt Datenbank zu Namen zurück              }
{ Parameter: DatabaseName                     }
{ Rückgabe: Zeiger auf Database oder nil      }
{---------------------------------------------}
function TDatabaseList.GetDatabase(sDatabaseName: string): TDatabase;
{---------------------------------------------}
var
  i : integer;
  s : string;
begin
  Result := nil;  // Vorgabe

  // Prüfen auf gleichen Namen
  for i := 0 to Count-1 do
    if (LowerCase(Database[i].DatabaseName) = LowerCase(sDatabaseName)) then begin
      Result := Database[i];
      Break;
    end;

  // Ggf. Prüfen auf erweiterten Namen  // 22.02.2006
  if (not Assigned(Result)) then begin;
    for i := 0 to Count-1 do begin
      if (Pos(C_DbSeparator, Database[i].DatabaseName) > 1) then begin
        s := LowerCase(Copy(Database[i].DatabaseName, 1,
          Pos(C_DbSeparator, Database[i].DatabaseName)-1));
        if (s = LowerCase(sDatabaseName)) then begin
          Result := Database[i];
          Break;
        end;
      end;
    end;
  end;
end;

end.
