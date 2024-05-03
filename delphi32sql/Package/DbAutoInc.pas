{------------------------------------------------------------------------------}
{ Objekt für das Autoinkrementieren von Tabellen-Indizes                       }
{                                                                              }
{ 11.11.2002  GD  Neu                                                          }
{ 02.06.2006  GD  Für SQL SERVER mit Trabactionskontrolle                      }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2002, 2006                                    }
{------------------------------------------------------------------------------}
unit DbAutoInc;

interface

uses
  SysUtils, Classes, DbTables, Db, BDE,
  WTables;

type
  TDbAutoInc = class(TObject)
    constructor Create(pDatabase: TDatabase); virtual;
    destructor Destroy; override;
  private
    FDatabase     : TDatabase;
    FOpened       : boolean;
    FTable        : TTableExt;
  protected
    function CreateTable: boolean; virtual;
    function GetNextIndex(const sTbName: string): integer; virtual;
    procedure SetNextIndex(const sTbName: string; iIndex: integer); virtual;
  public
    property NextIndex [const sTbName: string]: integer
      read GetNextIndex write SetNextIndex;
  end;

implementation

const
  C_Tb_TAutoInc          = 'tautoinc';

  C_Tf_TAutoInc_TbName   = 'tbname';   // str20
  C_Tf_TAutoInc_AktIndex = 'aktindex'; // str20

  C_TI_TAutoInc_Index    = 'ixmain';   // Primärindex

{---------------------------------------------}
constructor TDbAutoInc.Create(pDatabase: TDatabase);
{---------------------------------------------}
begin
  inherited Create;

  FDatabase := pDatabase;
  FOpened := CreateTable;
  FTable := TTableExt.Create(nil);
  FTable.TableName := C_Tb_TAutoInc;
  FTable.DatabaseName := FDatabase.DatabaseName;
  FTable.SessionName := FDatabase.SessionName;
  FTable.SafeUpdateIndexDefs;
  FTable.IndexName := FTable.IndexDefs[0].Name;
end;

{---------------------------------------------}
destructor TDbAutoInc.Destroy;
{---------------------------------------------}
begin
  FTable.Free;

  inherited;
end;

{ Legt ggf. Tabelle neu an                    }
{ Rückgabe: Tabelle vorhanden ja/nein         }
{---------------------------------------------}
function TDbAutoInc.CreateTable: boolean;
{---------------------------------------------}
begin
  try
    with TTableExt.Create(nil) do
    try
      TableName := C_Tb_TAutoInc;
      DatabaseName := FDatabase.DatabaseName;
      SessionName := FDatabase.SessionName;
      Result := Exists;

      if (not Exists) then begin
        with FieldDefs.AddFieldDef do begin
          Name := C_Tf_TAutoInc_TbName;
          DataType := ftString;
          Size := 20;
          Required := True;
        end;
        with FieldDefs.AddFieldDef do begin
          Name := C_Tf_TAutoInc_AktIndex;
          DataType := ftInteger;
          Required := True;
        end;
        with IndexDefs.AddIndexDef do begin
          Name := C_TI_TAutoInc_Index;
          Fields := C_Tf_TAutoInc_TbName;
          Options := [ixPrimary, ixUnique];
        end;
        CreateTable;
        Result := True;
      end;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

{ Gibt nächsten Index zurück und inkrementiert}
{ Parameter: Name der betreffenden Tabelle    }
{ Rückgabe: Nächster AutoInc-Index für Tabelle}
{---------------------------------------------}
function TDbAutoInc.GetNextIndex(const sTbName: string): integer;
{---------------------------------------------}

  function GetNextSqlIndex: integer;
  var
    bTransact : boolean;
  begin
    bTransact := (not FDatabase.InTransaction);
    if (bTransact) then FDatabase.StartTransaction;
    try
      with TQuery.Create(nil) do
      try
        DatabaseName := FDatabase.DatabaseName;
        SessionName := FDatabase.SessionName;
        Sql.Text := 'UPDATE ' + C_Tb_TAutoInc +
          ' SET ' + C_Tf_TAutoInc_AktIndex + ' = ' + C_Tf_TAutoInc_AktIndex + '+1' +
          ' WHERE ' + C_Tf_TAutoInc_TbName + ' = ''' + LowerCase(sTbName) + '''';
        ExecSQL;
        Sql.Text := 'SELECT ' + C_Tf_TAutoInc_AktIndex +
          ' FROM ' + C_Tb_TAutoInc +
          ' WHERE ' + C_Tf_TAutoInc_TbName + ' = ''' + LowerCase(sTbName) + '''';
        Open;
        Result := Fields[0].asInteger - 1;
        Close;
        if (bTransact) then FDatabase.Commit;
      finally
        Free;
      end;
    except
      on E:Exception do begin
        if (bTransact) then FDatabase.Rollback;
        Result := -1;
      end;
    end;
  end;

var
  sName : string;
begin
  if (FOpened) then begin
    sName := LowerCase(sTbName);

    if (FTable.OpenExclusive) then
    try
      // Falls Index vorhanden: zurückgeben
      if (FTable.FindKey([sName])) then begin
        if (FDatabase.IsSQLBased) then begin // 02.06.2006
          FTable.Close;
          Result := GetNextSqlIndex;
          Exit;
        end
        else begin
          FTable.Edit;
          Result := FTable.FieldByName(C_Tf_TAutoInc_AktIndex).asInteger;
        end;
      end

      // Falls Index nicht vorhanden: '1' zurückgeben und neu anlegen
      else begin
        FTable.Append;
        Result := 1;
        FTable.FieldByName(C_Tf_TAutoInc_TbName).asString := sName;
      end;

      // Inkrementieren und speichern
      FTable.FieldByName(C_Tf_TAutoInc_AktIndex).asInteger := Result + 1;
      FTable.Post;

    finally
      FTable.Close;
    end
    else Result := -1;
  end
  else Result := -1;
end;

{ Setzt nächsten Index einer Tabelle          }
{ Parameter: Name der betreffenden Tabelle,   }
{   nächster AutoInc-Index für Tabelle        }
{---------------------------------------------}
procedure TDbAutoInc.SetNextIndex(const sTbName: string; iIndex: integer);
{---------------------------------------------}
var
  sName : string;
begin
  if (FOpened) then begin
    sName := LowerCase(sTbName);

    if (FTable.OpenExclusive) then
    try
      // Falls Index vorhanden: zurückgeben
      if (FTable.FindKey([sName])) then begin
        FTable.Edit;
      end

      // Falls Index nicht vorhanden: neu anlegen
      else begin
        FTable.Append;
        FTable.FieldByName(C_Tf_TAutoInc_TbName).asString := sName;
      end;

      // Eintragen und speichern
      FTable.FieldByName(C_Tf_TAutoInc_AktIndex).asInteger := iIndex;
      FTable.Post;
    finally
      FTable.Close;
    end;
  end
end;

end.
