{------------------------------------------------------------------------------}
{ Unit mit Datenbankfunktionen                                                 }
{                                                                              }
{ 03.12.2003  GD  Neu                                                          }
{ 02.03.2005  GD  Erweitert um WGetTableNames                                  }
{ 21.10.2005  GD  Erweitert um "MyRebuildTable"                                }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003, 2005                                    }
{------------------------------------------------------------------------------}
unit DB_Utils;

interface

uses
  SysUtils, Classes, DbTables, DbClient, Provider;

function DeleteTables(pDatabase: TDatabase; sMask: string): integer;
function GetTableList(sDatabaseName: string; sMask: string = ''): TStringList;
procedure WGetTableNames(pDatabase: TDatabase; sWildCard: string;
  bExtensions, bSystemTables: boolean; pSl: TStrings;
  bReloadDatabase: boolean = False);
function TableExists (aDatabase: TDatabase; aTableName: string): boolean;
procedure MyRebuildTable(pTable: TTable);

implementation


{------------------------------------------------------}
procedure WGetTableNames(pDatabase: TDatabase; sWildCard: string;
  bExtensions, bSystemTables: boolean; pSl: TStrings;
  bReloadDatabase: boolean = False);
{------------------------------------------------------}
var
  pDb : TDatabase;
begin
  if (not bReloadDatabase) and ((not pDatabase.IsSQLBased) or (sWildCard = ''))
  then begin
    pDatabase.Session.GetTableNames(
      pDatabase.DatabaseName, sWildCard, False, False, pSl);
  end
  else begin
    pDb := TDatabase.Create(nil);
    try
      pDb.DatabaseName := 'db' + IntToStr(Integer(pDb));
      pDb.SessionName := pDatabase.SessionName;
      if (pDatabase.AliasName <> '')
      then pDb.AliasName := pDatabase.AliasName
      else pDb.DriverName := pDatabase.DriverName;
      pDb.Params.Assign(pDatabase.Params);
      pDb.LoginPrompt := pDatabase.LoginPrompt;
      pDb.OnLogin := pDatabase.OnLogin;
      pDb.Connected := True;
      try
        pDb.Session.GetTableNames(pDb.DatabaseName, sWildCard, False, False, pSl);
      finally
        pDb.Connected := False;;
      end;
    finally
      pDb.Free;
    end;
  end;
end;

{------------------------------------------------------}
function GetTableList(sDatabaseName: string; sMask: string = ''): TStringList;
{------------------------------------------------------}
const
  C_TableList    : TStrings = nil;
  C_DatabaseName : string = '';
  C_Mask         : string = '';
var
  i : integer;
begin
  if (sDatabaseName = '') then begin
    FreeAndNil(C_TableList);
    Result := nil;
    Exit;
  end
  else if (not Assigned(C_TableList)) then C_TableList := TStringList.Create;

  Result := TStringList.Create;

  if (sDatabaseName = C_DatabaseName) and (sMask = C_Mask) then begin
    Result.Assign(C_TableList);
  end
  else begin
    Session.GetTableNames(sDatabaseName, sMask, False, False, Result);
    for i := 0 to Result.Count-1 do  // ggf. User entfernen
      if (ExtractFileExt(Result[i]) <> '')
      then Result[i] := Copy(ExtractFileExt(Result[i]), 2, 20)
      else Break;
    C_TableList.Assign(Result);
    C_DatabaseName := sDatabaseName;
    C_Mask := sMask;
  end;
end;

{ Löscht mehrere Tabellen über Liste                 }
{ Parameter: Datenbankname, Maske für Tabellenname,  }
{            Session (nil=Default)                   }
{ Rückgabe: Anzahl der gelöschten Tabellen, -1=Error }
{----------------------------------------------------}
function DeleteTables(pDatabase: TDatabase; sMask: string): integer;
{----------------------------------------------------}
var
  pSl : TStringList;
  i   : integer;
begin
  try
    Result := 0;
    pSl := TStringList.Create;
    try
      pDatabase.Session.GetTableNames(pDatabase.DatabaseName, sMask, False, False, pSl);
      if (pSl.Count > 0) then
        with TQuery.Create(nil) do
        try
          SessionName := pDatabase.SessionName;
          DatabaseName := pDatabase.DatabaseName;

          for i := 0 to pSl.Count-1 do begin
            Sql.Text := 'DROP TABLE ' + pSl[i];
            ExecSQL;
            Inc(Result);
          end;
        finally
          Free;
        end;
    finally
      pSl.Free;
    end;
  except
    Result := -1;
  end;
end;

{-----------------------------------------------------------------------}
function TableExists (aDatabase: TDatabase; aTableName: string): boolean;
{-----------------------------------------------------------------------}
{ prüft Vorhandensein einer Tabelle;
  Übergabe: Database und Tabellenname
  Ergebnis: true, wenn Tabelle vorhanden ist }
begin
  with TTable.Create (nil) do
  try
    SessionName:=aDatabase.SessionName;
    DatabaseName:=aDatabase.DatabaseName;
    TableName:=aTableName;
    Result:=Exists;
  finally
    Free;
  end;
end;

{ Tabelle neu aufbauen                        }
{ Parameter: Tabelle                          }
{---------------------------------------------}
procedure MyRebuildTable(pTable: TTable);   // 21.10.2005
{---------------------------------------------}
var
  i      : integer;
  pCDS   : TClientDataSet;
  pDSP   : TDataSetProvider;
begin
  if (not Assigned(pTable)) then Exit;
  pCDS := nil;
  pDSP := nil;

  try

    if (Assigned(pTable)) and (pTable.Exists) then begin
      if (pTable.Active) then pTable.Close;

      pCDS := TClientDataSet.Create(nil);
      pDSP := TDataSetProvider.Create(nil);

      // Tabellendaten zwischenspeichern
      pDSP.DataSet := pTable;
      pCDS.SetProvider(pDSP);
      pCDS.Open;

      // Tabelle löschen und neu erzeugen
      pTable.FieldDefs.Update;
      pTable.IndexDefs.Update;
      pTable.DeleteTable;
      pTable.CreateTable;

      // Tabelle füllen
      if (Assigned(pCDS)) then begin
        pTable.Open;
        pCDS.First;
        while (not pCDS.Eof) do begin
          pTable.Append;
          for i := 0 to pTable.FieldDefs.Count-1 do begin
            if (pCDS.FieldDefs.IndexOf(pTable.Fields[i].FieldName) >= 0) then
              pTable.Fields[i].Value := pCDS.FieldByName(pTable.Fields[i].FieldName).Value;
          end;
          pTable.Post;
          pCDS.Next;
        end;
      end;
    end;
  finally
    if (Assigned(pDSP)) then pDSP.Free;
    if (Assigned(pCDS)) then pCDS.Free;
  end;
end;

initialization

finalization
  GetTableList('');

end.
