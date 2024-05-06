unit DBUtils;

interface

uses
  Sysutils, DB, DBTables;

const
  CSQLSelect        = 'SELECT';
  CSQLFrom          = 'FROM';
  CSQLWhere         = 'WHERE';
  CSQLAnd           = 'AND';
  CSQLOrderBy       = 'ORDER BY';
  CSQLLeftOuterJoin = 'LEFT OUTER JOIN';
  CSQLOn            = 'ON';

function GetPathNameOfDatabase (const DatabaseName: string): TFileName;
function GetSQLTableName (const DatabaseName, TableName: string): string;

implementation

{------------------------------------------------}
function TruncAllSpaces (const S: string): string;
{------------------------------------------------}
var
  i: Byte;
begin
  Result := '';
  for i := 1 to Length (S) do
  begin
    if S [i] <> ' ' then
      Result := Result + S [i];
  end;
end;

{--------------------------------------------------------------------}
function GetPathNameOfDatabase (const DatabaseName: string): TFileName;
{--------------------------------------------------------------------}
var
  Database: TDataBase;
  i: Integer;
  S: String;
begin
  Result := ':' + DatabaseName + ':';
  DataBase := Session.FindDatabase (DatabaseName);
  if Assigned (DataBase) then
  begin
    for i := 0 to Database.Params.Count - 1 do
    begin
      S := TruncAllSpaces (AnsiUpperCase (Database.Params [i]));
      if (Length (S) > 6) and (Pos ('PATH=', S) = 1) then
      begin
        Result := copy (S, 6, Length (S) - 6) + '\';
        exit;
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------}
function GetSQLTableName (const DatabaseName, TableName: string): string;
{-----------------------------------------------------------------------}
begin
  Result := '"' + GetPathNameOfDatabase (DatabaseName) + TableName + '"';
end;

end.
