{**************************************************************************************}
{* Unit: Zugriffe auf Tabelle mit Rohdateinamen abgerufener Daten                     *}
{* 20.12.1999 WW                                                                      *}
{**************************************************************************************}
Unit RFilesDb;

INTERFACE

Uses
  SysUtils, DBTables, DB, WTables;

Const

  { Tabelle 'WJRFiles.db' }

  CDBWJRFiles = 'WJRFiles.db';

  C_WJRFiles_JournalId    = 'JournalId';      { JournalId aus WJournal.db }
  C_WJRFiles_Rohdateiname = 'Rohdateiname';   { vollst�ndiger Pfad }


type

  { Objekt f�r Zugriff auf WJRFiles.db }

  TWJRFilesDB = class (TObject)
  private
    procedure CreateRFilesDB;
  public
    RFilesTable: TTableExt;
    constructor Create (FilePath: TFileName);
    destructor Destroy; override;
    procedure WriteRohdateiname (JournalId: integer; FileName: TFileName);
    function DeleteRecords(JournalId: integer): boolean;
    function DeleteRecordsBefore(JournalId: integer): boolean;
    function DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
  end;

IMPLEMENTATION

uses
  JournlDb;

{ TWJRFilesDB }

{---------------------------------------------------}
constructor TWJRFilesDB.Create (FilePath: TFileName);
{---------------------------------------------------}
begin
  inherited Create;
  RFilesTable:=TTableExt.Create (nil);
  RFilesTable.DatabaseName:=FilePath;
  RFilesTable.TableName:=CDBWJRFiles;
  if not RFilesTable.Exists then
    CreateRFilesDB;                             { Tabelle automatisch anlegen }
end;

{-----------------------------}
Destructor TWJRFilesDB.Destroy;
{-----------------------------}
begin
  RFilesTable.Free;
  inherited Destroy;
end;

{-----------------------------------}
procedure TWJRFilesDB.CreateRFilesDB;
{-----------------------------------}
{ Rohdateinamen-Tabelle anlegen }
begin
  with RFilesTable.FieldDefs do begin
    Clear;
    Add(C_WJRFiles_JournalId, ftInteger, 0, false);
    Add(C_WJRFiles_Rohdateiname, ftString, 255, false);
  end;
  RFilesTable.IndexDefs.Clear;                             { kein Prim�rindex }
  RFilesTable.CreateTable;
end;

{--------------------------------------------------------------------------------}
procedure TWJRFilesDB.WriteRohdateiname (JournalId: integer; FileName: TFileName);
{--------------------------------------------------------------------------------}
{ Rohdateiname in Tabelle schreiben;
  �bergabe: JournalId
            Rohdateiname }
begin
  if JournalId > 0 then begin
    if RFilesTable.OpenShared then begin
      try
        RFilesTable.AppendRecord ([JournalId, FileName]);
      finally
        RFilesTable.Close;
      end;
    end;
  end;
end;

{ L�scht Eintr�ge zu einer Id aus der Rohdateinamen-Tabelle   }
{ Parameter:  JournalId der Records                           }
{ R�ckgabe: True - alles i.O.                                 }
{-------------------------------------------------------------}
function TWJRFilesDB.DeleteRecords(JournalId: integer): boolean;
{-------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= RFilesTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBWJRFiles + '"');
      Add('WHERE ' +  C_WJRFiles_JournalId + ' = ' + IntToStr(JournalId));
    end;
    q.ExecSql;
    result:= true;
  except
    q.free;
    result:= false;
    exit;
  end;
  q.free;
end;

{ L�scht Eintr�ge mit kleinerer JournalId als                        }
{ die �bergebene aus der DSfG-Zeitbereich-Tabelle                    }
{ Parameter:  JournalId                                              }
{ R�ckgabe: True - alles i.O.                                        }
{--------------------------------------------------------------------}
function TWJRFilesDB.DeleteRecordsBefore(JournalId: integer): boolean;
{--------------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= RFilesTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBWJRFiles + '"');
      Add('WHERE ' +  C_WJRFiles_JournalId + ' < ' + IntToStr(JournalId));
    end;
    q.ExecSql;
    result:= true;
  except
    q.free;
    result:= false;
    exit;
  end;
  q.free;
end;

{ L�scht alle �bergebenen Eintr�ge der Rohdateinamen-Tabelle    }
{ Parameter:  Query mit zu l�schenden Eintr�gen                 }
{             Query ist offen                                   }
{ R�ckgabe: True - alles i.O.                                   }
{---------------------------------------------------------------}
function TWJRFilesDB.DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
{---------------------------------------------------------------}
begin
  try
    if aQuery.RecordCount > 0 then begin
      aQuery.first;
      while not aQuery.Eof do begin
        DeleteRecords (aQuery.FieldByName(C_WJournal_JournalId).asInteger);
        aQuery.Next;
      end;
    end;
    result:= true;
  except
    result:= false;
    exit;
  end;
end;

end.
