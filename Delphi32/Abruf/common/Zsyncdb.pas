{**************************************************************************************}
{* Unit: Tabellenzugriffe auf WJZSync.DB (Zeitsynchronisation der Ger�te)             *}
{* 19.01.1999 WW                                                                      *}
{**************************************************************************************}
Unit ZSyncDb;

INTERFACE

Uses
  SysUtils, DBTables, DB, WTables;

Const

  CDBWJZSync = 'WJZSync.db';             { enth�lt Ger�t/PC-Zeit }

  { Tabelle 'WJZSync.db' }

  C_WJZSync_JournalId = 'JournalId';   { JournalId aus WJournal.db }
  C_WJZSync_DZGeraet  = 'DZGeraet';    { Ger�t-Zeit }
  C_WJZSync_DZPC      = 'DZPC';        { PC-Zeit }


type

  { Objekt f�r Zugriff auf WJZSync.db }

  TWJZSyncDB = class (TObject)
  private
    procedure CreateZSyncDB;
  public
    ZSyncTable: TTableExt;
    constructor Create (FilePath: TFileName);
    destructor Destroy; override;
    procedure WriteNewZSync (JournalId: integer);
    procedure UpdateZSync (JournalId: integer; GeraetZeit: TDateTime; PCZeit: TDateTime);
    function ReadGeraet_PCZeit (JournalId: integer; var GeraetZeit, PCZeit: TDateTime): boolean;
    function DeleteRecord(JournalId: integer): boolean;
    function DeleteRecordsBefore(JournalId: integer): boolean;
    function DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
  end;

IMPLEMENTATION

uses
  JournlDb;

{ TWJZSyncDB }

{--------------------------------------------------}
constructor TWJZSyncDB.Create (FilePath: TFileName);
{--------------------------------------------------}
begin
  inherited Create;
  ZSyncTable:=TTableExt.Create (nil);
  ZSyncTable.DatabaseName:=FilePath;
  ZSyncTable.TableName:=CDBWJZSync;
  if not FileExists(FilePath + CDBWJZSync) then
    CreateZSyncDB;                              { Tabelle automatisch anlegen }
end;

{----------------------------}
Destructor TWJZSyncDB.Destroy;
{----------------------------}
begin
  ZSyncTable.Free;
  inherited Destroy;
end;

{---------------------------------}
procedure TWJZSyncDB.CreateZSyncDB;
{---------------------------------}
{ ZeitSync-Tabelle anlegen }
begin
  with ZSyncTable.FieldDefs do begin
    Clear;
    Add(C_WJZSync_JournalId, ftInteger, 0, false);
    Add(C_WJZSync_DZGeraet, ftDateTime, 0, false);
    Add(C_WJZSync_DZPC, ftDateTime, 0, false);
  end;
  with ZSyncTable.IndexDefs do begin
    Clear;
    Add('PJournalId', C_WJZSync_JournalId, [ixPrimary,ixUnique]);            { Prim�rindex }
  end;
  ZSyncTable.CreateTable;
end;

{------------------------------------------------------}
procedure TWJZSyncDB.WriteNewZSync (JournalId: integer);
{------------------------------------------------------}
{ neuen Zeitsynchronisationseintrag in Tabelle einf�gen
  �bergabe: JournalId }
begin
  if JournalId > 0 then begin
    if ZSyncTable.OpenShared then begin
      try
        ZSyncTable.AppendRecord ([JournalId]);
      finally
        ZSyncTable.Close;
      end;
    end;
  end;
end;

{--------------------------------------------------------------------------}
procedure TWJZSyncDB.UpdateZSync (JournalId: integer;
                                  GeraetZeit: TDateTime; PCZeit: TDateTime);
{--------------------------------------------------------------------------}
{ Ger�te/PC-Zeit (vor der Synchronisation !) f�r JournalId in vorhandenen Tabelleneintrag
  schreiben;
  �bergabe: JournalId
            GeraetZeit
            PCZeit }
begin
  if JournalId > 0 then begin
    if ZSyncTable.OpenShared then begin
      try
        if ZSyncTable.FindKey ([JournalId]) then begin
          ZSyncTable.Edit;
          ZSyncTable.FieldByName (C_WJZSync_DZGeraet).AsDateTime:=GeraetZeit;
          ZSyncTable.FieldByName (C_WJZSync_DZPC).AsDateTime:=PCZeit;
          ZSyncTable.Post;
        end;
      finally
        ZSyncTable.Close;
      end;
    end;
  end;
end;

{ Gibt PC- und Mrg-Zeit VOR dem Synchronisieren zurueck }
{ Parameter: JournalId - kennzeichnet Eintrag,          }
{            �bergabeparameter f�r Zeiten               }
{ R�ckgabe: False -> sollte nicht synchronisieren       }
{-------------------------------------------------------}
function TWJZSyncDB.ReadGeraet_PCZeit (JournalId: integer; var GeraetZeit, PCZeit: TDateTime): boolean;
{-------------------------------------------------------}
begin
  result:=false;
  if ZSyncTable.OpenShared then begin
    try
      if ZSyncTable.FindKey([JournalId]) then begin
        GeraetZeit:= ZSyncTable.FieldByName(C_WJZSync_DZGeraet).asDateTime;
        PCZeit:= ZSyncTable.FieldByName(C_WJZSync_DZPC).asDateTime;
        result:= true;
      end;
    finally
      ZSyncTable.Close;
    end;
  end;
end;

{ L�scht einen Eintrag aus der ZeitSync-Tabelle            }
{ Parameter:  JournalId des Records                        }
{ R�ckgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TWJZSyncDB.DeleteRecord(JournalId: integer): boolean;
{----------------------------------------------------------}
begin
  Result:=false;
  if ZSyncTable.OpenShared then begin
    try
      if ZSyncTable.FindKey([JournalId]) then
        ZSyncTable.Delete;
      Result:=true;
    except
      ZSyncTable.Close;
      exit;
    end;
    ZSyncTable.Close;
  end;
end;

{ L�scht Eintr�ge mit kleinerer JournalId als                       }
{ die �bergebene aus der ZeitSync-Tabelle                           }
{ Parameter:  JournalId                                             }
{ R�ckgabe: True - alles i.O.                                       }
{-------------------------------------------------------------------}
function TWJZSyncDB.DeleteRecordsBefore(JournalId: integer): boolean;
{-------------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= ZSyncTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBWJZSync + '"');
      Add('WHERE ' +  C_WJZSync_JournalId + ' < ' + IntToStr(JournalId));
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

{ L�scht alle �bergebenen Eintr�ge der ZeitSync-Tabelle    }
{ Parameter:  Query mit zu l�schenden Eintr�gen            }
{             Query ist offen                              }
{ R�ckgabe: True - alles i.O.                              }
{----------------------------------------------------------}
function TWJZSyncDB.DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
{----------------------------------------------------------}
var
  JournalId: integer;
begin
  Result:=true;
  if aQuery.RecordCount > 0 then begin
    Result:=false;
    if ZSyncTable.OpenShared then begin
      try
        aQuery.first;
        while not aQuery.Eof do begin
          JournalId:=aQuery.FieldByName(C_WJournal_JournalId).asInteger;
          if ZSyncTable.FindKey([JournalId]) then
            ZSyncTable.Delete;
          aQuery.Next;
        end;
        Result:=true;
      except
        ZSyncTable.Close;
        exit;
      end;
      ZSyncTable.Close;
    end;
  end;
end;

end.
