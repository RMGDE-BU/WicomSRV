{********************************************************************************************}
{* Unit: Zugriffe auf Journal-Detailtabelle mit Aufmerksamkeits-Telegrammen                 *}
{* 15.05.2001 WW                                                                            *}
{********************************************************************************************}
Unit TelegrDb;

INTERFACE

Uses
  SysUtils, DBTables, DB, Dialogs, WTables, WSysCon;

Const

  { Tabelle 'DJTelegr.db' }

  CDBDJTelegr = 'DJTelegr.db';      { enthält DSfG-Aufmerksamkeits-Telegramme }

  C_DJTelegr_JournalId      = 'JournalId';      { JournalId aus WJournal.db }
  C_DJTelegr_Busadresse     = 'Busadresse';     { Telegramm-Absender }
  C_DJTelegr_Nachrichtentyp = 'Nachrichtentyp'; { NTY }
  C_DJTelegr_DatumZeit      = 'DatumZeit';      { Generierungsdatum/-zeit des Telegramms }
  C_DJTelegr_Zeitzone       = 'Zeitzone';       { Generierungszeitzone des Telegramms }

type

  { Objekt für Zugriff auf DJTelegr.db }

  TDJTelegrDB = class (TObject)
  private
    procedure CreateTelegrDB;
  public
    TelegrTable: TTableExt;
    constructor Create (FilePath: TFileName);
    destructor Destroy; override;
    function OpenTelegrTable: boolean;
    procedure CloseTelegrTable;
    procedure WriteTelegramm (JournalId: integer; Busadresse: string; Nachrichtentyp: string;
                              DatumZeit: TDateTime; Zeitzone: string);
    function GetTelegramme (var aQuery: TQuery;
                            JournalId: integer; StationId: integer): boolean;
    function DeleteRecords(JournalId: integer): boolean;
    function DeleteRecordsBefore(JournalId: integer): boolean;
    function DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
  end;


IMPLEMENTATION

uses
  JournlDb;

{ TDJTelegrDB }

{---------------------------------------------------}
constructor TDJTelegrDB.Create (FilePath: TFileName);
{---------------------------------------------------}
begin
  inherited Create;
  TelegrTable:=TTableExt.Create (nil);
  TelegrTable.DatabaseName:=FilePath;
  TelegrTable.TableName:=CDBDJTelegr;
  if not FileExists(FilePath + CDBDJTelegr) then
    CreateTelegrDB;                             { Tabelle automatisch anlegen }
end;

{-----------------------------}
Destructor TDJTelegrDB.Destroy;
{-----------------------------}
begin
  TelegrTable.Free;
  inherited Destroy;
end;

{-----------------------------------}
procedure TDJTelegrDB.CreateTelegrDB;
{-----------------------------------}
{ Telegramm-Tabelle anlegen }
begin
  with TelegrTable.FieldDefs do begin
    Clear;
    Add(C_DJTelegr_JournalId, ftInteger, 0, false);
    Add(C_DJTelegr_Busadresse, ftString, 1, false);
    Add(C_DJTelegr_Nachrichtentyp, ftString, 1, false);
    Add(C_DJTelegr_DatumZeit, ftDateTime, 0, false);
    Add(C_DJTelegr_Zeitzone, ftString, 1, false);
  end;
  TelegrTable.IndexDefs.Clear;
  TelegrTable.CreateTable;
end;

{--------------------------------------------}
function TDJTelegrDB.OpenTelegrTable: boolean;
{--------------------------------------------}
{ Telegramm-Tabelle öffnen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  Result:=TelegrTable.OpenShared;
end;

{-------------------------------------}
procedure TDJTelegrDB.CloseTelegrTable;
{-------------------------------------}
{ Telegramm-Tabelle schließen }
begin
  TelegrTable.Close;
end;

{---------------------------------------------------------------------------------------------------}
procedure TDJTelegrDB.WriteTelegramm (JournalId: integer; Busadresse: string; Nachrichtentyp: string;
                                      DatumZeit: TDateTime; Zeitzone: string);
{---------------------------------------------------------------------------------------------------}
{ Datensatz in Telegramm-Tabelle schreiben; Öffnen und schließen der Tabelle muß
  außerhalb der Prozedur vorgenommen werden !
  Übergabe: JournalId
            Busadresse
            Nachrichtentyp
            Datum/Zeit
  Ergebnis: true, wenn Datensatz geschrieben wurde }
begin
  if TelegrTable.Active then begin
    if DatumZeit > -1 then
      TelegrTable.AppendRecord ([JournalId, Busadresse, Nachrichtentyp, DatumZeit, Zeitzone])
    else
      TelegrTable.AppendRecord ([JournalId, Busadresse, Nachrichtentyp, nil, Zeitzone]);
  end;
end;

{-----------------------------------------------------------------------------------}
function TDJTelegrDB.GetTelegramme (var aQuery: TQuery;
                                    JournalId: integer; StationId: integer): boolean;
{-----------------------------------------------------------------------------------}
{ Gibt Telegrammdaten zu einer JournalId, StationId in einem Query zurück;
 Übergabe: Query
           JournalId
           StationId
 Ergebnis: true, wenn aQuery Einträge enthält }
begin
  try
    with aQuery do begin
      Close;
      DataBaseName:=TelegrTable.DatabaseName;
      Sql.Clear;
      Sql.Add('SELECT A.' + C_DJTelegr_Busadresse + ',');
      Sql.Add('A.' + C_DJTelegr_Nachrichtentyp + ',');
      Sql.Add('A.' + C_DJTelegr_DatumZeit + ',');
      Sql.Add('A.' + C_DJTelegr_Zeitzone + ',');
      Sql.Add('B.' + C_Tf_DNTY_Bezeichnung + ',');
      Sql.Add('C.' + C_DTF_Instanz_Instanzname);
      Sql.Add('FROM "' + CDBDJTelegr + '" A');
      Sql.Add('LEFT JOIN "' + C_Tb_DNTY + '" B');
      Sql.Add('ON A.' + C_DJTelegr_Nachrichtentyp + '= B.' + C_Tf_DNTY_Nachrichtentyp);
      Sql.Add('LEFT JOIN "' + C_DTB_Instanz + '" C');
      Sql.Add('ON ((A.' + C_DJTelegr_Busadresse + '= C.' + C_DTF_Instanz_Busadresse + ') AND');
      Sql.Add('    (C.' + C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId) + '))');
      Sql.Add('WHERE A.' + C_DJTelegr_JournalId + ' = ' + IntToStr(JournalId));
      aQuery.Open;
      result:=RecordCount > 0;
    end;
  except
    if IsDebugFlag then showMessage('TelegrDB - GetTelegramme');
    result:= false;
  end;
end;

{ Löscht Einträge zu einer Id aus der DSfG-Telegramm-Journaltabelle }
{ Parameter:  JournalId der Records                                 }
{ Rückgabe: True - alles i.O.                                       }
{--------------------------------------------------------------}
function TDJTelegrDB.DeleteRecords(JournalId: integer): boolean;
{--------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= TelegrTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBDJTelegr + '"');
      Add('WHERE ' +  C_DJTelegr_JournalId + ' = ' + IntToStr(JournalId));
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

{ Löscht Einträge mit kleinerer JournalId als                         }
{ die übergebene aus der DSfG-Telegramm-Journaltabelle                }
{ Parameter:  JournalId                                               }
{ Rückgabe: True - alles i.O.                                         }
{---------------------------------------------------------------------}
function TDJTelegrDB.DeleteRecordsBefore(JournalId: integer): boolean;
{---------------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= TelegrTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBDJTelegr + '"');
      Add('WHERE ' +  C_DJTelegr_JournalId + ' < ' + IntToStr(JournalId));
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

{ Löscht alle übergebenen Einträge der DSfG-Telegramm-JournalTabelle }
{ Parameter:  Query mit zu löschenden Einträgen                      }
{             Query ist offen                                        }
{ Rückgabe: True - alles i.O.                                        }
{----------------------------------------------------------------------}
function TDJTelegrDB.DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
{----------------------------------------------------------------------}
begin
  try
    if aQuery.RecordCount > 0 then begin
      aQuery.first;
      while not aQuery.Eof do begin
        if aQuery.FieldByName(C_WJournal_GeraeteArt).asString = C_GerArtDSfG then
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
