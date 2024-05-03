{******************************************************************************}
{* Unit: Tabellenzugriffe auf WZUSTAND.DB (Abruf-Zustände)                    *}
{* 07.01.1999 WW                                                              *}
{* 11.01.1999 GD; Proceduren zum Daten holen                                  *}
{* 29.04.1999 GD; function zum Holen eines Com-Zustandes                      *}
{* 06.02.2001 WW; Tabelle zum Schreiben und Löschen exklusiv öffnen wegen     *}
{*                regelmäßig aufgetretenem beschädigten Header, wenn mehr als *}
{*                10 COMs gleichzeitig abrufen                                *}
{* 22.02.2001 GD; Queries vor Freigeben schließem                             *}
{*                                                                            *}
{******************************************************************************}
Unit ZustndDb;

INTERFACE

Uses
  SysUtils, DBTables, DB, WSysCon, Db_Attn, dialogs, WTables;
                                          
Const

  CDBWZustand = 'WZustand.db';

  { Tabelle 'WZustand.db' }

  C_WZustand_ComNr       = 'ComNr';
  C_WZustand_GeraeteArt  = 'GeraeteArt';
  C_WZustand_GeraeteId   = 'GeraeteId';
  C_WZustand_Zustand     = 'Zustand';
  C_WZustand_DatumZeit   = 'DatumZeit';

type

  TZustandDB = class (TObject)
  private
    ZustandTable: TTableExt;
    FilePath: TFileName;
    procedure CreateZustandDB;
  public
    constructor Create (aFilePath: TFileName);
    destructor Destroy; override;
    procedure Append (ComNr: integer; GeraeteArt: string; GeraeteId: integer; Zustand: string);
    procedure GetMRGZustand(aQuery: TQueryExt; GeraeteArt: string; GeraeteId: integer);
    function GetComZustand(aQuery: TQueryExt; aComPort: integer): boolean;
    procedure DeleteAbruf (ComNr: integer);
  end;

IMPLEMENTATION

{--------------------------------------------------}
constructor TZustandDB.Create (aFilePath: TFileName);
{--------------------------------------------------}
begin
  inherited Create;
  FilePath:= aFilePath;
  ZustandTable:=TTableExt.Create (nil);
  ZustandTable.DatabaseName:=FilePath;
  ZustandTable.TableName:=CDBWZustand;
  if not FileExists(FilePath + CDBWZustand) then
    CreateZustandDB;                            { Tabelle automatisch anlegen }
end;

{----------------------------}
Destructor TZustandDB.Destroy;
{----------------------------}
begin
  ZustandTable.Free;
  inherited Destroy;
end;

{-----------------------------------}
procedure TZustandDB.CreateZustandDB;
{-----------------------------------}
{ Zustand-Tabelle anlegen }
begin
  with ZustandTable.FieldDefs do begin
    Clear;
    Add(C_WZustand_ComNr, ftSmallInt, 0, false);
    Add(C_WZustand_GeraeteArt, ftString, 10, false);
    Add(C_WZustand_GeraeteId, ftInteger, 0, false);
    Add(C_WZustand_Zustand, ftString, 100, false);
    Add(C_WZustand_DatumZeit, ftDateTime, 0, false);
  end;
  ZustandTable.IndexDefs.Clear;                               { keine Indizes }
  ZustandTable.CreateTable;
end;

{----------------------------------------------------------------------------------}
procedure TZustandDB.Append (ComNr: integer; GeraeteArt: string; GeraeteId: integer;
                             Zustand: string);
{----------------------------------------------------------------------------------}
{ Zustand eines Geräts am Ende der Zustand-Tabelle einfügen;
  Übergabe: ComNr
            GeraeteArt
            GeraeteId
            Zustand }
begin
  if ZustandTable.OpenExclusive then begin    { ab 06.02.2001 exklusiv öffnen }
    try
      if GeraeteId >= 0 then
        ZustandTable.AppendRecord ([ComNr, GeraeteArt, GeraeteId, Zustand, Now])
      else      { allg., keinem speziellen Gerät zugeordneter Zustand: Eintrag ohne GeraeteId }
        ZustandTable.AppendRecord ([ComNr, GeraeteArt, nil, Zustand, Now]);
    finally
      ZustandTable.Close;
    end;
    { Trigger-Datei schreiben }
    WriteNewTime(FilePath + CDBWZustand);
  end;
end;

{------------------------------------------------------------------------------------}
procedure TZustandDB.GetMRGZustand(aQuery: TQueryExt; GeraeteArt: string; GeraeteId: integer);
{------------------------------------------------------------------------------------}
{ Zustand eines Geräts aus der Zustand-Tabelle holen;
  Übergabe: GeraeteArt
            GeraeteId
            Ergebnis-Query }
var
  ComPort: integer;
  q: TQueryExt;
begin
  { 1. Durchlauf sucht den Comport }
  q:= TQueryExt.create(nil);
  try
    q.close;
    q.DataBaseName:= ZustandTable.DatabaseName;
    q.sql.clear;
    q.sql.add('SELECT ' + C_WZustand_ComNr);
    q.sql.add('FROM "' + CDBWZustand + '"');
    q.sql.add('WHERE ' + C_WZustand_GeraeteArt + '= :GArt');
    q.sql.add('AND ' + C_WZustand_GeraeteId + '= :GId');
    q.ParambyName('GArt').asString:= GeraeteArt;
    q.ParambyName('GId').asInteger:= GeraeteId;
    q.open;
    if q.RecordCount > 0 then Comport:= q.FieldByName(C_WZustand_ComNr).asInteger
    else Comport:=0;
  except
    q.Free;
    if IsDebugFlag then showMessage('Zustand - GetMrgZustand');
    exit;
  end;
  if (q.Active) then q.Close;  // 22.02.2001
  q.Free;
  { 2. Durchlauf gibt Query zurück }
  try
    aQuery.close;
    aQuery.DataBaseName:= ZustandTable.DatabaseName;
    aQuery.sql.clear;
    aQuery.sql.add('SELECT ' + C_WZustand_Zustand + ',' + C_WZustand_DatumZeit + ',');
    aQuery.sql.add(C_WZustand_ComNr + ',' + C_WZustand_GeraeteArt + ',' + C_WZustand_GeraeteId);
    aQuery.sql.add('FROM "' + CDBWZustand + '"');
    aQuery.sql.add('WHERE ' + C_WZustand_ComNr + '= :CPort');
    aQuery.sql.add('ORDER BY ' + C_WZustand_DatumZeit);
    aQuery.ParambyName('CPort').asInteger:= ComPort;
    aQuery.open;
  except
    if IsDebugFlag then showMessage('Zustand - GetMrgZustand2');
    aQuery.Close;
    { keine Ahnung ... }
  end;
end;

{ Zustand einer Schnittstelle aus der Zustand-Tabelle holen                   }
{-----------------------------------------------------------------------------}
function TZustandDB.GetComZustand(aQuery: TQueryExt; aComPort: integer): boolean;
{-----------------------------------------------------------------------------}
begin
  aQuery.close;
  aQuery.DataBaseName:= ZustandTable.DatabaseName;
  aQuery.sql.clear;
  aQuery.sql.add('SELECT ' + C_WZustand_Zustand + ',' + C_WZustand_DatumZeit + ',');
  aQuery.sql.add(C_WZustand_ComNr + ',' + C_WZustand_GeraeteArt + ',' + C_WZustand_GeraeteId);
  aQuery.sql.add('FROM "' + CDBWZustand + '"');
  aQuery.sql.add('WHERE ' + C_WZustand_ComNr + '= :CPort');
  aQuery.sql.add('ORDER BY ' + C_WZustand_DatumZeit);
  aQuery.ParambyName('CPort').asInteger:= aComPort;
  result:= aQuery.open;
end;

{------------------------------------------------}
procedure TZustandDB.DeleteAbruf (ComNr: integer);
{------------------------------------------------}
{ alle Einträge eines Abruf aus Zustandstabelle löschen
  Übergabe: ComNr }
begin
  if ZustandTable.OpenExclusive then begin    { ab 06.02.2001 exklusiv öffnen }
    try
      { Filter-String zusammensetzen: }
      ZustandTable.Filter:=C_WZustand_ComNr + ' = ' + IntToStr(ComNr);
      ZustandTable.Filtered:=true;                          { Filter aktivieren }
      try
        while not ZustandTable.Eof do
          ZustandTable.Delete;
      finally
        ZustandTable.Filtered:=false;                     { Filter deaktivieren }
      end;
    finally
      ZustandTable.Close;
    end;
    { Trigger-Datei schreiben }
    WriteNewTime(FilePath + CDBWZustand);
  end;  
end;

End.
