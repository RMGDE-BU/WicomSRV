{********************************************************************************************}
{* Unit: Zugriffe auf Tabellen mit Zeitbereichen abgerufener Daten                          *}
{*       MRG: MJZBDaten.DB (Zeitbereiche der Meßwerte)                                      *}
{*       DSfG: DJZBDaten.DB (Zeitbereiche oder Ordnungsnummern der Archiv- u. Logbuchdaten) *}
{* 15.01.1999 WW                                                                            *}
{********************************************************************************************}
Unit ZBDatDb;

INTERFACE

Uses
  SysUtils, Dialogs, DBTables, DB, WSysCon, WTables;

Const

  { Tabelle 'MJZBDat.db' }

  CDBMJZBDaten = 'MJZBDat.db';           { enthält MRG-Meßwertzeitbereiche }

  C_MJZBDaten_JournalId = 'JournalId';   { JournalId aus WJournal.db }
  C_MJZBDaten_SollVon   = 'SollVon';     { Daten abgerufen ab }
  C_MJZBDaten_SollBis   = 'SollBis';     { Daten abgerufen bis }
  C_MJZBDaten_IstVon    = 'IstVon';      { Daten in LGZD-Datei geschrieben ab }
  C_MJZBDaten_IstBis    = 'IstBis';      { Daten in LGZD-Datei geschrieben bis }


  { Tabelle 'DJZBDat.db' }

  CDBDJZBDaten = 'DJZBDat.db';                  { enthält DSfG-Archiv- u. Logbuch-Abrufbereiche }

  C_DJZBDaten_JournalId     = 'JournalId';      { JournalId aus WJournal.db }
  C_DJZBDaten_InstanzId     = 'InstanzId';      { InstanzId aus DSfG-Stammdaten }
  C_DJZBDaten_ArchLogbNr    = 'ArchLogbNr';     { Archivgruppe bzw. Logbuchnr. aus DSfG-Stammdaten }
  C_DJZBDaten_KanalNr       = 'KanalNr';        { Archivkanalnr. aus DSfG-Stammdaten }
  C_DJZBDaten_Fehler        = 'Fehler';         { Detail-Fehlercode }
  C_DJZBDaten_DZ_SollVon    = 'DZ_SollVon';     { Daten abgerufen ab Datum/Zeit }
  C_DJZBDaten_DZ_SollBis    = 'DZ_SollBis';     { Daten abgerufen bis Datum/Zeit }
  C_DJZBDaten_DZ_IstVon     = 'DZ_IstVon';      { Daten in Archiv-/Logbuchtabelle geschrieben ab Datum/Zeit }
  C_DJZBDaten_DZ_IstBis     = 'DZ_IstBis';      { Daten in Archiv-/Logbuchtabelle geschrieben bis Datum/Zeit }
  C_DJZBDaten_OrdNr_SollVon = 'OrdNr_SollVon';  { Daten abgerufen ab Ordnungsnummer }
  C_DJZBDaten_OrdNr_SollBis = 'OrdNr_SollBis';  { Daten abgerufen bis Ordnungsnummer }
  C_DJZBDaten_OrdNr_IstVon  = 'OrdNr_IstVon';   { Daten in Archiv-/Logbuchtabelle geschrieben ab Ordnungsnummer }
  C_DJZBDaten_OrdNr_IstBis  = 'OrdNr_IstBis';   { Daten in Archiv-/Logbuchtabelle geschrieben bis Ordnungsnummer }


type

  TZeitbereich = record
    SollVon: TDateTime;
    SollBis: TDateTime;
    IstVon: TDateTime;
    IstBis: TDateTime;
  end;

  { Objekt für Zugriff auf MJZBDaten.db }

  TMJZBDatenDB = class (TObject)
  private
    procedure CreateZBDatenDB;
  public
    ZBDatenTable: TTableExt;
    constructor Create (FilePath: TFileName);
    destructor Destroy; override;
    procedure WriteZBDatenSoll (JournalId: integer; SollVon: TDateTime; SollBis: TDateTime);
    procedure WriteZBDatenIst (JournalId: integer; IstVon: TDateTime; IstBis: TDateTime;
                               var SollVon: TDateTime; var SollBis: TDateTime);
    function ReadZbDaten(JournalId: integer): TZeitbereich;
    function DeleteRecord(JournalId: integer): boolean;
    function DeleteRecordsBefore(JournalId: integer): boolean;
    function DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
  end;

  { Objekt für Zugriff auf DJZBDaten.db }

  TDJZBDatenDB = class (TObject)
  private
    procedure CreateZBDatenDB;
  public
    ZBDatenTable: TTableExt;
    constructor Create (FilePath: TFileName);
    destructor Destroy; override;
    procedure WriteZBDatenSoll (JournalId: integer; InstanzId: integer; ArchLogbNr: integer; KanalNr: integer;
                                SollVon: TDateTime; SollBis: TDateTime; isOrdNr: boolean);
    procedure WriteZBDatenIst (JournalId: integer; InstanzId: integer; ArchLogbNr: integer; KanalNr: integer;
                               Fehlercode: integer;
                               IstVon_OrdNr: integer; IstBis_OrdNr: integer;
                               IstVon_DZ: TDateTime; IstBis_DZ: TDateTime);
    function DeleteRecords(JournalId: integer): boolean;
    function DeleteRecordsBefore(JournalId: integer): boolean;
    function DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
    function DeleteRecordsByInstanz(InstanzId: integer): boolean;
    function DeleteRecordsByArchiv(InstanzId: integer; ArchivNr: integer): boolean;
    function DeleteRecordsByArchivkanal(InstanzId: integer; ArchivNr: integer; KanalNr: integer): boolean;
    function DeleteRecordsByLogbuch(InstanzId: integer; LogbuchNr: integer): boolean;
    function ReadZbDaten_Archive (var aQuery: TQuery; JournalId: integer): boolean;
    function ReadZbDaten_Logbuecher (var aQuery: TQuery; JournalId: integer): boolean;
    function ReadZbDaten_Datenelemente (var aQuery: TQuery; JournalId: integer): boolean;
  end;

IMPLEMENTATION

uses
  JournlDb;

{ TMJZBDatenDB }

{----------------------------------------------------}
constructor TMJZBDatenDB.Create (FilePath: TFileName);
{----------------------------------------------------}
begin
  inherited Create;
  ZBDatenTable:=TTableExt.Create (nil);
  ZBDatenTable.DatabaseName:=FilePath;
  ZBDatenTable.TableName:=CDBMJZBDaten;
  if not FileExists(FilePath + CDBMJZBDaten) then
    CreateZBDatenDB;                            { Tabelle automatisch anlegen }
end;

{------------------------------}
Destructor TMJZBDatenDB.Destroy;
{------------------------------}
begin
  ZBDatenTable.Free;
  inherited Destroy;
end;

{-------------------------------------}
procedure TMJZBDatenDB.CreateZBDatenDB;
{-------------------------------------}
{ Datenzeitbereich-Tabelle anlegen }
begin
  with ZBDatenTable.FieldDefs do begin
    Clear;
    Add(C_MJZBDaten_JournalId, ftInteger, 0, false);
    Add(C_MJZBDaten_SollVon, ftDateTime, 0, false);
    Add(C_MJZBDaten_SollBis, ftDateTime, 0, false);
    Add(C_MJZBDaten_IstVon, ftDateTime, 0, false);
    Add(C_MJZBDaten_IstBis, ftDateTime, 0, false);
  end;
  with ZBDatenTable.IndexDefs do begin
    Clear;
    Add('PJournalId', C_MJZBDaten_JournalId, [ixPrimary,ixUnique]);           { Primärindex }
  end;
  ZBDatenTable.CreateTable;
end;

{ Gibt die Zeitbereiche zu einer Id zurück                  }
{ Parameter: JournalId des Eintrags                         }
{ Rückgabe: Record mit Daten-Zeit-Bereichen                 }
{-----------------------------------------------------------}
function TMJZBDatenDB.ReadZbDaten(JournalId: integer): TZeitbereich;
{-----------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DataBaseName:= ZBDatenTable.DatabaseName;
    q.Sql.Add('SELECT * FROM "' + CDBMJZBDaten + '"');
    q.Sql.Add('WHERE ' + C_MJZBDaten_JournalId + '= ' + IntToStr(JournalId));
    q.open;
    result.SollVon:= q.FieldByName(C_MJZBDaten_SollVon).asDateTime;
    result.SollBis:= q.FieldByName(C_MJZBDaten_SollBis).asDateTime;
    result.IstVon:= q.FieldByName(C_MJZBDaten_IstVon).asDateTime;
    result.IstBis:= q.FieldByName(C_MJZBDaten_IstBis).asDateTime;
  except
    q.Free;
    if IsDebugFlag then showMessage('Zustand - ReadZbDaten');
    result.SollVon:= 0;
    result.SollBis:= 0;
    result.IstVon:= 0;
    result.IstBis:= 0;
    exit;
  end;
  q.free;
end;

{-------------------------------------------------------------------------------}
procedure TMJZBDatenDB.WriteZBDatenSoll (JournalId: integer;
                                         SollVon: TDateTime; SollBis: TDateTime);
{-------------------------------------------------------------------------------}
{ Soll-Datenbereich für JournalId in Tabelle schreiben;
  Übergabe: JournalId
            SollVon
            SollBis }
begin
  if JournalId > 0 then begin
    if ZBDatenTable.OpenShared then begin
      try
        if ZBDatenTable.FindKey ([JournalId]) then begin
          ZBDatenTable.Edit;
          if Int (SollVon) > 0 then
            ZBDatenTable.FieldByName (C_MJZBDaten_SollVon).AsDateTime:=SollVon;
          ZBDatenTable.FieldByName (C_MJZBDaten_SollBis).AsDateTime:=SollBis;
          ZBDatenTable.Post;
        end
        else begin
          if Int (SollVon) > 0 then
            ZBDatenTable.InsertRecord ([JournalId, SollVon, SollBis])
          else
            ZBDatenTable.InsertRecord ([JournalId, nil, SollBis])
        end;
      finally
        ZBDatenTable.Close;
      end;
    end;
  end;
end;

{--------------------------------------------------------------------------------------}
procedure TMJZBDatenDB.WriteZBDatenIst (JournalId: integer;
                                        IstVon: TDateTime; IstBis: TDateTime;
                                        var SollVon: TDateTime; var SollBis: TDateTime);
{--------------------------------------------------------------------------------------}
{ Ist-Datenbereich für JournalId in Tabelle schreiben, wenn IstVon, IstBis > 0;
  ansonsten nur Soll-Datenbereich lesen
  Übergabe: JournalId
            IstVon
            IstBis
  Rückgabe: SollVon  ( = 0, wenn kein Eintrag vorhanden )
            SollBis  ( = 0, wenn kein Eintrag vorhanden ) }
begin
  if JournalId > 0 then begin
    SollVon:=0;
    SollBis:=0;
    if ZBDatenTable.OpenShared then begin
      try
        if ZBDatenTable.FindKey ([JournalId]) then begin
          { Soll-Zeitbereich lesen: }
          if not ZBDatenTable.FieldByName (C_MJZBDaten_SollVon).IsNull then
            SollVon:=ZBDatenTable.FieldByName (C_MJZBDaten_SollVon).AsDateTime;
          SollBis:=ZBDatenTable.FieldByName (C_MJZBDaten_SollBis).AsDateTime;

          { Ist-Zeitbereich schreiben, wenn Von, Bis > 0: }
          if (IstVon > 0) AND (IstBis > 0) then begin
            ZBDatenTable.Edit;
            if ZBDatenTable.FieldByName (C_MJZBDaten_IstVon).IsNull then   { nie überschreiben ! }
              ZBDatenTable.FieldByName (C_MJZBDaten_IstVon).AsDateTime:=IstVon;
            ZBDatenTable.FieldByName (C_MJZBDaten_IstBis).AsDateTime:=IstBis;
            ZBDatenTable.Post;
          end;
        end
        else begin
          if (IstVon > 0) AND (IstBis > 0) then
            ZBDatenTable.InsertRecord ([JournalId, nil, nil, IstVon, IstBis]);
        end;
      finally
        ZBDatenTable.Close;
      end;
    end;
  end;
end;

{ Löscht Eintrag zu einer Id aus der MRG-Zeitbereich-Tabelle }
{ Parameter:  JournalId des Records                          }
{ Rückgabe: True - alles i.O.                                }
{------------------------------------------------------------}
function TMJZBDatenDB.DeleteRecord(JournalId: integer): boolean;
{------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= ZBDatenTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBMJZBDaten + '"');
      Add('WHERE ' +  C_MJZBDaten_JournalId + ' = ' + IntToStr(JournalId));
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
{ die übergebene aus der MRG-Zeitbereich-Tabelle                      }
{ Parameter:  JournalId des Records                                   }
{ Rückgabe: True - alles i.O.                                         }
{---------------------------------------------------------------------}
function TMJZBDatenDB.DeleteRecordsBefore(JournalId: integer): boolean;
{---------------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= ZBDatenTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBMJZBDaten + '"');
      Add('WHERE ' +  C_MJZBDaten_JournalId + ' < ' + IntToStr(JournalId));
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

{ Löscht alle übergebenen Einträge der MRG-Zeitbereich-Tabelle }
{ Parameter:  Query mit zu löschenden Einträgen                }
{             Query ist offen                                  }
{ Rückgabe: True - alles i.O.                                  }
{--------------------------------------------------------------}
function TMJZBDatenDB.DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
{--------------------------------------------------------------}
begin
  try
    if aQuery.RecordCount > 0 then begin
      aQuery.first;
      while not aQuery.Eof do begin
        if aQuery.FieldByName(C_WJournal_GeraeteArt).asString = C_GerArtMrg then
          DeleteRecord (aQuery.FieldByName(C_WJournal_JournalId).asInteger);
        aQuery.Next;
      end;
    end;
    result:= true;
  except
    result:= false;
    exit;
  end;
end;


{ TDJZBDatenDB }

{----------------------------------------------------}
constructor TDJZBDatenDB.Create (FilePath: TFileName);
{----------------------------------------------------}
begin
  inherited Create;
  ZBDatenTable:=TTableExt.Create (nil);
  ZBDatenTable.DatabaseName:=FilePath;
  ZBDatenTable.TableName:=CDBDJZBDaten;
  if not ZBDatenTable.Exists then
    CreateZBDatenDB;                            { Tabelle automatisch anlegen }
end;

{------------------------------}
Destructor TDJZBDatenDB.Destroy;
{------------------------------}
begin
  ZBDatenTable.Free;
  inherited Destroy;
end;

{-------------------------------------}
procedure TDJZBDatenDB.CreateZBDatenDB;
{-------------------------------------}
{ Datenzeitbereich-Tabelle anlegen }
begin
  with ZBDatenTable.FieldDefs do begin
    Clear;
    Add(C_DJZBDaten_JournalId, ftInteger, 0, false);
    Add(C_DJZBDaten_InstanzId, ftInteger, 0, false);
    Add(C_DJZBDaten_ArchLogbNr, ftInteger, 0, false);
    Add(C_DJZBDaten_KanalNr, ftInteger, 0, false);
    Add(C_DJZBDaten_Fehler, ftSmallInt, 0, false);
    Add(C_DJZBDaten_DZ_SollVon, ftDateTime, 0, false);
    Add(C_DJZBDaten_DZ_SollBis, ftDateTime, 0, false);
    Add(C_DJZBDaten_DZ_IstVon, ftDateTime, 0, false);
    Add(C_DJZBDaten_DZ_IstBis, ftDateTime, 0, false);
    Add(C_DJZBDaten_OrdNr_SollVon, ftInteger, 0, false);
    Add(C_DJZBDaten_OrdNr_SollBis, ftInteger, 0, false);
    Add(C_DJZBDaten_OrdNr_IstVon, ftInteger, 0, false);
    Add(C_DJZBDaten_OrdNr_IstBis, ftInteger, 0, false);
  end;
  with ZBDatenTable.IndexDefs do begin
    Clear;
    Add('PJournalId', C_DJZBDaten_JournalId+';'+C_DJZBDaten_InstanzId+';'+C_DJZBDaten_ArchLogbNr+';'+C_DJZBDaten_KanalNr,
        [ixPrimary,ixUnique]);           { Primärindex }
  end;
  ZBDatenTable.CreateTable;
end;

{---------------------------------------------------------------------------------------------------------------------}
procedure TDJZBDatenDB.WriteZBDatenSoll (JournalId: integer; InstanzId: integer; ArchLogbNr: integer; KanalNr: integer;
                                         SollVon: TDateTime; SollBis: TDateTime; isOrdNr: boolean);
{---------------------------------------------------------------------------------------------------------------------}
{ Soll-Datenbereich eines DSfG-Archivkanals bzw. Logbuchs in Tabelle schreiben;
  Übergabe: JournalId
            InstanzId
            Archivgruppen- bzw. Logbuchnr.
            Archivkanalnr.
            SollVon
            SollBis
            isOrdNr (wenn true, enthält SollVon, SollBis eine Ordnungsnummer, sonst Zeitangabe) }
begin
  if JournalId > 0 then begin
    if ZBDatenTable.OpenShared then begin
      try
        if ZBDatenTable.FindKey ([JournalId, InstanzId, ArchLogbNr, KanalNr]) then begin
          ZBDatenTable.Edit;
          if isOrdNr then begin              { Soll-Ordnungsnummern schreiben }
            ZBDatenTable.FieldByName (C_DJZBDaten_OrdNr_SollVon).AsInteger:=Trunc (SollVon);
            ZBDatenTable.FieldByName (C_DJZBDaten_OrdNr_SollBis).AsInteger:=Trunc (SollBis);
          end
          else begin                             { Soll-Zeitbereich schreiben }
            if Int (SollVon) > 0 then
              ZBDatenTable.FieldByName (C_DJZBDaten_DZ_SollVon).AsDateTime:=SollVon;
            if Int (SollBis) > 0 then
              ZBDatenTable.FieldByName (C_DJZBDaten_DZ_SollBis).AsDateTime:=SollBis;
          end;
          ZBDatenTable.Post;
        end
        else begin
          if isOrdNr then
            ZBDatenTable.InsertRecord ([JournalId, InstanzId, ArchLogbNr, KanalNr, nil,
                                        nil, nil, nil, nil,
                                        SollVon, SollBis])
          else begin
            ZBDatenTable.Append;
            ZBDatenTable.FieldByName (C_DJZBDaten_JournalId).AsInteger:=JournalId;
            ZBDatenTable.FieldByName (C_DJZBDaten_InstanzId).AsInteger:=InstanzId;
            ZBDatenTable.FieldByName (C_DJZBDaten_ArchLogbNr).AsInteger:=ArchLogbNr;
            ZBDatenTable.FieldByName (C_DJZBDaten_KanalNr).AsInteger:=KanalNr;
            if Int (SollVon) > 0 then
              ZBDatenTable.FieldByName (C_DJZBDaten_DZ_SollVon).AsDateTime:=SollVon;
            if Int (SollBis) > 0 then
              ZBDatenTable.FieldByName (C_DJZBDaten_DZ_SollBis).AsDateTime:=SollBis;
            ZBDatenTable.Post;
          end;
        end;
      finally
        ZBDatenTable.Close;
      end;
    end;
  end;
end;

{--------------------------------------------------------------------------------------------------------------------}
procedure TDJZBDatenDB.WriteZBDatenIst (JournalId: integer; InstanzId: integer; ArchLogbNr: integer; KanalNr: integer;
                                        Fehlercode: integer;
                                        IstVon_OrdNr: integer; IstBis_OrdNr: integer;
                                        IstVon_DZ: TDateTime; IstBis_DZ: TDateTime);
{--------------------------------------------------------------------------------------------------------------------}
{ Ist-Datenbereich (Ordnungsnummer und Datum/Zeit) eines DSfG-Archivkanals bzw.
  Logbuchs und Detailfehlercode in Tabelle schreiben;
  Übergabe: JournalId
            InstanzId
            Archivgruppen- bzw. Logbuchnr.
            Archivkanalnr.
            Fehlercode (Detailfehlercode für DSfG)
            IstVon-Ordnungsnummer
            IstBis-Ordnungsnummer
            IstVon-Datum/Zeit
            IstBis-Datum/Zeit }
begin
  if JournalId > 0 then begin
    if ZBDatenTable.OpenShared then begin
      try
        if ZBDatenTable.FindKey ([JournalId, InstanzId, ArchLogbNr, KanalNr]) then begin
          ZBDatenTable.Edit;
          { Ist-Ordnungsnummern nur schreiben, wenn Von, Bis > 0: }
          if (IstVon_OrdNr > 0) AND (IstBis_OrdNr > 0) then begin
            if ZBDatenTable.FieldByName (C_DJZBDaten_OrdNr_IstVon).IsNull then { nie überschreiben ! }
              ZBDatenTable.FieldByName (C_DJZBDaten_OrdNr_IstVon).AsInteger:=IstVon_OrdNr;
            ZBDatenTable.FieldByName (C_DJZBDaten_OrdNr_IstBis).AsInteger:=IstBis_OrdNr;
          end;
          { Ist-Zeitbereich nur schreiben, wenn Von, Bis > 0: }
          if (IstVon_DZ > 0) AND (IstBis_DZ > 0) then begin
            if ZBDatenTable.FieldByName (C_DJZBDaten_DZ_IstVon).IsNull then    { nie überschreiben ! }
              ZBDatenTable.FieldByName (C_DJZBDaten_DZ_IstVon).AsDateTime:=IstVon_DZ;
            ZBDatenTable.FieldByName (C_DJZBDaten_DZ_IstBis).AsDateTime:=IstBis_DZ;
          end;
          { Fehlercode immer schreiben: }
          ZBDatenTable.FieldByName (C_DJZBDaten_Fehler).AsInteger:=Fehlercode;
          ZBDatenTable.Post;
        end
        else begin
          ZBDatenTable.Append;
          ZBDatenTable.FieldByName (C_DJZBDaten_JournalId).AsInteger:=JournalId;
          ZBDatenTable.FieldByName (C_DJZBDaten_InstanzId).AsInteger:=InstanzId;
          ZBDatenTable.FieldByName (C_DJZBDaten_ArchLogbNr).AsInteger:=ArchLogbNr;
          ZBDatenTable.FieldByName (C_DJZBDaten_KanalNr).AsInteger:=KanalNr;
          ZBDatenTable.FieldByName (C_DJZBDaten_Fehler).AsInteger:=Fehlercode;
          { Ist-Zeitbereich nur schreiben, wenn IstVon, IstBis > 0: }
          if (IstVon_DZ > 0) AND (IstBis_DZ > 0) then begin
            ZBDatenTable.FieldByName (C_DJZBDaten_DZ_IstVon).AsDateTime:=IstVon_DZ;
            ZBDatenTable.FieldByName (C_DJZBDaten_DZ_IstBis).AsDateTime:=IstBis_DZ;
          end;
          { Ist-Ordnungsnummernbereich nur schreiben, wenn IstVon, IstBis > 0: }
          if (IstVon_OrdNr > 0) AND (IstBis_OrdNr > 0) then begin
            ZBDatenTable.FieldByName (C_DJZBDaten_OrdNr_IstVon).AsInteger:=IstVon_OrdNr;
            ZBDatenTable.FieldByName (C_DJZBDaten_OrdNr_IstBis).AsInteger:=IstBis_OrdNr;
          end;
          ZBDatenTable.Post;
        end;
      finally
        ZBDatenTable.Close;
      end;
    end;
  end;
end;

{ Löscht Einträge zu einer Id aus der DSfG-Zeitbereich-Tabelle }
{ Parameter:  JournalId der Records                            }
{ Rückgabe: True - alles i.O.                                  }
{--------------------------------------------------------------}
function TDJZBDatenDB.DeleteRecords(JournalId: integer): boolean;
{--------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= ZBDatenTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBDJZBDaten + '"');
      Add('WHERE ' +  C_DJZBDaten_JournalId + ' = ' + IntToStr(JournalId));
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
{ die übergebene aus der DSfG-Zeitbereich-Tabelle                     }
{ Parameter:  JournalId                                               }
{ Rückgabe: True - alles i.O.                                         }
{---------------------------------------------------------------------}
function TDJZBDatenDB.DeleteRecordsBefore(JournalId: integer): boolean;
{---------------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= ZBDatenTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBDJZBDaten + '"');
      Add('WHERE ' +  C_DJZBDaten_JournalId + ' < ' + IntToStr(JournalId));
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

{ Löscht alle übergebenen Einträge der DSfG-Zeitbereich-Tabelle }
{ Parameter:  Query mit zu löschenden Einträgen                 }
{             Query ist offen                                   }
{ Rückgabe: True - alles i.O.                                   }
{---------------------------------------------------------------}
function TDJZBDatenDB.DeleteAllRecordsFromQuery(aQuery: TQuery): boolean;
{---------------------------------------------------------------}
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

{ Löscht Einträge zu einer InstanzId aus der DSfG-Zeitbereich-Tabelle }
{ Parameter:  InstanzId der Records                            }
{ Rückgabe: True - alles i.O.                                  }
{--------------------------------------------------------------}
function TDJZBDatenDB.DeleteRecordsByInstanz(InstanzId: integer): boolean;
{--------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= ZBDatenTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBDJZBDaten + '"');
      Add('WHERE ' +  C_DJZBDaten_InstanzId + ' = ' + IntToStr(InstanzId));
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

{ Löscht Einträge zu einer ArchivNr aus der DSfG-Zeitbereich-Tabelle }
{ Parameter:  InstanzId, ArchivNr der Records                  }
{ Rückgabe: True - alles i.O.                                  }
{--------------------------------------------------------------}
function TDJZBDatenDB.DeleteRecordsByArchiv(InstanzId: integer; ArchivNr: integer): boolean;
{--------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= ZBDatenTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBDJZBDaten + '"');
      Add('WHERE (' +  C_DJZBDaten_InstanzId + ' = ' + IntToStr(InstanzId) + ') AND ');
      Add('(' +  C_DJZBDaten_ArchLogbNr + ' = ' + IntToStr(ArchivNr) + ') AND ');
      Add('(' +  C_DJZBDaten_KanalNr + ' > 0)');
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

{ Löscht Einträge zu einer ArchivkanalNr aus der DSfG-Zeitbereich-Tabelle }
{ Parameter:  InstanzId, ArchivNr, KanalNr der Records         }
{ Rückgabe: True - alles i.O.                                  }
{--------------------------------------------------------------}
function TDJZBDatenDB.DeleteRecordsByArchivkanal(InstanzId: integer; ArchivNr: integer; KanalNr: integer): boolean;
{--------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= ZBDatenTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBDJZBDaten + '"');
      Add('WHERE (' +  C_DJZBDaten_InstanzId + ' = ' + IntToStr(InstanzId) + ') AND ');
      Add('(' +  C_DJZBDaten_ArchLogbNr + ' = ' + IntToStr(ArchivNr) + ') AND ');
      Add('(' +  C_DJZBDaten_KanalNr + ' = ' + IntToStr(KanalNr) + ')');
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

{ Löscht Einträge zu einer LogbuchNr aus der DSfG-Zeitbereich-Tabelle }
{ Parameter:  InstanzId, LogbuchNr der Records                 }
{ Rückgabe: True - alles i.O.                                  }
{--------------------------------------------------------------}
function TDJZBDatenDB.DeleteRecordsByLogbuch(InstanzId: integer; LogbuchNr: integer): boolean;
{--------------------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.Create(nil);
  try
    q.DatabaseName:= ZBDatenTable.DatabaseName;
    with q.Sql do begin
      Add('DELETE FROM "' + CDBDJZBDaten + '"');
      Add('WHERE (' +  C_DJZBDaten_InstanzId + ' = ' + IntToStr(InstanzId) + ') AND ');
      Add('(' +  C_DJZBDaten_ArchLogbNr + ' = ' + IntToStr(LogbuchNr) + ') AND ');
      Add('(' +  C_DJZBDaten_KanalNr + ' < 0)');
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

{ Gibt die Zeit-/Ordnungsnummern-Bereiche abgerufener Archivkanäle zu einer JournalId zurück }
{ -> für Archiv-Einträge in der Tabelle gilt: ArchLogbNr > 0, KanalNr > 0 ! }
{ Parameter: Übergabequery, JournalId                           }
{ Rückgabe: True - Aktion erfolgreich                           }
{---------------------------------------------------------------}
function TDJZBDatenDB.ReadZbDaten_Archive (var aQuery: TQuery; JournalId: integer): boolean;
{---------------------------------------------------------------}
begin
  try
    with aQuery do begin
      Close;
      DataBaseName:= ZBDatenTable.DatabaseName;
      Sql.Clear;
      Sql.Add('SELECT I.' + C_DTF_Instanz_Instanzname + ', ');
      Sql.Add('       A.' + C_DTF_Archive_Name + ', ');
      Sql.Add('       K.' + C_DTF_AKanaele_Name + ' as Kanalname'  + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_InstanzId + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_ArchLogbNr + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_KanalNr + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_Fehler + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_DZ_SollVon + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_DZ_SollBis + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_DZ_IstVon + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_DZ_IstBis + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_OrdNr_SollVon + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_OrdNr_SollBis + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_OrdNr_IstVon + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_OrdNr_IstBis);
      Sql.Add('FROM "' + C_DTB_Instanz + '" I, ');
      Sql.Add('     "' + C_DTB_Archive + '" A, ');
      Sql.Add('     "' + C_DTB_AKanaele + '" K, ');
      Sql.Add('     "' + CDBDJZBDaten + '" Z');
      Sql.Add('WHERE Z.' + C_DJZBDaten_JournalId + ' = ' + IntToStr(JournalId) + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_KanalNr + ' > 0 AND ');
      Sql.Add('Z.' + C_DJZBDaten_InstanzId + ' = I.' + C_DTF_Instanz_InstanzId + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_InstanzId + ' = A.' + C_DTF_Archive_InstanzId + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_ArchLogbNr + ' = A.' + C_DTF_Archive_ArchivNr + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_InstanzId + ' = K.' + C_DTF_AKanaele_InstanzId + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_ArchLogbNr + ' = K.' + C_DTF_AKanaele_ArchivNr + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_KanalNr + ' = K.' + C_DTF_AKanaele_KanalNr);
      Sql.Add('ORDER BY I.' + C_DTF_Instanz_Instanzname + ', Z.' + C_DJZBDaten_InstanzId + ', ');
      Sql.Add('         A.' + C_DTF_Archive_Name + ', Z.' + C_DJZBDaten_ArchLogbNr + ', ');
      Sql.Add('         K.' + C_DTF_AKanaele_Name);
      aQuery.Open;
    end;
    result:= true;
  except
    if IsDebugFlag then showMessage('Journal - DSfG-ReadZbDaten_Archive');
    result:= false;
  end;
end;

{ Gibt die Zeit-/Ordnungsnummern-Bereiche abgerufener Logbücher zu einer JournalId zurück }
{ -> für Logbuch-Einträge in der Tabelle gilt: ArchLogbNr > 0, KanalNr = -1 ! }
{ Parameter: Übergabequery, JournalId                           }
{ Rückgabe: True - Aktion erfolgreich                           }
{---------------------------------------------------------------}
function TDJZBDatenDB.ReadZbDaten_Logbuecher (var aQuery: TQuery; JournalId: integer): boolean;
{---------------------------------------------------------------}
begin
  try
    with aQuery do begin
      Close;
      DataBaseName:= ZBDatenTable.DatabaseName;
      Sql.Clear;
      Sql.Add('SELECT I.' + C_DTF_Instanz_Instanzname + ', ');
      Sql.Add('       L.' + C_DTF_Logbuch_Name + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_InstanzId + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_ArchLogbNr + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_Fehler + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_DZ_SollVon + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_DZ_SollBis + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_DZ_IstVon + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_DZ_IstBis + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_OrdNr_SollVon + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_OrdNr_SollBis + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_OrdNr_IstVon + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_OrdNr_IstBis);
      Sql.Add('FROM "' + C_DTB_Instanz + '" I, ');
      Sql.Add('     "' + C_DTB_Logbuch + '" L, ');
      Sql.Add('     "' + CDBDJZBDaten + '" Z');
      Sql.Add('WHERE Z.' + C_DJZBDaten_JournalId + ' = ' + IntToStr(JournalId) + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_ArchLogbNr + ' > 0 AND ');
      Sql.Add('Z.' + C_DJZBDaten_KanalNr + ' < 0 AND ');
      Sql.Add('Z.' + C_DJZBDaten_InstanzId + ' = I.' + C_DTF_Instanz_InstanzId + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_InstanzId + ' = L.' + C_DTF_Logbuch_InstanzId + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_ArchLogbNr + ' = L.' + C_DTF_Logbuch_LogbuchNr);
      Sql.Add('ORDER BY I.' + C_DTF_Instanz_Instanzname + ', Z.' + C_DJZBDaten_InstanzId + ', ');
      Sql.Add('         L.' + C_DTF_Logbuch_Name + ', Z.' + C_DJZBDaten_ArchLogbNr);
      aQuery.Open;
    end;
    result:= true;
  except
    if IsDebugFlag then showMessage('Journal - DSfG-ReadZbDaten_Logbuecher');
    result:= false;
  end;
end;

{ Gibt den Fehlercode abgerufener Datenelemente zu einer JournalId zurück }
{ -> für Datenelement-Einträge in der Tabelle gilt: ArchLogbNr = -1, KanalNr = -1 ! }
{ Parameter: Übergabequery, JournalId                           }
{ Rückgabe: True - Aktion erfolgreich                           }
{---------------------------------------------------------------}
function TDJZBDatenDB.ReadZbDaten_Datenelemente (var aQuery: TQuery; JournalId: integer): boolean;
{---------------------------------------------------------------}
begin
  try
    with aQuery do begin
      Close;
      DataBaseName:= ZBDatenTable.DatabaseName;
      Sql.Clear;
      Sql.Add('SELECT I.' + C_DTF_Instanz_Instanzname + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_InstanzId + ', ');
      Sql.Add('       Z.' + C_DJZBDaten_Fehler);
      Sql.Add('FROM "' + C_DTB_Instanz + '" I, ');
      Sql.Add('     "' + CDBDJZBDaten + '" Z');
      Sql.Add('WHERE Z.' + C_DJZBDaten_JournalId + ' = ' + IntToStr(JournalId) + ' AND ');
      Sql.Add('Z.' + C_DJZBDaten_ArchLogbNr + ' < 0 AND ');
      Sql.Add('Z.' + C_DJZBDaten_KanalNr + ' < 0 AND ');
      Sql.Add('Z.' + C_DJZBDaten_InstanzId + ' = I.' + C_DTF_Instanz_InstanzId);
      Sql.Add('ORDER BY I.' + C_DTF_Instanz_Instanzname + ', Z.' + C_DJZBDaten_InstanzId);
      aQuery.Open;
    end;
    result:= true;
  except
    if IsDebugFlag then showMessage('Journal - DSfG-ReadZbDaten_Logbuecher');
    result:= false;
  end;
end;

end.
