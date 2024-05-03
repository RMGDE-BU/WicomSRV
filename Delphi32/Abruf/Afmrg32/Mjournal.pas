{********************************************************************************************}
{* Unit: Zugriffe auf Journal-Tabelle und deren Detailtabellen (Fehler, Daten-Zeitbereiche, *}
{*       Zeitsynchronisation) für MRG-Abrufe                                                *}
{* 12.01.1999 WW                                                                            *}
{********************************************************************************************}
unit MJournal;

interface

uses MDBSta, PathIni, JournlDb, WSysCon, ErrPrc32, ZBDatDb, ZSyncDb;


function WriteNewMRGJournal (MrgId: TMrgId; Kennung: string; Abrufart: string;
                             Datentypen: integer; ComNr: integer): integer;
procedure UpdateMRGJournal (JournalId: integer; DZFieldName: string);

procedure UpdateMRGJournalMrgId (JournalId: integer; MrgId: TMrgId);

procedure UpdateMRGJournalKennung (JournalId: integer; Kennung: string);

procedure WriteJournalFehler (JournalId: integer; Gruppe: integer; Fehler: integer);

procedure WriteDatenzeitbereich_Soll (JournalId: integer; SollVon: TDateTime; SollBis: TDateTime);

procedure WriteDatenzeitbereich_Ist (JournalId: integer;
                                     IstVon: TDateTime; IstBis: TDateTime;
                                     var SollVon: TDateTime; var SollBis: TDateTime);

procedure WriteNewMRGZeitSync (JournalId: integer);

procedure UpdateMRGZeitSync (JournalId: integer; MRGZeit: TDateTime; PCZeit: TDateTime);

implementation

{----------------------------------------------------------------------------}
function WriteNewMRGJournal (MrgId: TMrgId; Kennung: string; Abrufart: string;
                             Datentypen: integer; ComNr: integer): integer;
{----------------------------------------------------------------------------}
{ neuen MRG-Journaleintrag in Journal-Tabelle einfügen;
  Rückgabe: neue JournalId }
var
  JournalDB: TJournalDB;
begin
  JournalDB:=TJournalDB.Create (Pathserver.Pathname [WStammDir]);
  try
    Result:=JournalDB.WriteNewJournal (C_GerArtMrg, MrgId, Kennung, Abrufart, Datentypen, ComNr);
  finally
    JournalDB.Free;
  end;
end;

{-------------------------------------------------------------------}
procedure UpdateMRGJournal (JournalId: integer; DZFieldName: string);
{-------------------------------------------------------------------}
{ Datum/Zeit-Feld in einem MRG-Eintrag der Journal-Tabelle aktualisieren
  Übergabe: JournalId
            Name des Datum/Zeit-Feldes der Tabelle }
var
  JournalDB: TJournalDB;
begin
  JournalDB:=TJournalDB.Create (Pathserver.Pathname [WStammDir]);
  try
    JournalDB.UpdateJournal (JournalId, 0, DZFieldName, '', 0);
  finally
    JournalDB.Free;
  end;
end;

{------------------------------------------------------------------}
procedure UpdateMRGJournalMrgId (JournalId: integer; MrgId: TMrgId);
{------------------------------------------------------------------}
{ MrgId in einem MRG-Eintrag der Journal-Tabelle aktualisieren;
  Übergabe: JournalId
            MrgId }
var
  JournalDB: TJournalDB;
begin
  JournalDB:=TJournalDB.Create (Pathserver.Pathname [WStammDir]);
  try
    JournalDB.UpdateJournal (JournalId, MrgId, '', '', 0);
  finally
    JournalDB.Free;
  end;
end;



{----------------------------------------------------------------------}
procedure UpdateMRGJournalKennung (JournalId: integer; Kennung: string);
{----------------------------------------------------------------------}
{ Kennung in einem MRG-Eintrag der Journal-Tabelle aktualisieren;
  Übergabe: JournalId
            Kennung }
var
  JournalDB: TJournalDB;
begin
  JournalDB:=TJournalDB.Create (Pathserver.Pathname [WStammDir]);
  try
    JournalDB.UpdateJournal (JournalId, 0, '', Kennung, 0);
  finally
    JournalDB.Free;
  end;
end;

{----------------------------------------------------------------------------------}
procedure WriteJournalFehler (JournalId: integer; Gruppe: integer; Fehler: integer);
{----------------------------------------------------------------------------------}
{ neuen MRG-Fehlereintrag in Fehler-Tabelle einfügen;
  Übergabe: JournalId
            Fehlergruppe
            Fehlercode }
var
  FehlerDB: TJFehlerDB;
  Klasse: integer;
begin
  FehlerDB:=TJFehlerDB.Create (Pathserver.Pathname [WStammDir]);
  try
    Klasse:=GetErrorKlasse (Gruppe, Fehler);
    FehlerDB.WriteFehler (JournalId, Klasse, Gruppe, Fehler);
  finally
    FehlerDB.Free;
  end;
end;

{----------------------------------------------------------------------------}
procedure WriteDatenzeitbereich_Soll (JournalId: integer;
                                      SollVon: TDateTime; SollBis: TDateTime);
{----------------------------------------------------------------------------}
{ Soll-Datenbereich für JournalId in Datenzeitbereich-Tabelle schreiben
  Übergabe: JournalId
            SollVon
            SollBis }
var
  MJZBDatenDB: TMJZBDatenDB;
begin
  MJZBDatenDB:=TMJZBDatenDB.Create (Pathserver.Pathname [WStammDir]);
  try
    MJZBDatenDB.WriteZBDatenSoll (JournalId, SollVon, SollBis);
  finally
    MJZBDatenDB.Free;
  end;
end;

{-----------------------------------------------------------------------------------}
procedure WriteDatenzeitbereich_Ist (JournalId: integer;
                                     IstVon: TDateTime; IstBis: TDateTime;
                                     var SollVon: TDateTime; var SollBis: TDateTime);
{-----------------------------------------------------------------------------------}
{ Ist-Datenbereich für JournalId in Datenzeitbereich-Tabelle schreiben
  Übergabe: JournalId
            IstVon
            IstBis
  Rückgabe: SollVon  ( = 0, wenn kein Eintrag vorhanden )
            SollBis  ( = 0, wenn kein Eintrag vorhanden ) }
var
  MJZBDatenDB: TMJZBDatenDB;
begin
  MJZBDatenDB:=TMJZBDatenDB.Create (Pathserver.Pathname [WStammDir]);
  try
    MJZBDatenDB.WriteZBDatenIst (JournalId, IstVon, IstBis, SollVon, SollBis);
  finally
    MJZBDatenDB.Free;
  end;
end;

{-------------------------------------------------}
procedure WriteNewMRGZeitSync (JournalId: integer);
{-------------------------------------------------}
{ Neuen Eintrag für JournalId in ZeitSync-Tabelle anlegen }
var
  WJZSyncDB: TWJZSyncDB;
begin
  WJZSyncDB:=TWJZSyncDB.Create (Pathserver.Pathname [WStammDir]);
  try
    WJZSyncDB.WriteNewZSync (JournalId);
  finally
    WJZSyncDB.Free;
  end;
end;

{--------------------------------------------------------------------------------------}
procedure UpdateMRGZeitSync (JournalId: integer; MRGZeit: TDateTime; PCZeit: TDateTime);
{--------------------------------------------------------------------------------------}
{ MRG/PC-Zeit für JournalId in ZeitSync-Tabelle schreiben }
var
  WJZSyncDB: TWJZSyncDB;
begin
  WJZSyncDB:=TWJZSyncDB.Create (Pathserver.Pathname [WStammDir]);
  try
    WJZSyncDB.UpdateZSync (JournalId, MRGZeit, PCZeit);
  finally
    WJZSyncDB.Free;
  end;
end;

end.

