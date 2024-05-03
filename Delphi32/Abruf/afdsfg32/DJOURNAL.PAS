{********************************************************************************************}
{* Unit: Zugriffe auf Journal-Tabelle und deren Detailtabellen (Fehler, Daten-Zeitbereiche) *}
{*       für DSfG-Abrufe                                                                    *}
{* 19.11.1999 WW                                                                            *}
{* 15.05.2001 WW  Zugriff auf DSfG-Telegrammtabelle                                         *}
(* 02.04.2002 GD  Funktionen mit optionaler Datenbankübergabe                               *)
{********************************************************************************************}
unit DJournal;

interface

uses
  Classes, SysUtils, PathIni, JournlDb, WSysCon, ErrPrc32, ZBDatDb, RFilesDb,
  TelegrDb;


function WriteNewDSfGJournal (StationId: integer; Kennung: string;
  Abrufart: string; Datentypen: integer; ComNr: integer;
  sDatabase: string = ''): integer;

procedure UpdateDSfGJournal (
  JournalId: integer; DZFieldName: string; sDatabase: string = '');

procedure UpdateDSfGJournalStationId (
  JournalId: integer; StationId: integer; sDatabase: string = '');

procedure UpdateDSfGJournalKennung (
  JournalId: integer; Kennung: string; sDatabase: string = '');

procedure UpdateDSfGJournalDatentypen (
  JournalId: integer; Datentypen: integer; sDatabase: string = '');

procedure WriteJournalFehler (
  JournalId: integer; Gruppe: integer; Fehler: integer; sDatabase: string = '');

procedure WriteDatenzeitbereich_Soll (JournalId: integer; InstanzId: integer;
  ArchLogbNr: integer; KanalNr: integer; SollVon: TDateTime;
  SollBis: TDateTime; isOrdNr: boolean; sDatabase: string = '');

procedure WriteDatenzeitbereich_Ist (JournalId: integer; InstanzId: integer;
  ArchLogbNr: integer; KanalNr: integer; Fehler: integer; IstVon_OrdNr: integer;
  IstBis_OrdNr: integer; IstVon_DZ: TDateTime; IstBis_DZ: TDateTime;
  sDatabase: string = '');

procedure WriteRohdateiname (
  JournalId: integer; FileName: TFileName; sDatabase: string = '');

procedure WriteJournalTelegramme (
  JournalId: integer; TelegrammList: TList; sDatabase: string = '');

implementation

uses
  DAufmTelegr;


{----------------------------------------------------------------------------------}
function WriteNewDSfGJournal (StationId: integer; Kennung: string;
  Abrufart: string; Datentypen: integer; ComNr: integer;
  sDatabase: string): integer;
{----------------------------------------------------------------------------------}
{ neuen DSfG-Journaleintrag in Journal-Tabelle einfügen;
  Rückgabe: neue JournalId }
var
  JournalDB: TJournalDB;
begin
  if (sDatabase = '') then sDatabase := Pathserver.Pathname [WStammDir];
  JournalDB:=TJournalDB.Create (sDatabase);
  try
    Result:=JournalDB.WriteNewJournal (C_GerArtDSfG, StationId, Kennung, Abrufart, Datentypen, ComNr);
  finally
    JournalDB.Free;
  end;
end;

{--------------------------------------------------------------------}
procedure UpdateDSfGJournal (
  JournalId: integer; DZFieldName: string; sDatabase: string = '');
{--------------------------------------------------------------------}
{ Zeitpunkt für einen DSfG-Eintrag in der Journal-Tabelle aktualisieren;
  Übergabe: JournalId
            Name des Datum/Zeit-Feldes der Tabelle }
var
  JournalDB: TJournalDB;
begin
  if (sDatabase = '') then sDatabase := Pathserver.Pathname [WStammDir];
  JournalDB:=TJournalDB.Create (sDatabase);
  try
    JournalDB.UpdateJournal (JournalId, 0, DZFieldName, '', 0);
  finally
    JournalDB.Free;
  end;
end;

{----------------------------------------------------------------------------}
procedure UpdateDSfGJournalStationId (
  JournalId: integer; StationId: integer; sDatabase: string = '');
{----------------------------------------------------------------------------}
{ StationId in einem DSfG-Eintrag der Journal-Tabelle aktualisieren;
  Übergabe: JournalId
            StationId }
var
  JournalDB: TJournalDB;
begin
  if (sDatabase = '') then sDatabase := Pathserver.Pathname [WStammDir];
  JournalDB:=TJournalDB.Create (sDatabase);
  try
    JournalDB.UpdateJournal (JournalId, StationId, '', '', 0);
  finally
    JournalDB.Free;
  end;
end;


{-----------------------------------------------------------------------}
procedure UpdateDSfGJournalKennung (
  JournalId: integer; Kennung: string; sDatabase: string = '');
{-----------------------------------------------------------------------}
{ Kennung für einen DSfG-Eintrag in der Journal-Tabelle aktualisieren;
  Übergabe: JournalId
            Kennung }
var
  JournalDB: TJournalDB;
begin
  if (sDatabase = '') then sDatabase := Pathserver.Pathname [WStammDir];
  JournalDB:=TJournalDB.Create (sDatabase);
  try
    JournalDB.UpdateJournal (JournalId, 0, '', Kennung, 0);
  finally
    JournalDB.Free;
  end;
end;

{------------------------------------------------------------------------------}
procedure UpdateDSfGJournalDatentypen (
  JournalId: integer; Datentypen: integer; sDatabase: string = '');
{------------------------------------------------------------------------------}
{ datentypen für einen DSfG-Eintrag in der Journal-Tabelle aktualisieren;
  Übergabe: JournalId
            Datentypen }
var
  JournalDB: TJournalDB;
begin
  if (sDatabase = '') then sDatabase := Pathserver.Pathname [WStammDir];
  JournalDB:=TJournalDB.Create (sDatabase);
  try
    JournalDB.UpdateJournal (JournalId, 0, '', '', Datentypen);
  finally
    JournalDB.Free;
  end;
end;

{----------------------------------------------------------------------------------}
procedure WriteJournalFehler (
  JournalId: integer; Gruppe: integer; Fehler: integer; sDatabase: string = '');
{----------------------------------------------------------------------------------}
{ neuen DSfG-Fehlereintrag in Fehler-Tabelle einfügen;
  Übergabe: JournalId
            Fehlergruppe
            Fehlercode }
var
  FehlerDB: TJFehlerDB;
  Klasse: integer;
begin
  if (sDatabase = '') then sDatabase := Pathserver.Pathname [WStammDir];
  FehlerDB:=TJFehlerDB.Create (sDatabase);
  try
    Klasse:=GetErrorKlasse (Gruppe, Fehler);
    FehlerDB.WriteFehler (JournalId, Klasse, Gruppe, Fehler);
  finally
    FehlerDB.Free;
  end;
end;

{------------------------------------------------------------------------------------------------------------------}
procedure WriteDatenzeitbereich_Soll (JournalId: integer; InstanzId: integer;
  ArchLogbNr: integer; KanalNr: integer; SollVon: TDateTime;
  SollBis: TDateTime; isOrdNr: boolean; sDatabase: string = '');
{------------------------------------------------------------------------------------------------------------------}
{ Soll-Datenbereich eines DSfG-Archivkanals bzw. Logbuchs in Tabelle schreiben;
  Übergabe: JournalId
            InstanzId
            Archivgruppen- bzw. Logbuchnr.
            Archivkanalnr.
            SollVon
            SollBis
            isOrdNr (wenn true, enthält SollVon, SollBis eine Ordnungsnummer, sonst Zeitangabe) }
var
  DJZBDatenDB: TDJZBDatenDB;
begin
  if (sDatabase = '') then sDatabase := Pathserver.Pathname [WStammDir];
  DJZBDatenDB:=TDJZBDatenDB.Create (sDatabase);
  try
    DJZBDatenDB.WriteZBDatenSoll (JournalId, InstanzId, ArchLogbNr, KanalNr, SollVon, SollBis, isOrdNr);
  finally
    DJZBDatenDB.Free;
  end;
end;

{-----------------------------------------------------------------------------------------------------------------}
procedure WriteDatenzeitbereich_Ist (JournalId: integer; InstanzId: integer;
  ArchLogbNr: integer; KanalNr: integer; Fehler: integer; IstVon_OrdNr: integer;
  IstBis_OrdNr: integer; IstVon_DZ: TDateTime; IstBis_DZ: TDateTime;
  sDatabase: string = '');
{-----------------------------------------------------------------------------------------------------------------}
{ Ist-Datenbereich (Ordnungsnummern und Datum/Zeit) eines DSfG-Archivkanals bzw.
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
var
  DJZBDatenDB: TDJZBDatenDB;
begin
  if (sDatabase = '') then sDatabase := Pathserver.Pathname [WStammDir];
  DJZBDatenDB:=TDJZBDatenDB.Create (sDatabase);
  try
    DJZBDatenDB.WriteZBDatenIst (JournalId, InstanzId, ArchLogbNr, KanalNr, Fehler,
                                 IstVon_OrdNr, IstBis_OrdNr, IstVon_DZ, IstBis_DZ);
  finally
    DJZBDatenDB.Free;
  end;
end;

{--------------------------------------------------------------------}
procedure WriteRohdateiname (
  JournalId: integer; FileName: TFileName; sDatabase: string = '');
{--------------------------------------------------------------------}
{ Rohdateiname in Journal-Detailtabelle schreiben;
  Übergabe: JournalId
            Rohdateiname }
var
  WJRFilesDB: TWJRFilesDB;
begin
  if (sDatabase = '') then sDatabase := Pathserver.Pathname [WStammDir];
  WJRFilesDB:=TWJRFilesDB.Create (sDatabase);
  try
    WJRFilesDB.WriteRohdateiname (JournalId, FileName);
  finally
    WJRFilesDB.Free;
  end;
end;

{--------------------------------------------------------------------------}
procedure WriteJournalTelegramme (
  JournalId: integer; TelegrammList: TList; sDatabase: string = '');
{--------------------------------------------------------------------------}
{ DSfG-Aufmerksamkeits-Telegramme in Journal-Detailtabelle schreiben;
  Übergabe: JournalId
            Telegrammliste }
var
  DJTelegrDB: TDJTelegrDB;
  i: integer;

begin
  DJTelegrDB:=TDJTelegrDB.Create (Pathserver.Pathname [WStammDir]);
  try
    if DJTelegrDB.OpenTelegrTable then begin
      for i:=0 to TelegrammList.Count-1 do begin
        DJTelegrDB.WriteTelegramm (JournalId,
                                   TAufmTelegrammListObj (TelegrammList.Items [i]).Busadresse,
                                   TAufmTelegrammListObj (TelegrammList.Items [i]).Nachrichtentyp,
                                   TAufmTelegrammListObj (TelegrammList.Items [i]).DatumZeit,
                                   TAufmTelegrammListObj (TelegrammList.Items [i]).Zeitzone);
      end;
      DJTelegrDB.CloseTelegrTable;
    end;
  finally
    DJTelegrDB.Free;
  end;
end;

end.

