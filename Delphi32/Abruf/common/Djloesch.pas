{***********************************************************************}
{* Unit: L�schen von DSfG-Journaldaten                                 *}
{* 16.02.2000 WW                                                       *}
{***********************************************************************}
unit DJLoesch;

interface

uses
  PathIni, ZBDatDb;


function DeleteDZBereichByInstanz (InstanzId: integer): boolean;

function DeleteDZBereichByArchiv (InstanzId: integer; ArchivNr: integer): boolean;

function DeleteDZBereichByArchivkanal (InstanzId: integer; ArchivNr: integer; KanalNr: integer): boolean;

function DeleteDZBereichByLogbuch (InstanzId: integer; LogbuchNr: integer): boolean;

implementation

{--------------------------------------------------------------}
function DeleteDZBereichByInstanz (InstanzId: integer): boolean;
{--------------------------------------------------------------}
{ Alle zu InstanzId geh�renden Datenzeitbereich-Eintr�ge aus Tabelle l�schen;
  �bergabe: InstanzId
  Ergebnis: true, wenn L�schen ok }
var
  DJZBDatenDB: TDJZBDatenDB;
begin
  DJZBDatenDB:=TDJZBDatenDB.Create (Pathserver.Pathname [WStammDir]);
  try
    Result:=DJZBDatenDB.DeleteRecordsByInstanz (InstanzId);
  finally
    DJZBDatenDB.Free;
  end;
end;

{--------------------------------------------------------------------------------}
function DeleteDZBereichByArchiv (InstanzId: integer; ArchivNr: integer): boolean;
{--------------------------------------------------------------------------------}
{ Alle zu InstanzId und ArchivNr geh�renden Datenzeitbereich-Eintr�ge aus Tabelle l�schen;
  �bergabe: InstanzId, ArchivNr
  Ergebnis: true, wenn L�schen ok }
var
  DJZBDatenDB: TDJZBDatenDB;
begin
  DJZBDatenDB:=TDJZBDatenDB.Create (Pathserver.Pathname [WStammDir]);
  try
    Result:=DJZBDatenDB.DeleteRecordsByArchiv (InstanzId, ArchivNr);
  finally
    DJZBDatenDB.Free;
  end;
end;

{-------------------------------------------------------------------------------------------------------}
function DeleteDZBereichByArchivkanal (InstanzId: integer; ArchivNr: integer; KanalNr: integer): boolean;
{-------------------------------------------------------------------------------------------------------}
{ Alle zu InstanzId, ArchivNr und KanalNr geh�renden Datenzeitbereich-Eintr�ge aus Tabelle l�schen;
  �bergabe: InstanzId, ArchivNr
  Ergebnis: true, wenn L�schen ok }
var
  DJZBDatenDB: TDJZBDatenDB;
begin
  DJZBDatenDB:=TDJZBDatenDB.Create (Pathserver.Pathname [WStammDir]);
  try
    Result:=DJZBDatenDB.DeleteRecordsByArchivkanal (InstanzId, ArchivNr, KanalNr);
  finally
    DJZBDatenDB.Free;
  end;
end;

{----------------------------------------------------------------------------------}
function DeleteDZBereichByLogbuch (InstanzId: integer; LogbuchNr: integer): boolean;
{----------------------------------------------------------------------------------}
{ Alle zu InstanzId und LogbuchNr geh�renden Datenzeitbereich-Eintr�ge aus Tabelle l�schen;
  �bergabe: InstanzId, LogbuchNr
  Ergebnis: true, wenn L�schen ok }
var
  DJZBDatenDB: TDJZBDatenDB;
begin
  DJZBDatenDB:=TDJZBDatenDB.Create (Pathserver.Pathname [WStammDir]);
  try
    Result:=DJZBDatenDB.DeleteRecordsByLogbuch (InstanzId, LogbuchNr);
  finally
    DJZBDatenDB.Free;
  end;
end;

end.

