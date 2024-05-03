{------------------------------------------------------------------------------}
{ 01.12.1998 GD; Unit für die Verwaltung der WTermin.Db                        }
{                                                                              }
{          -> ist unabhängig, d.h. es muß alles als Parameter übergeben werden }
{          -> Greift auf Einstellungen in den Systemdaten zu (WSysDat.pas)     }
{                                                                              }
{ 09.04.1999 GD; Beschleunigung: Speichern aus Liste, Sekundärindex            }
{ 22.04.1999 GD; Funktionen für das Anzeigen und Löschen einer Station         }
{ 27.04.1999 GD; Sekundärindex wird nur beim Speichern überprüft               }
{ 10.05.1999 GD; Anzeige einer Station korrigiert                              }
{ 24.01.2001 GD; GetNextTermin verändert (Error-Handling)                      }
{ 06.11.2001 GD/WW; Fehler GetNextTermin raus (separate Query für MRG-Gruppen) }
{ 08.11.2001 GD; Fehler GetAbgleichList raus                                   }
{ 14.07.2003 H.-P.R.: Gruppenoffset (MRG) berücksichtigen                      }
{                                                                              }
{  (C) Karl Wieser GmbH 1998, 2003                                             }
{------------------------------------------------------------------------------}
unit TerminDb;

interface

uses
  Classes, db, dbtables, SysUtils, controls, WSysCon, forms, Dialogs,
  WSysDat, Db_Attn, WTables, FPrgBar, My_Utils;

const

  { Tabelle der Abruftermine }
  C_TbTermin        = 'WTermin.db';

  C_TfTerminId      = 'TerminId';        { Id des Eintrags }
  C_TfGeraeteArt    = 'GeraeteArt';      { Art des Gerätes }
  C_TfGeraeteId     = 'GeraeteId';       { Id des Gerätes }
  C_TfDatum         = 'Datum';           { Datum des 1. Abrufs }
  C_TfZeit          = 'Zeit';            { Zeit des Abrufs }
  C_TfWochentag     = 'Wochentag';       { Wochentag  - 1 = Sonntag ! }
  C_TFAbrufe        = 'Datentypen';      { Art der Abrufe als Status }
  C_TfEinmalFlag    = 'Einmal';          { True: nur ein Aufruf }

  C_TbTerminIndex1  = 'SEintrag';        { Sekundärindex auf Termintabelle }

type

  TTerminRec = class(TObject)
    TerminId: integer;
    Typ:      string[2];
    Id:       integer;
    Datum:    TDateTime;
    Zeit:     byte;
    Tag:      byte;
    Abrufe:   integer;
    Einmal:   boolean;
    constructor create(Ti: integer; Ty: string; I: integer; D:TDateTime;
                       Z, Ta: byte; A: integer; E: boolean);
  end;

  TTerminDb = class(TObject)
  private
    Datenpfad     : string[80];
    SystemPfad  : string[80];
    isOk:         boolean;
    function InitTable: boolean;
    function CreateTbTermin: boolean;
    function CheckForIndex: boolean;
    function RecordExists(GArt: string; GId: integer; Zeit, DoW: integer): integer;
    function GetGeraetRecordCount(GerArt: string; GerId: integer): integer;
    function GetGeraetFromTerminId(TerId: integer; var GerArt: string;
                                   var GerId: integer): boolean;
    function GetNextTerminFromRecord(Datum: TDateTime; Zeit, Tag: Integer; MinutenOffset: integer): TDateTime;
    procedure GetGeraeteFromGruppe(sl: TStringList; anId: integer);
  public
    tbTermin      : TTableExt;
    constructor Create(FilePath, SysPath: string);
    destructor Free;
    procedure SaveToTbTermin(GArt: string; GId: integer; Datum: TDateTime;
                             Zeit, DoW: integer; Datentypen: integer; Einmal: boolean; MsgOnOff: boolean);
    function SaveListToTbTermin(aList: TList; MsgOnOff: boolean): boolean;
    function UpdateListToTbTermin(aList: TList; MsgOnOff: boolean): boolean;
    procedure DeleteFromTbTermin(aTerminId: integer);
    function DeleteOneStation(GArt: string; GId: integer): boolean;
    function GetNextTermin(GArt: string; GId: integer; var DatumZeit: TDateTime;
                           var Datentypen: integer): boolean;
    procedure GetWeekList(var sl: TStringList; ErsterTag: TDateTime);
    procedure GetOneStationList(GArt: string; GId: integer; var sl: TStringList);
    procedure GetAbgleichList(var sl: TStringList);
    procedure GetTerminList(var sl: TStringList; Typ: string; Datum: TDateTime;
                            Tag, Stunde: byte);
    procedure GetTerminStationsList(var sl: TStringList);
    procedure GetTerminQuery(var q: TQuery; Datum: TDateTime; Tag, Stunde: byte);
    function EmptyTable: boolean;
    function GetTerminTypen(Datum: TDateTime; Tag, Stunde: byte): string;
  end;

function GetMrgGrpRecordCount(Pfad: string): integer;

implementation

{------------------------- Allgemeine Funktionen ------------------------------}

{ Anzahl der Einträge in der MRGGRP.DB               }
{----------------------------------------------------}
function GetMrgGrpRecordCount(Pfad: string): integer;
{----------------------------------------------------}
var
  q          : TQueryExt;
begin
  result:= -1; { default }
  if fileExists(Pfad + C_TbMrgGrp) then begin
    q:= TQueryExt.Create(nil);
    try
      q.DatabaseName:= Pfad;
      q.Sql.Add('SELECT COUNT(*) FROM "' + C_TbMrgGrp + '"');
      if q.open then result:= q.Fields[0].asInteger;
    except
      on exception do begin
        q.Free;
        if IsDebugFlag then showMessage('Termin - GetMrgGrpRecordCount');
        exit;
      end;
    end;
    q.Free;
  end;
end;

{------------------------------- TTerminRec -----------------------------------}

{----------------------------------------------------}
constructor TTerminRec.Create(Ti: integer; Ty: string; I: integer; D:TDateTime;
                       Z, Ta: byte; A: integer; E: boolean);
{----------------------------------------------------}
begin
  inherited create;
  TerminId:= Ti;
  Typ:=      Ty;
  Id:=       I;
  Datum:=    D;
  Zeit:=     Z;
  Tag:=      Ta;
  Abrufe:=   A;
  Einmal:=   E;
end;

{-------------------------------- TTerminDb -----------------------------------}

{----------------------------------------------------}
constructor TTerminDb.Create(FilePath, SysPath: string);
{----------------------------------------------------}
begin
  inherited create;
  { Pfad zu Systemdaten initialisieren }
  SystemPfad:= SysPath;
  { Pfad zur WTermin.Db initialisieren }
  Datenpfad:= FilePath;
  if Datenpfad[Length(Datenpfad)] <> '\' then Datenpfad:= Datenpfad + '\';
  { Vorhandensein der WTermin.db überprüfen }
  if InitTable then isOk:= true else isOk:= false;
end;

{----------------------------------------------------}
destructor TTerminDb.Free;
{----------------------------------------------------}
begin
  tbTermin.free;
  inherited destroy;
end;

{ Überprüft, ob die WTermin.db im Datenpfad vorhanden }
{ ist und fragt nach, ob sie angelegt werden soll    }
{----------------------------------------------------}
function TTerminDb.InitTable: boolean;
{----------------------------------------------------}
begin
  result:= false; { default }

  tbTermin:= TTableExt.create(nil);
  try
    tbTermin.DataBaseName:= Datenpfad;
    tbTermin.TableName:= C_TbTermin;
  except
    exit;
  end;

  if not FileExists(Datenpfad + C_TbTermin) then begin
    if not CreateTbTermin then exit;
  end;
  { Wenn die Procedure bis hier kommt, ist alles in Ordnung }
  result:= true;
end;

{ Überprüft, ob Sekundärindex 'SEintrag' vorhanden   }
{ Wenn nicht, wird er angelegt                       }
{ Rückgabe: True - er ist spätestens jetzt vorhanden }
{----------------------------------------------------}
function TTerminDb.CheckForIndex: boolean;
{----------------------------------------------------}
var
  i     : integer;
begin
  result:= False; { default }
  try
  { Überprüfen, ob Sekundärindex 'SEintrag' vorhanden ist }
    for i:= 0 to tbTermin.IndexDefs.Count - 1 do begin
      if tbTermin.IndexDefs.Items[i].Name = C_TbTerminIndex1 then begin
        result:= True;
        exit;
      end;
    end;
  { Sekundärindex 'SEintrag' wird neu angelegt }
    tbTermin.AddIndex(C_TbTerminIndex1,
    C_TfGeraeteArt + ';' + C_TfGeraeteId + ';' + C_TfZeit + ';' + C_TfWochentag,
    [ixCaseInsensitive]);
    result:= True;
  except
    exit;
  end;
end;

{----------------------------------------------------}
function TTerminDb.CreateTbTermin: boolean;
{----------------------------------------------------}
begin
  result:= true;
  { Tabelle neu anlegen }
  try
    with tbTermin do begin
      Active := False;
      with FieldDefs do begin
        Clear;
        Add(C_TfTerminId, ftInteger, 0, false);
        Add(C_TfGeraeteArt, ftString, 10, false);
        Add(C_TfGeraeteId, ftInteger, 0, false);
        Add(C_TfDatum, ftDate, 0, false);
        Add(C_TfZeit, ftSmallInt, 0, false);
        Add(C_TfWochentag, ftSmallInt, 0, false);
        Add(C_TFAbrufe, ftInteger, 0, false);
        Add(C_TfEinmalFlag, ftboolean, 0, false);
      end;
      with IndexDefs do begin
        Clear;
        Add('', C_TfTerminId, [ixPrimary, ixUnique]);
        Add(C_TbTerminIndex1,
           C_TfGeraeteArt + ';' + C_TfGeraeteId + ';' + C_TfZeit + ';' + C_TfWochentag,
           [ixCaseInsensitive]);
      end;
      CreateTable;
    end;
  except
    on exception do begin
    end;
  end;
end;

{ Löscht einen Eintrag aus der WTermin.db            }
{ Parameter: Termin-Id                               }
{----------------------------------------------------}
procedure TTerminDb.DeleteFromTbTermin(aTerminId: integer);
{----------------------------------------------------}
var
  mr: TModalResult;
  GId: integer;
  GArt: string;
  q: TQuery;
begin
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  mr:= mrYes;  { default }
  GetGeraetFromTerminId(aTerminId, GArt, GId);
  if GetGeraetRecordCount(GArt, GId) > 1 then
    mr:= MessageDlg('Nur diesen (Ja) oder alle Abruftermine dieser Station löschen ?',
                    mtConfirmation, [mbYes, mbAll, mbCancel], 0);
  if mr <> mrCancel then begin
    q:= TQuery.create(nil);
    try
      q.DataBaseName:= DatenPfad;
      q.Sql.Add('DELETE FROM ' + C_TbTermin);
      q.Sql.Add('WHERE');
      if mr = mrYes then begin
        q.Sql.Add(C_TfTerminId + '= :TId');
        q.ParamByName('TId').asInteger:= aTerminId;
      end
      else if mr = mrAll then begin
        q.Sql.Add(C_TfGeraeteArt + '= :GArt');
        q.Sql.Add('AND');
        q.Sql.Add(C_TfGeraeteId + '= :GId');
        q.ParamByName('GArt').asString:= GArt;
        q.ParamByName('GId').asInteger:= GId;
      end;
      q.ExecSql;
    finally
      q.free;
    end;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbTermin);
end;

{ Löscht eine Station aus der WTermin.db             }
{ Parameter: Termin-Id                               }
{----------------------------------------------------}
function TTerminDb.DeleteOneStation(GArt: string; GId: integer): boolean;
{----------------------------------------------------}
var
  q: TQueryExt;
begin
  result:= False; { default }
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  q:= TQueryExt.create(nil);
  try
    q.DataBaseName:= DatenPfad;
    q.Sql.Add('DELETE FROM ' + C_TbTermin);
    q.Sql.Add('WHERE');
    q.Sql.Add(C_TfGeraeteArt + '= :GArt');
    q.Sql.Add('AND');
    q.Sql.Add(C_TfGeraeteId + '= :GId');
    q.ParamByName('GArt').asString:= GArt;
    q.ParamByName('GId').asInteger:= GId;
    if q.ExecSql then result:= True;
  finally
    q.free;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbTermin);
end;

{ Speichert einen Termin mit den übergebene Para-    }
{ metern ab - überschreibt bestehende Abrufe         }
{ MsgOnOff = true: ProgressBar wird angezeigt        }
{----------------------------------------------------}
procedure TTerminDb.SaveToTbTermin(GArt: string; GId: integer; Datum: TDateTime;
                          Zeit, DoW: integer; Datentypen: integer; Einmal: boolean; MsgOnOff: boolean);
{----------------------------------------------------}
var
  i: integer;
begin
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  i:= RecordExists(GArt, GId, Zeit, DoW);
  { Wird eingetragen, wenn bisher kein entsprechender Eintrag existiert }
  if i = 0 then begin
    tbTermin.Open;
    try
      tbTermin.last;
      if tbTermin.RecordCount = 0 then
        i:= 0
      else
        i:= tbTermin.FieldByName(C_TfTerminId).asInteger;
      tbTermin.Append;
      tbTermin.FieldByName(C_TfTerminId).asInteger:= i+1;
      tbTermin.FieldByName(C_TfGeraeteArt).asString:= GArt;
      tbTermin.FieldByName(C_TfGeraeteId).asInteger:= GId;
  { Datum des ersten Abrufs mit Wochentag abgleichen }
      if (DoW < DayOfWeek(Datum)) then
        i:= 7 + DoW - DayOfWeek(Datum)
      else
        i:= DoW - DayOfWeek(Datum);
      tbTermin.FieldByName(C_TfDatum).asDateTime:= Datum + i;
      tbTermin.FieldByName(C_TfZeit).asInteger:= Zeit;
      tbTermin.FieldByName(C_TfWochentag).asInteger:= DoW;
      tbTermin.FieldByName(C_TFAbrufe).asInteger:= Datentypen;
      tbTermin.FieldByName(C_TfEinmalFlag).asBoolean:= Einmal;
      tbTermin.Post;
    finally
      tbTermin.Close;
    end;
  end
  { Aktion, falls bereits ein entsprechender Eintrag existiert }
  else begin
    tbTermin.Open;
    try
      tbTermin.FindKey([i]);
  { Eintrag wird ohne Rückfrage überschrieben }
      tbTermin.edit;
      if (DoW < DayOfWeek(Datum)) then
        i:= 7 + DoW - DayOfWeek(Datum)
      else
        i:= DoW - DayOfWeek(Datum);
      tbTermin.FieldByName(C_TfDatum).asDateTime:= Datum + i;
      tbTermin.FieldByName(C_TFAbrufe).asInteger:= Datentypen;
      tbTermin.post;
    finally
      tbTermin.Close;
    end;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbTermin);
end;

{ Speichert eine Termin-Liste mit den übergebenen    }
{ Parametern ab - überschreibt bestehende Abrufe     }
{ MsgOnOff = true: ProgressBar wird angezeigt        }
{------------------------------------------------------------------------------}
function TTerminDb.SaveListToTbTermin(aList: TList; MsgOnOff: boolean): boolean;
{------------------------------------------------------------------------------}
var
  i, j, dk, GId, aZeit, DoW, Datentypen: integer;
  GArt                                 : string;
  aDatum                               : TDateTime;
  aEinmal                              : boolean;
  oldIndex                             : string;
  FormProgressBar                      : TFormProgressBar;
begin
  result:= False;
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  { Procedure verlassen, wenn Index nicht initialisiert wurde }
  if not CheckForIndex then exit;
  if MsgOnOff then
    FormProgressBar:=TFormProgressBar.Create(nil, 'Abruftermine speichern', 0, aList.Count-1)
  else
    FormProgressBar:=nil;

  oldIndex:= tbTermin.IndexName;
  tbTermin.IndexName:= '';    { Primärindex }
  tbTermin.Open;              { Letzten Index für Fortschreibung merken }
  if tbTermin.RecordCount = 0 then
    i:= 0
  else begin
    tbTermin.Last;
    i:= tbTermin.FieldByName(C_TfTerminId).asInteger;
  end;
  tbTermin.Close;
  tbTermin.IndexName:= C_TbTerminIndex1;
  tbTermin.Open;
  try
    for j:= 0 to aList.Count-1 do begin
      if FormProgressBar <> nil then begin
        FormProgressBar.Step;
        Application.ProcessMessages;
      end;

      with TTerminRec(aList[j]) do begin
        GArt:= Typ;
        GId:= Id;
        aDatum:= Datum;
        aZeit:= Zeit;
        DoW:= Tag;
        Datentypen:= Abrufe;
        aEinmal:= Einmal;
      end;
  { Datum des ersten Abrufs mit Wochentag abgleichen }
      if (DoW < DayOfWeek(aDatum)) then
        dk:= 7 + DoW - DayOfWeek(aDatum)
      else
        dk:= DoW - DayOfWeek(aDatum);

  { Aktion, falls bereits ein entsprechender Eintrag existiert }
      if tbTermin.FindKey([GArt, GId, aZeit, DoW]) then begin
  { Eintrag wird ohne Rückfrage überschrieben }
        tbTermin.edit;
        tbTermin.FieldByName(C_TfDatum).asDateTime:= aDatum + dk;
        tbTermin.FieldByName(C_TFAbrufe).asInteger:= Datentypen;
        tbTermin.FieldByName(C_TfEinmalFlag).asBoolean:= aEinmal;
        tbTermin.post;
      end
  { Wird eingetragen, wenn bisher kein entsprechender Eintrag existiert }
      else begin
        tbTermin.last;
        inc(i);    { Index erhöhen }
        tbTermin.Append;
        tbTermin.FieldByName(C_TfTerminId).asInteger:= i;
        tbTermin.FieldByName(C_TfGeraeteArt).asString:= GArt;
        tbTermin.FieldByName(C_TfGeraeteId).asInteger:= GId;
        tbTermin.FieldByName(C_TfDatum).asDateTime:= aDatum + dk;
        tbTermin.FieldByName(C_TfZeit).asInteger:= aZeit;
        tbTermin.FieldByName(C_TfWochentag).asInteger:= DoW;
        tbTermin.FieldByName(C_TFAbrufe).asInteger:= Datentypen;
        tbTermin.FieldByName(C_TfEinmalFlag).asBoolean:= aEinmal;
        tbTermin.Post;
      end;
    end;  { for j:= 0 to aList.Count-1 }
  except
    tbTermin.Close;
    FormProgressBar.Free;
    exit;
  end;
  tbTermin.Close;
  FormProgressBar.Free;
  tbTermin.IndexName:= oldIndex;
  result:= True;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbTermin);
end;

{ Speichert eine Termin-Liste mit den übergebenen    }
{ Parametern ab - ergänzt Datentypen bei bestehende Abrufe     }
{ MsgOnOff = true: ProgressBar wird angezeigt        }
{------------------------------------------------------------------------------}
function TTerminDb.UpdateListToTbTermin(aList: TList; MsgOnOff: boolean): boolean;
{------------------------------------------------------------------------------}
var
  i, j, dk, GId, aZeit, DoW, Datentypen: integer;
  GArt                                 : string;
  aDatum                               : TDateTime;
  aEinmal                              : boolean;
  oldIndex                             : string;
  FormProgressBar                      : TFormProgressBar;
begin
  result:= False;
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  { Procedure verlassen, wenn Index nicht initialisiert wurde }
  if not CheckForIndex then exit;
  if MsgOnOff then
    FormProgressBar:=TFormProgressBar.Create(nil, 'Abruftermine speichern', 0, aList.Count-1)
  else
    FormProgressBar:=nil;

  oldIndex:= tbTermin.IndexName;
  tbTermin.IndexName:= '';    { Primärindex }
  tbTermin.Open;              { Letzten Index für Fortschreibung merken }
  if tbTermin.RecordCount = 0 then
    i:= 0
  else begin
    tbTermin.Last;
    i:= tbTermin.FieldByName(C_TfTerminId).asInteger;
  end;
  tbTermin.Close;
  tbTermin.IndexName:= C_TbTerminIndex1;
  tbTermin.Open;
  try
    for j:= 0 to aList.Count-1 do begin
      if FormProgressBar <> nil then begin
        FormProgressBar.Step;
        Application.ProcessMessages;
      end;

      with TTerminRec(aList[j]) do begin
        GArt:= Typ;
        GId:= Id;
        aDatum:= Datum;
        aZeit:= Zeit;
        DoW:= Tag;
        Datentypen:= Abrufe;
        aEinmal:= Einmal;
      end;
  { Datum des ersten Abrufs mit Wochentag abgleichen }
      if (DoW < DayOfWeek(aDatum)) then
        dk:= 7 + DoW - DayOfWeek(aDatum)
      else
        dk:= DoW - DayOfWeek(aDatum);

  { Aktion, falls bereits ein entsprechender Eintrag existiert }
      if tbTermin.FindKey([GArt, GId, aZeit, DoW]) then begin
  { Eintrag wird ohne Rückfrage überschrieben }
        tbTermin.edit;
        tbTermin.FieldByName(C_TfDatum).asDateTime:= aDatum + dk;
        tbTermin.FieldByName(C_TFAbrufe).asInteger:= Datentypen OR tbTermin.FieldByName(C_TFAbrufe).asInteger;
        tbTermin.FieldByName(C_TfEinmalFlag).asBoolean:= aEinmal;
        tbTermin.post;
      end
  { Wird eingetragen, wenn bisher kein entsprechender Eintrag existiert }
      else begin
        tbTermin.last;
        inc(i);    { Index erhöhen }
        tbTermin.Append;
        tbTermin.FieldByName(C_TfTerminId).asInteger:= i;
        tbTermin.FieldByName(C_TfGeraeteArt).asString:= GArt;
        tbTermin.FieldByName(C_TfGeraeteId).asInteger:= GId;
        tbTermin.FieldByName(C_TfDatum).asDateTime:= aDatum + dk;
        tbTermin.FieldByName(C_TfZeit).asInteger:= aZeit;
        tbTermin.FieldByName(C_TfWochentag).asInteger:= DoW;
        tbTermin.FieldByName(C_TFAbrufe).asInteger:= Datentypen;
        tbTermin.FieldByName(C_TfEinmalFlag).asBoolean:= aEinmal;
        tbTermin.Post;
      end;
    end;  { for j:= 0 to aList.Count-1 }
  except
    tbTermin.Close;
    FormProgressBar.Free;
    exit;
  end;
  tbTermin.Close;
  FormProgressBar.Free;
  tbTermin.IndexName:= oldIndex;
  result:= True;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbTermin);
end;

{ Feststellen, ob für Parameter bereits ein Eintrag  }
{ existiert                                          }
{ Rückgabe: 0 oder TerminId des Eintrags             }
{----------------------------------------------------}
function TTerminDb.RecordExists(GArt: string; GId: integer; Zeit, DoW: integer): integer;
{----------------------------------------------------}
var
  q: TQuery;
begin
  result:= 0; { default }
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  q:= TQuery.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT ' + C_TfTerminId + ',' + C_TfGeraeteArt + ',' +
        C_TfGeraeteId + ',' + C_TfZeit + ',' + C_TfWochentag + ' FROM ' + C_TbTermin);
    q.sql.add('WHERE ' + C_TfGeraeteArt + '= "' + GArt + '"');
    q.sql.add('AND ' + C_TfGeraeteId + '= ' + intToStr(GId));
    q.sql.add('AND ' + C_TfZeit + '= ' + intToStr(Zeit));
    q.sql.add('AND ' + C_TfWochentag + '= ' + intToStr(DoW));
    q.open;
    if q.recordCount = 0 then result:= 0
    else begin
      q.first;
      result:= q.FieldByName(C_TfTerminId).asInteger;
    end;
  finally
    q.free;
  end;
end;

{ Gibt die Anzahl der Einträge für ein Gerät zurück  }
{ Parameter: Geräteart, -Id                          }
{ Rückgabe: Anzahl der Einträge                      }
{----------------------------------------------------}
function TTerminDb.GetGeraetRecordCount(GerArt: string; GerId: integer): integer;
{----------------------------------------------------}
var
  q: TQuery;
begin
  q:= TQuery.create(nil);
  try
    q.DataBaseName:= DatenPfad;
    q.Sql.Add('SELECT * FROM ' + C_TbTermin);
    q.Sql.Add('WHERE ' + C_TfGeraeteArt + '= :GArt');
    q.Sql.Add('AND ' + C_TfGeraeteId + '= :GId');
    q.ParamByName('GArt').asString:= GerArt;
    q.ParamByName('GId').asInteger:= GerId;
    q.Open;
    result:= q.RecordCount;
  finally
    q.free;
  end;
end;

{ Gibt die Ident-Daten für ein Gerät zurück          }
{ Parameter: TerminId, Geräteart, -Id                }
{ Rückgabe: False: Gerät ist nicht eingetragen       }
{----------------------------------------------------}
function TTerminDb.GetGeraetFromTerminId(TerId: integer; var GerArt: string;
                               var GerId: integer): boolean;
{----------------------------------------------------}
var
  q: TQuery;
begin
  result:= false; { default }
  GerArt:= '';
  GerId:= 0;
  q:= TQuery.create(nil);
  try
    q.DataBaseName:= DatenPfad;
    q.Sql.Add('SELECT * FROM ' + C_TbTermin);
    q.Sql.Add('WHERE ' + C_TfTerminId + '= :TId');
    q.ParamByName('TId').asInteger:= TerId;
    q.Open;
    if q.RecordCount = 1 then begin
      GerArt:= q.FieldByName(C_TfGeraeteArt).asString;
      GerId:= q.FieldByName(C_TfGeraeteId).asInteger;
      result:= true;
    end;
  finally
    q.free;
  end;
end;

{ Holt für ein gegebenes Gerät die nächsten          }
{ Abrufparameter Zeitpunkt und Abrufart              }
{ Parameter: Geräteart, -Id, Rückgabe-Parameter Zeit }
{            Rückgabe-Parameter Abrufart             }
{ Rückgabe: False: es gibt keine Einträge            }
{----------------------------------------------------}
function TTerminDb.GetNextTermin(GArt: string; GId: integer;
              var DatumZeit: TDateTime; var Datentypen: integer): boolean;
{----------------------------------------------------}
var
  q         : TQueryExt;
  t         : TDateTime;
  SystemDaten: TSystemEinstellungen;
  MinutenOffset : integer;
  MinutenOffset_Gruppe : integer;
  isGruppentermin: boolean;

begin
  Result:= false; { default }

  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then begin
    DatumZeit := 0;  // 24.01.2001
    Result := True;  // 24.01.2001
    Exit;
  end;

  { Initialisieren der Rückgabewerte } // 24.01.2001 - auskommentiert
  DatumZeit:= now + 1000; { Mehr als 1000 Tage wird der nächste Abruf wohl nicht entfernt sein ... }
//  Datentypen:= 0;

  { Minutenoffset aus Systemdaten holen: }
  SystemDaten:= TSystemEinstellungen.Create(SystemPfad);        
  try
    if GArt = C_GerArtMRG then
      MinutenOffset:= SystemDaten.MRGMinOffset
    else if GArt = C_GerArtDSfG then
      MinutenOffset:= SystemDaten.DSfGMinOffset
    else
      MinutenOffset:= 0;
  finally
    SystemDaten.Free;
  end;
  isGruppentermin:=false;   // Vorbelegung: nächster Termin ist kein Gruppen-Termin

  q:= TQueryExt.create(nil);
  try
    q.DatabaseName:= DatenPfad;

    { Holen aller Termin-Einträge für die übergebene Station }
    q.sql.add('SELECT * FROM "' + C_TbTermin + '" A');
    q.sql.add('WHERE');
    q.sql.add('(A.' + C_TfGeraeteArt + '= :GerArt');
    q.sql.add('AND A.' + C_TfGeraeteId + '= :GerId)');
    q.ParamByName('GerArt').asString:= GArt;
    q.ParamByName('GerId').asInteger:= GId;
    if q.open then begin
      if q.recordCount > 0 then begin
        result:= true;
        q.first;
        while not q.eof do begin
          t:= GetNextTerminFromRecord(q.FieldByName(C_TfDatum).asDateTime,
                                      q.FieldByName(C_TfZeit).asInteger,
                                      q.FieldByName(C_TfWochentag).asInteger,
                                      MinutenOffset);
          if (t > 0) and (t < (DatumZeit)) then begin
            DatumZeit:= t;
            Datentypen:= q.FieldByName(C_TfAbrufe).asInteger;
          end;
          q.next;
        end;
      end;
    end;

    { nur MRG: zusätzlich holen aller Termin-Einträge für die Gruppen, in der die
      übergebene Station definiert ist }                 // 06.11.2001  GD/WW
    MinutenOffset_Gruppe:=0;
    if (GArt = C_GerArtMrg) and (GetMrgGrpRecordCount(DatenPfad) > 0) then begin
      q.Close;
      q.Sql.Clear;
      q.sql.add('SELECT A.*,');
      q.sql.add('C.'+ C_TfGrpOffset);
      q.sql.add('FROM "' + C_TbTermin + '" A,');
      q.sql.add('"' + C_TbMrgGrp + '" B,');
      q.sql.add('"' + C_TbGrpStamm + '" C');
      q.sql.add('WHERE');
      q.sql.add('(A.' + C_TfGeraeteArt + '= "' + C_GerArtGruppe + '"');
      q.sql.add('AND A.' + C_TfGeraeteId + '= B.' + C_TfGrpId);
      q.sql.add('AND C.' + C_TfGrpId + '= B.' + C_TfGrpId);
      q.sql.add('AND B.' + C_TfMrgId + '= :GerId)');
      q.ParamByName('GerId').asInteger:= GId;
      if q.open then begin
        if q.recordCount > 0 then begin
          result:=true;
          q.first;
          while not q.eof do begin
            t:= GetNextTerminFromRecord(q.FieldByName(C_TfDatum).asDateTime,
                                        q.FieldByName(C_TfZeit).asInteger,
                                        q.FieldByName(C_TfWochentag).asInteger,
                                        q.FieldByName(C_TfGrpOffset).asInteger);
            if (t > 0) and (t < (DatumZeit)) then begin
              DatumZeit:= t;
              Datentypen:= q.FieldByName(C_TfAbrufe).asInteger;
              isGruppentermin:=true;   // nächster Termin ist Gruppen-Termin
              MinutenOffset_Gruppe:=q.FieldByName(C_TfGrpOffset).asInteger;  // Offset der Gruppe merken
            end;
            q.next;
          end;
        end
      end;
    end;

    { Minutenoffset miteinrechnen: }
    if isGruppentermin then
      DatumZeit:= DatumZeit + EncodeTime(0, MinutenOffset_Gruppe, 0, 0)
    else
      DatumZeit:= DatumZeit + EncodeTime(0, MinutenOffset, 0, 0);
  except
    DatumZeit := 0;  // 24.01.2001
    Result := True;  // 24.01.2001
    q.Close;
    q.free;
    exit;
  end;
  q.free;
end;

{ Generiert aus einem Tabelleneintrag den tatsächlichen }
{ nächsten Abrufzeitpunkt                              }
{ Parameter: Datum erster Abruf, Stunde, Wochentag     }
{ Rückgabe: nächster Abruf                             }
{------------------------------------------------------}
function TTerminDb.GetNextTerminFromRecord
                     (Datum: TDateTime; Zeit, Tag: Integer; MinutenOffset: integer): TDateTime;
{------------------------------------------------------}
var
  h, min, sec, ms: word;
begin
  result:= trunc(now) + encodeTime(Zeit, 0, 0, 0) + 1000;{ Abruf in 1000 Tagen }
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  if now > Datum then begin { Zeitpunkt des 1. Abrufs ist bereits gewesen }
    decodeTime(now - EncodeTime(0, MinutenOffset, 0, 0), h, min, sec, ms);
  { Wochentag ist der gleiche wie heute }
    if (Tag = DayOfWeek(now)) then
      if h < Zeit then begin
  { Nächster Abruf ist heute - ist default }
        result:= result - 1000;
      end
      else begin
  { Nächster Abruf ist in einer Woche }
        result:= result - 1000 + 7;
      end
  { Wochentag ist noch in nächste Woche }
    else if (Tag < DayOfWeek(now)) then
      result:= result - 1000 + 7 + Tag - DayOfWeek(now)
  { Wochentag ist noch in dieser Woche }
    else if (Tag > DayOfWeek(now)) then
      result:= result - 1000 + Tag - DayOfWeek(now);
  end
  { Zeitpunkt des 1. Abrufs kommt noch }
  else result:= Datum + encodeTime(Zeit, 0, 0, 0);
end;

{ Gibt für eine gegebene Woche alle Abrufe in einer  }
{ Stringliste zurück; String: Typ, Tag und Zeit      }
{ Parameter: Übergabestringliste, 1. Tag             }
{----------------------------------------------------}
procedure TTerminDb.GetWeekList(var sl: TStringList; ErsterTag: TDateTime);
{----------------------------------------------------}
var
  q: TQuery;
  i: integer;
  rec: TTerminRec;
begin
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  { Reinitialisieren der Stringliste }
  if sl.Count > 0 then for i:= 0 to sl.Count-1 do TTerminRec(sl.objects[i]).free;
  sl.clear;
  sl.Sorted:= True;
  q:= TQuery.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT * FROM ' + C_TbTermin);
    q.sql.add('WHERE ' + C_TfDatum + '< :Datum');
    q.sql.add('ORDER BY ' + C_TfWochentag + ',' + C_TfZeit);
    q.ParamByName('Datum').asDate:= ErsterTag + 7;
    q.open;
    q.first;
    while not q.eof do begin
      rec:= TTerminRec.create(q.FieldByName(C_TfTerminId).asInteger,
                              q.FieldByName(C_TfGeraeteArt).asString,
                              q.FieldByName(C_TfGeraeteId).asInteger,
                              q.FieldByName(C_TfDatum).asDateTime,
                              q.FieldByName(C_TfZeit).asInteger,
                              q.FieldByName(C_TfWochentag).asInteger,
                              q.FieldByName(C_TFAbrufe).asInteger,
                              q.FieldByName(C_TfEinmalFlag).asBoolean);
      sl.addObject((rec.typ + ';' + IntToStr(rec.Tag) + ';' + IntToStr(rec.Zeit)),
                   rec);
      q.next;
    end;
  finally
    q.free;
  end;
end;

{ Gibt für eine Station alle Abrufe in einer         }
{ Stringliste zurück                                 }
{ Parameter: Übergabestringliste, 1. Tag             }
{----------------------------------------------------}
procedure TTerminDb.GetOneStationList(GArt: string; GId: integer;
                                      var sl: TStringList);
{----------------------------------------------------}
var
  q     : TQuery;
  i     : integer;
  rec   : TTerminRec;
begin
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  { Reinitialisieren der Stringliste }
  if sl.Count > 0 then for i:= 0 to sl.Count-1 do TTerminRec(sl.objects[i]).free;
  sl.clear;
  sl.Sorted:= True;
  q:= TQuery.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT * FROM ' + C_TbTermin);
    q.sql.add('WHERE');
    q.sql.add(C_TfGeraeteArt + '= :GerArt');
    q.sql.add('AND ' + C_TfGeraeteId + '= :GerId');
    q.sql.add('ORDER BY ' + C_TfWochentag + ',' + C_TfZeit);
    q.ParamByName('GerArt').asString:= GArt;
    q.ParamByName('GerId').asInteger:= GId;
    q.open;
    q.first;
    while not q.eof do begin
      rec:= TTerminRec.create(q.FieldByName(C_TfTerminId).asInteger,
                              q.FieldByName(C_TfGeraeteArt).asString,
                              q.FieldByName(C_TfGeraeteId).asInteger,
                              q.FieldByName(C_TfDatum).asDateTime,
                              q.FieldByName(C_TfZeit).asInteger,
                              q.FieldByName(C_TfWochentag).asInteger,
                              q.FieldByName(C_TFAbrufe).asInteger,
                              q.FieldByName(C_TfEinmalFlag).asBoolean);
      sl.addObject((rec.typ + ';' + IntToStr(rec.Tag) + ';' + IntToStr(rec.Zeit)),
                   rec);
      q.next;
    end;
  finally
    q.free;
  end;
end;

{ Gibt für einen Abgleich alle Abrufe in einer       }
{ Stringliste zurück                                 }
{ Parameter: Übergabestringliste                     }
{----------------------------------------------------}
procedure TTerminDb.GetAbgleichList(var sl: TStringList);
{----------------------------------------------------}
var
  q           : TQuery;
  i, j, k, p  : integer;
  theId       : integer;
  theArt      : string;
  s, sh       : string;
  rec         : TTerminRec;
  dt          : TDateTime;
  aSl         : TStringList;
  bc0, bc1    : integer;
  SystemDaten: TSystemEinstellungen;
  MinutenOffsetMRG: integer;
  MinutenOffsetDSfG: integer;
  MinutenOffsetGruppe: integer;
  FormProgressBar: TFormProgressBar;
  DtSavePlace : TDateTime;  // 08.11.2001
begin
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  { Reinitialisieren der Stringliste }
  try
    if sl.Count > 0 then
      for i:= 0 to sl.Count-1 do TTerminRec(sl.objects[i]).free;
  except
  // tue nix
  end;
  sl.clear;
  q:= TQuery.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT * FROM "' + C_TbTermin + '" A');
    q.Sql.Add('LEFT JOIN "' + C_TbGrpStamm + '" B');
    q.Sql.Add('ON ((A.' + C_TfGeraeteId + '= B.' + C_TfGrpId + ') AND');
    q.Sql.Add('    (A.' + C_TfGeraeteArt + ' = "' + C_GerArtGruppe + '"))');
    q.open;
    if q.RecordCount > 0 then begin
      q.first;
      FormProgressBar:=TFormProgressBar.Create(nil, 'Abruftermine einlesen', 1, 100);
      try
        bc0:= 0;  { Counter für den Fortschritt des Progressbars }
        bc1:= q.RecordCount div 100; { Intervall für den Fortschritt des Progressbars }
        if (bc1 = 0) then bc1:= 1;

        { Minutenoffsets für jede Geräteart aus Systemdaten holen: }
        SystemDaten:= TSystemEinstellungen.Create(SystemPfad);
        try
          MinutenOffsetMRG := SystemDaten.MRGMinOffset;
          MinutenOffsetDSfG:= SystemDaten.DSfGMinOffset;
        finally
          SystemDaten.Free;
        end;

        while not q.eof do begin
          { Minutenoffset für jede Geräteart außer 'Gruppe' einrechnen: }
          if q.FieldByName(C_TfGeraeteArt).asString = C_GerArtMRG then begin
            dt:= GetNextTerminFromRecord(q.FieldByName(C_TfDatum).asDateTime,
                                         q.FieldByName(C_TfZeit).asInteger,
                                         q.FieldByName(C_TfWochentag).asInteger, MinutenOffsetMRG);
            dt:=dt + EncodeTime(0, MinutenOffsetMRG, 0, 0);
          end
          else if q.FieldByName(C_TfGeraeteArt).asString = C_GerArtDSfG then begin     
            dt:= GetNextTerminFromRecord(q.FieldByName(C_TfDatum).asDateTime,
                                         q.FieldByName(C_TfZeit).asInteger,
                                         q.FieldByName(C_TfWochentag).asInteger, MinutenOffsetDSfG);
            dt:=dt + EncodeTime(0, MinutenOffsetDSfG, 0, 0);
          end
          else begin
            { Gruppenoffset aus Query über WTermin.db und GRP.DB: }
            MinutenOffsetGruppe:=q.FieldByName(C_TfGrpOffset).asInteger;
            dt:= GetNextTerminFromRecord(q.FieldByName(C_TfDatum).asDateTime,
                                         q.FieldByName(C_TfZeit).asInteger,
                                         q.FieldByName(C_TfWochentag).asInteger, MinutenOffsetGruppe);
          end;

          if (q.FieldByName(C_TfGeraeteArt).asString = C_GerArtGruppe) then begin
  { Gruppe wird zerlegt }
            aSl:= TStringList.Create;
            GetGeraeteFromGruppe(aSl, q.FieldByName(C_TfGeraeteId).asInteger);
            for i:= 0 to aSl.Count-1 do begin
              DtSavePlace := dt;  // 08.11.2001
              p:= Pos(';', aSl[i]);                      { Trennzeichen }
              if (p = 0) then break;
              s:= copy(aSl[i], 1, p-1);                  { GeräteArt }
              j:= StrToIntDef(copy(aSl[i], p+1, Length(aSl[i])), -1); { GeräteId }
              if (j <> -1) then begin
                { Gruppenoffset aus Query über WTermin.db und GRP.DB: }
                MinutenOffsetGruppe:=q.FieldByName(C_TfGrpOffset).asInteger;
                DtSavePlace := DtSavePlace + EncodeTime(0, MinutenOffsetGruppe, 0, 0);

                sh:= s + IntToStr(j);
                k:= sl.IndexOf(sh);
                if (k > -1) then begin
                  if (TTerminRec(sl.Objects[k]).Datum > DtSavePlace) then begin
                    TTerminRec(sl.Objects[k]).Datum:= DtSavePlace;
                    TTerminRec(sl.Objects[k]).Abrufe:=
                      q.FieldByName(C_TFAbrufe).asInteger;
                    TTerminRec(sl.Objects[k]).Einmal:=
                      q.FieldByName(C_TfEinmalFlag).asBoolean;
                  end;
                end
                else begin
                  rec:= TTerminRec.create(0, s, j, DtSavePlace, 0, 0,
                                          q.FieldByName(C_TFAbrufe).asInteger,
                                          q.FieldByName(C_TfEinmalFlag).asBoolean);
                  sl.addObject(rec.typ + IntToStr(rec.Id), rec);
                end;
              end;
            end;
            aSl.Free;
          end
          else begin
  { Einzelstationen }
            theId:= q.FieldByName(C_TfGeraeteId).asInteger;
            theArt:= q.FieldByName(C_TfGeraeteArt).asString;
            sh:= theArt + IntToStr(theId);
            k:= sl.IndexOf(sh);
            if (k > -1) then begin
              if TTerminRec(sl.Objects[k]).Datum > dt then begin
                TTerminRec(sl.Objects[k]).Datum:= dt;
                TTerminRec(sl.Objects[k]).Abrufe:=
                  q.FieldByName(C_TFAbrufe).asInteger;
                TTerminRec(sl.Objects[k]).Einmal:=
                  q.FieldByName(C_TfEinmalFlag).asBoolean;
              end;
            end
            else begin
              rec:= TTerminRec.create(0, theArt, theId, dt, 0, 0,
                                      q.FieldByName(C_TFAbrufe).asInteger,
                                      q.FieldByName(C_TfEinmalFlag).asBoolean);
              sl.addObject(rec.typ + IntToStr(rec.Id), rec);
            end;
          end;
          q.next;
          inc(bc0);
          if (bc0 = bc1) then begin
            bc0:= 0;
            FormProgressBar.Step;
            Application.ProcessMessages;
          end;
        end;  { while not q.eof }
      finally
        FormProgressBar.Free;
      end;
    end; { if q.RecordCount > 0 }
  finally
    q.free;
  end;
end;

{ Gibt alle Geräte einer Gruppe in einer Stringliste  }
{ zurück; Format: <Art> ';' <Id>                      }
{ Parameter: Übergabestringliste, GruppenId           }
{-----------------------------------------------------}
procedure TTerminDb.GetGeraeteFromGruppe(sl: TStringList; anId: integer);
{-----------------------------------------------------}
var
  q       : TQuery;
  s       : string;
begin
  q:= TQuery.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT * FROM ' + C_TbMrgGrp);
    q.sql.add('WHERE ' + C_TfGrpId + ' = ' + IntToStr(anId));
    q.open;
    q.first;
    while not q.eof do begin
      s:= C_GerArtMrg + ';';   { Geräte sind bisher nur MRG's }
      s:= s + IntToStr(q.FieldByName(C_TfMrgId).asInteger);
      sl.Add(s);
      q.Next;
    end;
  except
    q.free;
    exit;
  end;
  q.free;
end;

{ Gibt für einen gegebenen Termin alle Abrufe in      }
{ einer Stringliste zurück                            }
{ Parameter: Übergabestringliste, Gerätetyp, Termin   }
{            -Datum, -Wochenag, -Stunde               }
{-----------------------------------------------------}
procedure TTerminDb.GetTerminList(var sl: TStringList; Typ: string;
                                  Datum: TDateTime; Tag, Stunde: byte);
{-----------------------------------------------------}
var
  q: TQuery;
  i: integer;
  rec: TTerminRec;
begin
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  { Reinitialisieren der Stringliste }
  if sl.Count > 0 then for i:= 0 to sl.Count-1 do TTerminRec(sl.objects[i]).free;
  sl.clear;
  q:= TQuery.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT * FROM ' + C_TbTermin);
    q.sql.add('WHERE ' + C_TfDatum + '<= :Datum');
    q.sql.add('AND ' + C_TfGeraeteArt + '= "' + Typ + '"');
    q.sql.add('AND ' + C_TfWochentag + '= ' + intToStr(Tag));
    q.sql.add('AND ' + C_TfZeit + '= ' + intToStr(Stunde));
    q.sql.add('ORDER BY ' + C_TfGeraeteArt);
    q.ParamByName('Datum').asDate:= Datum;
    q.open;
    q.first;
    while not q.eof do begin
      rec:= TTerminRec.create(q.FieldByName(C_TfTerminId).asInteger,
                              q.FieldByName(C_TfGeraeteArt).asString,
                              q.FieldByName(C_TfGeraeteId).asInteger,
                              q.FieldByName(C_TfDatum).asDateTime,
                              q.FieldByName(C_TfZeit).asInteger,
                              q.FieldByName(C_TfWochentag).asInteger,
                              q.FieldByName(C_TFAbrufe).asInteger,
                              q.FieldByName(C_TfEinmalFlag).asBoolean);
      sl.addObject(rec.typ, rec);
      q.next;
    end;
  finally
    q.free;
  end;
end;

{-----------------------------------------------------}
procedure TTerminDb.GetTerminStationsList(var sl: TStringList);
{-----------------------------------------------------}
var
  q: TQuery;
  i: integer;
  rec: TTerminRec;
begin
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  { Reinitialisieren der Stringliste }
  if sl.Count > 0 then for i:= 0 to sl.Count-1 do TTerminRec(sl.objects[i]).free;
  sl.clear;
  q:= TQuery.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT DISTINCT ' + C_TfGeraeteArt + ',' + C_TfGeraeteId);
    q.Sql.Add('FROM ' + C_TbTermin);
    q.open;
    q.first;
    while not q.eof do begin
      rec:= TTerminRec.create(0,
                              q.FieldByName(C_TfGeraeteArt).asString,
                              q.FieldByName(C_TfGeraeteId).asInteger,
                              0, 0, 0, 0, True);
      sl.addObject(GetStationsName(rec.Id, rec.Typ), rec);
      q.next;
    end;
  finally
    q.free;
  end;
end;

{ Leert die WTermin.Db vollständig                    }
{-----------------------------------------------------}
function TTerminDb.EmptyTable: boolean;
{-----------------------------------------------------}
begin
  try
    with TbTermin do begin
      Active := False;
      result:= EmptyTable;
    end;
  except
    result:= False;
    exit;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbTermin);
end;

{ Gibt für einen gegebenen Termin alle Abrufe in      }
{ einem Query zurück                                  }
{ Parameter: ÜbergabeQuery, Gerätetyp, Termin         }
{            -Datum, -Wochenag, -Stunde               }
{-----------------------------------------------------}
procedure TTerminDb.GetTerminQuery(var q: TQuery; Datum: TDateTime; Tag, Stunde: byte);
{-----------------------------------------------------}
begin
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  { Reinitialisieren des Queries }
  try
    q.close;
    q.sql.clear;
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT * FROM ' + C_TbTermin);
    q.sql.add('WHERE ' + C_TfDatum + '<= :Datum');
    q.sql.add('AND ' + C_TfWochentag + '= ' + intToStr(Tag));
    q.sql.add('AND ' + C_TfZeit + '= ' + intToStr(Stunde));
    q.sql.add('ORDER BY ' + C_TfGeraeteArt);
    q.ParamByName('Datum').asDate:= Datum;
    q.open;
  except
  end;
end;

{ Gibt für einen gegebenen Termin alle Typen zurück   }
{ Parameter: Termin-Datum, -Wochentag, -Stunde        }
{ Rückgabewert: string mit Typen                      }
{-----------------------------------------------------}
function TTerminDb.GetTerminTypen(Datum: TDateTime; Tag, Stunde: byte): string;
{-----------------------------------------------------}
var
  q: TQuery;
begin
  result:= '';
  { Procedure verlassen, wenn Tabelle nicht initialisiert wurde }
  if not isOk then exit;
  q:= TQuery.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT * FROM ' + C_TbTermin);
    q.sql.add('WHERE ' + C_TfDatum + '<= :Datum');
    q.sql.add('AND ' + C_TfWochentag + '= ' + intToStr(Tag));
    q.sql.add('AND ' + C_TfZeit + '= ' + intToStr(Stunde));
    q.ParamByName('Datum').asDate:= Datum;
    q.open;
    q.first;
    while not q.eof do begin
      if pos(q.FieldByName(C_TfGeraeteArt).asString, result) = 0
        then result:= Result + q.FieldByName(C_TfGeraeteArt).asString;
      q.next;
    end;
  finally
    q.free;
  end;
end;

end.

