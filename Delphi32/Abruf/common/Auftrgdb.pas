{------------------------------------------------------------------------------}
{ 11.12.1998 GD; Unit für die Verwaltung der WAuftrag.Db                       }
{                                                                              }
{          -> Greift auf Konstanten aus WSysCon.pas zu                         }
{          -> Greift auf Funktionen aus My_Utils.pas zu                        }
{          -> Verwendet die Db_Attn.pas zum Schreiben von 'Trigger'-Files      }
{                                                                              }
{ 09.04.1999 GD; Beschleunigung                                                }
{ 22.04.1999 GD; Alle Schreibfuktionen auf WTableExt umgestellt                }                                                
{                                                                              }
{  (C) Karl Wieser GmbH 1998, 1999                                             }
{------------------------------------------------------------------------------}
unit AuftrgDb;

interface

uses
  Classes, Forms, db, dbtables, SysUtils, Windows, dialogs, controls, TerminDb,
  WSysCon, Db_Attn, WTables, FPrgBar, My_Utils;

const
  { Tabelle der Abrufaufträge }
  C_TbAuftrag        = 'WAuftrag.db';

  C_TfAGeraeteId     = 'GeraeteId';       { Id des Gerätes }
  C_TfAGeraeteArt    = 'GeraeteArt';      { Art des Gerätes }
  C_TfADatumZeit     = 'Startzeit';       { Zeitpunkt des nächsten Abrufs }
  C_TFAPrioritaet    = 'Prioritaet';      { Priorität der Abrufe }
  C_TFAAbrufe        = 'Datentypen';      { Art der Abrufe als Status }
  C_TFAAbrufArt      = 'Abrufart';        { Automatisch, manuell }
  C_TFADurchlaufNr   = 'Anrufversuch';    { Wahlwiederholungszähler }
  C_TFALogport       = 'Schnittstelle';   { Comport - 0. beliebig }
  C_TFABelegt        = 'Belegt';          { True: Abruf findet gerade statt }
  C_TFAAktiv         = 'Aktiv'; { Aktiviert in der Termin.db vorhandene Aufträge }
  C_TfAVonDatum      = 'DatenVon';         { Von-Zeitpunkt des Daten-Abrufs }
  C_TfABisDatum      = 'DatenBis';        { Bis-Zeitpunkt des DatenAbrufs }

  C_MyLogFile        = 'DebugLog.txt';

type

  TAuftragRec = class(TObject)
    GerId:       integer;
    GerArt:      string[10];
    DatumZeit:   TDateTime;
    Prioritaet:  SmallInt;
    DatenTypen:  integer;
    Abrufart:    string[10];
    Durchlaeufe: integer;
    LogPort:     SmallInt;
    Belegt:      boolean;
    Aktiv:       boolean;
    constructor create(Gi: integer; Ga: string; Dz:TDateTime; Pr: SmallInt;
                       dt, Dl: Integer; AbrArt: string; Lp: SmallInt = 0;
                       Be: boolean = false; Av: boolean = true);
  end;

  TAuftragDb = class(TObject)
    Datenpfad   : string[80];
    SystemPfad  : string[80];
    tbAuftrag   : TTableExt;
  private
    isOk:         boolean;
    function InitTable: boolean;
    function CreateTbAuftrag: boolean;
    function EmptyTable: boolean;
    function GetPrioritaet(anId: integer; Typ: string): SmallInt;
    function GetLogPort(anId: integer; Typ: string): SmallInt;
    procedure GetInaktiveAuftraege (InaktivList: TStringList);
  public
    constructor Create(FilePath, SysPath: string);
    destructor destroy; override;
    procedure InitAuftragsTable (InaktivList: TStringList = nil);
    procedure NewAuftragsTable;
    procedure NewAuftragsTableWithAktivStati;
    procedure ResetAutomatikAuftraege;
    procedure setAktiv(anArt: string; anId: integer; anAbrArt: string; Value: boolean);
    procedure setBelegt(anArt: string; anId: integer; anAbrArt: string; Value: boolean);
    procedure setToNow(anArt: string; anId: integer; anAbrArt: string);
    procedure setAllToNow;
    function GetBelegt(anArt: string; anId: integer; anAbrArt: string): boolean;
    function GetAnzahlBelegt: integer;
    function GetRecordCount: integer;
    procedure deleteAuftrag(anArt: string; anId: integer; anAbrArt: string = C_AbrArtManu);
    function DeleteAuftraege (Geraeteart: string; GeraeteId: integer): boolean;
    function updateAuftrag(anArt: string; anId: integer; anAbrArt: string;
                            aDT: TDateTime; aDaten: integer; aAnrufz: integer = 0;
                            anAktivStatus: boolean = true): boolean;
    function appendNeuenAuftrag(anArt: string; anId: integer;
                                 aDT: TDateTime; aDaten: integer;
                                 aPrioritaet: smallInt;
                                 anAbrArt: string = C_AbrArtAuto;
                                 VonZeit: TDateTime = 0;
                                 BisZeit: TdateTime = 0): boolean;
    function aktuelleAuftraege(var aQuery: TQueryExt): boolean;
    function CheckNeuenAuftrag(anArt: string; anId: integer; anAbrArt: string): boolean;
  end;

procedure writeLogFile(s: string);

implementation


{------------------------ Allgemeine Funktionen -------------------------------}

{----------------------------------------------------}
procedure writeLogFile(s: string);
{----------------------------------------------------}
{$IFDEF ISLOGFILE}
var
  f: TextFile;
{$ENDIF}
begin
{$IFDEF ISLOGFILE}
  AssignFile(f, extractFilePath(paramStr(0)) + C_MyLogFile);
  {$I-}
  Reset(F);
  {$I+}
  if ioResult <> 0 then rewrite(f);
  append(f);
  writeln(f, DateTimeToStr(now));
  writeln(f, s);
  closeFile(f);
{$ENDIF}
end;


{-------------------------------- TAuftragRec ---------------------------------}

{----------------------------------------------------}
constructor TAuftragRec.create(Gi: integer; Ga: string; Dz:TDateTime; Pr: SmallInt;
                       Dt, Dl: Integer; AbrArt: string; Lp: SmallInt = 0;
                       Be: boolean = false; Av: boolean = true);
{----------------------------------------------------}
begin
  inherited create;
  GerId           := Gi;
  GerArt          := Ga;
  DatumZeit       := Dz;
  Prioritaet      := Pr;
  DatenTypen      := Dt;
  AbrufArt        := AbrArt;
  Durchlaeufe     := Dl;
  LogPort         := Lp;
  Belegt          := Be;
  Aktiv           := Av;
end;

{-------------------------------- TAuftragDb ----------------------------------}

{----------------------------------------------------}
constructor TAuftragDb.Create(FilePath, SysPath: string);
{----------------------------------------------------}
begin
  inherited create;
  { Pfad zu Systemdaten initialisieren }
  SystemPfad:= SysPath;
  { Pfad zur WAuftrag.Db initialisieren }
  Datenpfad:= FilePath;
  if Datenpfad[Length(Datenpfad)] <> '\' then Datenpfad:= Datenpfad + '\';
  { Vorhandensein der WAuftrag.db überprüfen }
  if InitTable then isOk:= true else isOk:= false;
  { Variablen initialisieren }
end;

{----------------------------------------------------}
destructor TAuftragDb.destroy;
{----------------------------------------------------}
begin
  tbAuftrag.free;
  inherited destroy;
end;

{ Überprüft, ob die WAuftrag.db im Pfad vorhanden    }
{ ist und fragt nach, ob sie angelegt werden soll    }
{----------------------------------------------------}
function TAuftragDb.InitTable: boolean;
{----------------------------------------------------}
begin
  result:= false; { default }

  tbAuftrag:= TTableExt.create(nil);
  try
    tbAuftrag.DataBaseName:= Datenpfad;
    tbAuftrag.TableName:= C_TbAuftrag;
  except
    writeLogFile('FEHLER - Konnte Tabellen-Parameter nicht einstellen !');
    exit;
  end;

  if not FileExists(Datenpfad + C_TbAuftrag) then begin
    if not CreateTbAuftrag then exit;
  end;
  { Wenn die Procedure bis hier kommt, ist alles in Ordnung }
  result:= true;
end;

{ Neuanlegen der WAUFTRAG.DB                         }
{----------------------------------------------------}
function TAuftragDb.CreateTbAuftrag: boolean;
{----------------------------------------------------}
begin
  result:= true;
  { Tabelle neu anlegen }
  try
    with tbAuftrag do begin
      Active := False;
      with FieldDefs do begin
        Clear;
        Add(C_TfAGeraeteId, ftInteger, 0, false);
        Add(C_TfAGeraeteArt, ftString, 10, false);
        Add(C_TFAAbrufArt, ftString, 10, false);
        Add(C_TfADatumZeit, ftDateTime, 0, false);
        Add(C_TFAPrioritaet, ftSmallInt, 0, false);
        Add(C_TFAAbrufe, ftInteger, 0, false);
        Add(C_TFADurchlaufNr, ftSmallInt, 0, false);
        Add(C_TFALogport, ftSmallInt, 0, false);
        Add(C_TFABelegt, ftboolean, 0, false);
        Add(C_TFAAktiv, ftboolean, 0, false);
        Add(C_TfAVonDatum, ftDateTime, 0, false);
        Add(C_TfABisDatum, ftDateTime, 0, false);
      end;
      with IndexDefs do begin
        Clear;
        Add('', (C_TfAGeraeteId+';'+C_TfAGeraeteArt+';'+C_TFAAbrufArt) , [ixPrimary, ixUnique]);
      end;
      CreateTable;
    end;
  except
    result:= false;
    exit;
  end;
end;

{ Leert die WAuftrag.Db vollständig                   }
{-----------------------------------------------------}
function TAuftragDb.EmptyTable: boolean;
{-----------------------------------------------------}
begin
  try
    with TbAuftrag do begin
      Active := False;
      result:= EmptyTable;
    end;
  except
    result:= False;
    exit;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{----------------------------------------------------}
function TAuftragDb.GetPrioritaet(anId: integer; Typ: string): SmallInt;
{----------------------------------------------------}
var
  q: TQueryExt;
begin
  result:= 0; { default }
  q:= TQueryExt.create(nil);
  try
    if Pos(C_GerArtMrg, Typ) > 0 then begin
    { Typ ist MRG }
      q.DatabaseName:= Datenpfad;
      q.sql.add('SELECT ' + C_Sta_MrgId + ',' + C_Sta_Prioritaet + ',' + C_Sta_Aktiv);
      q.sql.add('FROM ' + CDBSta);
      q.sql.add('WHERE ' + C_Sta_MrgId + '= :MRGId');
      q.sql.add('AND ' + C_Sta_Aktiv + '= :Aktiv');
      q.ParamByName('MRGId').asInteger:= anId;
      q.ParamByName('Aktiv').asInteger:= 0;
      if q.open then begin
        if (q.RecordCount = 1) and (not q.FieldByName(C_Sta_Prioritaet).isNull)
          then result:= q.FieldByName(C_Sta_Prioritaet).asInteger;
      end;
    end
    else if Pos(C_GerArtGruppe, Typ) > 0 then begin
    { Typ ist GRUPPE }
      q.DatabaseName:= Datenpfad;
      q.sql.add('SELECT ' + C_TfGrpPrioritaet + ',' + C_TfGrpId);
      q.sql.add('FROM ' + C_TbGrpStamm);
      q.sql.add('WHERE ' + C_TfGrpId + '= :GrpId');
      q.ParamByName('GrpId').asInteger:= anId;
      if q.open then begin
        if (q.RecordCount = 1) and (not q.FieldByName(C_TfGrpPrioritaet).isNull)
          then result:= q.FieldByName(C_TfGrpPrioritaet).asInteger;
      end;
    end
    else if Pos(C_GerArtDSfG, Typ) > 0 then begin
    { Typ ist DSfG }
      q.DatabaseName:= Datenpfad;
      q.sql.add('SELECT ' + C_DTF_Station_Prioritaet + ',' + C_DTF_Station_StationId);
      q.sql.add('FROM ' + C_DTB_Station);
      q.sql.add('WHERE ' + C_DTF_Station_StationId + '= :StationId');
      q.ParamByName('StationId').asInteger:= anId;
      if q.open then begin
        if (q.RecordCount = 1) and (not q.FieldByName(C_DTF_Station_Prioritaet).isNull) then
          result:= q.FieldByName(C_DTF_Station_Prioritaet).asInteger;
      end;
    end;
  finally
    q.free;
  end;
end;

{----------------------------------------------------}
function TAuftragDb.GetLogPort(anId: integer; Typ: string): SmallInt;
{----------------------------------------------------}
var
  q: TQueryExt;
begin
  result:= 0; { default }
  q:= TQueryExt.create(nil);
  try
    if Pos(C_GerArtMrg, Typ) > 0 then begin
    { Typ ist MRG }
      q.DatabaseName:= Datenpfad;
      q.sql.add('SELECT ' + C_StaDfu_MrgId + ',' + C_StaDfu_LogPort);
      q.sql.add('FROM ' + CDBStaDfu);
      q.sql.add('WHERE ' + C_StaDfu_MrgId + '= :MRGId');
      q.ParamByName('MRGId').asInteger:= anId;
      if q.open then begin
        if (q.RecordCount = 1) and (not q.FieldByName(C_StaDfu_LogPort).isNull)
          then result:= q.FieldByName(C_StaDfu_LogPort).asInteger;
      end;
    end
    else if Pos(C_GerArtGruppe, Typ) > 0 then begin
    { Typ ist GRUPPE }
      result:= 0; { ist bei Gruppen immer '0' }
    end
    else if Pos(C_GerArtDSfG, Typ) > 0 then begin
    { Typ ist DSfG }
      q.DatabaseName:= Datenpfad;
      q.sql.Add('SELECT C.' + C_DTF_InstDfu_LogPort);
      q.sql.Add('FROM "' + C_DTB_Station + '" A,');
      q.sql.Add('"' + C_DTB_Instanz + '" B,');
      q.sql.Add('"' + C_DTB_InstDfu + '" C');
      q.Sql.Add('WHERE C.' + C_DTF_InstDfu_InstanzId + ' = B.' + C_DTF_Instanz_InstanzId + ' AND ');
      q.sql.add('A.' + C_DTF_Station_StationId + '= B.' + C_DTF_Instanz_StationId + ' AND ');
      q.sql.add('A."' + C_DTF_Station_LIInstanz + '"= B.' + C_DTF_Instanz_Instanzname + ' AND ');
      q.sql.add('A.' + C_DTF_Station_StationId + '= :StationId');
      q.ParamByName('StationId').asInteger:= anId;
      if q.open then begin
        if (q.RecordCount = 1) and (not q.FieldByName(C_DTF_InstDfu_LogPort).isNull)
          then result:= q.FieldByName(C_DTF_InstDfu_LogPort).asInteger;
      end;
    end;
  finally
    q.free;
  end;
end;

{ Gibt den Belegt-Status eines Auftrags zurück       }
{ Parameter: Art, Id, Abrufart zur Identifizierung   }
{ Ergebnis: TRUE - Belegt, FALSE - nicht belegt      }
{----------------------------------------------------}
function TAuftragDb.GetBelegt(anArt: string; anId: integer; anAbrArt: string): boolean;
{----------------------------------------------------}
var
  q: TQueryExt;
begin
  result:= false; { default }
  q:= TQueryExt.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT ' + C_TFABelegt + ' FROM "' + C_TbAuftrag + '"');
    q.sql.add('WHERE ' + C_TfAGeraeteArt + '= :GArt');
    q.sql.add('AND ' + C_TfAGeraeteId + '= :GId');
    q.sql.add('AND ' + C_TFAAbrufArt + '= :GAbrArt');
    q.ParamByName('GArt').asString:= anArt;
    q.ParamByName('GId').asInteger:= anId;
    q.ParamByName('GAbrArt').asString:= anAbrArt;
    if q.open then begin
      if q.RecordCount > 0 then result:= q.FieldByName(C_TFABelegt).asBoolean;
    end;
  finally
    q.free;
  end;
end;

{ Gibt die Anzahl der z.Zt. belegten Aufträge aus    }
{ der WAUFTRAG.DB zurück                             }
{----------------------------------------------------}
function TAuftragDb.GetAnzahlBelegt: integer;
{----------------------------------------------------}
var
  q: TQueryExt;
begin
  result:= 0; { default }
  q:= TQueryExt.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT COUNT(' + C_TfAGeraeteId + ') FROM "' + DatenPfad + C_TbAuftrag + '"');
    q.sql.add('WHERE ' + C_TfABelegt + '= :Belegt');
    q.ParamByName('Belegt').asBoolean:= true;
    if q.open then result:= q.Fields[0].asInteger;
  finally
    q.free;
  end;
end;

{ Gibt die Anzahl der Einträge aus der WAUFTRAG.DB    }
{ zurück                                             }
{----------------------------------------------------}
function TAuftragDb.GetRecordCount: integer;
{----------------------------------------------------}
var
  q: TQueryExt;
begin
  result:= 0; { default }
  q:= TQueryExt.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT COUNT(' + C_TfAGeraeteId + ') FROM "' + DatenPfad + C_TbAuftrag + '"');
    if q.open then result:= q.Fields[0].asInteger;
  finally
    q.free;
  end;
end;

{-------------------------------------------------------------------}
procedure TAuftragDb.GetInaktiveAuftraege (InaktivList: TStringList); // 07.03.2002 WW
{-------------------------------------------------------------------}
{ inaktive Automatik-Aufträge aus Tabelle lesen und in Liste schreiben mit
  Geräteart und Geräte-Id }
var
  q: TQueryExt;
  s: string;
begin
  if InaktivList = nil then exit;
  q:= TQueryExt.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('SELECT ' + C_TFAGeraeteArt + ', ' + C_TFAGeraeteId);
    q.sql.add('FROM "' + C_TbAuftrag + '"');
    q.sql.add('WHERE ' + C_TfAAktiv + '= :Aktiv');
    q.sql.add('AND ' + C_TFAAbrufArt + '= :AbrArt');
    q.ParamByName('Aktiv').asBoolean:= false;
    q.ParamByName('AbrArt').asString:= C_AbrArtAuto;
    if q.open then begin
      while not q.Eof do begin
        s:=q.FieldByName(C_TFAGeraeteArt).asString +
           IntToStr (q.FieldByName(C_TFAGeraeteId).asInteger);
        InaktivList.Add (s);
        q.Next;
      end;
    end;
  finally
    q.free;
  end;
end;

{ Tabelle leeren und neu initialisieren              }
{----------------------------------------------------}
procedure TAuftragDb.NewAuftragsTable;
{----------------------------------------------------}
begin
  EmptyTable;
  InitAuftragsTable;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{ Inaktive Aufträge lesen und merken, Tabelle leeren und mit bisherigen
  Aktiv-Stati neu initialisieren              }
{--------------------------------------------------}
procedure TAuftragDb.NewAuftragsTableWithAktivStati;  // 07.03.2002 WW
{--------------------------------------------------}
var
  InaktivList: TStringList;
begin
  InaktivList:=TStringList.Create;
  try
    { inaktive Automatik-Aufträge aus bisheriger Tabelle lesen und in Liste merken }
    GetInaktiveAuftraege (InaktivList);
    { Tabelle leeren }
    EmptyTable;
    { ...und Tabelle mit den gemerkten Inaktiv-Stati neu füllen }
    InitAuftragsTable (InaktivList);
    { Trigger-Datei schreiben }
    WriteNewTime(DatenPfad + C_TbAuftrag);
  finally
    InaktivList.Free;
  end;
end;

{ Tabelle mit TERMIN.DB - Daten abgleichen           }
{----------------------------------------------------}
procedure TAuftragDb.InitAuftragsTable (InaktivList: TStringList = nil);
{----------------------------------------------------}
var
  anArt         : string[2];
  anId          : integer;
  Termin        : TTerminDb;
  DatumZeit     : TDateTime;
  Datentypen    : integer;
  sl, StaList   : TStringList;
  i             : integer;
  lp, pr        : integer;

  {--------------------------------}
  procedure GetPrioLp(theId: integer; theArt: string; var Prio, Logp: integer);
  {--------------------------------}
  var
    s           : string;
    j           : integer;
  begin
    s:= theArt + IntToStr(theId);
    j:= StaList.IndexOf(s);
    if (j > -1) then lp:= TIdRec(StaList.Objects[j]).LogPort else lp:= 0;
    if (j > -1) then pr:= TIdRec(StaList.Objects[j]).Prioritaet else pr:= 1;
  end;

  {--------------------------------}
  function GetInaktiv(theId: integer; theArt: string): boolean;
  {--------------------------------}
  { Ergebnis: true, wenn Eintrag für theId, theArt in InaktivList gefunden wurde }
  var
    s           : string;
    j           : integer;
  begin
    s:= theArt + IntToStr(theId);
    j:= InaktivList.IndexOf(s);
    Result:=j > -1;
  end;

begin
  if not isOk then exit;
  if not tbAuftrag.OpenExclusive then exit;
  StaList:= TStringList.Create;
  GetStationsnameListe(StaList, C_AuswahlAlle);

  sl:= TStringList.Create;
  Termin:= TTerminDb.create(Datenpfad, SystemPfad);
  try
    Termin.GetAbgleichList(sl);
    for i:= 0 to sl.Count-1 do begin
      anArt:= TTerminRec(sl.Objects[i]).Typ;
      anId:= TTerminRec(sl.Objects[i]).Id;
      Datentypen:= TTerminRec(sl.Objects[i]).Abrufe;
      DatumZeit:= TTerminRec(sl.Objects[i]).Datum;
{ Abzugleichende Aufträge sind immer von der Abrufart 'Automatik' }
      if tbAuftrag.FindKey([anId, anArt, C_AbrArtAuto]) then begin
{ Es existiert ein Eintrag zu diesem Gerät }
        if (tbAuftrag.FieldByName(C_TfADatumZeit).asFloat >= double(DatumZeit)) then begin
{ Überprüfung, ob der neue Auftrag früher abgerufen wird }
          GetPrioLp(anId, anArt, pr, lp);
          tbAuftrag.edit;
          tbAuftrag.FieldByName(C_TfADatumZeit).asDateTime:= DatumZeit;
          tbAuftrag.FieldByName(C_TfAAbrufe).asInteger:= Datentypen;
          tbAuftrag.FieldByName(C_TFAPrioritaet).asInteger:= pr;
          tbAuftrag.FieldByName(C_TFALogport).asInteger:= lp;
          tbAuftrag.post;
        end
      end
      else begin
{ Es existiert kein Eintrag zu diesem Gerät }
        GetPrioLp(anId, anArt, pr, lp);
        tbAuftrag.append;
        tbAuftrag.FieldByName(C_TfAGeraeteId).asInteger:= anId;
        tbAuftrag.FieldByName(C_TfAGeraeteArt).asString:= anArt;
        tbAuftrag.FieldByName(C_TfADatumZeit).asDateTime:= DatumZeit;
        tbAuftrag.FieldByName(C_TfAAbrufe).asInteger:= Datentypen;
        tbAuftrag.FieldByName(C_TfAAbrufArt).asString:= C_AbrArtAuto;
        tbAuftrag.FieldByName(C_TFAPrioritaet).asInteger:= pr;
        tbAuftrag.FieldByName(C_TFALogport).asInteger:= lp;
        tbAuftrag.FieldByName(C_TFADurchlaufNr).asInteger:= 0;
        tbAuftrag.FieldByName(C_TFABelegt).asBoolean:= False;
        if InaktivList = nil then
          { fest auf aktiv setzen }
          tbAuftrag.FieldByName(C_TFAAktiv).asBoolean:= True
        else begin
          { Aktiv-Einstellung aus DeaktivList nehmen }
          tbAuftrag.FieldByName(C_TFAAktiv).asBoolean:= not GetInaktiv(anId, anArt);
        end;  
        tbAuftrag.post;
      end;
    end;
  except
    sl.Free;
    StaList.Free;
    Termin.Free;
    tbAuftrag.close;
    exit;
  end;
  sl.Free;
  StaList.Free;
  Termin.Free;
  tbAuftrag.close;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{-------------------------------------------}
procedure TAuftragDb.ResetAutomatikAuftraege;
{-------------------------------------------}
{ alle belegten Automatik-Aufträge auf "nicht belegt" zurücksetzen und
  mit nächstem Termin versehen }
var
  anArt         : string[2];
  anId          : integer;
  Termin        : TTerminDb;
  DatumZeit     : TDateTime;
  Datentypen    : integer;
  sl, StaList   : TStringList;
  lp, pr        : integer;

  {-----------------------------------------------------------------------------------}
  function GetTermindaten (theId: integer; theArt: string;
                           var DatumZeit: TDateTime; var Datentypen: integer): boolean;
  {-----------------------------------------------------------------------------------}
  var
    j: integer;
  begin
    Result:=false;
    DatumZeit:=0;
    Datentypen:=0;
    for j:= 0 to sl.Count-1 do begin
      if (theArt = TTerminRec(sl.Objects[j]).Typ) AND
         (theId = TTerminRec(sl.Objects[j]).Id) then begin
        DatumZeit:= TTerminRec(sl.Objects[j]).Datum;
        Datentypen:= TTerminRec(sl.Objects[j]).Abrufe;
        Result:=true;
        Break;
      end;
    end;
  end;

  {--------------------------------}
  procedure GetPrioLp(theId: integer; theArt: string; var Prio, Logp: integer);
  {--------------------------------}
  var
    s           : string;
    j           : integer;
  begin
    s:= theArt + IntToStr(theId);
    j:= StaList.IndexOf(s);
    if (j > -1) then lp:= TIdRec(StaList.Objects[j]).LogPort else lp:= 0;
    if (j > -1) then pr:= TIdRec(StaList.Objects[j]).Prioritaet else pr:= 1;
  end;

begin
  if not isOk then exit;
  if not tbAuftrag.OpenExclusive then exit;
  StaList:= TStringList.Create;
  GetStationsnameListe(StaList, C_AuswahlAlle);

  sl:= TStringList.Create;
  Termin:= TTerminDb.create(Datenpfad, SystemPfad);
  try
    Termin.GetAbgleichList(sl);

    { Filter auf Automatik-Aufträge: }
    tbAuftrag.Filtered:=false;                     { Deaktivieren des Filters }
    tbAuftrag.Filter:=C_TfAAbrufArt + ' = ''' + C_AbrArtAuto + '''';
    tbAuftrag.Filtered:=true;                      { Aktivieren des Filters }

    while not tbAuftrag.Eof do begin
      anArt:=tbAuftrag.FieldByName(C_TfAGeraeteArt).asString;
      anId:=tbAuftrag.FieldByName(C_TfAGeraeteId).asInteger;
      if GetTermindaten (anId, anArt, DatumZeit, Datentypen) then begin
        GetPrioLp(anId, anArt, pr, lp);
        tbAuftrag.edit;
        tbAuftrag.FieldByName(C_TfADatumZeit).asDateTime:= DatumZeit;
        tbAuftrag.FieldByName(C_TfAAbrufe).asInteger:= Datentypen;
        tbAuftrag.FieldByName(C_TFAPrioritaet).asInteger:= pr;
        tbAuftrag.FieldByName(C_TFALogport).asInteger:= lp;
        tbAuftrag.FieldByName(C_TFADurchlaufNr).asInteger:= 0;
        tbAuftrag.FieldByName(C_TFABelegt).asBoolean:= False;
        { Wert für Feld "aktiv" beibehalten }
        tbAuftrag.post;
        tbAuftrag.Next;
      end else
        tbAuftrag.Delete;
    end;  { while }
  except
    sl.Free;
    StaList.Free;
    Termin.Free;
    tbAuftrag.Filtered:=false;
    tbAuftrag.close;
    exit;
  end;
  sl.Free;
  StaList.Free;
  Termin.Free;
  tbAuftrag.Filtered:=false;
  tbAuftrag.close;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{ WAuftrag updaten                                   }
{----------------------------------------------------}
function TAuftragDb.updateAuftrag(anArt: string; anId: integer; anAbrArt: string;
                            aDT: TDateTime; aDaten: integer; aAnrufz: integer = 0;
                            anAktivStatus: boolean = true): boolean;
{----------------------------------------------------}
begin
  result:= False;
  with TbAuftrag do begin
    if not OpenExclusive then Exit;
    try
      if FindKey([anId, anArt, anAbrArt]) then begin
        Edit;
        if Int (aDT) > -1 then
          FieldByName(C_TfADatumZeit).asDateTime:= TDateTime (aDT);
        if aDaten > -1 then
          FieldByName(C_TfAAbrufe).asInteger:= aDaten;
        FieldByName(C_TFAPrioritaet).asInteger:= GetPrioritaet(anId, anArt);
        FieldByName(C_TFALogport).asInteger:= GetLogPort(anId, anArt);
        FieldByName(C_TFADurchlaufNr).asInteger:= aAnrufz;
        FieldByName(C_TFABelegt).asBoolean:= False;
        FieldByName(C_TFAAktiv).asBoolean:= anAktivStatus;
        Post;
        result:= True;
      end
      else writeLogFile('Eintrag zum Updaten nicht gefunden');
    finally
      Close;
    end;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{ Neuen automatischen Eintrag an Tabelle anhängen,   }
{ wenn kein früherer automatischer existiert         }
{----------------------------------------------------}
function TAuftragDb.appendNeuenAuftrag(anArt: string; anId: integer;
                                       aDT: TDateTime; aDaten: integer;
                                       aPrioritaet: smallInt;
                                       anAbrArt: string = C_AbrArtAuto;
                                       VonZeit: TDateTime = 0;
                                       BisZeit: TdateTime = 0): boolean;
{----------------------------------------------------}
begin
  result:= False; { default }
  with TbAuftrag do begin
    if not OpenExclusive then Exit;
    try
      result:= True;
      if findKey([anId, anArt, anAbrArt]) then begin
        if anAbrArt = C_AbrArtAuto then begin
         { Überprüfung, ob neuer Automatik-Auftrag früher abgerufen werden soll }
          if double(aDT) <= double(FieldByName(C_TfADatumZeit).asDateTime) then begin
            edit;
            FieldByName(C_TfADatumZeit).asDateTime:= aDT;
            FieldByName(C_TfAAbrufe).asInteger:= aDaten;
            FieldByName(C_TFAPrioritaet).asInteger:= aPrioritaet;
            FieldByName(C_TFALogport).asInteger:= GetLogPort(anId, anArt);
            post;
          end;
        end
        else begin
          writeLogFile('Früherer manueller Abruf bereits vorhanden !');
          result:= false;
        end;
      end
      else begin
        append;
        FieldByName(C_TfAGeraeteId).asInteger:= anId;
        FieldByName(C_TfAGeraeteArt).asString:= anArt;
        FieldByName(C_TfADatumZeit).asDateTime:= aDT;
        FieldByName(C_TfAAbrufe).asInteger:= aDaten;
        FieldByName(C_TfAAbrufArt).asString:= anAbrArt;
        FieldByName(C_TFAPrioritaet).asInteger:= aPrioritaet;
        FieldByName(C_TFALogport).asInteger:= GetLogPort(anId, anArt);
        FieldByName(C_TFADurchlaufNr).asInteger:= 0;
        FieldByName(C_TFABelegt).asBoolean:= False;
        FieldByName(C_TFAAktiv).asBoolean:= True;
        if anAbrArt = C_AbrArtManu then begin
          if VonZeit <> 0 then FieldByName(C_TFAVonDatum).asDateTime:= VonZeit;
          if BisZeit <> 0 then FieldByName(C_TFABisDatum).asDateTime:= BisZeit;
        end;
        post;
      end;
    finally
      Close;
    end;
  end;
end;

{ 'Aktiv'-Feld in der WAuftrag.db setzen              }
{----------------------------------------------------}
procedure TAuftragDb.setAktiv(anArt: string; anId: integer; anAbrArt: string;
                              Value: boolean);
{----------------------------------------------------}
begin
  with TbAuftrag do begin
    if not OpenExclusive then exit;
    try
      if FindKey([anId, anArt, anAbrArt]) then begin
  { Manuelle Aufträge werden nicht deaktiviert, sondern gelöscht }
        if FieldByName(C_TfAAbrufArt).asString <> C_AbrArtAuto then begin
          if MessageDlg('Auftrag wird gelöscht !', mtWarning, [mbOk, mbCancel], 0) = mrOk then
            delete;
        end
        else begin
          edit;
          fieldByName(C_TFAAktiv).asBoolean:= value;
          if Value then fieldByName(C_TFADurchlaufNr).asInteger:= 0;
          post;
        end;
      end
      else
        writeLogFile('Eintrag nicht in der WAUFTRAG.DB gefunden');
    finally
      Close;
    end;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{ 'Belegt'-Feld in der WAuftrag.db setzen            }
{----------------------------------------------------}
procedure TAuftragDb.setBelegt(anArt: string; anId: integer; anAbrArt: string;
                              Value: boolean);
{----------------------------------------------------}
var
  q: TQueryExt;
begin
  q:= TQueryExt.create(nil);
  try
    q.DatabaseName:= DatenPfad;
    q.sql.add('UPDATE ' + C_TbAuftrag);
    q.sql.add('SET');
    q.sql.add(C_TFABelegt + '= :isTrue');
    q.sql.add('WHERE ' + C_TfAGeraeteArt + '= :GArt');
    q.sql.add('AND ' + C_TfAGeraeteId + '= :GId');
    q.sql.add('AND ' + C_TFAAbrufArt + '= :GAbrArt');
    q.ParamByName('isTrue').asBoolean:= true;
    q.ParamByName('GArt').asString:= anArt;
    q.ParamByName('GId').asInteger:= anId;
    q.ParamByName('GAbrArt').asString:= anAbrArt;
    q.ExecSql;
  finally
    q.free;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{ Startzeitpunkt auf 'Jetzt + AbrufOffset' setzen    }
{----------------------------------------------------}
procedure TAuftragDb.setToNow(anArt: string; anId: integer; anAbrArt: string);
{----------------------------------------------------}
begin
  with TbAuftrag do begin
    if not OpenExclusive then Exit;
    try
      if FindKey([anId, anArt, anAbrArt]) then begin
        Edit;
        { Startzeitpunkt }
        FieldByName(C_TfADatumZeit).asDateTime:= now;
        { Ist auf jeden Fall aktiv }
        FieldByName(C_TFAAktiv).asBoolean:= true;
        { Anrufversuche auf 0 setzen }
        FieldByName(C_TFADurchlaufNr).asInteger:= 0;
        Post;
      end
      else writeLogFile('Eintrag nicht in der WAUFTRAG.DB gefunden');
    finally
      Close;
    end;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{ Startzeitpunkt für alle aktiven und nicht belegten Automatik-Aufträge auf
  'Jetzt + AbrufOffset' setzen    }
{-------------------------------}
procedure TAuftragDb.setAllToNow;
{-------------------------------}
begin
  with TbAuftrag do begin
    if not OpenExclusive then Exit;
    try
      while not Eof do begin
        if not (FieldByName(C_TFABelegt).asBoolean) and
               (fieldByName(C_TFAAktiv).asBoolean)
        then begin
          Edit;
          { Startzeitpunkt }
          FieldByName(C_TfADatumZeit).asDateTime:= now;
          { Anrufversuche auf 0 setzen }
          FieldByName(C_TFADurchlaufNr).asInteger:= 0;
          Post;
        end;
        Next;
      end;
    finally
      Close;
    end;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{ Übergebenen Datensatz löschen   }
{----------------------------------------------------}
procedure TAuftragDb.deleteAuftrag(anArt: string; anId: integer;
                                   anAbrArt: string = C_AbrArtManu);
{----------------------------------------------------}
begin
  with TbAuftrag do begin
    if not OpenExclusive then Exit;
    try
      if FindKey([anId, anArt, anAbrArt]) then delete
      else writeLogFile('Eintrag nicht in der WAUFTRAG.DB gefunden');
    finally
      Close;
    end;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{------------------------------------------------------------------------------------}
function TAuftragDb.DeleteAuftraege (Geraeteart: string; GeraeteId: integer): boolean;
{------------------------------------------------------------------------------------}
{ Alle Aufträge für übergebene Station löschen;
  Übergabe: Geräteart
            Geräte-ID
  Ergebnis: true, wenn Aufträge erfolgreich gelöscht wurden }
begin
  Result:=false;
  with TbAuftrag do begin
    if not OpenExclusive then Exit;
    try
      Result:=true;
      { Filter auf Geräteart und GeräteId: }
      Filtered:=false;                     { Deaktivieren des Filters }
      Filter:='('+ C_TfAGeraeteArt + ' = ''' + Geraeteart +''') AND '+
              '('+ C_TfAGeraeteId + ' = ' + IntToStr(GeraeteId) +')';
      Filtered:=true;                      { Aktivieren des Filters }
      while not Eof do
        Delete;
    finally
      Filtered:=false;
      Close;
    end;
  end;
  { Trigger-Datei schreiben }
  WriteNewTime(DatenPfad + C_TbAuftrag);
end;

{ Aktuelle Aufträge an Query übergeben               }
{ ACHTUNG: Anrufversuche werden erst NACH der Über-  }
{ gabe inkrementiert -> bei Weitergabe beachten !    }
{ Parameter: übergabequery                           }
{ Rückgabe: True, wenn i.O. und RecordCount > 0      }
{----------------------------------------------------}
function TAuftragDb.aktuelleAuftraege(var aQuery: TQueryExt): boolean;
{----------------------------------------------------}
begin
  result:= false; { default }

  { 1.: Ergebnismenge in Übergabequery einlesen }
  with aQuery do begin
//    try
      close;
      DatabaseName:= Datenpfad;
  { Selektieren aller Einträge mit Startzeitpunkt 'bis jetzt' und mit 'nicht belegt' }
      sql.clear;
      sql.add('SELECT * FROM "' + DatenPfad + C_TbAuftrag + '"');
      sql.add('WHERE ' + C_TfADatumZeit + '<= :Jetzt');
      sql.add('AND ' + C_TfABelegt + '= :Belegt');
      sql.add('AND ' + C_TfAAktiv + '= :Aktiv');
      sql.add('ORDER BY ' + C_TfADatumZeit + ',' + C_TFAPrioritaet);
      paramByName('Jetzt').AsDateTime:= now;
      paramByName('Belegt').AsBoolean:= false;
      paramByName('Aktiv').AsBoolean:= true;
      if not Open then begin
        exit;
      end;
{    except
      writeLogFile('FEHLER - Konnte Aufruf-Query nicht übergeben !');
      Close;
      exit;
    end; }
  end;

  { 2.: Setzen der Einträge auf 'Belegt' und inkrementieren der Anrufversuche }
  if aQuery.RecordCount > 0 then begin
(*    aQuery.first;
    q:= TQueryExt.Create(nil);
    try
      q.DatabaseName:= DatenPfad;
      q.close;
      q.sql.clear;
      q.sql.add('UPDATE "' + DatenPfad + C_TbAuftrag + '"');
      q.sql.add('SET');
      q.sql.add(C_TfABelegt + '= :Belegt');
      q.sql.add(C_TFADurchlaufNr + '=' + C_TFADurchlaufNr + '+1');
      q.sql.add('WHERE ');
      q.sql.add('(' + C_TfAGeraeteArt + '= "'
                + aQuery.FieldByName(C_TfAGeraeteArt).asString + '"');
      q.sql.add('AND ' + C_TfAGeraeteId + '= '
                + IntToStr(aQuery.FieldByName(C_TfAGeraeteId).asInteger));
      q.sql.add('AND ' + C_TFAAbrufArt + '= "'
                + aQuery.FieldByName(C_TfAAbrufArt).asString + '")');
      if aQuery.RecordCount > 1 then begin
        aQuery.Next;
        while not aQuery.eof do begin
          q.sql.add('OR ');
          q.sql.add('(' + C_TfAGeraeteArt + '= "'
                    + aQuery.FieldByName(C_TfAGeraeteArt).asString + '"');
          q.sql.add('AND ' + C_TfAGeraeteId + '= '
                    + IntToStr(aQuery.FieldByName(C_TfAGeraeteId).asInteger));
          q.sql.add('AND ' + C_TFAAbrufArt + '= "'
                    + aQuery.FieldByName(C_TfAAbrufArt).asString + '")');
          aQuery.Next;
        end
      end;
      q.ParamByName('Belegt').asBoolean:= true;
      if not q.ExecSql then exit;
    except
      q.free;
      aQuery.Close;
      writeLogFile('FEHLER - Konnte Belegtstatus nicht setzen !');
      exit;
    end;
    q.free;
  end;
*)
    if not tbAuftrag.OpenExclusive then exit;
    try
      aQuery.first;
      while not aQuery.eof do begin
        if tbAuftrag.findKey([aQuery.FieldByName(C_TfAGeraeteId).asInteger,
                              aQuery.FieldByName(C_TfAGeraeteArt).asString,
                              aQuery.FieldByName(C_TfAAbrufArt).asString]) then begin
          tbAuftrag.Edit;
          tbAuftrag.FieldByName(C_TFADurchlaufNr).asInteger:=tbAuftrag.FieldByName(C_TFADurchlaufNr).asInteger + 1;
          tbAuftrag.FieldByName(C_TFABelegt).asBoolean:= True;
          tbAuftrag.Post;
        end else
          writeLogFile('Eintrag zum Updaten nicht gefunden');
        aQuery.Next;
      end;
    finally
      tbAuftrag.Close;
    end;
   { Trigger-Datei schreiben }
    WriteNewTime(DatenPfad + C_TbAuftrag);

    result:=true;
  end; { if aQuery.RecordCount > 0 }
end;

{ Prüfen, ob neuer Auftrag für eine Station eingetragen werden kann }
{ Ergebnis: true, wenn Eintragen möglich                            }
{----------------------------------------------------}
function TAuftragDb.CheckNeuenAuftrag(anArt: string; anId: integer; anAbrArt: string): boolean;
{----------------------------------------------------}
begin
  result:= False; { default }
  with TbAuftrag do begin
    if not OpenExclusive then Exit;
    try
      if findKey([anId, anArt, anAbrArt]) AND (anAbrArt <> C_AbrArtAuto) then
        result:= false
      else
        result:= True;
    finally
      Close;
    end;
  end;
end;

end.
