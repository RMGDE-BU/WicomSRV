{******************************************************************************}
{* Unit: Zugriffe auf LAKS-Abruftabelle                                       *}
{* 17.06.1999 WW                                                              *}
{******************************************************************************}
Unit LDbAbruf;

INTERFACE

Uses
  Dialogs, SysUtils, Db, DbTables, WTables, WSysCon, SrvCfgIni;

const

  { Tabellennamen }

  CDBLAKSAbruf = 'AbrfLaks.db';

  { Tabelle 'LAKSABRUF.DB' }

  C_LAKSAbruf_Schnittstelle = 'Schnittstelle';
  C_LAKSAbruf_Dfue          = 'Dfue';
  C_LAKSAbruf_LAKSId        = 'LAKSId';
  C_LAKSAbruf_Abrufart      = 'Abrufart';
  C_LAKSAbruf_Datentypen    = 'Datentypen';
  C_LAKSAbruf_DatenVon      = 'DatenVon';
  C_LAKSAbruf_DatenBis      = 'DatenBis';
  C_LAKSAbruf_Anrufversuch  = 'Anrufversuch';


type

  TAbrufart = ({aa_automatisch, aa_manuell, aa_ruf, }aa_momentan);

  TAbrufData = record
    LAKSId: integer;
    Datentypen: integer;
    DatenVon: TDateTime;
    DatenBis: TDateTime;
    Anrufversuch: integer;
    Erfolgreich: boolean;
  end;

  { Objekt für Zugriff auf LaksAbruf.db }

  TLAKSAbruf = class (TObject)
  private
    Path: TFileName;
    tbAbruf: TTableExt;
    procedure CreateLAKSAbrufDB;
  public
    constructor Create (APath: TFileName);
    destructor Destroy; override;
    function EmptyLAKSAbrufTable: boolean;
    function InsertToLAKSAbruf(anId, aLogPort, aDaten, aVersuch: integer;
                               anAbrufArt: string; Von, Bis: TDateTime;
                               isDfu: boolean): boolean;
    function IsLAKSAbruf(anId: integer; aLogPort: integer; aDfue: boolean;
                                    anAbrufArt: string): boolean;
    function GetLAKSAbruf (COMNr: byte; Dfue: boolean; var Abrufart: TAbrufart;
                           var AbrufData: TAbrufData): boolean;
    function GetLAKSAbrufMomentan (Dfue: boolean; LAKSId: integer;
                                   var Abrufart: string;
                                   var Datentypen: integer): boolean;
    function GetLAKSAbrufMomentanHalten (Dfue: boolean; LAKSId: integer): boolean;
    procedure DeleteLAKSMomentan (Dfue: boolean; LAKSId: integer);
    function GetRecordCount: integer;
  end;


IMPLEMENTATION

{-----------------------------------------------}
constructor TLAKSAbruf.Create (APath: TFileName);
{-----------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  tbAbruf:= TTableExt.create(nil);
  tbAbruf.DataBaseName:= Path;
  tbAbruf.TableName:= CDBLAKSAbruf;

  if not FileExists (Path + CDBLAKSAbruf) then
    CreateLAKSAbrufDB;                           { Tabelle automatisch anlegen }
end;

{----------------------------}
destructor TLAKSAbruf.Destroy;
{----------------------------}
begin
  tbAbruf.free;
  inherited destroy;
end;

{-------------------------------------}
procedure TLAKSAbruf.CreateLAKSAbrufDB;
{-------------------------------------}
{ LAKS-Abruf-Tabelle anlegen }
begin
  with tbAbruf.FieldDefs do begin
    Clear;
    Add(C_LAKSAbruf_Schnittstelle, ftSmallInt, 0, false);
    Add(C_LAKSAbruf_Dfue, ftBoolean, 0, false);
    Add(C_LAKSAbruf_LAKSId, ftInteger, 0, false);
    Add(C_LAKSAbruf_Abrufart, ftString, 10, false);
    Add(C_LAKSAbruf_Datentypen, ftInteger, 0, false);
    Add(C_LAKSAbruf_DatenVon, ftDateTime, 0, false);
    Add(C_LAKSAbruf_DatenBis, ftDateTime, 0, false);
    Add(C_LAKSAbruf_Anrufversuch, ftSmallInt, 0, false);
  end;
  tbAbruf.IndexDefs.Clear;                                       { kein Index }
  tbAbruf.CreateTable;
end;

{ Leert die Abruf-Tabelle           }
{-----------------------------------}
function TLAKSAbruf.EmptyLAKSAbrufTable: boolean;
{-----------------------------------}
begin
  try
    tbAbruf.Active:= False;
    result:= tbAbruf.EmptyTable;
  except
    result:= False;
  end;
end;

(*
{ Daten aus einem Query in die LAKS-Abruf-Tabelle einfügen }
{ Inkrementieren der Anrufversuche                        }
{ Übergabe: Query mit einzufügenden Daten aus AUFTRAG.DB  }
{---------------------------------------------------------}
function TLAKSAbruf.InsertLAKSAbruf(aQuery: TQuery): boolean;
{---------------------------------------------------------}
begin
  result:= False; { default }
  if tbAbruf.OpenShared then begin
    if aQuery.Active then begin
      try
        aQuery.first;
  { Schleife über Übergabe-Query - enthält die Felder aus AUFTRAG.DB }
        while not aQuery.eof do begin
  { Manuelle Aufträge werden am Anfang eingefügt, andere angehängt }
          if (aQuery.fieldByName(C_LAKSAbruf_Abrufart).asString = C_AbrArtManu)
          then begin
            tbAbruf.first;
            tbAbruf.insert;
          end
          else begin
            tbAbruf.append;
          end;
          tbAbruf.fieldByName(C_LAKSAbruf_Schnittstelle).asInteger:=
            aQuery.FieldByName(C_TFALogport).asInteger;
>> Dfü
          tbAbruf.fieldByName(C_LAKSAbruf_LAKSId).asInteger:=
            aQuery.FieldByName(C_TfAGeraeteId).asInteger;
          tbAbruf.fieldByName(C_LAKSAbruf_Abrufart).asString:=
            aQuery.FieldByName(C_TFAAbrufArt).asString;
          tbAbruf.fieldByName(C_LAKSAbruf_Datentypen).asInteger:=
            aQuery.FieldByName(C_TFAAbrufe).asInteger;
          tbAbruf.fieldByName(C_LAKSAbruf_DatenVon).asDateTime:=
            aQuery.FieldByName(C_TfAVonDatum).asDateTime;
          tbAbruf.fieldByName(C_LAKSAbruf_DatenBis).asDateTime:=
            aQuery.FieldByName(C_TfABisDatum).asDateTime;
          tbAbruf.fieldByName(C_LAKSAbruf_Anrufversuch).asInteger:=
            aQuery.FieldByName(C_TfADurchlaufNr).asInteger + 1;
          tbAbruf.post;
          aQuery.next;
        end;
      except
        tbAbruf.Close;
        exit;
      end;
      result:= True; { Wenn die function bis hierher kommt }
    end;
    tbAbruf.close;
  end;
end;
*)
{ Parameter in die LAKS-Abruf-Tabelle einfügen                                }
{ Übergabe: Parameter mit einzufügenden Daten                                 }
{ Rückgabe: Erfolg True/False                                                 }
{-----------------------------------------------------------------------------}
function TLAKSAbruf.InsertToLAKSAbruf(anId, aLogPort, aDaten, aVersuch: integer;
                              anAbrufArt: string; Von, Bis: TDateTime;
                              isDfu: boolean): boolean;
{-----------------------------------------------------------------------------}
begin
  result:= False; { default }
  if FileExists(Path + CDBLAKSAbruf) then begin
    if tbAbruf.OpenShared then begin
      try
  { Starts werden am Anfang eingefügt, andere angehängt }
        if (anAbrufArt = C_AbrArtMomStart)
        then begin
          tbAbruf.first;
          tbAbruf.insert;
        end
        else begin
          tbAbruf.append;
        end;
        tbAbruf.fieldByName(C_LAKSAbruf_Schnittstelle).asInteger:= aLogPort;
        tbAbruf.fieldByName(C_LAKSAbruf_LAKSId).asInteger:= anId;
        tbAbruf.fieldByName(C_LAKSAbruf_Dfue).asBoolean:= isDfu;
        tbAbruf.fieldByName(C_LAKSAbruf_Abrufart).asString:= anAbrufArt;
        tbAbruf.fieldByName(C_LAKSAbruf_Datentypen).asInteger:= aDaten;
        tbAbruf.fieldByName(C_LAKSAbruf_DatenVon).asDateTime:= Von;
        tbAbruf.fieldByName(C_LAKSAbruf_DatenBis).asDateTime:= Bis;
        tbAbruf.fieldByName(C_LAKSAbruf_Anrufversuch).asInteger:= aVersuch;
        tbAbruf.post;
        result:= true;  { wenn die function bis hierher kommt }
      except
        tbAbruf.Close;
        exit;
      end;
      tbAbruf.close;
    end;
  end;
end;

{ Überprüft, ob ein Eintrag mit übergebener Id, Schnittstelle, Verbindungsart (V24/DFÜ) und }
{ Abrufart in der LDBAbruf.Db steht                         }
{ Parameter: Id, LogPort, Dfue und Abrufart                 }
{ Rückgabe: True - steht drin; False - steht nicht drin     }
{------------------------------------------------------------------------------------------}
function TLAKSAbruf.IsLAKSAbruf(anId: integer; aLogPort: integer; aDfue: boolean;
                                anAbrufArt: string): boolean;
{------------------------------------------------------------------------------------------}
var
  q          : TQueryExt;
begin
  result:= False; { default }
  if fileExists(Path + CDBLAKSAbruf) then begin
    q:= TQueryExt.Create(nil);
    try

      q.DatabaseName:= Path;
      q.Sql.Add('SELECT COUNT(*) FROM "' + CDBLAKSAbruf + '"');
      q.Sql.Add('WHERE ' + C_LAKSAbruf_Dfue + ' = :DFU');
      q.Sql.Add('AND ' + C_LAKSAbruf_Schnittstelle + ' = ' + IntToStr(aLogPort));
      q.Sql.Add('AND ' + C_LAKSAbruf_LAKSId + ' = ' + IntToStr(anId));
      q.Sql.Add('AND ' + C_LAKSAbruf_Abrufart + '= "' + anAbrufArt + '"');
      q.ParamByName('DFU').asBoolean:= ADfue;

      if q.open then begin
        if (q.Fields[0].asInteger > 0) then result:= True;
      end;
    finally
      q.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------------}
function TLAKSAbruf.GetLAKSAbruf (COMNr: byte; Dfue: boolean; var Abrufart: TAbrufart;
                                  var AbrufData: TAbrufData): boolean;
{------------------------------------------------------------------------------------}
{ Abrufdaten für einen Abruf aus Tabelle lesen; gelesenen Datensatz aus Tabelle löschen }
{ Übergabe: COMNr
            Dfue ( -> aus Schnittstellenkonfiguration )
  Rückgabe: Abrufart
            AbrufData
  Ergebnis: true, wenn LAKS-Abruf gelesen werden konnte }
Const
  CComx = 0;                                        { beliebige Schnittstelle }

var
  TempSchnittstelle: integer;
  TempAbrufart: string;

begin
  Result := false;
  FillChar (AbrufData, SizeOf (TAbrufData), 0);                 { Vorbelegung für AbrufData }

  if FileExists(Path + CDBLAKSAbruf) then begin
    if tbAbruf.OpenExclusive then begin
      try
        while not tbAbruf.Eof do begin
          TempAbrufart := tbAbruf.FieldByName(C_LAKSAbruf_Abrufart).AsString;
          { "Momentanwerte beenden, halten" ignorieren }
          if (TempAbrufart = C_AbrArtMomStop) OR (TempAbrufart = C_AbrArtMomHalten) then begin
            tbAbruf.Next;
            Continue;
          end;

          { Abrufmodus DFÜ/seriell prüfen }
          if tbAbruf.FieldByName(C_LAKSAbruf_Dfue).AsBoolean <> Dfue then begin
            tbAbruf.Next;
            Continue;
          end;

          TempSchnittstelle:=tbAbruf.FieldByName(C_LAKSAbruf_Schnittstelle).AsInteger;
          { feste Schnittstelle: logische -> physikalische }
          if (TempSchnittstelle >= Low (LogPhysComZuordnung)) AND
             (TempSchnittstelle <= High (LogPhysComZuordnung)) then begin
            if LogPhysComZuordnung [TempSchnittstelle] <> COMNr then begin
              tbAbruf.Next;
              Continue;
            end;
          end;

          Result:=true;
          AbrufData.LAKSId := tbAbruf.FieldByName(C_LAKSAbruf_LAKSId).AsInteger;
          AbrufData.Datentypen := tbAbruf.FieldByName(C_LAKSAbruf_Datentypen).AsInteger;
          if not tbAbruf.FieldByName(C_LAKSAbruf_DatenVon).IsNull then
            AbrufData.DatenVon := tbAbruf.FieldByName(C_LAKSAbruf_DatenVon).AsDateTime
          else
            AbrufData.DatenVon := 0;
          if not tbAbruf.FieldByName(C_LAKSAbruf_DatenBis).IsNull then
            AbrufData.DatenBis := tbAbruf.FieldByName(C_LAKSAbruf_DatenBis).AsDateTime
          else
            AbrufData.DatenBis := 0;
          AbrufData.Anrufversuch := tbAbruf.FieldByName(C_LAKSAbruf_Anrufversuch).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
          Break;
        end;  { while not tbAbruf.eof }
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }

    { Rückgabe belegen: Abrufart }
(*    if TempAbrufart = C_AbrArtAuto then
      Abrufart := aa_automatisch
    else if SearchAbrufart = C_AbrArtMomStart then*)
      Abrufart := aa_momentan
(*    else
      Abrufart := aa_manuell;*)
  end; { if FileExists }
end;

{----------------------------------------------------------------------------}
function TLAKSAbruf.GetLAKSAbrufMomentan (Dfue: boolean; LAKSId: integer;
                                            var Abrufart: string;
                                            var Datentypen: integer): boolean;
{----------------------------------------------------------------------------}
{ LAKS-Abruftabelle nach Einträgen für laufende Momentanwertdarstellung durchsuchen;
  Suchpriorität: 1. MomArchiv, ParaLesen, ParaSenden
                 2. MomE
  Übergabe: Dfue
            LAKSId
  Rückgabe: Abrufart, Datentypen des Tabelleneintrags
  Ergebnis: false, wenn Tabelle nicht gelesen werden konnte }
var
  S: string;
  gefunden: boolean;
  
begin
  Result:=false;
  Abrufart:='';
  Datentypen:=-1;

  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        Result:=true;
        if Dfue then
          S:='''TRUE'''
        else
          S:='''FALSE''';

        { Suche nach Abrufart = MomArchiv: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_LAKSAbruf_Dfue + ' = ' + S + ') AND ' +
                        '(' + C_LAKSAbruf_LAKSId + ' = ' + IntToStr(LAKSId) + ') AND ' +
                        '(' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtMomArchiv + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle MomArchiv-Datensätze für LAKSId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_LAKSAbruf_Abrufart).AsString;
          Datentypen:=tbAbruf.FieldByName (C_LAKSAbruf_Datentypen).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = ParaLesen: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_LAKSAbruf_Dfue + ' = ' + S + ') AND ' +
                        '(' + C_LAKSAbruf_LAKSId + ' = ' + IntToStr(LAKSId) + ') AND ' +
                        '(' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtParaLesen + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle ParaLesen-Datensätze für LAKSId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_LAKSAbruf_Abrufart).AsString;
          Datentypen:=tbAbruf.FieldByName (C_LAKSAbruf_Datentypen).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = ParaSenden: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_LAKSAbruf_Dfue + ' = ' + S + ') AND ' +
                        '(' + C_LAKSAbruf_LAKSId + ' = ' + IntToStr(LAKSId) + ') AND ' +
                        '(' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtParaSenden + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle ParaLesen-Datensätze für LAKSId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_LAKSAbruf_Abrufart).AsString;
          Datentypen:=tbAbruf.FieldByName (C_LAKSAbruf_Datentypen).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = MomE: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_LAKSAbruf_Dfue + ' = ' + S + ') AND ' +
                        '(' + C_LAKSAbruf_LAKSId + ' = ' + IntToStr(LAKSId) + ') AND ' +
                        '(' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtMomStop + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        while not tbAbruf.Eof do begin                    { alle MomE-Datensätze für LAKSId }
          Abrufart:=tbAbruf.FieldByName (C_LAKSAbruf_Abrufart).AsString;
          Datentypen:=tbAbruf.FieldByName (C_LAKSAbruf_Datentypen).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        tbAbruf.Filtered:=false;
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }
  end; { if Exists }
end;

{---------------------------------------------------------------------------------------}
function TLAKSAbruf.GetLAKSAbrufMomentanHalten (Dfue: boolean; LAKSId: integer): boolean;
{---------------------------------------------------------------------------------------}
{ LAKS-Abruftabelle nach Einträgen mit Abrufart "Momentanwerte halten" für Dfue und LAKSId
  durchsuchen;
  Übergabe: Dfue
            LAKSId
  Ergebnis: true, wenn Verbindung für Momentanwerte gehalten werden soll }
var
  S: string;
begin
  Result := false;         { sicherheitshalber nicht "Halten" bei Tabellenzugriffsproblemen }
  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        if Dfue then
          S:='''TRUE'''
        else
          S:='''FALSE''';

        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_LAKSAbruf_Dfue + ' = ' + S + ') AND ' +
                        '(' + C_LAKSAbruf_LAKSId + ' = ' + IntToStr(LAKSId) + ') AND ' +
                        '(' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtMomHalten + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }
        try
          while not tbAbruf.Eof do begin              { alle MomHalten-Datensätze für LAKSId }
            Result:=true;
            tbAbruf.Delete;                                 { Datensatz aus Tabelle löschen }
          end;  { while not tbAbruf.eof }
        finally
          tbAbruf.Filtered:=false;
        end;
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }
  end; { if Exists }
end;

{-----------------------------------------------------------------------}
procedure TLAKSAbruf.DeleteLAKSMomentan (Dfue: boolean; LAKSId: integer);
{-----------------------------------------------------------------------}
{ alle noch evtl. verbliebenen Einträgen der beendeten Momentanwertdarstellung aus
  LAKS-Abruftabelle löschen;
  Übergabe: Dfue
            LAKSId }
var
  S: string;
begin
  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        if Dfue then
          S:='''TRUE'''
        else
          S:='''FALSE''';
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_LAKSAbruf_Dfue + ' = ' + S + ') AND ' +
                        '(' + C_LAKSAbruf_LAKSId + ' = ' + IntToStr(LAKSId) + ') AND ' +
                        '((' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtMomHalten + ''') OR ' +
                        ' (' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtParaLesen + ''') OR ' +
                        ' (' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtParaSenden + ''') OR ' +
                        ' (' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtMomArchiv + ''') OR ' +
                        ' (' + C_LAKSAbruf_Abrufart + ' = ''' + C_AbrArtMomStop + '''))';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }
        try
          while not tbAbruf.Eof do
            tbAbruf.Delete;                                 { Datensatz aus Tabelle löschen }
        finally
          tbAbruf.Filtered:=false;
        end;
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }
  end;
end;

{-------------------------}
function TLAKSAbruf.GetRecordCount: integer;
{-------------------------}
begin
  result:= -1; { default }
  if tbAbruf.OpenShared then begin
    try
      result:= tbAbruf.RecordCount;
    except
      tbAbruf.Close;
      if IsDebugFlag then showMessage('Abruf - GetRecCount');
      exit;
    end;
    tbAbruf.Close;
  end;
end;

end.
