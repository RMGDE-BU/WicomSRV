{******************************************************************************}
{* Unit: Zugriffe auf DSfG-Abruftabellen                                      *}
{* 18.11.1999 WW                                                              *}
{* 09.10.2000 GD; InsertToDSfGAbruf                                           *}
{* 05.05.2003 GD; Anpassung für mehrere TCP/IP-Verbindungen                   *}
{* 12.05.2003 GD; Archivabruf bei stehender Momentanwertverbindung            *}
{******************************************************************************}
Unit DDbAbruf;

INTERFACE

Uses
  Dialogs, SysUtils, Db, DbTables, WTables, WSysCon, AuftrgDB, SrvCfgIni,
  DListen;

const

  { Tabellennamen }

  CDBDSfGAbruf = 'AbrfDSfG.db';                                { Haupttabelle }
  CDBADManu    = 'ADManu.db';             { Detailtabelle für manuellen Abruf }

  { Tabelle 'DSFGABRUF.DB' }

  C_DSfGAbruf_Schnittstelle = 'Schnittstelle';
  C_DSfGAbruf_StationId     = 'StationId';
  C_DSfGAbruf_Abrufart      = 'Abrufart';
  C_DSfGAbruf_Datentypen    = 'Datentypen';
  C_DSfGAbruf_DatenVon      = 'DatenVon';
  C_DSfGAbruf_DatenBis      = 'DatenBis';
  C_DSfGAbruf_Anrufversuch  = 'Anrufversuch';

  { Tabelle 'ADMANU.DB' }

  C_ADManu_StationId     = 'StationId';
  C_ADManu_InstanzId     = 'InstanzId';
  C_ADManu_ArchLogbNr    = 'ArchLogbNr';
  C_ADManu_KanalNr       = 'KanalNr';
  C_ADManu_DelVon        = 'DelVon';
  C_ADManu_DelBis        = 'DelBis';
  C_ADManu_OrdNrVon      = 'OrdNrVon';
  C_ADManu_OrdNrBis      = 'OrdNrBis';
  C_ADManu_Datentyp      = 'Datentyp';


type

  { Objekt für Zugriff auf DSfGAbrf.db }

  TDSfGAbruf = class (TObject)
  private
    Path: TFileName;
    tbAbruf: TTableExt;
    procedure CreateDSfGAbrufDB;
  public
    constructor Create (APath: TFileName);
    destructor Destroy; override;
    function EmptyDSfGAbrufTable: boolean;
    function InsertDSfGAbruf(aQuery: TQuery): boolean;
    function InsertToDSfGAbruf(anId, aLogPort, aDaten, aVersuch: integer;
      anAbrufArt: string; Von, Bis: TDateTime): boolean;  // 09.10.2000
    function IsDSfGAbruf(anId: integer; aLogPort: integer; anAbrufArt: string): boolean;
    function GetDSfGAbruf (COMNr: integer; var Abrufart: TAbrufart;  var AbrufData: TAbrufData): boolean;
    function DeleteDSfGAutomatikAbrufe: boolean;
    function GetRecordCount: integer;
    function GetDSfGAbrufMomentan (StationId: integer; var Abrufart: string): boolean;
    function GetDSfGAbrufMomentanHalten (StationId: integer): boolean;
    procedure DeleteDSfGMomentan (StationId: integer);
    function GetDSfGAbrufDfueMomentan (StationId: integer; var Abrufart: string): boolean;
    function GetDSfGAbrufDfueMomentanHalten (StationId: integer): boolean;
    procedure DeleteDSfGDfueMomentan (StationId: integer);
    procedure DeleteDSfGAbrufe (StationId: integer);
  end;

  { Objekt für Zugriff auf ADManu.db }

  TADManu = class (TObject)
  private
    Path: TFileName;
    tbManu: TTableExt;
    procedure CreateADManuDB;
  public
    constructor Create (APath: TFileName);
    destructor Destroy; override;
    function EmptyADManuTable: boolean;
    procedure DeleteDSfGManuAbruf (StationId: integer);
    procedure DeleteDSfGManuAbruf_Instanz (InstanzId: integer);
    procedure DeleteDSfGManuAbruf_Archiv (InstanzId: integer; ArchivNr: integer);
    procedure DeleteDSfGManuAbruf_Archivkanal (InstanzId: integer; ArchivNr: integer; KanalNr: integer);
    procedure DeleteDSfGManuAbruf_Logbuch (InstanzId: integer; LogbuchNr: integer);
    procedure Get_DSfGManuAbrufliste (StationId: integer; Datentyp: integer; var AbrufListe: TAbrufList);
    Procedure Set_AppendArchivKanal(StationId:integer;InstanzId:integer;ArchLogbNr:integer;KanalNr:integer;ONrVon:integer;ONrBis:Integer);
    Procedure Set_AppendLogbuch(StationId:integer;InstanzId:integer;ArchLogbNr:integer;ONrVon:integer;ONrBis:Integer);
    Procedure Set_AppendDEL(StationId:integer;InstanzId:integer;DELVOn,DELBis:string);
    function Get_StaIdRecCount(iStationsId: integer): integer;  // 12.05.2003
  end;


IMPLEMENTATION

uses
  DDbSta;

{-----------------------------------------------}
constructor TDSfGAbruf.Create (APath: TFileName);
{-----------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  tbAbruf:= TTableExt.Create(nil);
  tbAbruf.DataBaseName:= Path;
  tbAbruf.TableName:= CDBDSfGAbruf;

  if not tbAbruf.Exists then
    CreateDSfGAbrufDB;                           { Tabelle automatisch anlegen }
end;

{----------------------------}
destructor TDSfGAbruf.Destroy;
{----------------------------}
begin
  tbAbruf.Free;
  inherited Destroy;
end;

{-------------------------------------}
procedure TDSfGAbruf.CreateDSfGAbrufDB;
{-------------------------------------}
{ DSfG-Abruf-Tabelle anlegen }
begin
  with tbAbruf.FieldDefs do begin
    Clear;
    Add(C_DSfGAbruf_Schnittstelle, ftSmallint, 0, false);
    Add(C_DSfGAbruf_StationId, ftInteger, 0, false);
    Add(C_DSfGAbruf_Abrufart, ftString, 10, false);
    Add(C_DSfGAbruf_Datentypen, ftInteger, 0, false);
    Add(C_DSfGAbruf_DatenVon, ftDateTime, 0, false);
    Add(C_DSfGAbruf_DatenBis, ftDateTime, 0, false);
    Add(C_DSfGAbruf_Anrufversuch, ftSmallInt, 0, false);
  end;
  tbAbruf.IndexDefs.Clear;                                       { kein Index }
  tbAbruf.CreateTable;
end;

{ Leert die Abruf-Tabelle           }
{-----------------------------------------------}
function TDSfGAbruf.EmptyDSfGAbrufTable: boolean;
{-----------------------------------------------}
begin
  try
    tbAbruf.Active:= False;
    result:= tbAbruf.EmptyTable;
  except
    result:= False;
  end;
end;

{ DSfG-Aufträge aus einem Query in die DSfG-Abruf-Tabelle einfügen }
{ Inkrementieren der Anrufversuche                               }
{ Übergabe: Query mit einzufügenden Daten aus AUFTRAG.DB         }
{-----------------------------------------------------------}
function TDSfGAbruf.InsertDSfGAbruf(aQuery: TQuery): boolean;
{-----------------------------------------------------------}
begin
  result:= False; { default }
  if tbAbruf.OpenShared then begin
    if aQuery.Active then begin
      try
        aQuery.first;
        { Schleife über Übergabe-Query - enthält die Felder aus AUFTRAG.DB }
        while not aQuery.eof do begin
          { nur DSfG-Aufträge eintragen }
          if aQuery.fieldByName(C_TfAGeraeteArt).asString = C_GerArtDSfG then begin
            { Manuelle Aufträge werden hinter dem letzten manuellen eingefügt, automatische angehängt }
            if aQuery.fieldByName(C_TFAAbrufArt).asString <> C_AbrArtAuto then begin
              tbAbruf.first;
              while not tbAbruf.eof do begin
                if tbAbruf.fieldByName(C_DSfGAbruf_Abrufart).asString = C_AbrArtAuto then
                  Break;
                tbAbruf.Next;
              end;
              if tbAbruf.eof then
                tbAbruf.append
              else
                tbAbruf.insert;
            end
            else begin
              tbAbruf.append;
            end;
            tbAbruf.fieldByName(C_DSfGAbruf_Schnittstelle).asInteger:=
              aQuery.FieldByName(C_TFALogport).asInteger;
            tbAbruf.fieldByName(C_DSfGAbruf_StationId).asInteger:=
              aQuery.FieldByName(C_TfAGeraeteId).asInteger;
            tbAbruf.fieldByName(C_DSfGAbruf_Abrufart).asString:=
              aQuery.FieldByName(C_TFAAbrufArt).asString;
            tbAbruf.fieldByName(C_DSfGAbruf_Datentypen).asInteger:=
              aQuery.FieldByName(C_TFAAbrufe).asInteger;
            tbAbruf.fieldByName(C_DSfGAbruf_DatenVon).asDateTime:=
              aQuery.FieldByName(C_TfAVonDatum).asDateTime;
            tbAbruf.fieldByName(C_DSfGAbruf_DatenBis).asDateTime:=
              aQuery.FieldByName(C_TfABisDatum).asDateTime;
            tbAbruf.fieldByName(C_DSfGAbruf_Anrufversuch).asInteger:=
              aQuery.FieldByName(C_TfADurchlaufNr).asInteger + 1;
            tbAbruf.post;
            result:= True; { Wenn mind. 1 Auftrag eingetragen }
          end;
          aQuery.next;
        end;
      except
        tbAbruf.Close;
        exit;
      end;
    end;
    tbAbruf.close;
  end;
end;

{ Überprüft, ob ein Eintrag mit übergebener Id, Schnittstelle und Abrufart }
{ in der AbrfDSfG.Db steht                                  }
{ Parameter: Id, LogPort und Abrufart                       }
{ Rückgabe: True - steht drin; False - steht nicht drin     }
{-----------------------------------------------------------}
function TDSfGAbruf.IsDSfGAbruf(anId: integer; aLogPort: integer; anAbrufArt: string): boolean;
{-----------------------------------------------------------}
var
  q          : TQueryExt;
begin
  result:= False; { default }
  if fileExists(Path + CDBDSfGAbruf) then begin
    q:= TQueryExt.Create(nil);
    try
      q.DatabaseName:= Path;
      q.Sql.Add('SELECT COUNT(*) FROM "' + CDBDSfGAbruf + '"');
      q.Sql.Add('WHERE ' + C_DSfGAbruf_StationId + '= ' + IntToStr(anId));
      q.Sql.Add('AND ' + C_DSfGAbruf_Schnittstelle + ' = ' + IntToStr(aLogPort));
      q.Sql.Add('AND ' + C_DSfGAbruf_Abrufart + '= "' + anAbrufArt + '"');
      if q.open then
        if q.Fields[0].AsInteger > 0 then result:= True;
    except
      on exception do begin
        q.Free;
        if IsDebugFlag then showMessage('DSfGAbruf - IsDSfGAbruf');
        exit;
      end;
    end;
    q.Free;
  end;
end;

{-------------------------------------------------------------------------------------------------------------}
function TDSfGAbruf.GetDSfGAbruf (COMNr: integer; var Abrufart: TAbrufart; var AbrufData: TAbrufData): boolean;
{-------------------------------------------------------------------------------------------------------------}
{ Abrufdaten für einen Abruf aus Tabelle lesen; gelesenen Datensatz aus Tabelle löschen }
{ Übergabe: COMNr
  Rückgabe: Abrufart
            AbrufData
  Ergebnis: true, wenn DSfG-Abruf gelesen werden konnte }
Const
  CSx = 0;                               { beliebige serielle Schnittstelle }

var
  TempSchnittstelle: integer;
  TempAbrufart: string;

begin
  Result := false;
  FillChar (AbrufData, SizeOf (TAbrufData), 0);                 { Vorbelegung für AbrufData }

  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        while not tbAbruf.Eof do begin
          TempAbrufart := tbAbruf.FieldByName(C_DSfGAbruf_Abrufart).AsString;
          { "Momentanwerte beenden, halten" und "DFÜ-Parametrierung beenden, halten"
            ignorieren: }
          if (TempAbrufart = C_AbrArtMomStop) OR (TempAbrufart = C_AbrArtMomHalten) OR
             (TempAbrufart = C_AbrArtMomDfueStop) OR (TempAbrufart = C_AbrArtMomDfueHalten) then begin
            tbAbruf.Next;
            Continue;
          end;

          TempSchnittstelle:=tbAbruf.FieldByName(C_DSfGAbruf_Schnittstelle).AsInteger;
          if (TempSchnittstelle >= Low (LogPhysComZuordnung)) AND
             (TempSchnittstelle <= High (LogPhysComZuordnung)) then begin
            { TempSchnittstelle ist feste serielle Schnittstelle:
              aus der in der Tabelle enthaltenen logischen Schnittstelle die physikalische
              ermitteln und mit übergebener physikalischer Schnittstelle vergleichen }
            if LogPhysComZuordnung [TempSchnittstelle] <> COMNr then begin
              tbAbruf.Next;
              Continue;
            end;
          end
          else begin   { TempSchnittstelle ist Sx oder TCP/IP-Pseudo-Schnittstellennummer }
            if (TempSchnittstelle div 100 = CComTCP_IP div 100) then begin  // 05.05.2003
              { Pseudo-Schnittstellennummer für TCP/IP-Kommunikation:
                mit übergebener physikalischer Schnittstelle vergleichen (bei TCP/IP
                ist die logische und "physikalische" Schnittstellenummer identisch) }
              if TempSchnittstelle <> COMNr then begin
                tbAbruf.Next;
                Continue;
              end;
            end;
            { wahlfreie Schnittstelle Sx nicht bei TCP/IP-Abruf: }
            if (TempSchnittstelle = CSx) AND (COMNr div 100 = CComTCP_IP  div 100)  // 05.05.2003
            then begin
              tbAbruf.Next;
              Continue;
            end;
          end;

          Result:=true;
          AbrufData.StationId := tbAbruf.FieldByName(C_DSfGAbruf_StationId).AsInteger;
          AbrufData.Datentypen := tbAbruf.FieldByName(C_DSfGAbruf_Datentypen).AsInteger;
          if not tbAbruf.FieldByName(C_DSfGAbruf_DatenVon).IsNull then
            AbrufData.DatenVon := tbAbruf.FieldByName(C_DSfGAbruf_DatenVon).AsDateTime
          else
            AbrufData.DatenVon := 0;
          if not tbAbruf.FieldByName(C_DSfGAbruf_DatenBis).IsNull then
            AbrufData.DatenBis := tbAbruf.FieldByName(C_DSfGAbruf_DatenBis).AsDateTime
          else
            AbrufData.DatenBis := 0;
          AbrufData.Anrufversuch := tbAbruf.FieldByName(C_DSfGAbruf_Anrufversuch).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
          Break;
        end;  { while not tbAbruf.eof }
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }

    { Rückgabe belegen: Abrufart }
    if TempAbrufart = C_AbrArtAuto then
      Abrufart := aa_automatisch
    else if TempAbrufart = C_AbrArtKonfLesen then
      Abrufart := aa_konflesen
    else if TempAbrufart = C_AbrArtMomStart then
      Abrufart := aa_momentan
    else if TempAbrufart = C_AbrArtMomDfueStart then
      Abrufart := aa_dfue_momentan
    else if TempAbrufart = C_AbrArtRufReakt then
      Abrufart := aa_rufreakt
    else
      Abrufart := aa_manuell;
  end; { if tbAbruf.Exists }
end;

{-----------------------------------------------------}
function TDSfGAbruf.DeleteDSfGAutomatikAbrufe: boolean;
{-----------------------------------------------------}
{ alle Automatik-Abrufe aus Tabelle löschen }
{ Ergebnis: true, wenn mind. 1 DSfG-Abruf gelöscht wurde }
begin
  Result := false;

  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        { Filter auf Automatik-Aufträge: }
        tbAbruf.Filtered:=false;                   { Deaktivieren des Filters }
        tbAbruf.Filter:=C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtAuto + '''';
        tbAbruf.Filtered:=true;                      { Aktivieren des Filters }

        while not tbAbruf.Eof do begin
          Result:=true;
          tbAbruf.Delete;                     { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
      finally
        tbAbruf.Filtered:=false;                   { Deaktivieren des Filters }
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }
  end; { if tbAbruf.Exists }
end;

{-------------------------}
function TDSfGAbruf.GetRecordCount: integer;
{-------------------------}
begin
  result:= -1; { default }
  if tbAbruf.OpenShared then begin
    try
      result:= tbAbruf.RecordCount;
    except
      tbAbruf.Close;
      if IsDebugFlag then showMessage('DSfGAbruf - GetRecCount');
      exit;
    end;
    tbAbruf.Close;
  end;
end;

{-----------------------------------------------------------------------}
function TDSfGAbruf.GetDSfGAbrufMomentan (StationId: integer;
                                          var Abrufart: string): boolean;
{-----------------------------------------------------------------------}
{ DSfG-Abruftabelle nach Einträgen für laufende Momentanwertdarstellung durchsuchen;
  Suchpriorität: ParaStart, ParaSenden, BinaerSenden, MomE, ParaEnde
  Übergabe: StationId
  Rückgabe: Abrufart
  Ergebnis: false, wenn Tabelle nicht gelesen werden konnte }
var
  gefunden: boolean;

begin
  Result:=false;
  Abrufart:='';

  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        Result:=true;
        { Suche nach Abrufart = ParaStart: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaStart + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle ParaStart-Datensätze für StationId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_DSfGAbruf_Abrufart).AsString;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = ParaSenden: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaSenden + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle ParaSenden-Datensätze für StationId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_DSfGAbruf_Abrufart).AsString;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = BinaerSenden: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtBinaerSenden + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle ParaSenden-Datensätze für StationId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_DSfGAbruf_Abrufart).AsString;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = MomE: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtMomStop + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin                    { alle MomE-Datensätze für StationId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_DSfGAbruf_Abrufart).AsString;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = ParaEnde: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaEnde + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        while not tbAbruf.Eof do begin               { alle ParaEnde-Datensätze für StationId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_DSfGAbruf_Abrufart).AsString;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = C_AbrArtAuto: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtAuto + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        while not tbAbruf.Eof do begin               { alle ParaEnde-Datensätze für StationId }
          Abrufart:=tbAbruf.FieldByName (C_DSfGAbruf_Abrufart).AsString;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }
  end; { if Exists }
end;

{---------------------------------------------------------------------------}
function TDSfGAbruf.GetDSfGAbrufMomentanHalten (StationId: integer): boolean;
{---------------------------------------------------------------------------}
{ DSfG-Abruftabelle nach Einträgen mit Abrufart "Momentanwerte halten" für StationId
  durchsuchen;
  Übergabe: StationId
  Ergebnis: true, wenn Verbindung für Momentanwerte gehalten werden soll }
begin
  Result := false;         { sicherheitshalber nicht "Halten" bei Tabellenzugriffsproblemen }
  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtMomHalten + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }
        try
          while not tbAbruf.Eof do begin              { alle MomHalten-Datensätze für StationId }
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

{-----------------------------------------------------------}
procedure TDSfGAbruf.DeleteDSfGMomentan (StationId: integer);
{-----------------------------------------------------------}
{ alle noch evtl. verbliebenen Einträge der beendeten Momentanwertdarstellung aus
  DSfG-Abruftabelle löschen;
  Übergabe: StationId }
begin
  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        tbAbruf.Filtered:=false;                                  { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '((' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtMomHalten + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaStart + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaSenden + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaEnde + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtBinaerSenden + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtMomStop + '''))';
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

{---------------------------------------------------------------------------}
function TDSfGAbruf.GetDSfGAbrufDfueMomentan (StationId: integer;
                                              var Abrufart: string): boolean;
{---------------------------------------------------------------------------}
{ DSfG-Abruftabelle nach Einträgen für laufende DFÜ-Momentanwertdarstellung durchsuchen;
  Suchpriorität: ParaStart, ParaSenden, BinaerSenden, MomDfueE, ParaEnde
  Übergabe: StationId
  Rückgabe: Abrufart
  Ergebnis: false, wenn Tabelle nicht gelesen werden konnte }
var
  gefunden: boolean;

begin
  Result:=false;
  Abrufart:='';

  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        Result:=true;
        { Suche nach Abrufart = ParaSenden: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaSenden + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle ParaSenden-Datensätze für StationId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_DSfGAbruf_Abrufart).AsString;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = BinaerSenden: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtBinaerSenden + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle Binaer-Datensätze für StationId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_DSfGAbruf_Abrufart).AsString;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = MomE: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtMomDfueStop + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        while not tbAbruf.Eof do begin               { alle MomDfueE-Datensätze für StationId }
          Abrufart:=tbAbruf.FieldByName (C_DSfGAbruf_Abrufart).AsString;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }

        tbAbruf.Filtered:=false;
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }
  end; { if Exists }
end;

{-------------------------------------------------------------------------------}
function TDSfGAbruf.GetDSfGAbrufDfueMomentanHalten (StationId: integer): boolean;
{-------------------------------------------------------------------------------}
{ DSfG-Abruftabelle nach Einträgen mit Abrufart "DFÜ-Momentanwerte halten" für StationId
  durchsuchen;
  Übergabe: StationId
  Ergebnis: true, wenn Verbindung für DFÜ-Momentanwerte gehalten werden soll }
begin
  Result := false;         { sicherheitshalber nicht "Halten" bei Tabellenzugriffsproblemen }
  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '(' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtMomDfueHalten + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }
        try
          while not tbAbruf.Eof do begin              { alle MomDfueHalten-Datensätze für StationId }
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

{---------------------------------------------------------------}
procedure TDSfGAbruf.DeleteDSfGDfueMomentan (StationId: integer);
{---------------------------------------------------------------}
{ alle noch evtl. verbliebenen Einträge des beendeten DFÜ-Momentanwertabrufs aus
  DSfG-Abruftabelle löschen;
  Übergabe: StationId }
begin
  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        tbAbruf.Filtered:=false;                                  { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                        '((' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtMomDfueHalten + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaStart + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaSenden + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtParaEnde + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtBinaerSenden + ''') OR ' +
                        ' (' + C_DSfGAbruf_Abrufart + ' = ''' + C_AbrArtMomDfueStop + '''))';
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


// 09.10.2000
{ Parameter für DSfG-Abruf direkt in Abruf-Tabelle einfügen }
{ Parameter:
{-----------------------------------------------------------}
function TDSfGAbruf.InsertToDSfGAbruf(anId, aLogPort, aDaten, aVersuch: integer;
                                    anAbrufArt: string; Von, Bis: TDateTime): boolean;
{-----------------------------------------------------------}
begin
  Result := False; { default }

  if (tbAbruf.OpenShared) then
  try
  // Manuelle Aufträge werden hinter dem letzten manuellen eingefügt, automatische angehängt
    if (anAbrufArt <> C_AbrArtAuto) then begin
      tbAbruf.first;
      while not tbAbruf.eof do begin
        if (tbAbruf.FieldByName(C_DSfGAbruf_Abrufart).asString = C_AbrArtAuto) then
          Break;
        tbAbruf.Next;
      end;
      if (tbAbruf.Eof) then tbAbruf.Append else tbAbruf.Insert;
    end
    else tbAbruf.append;

  // Daten eintragen
    tbAbruf.FieldByName(C_DSfGAbruf_Schnittstelle).asInteger:= aLogPort;
    tbAbruf.FieldByName(C_DSfGAbruf_StationId).asInteger:= anId;
    tbAbruf.FieldByName(C_DSfGAbruf_Abrufart).asString:= anAbrufArt;
    tbAbruf.FieldByName(C_DSfGAbruf_Datentypen).asInteger:=aDaten;
    tbAbruf.FieldByName(C_DSfGAbruf_DatenVon).asDateTime:= Von;
    tbAbruf.FieldByName(C_DSfGAbruf_DatenBis).asDateTime:= Bis;
    tbAbruf.FieldByName(C_DSfGAbruf_Anrufversuch).asInteger:= aVersuch;
    tbAbruf.Post;
    Result := True; { Wenn mind. 1 Auftrag eingetragen }
  except
    tbAbruf.Close;
    Exit;
  end;
  tbAbruf.Close;
end;

{---------------------------------------------------------}
procedure TDSfGAbruf.DeleteDSfGAbrufe (StationId: integer);
{---------------------------------------------------------}
{ alle Einträge einer Station aus DSfG-Abruftabelle löschen;
  Übergabe: StationId }
begin
  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        tbAbruf.Filtered:=false;                   { Deaktivieren des Filters }
        tbAbruf.Filter:=C_DSfGAbruf_StationId + ' = ' + IntToStr(StationId);  { Filter auf StationId }
        tbAbruf.Filtered:=true;                      { Aktivieren des Filters }
        try
          while not tbAbruf.Eof do
            tbAbruf.Delete;                   { Datensatz aus Tabelle löschen }
        finally
          tbAbruf.Filtered:=false;
        end;
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }
  end;
end;


{ TADManu }

{--------------------------------------------}
constructor TADManu.Create (APath: TFileName);
{--------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  tbManu:= TTableExt.Create(nil);
  tbManu.DataBaseName:= Path;
  tbManu.TableName:= CDBADManu;

  if not tbManu.Exists then
    CreateADManuDB;                             { Tabelle automatisch anlegen }
end;

{-------------------------}
destructor TADManu.Destroy;
{-------------------------}
begin
  tbManu.Free;
  inherited Destroy;
end;

{-------------------------------}
procedure TADManu.CreateADManuDB;
{-------------------------------}
{ Detailtabelle für manuellen Abruf anlegen }
begin
  with tbManu.FieldDefs do begin
    Clear;
    Add(C_ADManu_StationId, ftInteger, 0, false);
    Add(C_ADManu_InstanzId, ftInteger, 0, false);
    Add(C_ADManu_ArchLogbNr, ftInteger, 0, false);
    Add(C_ADManu_KanalNr, ftInteger, 0, false);
    Add(C_ADManu_DelVon, ftString, 10, false);
    Add(C_ADManu_DelBis, ftString, 10, false);
    Add(C_ADManu_OrdNrVon, ftInteger, 0, false);
    Add(C_ADManu_OrdNrBis, ftInteger, 0, false);
    Add(C_ADManu_Datentyp, ftInteger, 0, false);
  end;
  tbManu.IndexDefs.Clear;                                       { kein Index }
  tbManu.CreateTable;
end;

{ Leert die ADManu-Tabelle           }
{-----------------------------------------------}
function TADManu.EmptyADManuTable: boolean;
{-----------------------------------------------}
begin
  try
    tbManu.Active:= False;
    result:= tbManu.EmptyTable;
  except
    result:= False;
  end;
end;

{-----------------------------------------------------------------------------------------------------------}
procedure TADManu.Get_DSfGManuAbrufliste (StationId: integer; Datentyp: integer; var AbrufListe: TAbrufList);
{-----------------------------------------------------------------------------------------------------------}
{ Abrufdaten für einen manuellen Abruf aus ADManu-Tabelle lesen; übrige für Abrufliste benötigte Daten aus
  Stammdaten lesen; gelesene Datensätze aus ADManu löschen }
{ Übergabe: StationId
            Datentyp (z.B. C_IsArchive)
  Rückgabe: AbrufListe
  Ergebnis: true, wenn manuelle Abrufdaten gelesen werden konnten }
const
  GS = #29;

var
  AbrufListObj: TAbrufListObj;
  InstId: integer;
  Del_von: string;
  Del_bis: string;
  AddEintrag: boolean;
  i: integer;
  InstanzData: TInstanzData;
  AKanaeleData: TAKanaeleData;
  LogbuchData: TLogbuchData;
  Stammdaten: TDSfGStammdaten;

begin
  if tbManu.Exists then begin
    Stammdaten:=TDSfGStammdaten.Create (Path);
    try
      if Stammdaten.InitTabellen then begin
        if tbManu.OpenExclusive then begin
          try
            { Filter auf StationId und Datentyp }
            tbManu.Filter:='(' + C_ADManu_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                           '(' + C_ADManu_Datentyp + ' = ' + IntToStr(Datentyp) + ')';
            tbManu.Filtered:=true;

            while not tbManu.Eof do begin
              if Stammdaten.GetInstanzData (tbManu.FieldByName(C_ADManu_InstanzId).AsInteger, InstanzData) then begin
                if InstanzData.Busadresse <> '' then begin
                  case Datentyp of
                    C_IsArchive: begin
                                   if Stammdaten.GetAKanaeleData (tbManu.FieldByName(C_ADManu_InstanzId).AsInteger,
                                                                  tbManu.FieldByName(C_ADManu_ArchLogbNr).AsInteger,
                                                                  tbManu.FieldByName(C_ADManu_KanalNr).AsInteger,
                                                                  AKanaeleData) then begin
                                     AbrufListObj:=TAbrufListObj.Create;
                                     AbrufListObj.SetData (InstanzData.Busadresse,
                                                           tbManu.FieldByName(C_ADManu_InstanzId).AsInteger,
                                                           tbManu.FieldByName(C_ADManu_ArchLogbNr).AsInteger,
                                                           tbManu.FieldByName(C_ADManu_KanalNr).AsInteger,
                                                           AKanaeleData.Kanaltyp,
                                                           AKanaeleData.Werteart,
                                                           '', '', '',     { für Archive nicht relevant }
                                                           tbManu.FieldByName(C_ADManu_OrdNrVon).AsInteger,
                                                           tbManu.FieldByName(C_ADManu_OrdNrBis).AsInteger,
                                                           -1, -1);        { für Archive nicht relevant }
                                     AbrufListe.Add (AbrufListObj);
                                   end;
                                 end;
                    C_IsLogbuecher: begin
                                      if Stammdaten.GetLogbuchData (tbManu.FieldByName(C_ADManu_InstanzId).AsInteger,
                                                                    tbManu.FieldByName(C_ADManu_ArchLogbNr).AsInteger,
                                                                    LogbuchData) then begin
                                        if LogbuchData.EAdr <> '' then begin
                                          AbrufListObj:=TAbrufListObj.Create;
                                          AbrufListObj.SetData (InstanzData.Busadresse,
                                                                tbManu.FieldByName(C_ADManu_InstanzId).AsInteger,
                                                                tbManu.FieldByName(C_ADManu_ArchLogbNr).AsInteger,
                                                                -1, '', '',   { für Logbücher nicht relevant }
                                                                LogbuchData.EAdr,
                                                                '', '',       { für Logbücher nicht relevant }
                                                                tbManu.FieldByName(C_ADManu_OrdNrVon).AsInteger,
                                                                tbManu.FieldByName(C_ADManu_OrdNrBis).AsInteger,
                                                                -1, -1);      { Id und Gerätetyp-Nr. der Quell-Instanz hier nur vorbelegen }
                                          AbrufListe.Add (AbrufListObj);
                                        end;
                                      end;
                                    end;
                    C_IsDatenelemente: begin
                                         InstId:=tbManu.FieldByName(C_ADManu_InstanzId).AsInteger;
                                         Del_von:=tbManu.FieldByName(C_ADManu_DelVon).AsString;
                                         Del_bis:=tbManu.FieldByName(C_ADManu_DelBis).AsString;
                                         AddEintrag:=true;                                       { Vorbelegung: neuer Eintrag }
                                         if Del_bis = '' then begin                                  { einzelnes Datenelement }
                                           for i:=0 to AbrufListe.Count - 1 do begin { Eintrag für Einzelelement schon vorhanden ? }
                                             if (TAbrufListObj (AbrufListe [i]).EAdr = InstanzData.Busadresse) AND
                                                (TAbrufListObj (AbrufListe [i]).InstanzId = InstId) AND
                                                (TAbrufListObj (AbrufListe [i]).Del_bis = '') then begin
                                               { Länge noch unter 8k-Grenze ? }
                                               if length (TAbrufListObj (AbrufListe [i]).Del_von) < CMaxDSfGTelegrammLaenge then begin
                                                 TAbrufListObj (AbrufListe [i]).Del_von:=TAbrufListObj (AbrufListe [i]).Del_von +
                                                                                         GS + Del_von;    { Befehl erweitern }
                                                 AddEintrag:=false;
                                               end;
                                               Break;
                                             end;
                                           end;  { for }
                                         end;
                                         if AddEintrag then begin                                    { neuen Eintrag einfügen }
                                           AbrufListObj:=TAbrufListObj.Create;
                                           AbrufListObj.SetData (InstanzData.Busadresse,
                                                                 InstId,
                                                                 -1, -1, '', '', '',  { für Datenelemente nicht relevant }
                                                                 Del_von,
                                                                 Del_bis,
                                                                 0, 0, -1, -1);       { für Datenelemente nicht relevant }
                                           AbrufListe.Add (AbrufListObj);
                                         end;
                                       end;
                  end;
                end;  { if InstanzData.Busadresse <> '' }
              end;  { if Stammdaten.GetInstanzdata }
              tbManu.Delete;
            end;  { while not tbAbruf.eof }
          finally
            tbManu.Filtered:=false;
            tbManu.Close;
          end;
        end;  { if OpenExclusive }

        { jetzt noch die Id und Gerätetyp-Nr. der Quell-Instanzen in der Abrufliste
          setzen (vorerst nur bei Logbüchern): }
        if Datentyp = C_IsLogbuecher then
          Stammdaten.SetAbrufList_InstanzId_GerTypNrQuelle (StationId, AbrufListe);
      end;  { if InitTabellen }
    finally
      Stammdaten.Free;
    end;
  end; { if tbManu.Exists }
end;

{---------------------------------------------------------}
procedure TADManu.DeleteDSfGManuAbruf (StationId: integer);
{---------------------------------------------------------}
{ Abrufdaten für einen manuellen Abruf einer Station aus ADManu-Tabelle löschen;
  Übergabe: StationId }
begin
  if tbManu.Exists then begin
    if tbManu.OpenExclusive then begin
      try
        { Filter auf StationId }
        tbManu.Filter:=C_ADManu_StationId + ' = ' + IntToStr(StationId);
        tbManu.Filtered:=true;
        while not tbManu.Eof do
          tbManu.Delete;
      finally
        tbManu.Filtered:=false;
        tbManu.Close;
      end;
    end;  { if OpenExclusive }
  end; { if tbManu.Exists }
end;

{-----------------------------------------------------------------}
procedure TADManu.DeleteDSfGManuAbruf_Instanz (InstanzId: integer);
{-----------------------------------------------------------------}
{ Abrufdaten für einen manuellen Abruf einer Instanz aus ADManu-Tabelle löschen;
  Übergabe: InstanzId }
begin
  if tbManu.Exists then begin
    if tbManu.OpenExclusive then begin
      try
        { Filter auf InstanzId }
        tbManu.Filter:=C_ADManu_InstanzId + ' = ' + IntToStr(InstanzId);
        tbManu.Filtered:=true;
        while not tbManu.Eof do
          tbManu.Delete;
      finally
        tbManu.Filtered:=false;
        tbManu.Close;
      end;
    end;  { if OpenExclusive }
  end; { if tbManu.Exists }
end;

{-----------------------------------------------------------------------------------}
procedure TADManu.DeleteDSfGManuAbruf_Archiv (InstanzId: integer; ArchivNr: integer);
{-----------------------------------------------------------------------------------}
{ Abrufdaten für einen manuellen Abruf einer Archivgruppe aus ADManu-Tabelle löschen;
  Übergabe: InstanzId
            ArchivNr }
begin
  if tbManu.Exists then begin
    if tbManu.OpenExclusive then begin
      try
        { Filter auf InstanzId, ArchivNr, KanalNr > 0:  Archiv-Eintrag }
        tbManu.Filter:='(' + C_ADManu_InstanzId + ' = ' + IntToStr(InstanzId) + ') AND ' +
                       '(' + C_ADManu_ArchLogbNr + ' = ' + IntToStr(ArchivNr) + ') AND ' +
                       '(' + C_ADManu_KanalNr + ' > 0)';
        tbManu.Filtered:=true;
        while not tbManu.Eof do
          tbManu.Delete;
      finally
        tbManu.Filtered:=false;
        tbManu.Close;
      end;
    end;  { if OpenExclusive }
  end; { if tbManu.Exists }
end;

{--------------------------------------------------------------------------------------}
procedure TADManu.DeleteDSfGManuAbruf_Archivkanal (InstanzId: integer;
                                                   ArchivNr: integer; KanalNr: integer);
{--------------------------------------------------------------------------------------}
{ Abrufdaten für einen manuellen Abruf eines Archivkanals aus ADManu-Tabelle löschen;
  Übergabe: InstanzId
            ArchivNr
            KanalNr }
begin
  if tbManu.Exists then begin
    if tbManu.OpenExclusive then begin
      try
        { Filter auf StationId, InstanzId, ArchivNr, KanalNr }
        tbManu.Filter:='(' + C_ADManu_InstanzId + ' = ' + IntToStr(InstanzId) + ') AND ' +
                       '(' + C_ADManu_ArchLogbNr + ' = ' + IntToStr(ArchivNr) + ') AND ' +
                       '(' + C_ADManu_KanalNr + ' = ' + IntToStr(KanalNr) + ')';
        tbManu.Filtered:=true;
        while not tbManu.Eof do
          tbManu.Delete;
      finally
        tbManu.Filtered:=false;
        tbManu.Close;
      end;
    end;  { if OpenExclusive }
  end; { if tbManu.Exists }
end;

{-------------------------------------------------------------------------------------}
procedure TADManu.DeleteDSfGManuAbruf_Logbuch (InstanzId: integer; LogbuchNr: integer);
{-------------------------------------------------------------------------------------}
{ Abrufdaten für einen manuellen Abruf eines Logbuchs aus ADManu-Tabelle löschen;
  Übergabe: InstanzId
            LogbuchNr }
begin
  if tbManu.Exists then begin
    if tbManu.OpenExclusive then begin
      try
        { Filter auf StationId, InstanzId, LogbuchNr, KanalNr < 0:  Logbuch-Eintrag }
        tbManu.Filter:='(' + C_ADManu_InstanzId + ' = ' + IntToStr(InstanzId) + ') AND ' +
                       '(' + C_ADManu_ArchLogbNr + ' = ' + IntToStr(LogbuchNr) + ') AND ' +
                       '(' + C_ADManu_KanalNr + ' < 0)';
        tbManu.Filtered:=true;
        while not tbManu.Eof do
          tbManu.Delete;
      finally
        tbManu.Filtered:=false;
        tbManu.Close;
      end;
    end;  { if OpenExclusive }
  end; { if tbManu.Exists }
end;

{------------------------------------------------------}
{ Einfügen eines Archivkanals in die ADMANU            }
Procedure TADManu.Set_AppendArchivKanal(StationId:integer;InstanzId:integer;ArchLogbNr:integer;KanalNr:integer;ONrVon:integer;ONrBis:Integer);
Var q:TQueryExt;
begin
   q:=TQueryExt.Create(nil);
   Try
       with q do begin
         Q.DataBaseName:=Path;
         Close;
         Sql.Clear;
         if ((ONrVon > 0) and (ONrBis > 0)) then begin  // Abfrage nach Ordnungsnummer
             Sql.Add('Insert into ADMANU');
             SQL.Add('('+C_ADManu_StationId+','+C_ADManu_InstanzId+','+C_ADManu_ArchLogbNr+','+C_ADManu_KanalNr+','+C_ADManu_DatenTyp+','+C_ADManu_OrdNrVon+','+C_ADManu_OrdNrBis+')');
             Sql.Add('VALUES (:sId,:IId,:AGNr,:KNr,:DTyp,:ONRVon,:ONrBis)');
             ParamByname('SID').AsInteger:=StationId;
             ParamByname('IID').AsInteger:=InstanzId;
             ParamByname('AGNr').AsInteger:=ArchLogBNr;
             ParamByname('KNr').AsInteger:=KanalNR;
             ParamByname('DTyp').AsInteger:=C_IsArchive;
             ParamByname('ONrVon').AsInteger:=ONrVon;
             ParamByname('ONrBis').AsInteger:=ONrBis;
         end
         else begin
             Sql.Add('Insert into ADMANU');
             SQL.Add('('+C_ADManu_StationId+','+C_ADManu_InstanzId+','+C_ADManu_ArchLogbNr+','+C_ADManu_KanalNr+','+C_ADManu_DatenTyp+')');
             Sql.Add('VALUES (:sId,:IId,:AGNr,:KNr,:DTyp)');
             ParamByname('SID').AsInteger:=StationId;
             ParamByname('IID').AsInteger:=InstanzId;
             ParamByname('AGNr').AsInteger:=ArchLogBNr;
             ParamByname('KNr').AsInteger:=KanalNR;
             ParamByname('DTyp').AsInteger:=C_IsArchive;
         end;
         ExecSql;
       end;

   Finally
      q.Free;
   end;
end;

{------------------------------------------------------}
{ Einfügen eines Logbuchs in die ADMANU                }
Procedure TADManu.Set_AppendLogbuch(StationId:integer;InstanzId:integer;ArchLogbNr:integer;ONrVon:integer;ONrBis:Integer);
Var q:TQueryExt;
begin
   q:=TQueryExt.Create(nil);
   Try
       with q do begin
         Q.DataBaseName:=Path;
         Close;
         Sql.Clear;
         if ((ONrVon > 0) and (ONrBis > 0)) then begin  // Abfrage nach Ordnungsnummer
             Sql.Add('Insert into ADMANU');
             SQL.Add('('+C_ADManu_StationId+','+C_ADManu_InstanzId+','+C_ADManu_ArchLogbNr+','+C_ADManu_KanalNR+','+C_ADManu_DatenTyp+','+C_ADManu_OrdNrVon+','+C_ADManu_OrdNrBis+')');
             Sql.Add('VALUES (:sId,:IId,:AGNr,:KNr,:DTyp,:ONRVon,:ONrBis)');
             ParamByname('SID').AsInteger:=StationId;
             ParamByname('IID').AsInteger:=InstanzId;
             ParamByname('AGNr').AsInteger:=ArchLogBNr;
             ParamByname('KNr').AsInteger:=-1;
             ParamByname('DTyp').AsInteger:=C_IsLogbuecher;
             ParamByname('ONrVon').AsInteger:=ONrVon;
             ParamByname('ONrBis').AsInteger:=ONrBis;

         end
         else begin
             Sql.Add('Insert into ADMANU');
             SQL.Add('('+C_ADManu_StationId+','+C_ADManu_InstanzId+','+C_ADManu_ArchLogbNr+','+C_ADManu_KanalNR+','+C_ADManu_DatenTyp+')');
             Sql.Add('VALUES (:sId,:IId,:AGNr,:KNr,:DTyp)');
             ParamByname('SID').AsInteger:=StationId;
             ParamByname('IID').AsInteger:=InstanzId;
             ParamByname('AGNr').AsInteger:=ArchLogBNr;
             ParamByname('KNr').AsInteger:=-1;
             ParamByname('DTyp').AsInteger:=C_IsLogbuecher;
         end;
         ExecSql;
       end;
   Finally
      q.Free;
   end;
end;

{---------------------------------------------------------------------}
{ Einfügen eines Datenelements (einzelnes oder Bereich) in die ADMANU }
Procedure TADManu.Set_AppendDEL(StationId:integer;InstanzId:integer;DELVOn,DELBis:string);
Var q:TQueryExt;
begin
   q:=TQueryExt.Create(nil);
   Try
       with q do begin
         Q.DataBaseName:=Path;
         Close;
         Sql.Clear;
         Sql.Add('Insert into ADMANU');
         SQL.Add('('+C_ADManu_StationId+','+C_ADManu_InstanzId+','+C_ADManu_DELVon+','+C_ADManu_DELBis+','+C_ADManu_DatenTyp+')');
         Sql.Add('VALUES (:sId,:IId,:DV,:DB,:DTyp)');
         ParamByname('SID').AsInteger:=StationId;
         ParamByname('IID').AsInteger:=InstanzId;
         ParamByname('DV').AsString:=DELVon;
         ParamByname('DB').AsString:=DELBis;
         ParamByname('DTyp').AsInteger:=C_IsDatenelemente;
         ExecSql;
       end;
   Finally
      q.Free;
   end;
end;

{---------------------------------------------------------------------}
function TADManu.Get_StaIdRecCount(iStationsId: integer): integer;
{---------------------------------------------------------------------}
begin
  with TQueryExt.Create(nil) do
  try
    DatabaseName := Path;
    Sql.Add('SELECT COUNT(' + C_ADManu_StationId + ')');
    Sql.Add('FROM ' + ChangeFileExt(CDBADManu, ''));
    if (Open) then begin
      Result := Fields[0].asInteger;
      Close;
    end
    else Result := 0;
  finally
    Free;
  end;
end;

end.
