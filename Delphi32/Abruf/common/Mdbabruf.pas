{******************************************************************************}
{* Unit: Zugriffe auf MRG-Abruftabelle                                        *}
{* 08.12.1998 WW                                                              *}
{ 27.04.1999 WW, GD: Inkrementieren der Anrufversuche                          }
{******************************************************************************}
Unit MDBAbruf;

INTERFACE

Uses
  SysUtils, Dialogs, Db, DbTables, WTables, MDBSta, WSysCon, AuftrgDb, SrvCfgIni,
  MDBMRG;

const

  { Tabellennamen }

  CDBMRGAbruf = 'AbrfMrg.db';

  { Tabelle 'MRGABRUF.DB' }

  C_MRGAbruf_Schnittstelle = 'Schnittstelle';
  C_MRGAbruf_MrgId         = 'MrgId';
  C_MRGAbruf_Abrufart      = 'Abrufart';
  C_MRGAbruf_Datentypen    = 'Datentypen';
  C_MRGAbruf_DatenVon      = 'DatenVon';
  C_MRGAbruf_DatenBis      = 'DatenBis';
  C_MRGAbruf_Anrufversuch  = 'Anrufversuch';


type

  TAbrufListe = array [1..MAXDSFGGERAETE] of TAbrufData;

  { Objekt für Zugriff auf MrgAbruf.db }

  TMRGAbruf = class (TObject)
  private
    Path: TFileName;
    tbAbruf: TTableExt;
    procedure CreateMRGAbrufDB;
  public
    constructor Create (APath: TFileName);
    destructor Destroy; override;
    function EmptyMRGAbrufTable: boolean;
    function GetRecordCount: integer;
    function InsertMRGAbruf(aQuery: TQuery): boolean;
    function InsertToMRGAbruf(anId, aLogPort, aDaten, aVersuch: integer;
                               anAbrufArt: string; Von, Bis: TDateTime): boolean;
    function IsMRGAbruf(anId: integer; aLogPort: integer; anAbrufArt: string): boolean;
    function GetMRGAbruf (ComNr: byte; Thema: string; MRGStammdaten: TMRGStammdaten;
                          var Abrufart: TAbrufart; var AbrufListe: TAbrufListe): boolean;
    function DeleteMRGAutomatikAbrufe: boolean;
    function GetMRGAbrufMomentan (MrgId: TMrgId;
                                  var Abrufart: string; var Datentypen: integer): boolean;
    function GetMRGAbrufMomentanHalten (MrgId: TMrgId): boolean;
    procedure DeleteMRGMomentan (MrgId: TMrgId);
  end;


IMPLEMENTATION

{----------------------------------------------}
constructor TMRGAbruf.Create (APath: TFileName);
{----------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  tbAbruf:= TTableExt.create(nil);
  tbAbruf.DataBaseName:= Path;
  tbAbruf.TableName:= CDBMRGAbruf;

  if not FileExists (Path + CDBMRGAbruf) then
    CreateMRGAbrufDB;                           { Tabelle automatisch anlegen }
end;

{---------------------------}
destructor TMRGAbruf.Destroy;
{---------------------------}
begin
  tbAbruf.free;
  inherited destroy;
end;

{-----------------------------------}
procedure TMRGAbruf.CreateMRGAbrufDB;
{-----------------------------------}
{ MRG-Abruf-Tabelle anlegen }
begin
  with tbAbruf.FieldDefs do begin
    Clear;
    Add(C_MRGAbruf_Schnittstelle, ftSmallInt, 0, false);
    Add(C_MRGAbruf_MrgId, ftInteger, 0, false);
    Add(C_MRGAbruf_Abrufart, ftString, 10, false);
    Add(C_MRGAbruf_Datentypen, ftInteger, 0, false);
    Add(C_MRGAbruf_DatenVon, ftDateTime, 0, false);
    Add(C_MRGAbruf_DatenBis, ftDateTime, 0, false);
    Add(C_MRGAbruf_Anrufversuch, ftSmallInt, 0, false);
  end;
  tbAbruf.IndexDefs.Clear;                                       { kein Index }
  tbAbruf.CreateTable;
end;

{ Leert die Abruf-Tabelle           }
{-----------------------------------}
function TMRGAbruf.EmptyMRGAbrufTable: boolean;
{-----------------------------------}
begin
  try
    tbAbruf.Active:= False;
    result:= tbAbruf.EmptyTable;
  except
    result:= False;
  end;
end;

{ MRG-Aufträge aus einem Query in die MRG-Abruf-Tabelle einfügen }
{ Inkrementieren der Anrufversuche                               }
{ Übergabe: Query mit einzufügenden Daten aus AUFTRAG.DB         }
{---------------------------------------------------------}
function TMRGAbruf.InsertMRGAbruf(aQuery: TQuery): boolean;
{---------------------------------------------------------}
begin
  result:= False; { default }
  if tbAbruf.OpenShared then begin
    if aQuery.Active then begin
      try
        aQuery.first;
        { Schleife über Übergabe-Query - enthält die Felder aus AUFTRAG.DB }
        while not aQuery.eof do begin
          { nur MRG-Aufträge eintragen }
          if aQuery.fieldByName(C_TfAGeraeteArt).asString = C_GerArtMrg then begin
            { Manuelle Aufträge werden hinter dem letzten manuellen eingefügt, automatische angehängt }
            if aQuery.fieldByName(C_TFAAbrufArt).asString <> C_AbrArtAuto then begin
              tbAbruf.first;
              while not tbAbruf.eof do begin
                if tbAbruf.fieldByName(C_MRGAbruf_Abrufart).asString = C_AbrArtAuto then
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
            tbAbruf.fieldByName(C_MRGAbruf_Schnittstelle).asInteger:=
              aQuery.FieldByName(C_TFALogport).asInteger;
            tbAbruf.fieldByName(C_MRGAbruf_MrgId).asInteger:=
              aQuery.FieldByName(C_TfAGeraeteId).asInteger;
            tbAbruf.fieldByName(C_MRGAbruf_Abrufart).asString:=
              aQuery.FieldByName(C_TFAAbrufArt).asString;
            tbAbruf.fieldByName(C_MRGAbruf_Datentypen).asInteger:=
              aQuery.FieldByName(C_TFAAbrufe).asInteger;
            tbAbruf.fieldByName(C_MRGAbruf_DatenVon).asDateTime:=
              aQuery.FieldByName(C_TfAVonDatum).asDateTime;
            tbAbruf.fieldByName(C_MRGAbruf_DatenBis).asDateTime:=
              aQuery.FieldByName(C_TfABisDatum).asDateTime;
            tbAbruf.fieldByName(C_MRGAbruf_Anrufversuch).asInteger:=
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

{--------------------------------------------------------------------------------------------}
function TMRGAbruf.InsertToMRGAbruf(anId, aLogPort, aDaten, aVersuch: integer;
                                    anAbrufArt: string; Von, Bis: TDateTime): boolean;
{--------------------------------------------------------------------------------------------}
{ Parameter in die MRG-Abruf-Tabelle einfügen
  Übergabe: Parameter mit einzufügenden Daten }
begin
  result:= False; { default }
  if tbAbruf.OpenShared then begin
    try
  { Manuelle Aufträge werden hinter dem letzten manuellen eingefügt, automatische angehängt }
      if anAbrufArt <> C_AbrArtAuto then begin
        tbAbruf.first;
        while not tbAbruf.eof do begin
          if tbAbruf.fieldByName(C_MRGAbruf_Abrufart).asString = C_AbrArtAuto then
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
      tbAbruf.fieldByName(C_MRGAbruf_Schnittstelle).asInteger:= aLogPort;
      tbAbruf.fieldByName(C_MRGAbruf_MrgId).asInteger:= anId;
      tbAbruf.fieldByName(C_MRGAbruf_Abrufart).asString:= anAbrufArt;
      tbAbruf.fieldByName(C_MRGAbruf_Datentypen).asInteger:= aDaten;
      tbAbruf.fieldByName(C_MRGAbruf_DatenVon).asDateTime:= Von;
      tbAbruf.fieldByName(C_MRGAbruf_DatenBis).asDateTime:= Bis;
      tbAbruf.fieldByName(C_MRGAbruf_Anrufversuch).asInteger:= aVersuch;
      tbAbruf.post;
      result:= true;  { wenn die function bis hierher kommt }
    except
      tbAbruf.Close;
      exit;
    end;
    tbAbruf.close;
  end;
end;

{ Überprüft, ob ein Eintrag mit übergebener Id, Schnittstelle und Abrufart }
{ in der MDBAbruf.Db steht                                  }
{ Parameter: Id, LogPort und Abrufart                       }
{ Rückgabe: True - steht drin; False - steht nicht drin     }
{-----------------------------------------------------------}
function TMRGAbruf.IsMRGAbruf(anId: integer; aLogPort: integer; anAbrufArt: string): boolean;
{-----------------------------------------------------------}
var
  q          : TQueryExt;
begin
  result:= False; { default }                               
  if fileExists(Path + CDBMRGAbruf) then begin
    q:= TQueryExt.Create(nil);
    try
      q.DatabaseName:= Path;
      q.Sql.Add('SELECT COUNT(*) FROM "' + CDBMRGAbruf + '"');
      q.Sql.Add('WHERE ' + C_MRGAbruf_MrgId + '= ' + IntToStr(anId));
      q.Sql.Add('AND ' + C_MRGAbruf_Schnittstelle + ' = ' + IntToStr(aLogPort));
      q.Sql.Add('AND ' + C_MRGAbruf_Abrufart + '= "' + anAbrufArt + '"');
      if q.open then
        if q.Fields[0].AsInteger > 0 then result:= True;
    except
      on exception do begin
        q.Free;
        if IsDebugFlag then showMessage('Abruf - WaitForAbruf');
        exit;
      end;
    end;
    q.Free;
  end;
end;

{---------------------------------------------------------------------------------------------}
function TMRGAbruf.GetMRGAbruf (ComNr: byte; Thema: string; MRGStammdaten: TMRGStammdaten;
                                var Abrufart: TAbrufart; var AbrufListe: TAbrufListe): boolean;
{---------------------------------------------------------------------------------------------}
{ Abrufliste für einen Abruf aus Tabelle lesen und zusammenstellen;
  gelesene Datensätze aus Tabelle löschen }
{ Übergabe: ComNr
            Comserve-Thema (Fup oder Modem)
            MRGStammdaten
  Rückgabe: Abrufart
            AbrufListe
  Ergebnis: true, wenn MRG-Abruf zusammengestellt werden konnte }
Const
  CComx = 0;                                        { beliebige Schnittstelle }

var
  TempSchnittstelle: integer;
  TempMrgId: integer;
  TempAbrufart: string;
  TempDatentypen: integer;
  TempDatenVon: TDateTime;
  TempDatenBis: TDateTime;
  TempAnrufversuch: integer;
  TempMasterId: integer;
  SearchAbrufart: string;
  SearchMasterId: integer;     { wenn > 0 werden alle Geräte, die zu dieser MasterId gehören,
                                 gesucht }
  MasterAbrufData: TAbrufData; { wenn MasterAbrufData.MrgId > 0 wird auch der Master mit
                                 abgefragt }
  SlaveListe: array [1..MAXDSFGGERAETE - 1] of TAbrufData;        { ohne Master, nur Slaves }
  ListIndex: integer;
  SlaveAdr: char;
  i: integer;
  AbrufDatensatz: boolean;     { true, wenn der Tabellendatensatz in Abruf aufgenommen wird }
  nur_ein_Geraet: boolean;
  ModemAbrufgruppe: integer;
//  Modemtyp: integer;

begin
  Result := false;
  FillChar (AbrufListe, SizeOf (TAbrufListe), 0);              { Vorbelegung für AbrufListe }

  if FileExists(Path + CDBMRGAbruf) then begin
    if tbAbruf.OpenExclusive then begin
      try
        SearchMasterId:=0;
        SearchAbrufart:='';
        FillChar (MasterAbrufData, SizeOf (TAbrufData), 0); { Vorbelegung für MasterAbrufData }
        FillChar (SlaveListe, SizeOf (SlaveListe), 0);         { Vorbelegung für SlaveListe }
        ListIndex:=0;

        while not tbAbruf.Eof do begin
          AbrufDatensatz:=false;
          TempAbrufart := tbAbruf.FieldByName(C_MRGAbruf_Abrufart).AsString;
          { "Momentanwerte beenden" ignorieren }
          if (TempAbrufart = C_AbrArtMomStop) OR (TempAbrufart = C_AbrArtMomHalten) then begin
            tbAbruf.Next;
            Continue;
          end;

          { bei Momentanwerte-Abruf, Rufreaktivierung, Rückrufauslösung nur 1 Gerät
            in Abrufliste aufnehmen: }
          nur_ein_Geraet:=(TempAbrufart = C_AbrArtMomStart) OR
                          (TempAbrufart = C_AbrArtRufReakt) OR
                          (TempAbrufart = C_AbrArtRueckRuf);

          TempSchnittstelle:=tbAbruf.FieldByName(C_MRGAbruf_Schnittstelle).AsInteger;
          TempMrgId := tbAbruf.FieldByName(C_MRGAbruf_MrgId).AsInteger;

          { beliebige Schnittstelle }
          if (TempSchnittstelle = CComx) then begin
            if MRGStammdaten.GetModemAbrufgruppe (TempMrgId, ModemAbrufgruppe) then begin
              if (Thema = Themen [thMRGFup]) AND (ModemAbrufgruppe <> 1) then begin
                { wenn ein FUP an der Schnittstelle hängt, können nur Geräte der
                  Modem-Abrufgruppe 1 abgerufen werden }
                tbAbruf.Next;
                Continue;
              end
              else if (Thema = Themen [thMRGModem]) AND (ModemAbrufgruppe = 1) then begin
                tbAbruf.Next;
                Continue;

(*  später, wenn 2400DX richtig gut funktioniert:
               if MRGStammdaten.GetModemtyp (TempMrgId, Modemtyp) then begin
                  if Modemtyp = 1 then begin
                    { wenn ein Modem an der Schnittstelle hängt, können keine Geräte
                      der Modem-Abrufgruppe 1 abgerufen werden, für die als Modemtyp
                      1200HDX im Stammsatz eingetragen ist: }
                    tbAbruf.Next;
                    Continue;
                  end;
                end
                else begin
                  tbAbruf.Next;
                  Continue;
                end; *)
              end;
            end
            else begin
              tbAbruf.Next;
              Continue;
            end;
          end;

          { feste Schnittstelle: logische -> physikalische }
          if (TempSchnittstelle >= Low (LogPhysComZuordnung)) AND
             (TempSchnittstelle <= High (LogPhysComZuordnung)) then begin
            if LogPhysComZuordnung [TempSchnittstelle] <> ComNr then begin
              tbAbruf.Next;
              Continue;
            end;
          end;

          Result:=true;
          TempDatentypen := tbAbruf.FieldByName(C_MRGAbruf_Datentypen).AsInteger;
          if not tbAbruf.FieldByName(C_MRGAbruf_DatenVon).IsNull then
            TempDatenVon := tbAbruf.FieldByName(C_MRGAbruf_DatenVon).AsDateTime
          else
            TempDatenVon := 0;
          if not tbAbruf.FieldByName(C_MRGAbruf_DatenBis).IsNull then
            TempDatenBis := tbAbruf.FieldByName(C_MRGAbruf_DatenBis).AsDateTime
          else
            TempDatenBis := 0;
          TempAnrufversuch := tbAbruf.FieldByName(C_MRGAbruf_Anrufversuch).AsInteger;

          if MRGStammDaten.DSFGTyp (TempMrgId, SlaveAdr, TempMasterId) = DSFGNO then begin
            { normales MRG, keine weiteren Master oder Slaves }
            if SearchMasterId = 0 then begin
              SearchAbrufart:=TempAbrufart;

              AbrufListe [1].StationId:=TempMrgId;                              { nur ein Gerät }
              AbrufListe [1].Datentypen := TempDatentypen;
              AbrufListe [1].DatenVon := TempDatenVon;
              AbrufListe [1].DatenBis := TempDatenBis;
              AbrufListe [1].Anrufversuch := TempAnrufversuch;

              tbAbruf.Delete;                               { Datensatz aus Tabelle löschen }
              Break;
            end;
          end
          else begin
            { Master oder Slave }
            if SearchMasterId = 0 then begin
              SearchMasterId:=TempMasterId;               { SearchMasterId für Suche setzen }
              SearchAbrufart:=TempAbrufart;               { SearchAbrufart für Suche setzen }
            end;

            if (TempMasterId = SearchMasterId) AND (TempAbrufart = SearchAbrufart) then begin
              { zugehöriger Master oder Slave, gleiche Abrufart }
              if TempMrgId = SearchMasterId then begin    { Master: MasterAbrufData belegen }
                MasterAbrufData.StationId:=TempMrgId;
                MasterAbrufData.Datentypen := TempDatentypen;
                MasterAbrufData.DatenVon := TempDatenVon;
                MasterAbrufData.DatenBis := TempDatenBis;
                MasterAbrufData.Anrufversuch := TempAnrufversuch;
                AbrufDatensatz:=true;
              end
              else begin                                        { Slave: SlaveListe belegen }
                if ListIndex < High (SlaveListe) then begin
                  inc (ListIndex);
                  SlaveListe [ListIndex].StationId:=TempMrgId;
                  SlaveListe [ListIndex].Datentypen := TempDatentypen;
                  SlaveListe [ListIndex].DatenVon := TempDatenVon;
                  SlaveListe [ListIndex].DatenBis := TempDatenBis;
                  SlaveListe [ListIndex].Anrufversuch := TempAnrufversuch;
                  AbrufDatensatz:=true;
                end;
              end;
            end;
          end; { if MRGStammdaten.DSfGTyp }

          if AbrufDatensatz then begin
            tbAbruf.Delete;                                 { Datensatz aus Tabelle löschen }
            if nur_ein_Geraet then Break;
          end else
            tbAbruf.Next;
        end;  { while not tbAbruf.eof }
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }

    { Rückgabe belegen: Abrufart }
    if SearchAbrufart = C_AbrArtAuto then
      Abrufart := aa_automatisch
    else if SearchAbrufart = C_AbrArtMomStart then
      Abrufart := aa_momentan
    else if SearchAbrufart = C_AbrArtRufReakt then
      Abrufart := aa_rufreakt
    else if SearchAbrufart = C_AbrArtRueckRuf then
      Abrufart := aa_rueckruf
    else
      Abrufart := aa_manuell;

    { hier Rückgabe belegen bei DSfG-Abruf: AbrufListe }
    if AbrufListe [1].StationId = 0 then begin
      ListIndex:=0;
      if MasterAbrufData.StationId <> 0 then begin
        inc (ListIndex);
        AbrufListe [ListIndex]:=MasterAbrufData;
      end;
      for i:=Low (SlaveListe) to High (SlaveListe) do begin
        if SlaveListe [i].StationId <> 0 then begin
          inc (ListIndex);
          AbrufListe [ListIndex]:=SlaveListe [i];
        end;
      end;
    end;
  end; { if FileExists }
end;

{---------------------------------------------------}
function TMRGAbruf.DeleteMRGAutomatikAbrufe: boolean;
{---------------------------------------------------}
{ alle Automatik-Abrufe aus Tabelle löschen }
{ Ergebnis: true, wenn mind. 1 MRG-Abruf gelöscht wurde }
begin
  Result := false;

  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        { Filter auf Automatik-Aufträge: }
        tbAbruf.Filtered:=false;                   { Deaktivieren des Filters }
        tbAbruf.Filter:=C_MRGAbruf_Abrufart + ' = ''' + C_AbrArtAuto + '''';
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

{------------------------------------------------------------------------}
function TMRGAbruf.GetMRGAbrufMomentan (MrgId: TMrgId;
                                        var Abrufart: string;
                                        var Datentypen: integer): boolean;
{------------------------------------------------------------------------}
{ MRG-Abruftabelle nach Einträgen für laufende Momentanwertdarstellung durchsuchen;
  Suchpriorität: ParaStart, ParaSenden, MomE, ParaEnde
  Übergabe: MrgId
  Rückgabe: Abrufart, Datentypen des Tabelleneintrags
  Ergebnis: false, wenn Tabelle nicht gelesen werden konnte }
var
  gefunden: boolean;

begin
  Result:=false;
  Abrufart:='';
  Datentypen:=-1;

  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        Result:=true;
        { Suche nach Abrufart = ParaStart: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_MRGAbruf_MrgId + ' = ' + IntToStr(MrgId) + ') AND ' +
                        '(' + C_MRGAbruf_Abrufart + ' = ''' + C_AbrArtParaStart + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle ParaStart-Datensätze für MrgId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_MrgAbruf_Abrufart).AsString;
          Datentypen:=tbAbruf.FieldByName (C_MrgAbruf_Datentypen).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = ParaSenden: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_MrgAbruf_MrgId + ' = ' + IntToStr(MrgId) + ') AND ' +
                        '(' + C_MrgAbruf_Abrufart + ' = ''' + C_AbrArtParaSenden + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin               { alle ParaSenden-Datensätze für MrgId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_MrgAbruf_Abrufart).AsString;
          Datentypen:=tbAbruf.FieldByName (C_MrgAbruf_Datentypen).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = MomE: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_MrgAbruf_MrgId + ' = ' + IntToStr(MrgId) + ') AND ' +
                        '(' + C_MrgAbruf_Abrufart + ' = ''' + C_AbrArtMomStop + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        gefunden:=false;
        while not tbAbruf.Eof do begin                    { alle MomE-Datensätze für MrgId }
          gefunden:=true;
          Abrufart:=tbAbruf.FieldByName (C_MrgAbruf_Abrufart).AsString;
          Datentypen:=tbAbruf.FieldByName (C_MrgAbruf_Datentypen).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }
        if gefunden then exit;

        { Suche nach Abrufart = ParaEnde: }
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_MrgAbruf_MrgId + ' = ' + IntToStr(MrgId) + ') AND ' +
                        '(' + C_MrgAbruf_Abrufart + ' = ''' + C_AbrArtParaEnde + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }

        tbAbruf.First;
        while not tbAbruf.Eof do begin               { alle ParaEnde-Datensätze für MrgId }
          Abrufart:=tbAbruf.FieldByName (C_MrgAbruf_Abrufart).AsString;
          Datentypen:=tbAbruf.FieldByName (C_MrgAbruf_Datentypen).AsInteger;
          tbAbruf.Delete;                                   { Datensatz aus Tabelle löschen }
        end;  { while not tbAbruf.eof }

        tbAbruf.Filtered:=false;
      finally
        tbAbruf.Close;
      end;
    end;  { if OpenExclusive }
  end; { if Exists }
end;

{--------------------------------------------------------------------}
function TMRGAbruf.GetMRGAbrufMomentanHalten (MrgId: TMrgId): boolean;
{--------------------------------------------------------------------}
{ MRG-Abruftabelle nach Einträgen mit Abrufart "Momentanwerte halten" für MrgId
  durchsuchen;
  Übergabe: MrgId
  Ergebnis: true, wenn Verbindung für Momentanwerte gehalten werden soll }
begin
  Result := false;         { sicherheitshalber nicht "Halten" bei Tabellenzugriffsproblemen }
  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        tbAbruf.Filtered:=false;                                 { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_MRGAbruf_MrgId + ' = ' + IntToStr(MrgId) + ') AND ' +
                        '(' + C_MRGAbruf_Abrufart + ' = ''' + C_AbrArtMomHalten + ''')';
        tbAbruf.Filtered:=true;                                    { Aktivieren des Filters }
        try
          while not tbAbruf.Eof do begin              { alle MomHalten-Datensätze für MrgId }
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

{----------------------------------------------------}
procedure TMRGAbruf.DeleteMRGMomentan (MrgId: TMrgId);
{----------------------------------------------------}
{ alle noch evtl. verbliebenen Einträge der beendeten Momentanwertdarstellung aus
  MRG-Abruftabelle löschen;
  Übergabe: MrgId }
begin
  if tbAbruf.Exists then begin
    if tbAbruf.OpenExclusive then begin
      try
        tbAbruf.Filtered:=false;                                  { Deaktivieren des Filters }
        tbAbruf.Filter:='(' + C_MRGAbruf_MrgId + ' = ' + IntToStr(MrgId) + ') AND ' +
                        '((' + C_MRGAbruf_Abrufart + ' = ''' + C_AbrArtMomHalten + ''') OR ' +
                        ' (' + C_MRGAbruf_Abrufart + ' = ''' + C_AbrArtParaStart + ''') OR ' +
                        ' (' + C_MRGAbruf_Abrufart + ' = ''' + C_AbrArtParaSenden + ''') OR ' +
                        ' (' + C_MRGAbruf_Abrufart + ' = ''' + C_AbrArtParaEnde + ''') OR ' +
                        ' (' + C_MRGAbruf_Abrufart + ' = ''' + C_AbrArtMomStop + '''))';
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
function TMRGAbruf.GetRecordCount: integer;
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
