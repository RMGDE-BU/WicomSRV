{**************************************************************************************}
{* Unit: Tabellenzugriffe auf WRUFE.DB (Rufdeaktivierung der Geräte)                  *}
{* 01.10.1999 WW                                                                      *}
{**************************************************************************************}
Unit RufeDb;

INTERFACE

Uses
  SysUtils, Dialogs, DBTables, DB, WSysCon, DB_Attn, WTables;

Const

  CDBWRufe = 'WRufe.db';     { enthält Rufinformationen und im Gerät eingestellte Rufnummer }

  { Tabelle 'WRufe.db' }

  C_WRufe_GeraeteArt  = 'GeraeteArt';      { MRG, DSfG, LAKS etc. }
  C_WRufe_GeraeteId   = 'GeraeteId';       { GeräteId in Stammdaten, z.B. MrgId }
  C_WRufe_DatumZeit   = 'DatumZeit';       { Datum und Zeit der Rufprotokollierung }
  C_WRufe_Anzahl      = 'Anzahl';          { Anzahl der Rufe }
  C_WRufe_Rufnummer   = 'Rufnummer';       { im Gerät eingestellte Rufnummer für Zentrale 1 }
  C_WRufe_Deaktiviert = 'Deaktiviert';     { true, wenn Rufnummer im Gerät auf "N" gestellt }


type

  { Objekt für Zugriff auf WRufe.db }

  TWRufeDB = class (TObject)
  private
    procedure CreateRufeDB;
  public
    RufeTable: TTableExt;
    constructor Create (FilePath: TFileName);
    destructor Destroy; override;
    function WriteRuf (GeraeteArt: string; GeraeteId: integer; MaxRufe: integer): boolean;
    procedure WriteRufDeaktivierung (GeraeteArt: string; GeraeteId: integer;
                                     Rufnummer: string);
    function GetDeaktivierungen(aQuery: TQueryExt): boolean;
    function GetRufnummer (GeraeteArt: string; GeraeteId: integer;
                           var Rufnummer: string): boolean;
    function GetDeaktiviert (GeraeteArt: string; GeraeteId: integer;
                             var Deaktiviert: boolean): boolean;
    procedure WriteRufReaktivierung (GeraeteArt: string; GeraeteId: integer);
    function DeleteRecord (GeraeteArt: string; GeraeteId: integer): boolean;
  end;

IMPLEMENTATION

{ TWRufeDB }

{------------------------------------------------}
constructor TWRufeDB.Create (FilePath: TFileName);
{------------------------------------------------}
begin
  inherited Create;
  RufeTable:=TTableExt.Create (nil);
  RufeTable.DatabaseName:=FilePath;
  RufeTable.TableName:=CDBWRufe;
  if not RufeTable.Exists then
    CreateRufeDB;                               { Tabelle automatisch anlegen }
end;

{--------------------------}
Destructor TWRufeDB.Destroy;
{--------------------------}
begin
  RufeTable.Free;
  inherited Destroy;
end;

{------------------------------}
procedure TWRufeDB.CreateRufeDB;
{------------------------------}
{ Rufe-Tabelle anlegen }
begin
  with RufeTable.FieldDefs do begin
    Clear;
    Add(C_WRufe_GeraeteArt, ftString, 10, false);
    Add(C_WRufe_GeraeteId, ftInteger, 0, false);
    Add(C_WRufe_DatumZeit, ftDateTime, 0, false);
    Add(C_WRufe_Anzahl, ftSmallInt, 0, false);
    Add(C_WRufe_Rufnummer, ftString, 20, false);
    Add(C_WRufe_Deaktiviert, ftBoolean, 0, false);
  end;
  with RufeTable.IndexDefs do begin
    Clear;
    { Primärindex: }
    Add('PGeraeteArt_Id', C_WRufe_GeraeteArt +';'+ C_WRufe_GeraeteId, [ixPrimary,ixUnique]);
  end;
  RufeTable.CreateTable;
end;

{---------------------------------------------------------------------------------------------}
function TWRufeDB.WriteRuf (GeraeteArt: string; GeraeteId: integer; MaxRufe: integer): boolean;
{---------------------------------------------------------------------------------------------}
{ ankommenden Ruf in Tabelle protokollieren und Prüfung, ob Rufanzahl erreicht ist;
  Übergabe: GeraeteArt
            GeraeteId
            Max. Anzahl von Rufen
  Ergebnis: false, wenn max. Anzahl der Rufe erreicht }
var
  AktDatum: TDateTime;
  Datum_LetzterRuf: TDateTime;

begin
  Result:=true;
  if RufeTable.OpenShared then begin
    try
      AktDatum:=Now;
      if RufeTable.FindKey ([GeraeteArt, GeraeteId]) then begin
        Datum_LetzterRuf:=RufeTable.FieldByName (C_WRufe_DatumZeit).AsDateTime;
        RufeTable.Edit;
        RufeTable.FieldByName (C_WRufe_DatumZeit).AsDateTime:=AktDatum;

        if RufeTable.FieldByName (C_WRufe_Deaktiviert).AsBoolean then begin      { Station ist als deaktiviert vermerkt }
          RufeTable.FieldByName (C_WRufe_Anzahl).AsInteger:=1;
          RufeTable.FieldByName (C_WRufe_Deaktiviert).AsBoolean:=false;                { jetzt nicht mehr deaktiviert ! }
        end
        else begin
          if Int(AktDatum) = Int(Datum_LetzterRuf) then                                           { Ruf am gleichen Tag }
            RufeTable.FieldByName (C_WRufe_Anzahl).AsInteger:=
              RufeTable.FieldByName (C_WRufe_Anzahl).AsInteger + 1
          else                                                                               { Ruf an einem anderen Tag }
            RufeTable.FieldByName (C_WRufe_Anzahl).AsInteger:=1;
        end;
        RufeTable.Post;
      end else
        RufeTable.InsertRecord ([GeraeteArt, GeraeteId, AktDatum, 1, nil, false]);
      if RufeTable.FieldByName (C_WRufe_Anzahl).AsInteger >= MaxRufe then
        Result:=false;                                                                  { max. Anzahl der Rufe erreicht }
    finally
      RufeTable.Close;
    end;
    { Trigger-Datei für WRufeDb schreiben: }
    WriteNewTime(RufeTable.DatabaseName + RufeTable.TableName);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TWRufeDB.WriteRufDeaktivierung (GeraeteArt: string; GeraeteId: integer;
                                          Rufnummer: string);
{-------------------------------------------------------------------------------}
{ erfolgreiche Rufdeaktivierung in Tabelle protokollieren;
  Übergabe: GeraeteArt
            GeraeteId
            im Gerät parametrierte Rufnummer für Zentrale 1 }
begin
  if RufeTable.OpenShared then begin;
    try
      if RufeTable.FindKey ([GeraeteArt, GeraeteId]) then begin
        RufeTable.Edit;
        RufeTable.FieldByName (C_WRufe_Rufnummer).AsString:=Rufnummer;
        RufeTable.FieldByName (C_WRufe_Deaktiviert).AsBoolean:=true;
        RufeTable.Post;

        { Trigger-Datei für WRufeDb bei Deaktivierung schreiben: }
        WriteNewTime(RufeTable.DatabaseName + RufeTable.TableName);
      end;
    finally
      RufeTable.Close;
    end;
  end;
end;

{---------------------------------------------------------------}
function TWRufeDB.GetDeaktivierungen(aQuery: TQueryExt): boolean;
{---------------------------------------------------------------}
{ gibt alle Einträge mit "Deaktiviert = true" zurück }
begin
  result:= true; { default }
  try
    aQuery.Close;
    aQuery.DatabaseName:= RufeTable.DatabaseName;
    with aQuery.Sql do begin
      Clear;
      Add('SELECT ' + C_WRufe_GeraeteArt + ',' +
                      C_WRufe_GeraeteId + ',' +
                      C_WRufe_DatumZeit + ',' +
                      C_WRufe_Rufnummer);
      Add('FROM "' + CDBWRufe + '"');
      Add('WHERE ' + C_WRufe_Deaktiviert + '= :Deaktiviert');
      Add('ORDER BY ' + C_WRufe_DatumZeit);
      aQuery.ParamByName('Deaktiviert').asBoolean:=true;
    end;
    aQuery.Open;
  except
    Result:=false;
    if IsDebugFlag then ShowMessage('Rufe - GetDeaktivierungen');
    exit;
  end;
end;

{---------------------------------------------------------------------}
function TWRufeDB.GetRufnummer (GeraeteArt: string; GeraeteId: integer;
                                var Rufnummer: string): boolean;
{---------------------------------------------------------------------}
{ Rufnummer aus Tabelle lesen;
  Übergabe: GeraeteArt
            GeraeteId
  Rückgabe: Rufnummer
  Ergebnis: true, wenn Rufnummer gefunden wurde }
begin
  Result:=false;
  Rufnummer:='';
  if RufeTable.OpenShared then begin
    try
      if RufeTable.FindKey ([GeraeteArt, GeraeteId]) then begin
        Rufnummer:=RufeTable.FieldByName (C_WRufe_Rufnummer).AsString;
        Result:=true;
      end;
    finally
      RufeTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------------------}
function TWRufeDB.GetDeaktiviert (GeraeteArt: string; GeraeteId: integer;
                                  var Deaktiviert: boolean): boolean;
{-----------------------------------------------------------------------}
{ Feld "Deaktiviert" aus Tabelle lesen;
  Übergabe: GeraeteArt
            GeraeteId
  Rückgabe: Deaktiviert
  Ergebnis: true, wenn "Deaktiviert" gefunden wurde }
begin
  Result:=false;
  Deaktiviert:=false;
  if RufeTable.OpenShared then begin
    try
      if RufeTable.FindKey ([GeraeteArt, GeraeteId]) then begin
        Deaktiviert:=RufeTable.FieldByName (C_WRufe_Deaktiviert).AsBoolean;
        Result:=true;
      end;
    finally
      RufeTable.Close;
    end;
  end;
end;

{--------------------------------------------------------------------------------}
procedure TWRufeDB.WriteRufReaktivierung (GeraeteArt: string; GeraeteId: integer);
{--------------------------------------------------------------------------------}
{ erfolgreiche Rufreaktivierung in Tabelle protokollieren;
  Übergabe: GeraeteArt
            GeraeteId }
begin
  if RufeTable.OpenShared then begin
    try
      if RufeTable.FindKey ([GeraeteArt, GeraeteId]) then begin
        RufeTable.Edit;
        RufeTable.FieldByName (C_WRufe_Anzahl).AsInteger:=0;
        RufeTable.FieldByName (C_WRufe_Deaktiviert).AsBoolean:=false;
        RufeTable.Post;

        { Trigger-Datei für WRufeDb bei Reaktivierung schreiben: }
        WriteNewTime(RufeTable.DatabaseName + RufeTable.TableName);
      end;
    finally
      RufeTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
function TWRufeDB.DeleteRecord (GeraeteArt: string; GeraeteId: integer): boolean;
{-------------------------------------------------------------------------------}
{ Löscht einen Eintrag aus Tabelle;
  Übergabe: Geraeteart
            GeraeteId
  Ergebnis: true, wenn Eintrag erfolgreich gelöscht wurde }
begin
  Result:=false;
  if RufeTable.OpenShared then begin
    try
      if RufeTable.FindKey([GeraeteArt, GeraeteId]) then
        RufeTable.Delete;
      Result:=true;
    except
      RufeTable.Close;
      exit;
    end;
    RufeTable.Close;

   { Trigger-Datei für WRufeDb beim Löschen schreiben: }
   WriteNewTime(RufeTable.DatabaseName + RufeTable.TableName);
  end;
end;

end.
