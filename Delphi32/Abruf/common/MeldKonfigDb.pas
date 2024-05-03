{******************************************************************************}
{* Unit: Zugriffe auf gemeinsame MRG-/DSfG-Meldungs-Konfigurationstabellen    *}
{* -> MELDNR.DB (Zuordnung gerätespez. Meld.nr <-> allg. Meld.nr.)            *}
{* -> MELDTEXT.DB (Standard-Meldungstext, Standard-Meldungsart (Hinweis,      *}
{*                 Warnung etc.), Meldungstyp (einwertig, kommt, geht))       *}
{* -> MELDNRSTATION.DB (Zuordnung allg. Meld.nr. <-> stationsspez. Meld.nr.)  *}
{* -> MELDTEXTSTATION.DB  (Def. stationsspez. Meldungstext und Meldungsart)   *}
{* 11.06.2001 WW                                                              *}
{ 09.11.2004  GD  Kommt- und Geht-Meldung wieder getrennt editierbar           }
{******************************************************************************}
unit MeldKonfigDb;

interface

uses
  SysUtils, WTables, WSysCon, WStrUtils, WResMeld;

type

  { Record für Daten aus MeldText.db }

  TMeldTextDataDB = record
    MNrAllg: string [szLen_MNrAllg];
    MText: string [szLen_MText];
    MArt: string [szLen_MArt];
    MTyp: string [szLen_MTyp];
  end;


  { Objekt für Meldungskonfiguration }

  TMeldKonfigurationDb = class (TObject)
  private
    Path: TFileName;
    tbMeldNr: TTableExt;
    tbMeldText: TTableExt;
    tbMeldNrStation: TTableExt;
    tbMeldTextStation: TTableExt;
    tbMeldAlarm: TTableExt;
  public
    constructor Create (aPath: string);
    destructor Destroy; override;
    function OpenMeldNrTable: boolean;
    function OpenMeldTextTable: boolean;
    function OpenMeldNrStationTable: boolean;
    function OpenMeldTextStationTable: boolean;
    function OpenMeldAlarmTable: boolean;
    procedure CloseMeldNrTable;
    procedure CloseMeldTextTable;
    procedure CloseMeldNrStationTable;
    procedure CloseMeldTextStationTable;
    procedure CloseMeldAlarmTable;

    function GetMeldungData (GeraeteArt: string; GeraeteId: integer;
                             MNrAllg: string; OpenAndCloseDB: boolean;
                             var MeldTextDataDB: TMeldTextDataDB): boolean;
    function GetAllgMeldNr (GeraeteArt: string; MeldGrpNr: integer;
                            MNrGeraet: string; OpenAndCloseDB: boolean;
                            var MNrAllg: string): boolean;
    function GetMeldungenQuery (aQuery: TQueryExt;
                                GeraeteArt: string; MeldGrpNr: integer;
                                MNrAllgMin: string; MNrAllgMax: string;
                                MArt: string): boolean;
    function GetMeldText_KonfigList (GeraeteArt: string; GeraeteId: integer;
                                     MeldTextKonfigList: TMeldTextKonfigList): boolean;
    { Methoden für tbMeldAlarm }
    function GetMeldAlarmQuery (aQuery: TQueryExt;
                                GeraeteArt: string; GeraeteId: integer): boolean;
    procedure DeleteMeldAlarm (GeraeteArt: string; GeraeteId: integer;
                               MNrAllg: string);
    function DeleteMeldAlarme_Geraet (GeraeteArt: string; GeraeteId: integer): boolean;
    procedure WriteMeldAlarm (GeraeteArt: string; GeraeteId: integer;
                              MNrAllg: string; KontaktNr: integer);
    { Methoden für tbMeldNrStation }
    function GetMeldNrStationQuery (aQuery: TQueryExt;
                                    GeraeteArt: string; GeraeteId: integer): boolean;
    function MNrStationCount (MNrStation: string): integer;
    function MNrStationExists (MNrStation: string): boolean;
    procedure WriteMeldNrStation (GeraeteArt: string; GeraeteId: integer;
                                  MNrAllg: string; MNrStation: string);
    function DeleteMeldNrStation_Geraet (GeraeteArt: string; GeraeteId: integer): boolean;
    function DeleteMeldNrStation_MNrStation (sMNrStation: string): boolean;
    { Methoden für tbMeldTextStation }
    function GetMeldTextStationQuery (aQuery: TQueryExt): boolean;
    procedure WriteMeldTextStation (MNrStation: string; MText: string; MArt: string);
    function DeleteMeldTextStation (sMNrStation: string): boolean;
  end;


implementation

{ TMeldKonfiguration }

{------------------------------------------------------}
constructor TMeldKonfigurationDb.Create (aPath: string);
{------------------------------------------------------}
{ Übergabe: Pfad, in dem sich die Meldungs-Konfigurationstabellen befinden }
begin
  inherited Create;
  Path:=aPath;

  tbMeldNr := TTableExt.Create (nil);
  tbMeldNr.DatabaseName:=aPath;
  tbMeldNr.TableName:=CDBMeldNr;

  tbMeldText := TTableExt.Create (nil);
  tbMeldText.DatabaseName:=aPath;
  tbMeldText.TableName:=CDBMeldText;

  tbMeldNrStation := TTableExt.Create (nil);
  tbMeldNrStation.DatabaseName:=aPath;
  tbMeldNrStation.TableName:=CDBMeldNrStation;

  tbMeldTextStation := TTableExt.Create (nil);
  tbMeldTextStation.DatabaseName:=aPath;
  tbMeldTextStation.TableName:=CDBMeldTextStation;

  tbMeldAlarm := TTableExt.Create (nil);
  tbMeldAlarm.DatabaseName:=aPath;
  tbMeldAlarm.TableName:=CDBMeldAlarm;
end;

{--------------------------------------}
destructor TMeldKonfigurationDb.Destroy;
{--------------------------------------}
begin
  tbMeldAlarm.Close;
  tbMeldNrStation.Close;
  tbMeldTextStation.Close;
  tbMeldText.Close;
  tbMeldAlarm.Free;
  tbMeldNrStation.Free;
  tbMeldTextStation.Free;
  tbMeldText.Free;
  inherited Destroy;
end;

{-----------------------------------------------------}
function TMeldKonfigurationDb.OpenMeldNrTable: boolean;
{-----------------------------------------------------}
{ Tabelle MELDNR.DB öffnen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if tbMeldNr.Exists then
    Result:=tbMeldNr.OpenShared
  else
    Result:=false;
end;

{-------------------------------------------------------}
function TMeldKonfigurationDb.OpenMeldTextTable: boolean;
{-------------------------------------------------------}
{ Tabelle MELDTEXT.DB öffnen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if tbMeldText.Exists then
    Result:=tbMeldText.OpenShared
  else
    Result:=false;
end;

{------------------------------------------------------------}
function TMeldKonfigurationDb.OpenMeldNrStationTable: boolean;
{------------------------------------------------------------}
{ Tabelle MELDNRSTATION.DB öffnen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if tbMeldNrStation.Exists then
    Result:=tbMeldNrStation.OpenShared
  else
    Result:=false;
end;

{--------------------------------------------------------------}
function TMeldKonfigurationDb.OpenMeldTextStationTable: boolean;
{--------------------------------------------------------------}
{ Tabelle MELDTEXTSTATION.DB öffnen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if tbMeldTextStation.Exists then
    Result:=tbMeldTextStation.OpenShared
  else
    Result:=false;
end;

{--------------------------------------------------------}
function TMeldKonfigurationDb.OpenMeldAlarmTable: boolean;
{--------------------------------------------------------}
{ Tabelle MELDALARM.DB öffnen;
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if tbMeldAlarm.Exists then
    Result:=tbMeldAlarm.OpenShared
  else
    Result:=false;
end;

{----------------------------------------------}
procedure TMeldKonfigurationDb.CloseMeldNrTable;
{----------------------------------------------}
{ Tabelle MELDNR.DB schließen }
begin
  tbMeldNr.Close;
end;

{------------------------------------------------}
procedure TMeldKonfigurationDb.CloseMeldTextTable;
{------------------------------------------------}
{ Tabelle MELDTEXT.DB schließen }
begin
  tbMeldText.Close;
end;

{-----------------------------------------------------}
procedure TMeldKonfigurationDb.CloseMeldNrStationTable;
{-----------------------------------------------------}
{ Tabelle MELDNRSTATION.DB schließen }
begin
  tbMeldNrStation.Close;
end;

{-------------------------------------------------------}
procedure TMeldKonfigurationDb.CloseMeldTextStationTable;
{-------------------------------------------------------}
{ Tabelle MELDTEXTSTATION.DB schließen }
begin
  tbMeldTextStation.Close;
end;

{-------------------------------------------------}
procedure TMeldKonfigurationDb.CloseMeldAlarmTable;
{-------------------------------------------------}
{ Tabelle MELDALARM.DB schließen }
begin
  tbMeldAlarm.Close;
end;

{------------------------------------------------------------------------------------------}
function TMeldKonfigurationDb.GetMeldungData (GeraeteArt: string;
                                              GeraeteId: integer;
                                              MNrAllg: string;
                                              OpenAndCloseDB: boolean;
                                              var MeldTextDataDB: TMeldTextDataDB): boolean;
{------------------------------------------------------------------------------------------}
{ liefert Meldungstext, -art und -typ zu einer allg. Meldumgsnummer einer
  MRG-Station oder DSfG-Instanz; falls Meldungstext und -art nicht
  stationsspezifisch definiert sind, werden die Standard-Werte aus MELDTEXT.DB
  zurückgegeben;
  Übergabe: GeraeteArt (M = MRG, D = DSfG)
            GeraeteId (MrgId bei MRG, InstanzId bei DSfG)
            allgemeine Meldungsnummer
            OpenAndCloseDB (wenn true, wird das Öffnen und Schließen der Tabellen
                            innerhalb der Routine vorgenommen)
  Rückgabe: MeldTextDataDB-Record
  Ergebnis: true, wenn Meldungsdaten gefunden wurden }
var
  MNrStation: string;
begin
  Result := false;
  try
    if OpenAndCloseDB then begin
      OpenMeldNrStationTable;
      OpenMeldTextStationTable;
      OpenMeldTextTable;
    end;
    if tbMeldNrStation.Active AND tbMeldTextStation.Active AND tbMeldText.Active then begin
      if tbMeldText.FindKey ([MNrAllg]) then begin
        MeldTextDataDB.MTyp:=tbMeldText.FieldByName(C_MeldText_MTyp).AsString;

        if tbMeldNrStation.FindKey ([GeraeteArt, GeraeteId, MNrAllg]) then begin
          MNrStation:=tbMeldNrStation.FieldByName(C_MeldNrStation_MNrStation).AsString;
          if tbMeldTextStation.FindKey ([MNrStation]) then begin
            MeldTextDataDB.MText:=tbMeldTextStation.FieldByName(C_MeldTextStation_MText).AsString;
            MeldTextDataDB.MArt:=tbMeldTextStation.FieldByName(C_MeldTextStation_MArt).AsString;
          end
          else begin
            MeldTextDataDB.MText:='undefiniert';
            MeldTextDataDB.MArt:='';
          end;
        end
        else begin
          MeldTextDataDB.MText:=tbMeldText.FieldByName(C_MeldText_MText).AsString;
          MeldTextDataDB.MArt:=tbMeldText.FieldByName(C_MeldText_MArt).AsString;
        end;
        Result := true;
      end;
    end;
  finally
    if OpenAndCloseDB then begin
      CloseMeldNrStationTable;
      CloseMeldTextStationTable;
      CloseMeldTextTable;
    end;
  end;
end;

{-------------------------------------------------------------------------}
function TMeldKonfigurationDb.GetAllgMeldNr (GeraeteArt: string;
                                             MeldGrpNr: integer;
                                             MNrGeraet: string;
                                             OpenAndCloseDB: boolean;
                                             var MNrAllg: string): boolean;
{-------------------------------------------------------------------------}
{ liefert die allgemeine Meldungsnummer zu einer Geräte-Meldungsnummer einer
  MRG-Station oder DSfG-Instanz;
  Übergabe: GeraeteArt (M = MRG, D = DSfG)
            Meldungs-Gruppennummer
            gerätespezifische Meldungsnummer
            OpenAndCloseDB (wenn true, wird das Öffnen und Schließen der Tabelle
                            innerhalb der Routine vorgenommen)
  Rückgabe: allgemeine Meldungsnummer
  Ergebnis: true, wenn allgemeine Meldungsnummer gefunden wurde }
var
  SearchMNrGeraet: string;
begin
  Result:=false;
  if OpenAndCloseDB then
    OpenMeldNrTable;
  try
    if tbMeldNr.Active then begin
      { Bei DSfG muß der String der gerätespezifischen Meldungsnummer für die
        Suche in der Tabelle ohne Vorzeichen sein und 4-stellig mit führenden Nullen
        versehen werden. Bei MRG-Nummern sind keine Anpassungen nötig: }
      SearchMNrGeraet:=MNrGeraet;
      if GeraeteArt = C_GerArtDSfG then begin
        SearchMNrGeraet:=F_LeftTrunc (SearchMNrGeraet, '-');
        SearchMNrGeraet:=F_LeftPad (SearchMNrGeraet, '0', 4);
      end;

      if tbMeldNr.FindKey ([GeraeteArt, MeldGrpNr, SearchMNrGeraet]) then begin
        MNrAllg:=tbMeldNr.FieldByName(C_MeldNr_MNrAllg).AsString;
        Result:=true;
      end
      else begin
        { bei DSfG zusätzlich im allgemeingültigen Meldungsnummern-Bereich 1-999
          (Meldungs-Gruppe 0) suchen: }
        if (GeraeteArt = C_GerArtDSfG) AND (MeldGrpNr <> 0) then begin
          if tbMeldNr.FindKey ([GeraeteArt, 0, SearchMNrGeraet]) then begin
            MNrAllg:=tbMeldNr.FieldByName(C_MeldNr_MNrAllg).AsString;
            Result:=true;
          end
        end;
      end;
    end;
  finally
    if OpenAndCloseDB then
      CloseMeldNrTable;
  end;
end;

{-------------------------------------------------------------------------------------}
function TMeldKonfigurationDb.GetMeldungenQuery (aQuery: TQueryExt;
                                                 GeraeteArt: string; MeldGrpNr: integer;
                                                 MNrAllgMin: string; MNrAllgMax: string;
                                                 MArt: string): boolean;
{-------------------------------------------------------------------------------------}
{ Meldungen für Geräteart und Meldungsgruppen-Nummer aus Konfigurationstabellen
  lesen und in Query zurückgeben;
  Übergabe: GeraeteArt ('M' = MRG; 'D' = DSfG)
            Meldungsgruppen-Nummer
            optionale Filter:
            MNrAllgMin, MNrAllgMax  (allg. Meldungsnummern-Bereich)
            MArt (Meldungsart)
  Rückgabe: Query
  Ergebnis: true, wenn Query geöffnet }
begin
  if tbMeldText.Exists AND tbMeldNr.Exists then begin
    with aQuery do begin
      Close;
      DataBaseName:=Path;
      Sql.Clear;
      Sql.Add('SELECT A.' + C_MeldText_MNrAllg + ',');
      Sql.Add('A.' + C_MeldText_MText + ',');
      Sql.Add('A.' + C_MeldText_MArt + ',');
      Sql.Add('A.' + C_MeldText_MTyp);
      Sql.Add('FROM "' + CDBMeldText + '" A, "' + CDBMeldNr + '" B');
      Sql.Add('WHERE (B.' + C_MeldNr_GeraeteArt + ' = :GeraeteArt) ');
      { Bei DSfG-Meldungsgruppen > 0 (Wieser-Instanzen) auch die allgemeinen
        Meldungen der Gruppe 0 mitliefern: } 
      if (GeraeteArt = C_GerArtDSfG) AND (MeldGrpNr > 0) then begin
        Sql.Add('AND ((B.' + C_MeldNr_MeldGrpNr + ' = :MeldGrpNr)');
        Sql.Add('OR (B.' + C_MeldNr_MeldGrpNr + ' = 0))');
      end else
        Sql.Add('AND (B.' + C_MeldNr_MeldGrpNr + ' = :MeldGrpNr)');
      Sql.Add('AND (B.' + C_MeldNr_MNrAllg + ' = A.' + C_MeldText_MNrAllg + ')');
      if length (MNrAllgMin) > 0 then begin
        Sql.Add('AND (A.' + C_MeldText_MNrAllg + ' >= :MNrAllgMin)');
        Sql.Add('AND (A.' + C_MeldText_MNrAllg + ' <= :MNrAllgMax)');
      end;
      if length (MArt) > 0 then
        Sql.Add ('AND (A.' + C_MeldText_MArt + ' = :MArt)');
      Sql.Add ('ORDER BY A.' + C_MeldText_MNrAllg);
      ParamByName ('GeraeteArt').asString:=GeraeteArt;
      ParamByName ('MeldGrpNr').asInteger:=MeldGrpNr;
      if length (MNrAllgMin) > 0 then begin
        ParamByName ('MNrAllgMin').asString:=MNrAllgMin;
        ParamByName ('MNrAllgMax').asString:=MNrAllgMax;
      end;
      if length (MArt) > 0 then
        ParamByName ('MArt').asString:=MArt;
      Result:=Open;
    end;
  end else
    Result:=false;
end;

{------------------------------------------------------------------------------------------------------}
function TMeldKonfigurationDb.GetMeldText_KonfigList (GeraeteArt: string; GeraeteId: integer;
                                                      MeldTextKonfigList: TMeldTextKonfigList): boolean;
{------------------------------------------------------------------------------------------------------}
{ liefert alle Meldungstexte (incl. Meldungsart und -typ); falls Meldungstext und -art nicht
  stationsspezifisch definiert sind, werden die Standard-Werte aus MELDTEXT.DB
  zurückgegeben;
  Übergabe: GeraeteArt (M = MRG, D = DSfG)
            GeraeteId (MrgId bei MRG, InstanzId bei DSfG)
  Rückgabe: Liste mit Meldungstexten
  Ergebnis: true, wenn Meldungstexte in die Liste geladen wurden }
var
  MNrStation: string;
  MeldTextData: TMeldTextData;
  MeldTextDataObj: TMeldTextDataObj;

begin
  Result := false;
  if MeldTextKonfigList = nil then exit;

  try
    OpenMeldNrStationTable;
    OpenMeldTextStationTable;
    OpenMeldTextTable;
    if tbMeldNrStation.Active AND tbMeldTextStation.Active AND tbMeldText.Active then begin
      while not tbMeldText.Eof do begin
        MeldTextData.MNrAllg:=tbMeldText.FieldByName(C_MeldText_MNrAllg).AsString;
        MeldTextData.MTyp:=tbMeldText.FieldByName(C_MeldText_MTyp).AsString;

        if tbMeldNrStation.FindKey ([GeraeteArt, GeraeteId, MeldTextData.MNrAllg]) then begin
          MNrStation:=tbMeldNrStation.FieldByName(C_MeldNrStation_MNrStation).AsString;
          if tbMeldTextStation.FindKey ([MNrStation]) then begin
            MeldTextData.MText:=tbMeldTextStation.FieldByName(C_MeldTextStation_MText).AsString;
            MeldTextData.MArt:=tbMeldTextStation.FieldByName(C_MeldTextStation_MArt).AsString;
          end
          else begin
            MeldTextData.MText:='undefiniert';
            MeldTextData.MArt:='';
          end;
        end
        else begin
          MeldTextData.MText:=tbMeldText.FieldByName(C_MeldText_MText).AsString;
          MeldTextData.MArt:=tbMeldText.FieldByName(C_MeldText_MArt).AsString;
        end;

        { Listenobjekt createn und in Liste einfügen: }
        MeldTextDataObj:=TMeldTextDataObj.Create;
        MeldTextDataObj.SetData (MeldTextData);
        MeldTextKonfigList.Add (MeldTextDataObj);

        tbMeldText.Next;
      end;  { while }
      Result := true;
    end;
  finally
    CloseMeldNrStationTable;
    CloseMeldTextStationTable;
    CloseMeldTextTable;
  end;
end;


{--------------------- Methoden für tbMeldAlarm -------------------------------}

{------------------------------------------------------------------------------------------------}
function TMeldKonfigurationDb.GetMeldAlarmQuery (aQuery: TQueryExt;
                                                 GeraeteArt: string; GeraeteId: integer): boolean;
{------------------------------------------------------------------------------------------------}
{ Einträge für Geräteart und GeraeteId aus MeldAlarm-Tabelle lesen und in Query zurückgeben;
  Übergabe: GeraeteArt ('M' = MRG; 'D' = DSfG)
            GeraeteId
  Rückgabe: Query
  Ergebnis: true, wenn Query geöffnet }
begin
  if tbMeldAlarm.Exists then begin
    with aQuery do begin
      Close;
      DataBaseName:=Path;
      Sql.Clear;
      Sql.Add('SELECT ' + C_MeldAlarm_MNrAllg + ',' + C_MeldAlarm_KontaktNr);
      Sql.Add('FROM "' + CDBMeldAlarm + '"');
      Sql.Add('WHERE (' + C_MeldAlarm_GeraeteArt + ' = :GeraeteArt) ');
      Sql.Add('AND (' + C_MeldAlarm_GeraeteId + ' = :GeraeteId)');
      ParamByName ('GeraeteArt').asString:=GeraeteArt;
      ParamByName ('GeraeteId').asInteger:=GeraeteId;
      Result:=Open;
    end;
  end else
    Result:=false;
end;

{-------------------------------------------------------------------------------------}
procedure TMeldKonfigurationDb.DeleteMeldAlarm (GeraeteArt: string; GeraeteId: integer;
                                                MNrAllg: string);
{-------------------------------------------------------------------------------------}
{ Einträge aus MeldAlarm-Tabelle löschen;
  Übergabe: GeraeteArt ('M' = MRG; 'D' = DSfG)
            GeraeteId
            Allg. Meldungsnummer }
begin
  if OpenMeldAlarmTable then begin
    try
      { Filter auf Tabelle setzen: }
      tbMeldAlarm.Filter:='(' + C_MeldAlarm_GeraeteArt + ' = ''' + GeraeteArt + ''') AND ' +
                          '(' + C_MeldAlarm_GeraeteId + ' = ' + IntToStr(GeraeteId) + ') AND ' +
                          '(' + C_MeldAlarm_MNrAllg + ' = ''' + MNrAllg + ''')';
      tbMeldAlarm.Filtered:=true;
      while not tbMeldAlarm.Eof do              { Einträge löschen }
        tbMeldAlarm.Delete;
    finally
      tbMeldAlarm.Filtered:=false;
      CloseMeldAlarmTable;
    end;
  end;
end;

{------------------------------------------------------------------------------------------------------}
function TMeldKonfigurationDb.DeleteMeldAlarme_Geraet (GeraeteArt: string; GeraeteId: integer): boolean;
{------------------------------------------------------------------------------------------------------}
{ Alle Einträge eines Geräts aus MeldAlarm-Tabelle löschen;
  Übergabe: GeraeteArt ('M' = MRG; 'D' = DSfG)
            GeraeteId
  Ergebnis: true, wenn Einträge erfogreich gelöscht wurden }
begin
  if not tbMeldAlarm.Exists then begin
    Result:=true;
    exit;
  end;
  Result:=false;
  if OpenMeldAlarmTable then begin
    try
      { Filter auf Tabelle setzen: }
      tbMeldAlarm.Filter:='(' + C_MeldAlarm_GeraeteArt + ' = ''' + GeraeteArt + ''') AND ' +
                          '(' + C_MeldAlarm_GeraeteId + ' = ' + IntToStr(GeraeteId) + ')';
      tbMeldAlarm.Filtered:=true;
      while not tbMeldAlarm.Eof do              { Einträge löschen }
        tbMeldAlarm.Delete;
      Result:=true;
    finally
      tbMeldAlarm.Filtered:=false;
      CloseMeldAlarmTable;
    end;
  end;
end;

{------------------------------------------------------------------------------------}
procedure TMeldKonfigurationDb.WriteMeldAlarm (GeraeteArt: string; GeraeteId: integer;
                                               MNrAllg: string; KontaktNr: integer);
{------------------------------------------------------------------------------------}
{ Eintrag in MeldAlarm-Tabelle einfügen;
  Achtung: Tabelle muß vor Aufruf der Methode geöffnet werden !
  Übergabe: Geräteart
            GeräteId
            allg. Meldungsnummer
            KontaktNr }
begin
  if tbMeldAlarm.Active then
    tbMeldAlarm.InsertRecord ([GeraeteArt, GeraeteId, MNrAllg, KontaktNr]);
end;


{--------------------- Methoden für tbMeldNrStation -------------------------}

{--------------------------------------------------------------------------------}
function TMeldKonfigurationDb.GetMeldNrStationQuery (aQuery: TQueryExt;
                                                     GeraeteArt: string;
                                                     GeraeteId: integer): boolean;
{--------------------------------------------------------------------------------}
{ Einträge für Geräteart und GeraeteId aus MeldNrStation-Tabelle lesen und in
  Query zurückgeben;
  Übergabe: GeraeteArt ('M' = MRG; 'D' = DSfG)
            GeraeteId
  Rückgabe: Query
  Ergebnis: true, wenn Query geöffnet }
begin
  if tbMeldNrStation.Exists then begin
    with aQuery do begin
      Close;
      DataBaseName:=Path;
      Sql.Clear;
      Sql.Add('SELECT ' + C_MeldNrStation_MNrAllg + ',' + C_MeldNrStation_MNrStation);
      Sql.Add('FROM "' + CDBMeldNrStation + '"');
      Sql.Add('WHERE (' + C_MeldNrStation_GeraeteArt + ' = :GeraeteArt) ');
      Sql.Add('AND (' + C_MeldNrStation_GeraeteId + ' = :GeraeteId)');
      ParamByName ('GeraeteArt').asString:=GeraeteArt;
      ParamByName ('GeraeteId').asInteger:=GeraeteId;
      Result:=Open;
    end;
  end else
    Result:=false;
end;

{---------------------------------------------------------------------------}
function TMeldKonfigurationDb.MNrStationCount (MNrStation: string): integer;
{---------------------------------------------------------------------------}
begin
  Result := 0;
  with TQueryExt.Create(nil) do
  try
    Close;
    DataBaseName:=Path;
    Sql.Clear;
    Sql.Add('SELECT DISTINCT ' + C_MeldNrStation_GeraeteArt + ',' +
      C_MeldNrStation_GeraeteId );
    Sql.Add('FROM ' + ChangeFileExt(CDBMeldNrStation, ''));
    Sql.Add('WHERE ' + C_MeldNrStation_MNrStation + '= :MNrStation');
    ParamByName('MNrStation').asString := MNrStation;
    if open then begin
      Last;
      Result := RecordCount;
      Close;
    end;
  finally
    free;
  end;
end;

{---------------------------------------------------------------------------}
function TMeldKonfigurationDb.MNrStationExists (MNrStation: string): boolean;
{---------------------------------------------------------------------------}
begin
  Result := (MNrStationCount(MNrStation) > 0);
end;

{---------------------------------------------------------------------}
procedure TMeldKonfigurationDb.WriteMeldNrStation (GeraeteArt: string;
                                                   GeraeteId: integer;
                                                   MNrAllg: string;
                                                   MNrStation: string);
{---------------------------------------------------------------------}
{ Eintrag in MeldNrStation-Tabelle schreiben;
  Übergabe: GeraeteArt ('M' = MRG; 'D' = DSfG)
            GeraeteId
            allgemeine Meldungsnummer
            stationsspezifische Meldungsnummer }
begin
  if tbMeldNrStation.Active then begin
    if tbMeldNrStation.FindKey ([GeraeteArt, GeraeteId, MNrAllg]) then
      tbMeldNrStation.Delete;
    if Length (MNrStation) > 0 then
      tbMeldNrStation.InsertRecord ([Geraeteart, GeraeteId, MNrAllg, MNrStation]);
  end;
end;

{---------------------------------------------------------------------------------------------------------}
function TMeldKonfigurationDb.DeleteMeldNrStation_Geraet (GeraeteArt: string; GeraeteId: integer): boolean;
{---------------------------------------------------------------------------------------------------------}
{ Alle Einträge eines Geräts aus MeldNrStation-Tabelle löschen;
  Übergabe: GeraeteArt ('M' = MRG; 'D' = DSfG)
            GeraeteId
  Ergebnis: true, wenn Einträge erfogreich gelöscht wurden }
begin
  if not tbMeldNrStation.Exists then begin
    Result:=true;
    exit;
  end;
  Result:=false;
  if OpenMeldNrStationTable then begin
    try
      { Filter auf Tabelle setzen: }
      tbMeldNrStation.Filter:='(' + C_MeldNrStation_GeraeteArt + ' = ''' + GeraeteArt + ''') AND ' +
                              '(' + C_MeldNrStation_GeraeteId + ' = ' + IntToStr(GeraeteId) + ')';
      tbMeldNrStation.Filtered:=true;
      while not tbMeldNrStation.Eof do              { Einträge löschen }
        tbMeldNrStation.Delete;
      Result:=true;
    finally
      tbMeldNrStation.Filtered:=false;
      CloseMeldNrStationTable;
    end;
  end;
end;


{----------------------------------------------------------------------------}
function TMeldKonfigurationDb.DeleteMeldNrStation_MNrStation(
  sMNrStation: string): boolean;
{----------------------------------------------------------------------------}
begin
  Result := False;
  if tbMeldNrStation.Exists then begin
    with TQueryExt.Create(nil) do
    try
      DataBaseName := Path;
      Sql.Add('DELETE FROM ' + ChangeFileExt(CDBMeldNrStation, ''));
      Sql.Add('WHERE ' + C_MeldNrStation_MNrStation + ' = ''' +
        sMNrStation + '''');
      Result := ExecSql;
    finally
      Free;
    end;
  end;
end;

{--------------------- Methoden für tbMeldTextStation -------------------------}

{---------------------------------------------------------------------------------}
function TMeldKonfigurationDb.GetMeldTextStationQuery (aQuery: TQueryExt): boolean;
{---------------------------------------------------------------------------------}
{ Einträge aus MeldTextStation-Tabelle lesen und in Query zurückgeben;
  Rückgabe: Query
  Ergebnis: true, wenn Query geöffnet }
begin
  if tbMeldTextStation.Exists then begin
    with aQuery do begin
      Close;
      DataBaseName:=Path;
      Sql.Clear;
      Sql.Add('SELECT ' + C_MeldTextStation_MNrStation + ',');
      Sql.Add(C_MeldTextStation_MText + ',');
      Sql.Add(C_MeldTextStation_MArt);
      Sql.Add('FROM "' + CDBMeldTextStation + '"');
      Result:=Open;
    end;
  end else
    Result:=false;
end;

{--------------------------------------------------------------------------------}
procedure TMeldKonfigurationDb.WriteMeldTextStation (MNrStation: string;
                                                     MText: string; MArt: string);
{--------------------------------------------------------------------------------}
{ Eintrag in MeldTextStation-Tabelle schreiben;
  Übergabe: stationsspezifische Meldungsnummer
            stationsspezifischer Meldungstext
            stationsspezifische Meldungsart }
begin
  if tbMeldTextStation.Active then begin
    if not tbMeldTextStation.FindKey ([MNrStation]) then
      tbMeldTextStation.InsertRecord ([MNrStation, MText, MArt])
    else begin  // 09.11.2004
      tbMeldTextStation.Edit;
      tbMeldTextStation.FieldByName(C_MeldTextStation_MText).asString := MText;
      tbMeldTextStation.Post;
    end;
  end;
end;

{-----------------------------------------------------------------------------}
function TMeldKonfigurationDb.DeleteMeldTextStation(
  sMNrStation: string): boolean;
{-----------------------------------------------------------------------------}
{ Eintrag mit MNrStation aus MeldTextStation-Tabelle löschen;
  Übergabe: stationsspezifische Meldungsnummer
  Ergebnis: true, wenn Eintrag vorhanden und gelöscht wurde }
begin
  Result := False;
  if tbMeldNrStation.Exists then begin
    with TQueryExt.Create(nil) do
    try
      DataBaseName := Path;
      Sql.Add('DELETE FROM ' + ChangeFileExt(CDBMeldTextStation, ''));
      Sql.Add('WHERE ' + C_MeldTextStation_MNrStation + ' = ''' +
        sMNrStation + '''');
      Result := ExecSql;
    finally
      Free;
    end;
  end;
end;

end.

