{******************************************************************************}
{* Unit: Zugriff auf gemeinsame MRG/DSfG-Meldungstabelle und Detailtabellen   *}
{* 11.06.2001 WW                                                              *}
{* 08.08.2002 SM	Funktionen (für GAMESS:						*}
{*				     Get_LogbuchDaten_By_Zeit 				*}		
{*				     Get_LogbuchDaten_By_OrdNr 				*}		
{* 17.04.2003 WW neue Methode 'Get_LogbuchDaten_By_Zeit_OrdNr' ersetzt die    *}
{*               bisherigen Daten-Zugriffsmethoden ('_By_Zeit', '_By_OrdNr')  *}
{******************************************************************************}
unit MeldungenDB;

interface

uses
  Classes, SysUtils, WTables, Db, DbTables, WSysCon;

type

  { Datenrecords für Meldungs-Tabellen }

  TMeldungenData = record
    MeldungId: integer;
    Benutzer: string [szLen_Benutzer];
    GeraeteArt: string [szLen_GeraeteArt];
    GeraeteId: integer;
    InstanzId_Archiv: integer;
    LogbuchNr_Archiv: integer;
    OrdnungsNr: integer;
    DatumZeit: TDateTime;
    Zeitzone: string [szLen_Zeitzone];
    MNrAllg: string [szLen_MNrAllg];
    MNrGeraet: string [szLen_MNrGeraet];
    MText: string [szLen_MText];
    MTyp: string [szLen_MTyp];
    MArt: string [szLen_MArt];
    Quittiert: boolean;
    Bemerkung: string [szLen_Bemerkung];
  end;

  TMeldungenDSfGData = record
    MeldungId: integer;
    Status: string [szLen_DSfGStatus];
    CRC: string [szLen_DSfGCRC];
  end;

  TMeldungenParaData = record
    MeldungId: integer;
    ParaNrAllg: string [szLen_ParaNrAllg];
    WertAlt: string [szLen_ParaWert];
    WertNeu: string [szLen_ParaWert];
  end;

  TMeldungenAlarmData = record
    MeldungId: integer;
    PCAlarm: boolean;
    RelaisAlarm: boolean;
    VoiceAlarm: boolean;
    Gedruckt: boolean;
  end;


  { Objekt für Datentabellen mit MRG-Meldungen und DSfG-Logbucheinträgen }

  TMeldungenDb = class(TObject)
  private
    Path: TFileName;
    function CreateTbWMeldungen: boolean;
    function CreateTbWMeldungenDSfG: boolean;
    function CreateTbWMeldungenPara: boolean;
    function CreateTbWMeldungenAlarm: boolean;
  public
    tbWMeldungen: TTableExt;
    tbWMeldungenDSfG: TTableExt;
    tbWMeldungenPara: TTableExt;
    tbWMeldungenAlarm: TTableExt;
    constructor Create (APath: TFileName);
    destructor Destroy; override;
    { Allgemeine Methoden: }
    function OpenMeldungenTable (Exklusiv: boolean): boolean;
    function OpenMeldungenDSfGTable (Exklusiv: boolean): boolean;
    function OpenMeldungenParaTable (Exklusiv: boolean): boolean;
    function OpenMeldungenAlarmTable (Exklusiv: boolean): boolean;
    procedure CloseMeldungenTable (Flush: boolean = false);
    procedure CloseMeldungenDSfGTable (Flush: boolean = false);
    procedure CloseMeldungenParaTable (Flush: boolean = false);
    procedure CloseMeldungenAlarmTable (Flush: boolean = false);
    procedure SetIndexNameTbWMeldungen (AIndexName: string);
    procedure SetFilterTbWMeldungen (AFilter: string);
    procedure ClearFilterTbWMeldungen;
    procedure WriteMeldungPara (MeldungenParaData: TMeldungenParaData);
    procedure WriteMeldungAlarm (MeldungenAlarmData: TMeldungenAlarmData);
    function GetMeldungen (aQuery: TQueryExt; GeraeteArt: string;
                           GeraeteId: integer; Benutzer: string): boolean;
    function QuittiereMeldung (MeldungId: integer; OpenAndCloseDB: boolean): boolean;
    function WriteBemerkung (MeldungId: integer; Bemerkung: string): boolean;
    { MRG-Methoden: }
    function GetLetztMRGMeldung (Benutzer: string; GeraeteId: integer;
                                 var DatumZeit: TDateTime;
                                 var MNrAllg: string): boolean;
    function WriteMRGMeldung (MeldungenData: TMeldungenData): integer;
    procedure DeleteMRGMeldungen (Benutzer: string; GeraeteId: integer);
    function ReorganizeMRGMeldungen (GeraeteId: integer; MaxAnzahl: integer): boolean;
    { DSfG-Methoden: }
    function GetLetztDSfGMeldungOrdNr_DatumZeit (Benutzer: string;
                                                 InstanzId_Archiv: integer;
                                                 LogbuchNr_Archiv: integer;
                                                 var OrdNr: integer;
                                                 var DatumZeit: TDateTime): boolean;
    function GetLetztVortagDSfGMeldungOrdNr_DatumZeit (Benutzer: string;
                                                       InstanzId_Archiv: integer;
                                                       LogbuchNr_Archiv: integer;
                                                       var OrdNr: integer;
                                                       var DatumZeit: TDateTime): boolean;
    function DSfGMeldungAlreadyExists (ADatumZeit: TDateTime; AOrdnungsNr: integer): boolean;
    function WriteDSfGMeldung (MeldungenData: TMeldungenData): integer;
    procedure WriteDSfGMeldungDSfG (MeldungenDSfGData: TMeldungenDSfGData);
    procedure DeleteDSfGMeldungen (Benutzer: string; InstanzId_Archiv: integer;
                                   LogbuchNr_Archiv: integer);
    function ReorganizeDSfGMeldungen (InstanzId_Archiv: integer; LogbuchNr_Archiv: integer;
                                      MaxAnzahl: integer): boolean;
    function Get_LogbuchDaten_By_Zeit_OrdNr (InstanzId, LogbuchNr: integer;
                                             Benutzer: string;
                                             var ergebnis: TQueryExt;
                                             VonDatumZeit, BisDatumZeit: TDateTime;
                                             ONrVon: integer = -1;
                                             ONrBis: integer = -1): boolean;
  end;

implementation

{ TMeldungen }

{-------------------------------------------------}
constructor TMeldungenDb.Create (APath: TFileName);
{-------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  tbWMeldungen:=TTableExt.Create(nil);
  tbWMeldungen.DataBaseName:= Path;
  tbWMeldungen.TableName:= C_Tb_WMeldungen;
  tbWMeldungenDSfG:=TTableExt.Create(nil);
  tbWMeldungenDSfG.DataBaseName:= Path;
  tbWMeldungenDSfG.TableName:= C_Tb_WMeldungenDSfG;
  tbWMeldungenPara:=TTableExt.Create(nil);
  tbWMeldungenPara.DataBaseName:= Path;
  tbWMeldungenPara.TableName:= C_Tb_WMeldungenPara;
  tbWMeldungenAlarm:=TTableExt.Create(nil);
  tbWMeldungenAlarm.DataBaseName:= Path;
  tbWMeldungenAlarm.TableName:= C_Tb_WMeldungenAlarm;

  if not tbWMeldungen.Exists then
    CreatetbWMeldungen;                        { WMeldungen.db neuanlegen }
  if not tbWMeldungenDSfG.Exists then
    CreatetbWMeldungenDSfG;                    { WMeldungenDSfG.db neuanlegen }
  if not tbWMeldungenPara.Exists then
    CreatetbWMeldungenPara;                    { WMeldungenPara.db neuanlegen }
  if not tbWMeldungenAlarm.Exists then
    CreatetbWMeldungenAlarm;                   { WMeldungenAlarm.db neuanlegen }
end;

{------------------------------}
destructor TMeldungenDb.Destroy;
{------------------------------}
begin
  tbWMeldungenAlarm.Free;
  tbWMeldungenPara.Free;
  tbWMeldungenDSfG.Free;
  tbWMeldungen.Free;
  inherited Destroy;
end;

{------------------------------------------------}
function TMeldungenDb.CreateTbWMeldungen: boolean;
{------------------------------------------------}
{ Haupt-Tabelle WMeldungen.db anlegen }
begin
  with tbWMeldungen.FieldDefs do begin
    Clear;
    Add(C_Tf_WMeldungen_MeldungId, ftAutoInc, 0, false);
    Add(C_Tf_WMeldungen_Benutzer, ftString, szLen_Benutzer, false);
    Add(C_Tf_WMeldungen_GeraeteArt, ftString, szLen_GeraeteArt, false);
    Add(C_Tf_WMeldungen_GeraeteId, ftInteger, 0, false);
    Add(C_Tf_WMeldungen_InstanzId_Archiv, ftInteger, 0, false);
    Add(C_Tf_WMeldungen_LogbuchNr_Archiv, ftSmallInt, 0, false);
    Add(C_Tf_WMeldungen_OrdnungsNr, ftInteger, 0, false);
    Add(C_Tf_WMeldungen_DatumZeit, ftDateTime, 0, false);
    Add(C_Tf_WMeldungen_Zeitzone, ftString, szLen_Zeitzone, false);
    Add(C_Tf_WMeldungen_MNrAllg, ftString, szLen_MNrAllg, false);
    Add(C_Tf_WMeldungen_MNrGeraet, ftString, szLen_MNrGeraet, false);
    Add(C_Tf_WMeldungen_MText, ftString, szLen_MText, false);
    Add(C_Tf_WMeldungen_MTyp, ftString, szLen_MTyp, false);
    Add(C_Tf_WMeldungen_MArt, ftString, szLen_MArt, false);
    Add(C_Tf_WMeldungen_Quittiert, ftBoolean, 0, false);
    Add(C_Tf_WMeldungen_Bemerkung, ftString, szLen_Bemerkung, false);
  end;
  with tbWMeldungen.IndexDefs do begin
    Clear;
    Add(C_TI_WMeldungen_ixMeldId, C_Tf_WMeldungen_MeldungId, [ixPrimary, ixUnique]);
    Add(C_TI_WMeldungen_ixDatOrd, C_Tf_WMeldungen_DatumZeit+';'+C_Tf_WMeldungen_OrdnungsNr, [ixCaseInsensitive]);         { Sekundärindex }
  end;
  Result:=tbWMeldungen.CreateTable;
end;

{----------------------------------------------------}
function TMeldungenDb.CreateTbWMeldungenDSfG: boolean;
{----------------------------------------------------}
{ Detail-Tabelle WMeldungenDSfG.db anlegen }
begin
  with tbWMeldungenDSfG.FieldDefs do begin
    Clear;
    Add(C_Tf_WMeldungenDSfG_MeldungId, ftInteger, 0, false);
    Add(C_Tf_WMeldungenDSfG_Status, ftString, szLen_DSfGStatus, false);
    Add(C_Tf_WMeldungenDSfG_CRC, ftString, szLen_DSfGCRC, false);
  end;
  with tbWMeldungenDSfG.IndexDefs do begin
    Clear;
    Add(C_TI_WMeldungenDSfG_ixMeldId, C_Tf_WMeldungenDSfG_MeldungId, [ixPrimary, ixUnique]);
  end;
  Result:=tbWMeldungenDSfG.CreateTable;
end;

{----------------------------------------------------}
function TMeldungenDb.CreateTbWMeldungenPara: boolean;
{----------------------------------------------------}
{ Detail-Tabelle WMeldungenPara.db anlegen }
begin
  with tbWMeldungenPara.FieldDefs do begin
    Clear;
    Add(C_Tf_WMeldungenPara_MeldungId, ftInteger, 0, false);
    Add(C_Tf_WMeldungenPara_ParaNrAllg, ftString, szLen_ParaNrAllg, false);
    Add(C_Tf_WMeldungenPara_WertAlt, ftString, szLen_ParaWert, false);
    Add(C_Tf_WMeldungenPara_WertNeu, ftString, szLen_ParaWert, false);
  end;
  with tbWMeldungenPara.IndexDefs do begin
    Clear;
    Add(C_TI_WMeldungenPara_ixMeldId, C_Tf_WMeldungenPara_MeldungId, [ixPrimary, ixUnique]);
  end;
  Result:=tbWMeldungenPara.CreateTable;
end;

{-----------------------------------------------------}
function TMeldungenDb.CreateTbWMeldungenAlarm: boolean;
{-----------------------------------------------------}
{ Detail-Tabelle WMeldungenAlarm.db anlegen }
begin
  with tbWMeldungenAlarm.FieldDefs do begin
    Clear;
    Add(C_Tf_WMeldungenAlarm_MeldungId, ftInteger, 0, false);
    Add(C_Tf_WMeldungenAlarm_PCAlarm, ftBoolean, 0, false);
    Add(C_Tf_WMeldungenAlarm_RelaisAlarm, ftBoolean, 0, false);
    Add(C_Tf_WMeldungenAlarm_VoiceAlarm, ftBoolean, 0, false);
    Add(C_Tf_WMeldungenAlarm_Gedruckt, ftBoolean, 0, false);
  end;
  with tbWMeldungenAlarm.IndexDefs do begin
    Clear;
    Add(C_TI_WMeldungenAlarm_ixMeldId, C_Tf_WMeldungenAlarm_MeldungId, [ixPrimary, ixUnique]);
  end;
  Result:=tbWMeldungenAlarm.CreateTable;
end;
(*
{-------------------------------------------------}
procedure TMeldungenDb.CreateReferenzintegritaeten;
{-------------------------------------------------}
var
  q: TQuery;
begin
  q:=TQuery.Create(nil);
//  try
    q.DatabaseName:=Path;
    with q.Sql do begin
      Add('ALTER TABLE "' + C_Tb_WMeldungen + '" ');
      Add('ADD FOREIGN KEY (' + C_Tf_WMeldungen_MeldungId + ') ');
      Add('REFERENCES "' + C_Tb_WMeldungenPara + '" (' + C_Tf_WMeldungenPara_MeldungId + ')');
    end;
    q.ExecSql;
//  except
  //  q.free;
//    exit;           -> funktioniert nicht: "Merkmal nicht verfügbar"
//  end;
  q.free;
end;*)

{--------------------------------------------------------------------}
function TMeldungenDb.OpenMeldungenTable (Exklusiv: boolean): boolean;
{--------------------------------------------------------------------}
{ Meldungs-Haupttabelle öffnen;
  Übergabe: Exklusiv (wenn true, wird die Tabelle exklusiv geöffnet)
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if tbWMeldungen.Exists then begin
    if Exklusiv then
      Result:=tbWMeldungen.OpenExclusive
    else
      Result:=tbWMeldungen.OpenShared;
  end else
    Result:=false;
end;

{------------------------------------------------------------------------}
function TMeldungenDb.OpenMeldungenDSfGTable (Exklusiv: boolean): boolean;
{------------------------------------------------------------------------}
{ Detail-Tabelle "DSfG" öffnen;
  Übergabe: Exklusiv (wenn true, wird die Tabelle exklusiv geöffnet)
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if tbWMeldungenDSfG.Exists then begin
    if Exklusiv then
      Result:=tbWMeldungenDSfG.OpenExclusive
    else
      Result:=tbWMeldungenDSfG.OpenShared;
  end else
    Result:=false;
end;

{------------------------------------------------------------------------}
function TMeldungenDb.OpenMeldungenParaTable (Exklusiv: boolean): boolean;
{------------------------------------------------------------------------}
{ Detail-Tabelle "Parameteränderungen" öffnen;
  Übergabe: Exklusiv (wenn true, wird die Tabelle exklusiv geöffnet)
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if tbWMeldungenPara.Exists then begin
    if Exklusiv then
      Result:=tbWMeldungenPara.OpenExclusive
    else
      Result:=tbWMeldungenPara.OpenShared;
  end else
    Result:=false;
end;

{-------------------------------------------------------------------------}
function TMeldungenDb.OpenMeldungenAlarmTable (Exklusiv: boolean): boolean;
{-------------------------------------------------------------------------}
{ Detail-Tabelle "Alarme" öffnen;
  Übergabe: Exklusiv (wenn true, wird die Tabelle exklusiv geöffnet)
  Ergebnis: true, wenn Tabelle geöffnet werden konnte }
begin
  if tbWMeldungenAlarm.Exists then begin
    if Exklusiv then
      Result:=tbWMeldungenAlarm.OpenExclusive
    else
      Result:=tbWMeldungenAlarm.OpenShared;
  end else
    Result:=false;
end;

{------------------------------------------------------------------}
Procedure TMeldungenDb.CloseMeldungenTable (Flush: boolean = false);
{------------------------------------------------------------------}
{ Meldungs-Haupttabelle schließen;
  Übergabe: Flush (wenn true, werden zwischengespeicherte Änderungen sofort
                   in die Tabelle eintragen) }
begin
  if Flush then
    tbWMeldungen.FlushBuffers;
  tbWMeldungen.Close;
  tbWMeldungen.IndexName:='';
end;

{----------------------------------------------------------------------}
Procedure TMeldungenDb.CloseMeldungenDSfGTable (Flush: boolean = false);
{----------------------------------------------------------------------}
{ Detail-Tabelle "DSfG" schließen;
  Übergabe: Flush (wenn true, werden zwischengespeicherte Änderungen sofort
                   in die Tabelle eintragen) }
begin
  if Flush then
    tbWMeldungenDSfG.FlushBuffers;
  tbWMeldungenDSfG.Close;
end;

{----------------------------------------------------------------------}
Procedure TMeldungenDb.CloseMeldungenParaTable (Flush: boolean = false);
{----------------------------------------------------------------------}
{ Detail-Tabelle "Parameteränderungen" schließen;
  Übergabe: Flush (wenn true, werden zwischengespeicherte Änderungen sofort
                   in die Tabelle eintragen) }
begin
  if Flush then
    tbWMeldungenPara.FlushBuffers;
  tbWMeldungenPara.Close;
end;

{-----------------------------------------------------------------------}
Procedure TMeldungenDb.CloseMeldungenAlarmTable (Flush: boolean = false);
{-----------------------------------------------------------------------}
{ Detail-Tabelle "Alarme" schließen;
  Übergabe: Flush (wenn true, werden zwischengespeicherte Änderungen sofort
                   in die Tabelle eintragen) }
begin
  if Flush then
    tbWMeldungenAlarm.FlushBuffers;
  tbWMeldungenAlarm.Close;
end;

{-------------------------------------------------------------------}
procedure TMeldungenDb.SetIndexNameTbWMeldungen (AIndexName: string);
{-------------------------------------------------------------------}
{ Übergabe: Name eines Sekundärindex, der auf die WMeldungen.db gelegt werden soll;
            wird ein Leerstring übergeben, so kann die Tabelle anschließend
            wieder mit dem Primärindex geöffnet werden }
begin
  tbWMeldungen.IndexName:=AIndexName;
end;

{-------------------------------------------------------------}
procedure TMeldungenDb.SetFilterTbWMeldungen (AFilter: string);
{-------------------------------------------------------------}
{ Filter für tbWMeldungen setzen und aktivieren }
begin
  tbWMeldungen.Filter:=AFilter;
  tbWMeldungen.Filtered:=true;
end;

{---------------------------------------------}
procedure TMeldungenDb.ClearFilterTbWMeldungen;
{---------------------------------------------}
{ Filter für tbWMeldungen deaktivieren }
begin
  tbWMeldungen.Filter:='';
  tbWMeldungen.Filtered:=false;
end;

{------------------------------------------------------------------------------}
procedure TMeldungenDb.WriteMeldungPara (MeldungenParaData: TMeldungenParaData);
{------------------------------------------------------------------------------}
{ Datensatz in Detailtabelle WMeldungenPara.db einfügen;
  Achtung: Tabelle muß vor Aufruf der Methode geöffnet werden !
  Übergabe: Datenrecord für Tabelle }
begin
  if tbWMeldungenPara.Active then begin
    with MeldungenParaData do
      tbWMeldungenPara.InsertRecord ([MeldungId, ParaNrAllg, WertAlt, WertNeu]);
  end;
end;

{---------------------------------------------------------------------------------}
procedure TMeldungenDb.WriteMeldungAlarm (MeldungenAlarmData: TMeldungenAlarmData);
{---------------------------------------------------------------------------------}
{ Datensatz in Detailtabelle WMeldungenAlarm.db einfügen;
  Achtung: Tabelle muß vor Aufruf der Methode geöffnet werden !
  Übergabe: Datenrecord für Tabelle }
begin
  if tbWMeldungenAlarm.Active then begin
    with MeldungenAlarmData do
      tbWMeldungenAlarm.InsertRecord ([MeldungId, PCAlarm, RelaisAlarm, VoiceAlarm, Gedruckt]);
  end;
end;

{---------------------------------------------------------------------------------}
function TMeldungenDb.GetMeldungen (aQuery: TQueryExt; GeraeteArt: string;
                                    GeraeteId: integer; Benutzer: string): boolean;
{---------------------------------------------------------------------------------}
{ Meldungen für Benutzer aus Tabellen lesen und in Query zurückgeben;
  Übergabe: GeraeteArt ('M' = nur MRG-Geräte; 'D' = nur DSfG-Geräte; '' = beide)
            GeraeteId (<= 0 -> "alle" )
            Benutzer
  Rückgabe: Query }
begin
  with aQuery do begin
    Close;
    DataBaseName:=Path;
    Sql.Clear;
    Sql.Add('SELECT A.' + C_Tf_WMeldungen_MeldungId + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_GeraeteArt + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_GeraeteId + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_OrdnungsNr + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_DatumZeit + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_Zeitzone + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_MNrAllg + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_MNrGeraet + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_MText + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_MTyp + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_MArt + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_Quittiert + ',');
    Sql.Add('A.' + C_Tf_WMeldungen_Bemerkung + ',');
    Sql.Add('B.' + C_Tf_WMeldungenDSfG_Status + ',');
    Sql.Add('B.' + C_Tf_WMeldungenDSfG_CRC + ',');
    Sql.Add('C.' + C_Tf_WMeldungenPara_WertAlt + ',');
    Sql.Add('C.' + C_Tf_WMeldungenPara_WertNeu + ',');
    Sql.Add('D.' + C_TfParameterText);
    Sql.Add('FROM "' + C_Tb_WMeldungen + '" A');
    Sql.Add('LEFT JOIN "' + C_Tb_WMeldungenDSfG + '" B');
    Sql.Add('ON A.' + C_Tf_WMeldungen_MeldungId + ' = B.' + C_Tf_WMeldungenDSfG_MeldungId);
    Sql.Add('LEFT JOIN "' + C_Tb_WMeldungenPara + '" C');
    Sql.Add('ON A.'+ C_Tf_WMeldungen_MeldungId + ' = C.' + C_Tf_WMeldungenPara_MeldungId);
    Sql.Add('LEFT JOIN "' + C_TbParaText + '" D');
    Sql.Add('ON C.'+ C_Tf_WMeldungenPara_ParaNrAllg + ' = D.' + C_TfParameterNummer);
    Sql.Add('WHERE (A.' + C_Tf_WMeldungen_Benutzer + '= :Benutzer)');
    if (GeraeteArt = C_GerArtMrg) OR (GeraeteArt = C_GerArtDSfG) then begin
      Sql.Add('AND (A.' + C_Tf_WMeldungen_GeraeteArt + '= :GeraeteArt)');
      ParamByName ('GeraeteArt').asString:=GeraeteArt;
    end;
    if GeraeteId > 0 then begin
      Sql.Add('AND (A.' + C_Tf_WMeldungen_GeraeteId + '= :GeraeteId)');
      ParamByName ('GeraeteId').asInteger:=GeraeteId;
    end;
    ParamByName ('Benutzer').asString:=Benutzer;
    Result:=Open;
  end;
end;

{------------------------------------------------------------------------}
function TMeldungenDb.QuittiereMeldung (MeldungId: integer;
                                        OpenAndCloseDB: boolean): boolean;
{------------------------------------------------------------------------}
{ Quittiert-Feld in Meldungs-Haupttabelle auf "True" setzen;
  Übergabe: MeldungId
            OpenAndCloseDB (wenn true, wird das Öffnen und Schließen der Tabelle
                            innerhalb der Routine vorgenommen) }
begin
  Result:=false;
  try
    if OpenAndCloseDB then
      OpenMeldungenTable (false);
    if tbWMeldungen.Active then begin
      if tbWMeldungen.FindKey ([MeldungId]) then begin
        tbWMeldungen.Edit;
        tbWMeldungen.FieldByName (C_Tf_WMeldungen_Quittiert).AsBoolean:=true;
        tbWMeldungen.Post;
        Result:=true;
      end;
    end;
  finally
    if OpenAndCloseDB then
      CloseMeldungenTable;
  end;
end;

{------------------------------------------------------------------------------------}
function TMeldungenDb.WriteBemerkung (MeldungId: integer; Bemerkung: string): boolean;
{------------------------------------------------------------------------------------}
{ Bemerkung in Bemerkung-Feld der Meldungs-Haupttabelle schreiben;
  Übergabe: MeldungId
            Bemerkungs-String }
begin
  Result:=false;
  if OpenMeldungenTable (false) then begin
    try
      if tbWMeldungen.FindKey ([MeldungId]) then begin
        tbWMeldungen.Edit;
        tbWMeldungen.FieldByName (C_Tf_WMeldungen_Bemerkung).AsString:=Bemerkung;
        tbWMeldungen.Post;
        Result:=true;
      end;
    finally
      CloseMeldungenTable;
    end;
  end;
end;


{---------------------------- MRG-Methoden ------------------------------------}

{-----------------------------------------------------------------------------}
function TMeldungenDb.GetLetztMRGMeldung (Benutzer: string; GeraeteId: integer;
                                          var DatumZeit: TDateTime;
                                          var MNrAllg: string): boolean;
{-----------------------------------------------------------------------------}
{ Datum/Zeit und allgemeine Meldungs-Nr. der letzten Meldung einer MRG-Station ermitteln;
  Übergabe: Benutzer (Automatik etc.)
            GeraeteId
  Rückgabe: Datum/Zeit, allg. Meldungs-Nr. der letzten Meldung
  Ergebnis: true, wenn Meldung gefunden }
begin
  Result:=false;
  DatumZeit:=-1;
  MNrAllg:='';

  { MRG-Meldungen: Suche in Haupt-Tabelle über GeraeteId (= MrgId) }
  if OpenMeldungenTable (false) then begin
    try
      { Filter auf Tabelle setzen: }
      tbWMeldungen.Filter:='(' + C_Tf_WMeldungen_Benutzer + ' = ''' + Benutzer + ''') AND ' +
                           '(' + C_Tf_WMeldungen_GeraeteArt + ' = ''' + C_GerArtMrg + ''') AND ' +
                           '(' + C_Tf_WMeldungen_GeraeteId + ' = ' + IntToStr(GeraeteId) + ')';
      tbWMeldungen.Filtered:=true;
      tbWMeldungen.Last;
      if not tbWMeldungen.BOF then begin
        DatumZeit:=tbWMeldungen.FieldByName (C_Tf_WMeldungen_DatumZeit).AsDateTime;
        MNrAllg:=tbWMeldungen.FieldByName (C_Tf_WMeldungen_MNrAllg).AsString;
        Result:=true;
      end;
    finally
      tbWMeldungen.Filtered:=false;
      CloseMeldungenTable;
    end;
  end; { if OpenMeldungenDSfGTable }
end;

{-----------------------------------------------------------------------------}
function TMeldungenDb.WriteMRGMeldung (MeldungenData: TMeldungenData): integer;
{-----------------------------------------------------------------------------}
{ neue MRG-Meldung in Haupttabelle WMeldungen.db einfügen;
  Achtung: Tabelle muß vor Aufruf der Methode geöffnet werden !
  Übergabe: Datenrecord für Tabelle
  Ergebnis: 0, falls einfügen nicht möglich
            sonst MeldungId des neuen Eintrags }
begin
  Result:=0;
  if tbWMeldungen.Active then begin
    with MeldungenData do
      tbWMeldungen.InsertRecord ([nil, Benutzer, GeraeteArt, GeraeteId, nil, nil,
                                  nil, DatumZeit, Zeitzone, MNrAllg, MNrGeraet,
                                  MText, MTyp, MArt, Quittiert, Bemerkung]);
    Result:=tbWMeldungen.FieldByName (C_Tf_WMeldungen_MeldungId).AsInteger;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TMeldungenDb.DeleteMRGMeldungen (Benutzer: string; GeraeteId: integer);
{-------------------------------------------------------------------------------}
{ alle zu Benutzer und GeraeteId gehörenden MRG-Einträge aus den Meldungstabellen
  löschen;
  Übergabe: Benutzer
            GeraeteId }
var
  MeldungId: integer;
begin
  if OpenMeldungenTable (true) then begin   { Exklusiv öffnen zur Sicherheit }
    try
      if OpenMeldungenParaTable (true) then begin
        try
          if OpenMeldungenAlarmTable (true) then begin
            try
              { Filter auf Tabelle setzen: }
              tbWMeldungen.Filter:='(' + C_Tf_WMeldungen_Benutzer + ' = ''' + Benutzer + ''') AND ' +
                                   '(' + C_Tf_WMeldungen_GeraeteArt + ' = ''' + C_GerArtMrg + ''') AND ' +
                                   '(' + C_Tf_WMeldungen_GeraeteId + ' = ' + IntToStr(GeraeteId) + ')';
              tbWMeldungen.Filtered:=true;
              while not tbWMeldungen.Eof do begin
                MeldungId:=tbWMeldungen.FieldByName(C_Tf_WMeldungen_MeldungId).AsInteger;
                { aus WMeldungenPara löschen: }
                if tbWMeldungenPara.FindKey ([MeldungId]) then
                  tbWMeldungenPara.Delete;
                { aus WMeldungenAlarm löschen: }
                if tbWMeldungenAlarm.FindKey ([MeldungId]) then
                  tbWMeldungenAlarm.Delete;
                { aus WMeldungen löschen: }
                tbWMeldungen.Delete;
              end;
            finally
              CloseMeldungenAlarmTable (true);
            end;
          end;
        finally
          CloseMeldungenParaTable (true);
        end;
      end;
    finally
      tbWMeldungen.Filtered:=false;
      CloseMeldungenTable (true);
    end;
  end;
end;

{---------------------------------------------------------------------------------------------}
function TMeldungenDb.ReorganizeMRGMeldungen (GeraeteId: integer; MaxAnzahl: integer): boolean;
{---------------------------------------------------------------------------------------------}
{ Automatik-MRG-Meldungen in den Meldungstabellen reorganisieren ("Rundpuffer"), d.h.
  nur maximal die letzten MaxAnzahl Meldungen je Station speichern, ältere Meldungen löschen;
  Übergabe: GeraeteId
            Max. Anzahl der Meldungen (wenn = 0, dann keine Organisation der Tabelle als Rundpuffer)
  Ergebnis: true, wenn Reorganisation durchgeführt wurde }
var
  MeldungId: integer;
  LoeschAnz: integer;
  i: integer;

begin
  Result:=false;
  if MaxAnzahl > 0 then begin
    if OpenMeldungenTable (true) then begin  { Exklusiv öffnen zur Sicherheit }
      try
        if OpenMeldungenParaTable (true) then begin
          try
            if OpenMeldungenAlarmTable (true) then begin
              try
                { Filter auf Tabelle setzen: }
                tbWMeldungen.Filter:='(' + C_Tf_WMeldungen_Benutzer + ' = ''' + C_BenutzerAuto + ''') AND ' +
                                     '(' + C_Tf_WMeldungen_GeraeteArt + ' = ''' + C_GerArtMrg + ''') AND ' +
                                     '(' + C_Tf_WMeldungen_GeraeteId + ' = ' + IntToStr(GeraeteId) + ')';
                tbWMeldungen.Filtered:=true;

                LoeschAnz:=tbWMeldungen.RecordCount - MaxAnzahl;
                for i:=1 to LoeschAnz do begin
                  tbWMeldungen.First;
                  if not tbWMeldungen.Eof then begin
                    MeldungId:=tbWMeldungen.FieldByName(C_Tf_WMeldungen_MeldungId).AsInteger;
                    { aus WMeldungenPara löschen: }
                    if tbWMeldungenPara.FindKey ([MeldungId]) then
                      tbWMeldungenPara.Delete;
                    { aus WMeldungenAlarm löschen: }
                    if tbWMeldungenAlarm.FindKey ([MeldungId]) then
                      tbWMeldungenAlarm.Delete;
                    { aus WMeldungen löschen: }
                    tbWMeldungen.Delete;
                  end;
                end; { for }
              finally
                CloseMeldungenAlarmTable (true);
              end;
            end;
          finally
            CloseMeldungenParaTable (true);
          end;
        end;
      finally
        tbWMeldungen.Filtered:=false;
        CloseMeldungenTable (true);
      end;
    end;
  end; { if MaxAnzahl > 0 }
end;


{---------------------------- DSfG-Methoden -----------------------------------}

{-------------------------------------------------------------------------------------------}
function TMeldungenDb.GetLetztDSfGMeldungOrdNr_DatumZeit (Benutzer: string;
                                                          InstanzId_Archiv: integer;
                                                          LogbuchNr_Archiv: integer;
                                                          var OrdNr: integer;
                                                          var DatumZeit: TDateTime): boolean;
{-------------------------------------------------------------------------------------------}
{ Ordnungsnummer und Datum/Zeit der letzten Meldung eines Logbuchs einer
  DSfG-Registrier-Instanz ermitteln;
  Übergabe: Benutzer (Automatik etc.)
            InstanzId der Registrier-Instanz
            Logbuch-Nr. der Registrier-Instanz
  Rückgabe: Ordnungsnummer und Datum/Zeit der letzten Meldung
  Ergebnis: true, wenn Meldung gefunden }
begin
  Result:=false;
  DatumZeit:=-1;

  { DSfG-Meldungen: Suche in Haupt-Tabelle über InstanzId_Archiv und LogbuchNr_Archiv }
  if OpenMeldungenTable (false) then begin
    try
      { Filter auf Tabelle setzen: }
      tbWMeldungen.Filter:='(' + C_Tf_WMeldungen_Benutzer + ' = ''' + Benutzer + ''') AND ' +
                           '(' + C_Tf_WMeldungen_GeraeteArt + ' = ''' + C_GerArtDSfG + ''') AND ' +
                           '(' + C_Tf_WMeldungen_InstanzId_Archiv + ' = ' + IntToStr(InstanzId_Archiv) + ') AND ' +
                           '(' + C_Tf_WMeldungen_LogbuchNr_Archiv + ' = ' + IntToStr(LogbuchNr_Archiv) + ')';
      tbWMeldungen.Filtered:=true;
      tbWMeldungen.Last;
      if not tbWMeldungen.BOF then begin
        OrdNr:=tbWMeldungen.FieldByName (C_Tf_WMeldungen_OrdnungsNr).AsInteger;
        DatumZeit:=tbWMeldungen.FieldByName (C_Tf_WMeldungen_DatumZeit).AsDateTime;
        Result:=true;
      end;
    finally
      tbWMeldungen.Filtered:=false;
      CloseMeldungenTable;
    end;
  end; { if OpenMeldungenDSfGTable }
end;

{-------------------------------------------------------------------------------------------------}
function TMeldungenDb.GetLetztVortagDSfGMeldungOrdNr_DatumZeit (Benutzer: string;
                                                                InstanzId_Archiv: integer;
                                                                LogbuchNr_Archiv: integer;
                                                                var OrdNr: integer;
                                                                var DatumZeit: TDateTime): boolean;
{-------------------------------------------------------------------------------------------------}
{ Ordnungsnummer und Datum/Zeit des jüngsten Logbucheintrags vom Vortag des letzten
  in der Tabelle befindlichen Logbucheintrags ermitteln;
  Übergabe: Benutzer (Automatik etc.)
            InstanzId der Registrier-Instanz
            Logbuch-Nr. der Registrier-Instanz
  Rückgabe: Ordnungsnummer und Datum/Zeit der Vortags-Meldung
  Ergebnis: true, wenn Meldung gefunden }
var
  letzt_DatumZeit: TDateTime;
  OrdNrBuf: integer;
  DatumZeitBuf: TDateTime;
begin
  Result:=false;
  DatumZeit:=-1;

  { DSfG-Meldungen: Suche in Haupt-Tabelle über InstanzId_Archiv und LogbuchNr_Archiv }
  if OpenMeldungenTable (false) then begin
    try
      { Filter auf Tabelle setzen: }
      tbWMeldungen.Filter:='(' + C_Tf_WMeldungen_Benutzer + ' = ''' + Benutzer + ''') AND ' +
                           '(' + C_Tf_WMeldungen_GeraeteArt + ' = ''' + C_GerArtDSfG + ''') AND ' +
                           '(' + C_Tf_WMeldungen_InstanzId_Archiv + ' = ' + IntToStr(InstanzId_Archiv) + ') AND ' +
                           '(' + C_Tf_WMeldungen_LogbuchNr_Archiv + ' = ' + IntToStr(LogbuchNr_Archiv) + ')';
      tbWMeldungen.Filtered:=true;
      tbWMeldungen.Last;
      letzt_DatumZeit:=-1;
      while not tbWMeldungen.BOF do begin
        OrdNrBuf:=tbWMeldungen.FieldByName (C_Tf_WMeldungen_OrdnungsNr).AsInteger;
        DatumZeitBuf:=tbWMeldungen.FieldByName (C_Tf_WMeldungen_DatumZeit).AsDateTime;
        if letzt_DatumZeit < 0 then        { Datum der letzten Meldung merken }
          letzt_DatumZeit:=DatumZeitBuf
        else begin
          if Int (DatumZeitBuf) < Int (letzt_DatumZeit) then begin
            { aktuelle Meldung ist vom Datum her jünger als die letzte in der Tabelle
              befindlichen Meldung }
            OrdNr:=OrdNrBuf;
            DatumZeit:=DatumZeitBuf;
            Result:=true;
            Break;
          end;
        end;
        tbWMeldungen.Prior;
      end;
    finally
      tbWMeldungen.Filtered:=false;
      CloseMeldungenTable;
    end;
  end; { if OpenMeldungenDSfGTable }
end;

{----------------------------------------------------------------------------------------------------}
function TMeldungenDb.DSfGMeldungAlreadyExists (ADatumZeit: TDateTime; AOrdnungsNr: integer): boolean;
{----------------------------------------------------------------------------------------------------}
{ prüfen, ob DSfG-Meldung mit übergebenem Datum/Zeit-Stempel und Ordnungsnummer
  bereits in der Tabelle enthalten ist;
  Achtung: Tabelle muß vor Aufruf der Methode geöffnet werden !
  Ergebnis: true, wenn Meldung bereits in der Tabelle enthalten ist }
begin
  if tbWMeldungen.Active then
    Result:=tbWMeldungen.FindKey([ADatumZeit, AOrdnungsNr])
  else
    Result:=false;
end;

{------------------------------------------------------------------------------}
function TMeldungenDb.WriteDSfGMeldung (MeldungenData: TMeldungenData): integer;
{------------------------------------------------------------------------------}
{ neue DSfG-Meldung in Haupttabelle WMeldungen.db einfügen;
  Achtung: Tabelle muß vor Aufruf der Methode geöffnet werden !
  Übergabe: Datenrecord für Tabelle
  Ergebnis: 0, falls einfügen nicht möglich
            sonst MeldungId des neuen Eintrags }
begin
  Result:=0;
  if tbWMeldungen.Active then begin
    with MeldungenData do
      tbWMeldungen.InsertRecord ([nil, Benutzer, GeraeteArt, GeraeteId,
                                  InstanzId_Archiv, LogbuchNr_Archiv, OrdnungsNr,
                                  DatumZeit, Zeitzone, MNrAllg, MNrGeraet,
                                  MText, MTyp, MArt, Quittiert, Bemerkung]);
    Result:=tbWMeldungen.FieldByName (C_Tf_WMeldungen_MeldungId).AsInteger;
  end;
end;

{----------------------------------------------------------------------------------}
procedure TMeldungenDb.WriteDSfGMeldungDSfG (MeldungenDSfGData: TMeldungenDSfGData);
{----------------------------------------------------------------------------------}
{ Datensatz in Detailtabelle WMeldungenDSfG.db einfügen;
  Achtung: Tabelle muß vor Aufruf der Methode geöffnet werden !
  Übergabe: Datenrecord für Tabelle }
begin
  if tbWMeldungenDSfG.Active then begin
    with MeldungenDSfGData do
      tbWMeldungenDSfG.InsertRecord ([MeldungId, Status, CRC]);
  end;
end;

{--------------------------------------------------------------------------------------}
procedure TMeldungenDb.DeleteDSfGMeldungen (Benutzer: string; InstanzId_Archiv: integer;
                                            LogbuchNr_Archiv: integer);
{--------------------------------------------------------------------------------------}
{ alle zu Benutzer, InstanzId und LogbuchNr gehörenden DSfG-Einträge aus den
  Meldungstabellen löschen;
  Übergabe: Benutzer
            InstanzId der Registrier-Instanz
            LogbuchNr der Registrier-Instanz (wenn <= 0 werden alle zu InstanzId_Archiv
            gehörenden Meldungen gelöscht, unabhängig von der LogbuchNr) }
var
  MeldungId: integer;
begin
  if OpenMeldungenTable (true) then begin    { Exklusiv öffnen zur Sicherheit }
    try
      if OpenMeldungenDSfGTable (true) then begin
        try
          { Filter auf Tabelle setzen: }
          tbWMeldungen.Filter:='(' + C_Tf_WMeldungen_Benutzer + ' = ''' + Benutzer + ''') AND ' +
                               '(' + C_Tf_WMeldungen_GeraeteArt + ' = ''' + C_GerArtDSfG + ''') AND ' +
                               '(' + C_Tf_WMeldungen_InstanzId_Archiv + ' = ' + IntToStr(InstanzId_Archiv) + ')';
          if LogbuchNr_Archiv > 0 then
            tbWMeldungen.Filter:=tbWMeldungen.Filter +
                                 ' AND (' + C_Tf_WMeldungen_LogbuchNr_Archiv + ' = ' + IntToStr(LogbuchNr_Archiv) + ')';

          tbWMeldungen.Filtered:=true;
          while not tbWMeldungen.Eof do begin
            MeldungId:=tbWMeldungen.FieldByName(C_Tf_WMeldungen_MeldungId).AsInteger;
            { aus WMeldungenDSfG löschen: }
            if tbWMeldungenDSfG.FindKey ([MeldungId]) then
              tbWMeldungenDSfG.Delete;
            { aus WMeldungen löschen: }
            tbWMeldungen.Delete;
          end;
        finally
          CloseMeldungenDSfGTable (true);
        end;
      end;
    finally
      tbWMeldungen.Filtered:=false;
      CloseMeldungenTable (true);
    end;
  end;
end;

{--------------------------------------------------------------------------}
function TMeldungenDb.ReorganizeDSfGMeldungen (InstanzId_Archiv: integer;
                                               LogbuchNr_Archiv: integer;
                                               MaxAnzahl: integer): boolean;
{--------------------------------------------------------------------------}
{ Automatik-DSfG-Meldungen in den Meldungstabellen reorganisieren ("Rundpuffer"), d.h.
  nur maximal die letzten MaxAnzahl Meldungen je Logbuch speichern, ältere Meldungen löschen;
  Übergabe: InstanzId der Registrier-Instanz
            LogbuchNr der Registrier-Instanz
            Max. Anzahl der Meldungen (wenn = 0, dann keine Organisation der Tabelle als Rundpuffer)
  Ergebnis: true, wenn Reorganisation durchgeführt wurde }
var
  MeldungId: integer;
  LoeschAnz: integer;
  i: integer;

begin
  Result:=false;
  if MaxAnzahl > 0 then begin
    if OpenMeldungenTable (true) then begin  { Exklusiv öffnen zur Sicherheit }
      try
        if OpenMeldungenDSfGTable (true) then begin
          try
            { Filter auf Tabelle setzen: }
            tbWMeldungen.Filter:=
              '(' + C_Tf_WMeldungen_Benutzer + ' = ''' + C_BenutzerAuto + ''') AND ' +
              '(' + C_Tf_WMeldungen_GeraeteArt + ' = ''' + C_GerArtDSfG + ''') AND ' +
              '(' + C_Tf_WMeldungen_InstanzId_Archiv + ' = ' + IntToStr(InstanzId_Archiv) + ') AND ' +
              '(' + C_Tf_WMeldungen_LogbuchNr_Archiv + ' = ' + IntToStr(LogbuchNr_Archiv) + ')';
            tbWMeldungen.Filtered:=true;

            LoeschAnz:=tbWMeldungen.RecordCount - MaxAnzahl;
            for i:=1 to LoeschAnz do begin
              tbWMeldungen.First;
              if not tbWMeldungen.Eof then begin
                MeldungId:=tbWMeldungen.FieldByName(C_Tf_WMeldungen_MeldungId).AsInteger;
                { aus WMeldungenDSfG löschen: }
                if tbWMeldungenDSfG.FindKey ([MeldungId]) then
                  tbWMeldungenDSfG.Delete;
                { aus WMeldungen löschen: }
                tbWMeldungen.Delete;
              end;
            end; { for }
          finally
            CloseMeldungenDSfGTable (true);
          end;
        end;
      finally
        tbWMeldungen.Filtered:=false;
        CloseMeldungenTable (true);
      end;
    end;
  end; { if MaxAnzahl > 0 }
end;

{------------------------------------------------------------------------------------------}
function TMeldungenDB.Get_LogbuchDaten_By_Zeit_OrdNr (InstanzId, LogbuchNr: integer;
                                                      Benutzer: string;
                                                      var ergebnis: TQueryExt;
                                                      VonDatumZeit, BisDatumZeit: TDateTime;
                                                      ONrVon: integer = -1;
                                                      ONrBis: integer = -1): boolean;
{------------------------------------------------------------------------------------------}
{ liefert Meldungen für übergebenen Zeitbereich (optional zusätzlich auch für
  Ordnungsnummern-Bereich) in Query zurück;
  Übergabe: InstanzId
            Logbuch-Nummer
            Benutzer
            von/bis-Zeitbereich
            von/bis-Ordnungsnummernbereich (optional)
  Rückgabe: Ergebnis-Query
  Ergebnis: false, wenn keine Daten für angeforderten Bereich vorhanden sind }
var
  Anzahl:Integer;

begin
  Anzahl:=0;
  try
    with ergebnis do begin
      close;
      Databasename:=Path;
      sql.Clear;
      sql.Add('Select '+C_Tf_WMeldungen_OrdnungsNr+','+C_Tf_WMeldungen_DatumZeit+','+C_Tf_WMeldungen_MNrGeraet+','+
                        C_Tf_WMeldungen_MTyp+','+C_Tf_WMeldungenDSfG_Status+','+
                        C_Tf_WMeldungenDSfG_CRC+','+C_Tf_WMeldungen_Zeitzone);
      sql.Add('from "'+tbWMeldungen.Tablename+'" l, "'+tbWMeldungenDSfG.Tablename+'" detail');
      sql.Add('Where (l.'+C_Tf_WMeldungen_MeldungId+' = detail.'+C_Tf_WMeldungenDSfG_MeldungId+')');
      sql.Add(' AND (l.'+C_Tf_WMeldungen_InstanzId_Archiv+' = '+inttostr(InstanzID)+')');
      sql.Add(' AND (l.'+C_Tf_WMeldungen_LogbuchNr_Archiv+' = '+inttostr(LogbuchNr)+')');
      sql.Add(' AND (l.'+C_Tf_WMeldungen_DatumZeit+' >= :VonDtZt) AND (l.'+ C_Tf_WMeldungen_DatumZeit+' <= :BisDtZt)');
      if (ONrVon > -1) AND (ONrBis > -1) then
        sql.Add(' AND (l.'+C_Tf_WMeldungen_OrdnungsNr+' >= :ov) AND (l.'+ C_Tf_WMeldungen_OrdnungsNr+' <= :ob)');

      ParamByname('VonDtZt').AsDatetime:=VonDatumZeit;
      ParamByname('BisDtZt').AsDateTime:=BisDatumZeit;
      if (ONrVon > -1) AND (ONrBis > -1) then begin
        ParamByname('ov').AsInteger:=ONrVon;
        ParamByname('ob').AsInteger:=ONrBis;
      end;
      if Open then Anzahl:=ergebnis.RecordCount;
    end;
  finally
    if Anzahl > 0 then
      Result:=true
    else
      Result:=false;
  end;
end;

end.
