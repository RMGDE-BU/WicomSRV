{******************************************************************************}
{* Unit: Tabellenzugriffe auf DSfG-Stammdaten                                 *}
{* 19.11.99  WW                                                               *}
{* 30.04.01  GD  Löschen von neuen Untertabellen                              *}
{* 24.01.02  GD  Erweitert um Werteart                                        *}
{* 13.02.02  GD  Erweitert um GetArchivKanalList                              *}
{* 02.05.02  GD  Erweitert um Feldinfos                                       *}
{* 15.04.03  WW  Erweitert um Löschfunktionen                                 *}
{* 20.05.03  WW  Erweitert um Zugriff auf Kavernen-Zuordnungstabellen         *}
{* 13.06.03  WW  Erweitert um Zugriff auf Konfigurationstabellen für          *}
{*               IEC-Leitwarte                                                *}
{******************************************************************************}
Unit DDbSta;

INTERFACE

uses
  Forms, DBTables, Classes,SysUtils, WStrUtils, WSysCon, DListen, DJLoesch,
  DbDSfGParams, DbDSfGMom,  // 30.04.2001
  JournlDb, TerminDb, AuftrgDb, RufeDb, MeldKonfigDb, DDbAbruf, DKavKonfigDb,
  IecKonfDb;

type

  { DSfG-Stammdaten-Objekt }

  TDSfGStammDaten = class (TObject)
  private
    Path: TFileName;
    { Tabellenobjekte }
    StationTable: TTable;
    InstanzTable: TTable;
    InstDfuTable: TTable;
    ArchiveTable: TTable;
    AKanaeleTable: TTable;
    LogbuchTable: TTable;
    DelAutoTable: TTable;
    DelInstTable: TTable;
    DelITypTable: TTable;
    DSfGDefTable: TTable;
    WerteartTable: TTable;  // 24.01.2002
    function GetInstDfuData (InstanzId: integer; var InstDfuData: TInstDfuData): boolean;
    function GetInstDfuDataByKennung (Kennung: string; var InstDfuData: TInstDfuData;
                                      var eindeutig: boolean): boolean;
    procedure GetAutoArchivAbrufListe(InstanzID: integer; Busadresse: string;
      Abrufliste: TAbrufList; bAutomatik: boolean = True);
    procedure GetAutoLogbuchAbrufListe(InstanzId: integer; Busadresse: string;
      Abrufliste: TAbrufList; bAutomatik: boolean = True);
    procedure GetAutoDatenelementeAbrufListe(InstanzId: integer; Busadresse: string; Abrufliste: TAbrufList);
    procedure GetDELInstAbrufListe (InstanzId: integer; Busadresse: string; vorher_nachher: char;
                                     Abrufliste: TAbrufList);
    procedure GetDELInstTypAbrufListe (InstanzId: integer; Instanztyp: string; Busadresse: string;
                                        vorher_nachher: char; Abrufliste: TAbrufList);
    function DeleteInstanzDataByStation (StationId: integer): boolean;
    procedure DeleteInstDfuData (InstanzId: integer);
    procedure DeleteArchiveDataByInstanz (InstanzId: integer);
    procedure DeleteArchiveData (InstanzId: integer; ArchivNr: integer);
    procedure DeleteAKanaeleDataByInstanz (InstanzId: integer);
    procedure DeleteAKanaeleDataByArchiv (InstanzId: integer; ArchivNr: integer);
    procedure DeleteLogbuchDataByInstanz (InstanzId: integer);
    procedure DeleteAKanaeleData (InstanzId: integer; ArchivNr: integer; KanalNr: integer);
    procedure DeleteLogbuchData (InstanzId: integer; LogbuchNr: integer);
    procedure DeleteDELAutoData (InstanzId: integer);
    procedure DeleteDELInstData (InstanzId: integer);
    function DeleteParamData (InstanzId: integer): boolean;  // 30.04.2001
    function DeleteMomTb (iStationsId: integer): boolean;  // 30.04.2001
  public
    constructor Create (APath: TFileName);
    destructor Destroy; override;
    function InitTabellen: boolean;
    procedure GetStationDataList (StationListe: TStationDataList);
    function GetRufStammDaten (StationId: integer; var RufStammDaten: TRufStammDaten): boolean;
    function GetRufStammDatenByKennung (Kennung: string; var RufStammDaten: TRufStammDaten;
                                        var eindeutig: boolean): boolean;
    procedure GetAutoDatenAbrufliste (StationId: integer; Datentyp: integer;
      Abrufliste: TAbrufList; bAutomatik: boolean = True);
    procedure GetDELInst_InstTypAbrufliste (StationId: integer; vorher_nachher: char; Abrufliste: TAbrufList);
    Procedure SetDelInst_InstTyp(InstTypList:TList;Modus:char);  // Sm
    procedure GetLogbuchAbruflisteByQuellBusadressen (StationId: integer; QuellBusadressen: string;
                                                      AbrufListe: TAbrufList);
    procedure SetAbrufList_InstanzId_GerTypNrQuelle (StationId: integer; AbrufListe: TAbrufList);

    function GetBusadressen (StationId: integer; ExcludedInstanztyp: char): string;
    function GetInstanzCount (StationId: integer): integer;
    procedure GetInstanzQuery (StationId: integer; var Query: TQuery);
    procedure GetArchivkanalQuery (InstanzId, ArchivNr: integer; var Query: TQuery);
    procedure GetStationsInstanznamen (Query: TQuery);
    function GetStationData (StationId: integer; var StationData: TStationData): boolean;
    function GetInstanzData (InstanzId: integer; var InstanzData: TInstanzData): boolean;
    function GetInstanzData_Name (StationId: integer; Instanzname: string; Instanztyp: char;
                                  var InstanzData: TInstanzData): boolean;
    function GetAKanaeleData (InstanzId: integer; ArchivNr: integer; KanalNr: integer;
                              var AKanaeleData: TAKanaeleData; bInfo: boolean = False): boolean;
    function GetArchivKanalList (iInstId, iArchivNr: integer;
      pArchivList: TAKanaeleDataList; bInfo: boolean = False): boolean;   // 13.02.2002
    function GetLogbuchData (InstanzId: integer; LogbuchNr: integer; var LogbuchData: TLogbuchData): boolean;
    function GetLoginInstanzData (StationId: integer;
                                  var InstanzData: TInstanzData; var InstDfuData: TInstDfuData): boolean;

    function InsertStationDataByFieldname (AFieldname: string; var StationData: TStationData): integer;
    function InsertInstanzData (var InstanzData: TInstanzData): integer;
    function InsertInstDfuData (InstDfuData: TInstDfuData): integer;
    procedure UpdateLoginInstanzname (StationId: integer; LoginInstanzname: string);
    function UpdateRufNr_LoginPW_LogPort (InstanzId: integer; LoginAdr: string;
                                          Rufnummer: string; Passwort: string; LogPort: integer): boolean;

    function UpdateInstanzDataByList (InstanzList: TInstanzDataList; AStationId: integer;
                                      EAdr_LoginDfue: string; EAdr_nichtloeschen: string;
                                      var LoginInstanzname: string): boolean;
    procedure UpdateInstDfuDataByList (InstDfuList: TInstDfuDataList);
    function UpdateArchiveDataByList (ArchiveList: TArchiveDataList; InstanzList: TInstanzDataList): boolean;
    function UpdateAKanaeleDataByList (AKanaeleList: TAKanaeleDataList; ArchiveList: TArchiveDataList): boolean;
    function UpdateLogbuchDataByList (LogbuchList: TLogbuchDataList; InstanzList: TInstanzDataList): boolean;

    procedure SetArchivkanalAutomatik (AKanaeleList: TAKanaeleDataList);
    procedure SetAlleArchivkanaeleAutomatik (StationId: integer; Automatik: boolean);
    procedure SetLogbuchAutomatik (LogbuchList: TLogbuchDataList);
    procedure SetAlleLogbuecherAutomatik (StationId: integer; Automatik: boolean);

    procedure SetLoginDFUEKennung (StationId: integer; Kennung: string);
    procedure SetLoginDFUELoginAdresse (StationId: integer; LoginAdr: string);
    procedure SetLoginDFUELoginPasswort (StationId: integer; LoginPW: string);
    procedure SetLoginDFUEAdresse (StationId: integer; AdrNr: integer; Adr: string);
    procedure SetLoginDFUEPasswort (StationId: integer; PWNr: integer; PW: string);
    procedure SetInstanzAdresse (InstanzId: integer; Adr: string);

    function DeleteStation (StationId: integer): boolean;
    function DeleteInstanz (InstanzId: integer): boolean;
    function DeleteArchiv (InstanzId: integer; ArchivNr: integer): boolean;
    function DeleteArchivkanal (InstanzId: integer; ArchivNr: integer; KanalNr: integer): boolean;
    function DeleteLogbuch (InstanzId: integer; LogbuchNr: integer): boolean;

    function GetDSfGDefData (GerTypNr: integer; var DSfGDefData: TDSfGDefData): boolean;
    function GetGeraetetypNrByName (AGerTypName_Geraet: string): integer;
    function StationsnameAlreadyExists (Stationsname: string;
                                        var Exists: boolean): boolean;
    function KennungAlreadyExists (Kennung: string; InstanzId_Excl: integer;
                                   var Exists: boolean): boolean;
    { für Kavernen-Stammsätze: }
    function CreateKavernenStation (Stationsname_Ziel: string; InstanzId_Quelle_Reg: integer;
                                    AnzKavInstanzen: integer): integer;
  end;


 { Prozeduren zur Belegung der Stammdatenrecords }

procedure SetStationDataRec (R: PStationData;
                             AStationId: integer; AStationsname: string; AAutomatik: boolean; AErsterAbruf: TDateTime;
                             ALoginInstanzname: string; AZweitname: string; APrioritaet: integer);
procedure SetInstanzDataRec (R: PInstanzData;
                             AInstanzId: integer; AStationId: integer; AInstanzname: string; AInstanztyp: string;
                             ABusadresse: string; AHersteller: string; AGeraetetyp: string; AStand: integer;
                             AFabrikNr: string; ASoftwareVersion: string; ABaujahr: integer; AInbetriebnahme: TDateTime;
                             AGerTypNr: integer);
procedure SetInstDfuDataRec (R: PInstDfuData;
                             AInstanzId: integer; AKennung: string; ARufnummer: string; AAdresse: array of string;
                             APasswort: array of string; ALogPort: integer);
procedure SetArchiveDataRec (R: PArchiveData;
                             AInstanzId: integer; AArchivNr: integer; AName: string; AAutomatik: boolean);
procedure SetAKanaeleDataRec (R: PAKanaeleData;
                              AKanalNr: integer; AInstanzId: integer; AArchivNr: integer; AName: string;
                              AKanaltyp: string; AWerteart: string; AEAdr: string; AAutomatik: boolean; AQuellDEL: string);
procedure SetLogbuchDataRec (R: PLogbuchData;
                             AInstanzId: integer; ALogbuchNr: integer; AName: string; AEAdr: string; AAutomatik: boolean);


IMPLEMENTATION

uses
  DDLoesch;

const
  NUL = #0;
  GS  = #29;


{--------------------------------------------------------------------------------------------------------------------}
procedure SetStationDataRec (R: PStationData;
                             AStationId: integer; AStationsname: string; AAutomatik: boolean; AErsterAbruf: TDateTime;
                             ALoginInstanzname: string; AZweitname: string; APrioritaet: integer);
{--------------------------------------------------------------------------------------------------------------------}
begin
  with R^ do begin
    StationId:=AStationId;
    Stationsname:=AStationsname;
    Automatik:=AAutomatik;
    ErsterAbruf:=AErsterAbruf;
    LoginInstanzname:=ALoginInstanzName;
    Zweitname:=AZweitname;
    Prioritaet:=APrioritaet;
  end;
end;

{----------------------------------------------------------------------------------------------------------------------}
procedure SetInstanzDataRec (R: PInstanzData;
                             AInstanzId: integer; AStationId: integer; AInstanzname: string; AInstanztyp: string;
                             ABusadresse: string; AHersteller: string; AGeraetetyp: string; AStand: integer;
                             AFabrikNr: string; ASoftwareVersion: string; ABaujahr: integer; AInbetriebnahme: TDateTime;
                             AGerTypNr: integer);
{----------------------------------------------------------------------------------------------------------------------}
begin
  with R^ do begin
    InstanzId:=AInstanzId;
    StationId:=AStationId;
    Instanzname:=AInstanzname;
    Instanztyp:=AInstanztyp;
    Busadresse:=ABusadresse;
    Hersteller:=AHersteller;
    Geraetetyp:=AGeraetetyp;
    Stand:=AStand;
    FabrikNr:=AFabrikNr;
    SoftwareVersion:=ASoftwareVersion;
    Baujahr:=ABaujahr;
    Inbetriebnahme:=AInbetriebnahme;
    GerTypNr:=AGerTypNr;
  end;
end;

{----------------------------------------------------------------------------------------------------------------}
procedure SetInstDfuDataRec (R: PInstDfuData;
                             AInstanzId: integer; AKennung: string; ARufnummer: string; AAdresse: array of string;
                             APasswort: array of string; ALogPort: integer);
{----------------------------------------------------------------------------------------------------------------}
var
  i: integer;
begin
  with R^ do begin
    InstanzId:=AInstanzId;
    Kennung:=AKennung;
    Rufnummer:=ARufnummer;
    for i:=Low (Adresse) to High (Adresse) do begin
      if (i >= Low (AAdresse)+1) AND (i <= High (AAdresse)+1) then
        Adresse[i]:=AAdresse[i-1]
      else
        Adresse[i]:='';
    end;
    for i:=Low (Passwort) to High (Passwort) do begin
      if (i >= Low (APasswort)+1) AND (i <= High (APasswort)+1) then
        Passwort[i]:=APasswort[i-1]
      else
        Passwort[i]:='';
    end;
    LogPort:=ALogPort;
  end;
end;

{--------------------------------------------------------------------------------------------------------}
procedure SetArchiveDataRec (R: PArchiveData;
                             AInstanzId: integer; AArchivNr: integer; AName: string; AAutomatik: boolean);
{--------------------------------------------------------------------------------------------------------}
begin
  with R^ do begin
    InstanzId:=AInstanzId;
    ArchivNr:=AArchivNr;
    Name:=AName;
    Automatik:=AAutomatik;
  end;
end;

{-------------------------------------------------------------------------------------------------------------------------}
procedure SetAKanaeleDataRec (R: PAKanaeleData;
                              AKanalNr: integer; AInstanzId: integer; AArchivNr: integer; AName: string;
                              AKanaltyp: string; AWerteart: string; AEAdr: string; AAutomatik: boolean; AQuellDEL: string);
{-------------------------------------------------------------------------------------------------------------------------}
begin
  with R^ do begin
    KanalNr:=AKanalNr;
    InstanzId:=AInstanzId;
    ArchivNr:=AArchivNr;
    Name:=AName;
    Kanaltyp:=AKanaltyp;
    Werteart:=AWerteart;
    EAdr:=AEAdr;
    Automatik:=AAutomatik;
    QuellDEL:=AQuellDEL;
  end;
end;

{------------------------------------------------------------------------------------------------------------------------}
procedure SetLogbuchDataRec (R: PLogbuchData;
                             AInstanzId: integer; ALogbuchNr: integer; AName: string; AEAdr: string; AAutomatik: boolean);
{------------------------------------------------------------------------------------------------------------------------}
begin
  with R^ do begin
    InstanzId:=AInstanzId;
    LogbuchNr:=ALogbuchNr;
    Name:=AName;
    EAdr:=AEAdr;
    Automatik:=AAutomatik;
  end;
end;


{ TDSfGStammDaten }

{-----------------------------------------------}
constructor TDSfGStammDaten.Create (APath:TFileName);
{-----------------------------------------------}
begin
  inherited Create;
  Path:=APath;
end;

{-----------------------------}
destructor TDSfGStammDaten.Destroy;
{-----------------------------}
begin
  DSfGDefTable.Free;
  DelITypTable.Free;
  DelInstTable.Free;
  DelAutoTable.Free;
  LogbuchTable.Free;
  AKanaeleTable.Free;
  ArchiveTable.Free;
  InstDfuTable.Free;
  InstanzTable.Free;
  StationTable.Free;
  WerteartTable.Free;
  inherited Destroy;
end;

{-----------------------------------------}
function TDSfGStammDaten.InitTabellen: boolean;
{-----------------------------------------}
{ Initialisierung für Zugriff auf Stammdaten-Tabellen;
  Ergebnis: true, wenn alle Stammdaten-Tabellen vorhanden sind }
var
  sPath : TFileName;  // 13.02.2002
begin
  Result := false;

  sPath := IncludeTrailingBackslash(Path);
  if FileExists(sPath+C_DTB_Station) AND
     FileExists(sPath+C_DTB_Instanz) AND
     FileExists(sPath+C_DTB_InstDfu) AND
     FileExists(sPath+C_DTB_Archive) AND
     FileExists(sPath+C_DTB_AKanaele) AND
     FileExists(sPath+C_DTB_Logbuch) AND
     FileExists(sPath+C_DTB_DDelAuto) AND
     FileExists(sPath+C_DTB_DDelInst) AND
     FileExists(sPath+C_DTB_DDelITyp) AND
     FileExists(sPath+ChangeFileExt(C_Tb_Werteart, '.DB')) then begin  // 24.01.2002

    StationTable:=TTable.Create(nil);
    StationTable.DatabaseName:=Path;
    StationTable.TableName:=C_DTB_Station;

    InstanzTable:=TTable.Create(nil);
    InstanzTable.DatabaseName:=Path;
    InstanzTable.TableName:=C_DTB_Instanz;

    InstDfuTable:=TTable.Create(nil);
    InstDfuTable.DatabaseName:=Path;
    InstDfuTable.TableName:=C_DTB_InstDfu;

    ArchiveTable:=TTable.Create(nil);
    ArchiveTable.DatabaseName:=Path;
    ArchiveTable.TableName:=C_DTB_Archive;

    AKanaeleTable:=TTable.Create(nil);
    AKanaeleTable.DatabaseName:=Path;
    AKanaeleTable.TableName:=C_DTB_AKanaele;

    LogbuchTable:=TTable.Create(nil);
    LogbuchTable.DatabaseName:=Path;
    LogbuchTable.TableName:=C_DTB_Logbuch;

    DelAutoTable:=TTable.Create(nil);
    DelAutoTable.DatabaseName:=Path;
    DelAutoTable.TableName:=C_DTB_DDelAuto;

    DelInstTable:=TTable.Create(nil);
    DelInstTable.DatabaseName:=Path;
    DelInstTable.TableName:=C_DTB_DDelInst;

    DelITypTable:=TTable.Create(nil);
    DelITypTable.DatabaseName:=Path;
    DelITypTable.TableName:=C_DTB_DDelITyp;

    DSfGDefTable:=TTable.Create(nil);
    DSfGDefTable.DatabaseName:=Path;
    DSfGDefTable.TableName:=C_DTB_DSfGDef;

    WerteartTable := TTable.Create(nil);  // 24.01.2002
    WerteartTable.DatabaseName := Path;
    WerteartTable.TableName := C_Tb_Werteart;

    Result:=true;
  end;
end;

{-----------------------------------------------------------------------------------------------}
function TDSfGStammDaten.GetStationData (StationId: integer; var StationData: TStationData): boolean;
{-----------------------------------------------------------------------------------------------}
{ liefert Datenrecord aus STATION.DB über StationId;
  Übergabe: StationId
  Rückgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
begin
  Result := false;
  if StationTable.Exists then begin
    StationTable.Open;
    try
      if StationTable.FindKey ([StationId]) then begin
        with StationData do begin
          StationId := StationTable.FieldByName (C_DTF_Station_StationId).AsInteger;
          Stationsname := StationTable.FieldByName (C_DTF_Station_Stationsname).AsString;
          Automatik := StationTable.FieldByName (C_DTF_Station_Automatik).AsBoolean;
          ErsterAbruf := StationTable.FieldByName (C_DTF_Station_ErsterAbruf).AsDateTime;
          LoginInstanzname := StationTable.FieldByName (C_DTF_Station_LIInstanz).AsString;
          Zweitname := StationTable.FieldByName (C_DTF_Station_Zweitname).AsString;
          Prioritaet := StationTable.FieldByName (C_DTF_Station_Prioritaet).AsInteger;
        end;
        Result:=true;
      end;
    finally
      StationTable.Close;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetStationDataList (StationListe: TStationDataList);
{----------------------------------------------------------------------------}
{ liefert Liste aller Datenrecords aus STATION.DB;
  Rückgabe: StationListe }
var
  StationData: PStationData;
begin
  if StationTable.Exists then begin
    StationTable.Open;
    try
      while not StationTable.Eof do begin
        New (StationData);
        SetStationDataRec (StationData,
                           StationTable.FieldByName (C_DTF_Station_StationId).AsInteger,
                           StationTable.FieldByName (C_DTF_Station_Stationsname).AsString,
                           StationTable.FieldByName (C_DTF_Station_Automatik).AsBoolean,
                           StationTable.FieldByName (C_DTF_Station_ErsterAbruf).AsDateTime,
                           StationTable.FieldByName (C_DTF_Station_LIInstanz).AsString,
                           StationTable.FieldByName (C_DTF_Station_Zweitname).AsString,
                           StationTable.FieldByName (C_DTF_Station_Prioritaet).AsInteger);
        StationListe.Add (StationData);
        StationTable.Next;
      end;
    finally
      StationTable.Close;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetInstanzQuery (StationId: integer; var Query: TQuery);
{----------------------------------------------------------------------------}
{ liefert alle Datenrecords aus INSTANZ.DB für StationId;
  Übergabe: StationId
  Rückgabe: Query }
begin
  Query.Close;
  Query.Sql.Clear;
  Query.DatabaseName:=Path;
  Query.Sql.Add ('SELECT * FROM "' + C_DTB_Instanz + '"');
  Query.Sql.Add ('WHERE ' + C_DTF_Instanz_StationId + ' = ' + IntToStr (StationId));
  Query.Open;
end;

{----------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetArchivkanalQuery (InstanzId, ArchivNr: integer; var Query: TQuery);
{----------------------------------------------------------------------------------------------}
{ liefert alle Datenrecords aus AKANAELE.DB für InstanzId und Archivnummer;
  Übergabe: InstanzId
            Archivnummer
  Rückgabe: Query }
begin
  Query.Close;
  Query.Sql.Clear;
  Query.DatabaseName:=Path;
  Query.Sql.Add ('SELECT * FROM "' + C_DTB_AKanaele + '"');
  Query.Sql.Add ('WHERE ' + C_DTF_AKanaele_InstanzId + ' = ' + IntToStr (InstanzId) + ' AND ');
  Query.Sql.Add (C_DTF_AKanaele_ArchivNr + ' = ' + IntToStr (ArchivNr));
  Query.Open;
end;

{----------------------------------------------------------------}
procedure TDSfGStammdaten.GetStationsInstanznamen (Query: TQuery);
{----------------------------------------------------------------}
{ Instanznamen mit Stationsnamen aller Stationen aus Tabellen lesen und
  in Query zurückgeben;
  Rückgabe: Query }
begin
  with Query do begin
    Close;
    DataBaseName:=Path;
    Sql.Clear;
    Sql.Add('SELECT A.' + C_DTF_Instanz_InstanzId + ',');
    Sql.Add('A.' + C_DTF_Instanz_Instanzname + ',');
    Sql.Add('B.' + C_DTF_Station_Stationsname);
    Sql.Add('FROM "' + C_DTB_Instanz + '" A, ');
    Sql.Add('"' + C_DTB_Station + '" B');
    Sql.Add('WHERE A.' + C_DTF_Instanz_StationId + ' = B.' + C_DTF_Station_StationId);
    Open;
  end;
end;

{-----------------------------------------------------------------------------------------------}
function TDSfGStammDaten.GetInstanzData (InstanzId: integer; var InstanzData: TInstanzData): boolean;
{-----------------------------------------------------------------------------------------------}
{ liefert Datenrecord aus INSTANZ.DB über InstanzId;
  Übergabe: StationId
            InstanzId
  Rückgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
begin
  Result := false;
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      if InstanzTable.FindKey ([InstanzId]) then begin
        with InstanzData do begin
          InstanzId := InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
          StationId := InstanzTable.FieldByName (C_DTF_Instanz_StationId).AsInteger;
          Instanzname := InstanzTable.FieldByName (C_DTF_Instanz_Instanzname).AsString;
          Instanztyp := InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString;
          Busadresse := InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString;
          Hersteller := InstanzTable.FieldByName (C_DTF_Instanz_Hersteller).AsString;
          Geraetetyp := InstanzTable.FieldByName (C_DTF_Instanz_Geraetetyp).AsString;
          Stand := InstanzTable.FieldByName (C_DTF_Instanz_Stand).AsInteger;
          FabrikNr := InstanzTable.FieldByName (C_DTF_Instanz_FabrikNr).AsString;
          SoftwareVersion := InstanzTable.FieldByName (C_DTF_Instanz_SoftwareVs).AsString;
          Baujahr:=InstanzTable.FieldByName (C_DTF_Instanz_Baujahr).AsInteger;
          Inbetriebnahme:=InstanzTable.FieldByName (C_DTF_Instanz_Inbetriebnahme).AsDateTime;
          GerTypNr:=InstanzTable.FieldByName (C_DTF_Instanz_GerTypNr).AsInteger;
        end;
        Result:=true;
      end;
    finally
      InstanzTable.Close;
    end;
  end;
end;

{--------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.GetInstanzData_Name (StationId: integer; Instanzname: string; Instanztyp: char;
                                          var InstanzData: TInstanzData): boolean;
{--------------------------------------------------------------------------------------------------}
{ liefert Datenrecord aus INSTANZ.DB über StationId, Instanzname und Instanztyp;
  Übergabe: StationId
            Instanzname
            Instanztyp
  Rückgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
begin
  Result := false;
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      { Filter auf StationId }
      InstanzTable.Filter:=C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId);
      InstanzTable.Filtered:=true;

      while not InstanzTable.Eof do begin
        if (InstanzTable.FieldByName (C_DTF_Instanz_Instanzname).AsString = Instanzname) AND
           (InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString = Instanztyp) then begin
          with InstanzData do begin
            InstanzId := InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
            StationId := InstanzTable.FieldByName (C_DTF_Instanz_StationId).AsInteger;
            Instanzname := InstanzTable.FieldByName (C_DTF_Instanz_Instanzname).AsString;
            Instanztyp := InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString;
            Busadresse := InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString;
            Hersteller := InstanzTable.FieldByName (C_DTF_Instanz_Hersteller).AsString;
            Geraetetyp := InstanzTable.FieldByName (C_DTF_Instanz_Geraetetyp).AsString;
            Stand := InstanzTable.FieldByName (C_DTF_Instanz_Stand).AsInteger;
            FabrikNr := InstanzTable.FieldByName (C_DTF_Instanz_FabrikNr).AsString;
            SoftwareVersion := InstanzTable.FieldByName (C_DTF_Instanz_SoftwareVs).AsString;
            Baujahr:=InstanzTable.FieldByName (C_DTF_Instanz_Baujahr).AsInteger;
            Inbetriebnahme:=InstanzTable.FieldByName (C_DTF_Instanz_Inbetriebnahme).AsDateTime;
            GerTypNr:=InstanzTable.FieldByName (C_DTF_Instanz_GerTypNr).AsInteger;
          end;
          Result:=true;
          Break;
        end;
        InstanzTable.Next;
      end;
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------------------------------------------}
function TDSfGStammDaten.GetInstDfuData (InstanzId: integer; var InstDfuData: TInstDfuData): boolean;
{-----------------------------------------------------------------------------------------------}
{ liefert Datenrecord aus INSTDFU.DB über InstanzId;
  Übergabe: InstanzId
  Rückgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
begin
  Result := false;
  if InstDfuTable.Exists then begin
    InstDfuTable.Open;
    try
      if InstDfuTable.FindKey ([InstanzId]) then begin
        with InstDfuData do begin
          InstanzId := InstDfuTable.FieldByName (C_DTF_InstDfu_InstanzId).AsInteger;
          Kennung := InstDfuTable.FieldByName (C_DTF_InstDfu_Kennung).AsString;
          Rufnummer := InstDfuTable.FieldByName (C_DTF_InstDfu_Rufnummer).AsString;
          Adresse[1] := InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse1).AsString;
          Adresse[2] := InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse2).AsString;
          Adresse[3] := InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse3).AsString;
          Adresse[4] := InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse4).AsString;
          Passwort[1] := InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort1).AsString;
          Passwort[2] := InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort2).AsString;
          Passwort[3] := InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort3).AsString;
          Passwort[4] := InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort4).AsString;
          LogPort := InstDfuTable.FieldByName (C_DTF_InstDfu_LogPort).AsInteger;
        end;
        Result:=true;
      end;
    finally
      InstDfuTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------------------------------------------}
function TDSfGStammDaten.GetInstDfuDataByKennung (Kennung: string; var InstDfuData: TInstDfuData;
                                                  var eindeutig: boolean): boolean;
{-----------------------------------------------------------------------------------------------}
{ liefert Datenrecord aus INSTDFU.DB über Kennung; bei mehrfach vorhandener Kennung wird der
  letzte gefundene Stammsatz zurückgegeben;
  Übergabe: Kennung
  Rückgabe: Datenrecord
            Eindeutigkeit des Datenrecords (es können mehrere DFÜ-Instanz-Datensätze mit
            gleicher Kennung existieren !)
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
var
  count: integer;
begin
  Result:=false;
  eindeutig:=true;       { Vorbelegung: Datensatz "eindeutig nicht vorhanden" }
  if InstDfuTable.Exists then begin
    InstDfuTable.Open;
    try
      count:=0;
      while not InstDfuTable.Eof do begin
        if Kennung = InstDfuTable.FieldByName (C_DTF_InstDfu_Kennung).AsString then begin
          with InstDfuData do begin
            InstanzId := InstDfuTable.FieldByName (C_DTF_InstDfu_InstanzId).AsInteger;
            Kennung := InstDfuTable.FieldByName (C_DTF_InstDfu_Kennung).AsString;
            Rufnummer := InstDfuTable.FieldByName (C_DTF_InstDfu_Rufnummer).AsString;
            Adresse[1] := InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse1).AsString;
            Adresse[2] := InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse2).AsString;
            Adresse[3] := InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse3).AsString;
            Adresse[4] := InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse4).AsString;
            Passwort[1] := InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort1).AsString;
            Passwort[2] := InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort2).AsString;
            Passwort[3] := InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort3).AsString;
            Passwort[4] := InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort4).AsString;
            LogPort := InstDfuTable.FieldByName (C_DTF_InstDfu_LogPort).AsInteger;
          end;
          inc (count);
          eindeutig:=count = 1;
          Result:=true;
        end;  { if Kennung }
        InstDfuTable.Next;
      end;  { while }
    finally
      InstDfuTable.Close;
    end;
  end;
end;

{--------------------------------------------------------------------------------------------}
function TDSfGStammDaten.GetAKanaeleData (InstanzId: integer; ArchivNr: integer; KanalNr: integer;
                             var AKanaeleData: TAKanaeleData; bInfo: boolean = False): boolean;
{--------------------------------------------------------------------------------------------}
{ liefert Datenrecord aus AKANAELE.DB über InstanzId, ArchivNr und KanalNr;
  Übergabe: InstanzId
            ArchivNr
            KanalNr
            Flag, ob auch Infos (Kommastellen) zurückgeliefert werden sollen
  Rückgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
begin
  Result := false;
  if AKanaeleTable.Exists then begin
    AKanaeleTable.Open;
    try
      if AKanaeleTable.FindKey ([KanalNr, InstanzId, ArchivNr]) then begin
        with AKanaeleData do begin
          KanalNr := AKanaeleTable.FieldByName (C_DTF_AKanaele_KanalNr).AsInteger;
          InstanzId := AKanaeleTable.FieldByName (C_DTF_AKanaele_InstanzId).AsInteger;
          ArchivNr := AKanaeleTable.FieldByName (C_DTF_AKanaele_ArchivNr).AsInteger;
          Name := AKanaeleTable.FieldByName (C_DTF_AKanaele_Name).AsString;
          Kanaltyp := AKanaeleTable.FieldByName (C_DTF_AKanaele_Kanaltyp).AsString;
          Werteart := AKanaeleTable.FieldByName (C_DTF_AKanaele_Werteart).AsString;
          EADR := AKanaeleTable.FieldByName (C_DTF_AKanaele_EADR).AsString;
          Automatik := AKanaeleTable.FieldByName (C_DTF_AKanaele_Automatik).AsBoolean;
          QuellDEL := AKanaeleTable.FieldByName (C_DTF_AKanaele_QuellDEL).AsString;

          // Weiter infos ausgeben, falls angefordert
          if (bInfo) then begin  // 24.01.2002
             if (WerteartTable.Exists) then begin
               WerteartTable.Open;
               try
                 if (WerteartTable.FindKey ([WerteArt]))
                 then Kommastellen := WerteartTable.FieldByName(
                   C_Tf_Werteart_Kommastellen).asInteger
                 else Kommastellen := 0;
               finally
                 WerteartTable.Close;
               end;
             end;
          end
          else Kommastellen := 0;
        end;

        Result:=true;
      end;
    finally
      AKanaeleTable.Close;
    end;
  end;
end;

{ Liefert Liste der Kanäle einer Archivgruppe zurück                   }
{ Parameter: InstanzId, Archivgruppennr., Kanalnr., Übergabeliste, Flag}
{ Rückgabe: Erfolg ja/nein                                             }
{----------------------------------------------------------------------}
function TDSfGStammDaten.GetArchivKanalList (
  iInstId, iArchivNr: integer; pArchivList: TAKanaeleDataList;
  bInfo: boolean = False): boolean;   // 13.02.2002
{----------------------------------------------------------------------}
var
  pKanalData : PAKanaeleData;
  i          : integer;
begin
  Result := False;

  if (ArchiveTable.Exists) then begin
    ArchiveTable.Open;
    try
      if ArchiveTable.FindKey ([iInstId, iArchivNr]) then begin

        // Schleife über alle Kanäle
        for i := 1 to 21 do begin
          New(pKanalData);
          if (GetAKanaeleData(iInstId, iArchivNr, i, pKanalData^, bInfo))
          then pArchivList.Add(pKanalData)
          else Dispose(pKanalData);
        end;
        
        Result := True;
      end;
    finally
      ArchiveTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.GetLogbuchData (InstanzId: integer; LogbuchNr: integer; var LogbuchData: TLogbuchData): boolean;
{-------------------------------------------------------------------------------------------------------------------}
{ liefert Datenrecord aus LOGBUCH.DB über InstanzId und LogbuchNr;
  Übergabe: InstanzId
            LogbuchNr
  Rückgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
begin
  Result := false;
  if LogbuchTable.Exists then begin
    LogbuchTable.Open;
    try
      if LogbuchTable.FindKey ([InstanzId, LogbuchNr]) then begin
        with LogbuchData do begin
          InstanzId := LogbuchTable.FieldByName (C_DTF_Logbuch_InstanzId).AsInteger;
          LogbuchNr := LogbuchTable.FieldByName (C_DTF_Logbuch_LogbuchNr).AsInteger;
          Name := LogbuchTable.FieldByName (C_DTF_Logbuch_Name).AsString;
          EADR := LogbuchTable.FieldByName (C_DTF_Logbuch_EADR).AsString;
          Automatik := LogbuchTable.FieldByName (C_DTF_Logbuch_Automatik).AsBoolean;
        end;
        Result:=true;
      end;
    finally
      LogbuchTable.Close;
    end;
  end;
end;

{------------------------------------------------------------------------------------}
function TDSfGStammdaten.GetLoginInstanzData (StationId: integer;
                                              var InstanzData: TInstanzData;
                                              var InstDfuData: TInstDfuData): boolean;
{------------------------------------------------------------------------------------}
{ liefert Datenrecords der Login-DFÜ-Instanz einer Station aus INSTANZ.DB und INST_DFU.DB;
  Übergabe: StationId
  Rückgabe: Datenrecords
  Ergebnis: true, wenn beide Datensätze in Tabellen gefunden }
var
  StationData: TStationData;

begin
  Result := false;
  if GetStationData (StationId, StationData) then begin
    if GetInstanzData_Name (StationId, StationData.LoginInstanzname, C_D_Instanztyp_DFU, InstanzData) then begin
      if GetInstDfuData (InstanzData.InstanzId, InstDfuData) then
        Result:=true;
    end;
  end;  { if GetStationData }
end;

{---------------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.GetRufStammDaten (StationId: integer; var RufStammDaten: TRufStammDaten): boolean;
{---------------------------------------------------------------------------------------------------------}
{ Rufstammdaten über StationId ermitteln
  Ergebnis: true, wenn alle erforderlichen Rufstammdaten-Felder vorhanden }
var
  StationData: TStationData;
  InstanzData: TInstanzData;
  InstDfuData: TInstDfuData;
  i: byte;

begin
  Result := false;
  if GetStationData (StationId, StationData) then begin
    if GetInstanzData_Name (StationId, StationData.LoginInstanzname, C_D_Instanztyp_DFU, InstanzData) then begin
      if GetInstDfuData (InstanzData.InstanzId, InstDfuData) then begin
        RufStammDaten.StationId := StationId;
        RufStammDaten.StationsName := StationData.StationsName;
        RufStammDaten.Kennung := InstDfuData.Kennung;
        RufStammDaten.Rufnummer := InstDfuData.Rufnummer;
        RufStammDaten.LogPort :=InstDfuData.LogPort;
        RufStammdaten.LoginInstanzId:=InstDfuData.InstanzId;
        for i:= Low (InstDfuData.Adresse) to High (InstDfuData.Adresse) do begin
          if InstDfuData.Adresse [i] = InstanzData.Busadresse then begin
            if InstDfuData.Adresse [i] <> '' then begin
              RufStammDaten.Passwort := InstDfuData.Passwort [i];
              RufStammdaten.Busadresse := InstDfuData.Adresse [i];
              Result:=true;
            end;
            Break;
          end;
        end;
      end;
    end;
  end;  { if GetStationData }
end;

{-----------------------------------------------------------------------------------------------------}
function TDSfGStammdaten.GetRufStammDatenByKennung (Kennung: string; var RufStammDaten: TRufStammDaten;
                                                    var eindeutig: boolean): boolean;
{-----------------------------------------------------------------------------------------------------}
{ Rufstammdaten über Kennung ermitteln;
  Übergabe: Kennung
  Rückgabe: Stammsatz
            Eindeutigkeit des Stammsatzes (false, wenn mehrere Stammsätze zu der
            übergebenen Kennung existieren }
var
  StationData: TStationData;
  InstanzData: TInstanzData;
  InstDfuData: TInstDfuData;
  i: byte;

begin
  Result := false;
  if GetInstDfuDataByKennung (Kennung, InstDfuData, eindeutig) then begin
    if GetInstanzData (InstDfuData.InstanzId, InstanzData) then begin
      if GetStationData (InstanzData.StationId, StationData) then begin
        RufStammDaten.StationId := StationData.StationId;
        RufStammDaten.StationsName := StationData.Stationsname;
        RufStammDaten.Kennung := InstDfuData.Kennung;
        RufStammDaten.Rufnummer := InstDfuData.Rufnummer;
        RufStammDaten.LogPort := InstDfuData.LogPort;
        RufStammdaten.LoginInstanzId:=InstDfuData.InstanzId;
        for i:= Low (InstDfuData.Adresse) to High (InstDfuData.Adresse) do begin
          if InstDfuData.Adresse [i] = InstanzData.Busadresse then begin
            if InstDfuData.Adresse [i] <> '' then begin
              RufStammDaten.Passwort := InstDfuData.Passwort [i];
              RufStammdaten.Busadresse := InstDfuData.Adresse [i];
              Result:=true;
            end;
            Break;
          end;
        end;
      end;
    end;
  end;  { if GetInstDfuDataByKennung }
end;

{------------------------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetAutoArchivAbrufListe(InstanzID: integer;
  Busadresse: string; Abrufliste: TAbrufList; bAutomatik: boolean = True);
{------------------------------------------------------------------------------------------------------------}
{ erzeugt Liste aller automatisch abzurufenden Archivkanäle einer Instanz;
  Übergabe: InstanzId
            Busadresse der Instanz
  Rückgabe: Abrufliste }
var
  ArchivNr: integer;
  Kanal: integer;
  Kanaltyp, Werteart : string;
  AbrufListObj       : TAbrufListObj;
  sQuellBusAdr       : string[1];
begin
  if ArchiveTable.Exists AND AKanaeleTable.Exists then begin
    ArchiveTable.Open;
    try
      AKanaeleTable.IndexName:=CSArchivK;                         { sortiert nach InstanzID, ArchivNr }
      try
        AKanaeleTable.Open;
        try
          { Filter auf InstanzId und Automatik = 'true' }
          ArchiveTable.Filter:='(' + C_DTF_Archive_InstanzId + ' = ' + IntToStr(InstanzId) + ') AND ' +
                               '(' + C_DTF_Archive_Automatik + ' = ''TRUE'')';
          ArchiveTable.Filtered:=true;

          while not ArchiveTable.EOF do begin
            ArchivNr:=ArchiveTable.FieldByName(C_DTF_Archive_ArchivNr).AsInteger;

            { Filter auf InstanzID, ArchivNr und Automatik = 'true' }
            AKanaeleTable.Filtered:=false;
            AKanaeleTable.Filter:='(' + C_DTF_AKanaele_InstanzID + ' = ' + IntToStr(InstanzId) + ') AND ' +
                                  '(' + C_DTF_AKanaele_ArchivNr + ' = ' + IntToStr(ArchivNr) + ')';
            if (bAutomatik) then  // 02.05.2002
              AKanaeleTable.Filter := AKanaeleTable.Filter +
                 ' AND (' + C_DTF_AKanaele_Automatik + ' = ''TRUE'')';
            AKanaeleTable.Filtered:=true;

            while not AKanaeleTable.EOF do begin
              Kanal:=AKanaeleTable.FieldByName(C_DTF_AKanaele_KanalNr).AsInteger;
              Kanaltyp:=AKanaeleTable.FieldByName(C_DTF_AKanaele_Kanaltyp).AsString;
              Werteart:=AKanaeleTable.FieldByName(C_DTF_AKanaele_Werteart).AsString;
              sQuellBusAdr :=   // 02.05.2002
                AKanaeleTable.FieldByName(C_DTF_AKanaele_EADR).AsString;
              AbrufListObj:=TAbrufListObj.Create;
              AbrufListObj.SetData (Busadresse,
                                    InstanzId, ArchivNr, Kanal,
                                    Kanaltyp, Werteart, sQuellBusAdr,
                                    '', '', 0, 0, -1, -1);    { für Archive nicht relevant }
              AbrufListe.Add (AbrufListObj);
              AKanaeleTable.Next;
            end;  { while not AKanaeleTable.EOF }

            ArchiveTable.Next;
          end;  { while not ArchiveTable.EOF }
        finally
          AKanaeleTable.Filtered:=false;
          AKanaeleTable.Close;
        end;
      finally
        AKanaeleTable.IndexName:='';
      end;
    finally
      ArchiveTable.Filtered:=false;
      ArchiveTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetAutoLogbuchAbrufListe(InstanzId: integer;
  Busadresse: string; Abrufliste: TAbrufList; bAutomatik: boolean = True);
{-------------------------------------------------------------------------------------------------------------}
{ erzeugt Liste aller automatisch abzurufenden Logbücher einer Instanz;
  Übergabe: InstanzId
            Busadresse der Instanz
  Rückgabe: Abrufliste }
var
  AbrufListObj: TAbrufListObj;
  LogbuchNr: integer;
  QuellAdr: string;

begin
  if LogbuchTable.Exists then begin
    LogbuchTable.Open;
    try
      { Filter auf InstanzId und Automatik = 'true' }
      LogbuchTable.Filter:='(' + C_DTF_Logbuch_InstanzID + ' = ' + IntToStr(InstanzId) + ')';
      if (bAutomatik) then             // 02.05.2002
        LogbuchTable.Filter := LogbuchTable.Filter + ' AND ' +
                           '(' + C_DTF_Logbuch_Automatik + ' = ''TRUE'')';
      LogbuchTable.Filtered:=true;

      while not LogbuchTable.EOF do begin
        LogbuchNr:=LogbuchTable.FieldByName(C_DTF_Logbuch_LogbuchNr).AsInteger;
        QuellAdr:=LogbuchTable.FieldByName(C_DTF_Logbuch_EADR).AsString;
        if QuellAdr <> '' then begin
          AbrufListObj:=TAbrufListObj.Create;
          AbrufListObj.SetData (Busadresse, InstanzId, LogbuchNr,
                                -1, '', '',    { für Logbücher nicht relevant }
                                QuellAdr,
                                '', '', 0, 0,  { für Logbücher nicht relevant }
                                -1, -1);       { Id und Gerätetyp-Nr. der Quell-Instanz hier nur vorbelegen }
          AbrufListe.Add (AbrufListObj);
        end;
        LogbuchTable.Next;
      end;
    finally
      LogbuchTable.Filtered:=false;
      LogbuchTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetAutoDatenelementeAbrufListe(InstanzId: integer; Busadresse: string; Abrufliste: TAbrufList);
{-------------------------------------------------------------------------------------------------------------------}
{ erzeugt Liste aller automatisch abzurufenden Datenelemente einer Instanz;
  Übergabe: InstanzId
            Busadresse der Instanz
  Rückgabe: Abrufliste }
var
  AbrufListObj: TAbrufListObj;
  Del_von: string;
  Del_bis: string;
  AddEintrag: boolean;
  i: integer;

begin
  if DelAutoTable.Exists then begin
    DelAutoTable.Open;
    try
      { Filter auf InstanzId }
      DelAutoTable.Filter:=C_DTF_DDelAuto_InstanzID + ' = ' + IntToStr(InstanzId);
      DelAutoTable.Filtered:=true;

      while not DelAutoTable.EOF do begin
        Del_von:=DelAutoTable.FieldByName(C_DTF_DDelAuto_DelVon).AsString;
        Del_bis:=DelAutoTable.FieldByName(C_DTF_DDelAuto_DelBis).AsString;
        AddEintrag:=true;                                                                  { Vorbelegung: neuer Eintrag }
        if Del_bis = '' then begin                                                             { einzelnes Datenelement }
          for i:=0 to AbrufListe.Count - 1 do begin                       { Eintrag für Einzelelement schon vorhanden ? }
            if (TAbrufListObj (AbrufListe [i]).EAdr = BusAdresse) AND
               (TAbrufListObj (AbrufListe [i]).InstanzId = InstanzId) AND
               (TAbrufListObj (AbrufListe [i]).Del_bis = '') then begin
              { Länge noch unter 8k-Grenze ? }
              if length (TAbrufListObj (AbrufListe [i]).Del_von) < CMaxDSfGTelegrammLaenge then begin
                TAbrufListObj (AbrufListe [i]).Del_von:=TAbrufListObj (AbrufListe [i]).Del_von + GS + Del_von;                                { Befehl erweitern }
                AddEintrag:=false;
                Break;
              end;
            end;
          end;  { for }
        end;
        if AddEintrag then begin                                                               { neuen Eintrag einfügen }
          AbrufListObj:=TAbrufListObj.Create;
          AbrufListObj.SetData (BusAdresse, InstanzId,
                                -1, -1, '', '', '',    { für Datenelemente nicht relevant }
                                Del_von, Del_bis,
                                0, 0, -1, -1);         { für Datenelemente nicht relevant }
          AbrufListe.Add (AbrufListObj);
        end;

        DelAutoTable.Next;
      end;
    finally
      DelAutoTable.Filtered:=false;
      DelAutoTable.Close;
    end;
  end;
end;

{----------------------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetAutoDatenAbrufliste(StationId: integer;
  Datentyp: integer; Abrufliste: TAbrufList; bAutomatik: boolean = True);
{----------------------------------------------------------------------------------------------------------}
{ erzeugt Liste aller Archivgruppen/Kanäle, Logbücher oder Datenelemente aller Instanzen einer Station
  mit allen für Abruf und Konvertierung benötigten Stammdaten-Informationen;
  Übergabe: StationId
            Datentyp (Archive, Logbücher, Datenelemente)
  Rückgabe: Abrufliste }
var
  InstanzId: integer;
  Busadresse: string;

begin
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      { Filter auf StationId und bei Datentyp 'Archive' oder 'Logbücher' auch auf Instanztyp 'R' }
      InstanzTable.Filter:='(' + C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId) + ')';
      if (Datentyp = C_IsArchive) OR (Datentyp = C_IsLogbuecher) then
        InstanzTable.Filter:=InstanzTable.Filter +
                             ' AND (' + C_DTF_Instanz_Instanztyp + ' = ''' + C_D_Instanztyp_Reg + ''')';
      InstanzTable.Filtered:=true;

      while not InstanzTable.Eof do begin
        InstanzId := InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
        Busadresse := InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString;
        if Busadresse <> '' then begin
          case Datentyp of
            C_IsArchive:       GetAutoArchivAbrufListe (
                                 InstanzId, Busadresse, AbrufListe, bAutomatik);
            C_IsLogbuecher:    GetAutoLogbuchAbrufListe (
                                 InstanzId, Busadresse, AbrufListe, bAutomatik);
            C_IsDatenelemente: GetAutoDatenelementeAbrufListe (InstanzId, Busadresse, AbrufListe);
          end;
        end;  { if Busadresse }
        InstanzTable.Next;
      end;  { while not InstanzTable.Eof }
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;

    { jetzt noch die Id und Gerätetyp-Nr. der Quell-Instanzen in der Abrufliste
      setzen (vorerst nur bei Logbüchern): }
    if Datentyp = C_IsLogbuecher then
      SetAbrufList_InstanzId_GerTypNrQuelle (StationId, AbrufListe);
  end;
end;

{-------------------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetDELInstAbrufListe (InstanzId: integer; Busadresse: string; vorher_nachher: char;
                                             Abrufliste: TAbrufList);
{-------------------------------------------------------------------------------------------------------}
{ erzeugt Liste aller in den Stammdaten definierten instanzspezifischen Datenelemente einer Instanz mit allen für Abruf
  und Konvertierung benötigten Stammdaten-Informationen;
  Übergabe: InstanzId
            Busadresse der Instanz
            vorher_nachher: 'V' = Datenelemente, die für einen Abruf vor den Archiven/Logbüchern definiert sind
                            'N' = Datenelemente, die für einen Abruf nach den Archiven/Logbüchern definiert sind
  Rückgabe: Abrufliste }
var
  AbrufListObj: TAbrufListObj;
  Del_von: string;
  Del_bis: string;
  AddEintrag: boolean;
  i: integer;

begin
  if DelInstTable.Exists then begin
    DelInstTable.Open;
    try
      { Filter auf InstanzId und vorher_nachher }
      DelInstTable.Filter:='(' + C_DTF_DDelInst_InstanzID + ' = ' + IntToStr(InstanzId) + ') AND ' +
                           '(' + C_DTF_DDelInst_Vorher_Nachher + ' = ''' + vorher_nachher + ''')';
      DelInstTable.Filtered:=true;

      while not DelInstTable.EOF do begin
        Del_von:=DelInstTable.FieldByName(C_DTF_DDelInst_DelVon).AsString;
        Del_bis:=DelInstTable.FieldByName(C_DTF_DDelInst_DelBis).AsString;
        AddEintrag:=true;                                                                  { Vorbelegung: neuer Eintrag }
        if Del_bis = '' then begin                                                             { einzelnes Datenelement }
          for i:=0 to AbrufListe.Count - 1 do begin                       { Eintrag für Einzelelement schon vorhanden ? }
            if (TAbrufListObj (AbrufListe [i]).EAdr = BusAdresse) AND
               (TAbrufListObj (AbrufListe [i]).InstanzId = InstanzId) AND
               (TAbrufListObj (AbrufListe [i]).Del_bis = '') then begin
              { Länge noch unter 8k-Grenze ? }
              if length (TAbrufListObj (AbrufListe [i]).Del_von) < CMaxDSfGTelegrammLaenge then begin
                TAbrufListObj (AbrufListe [i]).Del_von:=TAbrufListObj (AbrufListe [i]).Del_von +
                                                        GS + Del_von;                                { Befehl erweitern }
                AddEintrag:=false;
                Break;
              end;
            end;
          end;  { for }
        end;
        if AddEintrag then begin                                                               { neuen Eintrag einfügen }
          AbrufListObj:=TAbrufListObj.Create;
          AbrufListObj.SetData (BusAdresse, InstanzId,
                                -1, -1, '', '', '',     { für Datenelemente nicht relevant }
                                Del_von, Del_bis,
                                0, 0, -1, -1);          { für Datenelemente nicht relevant }
          AbrufListe.Add (AbrufListObj);
        end;

        DelInstTable.Next;
      end;
    finally
      DelInstTable.Filtered:=false;
      DelInstTable.Close;
    end;
  end;
end;

{--------------------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetDELInstTypAbrufListe (InstanzId: integer; Instanztyp: string; Busadresse: string;
                                                vorher_nachher: char; Abrufliste: TAbrufList);
{--------------------------------------------------------------------------------------------------------}
{ erzeugt Liste aller in den Stammdaten definierten instanztypspezifischen Datenelemente einer Instanz mit allen für Abruf
  und Konvertierung benötigten Stammdaten-Informationen;
  Übergabe: InstanzId
            Instanztyp
            Busadresse der Instanz
            vorher_nachher: 'V' = Datenelemente, die für einen Abruf vor den Archiven/Logbüchern definiert sind
                            'N' = Datenelemente, die für einen Abruf nach den Archiven/Logbüchern definiert sind
  Rückgabe: Abrufliste }
var
  AbrufListObj: TAbrufListObj;
  Del_von: string;
  Del_bis: string;
  AddEintrag: boolean;
  i: integer;

begin
  if DelITypTable.Exists then begin
    DelITypTable.Open;
    try
      { Filter auf InstanzId und vorher_nachher }
      DelITypTable.Filter:='(' + C_DTF_DDelITyp_Instanztyp + ' = ''' + Instanztyp + ''') AND ' +
                           '(' + C_DTF_DDelITyp_Vorher_Nachher + ' = ''' + vorher_nachher + ''')';
      DelITypTable.Filtered:=true;

      while not DelITypTable.EOF do begin
        Del_von:=DelITypTable.FieldByName(C_DTF_DDelITyp_DelVon).AsString;
        Del_bis:=DelITypTable.FieldByName(C_DTF_DDelITyp_DelBis).AsString;
        AddEintrag:=true;                                                                  { Vorbelegung: neuer Eintrag }
        if Del_bis = '' then begin                                                             { einzelnes Datenelement }
          for i:=0 to AbrufListe.Count - 1 do begin                       { Eintrag für Einzelelement schon vorhanden ? }
            if (TAbrufListObj (AbrufListe [i]).EAdr = BusAdresse) AND
               (TAbrufListObj (AbrufListe [i]).InstanzId = InstanzId) AND
               (TAbrufListObj (AbrufListe [i]).Del_bis = '') then begin
              { Länge noch unter 8k-Grenze ? }
              if length (TAbrufListObj (AbrufListe [i]).Del_von) < CMaxDSfGTelegrammLaenge then begin
                TAbrufListObj (AbrufListe [i]).Del_von:=TAbrufListObj (AbrufListe [i]).Del_von +
                                                        GS + Del_von;                                { Befehl erweitern }
                AddEintrag:=false;
                Break;
              end;
            end;
          end;  { for }
        end;
        if AddEintrag then begin                                                               { neuen Eintrag einfügen }
          AbrufListObj:=TAbrufListObj.Create;
          AbrufListObj.SetData (BusAdresse, InstanzId,
                                -1, -1, '', '', '',    { für Datenelemente nicht relevant }
                                Del_von, Del_bis,
                                0, 0, -1, -1);         { für Datenelemente nicht relevant }
          AbrufListe.Add (AbrufListObj);
        end;

        DelITypTable.Next;
      end;
    finally
      DelITypTable.Filtered:=false;
      DelITypTable.Close;
    end;
  end;
end;

{--------------------------------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.GetDELInst_InstTypAbrufliste (StationId: integer; vorher_nachher: char; Abrufliste: TAbrufList);
{--------------------------------------------------------------------------------------------------------------------}
{ erzeugt Liste aller in den Stammdaten definierten instanzspezifischen und instanztypspezifischen Datenelemente einer
  Station mit allen für Abruf und Konvertierung benötigten Stammdaten-Informationen;
  Übergabe: StationId
            vorher_nachher: 'V' = Datenelemente, die für einen Abruf vor den Archiven/Logbüchern definiert sind
                            'N' = Datenelemente, die für einen Abruf nach den Archiven/Logbüchern definiert sind
  Rückgabe: Abrufliste }
var
  InstanzId: integer;
  Busadresse: string;
  Instanztyp: string;

begin
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      { Filter auf StationId }
      InstanzTable.Filter:=C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId);
      InstanzTable.Filtered:=true;

      while not InstanzTable.Eof do begin
        InstanzId := InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
        Instanztyp := InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString;
        Busadresse := InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString;
        if Busadresse <> '' then begin
          GetDELInstAbrufListe (InstanzId, Busadresse, vorher_nachher, AbrufListe);
          if Instanztyp <> '' then
            GetDELInstTypAbrufListe (InstanzId, Instanztyp, Busadresse, vorher_nachher, AbrufListe);
        end;
        InstanzTable.Next;
      end;  { while not InstanzTable.Eof }
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------}
Procedure TDSfGStammDaten.SetDelInst_InstTyp(InstTypList:TList;Modus:char);
{-------------------------------------------------------------------------}
{ speichert Liste instanztypspezifischen Datenelemente in die DDelITYp Tabelle;
  Übergabe: InsttypList : enthält Elemente vom Typ TDDelITyp
            Modus
                         U : Update alle Datenelemente des INstanztyps werden angehängt bereits bestehende geändert
                         D : Delete alle Datenelemente der Liste werden gelöscht
  Rückgabe: - }
var
  Del_von: string;
  Del_bis: string;
  InstanzTyp: char;
  Vorher_Nachher: char;
  i,AnzElemente:Integer;
  PDDelITypDat:PDDelITypDaten;
begin
  if DelITypTable.Exists then begin
    DelITypTable.Open;
    try
      AnzElemente:=InstTypList.Count;
      if AnzElemente > 0 then begin
        New(PDDelITypDat);
        try
          for i:=AnzElemente-1 downto 0 do begin
            PDDelITypDat:=InsttypList.Items[i];
            InstanzTyp:= PDDelITypDat^.InstanzTyp;
            Vorher_Nachher:= PDDelITypDat^.Vorher_Nachher;
            Del_von:=PDDelITypDat^.DelVon;
            Del_bis:=PDDelITypDat^.DelBis;
            case Modus of
                  'D':Begin
                       If DelITypTable.Findkey([InstanzTyp,Del_Von,Del_Bis,Vorher_Nachher]) then begin
                              DelITypTable.Delete;
                       end;
                      end;
                  'I':begin
                         With DelITypTable do begin
                           Edit;
                           Append;
                           Fieldbyname('InstanzTyp').Asstring:=Instanztyp;
                           Fieldbyname('Vorher_Nachher').Asstring:=Vorher_Nachher;
                           Fieldbyname('DelVon').Asstring:=Del_Von;
                           Fieldbyname('DelBis').Asstring:=Del_Bis;
                         end;
                      end;
            end; // case
          end; // for
          DelITypTable.Post;
        finally
          Dispose(PDDelITypDat);
        end;
      end; // if
    Finally
      DelITypTable.Close;
    end;
  end; // TableExists;
end;

{-----------------------------------------------------------------------------------------}
procedure TDSfGStammdaten.GetLogbuchAbruflisteByQuellBusadressen (StationId: integer;
                                                                  QuellBusadressen: string;
                                                                  AbrufListe: TAbrufList);
{-----------------------------------------------------------------------------------------}
{ erzeugt Logbuchabruf-Liste von Registrier-Instanzen einer Station mit allen für
  Abruf und Konvertierung benötigten Stammdaten-Informationen;
  Die Liste enthält nur diejenigen Logbücher, deren Quelladresse im übergebenen
  Quell-Adressen-String enthalten ist. Für Rufentgegennahme !
  Übergabe: StationId
            Quell-Busadressen
  Rückgabe: Abrufliste }
var
  InstanzId: integer;
  Busadresse: string;
  AbrufListObj: TAbrufListObj;
  LogbuchNr: integer;
  QuellAdr: string;

begin
  if InstanzTable.Exists AND LogbuchTable.Exists then begin
    InstanzTable.Open;
    try
      LogbuchTable.Open;
      try
        { Filter auf StationId und Instanztyp 'R' }
        InstanzTable.Filter:='(' + C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                             '(' + C_DTF_Instanz_Instanztyp + ' = ''' + C_D_Instanztyp_Reg + ''')';
        InstanzTable.Filtered:=true;

        while not InstanzTable.Eof do begin
          InstanzId := InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
          Busadresse := InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString;
          if Busadresse <> '' then begin
            { Filter nur auf InstanzId, Feld "Automatik" nicht relevant }
            LogbuchTable.Filtered:=false;
            LogbuchTable.Filter:=C_DTF_Logbuch_InstanzID + ' = ' + IntToStr(InstanzId);
            LogbuchTable.Filtered:=true;

            while not LogbuchTable.EOF do begin
              LogbuchNr:=LogbuchTable.FieldByName(C_DTF_Logbuch_LogbuchNr).AsInteger;
              QuellAdr:=LogbuchTable.FieldByName(C_DTF_Logbuch_EADR).AsString;
              if QuellAdr <> '' then begin
                if Pos (QuellAdr, QuellBusadressen) > 0 then begin
                  AbrufListObj:=TAbrufListObj.Create;
                  AbrufListObj.SetData (Busadresse, InstanzId, LogbuchNr,
                                        -1, '', '',      { für Logbücher nicht relevant }
                                        QuellAdr,
                                        '', '', 0, 0,    { für Logbücher nicht relevant }
                                        -1, -1);         { Id und Gerätetyp-Nr. der Quell-Instanz hier nur vorbelegen }
                  AbrufListe.Add (AbrufListObj);
                end;
              end;
              LogbuchTable.Next;
            end;
          end;  { if Busadresse }
          InstanzTable.Next;
        end;  { while not InstanzTable.Eof }
      finally
        LogbuchTable.Filtered:=false;
        LogbuchTable.Close;
      end;
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;

    { jetzt noch die Id und Gerätetyp-Nr. der Quell-Instanzen in der Abrufliste setzen: }
    SetAbrufList_InstanzId_GerTypNrQuelle (StationId, AbrufListe);
  end;
end;

{-----------------------------------------------------------------------------------------------------------}
procedure TDSfGStammdaten.SetAbrufList_InstanzId_GerTypNrQuelle (StationId: integer; AbrufListe: TAbrufList);
{-----------------------------------------------------------------------------------------------------------}
{ Id und Gerätetyp-Nr. der Quell-Instanzen einer Station in Abrufliste setzen;
  Übergabe: StationId
            AbrufListe }
var
  InstanzId: integer;
  Busadresse: string;
  GerTypNr: integer;

begin
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      { Filter nur auf StationId: }
      InstanzTable.Filter:=C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId);
      InstanzTable.Filtered:=true;
      while not InstanzTable.Eof do begin
        InstanzId := InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
        Busadresse := InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString;
        GerTypNr := InstanzTable.FieldByName (C_DTF_Instanz_GerTypNr).AsInteger;
        if (Busadresse <> '') AND (Busadresse <> '0') then
          AbrufListe.SetInstanzId_GerTypNrQuelle (Busadresse, InstanzId, GerTypNr);
        InstanzTable.Next;
      end;  { while not InstanzTable.Eof }
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------}
function TDSfGStammDaten.GetBusadressen (StationId: integer;
                                         ExcludedInstanztyp: char): string;
{-------------------------------------------------------------------------}
{ erzeugt String mit den Busdressen aller Instanzen einer Station;
  Übergabe: StationId
            ExcludedInstanztyp (Busadressen von Instanzen des Typs 'ExcludedInstanztyp'
                                werden nicht in den Busadressen-String aufgenommen)
  Ergebnis: Adressen }
var
  Busadresse: string;
  Instanztyp: string;
  S: string;

begin
  S:='';
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      { Filter auf StationId }
      InstanzTable.Filter:=C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId);
      InstanzTable.Filtered:=true;

      while not InstanzTable.Eof do begin
        Busadresse := InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString;
        Instanztyp := InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString;
        if (Busadresse <> '') AND (Instanztyp <> ExcludedInstanztyp) then
          S:=S + Busadresse;
        InstanzTable.Next;
      end;  { while not InstanzTable.Eof }
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;
  end;
  Result:=S;
end;

{---------------------------------------------------------------------}
function TDSfGStammDaten.GetInstanzCount (StationId: integer): integer;
{---------------------------------------------------------------------}
{ Ergebnis: Anzahl der zu StationId gehörenden Instanzen }
var
  InstCount: integer;
begin
  InstCount:=0;
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      { Filter auf StationId }
      InstanzTable.Filter:=C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId);
      InstanzTable.Filtered:=true;

      while not InstanzTable.Eof do begin
        inc (InstCount);
        InstanzTable.Next;
      end;  { while not InstanzTable.Eof }
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;
  end;
  Result:=InstCount;
end;

{-------------------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.InsertStationDataByFieldname (AFieldname: string; var StationData: TStationData): integer;
{-------------------------------------------------------------------------------------------------------------}
{ neue Station mit Inhalt des StationData-Records in Station.db eintragen; Schlüssel = Stationsname oder Zweitname
  Übergabe: Schlüssel-Feldname
  Übergabe/Rückgabe: StationData-Record
  Ergebnis:  0 = neue Station wurde erfolgreich angelegt (ermittelte StationId wird im Record zurückgegeben)
            -1 = Tabelle nicht vorhanden
            -2 = Station wurde nicht angelegt, weil Stationsname bzw. Zweitname bereits vergeben ist
                 (Tabellensatz wird im Record zurückgegeben) }
begin
  if StationTable.Exists then begin
    StationTable.Open;
    try
      { Filter auf Zweitname }
      if AFieldname = C_DTF_Station_Stationsname then
        StationTable.Filter:=AFieldname + ' = ''' + StationData.Stationsname + ''''
      else if AFieldname = C_DTF_Station_Zweitname then
        StationTable.Filter:=AFieldname + ' = ''' + StationData.Zweitname + ''''
      else
        StationTable.Filter:='';
      StationTable.Filtered:=true;
      if StationTable.RecordCount = 0 then begin
        { neue StationId ermitteln und in Record eintragen: }
        StationTable.Filtered:=false;
        StationTable.Last;
        if StationTable.Bof then
          StationData.StationId:=1
        else
          StationData.StationId:=StationTable.FieldByName (C_DTF_Station_StationId).AsInteger + 1;
        StationTable.InsertRecord ([StationData.StationId,
                                    StationData.Stationsname,
                                    StationData.Automatik,
                                    StationData.ErsterAbruf,
                                    StationData.LoginInstanzname,
                                    StationData.Zweitname,
                                    StationData.Prioritaet]);
        Result:=0;
      end
      else begin
        with StationData do begin
          StationId := StationTable.FieldByName (C_DTF_Station_StationId).AsInteger;
          Stationsname := StationTable.FieldByName (C_DTF_Station_Stationsname).AsString;
          Automatik := StationTable.FieldByName (C_DTF_Station_Automatik).AsBoolean;
          ErsterAbruf := StationTable.FieldByName (C_DTF_Station_ErsterAbruf).AsDateTime;
          LoginInstanzname := StationTable.FieldByName (C_DTF_Station_LIInstanz).AsString;
          Zweitname := StationTable.FieldByName (C_DTF_Station_Zweitname).AsString;
          Prioritaet := StationTable.FieldByName (C_DTF_Station_Prioritaet).AsInteger;
        end;
        Result:=-2;
      end;
    finally
      StationTable.Filtered:=false;
      StationTable.Close;
    end;
  end else
    Result:=-1;
end;

{------------------------------------------------------------------------------}
function TDSfGStammDaten.InsertInstanzData (var InstanzData: TInstanzData): integer;
{------------------------------------------------------------------------------}
{ neue Instanz mit Inhalt des InstanzData-Records in Instanz.db eintragen;
  Übergabe/Rückgabe: InstanzData-Record  (ermittelte InstanzId wird im Record zurückgegeben)
  Ergebnis:  0 = neue Instanz wurde erfolgreich angelegt
            -1 = Tabelle nicht vorhanden }
begin
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      { neue InstanzId ermitteln und in Record eintragen: }
      InstanzTable.Last;
      if InstanzTable.Bof then
        InstanzData.InstanzId:=1
      else
        InstanzData.InstanzId:=InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger + 1;
      InstanzTable.Insert;
      InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger:=InstanzData.InstanzId;
      InstanzTable.FieldByName (C_DTF_Instanz_StationId).AsInteger:=InstanzData.StationId;
      InstanzTable.FieldByName (C_DTF_Instanz_Instanzname).AsString:=InstanzData.Instanzname;
      InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString:=InstanzData.Instanztyp;
      InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString:=InstanzData.Busadresse;
      InstanzTable.FieldByName (C_DTF_Instanz_Hersteller).AsString:=InstanzData.Hersteller;
      InstanzTable.FieldByName (C_DTF_Instanz_Geraetetyp).AsString:=InstanzData.Geraetetyp;
      if InstanzData.Stand > 0 then
        InstanzTable.FieldByName (C_DTF_Instanz_Stand).AsInteger:=InstanzData.Stand;
      InstanzTable.FieldByName (C_DTF_Instanz_FabrikNr).AsString:=InstanzData.FabrikNr;
      InstanzTable.FieldByName (C_DTF_Instanz_SoftwareVs).AsString:=InstanzData.SoftwareVersion;
      if InstanzData.Baujahr > 0 then
        InstanzTable.FieldByName (C_DTF_Instanz_Baujahr).AsInteger:=InstanzData.Baujahr;
      if InstanzData.Inbetriebnahme > 0 then
        InstanzTable.FieldByName (C_DTF_Instanz_Inbetriebnahme).AsDateTime:=InstanzData.Inbetriebnahme;
      InstanzTable.FieldByName (C_DTF_Instanz_GerTypNr).AsInteger:=InstanzData.GerTypNr;
      InstanzTable.Post;
      Result:=0;
    finally
      InstanzTable.Close;
    end;
  end else
    Result:=-1;
end;

{--------------------------------------------------------------------------}
function TDSfGStammDaten.InsertInstDfuData (InstDfuData: TInstDfuData): integer;
{--------------------------------------------------------------------------}
{ neue DFÜ-Instanz mit Inhalt des InstDfuData-Records in Inst_Dfu.db eintragen;
  Übergabe: InstDfuData-Record
  Ergebnis:  0 = neue DFÜ-Instanz wurde erfolgreich angelegt
            -1 = Tabelle nicht vorhanden
            -2 = Kein Eintrag, da InstanzId schon vorhanden }
begin
  if InstDfuTable.Exists then begin
    InstDfuTable.Open;
    try
      if not InstDfuTable.FindKey ([InstDfuData.InstanzId]) then begin
        InstDfuTable.InsertRecord ([InstDfuData.InstanzId,
                                    InstDfuData.Kennung,
                                    InstDfuData.Rufnummer,
                                    InstDfuData.Adresse [1],
                                    InstDfuData.Adresse [2],
                                    InstDfuData.Adresse [3],
                                    InstDfuData.Adresse [4],
                                    InstDfuData.Passwort [1],
                                    InstDfuData.Passwort [2],
                                    InstDfuData.Passwort [3],
                                    InstDfuData.Passwort [4],
                                    InstDfuData.LogPort]);
        Result:=0;
      end else
        Result:=-2;
    finally
      InstDfuTable.Close;
    end;
  end else
    Result:=-1;
end;

{-----------------------------------------------------------------------------------------}
function TDSfGStammdaten.UpdateRufNr_LoginPW_LogPort (InstanzId: integer; LoginAdr: string;
                                                      Rufnummer: string; Passwort: string;
                                                      LogPort: integer): boolean;
{-----------------------------------------------------------------------------------------}
{ Rufnummer, Login-Passwort und log. Schnittstelle einer DFÜ-Instanz in Inst_Dfu.db updaten;
  Übergabe: InstanzId
            Login-Adresse
            Rufnummer
            Passwort
            logische Schnittstelle
  Ergebnis: true, wenn erfolgreich geändert }
begin
  Result:=false;
  if InstDfuTable.Exists then begin
    InstDfuTable.Open;
    try
      if InstDfuTable.FindKey ([InstanzId]) then begin
        InstDfuTable.Edit;
        InstDfuTable.FieldByName (C_DTF_InstDfu_Rufnummer).AsString:=Rufnummer;
        if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse1).AsString = LoginAdr then
          InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort1).AsString:=Passwort
        else if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse2).AsString = LoginAdr then
          InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort2).AsString:=Passwort
        else if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse3).AsString = LoginAdr then
          InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort3).AsString:=Passwort
        else if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse4).AsString = LoginAdr then
          InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort4).AsString:=Passwort;
        InstDfuTable.FieldByName (C_DTF_InstDfu_LogPort).AsInteger:=LogPort;
        InstDfuTable.Post;
        Result:=true;
      end;
    finally
      InstDfuTable.Close;
    end;
  end;
end;

{------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.UpdateLoginInstanzname (StationId: integer; LoginInstanzname: string);
{------------------------------------------------------------------------------------------}
{ Login-Instanzname in Station.db ändern;
  Übergabe: StationId
            Login-Instanzname }
begin
  if StationTable.Exists then begin
    StationTable.Open;
    try
      if StationTable.FindKey ([StationId]) then begin
        StationTable.Edit;
        StationTable.FieldByName (C_DTF_Station_LIInstanz).AsString:=LoginInstanzname;
        StationTable.Post;
      end;
    finally
      StationTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.UpdateInstanzDataByList (InstanzList: TInstanzDataList; AStationId: integer;
                                                  EAdr_LoginDfue: string; EAdr_nichtloeschen: string;
                                                  var LoginInstanzname: string): boolean;
{-----------------------------------------------------------------------------------------------------------}
{ Inhalt der in InstanzList enthaltenen allg. Instanzkonfiguration in Instanz.db eintragen. Nicht in InstanzList
  enthaltene Instanzen werden zuvor aus Instanz.db gelöscht (außer die in EAdr_nichtloeschen enthaltenen);
  Suche nach Tabelleneintrag über StationId, Busadresse
  -> bei neuem Eintrag wird die ermittelte InstanzId im Listeneintrag zurückgegeben !
  -> Integer-, DateTime-Werte werden nur eingetragen, wenn sie ungleich der Vorbelegung -1 sind ("Wert nicht vorhanden")
  -> Der Instanzname wird nie überschrieben
  Übergabe: Instanzdatenliste
            StationId
            Busadresse der Login-DfÜ-Instanz
            Busadressen der Instanzen, die nicht aus der Tabelle gelöscht werden sollen
  Rückgabe: neuer LoginInstanzname (wenn keine Änderung, dann Leer-String)
  Ergebnis: false, wenn Fehler beim Löschen der Archiv-/Instanzwertdaten auftritt }
var
  BusAdr: string;
  InstId: integer;
  InstTyp: string;
//  DeleteData: boolean;
  i: integer;
  InstanzData: PInstanzData;
  gefunden: boolean;
  NewInstanzId: integer;

begin
  Result:=true;
  LoginInstanzname:='';                                        { Vorbelegung: Login-Instanzname hat sich nicht geändert }
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
    { ab 10.05.2005 kein automatisches Löschen mehr von Stammdaten und abhängigen Archivdaten; WW/SM/GD/HPR
      -> Ausnahme: den beim Neu-Einlesen vorhandenen Dummy-DFÜ-Instanzeintrag löschen ! }
      InstanzTable.Filter:=C_DTF_Instanz_StationId + ' = ' + IntToStr(AStationId);
      InstanzTable.Filtered:=true;
      while not InstanzTable.Eof do begin
        Application.ProcessMessages;
        BusAdr:=InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString;
        InstTyp:=InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString;
        if (BusAdr = '0') AND (InstTyp = C_D_Instanztyp_DFU) then begin
          InstId:=InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
          DeleteInstDfuData (InstId);
          InstanzTable.Delete;
        end else
          InstanzTable.Next;
      end;  { while not InstanzTable.Eof }
      InstanzTable.Filtered:=false;

(*    { erst mal alle Instanzen, die es in der neuen Konfiguration nicht mehr gibt, incl. aller
        abhängigen Datensätze rauslöschen: }
      InstanzTable.Filter:=C_DTF_Instanz_StationId + ' = ' + IntToStr(AStationId);
      InstanzTable.Filtered:=true;
      while not InstanzTable.Eof do begin
        Application.ProcessMessages;
        BusAdr:=InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString;
        if Pos (BusAdr, EAdr_nichtloeschen) > 0 then             { BusAdr ist in den nicht zu löschenden EAdr enthalten }
          gefunden:=true
        else begin
          gefunden:=false;
          for i:=0 to InstanzList.Count-1 do begin
            InstanzData:=InstanzList.Items[i];
            if InstanzData^.Busadresse = BusAdr then begin
              gefunden:=true;
              Break;
            end;
          end; { for i:=0 }
        end;

        if gefunden then
          InstanzTable.Next
        else begin                              { abhängige Datensätze anderer Tabellen und aktuellen Datensatz löschen }
          InstId:=InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
          InstTyp:=InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString;
          if InstTyp = C_D_Instanztyp_DFU then
            DeleteInstDfuData (InstId);
          if InstTyp = C_D_Instanztyp_Reg then begin
            DeleteAKanaeleDataByInstanz (InstId);
            DeleteArchiveDataByInstanz (InstId);
            DeleteLogbuchDataByInstanz (InstId);
          end;
          DeleteDELAutoData (InstId);
          DeleteDELInstData (InstId);

          InstanzTable.Delete;

          { zugehörige Archiv- und Instanzwertdaten der Instanz löschen: }
          if not LoescheDaten_Instanz (true, InstId) then                                          { Auto-Daten löschen }
            Result:=false;
          if not LoescheDaten_Instanz (false, InstId) then                                         { Manu-Daten löschen }
            Result:=false;
          { zugehörige Journaldaten der Instanz löschen: }
          if not DeleteDZBereichByInstanz (InstId) then                        { Zeitbereiche abgerufener Daten löschen }
            Result:=false;
        end;
      end;  { while not InstanzTable.Eof }
      InstanzTable.Filtered:=false; *)

      { neu dazugekommene Instanzen einfügen, bestehende updaten: }
      for i:=0 to InstanzList.Count-1 do begin
        Application.ProcessMessages;
        InstanzData:=InstanzList.Items[i];
//        DeleteData:=false;                            { Vorbelegung: Archiv-/Instanzwertdaten der Instanz nicht löschen }
        with InstanzData^ do begin
          { Filter auf StationId }
          InstanzTable.Filter:=C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId);
          InstanzTable.Filtered:=true;

          gefunden:=false;
          InstanzTable.First;
          while not InstanzTable.Eof do begin
            if Busadresse = InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString then begin
              { InstanzId aus Tabellenfeld in die Liste eintragen: }
              PInstanzData (InstanzList.Items[i])^.InstanzId:=InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
              gefunden:=true;
              Break;
            end;
            InstanzTable.Next;
          end;  { while not InstanzTable.Eof }

          if not gefunden then begin                                              { neuer Datensatz mit neuer InstanzId }
            { neue InstanzId ermitteln: }
            InstanzTable.Filtered:=false;
            InstanzTable.Last;
            if InstanzTable.Bof then
              NewInstanzId:=1
            else
              NewInstanzId:=InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger + 1;

            PInstanzData (InstanzList.Items[i])^.InstanzId:=NewInstanzId;       { neue InstanzId in die Liste eintragen }

            InstanzTable.Insert;
            InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger:=NewInstanzId;
            InstanzTable.FieldByName (C_DTF_Instanz_StationId).AsInteger:=StationId;
            InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString:=Busadresse;
          end
          else begin                                                   { Tabellenfelder prüfen auf Änderung und updaten }
(* ab 10.05.2005 kein automatisches Löschen mehr von Stammdaten und abhängigen Archivdaten; WW/SM/GD/HPR
            if InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString <> Instanztyp then
              DeleteData:=true;
            if InstanzTable.FieldByName (C_DTF_Instanz_Hersteller).AsString <> F_RightTrunc(Hersteller, ' ') then
              DeleteData:=true;
            if InstanzTable.FieldByName (C_DTF_Instanz_Geraetetyp).AsString <> F_RightTrunc(Geraetetyp, ' ') then
              DeleteData:=true;
            if InstanzTable.FieldByName (C_DTF_Instanz_Stand).IsNull then begin
              if Stand > 0 then
                DeleteData:=true;
            end
            else begin
              if InstanzTable.FieldByName (C_DTF_Instanz_Stand).AsInteger <> Stand then
                DeleteData:=true;
            end;
            if InstanzTable.FieldByName (C_DTF_Instanz_FabrikNr).AsString <> F_RightTrunc(FabrikNr, ' ') then
              DeleteData:=true;
            if InstanzTable.FieldByName (C_DTF_Instanz_SoftwareVs).AsString <> F_RightTrunc(SoftwareVersion, ' ') then
              DeleteData:=true;
            if InstanzTable.FieldByName (C_DTF_Instanz_Baujahr).IsNull then begin
              if Baujahr > 0 then
                DeleteData:=true;
            end
            else begin
              if InstanzTable.FieldByName (C_DTF_Instanz_Baujahr).AsInteger <> Baujahr then
                DeleteData:=true;
            end;

            { ab 13.01.2005: Inbetriebnahme nicht mehr prüfen (darf sich ändern, ohne
                             daß Archivdaten gelöscht werden) }

            { GerTypNr braucht nicht geprüft zu werden, wird schon durch Geraetetyp gemacht } *)

            InstanzTable.Edit;
(* ab 10.05.2005 kein automatisches Löschen mehr von Stammdaten und abhängigen Archivdaten; WW/SM/GD/HPR
            { bei Änderung der Instanzkonfiguration abhängige Datensätze anderer Tabellen löschen: }
            if DeleteData then begin
              InstId:=InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
              InstTyp:=InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString;
              { Name löschen, damit er neu geschrieben werden kann: }
              InstanzTable.FieldByName (C_DTF_Instanz_Instanzname).AsString:='';
              { zugehörige Stammdaten der Instanz löschen: }
              if InstTyp = C_D_Instanztyp_DFU then
                DeleteInstDfuData (InstId);
              if InstTyp = C_D_Instanztyp_Reg then begin
                DeleteAKanaeleDataByInstanz (InstId);
                DeleteArchiveDataByInstanz (InstId);
                DeleteLogbuchDataByInstanz (InstId);
              end;
              DeleteDELAutoData (InstId);
              DeleteDELInstData (InstId);

              { zugehörige Archiv- und Instanzwertdaten der Instanz löschen: }
              if not LoescheDaten_Instanz (true, InstId) then                                      { Auto-Daten löschen }
                Result:=false;
              if not LoescheDaten_Instanz (false, InstId) then                                     { Manu-Daten löschen }
                Result:=false;
              { zugehörige Journaldaten der Instanz löschen: }
              if not DeleteDZBereichByInstanz (InstId) then                    { Zeitbereiche abgerufener Daten löschen }
                Result:=false;
            end;  *)
          end;

          if InstanzTable.FieldByName (C_DTF_Instanz_Instanzname).isNull then begin          { Name nicht überschreiben }
            InstanzTable.FieldByName (C_DTF_Instanz_Instanzname).AsString:=Instanzname;
            if Busadresse = EAdr_LoginDfue then
              LoginInstanzname:=Instanzname;                            { neuen Namen der Login-DFÜ-Instanz zurückgeben }
          end;
          InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString:=Instanztyp;
          InstanzTable.FieldByName (C_DTF_Instanz_Hersteller).AsString:=Hersteller;
          InstanzTable.FieldByName (C_DTF_Instanz_Geraetetyp).AsString:=Geraetetyp;
          if Stand > 0 then
            InstanzTable.FieldByName (C_DTF_Instanz_Stand).AsInteger:=Stand
          else
            InstanzTable.FieldByName (C_DTF_Instanz_Stand).Clear;                                         { leeres Feld }
          InstanzTable.FieldByName (C_DTF_Instanz_FabrikNr).AsString:=FabrikNr;
          InstanzTable.FieldByName (C_DTF_Instanz_SoftwareVs).AsString:=SoftwareVersion;
          if Baujahr > 0 then
            InstanzTable.FieldByName (C_DTF_Instanz_Baujahr).AsInteger:=Baujahr
          else
            InstanzTable.FieldByName (C_DTF_Instanz_Baujahr).Clear;                                       { leeres Feld }
          if Inbetriebnahme > 0 then
            InstanzTable.FieldByName (C_DTF_Instanz_Inbetriebnahme).AsDateTime:=Inbetriebnahme
          else
            InstanzTable.FieldByName (C_DTF_Instanz_Inbetriebnahme).Clear;                                { leeres Feld }
          InstanzTable.FieldByName (C_DTF_Instanz_GerTypNr).AsInteger:=GerTypNr;         { GerTypNr wird immer vergeben }
          InstanzTable.Post;

          InstanzTable.Filtered:=false;
        end; { with }
      end;  { for }
    finally
      InstanzTable.Close;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure TDSfGStammDaten.UpdateInstDfuDataByList (InstDfuList: TInstDfuDataList);
{----------------------------------------------------------------------------}
{ Inhalt der in List enthaltenen DFÜ-Instanzkonfiguration in Inst_dfu.db eintragen. Nicht in InstDfuList
  enthaltene DFÜ-Instanz-Einträge werden zuvor aus InstDfu.db gelöscht;
  -> sämtliche Tabellenfeldwerte werden nicht überschrieben, da für DSfG-DFÜs keine Konfiguration vom Bus gelesen werden
     kann. Es entsteht daher nur ein "Pseudo-Datensatz" mit den notwendigsten bzw. anderweitig bekannten Infos.
  -> 1:1 Verknüpfung mit Instanz.db, daher müssen keine Datensätze gelöscht werden
  Übergabe: DFÜ-Instanzdatenliste }
var
  i: integer;
  InstDfuData: PInstDfuData;
begin
  if InstDfuTable.Exists then begin
    InstDfuTable.Open;
    try
      for i:=0 to InstDfuList.Count-1 do begin
        Application.ProcessMessages;
        InstDfuData:=InstDfuList.Items[i];
        with InstDfuData^ do begin
          if InstDfuTable.FindKey ([InstanzId]) then
            InstDfuTable.Edit                                                                  { Tabellenfelder updaten }
          else begin
            InstDfuTable.Insert;                                                                      { neuer Datensatz }
            InstDfuTable.FieldByName (C_DTF_InstDfu_InstanzId).AsInteger:=InstanzId;
          end;
          { Felder nicht überschreiben: }
          if InstDfuTable.FieldByName (C_DTF_InstDfu_Kennung).IsNull then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Kennung).AsString:=Kennung;
          if InstDfuTable.FieldByName (C_DTF_InstDfu_Rufnummer).IsNull then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Rufnummer).AsString:=Rufnummer;
          if Adresse[1] <> '' then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse1).AsString:=Adresse[1];
          if Adresse[2] <> '' then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse2).AsString:=Adresse[2];
          if Adresse[3] <> '' then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse3).AsString:=Adresse[3];
          if Adresse[4] <> '' then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse4).AsString:=Adresse[4];
          if Passwort[1] <> '' then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort1).AsString:=Passwort[1];
          if Passwort[2] <> '' then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort2).AsString:=Passwort[2];
          if Passwort[3] <> '' then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort3).AsString:=Passwort[3];
          if Passwort[4] <> '' then
            InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort4).AsString:=Passwort[4];
          if InstDfuTable.FieldByName (C_DTF_InstDfu_LogPort).IsNull then
            InstDfuTable.FieldByName (C_DTF_InstDfu_LogPort).AsInteger:=LogPort;
          InstDfuTable.Post;
        end;
      end; { for }
    finally
      InstDfuTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.UpdateArchiveDataByList (ArchiveList: TArchiveDataList; InstanzList: TInstanzDataList): boolean;
{-------------------------------------------------------------------------------------------------------------------}
{ Inhalt der in ArchiveList enthaltenen Archivkonfiguration in Archive.db eintragen. Nicht in ArchiveList
  enthaltene Archivgruppen-Einträge werden zuvor aus Archive.db gelöscht;
  Übergabe: Archivliste
            InstanzList
  Ergebnis: false, wenn Fehler beim Löschen der Archivdaten auftritt }
var
//  j: integer;
//  InstId: integer;
//  ArchNr: integer;
//  InstanzData: PInstanzData;
//  gefunden: boolean;
  i: integer;
  ArchiveData: PArchiveData;

begin
  Result:=true;
  if ArchiveTable.Exists then begin
    ArchiveTable.Open;
    try
(* ab 10.05.2005 kein automatisches Löschen mehr von Stammdaten und abhängigen Archivdaten; WW/SM/GD/HPR
      { alle Archive, die es in der neuen Konfiguration nicht mehr gibt, incl. aller abhängigen
        Datensätze rauslöschen: }
      for i:=0 to InstanzList.Count-1 do begin
        Application.ProcessMessages;
        InstanzData:=InstanzList.Items[i];
        if InstanzData^.Instanztyp = C_D_Instanztyp_Reg then begin
          ArchiveTable.Filter:=C_DTF_Archive_InstanzId + ' = ' + IntToStr(InstanzData^.InstanzId);
          ArchiveTable.Filtered:=true;
          while not ArchiveTable.Eof do begin
            Application.ProcessMessages;
            InstId:=ArchiveTable.FieldByName (C_DTF_Archive_InstanzId).AsInteger;
            ArchNr:=ArchiveTable.FieldByName (C_DTF_Archive_ArchivNr).AsInteger;
            gefunden:=false;
            for j:=0 to ArchiveList.Count-1 do begin
              ArchiveData:=ArchiveList.Items[j];
              if (ArchiveData^.InstanzId = InstId) AND (ArchiveData^.ArchivNr = ArchNr) then begin
                gefunden:=true;
                Break;
              end;
            end; { for j:=0 }
            if gefunden then
              ArchiveTable.Next
            else begin                         { abhängige Datensätze anderer Tabellen und aktuellen Datensatz löschen }
              DeleteAKanaeleDataByArchiv (InstId, ArchNr);
              ArchiveTable.Delete;

              { zugehörige Archivdaten der Archivgruppe löschen: }
              if not LoescheDaten_Archiv (true, InstId, ArchNr) then                               { Auto-Daten löschen }
                Result:=false;
              if not LoescheDaten_Archiv (false, InstId, ArchNr) then                              { Manu-Daten löschen }
                Result:=false;
              if not DeleteDZBereichByArchiv (InstId, ArchNr) then             { Zeitbereiche abgerufener Daten löschen }
                Result:=false;
            end;
          end;  { while not ArchiveTable.Eof }
          ArchiveTable.Filtered:=false;
        end;
      end; { for i:=0 } *)

      { neu dazugekommene Archive einfügen, bestehende updaten: }
      for i:=0 to ArchiveList.Count-1 do begin
        Application.ProcessMessages;
        ArchiveData:=ArchiveList.Items[i];
        with ArchiveData^ do begin
          if ArchiveTable.FindKey ([InstanzId, ArchivNr]) then
            ArchiveTable.Edit                                                                  { Tabellenfelder updaten }
          else begin
            ArchiveTable.Insert;                                                                      { neuer Datensatz }
            ArchiveTable.FieldByName (C_DTF_Archive_InstanzId).AsInteger:=InstanzId;
            ArchiveTable.FieldByName (C_DTF_Archive_ArchivNr).AsInteger:=ArchivNr;
          end;
          ArchiveTable.FieldByName (C_DTF_Archive_Name).AsString:=Name;
          if ArchiveTable.FieldByName (C_DTF_Archive_Automatik).IsNull then             { Automatik nicht überschreiben }
            ArchiveTable.FieldByName (C_DTF_Archive_Automatik).AsBoolean:=Automatik;
          ArchiveTable.Post;
        end;
      end; { for i:=0 }
    finally
      ArchiveTable.Close;
    end;
  end;
end;

{----------------------------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.UpdateAKanaeleDataByList (AKanaeleList: TAKanaeleDataList; ArchiveList: TArchiveDataList): boolean;
{----------------------------------------------------------------------------------------------------------------------}
{ Inhalt der in List enthaltenen Archivkanalkonfiguration in AKanaele.db eintragen. Nicht in AKanaeleList
  enthaltene Archivkanal-Einträge werden zuvor aus AKanaele.db gelöscht;
  Übergabe: Archivkanalliste
            Archiveliste }
var
//  j: integer;
//  ArchiveData: PArchiveData;
//  InstId: integer;
//  ArchNr: integer;
//  KanNr: integer;
//  gefunden: boolean;
//  DeleteData: boolean;
  i: integer;
  AKanaeleData: PAKanaeleData;

begin
  Result:=true;
  if AKanaeleTable.Exists then begin
    AKanaeleTable.Open;
    try
(* ab 10.05.2005 kein automatisches Löschen mehr von Stammdaten und abhängigen Archivdaten; WW/SM/GD/HPR
      { alle Archivkanäle, die es in der neuen Konfiguration nicht mehr gibt, rauslöschen: }
      for i:=0 to ArchiveList.Count-1 do begin
        Application.ProcessMessages;
        ArchiveData:=ArchiveList.Items[i];
        AKanaeleTable.Filter:='('+ C_DTF_AKanaele_InstanzId + ' = ' + IntToStr(ArchiveData^.InstanzId) +') AND '+
                              '('+ C_DTF_AKanaele_ArchivNr + ' = ' + IntToStr(ArchiveData^.ArchivNr) +')';
        AKanaeleTable.Filtered:=true;
        while not AKanaeleTable.Eof do begin
          Application.ProcessMessages;
          InstId:=AKanaeleTable.FieldByName (C_DTF_AKanaele_InstanzId).AsInteger;
          ArchNr:=AKanaeleTable.FieldByName (C_DTF_AKanaele_ArchivNr).AsInteger;
          KanNr:=AKanaeleTable.FieldByName (C_DTF_AKanaele_KanalNr).AsInteger;
          gefunden:=false;
          for j:=0 to AKanaeleList.Count-1 do begin
            AKanaeleData:=AKanaeleList.Items[j];
            if (AKanaeleData^.InstanzId = InstId) AND (AKanaeleData^.ArchivNr = ArchNr) AND
               (AKanaeleData^.KanalNr = KanNr) then begin
              gefunden:=true;
              Break;
            end;
          end; { for j:=0 }
          if gefunden then
            AKanaeleTable.Next
          else begin
            AKanaeleTable.Delete;                                                         { aktuellen Datensatz löschen }

            { zugehörige Archivdaten des Archivkanals löschen: }
            if not LoescheDaten_Archivkanal (true, InstId, ArchNr, KanNr) then                     { Auto-Daten löschen }
              Result:=false;
            if not LoescheDaten_Archivkanal (false, InstId, ArchNr, KanNr) then                    { Manu-Daten löschen }
              Result:=false;
            if not DeleteDZBereichByArchivkanal (InstId, ArchNr, KanNr) then   { Zeitbereiche abgerufener Daten löschen }
              Result:=false;
          end;
        end;  { while not AKanaeleTable.Eof }
        AKanaeleTable.Filtered:=false;
      end; { for i:=0 } *)

      { neu dazugekommene Archivkanäle einfügen, bestehende updaten: }
      for i:=0 to AKanaeleList.Count-1 do begin
        Application.ProcessMessages;
        AKanaeleData:=AKanaeleList.Items[i];
//        DeleteData:=false;                            { Vorbelegung: Archivdaten des Kanals nicht löschen }
        with AKanaeleData^ do begin
          if AKanaeleTable.FindKey ([KanalNr, InstanzId, ArchivNr]) then begin
(* ab 10.05.2005 kein automatisches Löschen mehr von Stammdaten und abhängigen Archivdaten; WW/SM/GD/HPR
            { Tabellenfelder prüfen auf Änderung und updaten: }
            if AKanaeleTable.FieldByName(C_DTF_AKanaele_EAdr).AsString <> EAdr then
              DeleteData:=true;
            if AKanaeleTable.FieldByName(C_DTF_AKanaele_QuellDEL).AsString <> F_RightTrunc(QuellDEL, ' ') then
              DeleteData:=true; *)

            AKanaeleTable.Edit;
          end
          else begin
            AKanaeleTable.Insert;                                                                     { neuer Datensatz }
            AKanaeleTable.FieldByName (C_DTF_AKanaele_KanalNr).AsInteger:=KanalNr;
            AKanaeleTable.FieldByName (C_DTF_AKanaele_InstanzId).AsInteger:=InstanzId;
            AKanaeleTable.FieldByName (C_DTF_AKanaele_ArchivNr).AsInteger:=ArchivNr;
          end;
          AKanaeleTable.FieldByName (C_DTF_AKanaele_Name).AsString:=Name;
          AKanaeleTable.FieldByName(C_DTF_AKanaele_Kanaltyp).AsString:=Kanaltyp;
          if AKanaeleTable.FieldByName(C_DTF_AKanaele_Werteart).IsNull then              { Werteart nicht überschreiben }
            AKanaeleTable.FieldByName(C_DTF_AKanaele_Werteart).AsString:=Werteart;
          AKanaeleTable.FieldByName(C_DTF_AKanaele_EAdr).AsString:=EAdr;
          if AKanaeleTable.FieldByName (C_DTF_AKanaele_Automatik).IsNull then           { Automatik nicht überschreiben }
            AKanaeleTable.FieldByName (C_DTF_AKanaele_Automatik).AsBoolean:=Automatik;
          AKanaeleTable.FieldByName(C_DTF_AKanaele_QuellDEL).AsString:=QuellDEL;
          AKanaeleTable.Post;

(* ab 10.05.2005 kein automatisches Löschen mehr von Stammdaten und abhängigen Archivdaten; WW/SM/GD/HPR
          { bei Änderung der Kanalkonfiguration zugehörige Archivdaten des Kanals löschen: }
          if DeleteData then begin
            if not LoescheDaten_Archivkanal (true, InstanzId, ArchivNr, KanalNr) then              { Auto-Daten löschen }
              Result:=false;
            if not LoescheDaten_Archivkanal (false, InstanzId, ArchivNr, KanalNr) then             { Manu-Daten löschen }
              Result:=false;
            if not DeleteDZBereichByArchivkanal (InstanzId, ArchivNr, KanalNr) then   { Zeitbereiche abgerufener Daten löschen }
              Result:=false;
          end; *)
        end;
      end; { for }
    finally
      AKanaeleTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.UpdateLogbuchDataByList (LogbuchList: TLogbuchDataList; InstanzList: TInstanzDataList): boolean;
{-------------------------------------------------------------------------------------------------------------------}
{ Inhalt der in List enthaltenen Logbuchkonfiguration in Logbuch.db eintragen. Nicht in LogbuchList enthaltene
  Logbuch-Einträge werden zuvor aus Logbuch.db gelöscht;
  Übergabe: Logbuchliste
            Instanzliste
  Ergebnis: false, wenn Fehler beim Löschen der Logbuchdaten auftritt }
var
//  j: integer;
//  InstanzData: PInstanzData;
//  LogbNr: integer;
//  InstId: integer;
//  Adr: string;
  i: integer;
  LogbuchData: PLogbuchData;
  gefunden: boolean;
  NewLogbuchNr: integer;

begin
  Result:=true;
  if LogbuchTable.Exists then begin
    LogbuchTable.Open;
    try
(* ab 10.05.2005 kein automatisches Löschen mehr von Stammdaten und abhängigen Archivdaten; WW/SM/GD/HPR
      { alle Logbücher, die es in der neuen Konfiguration nicht mehr gibt, rauslöschen: }
      for i:=0 to InstanzList.Count-1 do begin
        Application.ProcessMessages;
        InstanzData:=InstanzList.Items[i];
        if InstanzData^.Instanztyp = C_D_Instanztyp_Reg then begin
          LogbuchTable.Filter:=C_DTF_Logbuch_InstanzId + ' = ' + IntToStr(InstanzData^.InstanzId);
          LogbuchTable.Filtered:=true;
          while not LogbuchTable.Eof do begin
            Application.ProcessMessages;
            InstId:=LogbuchTable.FieldByName (C_DTF_Logbuch_InstanzId).AsInteger;
            LogbNr:=LogbuchTable.FieldByName (C_DTF_Logbuch_LogbuchNr).AsInteger;
            Adr:=LogbuchTable.FieldByName (C_DTF_Logbuch_EAdr).AsString;
            gefunden:=false;
            for j:=0 to LogbuchList.Count-1 do begin
              LogbuchData:=LogbuchList.Items[j];
              if (LogbuchData^.InstanzId = InstId) AND (LogbuchData^.EAdr = Adr) then begin
                gefunden:=true;
                Break;
              end;
            end; { for j:=0 }
            if gefunden then
              LogbuchTable.Next
            else begin
              LogbuchTable.Delete;                                                        { aktuellen Datensatz löschen }

              { zugehörige Logbuchdaten löschen: }
              if not LoescheDaten_Logbuch (true, InstId, LogbNr) then                              { Auto-Daten löschen }
                Result:=false;
              if not LoescheDaten_Logbuch (false, InstId, LogbNr) then                             { Manu-Daten löschen }
                Result:=false;
              if not DeleteDZBereichByLogbuch (InstId, LogbNr) then            { Zeitbereiche abgerufener Daten löschen }
                Result:=false;
            end;
          end;  { while not LogbuchTable.Eof }
          LogbuchTable.Filtered:=false;
        end;
      end; { for i:=0 } *)

      { neu dazugekommene Logbücher einfügen, bestehende updaten: }
      for i:=0 to LogbuchList.Count-1 do begin
        Application.ProcessMessages;
        LogbuchData:=LogbuchList.Items[i];
        with LogbuchData^ do begin
          { Filter auf InstanzId }
          LogbuchTable.Filter:=C_DTF_Logbuch_InstanzId + ' = ' + IntToStr(InstanzId);
          LogbuchTable.Filtered:=true;

          gefunden:=false;
          LogbuchTable.First;
          while not LogbuchTable.Eof do begin
            if EAdr = LogbuchTable.FieldByName (C_DTF_Logbuch_EAdr).AsString then begin
              gefunden:=true;
              Break;
            end;
            LogbuchTable.Next;
          end;  { while not LogbuchTable.Eof }

          if not gefunden then begin                                              { neuer Datensatz mit neuer LogbuchNr }
            { neue LogbuchNr ermitteln: }
            LogbuchTable.Last;
            if LogbuchTable.Bof then
              NewLogbuchNr:=1
            else
              NewLogbuchNr:=LogbuchTable.FieldByName (C_DTF_Logbuch_LogbuchNr).AsInteger + 1;

            LogbuchTable.Insert;
            LogbuchTable.FieldByName (C_DTF_Logbuch_InstanzId).AsInteger:=InstanzId;
            LogbuchTable.FieldByName (C_DTF_Logbuch_LogbuchNr).AsInteger:=NewLogbuchNr;
            LogbuchTable.FieldByName (C_DTF_Logbuch_EAdr).AsString:=EAdr;
          end else                                                                             { Tabellenfelder updaten }
            LogbuchTable.Edit;

          if LogbuchTable.FieldByName (C_DTF_Logbuch_Name).isNull then                       { Name nicht überschreiben }
            LogbuchTable.FieldByName (C_DTF_Logbuch_Name).AsString:=Name;
          if LogbuchTable.FieldByName (C_DTF_Logbuch_Automatik).IsNull then             { Automatik nicht überschreiben }
            LogbuchTable.FieldByName (C_DTF_Logbuch_Automatik).AsBoolean:=Automatik;
          LogbuchTable.Post;
          LogbuchTable.Filtered:=false;
        end;
      end; { for }
    finally
      LogbuchTable.Close;
    end;
  end;
end;

{----------------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetArchivkanalAutomatik (AKanaeleList: TAKanaeleDataList);
{----------------------------------------------------------------------------------}
{ setzt in AKANAELE.DB das Feld "Automatik" nach den in der Liste übergebenen Daten;
  -> von AKanaeleList werden nur die Werte für KanalNr, InstanzId, ArchivNr und Automatik ausgewertet
  Übergabe: AKanaeleList }
var
  i: integer;
  AKanaeleData: PAKanaeleData;
  Wert_setzen: boolean;
begin
  if AKanaeleTable.Exists then begin
    AKanaeleTable.Open;
    try
      for i:=0 to AKanaeleList.Count-1 do begin
        Application.ProcessMessages;
        AKanaeleData:=AKanaeleList.Items[i];
        if AKanaeleTable.FindKey ([AKanaeleData^.KanalNr, AKanaeleData^.InstanzId, AKanaeleData^.ArchivNr]) then begin
          if not AKanaeleTable.FieldByName (C_DTF_AKanaele_Automatik).IsNull then
            Wert_setzen:=AKanaeleTable.FieldByName (C_DTF_AKanaele_Automatik).AsBoolean = not AKanaeleData^.Automatik
          else
            Wert_setzen:=true;
          if Wert_setzen then begin
            AKanaeleTable.Edit;
            AKanaeleTable.FieldByName (C_DTF_AKanaele_Automatik).AsBoolean:=AKanaeleData^.Automatik;
            AKanaeleTable.Post;
          end;
        end;
      end; { for i:=0 }
    finally
      AKanaeleTable.Close;
    end;
  end;  { if AKanaeleTable.Exists }
end;

{-----------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetAlleArchivkanaeleAutomatik (StationId: integer; Automatik: boolean);
{-----------------------------------------------------------------------------------------------}
{ setzt in AKANAELE.DB das Feld "Automatik" für alle Archivkanäle einer Station auf true bzw. false;
  Übergabe: Automatik }
var
  InstanzId: integer;
  Wert_setzen: boolean;

begin
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      if AKanaeleTable.Exists then begin
        AKanaeleTable.Open;
        try
          { Filter auf StationId und Instanztyp 'R' }
          InstanzTable.Filter:='(' + C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                               '(' + C_DTF_Instanz_Instanztyp + ' = ''' + C_D_Instanztyp_Reg + ''')';
          InstanzTable.Filtered:=true;

          while not InstanzTable.Eof do begin
            Application.ProcessMessages;
            InstanzId := InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;

            { Filter auf InstanzId }
            AKanaeleTable.Filtered:=false;
            AKanaeleTable.Filter:=C_DTF_AKanaele_InstanzId + ' = ' + IntToStr(InstanzId);
            AKanaeleTable.Filtered:=true;

            while not AKanaeleTable.Eof do begin
              Application.ProcessMessages;
              if not AKanaeleTable.FieldByName (C_DTF_AKanaele_Automatik).IsNull then
                Wert_setzen:=AKanaeleTable.FieldByName (C_DTF_AKanaele_Automatik).AsBoolean = not Automatik
              else
                Wert_setzen:=true;
              if Wert_setzen then begin
                AKanaeleTable.Edit;
                AKanaeleTable.FieldByName (C_DTF_AKanaele_Automatik).AsBoolean:=Automatik;
                AKanaeleTable.Post;
              end;
              AKanaeleTable.Next;
            end;  { while not AKanaeleTable.Eof }

            InstanzTable.Next;
          end;  { while not InstanzTable.Eof }
        finally
          AKanaeleTable.Filtered:=false;
          AKanaeleTable.Close;
        end;
      end;  { if AKanaeleTable.Exists }
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;
  end;  { if InstanzTable.Exists }
end;

{----------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetLogbuchAutomatik (LogbuchList: TLogbuchDataList);
{----------------------------------------------------------------------------}
{ setzt in LOGBUCH.DB das Feld "Automatik" nach den in der Liste übergebenen Daten;
  -> von LogbuchList werden nur die Werte für InstanzId, LogbuchNr und Automatik ausgewertet
  Übergabe: LogbuchList }
var
  i: integer;
  LogbuchData: PLogbuchData;
  Wert_setzen: boolean;
begin
  if LogbuchTable.Exists then begin
    LogbuchTable.Open;
    try
      for i:=0 to LogbuchList.Count-1 do begin
        Application.ProcessMessages;
        LogbuchData:=LogbuchList.Items[i];
        if LogbuchTable.FindKey ([LogbuchData^.InstanzId, LogbuchData^.LogbuchNr]) then begin
          if not LogbuchTable.FieldByName (C_DTF_Logbuch_Automatik).IsNull then
            Wert_setzen:=LogbuchTable.FieldByName (C_DTF_Logbuch_Automatik).AsBoolean = not LogbuchData^.Automatik
          else
            Wert_setzen:=true;
          if Wert_setzen then begin
            LogbuchTable.Edit;
            LogbuchTable.FieldByName (C_DTF_Logbuch_Automatik).AsBoolean:=LogbuchData^.Automatik;
            LogbuchTable.Post;
          end;
        end;
      end; { for i:=0 }
    finally
      LogbuchTable.Close;
    end;
  end;  { if LogbuchTable.Exists }
end;

{--------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetAlleLogbuecherAutomatik (StationId: integer; Automatik: boolean);
{--------------------------------------------------------------------------------------------}
{ setzt in LOGBUCH.DB das Feld "Automatik" für alle Logbuecher einer Station auf true bzw. false;
  Übergabe: Automatik }
var
  InstanzId: integer;
  Wert_setzen: boolean;

begin
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      if LogbuchTable.Exists then begin
        LogbuchTable.Open;
        try
          { Filter auf StationId und Instanztyp 'R' }
          InstanzTable.Filter:='(' + C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId) + ') AND ' +
                               '(' + C_DTF_Instanz_Instanztyp + ' = ''' + C_D_Instanztyp_Reg + ''')';
          InstanzTable.Filtered:=true;

          while not InstanzTable.Eof do begin
            Application.ProcessMessages;
            InstanzId := InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;

            { Filter auf InstanzId }
            LogbuchTable.Filtered:=false;
            LogbuchTable.Filter:=C_DTF_Logbuch_InstanzId + ' = ' + IntToStr(InstanzId);
            LogbuchTable.Filtered:=true;

            while not LogbuchTable.Eof do begin
              Application.ProcessMessages;
              if not LogbuchTable.FieldByName (C_DTF_Logbuch_Automatik).IsNull then
                Wert_setzen:=LogbuchTable.FieldByName (C_DTF_Logbuch_Automatik).AsBoolean = not Automatik
              else
                Wert_setzen:=true;
              if Wert_setzen then begin
                LogbuchTable.Edit;
                LogbuchTable.FieldByName (C_DTF_Logbuch_Automatik).AsBoolean:=Automatik;
                LogbuchTable.Post;
              end;
              LogbuchTable.Next;
            end;  { while not LogbuchTable.Eof }

            InstanzTable.Next;
          end;  { while not InstanzTable.Eof }
        finally
          LogbuchTable.Filtered:=false;
          LogbuchTable.Close;
        end;
      end;  { if LogbuchTable.Exists }
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;
  end;  { if InstanzTable.Exists }
end;

{----------------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetLoginDFUEKennung (StationId: integer; Kennung: string);
{----------------------------------------------------------------------------------}
{ ändert die Kennung für den Login-DFÜ-Instanz-Stammsatz einer Station }
var
  StationData: TStationData;
  InstanzData: TInstanzData;

begin
  if GetStationData (StationId, StationData) then begin
    if GetInstanzData_Name (StationId, StationData.LoginInstanzname, C_D_Instanztyp_DFU, InstanzData) then begin
      if InstDfuTable.Exists then begin
        InstDfuTable.Open;
        try
          if InstDfuTable.FindKey ([InstanzData.InstanzId]) then begin
            InstDfuTable.Edit;
            InstDfuTable.FieldByName (C_DTF_InstDfu_Kennung).AsString:=Kennung;
            InstDfuTable.Post;
          end;
        finally
          InstDfuTable.Close;
        end;
      end;
    end;  { if GetInstanzData_Name }
  end;  { if GetStationData }
end;

{----------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetLoginDFUELoginAdresse (StationId: integer; LoginAdr: string);
{----------------------------------------------------------------------------------------}
{ ändert die Login-Adresse für den Login-DFÜ-Instanz-Stammsatz einer Station.
  Die Adresse muß sowohl in der Instanz.db als auch in der Inst_Dfu.db geändert werden ! }
var
  StationData: TStationData;
  InstanzData: TInstanzData;
  LoginAdr_Merk: string;

begin
  if GetStationData (StationId, StationData) then begin
    if GetInstanzData_Name (StationId, StationData.LoginInstanzname, C_D_Instanztyp_DFU, InstanzData) then begin
      LoginAdr_Merk:=InstanzData.Busadresse;        { bisherige Login-Adresse }
      if InstDfuTable.Exists then begin
        InstDfuTable.Open;
        try
          if InstDfuTable.FindKey ([InstanzData.InstanzId]) then begin
            InstDfuTable.Edit;
            if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse1).AsString = LoginAdr_Merk then
              InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse1).AsString:=LoginAdr
            else if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse2).AsString = LoginAdr_Merk then
              InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse2).AsString:=LoginAdr
            else if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse3).AsString = LoginAdr_Merk then
              InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse3).AsString:=LoginAdr
            else if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse4).AsString = LoginAdr_Merk then
              InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse4).AsString:=LoginAdr;
            InstDfuTable.Post;

            { Adresse auch in Instanz.db mitändern: }
            SetInstanzAdresse (InstanzData.InstanzId, LoginAdr);
          end;
        finally
          InstDfuTable.Close;
        end;
      end;
    end;  { if GetInstanzData_Name }
  end;  { if GetStationData }
end;

{----------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetLoginDFUELoginPasswort (StationId: integer; LoginPW: string);
{----------------------------------------------------------------------------------------}
{ ändert das Login-Passwort für den Login-DFÜ-Instanz-Stammsatz einer Station }
var
  StationData: TStationData;
  InstanzData: TInstanzData;
  LoginAdr: string;

begin
  if GetStationData (StationId, StationData) then begin
    if GetInstanzData_Name (StationId, StationData.LoginInstanzname, C_D_Instanztyp_DFU, InstanzData) then begin
      LoginAdr:=InstanzData.Busadresse;                       { Login-Adresse }
      if InstDfuTable.Exists then begin
        InstDfuTable.Open;
        try
          if InstDfuTable.FindKey ([InstanzData.InstanzId]) then begin
            InstDfuTable.Edit;
            if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse1).AsString = LoginAdr then
              InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort1).AsString:=LoginPW
            else if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse2).AsString = LoginAdr then
              InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort2).AsString:=LoginPW
            else if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse3).AsString = LoginAdr then
              InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort3).AsString:=LoginPW
            else if InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse4).AsString = LoginAdr then
              InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort4).AsString:=LoginPW;
            InstDfuTable.Post;
          end;
        finally
          InstDfuTable.Close;
        end;
      end;
    end;  { if GetInstanzData_Name }
  end;  { if GetStationData }
end;

{----------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetLoginDFUEAdresse (StationId: integer; AdrNr: integer; Adr: string);
{----------------------------------------------------------------------------------------------}
{ ändert die Adresse mit der Nummer "AdrNr" für den Login-DFÜ-Instanz-Stammsatz einer Station.
  Falls die Adresse geändert wird, über die das Login erfolgt, muß auch die Adresse in der
  Instanz.db mitgeändert werden ! }
var
  StationData: TStationData;
  InstanzData: TInstanzData;
  LoginAdr_Merk: string;
  Adr_InstDfu_Merk: string;

begin
  if GetStationData (StationId, StationData) then begin
    if GetInstanzData_Name (StationId, StationData.LoginInstanzname, C_D_Instanztyp_DFU, InstanzData) then begin
      LoginAdr_Merk:=InstanzData.Busadresse;        { bisherige Login-Adresse }
      if InstDfuTable.Exists then begin
        InstDfuTable.Open;
        try
          if InstDfuTable.FindKey ([InstanzData.InstanzId]) then begin
            InstDfuTable.Edit;
            case AdrNr of
              1: begin
                   Adr_InstDfu_Merk:=InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse1).AsString;
                   InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse1).AsString:=Adr;
                 end;
              2: begin
                   Adr_InstDfu_Merk:=InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse2).AsString;
                   InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse2).AsString:=Adr;
                 end;
              3: begin
                   Adr_InstDfu_Merk:=InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse3).AsString;
                   InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse3).AsString:=Adr;
                 end;
              4: begin
                   Adr_InstDfu_Merk:=InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse4).AsString;
                   InstDfuTable.FieldByName (C_DTF_InstDfu_Adresse4).AsString:=Adr;
                 end;
            else
              Adr_InstDfu_Merk:='';
            end;
            InstDfuTable.Post;

            { wenn Login-Adresse geändert wurde -> Adresse auch in Instanz.db mitändern: }
            if LoginAdr_Merk = Adr_InstDfu_Merk then
              SetInstanzAdresse (InstanzData.InstanzId, Adr);
          end;
        finally
          InstDfuTable.Close;
        end;
      end;
    end;  { if GetInstanzData_Name }
  end;  { if GetStationData }
end;

{---------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetLoginDFUEPasswort (StationId: integer; PWNr: integer; PW: string);
{---------------------------------------------------------------------------------------------}
{ ändert das Passwort mit der Nummer "PWNr" für den Login-DFÜ-Instanz-Stammsatz einer Station }
var
  StationData: TStationData;
  InstanzData: TInstanzData;

begin
  if GetStationData (StationId, StationData) then begin
    if GetInstanzData_Name (StationId, StationData.LoginInstanzname, C_D_Instanztyp_DFU, InstanzData) then begin
      if InstDfuTable.Exists then begin
        InstDfuTable.Open;
        try
          if InstDfuTable.FindKey ([InstanzData.InstanzId]) then begin
            InstDfuTable.Edit;
            case PWNr of
              1: InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort1).AsString:=PW;
              2: InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort2).AsString:=PW;
              3: InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort3).AsString:=PW;
              4: InstDfuTable.FieldByName (C_DTF_InstDfu_Passwort4).AsString:=PW;
            end;
            InstDfuTable.Post;
          end;
        finally
          InstDfuTable.Close;
        end;
      end;
    end;  { if GetInstanzData_Name }
  end;  { if GetStationData }
end;

{----------------------------------------------------------------------------}
procedure TDSfGStammDaten.SetInstanzAdresse (InstanzId: integer; Adr: string);
{----------------------------------------------------------------------------}
{ ändert Adresse für InstanzId in der Instanz.db }
begin
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      if InstanzTable.FindKey ([InstanzId]) then begin
        InstanzTable.Edit;
        InstanzTable.FieldByName (C_DTF_Instanz_Busadresse).AsString:=Adr;
        InstanzTable.Post;
      end;
    finally
      InstanzTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------}
function TDSfGStammDaten.DeleteStation (StationId: integer): boolean;
{-------------------------------------------------------------------}
{ Datensatz für StationId aus Station.db und Datensätze aus allen untergeordneten Stammdatentabellen löschen;
  erweitert 15.04.2003, WW: zusätzliches Löschen aller abhängigen Daten (Termine, Aufträge,
  Instanzdaten, Journaldaten etc.)
  Ergebnis: true, wenn Stations-Stammsatz und alle abhängigen Daten erfolgreich gelöscht wurden }
var
  JournalDB: TJournalDB;
  TerminDB: TTerminDB;
  AuftragDB: TAuftragDB;
  DSfGAbruf: TDSfGAbruf;
  ADManu: TADManu;
  WRufeDB: TWRufeDB;
  OK: boolean;

begin
  Result:=true;
  if StationTable.Exists then begin
    StationTable.Open;
    try
      if StationTable.FindKey ([StationId]) then begin
        OK:=true;
        { Alle WISERV-Mom-Tabellen der Station löschen: }
        if not DeleteMomTb (StationId) then  // 30.04.2001
          OK:=false;
        { Alle Journaleinträge der Station löschen: }
        JournalDB:=TJournalDB.Create (Path);
        try
          if not JournalDB.DeleteJournal (C_GerArtDSfG, StationId) then
            OK:=false;
        finally
          JournalDB.Free;
        end;
        { Alle Termine der Station löschen: }
        TerminDB:=TTerminDB.Create (Path, '');    { Pfad zu Systemdaten-File nicht notwendig }
        try
          if not TerminDB.DeleteOneStation (C_GerArtDSfG, StationId) then
            OK:=false;
        finally
          TerminDB.free;
        end;
        { Alle Aufträge der Station löschen: }
        AuftragDB:=TAuftragDB.Create (Path, '');  { Pfad zu Systemdaten-File nicht notwendig }
        try
          if not AuftragDB.DeleteAuftraege (C_GerArtDSfG, StationId) then
            OK:=false;
        finally
          AuftragDB.Free;
        end;
        { Alle Einträge der Station aus den DSfG-Abruftabellen löschen: }
        DSfGAbruf:=TDSfGAbruf.Create (Path);
        try
          DSfGAbruf.DeleteDSfGAbrufe (StationId);
        finally
          DSfGAbruf.Free;
        end;
        ADManu:=TADManu.Create (Path);
        try
          ADManu.DeleteDSfGManuAbruf (StationId);
        finally
          ADManu.Free;
        end;
        { Rufdeaktivierung der Station löschen: }
        WRufeDB:=TWRufeDB.Create (Path);
        try
          if not WRufeDB.DeleteRecord (C_GerArtDSfG, StationId) then
            OK:=false;
        finally
          WRufeDB.Free;
        end;

        { Instanz-Stammsätze und alle zugehörigen Instanz-spezifischen abhängigen Daten löschen: }
        if not DeleteInstanzDataByStation (StationId) then
          OK:=false;
        { Stations-Stammsatz löschen, wenn alle abhängigen Daten erfolgreich gelöscht wurden: }
        if OK then
          StationTable.Delete;

        if not OK then
          Result:=false;
      end;
    finally
      StationTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------}
function TDSfGStammDaten.DeleteInstanz (InstanzId: integer): boolean;
{-------------------------------------------------------------------}
{ Datensatz für InstanzId aus Instanz.db, Datensätze aus allen untergeordneten Stammdatentabellen und
  zusätzlich alle abhängigen Daten löschen (Instanzdaten, Journaldaten etc.)
  Ergebnis: true, wenn Instanz-Stammsatz und alle abhängigen Daten erfolgreich gelöscht wurden }
var
  InstTyp: string;
  MeldKonfigurationDb: TMeldKonfigurationDb;
  ADManu: TADManu;
  OK: boolean;
  KavKonfigDb: TKavKonfigDb;
  IECKonfigDb: TIECKonfigDb;

begin
  Result:=true;
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      if InstanzTable.FindKey ([InstanzId]) then begin
        InstTyp:=InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString;
        if InstTyp = C_D_Instanztyp_DFU then
          DeleteInstDfuData (InstanzId);
        if InstTyp = C_D_Instanztyp_Reg then begin
          DeleteAKanaeleDataByInstanz (InstanzId);
          DeleteArchiveDataByInstanz (InstanzId);
          DeleteLogbuchDataByInstanz (InstanzId);
        end;
        DeleteDELAutoData (InstanzId);
        DeleteDELInstData (InstanzId);

        OK:=true;
        { Alle WISERV-Parametertabellen der Instanz löschen: }
        if not DeleteParamData (InstanzId) then  // 30.04.2001
          OK:=false;
        { Alle Meldungsstammdaten der Instanz löschen }
        MeldKonfigurationDb:=TMeldKonfigurationDb.Create (Path);
        try
          if not MeldKonfigurationDb.DeleteMeldAlarme_Geraet (C_GerArtDSfG, InstanzId) then  { definierte Alarme }
            OK:=false;
          if not MeldKonfigurationDb.DeleteMeldNrStation_Geraet (C_GerArtDSfG, InstanzId) then  { definierte stationsspezifische Meldungsnummern }
            OK:=false;
        finally
          MeldKonfigurationDb.Free;
        end;
        { Alle zugehörigen Archiv- und Instanzwertdaten der Instanz löschen: }
        if not LoescheDaten_Instanz (true, InstanzId) then     { Auto-Daten löschen }
          OK:=false;
        if not LoescheDaten_Instanz (false, InstanzId) then    { Manu-Daten löschen }
          OK:=false;
        { zugehörige Journaldaten der Instanz löschen: }
        if not DeleteDZBereichByInstanz (InstanzId) then          { Zeitbereiche abgerufener Daten löschen }
          Result:=false;
        { Alle zugehörigen Einträge der Instanz aus der DSfG-Manu-Abruftabelle löschen: }
        ADManu:=TADManu.Create (Path);
        try
          ADManu.DeleteDSfGManuAbruf_Instanz (InstanzId);
        finally
          ADManu.Free;
        end;
        { Alle zugehörigen Einträge der Instanz aus den Kavernen-Zuordnungstabellen löschen: }
        KavKonfigDb:=TKavKonfigDb.Create (Path);
        try
          KavKonfigDb.DeleteKavKonfigDataByInstanz (InstanzId);
        finally
          KavKonfigDb.Free;
        end;
        { Alle zugehörigen Einträge der Instanz aus den IEC-Konfigurationstabellen löschen: }
        IECKonfigDb:=TIECKonfigDb.Create (Path);
        try
          IECKonfigDb.DeleteDSfGDataByInstanz (InstanzId);
        finally
          IECKonfigDb.Free;
        end;

        { Instanz-Stammsatz löschen, wenn alle abhängigen Daten erfolgreich gelöscht wurden: }
        if OK then
          InstanzTable.Delete;

        if not OK then
          Result:=false;
      end;
    finally
      InstanzTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------------}
function TDSfGStammDaten.DeleteArchiv (InstanzId: integer; ArchivNr: integer): boolean;
{-------------------------------------------------------------------------------------}
{ Datensatz für InstanzId, ArchivNr aus Archive.db, Datensätze aus allen untergeordneten Stammdatentabellen und
  zusätzlich alle abhängigen Daten löschen (Archivdaten, Journaldaten etc.)
  Ergebnis: true, wenn Archiv-Stammsatz und alle abhängigen Daten erfolgreich gelöscht wurden }
var
  OK: boolean;
  ADManu: TADManu;
  KavKonfigDb: TKavKonfigDb;
  IECKonfigDb: TIECKonfigDb;
begin
  OK:=true;
  { zugehörige Archivkanäle löschen: }
  DeleteAKanaeleDataByArchiv (InstanzId, ArchivNr);
  { zugehörige Archivdaten der Archivgruppe löschen: }
  if not LoescheDaten_Archiv (true, InstanzId, ArchivNr) then                               { Auto-Daten löschen }
    OK:=false;
  if not LoescheDaten_Archiv (false, InstanzId, ArchivNr) then                              { Manu-Daten löschen }
    OK:=false;
  if not DeleteDZBereichByArchiv (InstanzId, ArchivNr) then             { Zeitbereiche abgerufener Daten löschen }
    OK:=false;
  { Alle zugehörigen Einträge des Archivs aus der DSfG-Manu-Abruftabelle löschen: }
  ADManu:=TADManu.Create (Path);
  try
    ADManu.DeleteDSfGManuAbruf_Archiv (InstanzId, ArchivNr);
  finally
    ADManu.Free;
  end;
  { Alle zugehörigen Einträge des Archivs aus den Kavernen-Zuordnungstabellen löschen: }
  KavKonfigDb:=TKavKonfigDb.Create (Path);
  try
    KavKonfigDb.DeleteKavKonfigDataByArchiv (InstanzId, ArchivNr);
  finally
    KavKonfigDb.Free;
  end;
  { Alle zugehörigen Einträge des Archivs aus den IEC-Konfigurationstabellen löschen: }
  IECKonfigDb:=TIECKonfigDb.Create (Path);
  try
    IECKonfigDb.DeleteDSfGDataByArchiv (InstanzId, ArchivNr);
  finally
    IECKonfigDb.Free;
  end;

  { Archivgruppen-Stammsatz löschen, wenn alle abhängigen Daten erfolgreich gelöscht wurden: }
  if OK then
    DeleteArchiveData (InstanzId, ArchivNr);
  Result:=OK;
end;

{------------------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.DeleteArchivkanal (InstanzId: integer; ArchivNr: integer; KanalNr: integer): boolean;
{------------------------------------------------------------------------------------------------------------}
{ Datensatz für InstanzId, ArchivNr, KanalNr aus AKanaele.db und
  zusätzlich alle abhängigen Daten löschen (Archivkanaldaten, Journaldaten etc.)
  Ergebnis: true, wenn Archivkanal-Stammsatz und alle abhängigen Daten erfolgreich gelöscht wurden }
var
  OK: boolean;
  ADManu: TADManu;
  KavKonfigDb: TKavKonfigDb;
  IECKonfigDb: TIECKonfigDb;
begin
  OK:=true;
  { zugehörige Archivdaten des Archivkanals löschen: }
  if not LoescheDaten_Archivkanal (true, InstanzId, ArchivNr, KanalNr) then                     { Auto-Daten löschen }
    OK:=false;
  if not LoescheDaten_Archivkanal (false, InstanzId, ArchivNr, KanalNr) then                    { Manu-Daten löschen }
    OK:=false;
  if not DeleteDZBereichByArchivkanal (InstanzId, ArchivNr, KanalNr) then   { Zeitbereiche abgerufener Daten löschen }
    OK:=false;
  { Alle zugehörigen Einträge des Archivkanals aus der DSfG-Manu-Abruftabelle löschen: }
  ADManu:=TADManu.Create (Path);
  try
    ADManu.DeleteDSfGManuAbruf_Archivkanal (InstanzId, ArchivNr, KanalNr);
  finally
    ADManu.Free;
  end;
  { Alle zugehörigen Einträge des Archivkanals aus den Kavernen-Zuordnungstabellen löschen: }
  KavKonfigDb:=TKavKonfigDb.Create (Path);
  try
    KavKonfigDb.DeleteKavKonfigDataByArchivkanal (InstanzId, ArchivNr, KanalNr);
  finally
    KavKonfigDb.Free;
  end;
  { Alle zugehörigen Einträge des Archivkanals aus den IEC-Konfigurationstabellen löschen: }
  IECKonfigDb:=TIECKonfigDb.Create (Path);
  try
    IECKonfigDb.DeleteDSfGDataByArchivkanal (InstanzId, ArchivNr, KanalNr);
  finally
    IECKonfigDb.Free;
  end;


  { Archivkanal-Stammsatz löschen, wenn alle abhängigen Daten erfolgreich gelöscht wurden: }
  if OK then
    DeleteAKanaeleData (InstanzId, ArchivNr, KanalNr);
  Result:=OK;
end;

{---------------------------------------------------------------------------------------}
function TDSfGStammDaten.DeleteLogbuch (InstanzId: integer; LogbuchNr: integer): boolean;
{---------------------------------------------------------------------------------------}
{ Datensatz für InstanzId, LogbuchNr aus Logbuch.db und
  zusätzlich alle abhängigen Daten löschen (Logbuchdaten, Journaldaten etc.)
  Ergebnis: true, wenn Logbuch-Stammsatz und alle abhängigen Daten erfolgreich gelöscht wurden }
var
  OK: boolean;
  ADManu: TADManu;
  IECKonfigDb: TIECKonfigDb;
begin
  OK:=true;
  { zugehörige Logbuchdaten löschen: }
  if not LoescheDaten_Logbuch (true, InstanzId, LogbuchNr) then                              { Auto-Daten löschen }
    OK:=false;
  if not LoescheDaten_Logbuch (false, InstanzId, LogbuchNr) then                             { Manu-Daten löschen }
    OK:=false;
  if not DeleteDZBereichByLogbuch (InstanzId, LogbuchNr) then            { Zeitbereiche abgerufener Daten löschen }
    OK:=false;
  { Alle zugehörigen Einträge des Logbuchs aus der DSfG-Manu-Abruftabelle löschen: }
  ADManu:=TADManu.Create (Path);
  try
    ADManu.DeleteDSfGManuAbruf_Logbuch (InstanzId, LogbuchNr);
  finally
    ADManu.Free;
  end;
  { Alle zugehörigen Einträge des Logbuchs aus den IEC-Konfigurationstabellen löschen: }
  IECKonfigDb:=TIECKonfigDb.Create (Path);
  try
    IECKonfigDb.DeleteDSfGDataByLogbuch (InstanzId, LogbuchNr);
  finally
    IECKonfigDb.Free;
  end;

  { Logbuch-Stammsatz löschen, wenn alle abhängigen Daten erfolgreich gelöscht wurden: }
  if OK then
    DeleteLogbuchData (InstanzId, LogbuchNr);
  Result:=OK;
end;

{--------------------------------------------------------------------------------}
function TDSfGStammDaten.DeleteInstanzDataByStation (StationId: integer): boolean;
{--------------------------------------------------------------------------------}
{ alle Datensätze für StationId aus Instanz.db und allen untergeordneten Stammdatentabellen löschen;
  erweitert 15.04.2003, WW: zusätzliches Löschen aller rein instanz-abhängigen Daten
  Ergebnis: true, wenn Instanz-Stammsätze der Station und alle abhängigen Daten erfolgreich gelöscht wurden }
var
  InstId: integer;
  InstTyp: string;
  OK: boolean;
  MeldKonfigurationDb: TMeldKonfigurationDb;
  KavKonfigDb: TKavKonfigDb;
  IECKonfigDb: TIECKonfigDb;

begin
  Result:=true;
  if InstanzTable.Exists then begin
    InstanzTable.Open;
    try
      MeldKonfigurationDb:=TMeldKonfigurationDb.Create (Path);
      try
        { Filter auf StationId }
        InstanzTable.Filter:=C_DTF_Instanz_StationId + ' = ' + IntToStr(StationId);
        InstanzTable.Filtered:=true;
        while not InstanzTable.Eof do begin
          Application.ProcessMessages;
          InstId:=InstanzTable.FieldByName (C_DTF_Instanz_InstanzId).AsInteger;
          InstTyp:=InstanzTable.FieldByName (C_DTF_Instanz_Instanztyp).AsString;
          if InstTyp = C_D_Instanztyp_DFU then
            DeleteInstDfuData (InstId);
          if InstTyp = C_D_Instanztyp_Reg then begin
            DeleteAKanaeleDataByInstanz (InstId);
            DeleteArchiveDataByInstanz (InstId);
            DeleteLogbuchDataByInstanz (InstId);
          end;
          DeleteDELAutoData (InstId);
          DeleteDELInstData (InstId);

          OK:=true;
          { Alle WISERV-Parametertabellen der Instanz löschen: }
          if not DeleteParamData (InstId) then  // 30.04.2001
            OK:=false;
          { Alle Meldungsstammdaten der Instanz löschen }
          if not MeldKonfigurationDb.DeleteMeldAlarme_Geraet (C_GerArtDSfG, InstId) then  { definierte Alarme }
            OK:=false;
          if not MeldKonfigurationDb.DeleteMeldNrStation_Geraet (C_GerArtDSfG, InstId) then  { definierte stationsspezifische Meldungsnummern }
            OK:=false;
          { Alle zugehörigen Archiv- und Instanzwertdaten der Instanz löschen: }
          if not LoescheDaten_Instanz (true, InstId) then     { Auto-Daten löschen }
            OK:=false;
          if not LoescheDaten_Instanz (false, InstId) then    { Manu-Daten löschen }
            OK:=false;
          { Alle zugehörigen Einträge der Instanz aus den Kavernen-Zuordnungstabellen löschen: }
          KavKonfigDb:=TKavKonfigDb.Create (Path);
          try
            KavKonfigDb.DeleteKavKonfigDataByInstanz (InstId);
          finally
            KavKonfigDb.Free;
          end;
          { Alle zugehörigen Einträge der Instanz aus den IEC-Konfigurationstabellen löschen: }
          IECKonfigDb:=TIECKonfigDb.Create (Path);
          try
            IECKonfigDb.DeleteDSfGDataByInstanz (InstId);
          finally
            IECKonfigDb.Free;
          end;

          { Instanz-Stammsatz löschen, wenn alle abhängigen Daten erfolgreich gelöscht wurden: }
          if OK then
            InstanzTable.Delete
          else
            InstanzTable.Next;

          if not OK then
            Result:=false;
        end;
      finally
        MeldKonfigurationDb.Free;
      end;
    finally
      InstanzTable.Filtered:=false;
      InstanzTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------}
procedure TDSfGStammDaten.DeleteInstDfuData (InstanzId: integer);
{-----------------------------------------------------------}
{ Datensatz für InstanzId aus Inst_Dfu.db löschen }
begin
  if InstDfuTable.Exists then begin
    InstDfuTable.Open;
    try
      if InstDfuTable.FindKey ([InstanzId]) then
        InstDfuTable.Delete;
    finally
      InstDfuTable.Close;
    end;
  end;
end;

{------------------------------------------------------------------------}
procedure TDSfGStammDaten.DeleteArchiveDataByInstanz (InstanzId: integer);
{------------------------------------------------------------------------}
{ alle Datensätze für InstanzId aus Archive.db löschen }
begin
  if ArchiveTable.Exists then begin
    ArchiveTable.Open;
    try
      ArchiveTable.Filter:=C_DTF_Archive_InstanzId + ' = ' + IntToStr (InstanzId);
      ArchiveTable.Filtered:=true;
      while not ArchiveTable.Eof do
        ArchiveTable.Delete;
    finally
      ArchiveTable.Filtered:=false;
      ArchiveTable.Close;
    end;
  end;
end;

{----------------------------------------------------------------------------------}
procedure TDSfGStammDaten.DeleteArchiveData (InstanzId: integer; ArchivNr: integer);
{----------------------------------------------------------------------------------}
{ Datensatz für InstanzId/ArchivNr aus Archive.db löschen }
begin
  if ArchiveTable.Exists then begin
    ArchiveTable.Open;
    try
      if ArchiveTable.FindKey ([InstanzId, ArchivNr]) then
        ArchiveTable.Delete;
    finally
      ArchiveTable.Close;
    end;
  end;
end;

{---------------------------------------------------------------------}
procedure TDSfGStammDaten.DeleteAKanaeleDataByInstanz (InstanzId: integer);
{---------------------------------------------------------------------}
{ alle Datensätze für InstanzId aus AKanaele.db löschen }
begin
  if AKanaeleTable.Exists then begin
    AKanaeleTable.Open;
    try
      AKanaeleTable.Filter:=C_DTF_AKanaele_InstanzId + ' = ' + IntToStr (InstanzId);
      AKanaeleTable.Filtered:=true;
      while not AKanaeleTable.Eof do
        AKanaeleTable.Delete;
    finally
      AKanaeleTable.Filtered:=false;
      AKanaeleTable.Close;
    end;
  end;
end;

{---------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.DeleteAKanaeleDataByArchiv (InstanzId: integer; ArchivNr: integer);
{---------------------------------------------------------------------------------------}
{ alle Datensätze für InstanzId und ArchivNr aus AKanaele.db löschen }
begin
  if AKanaeleTable.Exists then begin
    AKanaeleTable.Open;
    try
      AKanaeleTable.Filter:='('+ C_DTF_AKanaele_InstanzId + ' = ' + IntToStr (InstanzId) +') AND '+
                            '('+ C_DTF_AKanaele_ArchivNr + ' = ' + IntToStr (ArchivNr) +')';
      AKanaeleTable.Filtered:=true;
      while not AKanaeleTable.Eof do
        AKanaeleTable.Delete;
    finally
      AKanaeleTable.Filtered:=false;
      AKanaeleTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------------------------------------------------}
procedure TDSfGStammDaten.DeleteAKanaeleData (InstanzId: integer; ArchivNr: integer; KanalNr: integer);
{-----------------------------------------------------------------------------------------------------}
{ Datensatz für InstanzId/ArchivNr/KanalNr aus AKanaele.db löschen }
begin
  if AKanaeleTable.Exists then begin
    AKanaeleTable.Open;
    try
      if AKanaeleTable.FindKey ([KanalNr, InstanzId, ArchivNr]) then
        AKanaeleTable.Delete;
    finally
      AKanaeleTable.Close;
    end;
  end;
end;

{------------------------------------------------------------------------}
procedure TDSfGStammDaten.DeleteLogbuchDataByInstanz (InstanzId: integer);
{------------------------------------------------------------------------}
{ alle Datensätze für InstanzId aus Logbuch.db löschen }
begin
  if LogbuchTable.Exists then begin
    LogbuchTable.Open;
    try
      LogbuchTable.Filter:=C_DTF_Logbuch_InstanzId + ' = ' + IntToStr (InstanzId);
      LogbuchTable.Filtered:=true;
      while not LogbuchTable.Eof do
        LogbuchTable.Delete;
    finally
      LogbuchTable.Filtered:=false;
      LogbuchTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------------------------------}
procedure TDSfGStammDaten.DeleteLogbuchData (InstanzId: integer; LogbuchNr: integer);
{-----------------------------------------------------------------------------------}
{ Datensatz für InstanzId/LogbuchNr aus Logbuch.db löschen }
begin
  if LogbuchTable.Exists then begin
    LogbuchTable.Open;
    try
      if LogbuchTable.FindKey ([InstanzId, LogbuchNr]) then
        LogbuchTable.Delete;
    finally
      LogbuchTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------}
procedure TDSfGStammDaten.DeleteDELAutoData (InstanzId: integer);
{-----------------------------------------------------------}
{ alle Datensätze für InstanzId aus DDelAuto.db löschen }
begin
  if DelAutoTable.Exists then begin
    DelAutoTable.Open;
    try
      DelAutoTable.Filter:=C_DTF_DDelAuto_InstanzId + ' = ' + IntToStr (InstanzId);
      DelAutoTable.Filtered:=true;
      while not DelAutoTable.Eof do
        DelAutoTable.Delete;
    finally
      DelAutoTable.Filtered:=false;
      DelAutoTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------}
procedure TDSfGStammDaten.DeleteDELInstData (InstanzId: integer);
{-----------------------------------------------------------}
{ alle Datensätze für InstanzId aus DDelInst.db löschen }
begin
  if DelInstTable.Exists then begin
    DelInstTable.Open;
    try
      DelInstTable.Filter:=C_DTF_DDelInst_InstanzId + ' = ' + IntToStr (InstanzId);
      DelInstTable.Filtered:=true;
      while not DelInstTable.Eof do
        DelInstTable.Delete;
    finally
      DelInstTable.Filtered:=false;
      DelInstTable.Close;
    end;
  end;
end;

{ Löscht gespeicherte Parameter zu der Instanz (PDEWerte)   }
{ Parameter: Instanz-ID der zu löschenden Instanz           }
{-----------------------------------------------------------}
function TDSfGStammDaten.DeleteParamData (InstanzId: integer): boolean;  // 30.04.2001
{-----------------------------------------------------------}
begin
  with TDSfGParams.Create(Path) do
  try
    Result:=Delete(InstanzId);
  finally
    Free;
  end;
end;

{ Löscht spezifische Tabellen für DDP                       }
{ Parameter: Stations-ID der zu löschenden Station          }
{-----------------------------------------------------------}
function TDSfGStammDaten.DeleteMomTb (iStationsId: integer): boolean;  // 30.04.2001
{-----------------------------------------------------------}
begin
  with TDbDSfGMom.Create(Path) do
  try
    Result:=DeleteStationsId(iStationsId);
  finally
    Free;
  end;
end;

{-------------------------------------------------------------------------------}
function TDSfGStammdaten.GetDSfGDefData (GerTypNr: integer;
                                         var DSfGDefData: TDSfGDefData): boolean;
{-------------------------------------------------------------------------------}
{ liefert Datenrecord aus DSFGDEF.DB über Gerätetyp-Nr.;
  Übergabe: GerTypnr
  Rückgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
begin
  Result:=false;
  if DSfGDefTable.Exists then begin
    DSfGDefTable.Open;
    try
      if DSfGDefTable.FindKey ([GerTypNr]) then begin
        with DSfGDefData do begin
          GerTypNr:=DSfGDefTable.FieldByName (C_DTF_DSfGDef_GerTypNr).AsInteger;
          GerTypName:=DSfGDefTable.FieldByName (C_DTF_DSfGDef_GerTypName).AsString;
          DELGrp:=DSfGDefTable.FieldByName (C_DTF_DSfGDef_DELGrp).AsInteger;
          MeldGrp:=DSfGDefTable.FieldByName (C_DTF_DSfGDef_MeldGrp).AsInteger;
          StatusGrp:=DSfGDefTable.FieldByName (C_DTF_DSfGDef_StatusGrp).AsInteger;
        end;
        Result:=true;
      end;
    finally
      DSfGDefTable.Close;
    end;
  end;
end;

{-----------------------------------------------------------------------------------}
function TDSfGStammDaten.GetGeraetetypNrByName (AGerTypName_Geraet: string): integer;
{-----------------------------------------------------------------------------------}
{ liefert für den Gerätetyp-Namen einer Wieser-Instanz die in DSFGDEF.DB zugeordnete Gerätetyp-Nummer;
  Übergabe: Geraetetyp-Name
  Ergebnis: Gerätetyp-Nr (= 0, wenn keine Zuordnung vorhanden, sonst > 0) }
var
  sGerTypName_Geraet: string;
  sGerTypName_DB: string;

begin
  Result:=0;                              { Vorbelegung für "allgemeiner" Typ }
  if DSfGDefTable.Exists then begin
    DSfGDefTable.Open;
    try
      { Leerzeichen nicht berücksichtigen beim Vergleich:
        auch Groß-/Kleinschreibung nicht berücksichtigen (wegen FlowComp Q1), 15.06.2005 WW }
      sGerTypName_Geraet:=UpperCase (StrFilter (AGerTypName_Geraet, ' '));
      while not DSfGDefTable.Eof do begin
        sGerTypName_DB:=UpperCase (StrFilter (DSfGDefTable.FieldByName (C_DTF_DSfGDef_GerTypName).AsString, ' '));
        if sGerTypName_Geraet = sGerTypName_DB then begin
          Result:=DSfGDefTable.FieldByName (C_DTF_DSfGDef_GerTypNr).AsInteger;
          Break;
        end;
        DSfGDefTable.Next;
      end;  { while not DSfGDefTable.Eof }
    finally
      DSfGDefTable.Close;
    end;
  end;
end;

{--------------------------------------------------------------------------------}
function TDSfGStammdaten.StationsnameAlreadyExists (Stationsname: string;
                                                    var Exists: boolean): boolean;
{--------------------------------------------------------------------------------}
{ prüft, ob der übergebene Stationsname in den Stammdaten bereits vorhanden ist;
  Übergabe: zu prüfender Stationsname
  Rückgabe: Exists (true, wenn Stationsname schon vorhanden ist)
  Ergebnis: true, wenn Zugriff auf Tabelle ok }
begin
  Result:=false;
  Exists:=false;
  if StationTable.Exists then begin
    StationTable.Open;
    try
      Result:=true;
      while not StationTable.Eof do begin
        if F_RightTrunc (Stationsname, ' ') =
           F_RightTrunc (StationTable.FieldByName (C_DTF_Station_Stationsname).AsString, ' ') then begin
          Exists:=true;
          Break;
        end;
        StationTable.Next;
      end;
    finally
      StationTable.Close;
    end;
  end;
end;

{--------------------------------------------------------------------------------------}
function TDSfGStammdaten.KennungAlreadyExists (Kennung: string; InstanzId_Excl: integer;
                                               var Exists: boolean): boolean;
{--------------------------------------------------------------------------------------}
{ prüft, ob die übergebene Kennung in den Stammdaten bereits vorhanden ist;
  Übergabe: zu prüfende Kennung
            InstanzId >  0: Instanz, die von der Prüfung ausgeschlossen sein soll
                      <= 0: alle Instanzen werden geprüft
  Rückgabe: Exists (true, wenn Kennung schon vorhanden ist)
  Ergebnis: true, wenn Zugriff auf Tabelle ok }
var
  InstanzId: integer;
begin
  Result:=false;
  Exists:=false;
  if InstDfuTable.Exists then begin
    InstDfuTable.Open;
    try
      Result:=true;
      while not InstDfuTable.Eof do begin
        InstanzId := InstDfuTable.FieldByName (C_DTF_InstDfu_InstanzId).AsInteger;
        if ((InstanzId_Excl > 0) AND (InstanzId <> InstanzId_Excl)) OR
           (InstanzId_Excl <= 0) then begin
          if F_RightTrunc (Kennung, ' ') =
             F_RightTrunc (InstDfuTable.FieldByName (C_DTF_InstDfu_Kennung).AsString, ' ') then begin
            Exists:=true;
            Break;
          end;
        end;
        InstDfuTable.Next;
      end;
    finally
      InstDfuTable.Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------------------------------}
function TDSfGStammDaten.CreateKavernenStation (Stationsname_Ziel: string; InstanzId_Quelle_Reg: integer;
                                                AnzKavInstanzen: integer): integer;
{-------------------------------------------------------------------------------------------------------}
{ erzeugt virtuelle Kavernen-Station }
{ Ergebnis: 0  = OK
            -1 = Kavernen-Station konnte nicht angelegt werden, Stationsname bereits vorhanden
            -2 = Fehler beim Zugriff auf Stammdaten-Tabellen
            -3 = Fehler beim Zugriff auf Kavernen-Zuordnungstabellen }
var
  StationData: TStationData;
  InstanzData_Quelle: TInstanzData;
  InstanzData_Ziel: TInstanzData;
  i: integer;
  QueryQuellArchive: TQuery;
  QueryQuellArchivkanaele: TQuery;
  ArchivNr: integer;
  KanalNr: integer;
  Automatik: boolean;
  KavKonfigDb: TKavKonfigDb;

begin
  Result:=0;

  { Datensatz in Station.db eintragen: }
  with StationData do begin
    StationId:=-1;
    Stationsname:=F_RightTrunc (Stationsname_Ziel, ' ');
    Automatik:=true;
    ErsterAbruf:=Date;
    LoginInstanzname:='';
    Zweitname:=F_RightTrunc (Stationsname_Ziel, ' ');
    Prioritaet:=10;
  end;
  if InsertStationDataByFieldname (C_DTF_Station_Stationsname, StationData) <> 0 then begin
    Result:=-1;
    exit;
  end;

  { Datensatz für Quell-Registrierinstanz lesen: }
  if not GetInstanzData (InstanzId_Quelle_Reg, InstanzData_Quelle) then begin
    Result:=-2;
    exit;
  end;

  if ArchiveTable.Exists AND AKanaeleTable.Exists then begin
    { Quell-Datensätze aus ARCHIVE.DB lesen: }
    QueryQuellArchive:=TQuery.Create (nil);
    try
      QueryQuellArchive.DatabaseName:=Path;
      QueryQuellArchive.Sql.Add ('SELECT * FROM "' + C_DTB_Archive + '"');
      QueryQuellArchive.Sql.Add ('WHERE ' + C_DTF_Archive_InstanzId + ' = ' + IntToStr (InstanzData_Quelle.InstanzId));
      QueryQuellArchive.Open;

      { Quell-Datensätze aus AKANAELE.DB lesen: }
      QueryQuellArchivKanaele:=TQuery.Create (nil);
      try
        QueryQuellArchivKanaele.DatabaseName:=Path;
        QueryQuellArchivKanaele.Sql.Add ('SELECT * FROM "' + C_DTB_AKanaele + '"');
        QueryQuellArchivKanaele.Sql.Add ('WHERE ' + C_DTF_AKanaele_InstanzId + ' = ' + IntToStr (InstanzData_Quelle.InstanzId));
        QueryQuellArchivKanaele.Open;

        KavKonfigDb:=TKavKonfigDb.Create (Path);
        try
          { Datensätze für Ziel-Kavernen eintragen (virtuelle Registrierinstanzen als Kopie
            der Quell-Registrierinstanz): }
          for i:=1 to AnzKavInstanzen do begin
            { Datensatz in Instanz.db eintragen: }
            InstanzData_Ziel:=InstanzData_Quelle;   { Kopie }
            with InstanzData_Ziel do begin
              InstanzId:=-1;                     { nur vorbelegen, wird neu vergeben }
              StationId:=StationData.StationId;  { Ziel-Stations-Id }
              Instanzname:='Kaverne ' + IntToStr (i);   { Kavernenname  }
              Busadresse:='0';                          { Dummy-Busadresse }
            end;

            { Datensatz für Ziel-Registrierinstanz eintragen: }
            if InsertInstanzData (InstanzData_Ziel) <> 0 then begin
              Result:=-2;
              exit;
            end;

            { alle Quell-Archive in Ziel-Instanz kopieren: }
            ArchiveTable.Open;
            try
              QueryQuellArchive.First;
              while not QueryQuellArchive.EOF do begin
                ArchivNr:=QueryQuellArchive.FieldByName(C_DTF_Archive_ArchivNr).AsInteger;
                Automatik:=false;
                if not ArchiveTable.FindKey ([InstanzData_Ziel.InstanzId, ArchivNr]) then
                  ArchiveTable.InsertRecord ([InstanzData_Ziel.InstanzId,
                                              ArchivNr,
                                              QueryQuellArchive.FieldByName(C_DTF_Archive_Name).AsString,
                                              Automatik]);
                QueryQuellArchive.Next;
              end;  { while not Query.EOF }
            finally
              ArchiveTable.Close;
            end;

            { alle Quell-Archivkanäle in Ziel-Instanz kopieren: }
            AKanaeleTable.Open;
            try
              QueryQuellArchivkanaele.First;
              while not QueryQuellArchivKanaele.EOF do begin
                KanalNr:=QueryQuellArchivKanaele.FieldByName(C_DTF_AKanaele_KanalNr).AsInteger;
                ArchivNr:=QueryQuellArchivKanaele.FieldByName(C_DTF_AKanaele_ArchivNr).AsInteger;
                Automatik:=false;
                if not AKanaeleTable.FindKey ([KanalNr, InstanzData_Ziel.InstanzId, ArchivNr]) then
                  AKanaeleTable.InsertRecord ([KanalNr,
                                               InstanzData_Ziel.InstanzId,
                                               ArchivNr,
                                               QueryQuellArchivKanaele.FieldByName(C_DTF_AKanaele_Name).AsString,
                                               QueryQuellArchivKanaele.FieldByName(C_DTF_AKanaele_Kanaltyp).AsString,
                                               QueryQuellArchivKanaele.FieldByName(C_DTF_AKanaele_Werteart).AsString,
                                               QueryQuellArchivKanaele.FieldByName(C_DTF_AKanaele_EADR).AsString,
                                               Automatik,
                                               QueryQuellArchivKanaele.FieldByName(C_DTF_AKanaele_QuellDEL).AsString]);
                QueryQuellArchivKanaele.Next;
              end;  { while not Query.EOF }
            finally
              AKanaeleTable.Close;
            end;

            { KavInstanz.db füllen (Zuordnung: Quell-InstanzId, Kavernennummer -> Kavernen-InstanzId) }
            if not KavKonfigDb.InsertKaverneNr_InstanzId (InstanzData_Quelle.InstanzId, i, InstanzData_Ziel.InstanzId) then begin
              Result:=-3;
              exit;
            end;
          end;  { for i }
        finally
          KavKonfigDb.Free;
        end;
      finally
        QueryQuellArchivKanaele.Free;
      end;
    finally
      QueryQuellArchive.Free;
    end;
  end;
end;

end.
