{******************************************************************************}
{* Unit: Tabellenzugriffe auf MRG-Stammdaten                                  *}
{* 02.12.98 WW                                                                *}
{******************************************************************************}
Unit MDBSta;

INTERFACE

uses
  Classes, DBTables, SysUtils, WSysCon, WSysDat, T_Tools, MDBMrg;

const
  MAXDSFGGERAETE = 7;  { ger‰tetechnisch bedingt }

type

  TDSFGTyp = (DSFGNO, DSFGMASTER, DSFGSLAVE);

  TMrgId = LongInt;

  TStaData = record
    Kennung: string [szlen_Kennung];
    Aktiv: LongInt;
    MrgId: TMrgId;
    MrgTyp: TMrgTyp;
    StationsName: string [szlen_StationsName];
    TagesEnde: LongInt;
    Prioritaet: integer;
  end;

  TStaDfuData = record
    MrgId: TMrgId;
    Rufnummer: string [szlen_Rufnummer];
    Passwort: array [1..4] of string [szlen_Passwort];
    PasswortNr: Integer;
    ModemTyp: Integer;
    LogPort: Integer;
  end;

  TStaAutoData = record
    MrgId: TMrgId;
    Automatik: Boolean;
    Intervall: LongInt;
    Zeitsynchronisation: Boolean;
    Rundpufferreset: Boolean;
  end;

  TStaDSFGData = record
    MrgId: TMrgId;
    Adresse: string [szlen_DSFGAdresse];
    MasterId: TMrgId;
  end;

  TStaKanalData = record
    MrgId: TMrgId;
    MrgKanal: Integer;
    KanalId: LongInt;
    Aktiv: Boolean;
    KanalName: string [szlen_KanalName];
    KanalTyp: string [szlen_KanalTyp];
    Einstellbar: Boolean;
    Kontroll: Boolean;
    Eingang: Boolean;
    Einheit: string [szLen_Einheit];
    Kommastellen: Integer;
  end;

  TStaImpData = record
    KanalId: LongInt;
    Faktor: LongInt;
    Teiler: LongInt;
    OrgFaktor: Double;
  end;

  TStaAnaData = record
    KanalId: LongInt;
    MessBereichMin: Double;
    MessBereichMax: Double;
    StromBereich: Integer;
    Offset: Boolean;
    AufzMin: LongInt;
    AufzMax: LongInt;
  end;

  { Stammdatenteil, welcher zum Verbindungsaufbau notwendig ist }

  TRufStammDaten = Record
    MrgId : TMrgId;
    Kennung : string [szLen_Kennung];
    Stationsname : string [szLen_StationsName];
    MrgTyp : TMrgTyp;
    ModemTyp : Integer;
    Rufnummer : string [szLen_RufNummer + szLen_Vorwahl];
    PasswortNummer : Integer;
    Passwort : string [szLen_Passwort];
    TagesEnde: Integer;
    Zeitsynchronisation: boolean;
    Rundpufferreset: boolean;
  End;

  TMRGStammdaten = class (TObject)
  private
    Path: TFileName;
    { Tabellenobjekte }
    StaTable: TTable;
    StaDfuTable: TTable;
    StaDSFGTable: TTable;
    StaAutoTable: TTable;
    function GetStaDSFGData (MrgId: TMrgId; var StaDSfGData: TStaDSFGData): boolean;
    function GetStaAutoData (MrgId: TMrgId; var StaAutoData: TStaAutoData): boolean;
    function GetStaDFUData (MrgId: TMrgId; var StaDFUData: TStaDFUData): boolean;
  public
    constructor Create (APath: TFileName);
    destructor Destroy; override;
    function InitTabellen: boolean;
    function GetStaData (MrgId: TMrgId; var StaData: TStaData): boolean;
    function GetStaDataKennung (Kennung: string; var StaData: TStaData): boolean;
    function GetMrgTyp (MrgId: TMrgId): TMrgTyp;
    function GetMesswertKanalMaske (MrgId: TMrgId): string;
    function GetTagessatzKanalMaske (MrgId: TMrgId): string;
    function DSfGTyp (MrgId: TMrgId; var Adresse: Char; var MasterId: TMrgId): TDSFGTyp;
    function GetDSfGSlaveId (MasterId: TMrgId; SlaveAdr: char): TMrgId;
    function GetRufStammDatenMrgId (MrgId: TMrgId; Vorwahl: string;
                                    var RufStammDaten: TRufStammDaten): boolean;
    function GetRufStammDatenKennung (Kennung: string; Vorwahl: string;
                                      var RufStammDaten: TRufStammDaten): boolean;
    function GetModemAbrufgruppe (MrgId: TMrgId; var ModemAbrufgruppe: integer): boolean;
    function GetModemtyp (MrgId: TMrgId; var Modemtyp: integer): boolean;
    procedure GetStationsnamenKennungen (aQuery: TQuery);
    procedure GetKennungenByMrgTyp (AMrgTyp: integer; aQuery: TQuery);
    function GetMDEStationsnamenKennungenGeraetetyp (aQuery: TQuery): boolean;
    procedure SetKennung (MrgId: integer; Value: string);
    procedure SetPasswort (MrgId: integer; PWNr: integer; Value: string);
  end;

  TKanalTyp = (KTyp_Unknown, KTyp_Impuls, KTyp_Analog);

  TKanal = record
    MrgKanal: Integer;
    Aktiv: Boolean;
    KanalName: string [szlen_KanalName];
    Kontroll: Boolean;
    Eingang: Boolean;
    Einheit: string [szLen_Einheit];
    Kommastellen: Integer;
  end;

  TImpulsKanal = record
    Faktor: LongInt;
    Teiler: LongInt;
    OrgFaktor: Double;
  end;

  TAnalogKanal = record
    MessBereichMin: Double;
    MessBereichMax: Double;
    StromBereich: Integer;
    Offset: Boolean;
    AufzMin: LongInt;
    AufzMax: LongInt;
  end;

  { Objekt zur Aufnahme von Kanaldaten (in Kanalliste von TKanalListObj) }

  TKanalObj = class (TObject)
  public
    KanalTyp: TKanalTyp;
    Kanal: TKanal;
    ImpulsKanal: TImpulsKanal;
    AnalogKanal: TAnalogKanal;
    constructor Create (StaKanalData: TStaKanalData);
    procedure SetImpKanalData (StaImpData: TStaImpData);
    procedure SetAnaKanalData (StaAnaData: TStaAnaData);
    function GetKanalTyp: TKanalTyp; virtual;
  end;

  { Objekt zur Aufnahme von Impulskanaldaten (in ImpulsKanalliste von TKanalListObj) }

  TImpulsKanalObj = class (TObject)
  public
    Kanal: TKanal;
    ImpulsKanal: TImpulsKanal;
    constructor Create (StaKanalData: TStaKanalData; StaImpData: TStaImpData);
  end;

  { Objekt zur Aufnahme von Analogkanaldaten (in AnalogKanalliste von TKanalListObj) }

  TAnalogKanalObj = class (TObject)
  public
    Kanal: TKanal;
    AnalogKanal: TAnalogKanal;
    constructor Create (StaKanalData: TStaKanalData; StaAnaData: TStaAnaData);
  end;

  { Objekt f¸r Kanalinformationen einer MRG-Station }

  TKanalListObj = class (TObject)
  private
    Path: TFileName;
    MrgId: TMrgId;
    Kanalliste: TList;            { Liste mit Kanaldaten eines jeden MRG-Kanal }
    ImpulsKanalliste: TList;      { Liste mit Kanaldaten nur von Impuls-Kan‰len }
    AnalogKanalliste: TList;      { Liste mit Kanaldaten nur von Analog-Kan‰len }
    procedure LoadListen;
    function Get_StaImpData(Table: TTable; KeepOpen: boolean; KanalId: longint;
                            var StaImpData: TStaImpData): boolean;
    function Get_StaAnaData(Table: TTable; KeepOpen: boolean; KanalId: longint;
                            var StaAnaData: TStaAnaData): boolean;
  public
    constructor Create (AMrgId: TMrgId; APath: TFileName);
    destructor Destroy; override;
    function GetMRGKanal (MRGKanal: Integer): TKanalObj;
    function GetKanalTyp (MRGKanal: Integer): TKanalTyp;
    function IsAktiv (MRGKanal: Integer): Boolean;
    function IsAktivImpuls (MRGKanal: Integer): Boolean;
    function IsAktivKontroll (MRGKanal: Integer): Boolean;
    function IsAktivEingang (MRGKanal: Integer): Boolean;
    function GetKanalAnzahl: integer;
    function GetEingangszaehlerAnzahl: integer;
    function GetKontrollzaehlerAnzahl: integer;
    procedure GetAnalogkanal_von_bis (var von: integer; var bis: integer);
    function GetAnalogkanal_AufzMax: integer;
  end;

IMPLEMENTATION

{--------------------------------------------------------}
function MrgKanalCompare (Item1, Item2: Pointer): Integer;
{--------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TKanal-Objekten nach der MRG-Kanalnummer }
begin
  Result := TKanalObj (Item1).Kanal.MrgKanal - TKanalObj (Item2).Kanal.MrgKanal;
end;

{--------------------------------------------------------------}
function MrgImpulsKanalCompare (Item1, Item2: Pointer): Integer;
{--------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TImpulsKanal-Objekten nach der MRG-Kanalnummer }
begin
  Result := TImpulsKanalObj (Item1).Kanal.MrgKanal - TImpulsKanalObj (Item2).Kanal.MrgKanal;
end;

{--------------------------------------------------------------}
function MrgAnalogKanalCompare (Item1, Item2: Pointer): Integer;
{--------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TAnalogKanal-Objekten nach der MRG-Kanalnummer }
begin
  Result := TAnalogKanalObj (Item1).Kanal.MrgKanal - TAnalogKanalObj (Item2).Kanal.MrgKanal;
end;


{ TMRGStammdaten }

{---------------------------------------------------}
constructor TMRGStammDaten.Create (APath: TFileName);
{---------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
end;

{--------------------------------}
destructor TMRGStammdaten.Destroy;
{--------------------------------}
begin
  StaAutoTable.Free;
  StaDSFGTable.Free;
  StaDfuTable.Free;
  StaTable.Free;
  inherited Destroy;
end;

{--------------------------------------------}
function TMRGStammdaten.InitTabellen: boolean;
{--------------------------------------------}
{ Initialisierung f¸r Zugriff auf Stammdaten-Tabellen;
  Ergebnis: true, wenn alle Stammdaten-Tabellen vorhanden sind }
begin
  Result := false;
  if FileExists(Path+CDBSta) AND
     FileExists(Path+CDBStaDfu) AND
     FileExists(Path+CDBStaDSfG) AND
     FileExists(Path+CDBStaAuto) then begin
    StaTable:=TTable.Create(nil);
    StaTable.DatabaseName:=Path;
    StaTable.TableName:=CDBSta;

    StaDfuTable:=TTable.Create(nil);
    StaDfuTable.DatabaseName:=Path;
    StaDfuTable.TableName:=CDBStaDfu;

    StaDSfGTable:=TTable.Create(nil);
    StaDSfGTable.DatabaseName:=Path;
    StaDSfGTable.TableName:=CDBStaDSfG;

    StaAutoTable:=TTable.Create(nil);
    StaAutoTable.DatabaseName:=Path;
    StaAutoTable.TableName:=CDBStaAuto;
    Result:=true;
  end;
end;

{---------------------------------------------------------}
function TMRGStammdaten.GetMrgTyp (MrgId: TMrgId): TMrgTyp;
{---------------------------------------------------------}
{ liefert Ger‰tetyp;
  ‹bergabe: MrgId
  Ergebnis: Ger‰tetyp-Nummer, wenn in Stammdaten-Tabellen gefunden, sonst -1 }
var
  StaData: TStaData;
begin
  if GetStaData (MrgId, StaData) then
    Result := StaData.MrgTyp
  else
    Result := -1;
end;

{--------------------------------------------------------------------}
function TMRGStammdaten.GetMesswertKanalMaske (MrgId: TMrgId): string;
{--------------------------------------------------------------------}
{ liefert Meﬂwert-Kanalmaske (aktive/inaktive MRG-Kan‰le in Stammdaten);
  ‹bergabe: MrgId
  Ergebnis: Kanalmaske-String }
var
  MrgDefDataDB: TMrgDefDataDB;
  KanalListObj: TKanalListObj;
  KanalMaske: string;
  MrgKanal: Integer;
  MrgTyp: TMrgTyp;

begin
  KanalMaske := '';
  MrgTyp:=GetMrgTyp (MrgId);
  if MrgTyp >= 0 then begin
    if GetMrgDefDataDB (MrgTyp, MrgDefDataDB) then begin
      if MrgDefDataDB.MesswertKanaele > 0 then begin
        KanalListObj := TKanalListObj.Create (MrgId, Path);
        try
          for MrgKanal := 1 to MrgDefDataDB.MesswertKanaele do begin
            if KanalListObj.IsAktiv (MrgKanal) then
              KanalMaske := KanalMaske + '1'
            else
              KanalMaske := KanalMaske + '0';
          end;
        finally
          KanalListObj.Free;
        end;
      end;
    end;
  end;
  Result := KanalMaske;
end;

{---------------------------------------------------------------------}
function TMRGStammdaten.GetTagessatzKanalMaske (MrgId: TMrgId): string;
{---------------------------------------------------------------------}
var
  MrgDefDataDB: TMrgDefDataDB;
  KanalListObj: TKanalListObj;
  KanalMaske: string;
  MrgKanal: Integer;
  MrgTyp: TMrgTyp;

begin
  KanalMaske := '';
  MrgTyp:=GetMrgTyp (MrgId);
  if MrgTyp >= 0 then begin
    if GetMrgDefDataDB (MrgTyp, MrgDefDataDB) then begin
      if MrgDefDataDB.ZaehlerKanaele > 0 then begin
        KanalListObj := TKanalListObj.Create (MrgId, Path);
        try
          for MrgKanal := 1 to MrgDefDataDB.ZaehlerKanaele do
          begin
            if KanalListObj.IsAktiv (MrgKanal) then
              KanalMaske := KanalMaske + '1'
            else
              KanalMaske := KanalMaske + '0';
          end;
        finally
          KanalListObj.Free;
        end;
      end;
    end;
  end;
  Result := KanalMaske;
end;

{-------------------------------------------------------------------------------------------------}
function TMRGStammdaten.DSfGTyp (MrgId: TMrgId; var Adresse: Char; var MasterId: TMrgId): TDSfGTyp;
{-------------------------------------------------------------------------------------------------}
{ liefert DSfG-Information der Station;
  ‹bergabe: MrgId
  R¸ckgabe: DSfG-Adresse, MasterId
  Ergebnis: DSfG-Typ (Master, Slave, normales Ger‰t) }
var
  StaDSFGData: TStaDSFGData;
begin
  Result := DSFGNO;
  Adresse := ' ';
  MasterId := MrgId;
  if GetStaDSfGData (MrgId, StaDSFGData) then begin
    Adresse := StaDSfGData.Adresse [1];
    MasterId := StaDSfGData.MasterId;
    if StaDSfGData.MrgId = StaDSfGData.MasterId then
      Result := DSFGMASTER
    else
      Result := DSFGSLAVE;
  end;
end;

{--------------------------------------------------------------------------------}
function TMRGStammdaten.GetDSfGSlaveId (MasterId: TMrgId; SlaveAdr: char): TMrgId;
{--------------------------------------------------------------------------------}
{ liefert MrgId eines DSfG-Slave;
  ‹bergabe: MrgId des Master
            Adresse des Slave
  Ergebnis: MrgId des Slave }
var
  MrgId: TMrgId;
begin
  Result := -1;
  if StaTable.Exists AND StaDSfGTable.Exists then begin
    StaDSFGTable.Open;
    try
      StaTable.IndexName:=CSMrgId;                      { sortiert nach MrgID }
      StaTable.Open;
      try
        { Filter auf MasterId }
        StaDSFGTable.Filter:=C_StaDSfG_MasterId + ' = ' + IntToStr(MasterId);
        StaDSFGTable.Filtered:=true;

        while not StaDSfGTable.Eof do begin
          if StaDSfGTable.FieldByName (C_StaDSFG_Adresse).AsString [1] = SlaveAdr then begin
            MrgId:=StaDSfGTable.FieldByName (C_StaDSFG_MrgId).AsInteger;

            { Pr¸fen, ob Stammsatz f¸r MrgId nicht gelˆscht (Feld "Aktiv" in STA.DB): }
            if StaTable.FindKey ([MrgId]) then begin
              if StaTable.FieldByName (C_Sta_Aktiv).AsInteger = 0 then begin
                Result := MrgId;
                Break;
              end;
            end;
          end;
          StaDSfGTable.Next;
        end;
      finally
        StaTable.Close;
      end;
    finally
      StaDSFGTable.Close;
    end;
  end;
end;

{---------------------------------------------------------------------------------}
function TMRGStammdaten.GetStaData (MrgId: TMrgId; var StaData: TStaData): boolean;
{---------------------------------------------------------------------------------}
{ liefert Datenrecord aus STA.DB ¸ber MrgId;
  ‹bergabe: MrgId
  R¸ckgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
var
  AktivId: integer;
begin
  Result := false;
  if StaTable.Exists then begin
    StaTable.IndexName:=CSMrgId;                        { sortiert nach MrgID }
    StaTable.Open;
    try
      if StaTable.FindKey ([MrgId]) then begin
        AktivId:=StaTable.FieldByName(C_Sta_Aktiv).AsInteger;
        if AktivId = 0 then begin                  { Stammsatz nicht gelˆscht }
          with StaData do begin
            Kennung := StaTable.FieldByName(C_Sta_Kennung).AsString;
            Aktiv := AktivId;
            MrgId := StaTable.FieldByName(C_Sta_MrgId).AsInteger;
            MrgTyp := StaTable.FieldByName(C_Sta_MrgTyp).AsInteger;
            StationsName := StaTable.FieldByName(C_Sta_StationsName).AsString;
            TagesEnde := StaTable.FieldByName(C_Sta_TagesEnde).AsInteger;
            Prioritaet := StaTable.FieldByName(C_Sta_Prioritaet).AsInteger;
          end;
          Result:=true;
        end;
      end;
    finally
      StaTable.Close;
    end;
  end;
end;

{------------------------------------------------------------------------------------------}
function TMRGStammdaten.GetStaDataKennung (Kennung: string; var StaData: TStaData): boolean;
{------------------------------------------------------------------------------------------}
{ liefert Datenrecord aus STA.DB ¸ber Kennung;
  Achtung auf ' ' und #0 in der ¸bergebenen und der Sta-Kennung (-> FilterKennung);
  ‹bergabe: Kennung
  R¸ckgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
var
  AktivId: integer;
  StaKennung: string;
begin
  Result := false;
  if StaTable.Exists then begin
    StaTable.Open;
    try
      Kennung:=FilterKennung (Kennung);
      while not StaTable.Eof do begin
        AktivId:=StaTable.FieldByName(C_Sta_Aktiv).AsInteger;
        if AktivId = 0 then begin                  { Stammsatz nicht gelˆscht }
          StaKennung:=StaTable.FieldByName(C_Sta_Kennung).AsString;
          if FilterKennung (StaKennung) = Kennung then begin
            with StaData do begin
              Kennung := StaKennung;
              Aktiv := AktivId;
              MrgId := StaTable.FieldByName(C_Sta_MrgId).AsInteger;
              MrgTyp := StaTable.FieldByName(C_Sta_MrgTyp).AsInteger;
              StationsName := StaTable.FieldByName(C_Sta_StationsName).AsString;
              TagesEnde := StaTable.FieldByName(C_Sta_TagesEnde).AsInteger;
              Prioritaet := StaTable.FieldByName(C_Sta_Prioritaet).AsInteger;
            end;
            Result:=true;
            Break;
          end;
        end;
        StaTable.Next;
      end;
    finally
      StaTable.Close;
    end;
  end;
end;

{---------------------------------------------------------------------------------------------}
function TMRGStammdaten.GetStaDSFGData (MrgId: TMrgId; var StaDSfGData: TStaDSFGData): boolean;
{---------------------------------------------------------------------------------------------}
{ liefert Datenrecord aus STADSFG.DB, falls vorhanden;
  ‹bergabe: MrgId
  R¸ckgabe: Datenrecord
  Ergebnis: true, wenn Datensatz in Tabelle gefunden }
begin
  Result := false;
  if StaDSfGTable.Exists then begin
    StaDSFGTable.Open;
    try
      if StaDSFGTable.FindKey ([MrgId]) then begin
        with StaDSfGData do begin
          MrgId := StaDSFGTable.FieldByName(C_StaDSFG_MrgId).AsInteger;
          Adresse := StaDSFGTable.FieldByName(C_StaDSFG_Adresse).AsString;
          MasterId := StaDSFGTable.FieldByName(C_StaDSFG_MasterId).AsInteger;
        end;
        Result:=true;
      end;
    finally
      StaDSFGTable.Close;
    end;
  end;
end;

{---------------------------------------------------------------------------------------------}
function TMRGStammdaten.GetStaAutoData (MrgId: TMrgId; var StaAutoData: TStaAutoData): boolean;
{---------------------------------------------------------------------------------------------}
begin
  Result := false;
  if StaAutoTable.Exists then begin
    StaAutoTable.Open;
    try
      if StaAutoTable.FindKey ([MrgId]) then begin
        with StaAutoData do begin
          MrgId:=StaAutoTable.FieldByName (C_StaAuto_MrgId).AsInteger;
          Automatik:=StaAutoTable.FieldByName (C_StaAuto_Automatik).AsBoolean;
          Intervall:=StaAutoTable.FieldByName (C_StaAuto_Intervall).AsInteger;
          Zeitsynchronisation:=StaAutoTable.FieldByName (C_StaAuto_Zeitsynchronisation).AsBoolean;
          Rundpufferreset:=StaAutoTable.FieldByName (C_StaAuto_Rundpufferreset).AsBoolean;
        end;
        Result:=true;
      end;
    finally
      StaAutoTable.Close;
    end;
  end;
end;

{------------------------------------------------------------------------------------------}
function TMRGStammdaten.GetStaDFUData (MrgId: TMrgId; var StaDFUData: TStaDFUData): boolean;
{------------------------------------------------------------------------------------------}
begin
  Result := false;
  if StaDfuTable.Exists then begin
    StaDFUTable.Open;
    try
      if StaDFUTable.FindKey ([MrgId]) then begin
        with StaDFUData do begin
          MrgId:=StaDFUTable.FieldByName (C_StaDfu_MrgId).AsInteger;
          Rufnummer:=StaDFUTable.FieldByName (C_StaDfu_Rufnummer).AsString;
          Passwort [1]:=StaDFUTable.FieldByName (C_StaDfu_Passwort1).AsString;
          Passwort [2]:=StaDFUTable.FieldByName (C_StaDfu_Passwort2).AsString;
          Passwort [3]:=StaDFUTable.FieldByName (C_StaDfu_Passwort3).AsString;
          Passwort [4]:=StaDFUTable.FieldByName (C_StaDfu_Passwort4).AsString;
          PasswortNr:=StaDFUTable.FieldByName (C_StaDfu_PasswortNr).AsInteger;
          ModemTyp:=StaDFUTable.FieldByName (C_StaDfu_ModemTyp).AsInteger;
          LogPort:=StaDFUTable.FieldByName (C_StaDfu_LogPort).AsInteger;
        end;
        Result:=true;
      end;
    finally
      StaDFUTable.Close;
    end;
  end;
end;

{--------------------------------------------------------------------------------------}
function TMRGStammdaten.GetRufStammDatenMrgId (MrgId: TMrgId; Vorwahl: string;
                                            var RufStammDaten: TRufStammDaten): boolean;
{--------------------------------------------------------------------------------------}
{ Rufstammdaten ¸ber MrgId ermitteln }
var
  MasterId: TMrgId;
  StaData: TStaData;
  StaDfuData: TStaDfuData;
  StaDSFGData: TStaDSFGData;
  StaAutoData: TStaAutoData;
  MrgDefDataDB: TMrgDefDataDB;
  Ok: boolean;

begin
  Result := false;
  if GetStaData (MrgId, StaData) then begin
    Ok := GetStaDfuData (StaData.MrgId, StaDfuData);
    if not Ok then begin
      if GetStaDSFGData (StaData.MrgId, StaDSFGData) then begin
        MasterId := StaDSFGData.MasterId;
        Ok := GetStaDfuData (MasterId, StaDfuData);
      end;
    end;

    if Ok then begin
      RufStammDaten.MrgId := MrgId;
      RufStammDaten.Kennung := StaData.Kennung;
      RufStammDaten.StationsName := StaData.StationsName;
      RufStammDaten.MrgTyp := StaData.MrgTyp;
      RufStammDaten.ModemTyp := StaDfuData.ModemTyp;
      RufStammDaten.RufNummer := Vorwahl + StaDfuData.RufNummer;
      RufStammDaten.PasswortNummer := StaDfuData.Passwortnr;
      RufStammDaten.Passwort := StaDfuData.Passwort [RufStammDaten.PasswortNummer];
      RufStammDaten.TagesEnde := StaData.TagesEnde;

      RufStammDaten.Zeitsynchronisation := false;
      RufStammDaten.Rundpufferreset := false;
      if GetStaAutoData (StaData.MrgId, StaAutoData) then begin
        if GetMrgDefDataDB (StaData.MrgTyp, MrgDefDataDB) then begin
          { ZeitSync und Rundpuffereset nur, wenn Stammdaten und MrgDef-Eintrag dies vorsehen: }
          RufStammDaten.Zeitsynchronisation := StaAutoData.Zeitsynchronisation AND
                                               (Length (MrgDefDataDB.Datumformat) > 0) AND
                                               (Length (MrgDefDataDB.Zeitformat) > 0);
          RufStammDaten.Rundpufferreset := StaAutoData.Rundpufferreset AND MrgDefDataDB.RpReset;
        end;
      end;
      Result:=true;
    end;
  end;  { if GetStaData }
end;

{----------------------------------------------------------------------------------------}
function TMRGStammdaten.GetRufStammDatenKennung (Kennung: string; Vorwahl: string;
                                              var RufStammDaten: TRufStammDaten): boolean;
{----------------------------------------------------------------------------------------}
{ Rufstammdaten ¸ber Kennung ermitteln }
var
  MasterId: TMrgId;
  StaData: TStaData;
  StaDfuData: TStaDfuData;
  StaDSFGData: TStaDSFGData;
  Ok: boolean;

begin
  Result := false;
  if GetStaDataKennung (Kennung, StaData) then begin
    Ok := GetStaDfuData (StaData.MrgId, StaDfuData);
    if not Ok then begin
      if GetStaDSFGData (StaData.MrgId, StaDSFGData) then begin
        MasterId := StaDSFGData.MasterId;
        Ok := GetStaDfuData (MasterId, StaDfuData);
      end;
    end;

    if Ok then begin
      RufStammDaten.MrgId := StaData.MrgId;
      RufStammDaten.Kennung := StaData.Kennung;
      RufStammDaten.StationsName := StaData.StationsName;
      RufStammDaten.MrgTyp := StaData.MrgTyp;
      RufStammDaten.ModemTyp := StaDfuData.ModemTyp;
      RufStammDaten.RufNummer := Vorwahl + StaDfuData.RufNummer;
      RufStammDaten.PasswortNummer := StaDfuData.Passwortnr;
      RufStammDaten.Passwort := StaDfuData.Passwort [RufStammDaten.PasswortNummer];
      RufStammDaten.TagesEnde := StaData.TagesEnde;
      RufStammDaten.Zeitsynchronisation := false;
      RufStammDaten.Rundpufferreset := false;
      Result:=true;
    end;
  end;  { if GetStaData }
end;

{--------------------------------------------------------------------------------------------------}
function TMRGStammdaten.GetModemAbrufgruppe (MrgId: TMrgId; var ModemAbrufgruppe: integer): boolean;
{--------------------------------------------------------------------------------------------------}
var
  MrgTyp: TMrgTyp;
  MrgDefDataDB: TMrgDefDataDB;

begin
  Result:=false;
  ModemAbrufgruppe:=0;
  MrgTyp:=GetMrgTyp (MrgId);
  if MrgTyp >= 0 then begin
    if GetMrgDefDataDB (MrgTyp, MrgDefDataDB) then begin
      ModemAbrufgruppe:=MrgDefDataDB.ModemAbrufgruppe;
      Result:=true;
    end;
  end;
end;

{----------------------------------------------------------------------------------}
function TMRGStammdaten.GetModemtyp (MrgId: TMrgId; var Modemtyp: integer): boolean;
{----------------------------------------------------------------------------------}
var
  StaDfuData: TStaDfuData;

begin
  Result:=false;
  Modemtyp:=0;
  if GetStaDfuData (MrgId, StaDfuData) then begin
    Modemtyp:=StaDfuData.Modemtyp;
    Result:=true;
  end;
end;

{------------------------------------------------------------------}
procedure TMRGStammdaten.GetStationsnamenKennungen (aQuery: TQuery);
{------------------------------------------------------------------}
{ Stationsnamen und Kennungen aller aktiven Stationen aus Sta-Tabelle lesen und
  in Query zur¸ckgeben;
  R¸ckgabe: Query }
begin
  with aQuery do begin
    Close;
    DataBaseName:=Path;
    Sql.Clear;
    Sql.Add('SELECT ' + C_Sta_Kennung + ',');
    Sql.Add(C_Sta_MrgId + ',');
    Sql.Add(C_Sta_Stationsname);
    Sql.Add('FROM "' + CDBSta + '"');
    Sql.Add('WHERE ' + C_Sta_Aktiv + '= :Aktiv');
    ParamByName ('Aktiv').asInteger:=0;
    Open;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TMRGStammdaten.GetKennungenByMrgTyp (AMrgTyp: integer; aQuery: TQuery);
{-------------------------------------------------------------------------------}
{ Kennungen aller aktiven Stationen des ¸bergebenen MRG-Typs aus Sta-Tabelle lesen
  und in Query zur¸ckgeben;
  R¸ckgabe: Query }
begin
  with aQuery do begin
    Close;
    DataBaseName:=Path;
    Sql.Clear;
    Sql.Add('SELECT ' + C_Sta_Kennung);
    Sql.Add('FROM "' + CDBSta + '"');
    Sql.Add('WHERE ' + C_Sta_Aktiv + ' = :Aktiv AND ');
    Sql.Add(C_Sta_MrgTyp + ' = ' + IntToStr (AMrgTyp));
    ParamByName ('Aktiv').asInteger:=0;
    Open;
  end;
end;

{---------------------------------------------------------------------------------------}
function TMRGStammdaten.GetMDEStationsnamenKennungenGeraetetyp (aQuery: TQuery): boolean;
{---------------------------------------------------------------------------------------}
{ Stationsnamen mit Kennung und Ger‰tetyp aller aktiven MDE-Stationen lesen und
  in Query zur¸ckgeben;
  R¸ckgabe: Query
  Ergebnis: true, wenn Query erzeugt werden konnte }
begin
  Result:=false;
  with aQuery do begin
    Close;
    DataBaseName:=Path;
    if FileExists(Path+CTblMRGDEF) then begin
      Sql.Clear;
      Sql.Add('SELECT ' + C_Sta_Kennung + ',');
      Sql.Add(C_Sta_MrgId + ',');
      Sql.Add(C_Sta_MrgTyp + ',');
      Sql.Add(C_Sta_Stationsname + ',');
      Sql.Add(C_MrgDef_MrgName);
      Sql.Add('FROM "' + CDBSta + '" A,');
      Sql.Add('"' + CDBStaMDE + '" B,');
      Sql.Add('"' + CTblMRGDEF + '" C');
      Sql.Add('WHERE (' + C_Sta_Aktiv + '= :Aktiv) AND ');
      Sql.Add('(A.' + C_Sta_MrgId + ' = ' + 'B.' + C_StaMDE_MrgId + ') AND ');
      Sql.Add('(A.' + C_Sta_MrgTyp + ' = ' + 'C.' + C_MrgDef_MrgTyp + ')');
      Sql.Add('ORDER BY ' + C_Sta_Kennung);
      ParamByName ('Aktiv').asInteger:=0;
      Open;
      Result:=true;
    end;
  end;
end;

{------------------------------------------------------------------}
procedure TMRGStammdaten.SetKennung (MrgId: integer; Value: string);
{------------------------------------------------------------------}
{ Kennung in Stammdaten ‰ndern }
begin
  if StaTable.Exists then begin
    StaTable.IndexName:=CSMrgId;                       { sortiert nach MrgID }
    StaTable.Open;
    try
      if StaTable.FindKey ([MrgId]) then begin
        StaTable.Edit;
        StaTable.FieldByName (C_Sta_Kennung).AsString:=Value;
        StaTable.Post;
      end;
    finally
      StaTable.Close;
    end;
  end;
end;

{----------------------------------------------------------------------------------}
procedure TMRGStammdaten.SetPasswort (MrgId: integer; PWNr: integer; Value: string);
{----------------------------------------------------------------------------------}
{ Passwort in Stammdaten ‰ndern }
begin
  if StaDfuTable.Exists then begin
    StaDFUTable.Open;
    try
      if StaDFUTable.FindKey ([MrgId]) then begin
        StaDFUTable.Edit;
        case PWNr of
          1: StaDFUTable.FieldByName (C_StaDfu_Passwort1).AsString:=Value;
          2: StaDFUTable.FieldByName (C_StaDfu_Passwort2).AsString:=Value;
          3: StaDFUTable.FieldByName (C_StaDfu_Passwort3).AsString:=Value;
          4: StaDFUTable.FieldByName (C_StaDfu_Passwort4).AsString:=Value;
        end;
        StaDFUTable.Post;
      end;
    finally
      StaDFUTable.Close;
    end;
  end;
end;


{ TKanalObj }

{---------------------------------------------------------}
constructor TKanalObj.Create (StaKanalData: TStaKanalData);
{---------------------------------------------------------}
begin
  inherited Create;
  KanalTyp := KTyp_Unknown;
  with Kanal do begin
    MrgKanal := StaKanalData.MrgKanal;
    Aktiv := StaKanalData.Aktiv;
    KanalName:=StaKanalData.KanalName;
    Kontroll := StaKanalData.Kontroll;
    Eingang := StaKanalData.Eingang;
    Einheit:=StaKanalData.Einheit;
    Kommastellen := StaKanalData.Kommastellen;
  end;
  with ImpulsKanal do begin
    Faktor := -1;
    Teiler := -1;
    OrgFaktor := -1;
  end;
  with AnalogKanal do begin
    MessBereichMin := -1;
    MessBereichMax := -1;
    StromBereich := -1;
    Offset := false;
    AufzMin := -1;
    AufzMax := -1;
  end;
end;

{------------------------------------------------------------}
procedure TKanalObj.SetImpKanalData (StaImpData: TStaImpData);
{------------------------------------------------------------}
begin
  KanalTyp:= KTyp_Impuls;
  with ImpulsKanal do begin
    Faktor := StaImpData.Faktor;
    Teiler := StaImpData.Teiler;
    OrgFaktor := StaImpData.OrgFaktor;
  end;
end;

{------------------------------------------------------------}
procedure TKanalObj.SetAnaKanalData (StaAnaData: TStaAnaData);
{------------------------------------------------------------}
begin
  KanalTyp:= KTyp_Analog;
  with AnalogKanal do begin
    MessBereichMin := StaAnaData.MessBereichMin;
    MessBereichMax := StaAnaData.MessBereichMax;
    StromBereich := StaAnaData.StromBereich;
    Offset := StaAnaData.Offset;
    AufzMin := StaAnaData.AufzMin;
    AufzMax := StaAnaData.AufzMax;
  end;
end;

{----------------------------------------}
function TKanalObj.GetKanalTyp: TKanalTyp;
{----------------------------------------}
begin
  GetKanalTyp := KanalTyp;
end;


{ TImpulsKanalObj }

{----------------------------------------------------------------------------------------}
constructor TImpulsKanalObj.Create (StaKanalData: TStaKanalData; StaImpData: TStaImpData);
{----------------------------------------------------------------------------------------}
begin
  inherited Create;
  with Kanal do begin
    MrgKanal := StaKanalData.MrgKanal;
    Aktiv := StaKanalData.Aktiv;
    KanalName:=StaKanalData.KanalName;
    Kontroll := StaKanalData.Kontroll;
    Eingang := StaKanalData.Eingang;
    Einheit:=StaKanalData.Einheit;
    Kommastellen := StaKanalData.Kommastellen;
  end;
  with ImpulsKanal do begin
    Faktor := StaImpData.Faktor;
    Teiler := StaImpData.Teiler;
    OrgFaktor := StaImpData.OrgFaktor;
  end;
end;


{ TAnalogKanalObj }

{----------------------------------------------------------------------------------------}
constructor TAnalogKanalObj.Create (StaKanalData: TStaKanalData; StaAnaData: TStaAnaData);
{----------------------------------------------------------------------------------------}
begin
  inherited Create;
  with Kanal do begin
    MrgKanal := StaKanalData.MrgKanal;
    Aktiv := StaKanalData.Aktiv;
    KanalName:=StaKanalData.KanalName;
    Kontroll := StaKanalData.Kontroll;
    Eingang := StaKanalData.Eingang;
    Einheit:=StaKanalData.Einheit;
    Kommastellen := StaKanalData.Kommastellen;
  end;
  with AnalogKanal do begin
    MessBereichMin := StaAnaData.MessBereichMin;
    MessBereichMax := StaAnaData.MessBereichMax;
    StromBereich := StaAnaData.StromBereich;
    Offset := StaAnaData.Offset;
    AufzMin := StaAnaData.AufzMin;
    AufzMax := StaAnaData.AufzMax;
  end;
end;


{ TKanalListObj }

{------------------------------------------------------------------}
constructor TKanalListObj.Create (AMrgId: TMrgId; APath: TFilename);
{------------------------------------------------------------------}
begin
  inherited Create;
  MrgId := AMrgId;
  Path := APath;
  Kanalliste := TList.Create;
  ImpulsKanalliste := TList.Create;
  AnalogKanalliste := TList.Create;
  LoadListen;
end;

{-------------------------------}
destructor TKanalListObj.Destroy;
{-------------------------------}
var
  i: integer;
begin
  for i:=0 to ImpulsKanalliste.Count-1 do
    TImpulsKanalObj (ImpulsKanalliste [i]).Free;
  for i:=0 to AnalogKanalliste.Count-1 do
    TAnalogKanalObj (AnalogKanalliste [i]).Free;
  for i:=0 to Kanalliste.Count-1 do
    TKanalObj (Kanalliste [i]).Free;
  ImpulsKanalliste.Free;
  AnalogKanalliste.Free;
  Kanalliste.Free;
  inherited Destroy;
end;

{---------------------------------}
procedure TKanalListObj.LoadListen;
{---------------------------------}
{ liest Kanaldaten f¸r die im Konstruktor ¸bergebene MrgId aus den Stammdaten-
  Kanaltabellen in die Kanallisten ein }
var
  StaKanalTable: TTable;
  StaImpTable: TTable;
  StaAnaTable: TTable;
  StaKanalData: TStaKanalData;
  StaImpData: TStaImpData;
  StaAnaData: TStaAnaData;
  K: TKanalObj;
  KImpuls: TImpulsKanalObj;
  KAnalog: TAnalogKanalObj;

begin
  if FileExists(Path+CDBStaKanal) AND
     FileExists(Path+CDBStaImp) AND
     FileExists(Path+CDBStaAna) then begin
    StaKanalTable:=TTable.Create(nil);
    try
      StaImpTable:=TTable.Create(nil);
      try
        StaAnaTable:=TTable.Create(nil);
        try
          StaKanalTable.DatabaseName:=Path;
          StaKanalTable.TableName:=CDBStaKanal;
          StaKanalTable.Open;
          try
            StaImpTable.DatabaseName:=Path;
            StaImpTable.TableName:=CDBStaImp;
            StaImpTable.Open;
            try
              StaAnaTable.DatabaseName:=Path;
              StaAnaTable.TableName:=CDBStaAna;
              StaAnaTable.Open;
              try
                { Filter auf MrgId }
                StaKanalTable.Filter:=C_StaKanal_MrgId + ' = ' + IntToStr(MrgId);
                StaKanalTable.Filtered:=true;
                while not StaKanalTable.EOF do begin
                  with StaKanalData do begin
                    MrgId := StaKanalTable.FieldByName(C_StaKanal_MrgId).AsInteger;
                    MrgKanal := StaKanalTable.FieldByName(C_StaKanal_MrgKanal).AsInteger;
                    KanalId := StaKanalTable.FieldByName(C_StaKanal_KanalId).AsInteger;
                    Aktiv := StaKanalTable.FieldByName(C_StaKanal_Aktiv).AsBoolean;
                    KanalName := StaKanalTable.FieldByName(C_StaKanal_KanalName).AsString;
                    KanalTyp := StaKanalTable.FieldByName(C_StaKanal_KanalTyp).AsString;
                    Einstellbar := StaKanalTable.FieldByName(C_StaKanal_Einstellbar).AsBoolean;
                    Kontroll := StaKanalTable.FieldByName(C_StaKanal_Kontroll).AsBoolean;
                    Eingang := StaKanalTable.FieldByName(C_StaKanal_Eingang).AsBoolean;
                    Einheit := StaKanalTable.FieldByName(C_StaKanal_Einheit).AsString;
                    Kommastellen := StaKanalTable.FieldByName(C_StaKanal_Kommastellen).AsInteger;
                  end;
                  case StaKanalData.KanalTyp [1] of
                    'I': begin
                           if Get_StaImpData (StaImpTable, true, StaKanalData.KanalId,
                                              StaImpData) then begin
                             K:=TKanalObj.Create (StaKanalData);
                             K.SetImpKanalData (StaImpData);
                             Kanalliste.Add (K);
                             KImpuls:=TImpulsKanalObj.Create (StaKanalData, StaImpData);
                             ImpulsKanalliste.Add (KImpuls);
                           end;
                         end;
                    'A': begin
                           if Get_StaAnaData (StaAnaTable, true, StaKanalData.KanalId,
                                              StaAnaData) then begin
                             K:=TKanalObj.Create (StaKanalData);
                             K.SetAnaKanalData (StaAnaData);
                             Kanalliste.Add (K);
                             KAnalog:=TAnalogKanalObj.Create (StaKanalData, StaAnaData);
                             AnalogKanalliste.Add (KAnalog);
                           end;
                         end;
                  end;
                  StaKanalTable.Next;
                end;
                Kanalliste.Sort (MrgKanalCompare);
                ImpulsKanalliste.Sort (MrgImpulsKanalCompare);
                AnalogKanalliste.Sort (MrgAnalogKanalCompare);
              finally
                StaAnaTable.Close;
              end;
            finally
              StaImpTable.Close;
            end;
          finally
            StaKanalTable.Close;
          end;
        finally
          StaAnaTable.Free;
        end;
      finally
        StaImpTable.Free;
      end;
    finally
      StaKanalTable.Free;
    end;
  end;
end;

{---------------------------------------------------------------------------------------}
function TKanalListObj.Get_StaImpData(Table: TTable; KeepOpen: boolean; KanalId: longint;
                                      var StaImpData: TStaImpData): boolean;
{---------------------------------------------------------------------------------------}
{ liefert in STAIMP.DB gespeicherte Daten eines Impulskanals;
  ‹bergabe: initialisiertes Table-Objekt
            KeepOpen = true: Tabelle soll geˆffnet bleiben
            KanalId
  R¸ckgabe: StaImpData-Record
  Ergebnis: true, wenn StaImpData aus Tabelle gelesen werden konnten }

begin  Result:=false;
  if not Table.Active then Table.Open;
  try
    if Table.FindKey([KanalId]) then begin
      with StaImpData do begin
        KanalId:=Table.FieldByName(C_StaImp_KanalId).AsInteger;
        Faktor:=Table.FieldByName(C_StaImp_Faktor).AsInteger;
        Teiler:=Table.FieldByName(C_StaImp_Teiler).AsInteger;
        OrgFaktor:=Table.FieldByName(C_StaImp_OrgFaktor).AsFloat;
      end;
      Result:=true;
    end;
  finally
    if not KeepOpen then Table.Close;
  end;
end;

{---------------------------------------------------------------------------------------}
function TKanalListObj.Get_StaAnaData(Table: TTable; KeepOpen: boolean; KanalId: longint;
                                      var StaAnaData: TStaAnaData): boolean;
{---------------------------------------------------------------------------------------}
{ liefert in STAANA.DB gespeicherte Daten eines Analogkanals;
  ‹bergabe: initialisiertes Table-Objekt
            KeepOpen = true: Tabelle soll geˆffnet bleiben
            KanalId
  R¸ckgabe: StaAnaData-Record
  Ergebnis: true, wenn StaAnaData aus Tabelle gelesen werden konnten }

begin
  Result:=false;
  if not Table.Active then Table.Open;
  try
    if Table.FindKey([KanalId]) then begin
      with StaAnaData do begin
        KanalId:=Table.FieldByName(C_StaAna_KanalId).AsInteger;
        MessBereichMin:=Table.FieldByName(C_StaAna_MessBereichMin).AsFloat;
        MessBereichMax:=Table.FieldByName(C_StaAna_MessBereichMax).AsFloat;
        StromBereich:=Table.FieldByName(C_StaAna_StromBereich).AsInteger;
        Offset:=Table.FieldByName(C_StaAna_Offset).AsBoolean;
        AufzMin:=Table.FieldByName(C_StaAna_AufzMin).AsInteger;
        AufzMax:=Table.FieldByName(C_StaAna_AufzMax).AsInteger;
      end;
      Result:=true;
    end;
  finally
    if not KeepOpen then Table.Close;
  end;
end;

{----------------------------------------------------------------}
function TKanalListObj.GetMRGKanal (MRGKanal: Integer): TKanalObj;
{----------------------------------------------------------------}
{ liefert Kanaldaten f¸r ¸bergebene MRG-Kanalnummer }
begin
  if (MRGKanal > 0) AND (MRGKanal <= Kanalliste.Count) then
    Result := TKanalObj (KanalListe [MRGKanal-1])
  else
    Result := nil;
end;

{----------------------------------------------------------------}
function TKanalListObj.GetKanalTyp (MRGKanal: Integer): TKanalTyp;
{----------------------------------------------------------------}
{ ‹bergabe: MRG-Kanalnummer;
  Ergebnis: Kanaltyp (Impuls, Analog, unbekannt) }
var
  KanalObj: TKanalObj;
begin
  Result := KTyp_Unknown;
  KanalObj:=GetMRGKanal(MRGKanal);
  if KanalObj <> nil then
    Result := KanalObj.GetKanalTyp;
end;

{----------------------------------------------------------}
function TKanalListObj.IsAktiv (MRGKanal: Integer): Boolean;
{----------------------------------------------------------}
{ ‹bergabe: MRG-Kanalnummer
{ Ergebnis: true, wenn MRG-Kanal aktiv ist }
var
  KanalObj: TKanalObj;
begin
  Result := false;
  KanalObj:=GetMRGKanal(MRGKanal);
  if KanalObj <> nil then
    Result := KanalObj.Kanal.Aktiv;
end;

{----------------------------------------------------------------}
function TKanalListObj.IsAktivImpuls (MRGKanal: Integer): Boolean;
{----------------------------------------------------------------}
{ ‹bergabe: MRG-Kanalnummer
{ Ergebnis: true, wenn MRG-Kanal aktiver Impulskanal ist }
var
  KanalObj: TKanalObj;
begin
  Result := false;
  KanalObj:=GetMRGKanal(MRGKanal);
  if KanalObj <> nil then
    Result := (KanalObj.GetKanalTyp = KTyp_Impuls) AND KanalObj.Kanal.Aktiv;
end;

{----------------------------------------------------------------}
function TKanalListObj.IsAktivKontroll (MRGKanal: Integer): Boolean;
{----------------------------------------------------------------}
{ ‹bergabe: MRG-Kanalnummer
{ Ergebnis: true, wenn MRG-Kanal aktiv ist und Kontrollz‰hler hat }
var
  KanalObj: TKanalObj;
begin
  Result := False;
  KanalObj := GetMRGKanal (MRGKanal);
  if KanalObj <> nil then
    Result := KanalObj.Kanal.Kontroll AND KanalObj.Kanal.Aktiv;
end;

{-----------------------------------------------------------------}
function TKanalListObj.IsAktivEingang (MRGKanal: Integer): Boolean;
{-----------------------------------------------------------------}
{ ‹bergabe: MRG-Kanalnummer
{ Ergebnis: true, wenn MRG-Kanal aktiv ist und Eingangsz‰hler hat }
var
  KanalObj: TKanalObj;
begin
  Result := False;
  KanalObj := GetMRGKanal (MRGKanal);
  if KanalObj <> nil then
    Result := KanalObj.Kanal.Eingang AND KanalObj.Kanal.Aktiv;
end;

{---------------------------------------------}
function TKanalListObj.GetKanalAnzahl: integer;
{---------------------------------------------}
{ Ergebnis: Anzahl der Kan‰le }
begin
  Result:=KanalListe.Count;
end;

{-------------------------------------------------------}
function TKanalListObj.GetEingangszaehlerAnzahl: integer;
{-------------------------------------------------------}
{ Ergebnis: Anzahl der Eingangsz‰hler }
var
  i: integer;
  Anz: integer;
begin
  Anz:=0;
  for i:=0 to Kanalliste.Count-1 do
    if TKanalObj (Kanalliste [i]).Kanal.Eingang then
      inc (Anz);
  Result:=Anz;
end;

{-------------------------------------------------------}
function TKanalListObj.GetKontrollzaehlerAnzahl: integer;
{-------------------------------------------------------}
{ Ergebnis: Anzahl der Kontrollz‰hler }
var
  i: integer;
  Anz: integer;
begin
  Anz:=0;
  for i:=0 to Kanalliste.Count-1 do
    if TKanalObj (Kanalliste [i]).Kanal.Kontroll then
      inc (Anz);
  Result:=Anz;
end;

{----------------------------------------------------------------------------------}
procedure TKanalListObj.GetAnalogkanal_von_bis (var von: integer; var bis: integer);
{----------------------------------------------------------------------------------}
{ R¸ckgaben: von/bis-Bereich der Analogkanalnummern }
var
  i: integer;
begin
  von:=0;
  bis:=0;
  for i:=0 to Kanalliste.Count-1 do begin
    if TKanalObj (Kanalliste [i]).KanalTyp = kTyp_Analog then begin
      if von = 0 then
        von:=TKanalObj (Kanalliste [i]).Kanal.MrgKanal;
      bis:=TKanalObj (Kanalliste [i]).Kanal.MrgKanal;
    end;
  end;
end;

{-----------------------------------------------------}
function TKanalListObj.GetAnalogkanal_AufzMax: integer;
{-----------------------------------------------------}
{ Ergebnis: Aufzeichnungs-Maximum der Analogkan‰le
  -> Es kann davon ausgegangen werden, daﬂ das Aufzeichnungsmaximum bei allen
     Analogkan‰len gleich ist. }
var
  i: integer;
begin
  Result:=0;
  for i:=0 to Kanalliste.Count-1 do begin
    if TKanalObj (Kanalliste [i]).KanalTyp = kTyp_Analog then begin
      Result:=TKanalObj (Kanalliste [i]).AnalogKanal.AufzMax;
      Break;
    end;
  end;
end;

end.
