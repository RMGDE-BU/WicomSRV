{******************************************************************************}
{* Unit: Listen und zugehörige Listenobjekte im 32-Bit-DSfG-Abrufsystem       *}
{* 24.11.99 WW                                                                *}
{* 24.11.99 WW                                                                *}
{* 24.01.02 GD  TAKanaeleData um Kommastellen erweitert                       *}
{* 14.08.02 GD  TInstanzDataList geändert                                     *}
{* 12.01.06 GD  TAKanaeleData um Anzeigebereiche erweitert                    *}
{******************************************************************************}
Unit DListen;

INTERFACE

uses
  Classes, SysUtils, WSysCon, ErrConst;

type

  { Record mit Momentanwerten }

  TDMomValueRec = record
    cIAdr    : string[1];
    sDEA     : string[5];
    sWert    : string[40];
    iStellen : SmallInt;
  end;
  PDMomValueRec = ^TDMomValueRec;

  { Record mit Stammdatenteil, welcher zum Verbindungsaufbau notwendig ist }

  TRufStammDaten = Record
    StationId : integer;
    Stationsname : string [szLen_DSfGStationsName];
    Kennung : string [szLen_DSfGKennung];
    Rufnummer : string [szLen_DSfGRufNummer];
    Passwort : string [szLen_DSfGPasswort];
    Busadresse: string [szlen_DSfGBusadresse];
    LogPort: integer;
    LoginInstanzId: integer;
  end;

  { Record für Zeitangaben }

  TZeitangaben = record
    EAdr: char;     { Busadresse der Instanz, von der die Zeitangaben stammen }
    DatumZeit: TDateTime;
    Zeitzone: string [1];
    LetztVerstZZ: TDateTime;
    vom_PC: boolean;
  end;

  { Record mit Daten zum Senden eines DSfG-DFÜ-Parameters }

  DfueParaSendenDataRec = record
    Befehl: string [3];
    ParaAdr: string [3];
    Wert: string [40];
    Stellen: integer;
    StaAktu: boolean;
  end;

  { Record mit Ergebnisdaten einer DSfG-DFÜ-Parametrierung }

  DfueParaErgebnisDataRec = record
    Befehl: string [3];
    ParaAdr: string [3];
    ParaWertAlt: string [40];
    ParaWertNeu: string [40];
  end;

  { Records für Daten der einzelnen Stammdatentabellen }

  PStationData = ^TStationData;
  TStationData = record
    StationId: integer;
    Stationsname: string [szlen_DSfGStationsName];
    Automatik: boolean;
    ErsterAbruf: TDateTime;  { ab 7.7.2000 nicht mehr verwendet -> jetzt aus ALM-Systemdaten }
    LoginInstanzname: string [szlen_DSfGInstanzName];
    Zweitname: string  [szlen_DSfGZweitName];
    Prioritaet: integer;
  end;

  PInstanzData = ^TInstanzData;
  TInstanzData = record
    InstanzId: integer;
    StationId: integer;
    Instanzname: string [szlen_DSfGInstanzName];
    Instanztyp: string [szlen_DSfGInstanztyp];
    Busadresse: string [szlen_DSfGBusadresse];
    Hersteller: string [szlen_DSfGHersteller];
    Geraetetyp: string [szlen_DSfGGeraetetyp];
    Stand: integer;
    FabrikNr: string [szlen_DSfGFabrikNr];
    SoftwareVersion: string [szlen_DSfGSoftwareVersion];
    Baujahr: integer;
    Inbetriebnahme: TDateTime;
    GerTypNr: integer;                { ab 7.7.2000, Nummern siehe DSFGDEF.DB }
  end;

  PInstDfuData = ^TInstDfuData;
  TInstDfuData = record
    InstanzId: integer;
    Kennung: string [szlen_DSfGKennung];
    Rufnummer: string [szlen_DSfGRufnummer];
    Adresse: array [1..4] of string [szlen_DSfGBusadresse];
    Passwort: array [1..4] of string [szlen_DSfGPasswort];
    LogPort: integer;
  end;

  PArchiveData = ^TArchiveData;
  TArchiveData = record
    InstanzId: integer;
    ArchivNr: integer;
    Name: string [szLen_DSfGArchivName];
    Automatik: boolean;
  end;

  PAKanaeleData = ^TAKanaeleData;
  TAKanaeleData = record
    KanalNr: integer;
    InstanzId: integer;
    ArchivNr: integer;
    Name: string [szLen_DSfGKanalName];
    Kanaltyp: string [szLen_DSfGKanaltyp];
    Werteart: string [szLen_DSfGWerteart];
    EAdr: string [szlen_DSfGBusadresse];
    Automatik: boolean;
    QuellDEL: string [szlen_DSfGDEL];
    Kommastellen: byte;  // 24.01.2001
    AnzeigeMin: double;  // 12.01.2006
    AnzeigeMax: double;  // 12.01.2006
end;

  PLogbuchData = ^TLogbuchData;
  TLogbuchData = record
    InstanzId: integer;
    LogbuchNr: integer;
    Name: string [szLen_DSfGLogbuchName];
    EAdr: string [szlen_DSfGBusadresse];
    Automatik: boolean;
  end;

  TDDelInstDaten = Record
    InstanzId : Integer;
    Delvon : string [szLen_DSfGDel];
    DelBis : string [szLen_DSfGDel];
    VorHer_Nachher : char;
  End;

  PDDelITypDaten = ^TDDelITypDaten;
  TDDelITypDaten = Record
    InstanzTyp : char;
    Delvon : string [szLen_DSfGDel];
    DelBis : string [szLen_DSfGDel];
    VorHer_Nachher : char;
  End;

  { Record für Daten aus DSfGDef.db }

  TDSfGDefData = record
    GerTypNr: integer;
    GerTypName: string [szLen_DSfGGerTypName];
    DELGrp: integer;
    MeldGrp: integer;
    StatusGrp: integer;
  end;


  { Objekt für TKanalKonvList }

  TKanalKonvListObj = class(TObject)
    Kanal: integer;
    Kanaltyp: string [2];
    Werteart: string [2];
  protected
    procedure SetData (AKanal: integer; AKanaltyp, AWerteart: string);
  end;

  { Kanal-Konvertierungsliste }

  TKanalKonvList = class(TList)
  public
    Destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    function GetKanalData (iKanal: integer; var sKanaltyp, sWerteart: string): boolean;
  end;

  { Struktur für Journal-Zeitbereichs-Informationen }

  TJrnZBData = record
    OrdNr_Von: integer;
    OrdNr_Bis: integer;
    DZ_Von: TDateTime;
    DZ_Bis: TDateTime;
  end;

  { Objekt für TKanalJrnZBList }

  TKanalJrnZBListObj = class(TObject)
    Kanal: integer;
    Fehlercode: integer;
    JrnZB_Ist: TJrnZBData;
  private
    procedure SetData (AKanal: integer; AJrnZB_Ist: TJrnZBData; AFehlercode: integer);
  end;

  { Kanal-Journal-Zeitbereichsliste }

  TKanalJrnZBList = class(TList)
  public
    Destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
  end; 

  { Objekt für TAbrufList }

  TAbrufListObj = class(TObject)
    EAdr: string [1];       { Adresse der Instanz, deren Daten angefordert werden }
    InstanzId: integer;     { Id der Instanz, deren Daten angefordert werden }
    Gruppe: integer;
    Kanal: integer;
    Kanaltyp: string [2];
    Werteart: string [2];
    BusAdr_Quelle: string [1];
    Del_von: string;                  { ein oder mehrere Einzel-Datenelemente oder von-Del bei Bereich }
    Del_bis: string [10];             { leer oder bis-Del bei Bereich }
    OrdNr_von: integer;
    OrdNr_bis: integer;
    InstanzId_Quelle: integer;  { Id der Quell-Instanz, von der die Daten stammen }
    GerTypNr_Quelle: integer;   { Gerätetyp-Nr. der Quell-Instanz, von der die Daten stammen }
  public
    procedure SetData (AEAdr: string; AInstanzId, AGruppe, AKanal: integer; AKanaltyp, AWerteart: string;
                       ABusAdr_Quelle: string; ADel_von, ADel_bis: string; AOrdNr_von, AOrdNr_bis: integer;
                       AInstanzId_Quelle: integer; AGerTypNr_Quelle: integer);
  end;

  { Abrufliste für Archive, Logbücher, Instanzwerte }

  TAbrufList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    procedure SetInstanzId_GerTypNrQuelle (ABusAdr_Quelle: string;
                                           AInstanzId_Quelle: integer; AGerTypNr_Quelle: integer);
  end;


  { Objekt für TKonvList }

  TKonvListObj = class(TObject)
    InstanzId: integer;      { Id der Instanz, deren Daten konvertiert werden }
    Gruppe: integer;
    Kanal: integer;
    Kanaltyp: string [2];
    Werteart: string [2];
    Zeitangaben: TZeitangaben;
    InstanzId_Quelle: integer;  { Id der Quell-Instanz, von der die Daten stammen }
    GerTypNr_Quelle: integer;   { Gerätetyp-Nr. der Quell-Instanz, von der die Daten stammen }
    EAdr: string [1];       { Adresse der Instanz, deren Daten konvertiert werden; 12.07.2016, WW }
    // Journal-Zeitbereichs-Informationen; 28.12.2015, WW
    JrnZB: boolean;  // Flag: Journal-Zeitbereichs-Informationen vorhanden ja/nein
    JrnZB_Fehlercode: integer;
    JrnZB_Soll: TJrnZBData;  // Soll
    JrnZB_Ist: TJrnZBData;  // Ist
    // Kanal-Informationen bei zeilenweise abgefragter Archivgruppe; 12.07.2016, WW
    KanalKonvList: TKanalKonvList;  // Liste mit Kanal-Stammdaten
    KanalJrnZBList: TKanalJrnZBList;  // Liste mit Kanal-Journal-Zeitbereichs-Informationen
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetData (AInstanzId, AGruppe, AKanal: integer;
      AKanaltyp, AWerteart: string; AZeitangaben: TZeitangaben;
      AInstanzId_Quelle: integer; AGerTypNr_Quelle: integer; AEAdr: string);
    procedure SetJrnZBData_Soll (AJrnZB_OrdNr_SollVon, AJrnZB_OrdNr_SollBis: integer;
      AJrnZB_DZ_SollVon, AJrnZB_DZ_SollBis: TDateTime; AJrnZB_Fehlercode: integer);
    procedure SetJrnZBData_Ist (AJrnZB_OrdNr_IstVon, AJrnZB_OrdNr_IstBis: integer;
      AJrnZB_DZ_IstVon, AJrnZB_DZ_IstBis: TDateTime; AJrnZB_Fehlercode: integer);
    procedure SetKanalKonvList (AKanalKonvList: TKanalKonvList);
    procedure SetKanalJrnZBList (AKanalJrnZBList: TKanalJrnZBList);
    procedure AddKanalJrnZBListData (AKanal: integer;
      AJrnZB_OrdNr_IstVon, AJrnZB_OrdNr_IstBis: integer;
      AJrnZB_DZ_IstVon, AJrnZB_DZ_IstBis: TDateTime; AJrnZB_Fehlercode: integer);
  end;

  { Konvertierungsliste für Archive, Logbücher, Instanzwerte in Tabellen }

  TKonvList = class(TStringList)
  public
    Destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); override;
  end;


  { Objekt für TKonfigRohdataList }

  TKonfigRohdataListObj = class(TObject)
    RegInstData: string;
  public
    procedure SetData (ARegInstData: string);
  end;

  { Liste für Konfigurations-Rohdaten }

  TKonfigRohdataList = class(TStringList)
  public
    Destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); override;
  end;


  { Liste zur Speicherung von TStationData }

  TStationDataList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
  end;

  { Liste zur Speicherung von TInstanzData }

  TInstanzDataList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
  end;

  { Liste zur Speicherung von TInstDfuData }

  TInstDfuDataList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    procedure UpdateInstanzId (Alt, Neu: integer);
  end;

  { Liste zur Speicherung von TArchiveData }

  TArchiveDataList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    procedure UpdateInstanzId (Alt, Neu: integer);
  end;

  { Liste zur Speicherung von TAKanaeleData }

  TAKanaeleDataList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    procedure UpdateInstanzId (Alt, Neu: integer);
  end;

  { Liste zur Speicherung von TLogbuchData }

  TLogbuchDataList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    procedure UpdateInstanzId (Alt, Neu: integer);
  end;

  { Objekt für TDMomDfueList }

  TDMomDfueListObj = class(TObject)
    Befehl: string [3];
    ParaAdr: string [3];
    Wert: string [40];
    Stellen: integer;
  public
    procedure SetData (ABefehl, AParaAdr, AWert: string; AStellen: integer);
  end;

  { Liste für Momentanwerte der DSfG-DFÜ (Parameter, NTY-Masken, Wieser-Teilnehmerliste) }

  TDMomDfueList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
  end;

  { Objekt für TDDfuePEList }

  TDDfuePEListObj = class(TObject)
    Befehl: string [3];
    ParaAdr: string [3];
    WertAlt: string [40];
    WertNeu: string [40];
    Stellen: integer;
    StaAktu: boolean;
    Aendern: boolean;
    Fertig: boolean;
  public
    procedure SetData (ABefehl, AParaAdr, AWertAlt, AWertNeu: string; AStellen: integer;
                       AStaAktu, AAendern, AFertig: boolean);
  end;

  { Liste für Parameter-Änderungen der DSfG-DFÜ }

  TDDfuePEList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
  end;

  { Objekt für TDWieserDfueParaList }

  TDWieserDfueParaListObj = class(TObject)
    ParaNr: integer;
    Wert: string [40];
    Stellen: integer;
    ParaName: string [100];
    aenderbar: boolean;
    Typ: string [1];
    DefText: string [100];
    DefWert: string [100];
  public
    procedure SetData (AParaNr: integer; AWert: string; AStellen: integer;
                       AParaName: string; Aaenderbar: boolean; aTyp: string;
                       aDefText, aDefWert: string);
  end;

  { Liste für Wieser-DSfG-DFÜ-Parameter }

  TDWieserDfueParaList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
  end;

  { Objekt für TDSfGDataList }

  TDSfGDataListObj = class(TObject)
    EAdr: string;      { Busadresse der Instanz (Daten-Quelle) }
    DEL: string;       { Datenelement-Adresse (Daten-Quelle) }
  public
    procedure SetData (AEAdr, ADEL: string);
  end;

  { Datenliste für Archive, Logbücher, Instanzwerte }

  TDSfGDataList = class (TStringList)
  public
    Destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); override;
  end;

  { Objekt für TResponseLogList mit Datenprüf-/Konvertierungsergebnis }

  TDSfGDataKonvLogObj = class(TObject)
    EAdr: string;        { Busadresse der Instanz (Daten-Quelle) }
    DEL: string;         { Datenelement-Adresse (Daten-Quelle) }
    KonvStatus: integer; { Prüf-/Konvertierungsstatus -> DSFGKONVERR_...-Code aus Errconst.Pas }
  public
    procedure SetData (AEAdr, ADEL: string; AKonvStatus: integer);
  end;

  { Objekt für TResponseLogList mit Rohdaten-Dateiname }

  TRohfileLogObj = class(TObject)
    EAdr: string;        { Busadresse der Instanz (Daten-Quelle) }
    RohfileName: string; { Name der Rohdatendatei }
  public
    procedure SetData (AEAdr, ARohfileName: string);
  end;

  { Liste mit Responselog-Daten }

  TResponseLogList = class (TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    procedure InsertDataKonvLog (AEAdr, ADEL: string; AKonvStatus: integer);
    procedure SortByBusadresse;
  end;

  { Objekt für TAufmTelegrammList }

  TAufmTelegrammListObj = class(TObject)
    Busadresse: string [1];
    Nachrichtentyp: string [1];
    DatumZeit: TDateTime;
    Zeitzone: string [1];
  public
    constructor Create (ABusadresse: string; ANachrichtentyp: string;
                        ADatumZeit: TDateTime; AZeitzone: string);
  end;

  { Liste für Aufmerksamkeits-Telegramme }

  TAufmTelegrammList = class(TList)
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    procedure SortByBusadresse;
  end;

  { Objekt für Response-Rohdaten }

  TResponseRohdatenObj = class(TObject)
    EAdr: string;     { Busadresse der Instanz (Daten-Quelle) }
    DEL: string;      { Datenelement-Adresse (Daten-Quelle) }
    Encode: integer;  { Code für Kodierverfahren }
  public
    procedure SetData (AEAdr, ADEL: string; AEncode: integer);
  end;

IMPLEMENTATION


{ TKanalJrnZBListObj }

{-----------------------------------------------------------------------------}
procedure TKanalJrnZBListObj.SetData (AKanal: integer; AJrnZB_Ist: TJrnZBData;
  AFehlercode: integer);
{-----------------------------------------------------------------------------}
begin
  Kanal:=AKanal;
  JrnZB_Ist:=AJrnZB_Ist;
  Fehlercode:=AFehlercode;
end;

{ TKanalJrnZBList }

{---------------------------------}
destructor TKanalJrnZBList.Destroy;
{---------------------------------}
begin
  Clear;

  inherited Destroy;
end;

{------------------------------}
procedure TKanalJrnZBList.Clear;
{------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items [i])) then TObject(Items [i]).Free;

  inherited Clear;
end;

{------------------------------------------------}
procedure TKanalJrnZBList.Delete(iIndex: integer);
{------------------------------------------------}
begin
  if (Assigned(Items [iIndex])) then TObject(Items [iIndex]).Free;

  inherited Delete(iIndex);
end;                       


{ TKanalKonvListObj }

{----------------------------------------------------------------------------------}
procedure TKanalKonvListObj.SetData (AKanal: integer; AKanaltyp, AWerteart: string);
{----------------------------------------------------------------------------------}
begin
  Kanal:=AKanal;
  Kanaltyp:=AKanaltyp;
  Werteart:=AWerteart;
end;

{ TKanalKonvList }

{--------------------------------}
destructor TKanalKonvList.Destroy;
{--------------------------------}
begin
  Clear;

  inherited Destroy;
end;

{-----------------------------}
procedure TKanalKonvList.Clear;
{-----------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items [i])) then TObject(Items [i]).Free;

  inherited Clear;
end;

{-----------------------------------------------}
procedure TKanalKonvList.Delete(iIndex: integer);
{-----------------------------------------------}
begin
  if (Assigned(Items [iIndex])) then TObject(Items [iIndex]).Free;

  inherited Delete(iIndex);
end;

{----------------------------------------------------}
function TKanalKonvList.GetKanalData (iKanal: integer;
  var sKanaltyp, sWerteart: string): boolean;
{----------------------------------------------------}
var
  i: integer;
  KanalKonvListObj: TKanalKonvListObj;

begin
  Result:=false;
  for i:=0 to Count-1 do begin
    KanalKonvListObj:=TKanalKonvListObj (Items [i]);
    if KanalKonvListObj.Kanal = iKanal then begin
      sKanaltyp:=KanalKonvListObj.Kanaltyp;
      sWerteart:=KanalKonvListObj.Werteart;
      Result:=true;
      Break;
    end;
  end;
end;


{ TAbrufListObj }

{-------------------------------------------------------------------------------------------------------------------}
procedure TAbrufListObj.SetData (AEAdr: string; AInstanzId, AGruppe, AKanal: integer; AKanaltyp, AWerteart: string;
                                 ABusAdr_Quelle: string; ADel_von, ADel_bis: string; AOrdNr_von, AOrdNr_bis: integer;
                                 AInstanzId_Quelle: integer; AGerTypNr_Quelle: integer);
{-------------------------------------------------------------------------------------------------------------------}
begin
  EAdr:=AEAdr;
  InstanzId:=AInstanzId;
  Gruppe:=AGruppe;
  Kanal:=AKanal;
  Kanaltyp:=AKanaltyp;
  Werteart:=AWerteart;
  BusAdr_Quelle:=ABusAdr_Quelle;
  Del_von:=ADel_von;
  Del_bis:=ADel_bis;
  OrdNr_von:=AOrdNr_von;
  OrdNr_bis:=AOrdNr_bis;
  InstanzId_Quelle:=AInstanzId_Quelle;
  GerTypNr_Quelle:=AGerTypNr_Quelle;
 end;

{ TAbrufList }

{----------------------------}
procedure TAbrufList.Clear;
{----------------------------}
var
  i: integer;
Begin
  for i:=0 to Count-1 do
    if (Assigned(Items [i])) then TObject(Items[i]).Free;

  inherited Clear;
end;

{----------------------------}
procedure TAbrufList.Delete(iIndex: integer);
{----------------------------}
begin
  if (Assigned(Items[iIndex])) then TObject(Items [iIndex]).Free;

  inherited Delete(iIndex);
end;

{-------------------------------------------------------------------------------------------------------}
procedure TAbrufList.SetInstanzId_GerTypNrQuelle (ABusAdr_Quelle: string;
                                                  AInstanzId_Quelle: integer; AGerTypNr_Quelle: integer);
{-------------------------------------------------------------------------------------------------------}
{ bei alle Listen-Einträge, welche die übergebene Quell-Busadresse haben, Id und
  Gerätettyp-Nr. der Quell-Instanz setzen }
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if TAbrufListObj (Items [i]).BusAdr_Quelle = ABusAdr_Quelle then begin
      TAbrufListObj (Items [i]).InstanzId_Quelle:=AInstanzId_Quelle;
      TAbrufListObj (Items [i]).GerTypNr_Quelle:=AGerTypNr_Quelle;
    end;
end;

{ TKonvListObj }

{------------------------------}
constructor TKonvListObj.Create;
{------------------------------}
begin
  inherited Create;
  KanalKonvList:=TKanalKonvList.Create;  // 12.07.2016, WW
  KanalJrnZBList:=TKanalJrnZBList.Create;
end;

{------------------------------}
destructor TKonvListObj.Destroy;
{------------------------------}
begin
  KanalJrnZBList.Free;  // 12.07.2016, WW
  KanalKonvList.Free;
  inherited Destroy;
end;

{---------------------------------------------------------------------------------------}
procedure TKonvListObj.SetData (AInstanzId, AGruppe, AKanal: integer;
                                AKanaltyp, AWerteart: string; AZeitangaben: TZeitangaben;
                                AInstanzId_Quelle: integer; AGerTypNr_Quelle: integer;
                                AEAdr: string);
{---------------------------------------------------------------------------------------}
begin
  InstanzId:=AInstanzId;
  Gruppe:=AGruppe;
  Kanal:=AKanal;
  Kanaltyp:=AKanaltyp;
  Werteart:=AWerteart;
  Zeitangaben:=AZeitangaben;
  InstanzId_Quelle:=AInstanzId_Quelle;
  GerTypNr_Quelle:=AGerTypNr_Quelle;
  EAdr:=AEAdr;  // 12.07.2016, WW
  with JrnZB_Soll do begin
    OrdNr_Von:=-1;
    OrdNr_Bis:=-1;
    DZ_Von:=-1;
    DZ_Bis:=-1;
  end;
  with JrnZB_Ist do begin
    OrdNr_Von:=-1;
    OrdNr_Bis:=-1;
    DZ_Von:=-1;
    DZ_Bis:=-1;
  end;
  JrnZB_Fehlercode:=-1;
  JrnZB:=false;  // Keine Journal-Zeitbereichs-Informationen vorhanden; 28.12.2015, WW
end;

{-----------------------------------------------------------------------------}
procedure TKonvListObj.SetJrnZBData_Soll (
  AJrnZB_OrdNr_SollVon, AJrnZB_OrdNr_SollBis: integer;
  AJrnZB_DZ_SollVon, AJrnZB_DZ_SollBis: TDateTime; AJrnZB_Fehlercode: integer);  // 28.12.2015, WW
{-----------------------------------------------------------------------------}
begin
  with JrnZB_Soll do begin
    OrdNr_Von:=AJrnZB_OrdNr_SollVon;
    OrdNr_Bis:=AJrnZB_OrdNr_SollBis;
    DZ_Von:=AJrnZB_DZ_SollVon;
    DZ_Bis:=AJrnZB_DZ_SollBis;
  end;
  JrnZB_Fehlercode:=AJrnZB_Fehlercode;

  JrnZB:=true;
end;

{---------------------------------------------------------------------------}
procedure TKonvListObj.SetJrnZBData_Ist (
  AJrnZB_OrdNr_IstVon, AJrnZB_OrdNr_IstBis: integer;
  AJrnZB_DZ_IstVon, AJrnZB_DZ_IstBis: TDateTime; AJrnZB_Fehlercode: integer);  // 28.12.2015, WW
{---------------------------------------------------------------------------}
begin
  with JrnZB_Ist do begin
    OrdNr_Von:=AJrnZB_OrdNr_IstVon;
    OrdNr_Bis:=AJrnZB_OrdNr_IstBis;
    DZ_Von:=AJrnZB_DZ_IstVon;
    DZ_Bis:=AJrnZB_DZ_IstBis;
  end;
  JrnZB_Fehlercode:=AJrnZB_Fehlercode;

  JrnZB:=true;
end;

{---------------------------------------------------------------------------}
procedure TKonvListObj.SetKanalKonvList (AKanalKonvList: TKanalKonvList);  // 12.07.2016, WW
{---------------------------------------------------------------------------}
var
  i: integer;
  SrcKanalKonvListObj: TKanalKonvListObj;
  DestKanalKonvListObj: TKanalKonvListObj;

begin
  for i:=0 to AKanalKonvList.Count -1 do begin
    SrcKanalKonvListObj:=TKanalKonvListObj (AKanalKonvList[i]);
    DestKanalKonvListObj:=TKanalKonvListObj.Create;
    with SrcKanalKonvListObj do
      DestKanalKonvListObj.SetData(Kanal, Kanaltyp, Werteart);
    KanalKonvList.Add (DestKanalKonvListObj);
  end;
end;

{---------------------------------------------------------------------------}
procedure TKonvListObj.SetKanalJrnZBList (AKanalJrnZBList: TKanalJrnZBList);  // 12.07.2016, WW
{---------------------------------------------------------------------------}
var
  i: integer;
  SrcKanalJrnZBListObj: TKanalJrnZBListObj;
  DestKanalJrnZBListObj: TKanalJrnZBListObj;

begin
  for i:=0 to AKanalJrnZBList.Count -1 do begin
    SrcKanalJrnZBListObj:=TKanalJrnZBListObj (AKanalJrnZBList[i]);
    DestKanalJrnZBListObj:=TKanalJrnZBListObj.Create;
    with SrcKanalJrnZBListObj do
      DestKanalJrnZBListObj.SetData (Kanal, JrnZB_Ist, Fehlercode);
    KanalJrnZBList.Add (DestKanalJrnZBListObj);
  end;
end;

{---------------------------------------------------------------------------}
procedure TKonvListObj.AddKanalJrnZBListData (AKanal: integer;
  AJrnZB_OrdNr_IstVon, AJrnZB_OrdNr_IstBis: integer;
  AJrnZB_DZ_IstVon, AJrnZB_DZ_IstBis: TDateTime; AJrnZB_Fehlercode: integer);  // 12.07.2016, WW
{---------------------------------------------------------------------------}
var
  JrnZBData: TJrnZBData;
  KanalJrnZBListObj: TKanalJrnZBListObj;

begin
  with JrnZBData do begin
    OrdNr_Von:=AJrnZB_OrdNr_IstVon;
    OrdNr_Bis:=AJrnZB_OrdNr_IstBis;
    DZ_Von:=AJrnZB_DZ_IstVon;
    DZ_Bis:=AJrnZB_DZ_IstBis;
  end;
  KanalJrnZBListObj:=TKanalJrnZBListObj.Create;
  KanalJrnZBListObj.SetData (AKanal, JrnZBData, AJrnZB_Fehlercode);
  KanalJrnZBList.Add (KanalJrnZBListObj);
end;

{ TKonvList }

{---------------------------}
destructor TKonvList.Destroy;
{---------------------------}
begin
  Clear;

  inherited Destroy;
end;

{---------------------------}
procedure TKonvList.Clear;
{---------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Objects [i])) then Objects [i].Free;

  inherited Clear;
end;

{---------------------------}
procedure TKonvList.Delete(iIndex: integer);
{---------------------------}
begin
  if (Assigned(Objects [iIndex])) then Objects [iIndex].Free;

  inherited Delete(iIndex);
end;


{------------------------------------------------------------------------------------------------------------------------}

{ TKonfigRohdataListObj }

{-------------------------------------------------------------}
procedure TKonfigRohdataListObj.SetData (ARegInstData: string);
{-------------------------------------------------------------}
begin
  RegInstData:=ARegInstData;
end;

{ TKonfigRohdataList }

{---------------------------}
destructor TKonfigRohdataList.Destroy;
{---------------------------}
begin
  Clear;

  inherited Destroy;
end;

{---------------------------}
procedure TKonfigRohdataList.Clear;
{---------------------------}
var
  i: integer;
Begin
  for i:=0 to Count-1 do
    if (Assigned(Objects [i])) then Objects [i].Free;

  inherited Clear;
end;

{---------------------------}
procedure TKonfigRohdataList.Delete(iIndex: integer);
{---------------------------}
Begin
  if (Assigned(Objects [iIndex])) then Objects [iIndex].Free;

  inherited Delete(iIndex);
end;


{------------------------------------------------------------------------------------------------------------------------}

{ TStationDataList }

{----------------------------------}
procedure TStationDataList.Clear;
{----------------------------------}
var
  i: integer;
  P: PStationData;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then begin
      P:=Items[i];
      Dispose (P);
    end;

  inherited Clear;
end;

{----------------------------}
procedure TStationDataList.Delete(iIndex: integer);
{----------------------------}
var
  P: PStationData;
begin
  if (Assigned(Items[iIndex])) then begin
    P:=Items[iIndex];
    Dispose (P);
  end;

  inherited Delete(iIndex);
end;

{ TInstanzDataList }

{----------------------------------}
procedure TInstanzDataList.Clear; // 14.08.2002 wird auch bei Destroy aufgerufen !
{----------------------------------}
var
  i: integer;
Begin
  for i :=0 to Count-1 do if (Assigned(Items[i])) then Dispose(Items[i]);

  inherited Clear;
end;

{----------------------------------}
procedure TInstanzDataList.Delete(iIndex: integer); // 14.08.2002
{----------------------------------}
Begin
  if (Assigned(Items[iIndex])) then Dispose(Items[iIndex]);

  inherited Delete(iIndex);
end;

{ TInstDfuDataList }

{----------------------------------}
procedure TInstDfuDataList.Clear;
{----------------------------------}
var
  i: integer;
  P: PInstDfuData;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then begin
      P:=Items[i];
      Dispose (P);
    end;

  inherited Clear;
end;

{----------------------------}
procedure TInstDfuDataList.Delete(iIndex: integer);
{----------------------------}
var
  P: PInstDfuData;
begin
  if (Assigned(Items[iIndex])) then begin
    P:=Items[iIndex];
    Dispose (P);
  end;

  inherited Delete(iIndex);
end;

{-------------------------------------------------------------}
procedure TInstDfuDataList.UpdateInstanzId (Alt, Neu: integer);
{-------------------------------------------------------------}
var
  i: integer;
  P: PInstDfuData;
Begin
  for i:=0 to Count-1 do begin
    P:=Items[i];
    if P^.InstanzId = Alt then
      PInstDfuData (Items[i])^.InstanzId:=Neu;
  end;
end;

{ TArchiveDataList }

{----------------------------------}
procedure TArchiveDataList.Clear;
{----------------------------------}
var
  i: integer;
  P: PArchiveData;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then begin
      P:=Items[i];
      Dispose (P);
    end;

  inherited Clear;
end;

{----------------------------}
procedure TArchiveDataList.Delete(iIndex: integer);
{----------------------------}
var
  P: PArchiveData;
begin
  if (Assigned(Items[iIndex])) then begin
    P:=Items[iIndex];
    Dispose (P);
  end;

  inherited Delete(iIndex);
end;

{-------------------------------------------------------------}
procedure TArchiveDataList.UpdateInstanzId (Alt, Neu: integer);
{-------------------------------------------------------------}
var
  i: integer;
  P: PArchiveData;
Begin
  for i:=0 to Count-1 do begin
    P:=Items[i];
    if P^.InstanzId = Alt then
      PArchiveData (Items[i])^.InstanzId:=Neu;
  end;
end;

{ TAKanaeleDataList }

{--------------------------------------------------------------}
procedure TAKanaeleDataList.UpdateInstanzId (Alt, Neu: integer);
{--------------------------------------------------------------}
var
  i: integer;
  P: PAKanaeleData;
Begin
  for i:=0 to Count-1 do begin
    P:=Items[i];
    if P^.InstanzId = Alt then
      PAKanaeleData (Items[i])^.InstanzId:=Neu;
  end;
end;

{-----------------------------------}
procedure TAKanaeleDataList.Clear;
{-----------------------------------}
var
  i: integer;
  P: PAKanaeleData;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then begin
      P:=Items[i];
      Dispose (P);
    end;

  inherited Clear;
end;

{-----------------------------------}
procedure TAKanaeleDataList.Delete(iIndex: integer);
{-----------------------------------}
var
  P: PAKanaeleData;
begin
  if (Assigned(Items[iIndex])) then begin
    P:=Items[iIndex];
    Dispose (P);
  end;

  inherited Delete(iIndex);
end;

{ TLogbuchDataList }

{-----------------------------------}
procedure TLogbuchDataList.Clear;
{-----------------------------------}
var
  i: integer;
  P: PLogbuchData;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then begin
      P:=Items[i];
      Dispose (P);
    end;

  inherited Clear;
end;

{-----------------------------------}
procedure TLogbuchDataList.Delete(iIndex: integer);
{-----------------------------------}
var
  P: PLogbuchData;
begin
  if (Assigned(Items[iIndex])) then begin
    P:=Items[iIndex];
    Dispose (P);
  end;

  inherited Delete(iIndex);
end;

{-------------------------------------------------------------}
procedure TLogbuchDataList.UpdateInstanzId (Alt, Neu: integer);
{-------------------------------------------------------------}
var
  i: integer;
  P: PLogbuchData;
Begin
  for i:=0 to Count-1 do begin
    P:=Items[i];
    if P^.InstanzId = Alt then
      PLogbuchData (Items[i])^.InstanzId:=Neu;
  end;
end;

{ TDMomDfueListObj }

{---------------------------------------------------------------------------------------}
procedure TDMomDfueListObj.SetData (ABefehl, AParaAdr, AWert: string; AStellen: integer);
{---------------------------------------------------------------------------------------}
begin
  Befehl:=ABefehl;
  ParaAdr:=AParaAdr;
  Wert:=AWert;
  Stellen:=AStellen;
end;

{ TDMomDfueList }

{-----------------------------------}
procedure TDMomDfueList.Clear;
{-----------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then TObject(Items [i]).Free;

  inherited Clear;
end;

{-----------------------------------}
procedure TDMomDfueList.Delete(iIndex: integer);
{-----------------------------------}
begin
  if (Assigned(Items[iIndex])) then TObject(Items[iIndex]).Free;

  inherited Delete(iIndex);
end;

{ TDDfuePEListObj }

{------------------------------------------------------------------------------------------}
procedure TDDfuePEListObj.SetData (ABefehl, AParaAdr, AWertAlt, AWertNeu: string;
                                   AStellen: integer; AStaAktu, AAendern, AFertig: boolean);
{------------------------------------------------------------------------------------------}
begin
  Befehl:=ABefehl;
  ParaAdr:=AParaAdr;
  WertAlt:=AWertAlt;
  WertNeu:=AWertNeu;
  Stellen:=AStellen;
  StaAktu:=AStaAktu;
  Aendern:=AAendern;
  Fertig:=AFertig;
end;

{ TDDfuePEList }

{-----------------------------------}
procedure TDDfuePEList.Clear;
{-----------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then TObject(Items [i]).Free;

  inherited Clear;
end;

{-----------------------------------}
procedure TDDfuePEList.Delete(iIndex: integer);
{-----------------------------------}
begin
  if (Assigned(Items[iIndex])) then TObject(Items[iIndex]).Free;

  inherited Delete(iIndex);
end;

{ TDWieserDfueParaListObj }

{----------------------------------------------------------------------------------------------}
procedure TDWieserDfueParaListObj.SetData (AParaNr: integer; AWert: string; AStellen: integer;
                                           AParaName: string; Aaenderbar: boolean; ATyp: string;
                                           ADefText, ADefWert: string);
{----------------------------------------------------------------------------------------------}
begin
  ParaNr:=AParaNr;
  Wert:=AWert;
  Stellen:=AStellen;
  ParaName:=AParaName;
  aenderbar:=Aaenderbar;
  Typ:=aTyp;
  DefText:=ADefText;
  DefWert:=ADefWert;
end;

{ TDWieserDfueParaList }

{-----------------------------------}
procedure TDWieserDfueParaList.Clear;
{-----------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then TObject(Items [i]).Free;

  inherited Clear;
end;

{-----------------------------------}
procedure TDWieserDfueParaList.Delete(iIndex: integer);
{-----------------------------------}
begin
  if (Assigned(Items[iIndex])) then TObject(Items[iIndex]).Free;

  inherited Delete(iIndex);
end;

{ TDSfGDataListObj }

{-------------------------------------------------------}
procedure TDSfGDataListObj.SetData (AEAdr, ADEL: string);
{-------------------------------------------------------}
begin
  EAdr:=AEAdr;
  DEL:=ADEL;
end;

{ TDSfGDataList }

{---------------------------}
destructor TDSfGDataList.Destroy;
{---------------------------}
begin
  Clear;

  inherited Destroy;
end;

{-----------------------------------}
procedure TDSfGDataList.Clear;
{-----------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Objects[i])) then Objects[i].Free;

  inherited Clear;
end;

{-----------------------------------}
procedure TDSfGDataList.Delete(iIndex: integer);
{-----------------------------------}
begin
  if (Assigned(Objects[iIndex])) then Objects[iIndex].Free;

  inherited Delete(iIndex);
end;

{ TDSfGDataKonvLogObj }

{--------------------------------------------------------------------------------}
procedure TDSfGDataKonvLogObj.SetData (AEAdr, ADEL: string; AKonvStatus: integer);
{--------------------------------------------------------------------------------}
begin
  EAdr:=AEAdr;
  DEL:=ADEL;
  KonvStatus:=AKonvStatus;
end;

{ TRohfileLogObj }

{-------------------------------------------------------------}
procedure TRohfileLogObj.SetData (AEAdr, ARohfileName: string);
{-------------------------------------------------------------}
begin
  EAdr:=AEAdr;
  RohfileName:=ARohfileName;
end;

{ TResponseLogList }

{-------------------------------}
procedure TResponseLogList.Clear;
{-------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then TObject(Items [i]).Free;

  inherited Clear;
end;

{-------------------------------------------------}
procedure TResponseLogList.Delete(iIndex: integer);
{-------------------------------------------------}
begin
  if (Assigned(Items[iIndex])) then TObject(Items[iIndex]).Free;

  inherited Delete(iIndex);
end;

{---------------------------------------------------------------------------------------}
procedure TResponseLogList.InsertDataKonvLog (AEAdr, ADEL: string; AKonvStatus: integer);
{---------------------------------------------------------------------------------------}
{ fügt neuen Log-Eintrag mit Konvertierungsergebnis in Liste ein (TDSfGDataKonvLogObj);
  -> je Busadresse/DE-Adresse-Kombination nur 1 Eintrag
  Ergebnis: true, wenn neuer Eintrag eingefügt wurde }
var
  ListObj: TDSfGDataKonvLogObj;
  i: integer;
  found: boolean;
begin
  found:=false;
  for i:=0 to Count-1 do begin
    if TObject (Items [i]) is TDSfGDataKonvLogObj then begin
      ListObj:=TDSfGDataKonvLogObj (Items [i]);
      if (ListObj.EAdr = AEAdr) AND (ListObj.DEL = ADEL) then begin
        found:=true;
        // Konv-Status nur überschreiben, wenn er bisher OK-Status enthielt:
        if ListObj.KonvStatus = DSFGKONVERR_OK then
          ListObj.KonvStatus:=AKonvStatus;
        Break;
      end;
    end;
  end;

  if not found then begin
    ListObj:=TDSfGDataKonvLogObj.Create;
    ListObj.SetData (AEAdr, ADEL, AKonvStatus);
    Add (ListObj);
  end;
end;

{--------------------------------------------------------------}
function RLL_BusadresseCompare (Item1, Item2: Pointer): Integer;
{--------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von in TResponseLogList enthaltenen Objekten
  nach der Busadresse }
var
  EAdr1, EAdr2: string;
begin
  if TObject (Item1) is TDSfGDataKonvLogObj then
    EAdr1:=TDSfGDataKonvLogObj (Item1).EAdr
  else if TObject (Item1) is TRohfileLogObj then
    EAdr1:=TRohfileLogObj (Item1).EAdr
  else begin
    Result:=1;
    exit;
  end;

  if TObject (Item2) is TDSfGDataKonvLogObj then
    EAdr2:=TDSfGDataKonvLogObj (Item2).EAdr
  else if TObject (Item2) is TRohfileLogObj then
    EAdr2:=TRohfileLogObj (Item2).EAdr
  else begin
    Result:=1;
    exit;
  end;

  Result:=CompareStr (EAdr1, EAdr2);
end;

{------------------------------------------}
procedure TResponseLogList.SortByBusadresse;
{------------------------------------------}
begin
  Sort (RLL_BusadresseCompare);            { Liste nach Busadresse sortieren }
end;

{ TAufmTelegrammListObj }

{-------------------------------------------------------------------------------------}
constructor TAufmTelegrammListObj.Create (ABusadresse: string; ANachrichtentyp: string;
                                          ADatumZeit: TDateTime; AZeitzone: string);
{-------------------------------------------------------------------------------------}
begin
  inherited Create;
  Busadresse:=ABusadresse;
  Nachrichtentyp:=ANachrichtentyp;
  DatumZeit:=ADatumZeit;
  Zeitzone:=AZeitzone;
end;

{ TAufmTelegrammList }

{-----------------------------------}
procedure TAufmTelegrammList.Clear;
{-----------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if (Assigned(Items[i])) then TObject(Items [i]).Free;

  inherited Clear;
end;

{-----------------------------------}
procedure TAufmTelegrammList.Delete(iIndex: integer);
{-----------------------------------}
begin
  if (Assigned(Items[iIndex])) then TObject(Items[iIndex]).Free;

  inherited Delete(iIndex);
end;

{--------------------------------------------------------------}
function ATL_BusadresseCompare (Item1, Item2: Pointer): Integer;
{--------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TAufmTelegrammListObj-Objekten nach der
  Busadresse }
begin
  Result:=CompareStr (TAufmTelegrammListObj (Item1).Busadresse,
                      TAufmTelegrammListObj (Item2).Busadresse);
end;

{--------------------------------------------}
procedure TAufmTelegrammList.SortByBusadresse;
{--------------------------------------------}
begin
  Sort (ATL_BusadresseCompare);             { Liste nach Busadresse sortieren }
end;

{ TResponseRohdatenObj }

{-----------------------------------------------------------------------------}
procedure TResponseRohdatenObj.SetData (AEAdr, ADEL: string; AEncode: integer);
{-----------------------------------------------------------------------------}
begin
  EAdr:=AEAdr;
  DEL:=ADEL;
  Encode:=AEncode;
end;

end.
