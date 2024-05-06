{-------------------------------------------------------------------}
{ unit MrgStd                                                       }
{ Komponenten für Stammdatenzugriffe                                }
{ 23.09.1996 DA, Copyright Karl Wieser GmbH                         }
{ 19.11.1998 GD; Anpassung an Delphi 3                              }
{            -> TabellenFeldTyp 'S' als 'asSmallInt' ansprechen     }
{            -> 'LEFT JOIN'-Abfragen funktionieren nicht - Tabelle  }
{            -> Erweiterung der Sta.db um 'Prioritaet'              }
{               -> überall in TMSItemAllg eingebaut                 }
{            -> logische Scnittstelle Sx eingebaut (CLogPort 0..15) }
{ 15.04.1999 GD; Korrektur bei ParamByName().asSmallInt             }
{ 15.04.1999 GD; Erweiterung der Sta.db um 'isWZ_SZ'                }
{ 19.01.2000 WP; Fehler 8.Kanal ändern bei Offset=true behoben      }
{                in fillJoinTempTable                               }
{ 07.11.2000 WW; Temptabellen wieder raus, LEFT JOIN funktioniert   }
{                jetzt richtig                                      }
{ 16.07.2002 H.-P.R. mit STAMDE					    }
{-------------------------------------------------------------------}

unit Mrgstd;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, DB, DBTables, Menus,
  ObjData, DBUtils, WStrUtils;

{$B-}

const

{ Tables }

CTbSta      = 'Sta';
CTbStaAuto  = 'StaAuto';
CTbStaDfu   = 'StaDfu';
CTbStaDSFG  = 'StaDSFG';
CTbStaKanal = 'StaKanal';
CTbStaImp   = 'StaImp';
CTbStaAna   = 'StaAna';
CTbStaEing  = 'StaEing';
CTbStaKont  = 'StaKont';
CTbStaMDE   = 'StaMde';

CTbMrgDef = 'MrgDef';

{ FieldNames }

CFNMrgId        = 'MrgId';
CFNMrgTyp       = 'MrgTyp';
CFNKennung      = 'Kennung';
CFNStationsName = 'StationsName';
CFNTagesEnde    = 'TagesEnde';
CFNPrioritaet   = 'Prioritaet';  { Geda }
CFNisWz_SZ      = 'WZ_SZ_Umstellung';  { Geda }

CFNMrgName = 'MrgName';

CFNMeldungsgruppe = 'MeldungsGruppe';

CFNAutomatik           = 'Automatik';
CFNIntervall           = 'Intervall';
CFNZeitsynchronisation = 'Zeitsynchronisation';
CFNRundpufferreset     = 'Rundpufferreset';

CFNMrgKanal  = 'MrgKanal';
CFNKanalId   = 'KanalId';
CFNKanalName = 'KanalName';
CFNKanalTyp  = 'KanalTyp';
CFNAktiv     = 'Aktiv';
CFNEinstellbar = 'Einstellbar';
CFNKontroll = 'Kontroll';
CFNEingang = 'Eingang';
CFNImpulsKanalId = 'ImpulsKanalId';
CFNAnalogKanalId = 'AnalogKanalId';

CFNFaktor   = 'Faktor';
CFNTeiler   = 'Teiler';
CFNStdSumAvg = 'Stdsumavg';

CFNMessbereichMin = 'MessbereichMin';
CFNMessbereichMax = 'MessbereichMax';
CFNStrombereich   = 'Strombereich';
CFNOffset         = 'Offset';
CFNAufzMin        = 'AufzMin';
CFNAufzMax        = 'AufzMax';
CFNKommaStellen   = 'KommaStellen';
CFNEinheit        = 'Einheit';

type

  TMrgId   = LongInt;
  TMrgName = string [10];

  TMSListOption = (
    msl_SAll,
    msl_SKAll,
    msl_SKNAll,
    msl_SAuto,
    msl_SKAuto,
    msl_SKNAuto
  );

  TMSSector = (
    msi_Allgemein,
    msi_Info,
    msi_Konv,
    msi_DSFG,
    msi_DFU,
    msi_Automatik,
    msi_MDE,
    msi_Kanal
  );

  TMSSectors = set of TMSSector;

  EMSException = class (Exception);

  TMSQuery = class (TQuery)
  private
    procedure QueryList (ListOption: TMSListOption; const SysDatabase: string);
{    function QuerySelect (const Args: array of string): LongInt;}
  end;

  TMSSQLField = (msf_MrgId, msf_MrgName, msf_Kennung, msf_Stationsname);
  TMSSQLFields = set of TMSSQLField;

  TMSSQLCondition = (msc_Auto);
  TMSSQLConditions = set of TMSSQLCondition;

  TMSSQLOrder = (mso_Kennung, mso_Stationsname, mso_MrgName);

  TMSSQL = class (TComponent)
  private
    FMrgStamm: TFileName;
    FMrgSys: TFileName;
    FQuery: TQuery;
    FSQLFields: TMSSQLFields;
    FSQLConditions: TMSSQLConditions;
    FSQLOrder: TMSSQLOrder;
    procedure SetSQLFields (Value: TMSSQLFields);
    procedure SetSQLConditions (Value: TMSSQLConditions);
    procedure SetSQLOrder (Value: TMSSQLOrder);
    procedure SetMrgStamm (Value: TFileName);
    procedure SetMrgSys (Value: TFileName);
    procedure SetQuery (Value: TQuery);
    procedure CreateSQL;
    procedure AddSQLSelect (Strings: TStrings);
    procedure AddSQLFrom (Strings: TStrings);
    procedure AddSQLWhere (Strings: TStrings);
    procedure AddSQLOrder (Strings: TStrings);
  public
    constructor Create (AOwner: TComponent); override;
  published
    property SQLFields: TMSSQLFields
               read FSQLFields write SetSQLFields;
    property SQLConditions: TMSSQLConditions
               read FSQLConditions write SetSQLConditions;
    property SQLOrder: TMSSQLOrder
               read FSQLOrder write SetSQLOrder;
    property MrgStamm: TFileName
               read FMrgStamm write SetMrgStamm;
    property MrgSys: TFileName
               read FMrgSys write SetMrgSys;
    property Query: TQuery
               read FQuery write SetQuery;
  end;

  { Betriebsmodi für Stammdatenbereiche }

  TMSItemModus = (
    msm_Read,         { nur Lesezugriff }
    msm_ReadWrite,    { Leseschreibzugriff, d.h. Daten änderbar }
    msm_Create        { Neuanlegen von Stammdaten }
  );

  { Basisklasse für Stammdatenbereiche }

  TMSItemBasic = class (TObject)
  private
    FModus: TMSItemModus;
  protected
    procedure SetModus (Value: TMSItemModus); virtual;
    function GetModified: Boolean; virtual; abstract;
    procedure SetModified (Value: Boolean); virtual; abstract;
    function GetValid: Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Read (Key: LongInt; Query: TMSQuery); virtual;
    procedure Modify (Key: LongInt; Query: TMSQuery); virtual;
    function Append (Key: LongInt; Query: TMSQuery): LongInt; virtual;
    procedure Delete (Key: LongInt; Query: TMSQuery); virtual;
  public
    constructor Create;
    property Modified: Boolean read GetModified write SetModified;
    property Modus: TMSItemModus read FModus write SetModus;
    property Valid: Boolean read GetValid;
  end;

  { TMSItemAllg: Enthält allgemeine Stammdateninformationen }

  TMSItemAllg = class (TMSItemBasic)
  private
    FMrgTyp: TIntegerData;          { Mrg-Gerätetyp }
    FKennung: TXStringData;         { Kennung }
    FStationsName: TStringData;     { Stationsname }
    FTagesEnde: TIntegerData;       { Tagesende }
    FPrioritaet: TIntegerData;      { Priorität - GeDa }
    FisWZ_SZ: TBooleanData;         { Zeiumstellung - GeDa }
    procedure CheckIndex (const Kennung: string; Query: TMSQuery);
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
    function Append (Key: LongInt; Query: TMSQuery): LongInt; override;
    procedure Delete (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property MrgTyp: TIntegerData read FMrgTyp;
    property Kennung: TXStringData read FKennung;
    property StationsName: TStringData read FStationsName;
    property TagesEnde: TIntegerData read FTagesEnde;
    property Prioritaet: TIntegerData read FPrioritaet;  { Geda }
    property isWZ_SZ: TBooleanData read FisWZ_SZ;  { Geda }
  end;

  TMSDSFGTyp = (
    DSFG_None,         { Kein DSFG-Gerät }
    DSFG_Slave,        { Master-Gerät }
    DSFG_Master        { Slave-Gerät }
  );

  { TMSItemDSFG: Enthält DSFG-Informationen }

  TMSItemDSFG = class (TMSItemBasic)
  private
    FHasData: TBooleanData;     { Datensatz ist vorhanden }
    FMrgId: TIntegerData;       { MrgId }
    FAdresse: TStringData;      { DSFG-Adresse }
    FMasterId: TIntegerData;    { Id des Masters }
    function GetHasData: Boolean;
    procedure SetHasData (Value: Boolean);
    function GetDSFGTyp: TMSDSFGTyp;
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
    function Append (Key: LongInt; Query: TMSQuery): LongInt; override;
    procedure Delete (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Adresse: TStringData read FAdresse write FAdresse;
    property DSFGTyp: TMSDSFGTyp read GetDSFGTyp;
    property HasData: Boolean read GetHasData write SetHasData;
    property MasterId: TIntegerData read FMasterId write FMasterId;
  end;

  { TMSItemDFU: Enthält DFÜ-Daten }

  TMSItemDFU = class (TMSItemBasic)
  private
    FHasData: TBooleanData;
    FRufnummer: TStringData;
    FPasswort1: TStringData;
    FPasswort2: TStringData;
    FPasswort3: TStringData;
    FPasswort4: TStringData;
    FPasswortNr: TIntegerData;
    FModemTyp: TIntegerData;
    FLogPort: TIntegerData;
    function GetHasData: Boolean;
    procedure SetHasData (Value: Boolean);
    function GetPasswort: string;
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
    function Append (Key: LongInt; Query: TMSQuery): LongInt; override;
    procedure Delete (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property HasData: Boolean read GetHasData write SetHasData;
    property Rufnummer: TStringData read FRufnummer;
    property Passwort1: TStringData read FPasswort1;
    property Passwort2: TStringData read FPasswort2;
    property Passwort3: TStringData read FPasswort3;
    property Passwort4: TStringData read FPasswort4;
    property PasswortNr: TIntegerData read FPasswortNr;
    property ModemTyp: TIntegerData read FModemTyp;
    property LogPort: TIntegerData read FLogPort;
    property Passwort: string read GetPasswort;
  end;

  { TMSItemAuto: Enthält Automatikinformationen }

  TMSItemAuto = class (TMSItemBasic)
  private
    FHasData: TBooleanData;
    FAutomatik: TBooleanData;
    FIntervall: TIntegerData;
    FZeitsynchronisation: TBooleanData;
    FRundpufferreset: TBooleanData;
    function GetHasData: Boolean;
    procedure SetHasData (Value: Boolean);
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
    function Append (Key: LongInt; Query: TMSQuery): LongInt; override;
    procedure Delete (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property HasData: Boolean read GetHasData write SetHasData;
    property Automatik: TBooleanData read FAutomatik;
    property Intervall: TIntegerData read FIntervall;
    property Zeitsynchronisation: TBooleanData read FZeitsynchronisation;
    property Rundpufferreset: TBooleanData read FRundpufferreset;
  end;

  { TMSItemMDE: Enthält MDE Informationen }

  TMSItemMDE = class (TMSItemBasic)
  private
    FHasData: TBooleanData;
    function GetHasData: Boolean;
    procedure SetHasData (Value: Boolean);
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
    function Append (Key: LongInt; Query: TMSQuery): LongInt; override;
    procedure Delete (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property HasData: Boolean read GetHasData write SetHasData;
  end;

  { TMSNewImpulsKanal }

  TMSNewImpulsKanal = class (TMSItemBasic)
  private
    FFaktor: TIntegerData;
    FTeiler: TIntegerData;
    FOrgFaktor: TNumericData;
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Faktor: TIntegerData read FFaktor write FFaktor;
    property Teiler: TIntegerData read FTeiler write FTeiler;
    property OrgFaktor: TNumericData read FOrgFaktor write FOrgFaktor;
  end;

  { TMSNewImpulsKanalPlus }
  { = TMSNewImpulsKanal mit Stundensummand für einzelne Kanäle einstellbar}

  TMSNewImpulsKanalPlus = class (TMSItemBasic)
  private
    FFaktor: TIntegerData;
    FTeiler: TIntegerData;
    FOrgFaktor: TNumericData;
    FStdSumAvg: TStringData;{kbkb}
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Faktor: TIntegerData read FFaktor write FFaktor;
    property Teiler: TIntegerData read FTeiler write FTeiler;
    property OrgFaktor: TNumericData read FOrgFaktor write FOrgFaktor;
    property StdSumAvg: TStringData read FStdSumAvg write FStdSumAvg;{kbkb}
  end;

  { TMSNewAnalogKanal }

  TMSNewAnalogKanal = class (TMSItemBasic)
  private
    FMessbereichMin: TNumericData;
    FMessbereichMax: TNumericData;
    FStrombereich: TIntegerData;
    FOffset: TBooleanData;
    FAufzMin: TIntegerData;
    FAufzMax: TIntegerData;
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property MessbereichMin: TNumericData
               read FMessbereichMin write FMessbereichMin;
    property MessbereichMax: TNumericData
               read FMessbereichMax write FMessbereichMax;
    property Strombereich: TIntegerData
               read FStrombereich write FStrombereich;
    property Offset: TBooleanData read FOffset write FOffset;
    property AufzMin: TIntegerData read FAufzMin write FAufzMin;
    property AufzMax: TIntegerData read FAufzMax write FAufzMax;
  end;

  { TMSNewKanal }

  TMSKanalTyp = (KTypUnknown, KTypImpuls, KTypAnalog);

  TMSNewKanal = class (TMSItemBasic)
  private
    FMrgKanal: TIntegerData;
    FKanalid: TIntegerData;
    FKanalname: TStringData;
    FAktiv: TBooleanData;
    FEinstellbar: TBooleanData;
    FKanaltyp: TStringData;
    FKontroll: TBooleanData;
    FEingang: TBooleanData;
    FKommaStellen: TIntegerData;
    FEinheit: TStringData;
    FImpuls: TMSNewImpulsKanal;
    FAnalog: TMSNewAnalogKanal;
  private
    function GetKanalTyp: TMSKanalTyp;
    procedure SetKanalTyp (Value: TMSKanalTyp);
    function GetImpuls: TMSNewImpulsKanal;
    function GetAnalog: TMSNewAnalogKanal;
    function GetHasImpuls: Boolean;
    function GetHasAnalog: Boolean;
  protected
    procedure SetModus (Value: TMSItemModus); override;
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property MrgKanal: TIntegerData read FMrgKanal;
    property Kanalname: TStringData read FKanalname;
    property Aktiv: TBooleanData read FAktiv;
    property Einstellbar: TBooleanData read FEinstellbar;
    property Kanaltyp: TMSKanalTyp read GetKanalTyp write SetKanalTyp;
    property Kontroll: TBooleanData read FKontroll;
    property Eingang: TBooleanData read FEingang;
    property KommaStellen: TIntegerData
               read FKommaStellen write FKommaStellen;
    property Einheit: TStringData read FEinheit write FEinheit;
    property Impuls: TMSNewImpulsKanal read GetImpuls;
    property Analog: TMSNewAnalogKanal read GetAnalog;
    property HasImpuls: Boolean read GetHasImpuls;
    property HasAnalog: Boolean read GetHasAnalog;
  end;

  { TMSNewKanalPlus }

  TMSNewKanalPlus = class (TMSItemBasic)
  private
    FMrgKanal: TIntegerData;
    FKanalid: TIntegerData;
    FKanalname: TStringData;
    FAktiv: TBooleanData;
    FEinstellbar: TBooleanData;
    FKanaltyp: TStringData;
    FKontroll: TBooleanData;
    FEingang: TBooleanData;
    FKommaStellen: TIntegerData;
    FEinheit: TStringData;
    FImpuls: TMSNewImpulsKanalPlus;
    FAnalog: TMSNewAnalogKanal;
  private
    function GetKanalTyp: TMSKanalTyp;
    procedure SetKanalTyp (Value: TMSKanalTyp);
    function GetImpuls: TMSNewImpulsKanalPlus;
    function GetAnalog: TMSNewAnalogKanal;
    function GetHasImpuls: Boolean;
    function GetHasAnalog: Boolean;
  protected
    procedure SetModus (Value: TMSItemModus); override;
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property MrgKanal: TIntegerData read FMrgKanal;
    property Kanalname: TStringData read FKanalname;
    property Aktiv: TBooleanData read FAktiv;
    property Einstellbar: TBooleanData read FEinstellbar;
    property Kanaltyp: TMSKanalTyp read GetKanalTyp write SetKanalTyp;
    property Kontroll: TBooleanData read FKontroll;
    property Eingang: TBooleanData read FEingang;
    property KommaStellen: TIntegerData
               read FKommaStellen write FKommaStellen;
    property Einheit: TStringData read FEinheit write FEinheit;
    property Impuls: TMSNewImpulsKanalPlus read GetImpuls;
    property Analog: TMSNewAnalogKanal read GetAnalog;
    property HasImpuls: Boolean read GetHasImpuls;
    property HasAnalog: Boolean read GetHasAnalog;
  end;

{ TMSNewKanalList }

  TMSNewKanalList = class (TMSItemBasic)
  private
    FKanal: TList;
    function GetKanal (Index: Integer): TMSNewKanal;
    function GetCount: Integer;
    procedure CheckIndex (Index: Integer);
{    function GetMrgKanal (Index: Integer): TMSNewKanal;}
  protected
    procedure Clear; override;
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure SetModus (Value: TMSItemModus); override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Kanal [Index: Integer]: TMSNewKanal
               read GetKanal; default;
  end;

{ TMSNewKanalListPlus }

  TMSNewKanalListPlus = class (TMSItemBasic)
  private
    FKanal: TList;
    function GetKanal (Index: Integer): TMSNewKanalPlus;
    function GetCount: Integer;
    procedure CheckIndex (Index: Integer);
{    function GetMrgKanal (Index: Integer): TMSNewKanalPlus;}
  protected
    procedure SetModus (Value: TMSItemModus); override;
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Kanal [Index: Integer]: TMSNewKanalPlus
               read GetKanal; default;
  end;

  { TMSItemInfo: Enthält zusätzliche Stammdateninformationen }

  TMSItemInfo = class (TMSItemBasic)
  private
    FMrgName: TStringData;
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property MrgName: TStringData read FMrgName;
  end;

  { TMSItemKonv: Enthält Konvertierungsinformationen }

  TMSItemKonv = class (TMSItemBasic)
  private
    FMeldungsGruppe: TIntegerData;
  protected
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure Clear; override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
  public
    constructor Create;
    destructor Destroy; override;
    property MeldungsGruppe: TIntegerData read FMeldungsgruppe;
  end;

  { TMSStammdaten }

  TMSStammdaten = class;

  TMSStdNotifyEvent = procedure (MSStd: TMSStammdaten) of object;

  TMSStammdaten = class (TComponent)
  private
    FModus: TMSItemModus;
    FMrgId: TMrgId;
    FSectors: TMSSectors;
    FQuery: TMSQuery;
    FQuerySys: TMSQuery;
    FMSItem: array [TMSSector] of TMSItemBasic;
    FBeforeClear: TMSStdNotifyEvent;
    FAfterClear: TMSStdNotifyEvent;
    procedure SetModus (Value: TMSItemModus);
    function GetModified: Boolean;
    procedure SetModified (Value: Boolean);
    function GetValid: Boolean;
    function GetMrgId: TMrgId;
    procedure SetMrgId (Value: TMrgId);
    procedure SetSectors (Value: TMSSectors);
    function GetDatabaseName: TFileName;
    procedure SetDatabaseName (Value: TFileName);
    function GetSysDatabase: TFileName;
    procedure SetSysDatabase (Value: TFileName);
    function GetAllgemein: TMSItemAllg;
    function GetInfo: TMSItemInfo;
    function GetKonv: TMSItemKonv;
    function GetDSFG: TMSItemDSFG;
    function GetDFU: TMSItemDFU;
    function GetAutomatik: TMSItemAuto;
    function GetMDE: TMSItemMDE;
    function GetKanal: TMSNewKanalList;
    procedure FreeAllMSItems;
    procedure ClearAllMSItems;
    procedure CopyMrgKanalToStaKanal;
    procedure CreateImpulsAnalogChannels;
    procedure CreateImpulsKanal (MrgKanal: Integer; KanalId: LongInt);
    procedure CreateAnalogKanal (MrgKanal: Integer; KanalId: LongInt);
  protected
    procedure DoBeforeClear; virtual;
    procedure DoAfterClear; virtual;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Read;
    procedure Modify;
    procedure Append;
    procedure Delete;
    procedure Clear;
    property Modified: Boolean read GetModified write SetModified;
    property Valid: Boolean read GetValid;
    property MrgId: TMrgId read GetMrgId write SetMrgId;
    property Allgemein: TMSItemAllg read GetAllgemein;
    property Info: TMSItemInfo read GetInfo;
    property Konv: TMSItemKonv read GetKonv;
    property DSFG: TMSItemDSFG read GetDSFG;
    property DFU: TMSItemDFU read GetDFU;
    property Automatik: TMSItemAuto read GetAutomatik;
    property MDE: TMSItemMDE read GetMDE;
    property Kanal: TMSNewKanalList read GetKanal;
  published
    property DatabaseName: TFileName
               read GetDatabaseName write SetDatabaseName;
    property SysDatabase: TFileName
               read GetSysDatabase write SetSysDatabase;
    property Modus: TMSItemModus
               read FModus write SetModus;
    property Sectors: TMSSectors
               read FSectors write SetSectors;
    property BeforeClear: TMSStdNotifyEvent
               read FBeforeClear write FBeforeClear;
    property AfterClear: TMSStdNotifyEvent
               read FAfterClear write FAfterClear;
  end;

  { TMSStammdatenPlus }

  TMSStammdatenPlus = class;

  TMSStdPlusNotifyEvent = procedure (MSStdPlus: TMSStammdatenPlus) of object;

  TMSStammdatenPlus = class (TComponent)
  private
    FModus: TMSItemModus;
    FMrgId: TMrgId;
    FSectors: TMSSectors;
    FQuery: TMSQuery;
    FQuerySys: TMSQuery;
    FMSItem: array [TMSSector] of TMSItemBasic;
    FBeforeClear: TMSStdPlusNotifyEvent;
    FAfterClear: TMSStdPlusNotifyEvent;
    procedure SetModus (Value: TMSItemModus);
    function GetModified: Boolean;
    procedure SetModified (Value: Boolean);
    function GetValid: Boolean;
    function GetMrgId: TMrgId;
    procedure SetMrgId (Value: TMrgId);
    procedure SetSectors (Value: TMSSectors);
    function GetDatabaseName: TFileName;
    procedure SetDatabaseName (Value: TFileName);
    function GetSysDatabase: TFileName;
    procedure SetSysDatabase (Value: TFileName);
    function GetAllgemein: TMSItemAllg;
    function GetInfo: TMSItemInfo;
    function GetKonv: TMSItemKonv;
    function GetDSFG: TMSItemDSFG;
    function GetDFU: TMSItemDFU;
    function GetAutomatik: TMSItemAuto;
    function GetMDE: TMSItemMDE;
    function GetKanal: TMSNewKanalListPlus;
    procedure FreeAllMSItems;
    procedure ClearAllMSItems;
    procedure CopyMrgKanalToStaKanal;
    procedure CreateImpulsAnalogChannels;
    procedure CreateImpulsKanal (MrgKanal: Integer; KanalId: LongInt);
    procedure CreateAnalogKanal (MrgKanal: Integer; KanalId: LongInt);
  protected
    procedure DoBeforeClear; virtual;
    procedure DoAfterClear; virtual;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Read;
    procedure Modify;
    procedure Append;
    procedure Delete;
    procedure Clear;
    property Modified: Boolean read GetModified write SetModified;
    property Valid: Boolean read GetValid;
    property MrgId: TMrgId read GetMrgId write SetMrgId;
    property Allgemein: TMSItemAllg read GetAllgemein;
    property Info: TMSItemInfo read GetInfo;
    property Konv: TMSItemKonv read GetKonv;
    property DSFG: TMSItemDSFG read GetDSFG;
    property DFU: TMSItemDFU read GetDFU;
    property Automatik: TMSItemAuto read GetAutomatik;
    property MDE: TMSItemMDE read GetMDE;
    property Kanal: TMSNewKanalListPlus read GetKanal;
  published
    property DatabaseName: TFileName
               read GetDatabaseName write SetDatabaseName;
    property SysDatabase: TFileName
               read GetSysDatabase write SetSysDatabase;
    property Modus: TMSItemModus
               read FModus write SetModus;
    property Sectors: TMSSectors
               read FSectors write SetSectors;
    property BeforeClear: TMSStdPlusNotifyEvent
               read FBeforeClear write FBeforeClear;
    property AfterClear: TMSStdPlusNotifyEvent
               read FAfterClear write FAfterClear;
  end;

  TMSListBox = class (TCustomListBox)
  private
    FOption: TMSListOption;
    FQuery: TMSQuery;
    FSysDataBase: TFileName;
    FKennungLen: Integer;
    FMrgNameLen: Integer;
    function GetActive: Boolean;
    procedure SetActive (Value: Boolean);
    function GetDatabaseName: TFileName;
    procedure SetDatabaseName (Value: TFileName);
    procedure SetOption (Value: TMSListOption);
    function GetSelectedMrgId: TMrgId;
  protected
    procedure CreateWnd; override;
    procedure ExecQuery; virtual;
    procedure DrawItem (Index: Integer; Rect: TRect;
                          State: TOwnerDrawState); override;
    procedure QueryAfterOpen (DataSet: TDataset);
    procedure QueryBeforeClose (DataSet: TDataSet);
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    property SelectedMrgId: TMrgId read GetSelectedMrgId;
  published
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Active: Boolean
               read GetActive write SetActive;
    property Option: TMSListOption
               read FOption write SetOption;
    property DatabaseName: TFileName
               read GetDatabaseName write SetDatabaseName;
    property SysDatabase: TFileName
               read FSysDataBase write FSysDataBase;
  end;

implementation

const

{ Werte für Kanaltyp }

CVImpuls = 'I';
CVAnalog = 'A';

{ Select Command }

CSQLSelectAll  = 'SELECT %s FROM %s';
CSQLSelectCond = CSQLSelectAll + ' WHERE %s';

{ Bereichsgrenzen }

CMrgNameLen = 10;
CMrgTypMin = 0;
CMrgTypMax = MaxLongint;
CMrgIdMin = 0;
CMrgIdMax = MaxLongInt;
CKennungLen = 14;
CStationsNameLen = 40;
CTagesEndeMin = 0;
CTagesEndeMax = 23;
CPrioritaetMin = 1;    { GeDa }
CPrioritaetMax = 10;   { GeDa; WW 08.11.2000: geändert von 24 auf 10 }
CIntervallMin = 1;
CIntervallMax = 24;
CMrgKanalMin = 1;
CMrgKanalMax = MaxInt;
CKanalNameLen = 20;
CStrombereichMin = 0;
CStrombereichMax = 1;
CAufzMinMin = 0;
CAufzMinMax = MaxLongInt;
CAufzMaxMin = 0;
CAufzMaxMax = MaxLongInt;
CKommaStellenMin = 0;
CKommaStellenMax = 9; { WW 08.11.2000: geändert von MaxInt auf 9 }
CEinheitLen = 10;
CAdresseLen = 1;
CRufnummerLen = 20;
CPasswortlen = 10;
CPasswortNrMin = 1;
CPasswortNrMax = 4;
CModemTypMin = 1;
CModemTypMax = 2;
CLogPortMin = 0;  { GeDa - LogPort 0 ist 'Sx' }
CLogPortMax = 16;

COrgFaktorMin = 0.001;
COrgFaktorMax = 100000;


{ TMSQuery }

{-------------------------------------------------------}
procedure TMSQuery.QueryList (ListOption: TMSListOption;
                              const SysDatabase: string);
{-------------------------------------------------------}
begin
  Close;
  SQL.Clear;
  case ListOption of
    msl_SAll:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('MrgId, StationsName');
        SQL.Add ('FROM');
        SQL.Add ('Sta');
        SQL.Add ('WHERE');
        SQL.Add ('Sta.Aktiv = 0');
      end;
    msl_SKAll:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('MrgId, StationsName, Kennung');
        SQL.Add ('FROM');
        SQL.Add ('Sta');
        SQL.Add ('WHERE');
        SQL.Add ('Sta.Aktiv = 0');
      end;
    msl_SKNAll:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('Sta.MrgId, Sta.StationsName, Sta.Kennung, Def.MrgName');
        SQL.Add ('FROM');
        SQL.Add ('Sta,');
        SQL.Add ('":' + SysDatabase + ':' + 'MrgDef.db" Def');
        SQL.Add ('WHERE');
        SQL.Add ('Sta.Aktiv = 0 AND Sta.MrgTyp = Def.MrgTyp');
      end;
    msl_SAuto:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('Sta.MrgId, Sta.StationsName');
        SQL.Add ('FROM');
        SQL.Add ('Sta, StaAuto');
        SQL.Add ('WHERE');
        SQL.Add ('Sta.Aktiv = 0 AND StaAuto.MrgId = Sta.MrgId');
      end;
    msl_SKAuto:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('Sta.MrgId, Sta.StationsName, Sta.StationsName');
        SQL.Add ('FROM');
        SQL.Add ('Sta, StaAuto');
        SQL.Add ('WHERE');
        SQL.Add ('Sta.Aktiv = 0 AND StaAuto.MrgId = Sta.MrgId');
      end;
    msl_SKNAuto:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('Sta.MrgId, Sta.StationsName, Sta.Kennung, Def.MrgName');
        SQL.Add ('FROM');
        SQL.Add ('Sta, StaAuto,');
        SQL.Add ('":' + SysDatabase + ':' + 'MrgDef.db" Def');
        SQL.Add ('WHERE');
        SQL.Add ('Sta.Aktiv = 0 AND StaAuto.MrgId = Sta.MrgId AND');
        SQL.Add ('Sta.MrgTyp = Def.MrgTyp');
      end;
  end;
  Open;
end;

{ QuerySelect: Fügt alle Argumente zu einer SQL-Anweisung zusammen
               und führt diese aus. Es wird angenommen, daß eine SELECT
               Anweisung durchgeführt wird. Die Größe der Ergebnistabelle
               wird zurückgeliefert
function TMSQuery.QuerySelect (const Args: array of string): LongInt;
var
  i: Integer;
begin
  Close;
  SQL.Clear;
  for i := Low (Args) to High (Args) do
  begin
    SQL.Add (Args [i]);
  end;
  Open;
  result := RecordCount;
end;                                }

{--------}
{ TMSSQL }
{--------}

{--------------------------------------------------}
procedure TMSSQL.SetSQLFields (Value: TMSSQLFields);
{--------------------------------------------------}
begin
  if Value <> FSQLFields then
  begin
    FSQLFields := Value;
    CreateSQL;
  end;
end;

{----------------------------------------------------------}
procedure TMSSQL.SetSQLConditions (Value: TMSSQLConditions);
{----------------------------------------------------------}
begin
  if Value <> FSQLConditions then
  begin
    FSQLConditions := Value;
    CreateSQL;
  end;
end;

{------------------------------------------------}
procedure TMSSQL.SetSQLOrder (Value: TMSSQLOrder);
{------------------------------------------------}
begin
  if Value <> FSQLOrder then
  begin
    FSQLOrder := Value;
    CreateSQL;
  end;
end;

{----------------------------------------------}
procedure TMSSQL.SetMrgStamm (Value: TFileName);
{----------------------------------------------}
begin
  if Value <> FMrgStamm then
  begin
    FMrgStamm := Value;
    CreateSQL;
  end;
end;

{--------------------------------------------}
procedure TMSSQL.SetMrgSys (Value: TFileName);
{--------------------------------------------}
begin
  if Value <> FMrgSys then
  begin
    FMrgSys := Value;
    CreateSQL;
  end;
end;

{----------------------------------------}
procedure TMSSQL.SetQuery (Value: TQuery);
{----------------------------------------}
begin
  FQuery := Value;
  FQuery.DatabaseName := MrgStamm;
  CreateSQL;
end;

{-------------------------}
procedure TMSSQL.CreateSQL;
{-------------------------}
var
  StringList: TStringList;
begin
  if FQuery <> nil then
  begin
    StringList := TStringList.Create;
    AddSQLSelect (StringList);
    AddSQLFrom (StringList);
    AddSQLWhere (StringList);
    AddSQLOrder (StringList);
    Query.SQL.Clear;
    Query.SQL.AddStrings (StringList);
  end;
end;

{------------------------------------------------}
procedure TMSSQL.AddSQLSelect (Strings: TStrings);
{------------------------------------------------}
var
  FieldNames: TStringList;
  i: TMSSQLField;
  j: Integer;
begin
  Strings.Add ('SELECT');
  FieldNames := TStringList.Create;
  for i := Low (TMSSQLField) to High (TMSSQLField) do
  begin
    if (i in FSQLFields) then
      with FieldNames do
        case i of
          msf_MrgId:        Add (CTBSta + '.' + CFNMrgId);
          msf_MrgName:      Add ('"' + GetPathNameOfDatabase (FMrgSys) +
                                 CTBMrgDef + '"' + '.' + CFNMrgName);
          msf_Kennung:      Add (CTBSta + '.' + CFNKennung);
          msf_Stationsname: Add (CTBSta + '.' + CFNStationsName);
        end;
  end;
  for j := 0 to FieldNames.Count - 1 do
  begin
    if j > 0 then
      Strings.Add (', ' + FieldNames [j])
    else
      Strings.Add (FieldNames [j]);
  end;
  FieldNames.Free;
end;

{----------------------------------------------}
procedure TMSSQL.AddSQLFrom (Strings: TStrings);
{----------------------------------------------}
begin
  Strings.Add ('FROM');
  Strings.Add (CTbSta);
  if (msc_Auto in FSQLConditions) then
    Strings.Add (', ' + CTbStaAuto);
  if (msf_MrgName in FSQLFields) then
    Strings.Add (', "' + GetPathNameofDatabase (MrgSys) + CTbMrgDef + '"');
end;

{-----------------------------------------------}
procedure TMSSQL.AddSQLWhere (Strings: TStrings);
{-----------------------------------------------}
begin
  Strings.Add ('WHERE');
  Strings.Add (CTbSta + '.' + CFNAktiv + '= 0');
  if (msf_MrgName in FSQLFields) then
  begin
    Strings.Add ('AND');
    Strings.Add (CTBSta + '.' + CFNMrgTyp + '=' +
                 '"' + GetPathNameOfDatabase (FMrgSys) + CTbMrgDef +
                 '".' + CFNMrgTyp);
  end;
  if (msc_Auto in FSQLConditions) then
  begin
    Strings.Add ('AND');
    Strings.Add (CTBSta + '.' + CFNMrgId + '=' + CTBStaAuto + '.' + CFNMrgId);
  end;
end;

{-----------------------------------------------}
procedure TMSSQL.AddSQLOrder (Strings: TStrings);
{-----------------------------------------------}
begin
  Strings.Add ('ORDER BY');
  case FSQLOrder of
    mso_Kennung:      Strings.Add (CFNKennung);
    mso_StationsName: Strings.Add (CFNStationsName);
    mso_MrgName:      Strings.Add (CFNMrgName + ', ' + CFNKennung);
  end;
end;

{---------------------------------------------}
constructor TMSSQL.Create (AOwner: TComponent);
{---------------------------------------------}
begin
  inherited Create (AOwner);
  FMrgStamm := '';
  FMrgSys := '';
  FQuery := nil;
  FSQLFields := [msf_Kennung, msf_Stationsname];
  FSQLConditions := [];
  FSQLOrder := mso_Kennung;
end;

{--------------}
{ TMSItemBasic }
{--------------}

{ Setzt neuen Betriebsmodus. Datenfelder werden damit immer
  ungültig }
{----------------------------------------------------}
procedure TMSItemBasic.SetModus (Value: TMSItemModus);
{----------------------------------------------------}
begin
 { Clear; }
  if (Value = msm_Create) then
    Clear;
  FModus := Value;
end;

{ Überprüft Betriebsmodus und löscht Datenfelder }
{----------------------------------------------------------}
procedure TMSItemBasic.Read (Key: LongInt; Query: TMSQuery);
{----------------------------------------------------------}
begin
  if not (FModus in [msm_Read, msm_ReadWrite]) then
    raise EMSException.Create ('kein Lesezugriff erlaubt');
  Clear;
  Modified := False;
end;

{ Überprüft BetriebsModus, die Gültigkeit der Datenfelder und
  ob mindestens ein Datenfeld geändert worden ist }
{------------------------------------------------------------}
procedure TMSItemBasic.Modify (Key: LongInt; Query: TMSQuery);
{------------------------------------------------------------}
begin
  if not (FModus in [msm_ReadWrite]) then
    raise EMSException.Create ('kein Update erlaubt');
  if not Modified then
    raise EMSException.Create ('Daten wurden nicht geändert');
  if not Valid then
    raise EMSException.Create ('Daten ungültig');
end;

{ Überprüft BetriebsModus, die Gültigkeit der Datenfelder und
  ob mindestens ein Datenfeld geändert worden ist. Liefert als
  Ergebnis die StationsId wieder zurück }
{--------------------------------------------------------------------}
function TMSItemBasic.Append (Key: LongInt; Query: TMSQuery): LongInt;
{--------------------------------------------------------------------}
begin
  if not (FModus in [msm_Create, msm_ReadWrite]) then
    raise EMSException.Create ('kein Append erlaubt');
  if not Modified then
    raise EMSException.Create ('Daten wurden nicht geändert');
  if not Valid then
    raise EMSException.Create ('Daten ungültig');
  Result := Key;
end;

{ Überprüft Betriebsmodus und Gültigkeit der Daten }
{------------------------------------------------------------}
procedure TMSItemBasic.Delete (Key: LongInt; Query: TMSQuery);
{------------------------------------------------------------}
begin
  if not (FModus in [msm_ReadWrite]) then
    raise EMSException.Create ('kein Delete erlaubt');
  if not Valid then
    raise EMSException.Create ('Daten ungültig');
end;

{ Konstruktor: Voreingestellt ist reiner Lesezugriff }
{------------------------------}
constructor TMSItemBasic.Create;
{------------------------------}
begin
  inherited Create;
  FModus := msm_Read;
end;

{-------------}
{ TMSItemAllg }
{-------------}

{ Überprüft, ob Kennung schon in Tabelle vorhanden ist }
{------------------------------------------------------------------------}
procedure TMSItemAllg.CheckIndex (const Kennung: string; Query: TMSQuery);
{------------------------------------------------------------------------}
begin
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('SELECT');
    Add ('Sta.Kennung');
    Add ('FROM');
    Add ('Sta');
    Add ('WHERE');
    Add ('Sta.Kennung = :Kennung AND Sta.Aktiv = 0');
  end;
  with Query do
  begin
    ParamByName ('Kennung').asString := Kennung;
    Open;
    if RecordCount > 0 then
      raise EMSException.Create ('Kennung schon vorhanden !');
  end;
end;

{ Liefert True, wenn mindestens ein Datenfeld geändert wurde }
{----------------------------------------}
function TMSItemAllg.GetModified: Boolean;
{----------------------------------------}
begin
  Result := MrgTyp.Modified or
            Kennung.Modified or
            StationsName.Modified or
            TagesEnde.Modified or
            Prioritaet.Modified or
            isWZ_SZ.Modified;
end;

{ Setzt Änderungsstatus aller Datenfelder }
{-------------------------------------------------}
procedure TMSItemAllg.SetModified (Value: Boolean);
{-------------------------------------------------}
begin
  MrgTyp.Modified := Value;
  Kennung.Modified := Value;
  StationsName.Modified := Value;
  TagesEnde.Modified := Value;
  Prioritaet.Modified := Value;
  isWZ_SZ.Modified := Value;
end;

{ Liefert True, wenn alle Datenfelder gültige Werte besitzen }
{-------------------------------------}
function TMSItemAllg.GetValid: Boolean;
{-------------------------------------}
begin
  Result := not MrgTyp.Null and
            not Kennung.Null and
            not StationsName.Null and
            not TagesEnde.Null and
            not Prioritaet.Null;
end;

{ Löscht alle Datenfelder }
{--------------------------}
procedure TMSItemAllg.Clear;
{--------------------------}
begin
  FMrgTyp.Clear;
  FKennung.Clear;
  FStationsName.Clear;
  FTagesEnde.Clear;
  FPrioritaet.Clear;
  FisWZ_SZ.Clear;
end;

{ Liest allgemeine Stammdateninformation }
{---------------------------------------------------------}
procedure TMSItemAllg.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------}
begin
  inherited Read (Key, Query);
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('SELECT');
    Add ('Sta.MrgTyp, Sta.Kennung, Sta.StationsName, Sta.TagesEnde,');
    Add ('Sta.Prioritaet, Sta.WZ_SZ_Umstellung');
    Add ('FROM');
    Add ('Sta');
    Add ('WHERE');
    Add ('Sta.MrgId = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    Open;
    if RecordCount = 1 then
    begin
      FMrgTyp.asInteger := FieldByName (CFNMrgTyp).asInteger;
      FKennung.asString := FieldByName (CFNKennung).asString;
      FStationsName.asString := FieldByName (CFNStationsName).asString;
      FTagesEnde.asInteger := FieldByName (CFNTagesEnde).asInteger;

      if (FieldByName (CFNPrioritaet).asInteger < CPrioritaetMin) or
         (FieldByName (CFNPrioritaet).asInteger > CPrioritaetMax) then
        FPrioritaet.asInteger := 10
      else
        FPrioritaet.asInteger := FieldByName (CFNPrioritaet).asInteger;

      if (FieldByName (CFNisWz_SZ).isNull) then FisWZ_SZ.asBoolean := False
      else FisWZ_SZ.asBoolean := FieldByName (CFNisWz_SZ).asBoolean;
    end;
  end;
  Modified := False;
end;

{ Ändert allgemeine Stammdaten }
{-----------------------------------------------------------}
procedure TMSItemAllg.Modify (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Modify (Key, Query);
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('UPDATE');
    Add ('Sta');
    Add ('SET');
    Add ('StationsName = :StationsName, TagesEnde = :TagesEnde,');
    Add ('Prioritaet = :Prioritaet, WZ_SZ_Umstellung = :isWZSZ');
    Add ('WHERE');
    Add ('Sta.MrgId = :MrgId');
  end;
  Query.ParamByName ('StationsName').asString := FStationsName.asString;
  Query.ParamByName ('TagesEnde').asInteger := FTagesEnde.asInteger;
  Query.ParamByName ('Prioritaet').asSmallInt := FPrioritaet.asInteger;
  Query.ParamByName ('isWZSZ').asBoolean := FisWZ_SZ.asBoolean;
  Query.ParamByName ('MrgId').asInteger := Key;
  Query.ExecSQL;
  Modified := False;
end;

{ Erstellt neuen Datensatz und liest ihn wieder um MRGId zu erhalten }
{-------------------------------------------------------------------}
function TMSItemAllg.Append (Key: LongInt; Query: TMSQuery): LongInt;
{-------------------------------------------------------------------}
begin
  inherited Append (Key, Query);
  CheckIndex (FKennung.asString, Query);
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('INSERT INTO');
    Add ('Sta');
    Add ('(Kennung, Aktiv, MrgTyp, StationsName, TagesEnde, Prioritaet, WZ_SZ_Umstellung)');
    Add ('VALUES');
    Add ('(:Kennung, :Aktiv, :MrgTyp, :StationsName, :TagesEnde, :Prioritaet, :isWZSZ)');
  end;
  Query.ParamByName ('Aktiv').asInteger := 0;
  Query.ParamByName ('MrgTyp').asSmallInt := FMrgTyp.asInteger; { GeDa - asSmallInt }
  Query.ParamByName ('Kennung').asString := F_RightTrunc(FKennung.asString,' ');
  Query.ParamByName ('StationsName').asString := FStationsName.asString;
  Query.ParamByName ('TagesEnde').asInteger := FTagesEnde.asInteger;
  Query.ParamByName ('Prioritaet').asSmallInt := FPrioritaet.asInteger;
  Query.ParamByName ('isWZSZ').asBoolean := FisWZ_SZ.asBoolean;
  Query.ExecSQL;
  Modified := False;
  with Query.SQL do
  begin
    Clear;
    Add ('SELECT');
    Add ('Sta.MrgId');
    Add ('FROM');
    Add ('Sta');
    Add ('WHERE');
    Add ('Sta.Kennung = :Kennung AND Sta.Aktiv = 0');
  end;
  Query.ParamByName ('Kennung').asString := FKennung.asString;
  Query.Open;
  if Query.RecordCount = 1 then
    Result := Query.FieldByName ('MrgId').asInteger
  else
    raise EMSException.Create ('Table corrupted');
end;

{ Löscht Datensatz, in dem das Feld 'Aktiv' auf MrgId gesetzt wird }
{-----------------------------------------------------------}
procedure TMSItemAllg.Delete (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Delete (Key, Query);
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('UPDATE');
    Add ('Sta');
    Add ('SET');
    Add ('Aktiv = :MrgId');
    Add ('WHERE');
    Add ('MrgId = :MrgId');
  end;
  Query.ParamByName ('MrgId').asInteger := Key;
  Query.ExecSQL;
end;

{ Konstruktor, Legt Datenfelder an }
{-----------------------------}
constructor TMSItemAllg.Create;
{-----------------------------}
begin
  inherited Create;
  FMrgTyp := TIntegerData.CreateRange (CMrgTypMin, CMrgTypMax);
  FKennung := TXStringData.CreateRange (CKennungLen, ' ');
  FStationsName := TStringData.CreateRange (CStationsNameLen);
  FTagesEnde := TIntegerData.CreateRange (CTagesEndeMin, CTagesEndeMax);
  FPrioritaet := TIntegerData.CreateRange (CPrioritaetMin, CPrioritaetMax);
  FisWZ_SZ := TBooleanData.Create;
end;

{ Destruktor, Entfernt Datenfelder }
{-----------------------------}
destructor TMSItemAllg.Destroy;
{-----------------------------}
begin
  FMrgTyp.Free;
  FKennung.Free;
  FStationsName.Free;
  FTagesEnde.Free;
  FPrioritaet.Free;
  FisWZ_SZ.Free;
  inherited Destroy;
end;

{-------------}
{ TMSItemDSFG }
{-------------}

{ True, wenn DSFG-Daten vorhanden }
{---------------------------------------}
function TMSItemDSFG.GetHasData: Boolean;
{---------------------------------------}
begin
  Result := FHasData.asBoolean;
end;

{ Setzt FHasData }
{------------------------------------------------}
procedure TMSItemDSFG.SetHasData (Value: Boolean);
{------------------------------------------------}
begin
  FHasData.asBoolean := Value;
end;

{ Liefert DSFG-Gerätetyp }
{------------------------------------------}
function TMSItemDSFG.GetDSFGTyp: TMSDSFGTyp;
{------------------------------------------}
begin
  if not FHasData.asBoolean or FMasterId.Null then
    Result := DSFG_None
  else if (FMasterId.asInteger = 0) or
          (not FMrgId.Null and (FMasterId.asInteger = FMrgId.asInteger)) then
    Result := DSFG_Master
  else
    Result := DSFG_Slave;
end;

{----------------------------------------}
function TMSItemDSFG.GetModified: Boolean;
{----------------------------------------}
begin
  Result := FHasData.Modified or
            FAdresse.Modified or
            FMasterId.Modified;
end;

{-------------------------------------------------}
procedure TMSItemDSFG.SetModified (Value: Boolean);
{-------------------------------------------------}
begin
  FHasData.Modified := Value;
  FAdresse.Modified := Value;
  FMasterId.Modified := Value;
end;

{ GetValid: Datensatz ist gültig, wenn
            - Adresse und MasterId Werte enthalten, dh Slave oder Master
            - Weder Adresse noch MasterId einen gültigen Wert enthalten }
{-------------------------------------}
function TMSItemDSFG.GetValid: Boolean;
{-------------------------------------}
begin
  Result := not FHasData.asBoolean or
            (not FAdresse.Null and not FMasterId.Null);
end;

{--------------------------}
procedure TMSItemDSFG.Clear;
{--------------------------}
begin
  FHasData.asBoolean := False;
  FMrgId.Clear;
  FAdresse.Clear;
  FMasterId.Clear;
end;

{---------------------------------------------------------}
procedure TMSItemDSFG.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------}
begin
  inherited Read (Key, Query);
  FMrgId.asInteger := Key;
  Query.Close;
  With Query.SQL do
  begin
    Clear;
    Add ('SELECT');
    Add ('StaDsfg.MrgId, StaDsfg.Adresse, StaDsfg.MasterId');
    Add ('FROM');
    Add ('StaDsfg');
    Add ('WHERE');
    Add ('StaDsfg.MrgId = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    Open;
    if RecordCount = 1 then
    begin
      FHasData.asBoolean := True;
      FAdresse.asString := FieldByName ('Adresse').asString;
      FMasterId.asInteger := FieldByName ('MasterId').asInteger;
    end;
  end;
  Modified := False;
end;

{ Modify: DSFG-Daten werden geändert.
          Zunächst wird der Datensatz gelöscht, und dann neu hinzugefügt,
          falls er gültige DSFG-Daten enthält }
{-----------------------------------------------------------}
procedure TMSItemDSFG.Modify (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Modify (Key, Query);
  Delete (Key, Query);
  Append (Key, Query);
end;

{-------------------------------------------------------------------}
function TMSItemDSFG.Append (Key: LongInt; Query: TMSQuery): LongInt;
{-------------------------------------------------------------------}
begin
  Result := inherited Append (Key, Query);
  FMrgId.asInteger := Key;
  if FHasData.asBoolean then
  begin
    if FMasterId.asInteger = 0 then
      FMasterId.asInteger := FMrgId.asInteger;
    Query.Close;
    with Query.SQL do
    begin
      Clear;
      Add ('INSERT INTO');
      Add ('StaDsfg');
      Add ('(MrgId, Adresse, MasterId)');
      Add ('VALUES');
      Add ('(:MrgId, :Adresse, :MasterId)');
    end;
    with Query do
    begin
      ParamByName ('MrgId').asInteger := Key;
      ParamByName ('Adresse').asString := FAdresse.asString;
      ParamByName ('MasterId').asInteger := FMasterId.asInteger;
      ExecSQL;
    end;
    Modified := False;
  end;
end;

{-----------------------------------------------------------}
procedure TMSItemDSFG.Delete (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Delete (Key, Query);
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('DELETE FROM');
    Add ('StaDsfg');
    Add ('WHERE');
    Add ('MrgId = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    ExecSQL;
  end;
end;

{-----------------------------}
constructor TMSItemDSFG.Create;
{-----------------------------}
begin
  inherited Create;
  FMrgId := TIntegerData.CreateRange (CMrgIdMin, CMrgIdMax);
  FAdresse := TStringData.CreateRange (CAdresseLen);
  FMasterId := TIntegerData.CreateRange (CMrgIdMin, CMrgIdMax);
  FHasData := TBooleanData.Create;
  FHasData.asBoolean := False;
end;

{-----------------------------}
destructor TMSItemDSFG.Destroy;
{-----------------------------}
begin
  FMrgid.Free;
  FAdresse.Free;
  FMasterId.Free;
  FHasData.Free;
  inherited Destroy;
end;

{------------}
{ TMSItemDFU }
{------------}

{--------------------------------------}
function TMSItemDFU.GetHasData: Boolean;
{--------------------------------------}
begin
  Result := FHasData.asBoolean;
end;

{-----------------------------------------------}
procedure TMSItemDFU.SetHasData (Value: Boolean);
{-----------------------------------------------}
begin
  FHasData.asBoolean := Value;
end;

{--------------------------------------}
function TMSItemDFU.GetPasswort: string;
{--------------------------------------}
begin
  case FPasswortNr.asInteger of
    1: Result := FPasswort1.asString;
    2: Result := FPasswort2.asString;
    3: Result := FPasswort3.asString;
    4: Result := FPasswort4.asString;
    else raise EMSException.Create ('Passwort failed');
  end;
end;

{---------------------------------------}
function TMSItemDFU.GetModified: Boolean;
{---------------------------------------}
begin
  Result := FHasData.Modified or
            FRufnummer.Modified or
            FPasswort1.Modified or
            FPasswort2.Modified or
            FPasswort3.Modified or
            FPasswort4.Modified or
            FPasswortNr.Modified or
            FModemTyp.Modified or
            FLogPort.Modified;
end;

{------------------------------------------------}
procedure TMSItemDFU.SetModified (Value: Boolean);
{------------------------------------------------}
begin
  FHasData.Modified := Value;
  FRufnummer.Modified := Value;
  FPasswort1.Modified := Value;
  FPasswort2.Modified := Value;
  FPasswort3.Modified := Value;
  FPasswort4.Modified := Value;
  FPasswortNr.Modified := Value;
  FModemTyp.Modified := Value;
  FLogPort.Modified := Value;
end;

{------------------------------------}
function TMSItemDFU.GetValid: Boolean;
{------------------------------------}
begin
  if FHasData.asBoolean then
  begin
    Result := not FRufnummer.Null and
              not FPasswortNr.Null and
              not FModemTyp.Null and
              not FLogPort.Null;
    if Result then
      case FPasswortNr.asInteger of
        1: Result := not FPasswort1.Null;
        2: Result := not FPasswort2.Null;
        3: Result := not FPasswort3.Null;
        4: Result := not FPasswort4.Null;
        else Result := False;
      end;
  end
  else
    Result := True;
end;

{-------------------------}
procedure TMSItemDFU.Clear;
{-------------------------}
begin
  FHasData.AsBoolean := False;
  FRufnummer.Clear;
  FPasswort1.Clear;
  FPasswort2.Clear;
  FPasswort3.Clear;
  FPasswort4.Clear;
  FPasswortNr.Clear;
  FModemTyp.Clear;
  FLogPort.Clear;
end;

{--------------------------------------------------------}
procedure TMSItemDFU.Read (Key: LongInt; Query: TMSQuery);
{--------------------------------------------------------}
begin
  inherited Read (Key, Query);
  Query.Close;
  With Query.SQL do
  begin
    Clear;
    Add ('SELECT');
    Add ('StaDFU.Rufnummer, StaDFU.Passwort1, StaDFU.Passwort2, ' +
         'StaDFU.Passwort3, StaDFU.Passwort4, StaDFU.PasswortNr, ' +
         'StaDFU.ModemTyp, StaDFU.LogPort');
    Add ('FROM');
    Add ('StaDfu');
    Add ('WHERE');
    Add ('StaDfu.MrgId = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    Open;
    if RecordCount = 1 then
    begin
      FHasData.asBoolean := True;
      FRufnummer.asString := FieldByName ('Rufnummer').asString;
      FPasswort1.asString := FieldByName ('Passwort1').asString;
      FPasswort2.asString := FieldByName ('Passwort2').asString;
      FPasswort3.asString := FieldByName ('Passwort3').asString;
      FPasswort4.asString := FieldByName ('Passwort4').asString;
      FPasswortNr.asInteger := FieldByName ('PasswortNr').asInteger;
      FModemTyp.asInteger := FieldByName ('ModemTyp').asInteger;
      FLogPort.asInteger := FieldByName ('LogPort').asInteger;
    end;
  end;
  Modified := False;
end;                            

{----------------------------------------------------------}
procedure TMSItemDFU.Modify (Key: LongInt; Query: TMSQuery);
{----------------------------------------------------------}
begin
  inherited Modify (Key, Query);
  Delete (Key, Query);
  Append (Key, Query);
end;

{------------------------------------------------------------------}
function TMSItemDFU.Append (Key: LongInt; Query: TMSQuery): LongInt;
{------------------------------------------------------------------}
begin
  Result := inherited Append (Key, Query);
  if FHasData.asBoolean then
  begin
    Query.Close;
    with Query.SQL do
    begin
      Clear;
      Add ('INSERT INTO');
      Add ('StaDfu');
      Add ('(MrgId, Rufnummer, Passwort1, Passwort2, Passwort3, Passwort4, ' +
           'PasswortNr, ModemTyp, LogPort)');
      Add ('VALUES');
      Add ('(:MrgId, :Rufnummer, :Passwort1, :Passwort2, :Passwort3, ' +
           ':Passwort4, :PasswortNr, :ModemTyp, :LogPort)');
    end;
    with Query do
    begin
      ParamByName ('MrgId').asInteger := Key;
      ParamByName ('Rufnummer').asString := FRufnummer.asString;
      if FPasswort1.Null then
        ParamByName ('Passwort1').asString := ''
      else
        ParamByName ('Passwort1').asString := FPasswort1.asString;
      if FPasswort2.Null then
        ParamByName ('Passwort2').asString := ''
      else
        ParamByName ('Passwort2').asString := FPasswort2.asString;
      if FPasswort3.Null then
        ParamByName ('Passwort3').asString := ''
      else
        ParamByName ('Passwort3').asString := FPasswort3.asString;
      if FPasswort4.Null then
        ParamByName ('Passwort4').asString := ''
      else
        ParamByName ('Passwort4').asString := FPasswort4.asString;
      ParamByName ('PasswortNr').asSmallInt := FPasswortNr.asInteger; // GeDa
      ParamByName ('ModemTyp').asSmallInt := FModemTyp.asInteger;     // GeDa
      ParamByName ('LogPort').asSmallInt := FLogPort.asInteger;       // GeDa
      ExecSQL;
    end;
    Modified := False;
  end;
end;

{----------------------------------------------------------}
procedure TMSItemDFU.Delete (Key: LongInt; Query: TMSQuery);
{----------------------------------------------------------}
begin
  inherited Delete (Key, Query);
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('DELETE FROM');
    Add ('StaDfu');
    Add ('WHERE');
    Add ('MrgId = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    ExecSQL;
  end;
end;

{----------------------------}
constructor TMSItemDFU.Create;
{----------------------------}
begin
  inherited Create;
  FRufnummer := TStringData.CreateRange (CRufnummerLen);
  FPasswort1 := TStringData.CreateRange (CPasswortLen);
  FPasswort2 := TStringData.CreateRange (CPasswortLen);
  FPasswort3 := TStringData.CreateRange (CPasswortLen);
  FPasswort4 := TStringData.CreateRange (CPasswortLen);
  FPasswortNr := TIntegerData.CreateRange (CPasswortNrMin, CPasswortnrMax);
  FModemTyp := TIntegerData.CreateRange (CModemTypMin, CModemTypMax);
  FLogPort := TIntegerData.CreateRange (CLogPortMin, CLogPortMax);
  FHasData := TBooleanData.Create;
  FHasData.AsBoolean := False;
end;

{----------------------------}
destructor TMSItemDFU.Destroy;
{----------------------------}
begin
  FRufnummer.Free;
  FPasswort1.Free;
  FPasswort2.Free;
  FPasswort3.Free;
  FPasswort4.Free;
  FPasswortNr.Free;
  FModemTyp.Free;
  FLogPort.Free;
  FHasData.Free;
  inherited Destroy;
end;

{-------------}
{ TMSItemAuto }
{-------------}

{---------------------------------------}
function TMSItemAuto.GetHasData: Boolean;
{---------------------------------------}
begin
  Result := FHasData.asBoolean;
end;

{------------------------------------------------}
procedure TMSItemAuto.SetHasData (Value: Boolean);
{------------------------------------------------}
begin
  FHasData.asBoolean := Value;
end;

{----------------------------------------}
function TMSItemAuto.GetModified: Boolean;
{----------------------------------------}
begin
  Result := FHasData.Modified or
            FAutomatik.Modified or
            FIntervall.Modified or
            FZeitsynchronisation.Modified or
            FRundpufferreset.Modified;
end;

{-------------------------------------------------}
procedure TMSItemAuto.SetModified (Value: Boolean);
{-------------------------------------------------}
begin
  FHasData.Modified := Value;
  FAutomatik.Modified := Value;
  FIntervall.Modified := Value;
  FZeitsynchronisation.Modified := Value;
  FRundpufferreset.Modified := Value;
end;

{-------------------------------------}
function TMSItemAuto.GetValid: Boolean;
{-------------------------------------}
begin
  if FHasData.asBoolean then
  begin
    Result := not FAutomatik.Null and
              not FIntervall.Null and
              not FZeitsynchronisation.Null and
              not FRundpufferreset.Null;
  end
  else
    Result := True;
end;

{--------------------------}
procedure TMSItemAuto.Clear;
{--------------------------}
begin
  FHasData.AsBoolean := False;
  FAutomatik.Clear;
  FIntervall.Clear;
  FZeitsynchronisation.Clear;
  FRundpufferreset.Clear;
end;

{---------------------------------------------------------}
procedure TMSItemAuto.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------}
begin
  inherited Read (Key, Query);
  Query.Close;
  With Query.SQL do
  begin
    Clear;
    Add ('SELECT');
    Add ('StaAuto.Automatik, StaAuto.Intervall, ' +
         'StaAuto.Zeitsynchronisation, StaAuto.Rundpufferreset');
    Add ('FROM');
    Add ('StaAuto');
    Add ('WHERE');
    Add ('StaAuto.MrgId = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    Open;
    if RecordCount = 1 then
    begin
      FHasData.asBoolean := True;
      FAutomatik.asBoolean := FieldByName (CFNAutomatik).asBoolean;
      FIntervall.asInteger := FieldByName (CFNIntervall).asInteger;
      FZeitsynchronisation.asBoolean :=
        FieldByName (CFNZeitsynchronisation).asBoolean;
      FRundpufferreset.asBoolean :=
        FieldByName (CFNRundpufferreset).asBoolean;
    end;
  end;
  Modified := False;
end;

{-----------------------------------------------------------}
procedure TMSItemAuto.Modify (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Modify (Key, Query);
  Delete (Key, Query);
  Append (Key, Query);
end;

{-------------------------------------------------------------------}
function TMSItemAuto.Append (Key: LongInt; Query: TMSQuery): LongInt;
{-------------------------------------------------------------------}
begin
  Result := inherited Append (Key, Query);
  if FHasData.asBoolean then
  begin
    Query.Close;
    with Query.SQL do
    begin
      Clear;
      Add ('INSERT INTO');
      Add ('StaAuto');
      Add ('(MrgId, Automatik, Intervall, Zeitsynchronisation, ' +
           'Rundpufferreset)');
      Add ('VALUES');
      Add ('(:MrgId, :Automatik, :Intervall, :Zeitsynchronisation, :Rundpufferreset)');
    end;
    with Query do
    begin
      ParamByName ('MrgId').asInteger := Key;
      ParamByName ('Automatik').asBoolean := FAutomatik.asBoolean;
      ParamByName ('Intervall').asInteger := FIntervall.asInteger;
      ParamByName ('Zeitsynchronisation').asBoolean :=
        FZeitsynchronisation.asBoolean;
      ParamByName ('Rundpufferreset').asBoolean :=
        FRundpufferreset.asBoolean;
      ExecSQL;
    end;
    Modified := False;
  end;
end;

{-----------------------------------------------------------}
procedure TMSItemAuto.Delete (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Delete (Key, Query);
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('DELETE FROM');
    Add ('StaAuto');
    Add ('WHERE');
    Add ('MrgId = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    ExecSQL;
  end;
end;

{-----------------------------}
constructor TMSItemAuto.Create;
{-----------------------------}
begin
  inherited Create;
  FAutomatik := TBooleanData.Create;
  FIntervall := TIntegerData.CreateRange (CIntervallMin, CIntervallMax);
  FZeitsynchronisation := TBooleanData.Create;
  FRundpufferreset := TBooleanData.Create;
  FHasData := TBooleanData.Create;
  FHasData.AsBoolean := False;
end;

{-----------------------------}
destructor TMSItemAuto.Destroy;
{-----------------------------}
begin
  FAutomatik.Free;
  FIntervall.Free;
  FZeitsynchronisation.Free;
  FRundpufferreset.Free;
  FHasData.Free;
  inherited Destroy;
end;

{-------------}
{ TMSItemMDE }
{-------------}

{---------------------------------------}
function TMSItemMDE.GetHasData: Boolean;
{---------------------------------------}
begin
  Result := FHasData.asBoolean;
end;

{------------------------------------------------}
procedure TMSItemMDE.SetHasData (Value: Boolean);
{------------------------------------------------}
begin
  FHasData.asBoolean := Value;
end;

{----------------------------------------}
function TMSItemMDE.GetModified: Boolean;
{----------------------------------------}
begin
  Result := FHasData.Modified;
end;

{-------------------------------------------------}
procedure TMSItemMDE.SetModified (Value: Boolean);
{-------------------------------------------------}
begin
  FHasData.Modified := Value;
end;

{-------------------------------------}
function TMSItemMDE.GetValid: Boolean;
{-------------------------------------}
begin
  if FHasData.asBoolean then
  begin
    Result:=true;

    { hier (evtl. spätere ) Detaileinstellungen abprüfen }
    { ... }
  end
  else
    Result := True;
end;

{--------------------------}
procedure TMSItemMDE.Clear;
{--------------------------}
begin
  FHasData.AsBoolean := False;
end;

{---------------------------------------------------------}
procedure TMSItemMDE.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------}
begin
  inherited Read (Key, Query);
  Query.Close;
  With Query.SQL do
  begin
    Clear;
    Add ('SELECT');
    Add ('staMDE.mrgid');
    Add ('FROM');
    Add ('StaMDE');
    Add ('WHERE');
    Add ('StaMDE.MrgId = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    Open;
    if RecordCount = 1 then
    begin
      FHasData.asBoolean := True;
    end;
  end;
  Modified := False;
end;

{-----------------------------------------------------------}
procedure TMSItemMDE.Modify (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Modify (Key, Query);
  Delete (Key, Query);
  Append (Key, Query);
end;

{-------------------------------------------------------------------}
function TMSItemMDE.Append (Key: LongInt; Query: TMSQuery): LongInt;
{-------------------------------------------------------------------}
begin
  Result := inherited Append (Key, Query);
  if FHasData.asBoolean then
  begin
    Query.Close;
    with Query.SQL do
    begin
      Clear;
      Add ('INSERT INTO');
      Add ('StaMDE');
      Add ('(MrgId)');
      Add ('VALUES');
      Add ('(:MrgId)');
    end;
    with Query do
    begin
      ParamByName ('MrgId').asInteger := Key;
      ExecSQL;
    end;
    Modified := False;
  end;
end;

{-----------------------------------------------------------}
procedure TMSItemMDE.Delete (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Delete (Key, Query);
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('DELETE FROM');
    Add ('StaMDE');
    Add ('WHERE');
    Add ('MrgId = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    ExecSQL;
  end;
end;

{-----------------------------}
constructor TMSItemMDE.Create;
{-----------------------------}
begin
  inherited Create;
  FHasData := TBooleanData.Create;
  FHasData.AsBoolean := False;
end;

{-----------------------------}
destructor TMSItemMDE.Destroy;
{-----------------------------}
begin
  FHasData.Free;
  inherited Destroy;
end;


{-------------------}
{ TMSNewImpulsKanal }
{-------------------}

{----------------------------------------------}
function TMSNewImpulsKanal.GetModified: Boolean;
{----------------------------------------------}
begin
  Result := FFaktor.Modified or
            FTeiler.Modified or
            FOrgFaktor.Modified;
end;

{-------------------------------------------------------}
procedure TMSNewImpulsKanal.SetModified (Value: Boolean);
{-------------------------------------------------------}
begin
  FFaktor.Modified := Value;
  FTeiler.Modified := Value;
  FOrgFaktor.Modified := Value;
end;

{-------------------------------------------}
function TMSNewImpulsKanal.GetValid: Boolean;
{-------------------------------------------}
begin
  Result := not FFaktor.Null and
            not FTeiler.Null and
            not FOrgFaktor.Null;
end;

{--------------------------------}
procedure TMSNewImpulsKanal.Clear;
{--------------------------------}
begin
  FFaktor.Clear;
  FTeiler.Clear;
  FOrgFaktor.Clear;
end;

{---------------------------------------------------------------}
procedure TMSNewImpulsKanal.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------------}
begin
  inherited Read (Key, Query);
  with Query do
  begin
    FFaktor.asInteger := FieldByName ('Faktor').asInteger;
    FTeiler.asInteger := FieldByName ('Teiler').asInteger;
    FOrgFaktor.asFloat := FieldByName ('OrgFaktor').asFloat;
  end;
  Modified := False;
end;

{-----------------------------------------------------------------}
procedure TMSNewImpulsKanal.Modify (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------------}
var
  OldDecimalSeparator: Char;
begin
  inherited Modify (Key, Query);
  with Query do
  begin
    Close;
    OldDecimalSeparator := DecimalSeparator;
    try
      DecimalSeparator := '.';
      SQL.Clear;
      SQL.Add ('UPDATE StaImp');
      SQL.Add ('SET');
      SQL.Add ('Faktor = :Faktor, Teiler = :Teiler,');
      SQL.Add ('OrgFaktor = ' + FOrgFaktor.asString);
      SQL.Add ('WHERE');
      SQL.Add ('StaImp.KanalId = ' + IntToStr (Key));
      ParamByName ('Faktor').asInteger := FFaktor.asInteger;
      ParamByName ('Teiler').asInteger := FTeiler.asInteger;
      ExecSQL;
    finally
      DecimalSeparator := OldDecimalSeparator;
    end;
  end;
  Modified := False;
end;

{-----------------------------------}
constructor TMSNewImpulsKanal.Create;
{-----------------------------------}
begin
  inherited Create;
  FFaktor := TIntegerData.Create;
  FTeiler := TIntegerData.Create;
  FOrgFaktor := TNumericData.CreateRange (COrgFaktorMin, COrgFaktorMax);
end;

{-----------------------------------}
destructor TMSNewImpulsKanal.Destroy;
{-----------------------------------}
begin
  FFaktor.Free;
  FTeiler.Free;
  FOrgFaktor.Free;
  inherited Destroy;
end;

{-------------------}
{ TMSNewImpulsKanalPlus }
{-------------------}

{----------------------------------------------}
function TMSNewImpulsKanalPlus.GetModified: Boolean;
{----------------------------------------------}
begin
  Result := FFaktor.Modified or
            FTeiler.Modified or
            FOrgFaktor.Modified or
            FStdSumAvg.Modified;
end;

{-------------------------------------------------------}
procedure TMSNewImpulsKanalPlus.SetModified (Value: Boolean);
{-------------------------------------------------------}
begin
  FFaktor.Modified := Value;
  FTeiler.Modified := Value;
  FOrgFaktor.Modified := Value;
  FStdSumAvg.Modified:=Value;
end;

{-------------------------------------------}
function TMSNewImpulsKanalPlus.GetValid: Boolean;
{-------------------------------------------}
begin
  Result := not FFaktor.Null and
            not FTeiler.Null and
            not FOrgFaktor.Null and
            not FStdSumAvg.Null;
end;

{--------------------------------}
procedure TMSNewImpulsKanalPlus.Clear;
{--------------------------------}
begin
  FFaktor.Clear;
  FTeiler.Clear;
  FOrgFaktor.Clear;
  FStdSumAvg.Clear;
end;

{---------------------------------------------------------------}
procedure TMSNewImpulsKanalPlus.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------------}
begin
  inherited Read (Key, Query);
  with Query do
  begin
    FFaktor.asInteger := FieldByName ('Faktor').asInteger;
    FTeiler.asInteger := FieldByName ('Teiler').asInteger;
    FOrgFaktor.asFloat := FieldByName ('OrgFaktor').asFloat;
    FStdSumAvg.AsString:=FieldByName(CFNStdSumAvg).AsString;
  end;
  Modified := False;
end;

{-----------------------------------------------------------------}
procedure TMSNewImpulsKanalPlus.Modify (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------------}
var
  OldDecimalSeparator: Char;
begin
  inherited Modify (Key, Query);
  with Query do
  begin
    Close;
    OldDecimalSeparator := DecimalSeparator;
    try
      DecimalSeparator := '.';
      SQL.Clear;
      SQL.Add ('UPDATE StaImp');
      SQL.Add ('SET');
      SQL.Add ('Faktor = :Faktor, Teiler = :Teiler,');
      SQL.Add ('OrgFaktor = ' + FOrgFaktor.asString + ', ');
      SQL.Add('Stdsumavg = :StdSumAvg');
      SQL.Add ('WHERE');
      SQL.Add ('StaImp.KanalId = ' + IntToStr (Key));
      ParamByName ('Faktor').asInteger := FFaktor.asInteger;
      ParamByName ('Teiler').asInteger := FTeiler.asInteger;
      ParamByName('Stdsumavg').AsString:=FStdSumAvg.AsString;
      ExecSQL;
    finally
      DecimalSeparator := OldDecimalSeparator;
    end;
  end;
  Modified := False;
end;

{-----------------------------------}
constructor TMSNewImpulsKanalPlus.Create;
{-----------------------------------}
begin
  inherited Create;
  FFaktor := TIntegerData.Create;
  FTeiler := TIntegerData.Create;
  FOrgFaktor := TNumericData.CreateRange (COrgFaktorMin, COrgFaktorMax);
  FStdSumAvg:=TStringData.CreateRange(4);
end;

{-----------------------------------}
destructor TMSNewImpulsKanalPlus.Destroy;
{-----------------------------------}
begin
  FFaktor.Free;
  FTeiler.Free;
  FOrgFaktor.Free;
  FStdSumAvg.Free;
  inherited Destroy;
end;

{-------------------}
{ TMSNewAnalogKanal }
{-------------------}

{----------------------------------------------}
function TMSNewAnalogKanal.GetModified: Boolean;
{----------------------------------------------}
begin
  Result := FMessbereichMin.Modified or
            FMessbereichMax.Modified or
            FStrombereich.Modified or
            FOffset.Modified or
            FAufzMin.Modified or
            FAufzMax.Modified;
end;

{-------------------------------------------------------}
procedure TMSNewAnalogKanal.SetModified (Value: Boolean);
{-------------------------------------------------------}
begin
  FMessbereichMin.Modified := Value;
  FMessbereichMax.Modified := Value;
  FStrombereich.Modified := Value;
  FOffset.Modified := Value;
  FAufzMin.Modified := Value;
  FAufzMax.Modified := Value;
end;

{-------------------------------------------}
function TMSNewAnalogKanal.GetValid: Boolean;
{-------------------------------------------}
begin
  Result := not FMessbereichMin.Null and
            not FMessbereichMax.Null and
            not FStrombereich.Null and
            not FOffset.Null and
            not FAufzMin.Null and
            not FAufzMax.Null;
end;

{--------------------------------}
procedure TMSNewAnalogKanal.Clear;
{--------------------------------}
begin
  FMessBereichMin.Clear;
  FMessBereichMax.Clear;
  FStromBereich.Clear;
  FOffset.Clear;
  FAufzMin.Clear;
  FAufzMax.Clear;
end;

{---------------------------------------------------------------}
procedure TMSNewAnalogKanal.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------------}
begin
  inherited Read (Key, Query);
  with Query do
  begin
    FMessBereichMin.asFloat := FieldByName (CFNMessBereichMin).asFloat;
    FMessBereichMax.asFloat := FieldByName (CFNMessBereichMax).asFloat;
    FStromBereich.asInteger := FieldByName (CFNStromBereich).asInteger;
    FOffset.asBoolean := FieldByName (CFNOffset).asBoolean;
    FAufzMin.asInteger := FieldByName (CFNAufzMin).asInteger;
    FAufzMax.asInteger := FieldByName (CFNAufzMax).asInteger;
  end;
  Modified := False;
end;

{-----------------------------------------------------------------}
procedure TMSNewAnalogKanal.Modify (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------------}
var
  OldDecimalSeparator: Char;
begin
  inherited Modify (Key, Query);
  with Query do
  begin
    Close;
    OldDecimalSeparator := DecimalSeparator;
    SQL.Clear;
    try
      DecimalSeparator := '.';
      SQL.Add ('UPDATE StaAna');
      SQL.Add ('SET');
      SQL.Add ('MessBereichMin = ' + FMessBereichMin.asString + ',');
      SQL.Add ('MessbereichMax = ' + FMessBereichMax.asString + ',');
      SQL.Add ('StromBereich = :StromBereich, Offset = :Offset,');
      SQL.Add ('AufzMin = :AufzMin, AufzMax = :AufzMax');
      SQL.Add ('WHERE');
      SQL.Add ('StaAna.KanalId = ' + IntToStr (Key));
      ParamByName ('StromBereich').asSmallInt:= FStromBereich.asInteger; { GeDa }
      ParamByName ('Offset').asBoolean := FOffset.asBoolean;
      ParamByName ('AufzMin').asInteger := FAufzMin.asInteger;
      ParamByName ('AufzMax').asInteger := FAufzMax.asInteger;
      ExecSQL;
    finally
      DecimalSeparator := OldDecimalSeparator;
    end;
  end;
  Modified := False;
end;

{-----------------------------------}
constructor TMSNewAnalogKanal.Create;
{-----------------------------------}
begin
  inherited Create;
  FMessbereichMin := TNumericData.Create;
  FMessbereichMax := TNumericData.Create;
  FStrombereich := TIntegerData.CreateRange (CStrombereichMin,
                                             CStrombereichMax);
  FOffset := TBooleanData.Create;
  FAufzMin := TIntegerData.CreateRange (CAufzMinMin, CAufzMinMax);
  FAufzMax := TIntegerData.CreateRange (CAufzMaxMin, CAufzMaxMax);
end;

{-----------------------------------}
destructor TMSNewAnalogKanal.Destroy;
{-----------------------------------}
begin
  FMessbereichMin.Free;
  FMessbereichMax.Free;
  FStrombereich.Free;
  FOffset.Free;
  FAufzMin.Free;
  FAufzMax.Free;
  inherited Destroy;
end;

{-------------}
{ TMSNewKanal }
{-------------}

{---------------------------------------------------}
procedure TMSNewKanal.SetModus (Value: TMSItemModus);
{---------------------------------------------------}
begin
  inherited SetModus (Value);
  if GetHasImpuls then
    GetImpuls.Modus := Value;
  if GetHasAnalog then
    GetAnalog.Modus := Value;
end;

{--------------------------------------------}
function TMSNewKanal.GetKanalTyp: TMSKanalTyp;
{--------------------------------------------}
begin
  if CompareStr (FKanalTyp.asString, 'I') = 0 then
    Result := KTypImpuls
  else if CompareStr (FKanalTyp.asString, 'A') = 0 then
    Result := KTypAnalog
  else
    Result := KTypUnknown;
end;

{-----------------------------------------------------}
procedure TMSNewKanal.SetKanalTyp (Value: TMSKanalTyp);
{-----------------------------------------------------}
begin
  case Value of
    KTypUnknown: FKanalTyp.asString := 'N';
    KTypImpuls:  FKanalTyp.asString := 'I';
    KTypAnalog:  FKanalTyp.asString := 'A';
    else         FKanalTyp.asString := 'N';
  end;
end;

{------------------------------------------------}
function TMSNewKanal.GetImpuls: TMSNewImpulsKanal;
{------------------------------------------------}
begin
  if not assigned (FImpuls) then
    raise EMSException.Create ('Kanal nicht vorhanden');
  Result := FImpuls;
end;

{------------------------------------------------}
function TMSNewKanal.GetAnalog: TMSNewAnalogKanal;
{------------------------------------------------}
begin
  if not assigned (FAnalog) then
    raise EMSException.Create ('Kanal nicht vorhanden');
  Result := FAnalog;
end;

{-----------------------------------------}
function TMSNewKanal.GetHasImpuls: Boolean;
{-----------------------------------------}
begin
  Result := Assigned (FImpuls);
end;

{-----------------------------------------}
function TMSNewKanal.GetHasAnalog: Boolean;
{-----------------------------------------}
begin
  Result := Assigned (FAnalog);
end;

{----------------------------------------}
function TMSNewKanal.GetModified: Boolean;
{----------------------------------------}
begin
  Result := FKanalName.Modified or
            FAktiv.Modified or
            FKanaltyp.Modified or
            FKommastellen.Modified or
            FEinheit.Modified;
  if not Result and (FKanaltyp.asString = 'I') then
    Result := GetImpuls.Modified;
  if not Result and (FKanalTyp.asString = 'A') then
    Result := GetAnalog.Modified;
end;

{-------------------------------------------------}
procedure TMSNewKanal.SetModified (Value: Boolean);
{-------------------------------------------------}
begin
  FKanalName.Modified := Value;
  FAktiv.Modified := Value;
  FKanaltyp.Modified := Value;
  FKommastellen.Modified := Value;
  FEinheit.Modified := Value;
  if Assigned (FImpuls) then
    FImpuls.SetModified (Value);
  if Assigned (FAnalog) then
    FAnalog.SetModified (Value);
end;

{-------------------------------------}
function TMSNewKanal.GetValid: Boolean;
{-------------------------------------}
begin
  Result := not FMrgKanal.Null and
            not FKanalName.Null and
            not FAktiv.Null and
            not FEinstellbar.Null and
            not FKanaltyp.Null and
            not FKontroll.Null and
            not FEingang.Null and
            not FKommastellen.Null and
            not FEinheit.Null;
  if Result and (FKanaltyp.asString = 'I') and Assigned (FImpuls) then
    Result := FImpuls.Valid;
  if Result and (FKanaltyp.asString = 'A') and Assigned (FAnalog) then
    Result := FAnalog.Valid;
end;

{--------------------------}
procedure TMSNewKanal.Clear;
{--------------------------}
begin
  FMrgKanal.Clear;
  FKanalId.Clear;
  FAktiv.Clear;
  FKanalName.Clear;
  FKanalTyp.Clear;
  FEinstellbar.Clear;
  FKontroll.Clear;
  FEingang.Clear;
  FKommastellen.Clear;
  FEinheit.Clear;
  FImpuls.Free;
  FImpuls := nil;
  FAnalog.Free;
  FAnalog := nil;
end;

{---------------------------------------------------------}
procedure TMSNewKanal.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------}
begin
  inherited Read (Key, Query);
  with Query do
  begin
    FMrgKanal.asInteger := FieldByName (CFNMrgKanal).asInteger;
    FKanalId.asInteger := FieldByName (CFNKanalId).asInteger;
    FAktiv.asBoolean := FieldByName (CFNAktiv).asBoolean;
    FKanalName.asString := FieldByName (CFNKanalName).asString;
    FKanalTyp.asString := FieldByName (CFNKanalTyp).asString;
    FEinstellbar.asBoolean := FieldByName (CFNEinstellbar).asBoolean;
    FKontroll.asBoolean := FieldByName (CFNKontroll).asBoolean;
    FEingang.asBoolean := FieldByName (CFNEingang).asBoolean;
    FKommastellen.asInteger := FieldByName (CFNKommastellen).asInteger;
    FEinheit.asString := FieldByName (CFNEinheit).asString;
    if not FieldByName (CFNImpulsKanalId).isNull then
    begin
      FImpuls := TMSNewImpulsKanal.Create;
      FImpuls.Modus := FModus;
      FImpuls.Read (FKanalId.asInteger, Query);
    end;
    if not FieldByName (CFNAnalogKanalId).isNull then
    begin
      FAnalog := TMSNewAnalogKanal.Create;
      FAnalog.Modus := FModus;
      FAnalog.Read (FKanalId.asInteger, Query);
    end;
  end;
  Modified := False;
end;

{-----------------------------------------------------------}
procedure TMSNewKanal.Modify (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Modify (Key, Query);
  if GetHasImpuls then
    if FImpuls.Modified then
      FImpuls.Modify (FKanalId.asInteger, Query);
  if GetHasAnalog then
    if FAnalog.Modified then
      FAnalog.Modify (FKanalId.asInteger, Query);
  if Modified then
  begin
    with Query do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('UPDATE StaKanal');
      SQL.Add ('SET');
      SQL.Add ('Aktiv = :Aktiv, KanalName = :KanalName,');
      SQL.Add ('KanalTyp = :KanalTyp,');
      SQL.Add ('Kommastellen = :Kommastellen, Einheit = :Einheit');
      SQL.Add ('WHERE');
      SQL.Add ('StaKanal.MrgId = ' + IntToStr (Key));
      SQL.Add ('AND');
      SQL.Add ('StaKanal.MRGKanal = ' + FMRGKanal.asString);
      ParamByName ('Aktiv').asBoolean := FAktiv.asBoolean;
      ParamByName ('KanalName').asString := FKanalName.asString;
      ParamByName ('KanalTyp').asString := FKanalTyp.asString;
      ParamByName ('Kommastellen').asSmallInt := FKommastellen.asInteger; // GeDa
      ParamByName ('Einheit').asString := FEinheit.asString;
      ExecSQL;
    end;
    Modified := False;
  end;
end;

{-----------------------------}
constructor TMSNewKanal.Create;
{-----------------------------}
begin
  inherited Create;
  FMrgKanal := TIntegerData.CreateRange (CMrgKanalMin, CMrgKanalMax);
  FKanalId := TIntegerData.Create;
  FKanalName := TStringData.CreateRange (CKanalNameLen);
  FAktiv := TBooleanData.Create;
  FEinstellbar := TBooleanData.Create;
  FKanaltyp := TStringData.CreateRange (1);
  FKontroll := TBooleanData.Create;
  FEingang := TBooleanData.Create;
  FKommastellen := TIntegerData.CreateRange (CKommastellenMin,
                                             CKommastellenMax);
  FEinheit := TStringData.CreateRange (CEinheitLen);
  FImpuls := nil;
  FAnalog := nil;
end;

{-----------------------------}
destructor TMSNewKanal.Destroy;
{-----------------------------}
begin
  FMrgKanal.Free;
  FKanalName.Free;
  FAktiv.Free;
  FEinstellbar.Free;
  FKanalTyp.Free;
  FKontroll.Free;
  FEingang.Free;
  FKommastellen.Free;
  FEinheit.Free;
  FImpuls.Free;
  FAnalog.Free;
  inherited Destroy;
end;

{-----------------}
{ TMSNewKanalPlus }
{-----------------}

{---------------------------------------------------}
procedure TMSNewKanalPlus.SetModus (Value: TMSItemModus);
{---------------------------------------------------}
begin
  inherited SetModus (Value);
  if GetHasImpuls then
    GetImpuls.Modus := Value;
  if GetHasAnalog then
    GetAnalog.Modus := Value;
end;

{--------------------------------------------}
function TMSNewKanalPlus.GetKanalTyp: TMSKanalTyp;
{--------------------------------------------}
begin
  if CompareStr (FKanalTyp.asString, 'I') = 0 then
    Result := KTypImpuls
  else if CompareStr (FKanalTyp.asString, 'A') = 0 then
    Result := KTypAnalog
  else
    Result := KTypUnknown;
end;

{-----------------------------------------------------}
procedure TMSNewKanalPlus.SetKanalTyp (Value: TMSKanalTyp);
{-----------------------------------------------------}
begin
  case Value of
    KTypUnknown: FKanalTyp.asString := 'N';
    KTypImpuls:  FKanalTyp.asString := 'I';
    KTypAnalog:  FKanalTyp.asString := 'A';
    else         FKanalTyp.asString := 'N';
  end;
end;

{------------------------------------------------}
function TMSNewKanalPlus.GetImpuls: TMSNewImpulsKanalPlus;
{------------------------------------------------}
begin
  if not assigned (FImpuls) then
    raise EMSException.Create ('Kanal nicht vorhanden');
  Result := FImpuls;
end;

{------------------------------------------------}
function TMSNewKanalPlus.GetAnalog: TMSNewAnalogKanal;
{------------------------------------------------}
begin
  if not assigned (FAnalog) then
    raise EMSException.Create ('Kanal nicht vorhanden');
  Result := FAnalog;
end;

{-----------------------------------------}
function TMSNewKanalPlus.GetHasImpuls: Boolean;
{-----------------------------------------}
begin
  Result := Assigned (FImpuls);
end;

{-----------------------------------------}
function TMSNewKanalPlus.GetHasAnalog: Boolean;
{-----------------------------------------}
begin
  Result := Assigned (FAnalog);
end;

{----------------------------------------}
function TMSNewKanalPlus.GetModified: Boolean;
{----------------------------------------}
begin
  Result := FKanalName.Modified or
            FAktiv.Modified or
            FKanaltyp.Modified or
            FKommastellen.Modified or
            FEinheit.Modified;
  if not Result and (FKanaltyp.asString = 'I') then
    Result := GetImpuls.Modified;
  if not Result and (FKanalTyp.asString = 'A') then
    Result := GetAnalog.Modified;
end;

{-------------------------------------------------}
procedure TMSNewKanalPlus.SetModified (Value: Boolean);
{-------------------------------------------------}
begin
  FKanalName.Modified := Value;
  FAktiv.Modified := Value;
  FKanaltyp.Modified := Value;
  FKommastellen.Modified := Value;
  FEinheit.Modified := Value;
  if Assigned (FImpuls) then
    FImpuls.SetModified (Value);
  if Assigned (FAnalog) then
    FAnalog.SetModified (Value);
end;

{-------------------------------------}
function TMSNewKanalPlus.GetValid: Boolean;
{-------------------------------------}
begin
  Result := not FMrgKanal.Null and
            not FKanalName.Null and
            not FAktiv.Null and
            not FEinstellbar.Null and
            not FKanaltyp.Null and
            not FKontroll.Null and
            not FEingang.Null and
            not FKommastellen.Null and
            not FEinheit.Null;
  if Result and (FKanaltyp.asString = 'I') and Assigned (FImpuls) then
    Result := FImpuls.Valid;
  if Result and (FKanaltyp.asString = 'A') and Assigned (FAnalog) then
    Result := FAnalog.Valid;
end;

{--------------------------}
procedure TMSNewKanalPlus.Clear;
{--------------------------}
begin
  FMrgKanal.Clear;
  FKanalId.Clear;
  FAktiv.Clear;
  FKanalName.Clear;
  FKanalTyp.Clear;
  FEinstellbar.Clear;
  FKontroll.Clear;
  FEingang.Clear;
  FKommastellen.Clear;
  FEinheit.Clear;
  FImpuls.Free;
  FImpuls := nil;
  FAnalog.Free;
  FAnalog := nil;
end;

{---------------------------------------------------------}
procedure TMSNewKanalPlus.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------}
begin
  inherited Read (Key, Query);
  with Query do
  begin
    FMrgKanal.asInteger := FieldByName (CFNMrgKanal).asInteger;
    FKanalId.asInteger := FieldByName (CFNKanalId).asInteger;
    FAktiv.asBoolean := FieldByName (CFNAktiv).asBoolean;
    FKanalName.asString := FieldByName (CFNKanalName).asString;
    FKanalTyp.asString := FieldByName (CFNKanalTyp).asString;
    FEinstellbar.asBoolean := FieldByName (CFNEinstellbar).asBoolean;
    FKontroll.asBoolean := FieldByName (CFNKontroll).asBoolean;
    FEingang.asBoolean := FieldByName (CFNEingang).asBoolean;
    FKommastellen.asInteger := FieldByName (CFNKommastellen).asInteger;
    FEinheit.asString := FieldByName (CFNEinheit).asString;
    if not FieldByName (CFNImpulsKanalId).isNull then
    begin
      FImpuls := TMSNewImpulsKanalPlus.Create;
      FImpuls.Modus := FModus;
      FImpuls.Read (FKanalId.asInteger, Query);
    end;
    if not FieldByName (CFNAnalogKanalId).isNull then
    begin
      FAnalog := TMSNewAnalogKanal.Create;
      FAnalog.Modus := FModus;
      FAnalog.Read (FKanalId.asInteger, Query);
    end;
  end;
  Modified := False;
end;

{-----------------------------------------------------------}
procedure TMSNewKanalPlus.Modify (Key: LongInt; Query: TMSQuery);
{-----------------------------------------------------------}
begin
  inherited Modify (Key, Query);
  if GetHasImpuls then
    if FImpuls.Modified then
      FImpuls.Modify (FKanalId.asInteger, Query);
  if GetHasAnalog then
    if FAnalog.Modified then
      FAnalog.Modify (FKanalId.asInteger, Query);
  if Modified then
  begin
    with Query do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('UPDATE StaKanal');
      SQL.Add ('SET');
      SQL.Add ('Aktiv = :Aktiv, KanalName = :KanalName,');
      SQL.Add ('KanalTyp = :KanalTyp,');
      SQL.Add ('Kommastellen = :Kommastellen, Einheit = :Einheit');
      SQL.Add ('WHERE');
      SQL.Add ('StaKanal.MrgId = ' + IntToStr (Key));
      SQL.Add ('AND');
      SQL.Add ('StaKanal.MRGKanal = ' + FMRGKanal.asString);
      ParamByName ('Aktiv').asBoolean := FAktiv.asBoolean;
      ParamByName ('KanalName').asString := FKanalName.asString;
      ParamByName ('KanalTyp').asString := FKanalTyp.asString;
      ParamByName ('Kommastellen').asInteger := FKommastellen.asInteger;
      ParamByName ('Einheit').asString := FEinheit.asString;
      ExecSQL;
    end;
    Modified := False;
  end;
end;

{-----------------------------}
constructor TMSNewKanalPlus.Create;
{-----------------------------}
begin
  inherited Create;
  FMrgKanal := TIntegerData.CreateRange (CMrgKanalMin, CMrgKanalMax);
  FKanalId := TIntegerData.Create;
  FKanalName := TStringData.CreateRange (CKanalNameLen);
  FAktiv := TBooleanData.Create;
  FEinstellbar := TBooleanData.Create;
  FKanaltyp := TStringData.CreateRange (1);
  FKontroll := TBooleanData.Create;
  FEingang := TBooleanData.Create;
  FKommastellen := TIntegerData.CreateRange (CKommastellenMin,
                                             CKommastellenMax);
  FEinheit := TStringData.CreateRange (CEinheitLen);
  FImpuls := nil;
  FAnalog := nil;
end;

{-----------------------------}
destructor TMSNewKanalPlus.Destroy;
{-----------------------------}
begin
  FMrgKanal.Free;
  FKanalName.Free;
  FAktiv.Free;
  FEinstellbar.Free;
  FKanalTyp.Free;
  FKontroll.Free;
  FEingang.Free;
  FKommastellen.Free;
  FEinheit.Free;
  FImpuls.Free;
  FAnalog.Free;
  inherited Destroy;
end;

{-----------------}
{ TMSNewKanalList }
{-----------------}

{-------------------------------------------------------}
procedure TMSNewKanalList.SetModus (Value: TMSItemModus);
{-------------------------------------------------------}
var
  i: Integer;
begin
  inherited SetModus (Value);
  for i := 0 to GetCount - 1 do
    GetKanal (i).Modus := Value;
end;

{--------------------------------------------------------------}
function TMSNewKanalList.GetKanal (Index: Integer): TMSNewKanal;
{--------------------------------------------------------------}
begin
  CheckIndex (Index);
  result := FKanal.Items [Index];
end;

{--------------------------------------------}
function TMSNewKanalList.GetModified: Boolean;
{--------------------------------------------}
var
  i: Integer;
begin
  Result := False;
  for i := 0 to GetCount - 1 do
    if GetKanal (i).Modified then
    begin
      Result := True;
      break;
    end;
end;

{-----------------------------------------------------}
procedure TMSNewKanalList.SetModified (Value: Boolean);
{-----------------------------------------------------}
var
  i: Integer;
begin
  for i := 0 to GetCount - 1 do
    GetKanal (i).Modified := Value;
end;

{-----------------------------------------}
function TMSNewKanalList.GetValid: Boolean;
{-----------------------------------------}
var
  i: Integer;
begin
  Result := True;
  for i := 0 to GetCount - 1 do
    if not GetKanal (i).Valid then
    begin
      Result := False;
      break;
    end;
end;

{------------------------------}
procedure TMSNewKanalList.Clear;
{------------------------------}
var
  i: Integer;
begin
  for i := 0 to FKanal.Count - 1 do
    TMSNewKanal (FKanal.Items [i]).Free;
  FKanal.Clear;
end;

{-----------------------------------------}
function TMSNewKanalList.GetCount: Integer;
{-----------------------------------------}
begin
  result := FKanal.Count;
end;

{----------------------------------------------------}
procedure TMSNewKanalList.CheckIndex (Index: Integer);
{----------------------------------------------------}
begin
  if (Index < 0) or (Index >= FKanal.Count) then
    raise EMSException.Create ('Index Error');
end;
(*
{-----------------------------------------------------------------}
function TMSNewKanalList.GetMrgKanal (Index: Integer): TMSNewKanal;
{-----------------------------------------------------------------}
var
  i: Integer;
  MSKanal: TMSNewKanal;
begin
  Result := nil;
  for i := 0 to GetCount - 1 do
  begin
    MSKanal := GetKanal (i);
    if MSKanal.FMrgKanal.asInteger = Index then
    begin
      Result := MSKanal;
      break;
    end;
  end;
end;   *)

{-------------------------------------------------------------}
procedure TMSNewKanalList.Read (Key: LongInt; Query: TMSQuery);
{-------------------------------------------------------------}
var
  MSKanal: TMSNewKanal;                                                     
begin
  inherited Read (Key, Query);
  with Query do
  begin
    Close;
    SQL.Clear;
    SQL.Add ('SELECT');
    SQL.Add ('StaKanal.MrgKanal, StaKanal.KanalId, StaKanal.Aktiv,');
    SQL.Add ('StaKanal.KanalName,');
    SQL.Add ('StaKanal.KanalTyp, StaKanal.Einstellbar,');
    SQL.Add ('StaKanal.Kontroll, StaKanal.Eingang,');
    SQL.Add ('StaKanal.Kommastellen, StaKanal.Einheit,');
    SQL.Add ('StaImp.KanalId ImpulsKanalId, StaImp.Faktor, StaImp.Teiler,');
    SQL.Add ('StaImp.OrgFaktor,');
    SQL.Add ('StaAna.KanalId AnalogKanalId,');
    SQL.Add ('StaAna.MessBereichMin, StaAna.MessBereichMax,');
    SQL.Add ('StaAna.Strombereich, StaAna.Offset,');
    SQL.Add ('StaAna.AufzMin, StaAna.AufzMax');
    SQL.Add ('FROM');
    SQL.Add ('StaKanal LEFT JOIN StaImp ON StaKanal.KanalId = StaImp.KanalId');
    SQL.Add ('         LEFT JOIN StaAna ON StaKanal.KanalId = StaAna.KanalId');
    SQL.Add ('WHERE');
    SQL.Add ('StaKanal.MrgID = ' + IntToStr (Key));
    Open;
    First;
    while not Eof do
    begin
      MSKanal := TMSNewKanal.Create;
      MSKanal.Modus := FModus;
      MSKanal.Read (Key, Query);
      FKanal.Add (MSKanal);
      Next;
    end;
  end;
  Modified := False;
end;

{---------------------------------------------------------------}
procedure TMSNewKanalList.Modify (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------------}
var
  i: Integer;
  MSKanal: TMSNewKanal;
begin
  inherited Modify (Key, Query);
  for i := 0 to GetCount - 1 do
  begin
    MSKanal := FKanal [i];
    if MSKanal.Modified then
      MSKanal.Modify (Key, Query);
  end;
  Modified := False;
end;

{---------------------------------}
constructor TMSNewKanalList.Create;
{---------------------------------}
begin
  inherited Create;
  FKanal := TList.Create;
end;

{---------------------------------}
destructor TMSNewKanalList.Destroy;
{---------------------------------}
begin
  Clear;
  FKanal.Free;
  inherited Destroy;
end;

{---------------------}
{ TMSNewKanalListPlus }
{---------------------}

{-------------------------------------------------------}
procedure TMSNewKanalListPlus.SetModus (Value: TMSItemModus);
{-------------------------------------------------------}
var
  i: Integer;
begin
  inherited SetModus (Value);
  for i := 0 to GetCount - 1 do
    GetKanal (i).Modus := Value;
end;

{--------------------------------------------------------------}
function TMSNewKanalListPlus.GetKanal (Index: Integer): TMSNewKanalPlus;
{--------------------------------------------------------------}
begin
  CheckIndex (Index);
  result := FKanal.Items [Index];
end;

{--------------------------------------------}
function TMSNewKanalListPlus.GetModified: Boolean;
{--------------------------------------------}
var
  i: Integer;
begin
  Result := False;
  for i := 0 to GetCount - 1 do
    if GetKanal (i).Modified then
    begin
      Result := True;
      break;
    end;
end;

{-----------------------------------------------------}
procedure TMSNewKanalListPlus.SetModified (Value: Boolean);
{-----------------------------------------------------}
var
  i: Integer;
begin
  for i := 0 to GetCount - 1 do
    GetKanal (i).Modified := Value;
end;

{-----------------------------------------}
function TMSNewKanalListPlus.GetValid: Boolean;
{-----------------------------------------}
var
  i: Integer;
begin
  Result := True;
  for i := 0 to GetCount - 1 do
    if not GetKanal (i).Valid then
    begin
      Result := False;
      break;
    end;
end;

{------------------------------}
procedure TMSNewKanalListPlus.Clear;
{------------------------------}
var
  i: Integer;
begin
  for i := 0 to FKanal.Count - 1 do
    TMSNewKanalPlus (FKanal.Items [i]).Free;
  FKanal.Clear;
end;

{-----------------------------------------}
function TMSNewKanalListPlus.GetCount: Integer;
{-----------------------------------------}
begin
  result := FKanal.Count;
end;

{----------------------------------------------------}
procedure TMSNewKanalListPlus.CheckIndex (Index: Integer);
{----------------------------------------------------}
begin
  if (Index < 0) or (Index >= FKanal.Count) then
    raise EMSException.Create ('Index Error');
end;
(*
{-----------------------------------------------------------------}
function TMSNewKanalListPlus.GetMrgKanal (Index: Integer): TMSNewKanalPlus;
{-----------------------------------------------------------------}
var
  i: Integer;
  MSKanal: TMSNewKanalPlus;
begin
  Result := nil;
  for i := 0 to GetCount - 1 do
  begin
    MSKanal := GetKanal (i);
    if MSKanal.FMrgKanal.asInteger = Index then
    begin
      Result := MSKanal;
      break;
    end;
  end;
end; *)

{-------------------------------------------------------------}
procedure TMSNewKanalListPlus.Read (Key: LongInt; Query: TMSQuery);
{-------------------------------------------------------------}
var
  MSKanal: TMSNewKanalPlus;
begin
  inherited Read (Key, Query);
  with Query do
  begin
    Close;
    SQL.Clear;
    SQL.Add ('SELECT');
    SQL.Add ('StaKanal.MrgKanal, StaKanal.KanalId, StaKanal.Aktiv,');
    SQL.Add ('StaKanal.KanalName,');
    SQL.Add ('StaKanal.KanalTyp, StaKanal.Einstellbar,');
    SQL.Add ('StaKanal.Kontroll, StaKanal.Eingang,');
    SQL.Add ('StaKanal.Kommastellen, StaKanal.Einheit,');
    SQL.Add ('StaImp.KanalId ImpulsKanalId, StaImp.Faktor, StaImp.Teiler,');
    SQL.Add ('StaImp.OrgFaktor, StaImp.Stdsumavg,');
    SQL.Add ('StaAna.KanalId AnalogKanalId,');
    SQL.Add ('StaAna.MessBereichMin, StaAna.MessBereichMax,');
    SQL.Add ('StaAna.Strombereich, StaAna.Offset,');
    SQL.Add ('StaAna.AufzMin, StaAna.AufzMax');
    SQL.Add ('FROM');
    SQL.Add ('StaKanal LEFT JOIN StaImp ON StaKanal.KanalId = StaImp.KanalId');
    SQL.Add ('         LEFT JOIN StaAna ON StaKanal.KanalId = StaAna.KanalId');
    SQL.Add ('WHERE');
    SQL.Add ('StaKanal.MrgID = ' + IntToStr (Key));
    Open;
    First;
    while not Eof do
    begin
      MSKanal := TMSNewKanalPlus.Create;
      MSKanal.Modus := FModus;
      MSKanal.Read (Key, Query);
      FKanal.Add (MSKanal);
      Next;
    end;
  end;
  Modified := False;
end;

{---------------------------------------------------------------}
procedure TMSNewKanalListPlus.Modify (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------------}
var
  i: Integer;
  MSKanal: TMSNewKanalPlus;
begin
  inherited Modify (Key, Query);
  for i := 0 to GetCount - 1 do
  begin
    MSKanal := FKanal [i];
    if MSKanal.Modified then
      MSKanal.Modify (Key, Query);
  end;
  Modified := False;
end;

{---------------------------------}
constructor TMSNewKanalListPlus.Create;
{---------------------------------}
begin
  inherited Create;
  FKanal := TList.Create;
end;

{---------------------------------}
destructor TMSNewKanalListPlus.Destroy;
{---------------------------------}
begin
  Clear;
  FKanal.Free;
  inherited Destroy;
end;

{-------------}
{ TMSItemInfo }
{-------------}

{----------------------------------------}
function TMSItemInfo.GetModified: Boolean;
{----------------------------------------}
begin
  Result := MrgName.Modified;
end;

{-------------------------------------------------}
procedure TMSItemInfo.SetModified (Value: Boolean);
{-------------------------------------------------}
begin
  MrgName.Modified := Value;
end;

{-------------------------------------}
function TMSItemInfo.GetValid: Boolean;
{-------------------------------------}
begin
  Result := not MrgName.Null;
end;

{--------------------------}
procedure TMSItemInfo.Clear;
{--------------------------}
begin
  FMrgName.Clear;
end;

{---------------------------------------------------------}
procedure TMSItemInfo.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------}
begin
  inherited Read (Key, Query);
  with Query do
  begin
    Close;
    SQL.Clear;
    SQL.Add ('SELECT');
    SQL.Add ('MrgDef.MrgName');
    SQL.Add ('FROM');
    SQL.Add ('MrgDef');
    SQL.Add ('WHERE');
    SQL.Add ('MrgDef.MrgTyp = ' + IntToStr (Key));
    Open;
    if RecordCount = 1 then
    begin
      FMrgName.asString := FieldByName (CFNMrgName).asString;
    end;
  end;
  Modified := False;
end;

{-----------------------------}
constructor TMSItemInfo.Create;
{-----------------------------}
begin
  inherited Create;
  FMrgName := TStringData.CreateRange (CMRGNameLen);
end;

{-----------------------------}
destructor TMSItemInfo.Destroy;
{-----------------------------}
begin
  FMrgName.Free;
  inherited Destroy;
end;

{-------------}
{ TMSItemKonv }
{-------------}

{----------------------------------------}
function TMSItemKonv.GetModified: Boolean;
{----------------------------------------}
begin
  Result := Meldungsgruppe.Modified;
end;

{-------------------------------------------------}
procedure TMSItemKonv.SetModified (Value: Boolean);
{-------------------------------------------------}
begin
  Meldungsgruppe.Modified := Value;
end;

{-------------------------------------}
function TMSItemKonv.GetValid: Boolean;
{-------------------------------------}
begin
  Result := not Meldungsgruppe.Null;
end;

{--------------------------}
procedure TMSItemKonv.Clear;
{--------------------------}
begin
  FMeldungsgruppe.Clear;
end;

{---------------------------------------------------------}
procedure TMSItemKonv.Read (Key: LongInt; Query: TMSQuery);
{---------------------------------------------------------}
begin
  inherited Read (Key, Query);
  with Query do
  begin
    Close;
    SQL.Clear;
    SQL.Add ('SELECT');
    SQL.Add ('MrgKonv.Meldungsgruppe');
    SQL.Add ('FROM');
    SQL.Add ('MrgKonv');
    SQL.Add ('WHERE');
    SQL.Add ('MrgKonv.MrgTyp = ' + IntToStr (Key));
    Open;
    if RecordCount = 1 then
    begin
      FMeldungsgruppe.asString := FieldByName (CFNMeldungsgruppe).asString;
    end;
  end;
  Modified := False;
end;

{-----------------------------}
constructor TMSItemKonv.Create;
{-----------------------------}
begin
  inherited Create;
  FMeldungsgruppe := TIntegerData.Create;
end;

{-----------------------------}
destructor TMSItemKonv.Destroy;
{-----------------------------}
begin
  FMeldungsgruppe.Free;
  inherited Destroy;
end;

{---------------}
{ TMSStammdaten }
{---------------}

{-----------------------------------------------------}
procedure TMSStammdaten.SetModus (Value: TMSItemModus);
{-----------------------------------------------------}
var
  i: TMSSector;
begin
  if Value <> FModus then
  begin
    FModus := Value;
    for i := Low (FMSItem) to High (FMSItem) do
    begin
      if i in FSectors then
      begin
        FMSItem [i].Modus := FModus;
      end;
    end;
  end;
end;

{------------------------------------------}
function TMSStammdaten.GetModified: Boolean;
{------------------------------------------}
var
  i: TMSSector;
begin
  Result := False;
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      if FMSItem [i].Modified then
      begin
        Result := True;
        break;
      end;
    end;
  end;
end;

{---------------------------------------------------}
procedure TMSStammdaten.SetModified (Value: Boolean);
{---------------------------------------------------}
var
  i: TMSSector;
begin
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      FMSItem [i].Modified := False
    end;
  end;
end;

{---------------------------------------}
function TMSStammdaten.GetValid: Boolean;
{---------------------------------------}
var
  i: TMSSector;
begin
  Result := True;
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      if not FMSItem [i].Valid then
      begin
        Result := False;
        break;
      end;
    end;
  end;
end;

{--------------------------------------}
function TMSStammdaten.GetMrgId: TMrgId;
{--------------------------------------}
begin
  if FMrgId = 0 then
    raise EMSException.Create ('MrgId nicht vorhanden')
  else
    Result := FMrgId;
end;

{-----------------------------------------------}
procedure TMSStammdaten.SetMrgId (Value: TMrgId);
{-----------------------------------------------}
begin
  ClearAllMSItems;
  FMrgId := Value;
end;

{-----------------------------------------------------}
procedure TMSStammdaten.SetSectors (Value: TMSSectors);
{-----------------------------------------------------}
var
  i: TMSSector;
begin
  if FSectors <> Value then
  begin
    FreeAllMSItems;
    for i := Low (FMSItem) to High (FMSItem) do
    begin
      if i in Value then
      begin
        case i of
          msi_Allgemein    : FMSItem [i] := TMSItemAllg.Create;
          msi_Info         : FMSItem [i] := TMSItemInfo.Create;
          msi_Konv         : FMSItem [i] := TMSItemKonv.Create;
          msi_DSFG         : FMSItem [i] := TMSItemDSFG.Create;
          msi_DFU          : FMSItem [i] := TMSItemDFU.Create;
          msi_Automatik    : FMSItem [i] := TMSItemAuto.Create;
          msi_MDE          : FMSItem [i] := TMSItemMDE.Create;
          msi_Kanal        : FMSItem [i] := TMSNewKanalList.Create;
          else Continue;
        end;
        FMSItem [i].Modus := FModus;
        include (FSectors, i);
      end;
    end;
  end;
end;

{------------------------------------------------}
function TMSStammdaten.GetDatabaseName: TFileName;
{------------------------------------------------}
begin
  result := FQuery.DatabaseName;
end;

{---------------------------------------------------------}
procedure TMSStammdaten.SetDatabaseName (Value: TFileName);
{---------------------------------------------------------}
begin
  FQuery.DatabaseName := Value;
end;

{-----------------------------------------------}
function TMSStammdaten.GetSysDatabase: TFileName;
{-----------------------------------------------}
begin
  result := FQuerySys.DatabaseName;
end;

{--------------------------------------------------------}
procedure TMSStammdaten.SetSysDatabase (Value: TFileName);
{--------------------------------------------------------}
begin
  FQuerySys.DatabaseName := Value;
end;

{-----------------------------------------------}
function TMSStammdaten.GetAllgemein: TMSItemAllg;
{-----------------------------------------------}
begin
  if not (msi_Allgemein in FSectors) then
    raise EMSException.Create ('Keine allgemeine Daten vorhanden');
  result := FMSItem [msi_Allgemein] as TMSItemAllg;
end;

{------------------------------------------}
function TMSStammdaten.GetInfo: TMSItemInfo;
{------------------------------------------}
begin
  if not (msi_Info in FSectors) then
    raise EMSException.Create ('Keine Infodaten vorhanden');
  result := FMSItem [msi_Info] as TMSItemInfo;
end;

{------------------------------------------}
function TMSStammdaten.GetKonv: TMSItemKonv;
{------------------------------------------}
begin
  if not (msi_Konv in FSectors) then
    raise EMSException.Create ('Keine Konvdaten vorhanden');
  result := FMSItem [msi_Konv] as TMSItemKonv;
end;

{------------------------------------------}
function TMSStammdaten.GetDSFG: TMSItemDSFG;
{------------------------------------------}
begin
  if not (msi_DSFG in FSectors) then
    raise EMSException.Create ('Keine DSFG-Daten vorhanden');
  result := FMSItem [msi_DSFG] as TMSItemDSFG;
end;

{----------------------------------------}
function TMSStammdaten.GetDFU: TMSItemDFU;
{----------------------------------------}
begin
  if not (msi_DFU in FSectors) then
    raise EMSException.Create ('Keine DFU-Daten vorhanden');
  result := FMSItem [msi_DFU] as TMSItemDFU;
end;

{-----------------------------------------------}
function TMSStammdaten.GetAutomatik: TMSItemAuto;
{-----------------------------------------------}
begin
  if not (msi_Automatik in FSectors) then
    raise EMSException.Create ('Keine Automatikdaten vorhanden');
  result := FMSItem [msi_Automatik] as TMSItemAuto;
end;

{-----------------------------------------------}
function TMSStammdaten.GetMDE: TMSItemMDE;
{-----------------------------------------------}
begin
  if not (msi_MDE in FSectors) then
    raise EMSException.Create ('Keine MDE Daten vorhanden');
  result := FMSItem [msi_MDE] as TMSItemMDE;
end;

{-----------------------------------------------}
function TMSStammdaten.GetKanal: TMSNewKanalList;
{-----------------------------------------------}
begin
  if not (msi_Kanal in FSectors) then
    raise EMSException.Create ('Keine Kanaldaten vorhanden');
  result := FMSItem [msi_Kanal] as TMSNewKanalList;
end;

{-------------------------------------}
procedure TMSStammdaten.FreeAllMSItems;
{-------------------------------------}
var
  i: TMSSector;
begin
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      exclude (FSectors, i);
      FMSItem [i].Free;
    end;
  end;
end;

{--------------------------------------}
procedure TMSStammdaten.ClearAllMSItems;
{--------------------------------------}
var
  i: TMSSector;
begin
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      FMSItem [i].Clear;
    end;
  end;
end;

{---------------------------------------------}
procedure TMSStammdaten.CopyMrgKanalToStaKanal;
{---------------------------------------------}
var
  QueryKanal: TQuery;
  teststr: string;
begin
  QueryKanal := TQuery.Create (nil);
  try
    QueryKanal.DatabaseName := FQuerySys.DataBaseName;
    with QueryKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('MrgKanal, KanalId, KanalName, KanalTyp,');
      SQL.Add ('Einstellbar, Kontroll, Eingang,');
      SQL.Add ('Einheit, Kommastellen');
      SQL.Add ('FROM MRGKanal');
      SQL.Add ('WHERE MRGKanal.MrgTyp = ' + Allgemein.MrgTyp.AsString);
      Open;
      First;
    end;
    with FQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('INSERT INTO StaKanal');
      SQL.Add ('(MrgId, MrgKanal, Aktiv, KanalName, KanalTyp,');
      SQL.Add ('Einstellbar, Kontroll, Eingang, Einheit, Kommastellen)');
      SQL.Add ('VALUES');
      SQL.Add ('(' + IntToStr (MrgId) + ',');
      SQL.Add (':MrgKanal, :Aktiv, :KanalName,');
      SQL.Add (':KanalTyp, :Einstellbar, :Kontroll, :Eingang,');
      SQL.Add (':Einheit, :Kommastellen)');
      Prepare;
      while not QueryKanal.Eof do
      begin
        ParamByName ('MrgKanal').asSmallInt :=   // GeDa
          QueryKanal.FieldByName ('MrgKanal').asInteger;

        teststr:=QueryKanal.FieldByName ('KanalTyp').asString;
        if not ((teststr = CVImpuls) or (teststr = CVAnalog)) then
          ParamByName ('Aktiv').asBoolean := false{; kb 2.9.97 True;{}
        else
          ParamByName ('Aktiv').asBoolean := true;

        ParamByName ('KanalName').asString :=
          QueryKanal.FieldByName ('KanalName').asString;
        ParamByName ('KanalTyp').asString :=
          QueryKanal.FieldByName ('KanalTyp').asString;
        ParamByName ('Einstellbar').asBoolean :=
          QueryKanal.FieldByName ('Einstellbar').asBoolean;
        ParamByName ('Kontroll').asBoolean :=
          QueryKanal.FieldByName ('Kontroll').asBoolean;
        ParamByName ('Eingang').asBoolean :=
          QueryKanal.FieldByName ('Eingang').asBoolean;
        ParamByName ('Einheit').asString :=
          QueryKanal.FieldByName ('Einheit').asString;
        ParamByName ('Kommastellen').asSmallInt :=      // Geda
          QueryKanal.FieldByName ('Kommastellen').asInteger;
        ExecSQL;
        QueryKanal.Next;
      end;
    end;
  finally
    QueryKanal.Free;
  end;
end;

{-------------------------------------------------}
procedure TMSStammdaten.CreateImpulsAnalogChannels;
{-------------------------------------------------}
var
  QueryStaKanal: TQuery;
begin
  QueryStaKanal := TQuery.Create (nil);
  try
    QueryStaKanal.DataBaseName := FQuery.DatabaseName;
    with QueryStaKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('MrgKanal, KanalId');
      SQL.Add ('FROM');
      SQL.Add ('StaKanal');
      SQL.Add ('WHERE');
      SQL.Add ('StaKanal.MrgId = ' + IntToStr (MrgId));
      Open;
      First;
      while not Eof do
      begin
        CreateImpulsKanal (FieldByName ('MrgKanal').asInteger,
                           FieldByName ('KanalId').asInteger);
        CreateAnalogKanal (FieldByName ('MrgKanal').asInteger,
                           FieldByName ('KanalId').asInteger);
        Next;
      end;
    end;
  finally
    QueryStaKanal.Free;
  end;
end;

{-----------------------------------------------------------}
procedure TMSStammdaten.CreateImpulsKanal (MrgKanal: Integer;
            KanalId: LongInt);
{-----------------------------------------------------------}
var
  QueryKanal: TQuery;
begin
  QueryKanal := TQuery.Create (nil);
  try
    QueryKanal.DatabaseName := FQuerySys.DataBaseName;
    with QueryKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('MRGImp.Faktor, MRGImp.Teiler');
      SQL.Add ('FROM MRGKanal, MRGImp');
      SQL.Add ('WHERE');
      SQL.Add ('MRGKanal.MrgTyp = ' + Allgemein.MrgTyp.asString);
      SQL.Add ('AND');
      SQL.Add ('MRGKanal.MrgKanal = ' + IntToStr (MrgKanal));
      SQL.Add ('AND');
      SQL.Add ('MRGKanal.KanalId = MRGImp.KanalId');
      Open;
    end;
    if QueryKanal.RecordCount = 1 then
    begin
      QueryKanal.First;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add ('INSERT INTO StaImp');
        SQL.Add ('(KanalId, Faktor, Teiler, OrgFaktor)');
        SQL.Add ('VALUES');
        SQL.Add ('(' + IntToStr (KanalId) + ',');
        SQL.Add (':Faktor, :Teiler, 1)');
        ParamByName ('Faktor').asInteger :=
          QueryKanal.FieldByName ('Faktor').asInteger;
        ParamByName ('Teiler').asInteger :=
          QueryKanal.FieldByName ('Teiler').asInteger;
        ExecSQL;
      end;
    end;
  finally
    QueryKanal.Free;
  end;
end;

{-----------------------------------------------------------}
procedure TMSStammdaten.CreateAnalogKanal (MrgKanal: Integer;
            KanalId: LongInt);
{-----------------------------------------------------------}
var
  QueryKanal, QueryCheck: TQuery;
begin
  QueryKanal := TQuery.Create (nil);
  QueryCheck := TQuery.Create (nil);
  try
    QueryKanal.DatabaseName := FQuerySys.DataBaseName;
    with QueryKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('MRGAna.MessBereichMin, MRGAna.MessBereichMax,');
      SQL.Add ('MRGAna.StromBereich, MRGAna.Offset,');
      SQL.Add ('MRGAna.AufzMin, MRGAna.AufzMax');
      SQL.Add ('FROM MRGKanal, MRGAna');
      SQL.Add ('WHERE');
      SQL.Add ('MRGKanal.MrgTyp = ' + Allgemein.MrgTyp.asString);
      SQL.Add ('AND');
      SQL.Add ('MRGKanal.MrgKanal = ' + IntToStr (MrgKanal));
      SQL.Add ('AND');
      SQL.Add ('MRGKanal.KanalId = MRGAna.KanalId');
      Open;
    end;
    if QueryKanal.RecordCount = 1 then
    begin
      QueryKanal.First;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add ('INSERT INTO StaAna');
        SQL.Add ('(KanalId, MessBereichMin, MessBereichMax,');
        SQL.Add ('StromBereich, Offset, AufzMin, AufzMax)');
        SQL.Add ('VALUES');
        SQL.Add ('(' + IntToStr (KanalId) + ',');
        SQL.Add (':MessBereichMin, :MessBereichMax,');
        SQL.Add (':StromBereich, :Offset, :AufzMin, :AufzMax)');
        ParamByName ('MessBereichMin').asFloat :=
          QueryKanal.FieldByName ('MessBereichMin').asFloat;
        ParamByName ('MessBereichMax').asFloat :=
          QueryKanal.FieldByName ('MessBereichMax').asFloat;
        ParamByName ('StromBereich').asSmallInt :=    // GeDa
          QueryKanal.FieldByName ('StromBereich').asInteger;
        ParamByName ('Offset').asBoolean :=
          QueryKanal.FieldByName ('Offset').asBoolean;
        ParamByName ('AufzMin').asInteger :=
          QueryKanal.FieldByName ('AufzMin').asInteger;
        ParamByName ('AufzMax').asInteger :=
          QueryKanal.FieldByName ('AufzMax').asInteger;
        ExecSQL;
      end;
{Der folgende Abschnitt überprüft das Vorhandensein einer Vorbelegung "0.0"
 der Spalte MessBereichMin in "STAANA.db" und fügt sie ggf. ein.
 (Softwarefehler von Paradox-Datenbanken!!) kb 2.9.97 }
      QueryCheck.Close;
      QueryCheck.DatabaseName:=DatabaseName;
      With QueryCheck do begin
        SQL.Clear;
        SQL.Add('SELECT * FROM StaAna WHERE KanalId = ' + IntTostr(KanalId));
        QueryCheck.Open;
        if RecordCount > 0 then
          if FieldByName('MessBereichMin').AsString = '' then begin
            Close;
            SQL.Clear;
            SQL.Add('UPDATE StaAna');
            SQL.Add('SET MessbereichMin = 0.0');
            SQL.Add('WHERE KanalId = ' + IntTostr(KanalId));
            ExecSQL;
          end;
      end; { withQueryCheck }   {kb 2.9.97}
    end;
  finally
    QueryCheck.Free;
    QueryKanal.Free;
  end;
end;

{------------------------------------}
procedure TMSStammdaten.DoBeforeClear;
{------------------------------------}
begin
  if Assigned (FBeforeClear) then FBeforeClear (Self);
end;

{-----------------------------------}
procedure TMSStammdaten.DoAfterClear;
{-----------------------------------}
begin
  if Assigned (FAfterClear) then FAfterClear (Self);
end;

{----------------------------------------------------}
constructor TMSStammdaten.Create (AOwner: TComponent);
{----------------------------------------------------}
begin
  inherited Create (AOwner);
  FMrgId := 0;
  FSectors := [];
  FillChar (FMSItem, sizeof (FMSItem), 0);
  FQuery := TMSQuery.Create (Self);
  FQuerySys := TMSQuery.Create (Self);
  FBeforeClear := nil;
  FAfterClear := nil;
end;

{-------------------------------}
destructor TMSStammdaten.Destroy;
{-------------------------------}
begin
  FreeAllMSItems;
  FQuerySys.Free;
  FQuery.Free;
  inherited Destroy;
end;

{---------------------------}
procedure TMSStammdaten.Read;
{---------------------------}
var
  i: TMSSector;
begin
  if not (FModus in [msm_Read, msm_ReadWrite]) then
    raise EMSException.Create ('Lesen nicht erlaubt');
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      if (i = msi_Info) or (i = msi_Konv) then
        FMSItem [i].Read (Allgemein.MrgTyp.asInteger, FQuerySys)
      else
        FMSItem [i].Read (FMrgId, FQuery);
    end;
  end;
end;

{ Modify: Geänderte Stammdaten werden gesichert }
{-----------------------------}
procedure TMSStammdaten.Modify;
{-----------------------------}
var
  i: TMSSector;
begin
  if not (FModus in [msm_ReadWrite]) then
    raise EMSException.Create ('Update nicht erlaubt');
  if not Modified then
    raise EMSException.Create ('Daten wurden nicht geändert');
  if not Valid then
    raise EMSException.Create ('Daten ungültig');
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      if FMSItem [i].Modified then
        FMSItem [i].Modify (FMrgId, FQuery);
    end;
  end;
end;

{-----------------------------}
procedure TMSStammdaten.Append;
{-----------------------------}
begin
  if not (FModus in [msm_Create]) then
    raise EMSException.Create ('Append nicht erlaubt');
  if not Modified then
    raise EMSException.Create ('Daten wurden nicht geändert');
  if not Valid then
    raise EMSException.Create ('Daten ungültig');
  if msi_Allgemein in FSectors then
  begin
    FMrgId := FMSItem [msi_Allgemein].Append (0, FQuery);
    if msi_DSFG in FSectors then
      FMSItem [msi_DSFG].Append (FMrgId, FQuery);
    if msi_DFU in FSectors then
      FMSItem [msi_DFU].Append (FMrgId, FQuery);
    if msi_Automatik in FSectors then
      FMSItem [msi_Automatik].Append (FMrgId, FQuery);
    if msi_MDE in FSectors then
      FMSItem [msi_MDE].Append (FMrgId, FQuery);
    CopyMrgKanalToStaKanal;
    CreateImpulsAnalogChannels;
  end;
end;

{-----------------------------}
procedure TMSStammdaten.Delete;
{-----------------------------}
begin
  if not (FModus in [msm_ReadWrite]) then
    raise EMSException.Create ('Löschen nicht erlaubt');
  if not Valid then
    raise EMSException.Create ('Daten ungültig');
  if msi_Allgemein in FSectors then
  begin
    FMSItem [msi_Allgemein].Delete (FMrgId, FQuery);
  end;
end;

{ Löscht gesamten Datensatz }
{----------------------------}
procedure TMSStammdaten.Clear;
{----------------------------}
begin
  DoBeforeClear;
  ClearAllMSItems;
  DoAfterClear;
end;

{---------------}
{ TMSStammdatenPlus }
{---------------}

{-----------------------------------------------------}
procedure TMSStammdatenPlus.SetModus (Value: TMSItemModus);
{-----------------------------------------------------}
var
  i: TMSSector;
begin
  if Value <> FModus then
  begin
    FModus := Value;
    for i := Low (FMSItem) to High (FMSItem) do
    begin
      if i in FSectors then
      begin
        FMSItem [i].Modus := FModus;
      end;
    end;
  end;
end;

{------------------------------------------}
function TMSStammdatenPlus.GetModified: Boolean;
{------------------------------------------}
var
  i: TMSSector;
begin
  Result := False;
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      if FMSItem [i].Modified then
      begin
        Result := True;
        break;
      end;
    end;
  end;
end;

{---------------------------------------------------}
procedure TMSStammdatenPlus.SetModified (Value: Boolean);
{---------------------------------------------------}
var
  i: TMSSector;
begin
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      FMSItem [i].Modified := False
    end;
  end;
end;

{---------------------------------------}
function TMSStammdatenPlus.GetValid: Boolean;
{---------------------------------------}
var
  i: TMSSector;
begin
  Result := True;
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      if not FMSItem [i].Valid then
      begin
        Result := False;
        break;
      end;
    end;
  end;
end;

{--------------------------------------}
function TMSStammdatenPlus.GetMrgId: TMrgId;
{--------------------------------------}
begin
  if FMrgId = 0 then
    raise EMSException.Create ('MrgId nicht vorhanden')
  else
    Result := FMrgId;
end;

{-----------------------------------------------}
procedure TMSStammdatenPlus.SetMrgId (Value: TMrgId);
{-----------------------------------------------}
begin
  ClearAllMSItems;
  FMrgId := Value;
end;

{-----------------------------------------------------}
procedure TMSStammdatenPlus.SetSectors (Value: TMSSectors);
{-----------------------------------------------------}
var
  i: TMSSector;
begin
  if FSectors <> Value then
  begin
    FreeAllMSItems;
    for i := Low (FMSItem) to High (FMSItem) do
    begin
      if i in Value then
      begin
        case i of
          msi_Allgemein    : FMSItem [i] := TMSItemAllg.Create;
          msi_Info         : FMSItem [i] := TMSItemInfo.Create;
          msi_Konv         : FMSItem [i] := TMSItemKonv.Create;
          msi_DSFG         : FMSItem [i] := TMSItemDSFG.Create;
          msi_DFU          : FMSItem [i] := TMSItemDFU.Create;
          msi_Automatik    : FMSItem [i] := TMSItemAuto.Create;
          msi_MDE          : FMSItem [i] := TMSItemMDE.Create;
          msi_Kanal        : FMSItem [i] := TMSNewKanalListPlus.Create;
          else Continue;
        end;
        FMSItem [i].Modus := FModus;
        include (FSectors, i);
      end;
    end;
  end;
end;

{------------------------------------------------}
function TMSStammdatenPlus.GetDatabaseName: TFileName;
{------------------------------------------------}
begin
  result := FQuery.DatabaseName;
end;

{---------------------------------------------------------}
procedure TMSStammdatenPlus.SetDatabaseName (Value: TFileName);
{---------------------------------------------------------}
begin
  FQuery.DatabaseName := Value;
end;

{-----------------------------------------------}
function TMSStammdatenPlus.GetSysDatabase: TFileName;
{-----------------------------------------------}
begin
  result := FQuerySys.DatabaseName;
end;

{--------------------------------------------------------}
procedure TMSStammdatenPlus.SetSysDatabase (Value: TFileName);
{--------------------------------------------------------}
begin
  FQuerySys.DatabaseName := Value;
end;

{-----------------------------------------------}
function TMSStammdatenPlus.GetAllgemein: TMSItemAllg;
{-----------------------------------------------}
begin
  if not (msi_Allgemein in FSectors) then
    raise EMSException.Create ('Keine allgemeine Daten vorhanden');
  result := FMSItem [msi_Allgemein] as TMSItemAllg;
end;

{------------------------------------------}
function TMSStammdatenPlus.GetInfo: TMSItemInfo;
{------------------------------------------}
begin
  if not (msi_Info in FSectors) then
    raise EMSException.Create ('Keine Infodaten vorhanden');
  result := FMSItem [msi_Info] as TMSItemInfo;
end;

{------------------------------------------}
function TMSStammdatenPlus.GetKonv: TMSItemKonv;
{------------------------------------------}
begin
  if not (msi_Konv in FSectors) then
    raise EMSException.Create ('Keine Konvdaten vorhanden');
  result := FMSItem [msi_Konv] as TMSItemKonv;
end;

{------------------------------------------}
function TMSStammdatenPlus.GetDSFG: TMSItemDSFG;
{------------------------------------------}
begin
  if not (msi_DSFG in FSectors) then
    raise EMSException.Create ('Keine DSFG-Daten vorhanden');
  result := FMSItem [msi_DSFG] as TMSItemDSFG;
end;

{----------------------------------------}
function TMSStammdatenPlus.GetDFU: TMSItemDFU;
{----------------------------------------}
begin
  if not (msi_DFU in FSectors) then
    raise EMSException.Create ('Keine DFU-Daten vorhanden');
  result := FMSItem [msi_DFU] as TMSItemDFU;
end;

{-----------------------------------------------}
function TMSStammdatenPlus.GetAutomatik: TMSItemAuto;
{-----------------------------------------------}
begin
  if not (msi_Automatik in FSectors) then
    raise EMSException.Create ('Keine Automatikdaten vorhanden');
  result := FMSItem [msi_Automatik] as TMSItemAuto;
end;

{-----------------------------------------------}
function TMSStammdatenPlus.GetMDE: TMSItemMDE;
{-----------------------------------------------}
begin
  if not (msi_MDE in FSectors) then
    raise EMSException.Create ('Keine MDE Daten vorhanden');
  result := FMSItem [msi_MDE] as TMSItemMDE;
end;

{-----------------------------------------------}
function TMSStammdatenPlus.GetKanal: TMSNewKanalListPlus;
{-----------------------------------------------}
begin
  if not (msi_Kanal in FSectors) then
    raise EMSException.Create ('Keine Kanaldaten vorhanden');
  result := TMSNewKanalListPlus(FMSItem [msi_Kanal]);
end;

{-------------------------------------}
procedure TMSStammdatenPlus.FreeAllMSItems;
{-------------------------------------}
var
  i: TMSSector;
begin
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      exclude (FSectors, i);
      FMSItem [i].Free;
    end;
  end;
end;

{--------------------------------------}
procedure TMSStammdatenPlus.ClearAllMSItems;
{--------------------------------------}
var
  i: TMSSector;
begin
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      FMSItem [i].Clear;
    end;
  end;
end;

{---------------------------------------------}
procedure TMSStammdatenPlus.CopyMrgKanalToStaKanal;
{---------------------------------------------}
var
  QueryKanal: TQuery;
  teststr: string;
begin
  QueryKanal := TQuery.Create (nil);
  try
    QueryKanal.DatabaseName := FQuerySys.DataBaseName;
    with QueryKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('MrgKanal, KanalId, KanalName, KanalTyp,');
      SQL.Add ('Einstellbar, Kontroll, Eingang,');
      SQL.Add ('Einheit, Kommastellen');
      SQL.Add ('FROM MRGKanal');
      SQL.Add ('WHERE MRGKanal.MrgTyp = ' + Allgemein.MrgTyp.AsString);
      Open;
      First;
    end;
    with FQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('INSERT INTO StaKanal');
      SQL.Add ('(MrgId, MrgKanal, Aktiv, KanalName, KanalTyp,');
      SQL.Add ('Einstellbar, Kontroll, Eingang, Einheit, Kommastellen)');
      SQL.Add ('VALUES');
      SQL.Add ('(' + IntToStr (MrgId) + ',');
      SQL.Add (':MrgKanal, :Aktiv, :KanalName,');
      SQL.Add (':KanalTyp, :Einstellbar, :Kontroll, :Eingang,');
      SQL.Add (':Einheit, :Kommastellen)');
      Prepare;
      while not QueryKanal.Eof do
      begin
        ParamByName ('MrgKanal').asInteger :=
          QueryKanal.FieldByName ('MrgKanal').asInteger;

        teststr:=QueryKanal.FieldByName ('KanalTyp').asString;
        if not ((teststr = CVImpuls) or (teststr = CVAnalog)) then
          ParamByName ('Aktiv').asBoolean := false{; kb 2.9.97 True;{}
        else
          ParamByName ('Aktiv').asBoolean := true;

        ParamByName ('KanalName').asString :=
          QueryKanal.FieldByName ('KanalName').asString;
        ParamByName ('KanalTyp').asString :=
          QueryKanal.FieldByName ('KanalTyp').asString;
        ParamByName ('Einstellbar').asBoolean :=
          QueryKanal.FieldByName ('Einstellbar').asBoolean;
        ParamByName ('Kontroll').asBoolean :=
          QueryKanal.FieldByName ('Kontroll').asBoolean;
        ParamByName ('Eingang').asBoolean :=
          QueryKanal.FieldByName ('Eingang').asBoolean;
        ParamByName ('Einheit').asString :=
          QueryKanal.FieldByName ('Einheit').asString;
        ParamByName ('Kommastellen').asInteger :=
          QueryKanal.FieldByName ('Kommastellen').asInteger;
        ExecSQL;
        QueryKanal.Next;
      end;
    end;
  finally
    QueryKanal.Free;
  end;
end;

{-------------------------------------------------}
procedure TMSStammdatenPlus.CreateImpulsAnalogChannels;
{-------------------------------------------------}
var
  QueryStaKanal: TQuery;
begin
  QueryStaKanal := TQuery.Create (nil);
  try
    QueryStaKanal.DataBaseName := FQuery.DatabaseName;
    with QueryStaKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('MrgKanal, KanalId');
      SQL.Add ('FROM');
      SQL.Add ('StaKanal');
      SQL.Add ('WHERE');
      SQL.Add ('StaKanal.MrgId = ' + IntToStr (MrgId));
      Open;
      First;
      while not Eof do
      begin
        CreateImpulsKanal (FieldByName ('MrgKanal').asInteger,
                           FieldByName ('KanalId').asInteger);
        CreateAnalogKanal (FieldByName ('MrgKanal').asInteger,
                           FieldByName ('KanalId').asInteger);
        Next;
      end;
    end;
  finally
    QueryStaKanal.Free;
  end;
end;

{-----------------------------------------------------------}
procedure TMSStammdatenPlus.CreateImpulsKanal (MrgKanal: Integer;
            KanalId: LongInt);
{-----------------------------------------------------------}
var
  QueryKanal: TQuery;
begin
  QueryKanal := TQuery.Create (nil);
  try
    QueryKanal.DatabaseName := FQuerySys.DataBaseName;
    with QueryKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('MRGImp.Faktor, MRGImp.Teiler, MRGImp.StdSumAvg');
      SQL.Add ('FROM MRGKanal, MRGImp');
      SQL.Add ('WHERE');
      SQL.Add ('MRGKanal.MrgTyp = ' + Allgemein.MrgTyp.asString);
      SQL.Add ('AND');
      SQL.Add ('MRGKanal.MrgKanal = ' + IntToStr (MrgKanal));
      SQL.Add ('AND');
      SQL.Add ('MRGKanal.KanalId = MRGImp.KanalId');
      Open;
    end;
    if QueryKanal.RecordCount = 1 then
    begin
      QueryKanal.First;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add ('INSERT INTO StaImp');
        SQL.Add ('(KanalId, Faktor, Teiler, OrgFaktor, Stdsumavg)');
        SQL.Add ('VALUES');
        SQL.Add ('(' + IntToStr (KanalId) + ',');
        SQL.Add (':Faktor, :Teiler, 1, :StdSumAvg)');
        ParamByName ('Faktor').asInteger :=
          QueryKanal.FieldByName ('Faktor').asInteger;
        ParamByName ('Teiler').asInteger :=
          QueryKanal.FieldByName ('Teiler').asInteger;
        ParamByName('Stdsumavg').AsString:=
          QueryKanal.FieldByName('Stdsumavg').AsString; 
        ExecSQL;
      end;
    end;
  finally
    QueryKanal.Free;
  end;
end;

{-----------------------------------------------------------}
procedure TMSStammdatenPlus.CreateAnalogKanal (MrgKanal: Integer;
            KanalId: LongInt);
{-----------------------------------------------------------}
var
  QueryKanal, QueryCheck: TQuery;
begin
  QueryKanal := TQuery.Create (nil);
  QueryCheck := TQuery.Create (nil);
  try
    QueryKanal.DatabaseName := FQuerySys.DataBaseName;
    with QueryKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('MRGAna.MessBereichMin, MRGAna.MessBereichMax,');
      SQL.Add ('MRGAna.StromBereich, MRGAna.Offset,');
      SQL.Add ('MRGAna.AufzMin, MRGAna.AufzMax');
      SQL.Add ('FROM MRGKanal, MRGAna');
      SQL.Add ('WHERE');
      SQL.Add ('MRGKanal.MrgTyp = ' + Allgemein.MrgTyp.asString);
      SQL.Add ('AND');
      SQL.Add ('MRGKanal.MrgKanal = ' + IntToStr (MrgKanal));
      SQL.Add ('AND');
      SQL.Add ('MRGKanal.KanalId = MRGAna.KanalId');
      Open;
    end;
    if QueryKanal.RecordCount = 1 then
    begin
      QueryKanal.First;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add ('INSERT INTO StaAna');
        SQL.Add ('(KanalId, MessBereichMin, MessBereichMax,');
        SQL.Add ('StromBereich, Offset, AufzMin, AufzMax)');
        SQL.Add ('VALUES');
        SQL.Add ('(' + IntToStr (KanalId) + ',');
        SQL.Add (':MessBereichMin, :MessBereichMax,');
        SQL.Add (':StromBereich, :Offset, :AufzMin, :AufzMax)');
        ParamByName ('MessBereichMin').asFloat :=
          QueryKanal.FieldByName ('MessBereichMin').asFloat;
        ParamByName ('MessBereichMax').asFloat :=
          QueryKanal.FieldByName ('MessBereichMax').asFloat;
        ParamByName ('StromBereich').asSmallInt :=  { GeDa }
          QueryKanal.FieldByName ('StromBereich').asInteger;
        ParamByName ('Offset').asBoolean :=
          QueryKanal.FieldByName ('Offset').asBoolean;
        ParamByName ('AufzMin').asInteger :=
          QueryKanal.FieldByName ('AufzMin').asInteger;
        ParamByName ('AufzMax').asInteger :=
          QueryKanal.FieldByName ('AufzMax').asInteger;
        ExecSQL;
      end;
{Der folgende Abschnitt überprüft das Vorhandensein einer Vorbelegung "0.0"
 der Spalte MessBereichMin in "STAANA.db" und fügt sie ggf. ein.
 (Softwarefehler von Paradox-Datenbanken!!) kb 2.9.97 }
      QueryCheck.Close;
      QueryCheck.DatabaseName:=DatabaseName;
      With QueryCheck do begin
        SQL.Clear;
        SQL.Add('SELECT * FROM StaAna WHERE KanalId = ' + IntTostr(KanalId));
        QueryCheck.Open;
        if RecordCount > 0 then
          if FieldByName('MessBereichMin').AsString = '' then begin
            Close;
            SQL.Clear;
            SQL.Add('UPDATE StaAna');
            SQL.Add('SET MessbereichMin = 0.0');
            SQL.Add('WHERE KanalId = ' + IntTostr(KanalId));
            ExecSQL;
          end;
      end; { withQueryCheck }   {kb 2.9.97}
    end;
  finally
    QueryCheck.Free;
    QueryKanal.Free;
  end;
end;

{------------------------------------}
procedure TMSStammdatenPlus.DoBeforeClear;
{------------------------------------}
begin
  if Assigned (FBeforeClear) then FBeforeClear (Self);
end;

{-----------------------------------}
procedure TMSStammdatenPlus.DoAfterClear;
{-----------------------------------}
begin
  if Assigned (FAfterClear) then FAfterClear (Self);
end;

{----------------------------------------------------}
constructor TMSStammdatenPlus.Create (AOwner: TComponent);
{----------------------------------------------------}
begin
  inherited Create (AOwner);
  FMrgId := 0;
  FSectors := [];
  FillChar (FMSItem, sizeof (FMSItem), 0);
  FQuery := TMSQuery.Create (Self);
  FQuerySys := TMSQuery.Create (Self);
  FBeforeClear := nil;
  FAfterClear := nil;
end;

{-------------------------------}
destructor TMSStammdatenPlus.Destroy;
{-------------------------------}
begin
  FreeAllMSItems;
  FQuerySys.Free;
  FQuery.Free;
  inherited Destroy;
end;

{---------------------------}
procedure TMSStammdatenPlus.Read;
{---------------------------}
var
  i: TMSSector;
begin
  if not (FModus in [msm_Read, msm_ReadWrite]) then
    raise EMSException.Create ('Lesen nicht erlaubt');
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      if (i = msi_Info) or (i = msi_Konv) then
        FMSItem [i].Read (Allgemein.MrgTyp.asInteger, FQuerySys)
      else
        FMSItem [i].Read (FMrgId, FQuery);
    end;
  end;
end;

{ Modify: Geänderte Stammdaten werden gesichert }
{-----------------------------}
procedure TMSStammdatenPlus.Modify;
{-----------------------------}
var
  i: TMSSector;
begin
  if not (FModus in [msm_ReadWrite]) then
    raise EMSException.Create ('Update nicht erlaubt');
  if not Modified then
    raise EMSException.Create ('Daten wurden nicht geändert');
  if not Valid then
    raise EMSException.Create ('Daten ungültig');
  for i := Low (FMSItem) to High (FMSItem) do
  begin
    if i in FSectors then
    begin
      if FMSItem [i].Modified then
        FMSItem [i].Modify (FMrgId, FQuery);
    end;
  end;
end;

{-----------------------------}
procedure TMSStammdatenPlus.Append;
{-----------------------------}
begin
  if not (FModus in [msm_Create]) then
    raise EMSException.Create ('Append nicht erlaubt');
  if not Modified then
    raise EMSException.Create ('Daten wurden nicht geändert');
  if not Valid then
    raise EMSException.Create ('Daten ungültig');
  if msi_Allgemein in FSectors then
  begin
    FMrgId := FMSItem [msi_Allgemein].Append (0, FQuery);
    if msi_DSFG in FSectors then
      FMSItem [msi_DSFG].Append (FMrgId, FQuery);
    if msi_DFU in FSectors then
      FMSItem [msi_DFU].Append (FMrgId, FQuery);
    if msi_Automatik in FSectors then
      FMSItem [msi_Automatik].Append (FMrgId, FQuery);
    if msi_MDE in FSectors then
      FMSItem [msi_MDE].Append (FMrgId, FQuery);
    CopyMrgKanalToStaKanal;
    CreateImpulsAnalogChannels;
  end;
end;

{-----------------------------}
procedure TMSStammdatenPlus.Delete;
{-----------------------------}
begin
  if not (FModus in [msm_ReadWrite]) then
    raise EMSException.Create ('Löschen nicht erlaubt');
  if not Valid then
    raise EMSException.Create ('Daten ungültig');
  if msi_Allgemein in FSectors then
  begin
    FMSItem [msi_Allgemein].Delete (FMrgId, FQuery);
  end;
end;

{ Löscht gesamten Datensatz }
{----------------------------}
procedure TMSStammdatenPlus.Clear;
{----------------------------}
begin
  DoBeforeClear;
  ClearAllMSItems;
  DoAfterClear;
end;

{------------}
{ TMSListBox }
{------------}

{-------------------------------------}
function TMSListBox.GetActive: Boolean;
{-------------------------------------}
begin
  result := FQuery.Active;
end;

{----------------------------------------------}
procedure TMSListBox.SetActive (Value: Boolean);
{----------------------------------------------}
begin
  if Value <> FQuery.Active then
  begin
    if Value = True then
      ExecQuery
    else
      FQuery.Close;
  end;
end;

{---------------------------------------------}
function TMSListBox.GetDatabaseName: TFileName;
{---------------------------------------------}
begin
  result := FQuery.DatabaseName;
end;

{------------------------------------------------------}
procedure TMSListBox.SetDatabaseName (Value: TFileName);
{------------------------------------------------------}
begin
  FQuery.DatabaseName := Value;
end;

{----------------------------------------------------}
procedure TMSListBox.SetOption (Value: TMSListOption);
{----------------------------------------------------}
begin
  FOption := Value;
end;

{-------------------------------------------}
function TMSListBox.GetSelectedMrgId: TMrgId;
{-------------------------------------------}
begin
  Result := -1;
  if Active then
  begin
    if (ItemIndex >= 0) and  (ItemIndex < FQuery.RecordCount) then
    begin
      FQuery.First;
      FQuery.MoveBy (itemIndex);
      Result := FQuery.FieldByName (CFNMrgId).asInteger;
    end;
  end;
end;

{-----------------------------}
procedure TMSListBox.CreateWnd;
{-----------------------------}
begin
  inherited CreateWnd;
  FKennungLen := Canvas.TextWidth ('A') * 16;
  FMRGNameLen := Canvas.TextWidth ('A') * 12;
end;

{-----------------------------}
procedure TMSListBox.ExecQuery;
{-----------------------------}
begin
  FQuery.QueryList (FOption, FSysDatabase);
end;

{---------------------------------------------------------}
procedure TMSListBox.DrawItem (Index: Integer; Rect: TRect;
                               State: TOwnerDrawState);
{---------------------------------------------------------}
begin
  Canvas.FillRect (Rect);
  if Index < Items.Count then
  begin
    FQuery.First;
    FQuery.MoveBy (Index);
    case FOption of
      msl_SAll, msl_SAuto:
        begin
          Canvas.TextOut (Rect.Left + 2, Rect.Top,
                          FQuery.FieldByName (CFNStationsName).asString);
        end;
      msl_SKAll, msl_SKAuto:
        begin
          Canvas.TextOut (Rect.Left + 2, Rect.Top,
                          FQuery.FieldByName (CFNKennung).asString);
          Canvas.TextOut (Rect.Left + 2 + FKennungLen, Rect.Top,
                          FQuery.FieldByName (CFNStationsName).asString);
        end;
      msl_SKNAll, msl_SKNAuto:
        begin
          Canvas.TextOut (Rect.Left + 2, Rect.Top,
                          FQuery.FieldByName (CFNMrgName).asString);
          Canvas.TextOut (Rect.Left + 2 + FMrgNameLen, Rect.Top,
                          FQuery.FieldByName (CFNKennung).asString);
          Canvas.TextOut (Rect.Left + 2 + FMrgNameLen + FKennungLen, Rect.Top,
                          FQuery.FieldByName (CFNStationsName).asString);
        end;
    end;
  end;
end;

{------------------------------------------------------}
procedure TMSListBox.QueryAfterOpen (DataSet: TDataset);
{------------------------------------------------------}
var
  i: Integer;
begin
  for i := 0 to FQuery.RecordCount - 1 do
    Items.Add ('');
  if Items.Count > 0 then
    ItemIndex := 0;
end;

{--------------------------------------------------------}
procedure TMSListBox.QueryBeforeClose (DataSet: TDataSet);
{--------------------------------------------------------}
begin
  Clear;
end;

{-------------------------------------------------}
constructor TMSListBox.Create (AOwner: TComponent);
{-------------------------------------------------}
begin
  inherited Create (AOwner);
  Style := lbOwnerDrawFixed;
  FQuery := TMSQuery.Create (Self);
  FQuery.AfterOpen := QueryAfterOpen;
  FQuery.BeforeClose := QueryBeforeClose;
  FOption := msl_SKAll;
end;

{----------------------------}
destructor TMSListBox.Destroy;
{----------------------------}
begin
  FQuery.Free;
  inherited Destroy;
end;

{------------------------}
procedure TMSListBox.Open;
{------------------------}
begin
  ExecQuery;
end;

{-------------------------}
procedure TMSListBox.Close;
{-------------------------}
begin
  FQuery.Close;
end;

end.
