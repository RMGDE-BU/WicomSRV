{------------------------------------------------------------------------------}
{ unit MrgStd                                                                  }
{ Komponenten für Stammdatenzugriffe                                           }
{ 23.09.1996 DA, Copyright Karl Wieser GmbH                                    }
{ 19.11.1998 GD; Anpassung an Delphi 3                                         }
{            -> TabellenFeldTyp 'S' als 'asSmallInt' ansprechen                }
{            -> 'LEFT JOIN'-Abfragen funktionieren nicht - Tabelle             }
{            -> Erweiterung der Sta.db um 'Prioritaet'                         }
{               -> überall in TMSItemAllg eingebaut                            }
{            -> logische Schnittstelle Sx eingebaut (CLogPort 0..15)           }
{ 15.04.1999 GD; Korrektur bei ParamByName().asSmallInt                        }
{ 15.04.1999 GD; Erweiterung der Sta.db um 'isWZ_SZ'                           }
{ 19.01.2000 WP; Fehler 8.Kanal ändern bei Offset=true behoben                 }
{                in fillJoinTempTable                                          }
{ 07.11.2000 WW; Temptabellen wieder raus, LEFT JOIN funktioniert              }
{                jetzt richtig                                                 }
{ 16.07.2002 H.-P.R. mit STAMDE					               }
{ 19.11.2002 GD  Umstellung auf SQL Server, 'Plus' eleminiert                  }
{ 18.07.2005 WW; Erweiterung der Sta.db um Archivdaten-Typ,                    }
{                stakanal.db um Kanaltyp_ori, Datentyp_ori                     }
{ 20.01.2006 WW; resourcestrings                                               }
{ 27.09.2006 GD; Lokalen Query in "CopyMrgKanalToStaKanal" eingesetzt          }
{ 26.10.2006 GD; Kanalliste in "TMSNewKanalList" in "protected" veröffentlicht }
{ 22.01.2008 WW; Stadsfg.db: Feld 'masterid' auch zur Kennzeichnung von        }
{                Tritschler Multiplexer                                        }
{ 14.04.2008 WW; neue Felder Zählpunkt und EDIS-Kennziffer                     }
{ 24.11.2008 GD; Nach Zählpunkt sortierbar                                     }
{ 11.03.2009 GD; Erweiterung der StaDfu.db um Feld 'DfuModus'                  }
{ 15.02.2013 WW; mit dfumode_Netzwerk                                          }
{ 24.06.2013 WW; Erweiterung der StaDfu.db um Feld 'ipadr_port'; CLogPortMax   }
{                erhöht                                                        }
{ 30.09.2013 WW; TMSStammdaten.CheckMissingMrgKanaele                          }
{ 03.07.2014 WW; TMSStammdaten.Modify: Speicherreihenfolge der Sektoren geän-  }
{                dert ('Kennung schon vorhanden'-Problem)                      }
{------------------------------------------------------------------------------}

unit Mrgstd;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, DB, DBTables, Menus,
  ObjData, WStrUtils, WSysCon, DbAutoInc, SerialConst;

{$B-}

const
  { FieldNames }
  CFNImpulsKanalId = 'impulskanalid';
  CFNAnalogKanalId = 'analogkanalid';

type

  TMrgId   = LongInt;

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
  EMSKennungVorhandenException = class (Exception);

  TMSQuery = class (TQuery)
  private
    procedure QueryList (ListOption: TMSListOption; const SysDatabase: string);
{    function QuerySelect (const Args: array of string): LongInt;}
  end;

  TMSSQLField = (msf_MrgId, msf_MrgName, msf_Kennung, msf_Stationsname,
    msf_Zahlpunkt);
  TMSSQLFields = set of TMSSQLField;

  TMSSQLCondition = (msc_Auto);
  TMSSQLConditions = set of TMSSQLCondition;

  TMSSQLOrder = (mso_Kennung, mso_Stationsname, mso_MrgName, mso_Zaehlpunkt);

  TMSSQL = class (TComponent)
  private
    FDbMrgStamm: string;
    FDbMrgSys: string;
    FQuery: TQuery;
    FSQLFields: TMSSQLFields;
    FSQLConditions: TMSSQLConditions;
    FSQLOrder: TMSSQLOrder;
    procedure SetSQLFields (Value: TMSSQLFields);
    procedure SetSQLConditions (Value: TMSSQLConditions);
    procedure SetSQLOrder (Value: TMSSQLOrder);
    procedure SetMrgStamm (Value: string);
    procedure SetMrgSys (Value: string);
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
    property MrgStamm: string
               read FDbMrgStamm write SetMrgStamm;
    property MrgSys: string
               read FDbMrgSys write SetMrgSys;
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
    FModus     : TMSItemModus;
    FDatabase  : TDatabase;
    FTbAutoInc : TDbAutoInc;
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
    property Database: TDatabase read FDatabase;
  public
    constructor Create(pDatabase: TDatabase); virtual;
    destructor Destroy; override;
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
    FisWZ_SZ: TBooleanData;         { Zeitumstellung - GeDa }
    FArchivdatenTyp: TIntegerData;  { Archivdaten-Typ: LGZ-normiert, Originalwerte - WW }
    FZaehlpunkt: TStringData;       { Zählpunkt }
    procedure CheckIndex (const Kennung: string; Query: TMSQuery;
      MrgId: integer = -1);
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
    constructor Create(pDatabase: TDatabase); override;
    destructor Destroy; override;
    property MrgTyp: TIntegerData read FMrgTyp;
    property Kennung: TXStringData read FKennung;
    property StationsName: TStringData read FStationsName;
    property TagesEnde: TIntegerData read FTagesEnde;
    property Prioritaet: TIntegerData read FPrioritaet;  { Geda }
    property isWZ_SZ: TBooleanData read FisWZ_SZ;  { Geda }
    property ArchivdatenTyp: TIntegerData read FArchivdatentyp;  { WW }
    property Zaehlpunkt: TStringData read FZaehlpunkt;
  end;

  TMSDSFGTyp = (
    DSFG_None,         { Kein DSFG-Gerät }
    DSFG_Slave,        { Master-Gerät }
    DSFG_Master,       { Slave-Gerät }
    DSFG_Mux           { Gerät an Tritschler Multiplexer }
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
    constructor Create(pDatabase: TDatabase); override;
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
    FDfuModus: TIntegerData;
    FIPAdr_Port: TStringData;
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
    constructor Create(pDatabase: TDatabase); override;
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
    property DfuModus: TIntegerData read FDfuModus;
    property IPAdr_Port: TStringData read FIPAdr_Port;
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
    constructor Create(pDatabase: TDatabase); override;
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
    constructor Create(pDatabase: TDatabase); override;
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
    constructor Create(pDatabase: TDatabase); override;
    destructor Destroy; override;
    property Faktor: TIntegerData read FFaktor write FFaktor;
    property Teiler: TIntegerData read FTeiler write FTeiler;
    property OrgFaktor: TNumericData read FOrgFaktor write FOrgFaktor;
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
    constructor Create(pDatabase: TDatabase); override;
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
    FOriKanalTyp: TStringData;
    FOriDatenTyp: TIntegerData;
    FZaehlpunkt: TStringData;
    FEDIS_Kennziffer: TStringData;
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
    constructor Create(pDatabase: TDatabase); override;
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
    property OriKanalTyp: TStringData read FOriKanalTyp write FOriKanalTyp;
    property OriDatenTyp: TIntegerData
               read FOriDatenTyp write FOriDatenTyp;
    property Zaehlpunkt: TStringData read FZaehlpunkt write FZaehlpunkt;
    property EDIS_Kennziffer: TStringData read FEDIS_Kennziffer write FEDIS_Kennziffer;
    property Impuls: TMSNewImpulsKanal read GetImpuls;
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
  protected
    procedure Clear; override;
    function GetModified: Boolean; override;
    procedure SetModified (Value: Boolean); override;
    function GetValid: Boolean; override;
    procedure SetModus (Value: TMSItemModus); override;
    procedure Read (Key: LongInt; Query: TMSQuery); override;
    procedure Modify (Key: LongInt; Query: TMSQuery); override;
    property KanalList: TList read FKanal;
  public
    constructor Create(pDatabase: TDatabase); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Kanal [Index: Integer]: TMSNewKanal
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
    constructor Create(pDatabase: TDatabase); override;
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
    constructor Create(pDatabase: TDatabase); override;
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
    FTbAutoInc : TDbAutoInc;
    FDatabase  : TDatabase;
    procedure SetModus (Value: TMSItemModus);
    function GetModified: Boolean;
    procedure SetModified (Value: Boolean);
    function GetValid: Boolean;
    function GetMrgId: TMrgId;
    procedure SetMrgId (Value: TMrgId);
    procedure SetSectors (Value: TMSSectors);
    function GetDatabaseName: TFileName;
    procedure SetDatabaseName (Value: TFileName);
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
    function CheckMissingMrgKanaele: integer;
    procedure CopyMrgKanalToStaKanal (iKanalNrVon: integer);
    procedure CreateImpulsAnalogChannels (iKanalNrVon: integer);
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
               read GetDatabaseName write SetDatabaseName;
    property Modus: TMSItemModus
               read FModus write SetModus;
    property Sectors: TMSSectors
               read FSectors write SetSectors;
    property BeforeClear: TMSStdNotifyEvent
               read FBeforeClear write FBeforeClear;
    property AfterClear: TMSStdNotifyEvent
               read FAfterClear write FAfterClear;
  end;

  TMSListBox = class (TCustomListBox)
  private
    FOption: TMSListOption;
    FQuery: TMSQuery;
    FSysDataBase: string;
    FKennungLen: Integer;
    FMrgNameLen: Integer;
    function GetActive: Boolean;
    procedure SetActive (Value: Boolean);
    function GetDatabaseName: string;
    procedure SetDatabaseName (Value: string);
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
    property DatabaseName: string
               read GetDatabaseName write SetDatabaseName;
    property SysDatabase: string
               read FSysDataBase write FSysDataBase;
  end;

implementation

const

{ Werte für Kanaltyp }

CVImpuls = 'I';
CVAnalog = 'A';

{ Bereichsgrenzen }

CMrgNameLen = 20;  // vergrößert von 10 auf 20 wegen Veribox-Mini, 16.11.2004 WW
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
CRufnummerLen = 50; { WW 21.03.2007: geändert von 20 auf 50 für IEC-Geräteadresse }
CPasswortlen = 10;
CPasswortNrMin = 1;
CPasswortNrMax = 4;
CModemTypMin = 1;
CModemTypMax = 12;  { WW - Modemtyp 12 ist Modem-7E1 }
CLogPortMin = 0;  { GeDa - LogPort 0 ist 'Sx' }
CLogPortMax = MaxPort + 1;  // 11.05.2006: geändert von 16 auf 50
                            // 24.06.2013: nochmal geändert von 50 auf MaxPort + 1
CZaehlpunktLen = 40;
CEDISKennzifferLen = 40;
CIPAdr_PortLen = 40;

COrgFaktorMin = 0.001;
COrgFaktorMax = 100000;

resourcestring
  S_KeinLesen           = 'Lesen nicht erlaubt';
  S_KeinEinfuegen       = 'Einfügen nicht erlaubt';
  S_KeinAendern         = 'Ändern nicht erlaubt';
  S_KeinLoeschen        = 'Löschen nicht erlaubt';
  S_DatenNichtGeaendert = 'Daten wurden nicht geändert';
  S_DatenUngueltig      = 'Daten ungültig';

  S_KennungVorhanden  = 'Kennung schon vorhanden !';
  S_DbBeschaedigt     = 'Tabelle beschädigt';
  S_PWNrUngueltig     = 'Passwortnummer ungültig';
  S_KanalFehlt        = 'Kanal nicht vorhanden';
  S_KanalIndexFehler  = 'Kanal-Index ungültig';
  S_MrgIdFehlt        = 'MrgId nicht vorhanden';

  S_AllgDatenFehlen  = 'Keine allgemeine Daten vorhanden';
  S_InfoDatenFehlen  = 'Keine Info-Daten vorhanden';
  S_KonvDatenFehlen  = 'Keine Konv-Daten vorhanden';
  S_DSfGDatenFehlen  = 'Keine DSfG-Daten vorhanden';
  S_DFUEDatenFehlen  = 'Keine DFÜ-Daten vorhanden';
  S_AutoDatenFehlen  = 'Keine Automatik-Daten vorhanden';
  S_MDEDatenFehlen   = 'Keine MDE-Daten vorhanden';
  S_KanalDatenFehlen = 'Keine Kanal-Daten vorhanden';


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
        SQL.Add (C_Sta_MrgId + ', ' + C_Sta_StationsName);
        SQL.Add ('FROM');
        SQL.Add (CDBSta);
        SQL.Add ('WHERE');
        SQL.Add (C_Sta_Aktiv + ' = 0');
      end;
    msl_SKAll:
      begin
        SQL.Add ('SELECT');
        SQL.Add (C_Sta_MrgId + ', ' + C_Sta_StationsName + ', ' + C_Sta_Kennung);
        SQL.Add ('FROM');
        SQL.Add (CDBSta);
        SQL.Add ('WHERE');
        SQL.Add (C_Sta_Aktiv + '= 0');
      end;
    msl_SKNAll:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('A.' + C_Sta_MrgId + ', A.' + C_Sta_StationsName +
          ', A.' + C_Sta_Kennung + ', Def.' + C_MrgDef_MrgName);
        SQL.Add ('FROM');
        SQL.Add (CDBSta + 'A ,');
        SQL.Add (CTblMrgDef + ' Def');
        SQL.Add ('WHERE');
        SQL.Add ('A.' + C_Sta_Aktiv + ' = 0 AND A.' +
          C_Sta_MrgTyp + ' = Def.' + C_MrgDef_MrgTyp);
      end;
    msl_SAuto:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('A.' + C_Sta_MrgId + ', A.' + C_Sta_StationsName);
        SQL.Add ('FROM');
        SQL.Add (CDBSta + ' A, ' + CDBStaAuto + ' B');
        SQL.Add ('WHERE A.' + C_Sta_Aktiv + ' = 0');
        Sql.Add ('AND B.' + C_StaAuto_MrgId + ' = A.' + C_Sta_MrgId);
      end;
    msl_SKAuto:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('A.' + C_Sta_MrgId + ', A' + C_Sta_StationsName +
          ', A.' + C_Sta_StationsName);
        SQL.Add ('FROM');
        SQL.Add (CDBSta + ' A, ' + CDBStaAuto  + ' B');
        SQL.Add ('WHERE A.' + C_Sta_Aktiv + ' = 0');
        SQL.Add ('AND B.' + C_StaAuto_MrgId + ' = A.' + C_Sta_MrgId);
      end;
    msl_SKNAuto:
      begin
        SQL.Add ('SELECT');
        SQL.Add ('A.' + C_Sta_MrgId + ', A.' + C_Sta_StationsName +
          ', A.' + C_Sta_Kennung + ', Def.' + C_MrgDef_MrgName);
        SQL.Add ('FROM');
        SQL.Add (CDBSta + ' A, ' + CDBStaAuto + ' B,');
        SQL.Add (CTblMrgDef + ' Def');
        SQL.Add ('WHERE A.' + C_Sta_Aktiv + ' = 0');
        SQL.Add ('AND B.' + C_StaAuto_MrgId + ' = A.' + C_Sta_MrgId);
        SQL.Add ('AND A.' + C_Sta_MrgTyp + ' = Def.' + C_MrgDef_MrgTyp);
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
procedure TMSSQL.SetMrgStamm (Value: string);
{----------------------------------------------}
begin
  if Value <> FDbMrgStamm then
  begin
    FDbMrgStamm := Value;
    CreateSQL;
  end;
end;

{--------------------------------------------}
procedure TMSSQL.SetMrgSys (Value: string);
{--------------------------------------------}
begin
  if Value <> FDbMrgSys then
  begin
    FDbMrgSys := Value;
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
    try
      AddSQLSelect (StringList);
      AddSQLFrom (StringList);
      AddSQLWhere (StringList);
      AddSQLOrder (StringList);
      Query.SQL.Clear;
      Query.SQL.AddStrings (StringList);
    finally
      StringList.Free;
    end;
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
  try
    for i := Low (TMSSQLField) to High (TMSSQLField) do
    begin
      if (i in FSQLFields) then
        with FieldNames do
          case i of
            msf_MrgId:        Add (CDBSta + '.' + C_Sta_MrgId);
            msf_MrgName:      Add (CTblMrgDef + '.' + C_MrgDef_MrgName);
            msf_Kennung:      Add (CDBSta + '.' + C_Sta_Kennung);
            msf_Stationsname: Add (CDBSta + '.' + C_Sta_StationsName);
            msf_Zahlpunkt:    Add (CDBSta + '.' + C_Sta_Zaehlpunkt);
          end;
    end;
    for j := 0 to FieldNames.Count - 1 do
    begin
      if j > 0 then
        Strings.Add (', ' + FieldNames [j])
      else
        Strings.Add (FieldNames [j]);
    end;
  finally
    FieldNames.Free;
  end;
end;

{----------------------------------------------}
procedure TMSSQL.AddSQLFrom (Strings: TStrings);
{----------------------------------------------}
begin
  Strings.Add ('FROM');
  Strings.Add (CDBSta);
  if (msc_Auto in FSQLConditions) then Strings.Add (', ' + CDBStaAuto);
  if (msf_MrgName in FSQLFields) then Strings.Add (', ' + CTblMrgDef);
end;

{-----------------------------------------------}
procedure TMSSQL.AddSQLWhere (Strings: TStrings);
{-----------------------------------------------}
begin
  Strings.Add ('WHERE');
  Strings.Add (CDBSta + '.' + C_Sta_Aktiv + '= 0');
  if (msf_MrgName in FSQLFields) then
  begin
    Strings.Add ('AND');
    Strings.Add (CDBSta + '.' + C_Sta_MrgTyp + '=' +
      CTblMrgDef + '.' + C_MrgDef_MrgTyp);
  end;
  if (msc_Auto in FSQLConditions) then
  begin
    Strings.Add ('AND');
    Strings.Add (CDBSta + '.' + C_Sta_MrgId + '=' +
      CDBStaAuto + '.' + C_StaAuto_MrgId);
  end;
end;

{-----------------------------------------------}
procedure TMSSQL.AddSQLOrder (Strings: TStrings);
{-----------------------------------------------}
begin
  Strings.Add ('ORDER BY');
  case FSQLOrder of
    mso_Kennung:      Strings.Add (C_Sta_Kennung);
    mso_StationsName: Strings.Add (C_Sta_StationsName);
    mso_MrgName:      Strings.Add (C_MrgDef_MrgName + ', ' + C_Sta_Kennung);
    mso_Zaehlpunkt:   Strings.Add (C_Sta_Zaehlpunkt);
  end;
end;

{---------------------------------------------}
constructor TMSSQL.Create (AOwner: TComponent);
{---------------------------------------------}
begin
  inherited Create (AOwner);
  FDbMrgStamm := '';
  FDbMrgSys := '';
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
    raise EMSException.Create (S_KeinLesen);
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
    raise EMSException.Create (S_KeinAendern);
  if not Modified then
    raise EMSException.Create (S_DatenNichtGeaendert);
  if not Valid then
    raise EMSException.Create (S_DatenUngueltig);
end;

{ Überprüft BetriebsModus, die Gültigkeit der Datenfelder und
  ob mindestens ein Datenfeld geändert worden ist. Liefert als
  Ergebnis die StationsId wieder zurück }
{--------------------------------------------------------------------}
function TMSItemBasic.Append (Key: LongInt; Query: TMSQuery): LongInt;
{--------------------------------------------------------------------}
begin
  if not (FModus in [msm_Create, msm_ReadWrite]) then
    raise EMSException.Create (S_KeinEinfuegen);
  if not Modified then
    raise EMSException.Create (S_DatenNichtGeaendert);
  if not Valid then
    raise EMSException.Create (S_DatenUngueltig);
  Result := Key;

  if (not Assigned(FTbAutoInc)) then FTbAutoInc := TDbAutoInc.Create(FDatabase);
end;

{ Überprüft Betriebsmodus und Gültigkeit der Daten }
{------------------------------------------------------------}
procedure TMSItemBasic.Delete (Key: LongInt; Query: TMSQuery);
{------------------------------------------------------------}
begin
  if not (FModus in [msm_ReadWrite]) then
    raise EMSException.Create (S_KeinLoeschen);
  if not Valid then
    raise EMSException.Create (S_DatenUngueltig);
end;

{ Konstruktor: Voreingestellt ist reiner Lesezugriff }
{------------------------------}
constructor TMSItemBasic.Create(pDatabase: TDatabase);
{------------------------------}
begin
  inherited Create;
  FDatabase := pDatabase;
  FModus := msm_Read;
  FTbAutoInc := nil;
end;

{------------------------------}
destructor TMSItemBasic.Destroy;
{------------------------------}
begin
  if (Assigned(FTbAutoInc)) then FreeAndNil(FTbAutoInc);

  inherited Destroy;
end;

{-------------}
{ TMSItemAllg }
{-------------}

{ Überprüft, ob Kennung schon in Tabelle vorhanden ist }
{ Übergabe: Kennung
            optional:
              MrgId (wenn > -1, wird die zur MrgId gehörende Kennung nicht
                     in die Prüfung miteinbezogen) }
{------------------------------------------------------------------------}
procedure TMSItemAllg.CheckIndex (const Kennung: string; Query: TMSQuery;
  MrgId: integer = -1);
{------------------------------------------------------------------------}
begin
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('SELECT');
    Add (C_Sta_Kennung);
    Add ('FROM');
    Add (CDBSta);                                         
    Add ('WHERE');
    Add (C_Sta_Kennung + ' = :Kennung AND ' + C_Sta_Aktiv + ' = 0');
    if MrgId > -1 then
      Add (' AND ' + C_Sta_MrgId + ' <> :MrgId');  // 14.08.2012, WW
  end;
  with Query do
  begin
    ParamByName ('Kennung').asString := Kennung;
    if MrgId > -1 then
      ParamByName ('MrgId').asInteger := MrgId;  // 14.08.2012, WW
    Open;
    if RecordCount > 0 then
      raise EMSKennungVorhandenException.Create (S_KennungVorhanden);
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
            isWZ_SZ.Modified or
            ArchivdatenTyp.Modified or
            Zaehlpunkt.Modified;
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
  ArchivdatenTyp.Modified := Value;
  Zaehlpunkt.Modified := Value;
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
  // Zählpunkt darf leer sein, kein Pflichtfeld
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
  FArchivdatenTyp.Clear;
  FZaehlpunkt.Clear;
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
    Add (C_Sta_MrgTyp + ', ' + C_Sta_Kennung + ', ');
    Add (C_Sta_StationsName + ', ' + C_Sta_TagesEnde + ',');
    Add (C_Sta_Prioritaet + ', ' + C_Sta_IsWZSZ + ',');
    Add (C_Sta_ArchDatenTyp + ', ' + C_Sta_Zaehlpunkt);
    Add ('FROM');
    Add (CDBSta);
    Add ('WHERE');
    Add (C_Sta_MrgId + ' = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    Open;
    if RecordCount = 1 then
    begin
      FMrgTyp.asInteger := FieldByName (C_Sta_MrgTyp).asInteger;
      FKennung.asString := FieldByName (C_Sta_Kennung).asString;
      FStationsName.asString := FieldByName (C_Sta_StationsName).asString;
      FTagesEnde.asInteger := FieldByName (C_Sta_TagesEnde).asInteger;

      if (FieldByName (C_Sta_Prioritaet).asInteger < CPrioritaetMin) or
         (FieldByName (C_Sta_Prioritaet).asInteger > CPrioritaetMax) then
        FPrioritaet.asInteger := CPrioritaetMax
      else
        FPrioritaet.asInteger := FieldByName (C_Sta_Prioritaet).asInteger;

      if (FieldByName (C_Sta_IsWZSZ).isNull) then FisWZ_SZ.asBoolean := False
      else FisWZ_SZ.asBoolean := FieldByName (C_Sta_IsWZSZ).asBoolean;

      if (FieldByName (C_Sta_ArchDatenTyp).isNull) then
        FArchivdatenTyp.asInteger := mrg_arch_LGZnorm
      else begin
        if (FieldByName (C_Sta_ArchDatenTyp).asInteger <> mrg_arch_LGZnorm) and
           (FieldByName (C_Sta_ArchDatenTyp).asInteger <> mrg_arch_LGZorig) then
          FPrioritaet.asInteger := mrg_arch_LGZnorm
        else
          FArchivdatenTyp.asInteger := FieldByName (C_Sta_ArchDatenTyp).asInteger;
      end;
      FZaehlpunkt.asString := FieldByName (C_Sta_Zaehlpunkt).asString;
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
  CheckIndex (FKennung.asString, Query, Key);  // 14.08.2012, WW
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add ('UPDATE');
    Add (CDBSta);
    Add ('SET');
    Add (C_Sta_StationsName + ' = :StationsName, ' + C_Sta_TagesEnde + ' = :TagesEnde,');
    Add (C_Sta_Prioritaet + ' = :Prioritaet, ' + C_Sta_IsWZSZ + ' = :isWZSZ,');
    Add (C_Sta_ArchDatenTyp + ' = :ArchDatenTyp, ' + C_Sta_Zaehlpunkt + ' = :Zaehlpunkt,');
    Add (C_Sta_Kennung + ' = :Kennung');  // 14.08.2012, WW
    Add ('WHERE');
    Add (C_Sta_MrgId + ' = :MrgId');
  end;
  Query.ParamByName ('StationsName').asString := FStationsName.asString;
  Query.ParamByName ('TagesEnde').asInteger := FTagesEnde.asInteger;
  Query.ParamByName ('Prioritaet').asSmallInt := FPrioritaet.asInteger;
  Query.ParamByName ('isWZSZ').asBoolean := FisWZ_SZ.asBoolean;
  Query.ParamByName ('ArchDatenTyp').asSmallInt := FArchivdatenTyp.asInteger;
  Query.ParamByName ('MrgId').asInteger := Key;
  Query.ParamByName ('Zaehlpunkt').asString := FZaehlpunkt.asString;
  Query.ParamByName ('Kennung').asString := F_RightTrunc(FKennung.asString,' ');
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
    Add (CDBSta);
    Add ('(' + C_Sta_Kennung + ', ' + C_Sta_Aktiv + ',');
    Add (C_Sta_MrgId + ', ' + C_Sta_MrgTyp + ',');
    Add (C_Sta_StationsName + ', ' + C_Sta_TagesEnde + ', ');
    Add (C_Sta_Prioritaet + ', ' + C_Sta_IsWZSZ + ', ');
    Add (C_Sta_ArchDatenTyp + ', ' + C_Sta_Zaehlpunkt + ')');
    Add ('VALUES');
    Add ('(:Kennung, :Aktiv, :MrgId, :MrgTyp, :StationsName, :TagesEnde, ' +
         ':Prioritaet, :isWZSZ, :ArchDatenTyp, :Zaehlpunkt)');
  end;
  Query.ParamByName ('Aktiv').asInteger := 0;
  Query.ParamByName ('MrgId').asInteger := FTbAutoInc.NextIndex[CDBSta];
  Query.ParamByName ('MrgTyp').asSmallInt := FMrgTyp.asInteger; { GeDa - asSmallInt }
  Query.ParamByName ('Kennung').asString := F_RightTrunc(FKennung.asString,' ');
  Query.ParamByName ('StationsName').asString := FStationsName.asString;
  Query.ParamByName ('TagesEnde').asInteger := FTagesEnde.asInteger;
  Query.ParamByName ('Prioritaet').asSmallInt := FPrioritaet.asInteger;
  Query.ParamByName ('isWZSZ').asBoolean := FisWZ_SZ.asBoolean;
  Query.ParamByName ('ArchDatenTyp').asSmallInt := FArchivdatenTyp.asInteger;
  Query.ParamByName ('Zaehlpunkt').asString := FZaehlpunkt.asString;
  Query.ExecSQL;
  Modified := False;
  with Query.SQL do
  begin
    Clear;
    Add ('SELECT');
    Add (C_Sta_MrgId);
    Add ('FROM');
    Add (CDBSta);
    Add ('WHERE');
    Add (C_Sta_Kennung  + ' = :Kennung AND ' + C_Sta_Aktiv + ' = 0');
  end;
  Query.ParamByName ('Kennung').asString := FKennung.asString;
  Query.Open;
  if Query.RecordCount = 1 then
    Result := Query.FieldByName (C_Sta_MrgId).asInteger
  else
    raise EMSException.Create (S_DbBeschaedigt);
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
    Add (CDBSta);
    Add ('SET');
    Add (C_Sta_Aktiv + ' = :MrgId');
    Add ('WHERE');
    Add (C_Sta_MrgId + ' = :MrgId');
  end;
  Query.ParamByName ('MrgId').asInteger := Key;
  Query.ExecSQL;
end;

{ Konstruktor, Legt Datenfelder an }
{-----------------------------}
constructor TMSItemAllg.Create(pDatabase: TDatabase);
{-----------------------------}
begin
  inherited Create(pDatabase);
  FMrgTyp := TIntegerData.CreateRange (CMrgTypMin, CMrgTypMax);
  FKennung := TXStringData.CreateRange (CKennungLen, ' ');
  FStationsName := TStringData.CreateRange (CStationsNameLen);
  FTagesEnde := TIntegerData.CreateRange (CTagesEndeMin, CTagesEndeMax);
  FPrioritaet := TIntegerData.CreateRange (CPrioritaetMin, CPrioritaetMax);
  FisWZ_SZ := TBooleanData.Create;
  FArchivdatenTyp := TIntegerData.CreateRange (mrg_arch_LGZnorm, mrg_arch_LGZorig);
  FZaehlpunkt := TStringData.CreateRange (CZaehlpunktLen);
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
  FArchivdatenTyp.Free;
  FZaehlpunkt.Free;
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

{ Liefert DSFG-Gerätetyp
  ab 22.01.2008: erweitert für Tritschler Multiplexer; WW }
{------------------------------------------}
function TMSItemDSFG.GetDSFGTyp: TMSDSFGTyp;
{------------------------------------------}
begin
  if not FHasData.asBoolean or FMasterId.Null then
    Result := DSFG_None
  else if (FMasterId.asInteger = 0) or
          (not FMrgId.Null and (FMasterId.asInteger = FMrgId.asInteger)) then
    Result := DSFG_Master
  else if (FMasterId.asInteger = CMasterId_MuxTritschlerSSU) then
    Result:= DSFG_Mux
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
    Add (C_StaDSfG_MrgId + ', ' + C_StaDSfG_Adresse + ', ' + C_StaDSfG_MasterId);
    Add ('FROM');
    Add (CDBStaDSFG);
    Add ('WHERE');
    Add (C_StaDSfG_MrgId + ' = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    Open;
    if RecordCount = 1 then
    begin
      FHasData.asBoolean := True;
      FAdresse.asString := FieldByName (C_StaDSfG_Adresse).asString;
      FMasterId.asInteger := FieldByName (C_StaDSfG_MasterId).asInteger;
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
      Add (CDBStaDSFG);
      Add ('(' + C_StaDSfG_MrgId + ', ' + C_StaDSfG_Adresse + ', ' +
           C_StaDSfG_MasterId+ ')');
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
    Add (CDBStaDSFG);
    Add ('WHERE');
    Add (C_StaDSfG_MrgId + ' = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    ExecSQL;
  end;
end;

{-----------------------------}
constructor TMSItemDSFG.Create(pDatabase: TDatabase);
{-----------------------------}
begin
  inherited Create(pDatabase);
  FMrgId := TIntegerData.CreateRange (CMrgIdMin, CMrgIdMax);
  FAdresse := TStringData.CreateRange (CAdresseLen);   
  FMasterId := TIntegerData.CreateRange (CMasterId_MuxTritschlerSSU, CMrgIdMax); 
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
    else raise EMSException.Create (S_PWNrUngueltig);
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
            FLogPort.Modified or
            FDfuModus.Modified or
            FIPAdr_Port.Modified;
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
  FDfuModus.Modified := Value;
  FIPAdr_Port.Modified := Value;
end;

{------------------------------------}
function TMSItemDFU.GetValid: Boolean;
{------------------------------------}
begin
  if FHasData.asBoolean then
  begin
    Result := not FPasswortNr.Null and
              not FModemTyp.Null and
              not FLogPort.Null and
              not FDfuModus.Null;

    if Result then
      case FDfuModus.asInteger of
        dfumode_WaehlVerb,
        dfumode_GPRS_WaehlVerb:
          Result := not FRufnummer.Null;

        dfumode_Netzwerk:  // 15.02.2013, WW
          Result := not FIPAdr_Port.Null;  // 24.06.2013, WW
      end;

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
  FDfuModus.Clear;
  FIPAdr_Port.Clear;
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
    Add (C_StaDfu_Rufnummer + ', ' + C_StaDfu_Passwort1 + ', ' +
         C_StaDfu_Passwort2 + ', ' + C_StaDfu_Passwort3 + ', ' +
         C_StaDfu_Passwort4 + ', ' + C_StaDfu_PasswortNr + ', ' +
         C_StaDfu_ModemTyp + ', ' + C_StaDfu_LogPort + ', ' +
         C_StaDfu_DfuModus + ', ' + C_StaDfu_IPAdr_Port);      
    Add ('FROM');
    Add (CDBStaDfu);
    Add ('WHERE');
    Add (C_StaDfu_MrgId + ' = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    Open;
    if RecordCount = 1 then
    begin
      FHasData.asBoolean := True;
      FRufnummer.asString := FieldByName (C_StaDfu_Rufnummer).asString;
      FPasswort1.asString := FieldByName (C_StaDfu_Passwort1).asString;
      FPasswort2.asString := FieldByName (C_StaDfu_Passwort2).asString;
      FPasswort3.asString := FieldByName (C_StaDfu_Passwort3).asString;
      FPasswort4.asString := FieldByName (C_StaDfu_Passwort4).asString;
      FPasswortNr.asInteger := FieldByName (C_StaDfu_PasswortNr).asInteger;
      FModemTyp.asInteger := FieldByName (C_StaDfu_ModemTyp).asInteger;
      FLogPort.asInteger := FieldByName (C_StaDfu_LogPort).asInteger;
      // Abwärtskompatibilität: Feld 'Dfumodus' leer = Wählverbindung
      if FieldByName (C_StaDfu_DfuModus).IsNull then
        FDfuModus.asInteger := dfumode_WaehlVerb
      else
        FDfuModus.asInteger := FieldByName (C_StaDfu_DfuModus).asInteger;
      FIPAdr_Port.asString := FieldByName (C_StaDfu_IPAdr_Port).asString;
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
      Add (CDBStaDfu);
      Add ('(' + C_StaDfu_MrgId + ', ' + C_StaDfu_Rufnummer + ', ' +
           C_StaDfu_Passwort1 + ', ' + C_StaDfu_Passwort2 + ', ' +
           C_StaDfu_Passwort3 + ', ' + C_StaDfu_Passwort4 + ', ' +
           C_StaDfu_PasswortNr + ', ' + C_StaDfu_ModemTyp + ', ' +
           C_StaDfu_LogPort + ', ' + C_StaDfu_DfuModus + ', ' +
           C_StaDfu_IPAdr_Port + ')');
      Add ('VALUES');
      Add ('(:MrgId, :Rufnummer, :Passwort1, :Passwort2, :Passwort3, ' +
           ':Passwort4, :PasswortNr, :ModemTyp, :LogPort, :DfuModus, ' +
           ':IPAdr_Port)');
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
      ParamByName ('DfuModus').asSmallInt := FDfuModus.asInteger;
      ParamByName ('IPAdr_Port').asString := FIPAdr_Port.asString;
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
    Add (CDBStaDfu);
    Add ('WHERE');
    Add (C_StaDfu_MrgId + ' = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    ExecSQL;
  end;
end;

{--------------------------------------------------}
constructor TMSItemDFU.Create(pDatabase: TDatabase);
{--------------------------------------------------}
begin
  inherited Create(pDatabase);
  FRufnummer := TStringData.CreateRange (CRufnummerLen);
  FPasswort1 := TStringData.CreateRange (CPasswortLen);
  FPasswort2 := TStringData.CreateRange (CPasswortLen);
  FPasswort3 := TStringData.CreateRange (CPasswortLen);
  FPasswort4 := TStringData.CreateRange (CPasswortLen);
  FPasswortNr := TIntegerData.CreateRange (CPasswortNrMin, CPasswortnrMax);
  FModemTyp := TIntegerData.CreateRange (CModemTypMin, CModemTypMax);
  FLogPort := TIntegerData.CreateRange (CLogPortMin, CLogPortMax);
  FDfuModus := TIntegerData.CreateRange (dfumode_WaehlVerb, dfumode_GPRS_WaehlVerb);
  FIPAdr_Port := TStringData.CreateRange (CIPAdr_PortLen);
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
  FDfuModus.Free;
  FIPAdr_Port.Free;
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
    Add (C_StaAuto_Automatik + ', ' + C_StaAuto_Intervall + ', ' +
         C_StaAuto_Zeitsynchronisation + ', ' + C_StaAuto_Rundpufferreset);
    Add ('FROM');
    Add (CDBStaAuto);
    Add ('WHERE');
    Add (C_StaAuto_MrgId + ' = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    Open;
    if RecordCount = 1 then
    begin
      FHasData.asBoolean := True;
      FAutomatik.asBoolean := FieldByName (C_StaAuto_Automatik).asBoolean;
      FIntervall.asInteger := FieldByName (C_StaAuto_Intervall).asInteger;
      FZeitsynchronisation.asBoolean :=
        FieldByName (C_StaAuto_Zeitsynchronisation).asBoolean;
      FRundpufferreset.asBoolean :=
        FieldByName (C_StaAuto_Rundpufferreset).asBoolean;
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
      Add (CDBStaAuto);
      Add ('(' + C_StaAuto_MrgId + ', ' + C_StaAuto_Automatik + ',');
      Add (C_StaAuto_Intervall+ ', ' + C_StaAuto_Zeitsynchronisation + ',');
      Add (C_StaAuto_Rundpufferreset + ')');
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
    Add (CDBStaAuto);
    Add ('WHERE');
    Add (C_StaAuto_MrgId + ' = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    ExecSQL;
  end;
end;

{-----------------------------}
constructor TMSItemAuto.Create(pDatabase: TDatabase);
{-----------------------------}
begin
  inherited Create(pDatabase);
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
    Add (C_StaMDE_MrgId);
    Add ('FROM');
    Add (CDBStaMDE);
    Add ('WHERE');
    Add (C_StaMDE_MrgId + ' = :MrgId');
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
      Add (CDBStaMDE);
      Add ('(' + C_StaMDE_MrgId + ')');
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
    Add (CDBStaMDE);
    Add ('WHERE');
    Add (C_StaMDE_MrgId + ' = :MrgId');
  end;
  with Query do
  begin
    ParamByName ('MrgId').asInteger := Key;
    ExecSQL;
  end;
end;

{-----------------------------}
constructor TMSItemMDE.Create(pDatabase: TDatabase);
{-----------------------------}
begin
  inherited Create(pDatabase);
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
    FFaktor.asInteger := FieldByName (C_StaImp_Faktor).asInteger;
    FTeiler.asInteger := FieldByName (C_StaImp_Teiler).asInteger;
    FOrgFaktor.asFloat := FieldByName (C_StaImp_OrgFaktor).asFloat;
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
      SQL.Add ('UPDATE ' + CDBStaImp);
      SQL.Add ('SET');
      SQL.Add (C_StaImp_Faktor + ' = :Faktor, ' +
               C_StaImp_Teiler  + ' = :Teiler,');
      SQL.Add (C_StaImp_OrgFaktor + ' = ' + FOrgFaktor.asString);
      SQL.Add ('WHERE');
      SQL.Add (C_StaImp_KanalId + ' = ' + IntToStr (Key));
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
constructor TMSNewImpulsKanal.Create(pDatabase: TDatabase);
{-----------------------------------}
begin
  inherited Create(pDatabase);
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
    FMessBereichMin.asFloat := FieldByName (C_StaAna_MessBereichMin).asFloat;
    FMessBereichMax.asFloat := FieldByName (C_StaAna_MessBereichMax).asFloat;
    FStromBereich.asInteger := FieldByName (C_StaAna_StromBereich).asInteger;
    FOffset.asBoolean := FieldByName (C_StaAna_Offset).asBoolean;
    FAufzMin.asInteger := FieldByName (C_StaAna_AufzMin).asInteger;
    FAufzMax.asInteger := FieldByName (C_StaAna_AufzMax).asInteger;
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
      SQL.Add ('UPDATE ' + CDBStaAna);
      SQL.Add ('SET');
      SQL.Add (C_StaAna_MessBereichMin + ' = ' + FMessBereichMin.asString + ',');
      SQL.Add (C_StaAna_MessbereichMax + ' = ' + FMessBereichMax.asString + ',');
      SQL.Add (C_StaAna_StromBereich + ' = :StromBereich,');
      Sql.Add (C_StaAna_Offset + ' = :Offset,');
      SQL.Add (C_StaAna_AufzMin + ' = :AufzMin, ' + C_StaAna_AufzMax + ' = :AufzMax');
      SQL.Add ('WHERE');
      SQL.Add (C_StaAna_KanalId + ' = ' + IntToStr (Key));
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
constructor TMSNewAnalogKanal.Create(pDatabase: TDatabase);
{-----------------------------------}
begin
  inherited Create(pDatabase);
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
    KTypUnknown: FKanalTyp.asString := 'U';  // 21.07.2008, WW (vorher N)
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
    raise EMSException.Create (S_KanalFehlt);
  Result := FImpuls;
end;

{------------------------------------------------}
function TMSNewKanal.GetAnalog: TMSNewAnalogKanal;
{------------------------------------------------}
begin
  if not assigned (FAnalog) then
    raise EMSException.Create (S_KanalFehlt);
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
            FEinheit.Modified or
            FOriKanalTyp.Modified or
            FOriDatenTyp.Modified or
            FZaehlpunkt.Modified or
            FEDIS_Kennziffer.Modified;
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
  FOriKanalTyp.Modified := Value;
  FOriDatenTyp.Modified := Value;
  FZaehlpunkt.Modified := Value;
  FEDIS_Kennziffer.Modified := Value;
  if Assigned (FImpuls) then
    FImpuls.SetModified (Value);
  if Assigned (FAnalog) then
    FAnalog.SetModified (Value);
end;

{-------------------------------------}
function TMSNewKanal.GetValid: Boolean;
{-------------------------------------}
{ 12.06.2006; WW: Kanalname und Einheit leer jetzt erlaubt }
begin
  Result := not FMrgKanal.Null and
//            not FKanalName.Null and
            not FAktiv.Null and
            not FEinstellbar.Null and
            not FKanaltyp.Null and
            not FKontroll.Null and
            not FEingang.Null and
            not FKommastellen.Null; // and
//            not FEinheit.Null;
// Zählpunkt, EDIS-Kennziffer leer erlaubt, keine Pflichtfelder
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
  FOriKanalTyp.Clear;
  FOriDatenTyp.Clear;
  FZaehlpunkt.Clear;
  FEDIS_Kennziffer.Clear;
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
    FMrgKanal.asInteger := FieldByName (C_StaKanal_MrgKanal).asInteger;
    FKanalId.asInteger := FieldByName (C_StaKanal_KanalId).asInteger;
    FAktiv.asBoolean := FieldByName (C_StaKanal_Aktiv).asBoolean;
    FKanalName.asString := FieldByName (C_StaKanal_Kanalname).asString;
    FKanalTyp.asString := FieldByName (C_StaKanal_Kanaltyp).asString;
    FEinstellbar.asBoolean := FieldByName (C_StaKanal_Einstellbar).asBoolean;
    FKontroll.asBoolean := FieldByName (C_StaKanal_Kontroll).asBoolean;
    FEingang.asBoolean := FieldByName (C_StaKanal_Eingang).asBoolean;
    FKommastellen.asInteger := FieldByName (C_StaKanal_Kommastellen).asInteger;
    FEinheit.asString := FieldByName (C_StaKanal_Einheit).asString;
    FOriKanalTyp.asString := FieldByName (C_StaKanal_KanalTyp_ori).asString;
    FOriDatenTyp.asInteger := FieldByName (C_StaKanal_DatenTyp_ori).asInteger;
    FZaehlpunkt.asString := FieldByName (C_StaKanal_Zaehlpunkt).asString;
    FEDIS_Kennziffer.asString := FieldByName (C_StaKanal_EDISKennziffer).asString;
    if not FieldByName (CFNImpulsKanalId).isNull then
    begin
      FImpuls := TMSNewImpulsKanal.Create(Database);
      FImpuls.Modus := FModus;
      FImpuls.Read (FKanalId.asInteger, Query);
    end;
    if not FieldByName (CFNAnalogKanalId).isNull then
    begin
      FAnalog := TMSNewAnalogKanal.Create(Database);
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
      SQL.Add ('UPDATE ' + CDBStaKanal);
      SQL.Add ('SET');
      SQL.Add (C_StaKanal_Aktiv + ' = :Aktiv, ' +
               C_StaKanal_KanalName + ' = :KanalName,');
      SQL.Add (C_StaKanal_KanalTyp + ' = :KanalTyp,');
      SQL.Add (C_StaKanal_Kommastellen + ' = :Kommastellen, ' +
               C_StaKanal_Einheit + ' = :Einheit,');
      SQL.Add (C_StaKanal_KanalTyp_ori + ' = :OriKanalTyp, ' +
               C_StaKanal_DatenTyp_ori + ' = :OriDatenTyp,');
      SQL.Add (C_StaKanal_Zaehlpunkt + ' = :Zaehlpunkt, ' + 
               C_StaKanal_EDISKennziffer + ' = :EDIS_Kennziffer');
      SQL.Add ('WHERE');
      SQL.Add (C_StaKanal_MrgId + ' = ' + IntToStr (Key));
      SQL.Add ('AND');
      SQL.Add (C_StaKanal_MRGKanal + ' = ' + FMRGKanal.asString);
      ParamByName ('Aktiv').asBoolean := FAktiv.asBoolean;
      ParamByName ('KanalName').asString := FKanalName.asString;
      ParamByName ('KanalTyp').asString := FKanalTyp.asString;
      ParamByName ('Kommastellen').asSmallInt := FKommastellen.asInteger; // GeDa
      ParamByName ('Einheit').asString := FEinheit.asString;
      ParamByName ('OriKanalTyp').asString := FOriKanalTyp.asString;
      ParamByName ('OriDatenTyp').asSmallInt := FOriDatenTyp.asInteger; // GeDa
      ParamByName ('Zaehlpunkt').asString := FZaehlpunkt.asString;
      ParamByName ('EDIS_Kennziffer').asString := FEDIS_Kennziffer.asString;
      ExecSQL;
    end;
    Modified := False;
  end;
end;

{-----------------------------}
constructor TMSNewKanal.Create(pDatabase : TDatabase);
{-----------------------------}
begin
  inherited Create(pDatabase);
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
  FOriKanalTyp := TStringData.CreateRange (2);
  FOriDatenTyp := TIntegerData.CreateRange (0,9999);
  FZaehlpunkt := TStringData.CreateRange (CZaehlpunktLen);
  FEDIS_Kennziffer := TStringData.CreateRange (CEDISKennzifferLen);
  FImpuls := nil;
  FAnalog := nil;
end;

{-----------------------------}
destructor TMSNewKanal.Destroy;
{-----------------------------}
begin
  FMrgKanal.Free;
  FKanalId.Free;
  FKanalName.Free;
  FAktiv.Free;
  FEinstellbar.Free;
  FKanalTyp.Free;
  FKontroll.Free;
  FEingang.Free;
  FKommastellen.Free;
  FEinheit.Free;
  FOriKanalTyp.Free;
  FOriDatenTyp.Free;
  FZaehlpunkt.Free;
  FEDIS_Kennziffer.Free;
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
    raise EMSException.Create (S_KanalIndexFehler);
end;

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
    SQL.Add ('A.' + C_StaKanal_MrgKanal + ', A.' + C_StaKanal_KanalId + ',');
    Sql.Add ('A.' + C_StaKanal_Aktiv + ',A.' + C_StaKanal_KanalName+ ',');
    SQL.Add ('A.' + C_StaKanal_KanalTyp + ', A.' + C_StaKanal_Einstellbar + ',');
    SQL.Add ('A.' + C_StaKanal_Kontroll + ', A.' + C_StaKanal_Eingang + ',');
    SQL.Add ('A.' + C_StaKanal_Kommastellen + ', A.' + C_StaKanal_Einheit + ',');
    SQL.Add ('A.' + C_StaKanal_KanalTyp_ori + ', A.' + C_StaKanal_DatenTyp_ori + ',');
    SQL.Add ('A.' + C_StaKanal_Zaehlpunkt + ', A.' + C_StaKanal_EDISKennziffer + ', ');
    SQL.Add ('B.' + C_StaImp_KanalId + ' ' + CFNImpulsKanalId + ', B.' + C_StaImp_Faktor + ',');
    SQL.Add ('B.' + C_StaImp_Teiler + ', B.' + C_StaImp_OrgFaktor + ',');
    SQL.Add ('C.' + C_StaAna_KanalId + ' ' + CFNAnalogKanalId + ',');
    SQL.Add ('C.' + C_StaAna_MessBereichMin + ', C.' + C_StaAna_MessBereichMax + ',');
    SQL.Add ('C.' + C_StaAna_Strombereich + ', C.' + C_StaAna_Offset + ',');
    SQL.Add ('C.' + C_StaAna_AufzMin + ', C.' + C_StaAna_AufzMax);
    SQL.Add ('FROM');
    SQL.Add (CDBStaKanal + ' A LEFT JOIN ' + CDBStaImp + ' B ON A.' + C_StaKanal_KanalId + ' = B.' + C_StaImp_KanalId);
    SQL.Add ('         LEFT JOIN ' + CDBStaAna + ' C ON A.' + C_StaKanal_KanalId + ' = C.' + C_StaAna_KanalId);
    SQL.Add ('WHERE');
    SQL.Add ('A.' + C_StaKanal_MrgID + ' = ' + IntToStr (Key));
    Open;
    First;
    while not Eof do
    begin
      MSKanal := TMSNewKanal.Create(Database);
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
constructor TMSNewKanalList.Create(pDatabase: TDatabase);
{---------------------------------}
begin
  inherited Create(pDatabase);
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
    SQL.Add (C_MrgDef_MrgName);
    SQL.Add ('FROM');
    SQL.Add (CTblMrgDef);
    SQL.Add ('WHERE');
    SQL.Add (C_MrgDef_MrgTyp + ' = ' + IntToStr (Key));
    Open;
    if RecordCount = 1 then
    begin
      FMrgName.asString := FieldByName (C_MrgDef_MrgName).asString;
    end;
  end;
  Modified := False;
end;

{-----------------------------}
constructor TMSItemInfo.Create(pDatabase: TDatabase);
{-----------------------------}
begin
  inherited Create(pDatabase);
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
    SQL.Add (C_MrgKonv_Meldungsgruppe);
    SQL.Add ('FROM');
    SQL.Add (CTblMrgKonv);
    SQL.Add ('WHERE');
    SQL.Add (C_MrgKonv_MrgTyp + ' = ' + IntToStr (Key));
    Open;
    if RecordCount = 1 then
    begin
      FMeldungsgruppe.asString := FieldByName (C_MrgKonv_Meldungsgruppe).asString;
    end;
  end;
  Modified := False;
end;

{-----------------------------}
constructor TMSItemKonv.Create(pDatabase: TDatabase);
{-----------------------------}
begin
  inherited Create(pDatabase);
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
    raise EMSException.Create (S_MrgIdFehlt)
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
          msi_Allgemein    : FMSItem [i] := TMSItemAllg.Create(FDatabase);
          msi_Info         : FMSItem [i] := TMSItemInfo.Create(FDatabase);
          msi_Konv         : FMSItem [i] := TMSItemKonv.Create(FDatabase);
          msi_DSFG         : FMSItem [i] := TMSItemDSFG.Create(FDatabase);
          msi_DFU          : FMSItem [i] := TMSItemDFU.Create(FDatabase);
          msi_Automatik    : FMSItem [i] := TMSItemAuto.Create(FDatabase);
          msi_MDE          : FMSItem [i] := TMSItemMDE.Create(FDatabase);
          msi_Kanal        : FMSItem [i] := TMSNewKanalList.Create(FDatabase);
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
var
  i : integer;
begin
  for i := 0 to Sessions.Count-1 do begin
    FDatabase := Sessions[i].FindDatabase(Value);
    if (Assigned(FDatabase)) then Break;
  end;

  FQuery.DatabaseName := Value;
  FQuerySys.DatabaseName := Value;
  if (Assigned(FDatabase)) then begin
    FQuery.SessionName := FDatabase.SessionName;
    FQuerySys.SessionName := FDatabase.SessionName;
  end;
end;

{-----------------------------------------------}
function TMSStammdaten.GetAllgemein: TMSItemAllg;
{-----------------------------------------------}
begin
  if not (msi_Allgemein in FSectors) then
    raise EMSException.Create (S_AllgDatenFehlen);
  result := FMSItem [msi_Allgemein] as TMSItemAllg;
end;

{------------------------------------------}
function TMSStammdaten.GetInfo: TMSItemInfo;
{------------------------------------------}
begin
  if not (msi_Info in FSectors) then
    raise EMSException.Create (S_InfoDatenFehlen);
  result := FMSItem [msi_Info] as TMSItemInfo;
end;

{------------------------------------------}
function TMSStammdaten.GetKonv: TMSItemKonv;
{------------------------------------------}
begin
  if not (msi_Konv in FSectors) then
    raise EMSException.Create (S_KonvDatenFehlen);
  result := FMSItem [msi_Konv] as TMSItemKonv;
end;

{------------------------------------------}
function TMSStammdaten.GetDSFG: TMSItemDSFG;
{------------------------------------------}
begin
  if not (msi_DSFG in FSectors) then
    raise EMSException.Create (S_DSfGDatenFehlen);
  result := FMSItem [msi_DSFG] as TMSItemDSFG;
end;

{----------------------------------------}
function TMSStammdaten.GetDFU: TMSItemDFU;
{----------------------------------------}
begin
  if not (msi_DFU in FSectors) then
    raise EMSException.Create (S_DFUEDatenFehlen);
  result := FMSItem [msi_DFU] as TMSItemDFU;
end;

{-----------------------------------------------}
function TMSStammdaten.GetAutomatik: TMSItemAuto;
{-----------------------------------------------}
begin
  if not (msi_Automatik in FSectors) then
    raise EMSException.Create (S_AutoDatenFehlen);
  result := FMSItem [msi_Automatik] as TMSItemAuto;
end;

{-----------------------------------------------}
function TMSStammdaten.GetMDE: TMSItemMDE;
{-----------------------------------------------}
begin
  if not (msi_MDE in FSectors) then
    raise EMSException.Create (S_MDEDatenFehlen);
  result := FMSItem [msi_MDE] as TMSItemMDE;
end;

{-----------------------------------------------}
function TMSStammdaten.GetKanal: TMSNewKanalList;
{-----------------------------------------------}
begin
  if not (msi_Kanal in FSectors) then
    raise EMSException.Create (S_KanalDatenFehlen);
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

{-----------------------------------------------------}
function TMSStammdaten.CheckMissingMrgKanaele: integer;
{-----------------------------------------------------}
{ Prüft, ob Stammsatz weniger Kanäle enthält als in MrgKanal.db für MRG-Typ
  definiert;
  Rückgabe:   0: Alle Kanäle in Stammsatz vorhanden
            > 0: Kanalnummer des ersten fehlenden Kanals }
var
  QueryKanal: TQuery;

begin
  Result:=0;  // Default: Alle Kanäle vorhanden
  QueryKanal := TQuery.Create (nil);
  try
    QueryKanal.DatabaseName := FQuerySys.DataBaseName;
    QueryKanal.SessionName := FQuerySys.SessionName;
    with QueryKanal do
    begin
      SQL.Clear;
      SQL.Add ('SELECT ' + C_MrgKanal_MrgKanal);
      SQL.Add ('FROM ' + CDBMrgKanal);
      SQL.Add ('WHERE (' + C_MrgKanal_MrgTyp + ' = ' + Allgemein.MrgTyp.AsString + ')');
      SQL.Add ('AND');
      SQL.Add ('(' + C_MrgKanal_MrgKanal + ' NOT IN');
      SQL.Add ('  (SELECT ' + C_StaKanal_MrgKanal);
      SQL.Add ('   FROM ' + CDBStaKanal);
      SQL.Add ('   WHERE ' + C_StaKanal_MrgId + ' = ' + IntToStr (MrgId) + ')');
      SQL.Add (')');
      Open;
      if not Eof then
        Result:=QueryKanal.FieldByName (C_MrgKanal_MrgKanal).asInteger;  // Erste fehlende Kanalnummer
    end;
  finally
    QueryKanal.Free;
  end;
end;

{--------------------------------------------------------------------}
procedure TMSStammdaten.CopyMrgKanalToStaKanal (iKanalNrVon: integer);
{--------------------------------------------------------------------}
{ Kopiert Stammdatenkanäle aus MrgKanal.db nach StaKanal.db;
  Übergabe:   0: alle Kanäle
            > 0: Kanalnummer, ab der kopiert werden soll }
var
  QueryKanal: TQuery;
  teststr: string;
begin
  QueryKanal := TQuery.Create (nil);
  try
    QueryKanal.DatabaseName := FQuerySys.DataBaseName;
    QueryKanal.SessionName := FQuerySys.SessionName;
    with QueryKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add (C_MrgKanal_MrgKanal + ', ' + C_MrgKanal_KanalId + ',');
      Sql.Add (C_MrgKanal_KanalName + ', ' + C_MrgKanal_KanalTyp + ',');
      SQL.Add (C_MrgKanal_Einstellbar + ', ' + C_MrgKanal_Kontroll + ',');
      Sql.Add (C_MrgKanal_Eingang + ',' + C_MrgKanal_Einheit + ',');
      SQL.Add (C_MrgKanal_Kommastellen + ', ' + C_MrgKanal_KanalTyp_ori + ',');  // 22.07.2005, WW
      SQL.Add (C_MrgKanal_DatenTyp_ori);
      SQL.Add ('FROM ' + CDBMrgKanal);
      SQL.Add ('WHERE ' + C_MrgKanal_MrgTyp + ' = ' + Allgemein.MrgTyp.AsString);
      if iKanalNrVon > 0 then  // 30.09.2013, WW
        SQL.Add ('AND ' + C_MrgKanal_MrgKanal + ' >= ' + IntToStr (iKanalNrVon));
      Open;
      First;
    end;
    with TQuery.Create(nil) do   // 27.09.2006
    try
      DatabaseName := FQuerySys.DataBaseName;
      SessionName := FQuerySys.SessionName;
      SQL.Add ('INSERT INTO ' + CDBStaKanal);
      SQL.Add ('(' + C_StaKanal_MrgId + ', ' + C_StaKanal_MrgKanal + ',');
      Sql.Add (C_StaKanal_KanalId + ',');
      Sql.Add (C_StaKanal_Aktiv + ', ' + C_StaKanal_KanalName + ',');
      SQL.Add (C_StaKanal_KanalTyp + ', ' + C_StaKanal_Einstellbar + ',');
      Sql.Add (C_StaKanal_Kontroll + ', ' + C_StaKanal_Eingang + ',');
      Sql.Add (C_StaKanal_Einheit + ', ' + C_StaKanal_Kommastellen + ',');
      Sql.Add (C_StaKanal_KanalTyp_ori + ',' + C_StaKanal_DatenTyp_ori + ')');  // 22.07.2005, WW
      SQL.Add ('VALUES');
      SQL.Add ('(' + IntToStr (MrgId) + ',');
      SQL.Add (':MrgKanal, :KanalId, :Aktiv, :KanalName,');
      SQL.Add (':KanalTyp, :Einstellbar, :Kontroll, :Eingang,');
      SQL.Add (':Einheit, :Kommastellen, :KanalTypOri, :DatenTypOri)');  // 22.07.2005, WW
      Prepare;
      while not QueryKanal.Eof do
      begin
        ParamByName ('MrgKanal').asSmallInt :=   // GeDa
          QueryKanal.FieldByName (C_MrgKanal_MrgKanal).asInteger;

        teststr:=QueryKanal.FieldByName (C_MrgKanal_KanalTyp).asString;
        if not ((teststr = CVImpuls) or (teststr = CVAnalog)) then
          ParamByName ('Aktiv').asBoolean := false{; kb 2.9.97 True;{}
        else
          ParamByName ('Aktiv').asBoolean := true;

        ParamByName ('KanalName').asString :=
          QueryKanal.FieldByName (C_MrgKanal_KanalName).asString;

        if (not Assigned(FTbAutoInc)) then
          FTbAutoInc := TDbAutoInc.Create(FDatabase);
        ParamByName ('KanalId').asInteger := FTbAutoInc.NextIndex[CDBStaKanal];

        ParamByName ('KanalTyp').asString :=
          QueryKanal.FieldByName (C_MrgKanal_KanalTyp).asString;
        ParamByName ('Einstellbar').asBoolean :=
          QueryKanal.FieldByName (C_MrgKanal_Einstellbar).asBoolean;
        ParamByName ('Kontroll').asBoolean :=
          QueryKanal.FieldByName (C_MrgKanal_Kontroll).asBoolean;
        ParamByName ('Eingang').asBoolean :=
          QueryKanal.FieldByName (C_MrgKanal_Eingang).asBoolean;
        ParamByName ('Einheit').asString :=
          QueryKanal.FieldByName (C_MrgKanal_Einheit).asString;
        ParamByName ('Kommastellen').asSmallInt :=      // Geda
          QueryKanal.FieldByName (C_MrgKanal_Kommastellen).asInteger;
        ParamByName ('KanalTypOri').asString :=
          QueryKanal.FieldByName (C_MrgKanal_KanalTyp_ori).asString;  // 22.07.2005, WW
        ParamByName ('DatenTypOri').asSmallInt :=
          QueryKanal.FieldByName (C_MrgKanal_DatenTyp_ori).asInteger;
        ExecSQL;
        QueryKanal.Next;
      end;
    finally
      Free;
    end;
  finally
    QueryKanal.Free;
  end;
end;

{------------------------------------------------------------------------}
procedure TMSStammdaten.CreateImpulsAnalogChannels (iKanalNrVon: integer);
{------------------------------------------------------------------------}
{ Erzeugt Impuls-/Analog-Stammdatenkanäle in StaImp.db und StaAna.db;
  Übergabe:   0: für alle Kanäle
            > 0: Kanalnummer, ab der erzeugt werden soll }
var
  QueryStaKanal: TQuery;
begin
  QueryStaKanal := TQuery.Create (nil);
  try
    QueryStaKanal.DataBaseName := FQuery.DatabaseName;
    QueryStaKanal.SessionName := FQuery.SessionName;
    with QueryStaKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add (C_StaKanal_MrgKanal + ', ' + C_StaKanal_KanalId);
      SQL.Add ('FROM');
      SQL.Add (CDBStaKanal);
      SQL.Add ('WHERE');
      SQL.Add (C_StaKanal_MrgId + ' = ' + IntToStr (MrgId));
      if iKanalNrVon > 0 then  // 30.09.2013, WW
        SQL.Add ('AND ' + C_StaKanal_MrgKanal + ' >= ' + IntToStr (iKanalNrVon));
      Open;
      First;
      while not Eof do
      begin
        CreateImpulsKanal (FieldByName (C_StaKanal_MrgKanal).asInteger,
                           FieldByName (C_StaKanal_KanalId).asInteger);
        CreateAnalogKanal (FieldByName (C_StaKanal_MrgKanal).asInteger,
                           FieldByName (C_StaKanal_KanalId).asInteger);
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
    QueryKanal.SessionName := FQuerySys.SessionName;
    with QueryKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('B.' + C_MrgImp_Faktor + ', B.' + C_MRGImp_Teiler);
      SQL.Add ('FROM ' + CDBMrgKanal + ' A , ' + CDBMrgImp + ' B');
      SQL.Add ('WHERE');
      SQL.Add ('A.' + C_MrgKanal_MrgTyp + ' = ' + Allgemein.MrgTyp.asString);
      SQL.Add ('AND');
      SQL.Add ('A.' + C_MrgKanal_MrgKanal + ' = ' + IntToStr (MrgKanal));
      SQL.Add ('AND');
      SQL.Add ('A.' + C_MrgKanal_KanalId + ' = B.' + C_MrgImp_KanalId);
      Open;
    end;
    if QueryKanal.RecordCount = 1 then
    begin
      QueryKanal.First;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add ('INSERT INTO ' + CDBStaImp);
        SQL.Add ('(' + C_StaImp_KanalId + ', ' + C_StaImp_Faktor + ',');
        Sql.Add (C_StaImp_Teiler + ', ' + C_StaImp_OrgFaktor + ')');
        SQL.Add ('VALUES');
        SQL.Add ('(' + IntToStr (KanalId) + ',');
        SQL.Add (':Faktor, :Teiler, 1)');
        ParamByName ('Faktor').asInteger :=
          QueryKanal.FieldByName (C_MrgImp_Faktor).asInteger;
        ParamByName ('Teiler').asInteger :=
          QueryKanal.FieldByName (C_MrgImp_Teiler).asInteger;
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
    QueryKanal.SessionName := FQuerySys.SessionName;
    with QueryKanal do
    begin
      Close;
      SQL.Clear;
      SQL.Add ('SELECT');
      SQL.Add ('B.' + C_MrgAna_MessbereichMin + ', B.' + C_MrgAna_MessBereichMax + ',');
      SQL.Add ('B.' + C_MrgAna_StromBereich + ', B.' + C_MrgAna_Offset + ',');
      SQL.Add ('B.' + C_MrgAna_AufzMin + ', B.' + C_MrgAna_AufzMax);
      SQL.Add ('FROM ' + CDBMrgKanal + ' A, ' + CDBMrgAna + ' B');
      SQL.Add ('WHERE');
      SQL.Add ('A.' + C_MrgKanal_MrgTyp + ' = ' + Allgemein.MrgTyp.asString);
      SQL.Add ('AND');
      SQL.Add ('A.' + C_MrgKanal_MrgKanal + ' = ' + IntToStr (MrgKanal));
      SQL.Add ('AND');
      SQL.Add ('A.' + C_MrgKanal_KanalId + ' = B.' + C_MrgAna_KanalId);
      Open;
    end;
    if QueryKanal.RecordCount = 1 then
    begin
      QueryKanal.First;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add ('INSERT INTO ' + CDBStaAna);
        SQL.Add ('(' + C_StaAna_KanalId + ', ' + C_StaAna_MessBereichMin + ',');
        SQL.Add (C_StaAna_MessBereichMax + ', ' + C_StaAna_StromBereich + ',');
        Sql.Add (C_StaAna_Offset + ', ' + C_StaAna_AufzMin + ',');
        Sql.Add (C_StaAna_AufzMax+ ')');
        SQL.Add ('VALUES');
        SQL.Add ('(' + IntToStr (KanalId) + ',');
        SQL.Add (':MessBereichMin, :MessBereichMax,');
        SQL.Add (':StromBereich, :Offset, :AufzMin, :AufzMax)');
        ParamByName ('MessBereichMin').asFloat :=
          QueryKanal.FieldByName (C_MrgAna_MessBereichMin).asFloat;
        ParamByName ('MessBereichMax').asFloat :=
          QueryKanal.FieldByName (C_MrgAna_MessBereichMax).asFloat;
        ParamByName ('StromBereich').asSmallInt :=    // GeDa
          QueryKanal.FieldByName (C_MrgAna_StromBereich).asInteger;
        ParamByName ('Offset').asBoolean :=
          QueryKanal.FieldByName (C_MrgAna_Offset).asBoolean;
        ParamByName ('AufzMin').asInteger :=
          QueryKanal.FieldByName (C_MrgAna_AufzMin).asInteger;
        ParamByName ('AufzMax').asInteger :=
          QueryKanal.FieldByName (C_MrgAna_AufzMax).asInteger;
        ExecSQL;
      end;
{Der folgende Abschnitt überprüft das Vorhandensein einer Vorbelegung "0.0"
 der Spalte MessBereichMin in "STAANA.db" und fügt sie ggf. ein.
 (Softwarefehler von Paradox-Datenbanken!!) kb 2.9.97 }
      QueryCheck.Close;
      QueryCheck.DatabaseName := DatabaseName;
      QueryCheck.SessionName := FQuery.SessionName;
      With QueryCheck do begin
        SQL.Clear;
        SQL.Add('SELECT * FROM ' + CDBStaAna + ' WHERE ' + C_StaAna_KanalId + ' = ' + IntTostr(KanalId));
        QueryCheck.Open;
        if RecordCount > 0 then
          if FieldByName(C_StaAna_MessBereichMin).AsString = '' then begin
            Close;
            SQL.Clear;
            SQL.Add('UPDATE ' + CDBStaAna);
            SQL.Add('SET ' + C_StaAna_MessbereichMin + ' = 0.0');
            SQL.Add('WHERE ' + C_StaAna_KanalId + ' = ' + IntTostr(KanalId));
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
var
  i: TMSSector;
begin
  inherited Create (AOwner);
  FMrgId := 0;
  FSectors := [];
  for i := Low (FMSItem) to High (FMSItem) do
    FMSItem [i] := nil;
  FQuery := TMSQuery.Create (Self);
  FQuerySys := TMSQuery.Create (Self);
  FBeforeClear := nil;
  FAfterClear := nil;
  FTbAutoInc := nil;
  FDatabase := nil;
end;

{-------------------------------}
destructor TMSStammdaten.Destroy;
{-------------------------------}
begin
  if (Assigned(FTbAutoInc)) then FreeAndNil(FTbAutoInc);
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
    raise EMSException.Create (S_KeinLesen);
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
    raise EMSException.Create (S_KeinAendern);
  if not Modified then
    raise EMSException.Create (S_DatenNichtGeaendert);
  if not Valid then
    raise EMSException.Create (S_DatenUngueltig);

  // Schleifendurchlauf rückwärts: Sector 'msi_Allgemein' soll als letzter
  // durchlaufen werden, damit bei bereits vorhandener Kennung die Änderungen
  // der übrigen Sektoren gespeichert werden; 03.07.2014, WW
  for i := High (FMSItem) downto Low (FMSItem) do
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
    raise EMSException.Create (S_KeinEinfuegen);
  if not Modified then
    raise EMSException.Create (S_DatenNichtGeaendert);
  if not Valid then
    raise EMSException.Create (S_DatenUngueltig);
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
    CopyMrgKanalToStaKanal (0);
    CreateImpulsAnalogChannels (0);
  end;
end;

{-----------------------------}
procedure TMSStammdaten.Delete;
{-----------------------------}
begin
  if not (FModus in [msm_ReadWrite]) then
    raise EMSException.Create (S_KeinLoeschen);
  if not Valid then
    raise EMSException.Create (S_DatenUngueltig);
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
function TMSListBox.GetDatabaseName: string;
{---------------------------------------------}
begin
  result := FQuery.DatabaseName;
end;

{------------------------------------------------------}
procedure TMSListBox.SetDatabaseName (Value: string);
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
      Result := FQuery.FieldByName (C_Sta_MrgId).asInteger;
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
                          FQuery.FieldByName (C_Sta_StationsName).asString);
        end;
      msl_SKAll, msl_SKAuto:
        begin
          Canvas.TextOut (Rect.Left + 2, Rect.Top,
                          FQuery.FieldByName (C_Sta_Kennung).asString);
          Canvas.TextOut (Rect.Left + 2 + FKennungLen, Rect.Top,
                          FQuery.FieldByName (C_Sta_StationsName).asString);
        end;
      msl_SKNAll, msl_SKNAuto:
        begin
          Canvas.TextOut (Rect.Left + 2, Rect.Top,
                          FQuery.FieldByName (C_MrgDef_MrgName).asString);
          Canvas.TextOut (Rect.Left + 2 + FMrgNameLen, Rect.Top,
                          FQuery.FieldByName (C_Sta_Kennung).asString);
          Canvas.TextOut (Rect.Left + 2 + FMrgNameLen + FKennungLen, Rect.Top,
                          FQuery.FieldByName (C_Sta_StationsName).asString);
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
