{------------------------------------------------------------------------------}
{ Unit Langzeit                                                                }
{ 15.02.1996 D. Achter                                                         }
{ Komponenten zum Zugriff auf Langzeitdaten                                    }
{ 14.04.1999 GD  Umstellung auf Delphi32 und manuelle Daten                    }
{ 19.06.2002 WW  FindClose-Aufrufe ergänzt (Speicherleck !)                    }
{ 05.12.2003 GD  Umstellung auf Tabellen, Schreibzugriffe                      }
{ 19.04.2005 GD  Unschärfe bei Finden eines Zeitstempels eingebaut             }
{ 04.08.2005 GD  Erweitert um Zugriff aif Original-(=DSFG)-Daten               }
{ 12.12.2005 GD  Prüfung nach PREPARE auf NULL                                 }
{ 23.01.2006 WW  resourcestrings                                               }
{ 16.03.2006 GD  Bugfix: "Prepared" für Original-Tageszähler                   }
{ 24.04.2006 GD  Bugfix: "SearchDatum" für Original-Tageszähler                }
{ 13.02.2007 GD  Eigenschaft RohBuffer, Zusätzliche Exceptions                 }
{ 06.07.2009 GD  Stundenwerte bei Originaldaten                                }
{ 29.10.2009 GD  Bugfix: "First" beim laden normierter Daten                   }
{ 20.01.2010 WW  TLGZTag: EingangWert und KontrollWert jetzt double, Eigen-    }
{                schaft RohBuffer                                              }
{ 01.02.2010 GD  Bugfix: Kontrollzähler sichten, "Delete" setzte nicht "EOF"   }
{ 26.02.2010 GD  Sortierung bei Originaldaten jetzt nach Datum/Zeit            }
{ 11.10.2010 GD  Flag LastHourVal -> für Stundenwertbildung LETZTEN Wert nehmen}
{ 07.06.2011 WN  Bugfix Tagessätze Originaldaten: Indexfeld ist Datum/Zeit     }
{ 14.06.2013 GD  Bugfix: Integer-Überlauf beim Berechnen von Stundenwerten     }
{ 24.10.2013 GD  Bugfix: Letzter Zählerstand bei mehrfachen Stundenwerten      }
{ 11.12.2013 GD  Bugfix: IndexName statt IndexFieldNames                       }
{ 08.04.2014 WN  Bugfix: Stundenwertermittlung aus Zählerständen bei           }
{                Winter-Sommer-Zeitumstellung                                  }
{ 22.08.2014 WW  Bugfix TLangzeit.Delete: Close nach OpenExclusive für Ver-    }
{                zeichnis-Tabelle normierter LGZ-Daten (DB steht !); Einträge  }
{                für ALLE Archivgruppen in Verzeichnis-Tabelle für Original-   }
{                daten löschen (AG 3 wurde nicht gelöscht)                     }
{                                                                              }
{ Copyright Karl Wieser GmbH 2001, RMG Messtechnik GmbH 2013, 2014             }
{------------------------------------------------------------------------------}
unit Langzeit;

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, objfile, Db,
  DbTables, Variants, DbClient, Provider,
  LgzType, WTables, T_Zeit, WSysCon, DB_Utils;

{$A-,B-,H-}

const
  { Verzeichnistabelle }
  C_Tb_LGZDVerz          = 'lgzdverz';
  C_Tb_ManuVerz          = 'manuverz';

  C_Tf_LGZDVerz_Kennung  = 'kennung';    { str14 }
  C_Tf_LGZDVerz_FileNr   = 'filenr';     { int }
  C_Tf_LGZDVerz_Status   = 'status';     { smallint }

  C_TI_LGZDVerz_ixFileNr = 'ixfilenr';

  { Tabelle der Stundenwerte }
  C_Tb_LGZStd            = 'lgzd%.4d';                     // Formatangabe !
  C_Tb_LGZStdM           = 'mstd%.4d';                     // Formatangabe !

  C_Tf_LGZStd_OrdNr      = 'ordnr';      { int }
  C_Tf_LGZStd_KanalZahl  = 'kanalzahl';  { int }
  C_Tf_LGZStd_SatzStatus = 'satzstatus'; { smallint }
  C_Tf_LGZStd_DatumZeit  = 'datumzeit';  { timestamp }
  C_Tf_LGZStd_KanStatus  = 'kanstatus%.3d';  { smallint }  // Formatangabe !
  C_Tf_LGZStd_KanalWert  = 'kanalwert%.3d';  { int }       // Formatangabe !

  C_TI_LGZStd_ixDateTime = 'ixdatetime';

  { Tabelle der Tagessätze }
  C_Tb_LGZTag            = 'lgzt%.4d';                     // Formatangabe !
  C_Tb_LGZTagM           = 'mtag%.4d';                     // Formatangabe !

  C_Tf_LGZTag_OrdNr      = 'ordnr';      { int }
  C_Tf_LGZTag_KanalZahl  = 'kanalzahl';  { int }
  C_Tf_LGZTag_SatzStatus = 'satzstatus'; { smallint }
  C_Tf_LGZTag_DatumZeit  = 'datumzeit';  { timestamp }
  C_Tf_LGZTag_Stunden    = 'stunden';    { smallint }
  C_Tf_LGZTag_EKanStatus = 'ekanstatus%.3d';  { smallint }  // Formatangabe !
  C_Tf_LGZTag_EKanalWert = 'ekanalwert%.3d';  { int }       // Formatangabe !
  C_Tf_LGZTag_KKanStatus = 'kkanstatus%.3d';  { smallint }  // Formatangabe !
  C_Tf_LGZTag_KKanalWert = 'kkanalwert%.3d';  { int }       // Formatangabe !

  C_TI_LGZTag_ixDateTime = 'ixdatetime';

  { Satz- und Kanalstati }
  CImpulsMaske = $CF;

  CImpulsKanalStatusChar : array [0..7] of Char = (
    'Ü',
    'P',
    'S',
    'I',
    ' ',
    ' ',
    'K',
    'F'
  );

  CAnalogMaske = $CF;

  CAnalogKanalStatusChar : array [0..7] of Char = (
    'A',
    'P',
    'S',
    'I',
    ' ',
    ' ',
    'K',
    'F'
  );

  CMesswertSatzMaske = $4A;

  CMesswertSatzStatusChar : array [0..7] of Char = (
    ' ',
    'U',
    ' ',
    'R',
    ' ',
    ' ',
    'M',
    ' '
  );

  CZaehlerKanalMaske = $80;

  CZaehlerKanalStatusChar : array [0..7] of Char = (
    ' ',
    ' ',
    ' ',
    ' ',
    ' ',
    ' ',
    ' ',
    'F'
  );

  CZaehlerSatzMaske = $4F;

  CZaehlerSatzStatusChar : array [0..7] of Char = (  // 16.03.2006, WW
    'R',
    'N',
    'U',
    'L',
    ' ',
    ' ',
    'M',
    ' '
  );

type

  TLGZDVerz = record
    Kennung: string [14];
    FileNr: Word;
    Status: Byte;
    Index: word;
  end;

  TLGZException = class (Exception);

  TKennung = string [14];

  TLangzeitState = (
    lgzClosed,         { Langzeitdatei geschlossen }
    lgzKennung,        { Kennung in Verzeichnisdatei gefunden }
    lgzOpened          { Langzeitdatei geöffnet }
  );

  TLangzeitDataSet = class(TObject)
  private
  protected
  public
  end;

  TLangzeit = class (TComponent)
  private
    FState      : TLangzeitState;   { Status }
    FKennung    : TKennung;         { Kennung der Langzeitdatei }
    FKanalZahl  : Word;             { Kanalzahl }
    FAktTable   : TTableExt;        { Aktuell im Zugriff befindliche Tabelle }
    FDatabase   : TDatabase;        { Datenbank }
    FInTransaction : boolean;       { Flag für "interne" Transaction }
    FAktRecPos  : Longint;
    FIsLangzeit : boolean;          { Flag, ob Auto- (TRUE) oder Manu-Daten }
    FIsOriData  : boolean;          { Flag, ob Original- (TRUE) oder Alt-Daten }
    FArchGrpNr  : integer;          { Archivgruppe der Originaldaten }
    FPreparedData : TClientDataSet; { Für Abfrage vorbereitete Daten }
    FDataProvider : TProvider;      { Schnittstelle für "FPreparedData" }
    FTimeFuzzy  : word;             // Unschärfe bzgl. Stundenwechsel
    FVerbrauch  : boolean;          // 06.07.2009
    FLastHourval  : boolean;        // 11.10.2010
    procedure SetKanalZahl (Value: Word);
    function GetEof: Boolean;
    function GetBof: Boolean;
    function GetKennung: TKennung;
    function GetSize: LongInt;
    procedure CreateLgzdVerzTb;
  protected
    TableName: string;              { Langzeitdatei }
    DataBuf: TFields;                 { Langzeitdatensatz }
    procedure SetKennung (Value: TKennung); virtual;
    procedure SetDataFile (FileNr: Word); virtual; abstract;
    procedure ReadBuf;
    procedure CheckOpened;
    procedure CheckIndex (Index: Word);
    function GetDatabaseName: string;
    procedure SetDatabaseName(sDatabaseName: string);
    property Database: TDatabase read FDatabase;
    function GetOpen(iState: integer): boolean; virtual;
    procedure SetSql(pQuery: TQuery; dtVon, dtBis: TDateTime);
      overload; virtual; abstract;
    procedure SetSql(pQuery: TQuery; iVon, iBis: integer);
      overload; virtual; abstract;
    procedure CalcInMemData(dtVon: TDateTime = 0); virtual;
    property ArchGrpNr: integer write FArchGrpNr;
    property ReadKennung: TKennung read FKennung;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function Search (const AKennung: TKennung): Boolean; virtual;
    function Delete (const AKennung: TKennung): Boolean; virtual;
    function EraseData(
      dtVon: TDateTime = 0; dtBis: TDateTime = 0): boolean; virtual; abstract;
    function GetKennungsList: TStrings;
    function GetTableName (const AKennung: TKennung): string;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    procedure Close; virtual;
    procedure First; virtual;
    procedure Last; virtual;
    procedure Next; virtual;
    procedure Prior; virtual;
    procedure Prepare(dtVon: TDateTime = 0; dtBis: TDateTime = 0); overload; virtual;
    procedure Prepare(iVon: integer = 0; iBis: integer = 0); overload; virtual;
    procedure UnPrepare; virtual;
    function DeleteTable: boolean;
    function SearchPos (Position: LongInt): Boolean; virtual;
    function Seek(Offset: Longint; Origin: Word): Longint;
    procedure Truncate;
    procedure RenameKennung(sAlt, sNeu: string);
    function GetDatenKanalzahl(var iKanalzahl: byte): boolean;
    property State: TLangzeitState read FState;
    property Kennung: TKennung read GetKennung write SetKennung;
    property EOF: Boolean read GetEOF;
    property BOF: Boolean read GetBOF;
    property InTransaction: boolean read FInTransaction;
    property Position: Longint read FAktRecPos;
    property Open: boolean index 0 read GetOpen;
    property OpenShared: boolean index 1 read GetOpen;
    property OpenExclusive: boolean index 2 read GetOpen;
    property IsLangzeit: boolean read FIsLangzeit write FIsLangzeit;
    property IsOriData: boolean read FIsOriData write FIsOriData;
    property TimeFuzzy: word read FTimeFuzzy write FTimeFuzzy; // 06.07.2009
    property IsVerbrauch: boolean read FVerbrauch write FVerbrauch;  // 06.07.2009
    property LastHourval: boolean read FLastHourval write FLastHourval;  // 11.10.2010
  published
    property Databasename: string read GetDatabaseName write SetDatabaseName;
    property KanalZahl: Word read FKanalZahl write SetKanalZahl;
    property Size: LongInt read GetSize;
  end;

  TLGZStd = class (TLangzeit)
  private
    FirstOrdnungsNummer : LongInt;
    function GetOrdnungsnummer: LongInt;
    procedure SetOrdnungsnummer(iValue: LongInt);
    function GetDatum: TDateTime;
    procedure SetDatum(dtValue: TDateTime);
    function GetYear: Word;
    function GetMonth: Word;
    function GetDay: Word;
    function GetHour: Word;
    function GetMinute: Word;
    function GetSecond: Word;
    function GetSatzStatus: Byte;
    procedure SetSatzStatus(iValue: Byte);
    function GetWert (Index: Word): double;
    procedure SetWert (Index: Word; fValue: double);
    function GetKanalStatus (Index: Word): Byte;
    procedure SetKanalStatus (Index: Word; iValue: Byte);
    function GetTableName: string;   {kb 8.7.97}
    function ReadBuffer: LGZSRec;
    function ReadRohBuffer: RohSRec;
    procedure WriteBuffer(pBuffer: LGZSRec);
    procedure WriteRohBuffer(pBuffer: RohSRec);
    procedure CreateLgzStdTb;
  protected
    procedure SetDataFile (FileNr: Word); override;
    function GetOpen(iState: integer): boolean; override;
    procedure SetSql(pQuery: TQuery; dtVon, dtBis: TDateTime);
      overload; override;
    procedure SetSql(pQuery: TQuery; iVon, iBis: integer);
      overload; override;
    procedure CalcInMemData(dtVon: TDateTime = 0); override;
  public
    function EraseData(
      dtVon: TDateTime = 0; dtBis: TDateTime = 0): boolean; override;
    function SearchOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
    function SearchGEOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
    function SearchDatum (ADatum: TDateTime): Boolean;
    function SearchGEDatum (ADatum: TDateTime): Boolean;
    property Ordnungsnummer: LongInt
      read GetOrdnungsnummer write SetOrdnungsnummer;
    property Datum: TDateTime read GetDatum write SetDatum;
    property Year: Word read GetYear;
    property Month: Word read GetMonth;
    property Day: Word read GetDay;
    property Hour: Word read GetHour;
    property Minute: Word read GetMinute;
    property Second: Word read GetSecond;
    property SatzStatus: Byte read GetSatzStatus write SetSatzStatus;
    property Wert [Index: Word]: double read GetWert write SetWert;
    property KanalStatus [Index: Word]: Byte
      read GetKanalStatus write SetKanalStatus;
    property DateiName: string read GetTableName; {kbkb 8.7.97}
    property Buffer: LGZSRec read ReadBuffer write WriteBuffer;
    property RohBuffer: RohSRec read ReadRohBuffer write WriteRohBuffer;
  end;

  TLGZTag = class (TLangzeit)
  private
    FAktKontrollTable     : TTableExt; // Aktuell im Zugriff befindliche Tabelle
    FirstOrdnungsNummer   : LongInt;
    FPreparedKontrollData : TClientDataSet; // Für Abfrage vorbereitete Daten
    FDataProviderKontroll : TProvider;      // Schnittstelle für "FPrepared..."
    function GetOrdnungsnummer: LongInt;
    procedure SetOrdnungsnummer(iValue: LongInt);
    function GetSatzStatus: Byte;
    procedure SetSatzStatus(iValue: Byte);
    function GetDatum: TDateTime;
    procedure SetDatum(dtValue: TDateTime);
    function GetStunden: Byte;
    procedure SetStunden(iValue: Byte);
    function GetEingangStatus (Index: Word): Byte;
    procedure SetEingangStatus (Index: Word; iValue: Byte);
    function GetEingangWert (Index: Word): double;  // 20.01.2010, WW
    procedure SetEingangWert (Index: Word; iValue: double);  // 20.01.2010, WW
    function GetKontrollStatus (Index: Word): Byte;
    procedure SetKontrollStatus (Index: Word; iValue: Byte);
    function GetKontrollWert (Index: Word): double;  // 20.01.2010, WW
    procedure SetKontrollWert (Index: Word; iValue: double);  // 20.01.2010, WW
    function ReadBuffer: LGZTRec;
    function ReadRohBuffer: RohTRec;
    procedure WriteBuffer(pBuffer: LGZTRec);
    procedure WriteRohBuffer(pBuffer: RohTRec);
    function GetDateiName : TFileName;  {kbkb 8.7.97}
    procedure CreateLgzTagTb;
  protected
    procedure SetDataFile (FileNr: Word); override;
    function GetOpen(iState: integer): boolean; override;
    procedure SetKennung (Value: TKennung); override;
    procedure SetSql(pQuery: TQuery; dtVon, dtBis: TDateTime);  // 16.03.2006
      overload; override;
    procedure SetSql(pQuery: TQuery; iVon, iBis: integer);      // 16.03.2006
      overload; override;
    procedure CalcInMemData(dtVon: TDateTime = 0); override;
  public
    destructor Destroy; override;
    procedure Prepare(                                          // 16.03.2006
      dtVon: TDateTime = 0; dtBis: TDateTime = 0); overload; override;
    procedure Prepare(                                          // 16.03.2006
      iVon: integer = 0; iBis: integer = 0); overload; override;
    procedure UnPrepare; override;                              // 16.03.2006
    function EraseData(
      dtVon: TDateTime = 0; dtBis: TDateTime = 0): boolean; override;
    function Search (const AKennung: TKennung): Boolean; override;
    function SearchOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
    function SearchGEOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
    function SearchDatum (ADatum: TDateTime): Boolean;
    function SearchGEDatum (ADatum: TDateTime): Boolean;
    procedure First; override;   // 01.02.2010
    procedure Last; override;    // 01.02.2010
    procedure Next; override;    // 01.02.2010
    procedure Prior; override;   // 01.02.2010
    property Ordnungsnummer: LongInt
      read GetOrdnungsnummer write SetOrdnungsnummer;
    property SatzStatus: Byte read GetSatzStatus write SetSatzStatus;
    property Datum: TDateTime read GetDatum write SetDatum;
    property Stunden: Byte read GetStunden write SetStunden;
    property EingangStatus [Index: Word]: Byte
      read GetEingangStatus write SetEingangStatus;
    property EingangWert [Index: Word]: double
      read GetEingangWert write SetEingangWert;  // 20.01.2010, WW
    property KontrollStatus [Index: Word]: Byte
      read GetKontrollStatus write SetKontrollStatus;
    property KontrollWert [Index: Word]: double
      read GetKontrollWert write SetKontrollWert;  // 20.01.2010, WW
    property DateiName: TFileName read GetDateiName; {kbkb 8.7.97}
    property Buffer: LGZTRec read ReadBuffer write WriteBuffer;
    property RohBuffer: RohTRec read ReadRohBuffer write WriteRohBuffer; // 27.01.2010, WW
  end;

function FImpulsKanalStatus (BitNr: byte): string;
function FAnalogKanalStatus (BitNr: byte): string;
function FMesswertSatzStatus (BitNr: byte): string;
function FZaehlerKanalStatus (BitNr: byte): string;
function FZaehlerSatzStatus (BitNr: byte): string;

procedure Register;

implementation

uses DateUtils, Math;

// Funtionen für Originaldaten - aus TbDSfGAr kopiert

resourcestring
  // Texte für Satz- und Kanalstati
  S_Status_Ueberlauf             = 'Überlauf';
  S_Status_KanalInPruefung       = 'Kanal in Prüfung';
  S_Status_PrimaergeraetGestoert = 'Primärgerät gestört';
  S_Status_KanalInaktiv          = 'Kanal inaktiv';
  S_Status_Bit                   = 'Bit %d';
  S_Status_KanalKorrigiert       = 'Kanal wurde korrigiert';
  S_Status_WertFehlt             = 'Wert fehlt';
  S_Status_Undefiniert           = 'Undefiniert';

  S_Status_AnalogkarteNichtGesteckt = 'Analogkarte nicht gesteckt';

  S_Status_UhrGestellt             = 'Uhr gestellt';
  S_Status_Revision                = 'Revision';
  S_Status_SystemReset             = 'Systemreset';
  S_Status_Netzausfall             = 'Netzausfall';
  S_Status_InternerRAMGeloescht    = 'Interner RAM gelöscht';
  S_Status_StundeMehrfachVorhanden = 'Stunde mehrfach vorhanden';

  S_Status_Kontrollzaehler = 'Kontrollzähler';

  // Fehlertexte
  S_DateiNichtGeoeffnet           = 'LGZ-Datei ist nicht geöffnet !';
  S_TabelleGesperrt               = 'LGZ-Tabelle %s ist gesperrt !';
  S_KanalIndexUngueltig           = 'LGZ-Kanalindex ist ungültig !';
  S_FehlerLoeschen_TabGeoeffnet   = 'Fehler LGZ-Daten löschen. LGZ-Tabelle ist geöffnet !';
  S_DatenIndexFalsch              = 'LGZ-Datenindex %s ist ungültig !';
  S_OriDatenIndexFalsch           = 'LGZ-Originaldatenindex %s ist ungültig !';
  S_TabNichtGefundenOderGeoeffnet = 'LGZ-Tabelle nicht gefunden oder geöffnet';
  S_TabFehlt                      = 'LGZ-Tabelle %s fehlt !';

type
  PIndexMerkfeld = ^TIndexMerkfeld;
  TIndexMerkfeld = array[1..9999] of boolean;


{------------------------------------------------}
function FImpulsKanalStatus (BitNr: byte): string;
{------------------------------------------------}
{ liefert Statustext für Impulskanal-Status }
begin
  case BitNr of
    0: Result:=S_Status_Ueberlauf;
    1: Result:=S_Status_KanalInPruefung;
    2: Result:=S_Status_PrimaergeraetGestoert;
    3: Result:=S_Status_KanalInaktiv;
    4: Result:=Format (S_Status_Bit, [BitNr]);
    5: Result:=Format (S_Status_Bit, [BitNr]);
    6: Result:=S_Status_KanalKorrigiert;
    7: Result:=S_Status_WertFehlt;
  else
     Result:=S_Status_Undefiniert;
  end;
end;

{------------------------------------------------}
function FAnalogKanalStatus (BitNr: byte): string;
{------------------------------------------------}
{ liefert Statustext für Analogkanal-Status }
begin
  case BitNr of
    0: Result:=S_Status_AnalogkarteNichtGesteckt;
    1: Result:=S_Status_KanalInPruefung;
    2: Result:=S_Status_PrimaergeraetGestoert;
    3: Result:=S_Status_KanalInaktiv;
    4: Result:=Format (S_Status_Bit, [BitNr]);
    5: Result:=Format (S_Status_Bit, [BitNr]);
    6: Result:=S_Status_KanalKorrigiert;
    7: Result:=S_Status_WertFehlt;
  else
     Result:=S_Status_Undefiniert;
  end;
end;

{-------------------------------------------------}
function FMesswertSatzStatus (BitNr: byte): string;
{-------------------------------------------------}
{ liefert Statustext für Satz-Status von Messwerten }
begin
  case BitNr of
    0: Result:=Format (S_Status_Bit, [BitNr]);
    1: Result:=S_Status_UhrGestellt;
    2: Result:=Format (S_Status_Bit, [BitNr]);
    3: Result:=S_Status_Revision;
    4: Result:=Format (S_Status_Bit, [BitNr]);
    5: Result:=Format (S_Status_Bit, [BitNr]);
    6: Result:=S_Status_StundeMehrfachVorhanden;
    7: Result:='';  { Bit 7: Satz hinzugefügt von Aufbereitung (keine Angabe, Fehlend-Info steckt schon im Kanalstatus) }
  else
     Result:=S_Status_Undefiniert;
  end;
end;

{-------------------------------------------------}
function FZaehlerKanalStatus (BitNr: byte): string;
{-------------------------------------------------}
{ liefert Statustext für Zählerkanal-Status }
begin
  case BitNr of
    0: Result:=Format (S_Status_Bit, [BitNr]);
    1: Result:=S_Status_Kontrollzaehler;
    2: Result:=Format (S_Status_Bit, [BitNr]);
    3: Result:=Format (S_Status_Bit, [BitNr]);
    4: Result:=Format (S_Status_Bit, [BitNr]);
    5: Result:=Format (S_Status_Bit, [BitNr]);
    6: Result:=Format (S_Status_Bit, [BitNr]);
    7: Result:=S_Status_WertFehlt;
  else
     Result:=S_Status_Undefiniert;
  end;
end;

{------------------------------------------------}
function FZaehlerSatzStatus (BitNr: byte): string;
{------------------------------------------------}
{ liefert Statustext für Satz-Status von Zählern }
begin
  case BitNr of
    0: Result:=S_Status_SystemReset;
    1: Result:=S_Status_Netzausfall;
    2: Result:=S_Status_UhrGestellt;
    3: Result:=S_Status_InternerRAMGeloescht;
    4: Result:=Format (S_Status_Bit, [BitNr]);
    5: Result:=Format (S_Status_Bit, [BitNr]);
    6: Result:=S_Status_StundeMehrfachVorhanden;
    7: Result:='';  { Bit 7: Satz hinzugefügt von Aufbereitung (keine Angabe, Fehlend-Info steckt schon im Kanalstatus) }
  else
     Result:=S_Status_Undefiniert;
  end;
end;


{--------------------------------------------}
procedure CreateIndexDB(pDatabase: TDatabase);
{--------------------------------------------}
{ Index-Tabelle anlegen }
begin
  with TTableExt.Create(nil) do
  try
    DatabaseName := pDatabase.DatabaseName;
    SessionName := pDatabase.SessionName;
    TableName := C_DTB_Index;
    if (not Exists) then begin
      with FieldDefs do begin
        Add(C_DTF_Index_Index, ftSmallInt, 0, false);
        Add(C_DTF_Index_Kennung, ftString, 14, false);
        Add(C_DTF_Index_StatNr, ftSmallInt, 0, false);
        Add(C_DTF_Index_ArchivGruppenNr, ftSmallInt, 0, false);
        Add(C_DTF_Index_Instanz, ftSmallInt, 0, false);
        Add(C_DTF_Index_Geraetetyp, ftString, 3, false);
      end;
      with IndexDefs do begin
        Add('pindex', C_DTF_Index_Index, [ixPrimary, ixUnique]);
      end;
      CreateTable;
    end;
  finally
    Free;
  end;
end;

{-------------------------------------------------------}
function CreatetbDSatz(pDatabase: TDatabase; iIndex: integer): boolean;
{-------------------------------------------------------}
{ Tabelle mit Satz-Tabellenstruktur anlegen }
begin
  if (iIndex > 0) then
  try
    with TTable.Create(nil) do
    try
      DatabaseName := pDatabase.DatabaseName;
      SessionName := pDatabase.SessionName;
      TableName := C_TbDSatz + Format('%.4d', [iIndex]);
      if (not Exists) then begin
        with FieldDefs do begin
          Add(C_TfDSatz_ReferenzNr, ftInteger, 0, false);
          Add(C_TfDSatz_Index, ftSmallInt, 0, false);
          Add(C_TfDSatz_OrdNr, ftInteger, 0, false);
          Add(C_TfDSatz_DatumZeit, ftDateTime, 0, false);
          Add(C_TfDSatz_Status, ftString, 10, false);
          Add(C_TfDSatz_Zeitzone, ftString, 1, false);
        end;
        with IndexDefs do begin
          Add('prefnr', C_TfDSatz_ReferenzNr, [ixPrimary,ixUnique]);                      { Primärindex }
          Add(CSDatOrd, C_TfDSatz_DatumZeit+';'+C_TfDSatz_OrdNr, [ixCaseInsensitive]);        { Sekundärindex }
          Add(CSDat, C_TfDSatz_DatumZeit, [ixCaseInsensitive]);    { Sekundärindex für MRG-Originaldaten }
        end;
        CreateTable;
      end;

      Result := Exists;
    finally
      Free;
    end;
  except
    Result := False;
  end
  else Result := False;
end;

{---------------------------------}
function CreatetbDWert(pDatabase: TDatabase; iIndex, iKanal: integer): boolean;
{---------------------------------}
{ Wert-Tabelle anlegen }
begin
  try
    with TTable.Create(nil) do
    try
      DatabaseName := pDatabase.DatabaseName;
      SessionName := pDatabase.SessionName;
      TableName := C_TbDWert + Format('%.3d%.4d', [iKanal, iIndex]);
      if (not Exists) then begin
        with FieldDefs do begin
          Add(C_TfDWert_ReferenzNr, ftInteger, 0, false);
          Add(C_TfDWert_Kanaltyp, ftString, 2, false);
          Add(C_TfDWert_Werteart, ftString, 2, false);
          Add(C_TfDWert_Wert, ftFloat, 0, false);
          Add(C_TfDWert_Status, ftString, 16, false);
          Add(C_TfDWert_CRC, ftString, 10, false);
        end;
        with IndexDefs do begin
          Add('prefnr', C_TfDWert_ReferenzNr, [ixPrimary, ixUnique]);
        end;
        CreateTable;
      end;

      Result := Exists;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

{------------------------------------------------------------------------------}
function Get_MRGDatenIndex(pDatabase: TDatabase; const sKennung: string;
  iArchGrpID: integer; bEintragen: boolean): integer;
{------------------------------------------------------------------------------}
{ Index für DSfG-Archivdaten suchen/eintragen;
  Übergabe: Zeiger auf Archiv-Datenbank
            StationsID, InstanzID, ArchivgruppenID;
            Eintragen: true = neuer Eintrag für StationsID, InstanzID, ArchivgruppenID
                       false = Rückgabe -1, wenn fehlend
  Rückgabe im Erfolgsfall: Indexnummer, sonst -1 }
var
  iIdx, iArchGrpNr: integer;
  bGefunden: boolean;
  sKng : string;
  IndexMerkfeld: PIndexMerkfeld;
  pIndexTable: TTableExt;
begin
  Result:=-1;
  try
    IndexMerkfeld:=New(PIndexMerkfeld);
    try
      for iIdx:=Low(IndexMerkfeld^) to High(IndexMerkfeld^) do
        IndexMerkfeld^[iIdx]:=false;
      iIdx := -1;
      pIndexTable := TTableExt.Create(nil);
      try
        pIndexTable.DatabaseName := pDatabase.DatabaseName;
        pIndexTable.SessionName := pDatabase.SessionName;
        pIndexTable.TableName := C_DTB_Index;
        { Tabelle anlegen, wenn nicht vorhanden: }
        if (not pIndexTable.Exists) then CreateIndexDB(pDatabase);
        pIndexTable.SafeUpdateIndexDefs;
        pIndexTable.IndexName := pIndexTable.IndexDefs[0].Name;
        bGefunden:=false;
        if (pIndexTable.OpenShared) then
        try
          pIndexTable.First;
          while not pIndexTable.EOF do begin
            iIdx := pIndexTable.FieldByName(C_DTF_Index_Index).AsInteger;
            sKng := pIndexTable.FieldByName(C_DTF_Index_Kennung).asString;
            iArchGrpNr := pIndexTable.FieldByName(C_DTF_Index_ArchivGruppenNr).AsInteger;
            IndexMerkfeld^[iIdx]:=true;                       { Nummer belegt }
            if (sKng = sKennung) AND (iArchGrpID = iArchGrpNr) then begin       { Eintrag gefunden }
              bGefunden:=true;
              Break;
            end;
            pIndexTable.Next;
          end;

          if (not bGefunden) AND (bEintragen) then begin         { Eintrag einfügen }
            iIdx:=1;
            while IndexMerkfeld^[iIdx] do Inc(iIdx);                           { nächste freie Nummer suchen }
            pIndexTable.AppendRecord([iIdx, sKennung, nil, iArchGrpID, nil, nil]);
            bGefunden:=true;
          end;
        finally
          pIndexTable.Close;
        end;
      finally
        pIndexTable.Free;
      end;
    finally
      Dispose(IndexMerkfeld);
    end;
    if (iIdx > 0) AND (bGefunden) then Result := iIdx;
  except
  end;
end;

// Erstellt SQL-Anweisung
{------------------------------------------------------------------------------}
procedure CreateArchivSql(iIndex: integer; pQuery: TQuery; pSlWTables: TStrings;
  fVon, fBis: double; iSelectType: byte = 1);
{------------------------------------------------------------------------------}
var
  i      : integer;
  sTb    : string;
  sTbS   : string;
  bWhere : boolean;
begin
  if (pSlWTables.Count = 0) then begin
    pQuery.SQL.Text := '';
    Exit;
  end;

  sTbS := Format(C_TbDSatz + '%.4d', [iIndex]);  // Name der Satztabelle

  with pQuery do begin
    Sql.Clear;

    // Felder der Satztabelle
    Sql.Add('SELECT A.' + C_TfDSatz_ReferenzNr + ', A.' + C_TfDSatz_OrdNr + ',');
    Sql.Add('A.' + C_TfDSatz_DatumZeit + ', A.' + C_TfDSatz_Zeitzone + ',');
    Sql.Add('A.' + C_TfDSatz_Status);

    // Felder der Wertetabellen
    for i := 0 to pSlWTables.Count-1 do begin
      sTb := Chr(Ord('B') + i) + Copy(pSlWTables[i], 2, 3);
      Sql.Add(',' + sTb + '.' + C_TfDWert_Wert + ' AS Wert' + Copy(pSlWTables[i], 2, 3) + ',');
      Sql.Add(sTb + '.' + C_TfDWert_Status + ' AS Status' + Copy(pSlWTables[i], 2, 3) + ',');
      Sql.Add(sTb + '.' + C_TfDWert_Kanaltyp + ' AS Kanaltyp' + Copy(pSlWTables[i], 2, 3) + ',');
      Sql.Add(sTb + '.' + C_TfDWert_CRC + ' AS CRC' + Copy(pSlWTables[i], 2, 3));
    end;

    // Tabellenname Satztabelle
    Sql.Add('FROM ' + sTbS + ' A');
    sTb := Chr(Ord('B') + 0) + Copy(pSlWTables[0], 2, 3);
    Sql.Add('LEFT JOIN ' + pSlWTables[0] + ' ' + sTb + ' ON A.' +
      C_TfDSatz_ReferenzNr + ' = ' + sTb +  '.' + C_TfDWert_ReferenzNr);

    // Tabellennamen Wertetabellen
    for i := 1 to pSlWTables.Count-1 do begin
      sTb := Chr(Ord('B') + i) + Copy(pSlWTables[i], 2, 3);
      Sql.Add('LEFT JOIN ' + pSlWTables[i] + ' ' + sTb + ' ON A.' +
        C_TfDSatz_ReferenzNr + ' = ' + sTb + '.' + C_TfDWert_ReferenzNr);
    end;

    case iSelectType of
      1:
        begin
          if (fVon > 1) then begin
            Sql.Add('WHERE (A.' + C_TfDSatz_DatumZeit + '>= :VON)');
            ParamByName('VON').asDateTime := fVon;
            bWhere := True;
          end
          else bWhere := False;
          if (fBis > 1) then begin
            if (bWhere) then Sql.Add('AND') else Sql.Add('WHERE');
            Sql.Add('(A.' + C_TfDSatz_DatumZeit + '<= :BIS)');
            ParamByName('BIS').asDateTime := fBis;
          end;
        end;
      2:
        begin
          if (fVon > 0) then begin
            Sql.Add('WHERE A.' + C_TfDSatz_OrdNr + ' >= ' +
              IntToStr(Trunc(fVon)));
            bWhere := True;
          end
          else bWhere := False;
          if (fBis > 0) then begin
            if (bWhere) then Sql.Add('AND') else Sql.Add('WHERE');
            Sql.Add('A.' + C_TfDSatz_OrdNr + ' <= ' + IntToStr(Trunc(fBis)));
          end;
        end;
      3:
        begin
          if (fVon > 0) then begin
            Sql.Add('WHERE A.' + C_TfDSatz_ReferenzNr + ' >= ' +
              IntToStr(Trunc(fVon)));
            bWhere := True;
          end
          else bWhere := False;
          if (fBis > 0) then begin
            if (bWhere) then Sql.Add('AND') else Sql.Add('WHERE');
            Sql.Add('A.' + C_TfDSatz_ReferenzNr + ' <= ' +
              IntToStr(Trunc(fBis)));
          end;
        end;
      else begin
        Exit;
      end;
    end;

    Sql.Add('ORDER BY A.' + C_TfDSatz_ReferenzNr);
  end;
end;

{-----------}
{ TLangzeit }
{-----------}

{ Konstruktor }
{------------------------------------------------}
constructor TLangzeit.Create (AOwner: TComponent);
{------------------------------------------------}
begin
  inherited Create (AOwner);
  FState := lgzClosed;
  FKennung := '';
  FKanalZahl := 28;
  FIsLangzeit := True;      { Auto-Daten }
  FIsOriData := False;      { Stunden-normierte Daten }
  FAktTable := nil;         { Aktuelle Tabelle }
  FDatabase := nil;         { Datenbank }
  FInTransaction := False;  { Transaktions-Flag }
  FAktRecPos := -1;         { Position aktueller Datensatz }
  FArchGrpNr := ag_mrg_MW;  { AG1 = "Messwerte"  }
  FTimeFuzzy := 2500;       // Unschärfe bzgl. Stundenwechsel in msec.
  FVerbrauch := False;      // Keine Vebrauchsdatenbildung
  FLastHourval := False;    // Ersten Wert für Stundenwertbildung
end;

{ Destruktor }
{---------------------------}
destructor TLangzeit.Destroy;
{---------------------------}
begin
  Close;
  if (Assigned(FAktTable)) then FAktTable.Free;
  UnPrepare;

  inherited Destroy;
end;

{ Bereitet Datenmenge für schnellere Abfrage  }
{ Parameter: Von-Bis als DateTime             }
{---------------------------------------------}
procedure TLangzeit.Prepare(dtVon: TDateTime = 0; dtBis: TDateTime = 0);
{---------------------------------------------}
var
  q   : TQuery;
  dtV : TDateTime;
begin
  if (FState <> lgzOpened) then raise TLGZException.Create (S_DateiNichtGeoeffnet);

  if (FIsOriData) then begin
    if (not Assigned(FPreparedData)) then begin
      FPreparedData := TClientDataSet.Create(nil);
      FDataProvider := TProvider.Create(nil);
      FDataProvider.Name := 'dp' + IntToStr(Integer(FDataProvider));
      q := TQuery.Create(nil);
      q.DatabaseName := FDatabase.DatabaseName;
      q.SessionName := FDatabase.SessionName;
      FDataProvider.DataSet := q;
      FPreparedData.SetProvider(FDataProvider);
    end
    else q := TQuery(FDataProvider.DataSet);

    FPreparedData.Active := False;

    if (dtVon > 1) and (FVerbrauch)
    then dtV := IncHour(dtVon, -1)
    else dtV := dtVon;

    SetSql(q, dtV, dtBis);
    if (q.Sql.Text <> '') then begin
      FPreparedData.IndexFieldNames := C_TfDSatz_DatumZeit; // 26.02.2010
      FPreparedData.Active := True;
    end;

    CalcInMemData(dtVon);  // 06.07.2009
    First;  // 29.10.2009
  end
  else if (not FIsOriData) then UnPrepare;
end;

{ Bereitet Datenmenge für schnellere Abfrage  }
{ Parameter: Von-Bis als Referenzindices      }
{---------------------------------------------}
procedure TLangzeit.Prepare(iVon: integer = 0; iBis: integer = 0);
{---------------------------------------------}
var
  q : TQuery;
begin
  if (FState <> lgzOpened) then raise TLGZException.Create (S_DateiNichtGeoeffnet);

  if (FIsOriData) then begin
    if (not Assigned(FPreparedData)) then begin
      FPreparedData := TClientDataSet.Create(nil);
      FDataProvider := TProvider.Create(nil);
      FDataProvider.Name := 'dp' + IntToStr(Integer(FDataProvider));
      q := TQuery.Create(nil);
      q.DatabaseName := FDatabase.DatabaseName;
      q.SessionName := FDatabase.SessionName;
      FDataProvider.DataSet := q;
      FPreparedData.SetProvider(FDataProvider);
    end
    else q := TQuery(FDataProvider.DataSet);

    FPreparedData.Active := False;

    SetSql(q, iVon, iBis);
    if (q.Sql.Text <> '') then begin
      FPreparedData.IndexFieldNames := C_TfDSatz_ReferenzNr;
      FPreparedData.Active := True;
      CalcInMemData;  // 06.07.2009
    end
    else UnPrepare; // 01.02.2010
  end
  else if (not FIsOriData) then UnPrepare;
end;

{ Weitergehende Berechnung der Daten          }
{---------------------------------------------}
procedure TLangzeit.CalcInMemData(dtVon: TDateTime = 0);
{---------------------------------------------}
begin
end;

{ Gibt interne Datenmenge frei                }
{---------------------------------------------}
procedure TLangzeit.UnPrepare;
{---------------------------------------------}
var
  q : TQuery;
begin
  if (Assigned(FPreparedData)) then begin
    FPreparedData.Active := False;
    FreeAndNil(FPreparedData);
    q := TQuery(FDataProvider.DataSet);
    FreeAndNil(FDataProvider);
    FreeAndNil(q);
  end;
end;

{ Gibt Datenbanknamen zurück                  }
{---------------------------------------------}
function TLangzeit.GetDatabaseName: string;
{---------------------------------------------}
begin
  if (Assigned(FDatabase))
  then Result := FDatabase.DatabaseName
  else Result := '';
end;

{ Übergibt Datenbanknamen                     }
{---------------------------------------------}
procedure TLangzeit.SetDatabaseName(sDatabaseName: string);
{---------------------------------------------}
var
  i : integer;
begin
  for i := 0 to Sessions.Count-1 do begin
    FDatabase := Sessions[i].FindDatabase(sDatabaseName);
    if (Assigned(FDatabase)) then Break;
  end;
end;

{ Legt ggf. Verzeichnisdatei an               }
{---------------------------------------------}
procedure TLangzeit.CreateLgzdVerzTb;
{---------------------------------------------}
begin
  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;
    if (FIsOriData) then begin
      CreateIndexDB(FDatabase);
    end
    else begin
      if (isLangzeit)
      then TableName := C_Tb_LGZDVerz
      else TableName := C_Tb_ManuVerz;
      if (not Exists) then begin
        with FieldDefs do begin
          Add(C_Tf_LGZDVerz_Kennung, ftString, 14, True);
          Add(C_Tf_LGZDVerz_FileNr, ftInteger, 0, True);
          Add(C_Tf_LGZDVerz_Status, ftSmallint, 0, False);
        end;
        with IndexDefs do begin
          Add('ixmain', C_Tf_LGZDVerz_Kennung, [ixPrimary, ixUnique]);
          Add(C_TI_LGZDVerz_ixFileNr, C_Tf_LGZDVerz_FileNr, [ixCaseInsensitive]);
        end;
        CreateTable;
      end;
    end;
  finally
    Free;
  end;
end;

{---------------------------------------------}
function TLangzeit.GetKennungsList: TStrings;
{---------------------------------------------}
begin
  Result := TStringList.Create;
  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;
    if (FIsOriData) then begin
      TableName := C_DTB_Index;
      if (Exists) then begin
        with TQuery.Create(nil) do
        try
          DatabaseName := Self.Database.DatabaseName;
          SessionName := Self.Database.SessionName;
          Sql.Add('SELECT ' + C_DTF_Index_Index  + ',');
          Sql.Add(C_DTF_Index_Kennung + ',' + C_DTF_Index_ArchivGruppenNr);
          Sql.Add('FROM ' + C_DTB_Index);
          Sql.Add('WHERE ' + C_DTF_Index_Kennung + ' <> ''''');
          Sql.Add('ORDER BY ' + C_DTF_Index_Kennung + ', ' +
            C_DTF_Index_ArchivGruppenNr);
          Open;
          while not (Eof) do begin
            Result.AddObject(FieldByName(C_DTF_Index_Kennung).asString,
              TObject(FieldByName(C_DTF_Index_Index).asInteger));
            Next;
          end;
          Close;
        finally
          Free;
        end;
      end;
    end
    else begin
      if (isLangzeit)
      then TableName := C_Tb_LGZDVerz
      else TableName := C_Tb_ManuVerz;
      if (Exists) then begin
        Open;
        while not (Eof) do begin
          Result.AddObject(FieldByName(C_Tf_LGZDVerz_Kennung).asString,
            TObject(FieldByName(C_Tf_LGZDVerz_FileNr).asInteger));
          Next;
        end;
        Close;
      end;
    end;
  finally
    Free;
  end;
end;

{---------------------------------------------}
procedure TLangzeit.SetKanalZahl (Value: Word);
{---------------------------------------------}
begin
  if (Value <> FKanalZahl) then FKanalZahl := Value;
end;

{---------------------------------}
function TLangzeit.GetEof: Boolean;
{---------------------------------}
begin
  if (FIsOriData) and (Assigned(FPreparedData))
  then Result := FPreparedData.Eof
  else Result := FAktTable.Eof;
end;

{---------------------------------}
function TLangzeit.GetBof: Boolean;
{---------------------------------}
begin
  if (FIsOriData) and (Assigned(FPreparedData))
  then Result := FPreparedData.Bof
  else Result := FAktTable.Bof;
end;

{ Liefert Kennung der Langzeitdatei }
{--------------------------------------}
function TLangzeit.GetKennung: TKennung;
{--------------------------------------}
begin
  if FState <> lgzClosed then
    Result := FKennung
  else
    raise TLGZException.Create ('GetKennung: is closed');        
end;

{ Setzt Kennung für Langzeitdatei               }
{-----------------------------------------------}
procedure TLangzeit.SetKennung (Value: TKennung);
{-----------------------------------------------}
var
  pTbLgzdVerz : TTableExt;
  iFileNr     : integer;
begin
  if FState <> lgzOpened then
  begin
    FState := lgzClosed;

    { sucht Langzeitverzeichnistabelle }
    CreateLgzdVerzTb;
    pTbLgzdVerz := TTableExt.Create(nil);
    try
      iFileNr := -1;

      pTbLgzdVerz.DatabaseName := FDatabase.DatabaseName;
      pTbLgzdVerz.SessionName := FDatabase.SessionName;
      if (FIsOriData) then begin
        iFileNr := Get_MRGDatenIndex(FDatabase, Value, 1, True);
      end     // END - Originaldatenstruktur
      else begin
        if (isLangzeit)
        then pTbLgzdVerz.TableName := C_Tb_LGZDVerz
        else pTbLgzdVerz.TableName := C_Tb_ManuVerz;
        pTbLgzdVerz.SafeUpdateIndexDefs;
        pTbLgzdVerz.IndexName := pTbLgzdVerz.IndexDefs[0].Name;
        if (pTbLgzdVerz.OpenShared) then begin
          { sucht Kennung }
          if (pTbLgzdVerz.FindKey([Trim(Value)])) then begin
            { Kennung gefunden }
            FKennung := Trim(Value);
            iFileNr := pTbLgzdVerz.FieldByName(C_Tf_LGZDVerz_FileNr).asInteger;
          end
          else begin
            pTbLgzdVerz.Close;
            pTbLgzdVerz.IndexName := C_TI_LGZDVerz_ixFileNr;
            if (pTbLgzdVerz.OpenExclusive) then
            try
              iFileNr := 1;
              while (pTbLgzdVerz.FindKey([iFileNr])) do Inc(iFileNr);
              FKennung := Value;
              pTbLgzdVerz.AppendRecord([FKennung, iFileNr, nil]);
            finally
              pTbLgzdVerz.Close;
            end
            else raise TLGZException.Create(
              Format (S_TabelleGesperrt, [pTbLgzdVerz.Tablename]));
          end;
        end;
      end;    // END - Langzeitstruktur

      if (iFileNr > 0) then begin
        SetDataFile (iFileNr);
        with TTableExt.Create(nil) do
        try
          DatabaseName := FDatabase.DatabaseName;
          SessionName := FDatabase.SessionName;
          TableName := Self.TableName;
          if (Exists) then begin
            FState := lgzKennung;
          end
          else FState := lgzClosed;
        finally
          Free;
        end;
      end;
    finally
      pTbLgzdVerz.Free;
    end;
  end
  else raise TLGZException.Create ('SetKennung: not closed');
end;

{ Benennt Kenung um        }
{--------------------------}
procedure TLangzeit.RenameKennung(sAlt, sNeu: string);
{--------------------------}
begin
  { sucht Langzeitverzeichnistabelle }
  CreateLgzdVerzTb;
  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;
    if (FIsOriData) then begin
      TableName := C_DTB_Index;
      if (OpenExclusive) then begin
        try
          while (Locate(C_DTF_Index_Kennung, VarArrayOf([Trim(sAlt)]), [])) do begin  // mit Trim; 24.10.2007, WW
            Edit;
            FieldByName(C_Tf_LGZDVerz_Kennung).asString := Trim(sNeu);  // mit Trim; 24.10.2007, WW
            Post;
          end;
        finally  // 22.08.2014, WW
          Close;
        end;
      end;
    end
    else begin
      if (isLangzeit)
      then TableName := C_Tb_LGZDVerz
      else TableName := C_Tb_ManuVerz;
      SafeUpdateIndexDefs;
      IndexName := IndexDefs[0].Name;
      if (OpenExclusive) then begin
        try
          if (FindKey([Trim(sAlt)])) then begin  // mit Trim; 24.10.2007, WW
            Edit;
            FieldByName(C_Tf_LGZDVerz_Kennung).asString := Trim(sNeu);  // mit Trim; 24.10.2007, WW
            Post;
          end;
        finally  // 22.08.2014, WW
          Close;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

{ Liefert Größe der Langzeitdatei }
{----------------------------------}
function TLangzeit.GetSize: LongInt;
{----------------------------------}
begin
  if (FState = lgzOpened) then begin
    if (FIsOriData) and (Assigned(FPreparedData))
    then Result := FPreparedData.RecordCount
    else Result := FAktTable.RecordCount
  end
  else Result := 0;
end;

{ Liest Datensatz aus Datei }
{--------------------------}
procedure TLangzeit.ReadBuf;
{--------------------------}
begin
  CheckOpened;
  if (FIsOriData) and (Assigned(FPreparedData))
  then DataBuf := FPreparedData.Fields
  else DataBuf := FAktTable.Fields;
end;

{------------------------------}
procedure TLangzeit.CheckOpened;
{------------------------------}
begin
  if (FState <> lgzOpened) then
    raise TLGZException.Create (S_DateiNichtGeoeffnet);
end;

{-------------------------------------------}
procedure TLangzeit.CheckIndex (Index: Word);
{-------------------------------------------}
begin
  if (Index >= FKanalZahl) then
    raise TLGZException.Create (S_KanalIndexUngueltig);
end;

{ Sucht nach Kennung in der Verzeichnisdatei, liefert True, wenn
  Kennung gefunden wurde, und zugehörige Datendatei vorhanden ist }
{------------------------------------------------------------}
function TLangzeit.Search (const AKennung: TKennung): Boolean;
{------------------------------------------------------------}
var
  pTbLgzdVerz : TTableExt;
  bFound      : boolean;
  iFileNr     : integer;
begin
  Result := False;
  if FState <> lgzOpened then begin
    CreateLgzdVerzTb;
    pTbLgzdVerz := TTableExt.Create(nil);
    try
      bFound := False;
      pTbLgzdVerz.DatabaseName := FDatabase.DatabaseName;
      pTbLgzdVerz.SessionName := FDatabase.SessionName;
      if (FIsOriData) then begin
        pTbLgzdVerz.TableName := C_DTB_Index;
        if (pTbLgzdVerz.OpenShared) then
          bFound := (pTbLgzdVerz.Locate(
            C_DTF_Index_Kennung + ';' + C_DTF_Index_ArchivGruppenNr,
            VarArrayOf([Trim(AKennung), FArchGrpNr]), []));
      end
      else begin
        if (isLangzeit)
        then pTbLgzdVerz.TableName := C_Tb_LGZDVerz
        else pTbLgzdVerz.TableName := C_Tb_ManuVerz;
        pTbLgzdVerz.SafeUpdateIndexDefs;
        pTbLgzdVerz.IndexName := pTbLgzdVerz.IndexDefs[0].Name;
        if (pTbLgzdVerz.OpenShared) then
          bFound := (pTbLgzdVerz.FindKey([Trim(AKennung)]))
      end;

      if (bFound) then begin
        if (FIsOriData)
        then iFileNr := pTbLgzdVerz.FieldByName(C_DTF_Index_Index).asInteger
        else iFileNr := pTbLgzdVerz.FieldByName(C_Tf_LGZDVerz_FileNr).asInteger;
        FKennung := Trim(AKennung);
        SetDataFile(iFileNr);

        with TTableExt.Create(nil) do
        try
          DatabaseName := FDatabase.DatabaseName;
          SessionName := FDatabase.SessionName;
          TableName := Self.TableName;
          if (Exists) then begin
            FState := lgzKennung;
            Result := True;
          end
          else FState := lgzClosed;
        finally
          Free;
        end;
      end;
    finally
      pTbLgzdVerz.Free;
    end;
  end
  else raise TLGZException.Create ('Search: not closed');
end;

{ Löscht Kennung aus der Verzeichnisdatei                     }
{------------------------------------------------------------}
function TLangzeit.Delete (const AKennung: TKennung): Boolean;
{------------------------------------------------------------}
var
  pTbLgzdVerz : TTableExt;
begin
  if FState <> lgzOpened then begin
    CreateLgzdVerzTb;
    pTbLgzdVerz := TTableExt.Create(nil);
    try
      pTbLgzdVerz.DatabaseName := FDatabase.DatabaseName;
      pTbLgzdVerz.SessionName := FDatabase.SessionName;
      if (FIsOriData) then begin
        pTbLgzdVerz.TableName := C_DTB_Index;
        if (pTbLgzdVerz.OpenShared) then begin
          try
            // nur nach Kennung suchen (alle AG); 22.08.2014, WW
            while (pTbLgzdVerz.Locate(C_DTF_Index_Kennung, Trim(AKennung), [])) do
            begin
              pTbLgzdVerz.Delete;
            end;
          finally
            pTbLgzdVerz.Close;  // 22.08.2014, WW
          end;
        end;
      end
      else begin
        if (isLangzeit)
        then pTbLgzdVerz.TableName := C_Tb_LGZDVerz
        else pTbLgzdVerz.TableName := C_Tb_ManuVerz;
        pTbLgzdVerz.SafeUpdateIndexDefs;
        pTbLgzdVerz.IndexName := pTbLgzdVerz.IndexDefs[0].Name;
        if (pTbLgzdVerz.OpenExclusive) then begin
          try
            if (pTbLgzdVerz.Locate(C_DTF_Index_Kennung, Trim(AKennung), [])) then
              pTbLgzdVerz.Delete;
          finally
            pTbLgzdVerz.Close;  // sonst DB blockiert ! 22.08.2014, WW
          end;
        end;
      end;

      FKennung := '';
      FState := lgzClosed;
      Result := True;
    finally
      pTbLgzdVerz.Free;
    end;
  end
  else raise TLGZException.Create ('Delete: not closed');
end;

{-------------------------------------------------------------------}
function TLangzeit.GetTableName (const AKennung: TKennung): string;
{-------------------------------------------------------------------}
begin
  SetKennung (Akennung);
  Result := TableName;
end;

{ Öffnet aktuelle Tabelle                                           }
{ Parameter: 0=open; 1=shared; 2=exclusive                          }
{ Rückgabe: OK                                                      }
{-------------------------------------------------------------------}
function TLangzeit.GetOpen(iState: integer): boolean;
{-------------------------------------------------------------------}
begin
  Result := False;

  if (FState = lgzKennung) then
  begin
    if (not Assigned(FAktTable)) then FAktTable := TTableExt.Create (nil);
    FAktTable.DatabaseName := FDatabase.DatabaseName;
    FAktTable.SessionName := FDatabase.SessionName;
    FAktTable.TableName := Self.TableName;
    if (FAktTable.SafeUpdateIndexDefs) then begin  // 26.02.2010
      if (IsOriData)
      then FAktTable.IndexName := CSDat
      else FAktTable.IndexName := FAktTable.IndexDefs[0].Name
    end
    else raise Exception.Create('TLangzeit.GetOpen (' + FAktTable.TableName +
      '): Fatal access error (table locked/deleted?)');

    if (iState = 0) then begin
      FAktTable.Open;
      FState := lgzOpened;
    end
    else if ((iState = 1) and (FAktTable.OpenShared)) or
           ((iState = 2) and (FAktTable.OpenExclusive))
    then FState := lgzOpened;

    if (FState = lgzOpened) then begin
      FAktTable.First;
      FAktRecPos := 0;
      DataBuf := FAktTable.Fields;
    end;

    Result := (FState = lgzOpened);
  end
  else if (FState = lgzClosed) then
    raise TLGZException.Create ('GetOpen: No Kennung');
end;

{ Schließt Datendatei, falls diese geöffnet war }
{------------------------}
procedure TLangzeit.Close;
{------------------------}
begin
  if FState = lgzOpened then
  begin
    FAktTable.Close;
    FAktRecPos := -1;
    DataBuf := nil;
    FState := lgzKennung;
  end;

  UnPrepare;
end;

{ Setzt Cursor an den Anfang der Datei und liest ersten Satz in Puffer }
{------------------------}
procedure TLangzeit.First;
{------------------------}
var
  pDataset : TDataSet;
begin
  CheckOpened;

  if (FIsOriData) and (Assigned(FPreparedData))
  then pDataset := FPreparedData
  else pDataset := FAktTable;

  pDataset.First;
  DataBuf := pDataset.Fields;
  FAktRecPos := 0;
end;

{ Setzt Cursor an das Ende der Datei, nachdem der letzte Satz in den
  Puffer gelesen wurde }
{-----------------------}
procedure TLangzeit.Last;
{-----------------------}
var
  pDataset : TDataSet;
begin
  CheckOpened;

  if (FIsOriData) and (Assigned(FPreparedData))
  then pDataset := FPreparedData
  else pDataset := FAktTable;

  pDataset.Last;
  FAktRecPos := pDataset.RecordCount-1;
  DataBuf := pDataset.Fields;
end;

{ Setzt Cursor auf nächsten Datensatz und liest diesen in Puffer }
{-----------------------}
procedure TLangzeit.Next;
{-----------------------}
var
  pDataset : TDataSet;
begin
  CheckOpened;

  if (FIsOriData) and (Assigned(FPreparedData))
  then pDataset := FPreparedData
  else pDataset := FAktTable;

  if (not pDataset.Eof) then begin
    pDataset.Next;
    Inc(FAktRecPos);
    DataBuf := pDataset.Fields;
  end;
end;

{ Setzt Cursor auf vorherigen Datensatz und liest diesen in Puffer }
{-----------------------}
procedure TLangzeit.Prior;
{-----------------------}
var
  pDataset : TDataSet;
begin
  CheckOpened;

  if (FIsOriData) and (Assigned(FPreparedData))
  then pDataset := FPreparedData
  else pDataset := FAktTable;

  if (not pDataset.Bof) then begin
    pDataset.Prior;
    Dec(FAktRecPos);
    DataBuf := pDataset.Fields;
  end;
end;

{ Löscht aktuelle Tabelle                                }
{--------------------------------------------------------}
function TLangzeit.DeleteTable: boolean;
{--------------------------------------------------------}
begin
  Result := False;  // Default
  if (FState = lgzKennung) then begin
    Result := FAktTable.DeleteTable;
    if (Result) then FState := lgzClosed;
  end
  else if (FState = lgzOpened) then raise TLGZException.Create(S_FehlerLoeschen_TabGeoeffnet);
end;

{--------------------------------------------------------}
procedure TLangzeit.StartTransaction;
{--------------------------------------------------------}
begin
  if (Assigned(FDatabase)) and (FDatabase.IsSQLBased) and
     (not FDatabase.InTransaction) then
  begin
    FDatabase.StartTransaction;
    FInTransaction := FDatabase.InTransaction;
  end;
end;

{--------------------------------------------------------}
procedure TLangzeit.Commit;
{--------------------------------------------------------}
begin
  if (FInTransaction) then begin
    FDatabase.Commit;
    FInTransaction := FDatabase.InTransaction;
  end;
end;

{--------------------------------------------------------}
procedure TLangzeit.Rollback;
{--------------------------------------------------------}
begin
  if (FInTransaction) then begin
    FDatabase.Rollback;
    FInTransaction := False;
  end;
end;

{ Positioniert Cursor auf beliebige Position und liest Datensatz ein }
{--------------------------------------------------------}
function TLangzeit.SearchPos (Position: LongInt): Boolean;
{--------------------------------------------------------}
var
  pDataset : TDataSet;
begin
  Result := False;
  CheckOpened;

  if (FIsOriData) and (Assigned(FPreparedData))
  then pDataset := FPreparedData
  else pDataset := FAktTable;

  pDataset.First;
  FAktRecPos := FAktRecPos + pDataset.MoveBy(Position);
  if (not pDataset.Eof) then begin
    DataBuf := pDataset.Fields;
    Result := True;
  end;
end;

{--------------------------------------------------------}
function TLangzeit.Seek(Offset: Longint; Origin: Word): Longint;
{--------------------------------------------------------}
var
  pDataset : TDataSet;
begin
  CheckOpened;

  if (FIsOriData) and (Assigned(FPreparedData))
  then pDataset := FPreparedData
  else pDataset := FAktTable;

  if (Origin = soFromBeginning) then First
  else if (Origin = soFromEnd) then begin
    Last; // EOF ist gesetzt !
    if (Offset < 0) then begin
      pDataset.Prior;        // EOF löschen
      pDataset.Next;         // Wieder auf ursprünglichen Datensatz gehen
      Offset := Offset+1;     // Offset anpassen
    end;
  end;
  if (Offset <> 0) then FAktRecPos := FAktRecPos + pDataset.MoveBy(Offset);
  Result := FAktRecPos;
end;

{--------------------------------------------------------}
procedure TLangzeit.Truncate;
{--------------------------------------------------------}
begin
  CheckOpened;
  while (not FAktTable.Eof) do FAktTable.Delete;
  Dec(FAktRecPos);
end;

{ Gibt Kanalzahl der gespeicherten Daten als Param. zur. }
{ Parameter: Übergabe Kanalzahl (nix = 0)                }
{ Ergebnis: Daten da ja/nein                             }
{--------------------------------------------------------}
function TLangzeit.GetDatenKanalzahl(var iKanalzahl: byte): boolean;
{--------------------------------------------------------}
var
  pIndexList : TList;
  i, iFNr    : integer;
  sFieldName : string;
begin
  iKanalzahl := 0;  // Default

  pIndexList := TList.Create;
  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;

    if (FIsOriData) then begin
      iKanalzahl := FKanalZahl;
    end
    else begin
      // Verzeichnistabelle öffnen
      if (isLangzeit)
      then TableName := C_Tb_LGZDVerz
      else TableName := C_Tb_ManuVerz;
      if (Exists) then begin
        Open;
        // Vorhandene Filenummern in Liste eintragen
        while not (Eof) do begin
          pIndexList.Add(TObject(FieldByName(C_Tf_LGZDVerz_FileNr).asInteger));
          Next;
        end;
        Close;
      end;

      TableName := '';  // "Flag", ob Tabelle gefunden (<> "")

      // 1. existierende Tabelle aus Liste suchen
      for i := 0 to pIndexList.Count-1 do begin
        iFNr := Integer(pIndexList[i]);

        // Prüfung auf Messwerte
        if (isLangzeit)
        then TableName := Format(C_Tb_LGZStd, [iFNr])
        else TableName := Format(C_Tb_LGZStdM, [iFNr]);
        if (Exists) then Break else TableName := '';

        // Prüfung auf Tagessätze
        if (isLangzeit)
        then TableName := Format(C_Tb_LGZTag, [iFNr])
        else TableName := Format(C_Tb_LGZTagM, [iFNr]);
        if (Exists) then Break else TableName := '';
      end;

      // Wenn Tabelle vorhanden: Prüfen
      if (TableName <> '') then begin
        SafeUpdateFieldDefs;
        sFieldName := FieldDefs[FieldDefs.Count-1].Name;
        iKanalzahl :=
          StrToIntDef(Copy(sFieldName, Length(sFieldName)-2, 3), -1) + 1;
      end;
    end;

    Result := (iKanalzahl > 0);
  finally
    Free;
    pIndexList.Free;
  end;
end;

{ TLGZStd }

{ Legt ggf. Tabelle an                        }
{---------------------------------------------}
procedure TLgzStd.CreateLgzStdTb;
{---------------------------------------------}
var
  i, iIndex : integer;
begin
  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;
    TableName := Self.TableName;
    if (not Exists) then begin
      if (IsOriData) then begin
        iIndex := StrToIntDef(Copy(Self.TableName, 5, 4), -1);
        if (iIndex > 0) then begin
          CreatetbDSatz(Database, iIndex);
        end
        else Exception.Create(Format (S_DatenIndexFalsch, [Copy(Self.TableName, 5, 4)]));
      end
      else begin
        with FieldDefs do begin
          Add(C_Tf_LGZStd_OrdNr, ftInteger, 0, True);
          Add(C_Tf_LGZStd_KanalZahl, ftInteger, 0, True);
          Add(C_Tf_LGZStd_SatzStatus, ftSmallint, 0, False);
          Add(C_Tf_LGZStd_DatumZeit, ftDateTime, 0, True);
          for i := 0 to Kanalzahl-1 do begin
            Add(Format(C_Tf_LGZStd_KanStatus, [i]), ftSmallint, 0, False);
            Add(Format(C_Tf_LGZStd_KanalWert, [i]), ftInteger, 0, False);
          end;
        end;
        with IndexDefs do begin
          Add('ixmain', C_Tf_LGZStd_OrdNr, [ixPrimary, ixUnique]);
          Add(C_TI_LGZStd_ixDateTime, C_Tf_LGZStd_DatumZeit, [ixCaseInsensitive]);
        end;
        CreateTable;
      end;
    end;

    if (SafeUpdateIndexDefs)
    then IndexName := IndexDefs[0].Name
    else raise Exception.Create('TLgzStd.CreateLgzStdTb (' + Self.TableName +
      '): Fatal access error (table locked/deleted?)');
  finally
    Free;
  end;
end;

{------------------------------------------}
function TLGZStd.EraseData(
  dtVon: TDateTime = 0; dtBis: TDateTime = 0): boolean;
{------------------------------------------}
var
  sWA       : string;
  i, iIndex : integer;
begin
  Result := False;
  if (FState = lgzKennung) then begin
    if (FAktTable.Exists) then begin
      if (IsOriData) then begin
        iIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
        if (dtVon = 0) and (dtBis = 0) then begin  // Daten komplett löschen !
          with TTable.Create(nil) do
          try
            DatabaseName := FDatabase.DatabaseName;
            SessionName := FDatabase.SessionName;
            with TQueryExt.Create(nil) do
            try
              DatabaseName := FDatabase.DatabaseName;
              SessionName := FDatabase.SessionName;
              for i := 1 to KanalZahl do begin
                TableName := C_TbDWert + Format('%.3d%.4d', [i, iIndex]);
                if (Exists) then begin
                  Sql.Add('DROP TABLE ' + TableName);
                  ExecSql;
                  Sql.Clear;
                end;
              end;
              Sql.Add('DROP TABLE ' + FAktTable.TableName);
              ExecSql;
              FState := lgzClosed;
              Result := True;
            finally
              Free;
            end;
          finally
            Free;
          end;
        end
        else begin
          with TTable.Create(nil) do
          try
            DatabaseName := FDatabase.DatabaseName;
            SessionName := FDatabase.SessionName;
            with TQueryExt.Create(nil) do
            try
              DatabaseName := FDatabase.DatabaseName;
              SessionName := FDatabase.SessionName;
              for i := 1 to KanalZahl do begin
                TableName := C_TbDWert + Format('%.3d%.4d', [i, iIndex]);
                if (Exists) then begin
                  Sql.Add('DELETE FROM ' + TableName);
                  Sql.Add('WHERE ' + C_TfDWert_ReferenzNr + ' IN ');
                  Sql.Add('(');
                  Sql.Add('SELECT ' + C_TfDSatz_ReferenzNr);
                  Sql.Add('FROM ' + FAktTable.TableName);
                  sWA := 'WHERE ';
                  if (dtVon > 0) then begin
                    Sql.Add(sWA + C_TfDSatz_DatumZeit + ' >= :Von');
                    ParamByName('Von').asDateTime := dtVon;
                    sWA := 'AND ';
                  end;
                  if (dtBis > 0) then begin
                    Sql.Add(sWA + C_TfDSatz_DatumZeit + ' <= :Bis');
                    ParamByName('Bis').asDateTime := dtBis;
                  end;
                  Sql.Add(')');
                  ExecSql;
                  Sql.Clear;
                end;
              end;
              Sql.Add('DELETE FROM ' + FAktTable.TableName);
              sWA := 'WHERE ';
              if (dtVon > 0) then begin
                Sql.Add(sWA + C_TfDSatz_DatumZeit + ' >= :Von');
                ParamByName('Von').asDateTime := dtVon;
                sWA := 'AND ';
              end;
              if (dtBis > 0) then begin
                Sql.Add(sWA + C_TfDSatz_DatumZeit + ' <= :Bis');
                ParamByName('Bis').asDateTime := dtBis;
              end;
              ExecSql;
              Result := True;
            finally
              Free;
            end;
          finally
            Free;
          end;
        end;
      end
      else begin
        with TQueryExt.Create(nil) do
        try
          DatabaseName := FDatabase.DatabaseName;
          SessionName := FDatabase.SessionName;
          if (dtVon = 0) and (dtBis = 0) then begin  // Daten komplett löschen !
            Sql.Add('DROP TABLE ' + FAktTable.TableName);
            Result := ExecSql;
            if (Result) then FState := lgzClosed;
          end
          else begin
            Sql.Add('DELETE FROM ' + FAktTable.TableName);
            if (dtVon > 0) then begin
              Sql.Add('WHERE ' + C_Tf_LGZStd_DatumZeit + ' >= :Von');
              ParamByName('Von').asDateTime := dtVon;
            end;
            if (dtBis > 0) then begin
              if (Sql.Count = 1) then sWA := 'WHERE ' else sWA := 'AND ';
              Sql.Add(sWA + C_Tf_LGZStd_DatumZeit + ' <= :Bis');
              ParamByName('Bis').asDateTime := dtBis;
            end;
            Result := ExecSql;
          end;
        finally
          Free;
        end;
      end;
    end;
  end
  else raise TLGZException.Create(S_TabNichtGefundenOderGeoeffnet);
end;

{------------------------------------------}
procedure TLGZStd.SetSql(pQuery: TQuery; dtVon, dtBis: TDateTime);
{------------------------------------------}
var
  i, iIndex : integer;
  pSlTbs    : TStrings;
begin
  if (IsOriData) then begin
    iIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
    pSlTbs := TStringList.Create;  // Stringliste für Wertetabellen
    try
      with TTable.Create(nil) do
      try
        DatabaseName := FDatabase.DatabaseName;
        SessionName := FDatabase.SessionName;
        // Existierende Wertetabellen finden und eintragen
        for i := 1 to KanalZahl do begin
          TableName := C_TbDWert + Format('%.3d%.4d', [i, iIndex]);
          if (Exists) then pSlTbs.Add(TableName);
        end;
      finally
        Free;
      end;

      // SQL-Anweisung in Query eintragen
      CreateArchivSql(iIndex, pQuery, pSlTbs, dtVon, dtBis, 1);
    finally
      pSlTbs.Free;
    end;
  end
  else pQuery.Sql.Text := '';
end;

{------------------------------------------}
procedure TLGZStd.SetSql(pQuery: TQuery; iVon, iBis: integer);
{------------------------------------------}
var
  i, iIndex : integer;
  pSlTbs    : TStrings;
begin
  if (IsOriData) then begin
    iIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
    pSlTbs := TStringList.Create;  // Stringliste für Wertetabellen
    try
      with TTable.Create(nil) do
      try
        DatabaseName := FDatabase.DatabaseName;
        SessionName := FDatabase.SessionName;
        // Existierende Wertetabellen finden und eintragen
        for i := 1 to KanalZahl do begin
          TableName := C_TbDWert + Format('%.3d%.4d', [i, iIndex]);
          if (Exists) then pSlTbs.Add(TableName);
        end;
      finally
        Free;
      end;

      // SQL-Anweisung in Query eintragen
      CreateArchivSql(iIndex, pQuery, pSlTbs, iVon, iBis, 3);
    finally
      pSlTbs.Free;
    end;
  end
  else pQuery.Sql.Text := '';
end;

{------------------------------------------}
procedure TLGZStd.CalcInMemData(dtVon: TDateTime = 0);
{------------------------------------------}

   function GetHour(dt: TDateTime): TDateTime;
   var
     h,n,s,ms : word;
     iInc     : byte;
   begin
     DecodeTime(dt, h, n, s, ms);
     if (n < 30) then iInc := 0 else iInc := 1;
     Result := Trunc(dt) + IncHour(EncodeTime(h, 0, 0, 0), iInc);
   end;

   function IsHour(dt: TDateTime): boolean;
   begin
     Result := SameValue(dt, GetHour(dt),
       EncodeTime(0, 0, FTimeFuzzy div 1000, FTimeFuzzy mod 1000));
   end;

   function IsNextHour(dtPrior, dtNext: TDateTime): Shortint;
   var
     dt       : TDateTime;
   begin
     dt := IncHour(GetHour(dtPrior), 1);
     if (SameValue(dt, dtNext,
       EncodeTime(0, 0, FTimeFuzzy div 1000, FTimeFuzzy mod 1000)))
     then Result := 0
     else if (dt > dtNext) then Result := -1
     else Result := 1;
   end;

  function GetKanalTyp(iIndex: integer): string;
  begin
    if (FPreparedData.FieldDefs.IndexOf('Wert' + Format('%.3d%', [iIndex+1])) >= 0)
    then Result := FPreparedData.
      FieldByName('Kanaltyp' + Format('%.3d%', [iIndex+1])).AsString
    else Result := '';
  end;

  procedure SetKanalTyp(iIndex: integer; const sText: string);
  begin
    FPreparedData.FieldByName(
      'Kanaltyp' + Format('%.3d%', [iIndex+1])).AsString := sText;
  end;

  function GetWert(iIndex: integer): double;
  begin
    if (FPreparedData.
      FieldByName('Wert' + Format('%.3d%', [iIndex+1])).IsNull) 
    then Result := 0
    else Result := FPreparedData.
      FieldByName('Wert' + Format('%.3d%', [iIndex+1])).AsFloat;
  end;

  procedure SetWert(iIndex: integer; fValue: double);
  begin
    FPreparedData.FieldByName(
      'Wert' + Format('%.3d%', [iIndex+1])).AsFloat := fValue;
  end;

  function GetStatus(iIndex: integer): byte;
  begin
    if (FPreparedData.
      FieldByName('Status' + Format('%.3d%', [iIndex+1])).IsNull) 
    then Result := 0
    else Result := FPreparedData.
      FieldByName('Status' + Format('%.3d%', [iIndex+1])).AsInteger;
  end;

  procedure SetStatus(iIndex: integer; iStatus: byte);
  begin
    FPreparedData.FieldByName(
      'Status' + Format('%.3d%', [iIndex+1])).AsInteger := iStatus;
  end;

var
  pChannelExists : array [0..C_MaxKanalZahl-1] of boolean;
  pKanalTyp     : array [0..C_MaxKanalZahl-1] of string[3];
  pPrevValues   : array [0..C_MaxKanalZahl-1] of double;
  pValues       : array [0..C_MaxKanalZahl-1] of double;
  pStatus       : array [0..C_MaxKanalZahl-1] of byte;
  pPrevStatus   : array [0..C_MaxKanalZahl-1] of byte;  // 08.04.2014  WN
  dt            : TDateTime;
  i             : integer;    // 14.06.2013
  iCnt, iONr    : Int64;      // 14.06.2013
  f, fValue     : double;
  iStatus       : byte;
  bEof          : boolean;    // 01.02.2010
begin
  if (Assigned(FPreparedData)) and (FVerbrauch) then with FPreparedData do
  try
    IndexFieldNames := C_TfDSatz_DatumZeit + ';' + C_TfDSatz_ReferenzNr;
    dt := 0;
    iONr := 1;
    First;

    for i := 0 to C_MaxKanalZahl-1 do pChannelExists[i] :=
      (FPreparedData.FieldDefs.IndexOf('Wert' + Format('%.3d%', [i+1])) >= 0);
    for i := 0 to C_MaxKanalZahl-1 do pKanalTyp[i] := GetKanalTyp(i);
    for i := 0 to C_MaxKanalZahl-1 do pPrevValues[i] := 0;
    for i := 0 to C_MaxKanalZahl-1 do pValues[i] := 0;
    for i := 0 to C_MaxKanalZahl-1 do pStatus[i] := $80;
    for i := 0 to C_MaxKanalZahl-1 do pPrevStatus[i] := $80;  // 08.04.2014  WN

    while (not Eof) do begin
      if (IsHour(FieldByName(C_TfDSatz_DatumZeit).AsDateTime)) then begin
        if (dt = 0) or
          (IsNextHour(dt, FieldByName(C_TfDSatz_DatumZeit).AsDateTime) = 0) then
        begin
          Edit;

          FieldByName(C_TfDSatz_DatumZeit).AsDateTime := // Datum/Zeit auf Stunde
            GetHour(FieldByName(C_TfDSatz_DatumZeit).AsDateTime);
          dt := FieldByName(C_TfDSatz_DatumZeit).AsDateTime;
          FieldByName(C_TfDSatz_ReferenzNr).AsInteger := iONr; // Referenznummer
          FieldByName(C_TfDSatz_OrdNr).AsInteger := iONr; // Ordnungsnummer
          Inc(iONr);

          for i := 0 to KanalZahl-1 do begin
            if (not pChannelExists[i]) then Continue;
            if (i >= C_MaxKanalZahl) then Break;  // Maximale Kanalzahl abprüfen

            iStatus := GetStatus(i);
            if (pKanalTyp[i] = kt_ZS) then begin
              SetKanalTyp(i, kt_ZW);
              fValue := GetWert(i);
              if (iONr > 2) and ((iStatus and $80) = 0) then begin
                if ((pStatus[i] and $80) = 0) then begin
                  f := pValues[i];
                  if (fValue >= f) then begin  // Wert OK
                    SetWert(i, fValue - f);
                    SetStatus(i, iStatus);
                  end
                  else begin
                    SetWert(i, 0);
//                    SetStatus(i, iStatus or $01);  // Wert Überlauf
                    SetStatus(i, iStatus and $80);  // Wert falsch
                  end;
                end
                else begin  // Vorheriger Wert fehlt
                  // 08.04.2014  WN
                  if (IsWSZeitUmstellung(dt)) then   // nur bei Zeitumstellung
                  begin
                    if ((pPrevStatus[i] and $80) = 0) then begin
                      f := pPrevValues[i];
                      if (fValue >= f) then begin  // Wert OK
                        SetWert(i, fValue - f);
                        SetStatus(i, iStatus);
                      end
                      else begin
                        SetWert(i, 0);
//                        SetStatus(i, iStatus or $01);  // Wert Überlauf
                        SetStatus(i, iStatus and $80);  // Wert falsch
                      end;
                    end;
                  end else
                  begin
                    SetWert(i, 0);
                    SetStatus(i, iStatus and $80);  // Wert fehlt
                  end;
                end;
//                pPrevValues[i] := pValues[i];
                pValues[i] := fValue;
              end
              else begin
                SetWert(i, 0);
                SetStatus(i, iStatus and $80);  // Wert fehlt
                if ((iStatus and $80) = 0)
                then pValues[i] := fValue
                else pValues[i] := 0;
              end;
              // 08.04.2014  WN
              pPrevValues[i] := pValues[i];
              pPrevStatus[i] := pStatus[i];
            end
            else if (pKanalTyp[i] = kt_ZW) then begin
              if ((iStatus and $80) = 0) then begin
                SetWert(i, GetWert(i) + pValues[i]);
              end
              else begin
                SetWert(i, 0);
                SetStatus(i, iStatus and $80);  // Wert fehlt
              end;
              pValues[i] := 0;
            end
            else if (pKanalTyp[i] = kt_MW) then begin
              if ((iStatus and $80) = 0) then begin       // 01.02.2010
                iCnt := Round(pValues[i]) div 10000000;   // Anzahl der Werte
                fValue := pValues[i] - (iCnt * 10000000); // Offset abziehen
                fValue := ((fValue * iCnt) + GetWert(i)) / (iCnt + 1);

                SetWert(i, fValue);
              end
              else begin
                SetWert(i, 0);
                SetStatus(i, iStatus and $80);  // Wert fehlt
              end;
              pValues[i] := 0;
            end
            else begin
              // Wert unverändert lassen
            end;
            pStatus[i] := iStatus;
          end;

          Post;
          Next;
        end
        else begin
          if (IsNextHour(dt, FieldByName(C_TfDSatz_DatumZeit).AsDateTime) < 0)
          then begin
            Next;    // Dämliches System: EOF wird nicht automatisch gesetzt!
            bEof := (Eof);    // 01.02.2010
            if (not bEof) then Prior;
            // Doppelter Stundeneintrag
            if (FLastHourval) then begin    // 11.10.2010
              Prior;                        // "Letzter Stundenwert" heißt ...
              Delete;                       // ... vorherigen Wert löschen ...

              // Merker resetten
              for i := 0 to KanalZahl-1 do begin
                if (not pChannelExists[i]) then Continue;
                if (pKanalTyp[i] = kt_ZS) then begin
                  pValues[i] := pPrevValues[i];
                  pStatus[i] := pPrevStatus[i];  // 08.04.2014  WN
                end
              end;

              dt := IncHour(dt, -1);        // ... und Uhr zurückdrehen
            end
            else Delete;                    // Sonst aktuellen Eintrag löschen
            if (bEof) and (not FLastHourval) then Next;    // Einmal around worken ...  // 24.10.2013
          end
          else begin   // Fehlende Stunden
            Insert;
            FieldByName(C_TfDSatz_DatumZeit).AsDateTime := // Datum/Zeit auf Stunde
              IncHour(dt);
            FieldByName(C_TfDSatz_ReferenzNr).AsInteger := iONr; // Referenznummer
            FieldByName(C_TfDSatz_OrdNr).AsInteger := iONr; // Ordnungsnummer
            Inc(iONr);
            dt := FieldByName(C_TfDSatz_DatumZeit).AsDateTime;
            for i := 0 to KanalZahl-1 do begin
              if (not pChannelExists[i]) then Continue;

              SetKanalTyp(i, pKanalTyp[i]);
              SetWert(i, 0);
              SetStatus(i, $80);
              pValues[i] := 0;
              pStatus[i] := $80;
            end;
            Post;
            Next;
          end;
        end;
      end
      else begin
        for i := 0 to KanalZahl-1 do begin
          if (pKanalTyp[i] = kt_ZS) then begin
            // ignorieren
          end
          else if (pKanalTyp[i] = kt_ZW) then begin
            pValues[i] := pValues[i] + GetWert(i);
          end
          else if (pKanalTyp[i] = kt_MW) then begin   // 01.02.2010
            iCnt := Round(pValues[i]) div 10000000;   // Anzahl der Werte
            fValue := pValues[i] - (iCnt * 10000000); // Offset abziehen
            fValue := ((fValue * iCnt) + GetWert(i)) / (iCnt + 1);

            pValues[i] := fValue + ((iCnt+1) * 10000000); // Cnt als Offset
          end
          else begin
            // ignorieren
          end;
        end;

        Next;    // Dämliches System: EOF wird nicht automatisch gesetzt!
        bEof := (Eof);    // 01.02.2010
        if (not bEof) then Prior;
        Delete;  // Twischenwert
        if (bEof) then Next;    // Einmal around worken ...
      end;
    end;
    First;
    if (dtVon > 1) then begin
      while (FieldByName(C_TfDSatz_DatumZeit).AsDateTime < (dtVon -
        EncodeTime(0, 0, FTimeFuzzy div 1000, FTimeFuzzy mod 1000))) and
        (not Eof)
      do Delete;
    end;

    IndexFieldNames := C_TfDSatz_DatumZeit; // 26.02.2010
  except
    on E:Exception do
      raise Exception.Create('TLGZStd.CalcInMemData (' + FAktTable.TableName +
      '): InMem-Table not present(' + E.Message + ')');  // 24.10.2013
  end;
end;

{------------------------------------------}
function TLGZStd.GetOrdnungsnummer: LongInt;
{------------------------------------------}
begin
  CheckOpened;
  if (IsOriData)
  then Result := DataBuf.FieldByName(C_TfDSatz_OrdNr).asInteger
  else Result := DataBuf.FieldByName(C_Tf_LGZStd_OrdNr).asInteger;
end;

{------------------------------------------}
procedure TLGZStd.SetOrdnungsnummer(iValue: LongInt);
{------------------------------------------}
begin
  CheckOpened;
  if (IsOriData) then begin
    DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger := iValue;
    DataBuf.FieldByName(C_TfDSatz_OrdNr).asInteger := iValue;
  end
  else DataBuf.FieldByName(C_Tf_LGZStd_OrdNr).asInteger := iValue;
end;

{-----------------------------------}
function TLGZStd.GetDatum: TDateTime;
{-----------------------------------}
begin
  CheckOpened;
  if (IsOriData)
  then Result := DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime
  else Result := DataBuf.FieldByName(C_Tf_LGZStd_DatumZeit).asDateTime;
end;

{-----------------------------------}
procedure TLGZStd.SetDatum(dtValue: TDateTime);
{-----------------------------------}
begin
  CheckOpened;
  if (IsOriData)
  then DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime := dtValue
  else DataBuf.FieldByName(C_Tf_LGZStd_DatumZeit).asDateTime := dtValue;
end;

{-----------------------------------}
function TLGZStd.GetYear: Word;
{-----------------------------}
var
  m, d : word;
begin
  CheckOpened;
  if (IsOriData)
  then DecodeDate(
    DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime, Result, m, d)
  else DecodeDate(
    DataBuf.FieldByName(C_Tf_LGZStd_DatumZeit).asDateTime, Result, m, d);
end;

{------------------------------}
function TLGZStd.GetMonth: Word;
{------------------------------}
var
  y, d : word;
begin
  CheckOpened;
  if (IsOriData)
  then DecodeDate(
    DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime, y, Result, d)
  else DecodeDate(
    DataBuf.FieldByName(C_Tf_LGZStd_DatumZeit).asDateTime, y, Result, d);
end;

{----------------------------}
function TLGZStd.GetDay: Word;
{----------------------------}
var
  y, m : word;
begin
  CheckOpened;
  if (IsOriData)
  then DecodeDate(
    DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime, y, m , Result)
  else DecodeDate(
    DataBuf.FieldByName(C_Tf_LGZStd_DatumZeit).asDateTime, y, m, Result);
end;

{-----------------------------}
function TLGZStd.GetHour: Word;
{-----------------------------}
var
  m, s, ms : word;
begin
  CheckOpened;
  if (IsOriData)
  then DecodeTime(
    DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime, Result, m, s, ms)
  else DecodeTime(
    DataBuf.FieldByName(C_Tf_LGZStd_DatumZeit).asDateTime, Result, m, s, ms);
end;

{-------------------------------}
function TLGZStd.GetMinute: Word;
{-------------------------------}
var
  h, s, ms : word;
begin
  CheckOpened;
  if (IsOriData)
  then DecodeTime(
    DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime, h, Result, s, ms)
  else DecodeTime(
    DataBuf.FieldByName(C_Tf_LGZStd_DatumZeit).asDateTime, h, Result, s, ms);
end;

{-------------------------------}
function TLGZStd.GetSecond: Word;
{-------------------------------}
var
  h, m, ms : word;
begin
  CheckOpened;
  if (IsOriData)
  then DecodeTime(
    DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime, h, m, Result, ms)
  else DecodeTime(
    DataBuf.FieldByName(C_Tf_LGZStd_DatumZeit).asDateTime, h, m, Result, ms);
end;

{-----------------------------------}
function TLGZStd.GetSatzStatus: Byte;
{-----------------------------------}
begin
  CheckOpened;
  if (IsOriData)
  then Result := StrToIntDef(DataBuf.FieldByName(C_TfDSatz_Status).AsString, 0)
  else Result := DataBuf.FieldByName(C_Tf_LGZStd_SatzStatus).asInteger;
end;

{-------------------------------------------}
procedure TLGZStd.SetSatzStatus(iValue: Byte);
{-------------------------------------------}
begin
  CheckOpened;
  if (IsOriData)
  then DataBuf.FieldByName(C_TfDSatz_Status).asInteger := iValue
  else DataBuf.FieldByName(C_Tf_LGZStd_SatzStatus).asInteger := iValue;
end;

{-------------------------------------------}
function TLGZStd.GetWert (Index: Word): double;
{-------------------------------------------}
var
  iRef, iSatzIndex : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    if (Assigned(FPreparedData)) then begin
      if (FPreparedData.FieldDefs.IndexOf(
         'Wert' + Format('%.3d%', [Index+1])) >= 0) then
      begin
        if (not FPreparedData.FieldByName(
          'Wert' + Format('%.3d%', [Index+1])).IsNull)
        then Result := FPreparedData.
          FieldByName('Wert' + Format('%.3d%', [Index+1])).asFloat
        else Result := 0;
      end
      else Result := 0;
    end
    else begin
      iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
      iSatzIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
      if (iSatzIndex > 0) then begin
        with TTableExt.Create(nil) do
        try
          DatabaseName := Self.Database.DatabaseName;
          SessionName := Self.Database.SessionName;
          TableName := C_TbDWert + Format('%.3d%.4d', [Index+1, iSatzIndex]);
          if (Exists) then begin
            SafeUpdateIndexDefs;
            IndexName := IndexDefs[0].Name;
            Open;
            if (FindKey([iRef]))
            then Result := FieldByName(C_TfDWert_Wert).AsFloat
            else Result := 0;
            Close;
          end
          else Result := 0;
        finally
          Free;
        end;
      end
      else Result := 0;
    end;
  end
  else Result :=
    DataBuf.FieldByName(Format(C_Tf_LGZStd_KanalWert, [Index])).asInteger;
end;

{--------------------------------------------------}
procedure TLGZStd.SetWert (Index: Word; fValue: double);
{-------------------------------------------}
var
  iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
    iSatzIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
    if (iSatzIndex > 0) then begin
      CreatetbDWert(Database, iSatzIndex, Index+1);
      with TTableExt.Create(nil) do
      try
        DatabaseName := Self.Database.DatabaseName;
        SessionName := Self.Database.SessionName;
        TableName := C_TbDWert + Format('%.3d%.4d', [Index, iSatzIndex]);
        if (Exists) then begin
          SafeUpdateIndexDefs;
          IndexName := IndexDefs[0].Name;
          Open;
          if (FindKey([iRef]))
          then Edit
          else begin
            Append;
            FieldByName(C_TfDWert_ReferenzNr).asInteger := iRef;
          end;
          FieldByName(C_TfDWert_Wert).AsFloat := fValue;
          Post;
          Close;
        end;
      finally
        Free;
      end;
    end
    else Exception.Create(Format (S_TabFehlt, [C_TbDWert +
      Format('%.3d%.4d', [Index, iSatzIndex])]));
  end
  else DataBuf.FieldByName(Format(C_Tf_LGZStd_KanalWert, [Index])).asInteger :=
    Trunc(fValue);
end;

{--------------------------------------------------}
function TLGZStd.GetKanalStatus (Index: Word): Byte;
{--------------------------------------------------}
var
  iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    if (Assigned(FPreparedData)) then begin
      if (FPreparedData.FieldDefs.IndexOf(
         'Status' + Format('%.3d%', [Index+1])) >= 0) then
      begin
        if (not FPreparedData.FieldByName(
           'Status' + Format('%.3d%', [Index+1])).IsNull)
        then Result := FPreparedData.
           FieldByName('Status' + Format('%.3d%', [Index+1])).asInteger
        else Result := 128;
      end
      else Result := 128;
    end
    else begin
      iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
      iSatzIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
      if (iSatzIndex > 0) then begin
        with TTableExt.Create(nil) do
        try
          DatabaseName := Self.Database.DatabaseName;
          SessionName := Self.Database.SessionName;
          TableName := C_TbDWert + Format('%.3d%.4d', [Index+1, iSatzIndex]);
          if (Exists) then begin
            SafeUpdateIndexDefs;
            IndexName := IndexDefs[0].Name;
            Open;
            if (FindKey([iRef]))
            then Result := FieldByName(C_TfDWert_Status).asInteger
            else Result := 128;
            Close;
          end
          else Result := 128;
        finally
          Free;
        end;
      end
      else Result := 128;  // Wert fehlt
    end;
  end
  else Result :=
    DataBuf.FieldByName(Format(C_Tf_LGZStd_KanStatus, [Index])).asInteger;
end;

{-------------------------------------------}
procedure TLGZStd.SetKanalStatus (Index: Word; iValue: Byte);
{-------------------------------------------}
var
  iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    iSatzIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
    iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
    if (iSatzIndex > 0) then begin
      CreatetbDWert(Database, iSatzIndex, Index+1);
      with TTableExt.Create(nil) do
      try
        DatabaseName := Self.Database.DatabaseName;
        SessionName := Self.Database.SessionName;
        TableName := C_TbDWert + Format('%.3d%.4d', [Index, iSatzIndex]);
        if (Exists) then begin
          SafeUpdateIndexDefs;
          IndexName := IndexDefs[0].Name;
          Open;
          if (FindKey([iRef]))
          then Edit
          else begin
            Append;
            FieldByName(C_TfDWert_ReferenzNr).asInteger := iRef;
          end;
          FieldByName(C_TfDWert_Status).asInteger := iValue;
          Post;
          Close;
        end;
      finally
        Free;
      end;
    end
    else Exception.Create(Format ( S_TabFehlt, [C_TbDWert +
      Format('%.3d%.4d', [Index, iSatzIndex])]));
  end
  else DataBuf.FieldByName(Format(C_Tf_LGZStd_KanStatus, [Index])).asInteger :=
    iValue;
end;

{-------------------------------------------}
procedure TLGZStd.SetDataFile (FileNr: Word);
{-------------------------------------------}
begin
  if (IsOriData)
  then TableName := C_TbDSatz + Format('%.4d', [FileNr])
  else begin
    if (isLangzeit)
    then TableName := Format (C_Tb_LGZStd, [FileNr])
    else TableName := Format (C_Tb_LGZStdM, [FileNr]); { GeDa }
  end;

  if (not Assigned(FAktTable)) then begin
    FAktTable := TTableExt.Create(nil);
    FAktTable.DatabaseName := FDatabase.DatabaseName;
    FAktTable.SessionName := FDatabase.SessionName;
  end;
  FAktTable.TableName := Self.TableName;
end;

{---------------------------------------}
function TLGZStd.GetTableName: string;
{---------------------------------------}
begin
  Result := TableName;
end;

{ Öffnet aktuelle Tabelle                                           }
{ Parameter: 0=open; 1=shared; 2=exclusive                          }
{ Rückgabe: OK                                                      }
{-------------------------------------------------------------------}
function TLGZStd.GetOpen(iState: integer): boolean;
{-------------------------------------------------------------------}
begin
  CreateLgzStdTb;

  FState := lgzKennung;
  Result := inherited GetOpen(iState);
  FirstOrdnungsNummer := GetOrdnungsNummer;
end;

{------------------------------------------------------------------------}
function TLGZStd.SearchOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
{------------------------------------------------------------------------}
begin
  CheckOpened;

  FAktTable.SafeUpdateIndexDefs;
  if (FAktTable.IndexName <> FAktTable.IndexDefs[0].Name) then begin
    FAktTable.Close;
    FAktTable.IndexName := FAktTable.IndexDefs[0].Name;
    if (not FAktTable.OpenShared) then FState := lgzClosed;

    CheckOpened;
  end;

  Result := FAktTable.FindKey([AOrdnungsNummer]);
end;

{--------------------------------------------------------------------------}
function TLGZStd.SearchGEOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
{--------------------------------------------------------------------------}
var
  Delta: LongInt;
begin
  Delta := AOrdnungsNummer - FirstOrdnungsNummer;
  if (Delta <= 0) then Result := SearchPos (0) else Result := SearchPos (Delta);
end;

{--------------------------------------------------------}
function TLGZStd.SearchDatum (ADatum: TDateTime): Boolean;
{--------------------------------------------------------}
var
  sIndexName, sFieldName : string;
  pDataSet               : TDataSet;
begin
  Result := False;
  CheckOpened;

  if (IsOriData) then begin
    sIndexName := CSDat;
    sFieldName := C_TfDSatz_DatumZeit;
  end
  else begin
    sIndexName := C_TI_LGZStd_ixDateTime;
    sFieldName := C_Tf_LGZStd_DatumZeit;
  end;

  if (FAktTable.IndexName <> sIndexName) then begin
    FAktTable.Close;
    FAktTable.IndexName := sIndexName;
    if (not FAktTable.OpenShared) then FState := lgzClosed;
  end;

  if (FState = lgzOpened) then begin
    if (IsOriData) and (Assigned(FPreparedData)) then begin
      FPreparedData.IndexFieldNames := sFieldName;
      Result := FPreparedData.FindKey([ADatum]);
      pDataSet := FPreparedData;
    end
    else begin
      Result := FAktTable.FindKey([ADatum]);
      pDataSet := FAktTable;
    end;
    if (not Result) then begin  // 19.04.2005
      pDataSet.Last;
      if (pDataSet.Bof) or
        (pDataSet.FieldByName(sFieldName).asDateTime < ADatum)
      then Exit;
      pDataSet.First;
      if (pDataSet.Eof) or
        (pDataSet.FieldByName(sFieldName).asDateTime > ADatum)
      then Exit;

      while not (pDataSet.Eof) do begin
        if (SameValue(pDataSet.FieldByName(sFieldName).AsFloat, ADatum,
          EncodeTime(0, 0, FTimeFuzzy div 1000, FTimeFuzzy mod 1000))) then
        begin
          Result := True;
          Exit;
        end;
        pDataSet.Next;
      end;
    end;
  end;
end;

{----------------------------------------------------------}
function TLGZStd.SearchGEDatum (ADatum: TDateTime): Boolean;
{----------------------------------------------------------}
var
  sIndexName : string;
begin
  Result := False;
  CheckOpened;

  if (IsOriData) and (Assigned(FPreparedData)) then begin
    FPreparedData.IndexFieldNames := C_TfDSatz_DatumZeit; // 11.12.2013
    if (FPreparedData.FindKey([ADatum]))
    then Result := True
    else begin
      FPreparedData.FindNearest([ADatum]);
      Result := (Datum > ADatum);
    end;
  end
  else begin
    if (IsOriData)
    then sIndexName := CSDat
    else sIndexName := C_TI_LGZStd_ixDateTime;

    if (FAktTable.IndexName <> sIndexName) then begin
      FAktTable.Close;
      FAktTable.IndexName := sIndexName;
      if (not FAktTable.OpenShared) then FState := lgzClosed;
    end;

    if (FState = lgzOpened) then begin
      if (FAktTable.FindKey([ADatum]))
      then Result := True
      else begin
        FAktTable.FindNearest([ADatum]);
        Result := (Datum > ADatum);
      end;
    end;
  end;
end;

{---------------------------------}
function TLGZStd.ReadBuffer: LGZSRec;
{---------------------------------}
var
  i : integer;
begin
  CheckOpened;
  Result.SatzStatus := SatzStatus;
  DateTimeToRec(Datum, Result.Datum, Result.Zeit);
  for i := Low (Result.Kanal) to High (Result.Kanal) do begin  // Vorbelegung Kanäle
    Result.Kanal[i].KanalStatus := 0;
    Result.Kanal[i].Wert := 0;
  end;
  for i := 1 to FKanalZahl do begin  // Zuweisung Kanäle
    Result.Kanal[i].KanalStatus := KanalStatus[i-1];
    Result.Kanal[i].Wert := Trunc(Wert[i-1]);
  end;
end;

{---------------------------------}
function TLGZStd.ReadRohBuffer: RohSRec;
{---------------------------------}
var
  i : integer;
begin
  CheckOpened;
  Result.SatzStatus := SatzStatus;
  Result.DatumZeit := Datum;
  for i := Low (Result.Kanal) to High (Result.Kanal) do begin  // Vorbelegung Kanäle
    Result.Kanal[i].KanalStatus := 0;
    Result.Kanal[i].Wert := 0;
    Result.KanalOrig[i].KanalStatus := 0;
    Result.KanalOrig[i].Wert := 0;
  end;
  for i := 1 to FKanalZahl do begin  // Zuweisung Kanäle
    Result.KanalOrig[i].KanalStatus := KanalStatus[i-1];
    Result.KanalOrig[i].Wert := Wert[i-1];
  end;
end;

{---------------------------------}
procedure TLGZStd.WriteBuffer(pBuffer: LGZSRec);
{---------------------------------}
var
  iONr : Longint;
  i    : byte;
begin
  CheckOpened;
  if (not Eof) then begin
    FAktTable.Edit;
    DataBuf := FAktTable.Fields;
  end
  else begin
    if (Eof) and (Bof) then iONr := 1 else iONr := Ordnungsnummer + 1;
    FAktTable.Append;
    DataBuf := FAktTable.Fields;
    Ordnungsnummer := iONr;
  end;

  if (not IsOriData) then
    FAktTable.FieldByName(C_Tf_LGZStd_KanalZahl).asInteger := KanalZahl;
  SatzStatus := pBuffer.SatzStatus;
  Datum :=
    EncodeDate(pBuffer.Datum.Year, pBuffer.Datum.Month, pBuffer.Datum.Day) +
    EncodeTime(pBuffer.Zeit.Hour, pBuffer.Zeit.Min, pBuffer.Zeit.Sec,
               pBuffer.Zeit.Hsec);
  for i := 1 to FKanalZahl do begin
    Wert[i-1] := pBuffer.Kanal[i].Wert;
    KanalStatus[i-1] := pBuffer.Kanal[i].KanalStatus;
  end;
  FAktTable.Post;
  Next;
end;

{---------------------------------}
procedure TLGZStd.WriteRohBuffer(pBuffer: RohSRec);
{---------------------------------}
var
  iONr : Longint;
  i    : byte;
begin
  CheckOpened;
  if (not Eof) then begin
    FAktTable.Edit;
    DataBuf := FAktTable.Fields;
  end
  else begin
    if (Eof) and (Bof) then iONr := 1 else iONr := Ordnungsnummer + 1;
    FAktTable.Append;
    DataBuf := FAktTable.Fields;
    Ordnungsnummer := iONr;
  end;

  if (not IsOriData) then
    FAktTable.FieldByName(C_Tf_LGZStd_KanalZahl).asInteger := KanalZahl;
  SatzStatus := pBuffer.SatzStatus;
  Datum := pBuffer.DatumZeit;
  for i := 1 to FKanalZahl do begin
    Wert[i-1] := pBuffer.KanalOrig[i].Wert;
    KanalStatus[i-1] := pBuffer.KanalOrig[i].KanalStatus;
  end;
  FAktTable.Post;
  Next;
end;

{ TLGZTag }

{---------------------------------------------}
destructor TLgzTag.Destroy;
{---------------------------------------------}
begin
  FreeAndNil(FAktKontrollTable);

  inherited;
end;

{ Setzt die Kennung                           }
{---------------------------------------------}
procedure TLgzTag.SetKennung (Value: TKennung);
{---------------------------------------------}
begin
  ArchGrpNr := ag_mrg_EingZ;  // AG2 = "Eingangszähler"

  inherited;
end;

{------------------------------------------}
function TLGZTag.Search (const AKennung: TKennung): Boolean;
{------------------------------------------}
begin
  ArchGrpNr := ag_mrg_EingZ;  // AG2 = "Eingangszähler"

  Result := inherited Search(AKennung);
end;

{ Legt ggf. Tabelle an                        }
{---------------------------------------------}
procedure TLgzTag.CreateLgzTagTb;
{---------------------------------------------}
var
  i, iIndex : integer;
begin
  with TTableExt.Create(nil) do
  try
    DatabaseName := FDatabase.DatabaseName;
    SessionName := FDatabase.SessionName;
    TableName := Self.TableName;
    if (not Exists) then begin
      if (IsOriData) then begin
        iIndex := StrToIntDef(Copy(Self.TableName, 5, 4), -1);
        if (iIndex > 0) then begin
          CreatetbDSatz(Database, iIndex);
          for i := 1 to KanalZahl do CreatetbDWert(Database, iIndex, i);
        end
        else Exception.Create(Format (S_OriDatenIndexFalsch, [Copy(Self.TableName, 5, 4)]));
      end
      else begin
        with FieldDefs do begin
          Add(C_Tf_LGZTag_OrdNr, ftInteger, 0, True);
          Add(C_Tf_LGZTag_KanalZahl, ftInteger, 0, True);
          Add(C_Tf_LGZTag_SatzStatus, ftSmallint, 0, False);
          Add(C_Tf_LGZTag_DatumZeit, ftDateTime, 0, True);
          Add(C_Tf_LGZTag_Stunden, ftSmallInt, 0, True);
          for i := 0 to Kanalzahl-1 do begin
            Add(Format(C_Tf_LGZTag_EKanStatus, [i]), ftSmallint, 0, False);
            Add(Format(C_Tf_LGZTag_EKanalWert, [i]), ftInteger, 0, False);
          end;
          for i := 0 to Kanalzahl-1 do begin
            Add(Format(C_Tf_LGZTag_KKanStatus, [i]), ftSmallint, 0, False);
            Add(Format(C_Tf_LGZTag_KKanalWert, [i]), ftInteger, 0, False);
          end;
        end;
        with IndexDefs do begin
          Add('ixmain', C_Tf_LGZTag_OrdNr, [ixPrimary, ixUnique]);
          Add(C_TI_LGZTag_ixDateTime, C_Tf_LGZTag_DatumZeit, [ixCaseInsensitive]);
        end;
        CreateTable;
      end;
    end;

    if (SafeUpdateIndexDefs)
    then IndexName := IndexDefs[0].Name
    else raise Exception.Create('TLgzTag.CreateLgzTagTb (' + Self.TableName +
      '): Fatal access error (table locked/deleted?)');
  finally
    Free;
  end;
end;

{------------------------------------------}
function TLGZTag.EraseData(
  dtVon: TDateTime = 0; dtBis: TDateTime = 0): boolean;
{------------------------------------------}

  function DropOriTable(pSatzTable: TTable): boolean;
  var
    i, iIndex : integer;
  begin
    try
      iIndex := StrToIntDef(Copy(pSatzTable.TableName, 5, 4), -1);
      with TTable.Create(nil) do
      try
        DatabaseName := FDatabase.DatabaseName;
        SessionName := FDatabase.SessionName;
        with TQueryExt.Create(nil) do
        try
          DatabaseName := FDatabase.DatabaseName;
          SessionName := FDatabase.SessionName;
          for i := 1 to KanalZahl do begin
            TableName := C_TbDWert + Format('%.3d%.4d', [i, iIndex]);
            if (Exists) then begin
              Sql.Add('DROP TABLE ' + TableName);
              ExecSql;
              Sql.Clear;
            end;
          end;
          if (pSatzTable.Exists) then begin
            Sql.Add('DROP TABLE ' + pSatzTable.TableName);
            ExecSql;
          end;
          FState := lgzClosed;
        finally
          Free;
        end;
      finally
        Free;
      end;
      Result := True;
    except
      Result := False;
    end;
  end;

  function DeleteFromOriTable(pSatzTable: TTable): boolean;
  var
    i, iIndex : integer;
    sWA       : string;
  begin
    try
      iIndex := StrToIntDef(Copy(pSatzTable.TableName, 5, 4), -1);
      with TTable.Create(nil) do
      try
        DatabaseName := FDatabase.DatabaseName;
        SessionName := FDatabase.SessionName;
        with TQueryExt.Create(nil) do
        try
          DatabaseName := FDatabase.DatabaseName;
          SessionName := FDatabase.SessionName;
          for i := 1 to KanalZahl do begin
            TableName := C_TbDWert + Format('%.3d%.4d', [i, iIndex]);
            if (Exists) then begin
              Sql.Add('DELETE FROM ' + TableName);
              Sql.Add('WHERE ' + C_TfDWert_ReferenzNr + ' IN ');
              Sql.Add('(');
              Sql.Add('SELECT ' + C_TfDSatz_ReferenzNr);
              Sql.Add('FROM ' + pSatzTable.TableName);
              sWA := 'WHERE ';
              if (dtVon > 0) then begin
                Sql.Add(sWA + C_TfDSatz_DatumZeit + ' >= :Von');
                ParamByName('Von').asDateTime := dtVon;
                sWA := 'AND ';
              end;
              if (dtBis > 0) then begin
                Sql.Add(sWA + C_TfDSatz_DatumZeit + ' <= :Bis');
                ParamByName('Bis').asDateTime := dtBis;
              end;
              Sql.Add(')');
              ExecSql;
              Sql.Clear;
            end;
          end;
          if (pSatzTable.Exists) then begin
            Sql.Add('DELETE FROM ' + pSatzTable.TableName);
            sWA := 'WHERE ';
            if (dtVon > 0) then begin
              Sql.Add(sWA + C_TfDSatz_DatumZeit + ' >= :Von');
              ParamByName('Von').asDateTime := dtVon;
              sWA := 'AND ';
            end;
            if (dtBis > 0) then begin
              Sql.Add(sWA + C_TfDSatz_DatumZeit + ' <= :Bis');
              ParamByName('Bis').asDateTime := dtBis;
            end;
            ExecSql;
          end;
        finally
          Free;
        end;
      finally
        Free;
      end;
      Result := True;
    except
      Result := False
    end;
  end;

var
  sWA : string;
begin
  Result := False;
  if (FState = lgzKennung) then begin
    if (FAktTable.Exists) then begin
      if (IsOriData) then begin
        if (dtVon = 0) and (dtBis = 0) then begin  // Daten komplett löschen !
          Result :=
            (DropOriTable(FAktTable)) and (DropOriTable(FAktKontrollTable));
        end
        else begin
          Result := (DeleteFromOriTable(FAktTable)) and
            (DeleteFromOriTable(FAktKontrollTable));
        end;
      end
      else begin
        with TQueryExt.Create(nil) do
        try
          DatabaseName := FDatabase.DatabaseName;
          SessionName := FDatabase.SessionName;
          if (dtVon = 0) and (dtBis = 0) then begin  // Daten komplett löschen !
            Sql.Add('DROP TABLE ' + FAktTable.TableName);
            Result := ExecSql;
            if (Result) then FState := lgzClosed;
          end
          else begin
            Sql.Add('DELETE FROM ' + FAktTable.TableName);
            if (dtVon > 0) then begin
              Sql.Add('WHERE ' + C_Tf_LGZTag_DatumZeit + ' >= :Von');
              ParamByName('Von').asDateTime := dtVon;
            end;
            if (dtBis > 0) then begin
              if (Sql.Count = 1) then sWA := 'WHERE ' else sWA := 'AND ';
              Sql.Add(sWA + C_Tf_LGZTag_DatumZeit + ' <= :Bis');
              ParamByName('Bis').asDateTime := dtBis;
            end;
            Result := ExecSql;
          end;
        finally
          Free;
        end;
      end;
    end;
  end
  else raise TLGZException.Create(S_TabNichtGefundenOderGeoeffnet);
end;

{------------------------------------------}
procedure TLGZTag.SetSql(pQuery: TQuery; dtVon, dtBis: TDateTime);
{------------------------------------------}
var
  i, iIndex : integer;
  pSlTbs    : TStrings;
begin
  if (IsOriData) then begin
    if (pQuery = TQuery(FDataProvider.DataSet))
    then iIndex := StrToIntDef(Copy(TableName, 5, 4), -1)
    else iIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    pSlTbs := TStringList.Create;  // Stringliste für Wertetabellen
    try
      with TTable.Create(nil) do
      try
        DatabaseName := FDatabase.DatabaseName;
        SessionName := FDatabase.SessionName;
        // Existierende Wertetabellen finden und eintragen
        for i := 1 to KanalZahl do begin
          TableName := C_TbDWert + Format('%.3d%.4d', [i, iIndex]);
          if (Exists) then pSlTbs.Add(TableName);
        end;
      finally
        Free;
      end;

      // SQL-Anweisung in Query eintragen
      CreateArchivSql(iIndex, pQuery, pSlTbs, dtVon, dtBis, 1);
    finally
      pSlTbs.Free;
    end;
  end
  else pQuery.Sql.Text := '';
end;

{------------------------------------------}
procedure TLGZTag.SetSql(pQuery: TQuery; iVon, iBis: integer);
{------------------------------------------}
var
  i, iIndex : integer;
  pSlTbs    : TStrings;
begin
  if (IsOriData) then begin
    if (pQuery = TQuery(FDataProvider.DataSet))
    then iIndex := StrToIntDef(Copy(TableName, 5, 4), -1)
    else iIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    pSlTbs := TStringList.Create;  // Stringliste für Wertetabellen
    try
      with TTable.Create(nil) do
      try
        DatabaseName := FDatabase.DatabaseName;
        SessionName := FDatabase.SessionName;
        // Existierende Wertetabellen finden und eintragen
        for i := 1 to KanalZahl do begin
          TableName := C_TbDWert + Format('%.3d%.4d', [i, iIndex]);
          if (Exists) then pSlTbs.Add(TableName);
        end;
      finally
        Free;
      end;

      // SQL-Anweisung in Query eintragen
      CreateArchivSql(iIndex, pQuery, pSlTbs, iVon, iBis, 3);
    finally
      pSlTbs.Free;
    end;
  end
  else pQuery.Sql.Text := '';
end;

{ Bereitet Datenmenge für schnellere Abfrage  }
{ Parameter: Von-Bis als DateTime             }
{---------------------------------------------}
procedure TLGZTag.Prepare(dtVon: TDateTime = 0; dtBis: TDateTime = 0);
{---------------------------------------------}
var
  q : TQuery;
begin
  inherited;

  if (FIsOriData) and (Assigned(FAktKontrollTable)) and
    (FAktKontrollTable.Exists) then
  begin
    if (not Assigned(FPreparedKontrollData)) then begin
      FPreparedKontrollData := TClientDataSet.Create(nil);
      FDataProviderKontroll := TProvider.Create(nil);
      FDataProviderKontroll.Name :=
        'dp' + IntToStr(Integer(FDataProviderKontroll));
      q := TQuery.Create(nil);
      q.DatabaseName := FDatabase.DatabaseName;
      q.SessionName := FDatabase.SessionName;
      FDataProviderKontroll.DataSet := q;
      FPreparedKontrollData.SetProvider(FDataProviderKontroll);
    end
    else q := TQuery(FDataProviderKontroll.DataSet);

    FPreparedKontrollData.Active := False;

    SetSql(q, dtVon, dtBis);
    if (q.Sql.Text <> '') then begin
      FPreparedKontrollData.IndexFieldNames := C_TfDSatz_DatumZeit; // 26.02.2010
      FPreparedKontrollData.Active := True;
    end
    else begin
      FPreparedKontrollData.Active := False;
      FreeAndNil(FPreparedKontrollData);
      q := TQuery(FDataProviderKontroll.DataSet);
      FreeAndNil(FDataProviderKontroll);
      FreeAndNil(q);
    end;
  end;
end;

{ Bereitet Datenmenge für schnellere Abfrage  }
{ Parameter: Von-Bis als Referenzindices      }
{---------------------------------------------}
procedure TLGZTag.Prepare(iVon: integer = 0; iBis: integer = 0);
{---------------------------------------------}
var
  q : TQuery;
begin
  inherited;

  if (FIsOriData) and (Assigned(FAktKontrollTable)) and
    (FAktKontrollTable.Exists) then
  begin
    if (not Assigned(FPreparedKontrollData)) then begin
      FPreparedKontrollData := TClientDataSet.Create(nil);
      FDataProviderKontroll := TProvider.Create(nil);
      FDataProviderKontroll.Name :=
        'dp' + IntToStr(Integer(FDataProviderKontroll));
      q := TQuery.Create(nil);
      q.DatabaseName := FDatabase.DatabaseName;
      q.SessionName := FDatabase.SessionName;
      FDataProviderKontroll.DataSet := q;
      FPreparedKontrollData.SetProvider(FDataProviderKontroll);
    end
    else q := TQuery(FDataProviderKontroll.DataSet);

    FPreparedKontrollData.Active := False;

    SetSql(q, iVon, iBis);
    if (q.Sql.Text <> '') then begin
      FPreparedKontrollData.IndexFieldNames := C_TfDSatz_ReferenzNr;
      FPreparedKontrollData.Active := True;
    end
    else begin
      FPreparedKontrollData.Active := False;
      FreeAndNil(FPreparedKontrollData);
      q := TQuery(FDataProviderKontroll.DataSet);
      FreeAndNil(FDataProviderKontroll);
      FreeAndNil(q);
    end;
  end;
end;

{ Weitergehende Berechnung der Daten          }
{---------------------------------------------}
procedure TLGZTag.CalcInMemData(dtVon: TDateTime = 0);
{---------------------------------------------}

   function GetHour(dt: TDateTime): TDateTime;
   var
     h,n,s,ms : word;
     iInc     : byte;
   begin
     DecodeTime(dt, h, n, s, ms);
     if (n < 30) then iInc := 0 else iInc := 1;
     Result := Trunc(dt) + IncHour(EncodeTime(h, 0, 0, 0), iInc);
   end;

   function IsHour(dt: TDateTime): boolean;
   begin
     Result := SameValue(dt, GetHour(dt),
       EncodeTime(0, 0, FTimeFuzzy div 1000, FTimeFuzzy mod 1000));
   end;

   function IsNextHour(dtPrior, dtNext: TDateTime): Shortint;
   var
     dt       : TDateTime;
   begin
     dt := IncHour(GetHour(dtPrior), 1);
     if (SameValue(dt, dtNext,
       EncodeTime(0, 0, FTimeFuzzy div 1000, FTimeFuzzy mod 1000)))
     then Result := 0
     else if (dt > dtNext) then Result := -1
     else Result := 1;
   end;

  function GetKanalTyp(iIndex: integer): string;
  begin
    if (FPreparedData.FieldDefs.IndexOf('Wert' + Format('%.3d%', [iIndex+1])) >= 0)
    then Result := FPreparedData.
      FieldByName('Kanaltyp' + Format('%.3d%', [iIndex+1])).AsString
    else Result := '';
  end;

  procedure SetKanalTyp(iIndex: integer; const sText: string);
  begin
    FPreparedData.FieldByName(
      'Kanaltyp' + Format('%.3d%', [iIndex+1])).AsString := sText;
  end;

  function GetWert(iIndex: integer): double;
  begin
    if (FPreparedData.
      FieldByName('Wert' + Format('%.3d%', [iIndex+1])).IsNull) 
    then Result := 0
    else Result := FPreparedData.
      FieldByName('Wert' + Format('%.3d%', [iIndex+1])).AsFloat;
  end;

  procedure SetWert(iIndex: integer; fValue: double);
  begin
    FPreparedData.FieldByName(
      'Wert' + Format('%.3d%', [iIndex+1])).AsFloat := fValue;
  end;

  function GetStatus(iIndex: integer): byte;
  begin
    if (FPreparedData.
      FieldByName('Status' + Format('%.3d%', [iIndex+1])).IsNull) 
    then Result := 0
    else Result := FPreparedData.
      FieldByName('Status' + Format('%.3d%', [iIndex+1])).AsInteger;
  end;

  procedure SetStatus(iIndex: integer; iStatus: byte);
  begin
    FPreparedData.FieldByName(
      'Status' + Format('%.3d%', [iIndex+1])).AsInteger := iStatus;
  end;

var
  pChannelExists : array [0..C_MaxKanalZahl-1] of boolean;
  pKanalTyp     : array [0..C_MaxKanalZahl-1] of string[3];
  pValues       : array [0..C_MaxKanalZahl-1] of double;
  pPrevValues   : array [0..C_MaxKanalZahl-1] of double;
  pStatus       : array [0..C_MaxKanalZahl-1] of byte;
  dt            : TDateTime;
  i             : integer;    // 14.06.2013
  iCnt, iONr    : Int64;      // 14.06.2013
  f, fValue     : double;
  iStatus       : byte;
  bEof          : boolean;    // 01.02.2010
begin
  if (Assigned(FPreparedData)) and (FVerbrauch) then with FPreparedData do
  try
    IndexFieldNames := C_TfDSatz_DatumZeit + ';' + C_TfDSatz_ReferenzNr;
    dt := 0;
    iONr := 1;
    First;

    for i := 0 to C_MaxKanalZahl-1 do pChannelExists[i] :=
      (FPreparedData.FieldDefs.IndexOf('Wert' + Format('%.3d%', [i+1])) >= 0);
    for i := 0 to C_MaxKanalZahl-1 do pKanalTyp[i] := GetKanalTyp(i);
    for i := 0 to C_MaxKanalZahl-1 do pValues[i] := 0;
    for i := 0 to C_MaxKanalZahl-1 do pPrevValues[i] := 0;
    for i := 0 to C_MaxKanalZahl-1 do pStatus[i] := $80;

    while (not Eof) do begin
      if (IsHour(FieldByName(C_TfDSatz_DatumZeit).AsDateTime)) then begin
        if (dt = 0) or
          (IsNextHour(dt, FieldByName(C_TfDSatz_DatumZeit).AsDateTime) >= 0) then
        begin
          Edit;

          FieldByName(C_TfDSatz_DatumZeit).AsDateTime := // Datum/Zeit auf Stunde
            GetHour(FieldByName(C_TfDSatz_DatumZeit).AsDateTime);
          dt := FieldByName(C_TfDSatz_DatumZeit).AsDateTime;
          FieldByName(C_TfDSatz_ReferenzNr).AsInteger := iONr; // Referenznummer
          FieldByName(C_TfDSatz_OrdNr).AsInteger := iONr; // Ordnungsnummer
          Inc(iONr);

          for i := 0 to KanalZahl-1 do begin
            if (not pChannelExists[i]) then Continue;
            if (i >= C_MaxKanalZahl) then Break;  // Maximale Kanalzahl abprüfen

            iStatus := GetStatus(i);
            if (pKanalTyp[i] = kt_ZS) then begin
              SetKanalTyp(i, kt_ZW);
              fValue := GetWert(i);
              if (iONr > 2) and ((iStatus and $80) = 0) then begin
                if ((pStatus[i] and $80) = 0) then begin
                  f := pValues[i];
                  if (fValue >= f) then begin  // Wert OK
                    SetWert(i, fValue - f);
                    SetStatus(i, iStatus);
                  end
                  else begin
                    SetWert(i, 0);
                    SetStatus(i, iStatus or $01);  // Wert Überlauf
                  end;
                end
                else begin  // Vorheriger Wert fehlt
                  SetWert(i, 0);
                  SetStatus(i, iStatus and $80);  // Wert fehlt
                end;
                pPrevValues[i] := pValues[i];
                pValues[i] := fValue;
              end
              else begin
                SetWert(i, 0);
                SetStatus(i, iStatus and $80);  // Wert fehlt
                if ((iStatus and $80) = 0)
                then pValues[i] := fValue
                else pValues[i] := 0;
              end;
            end
            else if (pKanalTyp[i] = kt_ZW) then begin
              if ((iStatus and $80) = 0) then begin
                SetWert(i, GetWert(i) + pValues[i]);
              end
              else begin
                SetWert(i, 0);
                SetStatus(i, iStatus and $80);  // Wert fehlt
              end;
              pValues[i] := 0;
            end
            else if (pKanalTyp[i] = kt_MW) then begin
              if ((iStatus and $80) = 0) then begin       // 01.02.2010
                iCnt := Round(pValues[i]) div 10000000;   // Anzahl der Werte
                fValue := pValues[i] - (iCnt * 10000000); // Offset abziehen
                fValue := ((fValue * iCnt) + GetWert(i)) / (iCnt + 1);

                SetWert(i, fValue);
              end
              else begin
                SetWert(i, 0);
                SetStatus(i, iStatus and $80);  // Wert fehlt
              end;
              pValues[i] := 0;
            end
            else begin
              // Wert unverändert lassen
            end;
            pStatus[i] := iStatus;
          end;

          Post;
          Next;
        end
        else begin
          if (IsNextHour(dt, FieldByName(C_TfDSatz_DatumZeit).AsDateTime) < 0)
          then begin
            Next;    // Dämliches System: EOF wird nicht automatisch gesetzt!
            bEof := (Eof);    // 01.02.2010
            if (not bEof) then Prior;
            // Doppelter Stundeneintrag
            if (FLastHourval) then begin    // 11.10.2010
              Prior;                        // "Letzten Sundenwert" heißt ...
              Delete;                       // ... vorherigen Wert löschen ...

              // Merker resetten
              for i := 0 to KanalZahl-1 do begin
                if (not pChannelExists[i]) then Continue;
                if (pKanalTyp[i] = kt_ZS) then begin
                  pValues[i] := pPrevValues[i];
                end
              end;

              dt := IncHour(dt, -1);        // ... und Uhr zurückdrehen
            end
            else Delete;                    // Sonst aktuellen Eintrag löschen
            if (bEof) then Next;    // Einmal around worken ...
          end;
        end;
      end
      else begin
        for i := 0 to KanalZahl-1 do begin
          if (pKanalTyp[i] = kt_ZS) then begin
            // ignorieren
          end
          else if (pKanalTyp[i] = kt_ZW) then begin
            pValues[i] := pValues[i] + GetWert(i);
          end
          else if (pKanalTyp[i] = kt_MW) then begin   // 01.02.2010
            iCnt := Round(pValues[i]) div 10000000;   // Anzahl der Werte
            fValue := pValues[i] - (iCnt * 10000000); // Offset abziehen
            fValue := ((fValue * iCnt) + GetWert(i)) / (iCnt + 1);

            pValues[i] := fValue + ((iCnt+1) * 10000000); // Cnt al Offset
          end
          else begin
            // ignorieren
          end;
        end;

        Next;    // Dämliches System: EOF wird nicht automatisch gesetzt!
        bEof := (Eof);    // 01.02.2010
        if (not bEof) then Prior;
        Delete;  // Twischenwert
        if (bEof) then Next;    // Einmal around worken ...
      end;
    end;
    First;
    if (dtVon > 1) then begin
      while (FieldByName(C_TfDSatz_DatumZeit).AsDateTime < (dtVon -
        EncodeTime(0, 0, FTimeFuzzy div 1000, FTimeFuzzy mod 1000))) and
        (not Eof)
      do Delete;
    end;

    IndexFieldNames := C_TfDSatz_DatumZeit; // 26.02.2010
  except
    Exception.Create('TLGZStd.CalcInMemData (' + FAktTable.TableName +
      '): InMem-Table not present!');
  end;
end;

{ Übergibt Datenbanknamen                     }
{---------------------------------------------}
procedure TLGZTag.UnPrepare;
{---------------------------------------------}
var
  q : TQuery;
begin
  inherited;

  if (Assigned(FPreparedKontrollData)) then begin
    FPreparedKontrollData.Active := False;
    FreeAndNil(FPreparedKontrollData);
    q := TQuery(FDataProviderKontroll.DataSet);
    FreeAndNil(FDataProviderKontroll);
    FreeAndNil(q);
  end;
end;

{---------------------------------}
function TLGZTag.GetOrdnungsnummer: LongInt;
{---------------------------------}
begin
  CheckOpened;
  if (IsOriData)
  then Result := DataBuf.FieldByName(C_TfDSatz_OrdNr).asInteger
  else Result := DataBuf.FieldByName(C_Tf_LGZTag_OrdNr).asInteger;
end;

{---------------------------------}
procedure TLGZTag.SetOrdnungsnummer(iValue: LongInt);
{---------------------------------}
begin
  CheckOpened;
  if (IsOriData) then begin
    DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger := iValue;
    DataBuf.FieldByName(C_TfDSatz_OrdNr).asInteger := iValue;
  end
  else DataBuf.FieldByName(C_Tf_LGZTag_OrdNr).asInteger := iValue;
end;

{---------------------------------}
function TLGZTag.GetSatzStatus: Byte;
{---------------------------------}
begin
  CheckOpened;
  if (IsOriData)
  then Result := DataBuf.FieldByName(C_TfDSatz_Status).asInteger
  else Result := DataBuf.FieldByName(C_Tf_LGZTag_SatzStatus).asInteger;
end;

{---------------------------------}
procedure TLGZTag.SetSatzStatus(iValue: byte);
{---------------------------------}
begin
  CheckOpened;
  if (IsOriData)
  then DataBuf.FieldByName(C_TfDSatz_Status).asInteger := iValue
  else DataBuf.FieldByName(C_Tf_LGZTag_SatzStatus).asInteger := iValue;
end;

{---------------------------------}
function TLGZTag.GetDatum: TDateTime;
{---------------------------------}
begin
  CheckOpened;
  if (IsOriData)
  then Result := DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime
  else Result := DataBuf.FieldByName(C_Tf_LGZTag_DatumZeit).asDateTime
    + EncodeTime(DataBuf.FieldByName(C_Tf_LGZTag_Stunden).asInteger, 0, 0, 0);
end;

{---------------------------------}
procedure TLGZTag.SetDatum(dtValue: TDateTime);
{---------------------------------}
var
  dt : TDateTime;
begin
  CheckOpened;
  if (IsOriData)
  then DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime := dtValue
  else begin
    dt := Trunc(dtValue);
    DataBuf.FieldByName(C_Tf_LGZTag_DatumZeit).asDateTime := dt;
    DataBuf.FieldByName(C_Tf_LGZTag_Stunden).asInteger := Trunc(Frac(dtValue)*24);
  end;
end;

{---------------------------------}
function TLGZTag.GetStunden: Byte;
{---------------------------------}
var
  h, m, s, ms : word;
begin
  CheckOpened;
  if (IsOriData) then begin
    DecodeTime(DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime, h, m, s, ms);
    Result := h;
  end
  else Result := DataBuf.FieldByName(C_Tf_LGZTag_Stunden).asInteger;
end;

{---------------------------------}
procedure TLGZTag.SetStunden(iValue: byte);
{---------------------------------}
var
  h, m, s, ms : word;
begin
  CheckOpened;
  if (IsOriData) then begin
    DecodeTime(DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime, h, m, s, ms);
    DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime :=
      Trunc(DataBuf.FieldByName(C_TfDSatz_DatumZeit).asDateTime) +
      EncodeTime(iValue, m, s, ms);
  end
  else DataBuf.FieldByName(C_Tf_LGZTag_Stunden).asInteger := iValue;
end;

{---------------------------------}
function TLGZTag.GetEingangStatus (Index: Word): Byte;
{---------------------------------}
var
  iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    if (Assigned(FPreparedData)) then begin
      if (FPreparedData.FieldDefs.IndexOf(
         'Status' + Format('%.3d%', [Index+1])) >= 0) then
      begin
        if (not FPreparedData.FieldByName(
          'Status' + Format('%.3d%', [Index+1])).IsNull)
        then Result := FPreparedData.
          FieldByName('Status' + Format('%.3d%', [Index+1])).asInteger
        else Result := 128;
      end
      else Result := 128;
    end
    else begin
      iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
      iSatzIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
      if (iSatzIndex > 0) then begin
        with TTableExt.Create(nil) do
        try
          DatabaseName := Self.Database.DatabaseName;
          SessionName := Self.Database.SessionName;
          TableName := C_TbDWert + Format('%.3d%.4d', [Index+1, iSatzIndex]);
          if (Exists) then begin
            SafeUpdateIndexDefs;
            IndexName := IndexDefs[0].Name;
            Open;
            if (FindKey([iRef]))
            then Result := FieldByName(C_TfDWert_Status).asInteger
            else Result := 128;
            Close;
          end
          else Result := 128;
        finally
          Free;
        end;
      end
      else Result := 128;  // Wert fehlt
    end;
  end
  else Result :=
    DataBuf.FieldByName(Format(C_Tf_LGZTag_EKanStatus, [Index])).asInteger;
end;

{---------------------------------}
procedure TLGZTag.SetEingangStatus (Index: Word; iValue: Byte);
{---------------------------------}
var
  iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    iSatzIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
    iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
    if (iSatzIndex > 0) then begin
      CreatetbDWert(Database, iSatzIndex, Index+1);
      with TTableExt.Create(nil) do
      try
        DatabaseName := Self.Database.DatabaseName;
        SessionName := Self.Database.SessionName;
        TableName := C_TbDWert + Format('%.3d%.4d', [Index, iSatzIndex]);
        if (Exists) then begin
          SafeUpdateIndexDefs;
          IndexName := IndexDefs[0].Name;
          Open;
          if (FindKey([iRef]))
          then Edit
          else begin
            Append;
            FieldByName(C_TfDWert_ReferenzNr).asInteger := iRef;
          end;
          FieldByName(C_TfDWert_Status).asInteger := iValue;
          Post;
          Close;
        end;
      finally
        Free;
      end;
    end
    else Exception.Create(Format (S_TabFehlt, [C_TbDWert +
      Format('%.3d%.4d', [Index, iSatzIndex])]));
  end
  else DataBuf.FieldByName(Format(C_Tf_LGZTag_EKanStatus, [Index])).asInteger :=
    iValue;
end;

{---------------------------------}
function TLGZTag.GetEingangWert (Index: Word): double;
{---------------------------------}
var
  iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    if (Assigned(FPreparedData)) then begin
      if (FPreparedData.FieldDefs.IndexOf(
         'Wert' + Format('%.3d%', [Index+1])) >= 0) then
      begin
        if (not FPreparedData.FieldByName(
          'Wert' + Format('%.3d%', [Index+1])).IsNull)
        then Result := FPreparedData.
          FieldByName('Wert' + Format('%.3d%', [Index+1])).asFloat
        else Result := 0;
      end
      else Result := 0;
    end
    else begin
      iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
      iSatzIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
      if (iSatzIndex > 0) then begin
        with TTableExt.Create(nil) do
        try
          DatabaseName := Self.Database.DatabaseName;
          SessionName := Self.Database.SessionName;
          TableName := C_TbDWert + Format('%.3d%.4d', [Index+1, iSatzIndex]);
          if (Exists) then begin
            SafeUpdateIndexDefs;
            IndexName := IndexDefs[0].Name;
            Open;
            if (FindKey([iRef]))
            then Result := FieldByName(C_TfDWert_Wert).asFloat
            else Result := 0;
            Close;
          end
          else Result := 0;
        finally
          Free;
        end;
      end
      else Result := 0;  // Wert fehlt
    end;
  end
  else Result :=
    DataBuf.FieldByName(Format(C_Tf_LGZTag_EKanalWert, [Index])).asInteger;
end;

{---------------------------------}
procedure TLGZTag.SetEingangWert (Index: Word; iValue: double);
{---------------------------------}
var
  iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    iSatzIndex := StrToIntDef(Copy(TableName, 5, 4), -1);
    iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
    if (iSatzIndex > 0) then begin
      CreatetbDWert(Database, iSatzIndex, Index+1);
      with TTableExt.Create(nil) do
      try
        DatabaseName := Self.Database.DatabaseName;
        SessionName := Self.Database.SessionName;
        TableName := C_TbDWert + Format('%.3d%.4d', [Index, iSatzIndex]);
        if (Exists) then begin
          SafeUpdateIndexDefs;
          IndexName := IndexDefs[0].Name;
          Open;
          if (FindKey([iRef]))
          then Edit
          else begin
            Append;
            FieldByName(C_TfDWert_ReferenzNr).asInteger := iRef;
          end;
          FieldByName(C_TfDWert_Wert).asFloat := iValue;
          Post;
          Close;
        end;
      finally
        Free;
      end;
    end
    else Exception.Create(Format (S_TabFehlt, [C_TbDWert +
      Format('%.3d%.4d', [Index, iSatzIndex])]));
  end
  else DataBuf.FieldByName(Format(C_Tf_LGZTag_EKanalWert, [Index])).asInteger :=
    Trunc (iValue);
end;

{---------------------------------}
function TLGZTag.GetKontrollStatus (Index: Word): Byte;
{---------------------------------}
var
  iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    iSatzIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    if (iSatzIndex > 0) then begin
      if (Assigned(FPreparedKontrollData)) then begin
        if (FPreparedKontrollData.FieldDefs.IndexOf(
           'Status' + Format('%.3d%', [Index+1])) >= 0) then
        begin
          if (not FPreparedKontrollData.FieldByName(
            'Status' + Format('%.3d%', [Index+1])).IsNull)
          then Result := FPreparedKontrollData.
            FieldByName('Status' + Format('%.3d%', [Index+1])).asInteger
          else Result := 128;
        end
        else Result := 128;
      end
      else begin
        iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
        with TTableExt.Create(nil) do
        try
          DatabaseName := Self.Database.DatabaseName;
          SessionName := Self.Database.SessionName;
          TableName := C_TbDWert + Format('%.3d%.4d', [Index+1, iSatzIndex]);
          if (Exists) then begin
            SafeUpdateIndexDefs;
            IndexName := IndexDefs[0].Name;
            Open;
            if (FindKey([iRef]))
            then Result := FieldByName(C_TfDWert_Status).asInteger
            else Result := 128;
            Close;
          end
          else Result := 128;
        finally
          Free;
        end;
      end
    end
    else Result := 128;  // Wert fehlt
  end
  else Result := DataBuf.FieldByName(
    Format(C_Tf_LGZTag_KKanStatus, [Index])).asInteger;
end;

{---------------------------------}
procedure TLGZTag.SetKontrollStatus (Index: Word; iValue: Byte);
{---------------------------------}
var
  i, iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
    iSatzIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    if (iSatzIndex > 0) then begin
      CreatetbDSatz(Self.Database, iSatzIndex);
      FAktKontrollTable.SafeUpdateIndexDefs;
      FAktKontrollTable.IndexName := FAktKontrollTable.IndexDefs[0].Name;
      FAktKontrollTable.OpenExclusive;
      try
        if (not FAktKontrollTable.FindKey([iRef])) then begin
          FAktKontrollTable.Append;
          for i := 0 to DataBuf.Count-1 do
            FAktKontrollTable.Fields[i].Value := Self.DataBuf.Fields[i].Value;
          FAktKontrollTable.Post;
        end;
      finally
        FAktKontrollTable.Close;
      end;

      CreatetbDWert(Database, iSatzIndex, Index+1);
      with TTableExt.Create(nil) do
      try
        DatabaseName := Self.Database.DatabaseName;
        SessionName := Self.Database.SessionName;
        TableName := C_TbDWert + Format('%.3d%.4d', [Index, iSatzIndex]);
        if (Exists) then begin
          SafeUpdateIndexDefs;
          IndexName := IndexDefs[0].Name;
          Open;
          if (FindKey([iRef]))
          then Edit
          else begin
            Append;
            FieldByName(C_TfDWert_ReferenzNr).asInteger := iRef;
          end;
          FieldByName(C_TfDWert_Status).asInteger := iValue;
          Post;
          Close;
        end;
      finally
        Free;
      end;
    end
    else Exception.Create(Format (S_TabFehlt, [C_TbDWert +
      Format('%.3d%.4d', [Index, iSatzIndex])]));
  end
  else DataBuf.FieldByName(Format(C_Tf_LGZTag_KKanStatus, [Index])).asInteger :=
    iValue;
end;

{---------------------------------}
function TLGZTag.GetKontrollWert (Index: Word): double;
{---------------------------------}
var
  iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    iSatzIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    if (iSatzIndex > 0) then begin
      if (Assigned(FPreparedKontrollData)) then begin
        if (FPreparedKontrollData.FieldDefs.IndexOf(
           'Wert' + Format('%.3d%', [Index+1])) >= 0) then
        begin
          if (not FPreparedKontrollData.FieldByName(
            'Wert' + Format('%.3d%', [Index+1])).IsNull)
          then Result := FPreparedKontrollData.
            FieldByName('Wert' + Format('%.3d%', [Index+1])).asFloat
          else Result := 0;
        end
        else Result := 0;
      end
      else begin
        iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
        with TTableExt.Create(nil) do
        try
          DatabaseName := Self.Database.DatabaseName;
          SessionName := Self.Database.SessionName;
          TableName := C_TbDWert + Format('%.3d%.4d', [Index+1, iSatzIndex]);
          if (Exists) then begin
            SafeUpdateIndexDefs;
            IndexName := IndexDefs[0].Name;
            Open;
            if (FindKey([iRef]))
            then Result := FieldByName(C_TfDWert_Wert).asFloat
            else Result := 0;
            Close;
          end
          else Result := 0;
        finally
          Free;
        end;
      end
    end
    else Result := 0;  // Wert fehlt
  end
  else Result := DataBuf.FieldByName(
    Format(C_Tf_LGZTag_KKanalWert, [Index])).asInteger;
end;

{---------------------------------}
procedure TLGZTag.SetKontrollWert (Index: Word; iValue: double);
{---------------------------------}
var
  i, iSatzIndex, iRef : integer;
begin
  CheckOpened;
  CheckIndex (Index);
  if (IsOriData) then begin
    iRef := DataBuf.FieldByName(C_TfDSatz_ReferenzNr).asInteger;
    iSatzIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    if (iSatzIndex > 0) then begin
      CreatetbDSatz(Self.Database, iSatzIndex);
      FAktKontrollTable.SafeUpdateIndexDefs;
      FAktKontrollTable.IndexName := FAktKontrollTable.IndexDefs[0].Name;
      FAktKontrollTable.OpenExclusive;
      try
        if (not FAktKontrollTable.FindKey([iRef])) then begin
          FAktKontrollTable.Append;
          for i := 0 to DataBuf.Count-1 do
            FAktKontrollTable.Fields[i].Value := Self.DataBuf.Fields[i].Value;
          FAktKontrollTable.Post;
        end;
      finally
        FAktKontrollTable.Close;
      end;

      CreatetbDWert(Database, iSatzIndex, Index+1);
      with TTableExt.Create(nil) do
      try
        DatabaseName := Self.Database.DatabaseName;
        SessionName := Self.Database.SessionName;
        TableName := C_TbDWert + Format('%.3d%.4d', [Index, iSatzIndex]);
        if (Exists) then begin
          SafeUpdateIndexDefs;
          IndexName := IndexDefs[0].Name;
          Open;
          if (FindKey([iRef]))
          then Edit
          else begin
            Append;
            FieldByName(C_TfDWert_ReferenzNr).asInteger := iRef;
          end;
          FieldByName(C_TfDWert_Wert).asFloat := iValue;
          Post;
          Close;
        end;
      finally
        Free;
      end;
    end
    else Exception.Create(Format (S_TabFehlt, [C_TbDWert +
      Format('%.3d%.4d', [Index, iSatzIndex])]));
  end
  else DataBuf.FieldByName(Format(C_Tf_LGZTag_KKanalWert, [Index])).asInteger :=
    Trunc (iValue);
end;

{---------------------------------}
procedure TLGZTag.SetDataFile (FileNr: Word);
{---------------------------------}
begin
  if (IsOriData) then begin
    TableName := C_TbDSatz + Format('%.4d', [FileNr]);
    if (not Assigned(FAktKontrollTable)) then begin
      FAktKontrollTable := TTableExt.Create(nil);
      FAktKontrollTable.DatabaseName := FDatabase.DatabaseName;
      FAktKontrollTable.SessionName := FDatabase.SessionName;
    end;
    FAktKontrollTable.TableName := C_TbDSatz + Format('%.4d',
      [Get_MRGDatenIndex(Self.Database, Self.ReadKennung, ag_mrg_KontrZ, True)]);
  end
  else begin
    if (isLangzeit)
    then TableName := Format (C_Tb_LGZTag, [FileNr])
    else TableName := Format (C_Tb_LGZTagM, [FileNr]); { GeDa }
  end;

  if (not Assigned(FAktTable)) then begin
    FAktTable := TTableExt.Create(nil);
    FAktTable.DatabaseName := FDatabase.DatabaseName;
    FAktTable.SessionName := FDatabase.SessionName;
  end;
  FAktTable.TableName := Self.TableName;
end;

{---------------------------------}
function TLGZTag.GetDateiName: TFileName;  {kb 8.7.97}
{---------------------------------}
begin
  Result := TableName;
end;

{ Öffnet aktuelle Tabelle                                           }
{ Parameter: 0=open; 1=shared; 2=exclusive                          }
{ Rückgabe: OK                                                      }
{-------------------------------------------------------------------}
function TLGZTag.GetOpen(iState: integer): boolean;
{-------------------------------------------------------------------}
begin
  CreateLgzTagTb;

  if (IsOriData) and (FState = lgzKennung) then begin
    if (not Assigned(FAktKontrollTable)) then
      FAktKontrollTable := TTableExt.Create (nil);
    FAktKontrollTable.DatabaseName := FDatabase.DatabaseName;
    FAktKontrollTable.SessionName := FDatabase.SessionName;
    FAktKontrollTable.TableName := C_TbDSatz + Format('%.4d',
      [Get_MRGDatenIndex(Self.Database, Self.ReadKennung, ag_mrg_KontrZ, True)]);
    if (FAktKontrollTable.Exists) then begin
      if (FAktKontrollTable.SafeUpdateIndexDefs) then begin  // 26.02.2010
        if (IsOriData)
        then FAktKontrollTable.IndexName := CSDat
        else FAktKontrollTable.IndexName := FAktTable.IndexDefs[0].Name
      end
      else raise Exception.Create('TLGZTag.GetOpen (' +
        FAktKontrollTable.TableName +
        '): Fatal access error (table locked/deleted?)');
    end;
  end;
  FState := lgzKennung;
  Result := inherited GetOpen(iState);
  FirstOrdnungsNummer := GetOrdnungsNummer;
end;

{ Setzt Cursor an den Anfang der Datei und liest ersten Satz in Puffer }
{------------------------}
procedure TLGZTag.First;
{------------------------}
var
  iSatzIndex : integer;
  dtRef: TDateTime;
begin
  inherited;

  if (IsOriData) then begin
    iSatzIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    if (iSatzIndex > 0) and (Assigned(FPreparedKontrollData)) then begin
      // 07.06.2011  WN
      dtRef := DataBuf.FieldByName(C_TfDSatz_DatumZeit).AsDateTime;
      FPreparedKontrollData.FindKey([dtRef]);
    end;
  end;
end;

{ Setzt Cursor an das Ende der Datei, nachdem der letzte Satz in den
  Puffer gelesen wurde }
{-----------------------}
procedure TLGZTag.Last;
{-----------------------}
var
  iSatzIndex : integer;
  dtRef: TDateTime;
begin
  inherited;

  if (IsOriData) then begin
    iSatzIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    if (iSatzIndex > 0) and (Assigned(FPreparedKontrollData)) then begin
      // 07.06.2011  WN
      dtRef := DataBuf.FieldByName(C_TfDSatz_DatumZeit).AsDateTime;
      FPreparedKontrollData.FindKey([dtRef]);
    end;
  end;
end;

{ Setzt Cursor auf nächsten Datensatz und liest diesen in Puffer }
{-----------------------}
procedure TLGZTag.Next;
{-----------------------}
var
  iSatzIndex : integer;
  dtRef: TDateTime;
begin
  inherited;

  if (IsOriData) then begin
    iSatzIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    if (iSatzIndex > 0) and (Assigned(FPreparedKontrollData)) then begin
      // 07.06.2011  WN
      dtRef := DataBuf.FieldByName(C_TfDSatz_DatumZeit).AsDateTime;
      FPreparedKontrollData.FindKey([dtRef]);
    end;
  end;
end;

{ Setzt Cursor auf vorherigen Datensatz und liest diesen in Puffer }
{-----------------------}
procedure TLGZTag.Prior;
{-----------------------}
var
  iSatzIndex : integer;
  dtRef: TDateTime;
begin
  inherited;

  if (IsOriData) then begin
    iSatzIndex := StrToIntDef(Copy(FAktKontrollTable.TableName, 5, 4), -1);
    if (iSatzIndex > 0) and (Assigned(FPreparedKontrollData)) then begin
      // 07.06.2011  WN
      dtRef := DataBuf.FieldByName(C_TfDSatz_DatumZeit).AsDateTime;
      FPreparedKontrollData.FindKey([dtRef]);
    end;
  end;
end;

{---------------------------------}
function TLGZTag.SearchOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
{---------------------------------}
begin
  CheckOpened;

  FAktTable.SafeUpdateIndexDefs;
  if (FAktTable.IndexName <> FAktTable.IndexDefs[0].Name) then begin
    FAktTable.Close;
    FAktTable.IndexName := FAktTable.IndexDefs[0].Name;
    if (not FAktTable.OpenShared) then FState := lgzClosed;

    CheckOpened;
  end;

  Result := FAktTable.FindKey([AOrdnungsNummer]);
end;

{---------------------------------}
function TLGZTag.SearchGEOrdnungsNummer (AOrdnungsNummer: LongInt): Boolean;
{---------------------------------}
var
  Delta: LongInt;
begin
  Delta := AOrdnungsNummer - FirstOrdnungsNummer;
  if Delta <= 0 then
    Result := SearchPos (0)
  else
    Result := SearchPos (Delta);
end;

{---------------------------------}
function TLGZTag.SearchDatum (ADatum: TDateTime): Boolean;
{---------------------------------}
var
  sIndex : string;
  dt     : TDateTime;
  pBMark : TBookmark;
begin
  Result := False;
  CheckOpened;

  if (IsOriData) then sIndex := CSDat else sIndex := C_TI_LGZTag_ixDateTime;

  if (FAktTable.IndexName <> sIndex) then begin
    FAktTable.Close;
    FAktTable.IndexName := sIndex;
    if (not FAktTable.OpenShared) then FState := lgzClosed;
  end;

  if (IsOriData) then dt := ADatum else dt := Trunc(ADatum);
  if (FState = lgzOpened) then Result := FAktTable.FindKey([dt]);
  if (not Result) then begin
    pBMark := FAktTable.GetBookmark;
    try
      FAktTable.FindNearest([dt]);
      if (Trunc(Self.Datum) = Trunc(ADatum-1)) then FAktTable.Next
      // 24.04.2006  - Grenze auch in andere Richtung
      else if (Trunc(Self.Datum) = Trunc(ADatum+1)) then FAktTable.Prior;
      Result := (Trunc(Self.Datum) = Trunc(ADatum));
      if (not Result) then FAktTable.GotoBookmark(pBMark);
    finally
      FAktTable.FreeBookmark(pBMark);
    end;
  end;
end;

{---------------------------------}
function TLGZTag.SearchGEDatum (ADatum: TDateTime): Boolean;
{---------------------------------}
var
  sIndex : string;
  dt     : TDateTime;
begin
  Result := False;
  CheckOpened;

  if (IsOriData) then sIndex := CSDat else sIndex := C_TI_LGZTag_ixDateTime;

  if (FAktTable.IndexName <> sIndex) then begin
    FAktTable.Close;
    FAktTable.IndexName := sIndex;
    if (not FAktTable.OpenShared) then FState := lgzClosed;
  end;

  if (FState = lgzOpened) then begin
    if (IsOriData) then dt := ADatum else dt := Trunc(ADatum);
    if (FAktTable.FindKey([dt]))
    then Result := True
    else begin
      FAktTable.FindNearest([ADatum]);
      Result := (Datum > ADatum);
    end;
  end;
end;

{---------------------------------}
function TLGZTag.ReadBuffer: LGZTRec;
{---------------------------------}
var
  y, m, d : word;
  i       : integer;
begin
  CheckOpened;
  Result.satzstatus := SatzStatus;
  DecodeDate(Datum, y, m, d);
  Result.Datum.Year := y;
  Result.Datum.Month := m;
  Result.Datum.Day := d;
  Result.Stunden := Stunden;
  for i := Low (Result.E_Zaehler) to High (Result.E_Zaehler) do begin  // Vorbelegung Eingangszähler
    Result.E_Zaehler[i].zaehlerstatus := 0;
    Result.E_Zaehler[i].wert := 0;
  end;
  for i := Low (Result.K_Zaehler) to High (Result.K_Zaehler) do begin  // Vorbelegung Kontrollzähler
    Result.K_Zaehler[i].zaehlerstatus := 0;
    Result.K_Zaehler[i].wert := 0;
  end;
  for i := 1 to FKanalZahl do begin  // Zuweisung Eingangs- und Kontrollzähler
    Result.E_Zaehler[i].zaehlerstatus := EingangStatus[i-1];
    Result.E_Zaehler[i].wert := Trunc(EingangWert[i-1]);
    Result.K_Zaehler[i].zaehlerstatus := KontrollStatus[i-1];
    Result.K_Zaehler[i].wert := Trunc(KontrollWert[i-1]);
  end;
end;

{---------------------------------}
function TLGZTag.ReadRohBuffer: RohTRec;
{---------------------------------}
var
  i       : integer;
begin
  CheckOpened;
  Result.satzstatus := SatzStatus;
  Result.DatumZeit := Datum;
  for i := Low (Result.E_Zaehler) to High (Result.E_Zaehler) do begin  // Vorbelegung Eingangszähler
    Result.E_Zaehler[i].zaehlerstatus := 0;
    Result.E_Zaehler[i].wert := 0;
  end;
  for i := Low (Result.K_Zaehler) to High (Result.K_Zaehler) do begin  // Vorbelegung Kontrollzähler
    Result.K_Zaehler[i].zaehlerstatus := 0;
    Result.K_Zaehler[i].wert := 0;
  end;
  for i := 1 to FKanalZahl do begin  // Zuweisung Eingangs- und Kontrollzähler
    Result.E_Zaehler[i].zaehlerstatus := EingangStatus[i-1];
    Result.E_Zaehler[i].wert := EingangWert[i-1];
    Result.K_Zaehler[i].zaehlerstatus := KontrollStatus[i-1];
    Result.K_Zaehler[i].wert := KontrollWert[i-1];
  end;
end;

{---------------------------------}
procedure TLGZTag.WriteBuffer(pBuffer: LGZTRec);
{---------------------------------}
var
  iONr : Longint;
  i    : byte;
begin
  CheckOpened;

  if (not Eof) then begin
    FAktTable.Edit;
    DataBuf := FAktTable.Fields;
  end
  else begin
    if (Eof) and (Bof) then iONr := 1 else iONr := Ordnungsnummer + 1;
    FAktTable.Append;
    DataBuf := FAktTable.Fields;
    Ordnungsnummer := iONr;
  end;

  if (not IsOriData) then
    FAktTable.FieldByName(C_Tf_LGZTag_KanalZahl).asInteger := KanalZahl;
  SatzStatus := pBuffer.satzstatus;
  Datum :=
    EncodeDate(pBuffer.Datum.Year, pBuffer.Datum.Month, pBuffer.Datum.Day) +
    EncodeTime(pBuffer.Stunden, 0, 0, 0);
  for i := 1 to FKanalZahl do begin
    EingangStatus[i-1] := pBuffer.E_Zaehler[i].zaehlerstatus;
    EingangWert[i-1] := pBuffer.E_Zaehler[i].wert;
    KontrollStatus[i-1] := pBuffer.K_Zaehler[i].zaehlerstatus;
    KontrollWert[i-1] := pBuffer.K_Zaehler[i].wert;
  end;
  FAktTable.Post;
  Next;
end;

{---------------------------------}
procedure TLGZTag.WriteRohBuffer(pBuffer: RohTRec);
{---------------------------------}
var
  iONr : Longint;
  i    : byte;
begin
  CheckOpened;

  if (not Eof) then begin
    FAktTable.Edit;
    DataBuf := FAktTable.Fields;
  end
  else begin
    if (Eof) and (Bof) then iONr := 1 else iONr := Ordnungsnummer + 1;
    FAktTable.Append;
    DataBuf := FAktTable.Fields;
    Ordnungsnummer := iONr;
  end;

  if (not IsOriData) then
    FAktTable.FieldByName(C_Tf_LGZTag_KanalZahl).asInteger := KanalZahl;
  SatzStatus := pBuffer.satzstatus;
  Datum :=pBuffer.DatumZeit;
  for i := 1 to FKanalZahl do begin
    EingangStatus[i-1] := pBuffer.E_Zaehler[i].zaehlerstatus;
    EingangWert[i-1] := pBuffer.E_Zaehler[i].wert;
    KontrollStatus[i-1] := pBuffer.K_Zaehler[i].zaehlerstatus;
    KontrollWert[i-1] := pBuffer.K_Zaehler[i].wert;
  end;
  FAktTable.Post;
  Next;
end;

{---------------------------------}
procedure Register;
{---------------------------------}
begin
  RegisterComponents ('Wieser', [TLGZStd]);
  RegisterComponents ('Wieser', [TLGZTag]);
end;

end.
