{******************************************************************************}
{* Unit: Steuerung des DSfG-Abrufs (Abrufmanager)                             *}
{* 18.11.1999 WW                                                              *}
{* 08.08.2002 IFDEF GAMESS im JournalReoganisieren SM/H.-P.R.		      *}
(* 19.04.2003  GD  Pause bei Momentanwertabruf variabel aus INI               *)
(* 13.05.2003  GD  Abrufe bei Momentanwertabruf ermöglicht                    *)
(* 07.07.2003  GD  Bugfix: Abrufe bei Momentanwertabruf                       *)
{******************************************************************************}
unit DAbrfMan;

INTERFACE

Uses
  Forms, Classes, SysUtils, DBTables, ExtCtrls, SerDSfG, DDBAbruf, DAbruf, AbrfInfo,
  JournlDB, PathIni, DB_Attn, WTables, DSysCon, DJournal, DSysDat, WSysCon, DListen,
  DDbSta, DALKonvDb, DZustand, DIWKonv, DKfKonv, DDLoesch, TbDSfGMo, TbDSfGDE,
  T_Zeit, ErrConst, AuftrgDb, ZustndDb, WChars, TCPIP_DSfG, DKavKonv, DKavKonfigDb,
  ProgIni;

type

  { abstraktes Objekt zur Steuerung der Abruf-Dynamik }

  TAbrufManager = class (TObject)
  protected
    FVerbindungSteht : boolean;
    FADManuCount     : integer;
    COMPort: integer;
    Abruf: TAbruf;                                { Objekt zum DSfG-Datenabruf }
    Abrufart: TAbrufart;                          { automatisch, manuell }
    CommComponent: TComponent;  // 13.05.2003     { Verbindungskoponente }
    AbrufModus: byte;                             { nur neue Daten, manuell über Zeitbereich oder Ordnungsnummer etc. }
    AktuZeitangaben: TZeitangaben;
    KonvListeArchive: TKonvList;
    KonvListeLogbuecher: TKonvList;
    KonvListeDatenelemente: TKonvList;
    Stammdaten: TDSfGStammdaten;
    DfueTransparentmodus: boolean;                { Flag: DSfG-DFÜ transparent schalten beim Verbindungsaufbau ja/nein }
    function ArchiveAbrufen: boolean; virtual;
    function LogbuecherAbrufen: boolean; virtual;
    function DatenelementeAbrufen: boolean; virtual;
    procedure VerbindungSteht; virtual;
    procedure Verbindungsfehler;
    procedure VerbindungBeenden(bVerbSteht: boolean = False);
    procedure VerbindungIstBeendet; virtual;
    procedure DatenelementeAbruf; virtual;
    procedure DatenelementeAbrufFehler; virtual;
    procedure DatenelementeGelesen; virtual;
    procedure ArchiveAbruf;
    procedure ArchiveAbrufFehler;
    procedure ArchiveGelesen;
    procedure LogbuecherAbruf; virtual;
    procedure LogbuecherAbrufFehler; virtual;
    procedure LogbuecherGelesen; virtual;
    procedure AktuelleZeitangabenAbruf;
    procedure Konvertieren; virtual;
    procedure JournalReorganisieren;
  public
    AbrufData: TAbrufData;      { enthält Infos über abzurufende DSfG-Station und nach
                                  erfolgtem Abruf den Abrufstatus (Ok/Fehler) }
    constructor Create (ACommComponent: TComponent; AAbrufart: TAbrufart);
    destructor Destroy; override;
    procedure Abrufen;
  end;


  { Abruf-Manager für automatischen und manuellen Abruf }

  TAutoManuAbrufManager = class (TAbrufManager)
  protected
    function ArchiveAbrufen: boolean; override;
    function LogbuecherAbrufen: boolean; override;
    function DatenelementeAbrufen: boolean; override;
    procedure DatenelementeAbrufFehler; override;
    procedure DatenelementeGelesen; override;
    procedure DELInst_InstTypAbruf (vorher_nachher: char);
    procedure DELInst_InstTypAbrufFehler (vorher_nachher: char);
    procedure DELInst_InstTypGelesen (vorher_nachher: char);
    procedure LogbuecherAbrufFehler; override;
    procedure LogbuecherGelesen; override;
  public
    constructor Create (ACommComponent: TComponent; AAbrufart: TAbrufart;
      AAbrufData: TAbrufData; bVerbSteht: boolean = False);
    destructor Destroy; override;
  end;

  { Abruf-Manager für das Einlesen der Konfiguration }

  TKonfLesenAbrufManager = class (TAbrufManager)
  protected
    InstanzListe: TInstanzDataList;
    InstDfuListe: TInstDfuDataList;
    KonfigRohdataListe: TKonfigRohdataList;
    procedure VerbindungSteht; override;
    procedure KonfigurationAbruf;
    procedure KonfigurationAbrufFehler;
    procedure KonfigurationGelesen;
    procedure Konvertieren; override;
  public
    constructor Create (ACommComponent: TComponent; AAbrufData: TAbrufData);
    destructor Destroy; override;
  end;


  { Abruf-Manager für Momentanwerteabruf/Parametrierung von DSfG-Instanzen }

  TMomentanAbrufManager = class (TAbrufManager)
  protected
    procedure SetAbrufStation (AStationId: integer);
    procedure DatenelementeAbruf; override;
    procedure VerbindungIstBeendet; override;
    procedure DeleteMomTables;
    procedure DSfGAbrufTableAufraeumen;
  public
    constructor Create (ACommComponent: TComponent; AStationId: integer);
    destructor Destroy; override;
  end;


  { Abruf-Manager für Parametrierung der DSfG-DFÜ-Instanz }

  TDFUEMomentanAbrufManager = class (TAbrufManager)
  protected
    procedure SetAbrufStation (AStationId: integer);
    procedure VerbindungSteht; override;
    procedure ParameterAbruf;
    procedure VerbindungIstBeendet; override;
    procedure DeleteMomTables;
    procedure DSfGAbrufTableAufraeumen;
  public
    constructor Create (ACommComponent: TComponent; AStationId: integer);
    destructor Destroy; override;
  end;


  { Abruf-Manager für Rufentgegenahme }

  TRufManager = class (TAbrufManager)
  protected
    function LogbuecherAbrufen: boolean; override;
    procedure LogbuecherAbruf; override;
  public
    constructor Create (ACommComponent: TComponent; AAbrufart: TAbrufart);
    procedure RufEntgegennehmen;
  end;


  { Abruf-Manager für Rufreaktivierung }

  TRufReaktManager = class (TAbrufManager)
  protected
    procedure SetRufReaktStation (AStationId: integer);
    procedure VerbindungSteht; override;
    procedure Reaktivieren;
    procedure VerbindungIstBeendet; override;
  public
    constructor Create (ACommComponent: TComponent; AStationId: integer);
    destructor Destroy; override;
  end;

IMPLEMENTATION

{ TAbrufManager }

{--------------------------------------------------------------------------------}
constructor TAbrufManager.Create (ACommComponent: TComponent; AAbrufart: TAbrufart);
{--------------------------------------------------------------------------------}
begin
  inherited Create;
  FVerbindungSteht := False;
  FADManuCount := 0;
  Abrufart:=AAbrufart;
  CommComponent := ACommComponent;
  FillChar (AbrufData, SizeOf (TAbrufData), 0);                 { Vorbelegung für AbrufData }
  AbrufModus:=0;                                                { Vorbelegung für AbrufModus }
  DfueTransparentmodus:=true;  { Standard: DSfG-DFÜ transparent schalten beim Verbindungsaufbau }

  Abruf:=TAbruf.Create (ACommComponent, Abrufart);
  COMPort:=Abruf.COMPort;

  with AktuZeitangaben do begin                                 { Vorbelegung für AktuZeitangaben }
    EAdr:=NUL;
    DatumZeit:=0;
    Zeitzone:='';
    LetztVerstZZ:=0;
    vom_PC:=false;
  end;

  Stammdaten:=TDSfGStammdaten.Create (PathServer.PathName [WStammDir]);
  Stammdaten.InitTabellen;
  KonvListeArchive:=TKonvList.Create;
  KonvListeLogbuecher:=TKonvList.Create;
  KonvListeDatenelemente:=TKonvList.Create;

  { Einträge in Zustandstabelle vor dem Abruf sicherheitshalber löschen: }
  if Abruf.WithZustandtabelle then
    DeleteZustaende (COMPort);
end;

{-------------------------------}
destructor TAbrufManager.Destroy;
{-------------------------------}
begin
  JournalReorganisieren;
  { Einträge in Zustandstabelle nach Abruf wieder löschen: }
  if Abruf.WithZustandtabelle then begin
    DeleteZustaende (COMPort);
  end;

  KonvListeDatenelemente.Free;
  KonvListeLogbuecher.Free;
  KonvListeArchive.Free;
  Stammdaten.Free;
  Abruf.Free;
  inherited Destroy;
end;

{---------------------------------------------}
function TAbrufManager.ArchiveAbrufen: boolean;
{---------------------------------------------}
begin
  Result:=false;
end;

{------------------------------------------------}
function TAbrufManager.LogbuecherAbrufen: boolean;
{------------------------------------------------}
begin
  Result:=false;
end;

{---------------------------------------------------}
function TAbrufManager.DatenelementeAbrufen: boolean;
{---------------------------------------------------}
begin
  Result:=false;
end;

{------------------------------}
procedure TAbrufManager.Abrufen;
{------------------------------}
begin
  if AbrufData.StationId = 0 then exit;
  AbrufData.Erfolgreich := true;

  if Abruf.VerbAufbau (AbrufData.StationId, AbrufData.Datentypen, DfueTransparentmodus,
                       AbrufData.Keine_weiteren_Versuche) then
    VerbindungSteht                    { Verbindungsaufbau ok }
  else begin
    if Abruf.NoCarrier then begin
      UpdateDSfGJournal (Abruf.JournalId, C_WJournal_DZVerbEnde);
      AbrufData.Erfolgreich := false;   { Fehler beim Verbindungsaufbau, Verbindung steht nicht }
    end else
       Verbindungsfehler;              { Fehler beim Verbindungsaufbau, Verbindung steht }
  end;
end;

{--------------------------------------}
procedure TAbrufManager.VerbindungSteht;
{--------------------------------------}
begin
  DatenelementeAbruf;
end;

{----------------------------------------}
procedure TAbrufManager.Verbindungsfehler;
{----------------------------------------}
begin
  AbrufData.Erfolgreich := false;
  VerbindungBeenden;
end;

{----------------------------------------}
procedure TAbrufManager.VerbindungBeenden(bVerbSteht: boolean = False);
{----------------------------------------}
begin
  if (not bVerbSteht) then Abruf.VerbAbbau;
  VerbindungIstBeendet;
end;

{-------------------------------------------}
procedure TAbrufManager.VerbindungIstBeendet;
{-------------------------------------------}
begin
  Konvertieren;
end;

{-----------------------------------------}
procedure TAbrufManager.DatenelementeAbruf;
{-----------------------------------------}
var
  AbrufListe: TAbrufList;
  Ok: boolean;
  ADManu: TADManu;

begin
  if DatenelementeAbrufen then begin
    AbrufListe :=TAbrufList.Create;
    try
      if (Abrufart = aa_manuell) or ((FVerbindungSteht) and (FADManuCount > 0))
      then begin
        { Abrufliste füllen aus manueller Auftragstabelle: }
        ADManu:=TADManu.Create (Pathserver.PathName [WStammdir]);
        try
          ADManu.Get_DSfGManuAbrufliste (AbrufData.StationId, C_IsDatenelemente, AbrufListe);
        finally
          ADManu.Free;
        end;
      end
      else begin                                                                 { automatisch }
        { Abrufliste füllen aus Stammdaten: }
        Stammdaten.GetAutoDatenAbrufliste (AbrufData.StationId, C_IsDatenelemente, AbrufListe);
      end;

      if AbrufListe.Count > 0 then                                               { Abrufliste enthält Abrufdaten }
        Ok:=Abruf.AbrufDaten (d_DelAdr, 0.0, 0.0, C_IsDatenelemente, AbrufListe,
                              KonvListeDatenelemente, AktuZeitangaben)           { Abrufen }
      else
        Ok:=true;
    finally
      AbrufListe.Free;
    end;

    if Ok then
      DatenelementeGelesen
    else
      DatenelementeAbrufFehler;
  end else
    DatenelementeGelesen;
end;

{-----------------------------------------------}
procedure TAbrufManager.DatenelementeAbrufFehler;
{-----------------------------------------------}
begin
  AbrufData.Erfolgreich := false;
  ArchiveAbruf;
end;

{-------------------------------------------}
procedure TAbrufManager.DatenelementeGelesen;
{-------------------------------------------}
begin
  ArchiveAbruf;
end;

{-----------------------------------}
procedure TAbrufManager.ArchiveAbruf;
{-----------------------------------}
var
  AbrufListe: TAbrufList;
  Ok: boolean;
  ADManu: TADManu;

begin
  if ArchiveAbrufen then begin
    AbrufListe :=TAbrufList.Create;
    try
      if (Abrufart = aa_manuell) or ((FVerbindungSteht) and (FADManuCount > 0))
      then begin
        { Abrufliste füllen aus manueller Abruftabelle: }
        ADManu:=TADManu.Create (Pathserver.PathName [WStammdir]);
        try
          ADManu.Get_DSfGManuAbrufliste (AbrufData.StationId, C_IsArchive, AbrufListe);
        finally
          ADManu.Free;
        end;
      end else                                                                                            { automatisch }
        { Abrufliste füllen aus Stammdaten: }
        Stammdaten.GetAutoDatenAbrufliste (AbrufData.StationId, C_IsArchive, AbrufListe);

      if AbrufListe.Count > 0 then                                                      { Abrufliste enthält Abrufdaten }
        Ok:=Abruf.AbrufDaten (AbrufModus, AbrufData.DatenVon, AbrufData.DatenBis, C_IsArchive,
                              AbrufListe, KonvListeArchive, AktuZeitangaben)     { Abrufen }
      else
        Ok:=true;
    finally
      AbrufListe.Free;
    end;

    if Ok then
      ArchiveGelesen
    else
      ArchiveAbrufFehler;
  end else
    ArchiveGelesen;
end;

{-----------------------------------------}
procedure TAbrufManager.ArchiveAbrufFehler;
{-----------------------------------------}
begin
  AbrufData.Erfolgreich := false;
  LogbuecherAbruf;
end;

{-------------------------------------}
procedure TAbrufManager.ArchiveGelesen;
{-------------------------------------}
begin
  LogbuecherAbruf;
end;

{--------------------------------------}
procedure TAbrufManager.LogbuecherAbruf;
{--------------------------------------}
var
  AbrufListe: TAbrufList;
  Ok: boolean;
  ADManu: TADManu;

begin
  if LogbuecherAbrufen then begin
    AbrufListe :=TAbrufList.Create;
    try
      if (Abrufart = aa_manuell) or ((FVerbindungSteht) and (FADManuCount > 0))
      then begin
        { Abrufliste füllen aus manueller Abruftabelle: }
        ADManu:=TADManu.Create (Pathserver.PathName [WStammdir]);
        try
          ADManu.Get_DSfGManuAbrufliste (AbrufData.StationId, C_IsLogbuecher, AbrufListe);
        finally
          ADManu.Free;
        end;
      end else                                                                                            { automatisch }
        { Abrufliste füllen aus Stammdaten: }
        Stammdaten.GetAutoDatenAbrufliste (AbrufData.StationId, C_IsLogbuecher, AbrufListe);

      { Abrufen: }
      if AbrufListe.Count > 0 then                                                      { Abrufliste enthält Abrufdaten }
        Ok:=Abruf.AbrufDaten (AbrufModus, AbrufData.DatenVon, AbrufData.DatenBis, C_IsLogbuecher,
                              AbrufListe, KonvListeLogbuecher, AktuZeitangaben)
      else
        Ok:=true;
    finally
      AbrufListe.Free;
    end;

    if Ok then
      LogbuecherGelesen
    else
      LogbuecherAbrufFehler;
  end else
    LogbuecherGelesen;
end;

{--------------------------------------------}
procedure TAbrufManager.LogbuecherAbrufFehler;
{--------------------------------------------}
begin
  AbrufData.Erfolgreich := false;
  VerbindungBeenden;
end;

{----------------------------------------}
procedure TAbrufManager.LogbuecherGelesen;
{----------------------------------------}
begin
  VerbindungBeenden;
end;

{-----------------------------------------------}
procedure TAbrufManager.AktuelleZeitangabenAbruf;
{-----------------------------------------------}
{ für Konvertierung von Archiv- und Logbuchdaten (zur Ermittlung der Zeitzonen)
  aktuelle Zeitangaben (hier insbesondere aktuelle Zeitzone, letzte Verstellung
  der Zeitzone) von den DSfG-Instanzen abrufen (-> "Bus-Zeit"), bis eine Instanz
  einen Wert für die Zeitzone (DEL: acb) liefert (leider können nicht alle Geräte
  die Zeitangaben aus dem allgemeinen Teil der Datenelementeliste bereitstellen !) }
var
  Busadressen: string;
  i: integer;
  MerkBuf: TZeitangaben;

begin
  if ArchiveAbrufen OR LogbuecherAbrufen then begin
    ZustandMessage (COMPort, AbrufData.StationId, z_ZeitangabenAbrufen, '',
                    Abruf.WithZustandTabelle);
    { Busadressen der Instanzen aus Stammdaten lesen (ohne DFÜ-Instanzen, da von
      diesen systembedingt nichts abgefragt werden kann !):
      ab 08.05.2002: Busadressen ohne die DFÜ-Instanz(en), bisher nur ohne Login-DFÜ-Instanz }
    Busadressen:=Stammdaten.GetBusadressen (AbrufData.StationId, C_D_Instanztyp_DFU);
    { wenn innerhalb des Abrufs schon Zeitangaben gelesen wurden:
      zuerst nochmal von gleicher Adresse lesen }
    if AktuZeitangaben.EAdr <> NUL then
      Busadressen:=AktuZeitangaben.EAdr + Busadressen;
    for i:=1 to length (Busadressen) do begin
      Abruf.AbrufZeitangaben (Busadressen [i], MerkBuf);
      if MerkBuf.Zeitzone <> '' then begin
        AktuZeitangaben:=MerkBuf;
        Break;
      end;
    end;
  end;
end;

{-----------------------------------}
procedure TAbrufManager.Konvertieren;
{-----------------------------------}
var
  ArchivPfad: TFileName;
  DInstanzwertKonv: TDInstanzwertKonv;
  DArchivLogbuchKonvDB: TDArchivLogbuchKonvDB;
  Benutzer: string;
  KavKonfigDb: TKavKonfigDb;
  KavTables_OK: boolean;

begin
  { Zielpfad für abgerufene Daten, Daten anhängen ja/nein: }
  if Abrufart = aa_manuell then begin
    ArchivPfad:=PathServer.PathName [DManuDir];
    Benutzer:=C_BenutzerManu;
  end
  else begin
    ArchivPfad:=PathServer.PathName [DArchivDir];
    Benutzer:=C_BenutzerAuto;
  end;

  { Instanzwerte konvertieren: }
  if KonvListeDatenelemente.Count > 0 then begin
    DInstanzwertKonv:=TDInstanzwertKonv.Create (ArchivPfad);
    try
      { Konvertierungsliste der Instanzwerte abarbeiten: }
      ZustandMessage (COMPort, AbrufData.StationId, z_DatenelementeKonvertieren, '', Abruf.WithZustandTabelle);

      { kein manueller Abruf: vor der Konvertierung gespeicherte Datenelemente aller Instanzen der Station löschen
        (bei manuellem Abruf werden die Daten vor dem Abruf generell gelöscht) }
      if Abrufart <> aa_manuell then
        LoescheDatenelemente_Station (true, AbrufData.StationId);

      DInstanzwertKonv.Konvertiere (KonvListeDatenelemente, Abruf.JournalId, RohdatenLoeschen);
    finally
      DInstanzwertKonv.Free;
    end;
  end;

  { Archive/Logbücher konvertieren: }
  DArchivLogbuchKonvDB:=TDArchivLogbuchKonvDB.Create (ArchivPfad,
                                                      PathServer.PathName [WStammDir],
                                                      PathServer.PathName [WWorkDir],
                                                      PathServer.PathName [AsciiDir],
                                                      Benutzer, AbrufData.StationId,
                                                      Systemdaten.GrLogbuchPuffer);
  try
    if KonvListeArchive.Count > 0 then begin
      { Konvertierungsliste der Archive abarbeiten: }
      ZustandMessage (COMPort, AbrufData.StationId, z_ArchiveKonvertieren, '', Abruf.WithZustandTabelle);
      DArchivLogbuchKonvDB.Konvertiere (KonvListeArchive, AktuZeitangaben, Abruf.JournalId, RohdatenLoeschen);
    end;

    if KonvListeLogbuecher.Count > 0 then begin
      { Konvertierungsliste der Logbücher abarbeiten: }
      ZustandMessage (COMPort, AbrufData.StationId, z_LogbuecherKonvertieren, '', Abruf.WithZustandTabelle);
      DArchivLogbuchKonvDB.Konvertiere (KonvListeLogbuecher, AktuZeitangaben, Abruf.JournalId, RohdatenLoeschen);
    end;
  finally
    DArchivLogbuchKonvDB.Free;
  end;

  { zusätzliche optionale, kavernenbezogene Konvertierung der Archive starten, wenn
    Kavernen-Zuordnungstabellen vorhanden sind (nicht bei manuellem Abruf): }
  if Abrufart <> aa_manuell then begin
    KavKonfigDb:=TKavKonfigDb.Create (PathServer.PathName[WStammDir]);
    try
      KavTables_OK:=KavKonfigDb.TablesExist;
    finally
      KavKonfigDb.Free;
    end;
    if KavTables_OK then begin
      ZustandMessage (COMPort, AbrufData.StationId, z_KavArchiveKonvertieren, '', Abruf.WithZustandTabelle);
      Konvertiere_KavernenArchive (AbrufData.StationId, FW_Zeitfenster);
    end;
  end;
end;

{--------------------------------------------}
procedure TAbrufManager.JournalReorganisieren;
{--------------------------------------------}
var
  JournalDB: TJournalDB;
begin
  ZustandMessage (COMPort, AbrufData.StationId, z_JournalReorganisieren, '', Abruf.WithZustandTabelle);
  JournalDB:=TJournalDB.Create (PathServer.PathName [WStammDir]);
  try
    { nur Zeitbereiche für die letzten 5 Abrufe im Journal speichern, sonst wird DJZBDAT.DB zu groß: }
{$ifdef GAMESS}
    JournalDB.ReorganizeByStationsabrufe (C_GerArtDSfG, AbrufData.StationId, 164);
{$ELSE}
    JournalDB.ReorganizeByStationsabrufe (C_GerArtDSfG, AbrufData.StationId, 5);
{$ENDIF}
    JournalDB.Reorganize (Systemdaten.GrJournalPuffer);        { wird nur 1 mal täglich gemacht }
  finally
    JournalDB.Free;
  end;
end;


{ TAutoManuAbrufManager }

{-----------------------------------------------------------------------------------}
constructor TAutoManuAbrufManager.Create (ACommComponent: TComponent;
  AAbrufart: TAbrufart; AAbrufData: TAbrufData; bVerbSteht: boolean = False);
{-----------------------------------------------------------------------------------}
begin
  inherited Create (ACommComponent, AAbrufart);

  Abruf.NoCarrier := not bVerbSteht;
  AbrufData:=AAbrufData;
  FVerbindungSteht := bVerbSteht;

  if (FVerbindungSteht) then
    with TADManu.Create(PathServer[WStammDir]) do
    try
      FADManuCount := Get_StaIdRecCount(AAbrufData.StationId);
    finally
      Free;
    end;

  { AbrufModus für Archive/Logbücher: }
  if Abrufart = aa_manuell then begin
    if (Int (AbrufData.DatenVon) > 0) AND (Int (AbrufData.DatenBis) > 0) then
      AbrufModus:=d_VonBis                { manuelle Abfrage über Zeitbereich }
    else
      AbrufModus:=d_OrdNr;             { manuelle Abfrage über Ordnungsnummer }
  end
  else begin
    if AutomatikZeitabruf then
      AbrufModus:=d_NeueZeit     { automatisch: neue Daten holen über Zeit }
    else
      AbrufModus:=d_NeueOrdNr;   { automatisch: neue Daten holen über Ordnungsnummer }
  end;
end;

{---------------------------------------}
destructor TAutoManuAbrufManager.Destroy;
{---------------------------------------}
var
  ADManu: TADManu;
begin
  { evtl. übriggebliebene Abrufdaten der Station aus manueller Auftragstabelle löschen: }
  ADManu:=TADManu.Create (Pathserver.PathName [WStammdir]);
  try
    ADManu.DeleteDSfGManuAbruf (AbrufData.StationId);
  finally
    ADManu.Free;
  end;

  inherited Destroy;
end;

{-----------------------------------------------------}
function TAutoManuAbrufManager.ArchiveAbrufen: boolean;
{-----------------------------------------------------}
begin
  Result:=AbrufData.Datentypen AND C_IsArchive <> 0;
end;

{--------------------------------------------------------}
function TAutoManuAbrufManager.LogbuecherAbrufen: boolean;
{--------------------------------------------------------}
begin
  Result:=AbrufData.Datentypen AND C_IsLogbuecher <> 0;
end;

{-----------------------------------------------------------}
function TAutoManuAbrufManager.DatenelementeAbrufen: boolean;
{-----------------------------------------------------------}
begin
  Result:=AbrufData.Datentypen AND C_IsDatenelemente <> 0;
end;

{-------------------------------------------------------}
procedure TAutoManuAbrufManager.DatenelementeAbrufFehler;
{-------------------------------------------------------}
begin
  AbrufData.Erfolgreich := false;
  DELInst_InstTypAbruf ('V');
end;

{---------------------------------------------------}
procedure TAutoManuAbrufManager.DatenelementeGelesen;
{---------------------------------------------------}
begin
  DELInst_InstTypAbruf ('V');
end;

{--------------------------------------------------------------------------}
procedure TAutoManuAbrufManager.DELInst_InstTypAbruf (vorher_nachher: char);
{--------------------------------------------------------------------------}
{ bei Automatik instanzspezifische und instanztypspezifische Datenelemente vor und nach Archiven/Logbüchern mitabrufen;
  Übergabe: vorher_nachher: 'V' = Datenelemente abrufen, die für einen Abruf vor den Archiven/Logbüchern definiert sind
                            'N' = Datenelemente abrufen, die für einen Abruf nach den Archiven/Logbüchern definiert sind }
var
  AbrufListe: TAbrufList;
  Ok: boolean;

begin
  if (Abrufart = aa_automatisch) AND (ArchiveAbrufen OR LogbuecherAbrufen) then begin
    AbrufListe:=TAbrufList.Create;
    try
      { Abrufliste füllen aus Stammdaten: }
      Stammdaten.GetDELInst_InstTypAbrufliste (AbrufData.StationId, vorher_nachher, AbrufListe);
      if AbrufListe.Count > 0 then begin                                               { Abrufliste enthält Abrufdaten }
        { Datentyp "Datenelemente" im Journal vermerken: }
        UpdateDSfGJournalDatentypen (Abruf.JournalId, AbrufData.Datentypen OR C_IsDatenelemente);
        { Abrufen }
        Ok:=Abruf.AbrufDaten (d_DelAdr, 0.0, 0.0, C_IsDatenelemente, AbrufListe,
                              KonvListeDatenelemente, AktuZeitangaben);
      end else
        Ok:=true;
    finally
      AbrufListe.Free;
    end;

    if Ok then
      DELInst_InstTypGelesen (vorher_nachher)
    else
      DELInst_InstTypAbrufFehler (vorher_nachher);
  end else
    DELInst_InstTypGelesen (vorher_nachher);
end;

{--------------------------------------------------------------------------------}
procedure TAutoManuAbrufManager.DELInst_InstTypAbrufFehler (vorher_nachher: char);
{--------------------------------------------------------------------------------}
begin
  AbrufData.Erfolgreich := false;
  DELInst_InstTypGelesen (vorher_nachher);
end;

{----------------------------------------------------------------------------}
procedure TAutoManuAbrufManager.DELInst_InstTypGelesen (vorher_nachher: char);
{----------------------------------------------------------------------------}
begin
  if vorher_nachher = 'V' then begin
    { Abruf der Zeitangaben vor dem Archivabruf, damit die für die Konvertierung
      der Archiv- und Logbuchdaten benötigten Zeitangaben trotz einer möglichen
      Verbindungsunterbrechung auf jeden Fall vorhanden sind.
      -> wird ab 04.11.2002 auch für Anpassung des Abruf-Zeitbereichs für Logbücher
         benötigt }
    AktuelleZeitangabenAbruf;
    ArchiveAbruf;
  end
  else begin
    { nochmaliger Abruf der Zeitangaben vor dem Verbindungsende, damit AktuZeitangaben
      aktualisiert wird (es könnte während des Abrufs in der Station eine Zeit-
      zonenverstellung aufgetreten sein !): }
    AktuelleZeitangabenAbruf;
    VerbindungBeenden(FVerbindungSteht);
  end;
end;

{----------------------------------------------------}
procedure TAutoManuAbrufManager.LogbuecherAbrufFehler;
{----------------------------------------------------}
begin
  AbrufData.Erfolgreich := false;
  DELInst_InstTypAbruf ('N');
end;

{------------------------------------------------}
procedure TAutoManuAbrufManager.LogbuecherGelesen;
{------------------------------------------------}
begin
  DELInst_InstTypAbruf ('N');
end;


{ TKonfLesenAbrufManager }

{---------------------------------------------------------------------------------------}
constructor TKonfLesenAbrufManager.Create (ACommComponent: TComponent; AAbrufData: TAbrufData);
{---------------------------------------------------------------------------------------}
begin
  inherited Create (ACommComponent, aa_konflesen);
  AbrufData:=AAbrufData;
  InstanzListe:=TInstanzDataList.Create;
  InstDfuListe:=TInstDfuDataList.Create;
  KonfigRohdataListe:=TKonfigRohdataList.Create;
end;

{----------------------------------------}
destructor TKonfLesenAbrufManager.Destroy;
{----------------------------------------}
begin
  KonfigRohdataListe.Free;
  InstDfuListe.Free;
  InstanzListe.Free;
  inherited Destroy;
end;

{-----------------------------------------------}
procedure TKonfLesenAbrufManager.VerbindungSteht;
{-----------------------------------------------}
begin
  KonfigurationAbruf;
end;

{--------------------------------------------------}
procedure TKonfLesenAbrufManager.KonfigurationAbruf;
{--------------------------------------------------}
begin
  { Konfiguration abrufen und in Listen zurückgeben: }
  if Abruf.AbrufKonfiguration (InstanzListe, InstDfuListe, KonfigRohdataListe) then
    KonfigurationGelesen
  else
    KonfigurationAbrufFehler;
end;

{--------------------------------------------------------}
procedure TKonfLesenAbrufManager.KonfigurationAbrufFehler;
{--------------------------------------------------------}
begin
  AbrufData.Erfolgreich := false;
  VerbindungBeenden;
end;

{----------------------------------------------------}
procedure TKonfLesenAbrufManager.KonfigurationGelesen;
{----------------------------------------------------}
begin
  VerbindungBeenden;
end;

{--------------------------------------------}
procedure TKonfLesenAbrufManager.Konvertieren;
{--------------------------------------------}
{ wenn Konfigurationsabruf ok, dann die in den Listen enthaltene Gerätekonfigurationen in Stammdaten-Tabellen eintragen }
begin
  if AbrufData.Erfolgreich then begin
    ZustandMessage (COMPort, AbrufData.StationId, z_KonfigurationKonvertieren, '', Abruf.WithZustandTabelle);
    Konvertiere_Konfiguration (AbrufData.StationId, Abruf.JournalId, Abruf.Teilnehmer, Abruf.EAdr_Dfue,
                               Abruf.EAdr_nichterreicht, InstanzListe, InstDfuListe, KonfigRohdataListe);
  end;
end;


{ TMomentanAbrufManager }

{---------------------------------------------------------------------------------------}
constructor TMomentanAbrufManager.Create (ACommComponent: TComponent; AStationId: integer);
{---------------------------------------------------------------------------------------}
begin
  inherited Create (ACommComponent, aa_momentan);
  SetAbrufStation (AStationId);
  CheckMomHalten:=false;
  DSfGAbrufTableAufraeumen;    { vor dem Abruf erst mal alle MomH- und MomE-Einträge löschen }
end;

{---------------------------------------}
destructor TMomentanAbrufManager.Destroy;
{---------------------------------------}
begin
  DeleteMomTables;
  DSfGAbrufTableAufraeumen;    { alle noch evtl. vorhandenen MomH- und MomE-Einträge löschen }
  inherited Destroy;
end;

{--------------------------------------------------------------------}
procedure TMomentanAbrufManager.SetAbrufStation (AStationId: integer);
{--------------------------------------------------------------------}
begin
  AbrufData.StationId:=AStationId;
  AbrufData.Datentypen:=C_IsDatenelemente;
end;

{-------------------------------------------------}
procedure TMomentanAbrufManager.DatenelementeAbruf;
{-------------------------------------------------}
const
  C_MaxErrorCount = 3;
var
  DSfGAbruf: TDSfGAbruf;
  TbDSfGMomentanDef: TTbDSfGMomentanDef;
  AbrufListe: TAbrufList;
  Ok: boolean;
  StopIt: boolean;
  Abrufart: string;
  Keine_weiteren_Journaleintraege: boolean;
  MomDefTriggerTime       : integer;
  i, iErrorCount, iDatTyp : integer;
  nur_ein_DE_abrufen      : boolean;
begin
  ZustandMessage (COMPort, AbrufData.StationId, z_VerbindungSteht, '', Abruf.WithZustandTabelle);
  Keine_weiteren_Journaleintraege:=false;
  MomDefTriggerTime:=-1;
  iErrorCount := 0;  // Abbruch nach C_MaxErrorCount fehlerhaften Abrufen

  DSfGAbruf:=TDSfGAbruf.Create (PathServer.PathName [WStammDir]);
  try
    TbDSfGMomentanDef:=TTbDSfGMomentanDef.Create (PathServer.PathName [WNetWorkDir], AbrufData.StationId);
    try
      AbrufListe:=TAbrufList.Create;
      try
        nur_ein_DE_abrufen:=false;                              { Vorbelegung: alle angeforderten Datenelemente abrufen }
        Ok:=true;
        repeat
          Application.ProcessMessages;
          StopIt:=false;
          { Delay, um Belastung der seriellen Schnittstelle und der Abruf-Tabelle
            zu reduzieren: }
          Delay (AfDSfG32Ini.MomDelay);   // 19.04.2003

          { Momentanwert-Definitionstabelle neu lesen, wenn DOS-Zeit des Triggerfiles sich geändert hat: }
          i:=GetTriggerTime(PathServer.Pathname[WNetWorkDir] + C_TbDMD + Format('%.4d.DB', [AbrufData.StationId]));
          if i <> MomDefTriggerTime then begin
            TbDSfGMomentanDef.GetAbrufliste (AbrufListe);
            MomDefTriggerTime:=i;
          end;

          if AbrufListe.Count > 0 then
            Ok:=Abruf.AbrufMomentanwerte (AbrufListe, nur_ein_DE_abrufen, Keine_weiteren_Journaleintraege)
          else
            Ok:=true;

          if Ok then begin
            iErrorCount := 0;

            if DSfGAbruf.GetDSfGAbrufMomentan (AbrufData.StationId, Abrufart) then begin
              if Abrufart = C_AbrArtParaStart then begin
                { für schnellere Reaktion bis zum Datenelement-Senden ab jetzt nur 1 DE lesen, um die Verbindung zu halten
                  und diesen nicht speichern: }
                nur_ein_DE_abrufen:=true;
              end;
              if Abrufart = C_AbrArtParaSenden then
                Abruf.UebertragungDatenelemente;             { Datenelemente übertragen auf Anforderung }
              if Abrufart = C_AbrArtParaEnde then begin
                { ab jetzt wieder alle Datenelemente lesen und speichern: }
                nur_ein_DE_abrufen:=false;
              end;
              if Abrufart = C_AbrArtBinaerSenden then
                Abruf.AbrufBinaerdatei;              { Binärdateibefehl senden auf Anforderung }
              if Abrufart = C_AbrArtMomStop then
                StopIt:=true;                        { Momentanwerteholen beenden auf Anforderung }

              if (Abrufart = C_AbrArtAuto) then begin  // 13.05.2003
                iDatTyp := Self.AbrufData.Datentypen;
                Self.AbrufData.Datentypen := C_IsArchive + C_IsLogbuecher;
                with TAutoManuAbrufManager.Create(Self.CommComponent,
                  aa_automatisch, Self.AbrufData, True) do
                try
                  C_AktDSfGAbrufStatus := C_Status_Abruf;
                  Abruf.EAdr_Dfue := Self.Abruf.EAdr_Dfue;
                  Stammdaten.GetRufStammDaten(
                    Self.AbrufData.StationId, Abruf.Rufstammdaten);  // 07.07.2003
                  VerbindungSteht;
                  with TAuftragDb.Create (PathServer.PathName [WStammDir],
                                  PathServer.PathName [WNetProgDir]) do
                  try
                    DeleteAuftrag (C_GerArtDSfG, AbrufData.StationId, C_AbrArtAuto);
                  finally
                    Free;
                    C_AktDSfGAbrufStatus := C_Status_Momentanwerte;
                  end;
                finally
                  Free;
                  Self.AbrufData.Datentypen := iDatTyp;
                end;
              end;

              if not StopIt AND CheckMomHalten then begin
                { Sicherheitsstop, wenn kein MomHalten-Eintrag in ABRFDSFG.DB: }
                StopIt:= not DSfGAbruf.GetDSfGAbrufMomentanHalten (AbrufData.StationId);
                CheckMomHalten:=false;                                      { Flag zurücksetzen }
              end;
            end else  { Fehler beim Lesen aus DSfG-Abruftabelle }
              StopIt:=true;
          end
          else begin { Fehler beim Momentanwerte abrufen }
            Inc(iErrorCount);
            StopIt := (iErrorCount >= C_MaxErrorCount);
          end;
        until StopIt;
      finally
        AbrufListe.Free;
      end;
    finally
      TbDSfGMomentanDef.Free;
    end;
  finally
    DSfGAbruf.Free;
  end;

  if not Ok then
    AbrufData.Erfolgreich := false;
  VerbindungBeenden;
end;

{---------------------------------------------------}
procedure TMomentanAbrufManager.VerbindungIstBeendet;
{---------------------------------------------------}
begin
  { nichts machen }
end;

{----------------------------------------------}
procedure TMomentanAbrufManager.DeleteMomTables;
{----------------------------------------------}
{ Momentanwerte-Tabellen incl. Triggerfiles löschen }
var
  MomTable: TTableExt;
begin
  MomTable:=TTableExt.Create (nil);
  try
    MomTable.DatabaseName:=PathServer.PathName [WNetWorkDir];
    { Tabelle für Momentanwerte: }
    MomTable.TableName:=C_TbDMom + Format('%.4d.DB', [AbrufData.StationId]);
    if MomTable.Exists then
      MomTable.DeleteTable;
    DeleteTriggerFile (MomTable.DatabaseName + MomTable.TableName);

    { Tabelle für Parametrierung: }
    MomTable.TableName:=C_TbDDE + Format('%.4d.DB', [AbrufData.StationId]);
    if MomTable.Exists then
      MomTable.DeleteTable;
    DeleteTriggerFile (MomTable.DatabaseName + MomTable.TableName);
  finally
    MomTable.Free;
  end;
end;

{-------------------------------------------------------}
procedure TMomentanAbrufManager.DSfGAbrufTableAufraeumen;
{-------------------------------------------------------}
{ vorhandene MomH- und MomE-Datensätze aus DSfG-Abruftabelle löschen }
var
  DSfGAbruf: TDSfGAbruf;
begin
  DSfGAbruf:=TDSfGAbruf.Create (PathServer.PathName [WStammDir]);
  try
    DSfGAbruf.DeleteDSfGMomentan (AbrufData.StationId);
  finally
    DSfGAbruf.Free;
  end;
end;


{ TDFUEMomentanAbrufManager }

{-------------------------------------------------------------------------------------------}
constructor TDFUEMomentanAbrufManager.Create (ACommComponent: TComponent; AStationId: integer);
{-------------------------------------------------------------------------------------------}
begin
  inherited Create (ACommComponent, aa_dfue_momentan);
  DfueTransparentmodus:=false;  { DSfG-DFÜ nicht transparent schalten beim Verbindungsaufbau }
  SetAbrufStation (AStationId);
  CheckMomHalten:=false;
  DSfGAbrufTableAufraeumen;    { vor dem Abruf erst mal evtl. verbliebene Einträge
                                 von früheren DFÜ-Parametrierungen löschen }
end;

{-------------------------------------------}
destructor TDFUEMomentanAbrufManager.Destroy;
{-------------------------------------------}
begin
  DeleteMomTables;
  DSfGAbrufTableAufraeumen; { alle noch evtl. vorhandenen Einträge des DFÜ-Momentanwertabrufs löschen }
  inherited Destroy;
end;

{------------------------------------------------------------------------}
procedure TDFUEMomentanAbrufManager.SetAbrufStation (AStationId: integer);
{------------------------------------------------------------------------}
begin
  AbrufData.StationId:=AStationId;
end;

{--------------------------------------------------}
procedure TDFUEMomentanAbrufManager.VerbindungSteht;
{--------------------------------------------------}
begin
  ParameterAbruf;
end;

{-------------------------------------------------}
procedure TDFUEMomentanAbrufManager.ParameterAbruf;
{-------------------------------------------------}
var
  DSfGAbruf: TDSfGAbruf;
  StopIt: boolean;
  Abrufart: string;
  OK: boolean;
  Counter: integer;

begin
  ZustandMessage (COMPort, AbrufData.StationId, z_VerbindungSteht, '', Abruf.WithZustandTabelle);

  DSfGAbruf:=TDSfGAbruf.Create (PathServer.PathName [WStammDir]);
  try
    { DSfG-DFÜ-Daten abrufen (nicht in Stammdaten übernehmen): }
    OK:=Abruf.AbrufDfueMomentanwerte (false);
    if OK then begin
      Counter:=0;
      repeat
        Application.ProcessMessages;
        { Delay, um Belastung der seriellen Schnittstelle und der Abruf-Tabelle
          zu reduzieren: }
        Delay (2000);

        inc (Counter);
        if (Counter MOD 10) = 0 then begin      { nur bei jedem 10. Durchlauf }
          OK:=Abruf.DfueVerbindungHalten;
          Counter:=0;
        end else
          OK:=true;

        if OK then begin
          StopIt:=false;
          if DSfGAbruf.GetDSfGAbrufDfueMomentan (AbrufData.StationId, Abrufart) then begin
            if Abrufart = C_AbrArtParaSenden then
              Abruf.UebertragungDfueParameter;     { DSfG-DFÜ-Parameter übertragen auf Anforderung }
            if Abrufart = C_AbrArtBinaerSenden then
              Abruf.AbrufBinaerdatei;              { Binärdateibefehl senden auf Anforderung }
            if Abrufart = C_AbrArtMomDfueStop then
              StopIt:=true;                        { DFÜ-Momentanwerteholen beenden auf Anforderung }

            if not StopIt AND CheckMomHalten then begin
              { Sicherheitsstop, wenn kein MomDfueHalten-Eintrag in ABRFDSFG.DB: }
              StopIt:= not DSfGAbruf.GetDSfGAbrufDfueMomentanHalten (AbrufData.StationId);
              CheckMomHalten:=false;                                      { Flag zurücksetzen }
            end;
          end else  { Fehler beim Lesen aus DSfG-Abruftabelle }
            StopIt:=true;
        end else  { Fehler beim Halten der Verbindung }
          StopIt:=true;
      until StopIt;
    end;  { if OK }
  finally
    DSfGAbruf.Free;
  end;

  if not OK then
    AbrufData.Erfolgreich := false;
  VerbindungBeenden;
end;

{-------------------------------------------------------}
procedure TDFUEMomentanAbrufManager.VerbindungIstBeendet;
{-------------------------------------------------------}
begin
  { nichts machen }
end;

{--------------------------------------------------}
procedure TDFUEMomentanAbrufManager.DeleteMomTables;
{--------------------------------------------------}
{ DFÜ-Momentanwert-Tabellen incl. Triggerfiles und löschen }
var
  MomTable: TTableExt;
begin
  MomTable:=TTableExt.Create (nil);
  try
    MomTable.DatabaseName:=PathServer.PathName [WNetWorkDir];
    { Tabelle für DFÜ-Momentanwerte: }
    MomTable.TableName:=C_Tb_DMomDfue + Format('%.4d.DB', [AbrufData.StationId]);
    if MomTable.Exists then
      MomTable.DeleteTable;
    DeleteTriggerFile (MomTable.DatabaseName + MomTable.TableName);

    { Tabelle für Parametrierung: }
    MomTable.TableName:=C_Tb_DDfuePE + Format('%.4d.DB', [AbrufData.StationId]);
    if MomTable.Exists then
      MomTable.DeleteTable;
    DeleteTriggerFile (MomTable.DatabaseName + MomTable.TableName);
  finally
    MomTable.Free;
  end;
end;

{-----------------------------------------------------------}
procedure TDFUEMomentanAbrufManager.DSfGAbrufTableAufraeumen;
{-----------------------------------------------------------}
{ verbliebene Datensätze des DFÜ-Momentanwertabrufs aus DSfG-Abruftabelle löschen }
var
  DSfGAbruf: TDSfGAbruf;
begin
  DSfGAbruf:=TDSfGAbruf.Create (PathServer.PathName [WStammDir]);
  try
    DSfGAbruf.DeleteDSfGDfueMomentan (AbrufData.StationId);
  finally
    DSfGAbruf.Free;
  end;
end;


{ TRufManager }

{------------------------------------------------------------------------------}
constructor TRufManager.Create (ACommComponent: TComponent; AAbrufart: TAbrufart);
{------------------------------------------------------------------------------}
begin
  inherited Create (ACommComponent, AAbrufart);

  { AbrufModus für Logbücher: }
  if AutomatikZeitabruf then
    AbrufModus:=d_NeueZeit     { neue Daten holen über Zeit }
  else
    AbrufModus:=d_NeueOrdNr;   { neue Daten holen über Ordnungsnummer }
end;

{----------------------------------------------}
function TRufManager.LogbuecherAbrufen: boolean;
{----------------------------------------------}
{ nur wenn die Kennung der anrufenden DSfG-Station in den Stammdaten eindeutig ist,
  werden Logbücher abgerufen (sie wären sonst keiner Station zuzuordnen) }
begin
  if Abruf.Kennung_in_Stammdaten_eindeutig then
    Result := Systemdaten.RufDatenTypen AND C_IsLogbuecher <> 0
  else
    Result:=false;
end;

{--------------------------------------}
procedure TRufManager.RufEntgegennehmen;
{--------------------------------------}
begin
  if Abruf.RufAnnahme then begin
    { Ruf erfolgreich entgegengenommen: }
    if Abruf.Kennung_in_Stammdaten_eindeutig then
      AbrufData.StationId := Abruf.RufStammdaten.StationId;
    AbrufData.Erfolgreich := true;
    VerbindungSteht;
  end
  else begin
    { Fehler bei der Rufannahme (Verbindung steht): }
    if not Abruf.NoCarrier then
      Verbindungsfehler
    else begin                          { Fehler beim Verbindungsaufbau, Verbindung steht nicht }
      UpdateDSfGJournal (Abruf.JournalId, C_WJournal_DZVerbEnde);
      AbrufData.Erfolgreich := false;
    end;
  end;
end;

{------------------------------------}
procedure TRufManager.LogbuecherAbruf;
{------------------------------------}
var
  AbrufListe: TAbrufList;
  Ok: boolean;

begin
  if LogbuecherAbrufen then begin
    { Abruf der Zeitangaben vor dem Logbuchabruf, damit die für die Konvertierung
      der Logbuchdaten benötigten Zeitangaben trotz einer möglichen
      Verbindungsunterbrechung auf jeden Fall vorhanden sind.
      -> wird ab 04.11.2002 auch für Anpassung des Abruf-Zeitbereichs für Logbücher
         benötigt }
    AktuelleZeitangabenAbruf;

    AbrufListe:=TAbrufList.Create;
    try
      { Abrufliste füllen aus Stammdaten: }
      Stammdaten.GetLogbuchAbruflisteByQuellBusadressen (AbrufData.StationId,
                                                         Abruf.EAdr_Telegrammsender,
                                                         AbrufListe);
      { Abrufen: }
      if AbrufListe.Count > 0 then            { Abrufliste enthält Abrufdaten }
        Ok:=Abruf.AbrufDaten (AbrufModus, 0, 0, C_IsLogbuecher, AbrufListe,
                              KonvListeLogbuecher, AktuZeitangaben)
      else
        Ok:=true;
    finally
      AbrufListe.Free;
    end;

    { nochmaliger Abruf der Zeitangaben vor dem Verbindungsende, damit AktuZeitangaben
      aktualisiert wird (es könnte während des Abrufs in der Station eine Zeit-
      zonenverstellung aufgetreten sein !): }
    AktuelleZeitangabenAbruf;

    if Ok then
      LogbuecherGelesen
    else
      LogbuecherAbrufFehler;
  end else
    LogbuecherGelesen;
end;


{ TRufReaktManager }

{----------------------------------------------------------------------------------}
constructor TRufReaktManager.Create (ACommComponent: TComponent; AStationId: integer);
{----------------------------------------------------------------------------------}
begin
  inherited Create (ACommComponent, aa_rufreakt);
  DfueTransparentmodus:=false;  { DSfG-DFÜ nicht transparent schalten beim Verbindungsaufbau }
  SetRufReaktStation (AStationId);
end;

{----------------------------------}
destructor TRufReaktManager.Destroy;
{----------------------------------}
begin
  { Ende für RE32 signalisieren: }
  WriteNewTime(PathServer.PathName [WNetWorkDir] + C_DSfGRufReaktTriggerFile +
               Format('%.4d.', [AbrufData.StationId]) + C_TriggerExt);
  inherited Destroy;
end;

{------------------------------------------------------------------}
procedure TRufReaktManager.SetRufReaktStation (AStationId: integer);
{------------------------------------------------------------------}
begin
  AbrufData.StationId:=AStationId;
  AbrufData.Datentypen:=0;
end;

{-----------------------------------------}
procedure TRufReaktManager.VerbindungSteht;
{-----------------------------------------}
begin
  Reaktivieren;                    { Ruffunktion in der DSfG-DFÜ reaktivieren }
end;

{--------------------------------------}
procedure TRufReaktManager.Reaktivieren;
{--------------------------------------}
begin
  if not Abruf.RufReaktivierung then
    AbrufData.Erfolgreich := false;
  VerbindungBeenden;
end;

{----------------------------------------------}
procedure TRufReaktManager.VerbindungIstBeendet;
{----------------------------------------------}
begin
  { nichts machen }
end;

end.

