{******************************************************************************}
{* Unit: Steuerung der Konvertierung von GPRS-Daten ins DSfG-Archiv           *}
{* 06.11.2006 WW                                                              *}
{* 23.05.2011 WW  Journaltabelle mit Stationsname und Gerätetyp               *}
{******************************************************************************}
unit DGPRSKonvManager;

INTERFACE

Uses
  Forms, SysUtils, Classes, ExtCtrls, DbTables, Contnrs, PathIni, WSysCon, DListen, WStream,
  DDbSta, WChars, DALKonv, DALKonvDb, T_Zeit, WMessageReg, WMessageSend, JournalFkt,
  WComm, ErrConst, TbDSfGAr, MeldungenDB, WSysDat, SysDaten, ErrPrc32, LogFile,
  DKurzzeitWerte, JournlDb, AusgabeDirList, GPRSSrvIniFile, DGPRS_KZWKonv;

type

  { Objekt zur Steuerung der Konvertierung der GPRS-DSfG-Daten }

  TGPRSKonvManagerDSfG = class (TObject)
  protected
    COMNr: integer;
    JournalId: integer;
    DSfGStammdaten: TDSfGStammdaten;
    RufStammDaten: TRufStammDaten;  { Record mit DSfG-Stammdaten für den GPRS-Datenempfang }
    FAktuZeitangaben: TZeitangaben;
    KonvListeArchive: TKonvList;
    KonvListeLogbuecher: TKonvList;
    FStammDatabase: TDatabase;
    FAutoDatabase: TDatabase;
    FManuDatabase: TDatabase;
    FAusgabeDirList: TAusgabeDirList;
    ZwFileLoeschen: boolean;
    FLogPath: string;
    WMsgADAList_AK: TWMsgDataList_DSfG_AK_ADA;  { Benachrichtigungsliste "Archivkanäle abgerufen" }
    WMsgADAList_LB: TWMsgDataList_DSfG_LB_ADA;  { Benachrichtigungsliste "Logbücher abgerufen" }
    Procedure FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer;
      AFehlertext: string = '');
    function Get_AutoVonBisZeit (InstanzId, Gruppe, Kanal: integer; Kanaltyp: string;
                                 AktuZeitangaben: TZeitangaben;
                                 var von: TDateTime; var bis: TDateTime): boolean;
    procedure KonvertierenDB;
    procedure Benachrichtigen;
    procedure JournalReorganisieren;
    function KonvertToDB (ArLbDatenListe: TDSfGDataList; GPRSKennung: string;
                          Datentyp: integer): boolean;
  public
    constructor Create (StammDatabase: TDatabase; AutoDatabase: TDatabase;
                        ManuDatabase: TDatabase; AAusgabeDirList: TAusgabeDirList;
                        AZwFileLoeschen: boolean; ALogPath: string);
    destructor Destroy; override;
    function Konvert (ArLbDatenListe: TDSfGDataList;
                      GPRSKennung: string; Datentyp: integer;
                      GPRSFehlergruppe, GPRSFehlercode: integer;
                      GPRSTelegrammRohdaten: string; GPRSTelegrammNr: integer;
                      bAusgabeKurzzeitwerte: boolean): boolean;
  end;

IMPLEMENTATION

{ TGPRSKonvManagerDSfG }

{------------------------------------------------------------------------}
constructor TGPRSKonvManagerDSfG.Create (StammDatabase: TDatabase;
                                         AutoDatabase: TDatabase;
                                         ManuDatabase: TDatabase;
                                         AAusgabeDirList: TAusgabeDirList;
                                         AZwFileLoeschen: boolean;
                                         ALogPath: string);
{------------------------------------------------------------------------}
begin
  inherited Create;
  FStammDatabase:=StammDatabase;
  FAutoDatabase:=AutoDatabase;
  FManuDatabase:=ManuDatabase;
  FAusgabeDirList:=AAusgabeDirList;
  ZwFileLoeschen:=AZwFileLoeschen;
  FLogPath:=ALogPath;

  DSfGStammdaten:=TDSfGStammdaten.Create (StammDatabase, AutoDatabase, ManuDatabase);
  DSfGStammdaten.InitTabellen;
  KonvListeArchive:=TKonvList.Create;
  KonvListeLogbuecher:=TKonvList.Create;

  WMsgADAList_AK:=TWMsgDataList_DSfG_AK_ADA.Create;
  WMsgADAList_LB:=TWMsgDataList_DSfG_LB_ADA.Create;

  COMNr:=CComTCP_IP;  // Pseudo-Schnittstellennummer für GPRS-Datenempfang
  JournalId:=0;
  with FAktuZeitangaben do begin            { Vorbelegung für AktuZeitangaben }
    EAdr:=NUL;
    DatumZeit:=0;
    Zeitzone:='';
    LetztVerstZZ:=0;
    vom_PC:=false;
  end;
end;

{--------------------------------------}
destructor TGPRSKonvManagerDSfG.Destroy;
{--------------------------------------}
begin
  JournalReorganisieren;

  WMsgADAList_LB.Free;
  WMsgADAList_AK.Free;

  KonvListeLogbuecher.Free;
  KonvListeArchive.Free;
  DSfGStammdaten.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------------------}
Procedure TGPRSKonvManagerDSfG.FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer;
  AFehlertext: string = '');
{-----------------------------------------------------------------------------------------}
{ Schreibt Eintrag in die Journal-Fehlertabelle mit Fehlergruppe, Fehlercode und
  freiem Fehlertext bei Stati, die einen Fehler beschreiben (> 0);
  Übergabe: Fehlergruppe
            Fehlercode
            Optional:
              Freier Fehlertext }
Begin
  if not FehlerGruppeCode_OK (AFehlergruppe, AFehlercode) then
    WriteJournalFehler (JournalId, AFehlergruppe, AFehlercode, AFehlertext, FStammDatabase);
End;

{----------------------------------------------------------------------------------------------------}
function TGPRSKonvManagerDSfG.Get_AutoVonBisZeit (InstanzId, Gruppe, Kanal: integer; Kanaltyp: string;
                                                  AktuZeitangaben: TZeitangaben;
                                                  var von: TDateTime; var bis: TDateTime): boolean;
{----------------------------------------------------------------------------------------------------}
{ Soll-Zeitraum ermitteln für GPRS-Datenempfang (kanalweise bei Archiven bzw. für
  jedes Logbuch);
  Übergabe: InstanzId
            Gruppe (Archivgruppe bzw. Logbuchnummer)
            Kanal (Archivkanal bzw. -1 bei Logbuch)
            Kanaltyp
            aktuelle Zeitangaben aus Station
  Rückgabe: von-Zeitpunkt, bis-Zeitpunkt
  Ergebnis: true, wenn Automatikdaten bereits vorhanden sind }
var
  TbDArchiv: TTbDArchiv;
  MeldungenDb: TMeldungenDb;
  Ok: boolean;
  dummy: integer;
  vgl: TDateTime;

begin
  if Kanal > 0 then begin
    { Abruf_von für Archivkanal ermitteln: }
    TbDArchiv:=TTbDArchiv.Create (FAutoDatabase, RufStammdaten.StationId);
    try
      Ok:=TbDArchiv.GetLetztWertOrdNr_DatumZeit (InstanzId, Gruppe, Kanal, Kanaltyp,
                                                 dummy, von)
    finally
      TbDArchiv.Free;
    end;
  end
  else begin
    { Abruf_von für Logbuch ermitteln: }
    MeldungenDb:=TMeldungenDb.Create (FStammDatabase);
    try
      Ok:=MeldungenDb.GetLetztDSfGMeldungOrdNr_DatumZeit (C_BenutzerAuto, InstanzId, Gruppe,
                                                          dummy, von);
      { von-Zeitpunkt anpassen, damit bei Übergang von Sommer- auf Winterzeit nicht
        allein Logbucheinträge aus der zweiten 2-Uhr-Stunde abgerufen werden !
        -> wichtig für Zeitzonen-Berechnung in der Logbuch-Konvertierung }
      if Ok AND (AktuZeitangaben.LetztVerstZZ > 0) AND
         (UpperCase (AktuZeitangaben.Zeitzone) = CMEZ) then begin
        if Int (von) = Int (AktuZeitangaben.LetztVerstZZ) then begin             { gleiches Datum }
          vgl:=Int (AktuZeitangaben.LetztVerstZZ) + EncodeTime (4, 0, 0, 0);     { vor 4 Uhr }
          if CmpDateTime (von, vgl) < 0 then
            von:=Int (von);     { ganzen Tag abrufen }
        end;
      end;
    finally
      MeldungenDb.Free;
    end;
  end;

  if not Ok then                                 { noch keine Daten vorhanden }
    von:=-1;                                     { Dummy-Rückgabewert für GPRS-Daten }
  bis:=Now;                                      { Abruf bis PC-Datum }
  Result:=Ok;
end;

{--------------------------------------------}
procedure TGPRSKonvManagerDSfG.KonvertierenDB;
{--------------------------------------------}
var
  DSfGSystemdaten: TDSfGSystemDaten;
  DArchivLogbuchKonvDB: TDArchivLogbuchKonvDB;

begin
  DSfGSystemdaten:=GetDSfGAbrufSystemDaten (PathServer.PathName [WNetProgDir]);

  { Archive/Logbücher ins Auto-Archiv konvertieren: }
  DArchivLogbuchKonvDB:=TDArchivLogbuchKonvDB.Create (FAutoDatabase, FStammDatabase,
                                      PathServer.PathName [AsciiDir],
                                      PathServer.PathName [WNetProgDir],
                                      C_BenutzerAuto, RufStammdaten.StationId,
                                      DSfGSystemdaten.GrLogbuchPuffer);
  try
    if KonvListeArchive.Count > 0 then begin
      { Konvertierungsliste der Archive abarbeiten (Quelldateien im XML-Format): }
      DArchivLogbuchKonvDB.Konvertiere (KonvListeArchive, FAktuZeitangaben, JournalId, ZwFileLoeschen);
    end;

    if KonvListeLogbuecher.Count > 0 then begin
      { Konvertierungsliste der Logbücher abarbeiten: }
      DArchivLogbuchKonvDB.Konvertiere (KonvListeLogbuecher, FAktuZeitangaben, JournalId, ZwFileLoeschen);
    end;
  finally
    DArchivLogbuchKonvDB.Free;
  end;

  { nach dem Konvertieren Benachrichtigungen versenden: }
  Benachrichtigen;
end;

{---------------------------------------------}
procedure TGPRSKonvManagerDSfG.Benachrichtigen;
{---------------------------------------------}
{ Benachrichtigungen versenden }
var
  WMsgRegList: TObjectList;
  WMessageReg: TWMessageReg;
  bDebug: boolean;

begin
  bDebug:=true;  // Flag für Debugging der Wieser-Benachrichtigung, 04.08.2006 WW

  WMsgRegList:=TObjectList.Create (true);
  try
    WMessageReg:=TWMessageReg.Create (PathServer.PathName [WNetProgDir]);
    try
      { alle für "Archivdaten abgerufen" registrierten Nachrichten-Empfänger in Liste laden: }
      WMessageReg.GetMsgRegistrationList (wmt_ArchivDatenAbgerufen, WMsgRegList);
      { Benachrichtigung "Archivkanäle abgerufen": 04.08.2006, WW }
      SendWieserMsg_ArchivDatenAbgerufen_DSfG_AK (WMsgRegList,
        PathServer.PathName [WNetProgDir], WMsgADAList_AK, bDebug);
      { Benachrichtigung "Logbücher abgerufen": 04.08.2006, WW }
      SendWieserMsg_ArchivDatenAbgerufen_DSfG_LB (WMsgRegList,
        PathServer.PathName [WNetProgDir], WMsgADAList_LB, bDebug);
    finally
      WMessageReg.Free;
    end;
  finally
    WMsgRegList.Free;
  end;
end;

{---------------------------------------------------}
procedure TGPRSKonvManagerDSfG.JournalReorganisieren;
{---------------------------------------------------}
var
  JournalDB: TJournalDB;
begin
  JournalDB:=TJournalDB.Create (FStammDatabase);
  try
    { nur Zeitbereiche für die letzten 5 Abrufe dieser Station im Journal
      speichern, sonst wird DJZBDAT.DB zu groß: }
{$ifdef GAMESS}
    JournalDB.ReorganizeByStationsabrufe (C_GerArtDSfG, RufStammDaten.StationId, 164);
{$ELSE}
    JournalDB.ReorganizeByStationsabrufe (C_GerArtDSfG, RufStammDaten.StationId, 5);
{$ENDIF}
  finally
    JournalDB.Free;
  end;
end;


{------------------------------------------------------------------------------------------}
function TGPRSKonvManagerDSfG.KonvertToDB (ArLbDatenListe: TDSfGDataList;
                                           GPRSKennung: string; Datentyp: integer): boolean;
{------------------------------------------------------------------------------------------}
{ GPRS-Telegrammdaten (File mit TDSfGData-Struktur) in Archiv-/Logbuch-DB konvertieren;
  Übergabe: Archiv/Logbuch-Datenliste
            Kennung aus GPRS-Datentelegramm
            Datentyp
  Ergebnis: true, wenn ok }
var
  KonvListObj: TKonvListObj;
  InstanzData: TInstanzData;
  AbrufListe: TAbrufList;
  ZA_Dummy: TZeitangaben;
  AutoDaten_da: boolean;
  Soll_von, Soll_bis: TDateTime;
  gefunden: boolean;
  i, l: integer;
  c: char;
  DEL: string;
  AG: integer;
  AK: integer;
  Quell_EAdr: string;
  EAdr: string;
  Fehlergruppe: integer;
  Fehlercode: integer;
  FileName: string;
  sJrnFehler: string;

Begin
  Result:=false;

  { Zeitangaben-Record mit Null-Werten als Dummy: }
  ZA_Dummy.EAdr:=NUL;
  ZA_Dummy.DatumZeit:=0;
  ZA_Dummy.Zeitzone:='';
  ZA_Dummy.LetztVerstZZ:=0;
  ZA_Dummy.vom_PC:=false;

  if Datentyp = C_IsArchive then begin
    { es sind Archivdaten enthalten }
    AbrufListe:=TAbrufList.Create;
    try
      { Abrufliste mit allen Archivkanal-Stammsätzen der Station füllen:
        -> liefert die für die Konvertierung der GPRS-Daten nötigen Stammdaten-Infos }
      DSfGStammDaten.GetAutoDatenAbrufliste (RufStammDaten.StationId,
         C_IsArchive, Abrufliste, false);

      for l:=0 to ArLbDatenListe.Count-1 do begin  // für alle ArLb-Zwischendateien
        EAdr:=TDSfGDataListObj (ArLbDatenListe.Objects[l]).EAdr;
        DEL:=TDSfGDataListObj (ArLbDatenListe.Objects[l]).DEL;  // DE-Adresse des Archivkanals

        if l = 0 then begin
          // nur einmal: EAdr und Zeitangaben-Record gilt für alle in der ArLbDatenliste
          // enthaltenen Daten
          if not DSfGStammdaten.GetInstanzData_Adresse (RufStammDaten.StationId,
                                                        EAdr, InstanzData) then begin
            // Instanz-Stammsatz zu Busadresse nicht gefunden
            sJrnFehler:=Format (SBusAdr_n, [EAdr]);  // 17.07.2012, WW
            FehlerGruppeCodeUpdate (ST_STAMMERROR, SMERR_DSFGINST_NOTFOUND, sJrnFehler);
            exit;
          end;

          // aus Datei mit TDSfGSatzData-Struktur Zeitangaben-Record bilden zur
          // Übergabe an Standard-DSfG-Datenkonvertierung:
          CalcZeitangabenFromSatzDataFile(ArLbDatenListe[l], EAdr, FAktuZeitAngaben);
        end;

        // aus Datei mit TDSfGSatzData-Struktur eine Datei mit DSfG-Protokollstruktur
        // erzeugen zur Übergabe an Standard-DSfG-Datenkonvertierung:
        FileName:=ConvertSatzDataFileToTelegramFile (ArLbDatenListe[l], EAdr, DEL, Datentyp);

        if length (DEL) < 4 then begin  // DE-Adresse ungültig
          sJrnFehler:=Format (SBusAdr_n, [EAdr]);  // 17.07.2012, WW
          FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_ARCHIVKONV, sJrnFehler);
          Continue;
        end;
        AG:=Ord (DEL [3]) - C_INT_DELGRUPPEOFFSET;  // Archivgruppen-Nummer
        AK:=Ord (DEL [4]) - C_INT_DELKANALOFFSET;   // Archivkanal-Nummer

        gefunden:=false;
        for i:=0 to AbrufListe.Count-1 do begin
          if (TAbrufListObj (AbrufListe [i]).InstanzId = InstanzData.InstanzId) AND
             (TAbrufListObj (AbrufListe [i]).Gruppe = AG) AND
             (TAbrufListObj (AbrufListe [i]).Kanal = AK) then begin

            { Journal: Soll-Datenbereich (Zeitbereich) }
            AutoDaten_da:=Get_AutoVonBisZeit (InstanzData.InstanzId, AG, AK,
                                              TAbrufListObj (AbrufListe [i]).Kanaltyp,
                                              ZA_Dummy, Soll_von, Soll_bis);
            if AutoDaten_da then
              Soll_von:=Soll_von + EncodeTime (0, 0, 1, 0)         { plus 1 s }
            else
              Soll_von:=-1;  // kann bei GPRS-Daten nicht ermittelt werden
            WriteDSfGDatenzeitbereich_Soll (JournalId, InstanzData.InstanzId, AG, AK,
                                            Soll_von, Soll_bis, false, FStammDatabase);

            { zu den GPRS-Daten gehörenden Archivkanal-Stammsatz gefunden: Eintrag in
              Konvertierungsliste }
            KonvListObj:=TKonvListObj.Create;
            KonvListObj.SetData (TAbrufListObj (AbrufListe [i]).InstanzId,
                                 TAbrufListObj (AbrufListe [i]).Gruppe,
                                 TAbrufListObj (AbrufListe [i]).Kanal,
                                 TAbrufListObj (AbrufListe [i]).Kanaltyp,
                                 TAbrufListObj (AbrufListe [i]).Werteart,
                                 ZA_Dummy, -1, -1);
            KonvListeArchive.AddObject (FileName, KonvListObj);
            gefunden:=true;
            Break;
          end;
        end;  { for i }

        if gefunden then begin
          Fehlergruppe:=0;  // OK
          Fehlercode:=0;
        end
        else begin  // Archivkanal-Stammsatz nicht gefunden
          Fehlergruppe:=ST_STAMMERROR;
          Fehlercode:=SMERR_DSFGAK_NOTFOUND;
          sJrnFehler:=Format (SBusAdr_n_AGn_AKn, [EAdr, AG, AK]);  // 17.07.2012, WW
          FehlerGruppeCodeUpdate (Fehlergruppe, Fehlercode, sJrnFehler);
        end;

        { Archivkanal in ADA-Benachrichtigungsliste eintragen (OK bzw. Fehler): }
        if Assigned (WMsgADAList_AK) then
          WMsgADAList_AK.Add_WMsgData (RufStammDaten.StationId, Instanzdata.InstanzId,
                                       AG, AK, Fehlergruppe, Fehlercode,
                                       CWMsg_CommType_GPRS);
      end;  { for l }
    finally
      AbrufListe.Free;
    end;
  end

  else if Datentyp = C_IsLogbuecher then begin
    { es sind Logbuchdaten enthalten }
    AbrufListe:=TAbrufList.Create;
    try
      { Abrufliste mit allen Logbuch-Stammsätzen der Station füllen:
        -> liefert die für die Konvertierung der GPRS-Daten nötigen Stammdaten-Infos }
      DSfGStammDaten.GetAutoDatenAbrufliste (RufStammDaten.StationId,
         C_IsLogbuecher, Abrufliste, false);

      for l:=0 to ArLbDatenListe.Count-1 do begin  // für alle ArLb-Zwischendateien
        EAdr:=TDSfGDataListObj (ArLbDatenListe.Objects[l]).EAdr;
        DEL:=TDSfGDataListObj (ArLbDatenListe.Objects[l]).DEL;  // DE-Adresse des Logbuchs

        if l = 0 then begin
          // nur einmal: EAdr und Zeitangaben-Record gilt für alle in der ArLbDatenliste
          // enthaltenen Daten
          if not DSfGStammdaten.GetInstanzData_Adresse (RufStammDaten.StationId,
                                                        EAdr, InstanzData) then begin
            // Instanz-Stammsatz zu Busadresse nicht gefunden
            sJrnFehler:=Format (SBusAdr_n, [EAdr]);  // 17.07.2012, WW
            FehlerGruppeCodeUpdate (ST_STAMMERROR, SMERR_DSFGINST_NOTFOUND, sJrnFehler);
            exit;
          end;

          // aus Datei mit TDSfGSatzData-Struktur Zeitangaben-Record bilden zur
          // Übergabe an Standard-DSfG-Datenkonvertierung:
          CalcZeitangabenFromSatzDataFile(ArLbDatenListe[l], EAdr, FAktuZeitAngaben);
        end;

        // aus Datei mit TDSfGSatzData-Struktur eine Datei mit DSfG-Protokollstruktur
        // erzeugen zur Übergabe an Standard-DSfG-Datenkonvertierung:
        FileName:=ConvertSatzDataFileToTelegramFile (ArLbDatenListe[l], EAdr, DEL, Datentyp);

        // Quell-Busadresse aus DE-Adresse des Logbuchs ermitteln
        gefunden:=false;
        for c:=Low (CLogbuchDEL) to High (CLogbuchDEL) do begin
          if (CLogbuchDEL [c] + 'd') = DEL then begin
            Quell_EAdr:=c;
            gefunden:=true;
            Break;
          end;
        end;
        if not gefunden then begin  // DE-Adresse ungültig
          sJrnFehler:=Format (SBusAdr_n, [EAdr]);  // 17.07.2012, WW
          FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_LOGBKONV, sJrnFehler);
          Continue;
        end;

        gefunden:=false;
        for i:=0 to AbrufListe.Count-1 do begin
          if (TAbrufListObj (AbrufListe [i]).InstanzId = InstanzData.InstanzId) AND
             (TAbrufListObj (AbrufListe [i]).BusAdr_Quelle = Quell_EAdr) then begin

            { Journal: Soll-Datenbereich (Zeitbereich) }
            AutoDaten_da:=Get_AutoVonBisZeit (InstanzData.InstanzId,
                                              TAbrufListObj (AbrufListe [i]).Gruppe, -1,
                                              TAbrufListObj (AbrufListe [i]).Kanaltyp,
                                              ZA_Dummy, Soll_von, Soll_bis);
            if AutoDaten_da then
              Soll_von:=Soll_von + EncodeTime (0, 0, 1, 0)         { plus 1 s }
            else
              Soll_von:=-1;  // kann bei GPRS-Daten nicht ermittelt werden
            WriteDSfGDatenzeitbereich_Soll (JournalId, InstanzData.InstanzId,
                                            TAbrufListObj (AbrufListe [i]).Gruppe, -1,
                                            Soll_von, Soll_bis, false, FStammDatabase);

            { zu den GPRS-Daten gehörenden Logbuch-Stammsatz gefunden: Eintrag in
              Konvertierungsliste }
            KonvListObj:=TKonvListObj.Create;
            KonvListObj.SetData (TAbrufListObj (AbrufListe [i]).InstanzId,
                                 TAbrufListObj (AbrufListe [i]).Gruppe,
                                 -1, '', '', ZA_Dummy,
                                 TAbrufListObj (AbrufListe [i]).InstanzId_Quelle,
                                 TAbrufListObj (AbrufListe [i]).GerTypNr_Quelle);
            KonvListeLogbuecher.AddObject (FileName, KonvListObj);

            { Logbuch in ADA-Benachrichtigungsliste eintragen (OK): }
            if Assigned (WMsgADAList_LB) then
              WMsgADAList_LB.Add_WMsgData (RufStammDaten.StationId,
                                           TAbrufListObj (AbrufListe [i]).InstanzId,
                                           TAbrufListObj (AbrufListe [i]).Gruppe,
                                           0, 0, CWMsg_CommType_GPRS);

            gefunden:=true;
            Break;
          end;
        end;  { for i }

        if not gefunden then  // Logbuch-Stammsatz nicht gefunden
          sJrnFehler:=Format (SBusAdr_n_LBAdr_n, [EAdr, Quell_EAdr]);  // 17.07.2012, WW
          FehlerGruppeCodeUpdate (ST_STAMMERROR, SMERR_DSFGLB_NOTFOUND, sJrnFehler);
        { -> wenn der zum Logbuch gehörende Stammsatz nicht gefunden wird, kann
             keine Benachrichtigung versendet werden, da die Logbuchnummer ja
             nicht bekannt ist ! }
      end;  { for l }
    finally
      AbrufListe.Free;
    end;
  end;

  // in DB konvertieren:
  KonvertierenDB;

  Result:=true;
End;

{-------------------------------------------------------------------------------}
function TGPRSKonvManagerDSfG.Konvert (ArLbDatenListe: TDSfGDataList;
                                       GPRSKennung: string; Datentyp: integer;
                                       GPRSFehlergruppe, GPRSFehlercode: integer;
                                       GPRSTelegrammRohdaten: string;
                                       GPRSTelegrammNr: integer;
                                       bAusgabeKurzzeitwerte: boolean): boolean;
{-------------------------------------------------------------------------------}
{ GPRS-Telegrammdaten (File mit TDSfGData-Struktur) in Zieldaten (Archiv-/Logbuch-DB,
  Ascii-File) konvertieren;
  -> Archiv-/Logbuchdaten werden in DSfG-Archiv/Logbuch-DB konvertiert
  -> Kurzzeitwerte werden in ASCII-Datei abgelegt zur weiteren Verarbeitung
     durch IEC-Kopplung
  Übergabe: Archiv/Logbuch-Datenliste
            Kennung aus GPRS-Datentelegramm
            Datentyp
            Fehlergruppe/-code aus GPRS-Datenempfang
            Telegramm-Rohdaten (Info für Logfile)
            Flag 'bAusgabeKurzzeitwerte' (true: Kurzzeitwert-Dateien werden erzeugt)
  Ergebnis: true, wenn ok }
var
  KonvDB: boolean;
  KonvAscii: boolean;
  Kennung_in_Stammdaten_eindeutig: boolean;
  l: integer;

begin
  try
    Result:=true;

    KonvDB:=(Datentyp = C_IsArchive) OR (Datentyp = C_IsLogbuecher);
    KonvAscii:=(Datentyp = C_IsKurzzeitwerte) AND bAusgabeKurzzeitwerte;

    if KonvDB then begin
      { Journal-Eintrag für GPRS-Datenempfang (Station bislang nicht identifiziert,
        StationId = 0: }
      JournalId:=WriteNewJournal (C_GerArtDSfG, 0, '', GPRSKennung, '', C_AbrArtDatenEmpf,
                                  Datentyp, COMNr, FStammDatabase);

      { Fehlergruppe/-code aus GPRS-Datenempfang in Journal schreiben: }
      if not FehlerGruppeCode_OK (GPRSFehlergruppe, GPRSFehlercode) then begin
        FehlerGruppeCodeUpdate (GPRSFehlergruppe, GPRSFehlercode);
        Result:=false;
        exit;
      end;
    end;

    { Stammsatz für GPRS-Kennung suchen: }
    if not DSfGStammdaten.GetRufStammdatenByKennung (GPRSKennung,
                                                     RufStammdaten,
                                                     Kennung_in_Stammdaten_eindeutig) then begin
      if KonvDB then
        FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_KEINSTAMMSATZ);
      if KonvAscii then
        WriteGPRSKonvErrorLog (EST_KENNUNGCHECK, KENNERR_KEINSTAMMSATZ, GPRSKennung,
                               GPRSTelegrammRohdaten, FLogPath);
      Result:=false;
      exit;
    end;

    { ab jetzt sind Stammdaten zu den empfangenen GPRS-Daten bekannt. Die Kennung
      kann in den Stammdaten jedoch mehrfach vorhanden sein ! }

    { bei nicht eindeutiger Kennung in den Stammdaten
      -> Journal: Station unbekannt, Warnung "Kennung existiert mehrfach"
      -> keine Konvertierung der empfangenen GPRS-Daten, da diese einer
         Station nicht eindeutig zugeordnet werden könnten) }
    if Kennung_in_Stammdaten_eindeutig then begin
      if KonvDB then
        { -> Stations-Kennung wurde schon in Journal geschrieben; 14.06.2011, WW }
        UpdateJournalStation (JournalId, RufStammDaten.StationId, RufStammDaten.Stationsname,
                              '', RufStammDaten.Geraetetyp, FStammDatabase)  // 23.05.2011, WW
    end
    else begin
      if KonvDB then
        FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_MEHRFACH_NODATA);
      if KonvAscii then
        WriteGPRSKonvErrorLog (EST_KENNUNGCHECK, KENNERR_MEHRFACH_NODATA, GPRSKennung,
                               GPRSTelegrammRohdaten, FLogPath);
      Result:=false;
      exit;
    end;

    // in Archiv-/Logbuch-DB konvertieren:
    if KonvDB then
      if not KonvertToDB (ArLbDatenListe, GPRSKennung, Datentyp) then
        Result:=false;

    // in Ascii-File konvertieren:
    if KonvAscii then
      if not KonvertDSfG_KZW (ArLbDatenListe, GPRSKennung, GPRSTelegrammRohdaten,
                              GPRSTelegrammNr, FAusgabeDirList, FLogPath) then
        Result:=false;
  finally
    // SatzData-Dateien wurden konvertiert, können jetzt gelöscht werden: 31.01.2007, WW
    for l:=0 to ArLbDatenListe.Count-1 do  // für alle ArLb-Zwischendateien
      if ZwFileLoeschen then
        DeleteFile (ArLbDatenListe[l]);
  end;
end;

end.


