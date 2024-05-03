{******************************************************************************}
{* Unit: Objekt für DSfG-Abruf                                                *}
{* 18.11.1999 WW                                                              *}
{* 13.02.2001 WW NetReset für Rohdateizugriff (Gamess !)                      *}
{* 02.08.2004 GD Zustand wird bei Verbindungsauf-/-abbau immer geschrieben    *}
{* 23.02.2005 GD Instanzname jetzt aus Tabelle                                *}
{******************************************************************************}
Unit DAbruf;

INTERFACE

uses
  Windows, Classes, SysUtils, DSysDat, SerDSfG, PathIni, AbrfInfo, DZustand, ErrConst,
  T_Tools, T_Zeit, WSysCon, JournlDB, DJournal, DDBAbruf, DDbSta, WStrUtils, DSfGUtil,
  ModemIni, DSysCon, DListen, UnixDT, TbDSfGAr, DDELList, DD_Allg, DDLoesch,
  TbDSfGMo, DB_Attn, TbDSfGDE, SrvCfgIni, WChars, DMoInit, novell, DValidAnswer,
  DDfueParaList, TbDSfGMomDfue, TbDSfGDfuePe, DDfuParaNr, DAufmTelegr, RufeDB,
  MeldungenDb, WComm, TCPIP_DSfG, Lizenz32, my_utils, wtables;

Const
  { Datenanfragemodus }

  d_NeueOrdNr = 1;  { Abfrage neuer Daten über Ordnungsnummer (Standard-Automatikabfrage) }
  d_NeueZeit  = 2;  { Abfrage neuer Daten über Zeitbereich (optional) }
  d_VonBis    = 3;  { Abfrage über Zeitbereich (manuell) }
  d_OrdNr     = 4;  { Abfrage über Ordnungsnummer (manuell) }
  d_DelAdr    = 5;  { Abfrage über DEL-Adresse }

Type
  { Record mit typabhängiger Instanzanzahl }

  TInstanzCount = record
    Dfu: byte;
    Gas: byte;
    Rev: byte;
    Prot: byte;
    Reg: byte;
    Strg: byte;
    Umw: byte;
    Wieser: byte;
    Odor: byte;
    Unbest: byte;
  end;

  { Record mit Versionsdaten der DSfG-DFÜ }

  TDfueVersionsdaten = record
    Hersteller: string;
    ProgName: string;
    Version: string;
    Adresse: array [1..5] of string [1];    { 14.11.2001 WW, erweitert auf 5 Adressen }
  end;

  { Allgemeines Objekt zum DSfG-Datenabruf }

  TAbruf = class (TObject)
  private
    Abrufart: TAbrufart;
    Stop_bei_falscher_Kennung: boolean;  { wenn true, erfolgt Verbindungsabbau, falls die
                                           DSfG-DFÜ-Kennung nicht mit der Stammdaten-Kennung
                                           übereinstimmt }
    Fehlergruppe: integer;                            { Fehlergruppe serielle Kommunikation }
    Fehlercode: integer;        { Fehlercode serielle Kommunikation, wenn Fehlergruppe <> 0 }
    DSfGKennung: string;          { aus DSfG gelesene Kennung, nicht die aus den Stammdaten }
    Hersteller: char;
    Extensionmode: byte;                 { der von der DSfG-DFÜ bevorzugte Erweiterungsgrad }
    TID: integer;
    DELList: TDELList;
    DfueVersionsdaten: TDfueVersionsdaten;
    KonfRohdatenFileName: string;
    Stammdaten: TDSfGStammdaten;
    SourceChanged_Warnung: boolean;  { wenn true, wurde im Abruf bereits eine Warnung
                                       DCH_ORDNRFOLGE ins Jounral geschrieben }
    CommComponent: TComponent;
    function SendCommand (ABefehl: string; AEndezeichen: char; AEndezeichenAnzahl: integer;
                          ATimeout: integer; AAnswerDest: TAnswerDest;
                          var ARueckgabe: TRueckgabe; var ANoCarrier: boolean): boolean;
    Procedure FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer);
    function GetAbrufartString: string;
    procedure IncrementTID;
    function Get_AutoVonBisOrdNr (InstanzId, Gruppe, Kanal: integer; Kanaltyp: string;
                                  Fuellstand_bis: integer; AktuZeitangaben: TZeitangaben;
                                  var OrdNr_von: integer; var OrdNr_bis: integer;
                                  var Letzt_Tab_OrdNr: integer;
                                  var Letzt_Tab_DatumZeit: TDateTime): boolean;
    function Get_AutoVonBisZeit (InstanzId, Gruppe, Kanal: integer; Kanaltyp: string;
                                 AktuZeitangaben: TZeitangaben;
                                 var von: TDateTime; var bis: TDateTime): boolean;
    function Get_ArchivBefehl(EAdr: char; Gruppe, Kanal: integer; Modus: byte; von, bis: TDateTime): string;
    function Get_LogbuchBefehl (EAdr: char; QuellAdr: char; Modus: byte; von, bis: TDateTime): string;
    function Get_DatenelementeBefehl(EAdr: char; Del_von, Del_bis: string): string;
    function Get_EinstellBefehl(EAdr: char; Del: string; Wert: string;
                                Zugangscode1, Zugangscode2: string): string;
    function Get_DfueEinstellbefehl (Befehlscode: string; ParaAdr: string; Wert: string): string;
    function GetBefehlFromBinaerfile (FileName: TFileName; var Befehl: string): boolean;
    function F_IsWieserBefehl (Befehl: string): boolean;
    function CheckRohfile_Antworttelegramm (Filename: TFileName; Receiver: char;
                                            Anfragemodus: byte;
                                            Check_First_OrdNr_DatumZeit: boolean;
                                            Letzt_Tab_OrdNr: integer;
                                            Letzt_Tab_DZ: TDateTime;
                                            var alles_da: boolean;
                                            var DELAdr: string;
                                            var Letzt: TDateTime;
                                            var OrdNr_DatumZeit_Cmp_OK: boolean): integer;
    function CheckRohstring_Antworttelegramm (Rohstring: string; Receiver: char; var alles_da: boolean;
                           var DELAdr: string): integer;
    function DSfG_Datenelement_geaendert (Receiver: char; DELAdr: string; WertNeuSoll: string;
                                          Rohstring: string; var WertNeuIst: string): boolean;
    function CheckModemConnect (ModemAntwort: string): boolean;
    function KennungAbfragen: boolean;
    function PasswortLogin (var Keine_weiteren_Versuche: boolean): boolean;
    function GetAktiveBusteilnehmer: boolean;
    function GetDfueVersionsdaten: boolean;
    function GetDfueAdressen: boolean;
    function DfueTransparentSchalten: boolean;
    function GetDfueKommandos: boolean;
    procedure InitInstanzCount (var InstanzCount: TInstanzCount);
    procedure AddKonfiguration_LoginDfue (InstanzListe: TInstanzDataList; InstDfuListe: TInstDfuDataList;
                                          var InstanzCount: TInstanzCount);
    function AbrufKonfiguration_Allgemein (EAdr: char; InstanzListe: TInstanzDataList;
                                            var InstanzCount: TInstanzCount; var nicht_erreicht: boolean;
                                            var Valid: boolean): boolean;
    function AbrufKonfiguration_RegInst (EAdr: char; KonfigRohdataListe: TKonfigRohdataList): boolean;
    function Check_DfueStandardparameterAktivierung: boolean;
    procedure AktuStammdatenByDfueParameter (Befehl: string; ParaAdr: string; Wert: string);
    function RufAnnehmen_Modem: boolean;
    procedure RufDeaktivierung;
    function AbrufArchivFuellstand_bis (EAdr: char; Gruppe: integer;
                                        var Fuellstand_bis: integer): integer;
    function AbrufLogbuchFuellstand_bis (EAdr: char; Adr_Quelle: char;
                                         var Fuellstand_bis: integer): integer;
  public
    COMPort: integer;
    JournalId: integer;
    EAdr_Dfue: char;
    EAdr_nichterreicht: string;                  { für Konfiguration einlesen }
    EAdr_Telegrammsender: string;   { für Logbuchabfrage bei Rufentgegennahme }
    Teilnehmer: string;
    RufStammDaten : TRufStammDaten;       { Record mit Stammdaten für den Verbindungsaufbau }
    WithZustandTabelle: boolean;  { true, wenn Zustände in Zustandstabelle mitprotokolliert
                                    werden sollen, sonst nur Ausgabe in Statuszeile }
    NoCarrier: boolean;                               { true, wenn keine Verbindung besteht }
    Kennung_in_Stammdaten_eindeutig: boolean;          { für Rufentgegennahme }
    Dfue_Kommandos: string;         { von der DSfG-DFÜ unterstützte Kommandos }
    Constructor Create (ACommComponent: TComponent; AAbrufart: TAbrufart);
    Destructor  Destroy; override;
    function VerbAufbau (StationId: integer; Datentypen: integer; Transparentmodus: boolean;
                         var Keine_weiteren_Versuche: boolean): boolean;
    function VerbAbbau: boolean;
    function AbrufDaten (Modus: byte; von, bis: TDateTime; Datentyp: integer; AbrufListe: TAbrufList;
                         KonvListe: TKonvList; AktuZeitangaben: TZeitangaben): boolean;
    function AbrufZeitangaben (EAdr: char; var Zeitangaben: TZeitangaben): boolean;
    function AbrufKonfiguration (InstanzListe: TInstanzDataList; InstDfuListe: TInstDfuDataList;
                                 KonfigRohdataListe: TKonfigRohdataList): boolean;
    function AbrufMomentanwerte (AbrufListe: TAbrufList; nur_ein_DE_abrufen: boolean;
                                 var Keine_weiteren_Journaleintraege: boolean): boolean;
    procedure UebertragungDatenelemente;
    function AbrufDfueMomentanwerte (AktuStammdaten: boolean): boolean;
    function DfueVerbindungHalten: boolean;
    procedure UebertragungDfueParameter;
    function AbrufBinaerdatei: boolean;
    function RufAnnahme: boolean;
    function RufReaktivierung: boolean;
  end;

IMPLEMENTATION

uses
  FMain;

{ TAbruf }

{-------------------------------------------------------------------------}
Constructor TAbruf.Create (ACommComponent: TComponent; AAbrufart: TAbrufart);
{-------------------------------------------------------------------------}
var
  i: integer;
Begin
  inherited Create;
  CommComponent:=ACommComponent;
  Abrufart:=AAbrufart;

  if ACommComponent is TSerialDSfG then
    COMPort := TSerialDSfG (ACommComponent).COMPort
  else
    COMPort:= TComponent(ACommComponent).Tag;         { Pseudo-COM-Nummer für TCP/IP }

  DELList:=TDELList.Create;
  Stammdaten:=TDSfGStammdaten.Create (PathServer.PathName [WStammDir]);
  Stammdaten.InitTabellen;

  { Kennungsprüfung bei Verbindungsaufbau: ja/nein }
  case Abrufart of
    aa_manuell,
    aa_konflesen,
    aa_momentan,
    aa_dfue_momentan: Stop_bei_falscher_Kennung:=false;
  else
    Stop_bei_falscher_Kennung:=true;
  end;

  { Protokollierung in Zustandtabelle ein-/ausschalten: }
  WithZustandTabelle:=(Abrufart <> aa_momentan) AND
                      (Abrufart <> aa_dfue_momentan) AND
                      (Abrufart <> aa_ruf);

  { Initialisieren der Fehlerstati }
  Fehlergruppe:=0;
  Fehlercode:=0;

  JournalId:=0;
  DSfGKennung:='';
  NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }

  EAdr_Dfue:=NUL;
  EAdr_nichterreicht:='';       { die nicht erreichten Busadressen vorbelegen }
  EAdr_Telegrammsender:='';     { die Busadressen vorbelegen, die spontane Telegramme
                                  nach dem Transparentschalten gesendet haben }
  Hersteller:= NUL;
  Teilnehmer:='';
  Extensionmode:=0;

  with DfueVersionsdaten do begin
    Hersteller:='';
    ProgName:='';
    Version:='';
    for i:=Low (Adresse) to High (Adresse) do
      Adresse [i]:='';
  end;
  Dfue_Kommandos:='';

  KonfRohdatenFileName:='';
  Kennung_in_Stammdaten_eindeutig:=true;               { Vorbelegung: OK-Fall }
  SourceChanged_Warnung:=false;  { Vorbelegung: noch keine Warnung DCH_ORDNRFOLGE im Abruf }

  TID:=0;
End;

{------------------------}
Destructor TAbruf.Destroy;
{------------------------}
Begin
  ZustandMessage (COMPort, -1, z_Bereit, '', false);
  KennungMessage ('');
  StationMessage ('');
  Stammdaten.Free;
  DELList.Free;
  Inherited Destroy;
End;

{--------------------------------------------------------------------------------------------}
function TAbruf.SendCommand (ABefehl: string; AEndezeichen: char; AEndezeichenAnzahl: integer;
                             ATimeout: integer; AAnswerDest: TAnswerDest;
                             var ARueckgabe: TRueckgabe; var ANoCarrier: boolean): boolean;
{--------------------------------------------------------------------------------------------}
{ ruft die SendCommand-Methode der für den Abruf benutzten Kommunikations-Komponente auf }
begin
  if CommComponent is TSerialDSfG then
    Result:=TSerialDSfG (CommComponent).SendCommand (ABefehl, AEndezeichen, AEndezeichenAnzahl,
                                                         ATimeout, AAnswerDest, ARueckgabe, ANoCarrier)
  else
    Result:=TClientSocketDSfG (CommComponent).SendCommand (ABefehl, AEndezeichen, AEndezeichenAnzahl,
                                                               ATimeout, AAnswerDest, ARueckgabe, ANoCarrier);
end;

{----------------------------------------------------------------------------}
Procedure TAbruf.FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer);
{----------------------------------------------------------------------------}
{ setzt Fehlergruppe und Fehlercode; bei Stati, die einen Fehler beschreiben (> 0), erfolgt
  ein Eintrag in die Journal-Fehlertabelle;
  Übergabe: AFehlergruppe
            AFehlercode }
Begin
  Fehlergruppe := AFehlergruppe;
  Fehlercode := AFehlercode;

  { DCH_ORDNRFOLGE-Warnung nur einmal je Abruf schreiben: }
  if (Fehlergruppe = ST_DATACHECK) AND (Fehlercode = DCH_ORDNRFOLGE) AND
    SourceChanged_Warnung then exit;

  if Fehlergruppe > 0 then
    WriteJournalFehler (JournalId, Fehlergruppe, Fehlercode);

  if (Fehlergruppe = ST_DATACHECK) AND (Fehlercode = DCH_ORDNRFOLGE) then
    SourceChanged_Warnung:=true;
End;

{----------------------------------------}
function TAbruf.GetAbrufartString: string;
{----------------------------------------}
Begin
  case Abrufart of
    aa_automatisch:   Result:=C_AbrArtAuto;
    aa_manuell:       Result:=C_AbrArtManu;
    aa_konflesen:     Result:=C_AbrArtKonfLesen;
    aa_ruf:           Result:=C_AbrArtRuf;
    aa_momentan:      Result:=C_AbrArtMomStart;
    aa_dfue_momentan: Result:=C_AbrArtMomDfueStart;
    aa_rufreakt:      Result:=C_AbrArtRufReakt;
  else
    Result:='unbekannt';
  end;
End;

{----------------------------}
procedure TAbruf.IncrementTID;
{----------------------------}
{ TID inkrementieren (max. 4-stellig lt. G485, Anhang A, Tabelle 2) }
begin
  inc (TID);
  if TID > 9999 then
    TID:=1;
end;

{------------------------------------------------------------------------------------------}
function TAbruf.Get_AutoVonBisOrdNr (InstanzId, Gruppe, Kanal: integer; Kanaltyp: string;
                                     Fuellstand_bis: integer; AktuZeitangaben: TZeitangaben;
                                     var OrdNr_von: integer;
                                     var OrdNr_bis: integer;
                                     var Letzt_Tab_OrdNr: integer;
                                     var Letzt_Tab_DatumZeit: TDateTime): boolean;
{------------------------------------------------------------------------------------------}
{ Ordnungsnummernbereich für Automatikabruf über Ordnungsnummernkriterium ermitteln
  (kanalweise bei Archiven bzw. für jedes Logbuch);
  Übergabe: InstanzId
            Gruppe (Archivgruppe bzw. Logbuchnummer)
            Kanal (Archivkanal bzw. -1 bei Logbuch)
            Kanaltyp
            Füllstand bis Ordnungsnummer der Archivgruppe bzw. des Logbuchs
  Rückgabe: OrdNr_von, OrdNr_bis (ermittelter Ordnungsnummer-Bereich für Abfragebefehl)
            Letzt_Tab_OrdNr (Ordnungsnummer des letzten Tabelleneintrags)
            Letzt_Tab_DatumZeit (Datum/Zeit des letzten Tabelleneintrags)
  Ergebnis: true, wenn Automatikdaten bereits vorhanden sind }
var
  TbDArchiv: TTbDArchiv;
  MeldungenDb: TMeldungenDb;
  Ok: boolean;
  vgl: TDateTime;
  Vortag_Tab_OrdNr: integer;
  Vortag_Tab_DatumZeit: TDateTime;

begin
  { Vorbelegungen, falls noch keine Tabellendaten vorhanden sind: }
  Letzt_Tab_OrdNr:=-1;
  Letzt_Tab_DatumZeit:=-1;

  if Kanal > 0 then begin
    { OrdNr_von für Archivkanal ermitteln: }
    TbDArchiv:=TTbDArchiv.Create (PathServer.PathName [DArchivDir], RufStammdaten.StationId);
    try
      Ok:=TbDArchiv.GetLetztWertOrdNr_DatumZeit (InstanzId, Gruppe, Kanal, Kanaltyp,
                                                 Letzt_Tab_OrdNr, Letzt_Tab_DatumZeit)
    finally
      TbDArchiv.Free;
    end;
  end
  else begin
    { OrdNr_von für Logbuch ermitteln: }
    MeldungenDb:=TMeldungenDb.Create (PathServer.PathName [WStammDir]);
    try
      Ok:=MeldungenDb.GetLetztDSfGMeldungOrdNr_DatumZeit (C_BenutzerAuto, InstanzId, Gruppe,
                                                          Letzt_Tab_OrdNr, Letzt_Tab_DatumZeit);
    finally
      MeldungenDb.Free;
    end;
  end;

  OrdNr_von:=Letzt_Tab_OrdNr;
  if Ok then begin                    { Daten bereits in Tabelle vorhanden }
    if (OrdNr_von > Fuellstand_bis) AND (Fuellstand_bis > 0) then begin
      { Füllstand bis im Archiv höher als im Gerät:
        Alles auslesen ! Warnung, Gerät getauscht ! }
      OrdNr_von:=1;
      FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_ORDNRFOLGE);
    end
    else begin
      { Logbücher: von-Ordnungsnummer anpassen, damit bei Übergang von Sommer- auf Winterzeit nicht
        allein Logbucheinträge aus der zweiten 2-Uhr-Stunde abgerufen werden !
        -> wichtig für Zeitzonen-Berechnung in der Logbuch-Konvertierung }
      if (Kanal <= 0) AND (AktuZeitangaben.LetztVerstZZ > 0) AND
         (UpperCase (AktuZeitangaben.Zeitzone) = CMEZ) then begin
        if Int (Letzt_Tab_DatumZeit) = Int (AktuZeitangaben.LetztVerstZZ) then begin   { gleiches Datum }
          vgl:=Int (AktuZeitangaben.LetztVerstZZ) + EncodeTime (4, 0, 0, 0);           { vor 4 Uhr }
          if CmpDateTime (Letzt_Tab_DatumZeit, vgl) < 0 then begin
            { OrdNr_von Logbuch:
              -> es wird die Ordnungsnummer des jüngsten Logbucheintrag vom Vortag
              des letzten in der Tabelle befindlichen Logbucheintrags ermittelt: }
            MeldungenDb:=TMeldungenDb.Create (PathServer.PathName [WStammDir]);
            try
              if MeldungenDb.GetLetztVortagDSfGMeldungOrdNr_DatumZeit (C_BenutzerAuto, InstanzId, Gruppe,
                                                                        Vortag_Tab_OrdNr, Vortag_Tab_DatumZeit) then
               OrdNr_von:=Vortag_Tab_OrdNr
              else begin   { kein Logbucheintrag vom Vortag des Sommer-/Winterzeit-Wechsels vorhanden }
                { Abruf nach Systemdaten-Einstellung: }
                OrdNr_von:=Fuellstand_bis - Systemdaten.AnzahlErsteDaten + 1;
                if OrdNr_von < 1 then
                  OrdNr_von:=1;
              end;
            finally
              MeldungenDb.Free;
            end;
          end;
        end;
      end;
    end;
  end
  else begin                          { noch keine Daten in Tabelle vorhanden }
    OrdNr_von:=Fuellstand_bis - Systemdaten.AnzahlErsteDaten + 1;
    if OrdNr_von < 1 then
      OrdNr_von:=1;
  end;
  OrdNr_bis:=Fuellstand_bis;                     { Abruf bis "Füllstand bis"  }
  Result:=Ok;
end;

{--------------------------------------------------------------------------------------}
function TAbruf.Get_AutoVonBisZeit (InstanzId, Gruppe, Kanal: integer; Kanaltyp: string;
                                    AktuZeitangaben: TZeitangaben;
                                    var von: TDateTime; var bis: TDateTime): boolean;
{--------------------------------------------------------------------------------------}
{ Abrufzeitraum ermitteln für Automatikabruf über Zeitkriterium (kanalweise bei
  Archiven bzw. für jedes Logbuch);
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
    TbDArchiv:=TTbDArchiv.Create (PathServer.PathName [DArchivDir], RufStammdaten.StationId);
    try
      Ok:=TbDArchiv.GetLetztWertOrdNr_DatumZeit (InstanzId, Gruppe, Kanal, Kanaltyp,
                                                 dummy, von)
    finally
      TbDArchiv.Free;
    end;
  end
  else begin
    { Abruf_von für Logbuch ermitteln: }
    MeldungenDb:=TMeldungenDb.Create (PathServer.PathName [WStammDir]);
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
    von:=trunc(now) - AutomatikErsteDatenTage;   { Einstellung aus Ini-File }
  bis:=Now;                                      { Abruf bis PC-Datum }
  Result:=Ok;
end;

{-------------------------------------------------------------------------------------------------------------}
function TAbruf.Get_ArchivBefehl(EAdr: char; Gruppe, Kanal: integer; Modus: byte; von, bis: TDateTime): string;
{-------------------------------------------------------------------------------------------------------------}
{ Befehl zum Auslesen eines Archivkanals bilden
  -> von, bis enthält je nach Modus Zeit oder Ordnungsnummer }
Const
  DID = '255';
  BLO = '1';
  BLR = '1';
  NTY = 'A';
  DFO = 'J';
  ZAE = '2';

var
  DEL: string;
  Daten: string;
  DEB: char;
  Unix_vonStr, Unix_bisStr: string;
  OrdNr_vonStr, OrdNr_bisStr: string;

begin
  { Vorbelegungen: }
  Unix_vonStr:='';
  Unix_bisStr:='';
  OrdNr_vonStr:='';
  OrdNr_bisStr:='';

  if (Modus = d_OrdNr) OR (Modus = d_NeueOrdNr) then begin
    DEB:='O';
    { von-Ordnungsnummer, bis-Ordnungsnummer in String wandeln: }
    OrdNr_vonStr:=US + IntToStr (Trunc (von));
    OrdNr_bisStr:=US + IntToStr (Trunc (bis));
  end
  else begin
    DEB:='Z';
    { von-Zeit, bis-Zeit in Unix-Format wandeln: }
    DateTimeToUnixTimeStr (von, Unix_vonStr);
    DateTimeToUnixTimeStr (bis, Unix_bisStr);

    // Abruf über Zeit beim MRG 910 mit Status im Bis-Teil des Kommando wegen
    // Fehler im MRG (falsche Ordnungsnummern-Bildung in den gelieferten Daten !)
    // WW, 22.03.2004
    if (Hersteller = 'W') AND (DfueVersionsdaten.ProgName = 'MRG910') then
      Unix_bisStr:=Unix_bisStr + US + US + '1';
  end;

  DEL:='ca'+Chr(96+Gruppe)+Chr(101+Kanal)+'d';
  Daten:=DEL+US+US+Unix_vonStr+OrdNr_vonStr+GS+DEL+US+US+Unix_bisStr+OrdNr_bisStr;
  Result:=EAdr+US+DID+US+IntToStr(TID)+US+BLO+US+BLR+US+EAdr_Dfue+US+NTY+US+DFO+US+DEB+US+ZAE+US+Daten+FS;
end;

{-------------------------------------------------------------------------------------------------------}
function TAbruf.Get_LogbuchBefehl (EAdr: char; QuellAdr: char; Modus: byte; von, bis: TDateTime): string;
{-------------------------------------------------------------------------------------------------------}
{ Befehl zum Auslesen eines Logbuches bilden
  -> von, bis enthält je nach Modus Zeit oder Ordnungsnummer }
Const
  DID = '255';
  BLO = '1';
  BLR = '1';
  NTY = 'A';
  DFO = 'J';
  ZAE = '2';

var
  DEL: string;
  Daten: string;
  DEB: char;
  Unix_vonStr, Unix_bisStr: string;
  OrdNr_vonStr, OrdNr_bisStr: string;

begin
  { Vorbelegungen: }
  Unix_vonStr:='';
  Unix_bisStr:='';
  OrdNr_vonStr:='';
  OrdNr_bisStr:='';

  if (Modus = d_OrdNr) OR (Modus = d_NeueOrdNr) then begin
    DEB:='O';
    { von-Ordnungsnummer, bis-Ordnungsnummer in String wandeln: }
    OrdNr_vonStr:=US+IntToStr (Trunc (von));
    OrdNr_bisStr:=US+IntToStr (Trunc (bis));
  end
  else begin
    DEB:='Z';
    { von-Zeit, bis-Zeit in Unix-Format wandeln: }
    DateTimeToUnixTimeStr (von, Unix_vonStr);
    DateTimeToUnixTimeStr (bis, Unix_bisStr);
  end;

  if (QuellAdr >= Low (CLogbuchDEL)) AND (QuellAdr <= High (CLogbuchDEL)) then
    DEL:=CLogbuchDEL [QuellAdr]+'d'
  else
    DEL:='';
  Daten:=DEL+US+US+Unix_vonStr+OrdNr_vonStr+GS+DEL+US+US+Unix_bisStr+OrdNr_bisStr;
  Result:=EAdr+US+DID+US+IntToStr(TID)+US+BLO+US+BLR+US+EAdr_Dfue+US+NTY+US+DFO+US+DEB+US+ZAE+US+Daten+FS;
end;

{------------------------------------------------------------------------------------}
function TAbruf.Get_DatenelementeBefehl(EAdr: char; Del_von, Del_bis: string): string;
{------------------------------------------------------------------------------------}
{ Befehl zum Auslesen von Datenelementen bilden (einzelne oder Bereich) }
Const
  DID = '255';
  BLO = '1';
  BLR = '1';
  NTY = 'A';
  DFO = 'J';

var
  Daten: string;
  DEB: char;
  ZAE: integer;

begin
  if Del_bis <> '' then begin                    { Bereich von Datenelementen }
    DEB:='V';
    ZAE:=2;
    Daten:=Del_von+GS+Del_bis;
  end
  else begin                          { ein oder mehrere Einzel-Datenelemente }
    DEB:='M';
    ZAE:=F_TotalChars (Del_von, GS) + 1;
    Daten:=Del_von;
  end;
  Result:=EAdr+US+DID+US+IntToStr(TID)+US+BLO+US+BLR+US+EAdr_Dfue+US+NTY+US+DFO+US+DEB+US+IntToStr(ZAE)+US+Daten+FS;
end;

{-----------------------------------------------------------------------------}
function TAbruf.Get_EinstellBefehl(EAdr: char; Del: string; Wert: string;
                                   Zugangscode1, Zugangscode2: string): string;
{-----------------------------------------------------------------------------}
{ Befehl zum Einstellen eines Datenelements bilden (nur 1 DE je Telegramm möglich lt. A. Weber) }
Const
  DID = '255';
  BLO = '1';
  BLR = '1';
  NTY = 'E';
  DFO = 'J';

var
  Daten: string;
  DEB: char;
  ZAE: integer;
  ZC1, ZC2: string;

begin
  DEB:='M';
  ZAE:=3;

  { Vorbelegungen: }
  ZC1:='';
  ZC2:='';

  if Zugangscode1 <> '' then
    ZC1:=US+Zugangscode1;
  if Zugangscode2 <> '' then
    ZC2:=US+Zugangscode2;

  Daten:=CD_ALLG_Zugangscode1+ZC1+GS+CD_ALLG_Zugangscode2+ZC2+GS+Del+US+Wert;
  Result:=EAdr+US+DID+US+IntToStr(TID)+US+BLO+US+BLR+US+EAdr_Dfue+US+NTY+US+DFO+US+DEB+US+IntToStr(ZAE)+US+Daten+FS;
end;

{--------------------------------------------------------------------------------------------------}
function TAbruf.Get_DfueEinstellbefehl (Befehlscode: string; ParaAdr: string; Wert: string): string;
{--------------------------------------------------------------------------------------------------}
{ Befehl zum Einstellen eines Parameters oder einer NTY-Maske bilden }
begin
  { Befehlscode für NTY-Maske einstellen: ParaAdr = Busadresse (DNO); Wert = NTY-Maske }
  if Befehlscode = 'A' then
    Result:=STX+Befehlscode+Wert+'/'+ParaAdr+ETX

  { Befehlscodes für Einstellen aller übrigen Parameter (K-, E-, I-, R-Befehl, Wieser C-Befehl): }
  else begin
    if Befehlscode = 'YWB' then    { Wieser-Parameter werden mit dem YWC-Befehl geändert ! }
      Result:=STX+'YWC'+ParaAdr+Wert+ETX
    else                           { übrige Parameter: Befehl ohne Parameter-Adresse }
      Result:=STX+Befehlscode+Wert+ETX;
  end;
end;

{---------------------------------------------------------}
function TAbruf.F_IsWieserBefehl (Befehl: string): boolean;
{---------------------------------------------------------}
{ Ergebnis: true, wenn übergebener Befehl ein Wieser-spezifischer Befehl ist
            (z.B. DSfG-DFÜ-Befehl) }
begin
  Result:=Pos ('YW', Befehl) > 0;  { YW? -> hersteller-spezifischer Befehl "Wieser" }
end;

{-------------------------------------------------------------------------------------------}
function TAbruf.CheckRohfile_Antworttelegramm (Filename: TFileName; Receiver: char;
                                               Anfragemodus: byte;
                                               Check_First_OrdNr_DatumZeit: boolean;
                                               Letzt_Tab_OrdNr: integer;
                                               Letzt_Tab_DZ: TDateTime;
                                               var alles_da: boolean;
                                               var DELAdr: string;
                                               var Letzt: TDateTime;
                                               var OrdNr_DatumZeit_Cmp_OK: boolean): integer;
{-------------------------------------------------------------------------------------------}
{ - untersucht ein Rohdatenfile mit DSfG-Antworttelegramm auf Vollständigkeit
  - überprüft die Absenderadresse
  - ermittelt bei unvollständiger Antwort je nach Anfragemodus (über DEL-Adresse
    o. Datum/Zeit o. Ordnungsnummer) die DEL-Adresse, den Unixzeiteintrag oder
    die Ordnungsnummer des letzten Datenelements (Rückgabe von Zeit bzw.
    Ordnungsnummer in Letzt)
  - führt bei Automatikabruf über Ordnungsnummer und gesetztem
    Check_First_OrdNr_DatumZeit-Flag Ordnungsnummer-und Zeitstempelvergleich durch
    (Rückgabe in OrdNr_DatumZeit_Cmp_OK) }
var
  FStream: TFileStream;
  S: string;
  zeich: char;
  i, USCount, code: integer;
  DID, ZAE: integer;
  sNTY, sDZ: string;
  OrdNr: integer;
  DZ: TDateTime;

begin
  alles_da := false;                                  { Antwort unvollständig }
  DELAdr:='';
  Letzt := 0;
  OrdNr_DatumZeit_Cmp_OK:=true;

  if not FileExists (FileName) then begin
    Result:=DSFGKONVERR_FILEACCESS;
    exit;
  end;

  try
    FStream:=TFileStream.Create (FileName, fmOpenRead OR fmShareDenyWrite);
    try
      S := read_hdcl(FStream);             { alles bis zum ersten US (keine Auswertung) }
      S := read_hdcl(FStream);                                                    { DID }
      Val (S, DID, code);
      if (DID < C_DID_DataSimple) OR (S = '') then begin
        Result := DSFGKONVERR_HEADER;
        exit;
      end;

      read_hdcl(FStream);                                                         { TID }
      read_hdcl(FStream);                                                         { BLO }
      read_hdcl(FStream);                                                         { BNR }

      S := read_hdcl(FStream);                                                    { DNO }
      if S <> Receiver then begin                      { Überprüfen des Absenders }
        Result := DSFGKONVERR_DATA;
        exit;
      end;

      sNTY := read_hdcl(FStream);                                                 { NTY }
      if (sNTY <> 'R') AND (sNTY <> 'U') then begin
        Result := DSFGKONVERR_HEADER;                { nur R und U sind erlaubt ! }
        exit;
      end;

      read_hdcl(FStream);                                                         { DFO }
      S := read_hdcl(FStream);                                                    { DEB }
      if S <> '' then begin
         { außerplanmäßige Antwort: 20.07.2004, WW
           -> OK, weitere Rohdaten-Prüfung beendet
           -> Auswertung erfolgt in entsprechender Daten-Konvertierung }
        if S[1] = 'A' then begin
          alles_da := true;
          Result:=DSFGKONVERR_OK;
          exit;
        end;
      end
      else begin
        Result:=DSFGKONVERR_HEADER;
        exit;
      end;

      S := read_hdcl(FStream);                                                    { ZAE }
      val(S, ZAE, Code);                       { Anzahl der Datenelemente im File }
      if Code <> 0 then begin
        Result := DSFGKONVERR_HEADER;
        exit;
      end;

      { Test, ob im HDCL noch weitere Felder vorhanden sind (keine Auswertung): }
      if (DID AND C_PAS) <> 0 then
        read_hdcl(FStream);                                                       { PAS }
      if (DID AND C_DTY) <> 0 then
        read_hdcl(FStream);                                                       { DTY }
      if (DID AND C_ABS) <> 0 then
        read_hdcl(FStream);                                                       { ABS }
      if (DID AND C_EMF) <> 0 then
        read_hdcl(FStream);                                                       { EMF }
      if (DID AND C_TDA) <> 0 then
        read_hdcl(FStream);                                                       { TDA }
      if (DID AND C_TTI) <> 0 then
        read_hdcl(FStream);                                                       { TTI }
      if (DID AND C_PTB) <> 0 then
        read_hdcl(FStream);                                                       { PTB }

      { Sicherheitsüberprüfung bei Automatik-Abruf über Ordnungsnummer: }
      if (Anfragemodus = d_NeueOrdNr) AND Check_First_OrdNr_DatumZeit AND
         (Letzt_Tab_OrdNr > 0) AND (ZAE > 0) then begin
        { wenn der erste Datensatz im Rohfile die in Letzt_Tab_OrdNr übergebene
          Ordnungsnummer trägt: Datensatz-Zeitstempel mit Letzt_Tab_DZ vergleichen }
        read_hdcl(FStream);                                      { DE-Adresse überlesen }
        read_hdcl(FStream);                                            { Wert überlesen }
        sDZ := read_hdcl(FStream);                                { Datum/Zeit auslesen }
        S := read_hdcl(FStream);                              { Ordnungsnummer auslesen }
        try
          Val (S, OrdNr, code);
          if OrdNr = Letzt_Tab_OrdNr then begin
            try
              UnixTimeStrToDateTime (sDZ, DZ);
              OrdNr_DatumZeit_Cmp_OK:=CmpDateTime (DZ, Letzt_Tab_DZ) = 0;
            except
            end;
          end;
        except
        end;
      end;

      if sNTY = 'R' then begin                { vollständige Antwort: OK und raus }
        alles_da := true;
        Result := DSFGKONVERR_OK;
        exit;
      end;

      { bei unvollständiger Antwort dem Anfragemodus entsprechenden Teil des letzten
        Datensatzes ermitteln: }
      FStream.Seek (-1, soFromEnd);           { auf letztes Zeichen positionieren }
      if FStream.Read (zeich, 1) < 1 then begin
        Result := DSFGKONVERR_FILEACCESS;
        exit;
      end;

      { an den Beginn des letzten Datensatzes positionieren (bei NTY = U kann davon
        ausgegangen werden, daß mind. 1 Datensatz im Rohfile enthalten ist. Daher
        einfach rücklesen bis zum GS): }
      while zeich <> GS do begin
        FStream.Seek (-2, soFromCurrent);
        if FStream.Read (zeich, 1) < 1 then begin
          Result := DSFGKONVERR_FILEACCESS;
          exit;
        end;
      end;

      { an den Beginn des zurückzugebenden Datensatzteils positionieren: }
      case Anfragemodus of
        d_OrdNr,
        d_NeueOrdNr: USCount:=3;
        d_DelAdr:    USCount:=0;
      else
        USCount:=2;                                { bei Anfrage über Zeitbereich }
      end;
      i := 0;
      while i < USCount do begin
        if FStream.Read (zeich, 1) < 1 then begin
          Result := DSFGKONVERR_FILEACCESS;
          exit;
        end;
        if zeich = US then inc(i);
      end;  { End while }

      S := read_hdcl(FStream);         { DEL-Adresse, Zeit oder Ordnungsnummer auslesen }

      case Anfragemodus of
        d_OrdNr,
        d_NeueOrdNr: Letzt:=StrToInt (S);
        d_DelAdr:    DELAdr:=S;
      else                                         { bei Anfrage über Zeitbereich }
        try
          UnixTimeStrToDateTime (S, Letzt);
        except
          Letzt:=0;          { mögliche Formatfehler abfangen }
          Result := DSFGKONVERR_INVTIME;
          exit;
        end;
      end;
      Result := DSFGKONVERR_OK;
    finally
      FStream.Free;
    end;
  except
    Result:=DSFGKONVERR_FILEACCESS;
  end;
end;

{---------------------------------------------------------------------------------------------------}
function TAbruf.CheckRohstring_Antworttelegramm (Rohstring: string; Receiver: char;
                                                 var alles_da: boolean; var DELAdr: string): integer;
{---------------------------------------------------------------------------------------------------}
{ untersucht einen Rohdatenstring mit DSfG-Antworttelegramm auf Vollständigkeit, überprüft die Absenderadresse,
  ermittelt bei unvollständiger Antwort die DEL-Adresse des letzten Datenelements (Rückgabe in DELAdr) }
var
  S: string;
  zeich: char;
  code: integer;
  StringPos: integer;
  DID: integer;
  Ausserplanmaessige_Antwort: boolean;

begin
  alles_da := false;
  DELAdr:='';

  StringPos:=1;
  S := read_hdcl_fromString(Rohstring, StringPos);         { alles bis zum ersten US (keine Auswertung) }

  { HDCL auswerten: }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DID }
  val(S, DID, Code);
  if Code <> 0 then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;
  if DID < C_DID_DataSimple then begin
    Result := DSFGKONVERR_HEADER;
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { TID }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BLO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BNR }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DNO }
  if S <> Receiver then begin                      { Überprüfen des Absenders }
    Result := DSFGKONVERR_DATA;
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { NTY }
  if S <> '' then begin
    if S[1] = 'R' then begin                          { Antwort vollständig }
      alles_da := true;
    end
    else begin
      alles_da:=false;                                { Antwort unvollständig }
      if S[1] <> 'U' then begin
        Result := DSFGKONVERR_HEADER;
        exit;
      end;
    end;  { End if S = R then }
  end
  else begin
    Result:=DSFGKONVERR_HEADER;
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { DFO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DEB }
  Ausserplanmaessige_Antwort:=S = 'A';  // 20.07.2004, WW
  S := read_hdcl_fromString(Rohstring, StringPos);                      { ZAE }

  { Test, ob im HDCL noch weitere Felder vorhanden sind (keine Auswertung): }
  if (DID AND C_PAS) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { PAS }
  if (DID AND C_DTY) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { DTY }
  if (DID AND C_ABS) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { ABS }
  if (DID AND C_EMF) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { EMF }
  if (DID AND C_TDA) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { TDA }
  if (DID AND C_TTI) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { TTI }
  if (DID AND C_PTB) <> 0 then
    S := read_hdcl_fromString(Rohstring, StringPos);                    { PTB }

  { außerplanmäßige Antwort: Datenteil lesen und prüfen, 20.07.2004, WW }
  if Ausserplanmaessige_Antwort then begin
    S := read_hdcl_fromString(Rohstring, StringPos);
    if S = '?' then
      Result:=DSFGKONVERR_AA_NICHT_PLAUSIBEL
    else if S = '*' then
      Result:=DSFGKONVERR_AA_BESTAETIGUNG
    else if S = '!' then
      Result:=DSFGKONVERR_AA_KEINE_BERECHTIGUNG
    else if S = '#' then
      Result:=DSFGKONVERR_AA_UNBEKANNT
    else if S = ':' then
      Result:=DSFGKONVERR_AA_NICHT_BEHAND
    else
      Result:=DSFGKONVERR_AA_SONSTIGE;
    exit;
  end;

  if alles_da then begin
    Result := DSFGKONVERR_OK;
    exit;
  end;

  { bei unvollständiger Antwort letztes Datenelement ermitteln: }
  StringPos:=length(Rohstring);
  zeich:=Rohstring [StringPos];
  while zeich <> GS do begin              { an den Beginn des letzten Datensatzes positionieren (rücklesen bis zum GS) }
    dec (StringPos);
    zeich:=Rohstring [StringPos];
  end;
  inc (StringPos);
  S := read_hdcl_fromString(Rohstring, StringPos);              { DEL-Adresse }

  DELAdr:=S;
  Result := DSFGKONVERR_OK;
end;

{-----------------------------------------------------------------------------------------------}
function TAbruf.DSfG_Datenelement_geaendert (Receiver: char; DELAdr: string; WertNeuSoll: string;
                                             Rohstring: string; var WertNeuIst: string): boolean;
{-----------------------------------------------------------------------------------------------}
{ Antwort auf Einstelltelegramm auswerten;
  Übergabe: Busadresse der Instanz, deren DE geändert werden soll
            zu ändernde Datenelement-Adresse
            im Einstelltelegramm gesendeter neuer Datenelement-Wert (Soll)
            Antwort-Rohstring
  Rückgabe: tatsächlicher Datenelement-Wert nach der Einstellung (Ist)
  Ergebnis: true, wenn Datenelement geändert wurde }
var
  Code: Integer;
  S: string;
  DID: integer;
  StringPos: integer;
  ZAE: integer;
  sbuf, zbuf: PChar;
  Len: Word;
  DELAdr_Antwort: string;

begin
  Result:=false;
  WertNeuIst:='';                                { Vorbelegung für WertNeuIst }

  StringPos:=1;
  S := read_hdcl_fromString(Rohstring, StringPos);         { alles bis zum ersten US (keine Auswertung) }

  { HDCL auswerten: }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DID }
  val(S, DID, Code);
  if Code <> 0 then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;
  if DID < C_DID_DataSimple then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { TID }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BLO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { BNR }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DNO }
  if S <> Receiver then begin                      { Überprüfen des Absenders }
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { NTY }
  if S <> '' then begin
    if S[1] <> 'R' then begin
      FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
      exit;
    end;
  end
  else begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { DFO }
  S := read_hdcl_fromString(Rohstring, StringPos);                      { DEB }
  if S <> '' then begin
    if S[1] = 'A' then begin  { außerplanmäßige Antwort: Zugangscode(s) falsch o. nicht vorhandenes Datenelement }
      FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGCODENR_UNKNOWNDE);
      exit;
    end;
    if S[1] <> 'M' then begin
      FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
      exit;
    end;
  end
  else begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  S := read_hdcl_fromString(Rohstring, StringPos);                      { ZAE }
  val(S, ZAE, Code);                       { Anzahl der Datenelemente im File }
  if Code <> 0 then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;
  if ZAE <> 3 then begin     { Zugriffscode 1 (add), Zugriffscode 2 (ade), zu änderndes DE }
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  { Datenfeld aufsplitten: Datenelement-Adresse und Wert }
  Len := Length (Rohstring) + 1;
  getmem(sbuf,Len);
  try
    getmem(zbuf,Len);
    try
      StrPCopy(sbuf,Rohstring);
      If FilterString (zbuf, sbuf, GS, FS, nil, 1) <> Nil Then Begin  { Einstell-DE-Adresse/-Wert zwischen 2. GS und FS }
        StrCopy (sbuf, zbuf);            { Source ist jetzt der DE-Teilstring }
        If FilterString (zbuf, sbuf, nil, US, nil, 0) <> Nil Then      { DEL-Adresse }
          DELAdr_Antwort:=string(zbuf)
        else
          DELAdr_Antwort:='';
        If FilterString (zbuf, sbuf, US, nil, nil, 0) <> Nil Then      { Datenelement-Wert }
          WertNeuIst:=string(zbuf)
        else
          WertNeuIst:='';
      end
      else begin
        FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
        exit;
      end;
    finally
      freemem(zbuf,Len);
    end;
  finally
    freemem(sbuf,Len);
  end;

  { zu ändernde Datenelement-Adresse in Antwort prüfen: }
  if DELAdr_Antwort <> DelAdr then begin
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_WRONGANSWER);
    exit;
  end;

  { neuen Datenelement-Wert prüfen, ob er wirklich geändert wurde }
  if WertNeuSoll <> WertNeuIst then begin             { Datenelement nicht veränderbar o. Eichschalter geschlossen }
    FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_DSFG_NOCHANGEABLEDE_NOES);
    exit;
  end;
  Result:=true;
end;

{----------------------------------------------------------------}
function TAbruf.CheckModemConnect (ModemAntwort: string): boolean;
{----------------------------------------------------------------}
{ Modemantwort auf Verbindungsaufbau-Befehl bzw. Rufannahme-Befehl prüfen;
  Ergebnis: true, wenn CONNECT erfolgt ist.
  Falls kein CONNECT erfolgt ist, werden Fehlergruppe und Fehlercode gesetzt }
var
  S: string;
begin
  { für Auswertung der Modemantwort: Leerzeichen raus und in Großbuchstaben wandeln }
  S:=AnsiUpperCase (StrFilter (ModemAntwort, ' '));

  { Verbindung steht }
  if Pos('CONNECT', S) <> 0 then begin
    Result:=true;
    exit;
  end;

  Result:=false;
  { besetzt }
  if Pos('BUSY', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_BUSY);
    exit;
  end;

  { kein Freizeichen }
  if Pos('NODIALTONE', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_NODIALTONE);
    exit;
  end;

  { kein Antwortton }
  if Pos('NOANSWER', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_NOANSWER);
    exit;
  end;

  { kein Träger vorhanden }
  if Pos('NOCARRIER', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_NOCARRIER);
    exit;
  end;

  { Rufverzögerung }
  if Pos('DELAYED', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_DELAYED);
    exit;
  end;

  { Wählfunktion gesperrt }
  if Pos('DIALLOCKED', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_DIALLOCKED);
    exit;
  end;

  { Abbruch }
  if Pos('ABORT', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_ABORT);
    exit;
  end;

  { Error }
  if Pos('ERROR', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_ERROR);
    exit;
  end;

  { Ring }
  if Pos('RING', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_RING);
    exit;
  end;

  { OK (das ist an dieser Stelle wirklich ein Fehler !) }
  if Pos('OK', S) <> 0 then begin
    FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_OK);
    exit;
  end;

  { Sonstiges }
  FehlerGruppeCodeUpdate (COM_MODEMERROR, CME_SONST);
end;

{---------------------------------------}
function TAbruf.KennungAbfragen: boolean;
{---------------------------------------}
{ Kennung der DSfG-DFÜ abfragen;
  Ergebnis: true, wenn Kennungsabfrage erfolgreich durchgeführt wurde }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_KennungAbfragen, '', WithZustandTabelle);

  Befehl:=STX+'K'+ETX;
  { Kennungsabfragekommando senden }
  if not SendCommand (Befehl, ETX, 1, Timeout_Login, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kennungsabfrage-Kommando auswerten: }
  if R.Antwort[2] <> 'K' then begin
    FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_WRONGANSWER);
    exit;
  end;

  { Kennungsprüfung: }
  DSfGKennung:=F_RightTrunc (Copy(R.Antwort,3,12), ' ');    { DSfGKennung enthält jetzt die richtige Gerätekennung }
  UpDateDSfGJournalKennung (JournalId, DSfGKennung);        { DSfG-Kennung in Journal eintragen }
  Result:=true;
end;

{----------------------------------------------------------------------------}
function TAbruf.PasswortLogin (var Keine_weiteren_Versuche: boolean): boolean;
{----------------------------------------------------------------------------}
{ Paßwort übertragen;
  Rückgabe: Keine_weiteren_Versuche
  Ergebnis: true, wenn PasswortLogin ok }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_PasswortUebertragen, '', WithZustandTabelle);

  Befehl:=STX+'I'+F_RightPad (RufStammdaten.Passwort,' ',szlen_DSfGPasswort)+ETX;
  { Paßwortübertragungskommando senden }
  if not SendCommand (Befehl, ETX, 1, Timeout_Login, ad_String, R, NoCarrier) then begin
    if NoCarrier then begin                     { wenn die Verbindung jetzt nicht meht steht, war das Passwort falsch ! }
      FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
      Keine_weiteren_Versuche:=true;
    end else
      FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Paßwortübertragungskommando auswerten: }
  if R.Antwort[2] <> 'I' then begin             { Passwort falsch ! }
    FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
    Keine_weiteren_Versuche:=true;
    exit;
  end;

  { Bin ich jetzt schon drin oder was ? }
  ZustandMessage (COMPort, RufStammDaten.StationId, z_LoginErfolgt, '', WithZustandTabelle);
  UpDateDSfGJournal (JournalId, C_WJournal_DZLoggedIn);              { Login erfolgt: Ich bin drin ! Das ist ja einfach }

  EAdr_Dfue:=R.Antwort[3];                         { lokale EADR der DSfG-DFÜ }
  Hersteller:=R.Antwort[4];                        { Hersteller }
  if R.Antwort[5] <> ETX then                      { Extensionmode }
    Extensionmode:=StrToInt (R.Antwort [5])
  else
    ExtensionMode:=0;

  if EAdr_Dfue = '0' then begin
    FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_NODFUEBUSADDRESS);
    Keine_weiteren_Versuche:=true;
    exit;
  end;
  Result:=true;
end;

{----------------------------------------------}
function TAbruf.GetAktiveBusteilnehmer: boolean;
{----------------------------------------------}
{ Aktive Busteilnehmer auslesen (nur bei Wieser-DSfG-DFÜ !);
  Ergebnis: true, wenn Lesen der Teilnehmer ok }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_AktiveBusteilnehmer, '', WithZustandTabelle);

  Befehl:=STX+'YWT'+ETX;
  { Aktive-Busteilnehmer-Kommando senden }
  if not SendCommand (Befehl, ETX, 1, Timeout_Login, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Aktive-Busteilnehmer-Kommando auswerten: }
  if R.Antwort[2] = 'T' then begin
    Teilnehmer:=Copy (R.Antwort, 3, length (R.Antwort)-3);
    if length (Teilnehmer) < 1 then begin          { keine aktiven Teilnehmer }
      { wenn kein DSfG-DFÜ-Momentanwertabruf gemacht wird, Abruf abbrechen: }
      if Abrufart <> aa_dfue_momentan then begin
        FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_NOACTIVEUSER);
        exit;
      end;
    end;
  end;
  Result:=true;
end;

{--------------------------------------------}
function TAbruf.GetDfueVersionsdaten: boolean;
{--------------------------------------------}
{ Versionsdaten der DSfG-DFÜ auslesen (optional im Erweiterungsgrad 1);
  Ergebnis: true, wenn Lesen der Versionsdaten ok }
var
  Befehl, S: string;
  R: TRueckgabe;
  sbuf, zbuf: PChar;
  Len : Word;

begin
  Result:=false;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_DFUEVersionsdaten, '', WithZustandTabelle);

  Befehl:=STX+'V'+ETX;
  { Versionsdaten-Kommando senden }
  if not SendCommand (Befehl, ETX, 1, Timeout_Login, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);        // 10.10.2003, WW
    exit;
  end;

  { Konfiguration einlesen ohne Rohdatenloeschen -> ausgelesene Rohdaten-Strings in Datei mitprotokollieren }
  if not RohdatenLoeschen then
    WriteRohfile (KonfRohdatenFileName, R.Antwort);

  { Antwort auf Versionsdaten-Kommando auswerten: }
  S:=US + Copy (R.Antwort, 3, length (R.Antwort));
  Len := Length (s) + 1;
  getmem(sbuf,Len);
  try
    getmem(zbuf,Len);
    try
      StrPCopy(sbuf,s);
      If FilterString (zbuf, sbuf, US, US, nil, 0) <> Nil Then   { Hersteller }
        DfueVersionsdaten.Hersteller := string(zbuf);
      If FilterString (zbuf, sbuf, US, US, nil, 1) <> Nil Then   { Programmname }
        DfueVersionsdaten.ProgName := string(zbuf);
      If FilterString (zbuf, sbuf, US, US, nil, 2) <> Nil Then   { Version }
        DfueVersionsdaten.Version := string(zbuf);
    finally
      freemem(zbuf,Len);
    end;
  finally
    freemem(sbuf,Len);
  end;
  Result:=true;
end;

{---------------------------------------}
function TAbruf.GetDfueAdressen: boolean;
{---------------------------------------}
{ alle in der DSfG-DFÜ eingestellten Adressen abfragen (nur Wieser);
  Ergebnis: true, wenn Lesen der Adressen ok }
var
  Befehl, S: string;
  R: TRueckgabe;
  sbuf, zbuf: PChar;
  Len : Word;

begin
  Result:=false;
  { geändert 14.11.2001 WW: Parameter 010..014 (jetzt alle 5 Adressen; bisher nur DFÜ-Adressen 1-4) }
  Befehl:=STX+'YWB010014'+ETX;                                    // 10.10.2003, WW
  { Versionsdaten-Kommando senden }
  if not SendCommand (Befehl, ETX, 1, Timeout_Login, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Konfiguration einlesen ohne Rohdatenloeschen -> ausgelesene Rohdaten-Strings in Datei mitprotokollieren }
  if not RohdatenLoeschen then
    WriteRohFile (KonfRohdatenFileName, R.Antwort);

  { Antwort auf Adressen-Kommando auswerten: }
  S:=US + Copy (R.Antwort, 3, length (R.Antwort));
  Len := Length (s) + 1;
  getmem(sbuf,Len);
  try
    getmem(zbuf,Len);
    try
      StrPCopy(sbuf,s);
      If FilterString (zbuf, sbuf, US, US, nil, 0) <> Nil Then        { Adresse 1 }
        if Copy (string(zbuf), 1, 3) = '010' then
          DfueVersionsdaten.Adresse [1]:= Copy (string(zbuf), 4, 1);
      If FilterString (zbuf, sbuf, US, US, nil, 1) <> Nil Then        { Adresse 2 }
        if Copy (string(zbuf), 1, 3) = '011' then
          DfueVersionsdaten.Adresse [2]:= Copy (string(zbuf), 4, 1);
      If FilterString (zbuf, sbuf, US, US, nil, 2) <> Nil Then        { Adresse 3 }
        if Copy (string(zbuf), 1, 3) = '012' then
          DfueVersionsdaten.Adresse [3]:= Copy (string(zbuf), 4, 1);
      If FilterString (zbuf, sbuf, US, US, nil, 3) <> Nil Then        { Adresse 4 }
        if Copy (string(zbuf), 1, 3) = '013' then
          DfueVersionsdaten.Adresse [4]:= Copy (string(zbuf), 4, 1);
      If FilterString (zbuf, sbuf, US, US, nil, 4) <> Nil Then        { Adresse 5 }
        if Copy (string(zbuf), 1, 3) = '014' then
          DfueVersionsdaten.Adresse [5]:= Copy (string(zbuf), 4, 1);
    finally
      freemem(zbuf,Len);
    end;
  finally
    freemem(sbuf,Len);
  end;
  Result:=true;
end;

{-----------------------------------------------}
function TAbruf.DfueTransparentSchalten: boolean;
{-----------------------------------------------}
{ DSfG-DFÜ transparent schalten und anstehende Telegramme auslesen;
  Ergebnis: true, wenn Transparentschalten ok }
var
  Befehl: string;
  R: TRueckgabe;
  Extensionmode_Abruf: byte;  { der Erweiterungsgrad mit dem tatsächlich abgerufen wird }
  Antwort: string;
  MaxExtensionMode: byte;

begin
  Result:=false;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_DFUETransparent, '', WithZustandTabelle);

  { wenn Modul den von der DSfG-DFÜ bevorzugten Erweiterungsgrad nicht bedienen kann:
    -> mit Erweiterungsgrad 0 abrufen, den muß jede DSfG-DFÜ beherrschen können }
  if CommComponent is TSerialDSfG then
    MaxExtensionMode:=TSerialDSfG (CommComponent).GetMaxExtensionmode
  else
    MaxExtensionMode:=TClientSocketDSfG (CommComponent).GetMaxExtensionmode;
  if ExtensionMode > MaxExtensionmode then begin
    ExtensionMode_Abruf:=0;
    Befehl:=STX+'T0'+ETX;
  end
  else begin
    ExtensionMode_Abruf:=Extensionmode;
    Befehl:=STX+'T'+ETX;
  end;

  { Transparentschalten-Kommando senden: Es werden die Antwort des Transparent-Befehls
    und evtl. spontan gesendete Aufmerksamkeits-Telegramme zusammen erwartet. SendCommand
    muß daher hier in einen Timeout laufen, um alle Aufmerksamkeits-Telegramme auslesen zu können ! }
  ZustandMessage (COMPort, RufStammDaten.StationId, z_TelegrammeLesen, '', WithZustandTabelle);
  SendCommand (Befehl, ETX, 100000, Timeout_DFUETransparent, ad_String, R, NoCarrier);
  if not (((R.Fehlergruppe = 0) AND (R.Fehlercode = 0)) OR
          ((R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT))) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);  { Fehler in Antwort }
    exit;
  end;

  { Antwortteil auf Transparent-Befehl analysieren: }
  if length (R.Antwort) = 0 then begin     { "echter" Timeout ist aufgetreten }
    FehlerGruppeCodeUpdate (COM_KOMMERROR, KOMMERR_TIMEOUT);
    exit;
  end;

  Antwort:=ExtractString (R.Antwort, STX, ETX, 0);
  if Pos ('?', Antwort) <> 0 then begin
    FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_NODFUEBUSADDRESS);   { lt. DSfG-Spezifikation }
    exit;
  end;

  { Extensionmode setzen, mit dem tatsächlich abgerufen wird und mit dem auch schon eventuelle
    Aufmerksamkeits-Telegramme gesendet wurden: }
  if CommComponent is TSerialDSfG then
    TSerialDSfG (CommComponent).SetExtensionMode (Extensionmode_Abruf)
  else
    TClientSocketDSfG (CommComponent).SetExtensionMode (Extensionmode_Abruf);

  { Telegramme in Telegramm-Journaltabelle abspeichern: }
  Antwort:=ExtractString (R.Antwort, ETX, NUL, 0);  { Antwortteil mit den Aufmerksamkeits-Telegrammen }
  if not SaveAufmerksamkeitsTelegramme (JournalId, Antwort, Extensionmode_Abruf,
                                        EAdr_Telegrammsender) then
    FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_AUFMTELEGRAMMKONV);

  Result:=true;
end;

{----------------------------------------}
function TAbruf.GetDfueKommandos: boolean;
{----------------------------------------}
{ von der DSfG-DFÜ unterstützte Kommandos abfragen und nach "DfueKommandos" schreiben;
  Ergebnis: true, wenn DFÜ-Komandos abgefragt werden konnten }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  Befehl:='?';
  if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
    Dfue_Kommandos:=Copy (R.Antwort, 3, length (R.Antwort)-3);
  Result:=true;
end;

{---------------------------------------------------------------------------------------------}
function TAbruf.VerbAufbau (StationId: integer; Datentypen: integer; Transparentmodus: boolean;
                            var Keine_weiteren_Versuche: boolean): boolean;
{---------------------------------------------------------------------------------------------}
{ Aufbau einer Verbindung mit DSfG-Station;
  Übergabe: StationId
            Datentypen (für Journal)
            Transparentmodus = true, wenn DSfG-DFÜ transparent geschaltet werden soll
  Rückgabe: Keine_weiteren_Versuche = true, wenn kein Verbindungsaufbau aufgrund falscher
                                      Kennung oder Paßwort
  Ergebnis: true, wenn kompletter Verbindungsaufbau incl. Kennungsvergleich und Login erfolgreich
            (Flag "NoCarrier" gibt bei fehlerhaftem Verbindungsaufbau Auskunft, ob die Verbindung noch steht oder nicht) }

Var
  Befehl: string;
  R: TRueckgabe;
  RufNrStr: string;
  VerbStr: string;
  IPAdr: string;
  PortId: integer;
  AFehlergruppe: integer;
  AFehlercode: integer;
  S: string;
  GerTypNr: integer;
  WLizenz32: TWLizenz32;
  Abrufberechtigung_Wieser_MRG910_unbegrenzt: boolean;
  Abrufberechtigung_Wieser_ohne_MRG910_unbegrenzt: boolean;
  Abrufberechtigung_pruefen: boolean;
  LizErg: integer;
  bOldWithZustand: boolean;  // 02.08.2004
Begin
  Result:=false;
  bOldWithZustand := WithZustandTabelle;
  try
    WithZustandTabelle := True;
    NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
    FehlerGruppeCodeUpdate (0, 0);                            { Vorbelegung: Ok }
    { Modemstatus-Anzeige: }
    FormMainDSfGAbruf.ModemstatusMessage ('');

    { neuen Eintrag in Journaldatei schreiben: }
    JournalId:=WriteNewDSfGJournal (StationId, '', GetAbrufartString, Datentypen, COMPort);

    if Abrufart = aa_manuell then begin
      if not LoescheDaten_Station (false, StationId) then begin   { bestehende manuelle Daten der Station vorher löschen }
        FehlerGruppeCodeUpdate (ST_DATAERROR, DERR_DELETE);
        exit;
      end;
    end;

    { Konfiguration einlesen ohne Rohdatenloeschen -> ausgelesene Rohdaten-Strings zusätzlich in Datei mitschreiben }
    if (Abrufart = aa_konflesen) AND not RohdatenLoeschen then begin
      KonfRohdatenFileName:=CreateTempRohFile (PathServer.PathName [WWorkDir], '~DKF');
      WriteRohdateiname (JournalId, KonfRohdatenFileName);          { Rohfilename in Journal-Detailtabelle protokollieren }
    end;

    { Ruf-Stammdaten ermitteln: }
    if not Stammdaten.GetRufStammdaten (StationId, RufStammdaten) then begin
      FehlerGruppeCodeUpdate (ST_STAMMERROR, SMERR_RUFSTAMMDATEN);
      exit;
    end;

    KennungMessage(RufStammdaten.Kennung);
    StationMessage(RufStammdaten.Stationsname);
    DSfGKennung:=RufStammdaten.Kennung;       { Vorbelegung: Stammdaten-Kennung }

    { Extensionmode 0 setzen (Voreinstellung): }
    if CommComponent is TSerialDSfG then begin
      TSerialDSfG (CommComponent).SetExtensionMode (0);
      TSerialDSfG (CommComponent).SetDCDCheck (false);    { DCD-Überwachung aus }
    end else
      TClientSocketDSfG (CommComponent).SetExtensionMode (0);

    if CommComponent is TSerialDSfG then begin    { Abruf über serielle Schnittstelle }
      Modem_initialisieren (TSerialDSfG (CommComponent));   { Modem initialisieren }

      RufNrStr:=Systemdaten.Vorwahl + RufStammdaten.Rufnummer;
      ZustandMessage (COMPort, RufStammDaten.StationId, z_Verbindungaufbauen,
                      ' ' + Copy(RufNrStr, 2, length(RufNrStr)), WithZustandTabelle);
      Befehl:='atd'+RufNrStr+CR;
      { Verbindungsaufbaukommando senden }
      if not TSerialDSfG (CommComponent).SendModemCommand (Befehl, Timeout_Verbindungsaufbau, R) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      FormMainDSfGAbruf.ModemstatusMessage (Trim (R.Antwort));
      { Antwort auf Verbindungsaufbau-Kommando auswerten: }
      if not CheckModemConnect (R.Antwort) then exit;
    end
    else begin                                        { Abruf über TCP/IP }
      { Rufstammdaten: Rufnummer muß IP-Adresse und Port durch Leerzeichen getrennt enthalten }
      S:=RufStammdaten.Rufnummer;
      IPAdr:=F_Zerlegen (S, ' ');
      try
        PortId:=StrToInt (F_LeftTrunc (S, ' '));
      except
        PortId:=8000;
      end;
      VerbStr:='Adr.: '+ IPAdr + '  Port: ' + IntToStr (PortId);
      ZustandMessage (COMPort, RufStammDaten.StationId, z_VerbindungaufbauenTCPIP,
                      ' ' + VerbStr, WithZustandTabelle);
      { Socket-Verbindung öffnen: }
      if not TClientSocketDSfG (CommComponent).Connect (IPAdr, PortId, Timeout_Verbindungsaufbau,
                                                        AFehlergruppe, AFehlercode) then begin
        FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
        exit;
      end;
    end;

    { Verbindung steht jetzt: }
    NoCarrier:=false;
    FormMainDSfGAbruf.SetStatusColor (ps_VerbindungSteht);
    ZustandMessage (COMPort, RufStammDaten.StationId, z_VerbindungSteht, '', WithZustandTabelle);
    UpDateDSfGJournal (JournalId, C_WJournal_DZVerbSteht);

    if CommComponent is TSerialDSfG then
      TSerialDSfG (CommComponent).SetDCDCheck (true);     { DCD-Überwachung ein }
    Delay (200);       { Wartezeit (generell empfehlenswert, nötig für MRG 910) }

    { Kennungsabfrage und -überprüfung: }
    if not KennungAbfragen then exit;
    if DSfGKennung <> RufStammdaten.Kennung then begin
      if Stop_bei_falscher_Kennung then begin
        FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_KEINE_VERBINDUNG);
        Keine_weiteren_Versuche:=true;
        exit;
      end
      else begin
        { keine Warnung, wenn Kennung noch nicht in den Stammdaten vorhanden ist
          (bei erstem KonfLesen einer Station !): }
        if RufStammdaten.Kennung <> '' then
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_VERBINDUNG);
      end;
    end;

    { Paßwort übertragen: }
    if not PasswortLogin (Keine_weiteren_Versuche) then exit;

    { Wieser-DSfG-DFÜ: aktive Teilnehmer ermitteln }
    if Hersteller = 'W' then
      if not GetAktiveBusteilnehmer then exit;

    { Versionsdaten der DSfG-DFÜ abfragen (ab Erweiterungsgrad 1): }
    if Extensionmode > 0 then
      if not GetDfueVersionsdaten then exit;

    { nur beim Konfiguration einlesen: }
    if Abrufart = aa_konflesen then begin
      { Lizenzprüfung (05.09.2003, WW): }
      { Kriterien abprüfen, welche nur beim Neu-Einlesen der Konfiguration existieren: }
      if (RufStammdaten.Busadresse = '0') OR
         (StammDaten.GetInstanzCount (StationId) <= 1) then begin
        WLizenz32:=TWLizenz32.Create (PathServer.Pathname [WNetProgDir] + C_Lizenz_Filename, true);
        try
          Abrufberechtigung_Wieser_MRG910_unbegrenzt:=WLizenz32.ReadDSfGStationen_Wieser_MRG910_frei;
          Abrufberechtigung_Wieser_ohne_MRG910_unbegrenzt:=WLizenz32.ReadDSfGStationen_Wieser_ohne_MRG910_frei;
        finally
          WLizenz32.Free;
        end;
        if Hersteller = 'W' then begin      { Wieser DSfG-DFÜ }
          { Prüfung, ob ein Wieser MRG 910 eingelesen wird (über Programmname aus DFÜ-Versionsdaten): }
          GerTypNr:=Stammdaten.GetGeraetetypNrByName (DfueVersionsdaten.ProgName);
          if GerTypNr = C_GTypNr_MRG910 then
            Abrufberechtigung_pruefen:=not Abrufberechtigung_Wieser_MRG910_unbegrenzt { prüfen, wenn nicht freigeschaltet }
          else   { sonstige Wieser DSfG-DFÜ }
            Abrufberechtigung_pruefen:=not Abrufberechtigung_Wieser_ohne_MRG910_unbegrenzt; { prüfen, wenn nicht freigeschaltet }
        end else     { DSfG-DFÜ eines Fremdherstellers }
          Abrufberechtigung_pruefen:=true; { immer prüfen }

        if Abrufberechtigung_pruefen then begin  { Abrufberechtigung muß geprüft werden }
          LizErg:=WStationslizenz32_DSfG (PathServer.Pathname [WStammDir],
                                          Abrufberechtigung_Wieser_MRG910_unbegrenzt,
                                          Abrufberechtigung_Wieser_ohne_MRG910_unbegrenzt,
                                          false);
          if LizErg < 0 then begin  { Lizenzfehler }
            case LizErg of
              C_Lizenz_NoFile,
              C_Lizenz_NameError: FehlerGruppeCodeUpdate (SYS_LICENCEERROR, LICENCEERR_KEINESTATIONEN);
              C_Lizenz_MaxError : FehlerGruppeCodeUpdate (SYS_LICENCEERROR, LICENCEERR_ANZAHLSTATIONEN);
            else
              FehlerGruppeCodeUpdate (SYS_LICENCEERROR, LICENCEERR_UNBEKANNT);
            end;
            Keine_weiteren_Versuche:=true;
            exit;
          end;
        end;
      end;  { if RufStammdaten.Busadresse... }

      { alle in der DSfG-DFÜ eingestellten Adressen abfragen (nur Wieser): }
      if Hersteller = 'W' then
        if not GetDfueAdressen then exit;
    end;

    if Transparentmodus then begin
      { DSfG-DFÜ transparent schalten und spontane Aufmerksamkeits-Telegramme lesen: }
      if not DFUETransparentSchalten then exit;
    end
    else begin
      { bei Erweiterungsgrad 1: von der DSfG-DFÜ unterstützte Kommandos abfragen }
      if Extensionmode > 0 then
        if not GetDfueKommandos then exit;
    end;
    Result:=true;
  finally
    WithZustandTabelle := bOldWithZustand;
  end;
End;

{---------------------------------}
function TAbruf.VerbAbbau: boolean;
{---------------------------------}
{ Verbindung wird abgebaut durch Rücksetzen der DTR-Leitung }
var
  R: TRueckgabe;
  AFehlergruppe: integer;
  AFehlercode: integer;
Begin
  Result:=false;
  WithZustandTabelle := True; // 02.08.2004
  ZustandMessage (COMPort, RufStammDaten.StationId, z_VerbindungAbbauen, '', WithZustandTabelle);
  { Extensionmode 0 wieder setzen (Voreinstellung): }
  if CommComponent is TSerialDSfG then begin
    TSerialDSfG (CommComponent).SetDCDCheck (false);    { DCD-Überwachung aus }
    TSerialDSfG (CommComponent).SetExtensionMode (0)
  end else
    TClientSocketDSfG (CommComponent).SetExtensionMode (0);

  if CommComponent is TSerialDSfG then begin  { Abruf über serielle Schnittstelle }
    TSerialDSfG (CommComponent).ClearDTRSignal;        { DTR-Leitung rücksetzen }
    { OK vom Modem empfangen: Verbindung beendet }
    if TSerialDSfG (CommComponent).SendModemCommand ('', Timeout_Verbindungsabbau, R) then begin
      if Pos ('OK', AnsiUpperCase (R.Antwort)) <> 0 then
        Result:=true;
    end;
    TSerialDSfG (CommComponent).SetDTRSignal;          { DTR-Leitung wieder setzen }
  end
  else begin                                          { Abruf über TCP/IP }
    { Socket-Verbindung schließen: }
    if not TClientSocketDSfG (CommComponent).Disconnect (Timeout_Verbindungsabbau,
                                                         AFehlergruppe, AFehlercode) then
      FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode);
  end;

  FormMainDSfGAbruf.ModemstatusMessage ('');
  UpdateDSfGJournal (JournalId, C_WJournal_DZVerbEnde);
End;

{------------------------------------------------------------------------------------------------------}
Function TAbruf.AbrufDaten (Modus: byte; von, bis: TDateTime; Datentyp: integer; AbrufListe: TAbrufList;
                            KonvListe: TKonvList; AktuZeitangaben: TZeitangaben): boolean;
{------------------------------------------------------------------------------------------------------}
{ Abrufen der DSfG-Daten (Datentypen: Archive, Logbücher, Datenelemente);
  Welche Datentypen abgerufen werden sollen, steht in der Abrufliste.
  Übergabe: Abrufmodus (neue, Zeitbereich, Ordnungsnummer)
            von-Zeitpunkt, bis-Zeitpunkt (für Modus d_VonBis)
            Datentyp
            Abrufliste
            aktuelle Zeitangaben der Station
  Rückgabe: KonvListe
  Ergebnis: true, wenn Datenabruf ok }
var
  Befehl: string;
  R: TRueckgabe;
  Timeout: integer;
  i: integer;
  EAdr: char;
  InstanzId: integer;
  Gruppe, Kanal: integer;
  Kanaltyp, Werteart: string;
  BusAdr_Quelle: char;
  Del_von, Del_bis: string;
  OrdNr_von, OrdNr_bis: integer;
  Antwort_vollstaendig: boolean;
  KonvListObj: TKonvListObj;
  Abruf_von, Abruf_bis: TDateTime;
  Erg: integer;
  Zeitangaben: TZeitangaben;
  MerkEAdr: char;
  MerkGruppe: integer;
  dummy: TDateTime;
  AutoDaten_da: boolean;
  DatenFehler: boolean;                     { Flag für Fehler in den Rohdaten }
  tzi: TIME_ZONE_INFORMATION;
  isOrdNr: boolean;
  Soll_von, Soll_bis: TDateTime;
  InstanzId_Quelle: integer;
  GerTypNr_Quelle: integer;
  AutoOrdNr_von: integer;
  AutoOrdNr_bis: integer;
  SollAutoDatumZeit_bis: TDateTime;
  Fuellstand_bis: integer;
  Letzt_Tab_OrdNr: integer;
  Letzt_Tab_DatumZeit: TDateTime;
  erstes_Telegramm: boolean;
  OrdNr_DatumZeit_Cmp_OK: boolean;
  keine_neuen_Daten_abrufbar: boolean;

Begin
  Result:=true;
  DatenFehler:=false;
  SollAutoDatumZeit_bis:=-1;

  { Vorbelegung für Zeitangaben, falls nicht verwendet: }
  Zeitangaben.EAdr:=NUL;
  Zeitangaben.DatumZeit:=0;
  Zeitangaben.Zeitzone:='';
  Zeitangaben.LetztVerstZZ:=0;
  Zeitangaben.vom_PC:=false;

  MerkEAdr:=NUL;
  MerkGruppe:=-1;
  i:=0;
  { Abrufliste abarbeiten: }
  while (i <= AbrufListe.Count - 1) AND (not NoCarrier) do begin
    EAdr:=TAbrufListObj (AbrufListe [i]).EAdr[1];
    InstanzId:=TAbrufListObj (AbrufListe [i]).InstanzId;
    Gruppe:=TAbrufListObj (AbrufListe [i]).Gruppe;
    Kanal:=TAbrufListObj (AbrufListe [i]).Kanal;
    Kanaltyp:=TAbrufListObj (AbrufListe [i]).Kanaltyp;
    Werteart:=TAbrufListObj (AbrufListe [i]).Werteart;
    BusAdr_Quelle:=TAbrufListObj (AbrufListe [i]).BusAdr_Quelle[1];
    Del_von:=TAbrufListObj (AbrufListe [i]).Del_von;
    Del_bis:=TAbrufListObj (AbrufListe [i]).Del_bis;
    OrdNr_von:=TAbrufListObj (AbrufListe [i]).OrdNr_von;
    OrdNr_bis:=TAbrufListObj (AbrufListe [i]).OrdNr_bis;
    InstanzId_Quelle:=TAbrufListObj (AbrufListe [i]).InstanzId_Quelle;
    GerTypNr_Quelle:=TAbrufListObj (AbrufListe [i]).GerTypNr_Quelle;

    keine_neuen_Daten_abrufbar:=false;     { Vorbelegung: neue Daten im Gerät }
    { Ausgabe in Statuszeile: }
    case Datentyp of
      C_IsArchive: ZustandMessage (COMPort, RufStammDaten.StationId, z_ArchiveAbrufen,
                                   ' (Adr: '+ EAdr +'  AG: '+ IntToStr(Gruppe) +'  AK: '+ IntToStr(Kanal) + ')',
                                   WithZustandTabelle);
      C_IsLogbuecher: ZustandMessage (COMPort, RufStammDaten.StationId, z_LogbuecherAbrufen,
                                      ' (Adr: '+ EAdr +'  LB: '+ IntToStr(Gruppe) +')', WithZustandTabelle);
      C_IsDatenelemente: if EAdr <> MerkEAdr then                      { nur bei neuer Adresse eine neue ZustandMessage }
                           ZustandMessage (COMPort, RufStammDaten.StationId, z_DatenelementeAbrufen,
                                           ' (Adr: '+ EAdr+')', WithZustandTabelle);
    end;

    { bei Datenelementen zuvor Datum/Zeit und Zeitzone aus Gerät lesen (Zeitinfo für Konvertierung): }
    if (Datentyp = C_IsDatenelemente) AND (EAdr <> MerkEAdr) then begin     { nur bei neuer Adresse Zeitangaben abrufen }
      AbrufZeitangaben (EAdr, Zeitangaben); //!!
      { als Ersatz PC-Zeit und PC-Zeitzone, falls Gerät keine Zeitangaben liefert: }
      if Zeitangaben.DatumZeit = 0 then begin
        Zeitangaben.DatumZeit:=Now;
        if GetTimeZoneInformation (tzi) = TIME_ZONE_ID_DAYLIGHT then
          Zeitangaben.Zeitzone:=CMESZ
        else
          Zeitangaben.Zeitzone:=CMEZ;
        Zeitangaben.vom_PC:=true;
      end;
    end;

    { Abrufmodi unterscheiden: }
    if Modus = d_NeueOrdNr then begin    { neue Daten über Ordnungsnummer abrufen }
      if Datentyp = C_IsArchive then begin
        { "Füllstand bis Ordnungsnummer" für jede Archivgruppe nur einmal abrufen: }
        if (EAdr <> MerkEAdr) OR (Gruppe <> MerkGruppe) then begin
          Erg:=AbrufArchivFuellstand_bis (EAdr, Gruppe, Fuellstand_bis);
          if Erg <> 0 then begin      // Fehlerbehandlung korrigiert, 29.11.2004, WW
            Result:=false;
            if Erg = 1 then begin
              inc (i);   { weiter in der Abrufliste }
              Continue;
            end else
              exit;      { gravierender Kommunikationsfehler, raus }
          end;
          SollAutoDatumZeit_bis:=Now;               { für Zeitbereich-Journal }
        end;
      end
      else if Datentyp = C_IsLogbuecher then begin
        { "Füllstand bis Ordnungsnummer" für jedes Logbuch abrufen: }
        Erg:=AbrufLogbuchFuellstand_bis (EAdr, BusAdr_Quelle, Fuellstand_bis);
        if Erg <> 0 then begin      // Fehlerbehandlung korrigiert, 29.11.2004, WW
          Result:=false;
          if Erg = 1 then begin
            inc (i);   { weiter in der Abrufliste }
            Continue;
          end else
            exit;      { gravierender Kommunikationsfehler, raus }
        end;
        SollAutoDatumZeit_bis:=Now;                 { für Zeitbereich-Journal }
      end else
        Fuellstand_bis:=0;

      { Ordnungsnummernbereich für Automatik-Abruf: }
      AutoDaten_da:=Get_AutoVonBisOrdNr (InstanzId, Gruppe, Kanal, Kanaltyp,
                                         Fuellstand_bis, AktuZeitangaben,
                                         AutoOrdNr_von, AutoOrdNr_bis,
                                         Letzt_Tab_OrdNr, Letzt_Tab_DatumZeit);
      Abruf_von:=AutoOrdNr_von;
      Abruf_bis:=AutoOrdNr_bis;

      { Archivkanal/Logbuch: wenn im Gerät keine Daten gespeichert sind oder alle
        im Gerät gespeicherten Daten bereits ausgelesen wurden, kann man sich den
        Abruf sparen: }
      if (Fuellstand_bis <= 0) OR
         (AutoDaten_da AND (Fuellstand_bis = Letzt_Tab_OrdNr)) then
        keine_neuen_Daten_abrufbar:=true;   { Flag setzen }

      { Journal: Automatik-Soll-Abfragebereich }
      if (Datentyp = C_IsArchive) OR (Datentyp = C_IsLogbuecher) then begin
        if keine_neuen_Daten_abrufbar then
          WriteDatenzeitbereich_Soll (JournalId, InstanzId, Gruppe, Kanal, -1, -1, false)
        else begin
          { Ordnungsnummernbereich: }
          Soll_von:=Abruf_von;
          if AutoDaten_da then
            Soll_von:=Soll_von + 1;                  { nächste Ordnungsnummer }
          Soll_bis:=Abruf_bis;
          WriteDatenzeitbereich_Soll (JournalId, InstanzId, Gruppe, Kanal, Soll_von, Soll_bis, true);

          { ...und zusätzlich Zeitbereich: }
          if AutoDaten_da then
            Soll_von:=Letzt_Tab_DatumZeit + EncodeTime (0, 0, 1, 0)  { plus 1 s }
          else
            Soll_von:=-1;                                 { nicht ermittelbar }
          Soll_bis:=SollAutoDatumZeit_bis;
          WriteDatenzeitbereich_Soll (JournalId, InstanzId, Gruppe, Kanal, Soll_von, Soll_bis, false);
        end;
      end;
     end
     else if Modus = d_NeueZeit then begin     { neue Daten über Zeit abrufen }
      { Zeitbereich für Automatik-Abruf: }
      AutoDaten_da:=Get_AutoVonBisZeit (InstanzId, Gruppe, Kanal, Kanaltyp, AktuZeitangaben,
                                        Abruf_von, Abruf_bis);
      { Journal: Automatik-Soll-Abfragebereich (Zeitbereich) }
      if (Datentyp = C_IsArchive) OR (Datentyp = C_IsLogbuecher) then begin
        Soll_von:=Abruf_von;
        Soll_bis:=Abruf_bis;
        if AutoDaten_da then
          Soll_von:=Soll_von + EncodeTime (0, 0, 1, 0);            { plus 1 s }
        WriteDatenzeitbereich_Soll (JournalId, InstanzId, Gruppe, Kanal, Soll_von, Soll_bis, false);
      end;
    end
    else begin
      if Modus = d_OrdNr then begin     { manueller Abruf über Ordnungsnummer }
        Abruf_von:=OrdNr_von;
        Abruf_bis:=OrdNr_bis;
      end
      else begin          { manueller Abruf über Zeit oder Datenelementeabruf }
        Abruf_von:=von;
        Abruf_bis:=bis;
      end;

      { Journal: Manu-Soll-Abfragebereich }
      if (Datentyp = C_IsArchive) OR (Datentyp = C_IsLogbuecher) then begin
        isOrdNr:=Modus = d_OrdNr;
        WriteDatenzeitbereich_Soll (JournalId, InstanzId, Gruppe, Kanal, Abruf_von, Abruf_bis, isOrdNr);
      end;
    end;  { if Modus }

    MerkEAdr:=EAdr;
    MerkGruppe:=Gruppe;

    if keine_neuen_Daten_abrufbar then begin
      inc (i);
      Continue;                  { nicht abrufen und weiter in der Abrufliste }
    end;

    erstes_Telegramm:=true;
    Antwort_vollstaendig:=false;
    while (not Antwort_vollstaendig) AND (not NoCarrier) do begin { evtl. mehrere Abrufe pro Kanal/Logbuch nötig }
      { Befehl zusammensetzen: }
      IncrementTID;
      if Datentyp = C_IsArchive then begin                                                                { Archivkanal }
        Befehl:=Get_ArchivBefehl(EAdr, Gruppe, Kanal, Modus, Abruf_von, Abruf_bis);
        Timeout:=Timeout_Archive;
      end
      else if Datentyp = C_IsLogbuecher then begin                                                            { Logbuch }
        Befehl:=Get_LogbuchBefehl(EAdr, BusAdr_Quelle, Modus, Abruf_von, Abruf_bis);
        Timeout:=Timeout_Logbuecher;
      end
      else if Datentyp = C_IsDatenelemente then begin                             { einzelne Datenelemente oder Bereich }
        Befehl:=Get_DatenelementeBefehl(EAdr, Del_von, Del_bis);
        Timeout:=Timeout_Datenelemente;
      end
      else begin
        Befehl:='';
        Timeout:=0;
      end;

      { Datenabruf-Kommando senden }
      if not SendCommand (Befehl, FS, 1, Timeout, ad_File, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        Result:=false;
        if R.Fehlergruppe = ST_DSFGERROR then
          Break                               { weiter mit nächstem Telegramm }
        else
          exit;
      end;

      { Rohfile prüfen (Antwort = Rohfilename), bei unvollständiger Antwort von-Zeitpunkt für Folgebefehl zurückgeben: }
      Erg:=CheckRohfile_Antworttelegramm (R.Antwort, EAdr, Modus,
                                          erstes_Telegramm,
                                          Letzt_Tab_OrdNr, Letzt_Tab_DatumZeit,
                                          Antwort_vollstaendig,
                                          Del_von, Abruf_von,
                                          OrdNr_DatumZeit_Cmp_OK);
      erstes_Telegramm:=false;  { Belegung für alle Folgetelegramme des Kanals/Logbuchs/DE-Befehls}
      if not OrdNr_DatumZeit_Cmp_OK then begin
        { Antwort verwerfen und nochmal ab Ord.Nr 1 (alles) auslesen ! Warnung, Gerät getauscht ! }
        FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_ORDNRFOLGE);
        Abruf_von:=1;
        Antwort_vollstaendig:=false;
        Continue;
      end;

      { Rohfilename in Journal-Detailtabelle protokollieren: }
      if not RohdatenLoeschen then
        WriteRohdateiname (JournalId, R.Antwort);

      if Erg <> DSFGKONVERR_OK then begin                 { Fehler im Rohfile }
        { nur Fehlercode in Journaldetail-Tabelle eintragen: }
        WriteDatenzeitbereich_Ist (JournalId, InstanzId, Gruppe, Kanal, Erg, 0, 0, 0, 0);
        DatenFehler:=true;
        Break;                                { weiter mit nächstem Telegramm }
      end;

      { in Konvertierungsliste eintragen: }
      KonvListObj:=TKonvListObj.Create;
      KonvListObj.SetData (InstanzId, Gruppe, Kanal,
                           Kanaltyp, Werteart, Zeitangaben,
                           InstanzId_Quelle, GerTypNr_Quelle);
      KonvListe.AddObject (R.Antwort, KonvListObj);
    end; { while not Antwort_vollstaendig }

    inc(i);
  end;  { while AbrufListe }

  { bei fehlerhaften Rohdaten ein globaler Eintrag in Journal-Fehlertabelle: }
  if DatenFehler then begin
    case Datentyp of
      C_IsArchive:       FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_ARCHIVKONV);
      C_IsLogbuecher:    FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_LOGBKONV);
      C_IsDatenelemente: FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_INSTWERTKONV);
    end;
  end;

  if NoCarrier then
    Result:=false;
End;

{-------------------------------------------------------------------------------}
Function TAbruf.AbrufArchivFuellstand_bis (EAdr: char; Gruppe: integer;
                                           var Fuellstand_bis: integer): integer;
{-------------------------------------------------------------------------------}
{ "Füllstand bis Ordnungsnummer" einer Archivgruppe abrufen;
  Übergabe: Busadresse der Instanz
            Archivgruppe
  Rückgabe: Füllstand bis
  Ergebnis: 0 = Abruf des Füllstands ok
            1 = Fehler aufgetreten, Abruf weiterer Archivkanäle kann fortgeführt werden
            2 = gravierender Kommunikationsfehler aufgetreten, Fortführen des Abrufs
                ist nicht sinnvoll }
var
  Befehl: string;
  R: TRueckgabe;
  Wert: string;
  DEL: string;
  L: TDELList;
  Erg: integer;

begin
  Fuellstand_bis:=0;  { Vorbelegung für Fuellstand_bis: keine Daten im Archiv }
  if NoCarrier then begin
    Result:=2;
    exit;
  end;

  { Befehl zusammensetzen: }
  IncrementTID;
  DEL:='ca'+Chr(96+Gruppe)+'d';
  Befehl:=Get_DatenelementeBefehl(EAdr, DEL, '');

  { Abruf-Kommando für "Füllstand bis" senden }
  if not SendCommand (Befehl, FS, 1, Timeout_Datenelemente, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    if R.Fehlergruppe = ST_DSFGERROR then
      Result:=1
    else
      Result:=2;
    exit;
  end;

  L:=TDELList.Create;
  try
    { Antwortstring in Datenelementeliste (Zwischenpuffer) konvertieren: }
    Erg:=L.LoadFromRohstring (R.Antwort, EAdr);
    if Erg <> DSFGKONVERR_OK then begin
      if (Erg >= C_MinCode_DSfGKonvErr_AA) then   { außerplanmäßige Antwort }
        FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_DE)  // 29.11.2004, WW
      else
        FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_INSTWERTKONV);
      Result:=1;
      exit;
    end;
    { Rückgabe: Fuellstand_bis }
    if L.GetWert (EAdr, DEL, Wert) then
      if Wert <> '' then
        Fuellstand_bis:=StrToInt (Wert);
  finally
    L.Free;
  end;
  Result:=0;
end;

{--------------------------------------------------------------------------------}
Function TAbruf.AbrufLogbuchFuellstand_bis (EAdr: char; Adr_Quelle: char;
                                            var Fuellstand_bis: integer): integer;
{--------------------------------------------------------------------------------}
{ "Füllstand bis Ordnungsnummer" eines Logbuchs abrufen;
  Übergabe: Busadresse der Instanz
            Busadresse der Quelle des Logbuchs
  Rückgabe: Füllstand bis
  Ergebnis: 0 = Abruf des Füllstands ok
            1 = Fehler aufgetreten, Abruf weiterer Logbücher kann fortgeführt werden
            2 = gravierender Kommunikationsfehler aufgetreten, Fortführen des Abrufs
                ist nicht sinnvoll }
var
  Befehl: string;
  R: TRueckgabe;
  Wert: string;
  DEL: string;
  L: TDELList;
  Erg: integer;

begin
  Fuellstand_bis:=0;  { Vorbelegung für Fuellstand_bis: keine Daten im Logbuch }
  if NoCarrier then begin
    Result:=2;
    exit;
  end;

  { Befehl zusammensetzen: }
  IncrementTID;
  if (Adr_Quelle >= Low (CLogbuchDEL)) AND (Adr_Quelle <= High (CLogbuchDEL)) then
    DEL:=CLogbuchDEL [Adr_Quelle]+'b'
  else
    DEL:='';

  Befehl:=Get_DatenelementeBefehl(EAdr, DEL, '');

  { Abruf-Kommando für "Füllstand bis" senden }
  if not SendCommand (Befehl, FS, 1, Timeout_Datenelemente, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    if R.Fehlergruppe = ST_DSFGERROR then
      Result:=1
    else
      Result:=2;
    exit;
  end;

  L:=TDELList.Create;
  try
    { Antwortstring in Datenelementeliste (Zwischenpuffer) konvertieren: }
    Erg:=L.LoadFromRohstring (R.Antwort, EAdr);
    if Erg <> DSFGKONVERR_OK then begin
      if (Erg >= C_MinCode_DSfGKonvErr_AA) then   { außerplanmäßige Antwort }
        FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_DE)  // 29.11.2004, WW
      else
        FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_INSTWERTKONV);
      Result:=1;
      exit;
    end;
    { Rückgabe: Fuellstand_bis }
    if L.GetWert (EAdr, DEL, Wert) then
      if Wert <> '' then
        Fuellstand_bis:=StrToInt (Wert);
  finally
    L.Free;
  end;
  Result:=0;
end;

{------------------------------------------------------------------------------------}
Function TAbruf.AbrufZeitangaben (EAdr: char; var Zeitangaben: TZeitangaben): boolean;
{------------------------------------------------------------------------------------}
{ Abrufen von Zeitangabe-DELs einer DSfG-Instanz und Eintragen in DEL-Liste;
  Übergabe: Busadresse der Instanz
  Rückgabe: Zeitangaben-Record mit Daten für EAdr
  Ergebnis: true, wenn Abruf der Zeitangabe-DELs ok }
var
  Befehl: string;
  R: TRueckgabe;
  Wert: string;
  DELs: string;
  Erg: integer;

begin
  Result:=false;
  with Zeitangaben do begin                   { Vorbelegungen für Zeitangaben }
    EAdr:=NUL;
    DatumZeit:=0;
    Zeitzone:='';
    LetztVerstZZ:=0;
    vom_PC:=false;
  end;

  if NoCarrier then exit;
  { Befehl zusammensetzen: }
  IncrementTID;
  DELs:=CD_ALLG_DatumUhrzeit + GS + CD_ALLG_Zeitzone + GS + CD_ALLG_LetztVerstZz;
  Befehl:=Get_DatenelementeBefehl(EAdr, DELs, '');

  { Zeitangaben-Kommando senden }
  if not SendCommand (Befehl, FS, 1, Timeout_Datenelemente, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwortstring in Datenelementeliste (Zwischenpuffer) konvertieren: }
  Erg:=DELList.LoadFromRohstring (R.Antwort, EAdr);
  if Erg <> DSFGKONVERR_OK then begin
    if (Erg >= C_MinCode_DSfGKonvErr_AA) then   { außerplanmäßige Antwort }
      FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_DE)  // 20.07.2004, WW
    else
      FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_INSTWERTKONV);
    exit;
  end;

  Zeitangaben.EAdr:=EAdr;                             { Rückgabe: Zeitangaben }
  with Zeitangaben do begin
    if DELList.GetWert (EAdr, CD_ALLG_DatumUhrzeit, Wert) then
      if Wert <> '' then
        UnixTimeStrToDateTime (Wert, DatumZeit);
    if DELList.GetWert (EAdr, CD_ALLG_Zeitzone, Wert) then
      Zeitzone:=Wert;
    if DELList.GetWert (EAdr, CD_ALLG_LetztVerstZz, Wert) then
      if Wert <> '' then
        UnixTimeStrToDateTime (Wert, LetztVerstZz);
  end;
  Result:=true;
end;

{------------------------------------------------------------------}
procedure TAbruf.InitInstanzCount (var InstanzCount: TInstanzCount);
{------------------------------------------------------------------}
{ alle Instanzzähler auf 0 setzen }
begin
  with InstanzCount do begin
    Dfu:=0;
    Gas:=0;
    Rev:=0;
    Prot:=0;
    Reg:=0;
    Strg:=0;
    Umw:=0;
    Wieser:=0;
    Odor:=0;
    Unbest:=0;
  end;
end;

{----------------------------------------------------------------------------------------------------------}
procedure TAbruf.AddKonfiguration_LoginDfue (InstanzListe: TInstanzDataList; InstDfuListe: TInstDfuDataList;
                                             var InstanzCount: TInstanzCount);
{----------------------------------------------------------------------------------------------------------}
{ Instanzdaten der Login-DFÜ in Listen schreiben;
  Übergabe/Rückgabe: InstanzListe
                     InstDfuListe
                     Instanzzähler-Record }
var
  InstanzData: PInstanzData;
  InstDfuData: PInstDfuData;
  InstanzId: integer;
  InstanzName: string;
  Rufnummer: string;
  LogPort: integer;
  i: integer;
  Adresse: array [1..4] of string;
  Passwort: array [1..4] of string;

begin
  { Adressen- und Paßwort-Arrays vorbelegen:
    (ab 28.05.2003 WW; zuvor typisierte Konstanten mit Vorbelegung nur bei Programmstart: }
  for i:=Low (Adresse) to High (Adresse) do
    Adresse[i]:='';
  for i:=Low (Passwort) to High (Passwort) do
    Passwort[i]:='';

  { Eintrag in InstanzListe: }
  inc (InstanzCount.Dfu);
  Instanzname:=C_D_Instanzname_DFU + Format ('%3d',[InstanzCount.Dfu]);
  New (InstanzData);
  SetInstanzDataRec (InstanzData,
                     -1,                            { die InstanzId wird erst beim Schreiben in die Tabelle ermittelt ! }
                     RufStammdaten.StationId,
                     Instanzname,
                     C_D_Instanztyp_DFU,
                     EAdr_Dfue,
                     DfueVersionsdaten.Hersteller,
                     DfueVersionsdaten.ProgName,
                     1,
                     '',
                     DfueVersionsdaten.Version,
                     -1,
                     -1,
                     0);
  InstanzListe.Add (InstanzData);

  { Eintrag in InstDfuListe: }
  InstanzId:=Ord (EAdr_Dfue) * (-1);               { zur Verknüpfung des Listeneintrags mit dem Eintrag in InstanzListe }
  Rufnummer:=RufStammdaten.Rufnummer;

  if DfueVersionsdaten.Adresse[1] <> '' then begin          { bei Wieser DSfG-DFÜ können alle Adressen ermittelt werden }
    Adresse[1]:=DfueVersionsdaten.Adresse[1];
    Adresse[2]:=DfueVersionsdaten.Adresse[2];
    Adresse[3]:=DfueVersionsdaten.Adresse[3];
    Adresse[4]:=DfueVersionsdaten.Adresse[4];
    // 5. Adresse ist keine DFÜ-Adresse und wird nicht in Stammdaten abgelegt
    for i:=Low (Adresse) to High (Adresse) do             { Paßwort der richtigen Adresse zuordnen }
      if Adresse[i] = EAdr_Dfue then
        Passwort[i]:=RufStammdaten.Passwort;
  end
  else begin
    Adresse[1]:=EAdr_Dfue;
    Passwort[1]:=RufStammdaten.Passwort;
  end;

  LogPort:=RufStammdaten.LogPort;                    { frei wählbarer LogPort }
  New (InstDfuData);
  SetInstDfuDataRec (InstDfuData,
                     InstanzId,
                     DSfGKennung,
                     Rufnummer,
                     Adresse,
                     Passwort,
                     LogPort);
  InstDfuListe.Add (InstDfuData);
end;

{-------------------------------------------------------------------------------------------------------------------}
function TAbruf.AbrufKonfiguration_Allgemein (EAdr: char; InstanzListe: TInstanzDataList;
                                              var InstanzCount: TInstanzCount; var nicht_erreicht: boolean;
                                              var Valid: boolean): boolean;
{-------------------------------------------------------------------------------------------------------------------}
{ Abrufen der allgemeinen Konfigurationsdaten einer DSfG-Instanz;
  Übergabe: Busadresse der Instanz
  Übergabe/Rückgabe: InstanzListe
                     Instanzzähler-Record
                     nicht_erreicht: true, wenn Teilnehmer-EAdr nicht erreicht werden konnte
                     Valid (false, wenn Rohdaten ungültig)
  Ergebnis: true, wenn Abruf der allgemeinen Konfigurationsdaten ok }

  function GetInstanztypName(sInstTyp: string): string;  // 23.02.2005
  var
    s : string;
  begin
    s := Trim(sInstTyp);
    if (s <> '') then begin
      with TQueryExt.Create(nil) do
      try
        DatabaseName := PathServer[WStammDir];
        Sql.Add('SELECT ' + C_DTF_ITyp_Bezeichnung);
        Sql.Add('FROM ' + ChangeFileExt(C_DTB_ITyp, ''));
        Sql.Add('WHERE ' + C_DTF_ITyp_InstanzTyp + ' = ''' + s[1] + '''');
        if (Open) then begin
          if (not Eof)
          then Result := Fields[0].asString
          else Result := C_D_Instanzname_unbest;
          Close;
        end
        else Result := C_D_Instanzname_unbest;
      finally
        Free;
      end;
    end
    else Result := C_D_Instanzname_unbest;
  end;

var
  Befehl: string;
  R: TRueckgabe;
  DELs: string;
  Wert: string;
  InstanzData: PInstanzData;
  Instanzname: string;
  Instanztyp: string;
  Hersteller: string;
  Geraetetyp: string;
  Stand: integer;
  FabrikNr: string;
  SWVersion: string;
  Baujahr: integer;
  Inbetriebnahme: TDateTime;
  GerTypNr: integer;
  Erg: integer;

begin
  Result:=false;
  nicht_erreicht:=false;
  if NoCarrier then exit;

  ZustandMessage (COMPort, RufStammDaten.StationId, z_AllgInstKonfigAbrufen, ' (Adr: '+ EAdr+')',
                  WithZustandTabelle);

  { Befehl zusammensetzen: }
  IncrementTID;
  DELs:=CD_ALLG_eigInstanztyp + GS + CD_ALLG_Hersteller + GS + CD_ALLG_Geraetetyp + GS + CD_ALLG_FabrikNr + GS +
        CD_ALLG_Baujahr + GS + CD_ALLG_SWVersion + GS + CD_ALLG_Inbetriebnahme;
  Befehl:=Get_DatenelementeBefehl(EAdr, DELs, '');

  { Allg. Konfigurationsdatenabruf-Kommando senden }
  if not SendCommand (Befehl, FS, 1, Timeout_KonfLesen, ad_String, R, NoCarrier) then begin
    { DSfG-Fehler oder Timeout -> Teilnehmer wird als "nicht erreicht" behandelt
                               -> Konfiguration-Einlesen OK
                               -> Warnung, wenn Teilnehmerliste vorhanden ist }
    if R.Fehlergruppe = ST_DSFGERROR then begin
      { Anm. WW: CAN kann auftreten bei Wieser-DFÜs, wenn ein Teilnehmer nicht
                 erreicht wird (plausibel). ENQ kann auftreten unter bestimmten
                 (bislang nicht bekannten) Umständen bei FlowComp-DFÜs (irgendwie
                 nicht plausibel, es liegt ja kein formaler Fehler im Sendebefehl vor !). }
      if Teilnehmer <> '' then         { Warnung, wenn Teilnehmerliste vorhanden, sonst ok }
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      nicht_erreicht:=true;
      Result:=true;
    end
    else if (R.Fehlergruppe = COM_KOMMERROR) AND (R.Fehlercode = KOMMERR_TIMEOUT) then begin
      if Teilnehmer <> '' then          { Warnung, wenn Teilnehmerliste vorhanden, sonst ok }
        FehlerGruppeCodeUpdate (ST_DSFGERROR, DSFGERR_CAN);     { "Teilnehmer nicht vorhanden" }
      nicht_erreicht:=true;
      Result:=true;
    end else
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Konfiguration einlesen ohne Rohdatenloeschen -> ausgelesene Rohdaten-Strings in Datei mitprotokollieren }
  if not RohdatenLoeschen then
    WriteRohFile (KonfRohdatenFileName, R.Antwort);

  { Antwortstring in Datenelementeliste (Zwischenpuffer) konvertieren: }
  Erg:=DELList.LoadFromRohstring (R.Antwort, EAdr);
  if Erg <> DSFGKONVERR_OK then begin
    if (Erg >= C_MinCode_DSfGKonvErr_AA) then begin  { außerplanmäßige Antwort }
      FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_DE);  // 20.07.2004, WW
      nicht_erreicht:=true;
      Result:=true;
    end else
      FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_INSTWERTKONV);
    exit;
  end;

  { Konfigurationsdaten aus Datenelementeliste lesen und in InstanzListe eintragen: }

  if DELList.GetWert (EAdr, CD_ALLG_eigInstanztyp, Wert) then begin
    if Wert = '' then
      Instanztyp:=C_D_Instanztyp_unbest
    else
      Instanztyp:=Wert;
  end else
    Instanztyp:=C_D_Instanztyp_unbest;

  { Instanzname mit fortlaufender Nummerierung bilden: }
  if length (Instanztyp) > 0 then Instanztyp:=Instanztyp [1];                       { nur das erste Zeichen }
  Instanzname := GetInstanztypName(Instanztyp) + ' [' + EAdr + ']' ;  // 23.02.2005

  if DELList.GetWert (EAdr, CD_ALLG_Hersteller, Wert) then
    Hersteller:=Wert
  else
    Hersteller:='';
  if DELList.GetWert (EAdr, CD_ALLG_Geraetetyp, Wert) then
    Geraetetyp:=Wert
  else
    Geraetetyp:='';
  Stand:=1;           { Vorbelegung; der endgültige Stand kann erst beim Konvertieren der Archivkanäle ermittelt werden }
  if DELList.GetWert (EAdr, CD_ALLG_FabrikNr, Wert) then
    FabrikNr:=Wert
  else
    FabrikNr:='';
  if DELList.GetWert (EAdr, CD_ALLG_SWVersion, Wert) then
    SWVersion:=Wert
  else
    SWVersion:='';
  Baujahr:=-1;
  if DELList.GetWert (EAdr, CD_ALLG_Baujahr, Wert) then
    if Wert <> '' then begin
      try
        Baujahr:=StrToInt (Wert);
      except
        Baujahr:=-1;        { mögliche Formatfehler abfangen (RMG-Umwerter !) }
        Valid:=false;
      end;
    end;
  Inbetriebnahme:=-1;
  if DELList.GetWert (EAdr, CD_ALLG_Inbetriebnahme, Wert) then
    if Wert <> '' then begin
      try
        UnixTimeStrToDateTime (Wert, Inbetriebnahme);
      except
        Inbetriebnahme:=-1; { mögliche Formatfehler abfangen }
        Valid:=false;
      end;
    end;

  { Gerätetyp-Nr. von Wieser-Instanzen aus DSFGDEF.DB ermitteln. Falls keine oder
    unbekannte Wieser-Instanz -> 0
    25.04.2002: auch für Umwerter wegen ERZ2200 (hat Wieser-spezifische Meldungsnummern !) 
    16.06.2005: auch für KGMs wegen FlowComp Q1 (hat FlowComp-spezifische 2000er-Meldungsnummern !) }
  GerTypNr:=0;
  if (Instanztyp = C_D_Instanztyp_Wieser) OR
     (Instanztyp = C_D_Instanztyp_Umw) OR (Instanztyp = C_D_Instanztyp_KGM) then
    GerTypNr:=Stammdaten.GetGeraetetypNrByName (Geraetetyp);

  New (InstanzData);
  SetInstanzDataRec (InstanzData,
                     -1,                            { die InstanzId wird erst beim Schreiben in die Tabelle ermittelt ! }
                     RufStammdaten.StationId,
                     Instanzname,
                     Instanztyp,
                     EAdr,
                     Hersteller,
                     Geraetetyp,
                     Stand,
                     FabrikNr,
                     SWVersion,
                     Baujahr,
                     Inbetriebnahme,
                     GerTypNr);
  InstanzListe.Add (InstanzData);
  Result:=true;
end;

{-------------------------------------------------------------------------------------------------------}
function TAbruf.AbrufKonfiguration_RegInst (EAdr: char; KonfigRohdataListe: TKonfigRohdataList): boolean;
{-------------------------------------------------------------------------------------------------------}
{ Abrufen der Konfigurationsdaten von DSfG-Registrierinstanzen (Archiv- und Logbuchkonfigurationen);
  Übergabe: Busadresse der Registrierinstanz
  Übergabe/Rückgabe: KonfigRohdataListe
  Ergebnis: true, wenn Abruf der Konfigurationsdaten ok }
var
  Befehl: string;
  R: TRueckgabe;
  KonfigRohdataListObj: TKonfigRohdataListObj;
  Del_von: string;
  Del_bis: string;
  Antwort_vollstaendig: boolean;
  Erg: integer;

begin
  Result:=false;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_ArchivLogbKonfigAbrufen, ' (Adr: '+ EAdr+')',
                  WithZustandTabelle);
  Del_von:='c';
  Del_bis:='cbdae';  // geändert 05.11.2001 wg. RMG-Reg.-Instanz ERZ9000T, Vs. ...D (akzeptiert 'c'-'d' nicht)
  Antwort_vollstaendig:=false;
  while (not Antwort_vollstaendig) AND (not NoCarrier) do begin           { evtl. mehrere Abrufe pro Reg.Instanz nötig }
    { Befehl zusammensetzen: }
    IncrementTID;
    Befehl:=Get_DatenelementeBefehl(EAdr, Del_von, Del_bis);

    { Reg.Inst.-Konfigurationsdatenabruf-Kommando senden }
    if not SendCommand (Befehl, FS, 1, Timeout_Datenelemente, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Konfiguration einlesen ohne Rohdatenloeschen -> ausgelesene Rohdaten-Strings in Datei mitprotokollieren }
    if not RohdatenLoeschen then
      WriteRohFile (KonfRohdatenFileName, R.Antwort);

    { Rohstring prüfen, bei unvollständiger Antwort von-DEL für Folgebefehl zurückgeben: }
    Erg:=CheckRohstring_Antworttelegramm(R.Antwort, EAdr, Antwort_vollstaendig, Del_von);
    if Erg <> DSFGKONVERR_OK then begin                 { Fehler im Rohstring }
      if (Erg >= C_MinCode_DSfGKonvErr_AA) then   { außerplanmäßige Antwort }
        FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_DE)  // 20.07.2004, WW
      else
        FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_INSTWERTKONV);
      exit;
    end;

    { Rohdaten in Rohdatenliste einfügen: }
    KonfigRohdataListObj:=TKonfigRohdataListObj.Create;
    KonfigRohdataListObj.SetData (R.Antwort);
    KonfigRohdataListe.AddObject (EAdr, KonfigRohdataListObj);
  end; { while not Antwort_vollstaendig }
  Result:=true;
end;

{-------------------------------------------------------------------------------------------------}
Function TAbruf.AbrufKonfiguration (InstanzListe: TInstanzDataList; InstDfuListe: TInstDfuDataList;
                                    KonfigRohdataListe: TKonfigRohdataList): boolean;
{-------------------------------------------------------------------------------------------------}
{ Abrufen der Konfigurationsdaten einer DSfG-Station;
  Übergabe/Rückgabe: InstanzListe
                     KonfigRohdataListe
                     InstDfuDataList
  Ergebnis: true, wenn Abruf der Konfigurationsdaten ok }
var
  i: integer;
  EAdr: char;
  InstanzCount: TInstanzCount;
  InstanzData: PInstanzData;
  Adressen: string;
  nicht_erreicht: boolean;
  Valid: boolean;
  Kennung_Exists: boolean;

begin
  Result:=false;

  { Warnung, wenn eingelesene Kennung in den Stammdaten bereits existiert: }
  if Stammdaten.KennungAlreadyExists (DSfGKennung, RufStammdaten.LoginInstanzId, Kennung_Exists) then
    if Kennung_Exists then
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_ALREADYEXISTS);

  Valid:=true;
  InitInstanzCount (InstanzCount);                 { alle Instanzzähler auf 0 }

  { Instanzdaten der Login-DFÜ in Listen schreiben: }
  AddKonfiguration_LoginDfue (InstanzListe, InstDfuListe, InstanzCount);

  { allgemeine Instanz-Stammdaten auslesen:
    bei Wieser-DFÜ nur die in der Teilnehmerliste enthaltenen Instanzen abfragen (ohne die Adressen der
    Login-Instanz, wenn bekannt), ansonsten alle möglichen Busadressen anpollen (Teilnehmerliste leer) }
  if Teilnehmer <> '' then begin
    Adressen:=Teilnehmer;
    for i:=Low (DfueVersionsdaten.Adresse) to High (DfueVersionsdaten.Adresse) do
      if DfueVersionsdaten.Adresse[i] <> '' then
        Adressen:=StrFilter (Adressen, DfueVersionsdaten.Adresse[i][1]);
  end else
    Adressen:=CMaxDSfGTeilnehmer;
  for i:=1 to length (Adressen) do begin                                  { alle Adressen der Teilnehmerliste abfragen }
    EAdr:=Adressen[i];
    if EAdr <> EAdr_Dfue then begin                          { von der Login-DFÜ können keine Daten angefordert werden }
      if not AbrufKonfiguration_Allgemein (EAdr, InstanzListe, InstanzCount, nicht_erreicht, Valid) then exit;
      if nicht_erreicht then
        EAdr_nichterreicht:=EAdr_nichterreicht + EAdr;
    end;
  end;

  if not Valid then
    FehlerGruppeCodeUpdate (ST_DATACHECK, DCH_DEINVALID);

  { instanzspezifische Stammdaten auslesen: }
  for i:=0 to InstanzListe.Count-1 do begin
    InstanzData:=InstanzListe.Items[i];
    { Registrierinstanzen: Archiv- und Logbuch-Stammdaten }
    if InstanzData^.Instanztyp = C_D_Instanztyp_Reg then begin
      if not AbrufKonfiguration_RegInst (InstanzData^.Busadresse[1], KonfigRohdataListe) then exit;
    end;
  end;
  Result:=true;
end;

{-----------------------------------------------------------------------------------------}
Function TAbruf.AbrufMomentanwerte (AbrufListe: TAbrufList; nur_ein_DE_abrufen: boolean;
                                    var Keine_weiteren_Journaleintraege: boolean): boolean;
{-----------------------------------------------------------------------------------------}
{ ruft die in AbrufListe enthaltenen Datenelemente einer DSfG-Station ab und speichert die Werte in der zugehörigen
  Momentanwertetabelle;
  Übergabe: Abrufliste mit abzurufenden Datenelementen
            Flag, ob nur 1 Datenelement abgrufen werden soll (dient nur dazu, um die Verbindung zu halten)
  Rückgabe: Flag, ob Journalfehler in Tabelle geschrieben werden soll (nur max. 1 Fehler je Abruf !)
  Ergebnis: true, wenn Momentanwerteabruf erfolgreich (serielle Kommunikation) }
Var
  Befehl: string;
  R: TRueckgabe;
  i: integer;
  EAdr: char;
  Del_von, Del_bis: string;
  Antwort_vollstaendig: boolean;
  Erg: integer;
  MerkEAdr: char;
  TbDSfGMomentanDaten: TTbDSfGMomentanDaten;
  DELList: TDELList;         { Zwischenpuffer für Datenelemente-Konvertierung }
  Busadresse: string;
  DEL: string;
  Wert: string;
  FehlerWarnung: boolean;

begin
  Result:=false;
  FehlerWarnung:=false;

  DELList:=TDELList.Create;
  try
    MerkEAdr:=NUL;
    i:=0;                                       { ganze Abrufliste abarbeiten }
    { Abrufliste abarbeiten: }
    while (i <= AbrufListe.Count - 1) AND (not NoCarrier) do begin
      EAdr:=TAbrufListObj (AbrufListe [i]).EAdr[1];
      if nur_ein_DE_abrufen then begin     { nur wenn Verbindung gehalten werden soll }
        Del_von:=CD_ALLG_eigInstanztyp;
        Del_bis:='';
      end
      else begin                           { Normalfall: alle Momentanwerte holen }
        Del_von:=TAbrufListObj (AbrufListe [i]).Del_von;
        Del_bis:=TAbrufListObj (AbrufListe [i]).Del_bis;
      end;

      { Ausgabe in Statuszeile: }
      if EAdr <> MerkEAdr then                                           { nur bei neuer Adresse eine neue ZustandMessage }
        ZustandMessage (COMPort, RufStammDaten.StationId, z_DatenelementeAbrufen,
                        ' (Adr: '+ EAdr+')', WithZustandTabelle);
      MerkEAdr:=EAdr;

      Antwort_vollstaendig:=false;
      while (not Antwort_vollstaendig) AND (not NoCarrier) do begin { evtl. mehrere Abrufe nötig }
        { Befehl zusammensetzen: }
        IncrementTID;                         
        Befehl:=Get_DatenelementeBefehl(EAdr, Del_von, Del_bis);

        { Momentanwerteabruf-Kommando senden }
        if not SendCommand (Befehl, FS, 1, Timeout_Datenelemente, ad_String, R, NoCarrier) then begin
          if not Keine_weiteren_Journaleintraege then
            FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          FehlerWarnung:=true;
          Break;     { weiter mit nächstem Telegramm (ab 23.05.2003 jetzt grundsätzlich bei allen Fehlern , WW) }
        end;
        if nur_ein_DE_abrufen then Break;

        { Rohstring prüfen, bei unvollständiger Antwort von-Del für Folgebefehl zurückgeben: }
        Erg:=CheckRohstring_Antworttelegramm(R.Antwort, EAdr, Antwort_vollstaendig, Del_von);
        if Erg <> DSFGKONVERR_OK then begin               { Fehler im Rohstring }
          if not Keine_weiteren_Journaleintraege then begin
            if (Erg >= C_MinCode_DSfGKonvErr_AA) then   { außerplanmäßige Antwort }
              FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_DE)  // 20.07.2004, WW
            else
              FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_INSTWERTKONV);
          end;
          FehlerWarnung:=true;
          Break;
        end;

        { Rohstring in Datenelementeliste (Zwischenpuffer) konvertieren: }
        Erg:=DELList.LoadFromRohstring (R.Antwort, EAdr);
        if Erg <> DSFGKONVERR_OK then begin
          if not Keine_weiteren_Journaleintraege then begin
            if (Erg >= C_MinCode_DSfGKonvErr_AA) then   { außerplanmäßige Antwort }
              FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_AUSSERPLANMAESSIGEANTW_DE)  // 20.07.2004, WW
            else
              FehlerGruppeCodeUpdate (ST_KONVERROR, SKERR_INSTWERTKONV);
          end;
          FehlerWarnung:=true;
          Break;
        end;
      end; { while not Antwort_vollstaendig }

      if nur_ein_DE_abrufen then Break;
      inc(i);
    end;  { while AbrufListe }

    if not nur_ein_DE_abrufen then begin                   { nur speichern, wenn ganze Abrufliste abgearbeitet wurde }
      { alle Momentanwerte aus Zwischenpuffer in Momentanwert-Tabelle schreiben: }
      TbDSfGMomentanDaten:=TTbDSfGMomentanDaten.Create (PathServer.PathName [WNetWorkDir], RufStammDaten.StationId);
      try
        TbDSfGMomentanDaten.EmptyMomTable;                   { Momentanwerte-Tabelle zuerst leeren }
        if TbDSfGMomentanDaten.OpenMomTable then begin
          try
            for i:=0 to DELList.Count-1 do begin
              Busadresse:=TDELListObj (DELList[i]).EAdr;
              DEL:=TDELListObj (DELList[i]).DEL;
              Wert:=TDELListObj (DELList[i]).Wert;
              TbDSfGMomentanDaten.WriteMomTable (Busadresse, DEL, Wert, length (Wert));
            end;
          finally
            TbDSfGMomentanDaten.CloseMomTable;
          end;
        end;
        { Neue Werte in der Tabelle: Triggerfile schreiben }
        WriteNewTime (TbDSfGMomentanDaten.tbDMom.DatabaseName + TbDSfGMomentanDaten.tbDMom.TableName);
      finally
        TbDSfGMomentanDaten.Free;
      end;
    end;
  finally
    DELList.Free;
  end;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_VerbindungSteht, '', WithZustandTabelle);

  { wenn in diesem Zyklus Fehler/Warnung aufgetreten ist, ab sofort keine Journal-Fehler/Warnungen mehr schreiben,
    sonst wird die Tabelle zugemüllt: }
  if FehlerWarnung then
    Keine_weiteren_Journaleintraege:=true;

  if not NoCarrier then
    Result:=true;
end;

{-----------------------------------------}
Procedure TAbruf.UebertragungDatenelemente;
{-----------------------------------------}
{ Datenelemente in DSfG-Instanzen übertragen }
Var
  Befehl: string;
  R: TRueckgabe;
  TbDSfGParametrierung: TTbDSfGParametrierung;
  ParaSendenData: ParaSendenDataRec;
  WertNeuSoll, WertNeuIst: string;
  ParaErgebnisData: ParaErgebnisDataRec;
  TbDSfGMomentanDaten: TTbDSfGMomentanDaten;

Begin
  TbDSfGParametrierung:=TTbDSfGParametrierung.Create (PathServer.PathName [WNetWorkDir],
                                                      RufStammDaten.StationId);
  try
    ZustandMessage (COMPort, RufStammDaten.StationId, z_DatenelementeUebertragen, '', WithZustandTabelle);
    while TbDSfGParametrierung.GetSendeParameter (ParaSendenData) do begin
      with ParaSendenData do begin
        if Stellen > 0 then
          Wert:=F_RightPad (Wert, ' ', Stellen);
        WertNeuSoll:=Wert;
        if EADR <> '' then
          Befehl:=Get_EinstellBefehl(EADR[1], DEL_Adr, WertNeuSoll, Zugangscode1, Zugangscode2)
        else
          Continue;
      end;

      { Datenelementeinstell-Kommando senden }
      if not SendCommand (Befehl, FS, 1, Timeout_Einstellen, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;

      { prüfen, ob Datenelement in der DSfG-Instanz tatsächlich geändert wurde: }
      if not DSfG_Datenelement_geaendert (ParaSendenData.EADR[1], ParaSendenData.DEL_Adr, WertNeuSoll,
                                          R.Antwort, WertNeuIst) then
        Continue;

      { Ergebnis der Übertragung in Parametriertabelle schreiben: }
      with ParaErgebnisData do begin
        EADR:=ParaSendenData.EADR;
        DEL_Adr:=ParaSendenData.DEL_Adr;
        WertNeu_Soll:=WertNeuSoll;
        WertNeu_Ist:=WertNeuIst;
      end;
      TbDSfGParametrierung.WriteErgebnis (ParaErgebnisData);

      { geänderten Parameter immer auch in Momentanwerttabelle aktualisieren: }
      TbDSfGMomentanDaten:=TTbDSfGMomentanDaten.Create (PathServer.PathName [WNetWorkDir], RufStammDaten.StationId);
      try
        TbDSfGMomentanDaten.UpdateMomTable (ParaSendenData.EADR, ParaSendenData.DEL_Adr, WertNeuIst);
      finally
        TbDSfGMomentanDaten.Free;
      end;

      { Stammdatenaktualisierung, wenn gefordert: }
      if ParaSendenData.StaAktu then begin
(*
        wohl nur Busadresse der Instanz !?!

        if ParaSendenData.ParaNr = CP_DFUE_PW1 then
          Stammdaten.SetPasswort (RufStammdaten.MrgId, 1, WertNeuIst);
        if ParaSendenData.ParaNr = CP_DFUE_PW2 then
          Stammdaten.SetPasswort (RufStammdaten.MrgId, 2, WertNeuIst);
        if ParaSendenData.ParaNr = CP_DFUE_PW3 then
          Stammdaten.SetPasswort (RufStammdaten.MrgId, 3, WertNeuIst);
        if ParaSendenData.ParaNr = CP_DFUE_PW4 then
          Stammdaten.SetPasswort (RufStammdaten.MrgId, 4, WertNeuIst);*)
      end;
    end; { while }
    { Triggerfile schreiben, wenn alle Parameter übertragen und Ergebnisse geschrieben wurden: }
    WriteNewTime(PathServer.PathName [WNetWorkDir] +
                 C_TbDDE + Format('%.4d.DB', [RufStammDaten.StationId]));
  finally
    TbDSfGParametrierung.Free;
  end;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_VerbindungSteht, '', WithZustandTabelle);
End;

{------------------------------------------------------------------------}
Function TAbruf.AbrufDfueMomentanwerte (AktuStammdaten: boolean): boolean;
{------------------------------------------------------------------------}
{ ruft alle DSfG-DFÜ-Daten (Parameter, NTY-Masken) ab und speichert die Werte in der zugehörigen
  Momentanwertetabelle;
  Übergabe: AktuStammdaten (wenn true, Werte der abgerufenen stammdaten-relevanten DSfG-DFÜ-Parameter
                            in den Stammdaten aktualisieren)
  Ergebnis: true, wenn DFÜ-Momentanwerteabruf erfolgreich (serielle Kommunikation) }
var
  R: TRueckgabe;
  Befehl: string;
  TbDSfGMomDfueDaten: TTbDSfGMomDfueDaten;
  DfueParaList: TDfueParaList;      { Zwischenpuffer für DFÜ-Parameter-Konvertierung }
  i: integer;
  ParaAdr: string;
  Wert: string;

begin
  Result:=false;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_DfueParameterAbrufen,'', WithZustandTabelle);

  DfueParaList:=TDfueParaList.Create;
  try
    { Daten abrufen, welche über Befehle nach DSfG-Leitfaden bereitgestellt werden: }
    { NTY-Masken: }
    Befehl:='A';
    if (Pos (Befehl, Dfue_Kommandos) > 0) OR (length (Dfue_Kommandos) = 0) then begin
      if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
        DfueParaList.LoadFromRohstring_A (R.Antwort, Befehl)
      else
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    end;

    { Busadresse, über die das Login erfolgte: }
    Befehl:='E';
    if (Pos (Befehl, Dfue_Kommandos) > 0) OR (length (Dfue_Kommandos) = 0) then begin
      if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
        DfueParaList.LoadFromRohstring_Allg (R.Antwort, Befehl)
      else
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    end;

    { Passwort, über die das Login erfolgte: }
    Befehl:='I';
    if (Pos (Befehl, Dfue_Kommandos) > 0) OR (length (Dfue_Kommandos) = 0) then begin
      if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
        DfueParaList.LoadFromRohstring_Allg (R.Antwort, Befehl)
      else
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    end;

    { Kennung: }
    Befehl:='K';
    if (Pos (Befehl, Dfue_Kommandos) > 0) OR (length (Dfue_Kommandos) = 0) then begin
      if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
        DfueParaList.LoadFromRohstring_Allg (R.Antwort, Befehl)
      else
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    end;

    { Rufnummer der Zentrale, über die das Login erfolgte: }
    Befehl:='R';
    if (Pos (Befehl, Dfue_Kommandos) > 0) OR (length (Dfue_Kommandos) = 0) then begin
      if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
        DfueParaList.LoadFromRohstring_Allg (R.Antwort, Befehl)
      else
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    end;

    if Extensionmode > 0 then begin       { zusätzliche mögliche Befehle im Erweiterungsgrad 1 }
      { Versionsdaten: }
      Befehl:='V';
      if (Pos (Befehl, Dfue_Kommandos) > 0) OR (length (Dfue_Kommandos) = 0) then begin
        if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
        if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
          DfueParaList.LoadFromRohstring_Allg (R.Antwort, Befehl)
        else
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      end;

      { Zeitinfos: }
      Befehl:='U';
      if (Pos (Befehl, Dfue_Kommandos) > 0) OR (length (Dfue_Kommandos) = 0) then begin
        if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
        if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
          DfueParaList.LoadFromRohstring_Allg (R.Antwort, Befehl)
        else
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      end;

      { DFÜ-Statistik: }
      Befehl:='D';
      if (Pos (Befehl, Dfue_Kommandos) > 0) OR (length (Dfue_Kommandos) = 0) then begin
        if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
        if ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
          DfueParaList.LoadFromRohstring_Allg (R.Antwort, Befehl)
        else
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      end;
    end;  { if Extensionmode > 0 }

   { weitere Daten, welche nur von Wieser-DSfG-DFÜs bereitgestellt werden:
     -> kein Journal-Eintrag bei ungültiger Antwort, da MRG910 nicht alle
        Wieser-Befehle unterstützt ! }
    if Hersteller = 'W' then begin
      { Wieser-Parameter: }
      Befehl:='YWB';
      if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      if ValidDSfGDfueAntwort (true, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
        DfueParaList.LoadFromRohstring_B (R.Antwort, Befehl);

      { Teilnehmerliste: }
      Befehl:='YWT';
      if not SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      if ValidDSfGDfueAntwort (true, R.Antwort, R.Fehlergruppe, R.Fehlercode) then
        DfueParaList.LoadFromRohstring_Allg (R.Antwort, Befehl);
    end;

    if AktuStammdaten then begin
      { Werte der stammdaten-relevanten DSfG-DFÜ-Parameter in den Stammdaten aktualisieren: }
      for i:=0 to DfueParaList.Count-1 do begin
        Befehl:=TDfueParaListObj (DfueParaList[i]).Befehl;
        ParaAdr:=TDfueParaListObj (DfueParaList[i]).ParaAdr;
        Wert:=TDfueParaListObj (DfueParaList[i]).Wert;
        AktuStammdatenByDfueParameter (Befehl, ParaAdr, Wert);
      end;
    end;

    { alle DFÜ-Momentanwerte aus Zwischenpuffer in DFÜ-Momentanwert-Tabelle schreiben: }
    TbDSfGMomDfueDaten:=TTbDSfGMomDfueDaten.Create (PathServer.PathName [WNetWorkDir], RufStammDaten.StationId);
    try
      TbDSfGMomDfueDaten.EmptyMomDfueTable;                   { DFÜ-Momentanwerte-Tabelle zuerst leeren }
      if TbDSfGMomDfueDaten.OpenMomDfueTable then begin
        try
          for i:=0 to DfueParaList.Count-1 do begin
            Befehl:=TDfueParaListObj (DfueParaList[i]).Befehl;
            ParaAdr:=TDfueParaListObj (DfueParaList[i]).ParaAdr;
            Wert:=TDfueParaListObj (DfueParaList[i]).Wert;
            TbDSfGMomDfueDaten.WriteMomDfueTable (Befehl, ParaAdr, Wert, length (Wert));
          end;
        finally
          TbDSfGMomDfueDaten.CloseMomDfueTable;
        end;
      end;
      { Neue Werte in der Tabelle: Triggerfile schreiben }
      WriteNewTime (TbDSfGMomDfueDaten.tbDMomDfue.DatabaseName + TbDSfGMomDfueDaten.tbDMomDfue.TableName);
    finally
      TbDSfGMomDfueDaten.Free;
    end;
  finally
    DfueParaList.Free;
  end;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_VerbindungSteht, '', WithZustandTabelle);

  if not NoCarrier then
    Result:=true;
end;

{--------------------------------------------}
Function TAbruf.DfueVerbindungHalten: boolean;
{--------------------------------------------}
{ Verbindung zur DSfG-DFÜ halten;
  Ergebnis: true, wenn Verbindung-Halten erfolgreich (serielle Kommunikation) }
var
  R: TRueckgabe;
  Befehl: string;

begin
  if CommComponent is TSerialDSfG then begin  { nur bei serieller DFÜ-Kommunikation notwendig }
    Result:=false;
    ZustandMessage (COMPort, RufStammDaten.StationId, z_VerbindungHalten,'', WithZustandTabelle);

    { um die Verbindung zu halten, wird der allgemeingültige E-Befehl (Busadresse) gesendet: }
    Befehl:='E';
    TSerialDSfG (CommComponent).SendCommand (STX+Befehl+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier);
    if not NoCarrier then
      Result:=true;
  end else
    Result:=true;

  ZustandMessage (COMPort, RufStammDaten.StationId, z_VerbindungSteht, '', WithZustandTabelle);
end;

{-----------------------------------------}
Procedure TAbruf.UebertragungDfueParameter;
{-----------------------------------------}
{ Parameter in DSfG-DFÜ-Instanz übertragen (echte Parameter, NTY-Masken) }
Var
  Befehl: string;
  R: TRueckgabe;
  TbDSfGDfueParametrierung: TTbDSfGDfueParametrierung;
  DfueParaSendenData: DfueParaSendenDataRec;
  DfueParaErgebnisData: DfueParaErgebnisDataRec;
  WertAlt, WertNeu: string;
  WertNeuSoll: string;
  isWieserBefehl: boolean;
  WertAltPos: integer;

Begin
  TbDSfGDfueParametrierung:=TTbDSfGDfueParametrierung.Create (PathServer.PathName [WNetWorkDir],
                                                              RufStammDaten.StationId);
  try
    ZustandMessage (COMPort, RufStammDaten.StationId, z_DFUEParameterUebertragen, '', WithZustandTabelle);
    while TbDSfGDfueParametrierung.GetSendeParameter (DfueParaSendenData) AND not NoCarrier do begin
      with DfueParaSendenData do begin
        if Stellen > 0 then
          Wert:=F_RightPad (Wert, ' ', Stellen);
        WertNeu:=Wert;
      end;

      Befehl:=Get_DfueEinstellbefehl (DfueParaSendenData.Befehl, DfueParaSendenData.ParaAdr, WertNeu);
      { Einstell-Kommando senden: }
      if not SendCommand (Befehl, ETX, 1, Timeout_Einstellen, ad_String, R, NoCarrier) then begin
        FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
        Continue;
      end;

      { Antwort auf "Einstell"-Kommando auswerten: }
      isWieserBefehl:=F_IsWieserBefehl (DfueParaSendenData.Befehl);
      if not ValidDSfGDfueAntwort (isWieserBefehl, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
        FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
        Continue;
      end;

      { Prüfen, ob Wert in der DSfG-DFÜ tatsächlich geändert wurde: }
      if DfueParaSendenData.Befehl = 'A' then begin        { Änderung einer NTY-Maske }
        WertNeuSoll:=WertNeu;
        if not DSfGDfueNTYMaske_geaendert (DfueParaSendenData.ParaAdr, WertNeuSoll, R.Antwort, WertNeu) then begin
          FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
          Continue;
        end;
      end
      else begin                                           { Änderung aller übrigen "Parameter" }
        { Stelle in der Antwort, an welcher der Alt-Wert beginnt, festlegen: }
        if DfueParaSendenData.Befehl = 'YWB' then          { Wieser-Parameter }
          WertAltPos:=6
        else
          WertAltPos:=3;
        if not DSfGDfueParameter_geaendert (WertAltPos, R.Antwort, WertAlt, WertNeu) then begin
          FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
          Continue;
        end;
      end;

      { Ergebnis der Übertragung in Parametriertabelle schreiben: }
      with DfueParaErgebnisData do begin
        Befehl:=DfueParaSendenData.Befehl;
        ParaAdr:=DfueParaSendenData.ParaAdr;
        if Befehl <> 'A' then
          ParaWertAlt:=WertAlt
        else
          ParaWertAlt:=WertNeuSoll;  { beim A-Befehl wird der AltWert als Soll-NeuWert interpretiert ! }
        ParaWertNeu:=WertNeu;
      end;
      TbDSfGDfueParametrierung.WriteErgebnis (DfueParaErgebnisData);

      { wenn gefordert, Stammdatenaktualisierung für die DSfG-DFÜ-Instanz: }
      if DfueParaSendenData.StaAktu then
        AktuStammdatenByDfueParameter (DfueParaSendenData.Befehl,
                                       DfueParaSendenData.ParaAdr,
                                       WertNeu);
    end; { while }

    { alle Parameter rücklesen und in der Momentanwert-Tabelle neu zur Verfügung stellen
      (keine Stammdaten-Aktualisierung): }
    AbrufDfueMomentanwerte (false);

    { Triggerfile schreiben, wenn alle DFÜ-Parameter übertragen und Ergebnisse geschrieben wurden: }
    WriteNewTime(PathServer.PathName [WNetWorkDir] +
                 C_Tb_DDfuePE + Format('%.4d.DB', [RufStammDaten.StationId]));
  finally
    TbDSfGDfueParametrierung.Free;
  end;
  ZustandMessage (COMPort, RufStammDaten.StationId, z_VerbindungSteht, '', WithZustandTabelle);
End;

{-----------------------------------------------------------------------------------------}
function TAbruf.GetBefehlFromBinaerfile (FileName: TFileName; var Befehl: string): boolean;
{-----------------------------------------------------------------------------------------}
{ Inhalt von FileName in Befehl schreiben
  Ergebnis: true, wenn File gelesen werden konnte }
var
  BF: file of char;
  c: char;
begin
  Result:=false;
  Befehl:='';
  AssignFile (BF, FileName);
  {$I-} Reset (BF); {$I+}
  if IOResult <> 0 then exit;
  while not Eof (BF) do begin
    Read (BF, c);
    Befehl:=Befehl + c;
  end;
  CloseFile (BF);
  Result:=true;
end;

{----------------------------------------}
function TAbruf.AbrufBinaerdatei: boolean;
{----------------------------------------}
{ Senden von in Binärdatei(en) enthaltenen Befehlen, ablegen der Antwort(en) in Rückgabedatei(en);
  Achtung: Befehle an Instanzen ohne STX, ETX, BCC ! Das STX-ETX-Format wird nur
           im Erweiterungsgrad 1 verwendet und automatisch von der TSerialDSfG-Klasse gebildet.
  Befehlsdatei: 1 Befehl je Datei, Inhalt = kompletter Befehlsstring,
                Dateiname: MBEFnnnn.???  (nnnn = StationsId, z.B. 0001; Extension beliebig)
  Antwortdatei: Dateiname: MANTnnnn.???
  Ergebnis: true, wenn Binärdateibruf ok }
Var
  R: TRueckgabe;
  Befehl: string;
  Pfad: TFileName;
  FileName: TFileName;
  DestFileName: TFileName;
  i: integer;
  SR: TSearchRec;
  Endezeichen: char;
  isWieserBefehl: boolean;

Begin
  Result:=false;
  if NoCarrier then exit;

  Pfad:=PathServer.PathName [WWorkDir];
  FileName:=C_Tb_DBinaerBefehl + Format('%.4d.*', [RufStammDaten.StationId]);

  Result:=true;
  i:= FindFirst (Pfad + FileName, faAnyFile, SR);
  while (i = 0) AND not NoCarrier do begin
    if GetBefehlFromBinaerfile (Pfad + SR.Name, Befehl) then begin
      ZustandMessage (COMPort, RufStammDaten.StationId, z_BinaerdateiAbrufen, '', WithZustandTabelle);
      { Endezeichen abhängig vom Befehl: }
      if length (Befehl) > 0 then
        Endezeichen:=Befehl [length (Befehl)]
      else
        Endezeichen:=FS;                                  { Standard-Belegung }

      { Binärdatei-Kommando senden }
      if SendCommand (Befehl, Endezeichen, 1, Timeout_Binaerdatei, ad_File, R, NoCarrier) then begin
        { Antwort auf Binärdateibefehl korrekt gelesen }
        FileName:=R.Antwort;
        { Antwort auf "Einstell"-Kommando auswerten: }
        isWieserBefehl:=F_IsWieserBefehl (Befehl);
        if ValidDSfGDfueAntwort (isWieserBefehl, FileName, R.Fehlergruppe, R.Fehlercode) then begin
          { weitere Aktionen, die bei bestimmten Befehlen zusätzlich ausgeführt werden: }
          if Befehl = STX+'YWP'+ETX then begin      { Befehl: DSfG-DFÜ-Standardparameter aktivieren }
            Check_DfueStandardparameterAktivierung;      { prüfen, ob Aktivierung erfolgreich }
            { alle Parameter rücklesen und in Momentanwert-Tabelle abspeichern
              (Stammdaten-Aktualisierung durchführen !): }
            AbrufDfueMomentanwerte (true);
          end;

          { Rohfile in Antwortfile umkopieren (überschreiben, falls schon vorhanden) }
          DestFileName:=Pfad + C_Tb_DBinaerAntwort + Format('%.4d', [RufStammDaten.StationId]) + ExtractFileExt (SR.Name);
          CopyFile (pchar (FileName), pchar (DestFileName), false);
          if RohdatenLoeschen then
            DeleteFile (FileName);
        end
        else begin
          FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
          Result:=false;
        end;
      end
      else begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        Result:=false;
      end;
    end;
    i:= FindNext (SR);
  end;  { while }
  FindClose (SR);
End;

{--------------------------------------------------------------}
function TAbruf.Check_DfueStandardparameterAktivierung: boolean;
{--------------------------------------------------------------}
{ nach dem Aktivieren der Standardparameter in der Wieser-DSfG-DFÜ (YWP-Befehl)
  muß geprüft werden, ob die Fehlerliste gelöscht wurde. Wenn nicht (Fehler in
  frühen Versionen): Warmstart in der DSfG-DFÜ auslösen !
  Ergebnis: true, wenn Prüfungsablauf ohne Fehler }
var
  R: TRueckgabe;

begin
  Result:=false;
  { Teilnehmerliste abrufen: }
  if SendCommand (STX+'YWT'+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier) then begin
    if ValidDSfGDfueAntwort (true, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
      Result:=true;
      if length (R.Antwort) > 3 then  { Teilnehmerliste ist nicht gelöscht -> DSfG-DFÜ warmstarten }
        SendCommand (STX+'YWW'+ETX, ETX, 1, Timeout_DFUEParameter, ad_String, R, NoCarrier);
    end else
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
  end else
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
end;

{---------------------------------------------------------------------------------------------}
procedure TAbruf.AktuStammdatenByDfueParameter (Befehl: string; ParaAdr: string; Wert: string);
{---------------------------------------------------------------------------------------------}
{ Stammdaten aktualisieren mit DSfG-DFÜ-Parameterwert
  Übergabe: DSfG-DFÜ-Befehl
            ParaAdr ("Parameter-Adresse" wie in DSfG-DFÜ-Momentanwerttabelle definiert)
            neuer Wert }
var
  ParaNr: integer;

begin
  if Befehl = 'K' then
    Stammdaten.SetLoginDFUEKennung (RufStammdaten.StationId, Wert)
  else if Befehl = 'I' then
    Stammdaten.SetLoginDFUELoginPasswort (RufStammdaten.StationId, Wert)
  else if Befehl = 'E' then
    Stammdaten.SetLoginDFUELoginAdresse (RufStammdaten.StationId, Wert)

  else if Befehl = 'YWB' then begin
    ParaNr:=StrToInt (ParaAdr);
   if ParaNr = CDDfu_Kennung then
      Stammdaten.SetLoginDFUEKennung (RufStammdaten.StationId, Wert)
    else if ParaNr = CDDfu_Adresse1 then
      Stammdaten.SetLoginDFUEAdresse (RufStammdaten.StationId, 1, Wert)
    else if ParaNr = CDDfu_Adresse2 then
      Stammdaten.SetLoginDFUEAdresse (RufStammdaten.StationId, 2, Wert)
    else if ParaNr = CDDfu_Adresse3 then
      Stammdaten.SetLoginDFUEAdresse (RufStammdaten.StationId, 3, Wert)
    else if ParaNr = CDDfu_Adresse4 then
      Stammdaten.SetLoginDFUEAdresse (RufStammdaten.StationId, 4, Wert)
    else if ParaNr = CDDfu_Identifikation1 then
      Stammdaten.SetLoginDFUEPasswort (RufStammdaten.StationId, 1, Wert)
    else if ParaNr = CDDfu_Identifikation2 then
      Stammdaten.SetLoginDFUEPasswort (RufStammdaten.StationId, 2, Wert)
    else if ParaNr = CDDfu_Identifikation3 then
      Stammdaten.SetLoginDFUEPasswort (RufStammdaten.StationId, 3, Wert)
    else if ParaNr = CDDfu_Identifikation4 then
      Stammdaten.SetLoginDFUEPasswort (RufStammdaten.StationId, 4, Wert);
  end;
end;

{-----------------------------------------}
function TAbruf.RufAnnehmen_Modem: boolean;
{-----------------------------------------}
{ angekommenen Ruf per Modem annehmen;
  Ergebnis: true, wenn Rufannahme ok }
var
  Befehl: string;
  R: TRueckgabe;
begin
  if CommComponent is TSerialDSfG then begin   { logischerweise nur bei serieller DFÜ-Kommunikation }
    Result:=false;
    TSerialDSfG (CommComponent).SetDCDCheck (false);   { DCD-Überwachung aus }
    Befehl:='ata' + CR;
    { Rufannahmekommando senden: }
    if not TSerialDSfG (CommComponent).SendModemCommand (Befehl, Timeout_RufAnnahme, R) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    FormMainDSfGAbruf.ModemstatusMessage (Trim (R.Antwort));
    { Antwort auf Verbindungsaufbau-Kommando auswerten: }
    if not CheckModemConnect (R.Antwort) then exit;

    TSerialDSfG (CommComponent).SetDCDCheck (true);    { DCD-Überwachung ein }
  end;
  Result:=true;
end;

{----------------------------------}
function TAbruf.RufAnnahme: boolean;
{----------------------------------}
{ angekommenen Ruf annehmen;
  Ergebnis: true, wenn Rufannahme ok }
var
  dummy: boolean;
Begin
  Result:=false;
  NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
  FehlerGruppeCodeUpdate (0, 0);                            { Vorbelegung: Ok }
  FormMainDSfGAbruf.ModemstatusMessage ('');

  ZustandMessage (COMPort, -1, z_RufAnnehmen, '', WithZustandTabelle);
  { Journal-Eintrag für die bislang nicht identifizierte, rufende Station (StationId = 0, Kennung = '': }
  JournalId:=WriteNewDSfGJournal (0, '', GetAbrufartString,
                                  SystemDaten.RufDatentypen, COMPort);

  if not RufAnnehmen_Modem then begin
    { Trigger-Datei für RE-Client schreiben }
    WriteNewTime(Pathserver.Pathname [WStammDir] + C_RufTriggerFile);
    exit;
  end;

  { Ruf wurde entgegengenommen, Verbindung steht jetzt: }
  NoCarrier:=false;
  FormMainDSfGAbruf.SetStatusColor (ps_VerbindungSteht);
  ZustandMessage (COMPort, -1, z_VerbindungSteht, '', WithZustandTabelle);
  UpdateDSfGJournal (JournalId, C_WJournal_DZVerbSteht);
  Delay (200);       { Wartezeit (generell empfehlenswert, nötig für MRG 910) }

  if not KennungAbfragen then begin
    { Trigger-Datei für RE-Client schreiben }
    WriteNewTime(Pathserver.Pathname [WStammDir] + C_RufTriggerFile);
    exit;
  end;

  { Stammsatz für DSfG-Kennung suchen: }
  if not Stammdaten.GetRufStammdatenByKennung (DSfGKennung,
                                               RufStammdaten,
                                               Kennung_in_Stammdaten_eindeutig) then begin
    { nicht gefunden: Geräte-Kennung ins Journal schreiben }
    UpdateDSfGJournalKennung (JournalId, DSfGKennung);
    FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_KEINSTAMMSATZ);
    { Trigger-Datei für RE-Client schreiben }
    WriteNewTime(Pathserver.Pathname [WStammDir] + C_RufTriggerFile);
    exit;
  end;

  { ab jetzt sind Ruf-Stammdaten der anrufenden DSfG-Station bekannt. Die Kennung
    kann in den Stammdaten jedoch mehrfach vorhanden sein ! }
  DSfGKennung:=RufStammdaten.Kennung;
  KennungMessage (RufStammDaten.Kennung);
  StationMessage (RufStammDaten.Stationsname);

  { bei nicht eindeutiger Kennung in den Stammdaten
    -> Journal: Station unbekannt, Warnung "Kennung existiert mehrfach"
    -> Abruf läuft weiter (jedoch keine Archivdaten-Abfrage, da die Daten einer
       Station nicht eindeutig zugeordnet werden könnten) }
  if Kennung_in_Stammdaten_eindeutig then
    UpdateDSfGJournalStationId (JournalId, RufstammDaten.StationId)
  else
    FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_MEHRFACH_NODATA);
  UpdateDSfGJournalKennung (JournalId, DSfGKennung);

  { Trigger-Datei für RE-Client schreiben }
  WriteNewTime(Pathserver.Pathname [WStammDir] + C_RufTriggerFile);

  { Paßwort übertragen: }
  if not PasswortLogin (dummy) then exit;

  { Rufnummer der Zentrale in der DSfG-DFÜ deaktivieren, wenn in Systemdaten
    eingestellt und max. Anzahl der Anrufe überschritten: }
  if Systemdaten.MaxAnrufversuche_Station > 0 then
    RufDeaktivierung;

  { Wieser-DSfG-DFÜ: aktive Teilnehmer ermitteln }
  if Hersteller = 'W' then
    if not GetAktiveBusteilnehmer then exit;

  { DSfG-DFÜ transparent schalten und spontane Aufmerksamkeits-Telegramme lesen: }
  if not DFUETransparentSchalten then exit;
  Result:=true;
End;

{--------------------------------}
procedure TAbruf.RufDeaktivierung;
{--------------------------------}
{ Prüfen, ob Anzahl der eingegangenen Rufe einer Station das Maximum (Systemdaten-Einstellung)
  erreicht hat. Wenn ja, dann Parameter "Rufnummer Zentrale" in der DSfG-DFÜ auf "N" setzen }
var
  R: TRueckgabe;
  Befehl: string;
  WRufeDB: TWRufeDB;
  RufNrAlt, RufNrNeu: string;
  WertAltPos: integer;

begin
  if NoCarrier then exit;

  { Ruf in Tabelle protokollieren und Prüfung, ob Rufanzahl erreicht: }
  WRufeDB:=TWRufeDB.Create (PathServer.PathName [WStammDir]);
  try
    { exit, wenn Rufanzahl noch nicht erreicht (kein Fehler): }
    if WRufeDB.WriteRuf (C_GerArtDSfG, RufStammdaten.StationId,
                         Systemdaten.MaxAnrufversuche_Station) then exit;
  finally
    WRufeDB.Free;
  end;

  { "Rufnummer Zentrale" in der DSfG-DFÜ auf "N" parametrieren: }

  ZustandMessage (COMPort, RufStammDaten.StationId, z_RufDeaktivieren, '', WithZustandTabelle);

  RufNrNeu:='N';
  Befehl:=STX + 'R' + RufNrNeu + ETX;

  { Kommando für "Rufnummer Zentrale übertragen" senden }
  if not SendCommand (Befehl, ETX, 1, Timeout_Einstellen, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  if not ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Prüfen, ob die Rufnummer der Zentrale in der DSfG-DFÜ tatsächlich geändert wurde: }
  WertAltPos:=3;         { Stelle in der Antwort, an welcher der Alt-Wert beginnt }
  if not DSfGDfueParameter_geaendert (WertAltPos, R.Antwort, RufNrAlt, RufNrNeu) then begin
    FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR, RUFDEAKTERR_NOSUCCESS);
    exit;
  end;

  FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR, RUFDEAKTERR_SUCCESS);

  { erfolgreiche Rufdeaktivierung und Rufnummer in Tabelle protokollieren: }
  WRufeDB:=TWRufeDB.Create (PathServer.PathName [WStammDir]);
  try
    WRufeDB.WriteRufDeaktivierung (C_GerArtDSfG, RufStammdaten.StationId, RufNrAlt);
  finally
    WRufeDB.Free;
  end;
end;

{----------------------------------------}
function TAbruf.RufReaktivierung: boolean;
{----------------------------------------}
{ Parameter "Rufnummer Zentrale" in der DSfG-DFÜ zurück auf alten Wert setzen;
  Ergebnis: true, wenn Rufreaktivierung erfolgreich }
var
  R: TRueckgabe;
  Befehl: string;
  WRufeDB: TWRufeDB;
  RufNrAlt, RufNrNeu: string;
  RufNrMerk: string;
  erg: boolean;
  WertAltPos: integer;

begin
  Result:=false;
  if NoCarrier then exit;

  ZustandMessage (COMPort, RufStammDaten.StationId, z_RufReaktivieren, '', WithZustandTabelle);

  { Rufnummer aus Tabelle lesen: }
  WRufeDB:=TWRufeDB.Create (PathServer.PathName [WStammDir]);
  try
    if not WRufeDB.GetRufnummer (C_GerArtDSfG, RufStammdaten.StationId, RufNrNeu) then begin
      FehlerGruppeCodeUpdate (ST_RUFREAKTERROR, RUFREAKTERR_FINDRUFNR);
      exit;
    end;
  finally
    WRufeDB.Free;
  end;

  Befehl:=STX + 'R' + RufNrNeu + ETX;
  { Kommando für "Rufnummer Zentrale übertragen" senden }
  if not SendCommand (Befehl, ETX, 1, Timeout_Einstellen, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  if not ValidDSfGDfueAntwort (false, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Prüfen, ob Rufnummer in der DSfG-DFÜ tatsächlich geändert wurde. Es ist möglich, daß
    die neu zu parametrierende Rufnummer bereits im Gerät eingestellt ist.
    Die Rufreaktivierung ist in diesem Fall ok ! }
  RufNrMerk:=RufNrNeu;         { Rufnummer merken, die in der DSfG-DFÜ parametriert werden soll }

  WertAltPos:=3;         { Stelle in der Antwort, an welcher der Alt-Wert beginnt }
  erg:=DSfGDfueParameter_geaendert (WertAltPos, R.Antwort, RufNrAlt, RufNrNeu);
  if not erg AND (RufNrNeu <> RufNrMerk) then begin
    FehlerGruppeCodeUpdate (ST_RUFREAKTERROR, RUFREAKTERR_NOSUCCESS);
    exit;
  end;

  { erfolgreiche Rufreaktivierung in WRufe-Tabelle protokollieren: }
  WRufeDB:=TWRufeDB.Create (PathServer.PathName [WStammDir]);
  try
    WRufeDB.WriteRufReaktivierung (C_GerArtDSfG, RufStammdaten.StationId);
  finally
    WRufeDB.Free;
  end;

  Result:=true;
end;

End.

