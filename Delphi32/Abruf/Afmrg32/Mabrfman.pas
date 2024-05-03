{******************************************************************************}
{* Unit: Steuerung des MRG-Abrufs (Abrufmanager)                              *}
{* 07.12.1998 WW                                                              *}
{  12.02.2001 GD  erweitert um automatischen Export                            }
{******************************************************************************}
unit MAbrfMan;

INTERFACE

Uses
  Forms, SysUtils, DBTables, MDBSta, MDBAbruf, MObjKonv, WSysCon, MAbruf, AbrfInfo,
  MZustand, MDBMrg, MAbrfZB, JournlDB, PathIni, MSysDat, MObjParaDB, DB_Attn, WTables,
  MSysCon, RufeDB, TbMrgPE, MP_Allg, ZustndDb, AuftrgDb, SerMRG, MJournal, MFilenam,
  MObjMeld, T_Zeit, AsciiExportDLL, ExportSkript, MeldungenDB;

type

  { abstraktes Objekt zur Steuerung der Abruf-Dynamik }

  TAbrufManager = class (TObject)
  protected
    AktListenIndex: integer;          { enthält die gerade bearbeitete MRG-ID }
    GerArt: TDSFGTyp;              { Geräteart: Master, Slave, normales Gerät }
    AbrufKonvert: TAbrufKonvert;
    AbrufZeitbereich: TAbrufZeitbereich;
    WeitererAbruf: boolean;     { zur Steuerung des Messwertabrufs bei Slaves }
    Abruf: TAbruf;                                { Objekt zum MRG-Datenabruf }
    Abrufart: TAbrufart;
    Stammdaten: TMRGStammdaten;
    function ParameterAbrufen: boolean; virtual;
    function MeldungenAbrufen: boolean; virtual;
    function MesswerteAbrufen: boolean; virtual;
    function PruefsaetzeAbrufen: boolean; virtual;
    function BinaerdateiAbrufen: boolean; virtual;
    function ParameterNoetig: boolean;
    procedure VerbindungSteht; virtual;
    procedure Verbindungsfehler;
    procedure VerbindungBeenden;
    procedure VerbindungIstBeendet; virtual;
    procedure ParameterAbruf (Nummer: string); virtual;
    procedure ParameterAbrufFehler;
    procedure ParameterGelesen;
    procedure Meldungsabruf;
    procedure MeldungsabrufFehler;
    procedure MeldungenGelesen;
    procedure RundpufferRuecksetzen_Meldungen; virtual;
    procedure StundensaetzeAbruf;
    procedure StundensaetzeAbrufFehler;
    procedure StundensaetzeGelesen;
    procedure TagessaetzeAbruf;
    procedure TagessaetzeAbrufFehler;
    procedure TagessaetzeGelesen; virtual;
    procedure RundpufferRuecksetzen_Stundensaetze; virtual;
    function CheckUmschalten: boolean;
    procedure Umschalten;
    procedure UmschaltungFehler; virtual;
    procedure UmschaltungErfolgt; virtual;
    procedure PruefsaetzeAbruf;
    procedure PruefsaetzeAbrufFehler;
    procedure PruefsaetzeGelesen;
    procedure RundpufferRuecksetzen_Pruefsaetze; virtual;
    procedure BinaerdateiAbruf;
    procedure BinaerdateiAbrufFehler;
    procedure BinaerdateiGelesen;
    procedure ZeitSynchronisieren; virtual;
    procedure RufDeaktivieren; virtual;
    procedure Konvertieren;
    procedure JournalReorganisieren;
    function LoescheManuDaten (AMrgId: TMrgId): boolean;
  public
    AbrufListe: TAbrufListe;   { enthält Infos über alle abzurufenden MRGs (incl. Slaves !)
                                 und nach erfolgtem Abruf die Abrufstati (Ok/Fehler) }
    constructor Create (ASerialMRG: TSerialMRG; AAbrufart: TAbrufart; AModemRuf: integer);
    destructor Destroy; override;
    procedure Abrufen;
  end;


  { Abruf-Manager für automatischen und manuellen Abruf }

  TAutoManuAbrufManager = class (TAbrufManager)
  protected
    procedure SetAbrufListe (AAbrufListe: TAbrufListe);
    function ParameterAbrufen: boolean; override;
    function MeldungenAbrufen: boolean; override;
    function MesswerteAbrufen: boolean; override;
    function PruefsaetzeAbrufen: boolean; override;
    function BinaerdateiAbrufen: boolean; override;
    procedure RundpufferRuecksetzen_Meldungen; override;
    procedure RundpufferRuecksetzen_Stundensaetze; override;
    procedure RundpufferRuecksetzen_Pruefsaetze; override;
    procedure ZeitSynchronisieren; override;
  public
    constructor Create (ASerialMRG: TSerialMRG; AAbrufart: TAbrufart;
                        AAbrufListe: TAbrufListe; AModemRuf: integer);
  end;


  { Abruf-Manager für Rufentgegenahme }

  TRufManager = class (TAbrufManager)
  protected
    function ParameterAbrufen: boolean; override;
    function MeldungenAbrufen: boolean; override;
    function MesswerteAbrufen: boolean; override;
    function PruefsaetzeAbrufen: boolean; override;
    procedure VerbindungSteht; override;
    procedure SetRufListe (RufListe: string);
    procedure RuflisteAbfragen;
    procedure TagessaetzeGelesen; override;
    procedure RufDeaktivieren; override;
  public
    procedure RufEntgegennehmen;
  end;


  { Abruf-Manager für Momentanwerteabruf }

  TMomentanAbrufManager = class (TAbrufManager)
  protected
    procedure SetAbrufMrg (AMrgId: TMrgId);
    procedure ParameterAbruf (Nummer: string); override;
    procedure VerbindungIstBeendet; override;
    procedure DeleteMomTables;
    procedure MRGAbrufTableAufraeumen;
  public
    constructor Create (ASerialMRG: TSerialMRG; AMrgId: TMrgId; AModemRuf: integer);
    destructor Destroy; override;
  end;


  { Abruf-Manager für Rufreaktivierung }

  TRufReaktManager = class (TAbrufManager)
  protected
    procedure SetRufReaktMrg (AMrgId: TMrgId);
    procedure VerbindungSteht; override;
    procedure UmschaltungErfolgt; override;
    procedure Reaktivieren;
    procedure VerbindungIstBeendet; override;
  public
    constructor Create (ASerialMRG: TSerialMRG; AMrgId: TMrgId; AModemRuf: integer);
    destructor Destroy; override;
  end;


  { Abruf-Manager für Rückrufauslösung }

  TRueckRufManager = class (TAbrufManager)
  protected
    Zentrale: integer;
    procedure SetRueckRufMrg (AMrgId: TMrgId);
    procedure VerbindungSteht; override;
    procedure UmschaltungErfolgt; override;
    procedure RueckrufAusloesen;
    procedure VerbindungIstBeendet; override;
  public
    constructor Create (ASerialMRG: TSerialMRG; AMrgId: TMrgId; AZentrale: integer;
                        AModemRuf: integer);
  end;


IMPLEMENTATION

{ TAbrufManager }

{------------------------------------------------------------------------------}
constructor TAbrufManager.Create (ASerialMRG: TSerialMRG; AAbrufart: TAbrufart;
                                  AModemRuf: integer);
{------------------------------------------------------------------------------}
begin
  inherited Create;
  Abrufart:=AAbrufart;
  GerArt := DSFGNO;
  WeitererAbruf := false;

  FillChar (AbrufListe, SizeOf (TAbrufListe), 0);             { Vorbelegung für AbrufListe }
  AktListenIndex := 1;

  Abruf:=TAbruf.Create (ASerialMRG, Abrufart, AModemRuf);
  AbrufZeitbereich := TAbrufZeitbereich.Create;
  AbrufKonvert := TAbrufKonvert.Create (Abrufart);

  Stammdaten:=TMRGStammdaten.Create (PathServer.PathName [WStammDir]);
  Stammdaten.InitTabellen;

  { Einträge in Zustandstabelle vor dem Abruf sicherheitshalber löschen: }
  DeleteZustaende (Abruf.SerialMRG.COMPort);
end;

{-------------------------------}
destructor TAbrufManager.Destroy;
{-------------------------------}
begin
  JournalReorganisieren;
  { Einträge in Zustandstabelle nach Abruf wieder löschen: }
  DeleteZustaende (Abruf.SerialMRG.COMPort);

  Stammdaten.Free;
  AbrufKonvert.Free;
  AbrufZeitbereich.Free;
  Abruf.Free;
  inherited Destroy;
end;

{-----------------------------------------------}
function TAbrufManager.ParameterAbrufen: boolean;
{-----------------------------------------------}
begin
  Result:=false;
end;

{-----------------------------------------------}
function TAbrufManager.MeldungenAbrufen: boolean;
{-----------------------------------------------}
begin
  Result:=false;
end;

{-----------------------------------------------}
function TAbrufManager.MesswerteAbrufen: boolean;
{-----------------------------------------------}
begin
  Result:=false;
end;

{-------------------------------------------------}
function TAbrufManager.PruefsaetzeAbrufen: boolean;
{-------------------------------------------------}
begin
  Result:=false;
end;

{-------------------------------------------------}
function TAbrufManager.BinaerdateiAbrufen: boolean;
{-------------------------------------------------}
begin
  Result:=false;
end;

{----------------------------------------------}
function TAbrufManager.ParameterNoetig: boolean;
{----------------------------------------------}
{ Ergebnis: true, wenn der Parameterabruf lt. MRG-Systemdatentabelle nötig ist (unabhängig
            von den gewünschten Datentypen des Abrufs) }
begin
  Result:=GetAbrufParameterDb (Abruf.RufStammdaten.MrgTyp);
end;

{------------------------------}
procedure TAbrufManager.Abrufen;
{------------------------------}
var
  MasterId: TMrgId;
  MrgId: TMrgId;
  dummy: char;
  Datentypen: integer;
  i: integer;
  MrgTyp: TMrgTyp;

begin
  if AbrufListe [AktListenIndex].StationId = 0 then exit;
  AbrufListe [AktListenIndex].Erfolgreich := true;

  for i:=Low (AbrufListe) to High (Abrufliste) do begin
    { Parameterabruf für EC694 vorerst gesperrt: }
    MrgTyp:=Stammdaten.GetMrgTyp (AbrufListe [i].StationId);
    if ((AbrufListe [i].Datentypen AND C_IsParameter) <> 0) AND (MrgTyp = mrgtyp_EC694) then
      AbrufListe [i].Datentypen:=AbrufListe [i].Datentypen AND NOT C_IsParameter;

    { bestehende manuelle Daten des MRG vor dem Abruf löschen }
    if (Abrufart = aa_manuell) AND (AbrufListe [i].StationId > 0) then
      LoescheManuDaten (AbrufListe [i].StationId);
  end;

  GerArt := StammDaten.DSfGTyp (AbrufListe [AktListenIndex].StationId, dummy, MasterId);
  if GerArt = DSFGSLAVE then begin          { nur Slave soll abgerufen werden }
    MrgId := MasterId;                  { Verbindungsaufbau läuft über Master }
    Datentypen := 0;
  end
  else begin
    MrgId := AbrufListe [AktListenIndex].StationId;
    Datentypen:= AbrufListe [AktListenIndex].Datentypen;
  end;

  if Abruf.VerbAufbau (MrgId, Datentypen, AbrufListe [AktListenIndex].Keine_weiteren_Versuche) then
    VerbindungSteht                    { Verbindungsaufbau ok }
  else begin
    { Fehler beim Verbindungsaufbau (Verbindung steht) bzw. immer bei FUP-Abruf: }
    if not Abruf.NoCarrier OR Abruf.isFupAbruf then
      Verbindungsfehler
    else begin                          { Fehler beim Verbindungsaufbau, Verbindung steht nicht }
      UpdateMRGJournal (Abruf.JournalId, C_WJournal_DZVerbEnde);
      AbrufListe [AktListenIndex].Erfolgreich := false;
    end;
  end;
end;

{--------------------------------------}
procedure TAbrufManager.VerbindungSteht;
{--------------------------------------}
begin
  if GerArt = DSFGSLAVE then                { nur Slave soll abgerufen werden }
    Umschalten      { Verbindung zum Master steht, jetzt auf Slave umschalten }
  else
    ParameterAbruf ('0');                              { alle Parameter holen } 
end;

{----------------------------------------}
procedure TAbrufManager.VerbindungBeenden;
{----------------------------------------}
begin
  AbrufZeitbereich.ResetAbrufRecord;
  Abruf.VerbAbbau;
  VerbindungIstBeendet;
end;

{-------------------------------------------}
procedure TAbrufManager.VerbindungIstBeendet;
{-------------------------------------------}
begin
  Konvertieren;
end;

{----------------------------------------}
procedure TAbrufManager.Verbindungsfehler;
{----------------------------------------}
begin
  AbrufListe [AktListenIndex].Erfolgreich := false;
  VerbindungBeenden;
end;

{------------------------------------------------------}
procedure TAbrufManager.ParameterAbruf (Nummer: string);
{------------------------------------------------------}
begin
  if ParameterAbrufen OR ParameterNoetig then begin
    if Abruf.AbrufParameter (Nummer, false) then
      ParameterGelesen
    else
      ParameterAbrufFehler;
  end else
    ParameterGelesen;
end;

{---------------------------------------}
procedure TAbrufManager.ParameterGelesen;
{---------------------------------------}
begin
  if (ParameterAbrufen OR ParameterNoetig) and AbrufListe [AktListenIndex].Erfolgreich then begin
    AbrufKonvert.AddParameter (AbrufListe [AktListenIndex].StationId,
                               Abruf.RufStammdaten.MrgTyp,
                               Abruf.ParameterListe,
                               Abruf.JournalId,
                               ParameterAbrufen);
  end;
  MeldungsAbruf;
end;

{-------------------------------------------}
procedure TAbrufManager.ParameterAbrufFehler;
{-------------------------------------------}
begin
  AbrufListe [AktListenIndex].Erfolgreich := false;
  MeldungsAbruf;
end;

{------------------------------------}
procedure TAbrufManager.Meldungsabruf;
{------------------------------------}
var
  Modus: byte;
  von, bis: TDateTime;
begin
  if MeldungenAbrufen then begin
    { abzurufenden Datenbereich festlegen: }
    von:=AbrufListe [AktListenIndex].DatenVon;
    bis:=AbrufListe [AktListenIndex].DatenBis;
    if Abrufart = aa_manuell then begin
      if von = 0 then
        Modus:=d_alles
      else
        Modus:=d_vonbis;
    end else                                               { automatisch, Ruf }
      Modus:=d_neue;

    if Abruf.AbrufMeldungen (Modus, von, bis) then
      MeldungenGelesen
    else
      MeldungsAbrufFehler;
  end else
    MeldungenGelesen;
end;

{------------------------------------------}
procedure TAbrufManager.MeldungsabrufFehler;
{------------------------------------------}
begin
  AbrufListe [AktListenIndex].Erfolgreich := false;
  StundensaetzeAbruf;
end;

{---------------------------------------}
procedure TAbrufManager.MeldungenGelesen;
{---------------------------------------}
begin
  if MeldungenAbrufen and AbrufListe [AktListenIndex].Erfolgreich then
    AbrufKonvert.AddFileName (AbrufListe [AktListenIndex].StationId,
                              Abruf.RufStammdaten.MrgTyp,
                              Abruf.MRGAbrufrec [dt_Meldungen].Filename,
                              dt_Meldungen,
                              Abruf.JournalId);
  RundpufferRuecksetzen_Meldungen;
  StundensaetzeAbruf;
end;

{------------------------------------------------------}
procedure TAbrufManager.RundpufferRuecksetzen_Meldungen;
{------------------------------------------------------}
{ Dummy-Procedure }
begin
end;

{-----------------------------------------}
procedure TAbrufManager.StundensaetzeAbruf;
{-----------------------------------------}
var
  TempAbrufRec: TMRGAbrufRec;
  Modus: byte;
  von, bis: TDateTime;
begin
  if MesswerteAbrufen then begin
    { MRG-Abrufrecord für Meßwerte belegen }
    if not AbrufZeitbereich.AbrufRecordGesetzt then begin
      { abzurufenden Datenbereich festlegen: }
      von:=AbrufListe [AktListenIndex].DatenVon;
      bis:=AbrufListe [AktListenIndex].DatenBis;
      if Abrufart = aa_manuell then begin
        if von = 0 then
          Modus:=d_alles
        else
          Modus:=d_vonbis;
      end else
        Modus:=d_neue;                                     { automatisch, Ruf }
      Abruf.CreateMesswerteAbrufRec (Modus, von, bis);

      TempAbrufRec := Abruf.MRGAbrufRec [dt_Messwerte];
      AbrufZeitbereich.SetzeAbrufRecord (TempAbrufRec);
      AbrufZeitbereich.SetMrgID (AbrufListe [AktListenIndex].StationId);
    end;
    { in TempAbrufRec wird für DSfG-Slaves der nächste DSfG-1-Tages-AbrufRec
      zurückgeliefert, ansonsten der von CreateMesswerteAbrufRec erzeugte: }
    WeitererAbruf := AbrufZeitbereich.GetNextAbrufZeitbereich (TempAbrufRec);
    Abruf.MRGAbrufRec [dt_Messwerte] := TempAbrufRec;

    { bei Geräten, bei denen für jeden Kanal ein eigener Abrufbefehl nötig ist, wird
      auf den nächsten Kanal weitergeschaltet: }
    if not WeitererAbruf then
      WeitererAbruf:=Abruf.SetNextStdAbrufKanal;

    if Abruf.AbrufStdWerte then
      StundensaetzeGelesen
    else
      StundensaetzeAbrufFehler;
  end else
    StundensaetzeGelesen;
end;

{-----------------------------------------------}
procedure TAbrufManager.StundensaetzeAbrufFehler;
{-----------------------------------------------}
begin
  AbrufListe [AktListenIndex].Erfolgreich := false;
  TagessaetzeAbruf;
end;

{-------------------------------------------}
procedure TAbrufManager.StundensaetzeGelesen;
{-------------------------------------------}
begin
  if MesswerteAbrufen and AbrufListe [AktListenIndex].Erfolgreich then begin
    AbrufKonvert.AddFileName (AbrufListe [AktListenIndex].StationId,
                              Abruf.RufStammdaten.MrgTyp,
                              Abruf.MRGAbrufrec [dt_messwerte].Filename,
                              dt_Messwerte,
                              Abruf.JournalId);
    if WeitererAbruf then        { falls Intervallabruf für einen Slave noch nicht komplett }
      StundensaetzeAbruf
    else
      TagessaetzeAbruf;
  end else
    TagessaetzeGelesen;
end;

{---------------------------------------}
procedure TAbrufManager.TagessaetzeAbruf;
{---------------------------------------}
var
  Modus: byte;
  von, bis: TDateTime;
begin
  if GetAbrufTagessatzDb (Abruf.RufStammdaten.MrgTyp) then begin
    { abzurufenden Datenbereich festlegen: }
    von:=AbrufListe [AktListenIndex].DatenVon;
    bis:=AbrufListe [AktListenIndex].DatenBis;
    if Abrufart = aa_manuell then begin
      if von = 0 then
        Modus:=d_alles
      else
        Modus:=d_vonbis;
    end else
      Modus:=d_neue;                                       { automatisch, Ruf }

    if Abruf.AbrufTagwerte (Modus, von, bis) then
      TagessaetzeGelesen
    else
      TagessaetzeAbrufFehler;
  end else
    TagessaetzeGelesen;
end;

{---------------------------------------------}
procedure TAbrufManager.TagessaetzeAbrufFehler;
{---------------------------------------------}
begin
  AbrufListe [AktListenIndex].Erfolgreich := false;
  TagessaetzeGelesen;
end;

{-----------------------------------------}
procedure TAbrufManager.TagessaetzeGelesen;
{-----------------------------------------}
begin
  if MesswerteAbrufen AND GetAbrufTagessatzDb (Abruf.RufStammdaten.MrgTyp) AND
     AbrufListe [AktListenIndex].Erfolgreich then
    AbrufKonvert.AddFileName (AbrufListe [AktListenIndex].StationId,
                              Abruf.RufStammdaten.MrgTyp,
                              Abruf.MRGAbrufrec [dt_Tagessaetze].Filename,
                              dt_Tagessaetze,
                              Abruf.JournalId);

  RundpufferRuecksetzen_Stundensaetze;
  PruefsaetzeAbruf;
end;

{----------------------------------------------------------}
procedure TAbrufManager.RundpufferRuecksetzen_Stundensaetze;
{----------------------------------------------------------}
{ Dummy-Procedure }
begin
end;

{---------------------------------------}
procedure TAbrufManager.PruefsaetzeAbruf;
{---------------------------------------}
var
  Modus: byte;
  von, bis: TDateTime;
begin
  if GetAbrufPruefSatzDb (Abruf.RufStammdaten.MrgTyp) then begin   { MRG mit Prüfungssätzen ? }
    if PruefsaetzeAbrufen then begin
      { abzurufenden Datenbereich festlegen: }
      von:=AbrufListe [AktListenIndex].DatenVon;
      bis:=AbrufListe [AktListenIndex].DatenBis;
      if Abrufart = aa_manuell then begin
        if von = 0 then
          Modus:=d_alles
        else
          Modus:=d_vonbis;
      end else                                             { automatisch, Ruf }
        Modus:=d_neue;

      if Abruf.AbrufPruefwerte (Modus, von, bis) then
        PruefsaetzeGelesen
      else
        PruefsaetzeAbrufFehler;
    end else
      PruefsaetzeGelesen;
  end else
    BinaerdateiAbruf;
end;

{---------------------------------------------}
procedure TAbrufManager.PruefsaetzeAbrufFehler;
{---------------------------------------------}
begin
  AbrufListe [AktListenIndex].Erfolgreich := false;
  PruefSaetzeGelesen;
end;

{-----------------------------------------}
procedure TAbrufManager.PruefsaetzeGelesen;
{-----------------------------------------}
begin
  if PruefsaetzeAbrufen and AbrufListe [AktListenIndex].Erfolgreich then
    AbrufKonvert.AddFileName (AbrufListe [AktListenIndex].StationId,
                              Abruf.RufStammdaten.MrgTyp,
                              Abruf.MRGAbrufrec [dt_Pruefsaetze].Filename,
                              dt_Pruefsaetze,
                              Abruf.JournalId);
  RundpufferRuecksetzen_Pruefsaetze;
  BinaerdateiAbruf;
end;

{--------------------------------------------------------}
procedure TAbrufManager.RundpufferRuecksetzen_Pruefsaetze;
{--------------------------------------------------------}
{ Dummy-Procedure }
begin
end;

{---------------------------------------}
procedure TAbrufManager.BinaerdateiAbruf;
{---------------------------------------}
begin
  if BinaerdateiAbrufen then begin
    if Abruf.Abrufbinaerdatei then
      BinaerdateiGelesen
    else
      BinaerdateiAbrufFehler;
  end else
    BinaerdateiGelesen;
end;

{---------------------------------------------}
procedure TAbrufManager.BinaerdateiAbrufFehler;
{---------------------------------------------}
begin
  AbrufListe [AktListenIndex].Erfolgreich := false;
  BinaerdateiGelesen;
end;

{-----------------------------------------}
procedure TAbrufManager.BinaerdateiGelesen;
{-----------------------------------------}
begin
  ZeitSynchronisieren;
  RufDeaktivieren;

  if CheckUmschalten then begin
    inc (AktListenIndex);          { AktListenIndex für nächsten Slave setzen }
    AbrufListe [AktListenIndex].Erfolgreich := true;
    AbrufZeitbereich.ResetAbrufRecord;
    AbrufZeitbereich.SetMrgID (AbrufListe [AktListenIndex].StationId);
    Umschalten;                               { auf nächsten Slave umschalten }
  end else
    VerbindungBeenden;
end;

{------------------------------------------}
procedure TAbrufManager.ZeitSynchronisieren;
{------------------------------------------}
{ Dummy-Procedure }
begin
end;

{--------------------------------------}
procedure TAbrufManager.RufDeaktivieren;
{--------------------------------------}
{ Dummy-Procedure }
begin
end;

{----------------------------------------------}
function TAbrufManager.CheckUmschalten: boolean;
{----------------------------------------------}
{ Prüfen, ob auf nächsten Slave umgeschaltet werden muß
  Ergebnis: true, wenn umgeschaltet werden muß }
var
  dummy1: Char;
  dummy2: TMrgId;

begin
  Result:=false;
  case StammDaten.DSFGTyp (AbrufListe [AktlistenIndex].StationId, dummy1, dummy2) of
    DSFGMASTER, DSFGSLAVE:
    begin
      if AktListenIndex < High (AbrufListe) then begin
        if AbrufListe [AktListenIndex + 1].StationId <> 0 then
          Result:=true;
      end;
    end;
  end; { case }
end;

{---------------------------------}
procedure TAbrufManager.Umschalten;
{---------------------------------}
begin
  if Abruf.DSfGUmschaltung (AbrufListe [AktListenIndex].StationId,
                            AbrufListe [AktListenIndex].Datentypen) then
    UmschaltungErfolgt
  else
    UmschaltungFehler;
end;

{-----------------------------------------}
procedure TAbrufManager.UmschaltungErfolgt;
{-----------------------------------------}
begin
  ParameterAbruf ('0');                                { alle Parameter holen }
end;

{----------------------------------------}
procedure TAbrufManager.UmschaltungFehler;
{----------------------------------------}
begin
  AbrufListe [AktListenIndex].Erfolgreich := false;

  if AktListenIndex < High (AbrufListe) then begin
    if AbrufListe [AktListenIndex + 1].StationId <> 0 then begin
      inc (AktListenIndex);        { AktListenIndex für nächsten Slave setzen }
      AbrufListe [AktListenIndex].Erfolgreich := true;
      AbrufZeitbereich.ResetAbrufRecord;
      AbrufZeitbereich.SetMrgID (AbrufListe [AktListenIndex].StationId);
      Umschalten;                             { auf nächsten Slave umschalten }
    end else
      VerbindungBeenden;
  end else
    VerbindungBeenden;
end;

{-----------------------------------}
procedure TAbrufManager.Konvertieren;
{-----------------------------------}
var
  i : integer;
begin
  // ASCII-Export für alle abgerufenen MRGs initialisieren
  for i:=Low (AbrufListe) to High (Abrufliste) do            // 12.02.2001
    if (AbrufListe[i].StationId <> 0) then
      InitASCIIExport(C_GerArtMrg, AbrufListe[i].StationId,
        [aetStundenwerte, aetTageswerte, aetMeldungen],
        FormatDateTime('yyyymmddhhnnss', Now), 0, 0, PathServer[AsciiDir]);
  try
    ZustandMessage (Abruf.SerialMRG.COMPort, -1, z_ParameterKonvertieren, '', Abruf.WithZustandTabelle);
    AbrufKonvert.ParamKonvert;

    ZustandMessage (Abruf.SerialMRG.COMPort, -1, z_MeldungenKonvertieren, '', Abruf.WithZustandTabelle);
    AbrufKonvert.MeldKonvert;

    ZustandMessage (Abruf.SerialMRG.COMPort, -1, z_MesswerteKonvertieren, '', Abruf.WithZustandTabelle);
    AbrufKonvert.MessKonvert;

    ZustandMessage (Abruf.SerialMRG.COMPort, -1, z_PruefsaetzeKonvertieren, '', Abruf.WithZustandTabelle);
    AbrufKonvert.PruefKonvert;

    // ASCII-Exportdateien schreiben
    SaveASCIIExportFiles(True);                             // 12.02.2001
  finally
    DoneASCIIExport;    // ASCII-Export beenden
  end;
end;

{--------------------------------------------}
procedure TAbrufManager.JournalReorganisieren;
{--------------------------------------------}
var
  JournalDB: TJournalDB;
begin
  ZustandMessage (Abruf.SerialMRG.COMPort, -1, z_JournalReorganisieren, '', Abruf.WithZustandTabelle);
  JournalDB:=TJournalDB.Create (PathServer.PathName [WStammDir]);
  try
    JournalDB.Reorganize (Systemdaten.GrJournalPuffer);   { wird nur 1 mal täglich gemacht }
  finally
    JournalDB.Free;
  end;
end;

{----------------------------------------------------------------}
function TAbrufManager.LoescheManuDaten (AMrgId: TMrgId): boolean;
{----------------------------------------------------------------}
{ alle manuellen Daten eines MRG löschen }
var
  MeldungenDb: TMeldungenDb;
begin
  Result:=false;
  DeleteFile (GetManuDateiName(AMrgId, dd_mess));         { Meßwerte-Datei }
  DeleteFile (GetManuDateiName(AMrgId, dd_tags));         { Tagessätze-Datei }
  DeleteFile (GetManuDateiName(AMrgId, dd_pruef));        { Prüfungssätze-Datei }
  MeldungenDb:=TMeldungenDb.Create (PathServer.PathName [WStammDir]);
  try
    MeldungenDb.DeleteMRGMeldungen (C_BenutzerManu, AMrgId);{ Meldungs-Tabellen }
  finally
    MeldungenDb.Free;
  end;
  DeleteFromParamTable (AMrgId);                          { Parametertabelle }
end;


{ TAutoManuAbrufManager }

{--------------------------------------------------------------------------------------}
constructor TAutoManuAbrufManager.Create (ASerialMRG: TSerialMRG; AAbrufart: TAbrufart;
                                          AAbrufListe: TAbrufListe; AModemRuf: integer);
{--------------------------------------------------------------------------------------}
begin
  inherited Create (ASerialMRG, AAbrufart, AModemRuf);
  SetAbrufListe (AAbrufListe);
end;

{-----------------------------------------------------------------------}
procedure TAutoManuAbrufManager.SetAbrufListe (AAbrufListe: TAbrufListe);
{-----------------------------------------------------------------------}
begin
  AbrufListe:=AAbrufListe;
  AktListenIndex := 1;
end;

{-------------------------------------------------------}
function TAutoManuAbrufManager.ParameterAbrufen: boolean;
{-------------------------------------------------------}
begin
  Result := AbrufListe [AktListenIndex].Datentypen AND C_IsParameter <> 0;
end;

{-------------------------------------------------------}
function TAutoManuAbrufManager.MeldungenAbrufen: boolean;
{-------------------------------------------------------}
begin
  Result := AbrufListe [AktListenIndex].Datentypen AND C_IsMeldungen <> 0;
end;

{-------------------------------------------------------}
function TAutoManuAbrufManager.MesswerteAbrufen: boolean;
{-------------------------------------------------------}
begin
  Result:=AbrufListe [AktListenIndex].Datentypen AND C_IsMesswerte <> 0;
end;

{---------------------------------------------------------}
function TAutoManuAbrufManager.PruefsaetzeAbrufen: boolean;
{---------------------------------------------------------}
begin
  Result:=AbrufListe [AktListenIndex].Datentypen AND C_IsPruefsaetze <> 0;
end;

{---------------------------------------------------------}
function TAutoManuAbrufManager.BinaerdateiAbrufen: boolean;
{---------------------------------------------------------}
begin
  Result := AbrufListe [AktListenIndex].Datentypen AND C_IsBinaerdatei <> 0;
end;

{--------------------------------------------------------------}
procedure TAutoManuAbrufManager.RundpufferRuecksetzen_Meldungen;
{--------------------------------------------------------------}
begin
  if (Abrufart = aa_automatisch) AND
     MeldungenAbrufen AND
     Abruf.Rufstammdaten.Rundpufferreset then Abruf.ResetRundpuffer_Meld;
end;

{------------------------------------------------------------------}
procedure TAutoManuAbrufManager.RundpufferRuecksetzen_Stundensaetze;
{------------------------------------------------------------------}
begin
  if (Abrufart = aa_automatisch) AND
     MesswerteAbrufen AND
     Abruf.Rufstammdaten.Rundpufferreset then Abruf.ResetRundpuffer_Mess;
end;

{----------------------------------------------------------------}
procedure TAutoManuAbrufManager.RundpufferRuecksetzen_Pruefsaetze;
{----------------------------------------------------------------}
begin
  if (Abrufart = aa_automatisch) AND
     PruefsaetzeAbrufen AND
     Abruf.Rufstammdaten.Rundpufferreset then Abruf.ResetRundpuffer_Pruef;
end;

{--------------------------------------------------}
procedure TAutoManuAbrufManager.ZeitSynchronisieren;
{--------------------------------------------------}
begin
  if (Abrufart = aa_automatisch) AND
     Abruf.Rufstammdaten.ZeitSynchronisation then Abruf.ZeitSynchronisation;
end;


{ TRufManager }

{---------------------------------------------}
function TRufManager.ParameterAbrufen: boolean;
{---------------------------------------------}
begin
  Result := Systemdaten.RufDatenTypen AND C_IsParameter <> 0;
end;

{---------------------------------------------}
function TRufManager.MeldungenAbrufen: boolean;
{---------------------------------------------}
begin
  Result := Systemdaten.RufDatenTypen AND C_IsMeldungen <> 0;
end;

{---------------------------------------------}
function TRufManager.MesswerteAbrufen: boolean;
{---------------------------------------------}
begin
  Result:=Systemdaten.RufDatenTypen AND C_IsMesswerte <> 0;
end;

{-----------------------------------------------}
function TRufManager.PruefsaetzeAbrufen: boolean;
{-----------------------------------------------}
begin
  Result:=Systemdaten.RufDatenTypen AND C_IsPruefsaetze <> 0;
end;

{--------------------------------------}
procedure TRufManager.RufEntgegennehmen;
{--------------------------------------}
begin
  if Abruf.RufAnnahme then begin
    { Ruf erfolgreich entgegengenommen: }
    AktListenIndex := 1;
    AbrufListe [AktListenIndex].StationId := Abruf.RufStammdaten.MrgId;
    AbrufListe [AktListenIndex].Erfolgreich := true;
    VerbindungSteht;
  end
  else begin
    { Fehler bei der Rufannahme (Verbindung steht) bzw. immer bei FUP-Rufannahme: }
    if not Abruf.NoCarrier OR Abruf.isFupAbruf then
      Verbindungsfehler
    else begin                          { Fehler beim Verbindungsaufbau, Verbindung steht nicht }
      UpdateMRGJournal (Abruf.JournalId, C_WJournal_DZVerbEnde);
      AbrufListe [AktListenIndex].Erfolgreich := false;
    end;
  end;
end;

{------------------------------------}
procedure TRufManager.VerbindungSteht;
{------------------------------------}
var
  dummy1: char;
  dummy2: TMrgId;
begin
  GerArt := StammDaten.DSFGTyp (AbrufListe [AktListenIndex].StationId, dummy1, dummy2);
  if GerArt = DSFGNO then
    ParameterAbruf ('0')                               { alle Parameter holen }
  else
    RufListeAbfragen;
end;

{-------------------------------------}
procedure TRufManager.RufListeAbfragen;
{-------------------------------------}
var
  RufListe: string;
begin
  if Abruf.RufListeAbfragen (RufListe) then
    SetRufListe (RufListe);
  inherited VerbindungSteht;
end;

{---------------------------------------------------}
procedure TRufManager.SetRufListe (RufListe: string);
{---------------------------------------------------}
{ Konvertierung der RufListe (Adressen) in AbrufListe (MrgIds etc.) }
var
  i: integer;
  MasterId: TMrgId;
  SlaveId: TMrgId;
  SlaveAdr: char;
  MrgTyp: TMrgTyp;

begin
  MasterId := AbrufListe [AktListenIndex].StationId;
  for i := 1 to Length (RufListe) do begin
    if RufListe [i] <> '0' then begin
      SlaveAdr := RufListe[i];
      SlaveId := StammDaten.GetDSFGSlaveId (MasterId, SlaveAdr);
      if (SlaveId > 0) AND (AktListenIndex < High (AbrufListe)) then begin
        inc (AktListenIndex);
        AbrufListe [AktListenIndex].StationId := SlaveId;
        AbrufListe [AktListenIndex].Datentypen := SystemDaten.RufDatentypen;  { für Journal }
      end;
    end;
  end; { for }

  { Parameterabruf für EC694 vorerst gesperrt: }
  for i:=Low (AbrufListe) to High (Abrufliste) do begin
    MrgTyp:=Stammdaten.GetMrgTyp (AbrufListe [i].StationId);
    if ((AbrufListe [i].Datentypen AND C_IsParameter) <> 0) AND (MrgTyp = mrgtyp_EC694) then
      AbrufListe [i].Datentypen:=AbrufListe [i].Datentypen AND NOT C_IsParameter;
  end;

  AktListenIndex := 1;
end;

{---------------------------------------}
procedure TRufManager.TagessaetzeGelesen;
{---------------------------------------}
var
  SlaveAdr: char;
  dummy2: TMrgId;
begin
  if StammDaten.DSFGTyp (AbrufListe [AktlistenIndex].StationId, SlaveAdr, dummy2) = DSFGSLAVE then
    Abruf.SlaveRufQuittieren (SlaveAdr);
  inherited TagessaetzeGelesen;
end;

{------------------------------------}
procedure TRufManager.RufDeaktivieren;
{------------------------------------}
begin
  if Systemdaten.MaxAnrufversuche_Station > 0 then
    Abruf.RufDeaktivierung;
end;


{ TMomentanAbrufManager }

{-------------------------------------------------------------------------------}
constructor TMomentanAbrufManager.Create (ASerialMRG: TSerialMRG; AMrgId: TMrgId;
                                          AModemRuf: integer);
{-------------------------------------------------------------------------------}
begin
  inherited Create (ASerialMRG, aa_momentan, AModemRuf);
  SetAbrufMrg (AMrgId);
  CheckMomHalten:=false;
  MRGAbrufTableAufraeumen;    { vor dem Abruf erst mal alle MomH- und MomE-Einträge löschen }
end;

{---------------------------------------}
destructor TMomentanAbrufManager.Destroy;
{---------------------------------------}
begin
  DeleteMomTables;
  MRGAbrufTableAufraeumen;    { alle noch evtl. vorhandenen MomH- und MomE-Einträge löschen }
  inherited Destroy;
end;

{-----------------------------------------------------------}
procedure TMomentanAbrufManager.SetAbrufMrg (AMrgId: TMrgId);
{-----------------------------------------------------------}
begin
  AbrufListe [1].StationId:=AMrgId;
  AbrufListe [1].Datentypen:=C_IsParameter;
  AktListenIndex := 1;
end;

{--------------------------------------------------------------}
procedure TMomentanAbrufManager.ParameterAbruf (Nummer: string);
{--------------------------------------------------------------}
var
  MRGAbruf: TMRGAbruf;
  Ok: boolean;
  StopIt: boolean;
  Abrufart: string;
  Datentypen: integer;
  SaveToMom: boolean;
  ParaNr: string;

begin
  ParaNr:=Nummer;
  MRGAbruf:=TMRGAbruf.Create (PathServer.PathName [WStammDir]);
  try
    SaveToMom:=true;
    repeat
      Application.ProcessMessages;
      StopIt:=false;
      if ParaNr <> '0' then { bei Einzelparameter "Überlastschutz" für FUP/MRG }
        Delay (2000);
      Ok:=Abruf.AbrufParameter (ParaNr, SaveToMom);
      if Ok then begin
        if MRGAbruf.GetMRGAbrufMomentan (AbrufListe [AktListenIndex].StationId,
                                         Abrufart, Datentypen) then begin
          if Abrufart = C_AbrArtParaStart then begin
            { für schnellere Reaktion bis zum Parametersenden ab jetzt nur 1 Parameter lesen, um die Verbindung zu halten
              und diesen nicht speichern: }
            ParaNr:=CP_ALLG_Geraeteversion;
            SaveToMom:=false;
          end;
          if Abrufart = C_AbrArtParaSenden then
            Abruf.UebertragungParameter;             { Parameter übertragen auf Anforderung }
          if Abrufart = C_AbrArtParaEnde then begin
            { ab jetzt wieder alle Parameter lesen und speichern: }
            ParaNr:=Nummer;
            SaveToMom:=true;
          end;
          if Abrufart = C_AbrArtMomStop then
            StopIt:=true;                        { Momentanwerteholen beenden auf Anforderung }

          if not StopIt AND CheckMomHalten then begin
            { Sicherheitsstop, wenn kein MomHalten-Eintrag in ABRFLAKS.DB: }
            StopIt:= not MRGAbruf.GetMRGAbrufMomentanHalten (AbrufListe [AktListenIndex].StationId);
            CheckMomHalten:=false;                                      { Flag zurücksetzen }
          end;
        end else
          StopIt:=true;
      end else
        StopIt:=true;
    until StopIt;
  finally
    MRGAbruf.Free;
  end;

  if not Ok then
    AbrufListe [AktListenIndex].Erfolgreich := false;
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
    MomTable.TableName:=CDBMMom + Format('%.4d.DB', [AbrufListe [AktListenIndex].StationId]);
    if MomTable.Exists then
      MomTable.DeleteTable;
    DeleteTriggerFile (MomTable.DatabaseName + MomTable.TableName);

    { Tabelle für Parametrierung: }
    MomTable.TableName:=C_TbMPE + Format('%.4d.DB', [AbrufListe [AktListenIndex].StationId]);
    if MomTable.Exists then
      MomTable.DeleteTable;
    DeleteTriggerFile (MomTable.DatabaseName + MomTable.TableName);
  finally
    MomTable.Free;
  end;
end;

{------------------------------------------------------}
procedure TMomentanAbrufManager.MRGAbrufTableAufraeumen;
{------------------------------------------------------}
{ vorhandene MomH- und MomE-Datensätze aus MRG-Abruftabelle löschen }
var
  MRGAbruf: TMRGAbruf;
begin
  MRGAbruf:=TMRGAbruf.Create (PathServer.PathName [WStammDir]);
  try
    MRGAbruf.DeleteMRGMomentan (AbrufListe [AktListenIndex].StationId);
  finally
    MRGAbruf.Free;
  end;
end;


{ TRufReaktManager }

{--------------------------------------------------------------------------}
constructor TRufReaktManager.Create (ASerialMRG: TSerialMRG; AMrgId: TMrgId;
                                     AModemRuf: integer);
{--------------------------------------------------------------------------}
begin
  inherited Create (ASerialMRG, aa_rufreakt, AModemRuf);
  SetRufReaktMrg (AMrgId);
end;

{----------------------------------}
destructor TRufReaktManager.Destroy;
{----------------------------------}
begin
  { Ende für RE32 signalisieren: }
  WriteNewTime(PathServer.PathName [WNetWorkDir] + C_MrgRufReaktTriggerFile +
               Format('%.4d.', [AbrufListe [AktListenIndex].StationId]) + C_TriggerExt);
  inherited Destroy;
end;

{---------------------------------------------------------}
procedure TRufReaktManager.SetRufReaktMrg (AMrgId: TMrgId);
{---------------------------------------------------------}
begin
  AbrufListe [1].StationId:=AMrgId;
  AbrufListe [1].Datentypen:=0;
  AktListenIndex := 1;
end;

{-----------------------------------------}
procedure TRufReaktManager.VerbindungSteht;
{-----------------------------------------}
begin
  if GerArt = DSFGSLAVE then                { nur Slave soll abgerufen werden }
    Umschalten      { Verbindung zum Master steht, jetzt auf Slave umschalten }
  else
    Reaktivieren;                         { Ruffunktion im Gerät reaktivieren }
end;

{--------------------------------------------}
procedure TRufReaktManager.UmschaltungErfolgt;
{--------------------------------------------}
begin
  Reaktivieren;                           { Ruffunktion im Gerät reaktivieren }
end;

{--------------------------------------}
procedure TRufReaktManager.Reaktivieren;
{--------------------------------------}
begin
  if not Abruf.RufReaktivierung then
    AbrufListe [AktListenIndex].Erfolgreich := false;
  VerbindungBeenden;
end;

{----------------------------------------------}
procedure TRufReaktManager.VerbindungIstBeendet;
{----------------------------------------------}
begin
  { nichts machen }
end;


{ TRueckRufManager }

{---------------------------------------------------------------------------}
constructor TRueckRufManager.Create (ASerialMRG: TSerialMRG; AMrgId: TMrgId;
                                     AZentrale: integer; AModemRuf: integer);
{---------------------------------------------------------------------------}
begin
  inherited Create (ASerialMRG, aa_rueckruf, AModemRuf);
  SetRueckRufMrg (AMrgId);
  Zentrale:=AZentrale;
end;

{---------------------------------------------------------}
procedure TRueckRufManager.SetRueckRufMrg (AMrgId: TMrgId);
{---------------------------------------------------------}
begin
  AbrufListe [1].StationId:=AMrgId;
  AbrufListe [1].Datentypen:=0;
  AktListenIndex := 1;
end;

{-----------------------------------------}
procedure TRueckRufManager.VerbindungSteht;
{-----------------------------------------}
begin
  if GerArt = DSFGSLAVE then                { nur Slave soll abgerufen werden }
    Umschalten      { Verbindung zum Master steht, jetzt auf Slave umschalten }
  else
    RueckrufAusloesen;                            { Rückruf im Gerät auslösen }
end;

{--------------------------------------------}
procedure TRueckRufManager.UmschaltungErfolgt;
{--------------------------------------------}
begin
  RueckrufAusloesen;                              { Rückruf im Gerät auslösen }
end;

{-------------------------------------------}
procedure TRueckRufManager.RueckrufAusloesen;
{-------------------------------------------}
begin
  if not Abruf.RueckRufAusloesung (Zentrale) then
    AbrufListe [AktListenIndex].Erfolgreich := false;
  VerbindungBeenden;
end;

{----------------------------------------------}
procedure TRueckRufManager.VerbindungIstBeendet;
{----------------------------------------------}
begin
  { nichts machen }
end;

end.
