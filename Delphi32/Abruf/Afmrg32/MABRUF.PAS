{******************************************************************************}
{* Unit: Objekt für MRG-Abruf                                                 *}
{* 09.12.1998 WW                                                              *}
{******************************************************************************}
Unit MAbruf;

INTERFACE

uses
  Windows, Classes, DbTables, SysUtils, MObjPara, MObjParaDB, MDBSta, MSysDat,
  AbrfInfo, MZustand, ErrConst, MValidAnswer, T_Tools, T_Zeit, MeldungenDB,
  MStartDa, WSysCon, MP_Allg, MP_Dfue, MDTFormt, JournlDB, MJournal, MDBAbruf,
  my_utils, RufeDB, PathIni, TbMrgPe, Db_Attn, MLGZVerw, MSysCon, LGZType, Serial,
  SerMRG, SerMrgFup, SerMrgModem, WChars, MDbParam, SrvCfgIni, WStrUtils, 
  MDBMrg, MRGBefehl, MModemInit, MP_Elster, ErrPrc32, WComm;

Const

  { Datenmengen }

  d_Neue   = 1;
  d_Alles  = 2;
  d_VonBis = 3;

Type

  { Strukturen mit Informationen für MRG-Datenabruf }

  TMRGAbrufRec = Record
    AbrufRec : TAbrufRec;                       { Abrufzeitraum }
    FileName : TFileName;                       { Datenrohfile des Abrufs }
  End;

  { Stati des Zugangsschlosses (für IEC 1107-Geräte, z.B. Elster DL240, EK260) }
  TSchlossStatus = (Schloss_bereits_offen,
                    Lieferantenschloss_geoeffnet,
                    Kundenschloss_geoeffnet);

  { Allgmeines Objekt zum MRG-Datenabruf }

  TAbruf = class (TObject)
  private
    Abrufart: TAbrufart;
    Stop_bei_falscher_Kennung: boolean;  { wenn true, erfolgt Verbindungsabbau, falls die
                                           DSfG-DFÜ-Kennung nicht mit der Stammdaten-Kennung
                                           übereinstimmt }
    Fehlergruppe: integer;                            { Fehlergruppe serielle Kommunikation }
    Fehlercode: integer;        { Fehlercode serielle Kommunikation, wenn Fehlergruppe <> 0 }
    MRGKennung: string;           { aus MRG gelesene Kennung, nicht die aus den Stammdaten }
    Stammdaten: TMRGStammdaten;
    MrgDefData: TMrgDefDataDb;
    MrgInfoData: TMrgInfoDataDb;
    IEC1107_AbschaltBefehl: boolean;  { true: vor dem Verbindungsende wird der Abschaltbefehl gesendet }
    IEC1107_SchlossStatus: TSchlossStatus;
    StdKanalNr: integer;    { aktueller Kanal im Stundenwert-Abruf (wenn je Kanal ein eigener Abruf nötig ist !) }
    ParameterBefehlListe: TStringList;
    ModemRuf: integer;

    procedure FehlerGruppeCodeUpdate (AFehlergruppe, AFehlercode: integer);
    function GetAbrufartString: string;
    procedure CreateMeldungenAbrufRec (Modus: byte; von, bis: TDateTime);
    procedure CreateTagessatzAbrufRec (Modus: byte; von, bis: TDateTime);
    procedure CreatePruefSaetzeAbrufRec (Modus: byte; von, bis: TDateTime);
    procedure FillParameterBefehlListe;
    function GetBefehlFromBinaerfile (FileName: TFileName; var Befehl: string): boolean;
    function SetDUEGeschwindigkeit_Fup: boolean;
    function CheckModemConnect (ModemAntwort: string): boolean;
    function KennungAbfragen (ModemAbrufgruppe: integer; Infobefehl: string;
                              InfoData_Kennung: string;
                              Rufentgegennahme: boolean;
                              var Keine_weiteren_Versuche: boolean): boolean;
    function PasswortLogin (var Keine_weiteren_Versuche: boolean): boolean;
    function Init_IEC1107_Kommunikation (var Keine_weiteren_Versuche: boolean): boolean;
    function Verbindungsaufbau_Fup (var Keine_weiteren_Versuche: boolean): boolean;
    function Verbindungsaufbau_Modem (var Keine_weiteren_Versuche: boolean): boolean;
    function Verbindungsabbau_Fup: boolean;
    function Verbindungsabbau_Modem: boolean;
    function RufAnnehmen_Fup: boolean;
    function RufAnnehmen_Modem: boolean;
  public
    JournalId: integer;            { für jedes MRG ein eigener, eindeutiger Journaleintrag }
    SerialMRG: TSerialMRG;
    RufStammDaten: TRufStammDaten;       { Record mit Stammdaten für den Verbindungsaufbau }
    ParameterListe: TParameterListeDB;                    { Liste mit gelesenen Parametern }
    MRGAbrufRec: Array [1..maxDataTypes] of TMRGAbrufRec;     { Information für Datenabruf }
    WithZustandTabelle: boolean;  { true, wenn Zustände in Zustandstabelle mitprotokolliert
                                    werden sollen, sonst nur Ausgabe in Statuszeile }
    isFupAbruf: boolean;                   { true, wenn Abruf mit FUP erfolgt }
    NoCarrier: boolean; { true, wenn keine Verbindung besteht (es werden dann keine MRG-Befehle
                          mehr gesendet);
                          Anm: Bei Modem-Abrufen kann eine bestehende Verbindung sicher
                               durch Überwachung des DCD-Signals erkannt werden.
                               Bei FUP-Abrufen kann dies nur behelfsmäßig erkannt werden.
                               NoCarrier wird nach erfolgreichem Verbindungsaufbau auf true
                               gesetzt, wenn auf einen MRG-Befehl STX..ETX eine FUP-Antwort
                               ESC..CR statt einer MRG-Antwort zurückkommt }
    Constructor Create (ASerialMRG: TSerialMRG; AAbrufart: TAbrufart; AModemRuf: integer);
    Destructor  Destroy; override;
    procedure CreateMesswerteAbrufRec (Modus: byte; von, bis: TDateTime);
    function VerbAufbau (MrgId: TMrgId; Datentypen: integer;
                         var Keine_weiteren_Versuche: boolean): boolean;
    function VerbAbbau: boolean;
    function SetNextStdAbrufKanal: boolean;
    function AbrufParameter (Nummer: string; SaveToMom: boolean): boolean;
    function AbrufMeldungen (Modus: byte; von, bis: TDateTime): boolean;
    function AbrufStdWerte: boolean;
    function AbrufTagWerte (Modus: byte; von, bis: TDateTime): boolean;
    function DSfGUmschaltung (MrgId: TMrgId; Datentypen: integer): boolean;
    function AbrufPruefwerte (Modus: byte; von, bis: TDateTime): boolean;
    function AbrufBinaerdatei: boolean;
    procedure ResetRundpuffer_Meld;
    procedure ResetRundpuffer_Mess;
    procedure ResetRundpuffer_Pruef;
    procedure ZeitSynchronisation;
    procedure RufDeaktivierung;
    function RufReaktivierung: boolean;
    procedure UebertragungParameter;
    function RueckrufAusloesung (Zentrale: integer): boolean;
    function RufListeAbfragen (var RufListe: string): boolean;
    procedure SlaveRufQuittieren (Adresse: char);
    function RufAnnahme: boolean;
  End;

IMPLEMENTATION

uses
  FMain;

{ TAbruf }

{-------------------------------------------------------------------------------------------}
Constructor TAbruf.Create (ASerialMRG: TSerialMRG; AAbrufart: TAbrufart; AModemRuf: integer);
{-------------------------------------------------------------------------------------------}
Begin
  inherited Create;
  SerialMRG:=ASerialMRG;
  Abrufart:=AAbrufart;
  ModemRuf:=AModemRuf;
  Stammdaten:=TMRGStammdaten.Create (PathServer.PathName [WStammDir]);
  Stammdaten.InitTabellen;
  ParameterListe:=TParameterListeDB.Create (PathServer.PathName [WStammDir]);
  ParameterBefehlListe:=TStringList.Create;

  isFupAbruf:=SerialMRG is TSerialMrgFup;

  { Kennungsprüfung bei Verbindungsaufbau: ja/nein }
  case Abrufart of
    aa_manuell,
    aa_momentan: Stop_bei_falscher_Kennung:=false;
  else
    Stop_bei_falscher_Kennung:=true;
  end;

  { Protokollierung in Zustandtabelle ein-/ausschalten: }
  WithZustandTabelle:=(Abrufart <> aa_momentan) AND
                      (Abrufart <> aa_ruf);

  { für Abruf von Geräten mit IEC 1107-Protokoll: }
  IEC1107_AbschaltBefehl:=false;
  IEC1107_SchlossStatus:=Schloss_bereits_offen;
  StdKanalNr:=0;

  { Initialisieren der Fehlerstati }
  Fehlergruppe:=0;
  Fehlercode:=0;

  JournalId:=0;
  MRGKennung:='';
  NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
End;

{------------------------}
Destructor TAbruf.Destroy;
{------------------------}
Begin
  ZustandMessage(-1, -1, z_Bereit, '', WithZustandTabelle);
  KennungMessage ('');
  StationMessage ('');

  ParameterBefehlListe.Free;
  ParameterListe.Free;
  Stammdaten.Free;

  if isFupAbruf then begin
    { aufeinanderfolgende F2-Antworten des FUP zählen -> FUP-Reset statt
      standardmäßiger FUP-Initialisierung in FMain.AbrufStarten }
    if (Fehlergruppe = COM_FUPERROR) AND (Fehlercode = FUPERR_F2) then
      inc (FUP_F2_Count)
    else
      FUP_F2_Count:=0;
  end
  else begin
    { Baudrate, Schnittstellen-Parameter und Datenprotokoll zur Modem-Initialisierung
      aktivieren (für Rufentgegennahme entscheidend !): }
    if ModemRuf = mr_Modem then  { Rufentgegennahme für Modemgeräte mit max. möglicher Baudrate }
      SerialMRG.Baudrate:=TSerialMRGModem (SerialMRG).MaxModemBaudrate
    else                         { Rufentgegennahme für FUP-Geräte mit fester Standard-Baudrate 9600 }
      SerialMRG.Baudrate:=br_MRGStandard;
    TSerialMRGModem (SerialMRG).SetAbrufgruppe (0);
  end;

  inherited Destroy;
End;

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
  if Fehlergruppe > 0 then
    WriteJournalFehler (JournalId, Fehlergruppe, Fehlercode);
End;

{----------------------------------------}
function TAbruf.GetAbrufartString: string;
{----------------------------------------}
Begin
  case Abrufart of
    aa_automatisch: Result:=C_AbrArtAuto;
    aa_manuell:     Result:=C_AbrArtManu;
    aa_ruf:         Result:=C_AbrArtRuf;
    aa_momentan:    Result:=C_AbrArtMomStart;
    aa_rufreakt:    Result:=C_AbrArtRufReakt;
    aa_rueckruf:    Result:=C_AbrArtRueckRuf;
  else
    Result:='unbekannt';
  end;
End;

{--------------------------------------------------------------------------}
procedure TAbruf.CreateMeldungenAbrufRec (Modus: byte; von, bis: TDateTime);
{--------------------------------------------------------------------------}
{ erzeugt MRGAbrufRec [dt_Meldungen], welcher zum Meldungsabrufen benötigt wird;
  Übergabe: Modus -> d_Alles   = Es sollen alle Meldungen abgerufen werden
                     d_Neu     = Es sollen nur neue Meldungen abgerufen werden
                     d_Bereich = Es sollen Meldungen von - bis abgerufen werden }
Var
  MAR : TMRGAbrufRec;
  DatumZeit: TDateTime;
  dummy: word;
  MeldungenDb: TMeldungenDb;
  dummy_str: string;

Begin
  FillChar (MAR, Sizeof (MAR), 0);

  Case Modus of
    d_alles :
      Begin
        MAR.AbrufRec.AlleDaten := True;
        MRGAbrufRec [dt_Meldungen] := MAR;
      End;

    d_neue :                         { wenn noch keine Meldungen abgerufen, dann alle holen }
      Begin
        MAR.AbrufRec.AlleDaten := True;
        MeldungenDb:=TMeldungenDb.Create (PathServer.PathName [WStammDir]);
        try
          if MeldungenDb.GetLetztMRGMeldung (C_BenutzerAuto, RufStammDaten.MrgId,
                                           DatumZeit, dummy_str) then begin
            MAR.AbrufRec.AlleDaten := False;
            DecodeDate (DatumZeit,
                        MAR.AbrufRec.vonJahr,
                        MAR.AbrufRec.vonMonat,
                        MAR.AbrufRec.vonTag);
            DecodeTime (DatumZeit,
                        MAR.AbrufRec.vonStunde,
                        MAR.AbrufRec.vonMinute,
                        MAR.AbrufRec.vonSekunde,
                        dummy);
            with MAR.AbrufRec do begin
              { Bis-Datum: aktuelles PC-Datum plus "Sicherheitsreserve", damit auch
                wirklich alle neuen Meldungen ausgelesen werden, wenn z.B.:
                -> PC- und Gerätezeit nicht ganz synchron laufen (i.d.R. immer)
                -> PC- und Gerät in verschiedenen Zeitzonen laufen
                -> Geräte abgerufen werden, bei denen als Zeitangabe nur die bis-Stunde
                   im Abfragebefehl enthalten ist (z.B. MRG 910: bis-Stunde 12
                   liefert nur Meldungen bis einschließlich 12:00:00 !) }
              DecodeDate (Date + 1, bisJahr, bisMonat, bisTag);    // bis aktuelle PC-Zeit plus 1 Tag
              DecodeTime (Time, bisStunde, bisMinute, bisSekunde, dummy);
            end;
          end;
        finally
          MeldungenDb.Free;
        end;
        MRGAbrufRec [dt_Meldungen] := MAR;
      End;

    d_vonbis :
      Begin
        MAR.AbrufRec.AlleDaten := false;
        with MAR.AbrufRec do begin
          DecodeDate (von, vonJahr, vonMonat, vonTag);
          DecodeTime (von, vonStunde, vonMinute, vonSekunde, dummy);
          bis:=bis + 1;  { 1 Tag mehr abrufen wie bei "d_neue"; 04.03.2003, WW }
          DecodeDate (bis, bisJahr, bisMonat, bisTag);
          DecodeTime (bis, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Meldungen] := MAR;
      End;
  End;
End;

{-------------------------------------------------------------------------}
procedure TAbruf.CreateMesswerteAbrufRec(Modus: byte; von, bis: TDateTime);
{-------------------------------------------------------------------------}
{ erzeugt MRGAbrufRec [dt_Messwerte], welcher zum Meßwertabrufen benötigt wird;
  Übergabe: Modus -> d_Alles   = Es sollen alle Meßwerte abgerufen werden
                     d_Neu     = Es sollen nur neue Meßwerte abgerufen werden
                     d_Bereich = Es sollen Meßwerte von - bis abgerufen werden }
Var
  MAR : TMRGAbrufRec;
  LetztDatum: DateRec;
  LetztZeit: TimeRec;
  dummy: Word;
  SollBis: TDateTime;
  Hour, Min, Sec, MSec: word;
  SoWiZeitOffset: TDateTime;

Begin
  FillChar (MAR, Sizeof (MAR), 0);
  MAR.AbrufRec.KanalMaske := StammDaten.GetMesswertKanalMaske (RufStammDaten.MrgId);

  case Modus of
    d_alles:
      begin
        MAR.AbrufRec.AlleDaten := True;
        MRGAbrufRec [dt_Messwerte] := MAR;

        { Soll-Datenzeitbereich nach MZBDATEN.DB schreiben: }
        SollBis:=Now;
        DecodeTime (SollBis, Hour, Min, Sec, MSec);
        SollBis:=Int (SollBis) + EncodeTime (Hour, 0, 0, 0);          { akt. Datum, Stunde }
        SoWiZeitOffset:=GetWZ_SZ_OffsetNowFromDB (Rufstammdaten.MrgId, C_GerArtMrg);
        SollBis:=SollBis + SoWiZeitOffset; { Sommer/Winterzeit in MRG und PC berücksichtigen }
        WriteDatenzeitbereich_Soll (JournalId, 0, SollBis);                  { nur SollBis }
      end;

    d_neue:      { Soll-Datenzeitbereich wird in HoleStartDatum_Mess gebildet }
      begin
        HoleStartDatum_Mess (JournalId, RufStammDaten, LetztDatum, LetztZeit);
        MAR.AbrufRec.AlleDaten := false;
        MAR.AbrufRec.vonJahr := LetztDatum.year;
        MAR.AbrufRec.vonMonat := LetztDatum.month;
        MAR.AbrufRec.vonTag := LetztDatum.day;
        MAR.AbrufRec.vonStunde := LetztZeit.hour;
        MAR.AbrufRec.vonMinute := LetztZeit.min;
        MAR.AbrufRec.vonSekunde := LetztZeit.sec;
        with MAR.AbrufRec do begin
          DecodeDate(Date, bisJahr, bisMonat, bisTag);
          DecodeTime(Time, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Messwerte] := MAR;
      end;

    d_vonbis :
      Begin
        MAR.AbrufRec.AlleDaten := false;
        with MAR.AbrufRec do begin
          DecodeDate (von, vonJahr, vonMonat, vonTag);
          DecodeTime (von, vonStunde, vonMinute, vonSekunde, dummy);
          DecodeDate (bis, bisJahr, bisMonat, bisTag);
          DecodeTime (bis, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Messwerte] := MAR;

        { Soll-Datenzeitbereich nach MZBDATEN.DB schreiben: }
        WriteDatenzeitbereich_Soll (JournalId, von, bis);
      End;
  end; { case }
End;

{-------------------------------------------------------------------------}
procedure TAbruf.CreateTagessatzAbrufRec(Modus: byte; von, bis: TDateTime);
{-------------------------------------------------------------------------}
{ erzeugt MRGAbrufRec [dt_Tagessaetze], welcher zum Tagessatzabrufen benötigt wird;
  Übergabe: Modus -> d_Alles   = Es sollen alle Tagessätze abgerufen werden
                     d_Neu     = Es sollen nur neue Tagessätze abgerufen werden
                     d_Bereich = Es sollen Tagessätze von - bis abgerufen werden }
Var
  MAR : TMRGAbrufRec;
  LetztDatum: DateRec;
  LetztZeit: TimeRec;
  dummy: word;
Begin
  FillChar (MAR, Sizeof (MAR), 0);
  MAR.AbrufRec.KanalMaske := StammDaten.GetTagesSatzKanalMaske (RufStammDaten.MrgId);

  case Modus of
    d_alles:
      begin
        MAR.AbrufRec.AlleDaten := True;
        MRGAbrufRec [dt_Tagessaetze] := MAR;
      end;

    d_neue:
      begin
        HoleStartDatum_Tags (RufStammDaten, LetztDatum, LetztZeit);
        MAR.AbrufRec.AlleDaten := false;
        MAR.AbrufRec.vonJahr := LetztDatum.year;
        MAR.AbrufRec.vonMonat := LetztDatum.month;
        MAR.AbrufRec.vonTag := LetztDatum.day;
        MAR.AbrufRec.vonStunde := LetztZeit.hour;
        MAR.AbrufRec.vonMinute := LetztZeit.min;
        MAR.AbrufRec.vonSekunde := LetztZeit.sec;
        with MAR.AbrufRec do begin
          { Bis-Datum: aktuelles PC-Datum plus 1 Tag, sonst schickt das MRG 910 den
            letzten Tagessatz nicht; 04.03.2003, WW }
          DecodeDate(Date + 1, bisJahr, bisMonat, bisTag);    // bis aktuelle PC-Zeit plus 1 Tag
          DecodeTime(Time, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Tagessaetze] := MAR;
      end;

    d_vonbis :
      Begin
        MAR.AbrufRec.AlleDaten := false;
        with MAR.AbrufRec do begin
          DecodeDate (von, vonJahr, vonMonat, vonTag);
          DecodeTime (von, vonStunde, vonMinute, vonSekunde, dummy);
          bis:=bis + 1;  { 1 Tag mehr abrufen wie bei "d_neue"; 04.03.2003, WW }
          DecodeDate (bis, bisJahr, bisMonat, bisTag);
          DecodeTime (bis, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Tagessaetze] := MAR;
      End;
  end; { case }
End;

{----------------------------------------------------------------------------}
procedure TAbruf.CreatePruefSaetzeAbrufRec (Modus: byte; von, bis: TDateTime);
{----------------------------------------------------------------------------}
{ erzeugt MRGAbrufRec [dt_PruefSaetze], welcher zum Pruefsaetze abrufen benötigt wird;
  Übergabe: Modus -> d_Alles   = Es sollen alle Prüfungssätze abgerufen werden
                     d_Neu     = Es sollen nur neue Prüfungssätze abgerufen werden
                     d_Bereich = Es sollen Prüfungssätze von - bis abgerufen werden }
Var
  MAR : TMRGAbrufRec;
  LetztDatum: DateRec;
  LetztZeit: TimeRec;
  dummy: word;
Begin
  FillChar (MAR, Sizeof (MAR), 0);

  Case Modus of
    d_Alles :
      Begin
        MAR.AbrufRec.AlleDaten := True;
        MRGAbrufRec [dt_PruefSaetze] := MAR;
      End;

    d_neue:
      begin
        HoleStartDatum_Prfs (RufStammDaten, LetztDatum, LetztZeit);
        MAR.AbrufRec.AlleDaten := false;
        MAR.AbrufRec.vonJahr := LetztDatum.year;
        MAR.AbrufRec.vonMonat := LetztDatum.month;
        MAR.AbrufRec.vonTag := LetztDatum.day;
        MAR.AbrufRec.vonStunde := LetztZeit.hour;
        MAR.AbrufRec.vonMinute := LetztZeit.min;
        MAR.AbrufRec.vonSekunde := LetztZeit.sec;
        with MAR.AbrufRec do begin
          DecodeDate(Date, bisJahr, bisMonat, bisTag);
          DecodeTime(Time, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_PruefSaetze] := MAR;
      end;

    d_vonbis :
      Begin
        MAR.AbrufRec.AlleDaten := false;
        with MAR.AbrufRec do begin
          DecodeDate (von, vonJahr, vonMonat, vonTag);
          DecodeTime (von, vonStunde, vonMinute, vonSekunde, dummy);
          DecodeDate (bis, bisJahr, bisMonat, bisTag);
          DecodeTime (bis, bisStunde, bisMinute, bisSekunde, dummy);
        end;
        MRGAbrufRec [dt_Pruefsaetze] := MAR;
      End;
  End;
End;

{----------------------------------------}
procedure TAbruf.FillParameterBefehlListe;
{----------------------------------------}
{ Parameter-Befehlsliste zusammenstellen abhängig von Gerätegruppe;
  -> wird für Parameter-Abruf innerhalb eines Datenabrufs und für Momentanwerte-Abruf
     benötigt }
var
  Q: TQuery;
  ParaNrMrg: string;
  Befehl: string;

begin
  case MrgDefData.Kommandogruppe of
    101, 102:
      begin    { Elster DL240, EK260 }
        Q:=TQuery.Create (nil);
        try
          { Query mit allen zum Gerätetyp gehörenden Parameter-Nummern erstellen: }
          if GetAllParameterNrMrg_ByMrgTypDb (RufStammdaten.MrgTyp, Q) then begin
            while not Q.Eof do begin
              ParaNrMrg:=Q.FieldByName (C_ParamMrg_Parameternummer_im_MRG).AsString;
              { Befehl zur Abfrage der Parameter-Nummer bilden: }
              Befehl:=GetLIS200Kommando_Lesen (ParaNrMrg);
              ParameterBefehlListe.Add (Befehl);  { in Befehlsliste eintragen }
              Q.Next;
            end;
          end;
        finally
          Q.Close;
          Q.Free;
        end;
      end;
  end;
end;

{-------------------------------------------------}
function TAbruf.SetDUEGeschwindigkeit_Fup: boolean;
{-------------------------------------------------}
{ DÜ-Geschwindigkeit im FUP einstellen (Sx-Befehl); nur für FUP-9600 !
  Ergebnis: true, wenn Einstellen erfolgreich }
var
  SI: TSrvCfg32Ini;
  DUEStr: string;
  BefehlChar: char;
  Befehl: string;
  R: TRueckgabe;
  dummy: boolean;
  FupAntwort: string;

begin
  Result:=false;
   { Befehl zum Einstellen der FUP-DÜ-Geschwindigkeit lesen: }
  SI:=TSrvCfg32Ini.Create (PathServer.Pathname [WNetProgDir]);
  try
    case RufStammdaten.ModemTyp of
      fup_1200_HDX: DUEStr:=SI.Fup1200 [SerialMRG.COMPort];
      fup_2400_DX : DUEStr:=SI.Fup2400 [SerialMRG.COMPort];
    else
      DUEStr:='';
    end;
  finally
    SI.Free;
  end;

  if length (DUEStr) > 0 then begin
    BefehlChar:=DUEStr[1];
    if IsCharUpper (BefehlChar) then begin
      Befehl:=ESC+DUEStr+CR+ESC+LowerCase (BefehlChar)+CR;

      { DÜ-Geschwindigkeits-Kommando senden }
      if not SerialMRG.SendCommand (Befehl, [CR], 1, Timeout_FupAntwort, ad_String, R, dummy) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      FupAntwort:=ExtractString (R.Antwort, ESC, CR, 0);
      if FupAntwort <> DUEStr then begin     { FUP-Fehlermeldung oder sonstige falsche Antwort }
        FehlerGruppeCodeUpdate (COM_FUPERROR, GetFupErrorcode (FupAntwort));
        exit;
      end;
    end;
  end;
  Result:=true;
end;

{----------------------------------------------------------------}
function TAbruf.CheckModemConnect (ModemAntwort: string): boolean;
{----------------------------------------------------------------}
{ Modemantwort auf Verbindungsaufbau-Befehl bzw. Rufannahme-Befehl prüfen;
  Ergebnis: true, wenn CONNECT erfolgt ist.
  Falls kein CONNECT erfolgt ist, werden zusätzlich Fehlergruppe/Fehlercode gesetzt }
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

{-------------------------------------------------------------------------------}
function TAbruf.KennungAbfragen (ModemAbrufgruppe: integer; Infobefehl: string;
                                 InfoData_Kennung: string;
                                 Rufentgegennahme: boolean;
                                 var Keine_weiteren_Versuche: boolean) : boolean;
{-------------------------------------------------------------------------------}
{ Kennung des MRG abfragen;
  Übergabe: ModemAbrufgruppe
            Infobefehl
            Information zum Ermitteln der Kennung aus dem Infobefehl
            Flag Rufentgegennahme (true, wenn Kennungsabfrage im Rahmen einer
                                   Rufentgegennahme erfolgt)
  Rückgabe: Keine_weiteren_Versuche (wenn PW falsch bei Modem-Abrufgruppe 3, 4)
  Ergebnis: true, wenn Kennung erfolgreich abgefragt wurde }
type
  TAbfrageModus = (am_Infobefehl, am_k_Befehl, am_B003_Befehl);

var
  Befehl: string;
  R: TRueckgabe;
  WithCRC_Merk: boolean;
  Left_Count, Before: word;
  Left, Right: array [0..10] of char;
  pInfo_Kennung: array [0..szLen_Info] of char;
  cLeft, cRight: char;
  S: string;
  AbfrageModus: TAbfrageModus;
  Kommando_Soll: string;

begin
  Result:=false;
  { Kennungsabfrage für Modemgruppe 5 (IEC 1107-Protokoll, z.B. Elster DL240 EK260): }
  if ModemAbrufgruppe = 5 then begin
    TSerialMRGModem (SerialMRG).SetSendWithBCC (true);    { Flag setzen, es folgt Kennungs-Befehl mit BCC }
    TSerialMRGModem (SerialMRG).SetReceiveWithBCC (true); { Flag setzen, es folgt Kennungs-Antwort mit BCC }

    { Kennung (= "Stationsnummer") lesen: }
    Befehl:=GetLIS200Kommando_Lesen ('3:180');
    if not SerialMrg.SendCommand (Befehl, [ETX], 1, Timeout_IEC1107Telegr, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    { Antwort auswerten: }
    if not ValidIEC1107DatenTelegrammAntwort (R.Antwort, Fehlergruppe, Fehlercode) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
    R.Antwort:=ExtractString (R.Antwort, '(', ')', 0);  { Stationsnummer steht zwischen den runden Klammern }
  end

  { Kennungsabfrage für alle übrigen Modemgruppen: }
  else begin
    WithCRC_Merk:=false;
    { Kommando für Kennungsabfrage bilden: }
    if length (Infobefehl) > 0 then begin        { Kennung abrufen über Infobefehl (z.B. EC694) }
      AbfrageModus:=am_Infobefehl;
      Befehl:=STX+Infobefehl+ETX;
      { Achtung: Infobefehl ohne CRC senden, sonst Antwort verstümmelt (Fehler im EC694): }
      if not isFupAbruf then begin
        WithCRC_Merk:=TSerialMRGModem (SerialMRG).GetWithCRC;
        TSerialMRGModem (SerialMRG).SetWithCRC (false);
      end;
    end
    else if Rufentgegennahme AND not isFupAbruf AND (ModemRuf = mr_Modem) then begin
      { Rufentgegennahme per Modem von Modem-Geräten:
        1. Kennung abrufen über k-Befehl
           -> mit dem k-Befehl kann bei Geräten mit Auslesemodus "mit Passwort"
           die Kennung ohne Kenntnis des Passworts abgerufen werden (MRG 910 ab Ende 10/2002)
        2. wenn k-Befehl nicht bekannt, Kennung mit B003-Befehl auslesen
           -> für Kompatibilität mit MRG 910-Versionen ohne k-Befehl }
      AbfrageModus:=am_k_Befehl;
      Befehl:=GetMRGKommando_k
    end
    else begin   { in allen anderen Fällen: Kennung wird fest mit Parameter B003 abgerufen }
      AbfrageModus:=am_B003_Befehl;
      Befehl:=GetMRGKommando_B ('003');
    end;

    { Kennungsabfragekommando senden: }
    if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Kennung, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Soll-Kommando für Antwort-Validierung: }
    case AbfrageModus of
      am_Infobefehl : Kommando_Soll:=InfoBefehl;
      am_k_Befehl   : Kommando_Soll:='k';
      am_B003_Befehl: Kommando_Soll:='B003';
    else
      Kommando_Soll:='';
    end;

    if not ValidMRGAntwort (Kommando_Soll, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
      if (AbfrageModus = am_k_Befehl) AND
         (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_AENDERUNGNICHTZULAESSIG) then begin
        { wenn auf k-Befehl STX = ETX als Antwort kommt, ist im Gerät der Auslesemodus
          auf "mit Passwort" eingestellt (und der k-Befehl unbekannt) -> keine Rufentgegennahme möglich }
        FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_RE_PWACTIVATED);
        exit;
      end
      else if (AbfrageModus = am_k_Befehl) AND
              (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_KOMMANDOUNBEKANNT) then begin
        { wenn k-Befehl unbekannt, nochmal mit B003-Befehl probieren: }
        AbfrageModus:=am_B003_Befehl;
        Befehl:=GetMRGKommando_B ('003');
        if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Kennung, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
        if not ValidMRGAntwort ('B003', R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
      end
      else if (not isFupAbruf) AND ((ModemAbrufgruppe = 3) OR (ModemAbrufgruppe = 4)) AND
         (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_AENDERUNGNICHTZULAESSIG) then begin
        { Modem-Abrufgruppe 3, 4: wenn der Befehl vom MRG mit STX = ETX beantwortet wird,
          müssen Befehle mit Passwort geschickt werden: }
        TSerialMRGModem (SerialMRG).SetWithPasswort (true);
        if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Kennung, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
        if not ValidMRGAntwort (Kommando_Soll, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
          if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_AENDERUNGNICHTZULAESSIG) then begin
            { wenn jetzt wieder STX = ETX als Antwort kommt ist das Passwort falsch: }
            FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
            Keine_weiteren_Versuche:=true;
          end else
            FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
          exit;
        end;
      end
      else begin
        FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
    end;

    { Antwort auf Kennungsabfrage auswerten: }
    if AbfrageModus = am_Infobefehl then begin        { Kennung wurde über Infobefehl abgerufen }
      { Kennung aus Antwort auf Info-Befehl filtern: }
      StrPCopy (pInfo_Kennung, InfoData_Kennung);
      SplitMrgInfo (pInfo_Kennung, Left_Count, Left, Right);
      if Left_Count > 0 then
        Before:=Left_Count - 1
      else
        Before:=0;
      if Left = nil then
        cLeft:=NUL
      else
        cLeft:=Left[0];
      if Right = nil then
        cRight:=NUL
      else
        cRight:=Right[0];
      R.Antwort:=ExtractString (R.Antwort, cLeft, cRight, Before);

      if not isFupAbruf then         { WithCRC wieder in ursprünglichen Zustad versetzen }
        TSerialMRGModem (SerialMRG).SetWithCRC (WithCRC_Merk);
    end
    else if AbfrageModus = am_k_Befehl then begin     { Kennung wurde über k-Befehl abgerufen }
      S:=ExtractString (R.Antwort, STX, ETX, 0);
      R.Antwort:=F_Zerlegen (S, US);     { bis zum US lesen }
      Delete (R.Antwort, 1, 1);          { k rauslöschen -> R.Antwort enthält jetzt Kennung }
      if S = '1' then                    { S enthält Auslesemodus mit/ohne Passwort }
        TSerialMRGModem (SerialMRG).SetWithPasswort (true);
    end
    else if AbfrageModus = am_B003_Befehl then begin  { Kennung wurde über Parameter abgerufen }
      { Kennung aus Antwort auf B003-Befehl filtern: }
      R.Antwort:=ExtractString (R.Antwort, STX, ETX, 0);
      Delete (R.Antwort, 1, 4);          { Bnnn rauslöschen }
    end;
    R.Antwort:=F_LeftPad (R.Antwort, ' ', C_KennungLen);
  end;  { if ModemAbrufgruppe }

  MRGKennung:=Copy (R.Antwort, 1, C_KennungLen);    { MRGKennung enthält jetzt die richtige Gerätekennung }
  UpDateMRGJournalKennung (JournalId, MRGKennung);  { MRG-Kennung in Journal eintragen }
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
  ParaNrAllg: string;
  ParaNrMRG: string;

begin
  Result:=false;
  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_PasswortUebertragen, '', WithZustandTabelle);
  case RufStammdaten.PasswortNummer of
    1: ParaNrAllg:=CP_DFUE_PW1;
    2: ParaNrAllg:=CP_DFUE_PW2;
    3: ParaNrAllg:=CP_DFUE_PW3;
    4: ParaNrAllg:=CP_DFUE_PW4;
  else
    ParaNrAllg:=CP_DFUE_PW1;
  end;
  { Passwort: aus der allgemeinen Parameternummer die gerätespezifische ermitteln }
  if not GetParameterNrMrg_ByMrgTypDb (RufStammdaten.MrgTyp, ParaNrAllg, ParaNrMrg) then begin
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
    exit;
  end;
  Befehl:=STX+'Z'+ParaNrMrg+RufStammdaten.Passwort+ETX;

  { Paßwortübertragungskommando senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Login, ad_String, R, NoCarrier) then begin
    { Gegenstelle bricht Verbindung ab wegen ungültigem Passwort: }
    if (R.Fehlergruppe = COM_FUPERROR) AND (R.Fehlercode = FUPERR_FI) OR          { FUP }
       (R.Fehlergruppe = COM_MODEMPROTERROR) AND (R.Fehlercode = CMPE_DLE_EOT) OR { Modemabrufgruppe 1 (Modem-Protokoll) }
        NoCarrier then begin                                                      { Modemabrufgruppe 2 }
      FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
      Keine_weiteren_Versuche:=true;
    end else
      FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  if not ValidMRGAntwort ('Z', R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Paßwortübertragungskommando wird nicht ausgewertet }

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_LoginErfolgt, '', WithZustandTabelle);
  UpDateMRGJournal (JournalId, C_WJournal_DZLoggedIn);        { Login erfolgt }
  Result:=true;
end;

{-----------------------------------------------------------------------------------------}
function TAbruf.Init_IEC1107_Kommunikation (var Keine_weiteren_Versuche: boolean): boolean;
{-----------------------------------------------------------------------------------------}
{ einleiten der Geräte-Kommunikation nach Norm IEC 1107;
  Rückgabe: Keine_weiteren_Versuche (wenn PW falsch)
  Ergebnis: true, wenn erfolgreich }
var
  Befehl: string;
  i: integer;
  R: TRueckgabe;
  S: string;
  SBaudIdent: string;
  SchlossAdr: char;

begin
  Result:=false;
  { Weck-Befehl senden, es kommt keine Antwort ! }
  Befehl:='';
  for i:=1 to 53 do
   Befehl:=Befehl + NUL;
  SerialMRG.SendCommand (Befehl, [ETX], 1, 0, ad_String, R, NoCarrier);
  Sleep (1500);

  { Aufforderungstelegramm senden }
  Befehl:='/?!'+CR+LF;
  if not SerialMRG.SendCommand (Befehl, [LF], 1, Timeout_IEC1107Telegr, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { Aufforderungstelegramm-Antwort enthält:
    -> Hersteller (erste 3 Zeichen): nicht auswerten
    -> Baudratenidentifikation (4. Zeichen): bei DFÜ zwar keine Baudratenumschaltung
         notwendig, aber Baudratenidentifikations-Zeichen in nachfolgendem Quittierungs-/
         Optionsauswahltelegramm mitsenden ! (26.07.2005, WW)
    -> herstellerspez. Identifikation (z.B. bei Elster: Gerätetyp): nicht auswerten }
  S:=ExtractString (R.Antwort, '/', CR, 0);
  SBaudIdent:=Copy (S, 4, 1);

  { Quittierungs-/Optionsauswahltelegramm senden: }
  IEC1107_AbschaltBefehl:=true;  { Flag setzen, da ab jetzt vor dem Beenden der
                                   Verbindung Abschaltbefehl geschickt werden muß }
  TSerialMRGModem (SerialMRG).SetReceiveWithBCC (true);  { Flag setzen, es folgen Antworten mit BCC }

  Befehl:=ACK+'0'+SBaudIdent+'1'+CR+LF;
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_IEC1107Telegr, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { Antwort ist uninteressant, wird nicht ausgewertet }

  TSerialMRGModem (SerialMRG).SetSendWithBCC (true);  { Flag setzen, es folgen Befehle mit BCC }

  { Zugangsschloss prüfen, ggf. öffnen: }
  if RufStammDaten.PasswortNummer = 1 then   { "Login" erfolgt über Lieferantenschloss }
    SchlossAdr:='3'
  else                                       { "Login" erfolgt über Kundenschloss }
    SchlossAdr:='4';

  { Zustand des Lieferanten- bzw. Kundenschloss lesen: }
  Befehl:=GetLIS200Kommando_Lesen (SchlossAdr + ':170');
  if not SerialMrg.SendCommand (Befehl, [ETX], 1, Timeout_IEC1107Telegr, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  { Antwort auswerten: }
  IEC1107_SchlossStatus:=Schloss_bereits_offen;
  if Pos ('(1)', R.Antwort) = 0 then begin    { (1) nicht in der Antwort enthalten: Schloss ist zu ! }
    { Schloss öffnen: }
    ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_PasswortUebertragen, '', WithZustandTabelle);
    Befehl:=SOH+'P1'+STX+'('+SchlossAdr+':'+RufStammdaten.Passwort+')'+ETX;
    TSerialMRGModem (SerialMRG).SetReceiveWithBCC (false);  { Flag rücksetzen, es folgt Antwort ohne BCC }
    if not SerialMrg.SendCommand (Befehl, [ACK, NAK, ETX], 1, Timeout_IEC1107Telegr, ad_String, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_LOGINERROR + R.Fehlergruppe, R.Fehlercode);
      exit;
    end
    else begin
      if R.Antwort = ACK then begin                 { OK ! Schloss ist jetzt offen }
        if RufStammDaten.PasswortNummer = 1 then    { bei Login über Lieferantenschloss }
          IEC1107_SchlossStatus:=Lieferantenschloss_geoeffnet
        else                                        { bei Login über Kundenschloss }
          IEC1107_SchlossStatus:=Kundenschloss_geoeffnet;
        ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_LoginErfolgt, '', WithZustandTabelle);
        UpDateMRGJournal (JournalId, C_WJournal_DZLoggedIn);        { Login erfolgt }
      end
      else begin                               { Passwort wurde nicht akzeptiert }
        FehlerGruppeCodeUpdate (EST_LOGINERROR, LOGINERR_WRONGPW);
        Keine_weiteren_Versuche:=true;
        exit;
      end;
    end;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------------}
function TAbruf.Verbindungsaufbau_Fup (var Keine_weiteren_Versuche: boolean): boolean;
{------------------------------------------------------------------------------------}
{ Aufbau einer FUP-Verbindung mit MRG;
  Rückgabe: Keine_weiteren_Versuche = true, wenn kein Verbindungsaufbau aufgrund falscher
                                      Kennung oder Paßwort
  Ergebnis: true, wenn FUP-Verbindungsaufbau erfolgreich }
var
  Befehl: string;
  R: TRueckgabe;
  FUPAntwort: string;
  dummy: boolean;

begin
  Result:=false;

  { DÜ-Geschwindigkeit im FUP einstellen (nicht beim FUP-1200): }
  if not TSerialMRGFup (SerialMRG).isFUP1200 then
    if not SetDUEGeschwindigkeit_Fup then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_Verbindungaufbauen,
                  ' ' + Copy(RufStammdaten.Rufnummer, 2, length(RufStammdaten.Rufnummer)), WithZustandTabelle);
  Befehl:=ESC+'R'+RufStammdaten.Rufnummer+CR+ESC+'v'+CR;
  { Verbindungsaufbaukommando senden }
  if not SerialMRG.SendCommand (Befehl, [CR], 1, Timeout_Verbindungsaufbau, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  FupAntwort:=ExtractString (R.Antwort, ESC, CR, 0);
  if FupAntwort <> 'FV' then begin     { FUP-Fehlermeldung oder sonstige falsche Antwort }
    FehlerGruppeCodeUpdate (COM_FUPERROR, GetFupErrorcode (FupAntwort));
    exit;
  end;

  { Verbindung steht jetzt: }
  NoCarrier:=false;
  FormMainMRGAbruf.SetStatusColor (ps_VerbindungSteht);
  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_VerbindungSteht, '', WithZustandTabelle);
  UpDateMRGJournal (JournalId, C_WJournal_DZVerbSteht);

  { Kennungsabfrage (nur Dummy-Übergaben, die werden nur für Modem-Abrufe benötigt): }
  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_KennungAbfragen, '', WithZustandTabelle);
  if not KennungAbfragen (-1, '', '', false, dummy) then exit;

  { Geräte-Kennung mit Stammdaten-Kennung vergleichen: }
  if not KennungComp (MRGKennung, RufStammdaten.Kennung) then begin
    { Kennungen sind nicht identisch }
    if Stop_bei_falscher_Kennung then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_KEINE_VERBINDUNG);
      Keine_weiteren_Versuche:=true; { bei falscher Kennung sind weitere Versuche zwecklos }
      exit;
    end else
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_VERBINDUNG);
  end;

  { Paßwort übertragen: }
  if not PasswortLogin (Keine_weiteren_Versuche) then exit;
  Result:=true;
end;

{--------------------------------------------------------------------------------------}
function TAbruf.Verbindungsaufbau_Modem (var Keine_weiteren_Versuche: boolean): boolean;
{--------------------------------------------------------------------------------------}
{ Aufbau einer Modem-Verbindung mit MRG;
  Rückgabe: Keine_weiteren_Versuche = true, wenn kein Verbindungsaufbau aufgrund falscher
                                      Kennung oder Paßwort
  Ergebnis: true, wenn Modem-Verbindungsaufbau erfolgreich }
var
  Befehl: string;
  R: TRueckgabe;
  MRG800PTB: boolean;

begin
  Result:=false;
  TSerialMRGModem (SerialMRG).SetDCDCheck (false);      { DCD-Überwachung aus }

  { Schnittstellen-Parameter und Datenprotokoll zur Modem-Initialisierung
    aktivieren: }
  SerialMRG.Baudrate:=TSerialMRGModem (SerialMRG).MaxModemBaudrate; { höchstmögliche }
  TSerialMRGModem (SerialMRG).SetAbrufgruppe (0);

  { Modem initialisieren: }
  MRG800PTB:=MrgDefData.ModemAbrufgruppe = 3;
  Modem_Initialisieren (TSerialMRGModem (SerialMRG), MRG800PTB);

  { Baudrate für Abruf festlegen: }
  case MrgDefData.ModemAbrufgruppe of
    1, 2, 3: SerialMRG.Baudrate:=br_009600;
    4, 5:    SerialMRG.Baudrate:=TSerialMRGModem (SerialMRG).MaxModemBaudrate; { höchstmögliche }
  else
    SerialMRG.Baudrate:=br_009600;
  end;
  { Schnittstellen-Parameter und Datenprotokoll für Verbindungsaufbau aktivieren: }
  TSerialMRGModem (SerialMRG).SetAbrufgruppe (0);

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_Verbindungaufbauen,
                  ' ' + Copy(RufStammdaten.Rufnummer, 2, length(RufStammdaten.Rufnummer)), WithZustandTabelle);
  Befehl:='atd'+RufStammdaten.Rufnummer+CR;
  { Verbindungsaufbaukommando senden }
  if not TSerialMRGModem (SerialMRG).SendModemCommand (Befehl, Timeout_Verbindungsaufbau, R) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  FormMainMRGAbruf.ModemstatusMessage (Trim (R.Antwort));
  { Antwort auf Verbindungsaufbau-Kommando auswerten: }
  if not CheckModemConnect (R.Antwort) then exit;

  { Verbindung steht jetzt: }
  NoCarrier:=false;
  FormMainMRGAbruf.SetStatusColor (ps_VerbindungSteht);
  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_VerbindungSteht, '', WithZustandTabelle);
  UpDateMRGJournal (JournalId, C_WJournal_DZVerbSteht);

  TSerialMRGModem (SerialMRG).SetDCDCheck (true);       { DCD-Überwachung ein }
  Delay (200);       { Wartezeit (generell empfehlenswert, nötig für MRG 910) }

  { Schnittstellen-Parameter und Datenprotokoll für die nachfolgenden
    MRG-Befehle aktivieren: }
  TSerialMRGModem (SerialMRG).SetAbrufgruppe (MRGDefData.ModemAbrufgruppe);
  TSerialMRGModem (SerialMRG).SetPasswort (RufStammdaten.Passwort);

  case MrgDefData.ModemAbrufgruppe of
    { bei Geräten der Modem-Abrufgruppe 1 muß die Zentrale das Senderecht haben: }
    1:  begin
          { ENQ senden: }
          Befehl:='*' + ENQ + CR;
          if not SerialMRG.SendCommand (Befehl, [CR], 1, Timeouts_MRGModem.ACK01ProtMeldung, ad_String,
                                        R, NoCarrier) then begin
            FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
            exit;
          end;
        end;

    { bei Geräten der Modem-Abrufgruppe 3, 4 prüfen, ob MRG-Kommunikation mit CRC
      erfolgen muß: }
    3, 4: begin
            ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId,
                            z_GeraeteKonfiguration, '', WithZustandTabelle);
            if not TSerialMRGModem (SerialMRG).MRG_CRCKonfig_Check (R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
              exit;
            end;
          end;

    { bei Geräten der Modem-Abrufgruppe 5 Geräte-Kommunikation nach Norm IEC 1107
      einleiten: }
    5: begin
         ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId,
                         z_InitSequenz1107, '', WithZustandTabelle);
         if not Init_IEC1107_Kommunikation (Keine_weiteren_Versuche) then exit;
       end;
  end;

  { Kennungsabfrage: }
  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_KennungAbfragen, '', WithZustandTabelle);
  if not KennungAbfragen (MrgDefData.ModemAbrufgruppe, MRGDefData.Infobefehl,
                          MrgInfoData.Kennung, false, Keine_weiteren_Versuche) then exit;

  { Geräte-Kennung mit Stammdaten-Kennung vergleichen: }
  if not KennungComp (MRGKennung, RufStammdaten.Kennung) then begin
    { Kennungen sind nicht identisch }
    if Stop_bei_falscher_Kennung then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_KEINE_VERBINDUNG);
      Keine_weiteren_Versuche:=true; { bei falscher Kennung sind weitere Versuche zwecklos }
      exit;
    end else
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_VERBINDUNG);
  end;

  { Login: }
  case MrgDefData.ModemAbrufgruppe of
    1, 2: if not PasswortLogin (Keine_weiteren_Versuche) then exit;  { Paßwort übertragen }
    3, 4: begin
            { wenn Kennung-Befehl mit Passwort erfolgreich gesendet wurde,
              dann "Login" erfolgreich: }
            if TSerialMRGModem (SerialMRG).GetWithPasswort then begin
              ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_LoginErfolgt, '', WithZustandTabelle);
              UpDateMRGJournal (JournalId, C_WJournal_DZLoggedIn);
            end;
          end;
    { ModemAbrufgruppe 5: hier kein Login ("Login" erfolgt in Methode Init_IEC1107_Kommunikation
                          mit Öffnen des Lieferantenschlosses }
  end;
  Result:=true;
end;

{-------------------------------------------------------------------------}
function TAbruf.VerbAufbau (MrgId: TMrgId; Datentypen: integer;
                            var Keine_weiteren_Versuche: boolean): boolean;
{-------------------------------------------------------------------------}
{ Aufbau einer Verbindung mit MRG;
  Übergabe: MrgId
            Datentypen (für Journal)
  Rückgabe: Keine_weiteren_Versuche = true, wenn kein Verbindungsaufbau aufgrund falscher
                                      Kennung oder Paßwort
  Ergebnis: true, wenn kompletter Verbindungsaufbau incl. Kennungsvergleich und Login erfolgreich
            (Flag "NoCarrier" gibt bei fehlerhaftem Verbindungsaufbau Auskunft,
             ob die Verbindung noch steht oder nicht) }
Begin
  Result:=false;

  NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
  FehlerGruppeCodeUpdate (0, 0);                            { Vorbelegung: Ok }
  { Modemstatus-Anzeige bei Modem-Abruf: }
  if not isFupAbruf then
    FormMainMRGAbruf.ModemstatusMessage ('');

  { neuen Eintrag in Journaldatei schreiben: }
  JournalId:=WriteNewMRGJournal (MrgId, '', GetAbrufartString, Datentypen, SerialMRG.COMPort);

  { Ruf-Stammdaten ermitteln: }
  if not Stammdaten.GetRufStammdatenMrgId (MrgId,
                                           Systemdaten.Vorwahl,
                                           RufStammdaten) Then Begin
    FehlerGruppeCodeUpdate (ST_STAMMERROR, SMERR_RUFSTAMMDATEN);
    exit;
  end;

  { allgemeine Konfigurationsdaten für Gerätetyp ermitteln:
    -> für Modem-Abruf: Modem-Abrufgruppe des Gerätetyps ermitteln
    -> Infobefehl für Gerätetyp vorhanden ? }
  if not GetMrgDefDataDb (RufStammdaten.MrgTyp, MrgDefData) then begin
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;

  { wenn Infobefehl für Gerätetyp vorhanden, zusätzliche Informationen aus
    MRGINFO.DB holen: }
  if length (MrgDefData.Infobefehl) > 0 then begin
    if not GetMrgInfoDataDb (RufStammdaten.MrgTyp, MrgInfoData) then begin
      FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
      exit;
    end;
  end;

  { gerätetypabhängiges Laden der Parameter-Lesebefehle: }
  FillParameterBefehlListe;

  KennungMessage(RufStammdaten.Kennung);
  StationMessage(RufStammdaten.Stationsname);
  MRGKennung:=RufStammdaten.Kennung;        { Vorbelegung: Stammdaten-Kennung }

  if isFupAbruf then begin
    if not Verbindungsaufbau_Fup (Keine_weiteren_Versuche) then exit;
  end
  else begin
    if not Verbindungsaufbau_Modem (Keine_weiteren_Versuche) then exit;
  end;
  Result:=true;
End;

{--------------------------------------------}
function TAbruf.Verbindungsabbau_Fup: boolean;
{--------------------------------------------}
{ FUP-Verbindung wird abgebaut
  Ergebnis: true, wenn FUP-Verbindungsabbau ok }
Var
  Befehl: string;
  R: TRueckgabe;

Begin
  Result:=false;
  Befehl:=ESC+'e'+CR;
  { Verbindungsabbaukommando senden }
  if not SerialMRG.SendCommand (Befehl, [CR], 1, Timeout_Verbindungsabbau, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Verbindungsabbau-Kommando wird nicht ausgewertet }

  FormMainMRGAbruf.SetStatusColor (ps_KeineVerbindung);
  Result:=true;
End;

{----------------------------------------------}
function TAbruf.Verbindungsabbau_Modem: boolean;
{----------------------------------------------}
{ Modem-Verbindung wird abgebaut durch Rücksetzen der DTR-Leitung }
var
  Befehl: string;
  R: TRueckgabe;
Begin
  Result:=false;
  case MrgDefData.ModemAbrufgruppe of
    5: begin  { Geräte mit IEC 1107-Protokoll (z.B. Elster DL240, EK260) }
         { BCC-Einstellungen für die letzten Befehle setzen: }
         TSerialMrgModem (SerialMRG).SetSendWithBCC (true);
         TSerialMrgModem (SerialMRG).SetReceiveWithBCC (false);
         { wenn das Schloß, welches für den Abruf extra geöffnet wurde, muß vor
           dem Beenden der Verbindung auch wieder geschlossen werden: }
         if IEC1107_SchlossStatus = Lieferantenschloss_geoeffnet then begin
           { Befehl senden: Lieferantenschloss schließen }
           Befehl:=GetLIS200Kommando_Schreiben ('3:170', '0');
           SerialMrg.SendCommand (Befehl, [ACK, NAK], 1, Timeout_IEC1107Telegr, ad_String, R, NoCarrier);
         end
         else if IEC1107_SchlossStatus = Kundenschloss_geoeffnet then begin
           { Befehl senden: Kundenschloss schließen }
           Befehl:=GetLIS200Kommando_Schreiben ('4:170', '0');
           SerialMrg.SendCommand (Befehl, [ACK, NAK], 1, Timeout_IEC1107Telegr, ad_String, R, NoCarrier);
         end;

         { bei gesetztem Abschaltbefehl-Flag: Abschalt-Befehl senden, es kommt keine Antwort ! }
         if IEC1107_AbschaltBefehl then begin
           Befehl:=SOH+'B0'+ETX;
           SerialMRG.SendCommand (Befehl, [ETX], 1, 0, ad_String, R, NoCarrier);
         end;
       end;
  end;

  TSerialMRGModem (SerialMRG).SetDCDCheck (false);      { DCD-Überwachung aus }
  { Schnittstellen-Parameter und Datenprotokoll für die Modem-Antwort auf das
    Rücksetzen der DTR-Leitung einstellen (wie beim Senden des atd-Befehl): }
  TSerialMRGModem (SerialMRG).SetAbrufgruppe (0);

  SerialMRG.ClearDTRSignal;                          { DTR-Leitung rücksetzen }
  { OK vom Modem empfangen: Verbindung beendet }
  if TSerialMRGModem (SerialMRG).SendModemCommand ('', Timeout_Verbindungsabbau, R) then begin
    if Pos ('OK', AnsiUpperCase (R.Antwort)) <> 0 then
      Result:=true;
  end;
  SerialMRG.SetDTRSignal;                         { DTR-Leitung wieder setzen }
  FormMainMRGAbruf.ModemstatusMessage ('');
End;

{---------------------------------}
function TAbruf.VerbAbbau: boolean;
{---------------------------------}
{ Verbindung wird abgebaut
  Ergebnis: true, wenn Verbindungsabbau ok }
Begin
  Result:=false;
  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_VerbindungAbbauen, '', WithZustandTabelle);
  if isFupAbruf then begin
    if not Verbindungsabbau_Fup then exit;
  end else
    Verbindungsabbau_Modem;

  UpdateMRGJournal (JournalId, C_WJournal_DZVerbEnde);
  KennungMessage ('');
  StationMessage ('');
  ZustandMessage (-1, -1, z_Bereit, '', WithZustandTabelle);
  Result:=true;
end;

{--------------------------------------------}
function TAbruf.SetNextStdAbrufKanal: boolean;
{--------------------------------------------}
{ auf den nächsten abzurufenden Stundenwert-Abrufkanal weiterschalten;
  Ergebnis: false, wenn keine weiteren Kanäle abzurufen sind }
begin
  Result:=false;
  case MrgDefData.Kommandogruppe of
    102: begin        { Elster DL240 }
           inc (StdKanalNr);
           Result:=StdKanalNr < 4;
         end;
  end;  { case }
end;

{---------------------------------------------------------------------------}
Function TAbruf.AbrufParameter (Nummer: string; SaveToMom: boolean): boolean;
{---------------------------------------------------------------------------}
{ ruft einen oder alle Parameter im MRG ab;
  Übergabe: allg. Parameter-Nummer (0 = alle Parameter)
            SaveToMom (wenn true, dann Parameter in Momentanwerttabelle speichern )
  Ergebnis: true, wenn Parameterabruf erfolgreich }
Var
  Befehl: string;
  R: TRueckgabe;
  AnswerDest: TAnswerDest;
  ParaNrMrg: string;
  MrgKonvData: TMrgKonvDataDb;
  ParaKonv: TParaKonv;

Begin
  Result:=false;
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_ParameterAbrufen, '', WithZustandTabelle);
  if Nummer = '0' then     { alle Parameter }
    AnswerDest:=ad_File
  else begin               { einzelner Parameter }
    { aus der allgemeinen Parameternummer die gerätespezifische ermitteln: }
    GetParameterNrMrg_ByMrgTypDb (RufStammdaten.MrgTyp, Nummer, ParaNrMrg);
    AnswerDest:=ad_String;
  end;

  { Konfigurationsdaten für Parameterkonvertierung lesen: }
  if not GetMrgKonvDataDb (RufStammdaten.MrgTyp, MrgKonvData) then begin
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;
  { Übergaberecord für Parameterkonvertierung zusammenstellen: }
  with ParaKonv do begin
    ParaKonvGruppe:=MrgKonvData.ParaKonvGruppe;
    ParaGruppe:=MrgKonvData.ParameterGruppe;
    RohLoeschen:=RohdatenLoeschen;
  end;

  if MrgDefData.Kommandogruppe < 100 then begin
    { Kommando standardmäßig zusammensetzen und abrufen (Wieser-Geräte): }
    if Nummer = '0' then   { alle Parameter }
      Befehl:=GetMRGKommando_B ('')
    else                   { einzelner Parameter }
      Befehl:=GetMRGKommando_B (ParaNrMrg);

    { Parameterabruf-Kommando senden }
    if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Parameter, AnswerDest, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Parameter korrekt gelesen }
    if Nummer = '0' then begin               { Rohfile mit allen MRG-Parametern }
      if not ValidMRGAntwortfile ('B', R.Antwort, R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      ParameterListe.LoadFromFile (R.Antwort, ParaKonv, true);
    end
    else begin                             { String mit einzelnem MRG-Parameter }
      if not ValidMRGAntwort ('B' + ParaNrMrg, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
        FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
        exit;
      end;
      ParameterListe.LoadFromHeapA (R.Antwort, ParaKonv.ParaGruppe, true);
    end;
  end
  else begin
    { Fremdgeräte: }
    case MrgDefData.Kommandogruppe of
      101, 102:
        begin    { Elster DL240, EK260 }
          if Nummer = '0' then begin  { alle Parameter }
            { alle in der Parameter-Befehlsliste enthaltene Befehle senden und
              Daten empfangen (die Befehlsliste wird in der Verbindungsaufbau-Methode
              geladen): }
            if not TSerialMrgModem (SerialMrg).SendCommandList (
              ParameterBefehlListe, [ETX], 1, Timeout_IEC1107Telegr, ad_File, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
              exit;
            end;
          end
          else begin                  { einzelner Parameter }
            { Befehl bilden: }
            Befehl:=GetLIS200Kommando_Lesen (ParaNrMrg);
            { Parameterabruf-Kommando senden }
            if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_IEC1107Telegr, ad_File, R, NoCarrier) then begin
              FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
              exit;
            end;
          end;
          ParameterListe.LoadFromFile (R.Antwort, ParaKonv, true);
        end;
    end;  { case }
  end;

  if SaveToMom then
    ParameterListe.SaveToMomTable (RufStammDaten.MrgId);  { in Momentanwert-Tabelle abspeichern }
  Result:=true;
End;

{-------------------------------------------------------------------------}
function TAbruf.AbrufMeldungen (Modus: byte; von, bis: TDateTime): boolean;
{-------------------------------------------------------------------------}
{ Abrufen der Meldungen; MRGAbrufRec [dt_meldungen] wird je nach Modus erzeugt
  Übergabe: Abrufmodus (alles, neue, Bereich, nichts, MRGAbrufRec [dt_meldungen] nicht erzeugen)
            von-Zeitpunkt, bis-Zeitpunkt (für Abrufmodus Bereich)
  Ergebnis: true, wenn Meldungabruf ok }
Var
  R: TRueckgabe;
  Befehl: string;
  MAR: TAbrufRec;
  MrgAbrufData: TMrgAbrufDataDb;
  Kommando_Soll: string;

Begin
  Result:=false;
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_MeldungenAbrufen, '', WithZustandTabelle);
  CreateMeldungenAbrufRec (Modus, von, bis);
  if MrgDefData.Kommandogruppe < 100 then begin
    { Kommando standardmäßig tabellengesteuert zusammensetzen und abrufen (Wieser-Geräte): }
    if not GetMrgAbrufDataDb ('M', MrgDefData.Kommandogruppe, MrgAbrufData) then begin
      FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
      exit;
    end;
    GetMRGKommando_Archiv (MRGAbrufRec [dt_Meldungen].AbrufRec, MrgAbrufData.Kommandotyp,
                           MrgAbrufData.AbrufKommando, MrgAbrufData.AbrufKommando_alle_Daten,
                           MrgDefData.Jahreswechsel,
                           Befehl);

    { Meldungsabruf-Kommando senden }
    if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Meldungen, ad_File, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf Meldungsabruf-Kommando auswerten: }
    if length (Befehl) > 0 then
      Kommando_Soll:=Befehl[2]  // Zeichen nach dem STX
    else
      Kommando_Soll:='';
    if not ValidMRGAntwortfile (Kommando_Soll, R.Antwort, R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end
  else begin
    { Kommando per Quellcode zusammensetzen ohne zugrundeliegende Kommando-Definition aus Tabelle
      (Fremdgeräte, deren Kommando-Definitionen nicht in die Tabellenstruktur passen): }
    MAR:=MRGAbrufRec [dt_Meldungen].AbrufRec;
    case MrgDefData.Kommandogruppe of
      101: begin    { Elster EK260 }
             Befehl:=GetLIS200Kommando_Archiv (MAR, 4);            { Archiv 4 }
             { Befehl senden und Daten empfangen: }
             if not TSerialMrgModem (SerialMrg).SendIEC1107DatenTelegramm (Befehl, ad_File,
                                                                           Timeout_IEC1107Telegr, R, NoCarrier) then begin
               FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
               exit;
             end;
           end;

      102: begin    { Elster DL240 }
             Befehl:=GetLIS200Kommando_Archiv (MAR, 10);          { Archiv 10 }
             { Befehl senden und Daten empfangen: }
             if not TSerialMrgModem (SerialMrg).SendIEC1107DatenTelegramm (Befehl, ad_File,
                                                                           Timeout_IEC1107Telegr, R, NoCarrier) then begin
               FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
               exit;
             end;
           end;
    end;  { case }
  end;

  { Meldungen korrekt gelesen }
  MRGAbrufRec [dt_Meldungen].FileName:=R.Antwort;
  Result:=true;
End;


{-------------------------------------}
Function TAbruf.AbrufStdWerte: boolean;
{-------------------------------------}
{ Abrufen der Stundensätze; MRGAbrufRec [dt_messwerte] muß vor Aufruf der Funktion bereits erzeugt
  sein !
  Ergebnis: true, wenn Meßwertabruf ok }
Var
  R: TRueckgabe;
  Befehl: string;
  MAR: TAbrufRec;
  ArchivNr: integer;
  MrgAbrufData: TMrgAbrufDataDb;
  Kommando_Soll: string;

Begin
  Result:=false;
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_MesswerteAbrufen, '', WithZustandTabelle);

  if MrgDefData.Kommandogruppe < 100 then begin
    { Kommando standardmäßig tabellengesteuert zusammensetzen und abrufen (Wieser-Geräte): }
    if not GetMrgAbrufDataDb ('E', MrgDefData.Kommandogruppe, MrgAbrufData) then begin
      FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
      exit;
    end;
    GetMRGKommando_Archiv (MRGAbrufRec [dt_Messwerte].AbrufRec, MrgAbrufData.Kommandotyp,
                           MrgAbrufData.AbrufKommando, MrgAbrufData.AbrufKommando_alle_Daten,
                           MrgDefData.Jahreswechsel,
                           Befehl);

    { Meßwertabruf-Kommando senden }
    if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Messwerte, ad_File, R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { Antwort auf Meßwertabruf-Kommando auswerten: }
    if length (Befehl) > 0 then
      Kommando_Soll:=Befehl[2]  // Zeichen nach dem STX
    else
      Kommando_Soll:='';
    if not ValidMRGAntwortfile (Kommando_Soll, R.Antwort, R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end
  else begin
    { Kommando per Quellcode zusammensetzen ohne zugrundeliegende Kommando-Definition aus Tabelle
      (Fremdgeräte, deren Kommando-Definitionen nicht in die Tabellenstruktur passen): }
    MAR:=MRGAbrufRec [dt_Messwerte].AbrufRec;
    case MrgDefData.Kommandogruppe of
      101: begin    { Elster EK260 }
             Befehl:=GetLIS200Kommando_Archiv (MAR, 3);            { Archiv 3 }
             { Befehl senden und Daten empfangen: }
             if not TSerialMrgModem (SerialMrg).SendIEC1107DatenTelegramm (Befehl, ad_File,
                                                                           Timeout_IEC1107Telegr, R, NoCarrier) then begin
               FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
               exit;
             end;
           end;

      102: begin    { Elster DL240 }
             { Befehl bilden: es ist für jeden Kanal ein eigener Abruf nötig ! }
             ArchivNr:=StdKanalNr * 2; { Kanäle 1, 2, 3, 4 -> Archive 2, 4, 6, 8 }
             Befehl:=GetLIS200Kommando_Archiv (MAR, ArchivNr);
             { Befehl senden und Daten empfangen: }
             if not TSerialMrgModem (SerialMrg).SendIEC1107DatenTelegramm (Befehl, ad_File,
                                                                           Timeout_IEC1107Telegr, R, NoCarrier) then begin
               FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
               exit;
             end;
             { Kanalnummer durch Strichpunkt getrennt dem Rohfilenamen voranstellen, damit
               die Konvertierung weiß zu welchem Kanal die Rohdaten gehören ! }
             R.Antwort:=IntToStr (StdKanalNr) + ';' + R.Antwort;
           end;
    end;  { case }
  end;

  { Meßwerte korrekt gelesen }
  MRGAbrufRec [dt_Messwerte].FileName:=R.Antwort;
  Result:=true;
End;

{------------------------------------------------------------------------}
function TAbruf.AbrufTagWerte (Modus: byte; von, bis: TDateTime): boolean;
{------------------------------------------------------------------------}
{ Abrufen der Tagessätze; MRGAbrufRec [dt_tagessaetze] wird je nach Modus erzeugt
  Übergabe: Abrufmodus (alles, neue, Bereich, nichts, MRGAbrufRec [dt_tagessaetze] nicht erzeugen)
            von-Zeitpunkt, bis-Zeitpunkt (für Abrufmodus Bereich)
  Ergebnis: true, wenn Tagessatzabruf ok }
Var
  R: TRueckgabe;
  Befehl: string;
  MrgAbrufData: TMrgAbrufDataDb;
  Kommando_Soll: string;

Begin
  Result:=false;
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_TagessaetzeAbrufen, '', WithZustandTabelle);
  CreateTagessatzAbrufRec (Modus, von, bis);

  if not GetMrgAbrufDataDb ('F', MrgDefData.Kommandogruppe, MrgAbrufData) then begin
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;
  GetMRGKommando_Archiv (MRGAbrufRec [dt_Tagessaetze].AbrufRec, MrgAbrufData.Kommandotyp,
                         MrgAbrufData.AbrufKommando, MrgAbrufData.AbrufKommando_alle_Daten,
                         MrgDefData.Jahreswechsel,
                         Befehl);

  { Tagessatzabruf-Kommando senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Tagessaetze, ad_File, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Tagessatzabruf-Kommando auswerten: }
  if length (Befehl) > 0 then
    Kommando_Soll:=Befehl[2]  // Zeichen nach dem STX
  else
    Kommando_Soll:='';
  if not ValidMRGAntwortfile (Kommando_Soll, R.Antwort, R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

 { Tagessätze korrekt gelesen }
  MRGAbrufRec [dt_Tagessaetze].FileName:=R.Antwort;
  Result:=true;
End;

{----------------------------------------------------------------------------}
function TAbruf.DSfGUmschaltung (MrgId: TMrgId; Datentypen: integer): boolean;
{----------------------------------------------------------------------------}
{ Auf DSfG-Teilnehmer (Slave) umschalten;
  Übergabe: MrgId des DSfG-Teilnehmers, auf den umgeschaltet werden soll
            Datentypen des DSfG-Teilnehmers, auf den umgeschaltet werden soll (für Journal)
  Ergebnis: true, wenn Umschaltung ok }
var
  R: TRueckgabe;
  SlaveAdresse: char;
  dummy: TMrgId;
  Befehl: string;
  sdummy: string;

begin
  Result:=false;
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_DSfGUmschaltung, '', WithZustandTabelle);
  StammDaten.DSfGTyp (MrgId, SlaveAdresse, dummy);
  If not Stammdaten.GetRufStammdatenMrgId (MrgId,
                                           Systemdaten.Vorwahl,
                                           RufStammdaten) Then Begin
    FehlerGruppeCodeUpdate (ST_STAMMERROR, SMERR_RUFSTAMMDATEN);
    exit;
  end;

  Befehl:=STX + '}' + SlaveAdresse + ETX;

  { Umschalt-Kommando senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_DSfGUmschaltung, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Umschalt-Kommando auswerten: }
  if not ValidMRGAntwort ('}', R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  if not ValidDSfGUmschaltAntwort (R.Antwort, SlaveAdresse) then begin
    FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_UMSCHALTUNG);
    exit;
  end;

  { Umschaltung erfolgreich }

  KennungMessage (RufStammDaten.Kennung);
  StationMessage (RufStammDaten.Stationsname);

  { Kennung abfragen }

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_KennungAbfragen, '', WithZustandTabelle);
  Befehl:=GetMRGKommando_B ('003');

  { Abfrage-Kommando für Kennung senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Kennung, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Abfrage-Kommando für Kennung auswerten: }
  if not ValidMRGAntwort ('B003', R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  if not ValidSlaveKennung (R.Antwort, Rufstammdaten.Kennung, sdummy) then begin
    FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_INVKENNUNG_KEINE_UMSCHALTUNG);
    exit;
  end;

  { Kennung stimmt }

  UpdateMRGJournal(JournalId, C_WJournal_DZVerbEnde);

  { neuen Eintrag in Journaldatei schreiben: }
  JournalId:=WriteNewMRGJournal (MrgId, '', GetAbrufartString, Datentypen, SerialMRG.COMPort);
  UpdateMRGJournal(JournalId, C_WJournal_DZVerbSteht);
  UpdateMRGJournal(JournalId, C_WJournal_DZLoggedIn);
  UpdateMRGJournalKennung(JournalId, Rufstammdaten.Kennung);

  Result:=true;
end;

{--------------------------------------------------------------------------}
function TAbruf.AbrufPruefwerte (Modus: byte; von, bis: TDateTime): boolean;
{--------------------------------------------------------------------------}
{ Abrufen der Prüfungssätze; MRGAbrufRec [dt_pruefsaetze] wird je nach Modus erzeugt
  Übergabe: Abrufmodus (alles, neue, Bereich, nichts, MRGAbrufRec [dt_pruefsaetze] nicht erzeugen)
            von-Zeitpunkt, bis-Zeitpunkt (für Abrufmodus Bereich)
  Ergebnis: true, wenn Prüfsatzabruf ok }
Var
  R: TRueckgabe;
  Befehl: string;
  MrgAbrufData: TMrgAbrufDataDb;
  Kommando_Soll: string;

Begin
  Result:=false;
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_PruefungssaetzeAbrufen, '', WithZustandTabelle);
  CreatePruefSaetzeAbrufRec (Modus, von, bis);
  if not GetMrgAbrufDataDb ('X', MrgDefData.Kommandogruppe, MrgAbrufData) then begin
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;
  GetMRGKommando_Archiv (MRGAbrufRec [dt_Pruefsaetze].AbrufRec, MrgAbrufData.Kommandotyp,
                         MrgAbrufData.AbrufKommando, MrgAbrufData.AbrufKommando_alle_Daten,
                         MrgDefData.Jahreswechsel,
                         Befehl);

  { Prüfsatzabruf-Kommando senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Pruefsaetze, ad_File, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Prüfsatzabruf-Kommando auswerten: }
  if length (Befehl) > 0 then
    Kommando_Soll:=Befehl[2]  // Zeichen nach dem STX
  else
    Kommando_Soll:='';
  if not ValidMRGAntwortfile (Kommando_Soll, R.Antwort, R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

 { Prüfungssätze korrekt gelesen }
  MRGAbrufRec [dt_Pruefsaetze].FileName:=R.Antwort;
  Result:=true;
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
  Befehlsdatei: 1 Befehl je Datei, Inhalt = kompletter Befehlsstring (STX..ETX),
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

Begin
  Result:=false;
  if NoCarrier then exit;

  Pfad:=PathServer.PathName [WWorkDir];
  FileName:= 'MBEF' + Format('%.4d.*', [RufStammDaten.MrgId]);

  Result:=true;
  i:= FindFirst (Pfad + FileName, faAnyFile, SR);
  while (i = 0) AND not NoCarrier do begin
    if GetBefehlFromBinaerfile (Pfad + SR.Name, Befehl) then begin
      ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_BinaerdateiAbrufen, '', WithZustandTabelle);

      { Binärdatei-Kommando senden }
      if SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Binaerdatei, ad_File, R, NoCarrier) then begin
        { Antwort auf Binärdateibefehl korrekt gelesen }
        FileName:=R.Antwort;
        if ValidMRGAntwortfile ('', FileName, R.Fehlergruppe, R.Fehlercode, RohdatenLoeschen) then begin
          { Rohfile in Antwortfile umkopieren (überschreiben, falls schon vorhanden) }
          DestFileName:=Pfad + 'MANT' + Format('%.4d', [RufStammDaten.MrgId]) + ExtractFileExt (SR.Name);
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

{------------------------------------}
Procedure TAbruf.ResetRundpuffer_Meld;
{------------------------------------}
{ Rundpuffer für Meldungen im MRG zurücksetzen }
var
  R: TRueckgabe;
  Befehl: string;

Begin
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_ResetRundpufferMeld, '', WithZustandTabelle);
  { Rundpuffer zurücksetzen nur mit Paßwort 1: }
  if RufStammdaten.Passwortnummer <> 1 then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR, RPRESETERR_WRONGPASSWORDNR);
    exit;
  end;

  Befehl := STX + 'W' + ETX;

  { Kommando für Rundpufferreset senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_RundpufferReset, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kommando für Rundpufferreset auswerten: }
  if not ValidMRGAntwort ('W', R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR, RPRESETERR_NOSUCCESS)
    else
      FehlerGruppeCodeUpdate (EST_RPRESET_MEERROR+R.Fehlergruppe, R.Fehlercode);
  end;
End;

{------------------------------------}
Procedure TAbruf.ResetRundpuffer_Mess;
{------------------------------------}
{ Rundpuffer für Meßwerte im MRG zurücksetzen }
var
  R: TRueckgabe;
  Befehl: string;

Begin
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_ResetRundpufferMess, '', WithZustandTabelle);
  { Rundpuffer zurücksetzen nur mit Paßwort 1: }
  if RufStammdaten.Passwortnummer <> 1 then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR, RPRESETERR_WRONGPASSWORDNR);
    exit;
  end;

  Befehl := STX + 'T' + ETX;

  { Kommando für Rundpufferreset senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_RundpufferReset, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kommando für Rundpufferreset auswerten: }
  if not ValidMRGAntwort ('T', R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR, RPRESETERR_NOSUCCESS)
    else
      FehlerGruppeCodeUpdate (EST_RPRESET_MWERROR+R.Fehlergruppe, R.Fehlercode);
  end;
End;

{-------------------------------------}
Procedure TAbruf.ResetRundpuffer_Pruef;
{-------------------------------------}
{ Rundpuffer für Prüfungssätze im MRG zurücksetzen }
var
  R: TRueckgabe;
  Befehl: string;

Begin
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_ResetRundpufferPruef, '', WithZustandTabelle);
  { Rundpuffer zurücksetzen nur mit Paßwort 1: }
  if RufStammdaten.Passwortnummer <> 1 then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR, RPRESETERR_WRONGPASSWORDNR);
    exit;
  end;

  Befehl := STX + 'P' + ETX;

  { Kommando für Rundpufferreset senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_RundpufferReset, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kommando für Rundpufferreset auswerten: }
  if not ValidMRGAntwort ('P', R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR, RPRESETERR_NOSUCCESS)
    else
      FehlerGruppeCodeUpdate (EST_RPRESET_PRERROR+R.Fehlergruppe, R.Fehlercode);
  end;
End;

{-----------------------------------}
procedure TAbruf.ZeitSynchronisation;
{-----------------------------------}
{ Zeit im MRG auf PC-Zeit setzen }
Const
  C_Sicherheitsabstand = 30; { s }  { keine Sync in diesem Zeitraum vor einer vollen Stunde }
  C_KorrGetMRGZeit = 250; { ms }    { ungefährer Korrekturwert für ausgelesene MRG-Zeit  }
  C_KorrSetMRGZeit = 750; { ms }    { ungefährer Korrekturwert für Übertragung der neuen MRG-Zeit }

var
  R: TRueckgabe;
  Befehl: string;
  PCDateTime: TDateTime;
  MRGDateTime: TDateTime;
  NewMRGDateTime: TDateTime;
  MRGHour, PCHour: word;
  dummy1, dummy2, dummy3: word;
  Naechste_volle_Stunde_DateTime: TDateTime;
  DiffSec: longint;
  DiffSec_StdWechsel: longint;
  MRGTimeStr, MRGDateStr: string;
  NextSZToWZ: TDateTime;
  NextSZToWZHour: word;
  dummy4, dummy5: string;
  DatumParaNrMRG: string;
  ZeitParaNrMRG: string;

begin
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_GeraetezeitAbfragen, '', WithZustandTabelle);
  { Neuen "Leer-Eintrag" in ZeitSync-Tabelle anlegen: }
  WriteNewMRGZeitSync (JournalId);

  { Parameter "Datum" über allg. Parameternummer abrufen: }
  if not GetParameterNrMrg_ByMrgTypDb (RufStammdaten.MrgTyp, CP_ALLG_DATUM, DatumParaNrMrg) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
    exit;
  end;
  Befehl:=GetMRGKommando_B (DatumParaNrMrg);

  { Kommando für "Datum abrufen" senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Parameter, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kommando für "Datum abrufen" auswerten: }
  if not ValidMRGAntwort ('B' + DatumParaNrMRG, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  MRGDateStr:=copy(R.Antwort, 6, length(R.Antwort)-1);

  { Parameter "Zeit" über allg. Parameternummer abrufen: }
  if not GetParameterNrMrg_ByMrgTypDb (RufStammdaten.MrgTyp, CP_ALLG_ZEIT, ZeitParaNrMrg) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
    exit;
  end;
  Befehl:=GetMRGKommando_B (ZeitParaNrMrg);

  { Kommando für "Zeit abrufen" senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Parameter, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { PC-Zeit ermitteln:
    -> um sie mir der MRG-Zeit vergleichen zu können, wird die PC-Zeit gleich auf
       die im MRG herrschende Zeitzone umgerechnet }
  PCDateTime:=Now + GetWZ_SZ_OffsetNowFromDB (Rufstammdaten.MrgId, C_GerArtMrg);

  { Antwort auf Kommando für "Zeit abrufen" auswerten: }
  if not ValidMRGAntwort ('B' + ZeitParaNrMRG, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
  MRGTimeStr:=copy(R.Antwort, 6, length(R.Antwort)-1);

  { MRG-Zeit bereitstellen:
    -> String in DateTime konvertieren (geräteabhängig !)
    -> Korrektur wegen Übertragungslaufzeit }
  MRGDateTime:=MRGStrToDateTime(MrgDefData.Datumformat, MRGDateStr, MrgDefData.Zeitformat, MRGTimeStr) +
               EncodeTime (0, 0, 0, C_KorrGetMRGZeit);

  { MRG-Zeit und PC-Zeit in ZeitSync-Tabelle protokollieren: }
  UpdateMRGZeitSync (JournalId, MRGDateTime, PCDateTime);

  { Abweichung zwischen MRG-Zeit und PC-Zeit ermitteln: }
  F_TimeDiff(PCDateTime, MRGDateTime, DiffSec);
  DiffSec:=abs(DiffSec);

  { ZeitSync nur, wenn Abweichung >= ZeitSyncMin, ansonsten alles ok (keine Warnung)
    und fertig: }
  if DiffSec < Systemdaten.ZeitSyncMin then exit;

  { ZeitSync nur, wenn Abweichung <= ZeitSyncMax: }
  if DiffSec > Systemdaten.ZeitSyncMax then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_HIGHERMAX);
    exit;
  end;

  { Abweichung liegt innerhalb des Zeitfensters -> es muß synchronisiert werden }

  { Synchronisieren geht nur mit Paßwort 1: }
  if RufStammdaten.Passwortnummer <> 1 then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_WRONGPASSWORDNR);
    exit;
  end;

  { keine ZeitSynch zum Zeitpunkt der Umstellung von Sommer- auf Winterzeit wegen
    doppelt vorhandener Stunde: }
  NextSZToWZ:=GetNextSZ_To_WZFromDB;
  DecodeTime (Now, PCHour, dummy1, dummy2, dummy3);
  DecodeTime (NextSZToWZ, NextSZToWZHour, dummy1, dummy2, dummy3);
  if (Int (PCDateTime) = Int (NextSZToWZ)) AND (PCHour = (NextSZToWZHour-1)) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_SZTOWZ);
    exit;
  end;

  { ZeitSync nur bei gleichem Datum und gleicher Stunde: }
  DecodeTime(MRGDateTime, MRGHour, dummy1, dummy2, dummy3);
  DecodeTime(PCDateTime, PCHour, dummy1, dummy2, dummy3);
  if not ((int (MRGDateTime) = int (PCDateTime)) AND (MRGHour = PCHour)) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_DIFFERENTHOURS);
    exit;
  end;

  { MRG-Zeit und PC-Zeit müssen den Sicherheitsabstand zum nächsten
    Stundenwechsel einhalten: }
  Naechste_volle_Stunde_DateTime:=int (MRGDateTime) +
                                  EncodeTime(MRGHour, 0, 0, 0) +
                                  EncodeTime(1, 0, 0, 0);
  if MRGDateTime < PCDateTime then
    F_TimeDiff(PCDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel)
  else
    F_TimeDiff(MRGDateTime, Naechste_volle_Stunde_DateTime, DiffSec_StdWechsel);

  { ZeitSync nur bei genügend Sicherheitsabstand zum nächsten Stundenwechsel: }
  if DiffSec_StdWechsel <= C_Sicherheitsabstand then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_PERIODEND);
    exit;
  end;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_ZeitSynchronisieren, '', WithZustandTabelle);

  { neue MRG-Zeit bereitstellen:
    -> Korrektur der zu sendenden PC-Zeit wegen Übertragungslaufzeit: }
  { 28.08.2001 WW/GD; in NewMRGDateTime jetzt Stundenoffset berücksichtigt über
                      PCDateTime }                                                        
  NewMRGDateTime:=PCDateTime + EncodeTime (0, 0, 0, C_KorrSetMRGZeit);
  MRGTimeStr:=FormatMRGTime(MrgDefData.Zeitformat, NewMRGDateTime);   { geräteabhängig ! }

  Befehl:=GetMRGKommando_C (ZeitParaNrMRG, MRGTimeStr);
  { Kommando für "Zeit übertragen" senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Parametrieren, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf "Zeit übertragen"-Kommando auswerten: }
  if not ValidMRGAntwort ('C' + ZeitParaNrMRG, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Prüfen, ob MRG-Zeit tatsächlich geändert wurde: }
  if not MRGParameter_geaendert (R.Antwort, Dummy4, Dummy5) then begin
    if DiffSec > 1 then
      FehlerGruppeCodeUpdate (EST_ZEITSYNCERROR, ZSYNCERR_NOSUCCESS);
    { bei DiffSec = 1 liefert das Gerät evtl. Altwert = Neuwert. Das ist in diesem Fall
      kein Fehler, da wegen unvermeidbarer Ungenauigkeiten evtl. versucht wurde,
      die aktuelle Gerätezeit wieder zu parametrieren }
  end;
end;

{--------------------------------}
procedure TAbruf.RufDeaktivierung;
{--------------------------------}
{ Prüfen, ob Anzahl der eingegangenen Rufe einer Station das Maximum (Systemdaten-Einstellung)
  erreicht hat. Wenn ja, dann Parameter "Rufnummer Zentrale 1" im MRG auf "N" setzen }
var
  R: TRueckgabe;
  Befehl: string;
  WRufeDB: TWRufeDB;
  RufNrAlt, RufNrNeu: string;
  ParaNrMrg: string;

begin
  if NoCarrier then exit;

  { Ruf in Tabelle protokollieren und Prüfung, ob Rufanzahl erreicht: }
  WRufeDB:=TWRufeDB.Create (PathServer.PathName [WStammDir]);
  try
    { exit, wenn Rufanzahl noch nicht erreicht (kein Fehler): }
    if WRufeDB.WriteRuf (C_GerArtMrg, RufStammdaten.MrgId,
                         Systemdaten.MaxAnrufversuche_Station) then exit;
  finally
    WRufeDB.Free;
  end;

  { "Rufnummer Zentrale 1" im MRG auf "N" parametrieren: }

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_RufDeaktivieren, '', WithZustandTabelle);

  { Parametrieren nur mit Paßwort 1: }
  if RufStammdaten.Passwortnummer <> 1 then begin
    FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR, RUFDEAKTERR_WRONGPASSWORDNR);
    exit;
  end;

  { MRG-spezifischen Parameter für "Rufnummer Zentrale 1" ermitteln: }
  if not GetParameterNrMrg_ByMrgTypDb (RufStammdaten.MrgTyp, CP_DFUE_Zentrale1, ParaNrMrg) then begin
    FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR++ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
    exit;
  end;

  RufNrNeu:='N';
  Befehl:=GetMRGKommando_C (ParaNrMRG, RufNrNeu);

  { Kommando für "Rufnummer Zentrale 1 übertragen" senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Parametrieren, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf "Rufnummer Zentrale 1 übertragen"-Kommando auswerten: }
  if not ValidMRGAntwort ('C' + ParaNrMrg, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR+R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Prüfen, ob Rufnummer im MRG tatsächlich geändert wurde: }
  if not MRGParameter_geaendert (R.Antwort, RufNrAlt, RufNrNeu) then begin
    FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR, RUFDEAKTERR_NOSUCCESS);
    exit;
  end;

  FehlerGruppeCodeUpdate (EST_RUFDEAKTERROR, RUFDEAKTERR_SUCCESS);

  { erfolgreiche Rufdeaktivierung und Rufnummer in Tabelle protokollieren: }
  WRufeDB:=TWRufeDB.Create (PathServer.PathName [WStammDir]);
  try
    WRufeDB.WriteRufDeaktivierung (C_GerArtMrg, RufStammdaten.MrgId, RufNrAlt);
  finally
    WRufeDB.Free;
  end;
end;

{----------------------------------------}
function TAbruf.RufReaktivierung: boolean;
{----------------------------------------}
{ Parameter "Rufnummer Zentrale 1" im MRG zurück auf alten Wert setzen;
  Ergebnis: true, wenn Rufreaktivierung erfolgreich }
var
  R: TRueckgabe;
  Befehl: string;
  WRufeDB: TWRufeDB;
  RufNrAlt, RufNrNeu: string;
  RufNrMerk: string;
  erg: boolean;
  ParaNrMrg: string;

begin
  Result:=false;
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_RufReaktivieren, '', WithZustandTabelle);
  { Parametrieren nur mit Paßwort 1: }
  if RufStammdaten.Passwortnummer <> 1 then begin
    FehlerGruppeCodeUpdate (ST_RUFREAKTERROR, RUFREAKTERR_WRONGPASSWORDNR);
    exit;
  end;

  { Rufnummer aus Tabelle lesen: }
  WRufeDB:=TWRufeDB.Create (PathServer.PathName [WStammDir]);
  try
    if not WRufeDB.GetRufnummer (C_GerArtMrg, RufStammdaten.MrgId, RufNrNeu) then begin
      FehlerGruppeCodeUpdate (ST_RUFREAKTERROR, RUFREAKTERR_FINDRUFNR);
      exit;
    end;
  finally
    WRufeDB.Free;
  end;

  { MRG-spezifischen Parameter für "Rufnummer Zentrale 1" ermitteln: }
  if not GetParameterNrMrg_ByMrgTypDb (RufStammdaten.MrgTyp, CP_DFUE_Zentrale1, ParaNrMrg) then begin
    FehlerGruppeCodeUpdate (+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
    exit;
  end;

  Befehl:=GetMRGKommando_C (ParaNrMRG, RufNrNeu);
  { Kommando für "Rufnummer Zentrale 1 übertragen" senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Parametrieren, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf "Rufnummer Zentrale 1 übertragen"-Kommando auswerten: }
  if not ValidMRGAntwort ('C' + ParaNrMrg, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Prüfen, ob Rufnummer im MRG tatsächlich geändert wurde. Es ist möglich, daß die neu zu parametrierende Rufnummer
    bereits im Gerät eingestellt ist. Die Rufreaktivierung ist in diesem Fall ok ! }
  RufNrMerk:=RufNrNeu;                    { Rufnummer merken, die im Gerät parametriert werden soll }
  erg:=MRGParameter_geaendert (R.Antwort, RufNrAlt, RufNrNeu);
  if not erg AND (RufNrNeu <> RufNrMerk) then begin
    FehlerGruppeCodeUpdate (ST_RUFREAKTERROR, RUFREAKTERR_NOSUCCESS);
    exit;
  end;

  { erfolgreiche Rufreaktivierung in WRufe-Tabelle protokollieren: }
  WRufeDB:=TWRufeDB.Create (PathServer.PathName [WStammDir]);
  try
    WRufeDB.WriteRufReaktivierung (C_GerArtMrg, RufStammdaten.MrgId);
  finally
    WRufeDB.Free;
  end;

  Result:=true;
end;

{-------------------------------------}
Procedure TAbruf.UebertragungParameter;
{-------------------------------------}
{ Parameter ins MRG übertragen }
Var
  Befehl: string;
  R: TRueckgabe;
  TbMRGParametrierung: TTbMRGParametrierung;
  ParaSendenData: ParaSendenDataRec;
  WertAlt, WertNeu: string;
  ParaErgebnisData: ParaErgebnisDataRec;
  ParaNrMrg: string;
  Merk: boolean;
  OK: boolean;

Begin
  if NoCarrier then exit;
  TbMRGParametrierung:=TTbMRGParametrierung.Create (PathServer.PathName [WNetWorkDir],
                                                    RufStammDaten.MrgId);
  try
    ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_ParameterUebertragen, '', WithZustandTabelle);
    while TbMRGParametrierung.GetSendeParameter (ParaSendenData) AND not NoCarrier do begin
      with ParaSendenData do begin
        if Stellen > 0 then
          ParaWert:=F_RightPad (ParaWert, ' ', Stellen);
        WertNeu:=ParaWert;
      end;

      { MRG-spezifische Parameternummer ermitteln: }
      if not GetParameterNrMrg_ByMrgTypDb (RufStammdaten.MrgTyp, ParaSendenData.ParaNr, ParaNrMrg) then begin
        FehlerGruppeCodeUpdate (EST_PARAMERROR+ST_KONFIGERROR, KFERR_PARAMETERNOTFOUND);
        exit;
      end;

      if MrgDefData.Kommandogruppe < 100 then begin  { standardmäßige Parametrierung (Wieser-Geräte) }
        Befehl:=GetMRGKommando_C (ParaNrMrg, WertNeu);
        { Kommando für "Parameter übertragen" senden }
        if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_Parametrieren, ad_String, R, NoCarrier) then begin
          FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
          Continue;
        end;

        { Antwort auf "Parameter übertragen"-Kommando auswerten: }
        if not ValidMRGAntwort ('C' + ParaNrMrg, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
          FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
          Continue;
        end;

        { Prüfen, ob Parameter im MRG tatsächlich geändert wurde: }
        if not MRGParameter_geaendert (R.Antwort, WertAlt, WertNeu) then begin
          FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
          Continue;
        end;
      end
      else begin
        { Fremdgeräte: }
        case MrgDefData.Kommandogruppe of
          101, 102:
            begin    { Elster DL240, EK260 }
              Befehl:=GetLIS200Kommando_Schreiben (ParaNrMrg, WertNeu);
              Merk:=TSerialMRGModem (SerialMRG).GetReceiveWithBCC;  { Receice-Flag merken }
              TSerialMRGModem (SerialMRG).SetReceiveWithBCC (false); { Flag setzen, es folgt Antwort ohne BCC }

              { Kommando für "Parameter übertragen" senden }
              OK:=SerialMrg.SendCommand (Befehl, [ACK, NAK], 1, Timeout_IEC1107Telegr, ad_String, R, NoCarrier);
              TSerialMRGModem (SerialMRG).SetReceiveWithBCC (Merk); { Receive-Flag auf ursprünglichen Wert wieder zurücksetzen }
              if not OK then begin
                FehlerGruppeCodeUpdate (EST_PARAMERROR+R.Fehlergruppe, R.Fehlercode);
                Continue;
              end;

              { Antwort auf "Parameter übertragen"-Kommando auswerten:
                -> prüfen, ob Parameter im Gerät tatsächlich geändert wurde }
              if R.Antwort <> ACK then begin
                FehlerGruppeCodeUpdate (EST_PARAMERROR, PARAMERR_NOSUCCESS);
                Continue;
              end;

              WertAlt:='';  { unbekannt, Parametrierung funktioniert im Mom32 trotzdem }
            end;
        end;  { case }
      end;

      { Ergebnis der Übertragung in Parametriertabelle schreiben: }
      with ParaErgebnisData do begin
        ParaNr:=ParaSendenData.ParaNr;
        ParaWertAlt:=WertAlt;
        ParaWertNeu:=WertNeu;
      end;
      TbMRGParametrierung.WriteErgebnis (ParaErgebnisData);

      { geänderten Parameter auch in Momentanwerttabelle aktualisieren: }
      AktuMomTable (RufStammdaten.MrgId, ParaSendenData.ParaNr, WertNeu);

      { Stammdatenaktualisierung, wenn gefordert: }
      if ParaSendenData.StaAktu then begin
        if (ParaSendenData.ParaNr = CP_Allg_MRGKennung) OR               { -> für Wieser-Geräte }
           (ParaSendenData.ParaNr = CP_ELS_Stationsnummer) then begin    { -> für Elster-Geräte }
          Stammdaten.SetKennung (RufStammdaten.MrgId, WertNeu);
          SetLGZ_Kennung (WertAlt, WertNeu);                             { Kennung auch in LGZ-Verzeichnisdatei ändern }
        end;
        if (ParaSendenData.ParaNr = CP_DFUE_PW1) OR
           (ParaSendenData.ParaNr = CP_ELS_Lieferantenschloss) then
          Stammdaten.SetPasswort (RufStammdaten.MrgId, 1, WertNeu);
        if (ParaSendenData.ParaNr = CP_DFUE_PW2) OR
           (ParaSendenData.ParaNr = CP_ELS_Kundenschloss) then
          Stammdaten.SetPasswort (RufStammdaten.MrgId, 2, WertNeu);
        if ParaSendenData.ParaNr = CP_DFUE_PW3 then
          Stammdaten.SetPasswort (RufStammdaten.MrgId, 3, WertNeu);
        if ParaSendenData.ParaNr = CP_DFUE_PW4 then
          Stammdaten.SetPasswort (RufStammdaten.MrgId, 4, WertNeu);
      end;
    end; { while }
    { Triggerfile schreiben, wenn alle Parameter übertragen und Ergebnisse geschrieben wurden: }
    WriteNewTime(PathServer.PathName [WNetWorkDir] +
                 C_TbMPE + Format('%.4d.DB', [RufStammDaten.MrgId]));
  finally
    TbMRGParametrierung.Free;
  end;
End;

{--------------------------------------------------------------}
function TAbruf.RueckrufAusloesung (Zentrale: integer): boolean;
{--------------------------------------------------------------}
{ Rückruf im MRG auslösen;
  Übergabe: Nr. der Zentrale (1 oder 2)
  Ergebnis: true, wenn Rückrufauslösung erfolgreich }
var
  Befehl: string;
  R: TRueckgabe;

begin
  Result:=false;
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_RueckRufAusloesen, '', WithZustandTabelle);
  if Zentrale = 1 then
    Befehl:='A'
  else
    Befehl:='a';

  { Kommando für Rufauslösung senden }
  if not SerialMRG.SendCommand (STX + Befehl + ETX, [ETX], 1, Timeout_Rufausloesung, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Kommando für Rufauslösung auswerten: }
  if not ValidMRGAntwort (Befehl, R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (ST_RUECKRUFERROR, RUECKRUFERR_AUSLOESUNG)
    else
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  Result:=true;
end;

{---------------------------------------------------------------}
function TAbruf.RufListeAbfragen (var RufListe: string): boolean;
{---------------------------------------------------------------}
{ Rufanregungsliste abfragen; dieser String (Länge 14) enthält für jeden Slave, der
  einen Ruf ausgelöst hat, dessen DSfG-Adresse bzw. 0, wenn der Slave keinen Ruf ausgelöst hat
  Rückgabe: Ruflisten-String
  Ergebnis: true, wenn Ruflistenabfrage ok }
Var
  R: TRueckgabe;
  Befehl: string;

begin
  Result:=false;
  RufListe:='';
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_RuflisteAbfragen, '', WithZustandTabelle);
  Befehl:=STX + '#' + ETX;

  { Ruflistenabfrage-Kommando senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_DSfGRufliste, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Ruflistenabfrage-Kommando auswerten: }
  if not ValidMRGAntwort ('#', R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_RUFLISTE)
    else
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Rufliste aus Antwort extrahieren: }
  RufListe:=GetRufListe (R.Antwort);
  Result:=true;
end;

{--------------------------------------------------}
procedure TAbruf.SlaveRufQuittieren (Adresse: char);
{--------------------------------------------------}
{ Slave-Ruf in Rufliste quittieren;
  Übergabe: DSfG-Adresse des Slave }
var
  Befehl: string;
  R: TRueckgabe;

begin
  if NoCarrier then exit;

  ZustandMessage (SerialMRG.COMPort, RufStammdaten.MrgId, z_RufQuittieren, '', WithZustandTabelle);
  Befehl:=STX + '#' + Adresse + ETX;

  { Rufquittierungs-Kommando senden }
  if not SerialMRG.SendCommand (Befehl, [ETX], 1, Timeout_DSfGRufQuittung, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Antwort auf Rufquittierungs-Kommando auswerten: }
  if not ValidMRGAntwort ('#', R.Antwort, R.Fehlergruppe, R.Fehlercode) then begin
    if (R.Fehlergruppe = COM_MRGERROR) AND (R.Fehlercode = MRGERR_ANTWORTUNERWARTET) then
      FehlerGruppeCodeUpdate (ST_DSFGUMLERROR, UMLERR_RUFQUITTIEREN)
    else
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;
end;

{---------------------------------------}
function TAbruf.RufAnnehmen_Fup: boolean;
{---------------------------------------}
{ angekommenen Ruf per FUP annehmen;
  Ergebnis: true, wenn Rufannahme ok }
var
  Befehl: string;
  R: TRueckgabe;

Begin
  Result:=false;

  Befehl:=ESC+'z'+CR;
  { Rufannahmekommando senden, es kommt keine Antwort ! }
  if not SerialMRG.SendCommand (Befehl, [CR], 1, 0, ad_String, R, NoCarrier) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  { Ruf wurde entgegengenommen, Verbindung steht jetzt: }
  NoCarrier:=false;
  FormMainMRGAbruf.SetStatusColor (ps_VerbindungSteht);
  ZustandMessage (SerialMRG.COMPort, -1, z_VerbindungSteht, '', WithZustandTabelle);
  UpdateMRGJournal (JournalId, C_WJournal_DZVerbSteht);

  Result:=true;
end;

{-----------------------------------------}
function TAbruf.RufAnnehmen_Modem: boolean;
{-----------------------------------------}
{ angekommenen Ruf per Modem annehmen;
  Achtung: Es können nur Rufe von Geräten der Modem-Abrufgruppe 1 angenommen werden !
  Ergebnis: true, wenn Rufannahme ok }
var
  Befehl: string;
  R: TRueckgabe;

Begin
  Result:=false;
  TSerialMRGModem (SerialMRG).SetDCDCheck (false);      { DCD-Überwachung aus }

  { Schnittstellen-Parameter und Datenprotokoll für Rufannahme-Befehl sind bereits durch
    die letzte Modeminitialisierung festgelegt }

  Befehl:='ata' + CR;
  { Rufannahmekommando senden: }
  if not TSerialMRGModem (SerialMRG).SendModemCommand (Befehl, Timeout_RufAnnahmeModem, R) then begin
    FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
    exit;
  end;

  FormMainMRGAbruf.ModemstatusMessage (Trim (R.Antwort));
  { Antwort auf Verbindungsaufbau-Kommando auswerten: }
  if not CheckModemConnect (R.Antwort) then exit;

  { Ruf wurde entgegengenommen, Verbindung steht jetzt: }
  NoCarrier:=false;
  TSerialMRGModem (SerialMRG).SetDCDCheck (true);       { DCD-Überwachung ein }
  Delay (200);       { Wartezeit (generell empfehlenswert, nötig für MRG 910) }

  FormMainMRGAbruf.SetStatusColor (ps_VerbindungSteht);
  ZustandMessage (SerialMRG.COMPort, -1, z_VerbindungSteht, '', WithZustandTabelle);
  UpdateMRGJournal (JournalId, C_WJournal_DZVerbSteht);

  if ModemRuf = mr_Modem then begin       { Anruf eines Modem-Geräts annehmen }
    { Schnittstellen-Parameter und Datenprotokoll für die nachfolgenden
      MRG-Befehle für Abrufgruppe 4 (MRG 910 !) aktivieren: }
    TSerialMRGModem (SerialMRG).SetAbrufgruppe (4);
    {  prüfen, ob MRG-Kommunikation mit CRC erfolgen muß: }
    if not TSerialMRGModem (SerialMRG).MRG_CRCKonfig_Check (R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (EST_KENNUNGCHECK + R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end
  else begin                                { Anruf eines FUP-Geräts annehmen }
    { Schnittstellen-Parameter und Datenprotokoll für die nachfolgenden
      MRG-Befehle nach ACK01-Protokoll aktivieren: }
    TSerialMRGModem (SerialMRG).SetAbrufgruppe (1);

    { vom Gerät gesendetes ENQ entgegennehmen: }
    if not SerialMRG.SendCommand ('', [CR], 1, Timeouts_MRGModem.ACK01ProtMeldung, ad_String,
                                  R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;

    { bei Geräten der Modem-Abrufgruppe 1 muß die Zentrale das Senderecht haben: }
    { ENQ senden: }
    Befehl:='*' + ENQ + CR;
    if not SerialMRG.SendCommand (Befehl, [CR], 1, Timeouts_MRGModem.ACK01ProtMeldung, ad_String,
                                  R, NoCarrier) then begin
      FehlerGruppeCodeUpdate (R.Fehlergruppe, R.Fehlercode);
      exit;
    end;
  end;
  Result:=true;
end;

{----------------------------------}
function TAbruf.RufAnnahme: boolean;
{----------------------------------}
{ angekommenen Ruf annehmen;
  Ergebnis: true, wenn Rufannahme ok }
var
  OK: boolean;
  dummy: boolean;
Begin
  Result:=false;
  NoCarrier:=true;                 { Vorbelegung: es besteht keine Verbindung }
  FehlerGruppeCodeUpdate (0, 0);                            { Vorbelegung: Ok }
  { Modemstatus-Anzeige bei Modem-Abruf: }
  if not isFupAbruf then
    FormMainMRGAbruf.ModemstatusMessage ('');

  ZustandMessage (SerialMRG.COMPort, -1, z_RufAnnehmen, '', WithZustandTabelle);
  { Journal-Eintrag für die bislang nicht identifizierte, rufende Station (MrgId = 0, Kennung = '': }
  JournalId:=WriteNewMRGJournal (0, '', GetAbrufartString,
                                 SystemDaten.RufDatentypen, SerialMRG.COMPort);
  if isFupAbruf then
    OK:=RufAnnehmen_Fup
  else
    OK:=RufAnnehmen_Modem;

  if not OK then begin
    { Trigger-Datei für RE-Client schreiben }
    WriteNewTime(Pathserver.Pathname [WStammDir] + C_RufTriggerFile);
    exit;
  end;

  { Ruf wurde entgegengenommen, jetzt Kennung abfragen: }
  ZustandMessage (SerialMRG.COMPort, -1, z_KennungAbfragen, '', WithZustandTabelle);
  if not KennungAbfragen (-1, '', '', true, dummy) then begin  { Übergaben für Rufentgegennahme per FUP und Modem }
    { Trigger-Datei für RE-Client schreiben }
    WriteNewTime(Pathserver.Pathname [WStammDir] + C_RufTriggerFile);
    exit;
  end;

  { Stammsatz für MRG-Kennung suchen: }
  if not Stammdaten.GetRufStammdatenKennung (MRGKennung,
                                             Systemdaten.Vorwahl,
                                             RufStammdaten) then begin
    { nicht gefunden: Geräte-Kennung ins Journal schreiben }
    UpdateMRGJournalKennung (JournalId, MRGKennung);
    FehlerGruppeCodeUpdate (EST_KENNUNGCHECK, KENNERR_KEINSTAMMSATZ);
    { Trigger-Datei für RE-Client schreiben }
    WriteNewTime(Pathserver.Pathname [WStammDir] + C_RufTriggerFile);
    exit;
  end;

  { ab jetzt sind die Ruf-Stammdaten des anrufenden Geräts bekannt }

  MRGKennung:=RufStammdaten.Kennung;
  KennungMessage (RufStammDaten.Kennung);
  StationMessage (RufStammDaten.Stationsname);

  { MrgId und Kennung aus Stammdaten ins Journal schreiben: }
  UpdateMRGJournalMrgId (JournalId, RufstammDaten.MrgId);
  UpdateMRGJournalKennung (JournalId, MRGKennung);
  { Trigger-Datei für RE-Client schreiben }
  WriteNewTime(Pathserver.Pathname [WStammDir] + C_RufTriggerFile);

  { allgemeine Konfigurationsdaten für Gerätetyp ermitteln:
    -> für Modem-Abruf: Modem-Abrufgruppe des Gerätetyps ermitteln }
  if not GetMrgDefDataDb (RufStammdaten.MrgTyp, MrgDefData) then begin
    FehlerGruppeCodeUpdate (ST_KONFIGERROR, KFERR_KONFIGDATANOTFOUND);
    exit;
  end;

  { Login: }
  if isFupAbruf then begin
    if not PasswortLogin (dummy) then exit;              { Paßwort übertragen }
  end
  else begin
    TSerialMRGModem (SerialMRG).SetPasswort (RufStammdaten.Passwort);
    case MrgDefData.ModemAbrufgruppe of
      1, 2: if not PasswortLogin (dummy) then exit;        { Paßwort übertragen }
      3, 4: begin
              { wenn Kennung-Befehl mit Passwort erfolgreich gesendet wurde,
                dann "Login" erfolgreich: }
              if TSerialMRGModem (SerialMRG).GetWithPasswort then begin
                ZustandMessage (SerialMRG.COMPort, RufStammDaten.MrgId, z_LoginErfolgt, '', WithZustandTabelle);
                UpDateMRGJournal (JournalId, C_WJournal_DZLoggedIn);
              end;
            end;
      { ModemAbrufgruppe 5: hier kein Login ("Login" erfolgt in Methode Init_IEC1107_Kommunikation
                            mit Öffnen des Lieferantenschlosses }
    end;
  end;
  Result:=true;
End;

end.

