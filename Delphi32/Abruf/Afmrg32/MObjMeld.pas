{*************************************************************************************}
{* Unit: MRG-Meldungskonvertierung und -verarbeitung                                 *}
{* 27.11.1998 WW  abgeleitet von Konvertierung f�r 16-Bit-Win-DF�                    *}
{* 23.08.1999 WW  ASCII-Konvertierung der Meldungen: 3 oder 5-stellige (IFDEF Meld5) *}
{*                Meldungsnummern                                                    *}
{* 22.09.1999 Sm  TParaChange.CreateFromString l�ngenpr�fung wenn kein"_" dann crash *}
{* 06.06.2000 WW  EC694 aufgenommen                                                  *}
{* 16.10.2000 WW  Fehler beim Abspeichern von Mehrfach-Eintr�gen in GesMeld-Tabelle  *}
{*                behoben                                                            *}
{* 12.02.2001 GD  erweitert um automatischen Export                                  *}
{* 21.03.2001 GD  Provisorisches Fehlerhandling f�r fehlende Meldungsnummer          *}
{* 13.06.2001 WW  Umstellung auf gemeinsame MRG/DSfG-Meldungstabellen                *}
{* 17.04.2002 WW  mit Alarm-Tabelle                                                  *}
{* 13.05.2002 WW  TMeldungsliste mit Properties DataValid, AnzahlNeueMeldungen       *}
{* 23.05.2002 WW  erweitert um Excel-Konvertierung                                   *}
{* 10.10.2002 WW  erweitert um Konvertierung f�r Elster DL240 und EK260              *}
{* 19.02.2003 WW  Objekt-Funktionalit�ten von TMeldungsListe aufgeteilt auf          *}
{*                TMeldungsListe (ger�tespezifische Konvertierungen, Zugriff auf     *}
{*                Konfigurationsdateien, Datenbank-unabh�ngig !) und davon abgeleitet*}
{*                TMeldungsListeDB (Zugriff auch auf Konfigurationen in Tabellen,    *}
{*                Abspeichern in Tabellen, ASCII-Export)                             *}
{* 08.05.2008 WW  mit Schreiben der Alarmdatei f�r Tritschler SR-Meldungen           *}
{* 03.06.2009 WW  erweitert um Konvertierung f�r EC 900                              *}
{* 30.04.2012 WW  Erweiterte Datenplausibilisierung f�r Konvertierung Elster DL240,  *}
{*                EK260 (KonvMeldungenF)                                             *}
{* 27.03.2013 WW  erweitert um Konvertierung f�r Kamstrup UNIGAS 300                 *}
{* 24.01.2014 WW  KonvMeldungenF: erweitert f�r EK280, angepa�t an Abruf mit Block-  *}
{*                gr��e > 1                                                          *}
{* 31.03.2015 WW  TMeldung erweitert um Feld 'Bemerkung'                             *}
{* 06.10.2015 WW  Beschleunigte Rohdateizugriffe mit TFileOfCharStream               *}
{* 14.12.2017 WW  Konvertierung Veraut Veribox Kanalstati als Meldungen (CSV-Format);*}
{*                TMeldung erweitert um Feld 'IsStatusMeldung_Einwertig'             *}
{* 28.06.2018 WW  Anpassung Konvertierung Tritschler MC2 ohne eichpflichtiges Logbuch*}
{* 08.03.2019 WW  erweitert um Konvertierung f�r Modbus-Meldungen (Primus/Prilog)    *}
{* 28.09.2020 WW  mit Konvertierung der Archiv-Ordnungsnummer f�r Elster DL-/EK-     *}
{*                Serie (KonvMeldungenF), Tritschler VC3, VCC, TDS, MCO, MC2 (Konv-  *}
{*                MeldungenH), EC 900 (KonvMeldungenL); TMeldung erweitert um Feld   *}
{*                'OrdNr'                                                            *}
{* 18.02.2021 WW  Anpassung Konvertierung Modbus f�r TME400                          *}
{* 09.07.2021 WW  TMeldungsliste-Konvertierungsmethoden mit optionaler �bergabe der  *}
{*                Parameternummern-Konfigurationsliste                               *}
{* 06.08.2021 WW  Erweitert um optionale �bergabe der Konfigurationslisten mit       *}
{*                Meldungsnummern und meldungsspezifischen Parameternummern          *}
{* 29.09.2021 WW  erweitert um Konvertierung f�r SICK FLOWSIC500; TMeldung erweitert *}
{*                um Feld 'IsAdrMrg'                                                 *}
{* 11.02.2022 WW  �bergaberecord TMeldKonv erweitert um Parameter-Untergruppe und    *}
{*                MBAbrufData (f�r Modbuslisten-Variante Prilog 400)                 *}
{* 09.01.2024 WW  Anpassung Konvertierung Modbus f�r RSM200; Bugfix Meldungsliste    *}
{*                sortieren bei Meldungen mit gleichen Zeitstempeln                  *}
{*************************************************************************************}
Unit MObjMeld;

INTERFACE

Uses
  Windows, Forms, SysUtils, Classes, DateUtils, MObjList, WStrUtils, GD_Utils,
  T_Tools, T_Zeit, WSysCon, MResParam, WResMeld, WChars, COM_Utils, DecodeResp,
  WXmlConst, Novell, ErrConst, T_BinMask, MObjPara, UnixDT, WStream, ModbusUtil,
  MrgBefehl, MP_SICK, MResMrg;

Const
  szLen_MeldNrMrg = 4;     { erweitert von 3 auf 4 f�r EC694 }

  { k�nstliche Ger�te-Meldungsnummern f�r Meldungen des Parameter- und
    eichtechnischen Logbuchs: }
  CMeldNr_ParameterVeraendert     = 'P';
  CMeldNr_EichParameterVeraendert = 'EP';

  CMeldNr_ParameterVeraendert_CU  = 'P-CU';  // f�r EC 900

Type

  { �bergaberecord f�r MRG-Meldungskonvertierung }

  TMeldKonv = record
    MeldKonvGruppe: Integer;   { ger�teabh. Konvertierungsgruppe }
    MNrParaStart: string;      { Meldungsnummer f�r Meldung "Parameter ver�ndert" }
    MNrParameter: string;      { Meldungsnummer f�r Meldung "Parameter�nderung" mit altem/neuem Wert }
    MeldungsGruppe: Integer;   { ger�teabh. Meldungsgruppe }
    ParameterGruppe: Integer;  { ger�teabh. Parametergruppe }
    ParameterUnterGruppe: Integer;  { Variante der Parametergruppe; 11.02.2022, WW }
    MeldGeraeteart: string;    { Ger�teart, nach der Meldungen klassifiziert sind (MRG/DSfG); 04.06.2009 WW }
    RohLoeschen: boolean;      { Rohdaten nach Konvertierung l�schen ja/nein }
    MrgTyp: integer;           { Ger�tetyp (f�r Modbus-Meldungen); 08.03.2019 WW }
    ParameterListe: TParameterListe;  { obligatorisch f�r Ger�tetyp "FLOWSIC500" mit
                                        von UTC abweichender Zeitzone; 29.09.2021, WW }
    MBAbrufData: TMBAbrufData; { Konfigurationsdaten f�r Modbusregister-Konvertierung (f�r Modbus-Meldungen); 11.02.2022, WW }
  end;

  { Parameter�nderung-Objekt zur Aufnahme in TMeldung }

  TParaChange = class (TObject)
    Status : Word;                                { Status, siehe PS_XXXX Konstanten }
    NrMrg : string [szLen_ParaNrMrg];             { Parameternummer im MRG }
    NrAllg : string [szLen_ParaNrAllg];           { allgemeine Parameternummer }
    OldValue : string [szLen_ParaWert];           { alter Parameterwert }
    NewValue : string [szLen_ParaWert];           { neuer Parameterwert }
    Constructor Create (ANrMrg, ANrAllg, AOldValue, ANewValue: string);
    Constructor CreateFromRohString (S: string);
    procedure SetNrAllg (ANrAllg: string); virtual;
  End;

  { Meldungsobjekt zur Aufnahme in TMeldungsListe }

  TMeldung = class (TObject)
    Status : Word;                                { Status, siehe MS_XXXX Konstanten }
    NrMrg : string [szLen_MeldNrMRG];             { Meldenummer im MRG }
    Vz: string [1];                               { Meldenummer-Vorzeichen bei EC694 }
    NrAllg : string [szLen_MNrAllg];              { allgemeine Meldenummer }
    Jahr : Word;                                  { Datumsinformation }
    Monat : Word;                                 { " }
    Tag : Word;                                   { " }
    Stunde : Word;                                { Zeitinformation }
    Minute : Word;                                { " }
    Sekunde : Word;                               { " }
    ParaChange : TParaChange;                     { Zeiger auf Parameter�nderung }
    Bemerkung : string [szLen_Bemerkung];         { Bemerkung; 31.03.2015, WW }
    IsStatusMeldung_Einwertig : boolean;          { Flag f�r einwertige Status-Meldungen; 14.12.2017, WW }
    OrdNr : Longint;                              { Ordnungsnummer; 28.09.2020, WW }
    IsAdrMrg : boolean;                           { Flag f�r MRG-Meldungsadresse; 29.09.2021, WW }
    ListId: integer;                              { Index beim Eintragen in Meldungsliste; 09.01.2024, WW }
    constructor Create(sNrMrg, sNrAllg: string; dtDatumZeit: TDateTime;
      pParaChange: TParaChange; sBemerkung: string = ''); overload;
    Constructor Create (ANrMrg: string; AJahr, AMonat, ATag,
      AStunde, AMinute, ASekunde: Word; AParaChange: TParaChange); overload;
    Constructor CreateMrgNr4stellig (ANrMrg: string; AVz: string;
      AJahr, AMonat, ATag, AStunde, AMinute, ASekunde: Word;
      AParaChange: TParaChange; AIsStatusMeldung_Einwertig: boolean = false);
    Destructor Destroy; override;
    procedure SetNrAllg (ANrAllg: string);
    procedure SetNrMRG (ANrMrg: string);
    function GetParaChange: TParaChange;
    function IsEqual (ADateTime: TDateTime; AM5Nr: string; CompareNumber: boolean): boolean;
  End;

  { Liste von Meldungen }

  TMeldungsListe = class (TList)
  private
    procedure LoadFromListA (RTL: TRohTextListe; PSNr, PPNr: string);
    procedure LoadFromListB (RTL: TRohTextListe);
    procedure LoadFromListC (RTL: TRohTextListe);
    procedure LoadFromListD (RTL: TRohTextListe);
    procedure LoadFromListE (RTL: TRohTextListe);
    procedure LoadFromListF (slData: TStrings);
    procedure KonvMeldungenF (FileName: string; var DataValid: boolean);
    procedure KonvMeldungenG (FileName: string; var DataValid: boolean);
    procedure KonvMeldungenH (FileName: string; var DataValid: boolean;
      bEichLogb: boolean);
    procedure KonvMeldungenI (FileName: string; var DataValid: boolean);
    procedure KonvMeldungenJ (DBID_FileName: string; MeldKonv: TMeldKonv;
      AParamMrgKonfigList: TParamMrgKonfigList;
      AParamMeldKonfigList: TParamMeldKonfigList; var DataValid: boolean);
    procedure KonvMeldungenK (FileName: string; var DataValid: boolean);
    procedure KonvMeldungenL (FileName: string; var DataValid: boolean);
    procedure KonvMeldungenM (FileName: string; var DataValid: boolean);
    procedure KonvMeldungenN (FileName: string; var DataValid: boolean);
    procedure KonvMeldungenO (FileName: string; MeldKonv: TMeldKonv;
      AParamMrgKonfigList: TParamMrgKonfigList; var DataValid: boolean);
    procedure KonvMeldungenP (FileName: string; MeldKonv: TMeldKonv;
      AParamMrgKonfigList: TParamMrgKonfigList; var DataValid: boolean);
    function GetMeldungA (szRoh, PSNr: string): TMeldung;
    function GetMeldungB (szRoh: string): TMeldung;
    function GetMeldungC (szRoh: string): TMeldung;
    function GetMeldungD (szRoh: string): TMeldung;
    function GetMeldungE (szRoh: string): TMeldung;
    procedure SortByDatumZeit;
  protected
    KonfigPfad: string;
    FDataValid: boolean;           { false, wenn Rohdaten ung�ltige Meldungen enthalten }
    function KonvRohdatenFromFileList (FileNameList: TRohFileListe;
      MeldKonv: TMeldKonv; AParamMrgKonfigList: TParamMrgKonfigList = nil;
      AParamMeldKonfigList: TParamMeldKonfigList = nil): Boolean;
    function KonvRohdatenFromDataList (slData: TStrings;
      MeldKonv: TMeldKonv; bClearList: boolean): Boolean;
    procedure TransformNrAllgByResourcefile (MeldKonv: TMeldKonv;
      AParamMrgKonfigList: TParamMrgKonfigList = nil;
      AMeldNrKonfigList: TMeldNrKonfigList = nil);
    Procedure WriteExcelSheet (ExcelCaption: string;
      MeldTextKonfigList: TMeldTextKonfigList);
    function WriteFile_FTL_SR_NeuAlarm (AlarmFilename: string; Kennung: string;
      MeldTextKonfigList: TMeldTextKonfigList): integer;
  public
    constructor Create (AKonfigPfad: string);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer);
    function Add(AMeldung: TMeldung): Integer;
    function LoadFromFile (FileName: string; MeldKonv: TMeldKonv;
      AParamMrgKonfigList: TParamMrgKonfigList = nil;
      AParamMeldKonfigList: TParamMeldKonfigList = nil;
      AMeldNrKonfigList: TMeldNrKonfigList = nil): boolean;
    function LoadFromFileList (FileNameList: TRohFileListe; MeldKonv: TMeldKonv;
      bSortieren: boolean = false; AParamMrgKonfigList: TParamMrgKonfigList = nil;
      AParamMeldKonfigList: TParamMeldKonfigList = nil;
      AMeldNrKonfigList: TMeldNrKonfigList = nil): Boolean;
    function LoadFromXmlFile (sFileName: string; bDelete: boolean): boolean;
    function LoadFromAscFile (sFileName: string; MeldKonv: TMeldKonv;
      ConfigFromDB: boolean): boolean; virtual;
    procedure SaveToExcel (ExcelCaption: string);
    function SaveTo_FTL_SR_NeuAlarm (AlarmFilename: string; Kennung: string): integer;
    property DataValid: boolean read FDataValid;
  end;

IMPLEMENTATION

resourcestring
  S_DatumZeit = 'Datum/Zeit';
  S_Nummer    = 'Nummer';
  S_Meldung   = 'Meldung';
  S_kommt     = 'kommt';
  S_geht      = 'geht';

Const

  { Stati }

  MS_EMPTY = 0;   { keine Meldung enthalten }
  MS_MRG   = 1;   { nur MRG-spezifische Melddaten vorhanden }
  MS_OK    = 2;   { Meldung komplett vorhanden }

  PS_EMPTY = 0;   { keine Parameter�nderung vorhanden }
  PS_MRG   = 1;   { nur MRG-spezifische Parameternummer enthalten }
  PS_OK    = 2;   { Parameter�nderung vollst�ndig enthalten }


{----------------------------------------------------------------------------}
function Meldungsliste_DatumZeitIndexCompare (Item1, Item2: Pointer): integer;
{----------------------------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren der Meldungsliste nach dem Zeitstempel und
  Listenindex }
var
  MDatum1, MDatum2: DateRec;
  MZeit1, MZeit2: TimeRec;

begin
  with MDatum1 do begin
    Year:=TMeldung (Item1).Jahr;
    Month:=TMeldung (Item1).Monat;
    Day:=TMeldung (Item1).Tag;
  end;
  with MZeit1 do begin
    Hour:=TMeldung (Item1).Stunde;
    Min:=TMeldung (Item1).Minute;
    Sec:=TMeldung (Item1).Sekunde;
    HSec:=0;
  end;

  with MDatum2 do begin
    Year:=TMeldung (Item2).Jahr;
    Month:=TMeldung (Item2).Monat;
    Day:=TMeldung (Item2).Tag;
  end;
  with MZeit2 do begin
    Hour:=TMeldung (Item2).Stunde;
    Min:=TMeldung (Item2).Minute;
    Sec:=TMeldung (Item2).Sekunde;
    HSec:=0;
  end;
  // 1. Sortierkriterium: Datum, Zeit
  Result:=CmpDate (MDatum1, MDatum2) + CmpTime (MZeit1, MZeit2);

  // Bei gleichen Zeitstempeln nach Listenindex sortieren, um die Reihenfolge
  // beizubehalten (der QuickSort-Algorithmus w�rde sonst diese Listenelemente
  // umgekehrt sortieren !); 09.01.2024, WW
  // 2. Sortierkriterium: Listenindex
  if Result = 0 then
    Result:=TMeldung (Item1).ListId - TMeldung (Item2).ListId;
end;


{ TParaChange }

{-----------------------------------------------------------------------------}
Constructor TParaChange.Create (ANrMrg, ANrAllg, AOldValue, ANewValue: string);
{-----------------------------------------------------------------------------}
Begin
  inherited Create;
  Status := PS_EMPTY;
  NrMrg:=ANrMrg;
  if length (NrMrg) > 0 then
    Status := PS_MRG;
  NrAllg:=ANrAllg;
  if length (NrAllg) > 0 then
    Status := PS_OK;

  OldValue:=AOldValue;
  NewValue:=ANewValue;
End;

{ CreateFromString : Aus extrahiertem Rohdatenstring S werden die MRG-spezifischen
                     Daten der Parameter�nderung eingelesen }
{------------------------------------------------------}
constructor TParaChange.CreateFromRohString (S: string);
{------------------------------------------------------}
var
  i : integer;
begin
  inherited Create;
  Status := PS_EMPTY;
  NrMrg:='';
  OldValue:='';
  NewValue:='';
  if length (S) > 3 then begin
    NrMrg:=copy(S, 1, 3);
    S := Copy(S, 4, length(S));
    i := 1;
    while (i <= length (S)) and (S [i] <> '_') do begin
      OldValue:=OldValue + S [i];
      inc (i);
    end;
    if length(S) >= i then begin { L�ngenpr�fung Sm 22.9.99}
      if S [i] = '_' then begin
        S := Copy(S, i+1, length(S));
        i := 1;
        while (i <= length(S)) and (S [i] <> 'X') do begin
          NewValue:=NewValue + S [i];
          inc (i);
        end;
      end;
    end
    else begin
      OldValue:='';
      NewValue:='';
    end;
      ;
  end;
  if Length (NrMrg) = 3 then
    Status := PS_MRG;
end;

{ SetNrAllg : allgemeine Parameternummer wird gesetzt }
{------------------------------------------------}
procedure TParaChange.SetNrAllg (ANrAllg: string);
{------------------------------------------------}
begin
  if Status = PS_MRG then begin
    NrAllg:=Copy(ANrAllg, 1, szLen_ParaNrAllg);
    if Length (NrAllg) = szLen_ParaNrAllg then
      Status := PS_OK;
  end;
end;


{ TMeldung }

{ -------------------------------------------------------------------------}
constructor TMeldung.Create(sNrMrg, sNrAllg: string; dtDatumZeit: TDateTime;
  pParaChange: TParaChange; sBemerkung: string = '');
{--------------------------------------------------------------------------}
var
  ms : word;
begin
  inherited Create;

  Status := MS_MRG;
  if (Trim(sNrMrg) <> '') and (not (Trim(sNrMrg)[1] in ['1'..'9', '0'])) then
  begin
    Vz := Trim(sNrMrg)[1];
    NrMrg := Copy(Trim(sNrMrg), 2, Length(Trim(sNrMrg))-1);
  end
  else begin
    Vz:='';
    NrMrg := Trim(sNrMrg);
  end;
  NrAllg := Trim(sNrAllg);
  DecodeDate(dtDatumZeit, Jahr, Monat, Tag);
  DecodeTime(dtDatumZeit, Stunde, Minute, Sekunde, ms);
  ParaChange := pParaChange;
  Bemerkung := sBemerkung;  // 31.03.2015, WW
  IsStatusMeldung_Einwertig := false;  // 14.12.2017, WW
  OrdNr := -1;  // Default-Ordnungsnummer; 28.09.2020, WW
  IsAdrMrg := false;  // 29.09.2021, WW
  ListId := -1;  // Default-Listenindex; 09.01.2024, WW
end;

{------------------------------------------------------------------------}
Constructor TMeldung.Create (ANrMrg: string; AJahr, AMonat, ATag, AStunde,
  AMinute, ASekunde: Word; AParaChange: TParaChange);
{------------------------------------------------------------------------}
Begin
  inherited Create;
  Status := MS_EMPTY;
  NrMrg:=Copy(ANrMrg, 1, 3);
  Vz:='';
  NrAllg:='';
  Jahr := AJahr;
  Monat := AMonat;
  Tag := ATag;
  Stunde := AStunde;
  Minute := AMinute;
  Sekunde := ASekunde;
  ParaChange := AParaChange;
  Bemerkung := '';
  IsStatusMeldung_Einwertig := false;  // 14.12.2017, WW
  OrdNr := -1;  // Default-Ordnungsnummer; 28.09.2020, WW
  IsAdrMrg := false;  // 29.09.2021, WW
  ListId := -1;  // Default-Listenindex; 09.01.2024, WW
  if Length (NrMrg) = 3 then
    Status := MS_MRG;
End;

{-----------------------------------------------------------------------}
Constructor TMeldung.CreateMrgNr4stellig (ANrMrg: string; AVz: string;
  AJahr, AMonat, ATag, AStunde, AMinute, ASekunde: Word;
  AParaChange: TParaChange; AIsStatusMeldung_Einwertig: boolean = false);
{-----------------------------------------------------------------------}
Begin
  inherited Create;
  Status := MS_EMPTY;
  NrMrg:=Copy(ANrMrg, 1, szLen_MeldNrMrg);
  Vz:=AVz;
  NrAllg:='';
  Jahr := AJahr;
  Monat := AMonat;
  Tag := ATag;
  Stunde := AStunde;
  Minute := AMinute;
  Sekunde := ASekunde;
  ParaChange := AParaChange;
  Bemerkung := '';
  IsStatusMeldung_Einwertig := AIsStatusMeldung_Einwertig;  // 14.12.2017, WW
  OrdNr := -1;  // Default-Ordnungsnummer; 28.09.2020, WW
  IsAdrMrg := false;  // 29.09.2021, WW
  ListId := -1;  // Default-Listenindex; 09.01.2024, WW
  if Length (NrMrg) <= szLen_MeldNrMrg then
    Status := MS_MRG;
End;

{--------------------------}
Destructor TMeldung.Destroy;
{--------------------------}
Begin
  ParaChange.Free;
  Inherited Destroy;
End;

{---------------------------------------------}
procedure TMeldung.SetNrAllg (ANrAllg: string);
{---------------------------------------------}
begin
  if Status = MS_MRG then begin
    NrAllg:=Copy(ANrAllg, 1, szLen_MNrAllg);
    if Length (NrAllg) = szLen_MNrAllg then
      Status := MS_OK;
  end;
end;

{-------------------------------------------}
procedure TMeldung.SetNrMRG (ANrMrg: string);  // 29.09.2021, WW
{-------------------------------------------}
begin
  Vz:='';
  NrMrg:='';
  if length (ANrMrg) > 0 then begin
    if (ANrMrg [1] = '+') OR (ANrMrg [1] = '-') then begin  // mit Vorzeichen
      Vz:=ANrMrg [1];
      NrMrg:=Copy (ANrMrg, 2, szLen_MeldNrMrg);
    end else
      NrMrg:=Copy (ANrMrg, 1, szLen_MeldNrMrg);
  end;
end;

{-------------------------------------------}
function TMeldung.GetParaChange: TParaChange;
{-------------------------------------------}
begin
  Result := ParaChange;
end;

{ IsEqual: Vergleich mit �bergebenen Daten
  Parameter:
    Datum/Zeit, allg. Meldungsnummer: Vergleichsdaten
    Schalter "mit/ohne Vergleich der Meldungsnummer"
  R�ckgabe:
    True, wenn Meldungsnummer, Datum/Zeit �bereinstimmen }
{-------------------------------------------------------------}
function TMeldung.IsEqual (ADateTime: TDateTime; AM5Nr: string;
                           CompareNumber: boolean): boolean;
{-------------------------------------------------------------}
var
  MDatum, ADatum: DateRec;
  MZeit, AZeit: TimeRec;
  dummy: word;

begin
  Result := false;
  if Status <> MS_EMPTY then begin
    with MDatum do begin
      Year:=Jahr;
      Month:=Monat;
      Day:=Tag;
    end;
    with MZeit do begin
      Hour:=Stunde;
      Min:=Minute;
      Sec:=Sekunde;
      HSec:=0;
    end;
    DecodeDate (ADateTime, word (ADatum.Year), word (ADatum.Month), word (ADatum.Day));
    DecodeTime (ADateTime, word (AZeit.Hour), word (AZeit.Min), word (AZeit.Sec), dummy);
    AZeit.HSec:=0;

    if CompareNumber then
      Result := ((CmpDate (MDatum, ADatum) + CmpTime (MZeit, AZeit)) = 0) AND (NrAllg = AM5Nr)
    else
      Result := (CmpDate (MDatum, ADatum) + CmpTime (MZeit, AZeit)) = 0;
  end;
end;


{ TMeldungsListe }

{------------------------------------------------------}
Constructor TMeldungsListe.Create (AKonfigPfad: string);
{------------------------------------------------------}
Begin
  Inherited Create;
  KonfigPfad:=AKonfigPfad;

  FDataValid:=true;                   { Vorbelegung: Meldungs-Rohdaten g�ltig }
End;

{--------------------------------}
Destructor TMeldungsListe.Destroy;
{--------------------------------}
Begin
  Clear;

  inherited;
end;

{-----------------------------}
procedure TMeldungsListe.Clear;
{-----------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    TMeldung (Items [i]).Free;

  inherited;
end;

{-----------------------------------------------}
procedure TMeldungsListe.Delete(iIndex: integer);
{-----------------------------------------------}
begin
  if (Assigned(Items[iIndex])) then TMeldung (Items [iIndex]).Free;

  inherited;
end;

{-------------------------------------------------------}
function TMeldungsListe.Add(AMeldung: TMeldung): Integer;
{-------------------------------------------------------}
{ Meldung am Ende der Liste hinzuf�gen;
  -> Der Meldung wird der Listenindex zugewiesen }
begin
  Result:=inherited Add (AMeldung);
  if Assigned (AMeldung) then
    AMeldung.ListId:=Result;
end;

{--------------------------------------------------------------------------}
function TMeldungsListe.LoadFromFile (FileName: string; MeldKonv: TMeldKonv;
  AParamMrgKonfigList: TParamMrgKonfigList = nil;
  AParamMeldKonfigList: TParamMeldKonfigList = nil;
  AMeldNrKonfigList: TMeldNrKonfigList = nil): boolean;
{--------------------------------------------------------------------------}
{ Konvertieren der Meldungen aus Rohdatenfile in Meldungsliste und Ermitteln der
  allgemeinen Meldungsnummern �ber Resourcendatei;
  �bergabe: Rohfilename
            Record mit Angaben f�r MRG-Meldungskonvertierung
            Liste mit Parameternummern-Konfiguration (optional)
            Liste mit Konfiguration der meldungsspezifischen Parameternummern (optional)
            Liste mit Meldungsnummern-Konfiguration (optional)
  Ergebnis: true, wenn Laden der Meldungen OK }
var
  RFL: TRohFileListe;

begin
  // 09.07.2021, WW: mit �bergabe der Parameternummern-Konfigurationsliste
  // 06.08.2021, WW: mit �bergabe der Konfigurationslisten mit Meldungsnummern
  //                 und meldungsspezifischen Parameternummern
  RFL:=TRohFileListe.Create;
  try
    RFL.Add (FileName);
    Result:=KonvRohdatenFromFileList (RFL, MeldKonv, AParamMrgKonfigList,
                                      AParamMeldKonfigList);
  finally
    RFL.Free;
  end;
  TransformNrAllgByResourcefile (MeldKonv, AParamMrgKonfigList, AMeldNrKonfigList);
end;

{--------------------------------------------------------------------}
Function TMeldungsListe.LoadFromFileList (FileNameList: TRohFileListe;
  MeldKonv: TMeldKonv; bSortieren: boolean = false;
  AParamMrgKonfigList: TParamMrgKonfigList = nil;
  AParamMeldKonfigList: TParamMeldKonfigList = nil;
  AMeldNrKonfigList: TMeldNrKonfigList = nil): Boolean;
{--------------------------------------------------------------------}
{ Konvertieren der Meldungen aus Liste mit Rohdatenfiles und Ermitteln der
  allgemeinen Meldungsnummern �ber Resourcendatei;
  �bergabe: Liste mit Meldungen-Rohfilenamen
            Record mit Angaben f�r MRG-Meldungskonvertierung
            Flag zum Sortieren der Meldungsliste nach Zeitstempel (optional)
            Liste mit Parameternummern-Konfiguration (optional)
            Liste mit Konfiguration der meldungsspzifischen Parameternummern (optional)
            Liste mit Meldungsnummern-Konfiguration (optional)
  Ergebnis: true, wenn Laden der Meldungen OK }
Begin
  // 09.07.2021, WW: mit �bergabe der Parameternummern-Konfigurationsliste
  // 06.08.2021, WW: mit �bergabe der Konfigurationslisten mit Meldungsnummern
  //                 und meldungsspezifischen Parameternummern
  Result:=KonvRohdatenFromFileList (FileNameList, MeldKonv, AParamMrgKonfigList,
                                    AParamMeldKonfigList);
  TransformNrAllgByResourcefile (MeldKonv, AParamMrgKonfigList, AMeldNrKonfigList);

  if bSortieren then
    SortByDatumZeit;  // 23.05.2014, WW  
End;

{----------------------------------------------------------------------------}
function TMeldungsListe.KonvRohdatenFromFileList (FileNameList: TRohFileListe;
  MeldKonv: TMeldKonv; AParamMrgKonfigList: TParamMrgKonfigList = nil;
  AParamMeldKonfigList: TParamMeldKonfigList = nil): Boolean;
{----------------------------------------------------------------------------}
{ Konvertieren der Meldungen aus Liste mit Rohdatenfiles in Meldungsliste;
  �bergabe: Liste mit Meldungen-Rohfilenamen
            Record mit Angaben f�r MRG-Meldungskonvertierung
            Liste mit Parameternummern-Konfiguration (optional)
            Liste mit Konfiguration der meldungsspezifischen Parameternummern (optional)
  Ergebnis: true, wenn Laden der Meldungen OK }
var
  RohTextListe : TRohTextListe;
  i: integer;
  FileName: string;

begin
  Result:=true;
  Clear;         { Liste leeren }
  FDataValid:=true;                   { Vorbelegung: Meldungs-Rohdaten g�ltig }

  for i:=0 to FileNameList.Count - 1 do begin
    FileName:=FileNameList [i];
    if length (FileName) > 0 then begin                      
      case MeldKonv.MeldKonvGruppe of
        6: KonvMeldungenF (FileName, FDataValid);   { Konvertierung Elster DL240, EK260, EK280 }
        7: KonvMeldungenG (FileName, FDataValid);   { Konvertierung Elster DS-100 }
        8: KonvMeldungenH (FileName, FDataValid, true);   { Konvertierung Tritschler TDS, VC3, VCC }
        9: KonvMeldungenI (FileName, FDataValid);   { Konvertierung Tritschler SR }
       10: KonvMeldungenJ (FileName, MeldKonv, AParamMrgKonfigList,
                           AParamMeldKonfigList, FDataValid);   { Konvertierung Actaris Corus }
       11: KonvMeldungenK (FileName, FDataValid);   { Konvertierung IEC1107 }
       12: KonvMeldungenL (FileName, FDataValid);   { Konvertierung EC 900 }
       13: KonvMeldungenM (FileName, FDataValid);   { Konvertierung VC2 }
       14: KonvMeldungenN (FileName, FDataValid);   { Konvertierung Kamstrup UNIGAS 300 }
       16: KonvMeldungenH (FileName, FDataValid, false);   { Konvertierung Tritschler MC2 }
       17: KonvMeldungenO (FileName, MeldKonv, AParamMrgKonfigList,
                           FDataValid);   { Konvertierung Modbus (Primus/Prilog, TME400, RSM200) }
       18: KonvMeldungenP (FileName, MeldKonv, AParamMrgKonfigList,
                           FDataValid);   { Konvertierung SICK (FLOWSIC500) }
      else    { Konvertierung �ber RohTextListe (Standardkonvertierungen f�r Wieser-Ger�te) }
        RohTextListe := TRohTextListe.LoadFromMeldRohFile (FileName);
        try
          if RohTextListe.Count > 0 then begin
            case MeldKonv.MeldKonvGruppe of
              1: LoadFromListA (RohTextListe, MeldKonv.MNrParaStart, MeldKonv.MNrParameter);
              2: LoadFromListB (RohTextListe);
              3: LoadFromListC (RohTextListe);
              4: LoadFromListD (RohTextListe);
              5: LoadFromListE (RohTextListe);
            else
              Result:=false;
            end;  { case }
          end;
        finally
          RohTextListe.Free;
        end;
      end;  { case }
    end else
      Result:=false;

    if MeldKonv.RohLoeschen then begin
      { Aus dem Rohfilenamen evtl. vorangestellte Kanalnummer/Kennzeichen incl.
        Strichpunkt-Trenner l�schen: 08.09.2011, WW }
      if Pos (';', Filename) > 0 then
        F_Zerlegen (Filename, ';');
      DeleteFile (FileName);
    end;
  end;  { for i }
end;

{----------------------------------------------------------------------------}
function TMeldungsListe.KonvRohdatenFromDataList (slData: TStrings;
  MeldKonv: TMeldKonv; bClearList: boolean): Boolean;
{----------------------------------------------------------------------------}
{ Konvertieren der Meldungen aus Rohdaten-Liste in Meldungsliste; 14.12.2017, WW
  �bergabe: Liste mit Meldungen-Rohdaten
            Record mit Angaben f�r MRG-Meldungskonvertierung
            Schalter zum L�schen der Meldungsliste vor der Konvertierung
  Ergebnis: true, wenn Laden der Meldungen OK }
begin
  Result:=true;
  if bClearList then
    Clear;         { Liste leeren }
  FDataValid:=true;                   { Vorbelegung: Meldungs-Rohdaten g�ltig }

  if slData.Count > 0 then begin
    case MeldKonv.MeldKonvGruppe of
      15: LoadFromListF (slData);  // Veraut Veribox-Mini Kanalstati (CSV-Format)
    else
      Result:=false;
    end;  { case }
  end;
end;

{---------------------------------------------------------------------------}
procedure TMeldungsListe.TransformNrAllgByResourcefile (MeldKonv: TMeldKonv;
  AParamMrgKonfigList: TParamMrgKonfigList = nil;
  AMeldNrKonfigList: TMeldNrKonfigList = nil);
{---------------------------------------------------------------------------}
{ Ermitteln und Zuweisen der allgemeinen Meldungs- und Parameternummern f�r jeden
  Eintrag in der Liste (aus Resourcendateien MeldNr.dat, ParamMrg.dat;
  �bergabe: Record mit Angaben f�r MRG-Meldungskonvertierung
            Liste mit Parameternummern-Konfiguration (optional: Wenn nil, wird die
              Konfiguration aus der Ressourcendatei gelesen)
            Liste mit Meldungsnummern-Konfiguration (optional: Wenn nil, wird die
              Konfiguration aus der Ressourcendatei gelesen) }
var
  ParamMrgKonfigList: TParamMrgKonfigList;
  MeldNrKonfigList: TMeldNrKonfigList;
  MNrGeraet: string [szLen_MNrGeraet];
  MeldNrData: TMeldNrData;
  i: integer;
  Meldung: TMeldung;
  ParaNr_MRG: string [szLen_ParaNrMrg];
  ParamMrgData: TParamMrgData;

begin
  if not Assigned (AParamMrgKonfigList) then
    ParamMrgKonfigList:=TParamMrgKonfigList.Create  // Parameternummern-Konfigurationsliste lokal anlegen
  else
    ParamMrgKonfigList:=AParamMrgKonfigList;  // �bergebene Parameternummern-Konfigurationsliste wird verwendet; 09.07.2021, WW
  try
    { Parameternummern-Konfigurationsliste aus Resourcendatei laden, wenn lokal angelegt: }
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      GetParamMrg_KonfigList_ByParaGruppe (MeldKonv.ParameterGruppe,
                                           MeldKonv.ParameterUnterGruppe,  // 11.02.2022, WW
                                           ParamMrgKonfigList, KonfigPfad);

    if not Assigned (AMeldNrKonfigList) then
      MeldNrKonfigList:=TMeldNrKonfigList.Create  // Meldungsnummern-Konfigurationsliste lokal anlegen
    else
      MeldNrKonfigList:=AMeldNrKonfigList;  // �bergebene Meldungsnummern-Konfigurationsliste wird verwendet; 06.08.2021, WW
    try
      { Meldungsnummern-Konfigurationsliste aus Resourcendatei laden, wenn lokal angelegt: }
      if not Assigned (AMeldNrKonfigList) then  // 06.08.2021, WW
        GetMeldNr_KonfigList (MeldKonv.MeldGeraeteart, MeldKonv.Meldungsgruppe,
                              MeldNrKonfigList, KonfigPfad);

      for i:=0 to Count-1 do begin
        Application.ProcessMessages;

        Meldung:=TMeldung (Items [i]);
        MNrGeraet:=Meldung.Vz + Meldung.NrMrg;
        { allgemeine Meldungsnummer ermitteln:
          -> 29.09.2021, WW: �ber ger�tespezifische Meldungsnummer oder Meldungsadresse }
        if MeldNrKonfigList.FindMeldNrData (MeldKonv.MeldGeraeteart, MeldKonv.Meldungsgruppe,
                                            MNrGeraet, MeldNrData,
                                            Meldung.IsAdrMrg) then begin
          Meldung.SetNrAllg (MeldNrData.MNrAllg);
          if Meldung.IsAdrMrg then
            Meldung.SetNrMRG (MeldNrData.MNrGeraet);  // 29.09.2021, WW
        end;

        { bei Parameter�nderungen: allgemeine Parameternummer ermitteln }
        if Meldung.GetParaChange <> nil then begin
          ParaNr_MRG:=Meldung.GetParaChange.NrMrg;
          if ParamMrgKonfigList.FindParamMrgData (MeldKonv.Parametergruppe, ParaNr_MRG,
                                                  ParamMrgData) then
            Meldung.GetParaChange.SetNrAllg (ParamMrgData.Parameternummer);
        end;
      end;
    finally
      // Meldungsnummern-Konfigurationsliste nur freigeben, wenn lokal angelegt
      if not Assigned (AMeldNrKonfigList) then  // 06.08.2021, WW
        MeldNrKonfigList.Free;
    end;
  finally
    // Parameternummern-Konfigurationsliste nur freigeben, wenn lokal angelegt
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      ParamMrgKonfigList.Free;
  end;
end;

{ LoadFromListA : Einlesen von Rohmeldungen der Gruppe A
  Parameter:
    RTL : Liste mit Rohmeldungen
    PSNr : Meldenummer, welche den Beginn einer Parameter�nderung kennzeichnet
    PPNr : Meldenummer, welche eine Parameter�nderung kennzeichnet }
{------------------------------------------------------------------------------}
Procedure TMeldungsListe.LoadFromListA (RTL: TRohTextListe; PSNr, PPNr: string);
{------------------------------------------------------------------------------}
Const
  szLen_Meldung = 15;
  szLen_MeldungText = 200;

Var
  i : Integer;
  szRoh : string;
  szMNr : string [3];
  Meldung : TMeldung;
  TextListe : TTextListe;
  InParaChange : Boolean;
  szMeldung : string [szLen_Meldungtext];

begin
  if RTL.Count > 0 then begin

    { Die Meldetexte aus RTL werden nach Textliste kopiert, wobei Meldungen, welche eine
      Parameter�nderung kennzeichnen zu einem Asciistring zusammengefasst werden }

    TextListe := TTextListe.Create;
    try
      InParaChange := False;
      for i := 0 to RTL.Count - 1 do begin
        Application.ProcessMessages;

        szRoh := RTL [i];
        if Length (szRoh) >= 3 then begin
          szMNr:=Copy(szRoh, 1, 3);
          if szMNr = PSNr then begin
            if InParaChange then
              TextListe.Add (szMeldung);
            InParaChange := True;
            szMeldung:=szRoh;
          end
          else if szMNr = PPNr then begin
            if InParaChange then
              szMeldung:=szMeldung + Copy(szRoh,4, length(szRoh));
          end
          else begin
            if InParaChange then begin
              TextListe.Add (szMeldung);
              InParaChange := False;
            end;
            TextListe.Add (szRoh);
          end;
        end;
      end; { for }
      if InParaChange then
        TextListe.Add (szMeldung);

      { Aus den Meldungsrohtexten in Textliste werden Meldeobjekte erzeugt, und in Liste
        abgelegt }

      FDataValid:=true;
      for i := 0 to TextListe.Count  - 1 do begin
        Application.ProcessMessages;

        szRoh := TextListe [i];
        if Length (szRoh) >= szLen_Meldung Then begin
          Meldung := GetMeldungA (szRoh, PSNr);
          if Meldung <> Nil then
            Insert (0, Meldung)
          else
            FDataValid:=false;
        end else
          FDataValid:=false;
      end;
    finally
      TextListe.Free;
    end;
  end;
end;

{ LoadFromListB : Einlesen von Rohmeldungen der Gruppe B
  Parameter:
    RTL : Liste mit Rohmeldungen }
{----------------------------------------------------------}
procedure TMeldungsListe.LoadFromListB (RTL: TRohTextListe);
{----------------------------------------------------------}
const
  szLen_Meldung = 15;
var
  i : Integer;
  szRoh : string;
  Meldung : TMeldung;

begin
  FDataValid:=true;
  for i := 0 to RTL.Count - 1 do begin
    Application.ProcessMessages;

    szRoh := RTL [i];
    if Length (szRoh) >= szLen_Meldung then begin
      Meldung := GetMeldungB (szRoh);
      if Meldung <> Nil then
        Add (Meldung)
      else
        FDataValid:=false;
    end else
      FDataValid:=false;
  end;
end;

{ LoadFromListC : Einlesen von Rohmeldungen der Gruppe C
  Parameter:
    RTL : Liste mit Rohmeldungen }
{----------------------------------------------------------}
procedure TMeldungsListe.LoadFromListC (RTL: TRohTextListe);
{----------------------------------------------------------}
const
  szLen_Meldung = 14;
var
  i : Integer;
  szRoh : string;
  Meldung : TMeldung;

begin
  FDataValid:=true;
  for i := 0 to RTL.Count - 1 do begin
    Application.ProcessMessages;

    szRoh := RTL [i];
    { Rohsatzl�nge = 13 enth�lt Von-Bis Datum und mu� ignoriert werden,
      FDataValid ist dann True }
    if Length (szRoh) >= szLen_Meldung then begin
      Meldung := GetMeldungC (szRoh);
      if Meldung <> Nil then
        Add (Meldung)
      else
        FDataValid:=false;
    end;
  end;
end;

{ LoadFromListD : Einlesen von Rohmeldungen der Gruppe D
  Parameter:
    RTL : Liste mit Rohmeldungen }
{----------------------------------------------------------}
procedure TMeldungsListe.LoadFromListD (RTL: TRohTextListe);
{----------------------------------------------------------}
const
  szLen_Meldung = 8;
var
  i : Integer;
  szRoh : string;
  Meldung : TMeldung;

begin
  FDataValid:=true;
  for i := 0 to RTL.Count - 1 do begin
    Application.ProcessMessages;

     szRoh := RTL [i];
    if Length (szRoh) >= szLen_Meldung then begin
      Meldung := GetMeldungD (szRoh);
      if Meldung <> Nil then
        Add (Meldung)
      else
        FDataValid:=false;
    end else
      FDataValid:=false;
  end;
end;

{ LoadFromListE : Einlesen von Rohmeldungen der Gruppe E
  Parameter:
    RTL : Liste mit Rohmeldungen }
{----------------------------------------------------------}
procedure TMeldungsListe.LoadFromListE (RTL: TRohTextListe);
{----------------------------------------------------------}
const
  szLen_Meldung = 16;
var
  i : Integer;
  szRoh : string;
  Meldung : TMeldung;

begin
  FDataValid:=true;
  for i := 0 to RTL.Count - 1 do begin
    Application.ProcessMessages;

    szRoh := RTL [i];
    if Length (szRoh) >= szLen_Meldung then begin
      Meldung := GetMeldungE (szRoh);
      if Meldung <> Nil then
        Add (Meldung)
      else
        FDataValid:=false;
    end else
      FDataValid:=false;
  end;
end;

{--------------------------------------------------------}
procedure TMeldungsListe.LoadFromListF (slData: TStrings);
{--------------------------------------------------------}
{ Einlesen von Rohmeldungen der Gruppe F (Veraut Veribox-Mini Kanalstati,
  CSV-Format); 14.12.2017, WW
  �bergabe: Stringliste mit Kanalstati (Format: yyyymmddhhnnss;Kanalnummer;Kanalstatus) }

  {------------------------------------------------------------------------}
  function BuildMrgMeldNr_Veribox (ABitNr: integer; AKanalNr: string;
    AGeht: boolean): string;
  {------------------------------------------------------------------------}
  { k�nstliche MRG-Meldungsnummer aus Bitnummer des Kanalstatus,
    Kanalnummer und Kommt/Geht-Info bilden }
  begin
    Result:=Format('%.2d', [ABitNr + 1]);  { Bitnummer (0-basiert) + 1 }
    if AKanalNr <> '' then
      Result:=Result + AKanalNr;
    if AGeht then
      Result:=Result + '-';  { Minus f�r Geht-Meldung }
  end;

type
  { Statusmeldungs-Typen }
  TStatusMeldungTyp = (
    stm_Kanal,   // Die Meldung ist Kanal-spezifisch
    stm_Global,  // Die Meldung ist global (nicht Kanal-spezifisch)
    stm_Undef    // Die Meldung kann globakl oder Kanal-spezifisch sein
  );

var
  k, m: integer;
  iKanalstatus: integer;
  iMaske: integer;
  sMNr: string;
  bNull: boolean;
  Meldung: TMeldung;
  sDatumZeit: string;
  sKanalNr: string;
  sKanalStatus: string;
  StatusMeldTyp: TStatusMeldungTyp;
  dtDatumZeit: TDateTime;
  bAlleGleich: boolean;
  iWert: integer;
  iWert_Merk: integer;
  bOK: boolean;
  S: string;
  dtBuf: TDateTime;
  year, month, day: word;
  hour, min, sec, msec: word;
  bEinwertig: boolean;

begin
  FDataValid:=true;
  try
    // Die ersten 16 Bits auswerten. Achtung: Durch Erh�hen werden weitere (bislang
    // unn�tige (Geht-)Meldungen erzeugt !
    iMaske:=1;
    for m:=0 to 15 do begin
      Application.ProcessMessages;

      case iMaske of  // Kanalstatus-Wert dezimal
        1, 512:
          StatusMeldTyp:=stm_Kanal;

        2, 16, 256, 2048:
          StatusMeldTyp:=stm_Global
      else
        StatusMeldTyp:=stm_Undef;  // bislang bekannt: 8 = Parameter�nderung
      end;  // case iMaske

      if StatusMeldTyp = stm_Undef then begin
        // Pr�fen, ob in allen Kanalstati die Statusmeldung enthalten
        // ist -> Globale Meldung, wenn nicht: Kanal-spezifische Meldung
        bAlleGleich:=true;
        iWert_Merk:=-1;
        for k:=0 to slData.Count - 1 do begin
          S:=slData [k];
          sKanalStatus:=GetStringPart (S, 3, ';');
          iKanalstatus:=StrToInt (sKanalStatus);

          iWert:=(iKanalstatus AND iMaske);

          if (iWert_Merk <> -1) then begin
            if iWert <> iWert_Merk then begin
              bAlleGleich:=false;
              Break;
            end;
          end;

          iWert_Merk:=iWert;
        end;  { for k }

        if bAlleGleich then
          StatusMeldTyp:=stm_Global
        else
          StatusMeldTyp:=stm_Kanal;
      end;

      if (StatusMeldTyp = stm_Kanal) OR
         (StatusMeldTyp = stm_Global) then begin
        for k:=0 to slData.Count - 1 do begin
          S:=slData [k];
          sDatumZeit:=GetStringPart (S, 1, ';');
          sKanalNr:=GetStringPart (S, 2, ';');
          sKanalStatus:=GetStringPart (S, 3, ';');
          iKanalstatus:=StrToInt (sKanalStatus);

          // Einwertige Zust�nde sind in den ersten 8 Bits codiert:
          bEinwertig:=m <= 7;
          // Bit nicht gesetzt = 'Meldung geht' (bei zweiwertigen Zust�nden) bzw.
          //                     'keine Meldung' (bei einwertigen Zust�nden) }
          // -> F�r einwertige Zust�nde nur Meldung erzeugen, wenn der Wert nicht 0 ist.
          bNull:=(iKanalstatus AND iMaske) = 0;

          if (bEinwertig AND not bNull) OR (not bEinwertig) then begin
            { k�nstliche ger�tespez. Meldungsnummer: }
            if StatusMeldTyp = stm_Kanal then  // Kanal-spezifisch
              sMNr:=BuildMrgMeldNr_Veribox (m, sKanalNr, bNull)
            else  // global
              sMNr:=BuildMrgMeldNr_Veribox (m, '', bNull);

            { Zeitstempel }
            bOK:=false;
            if EncodeDateStr (Copy (sDatumZeit, 1, 8), 'YYYYMMDD', dtBuf) then begin
              dtDatumZeit:=dtBuf;
              if EncodeTimeStr (Copy (S, 9, 6), 'HHMMSS', dtBuf) then begin
                dtDatumZeit:=dtDatumZeit + dtBuf;
                bOK:=true;

                DecodeDateTime (dtDatumZeit, year, month, day, hour, min, sec, msec);
                Meldung:=TMeldung.CreateMrgNr4stellig (
                  sMNr, '', year, month, day, hour, min, sec, nil, bEinwertig);
                Add (Meldung);
              end;
            end;

            if not bOK then
              FDataValid:=false;    
          end;

          if StatusMeldTyp = stm_Global then
            Break;  // Globale Meldung nur 1 Mal
        end;  { for k }
      end;

      iMaske:=iMaske SHL 1;
    end;  // for m
  except
  end;
end;

{ Konvertieren von Rohmeldungen der Gruppe F (Elster DL240, EK260, EK280)
  -> Es werden die Meldungen des Logbuchs, �nderungs-Archivs oder eichtechnischen
     Logbuchs konvertiert.
  -> Filename mit Format: [<Meldungsarchiv-Kennzeichen>;]<Rohdateiname>
     Meldungsarchiv-Kennzeichen: ohne = Logbuch
                                    P = �nderungs-Archiv
                                   PE = Eichtechnisches Logbuch
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{---------------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenF (FileName: string; var DataValid: boolean);
{---------------------------------------------------------------------------------}
var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  Fehlertelegramm: boolean;
  rohsatz: string;
  zeichen: char;
  dummy: char;
  i: integer;
  satz_ok: boolean;
  wert_ok: boolean;
  sbuf: string;
  len: integer;
  szTemp: string [10];
  szMNr : string [6];
  Jahr, Monat, Tag, Stunde, Minute, Sekunde: Word;
  Code : Integer;
  Vz: string [1];
  Meldung: TMeldung;
  sMeldArchiv: string;
  sParaNrMrg: string;
  sOldValue: string;
  sNewValue: string;
  ParaChange: TParaChange;
  iPos: integer;
  iOrdNr: longint;
  iBuf: longint;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }

  { Wenn dem Rohfilenamen durch Strichpunkt getrennt ein Meldungsarchiv-Kennzeichen
    vorangestellt ist: Meldungsarchiv, zu dem die Meldungen geh�ren; 24.01.2014, WW }
  if Pos (';', Filename) > 0 then begin
    sMeldArchiv:=F_Zerlegen (Filename, ';');
  end else
    sMeldArchiv:='';

  if not FileExists (FileName) then exit;
  try
    FS:=TFileOfCharStream.Create (FileName, fmOpenRead OR fmShareDenyWrite);   { Rohfile }
    try
      FSize:=FS.Size;
      { Konvertierung }
      Fehlertelegramm:=false; { Vorbelegung: Rohfile enth�lt kein Fehlertelegramm }
      while FS.Position < FSize do begin
        rohsatz:='';
        zeichen:=NUL;
        { Datens�tze bilden: bis EOT, ETX oder LF lesen }
        while (zeichen <> ETX) AND (zeichen <> EOT) AND (zeichen <> LF) AND
              (FS.Position < FSize) do begin
          FS.Read (zeichen);
          if (zeichen <> CR) AND (zeichen <> LF) AND
             (zeichen <> EOT) AND (zeichen <> ETX) then
            rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne CR, LF, EOT, ETX) }

          { das auf ETX und EOT folgende BCC �berlesen }
          if ((zeichen = ETX) OR (zeichen = EOT)) AND (FS.Position < FSize) then begin
            FS.Read (dummy);
          end;
        end;

        Application.ProcessMessages;
        { vom Rohsatz interessiert nur der Teil ab STX:
          -> ge�ndert wegen EK260 mit NUL-Zeichen bei Datenempfang mit Break; 11.04.2003, WW
          -> ge�ndert f�r Abruf mit Blockgr��e > 1; 03.12.2013, WW }
        iPos:=Pos (STX, rohsatz);
        if iPos > 0 then
          rohsatz:=Copy (rohsatz, iPos+1, length (rohsatz));

        if length (rohsatz) > 0 then begin
          { rohsatz in Datensatz-Werte '(..)' aufsplitten  }
          Jahr:=0;
          Monat:=0;
          Tag:=0;
          Stunde:=0;
          Minute:=0;
          Sekunde:=0;
          if (sMeldArchiv = CMeldNr_ParameterVeraendert) OR
             (sMeldArchiv = CMeldNr_EichParameterVeraendert) then  // 24.01.2014, WW
            szMNr:=sMeldArchiv
          else
            szMNr:='';
          Vz:='';
          sParaNrMrg:='';
          sOldValue:='';
          sNewValue:='';
          ParaChange:=nil;
          iOrdNr:=-1;  { Default-Ordnungsnummer; 28.09.2020, WW }

          satz_ok:=false;   { Vorbelegung: Datensatz ist nicht ok; 30.04.2012, WW }
          i:=0;
          while length (rohsatz) > 0 do begin
            inc (i);
            sbuf:=F_Zerlegen (rohsatz, ')');      { lesen bis ')' }

            { Datensatz-Wert auf richtige Struktur '(..)' pr�fen }
            wert_ok:=false;                { Vorbelegung: Datensatz-Wert ist falsch }
            len:=length (sbuf);
            if len > 0 then begin
              if sbuf[1] = '(' then begin     { erstes Zeichen mu� '(' sein }
                wert_ok:=true;                { Datensatz-Wert ist ok }
                sbuf:=Copy (sbuf, 2, length (sbuf));          { '(' wegschneiden }

                { auf Fehlertelegramm pr�fen (gekennzeichnet durch #): }
                if length (sbuf) > 0 then begin
                  if sbuf[1] = '#' then begin
                    Fehlertelegramm:=true;
                    break;
                  end;
                end;
              end;
            end;
            if not wert_ok then
              DataValid:=false;    { Fehler in Rohfile-Struktur }

            { i = 2: Archiv-Ordnungsnummer; 28.09.2020, WW }
            if i = 2 then begin
              if (length (sbuf) > 0) AND wert_ok then begin
                Val (sbuf, iBuf, Code);
                if Code = 0 then
                  iOrdNr:=iBuf
                else
                  DataValid:=false;
              end;
            end

            { i = 3: Zeitstempel }
            else if i = 3 then begin
              satz_ok:=true;   { Neue Vorbelegung: Datensatz ok, wenn Zeitstempel vorhanden; 30.04.2012, WW }
              if not wert_ok OR (length (sbuf) <> 19) then begin  { bei Strukturfehler: ganzen Datensatz verwerfen }
                satz_ok:=false;
                DataValid:=false;
                break;
              end;

              szTemp:=Copy (sbuf, 1, 4);        { Jahr }
              Val (szTemp, Jahr, Code);
              if Code <> 0 then
                wert_ok:=false;
              szTemp:=Copy (sbuf, 6, 2);        { Monat }
              Val (szTemp, Monat, Code);
              if Code <> 0 then
                wert_ok:=false;
              szTemp:=Copy (sbuf, 9, 2);        { Tag }
              Val (szTemp, Tag, Code);
              if Code <> 0 then
                wert_ok:=false;
              szTemp:=Copy (sbuf, 12, 2);       { Stunde }
              Val (szTemp, Stunde, Code);
              if Code <> 0 then
                wert_ok:=false;
              szTemp:=Copy (sbuf, 15, 2);       { Minute }
              Val (szTemp, Minute, Code);
              if Code <> 0 then
                wert_ok:=false;
              szTemp:=Copy (sbuf, 18, 2);       { Sekunde }
              Val (szTemp, Sekunde, Code);
              if Code <> 0 then
                wert_ok:=false;

              if not wert_ok then begin
                satz_ok:=false;
                DataValid:=false;
                break;
              end;

              try                        { �berpr�fung auf plausibles Datum, Zeit }
                EncodeDate (Jahr, Monat, Tag);
                EncodeTime (Stunde, Minute, Sekunde, 0);
              except
                satz_ok:=false;
                DataValid:=false;
                break;
              end;
            end

            else if i = 4 then begin
              if sMeldArchiv = '' then begin  // Meldungen des Logbuchs; 21.01.2014, WW
                { i = 4: ausl�sendes Ereignis zur Archivierung (Archiv-Ereigniscode) }
                if (length (sbuf) > 0) AND wert_ok then begin
                  { Meldungsnummer-Format: 0xnnnn
                    -> die letzten 5 Stellen auswerten (xnnnn) }
                  szMNr:=Copy (sbuf, length (sbuf) - 3, 4);  { die letzten 4 Zeichen: Meldungsnummer vom Ger�t }
                  Vz:=Copy (sbuf, length (sbuf) - 4, 1);     { 5.-letztes Zeichen: x }
                end
                else begin
                  satz_ok:=false;
                  DataValid:=false;      { bei fehlendem Wert oder Strukturfehler }
                  break;
                end;
              end

              else if (sMeldArchiv = CMeldNr_ParameterVeraendert) OR
                      (sMeldArchiv = CMeldNr_EichParameterVeraendert) then begin
                // Meldungen des �nderungs-Archivs oder eichtechnischen Logbuchs; 21.01.2014, WW
                { i = 4: Adresse des ge�nderten Parameters }
                if wert_ok then begin
                  // Parameter-Adressen kommen mit 4-stelligen Instanz- und Objektwerten.
                  // F�hrende Nullen wegschneiden f�r Ermittlung der allg. Parameternummer !
                  sParaNrMrg:=F_Zerlegen (sbuf, ':');
                  sParaNrMrg:=F_LeftTrunc (sParaNrMrg, '0') + ':' +
                              F_LeftTrunc (sbuf, '0');
                end
                else begin
                  satz_ok:=false;
                  DataValid:=false;      { bei Strukturfehler }
                  break;
                end;
              end;
            end

            else if i = 5 then begin
              if (sMeldArchiv = CMeldNr_ParameterVeraendert) OR
                 (sMeldArchiv = CMeldNr_EichParameterVeraendert) then begin
                // Meldungen des �nderungs-Archivs oder eichtechnischen Logbuchs; 21.01.2014, WW
                { i = 5: Alter Parameterwert }
                if wert_ok then
                  sOldValue:=sbuf
                else begin
                  satz_ok:=false;
                  DataValid:=false;      { bei Strukturfehler }
                  break;
                end;
              end;
            end

            else if i = 6 then begin
              if (sMeldArchiv = CMeldNr_ParameterVeraendert) OR
                 (sMeldArchiv = CMeldNr_EichParameterVeraendert) then begin
                // Meldungen des �nderungs-Archivs oder eichtechnischen Logbuchs; 21.01.2014, WW
                { i = 5: Neuer Parameterwert }
                if wert_ok then
                  sNewValue:=sbuf
                else begin
                  satz_ok:=false;
                  DataValid:=false;      { bei Strukturfehler }
                  break;
                end;
              end;
            end
          end;  { while length (rohsatz) }

          if Fehlertelegramm then
            break           { bei Fehlertelegramm: raus ohne Meldungs-Eintrag }
          else begin
            if satz_ok then begin
              if (sMeldArchiv = CMeldNr_ParameterVeraendert) OR
                 (sMeldArchiv = CMeldNr_EichParameterVeraendert) then  // 24.01.2014, WW
                ParaChange:=TParaChange.Create (sParaNrMrg, '', sOldValue, sNewValue);

              Meldung:=TMeldung.CreateMrgNr4stellig (szMNr, Vz, Jahr, Monat, Tag,
                                                     Stunde, Minute, Sekunde, ParaChange);
              if iOrdNr <> -1 then
                Meldung.OrdNr:=iOrdNr;  { Ordnungsnummer zuweisen; 28.09.2020, WW }

              Add (Meldung);
            end;
          end;
        end;  { if length (rohsatz) > 0 }
      end;  { while FS.Position < FSize }
    finally
      FS.Free;
    end;
  except
  end;
end;

{ Konvertieren von Meldungen der Gruppe G (Elster DS-100)
  -> Rohfileformat: aneinander gereihte Rohantworten auf Verbrauchsdaten-Lesebefehl ?x
                    (Verbruchsrohdaten enthalten auch Meldungen !)
                    Bei 4-Kanal-Ger�t sind die Rohantworten der einzelnen Kan�le
                    durch NUL-Zeichen voneinander getrennt.
  Beispiel: x<Datenteil>%<Checksumme, 2 Zeichen><CR><LF>
            x<Datenteil>%<Checksumme, 2 Zeichen><CR><LF>
            .
            .
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{---------------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenG (FileName: string; var DataValid: boolean);
{---------------------------------------------------------------------------------}
var
  MeldList_InsertPos: integer;

  {-----------------------------------------------------------------------}
  procedure KonvMeldung (ASteuercode: char; ADaten: string; AKanalNr: byte;
                         var ADataValid: boolean);
  {-----------------------------------------------------------------------}
  { Konvertiert Meldungs-Rohdatensatz;
    �bergabe: Steuercode-Zeichen
              Daten-String
              Kanalnummer
    R�ckgabe: ADataValid (true, wenn Meldungsrohsatz g�ltig) }
  var
    L, code: integer;
    year, month, day: integer;
    hour, min, sec: integer;
    szMNr: string;
    Meldung: TMeldung;

  begin
    L:=length (ADaten);
    Val (Copy (ADaten, L-1, 2), year, code);
    year:=Jahr2to4stellig (year);
    Val (Copy (ADaten, L-4, 2), month, code);
    Val (Copy (ADaten, L-7, 2), day, code);
    Val (Copy (ADaten, L-10, 2), hour, code);
    Val (Copy (ADaten, L-13, 2), min, code);
    Val (Copy (ADaten, L-16, 2), sec, code);
    try                        { �berpr�fung auf plausibles Datum, Zeit }
      EncodeDate (year, month, day);
      EncodeTime (hour, min, sec, 0);
    except
      ADataValid:=false;
      exit;
    end;

    { Meldungsnummer-Format:
      -> Eine Ger�temeldungsnummer wie in Wieser-Ger�ten �blich existiert nicht.
         Es wird eine "k�nstliche" ger�tespezifische Meldungsnummer gebildet.
         Format (3 Stellen): <Steuercodezeichen>_<Kanalnummer> }
    szMNr:=ASteuercode + '_' + IntToStr (AKanalNr);
    Meldung:=TMeldung.Create (szMNr, year, month, day, hour, min, sec, nil);
    // Meldung an Kanal-Merkposition einf�gen:
    // -> Rohdaten sind zeitlich absteigend, kanalweise aufsteigend
    Insert (MeldList_InsertPos, Meldung);
  end;

  {--------------------------------------------------------------------------------}
  procedure KonvMeldungDefStCode (ASteuercode: char; ADaten: string; AKanalNr: byte;
                                  var ADataValid: boolean);
  {--------------------------------------------------------------------------------}
  { Konvertiert Meldungs-Rohdatensatz eines definierbaren Steuercodes;
    �bergabe: Steuercode-Zeichen
              Daten-String
              Kanalnummer
    R�ckgabe: ADataValid (true, wenn Meldungsrohsatz g�ltig) }
  var
    L, code: integer;
    year, month, day: integer;
    hour, min, sec: integer;
    szMNr: string;
    Vz: string [1];
    Meldung: TMeldung;
    JahrOffset: byte;
    KennungStCode: integer;

  begin
    L:=length (ADaten);
    { Kennung des definierbaren Steuercodes: }
    Val ('$'+ Copy (ADaten, L-5, 3), KennungStCode, code);
    if KennungStCode in [$02, $09, $0A] then begin        { Kennungen mit Meldungen }
      case KennungStCode of
        $02: JahrOffset:=6;
        $09: JahrOffset:=15;
        $0A: JahrOffset:=6;
      else
        exit;
      end;

      Val (Copy (ADaten, L-JahrOffset-1, 2), year, code);
      year:=Jahr2to4stellig (year);
      Val (Copy (ADaten, L-JahrOffset-4, 2), month, code);
      Val (Copy (ADaten, L-JahrOffset-7, 2), day, code);
      Val (Copy (ADaten, L-JahrOffset-10, 2), hour, code);
      Val (Copy (ADaten, L-JahrOffset-13, 2), min, code);
      Val (Copy (ADaten, L-JahrOffset-16, 2), sec, code);
      try                        { �berpr�fung auf plausibles Datum, Zeit }
        EncodeDate (year, month, day);
        EncodeTime (hour, min, sec, 0);
      except
        ADataValid:=false;
        exit;
      end;

      { Meldungsnummer-Format:
        -> Eine Ger�temeldungsnummer wie in Wieser-Ger�ten �blich existiert nicht.
           Es wird eine "k�nstliche" ger�tespezifische Meldungsnummer gebildet.
           Format (5 Stellen): <Steuercodezeichen>_<letzte Stelle der Steuercode-Kennung>_<Kanalnummer>
        -> mu� aufgeteilt werden in 1 Zeichen Vz und 4 Zeichen szMNr, da szMNr nur maximal L�nge 4 zul��t }
      szMNr:='_' + Copy (ADaten, L-3, 1) + '_' + IntToStr (AKanalNr);
      Vz:=ASteuercode;
      Meldung:=TMeldung.CreateMrgNr4stellig (szMNr, Vz, year, month, day, hour, min, sec, nil);
      // Meldung an Kanal-Merkposition einf�gen:
      // -> Rohdaten sind zeitlich absteigend, kanalweise aufsteigend
      Insert (MeldList_InsertPos, Meldung);
    end;
  end;

var
  FS: TFileOfCharStream;  { Rohdatendatei }  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  KanalNr: integer;
  s: string;
  FFxSatz: string;
  PosFF: integer;  // Integer statt Byte; 04.10.2011, WW
  Daten: string;
  Steuercode: string;
  c: char;
  sKanalNr: string;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe: Rohdatenstruktur ist OK }

  KanalNr:=1;  { Vorbelegung: die ersten Rohdaten stammen von Kanal 1 }

  { Wenn dem Rohfilenamen durch Strichpunkt getrennt eine Kanalnummer vorangestellt
    ist: Kanalnummer, zu der die Meldungen geh�ren; 24.08.2011, WW }
  if Pos (';', Filename) > 0 then begin
    sKanalNr:=F_Zerlegen (Filename, ';');
    KanalNr:=StrToInt (sKanalNr);
  end;

  if not FileExists (FileName) then exit;
  try
    FS:=TFileOfCharStream.Create (FileName, fmOpenRead OR fmShareDenyWrite);   { Rohfile }
    try
      FSize:=FS.Size;
      MeldList_InsertPos:=0;  // Vorbelegung: Meldungen des ersten Kanals am Listenanfang einf�gen
      FFxSatz:='';
      { Konvertierung }
      while FS.Position < FSize do begin
        rohsatz:='';
        zeichen:=NUL;
        while (zeichen <> LF) AND (FS.Position < FSize) do begin
          FS.Read (zeichen);
          if zeichen = NUL then begin   { Trennzeichen NUL: Beginn der Rohdaten des n�chsten Kanals }
            inc (KanalNr);
            rohsatz:='';
            MeldList_InsertPos:=Count;    // neue Einf�geposition f�r Meldungen des Kanals merken
          end else
            rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
        end;

        { Rohsatz-Struktur pr�fen: erstes Zeichen mu� 'x' sein und '%' als
          Trennzeichen zur Checksumme mu� enthalten sein: }
        if (Pos ('x', rohsatz) <> 1) OR (Pos ('%', rohsatz) = 0) then begin
          DataValid:=false;
          Break;
        end;

        S:=ExtractString (rohsatz, 'x', '%', 0);    { reinen Datenteil rauskopieren }
        FFxSatz:=FFxSatz + S;
        PosFF:=DS100_FFxPos (FFxSatz); { Position eines evtl. vorhand. FFx-Steuercodes }
        { FFxSatz solange abarbeiten bis kein FFx-Steuercode mehr enthalten ist: }
        while PosFF <> 0 do begin
          Daten:=Copy (FFxSatz, 1, PosFF-1);
          Steuercode:=Copy (FFxSatz, PosFF, 3);
          c:=Steuercode[3];                                          { Code FFx }
          FFxSatz:=Copy (FFxSatz, PosFF+3, length (FFxSatz));  { Daten des n�chsten FFx-Satzes }
          case c of
            '2': begin  { Altstart }
                   KonvMeldung (c, Daten, KanalNr, DataValid);
                 end;
            '8': begin  { definierbarer Steuercode }
                   KonvMeldungDefStCode (c, Daten, KanalNr, DataValid);
                 end;
            'C': begin  { Auslesen-Beginn }
                   KonvMeldung (c, Daten, KanalNr, DataValid);
                 end;
            'D': begin  { Auslesen-Ende }
                   KonvMeldung (c, Daten, KanalNr, DataValid);
                 end;
          end; { case }

          PosFF:=DS100_FFxPos (FFxSatz); { Position eines evtl. weiteren FFx-Steuercodes }
        end;  { while PosFF }
      end;  { while FS.Position < FSize }
    finally
      FS.Free;
    end;
  except
  end;
end;

{ Konvertieren von Rohmeldungen der Gruppe H (Tritschler TDS, MCO, MC2, VC3, VCC)
  -> Es werden die Meldungen des eichpflichtigen und nicht-eichpflichtigen Logbuchs
     konvertiert.
  �bergabe: Rohdateiname
            Flag eichpflichtiges Logbuch konvertieren ja/nein
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{--------------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenH (FileName: string; var DataValid: boolean;
  bEichLogb: boolean);
{--------------------------------------------------------------------------------}
  {--------------------------------------------------------------------------------}
  function Is_LogbuchBeginn_Rohsatz (ARohsatz: string; bEichLogb: boolean): boolean;
  {--------------------------------------------------------------------------------}
  { pr�ft, ob Rohsatz den Beginn des eichpflichtigen oder nicht-eichpflichtigen
    Logbuchs kennzeichnet;
    �bergabe: Rohsatz
              Flag eichpflichtiges Logbuch ja/nein
    Ergebnis: true, wenn Rohsatz Beginn eines Logbuchs kennzeichnet }
  begin
    Result:=(Copy (ARohsatz, 1, 5) = '9.91.');  // nicht-eichpflichtiges Logbuch

    if bEichLogb then  // 28.06.2018, WW
      Result:=Result OR
              (Copy (ARohsatz, 1, 5) = '9.92.');  // eichpflichtiges Logbuch
  end;

  {----------------------------------------------------------}
  function Is_LogbuchEnde_Rohsatz (ARohsatz: string): boolean;
  {----------------------------------------------------------}
  { pr�ft, ob Rohsatz das Ende eines Logbuchs kennzeichnet;
    �bergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Ende eines Logbuchs kennzeichnet }
  begin
    Result:=Copy (ARohsatz, 1, 1) = ')';
  end;

Const
  CTrenner = ';';

var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  s: string;
  i: integer;
  dtBuf: TDateTime;
  Satz_OK: boolean;
  S1: string;
  bLogbuchBeginn_gelesen: boolean;
  Meldung: TMeldung;
  DatumZeit: TDateTime;
  year, month, day: word;
  hour, min, sec, msec: word;
  szMNr: string;
  sKanalNr: string;
  iOrdNr: longint;
  iBuf: longint;
  Code : Integer;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }
  if not FileExists (FileName) then exit;
  try
    { Rohfile �ffnen: }
    FS:=TFileOfCharStream.Create (Filename, fmOpenRead OR fmShareDenyWrite);
    try
      FSize:=FS.Size;

      rohsatz:='';
      zeichen:=NUL;
      { Rohfileheader bis zum STX lesen }
      while (zeichen <> STX) AND (FS.Position < FSize) do begin
        FS.Read (zeichen);
        rohsatz:=rohsatz + zeichen;                    { Zeile bilden }
      end;

      { Konvertierung }
      bLogbuchBeginn_gelesen:=false;   { Vorbelegung: Beginn eines Logbuchs noch nicht gelesen }
      while (zeichen <> ETX) AND (FS.Position < FSize) do begin
        rohsatz:='';
        zeichen:=NUL;
        while (zeichen <> LF) AND (zeichen <> ETX) AND (FS.Position < FSize) do begin
          FS.Read (zeichen);
          rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
        end;

        Application.ProcessMessages;

        if not bLogbuchBeginn_gelesen then  { Beginn eines Logbuchs noch nicht gelesen }
          { pr�fen auf Beginn eines Logbuchs: }
          bLogbuchBeginn_gelesen:=Is_LogbuchBeginn_Rohsatz (rohsatz, bEichLogb)
        else begin
          { pr�fen auf Ende eines Logbuchs: }
          if Is_LogbuchEnde_Rohsatz (rohsatz) then
            bLogbuchBeginn_gelesen:=false   { wieder Vorbelegung: Beginn eines Logbuchs noch nicht gelesen }
          else begin
            if length (rohsatz) > 0 then begin
              DatumZeit:=0;
              iOrdNr:=-1;  { Default-Ordnungsnummer; 28.09.2020, WW }
              Satz_OK:=true;
              i:=0;
              while (length (rohsatz) > 0) AND Satz_OK do begin
                S:=F_Zerlegen (rohsatz, CTrenner);
                if length (S) > 0 then begin
                  inc (i);  // Z�hler f�r Teils�tze erh�hen
                  if i = 1 then begin  { 1. Teilsatz mit Zeitstempel, Logbuch-Kennzeichen, Ordnungsnummer }
                    S1:=Copy (S, 1, 6);  { Datum }
                    if EncodeDateStr (S1, 'YYMMDD', dtBuf) then
                      DatumZeit:=dtBuf
                    else
                      Satz_OK:=false;

                    S1:=Copy (S, 7, 6);  { Zeit }
                    if EncodeTimeStr (S1, 'HHMMSS', dtBuf) then
                      DatumZeit:=DatumZeit + dtBuf
                    else
                      Satz_OK:=false;

                    S1:=Copy (S, 13, 3);  { UTC-Zeitzonen-Differenz }
                    if UTCTimezoneStrToDateTime (S1, dtBuf) then
                      DatumZeit:=DatumZeit + dtBuf
                    else
                      Satz_OK:=false;

                    S1:=Copy (S, 17, length (S));  { Ordnungsnummer; 28.09.2020, WW }
                    if length (S1) > 0 then begin
                      Val (S1, iBuf, Code);
                      if Code = 0 then
                        iOrdNr:=iBuf
                      else
                        DataValid:=false;
                    end;
                  end
                  else if i = 2 then begin  { 2. Teilsatz mit Meldungskennung und Kanalnummer }
                    if length (S) >= 3 then begin
                      szMNr:=Copy (S, 1, 2);  { Meldungkennung }
                      { bei kanalspezifischen Meldungen, wird die Ger�te-
                        Meldungsnummer aus Meldungskennung und Kanalnummer gebildet: }
                      sKanalNr:=S [3];  { Kanalnummer }
                      if sKanalNr > '0' then  { 0 =  kanalneutral }
                        szMNr:=szMNr + '_' + sKanalNr;  { Kanalnummer mit "optischem" Trennzeichen anh�ngen }
                    end else
                      Satz_OK:=false;
                  end;
                end;  { if length (S) > 0 }
              end;  { while length (rohsatz) > 0 }

              if Satz_OK then begin
                DecodeDate (DatumZeit, year, month, day);
                DecodeTime (DatumZeit, hour, min, sec, msec);

                Meldung:=TMeldung.CreateMrgNr4stellig (szMNr, '', year, month, day,
                                                       hour, min, sec, nil);
                if iOrdNr <> -1 then
                  Meldung.OrdNr:=iOrdNr;  { Ordnungsnummer zuweisen; 28.09.2020, WW }

                // Meldung am Anfang der Liste einf�gen:
                // -> Rohdaten sind zeitlich absteigend
                Insert (0, Meldung);
              end else
                DataValid:=false;
            end;  { if length (rohsatz) > 0 }
          end;  { if not Is_LogbuchEnde_Rohsatz }
        end;  { if not bLogbuchBeginn_gelesen }
      end;  { while (zeichen <> ETX) AND (FS.Position < FSize) }
    finally
      FS.Free;           { Rohfile schlie�en }
    end;
  except
  end;
end;

{ Konvertieren von Rohmeldungen der Gruppe I (Tritschler SR)
  -> Es werden die Meldungss�tze in der Antwort auf das Login-Quittierungstelegramm
     konvertiert.
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{---------------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenI (FileName: string; var DataValid: boolean);
{---------------------------------------------------------------------------------}
  {--------------------------------------------------------------}
  function Is_MeldungenBeginn_Rohsatz (ARohsatz: string): boolean;
  {--------------------------------------------------------------}
  { pr�ft, ob Rohsatz den Beginn der Meldungen kennzeichnet;
    �bergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Beginn der Meldungen kennzeichnet }
  begin
    Result:=(Copy (ARohsatz, 1, 3) = '43.');
  end;

  {------------------------------------------------------------}
  function Is_MeldungenEnde_Rohsatz (ARohsatz: string): boolean;
  {------------------------------------------------------------}
  { pr�ft, ob Rohsatz das Ende der Meldungen kennzeichnet;
    �bergabe: Rohsatz
    Ergebnis: true, wenn Rohsatz Ende der Meldungen kennzeichnet }
  begin
    Result:=Copy (ARohsatz, 1, 1) = ')';
  end;

Const
  CTrenner = ';';

var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  s: string;
  i: integer;
  dtBuf: TDateTime;
  Satz_OK: boolean;
  bMeldungenBeginn_gelesen: boolean;
  Meldung: TMeldung;
  dtDatum: TDateTime;
  dtZeit: TDateTime;
  year, month, day: word;
  hour, min, sec, msec: word;
  sKG: string;
  sMNr: string;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }
  if not FileExists (FileName) then exit;
  try
    { Rohfile �ffnen: }
    FS:=TFileOfCharStream.Create (Filename, fmOpenRead OR fmShareDenyWrite);
    try
      FSize:=FS.Size;

      rohsatz:='';
      zeichen:=NUL;
      { Rohfileheader bis zum STX lesen }
      while (zeichen <> STX) AND (FS.Position < FSize) do begin
        FS.Read (zeichen);
        rohsatz:=rohsatz + zeichen;                    { Zeile bilden }
      end;

      { Konvertierung }
      bMeldungenBeginn_gelesen:=false;   { Vorbelegung: Beginn eines Logbuchs noch nicht gelesen }
      while (zeichen <> ETX) AND (FS.Position < FSize) do begin
        rohsatz:='';
        zeichen:=NUL;
        while (zeichen <> LF) AND (zeichen <> ETX) AND (FS.Position < FSize) do begin
          FS.Read (zeichen);
          rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
        end;

        Application.ProcessMessages;

        if not bMeldungenBeginn_gelesen then  { Beginn der Meldungen noch nicht gelesen }
          { pr�fen auf Beginn der Meldungen: }
          bMeldungenBeginn_gelesen:=Is_MeldungenBeginn_Rohsatz (rohsatz)
        else begin
          { pr�fen auf Ende der Meldungen: }
          if Is_MeldungenEnde_Rohsatz (rohsatz) then
            Break   { fertig, raus }
          else begin
            if length (rohsatz) > 0 then begin
              sKG:='';
              sMNr:='';
              dtDatum:=0;
              dtZeit:=0;
              Satz_OK:=true;
              i:=0;
              while (length (rohsatz) > 0) AND Satz_OK do begin
                S:=F_Zerlegen (rohsatz, CTrenner);
                if length (S) > 0 then begin
                  inc (i);  // Z�hler f�r Teils�tze erh�hen
                  if i = 1 then begin  { 1. Teilsatz mit Kommt/Geht }
                    if (S = 'K') OR (S = 'G') then
                      sKG:=S
                    else
                      Satz_OK:=false;
                  end
                  else if i = 2 then begin  { 2. Teilsatz mit Meldungstyp und -nummer }
                    sMNr:=S;
                  end
                  else if i = 3 then begin  { 3. Teilsatz mit Datum }
                    if EncodeDateStr (S, 'YY-MM-DD', dtBuf) then
                      dtDatum:=dtBuf
                    else
                      Satz_OK:=false;
                  end
                  else if i = 4 then begin  { 4. Teilsatz mit Zeit }
                    if EncodeTimeStr (S, 'HH:MM:SS', dtBuf) then
                      dtZeit:=dtBuf
                    else
                      Satz_OK:=false;
                  end
                end;  { if length (S) > 0 }
              end;  { while length (rohsatz) > 0 }

              if Satz_OK then begin
                DecodeDate (dtDatum, year, month, day);
                DecodeTime (dtZeit, hour, min, sec, msec);

                Meldung:=TMeldung.CreateMrgNr4stellig (sMNr + sKG, '', year, month, day, hour, min, sec, nil);
                Add (Meldung);
              end else
                DataValid:=false;
            end;  { if length (rohsatz) > 0 }
          end;  { if not Is_MeldungenEnde_Rohsatz }
        end;  { if not bMeldungenBeginn_gelesen }
      end;  { while (zeichen <> ETX) AND (FS.Position < FSize) }
    finally
      FS.Free;           { Rohfile schlie�en }
    end;
  except
  end;
end;

{ Konvertieren von Rohmeldungen der Gruppe J (Actaris Corus)
  -> Es werden die Meldungen des Ereignis-, Parameter- oder eichpflichtigen Logbuchs
     konvertiert.
  -> Filename mit Format: <Datenspeicher-ID>;<Rohdateiname>
     Datenspeicher-ID 4 = Ereignis-Logbuch
                      5 = Parameter-Logbuch
                      6 = Eichtechnisches Logbuch
  �bergabe: Record mit Angaben f�r MRG-Meldungskonvertierung
            Liste mit Parameternummern-Konfiguration (Wenn nil, wird die
              Konfiguration aus der Ressourcendatei gelesen)
            Liste mit Konfiguration der meldungsspezifischen Parameternummern (Wenn nil,
              wird die Konfiguration aus der Ressourcendatei gelesen)
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{-------------------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenJ (DBID_FileName: string; MeldKonv: TMeldKonv;
  AParamMrgKonfigList: TParamMrgKonfigList; AParamMeldKonfigList: TParamMeldKonfigList;
  var DataValid: boolean);
{-------------------------------------------------------------------------------------}
const
  { Recordgr��en }
  CRecSize_EventLog     =  5;  { Recordgr��e f�r Ereignis-Logbuch mit gesetzten
                                 Datamask-Bits f�r Datum und Ereigniscode }
  CRecSize_ParameterLog = 21;  { Recordgr��e Parameter-Logbuch mit gesetzten
                                 Datamask-Bits f�r Datum, Parametercode, alter
                                 und neuer Wert }
  CRecSize_VerifLog     = 21;  { Recordgr��e eichtechnisches Logbuch wie Parameter-Logbuch }

type
  { Zust�nde f�r Rohdatenkonvertierung }
  TModus = (m_Start, m_Size, m_NumFrame_End, m_RecSize_Num, m_Data);

var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  S: string;
  Meldung: TMeldung;
  year, month, day: word;
  hour, min, sec, msec: word;
  sMNr: string;
  Modus: TModus;
  iSize: byte;
  iNumframe: word;
  iRecSize: byte;
  iSizeData_Soll: integer;
  sData: string;
  sDataRec: string;
  bMNr: byte;
  dtDatumZeit: TDateTime;
  ParamMeldKonfigList: TParamMeldKonfigList;
  ParamMeldData: TParamMeldData;
  ParamMrgKonfigList: TParamMrgKonfigList;
  ParamMrgData: TParamMrgData;
  bParaCode: byte;
  ParaChange: TParaChange;
  sParaNrMrg: string;
  sParaOldValueRoh: string;
  sParaNewValueRoh: string;
  DatenspeicherID: integer;
  sDatenspeicherID: string;
  Filename: string;
  iRecSize_Soll: byte;
  sOldValue: string;
  sNewValue: string;
  iParaDatentyp: integer;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }

  S:=DBID_FileName;
  sDatenspeicherID:=F_Zerlegen (S, ';');  { Datenspeicher-ID bis zum Strichpunkt }
  DatenspeicherID:=StrToInt (sDatenspeicherID);
  Filename:=S;  { Rohfilename }

  case DatenspeicherID of
    4: begin  // Ereignislogbuch
         iRecSize_Soll:=CRecSize_EventLog;
       end;
    5: begin  // Parameter-Logbuch
         iRecSize_Soll:=CRecSize_ParameterLog;
       end;
    6: begin  // Eichtechnisches Logbuch
         iRecSize_Soll:=CRecSize_VerifLog;
       end;
  else
    exit;
  end;

  if not FileExists (FileName) then exit;
  try
    { Liste mit Konfigurationsdaten f�r meldungsspezifische Parameternummern
      laden (wird f�r Parameter�nderungsmeldungen des Parameter- und eichtechnischen
      Logbuchs ben�tigt): }
    if not Assigned (AParamMeldKonfigList) then
      ParamMeldKonfigList:=TParamMeldKonfigList.Create  // Liste lokal anlegen
    else
      ParamMeldKonfigList:=AParamMeldKonfigList;  // �bergebene Liste wird verwendet; 06.08.2021, WW
    try
      { Liste mit meldungsspezifischen Parameternummern aus Resourcendatei laden,
        wenn lokal angelegt: }
      if not Assigned (AParamMeldKonfigList) then  // 09.07.2021, WW
        GetParamMeld_KonfigList_ByParaGruppe (MeldKonv.ParameterGruppe, ParamMeldKonfigList, KonfigPfad);

      { Parameternummern-Konfigurationsliste: }
      if not Assigned (AParamMrgKonfigList) then
        ParamMrgKonfigList:=TParamMrgKonfigList.Create  // Parameternummern-Konfigurationsliste lokal anlegen
      else
        ParamMrgKonfigList:=AParamMrgKonfigList;  // �bergebene Parameternummern-Konfigurationsliste wird verwendet; 09.07.2021, WW
      try
        { Parameternummern-Konfigurationsliste aus Resourcendatei laden, wenn lokal angelegt: }
        if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
          GetParamMrg_KonfigList_ByParaGruppe (MeldKonv.ParameterGruppe,
                                               MeldKonv.ParameterUnterGruppe,  // 11.02.2022, WW
                                               ParamMrgKonfigList, KonfigPfad);

        { Rohfile �ffnen: }
        FS:=TFileOfCharStream.Create (Filename, fmOpenRead OR fmShareDenyWrite);
        try
          FSize:=FS.Size;

          rohsatz := '';
          sData:='';
          iSize:=0;
          iNumframe:=0;
          iRecSize:=0;
          Modus:=m_Start;  { als erstes wird das Startzeichen des 1. Telegramms gelesen }

          while FS.Position < FSize do begin
            Application.ProcessMessages;
            FS.Read (zeichen);
            rohsatz := rohsatz + zeichen;

            case Modus of
              m_Start:
                begin
                  if length (rohsatz) = 1 then begin
                    if rohsatz [1] = SOH then  { Startzeichen SOH gelesen }
                      Modus:=m_Size;  { als N�chstes bis zum L�ngenbyte lesen }
                  end;
                  rohsatz:='';
                end;

              m_Size:
                begin
                  if length (rohsatz) = 1 then begin  { L�ngenbyte gelesen }
                    iSize:=Ord (rohsatz [1]);
                    Modus:=m_NumFrame_End;  { als N�chstes Numframe_End-Bytes lesen  }
                    rohsatz:='';
                  end;
                end;

              m_NumFrame_End:
                begin
                  if length (rohsatz) = 2 then begin  { Numframe_End-Bytes gelesen }
                    iNumframe:=Bin2Word (rohsatz) AND $7FFF;
                    if iNumframe = 0 then  { erstes Telegramm }
                      Modus:=m_RecSize_Num  { als N�chstes Recordgr��e und Anzahl der Records lesen }
                    else  { alle weiteren Folgetelegramme }
                      Modus:=m_Data;  { als N�chstes der Datenteil des Telegramms }
                    rohsatz:='';
                  end;
                end;

              m_RecSize_Num:
                begin
                  if length (rohsatz) = 3 then begin  { Bytes f�r Recordgr��e und Anzahl der Records gelesen }
                    iRecSize:=Ord (rohsatz [1]);  { Recordgr��e }
                    if iRecSize <> iRecSize_Soll then begin  { Soll/Ist-Fehler Recordgr��e }
                      DataValid:=false;
                      Break;
                    end;
                    Modus:=m_Data;   { als N�chstes der Datenteil des Telegramms }
                    rohsatz:='';
                  end;
                end;

              m_Data:
                begin
                  if iNumframe = 0 then
                    iSizeData_Soll:=iSize - 5  { ohne Bytes f�r Numframe_End und Recordgr��e/Anzahl }
                  else
                    iSizeData_Soll:=iSize - 2;  { ohne Bytes f�r Numframe_End }

                  if length (rohsatz) = (iSizeData_Soll + 3) then begin  { Telegramm-Daten, Endezeichen und 2 CRC-Zeichen gelesen }
                    sData:=sData + Copy (rohsatz, 1, length (rohsatz)-3);  { Datenteil anh�ngen }
                    Modus:=m_Start;     { weiteres Daten-Folgetelegramm lesen }
                    rohsatz:='';
                    while length (sData) >= iRecSize do begin    { alle vollst�ndig enthaltenen Records jetzt Konvertieren }
                      sDataRec:=Copy (sData, 1, iRecSize);  // einen Datenrecord rauskopieren
                      System.Delete (sData, 1, iRecSize);  // Rest des Datenteils

                      ParaChange:=nil;
                      if DatenspeicherID = 4 then begin
                        { Eintrag im Ereignislogbuch: }
                        S:=Copy (sDataRec, 1, 4);  { Datum }
                        dtDatumZeit:=Bin2Date_Corus (S);
                        S:=Copy (sDataRec, 5, 1);  { Ereigniscode }
                        bMNr:=Bin2Byte (S);
                        sMNr:=IntToStr (bMNr);
                      end
                      else begin
                        { Eintrag im Parameter- oder eichtechnischen Logbuch }
                        S:=Copy (sDataRec, 1, 4);  { Datum }
                        dtDatumZeit:=Bin2Date_Corus (S);
                        S:=Copy (sDataRec, 5, 1);  { Parametercode }
                        bParaCode:=Bin2Byte (S);
                        sParaOldValueRoh:=Copy (sDataRec, 6, 8);  { alter Wert }
                        sParaNewValueRoh:=Copy (sDataRec, 14, 8);  { neuer Wert }

                        if DatenspeicherID = 5 then begin
                          sMNr:=CMeldNr_ParameterVeraendert;  { k�nstliche Meldungsnummer }
                          { Parameter-Logbuch: zum meldungsspezifischen Parametercode
                            die ger�tespezifische Parameternummer suchen }
                          if ParamMeldKonfigList.FindParamMeldData (MeldKonv.Parametergruppe,
                               IntToStr (bParaCode), ParamMeldData) then
                            sParaNrMrg:=ParamMeldData.Parameternummer_im_MRG
                          else
                            sParaNrMrg:='';
                        end
                        else begin
                          sMNr:=CMeldNr_EichParameterVeraendert;  { k�nstliche Meldungsnummer }
                          { eichtechnisches Logbuch enth�lt ger�tespezifische Parameternummer }
                          sParaNrMrg:=IntToStr (bParacode);
                        end;

                        sOldValue:='';
                        sNewValue:='';
                        if ParamMrgKonfigList <> nil then begin
                          if ParamMrgKonfigList.FindParamMrgData (MeldKonv.Parametergruppe,
                                                                  sParaNrMrg,
                                                                  ParamMrgData) then begin
                            { Bin�re Parameter-Rohwerte alt/neu zu Anzeige-Strings formatieren: }
                            iParaDatentyp:=StrToIntDef (ParamMrgData.ParaDatentyp, -1);  // 08.03.2019, WW
                            sOldValue:=FormatPara_Corus_Anzeige (sParaOldValueRoh, iParaDatentyp, 1);
                            sNewValue:=FormatPara_Corus_Anzeige (sParaNewValueRoh, iParaDatentyp, 1);
                          end;
                        end;
                        ParaChange:=TParaChange.Create (sParaNrMrg, '', sOldValue, sNewValue);
                      end;

                      if dtDatumZeit > 0 then begin
                        DecodeDateTime (dtDatumZeit, year, month, day, hour, min, sec, msec);
                        Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '',
                          year, month, day, hour, min, sec, ParaChange);

                        // Meldung am Anfang der Liste einf�gen:
                        // -> Rohdaten sind zeitlich absteigend
                        Insert (0, Meldung);
                      end else  { if DatumZeit > 0 }
                        DataValid:=false;
                    end;  { while length (sData) >= iRecSize }
                  end;  { if length (rohsatz) }
                end;
            end;  { case }
          end;   { while fs_roh.Position < FSize_roh }
        finally
          FS.Free;           { Rohfile schlie�en }
        end;
      finally
        // Parameternummern-Konfigurationsliste nur freigeben, wenn lokal angelegt
        if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
          ParamMrgKonfigList.Free;
      end;
    finally
      // Konfigurationsliste mit meldungsspezifischen Parameternummern nur
      // freigeben, wenn lokal angelegt
      if not Assigned (AParamMeldKonfigList) then  // 06.08.2021, WW
        ParamMeldKonfigList.Free;
    end;
  except
  end;
end;

{ Konvertieren von Rohmeldungen der Gruppe K (IEC1107)
  -> Es werden die Meldungen des Betriebs- und eichpflichtigen Logbuchs konvertiert.  
  -> Jeder Logbuchrohsatz enth�lt ein Statuswort, welches Informationen �ber
     Spontan-Ereignisse (einwertig) oder Zust�nde (zweiwertig, kommt/geht) enth�lt.
     Zur Konvertierung von Zust�nden in Kommt/Geht-Meldungen mu� der Wechsel
     des entsprechenden Statusbits ausgewertet werden. Es kann daher prinzipbedingt
     nie aus dem ersten Rohsatz eine Kommt/Geht-Meldung generiert werden, da ein
     Vergleichssatz nicht existiert. Um bei der Langzeitarchivierung der Meldungen
     dieses Problem zu vermeiden, mu� der j�ngste Logbucheintrag erneut ausgelesen
     werden (�berlappender Abruf) !
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{---------------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenK (FileName: string; var DataValid: boolean);
{---------------------------------------------------------------------------------}

  {-----------------------------------------------------------------}
  function BuildMrgMeldNr_1107 (AKennzahlLB: string; ABitNr: integer;
    AGeht: boolean): string;
  {-----------------------------------------------------------------}
  { k�nstliche MRG-Meldungsnummer aus Logbuch-Kennzahl, Bitnummer des Status und
    Kommt/Geht-Info bilden }
  begin
    if AKennzahlLB = 'P.98' then  { Betriebslogbuch }
      Result:='B'
    else if AKennzahlLB = 'P.99' then  { Logbuch f�r eichtechnisch relevante Daten }
      Result:='E'
    else
      Result:='?';
    Result:=Result + IntToStr (ABitNr);  { Bitnummer }
    if AGeht then
      Result:=Result + '-';  { Minus f�r Geht-Meldung }
  end;

const
  { Status-Bitnummern, welche einen Zustand repr�sentieren }
  CStatusbits_Zustand = [0, 1, 2, 9, 11, 15, 22, 23];

var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  s: string;
  dtBuf: TDateTime;
  Satz_OK: boolean;
  Meldung: TMeldung;
  year, month, day: word;
  hour, min, sec, msec: word;
  sMNr: string;
  dummy: char;
  iPos: integer;
  S1: string;
  dtDatumZeit: TDateTime;
  iStatus: cardinal;
  iStatus_Merk: cardinal;
  bStatus_Merk: boolean;
  Code: integer;
  iAnzDatenwerte: integer;
  ParaChange: TParaChange;
  i, j: integer;
  Maske: cardinal;
  sKennzahlLB: string;
  sKennzahlWert: string;
  sNewValue: string;
  bBitGesetzt: boolean;
  bBitGesetzt_Merk: boolean;
  sZeitformat: string;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }
  if not FileExists (FileName) then exit;
  try
    { Rohfile �ffnen: }
    FS:=TFileOfCharStream.Create (Filename, fmOpenRead OR fmShareDenyWrite);
    try
      FSize:=FS.Size;

      iStatus_Merk:=0;
      bStatus_Merk:=false;
      while FS.Position < FSize do begin
        rohsatz:='';
        zeichen:=NUL;
        { Datens�tze bilden: bis EOT, ETX oder LF lesen }
        while (zeichen <> ETX) AND (zeichen <> EOT) AND (zeichen <> LF) AND
              (FS.Position < FSize) do begin
          FS.Read (zeichen);
          if (zeichen <> CR) AND (zeichen <> LF) AND
             (zeichen <> ETX) AND (zeichen <> EOT) then
            rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne CR, LF, ETX, EOT) }

          if (zeichen = ETX) OR (zeichen = EOT) then begin
            if (FS.Position < FSize) then
              FS.Read (dummy);  { das auf ETX und EOT folgende BCC �berlesen }
          end;
        end;

        Application.ProcessMessages;

        { Falls STX im Rohsatz vorhanden ist, interessiert nur der nachfolgende Datenteil: }
        iPos:=Pos (STX, rohsatz);
        if iPos > 0 then
          rohsatz:=Copy (rohsatz, iPos + 1, length (rohsatz));

        { Logbuch-Eintrag: }
        if length (rohsatz) > 0 then begin
          sKennzahlLB:=ExtractString (rohsatz, NUL, '(', 0);   { Kennzahl des Logbuchs }

          S:=ExtractString (rohsatz, '(', ')', 0);  { Zeitstempel oder ERROR }
          if S = 'ERROR' then Break;  { keine Daten vorhanden }

          Satz_OK:=true;
          dtDatumZeit:=-1;

          { m�gliche Formate des Zeitstempels: ZST10, ZSTs11, ZST12, ZSTs13 }
          if (length (S) = 11) OR (length (S) = 13) then
            S:=Copy (S, 2, length (S));  // Saisonkennzeichen wegschneiden

          S1:=Copy (S, 1, 6);  { Datum }
          if EncodeDateStr (S1, 'YYMMDD', dtBuf) then begin
            dtDatumZeit:=dtBuf;

            S1:=Copy (S, 7, length (S));  { Zeit }
            if length (S1) = 4 then
              sZeitformat:='HHMM'
            else
              sZeitformat:='HHMMSS';
            if EncodeTimeStr (S1, sZeitformat, dtBuf) then
              dtDatumZeit:=dtDatumZeit + dtBuf
            else
              Satz_OK:=false;  { Zeit ung�ltig }
          end else
            Satz_OK:=false;  { Datum ung�ltig }

          S:=ExtractString (rohsatz, '(', ')', 1);  { Status (Hex) }
          S:=Copy (S, 1, 8);  { max. 32 Bit }
          Val ('$' + S, iStatus, Code);

          S:=ExtractString (rohsatz, '(', ')', 3);  { Anzahl der Datenwerte }
          Val (S, iAnzDatenwerte, Code);
          if Code <> 0 then
            Satz_OK:=false;  { Anzahl Datenwerte ung�ltig }

          if Satz_OK then begin
            DecodeDateTime (dtDatumZeit, year, month, day, hour, min, sec, msec);

            Maske:=1;
            for i:=0 to 31 do begin
              Application.ProcessMessages;

              if i in CStatusbits_Zustand then begin  { Zustand }
                if bStatus_Merk then begin
                  bBitGesetzt:=(iStatus AND Maske) <> 0;  { Bit-Zustand des aktuellen Eintrags }
                  bBitGesetzt_Merk:=(iStatus_Merk AND Maske) <> 0;  { Bit-Zustand des vorherigen Eintrags }

                  if bBitGesetzt <> bBitGesetzt_Merk then begin  { Bit hat gewechselt }
                    { k�nstliche ger�tespez. Meldungsnummer: }
                    sMNr:=BuildMrgMeldNr_1107 (sKennzahlLB, i, not bBitGesetzt);

                    Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '', year, month, day, hour, min, sec, nil);
                    Add (Meldung);
                  end;
                end;
              end
              else begin  { Ereignis oder reserviert }
                if (iStatus AND Maske) <> 0 then begin   { Bit gesetzt }
                  { k�nstliche ger�tespez. Meldungsnummer: }
                  sMNr:=BuildMrgMeldNr_1107 (sKennzahlLB, i, false);

                  if i = 5 then begin  { Bit 5: Ger�teuhr wurde gestellt -> Parameter�nderung }
                    for j:=1 to iAnzDatenwerte do begin  { f�r jeden ge�nderten Wert eine Parameter�nderungsmeldung }
                      sKennzahlWert:=ExtractString (rohsatz, '(', ')', (j + 1) * 2);  { Kennzahl des Datenwerts }
                      sNewValue:=ExtractString (rohsatz, '(', ')', ((iAnzDatenwerte + 2) * 2) + (j - 1));  { Datenwert }

                      ParaChange:=TParaChange.Create (sKennzahlWert, '', '', sNewValue);
                      Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '', year, month, day, hour, min, sec, ParaChange);
                      Add (Meldung);
                    end;  { for j }
                  end
                  else begin  { alle �brigen Ereignisse }
                    Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '', year, month, day, hour, min, sec, nil);
                    Add (Meldung);
                  end;
                end;
              end;

              Maske:=Maske SHL 1;
            end;  { for i }

            iStatus_Merk:=iStatus;
            bStatus_Merk:=true;
          end else
            DataValid:=false;
        end;  { if length (rohsatz) > 0 }
      end;  { while FS.Position < FSize }
    finally
      FS.Free;
    end;
  except
  end;
end;

{ Konvertieren von Rohmeldungen der Gruppe L (EC 900)
  -> Es werden die Meldungen des eichtechnischen, Parameter-, Ereignis-Logbuchs (EC 900)
     sowie des Parameter-Logbuchs und des Meldungsarchivs (CU) konvertiert.
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{---------------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenL (FileName: string; var DataValid: boolean);
{---------------------------------------------------------------------------------}

var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  s: string;
  Satz_OK: boolean;
  Meldung: TMeldung;
  year, month, day: word;
  hour, min, sec, msec: word;
  sMNr: string;
  iPos: integer;
  dtDatumZeit: TDateTime;
  ParaChange: TParaChange;
  i: integer;
  STX_gelesen: boolean;
  iArchivNr: integer;
  iKanaele: integer;
  sbuf: string;
  sParaNrMrg: string;
  sOldValue: string;
  sNewValue: string;
  Vz: string [1];
  iOrdNr: longint;
  iBuf: longint;
  Code: integer;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }
  if not FileExists (FileName) then exit;
  try
    { Rohfile �ffnen: }
    FS:=TFileOfCharStream.Create (Filename, fmOpenRead OR fmShareDenyWrite);
    try
      FSize:=FS.Size;
      STX_gelesen:=false;
      iArchivNr:=-1;  // Vorbelegung: undefiniert

      while FS.Position < FSize do begin
        rohsatz:='';
        zeichen:=NUL;
        { Datens�tze bilden: bis ETX oder US lesen }
        while (zeichen <> ETX) AND (zeichen <> US) AND
              (FS.Position < FSize) do begin
          FS.Read (zeichen);

          if (zeichen <> ETX) AND (zeichen <> US) then
            rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne ETX, US) }
        end;

        Application.ProcessMessages;

        { Falls STX im Rohsatz vorhanden ist, Header auswerten (Befehlszeichen,
          Archivnummer, Kan�le): }
        if not STX_gelesen then begin
          iPos:=Pos (STX, rohsatz);
          if iPos > 0 then begin
            STX_gelesen:=true;
            rohsatz:=Copy (rohsatz, iPos + 1, length (rohsatz));

            S:=Copy (rohsatz, 1, 1);  // Befehlszeichen
            if S = 'A' then begin  // Rohdaten aus Archiv-Abfragebefehl
              S:=Copy (rohsatz, 2, 2);  // Archivnummer
              iArchivNr:=StrToIntDef (S, -1);
              S:=Copy (rohsatz, 5, length (rohsatz));  // Kan�le
              iKanaele:=StrToIntDef ('$' + S, -1);
              if iKanaele < 0 then exit;

              case iArchivNr of
                7, 8, 10:  // eichtechnisches LB (EC 900), Parameter-Logbuch (EC 900), Parameter-Logbuch (CU)
                  begin
                    // "Kan�le" Parameternummer, alter Wert und neuer Wert m�ssen vorhanden sein:
                    if ((iKanaele AND $01) = 0) OR
                       ((iKanaele AND $02) = 0) OR
                       ((iKanaele AND $04) = 0) then exit;
                  end;

                9, 11:  // Ereignis-Logbuch (EC 900), Meldungsarchiv (CU)
                  begin
                    // "Kanal" Ereignisnummer mu� vorhanden sein:
                    if ((iKanaele AND $01) = 0) then exit;
                  end;
              else
                exit;
              end;  { case iArchivNr }
            end else
              exit;
          end;
        end
        else begin  { Logbuch-Eintrag }
          if  (length (rohsatz) > 0) then begin
            // k�nstliche Meldungsnummer:
            case iArchivNr of
               7: sMNr:=CMeldNr_EichParameterVeraendert;  // f�r Meldung aus eichtechnischem Logbuch EC
               8: sMNr:=CMeldNr_ParameterVeraendert;      // f�r Meldung aus Parameter-Logbuch EC
              10: sMNr:=CMeldNr_ParameterVeraendert_CU;   // f�r Meldung aus Parameter-Logbuch CU
            else
              sMNr:='';
            end;  { case iArchivNr }

            year:=0;
            month:=0;
            day:=0;
            hour:=0;
            min:=0;
            sec:=0;
            Vz:='';
            sParaNrMrg:='';
            sOldValue:='';
            sNewValue:='';
            iOrdNr:=-1;  { Default-Ordnungsnummer; 28.09.2020, WW }

            { rohsatz aufsplitten  }
            satz_ok:=true;                            { Vorbelegung: Datensatz ist ok }
            i:=0;
            while length (rohsatz) > 0 do begin
              inc (i);
              sbuf:=F_Zerlegen (rohsatz, RS);      { lesen bis RS }

              { i = 1: Zeitstempel, Unix-Format }
              if i = 1 then begin
                if UnixTimeStrToDateTime (sbuf, dtDatumZeit) then
                  DecodeDateTime (dtDatumZeit, year, month, day, hour, min, sec, msec)
                else begin
                  satz_ok:=false;
                  DataValid:=false;
                  break;
                end;
              end

              { i = 2: Ordnungsnummer; 28.09.2020, WW }
              else if i = 2 then begin
                if length (sbuf) > 0 then begin
                  Val (sbuf, iBuf, Code);
                  if Code = 0 then
                    iOrdNr:=iBuf
                  else
                    DataValid:=false;
                end;
              end

              else begin
                case iArchivNr of
                  7, 8, 10:  // eichtechnisches LB (EC 900), Parameter-Logbuch (EC 900), Parameter-Logbuch (CU)
                    begin
                      { i = 4: Parameternummer }
                      if i = 4 then
                        // Parameternummer kommt so kurz wie n�tig. 4-stellig
                        // auff�llen mit f�hrenden Nullen !
                        sParaNrMrg:=F_LeftPad (sbuf, '0', CLen_MrgParaNr_RMG_EC)

                      { i = 5: alter Parameterwert }
                      else if i = 5 then
                        sOldValue:=sbuf

                      { i = 6: neuer Parameterwert }
                      else if i = 6 then
                        sNewValue:=sbuf;
                    end;

                  9, 11:  // Ereignis-Logbuch (EC 900), Meldungsarchiv (CU)
                    begin
                      { i = 4: Ereignisnummer, kommt/geht durch GS getrennt }
                      if i = 4 then begin
                        sMNr:=F_Zerlegen (sbuf, GS);
                        Vz:=sbuf; { +/- }
                      end;
                    end;
                end;  { case iArchivNr }
              end;
            end;  { while length (rohsatz) }

            if satz_ok then begin
              case iArchivNr of
                7, 8, 10:  // eichtechnisches LB (EC 900), Parameter-Logbuch (EC 900), Parameter-Logbuch (CU)
                  ParaChange:=TParaChange.Create (sParaNrMrg, '', sOldValue, sNewValue);
              else
                ParaChange:=nil;
              end;

              Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, Vz, year, month, day,
                                                     hour, min, sec, ParaChange);
              if iOrdNr <> -1 then
                Meldung.OrdNr:=iOrdNr;  { Ordnungsnummer zuweisen; 28.09.2020, WW }
                                                     
              Add (Meldung);
            end;
          end;  { if length (rohsatz) > 0) }
        end;  { if STX_gelesen }

        if zeichen = ETX then
          Break;  // nach ETX ist Schluss mit Rohdaten
      end;  { while FS.Position < FSize }
    finally
      FS.Free;
    end;
  except
  end;
end;

{ Konvertieren des aktuellen Fehlerzustandes (Kennziffer F) in Meldungen (Tritschler VC2)
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{---------------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenM (FileName: string; var DataValid: boolean);
{---------------------------------------------------------------------------------}

  {--------------------------------------------------------------------}
  function BuildMrgMeldNr_VC2 (ABitNr: integer; AGeht: boolean): string;
  {--------------------------------------------------------------------}
  { k�nstliche MRG-Meldungsnummer aus Bitnummer des Fehlercodes und
    Kommt/Geht-Info bilden }
  begin
    Result:=Format('%.2d', [ABitNr + 1]);  { Fehlernummer = Bitnummer + 1 }
    if AGeht then
      Result:=Result + '-';  { Minus f�r Geht-Meldung }
  end;

var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  i: integer;
  dtBuf: TDateTime;
  Meldung: TMeldung;
  dtDatumZeit: TDateTime;
  year, month, day: word;
  hour, min, sec, msec: word;
  iFehlercode: cardinal;
  Code: integer;
  Maske: cardinal;
  sMNr: string;
  bGeht: boolean;
  sFehlercode: string;
  sDatum: string;
  sZeit: string;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }
  if not FileExists (FileName) then exit;
  try
    { Rohfile �ffnen: }
    FS:=TFileOfCharStream.Create (Filename, fmOpenRead OR fmShareDenyWrite);
    try
      FSize:=FS.Size;

      rohsatz:='';
      zeichen:=NUL;
      { Rohfileheader bis zum STX lesen }
      while (zeichen <> STX) AND (FS.Position < FSize) do begin
        FS.Read (zeichen);
        rohsatz:=rohsatz + zeichen;                    { Zeile bilden }
      end;

      { Vorbelegungen: Fehlercode, Ger�te-Zeit/Datum noch nicht gelesen }
      sFehlercode:='';
      sDatum:='';
      sZeit:='';

      { Fehlercode, Ger�te-Zeit/Datum lesen: }
      while (zeichen <> ETX) AND (FS.Position < FSize) do begin
        rohsatz:='';
        zeichen:=NUL;
        while (zeichen <> LF) AND (zeichen <> ETX) AND (FS.Position < FSize) do begin
          FS.Read (zeichen);
          rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
        end;

        Application.ProcessMessages;

        if Copy (rohsatz, 1, 2) = 'F(' then  // Fehlercode (Hex)
          sFehlercode:=ExtractString (rohsatz, '(', ')', 0)
        else if Copy (rohsatz, 1, 3) = '28.' then  // Ger�te-Uhrzeit
          sZeit:=ExtractString (rohsatz, '(', ')', 0)
        else if Copy (rohsatz, 1, 3) = '29.' then begin // Ger�te-Datum
          sDatum:=ExtractString (rohsatz, '(', ')', 0);
          Break;
        end;
      end;  { while (zeichen <> ETX) AND (FS.Position < FSize) }

      { Konvertierung: }
      if length (sFehlercode) > 0 then begin
        if EncodeDateStr (sDatum, 'YY-MM-DD', dtBuf) then begin
          dtDatumZeit:=dtBuf;
          if EncodeTimeStr (sZeit, 'HH:MM:SS', dtBuf) then
            dtDatumZeit:=dtDatumZeit + dtBuf
          else
            dtDatumZeit:=0;  // Zeit ung�ltig
        end else
          dtDatumZeit:=0;  // Datum ung�ltig

        if dtDatumZeit > 0 then begin
          Val ('$' + sFehlercode, iFehlercode, Code);
          DecodeDateTime (dtDatumZeit, year, month, day, hour, min, sec, msec);

          Maske:=1;
          for i:=0 to 11 do begin
            Application.ProcessMessages;

            bGeht:=(iFehlercode AND Maske) = 0;   { Bit nicht gesetzt = Meldung geht }
            { k�nstliche ger�tespez. Meldungsnummer: }
            sMNr:=BuildMrgMeldNr_VC2 (i, bGeht);
            Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '', year, month, day, hour, min, sec, nil);
            Add (Meldung);

            Maske:=Maske SHL 1;
          end;  { for i }
        end else
          DataValid:=false;
      end;  { if length (sFehlercode) > 0 }
    finally
      FS.Free;           { Rohfile schlie�en }
    end;
  except
  end;
end;

{ Konvertieren von Rohmeldungen der Gruppe N (Kamstrup UNIGAS 300, IEC1107-Variante)
  -> Es werden die Meldungen des Betriebs- und eichpflichtigen Logbuchs konvertiert.
  -> Das eichpflichtige Logbuch enth�lt nur Parameter�nderungen.
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{---------------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenN (FileName: string; var DataValid: boolean);
{---------------------------------------------------------------------------------}
var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  s: string;
  dtBuf: TDateTime;
  Satz_OK: boolean;
  Meldung: TMeldung;
  year, month, day: word;
  hour, min, sec, msec: word;
  sMNr: string;
  dummy: char;
  iPos: integer;
  S1: string;
  dtDatumZeit: TDateTime;
  iVDEWStatus: cardinal;
  Code: integer;
  ParaChange: TParaChange;
  sKennzahlLB: string;
  sKennzahlWert: string;
  sOldValue: string;
  sNewValue: string;
  sZeitformat: string;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }
  if not FileExists (FileName) then exit;
  try
    { Rohfile �ffnen: }
    FS:=TFileOfCharStream.Create (Filename, fmOpenRead OR fmShareDenyWrite);
    try
      FSize:=FS.Size;

      while FS.Position < FSize do begin
        rohsatz:='';
        zeichen:=NUL;
        { Datens�tze bilden: bis EOT, ETX oder LF lesen }
        while (zeichen <> ETX) AND (zeichen <> EOT) AND (zeichen <> LF) AND
              (FS.Position < FSize) do begin
          FS.Read (zeichen);
          if (zeichen <> CR) AND (zeichen <> LF) AND
             (zeichen <> ETX) AND (zeichen <> EOT) then
            rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne CR, LF, ETX, EOT) }

          if (zeichen = ETX) OR (zeichen = EOT) then begin
            if (FS.Position < FSize) then
              FS.Read (dummy);  { das auf ETX und EOT folgende BCC �berlesen }
          end;
        end;

        Application.ProcessMessages;

        { Falls STX im Rohsatz vorhanden ist, interessiert nur der nachfolgende Datenteil: }
        iPos:=Pos (STX, rohsatz);
        if iPos > 0 then
          rohsatz:=Copy (rohsatz, iPos + 1, length (rohsatz));

        { Logbuch-Eintrag: }
        if length (rohsatz) > 0 then begin
          sKennzahlLB:=ExtractString (rohsatz, NUL, '(', 0);   { Kennzahl des Logbuchs }

          S:=ExtractString (rohsatz, '(', ')', 0);  { Zeitstempel oder ERROR }
          if S = 'ERROR' then Break;  { keine Daten vorhanden }

          Satz_OK:=true;
          dtDatumZeit:=-1;

          { m�gliche Formate des Zeitstempels: ZST10, ZSTs11, ZST12, ZSTs13 }
          if (length (S) = 11) OR (length (S) = 13) then
            S:=Copy (S, 2, length (S));  // Saisonkennzeichen wegschneiden

          S1:=Copy (S, 1, 6);  { Datum }
          if EncodeDateStr (S1, 'YYMMDD', dtBuf) then begin
            dtDatumZeit:=dtBuf;

            S1:=Copy (S, 7, length (S));  { Zeit }
            if length (S1) = 4 then
              sZeitformat:='HHMM'
            else
              sZeitformat:='HHMMSS';
            if EncodeTimeStr (S1, sZeitformat, dtBuf) then
              dtDatumZeit:=dtDatumZeit + dtBuf
            else
              Satz_OK:=false;  { Zeit ung�ltig }
          end else
            Satz_OK:=false;  { Datum ung�ltig }

          if Satz_OK then begin
            DecodeDateTime (dtDatumZeit, year, month, day, hour, min, sec, msec);
            if sKennzahlLB = 'P.98' then begin { Statuslogbuch }
              S:=ExtractString (rohsatz, '(', ')', 1);  { VDEW-Status (Hex) }
              S:=Copy (S, 1, 8);  { max. 32 Bit }
              Val ('$' + S, iVDEWStatus, Code);

              sMNr:=ExtractString (rohsatz, '(', ')', 6);  { Aktueller Status = MRG-Meldungsnummer }
              if (iVDEWStatus AND $0800) > 0 then
                sMNr:=sMNr + '-';  { Geht-Meldung mit Minus kennzeichnen }

              Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '', year, month, day, hour, min, sec, nil);
              Add (Meldung);
            end
            else if sKennzahlLB = 'P.99' then begin  { Eichpflichtiges Logbuch }
              { enth�lt nur Parameter�nderungen: }
              sMNr:=CMeldNr_EichParameterVeraendert;  { k�nstliche ger�tespez. Meldungsnummer }
              sKennzahlWert:=ExtractString (rohsatz, '(', ')', 4);  { Kennzahl des Datenwerts }
              sOldValue:=ExtractString (rohsatz, '(', ')', 18);  { Alter Datenwert }
              sNewValue:=ExtractString (rohsatz, '(', ')', 19);  { Neuer Datenwert }

              ParaChange:=TParaChange.Create (sKennzahlWert, '', sOldValue, sNewValue);
              Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '', year, month, day, hour, min, sec, ParaChange);
              Add (Meldung);
            end;
          end else
            DataValid:=false;
        end;  { if length (rohsatz) > 0 }
      end;  { while FS.Position < FSize }
    finally
      FS.Free;
    end;
  except
  end;
end;

{ Konvertieren von Rohmeldungen der Gruppe O (Modbus)
  -> Primus/Prilog 400 (Statusarchiv)
  -> TME400, RSM200 (Ereignisarchiv, eichamtliches und nicht-eichamtliches Parameterarchiv)
  -> Filename mit Format: <Archivtyp>;<Rohdateiname>
     Archivtyp: at_Parameterarchiv_eichamtlich
                at_Parameterarchiv_nichteichamtlich
                at_Ereignisarchiv
     File mit TRegisterKonvDataArchivRec-Struktur
  �bergabe: Record mit Angaben f�r MRG-Meldungskonvertierung
            Liste mit Parameternummern-Konfiguration (Wenn nil, wird die
              Konfiguration aus der Ressourcendatei gelesen)
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{-----------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenO (FileName: string; MeldKonv: TMeldKonv;
  AParamMrgKonfigList: TParamMrgKonfigList; var DataValid: boolean);
{-----------------------------------------------------------------------------}

  {--------------------------------------------------------------------------------------}
  function BuildMrgMeldNr_MBStatus_PrimusPrilog (ABitNr: integer; AGeht: boolean): string;
  {--------------------------------------------------------------------------------------}
  { Primus/Prilog: K�nstliche MRG-Meldungsnummer aus Bitnummer des Fehlercodes und
    Kommt/Geht-Info bilden }
  begin
    Result:=Format('%.2d', [ABitNr + 1]);  { Fehlernummer = Bitnummer + 1 }
    if AGeht then
      Result:=Result + '-';  { Minus f�r Geht-Meldung }
  end;

  {--------------------------------------------------------------}
  function IdentNrToParamMrgNr_TME400 (iIdentNr: integer): string;
  {--------------------------------------------------------------}
  { TME400: Wandelt IdentNr in MRG-Parameternummer (Koordinate)
    -> auch f�r RSM200 }
  var
    iHighByte: byte;
    iLowByte: byte;
    sSpalte: string;
    sZeile: string;

  begin
    iHighByte:=(iIdentNr AND $FF00) SHR 8;
    if (iHighByte >= $20) AND (iHighByte <= $7F) then
      sSpalte:=Chr (iHighByte)  // High Byte: Spalte (A - Z als ASCII)
    else
      sSpalte:='?';
    iLowByte:=iIdentNr AND $FF;
    sZeile:=Format ('%.2d', [iLowByte]);  // Low Byte: Zeile (Nr. 2-stellig)
    Result:=sSpalte + '-' + sZeile;  // Koordinate, z.B. A-01
  end;

  {----------------------------------------------------------------------------------}
  function KonvParamChangeValueBinToAscii (sValueBin: string; bIdentNr_fehlt: boolean;
    iIdentNr: integer; ParamMrgKonfigList: TParamMrgKonfigList): string;
  {----------------------------------------------------------------------------------}
  { Wandelt einen bin�ren Wert-String 'Parameterwert alt/neu' in lesbares ASCII-
    Format;
    �bergaben: Parameterwert als Bin�r-String
               Flag 'IdentNr. fehlt' ja/nein
               IdentNr.
               Liste mit Parameternummern-Konfiguration
    Ergebnis: Parameterwert (ASCII) }
  var
    sParaNrMrg: string;
    ParamMrgData: TParamMrgData;
    ByteOrder_ChangeVal: TByteOrder;

  begin
    if (ParamMrgKonfigList <> nil) AND not bIdentNr_fehlt then begin
      // IdentNr. in MRG-Parameternummer wandeln, Byte-Order des bin�ren
      // Parameterwert-Strings festlegen:
      case MeldKonv.MrgTyp of
        mrgtyp_RSM200_VCF,
        mrgtyp_RSM200_VMF:  // 09.01.2024, WW
          begin
            // IdentNr enth�lt die Koordinate des ge�nderten Parameters:
            sParaNrMrg:=IdentNrToParamMrgNr_TME400 (iIdentNr);  // Koordinate, z.B. A-01
            ByteOrder_ChangeVal:=bo_LittleEndian;  // RSM200-Byteorder Big-Endian "geswappt"
          end;
      else
        sParaNrMrg:=IntToStr (iIdentNr);  // Default-Wandlung (bisher nicht verwendet)
        ByteOrder_ChangeVal:=bo_LittleEndian;  // Default: Big-Endian "geswappt"
      end;  // case MeldKonv.MrgTyp

      // Modbus-Werttyp des Parameters aus Parameternummern-
      // Konfigurationsliste lesen:
      if ParamMrgKonfigList.FindParamMrgData (MeldKonv.Parametergruppe,
                                              sParaNrMrg,
                                              ParamMrgData) then begin
        Result:=Modbus_BinData2Str (sValueBin, ByteOrder_ChangeVal, ParamMrgData.ParaDatentyp);
        // Parameterwert-String formatieren:
        if ParamMrgData.AusgabeFormat <> '' then
          WFormatString (Result, ParamMrgData.AusgabeFormat);
      end else
        Result:='???';  // Bin�rwert kann nicht typabh�ngig interpretiert werden
    end else
      Result:='???';  // Bin�rwert kann nicht typabh�ngig interpretiert werden
  end;

var
  FS: TFileOfRecStream;
  iRecPos: integer;
  RegisterKonvDataArchivRec: TRegisterKonvDataArchivRec;
  RegisterKonvData: TRegisterKonvData;
  dtDatumZeit: TDateTime;
  iGeraeteStatus: Int64;
  GeraeteStatus_fehlt: boolean;
  satz_OK: boolean;
  sMB_KanalDef: string;
  i: integer;
  Meldung: TMeldung;
  year, month, day: word;
  hour, min, sec, msec: word;
  Maske: Int64;
  sMNr: string;
  bGeht: boolean;
  iOrdNr: longint;
  IdentNr_fehlt: boolean;
  iIdentNr: integer;
  TypNr_fehlt: boolean;
  iTypNr: integer;
  sParaNrMrg: string;
  sOldValue: string;
  OldValue_fehlt: boolean;
  sNewValue: string;
  NewValue_fehlt: boolean;
  ParaChange: TParaChange;
  sArchivtyp: string;
  Archivtyp: TArchivtyp;
  ParamMrgKonfigList: TParamMrgKonfigList;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }

  { Wenn dem Rohfilenamen durch Strichpunkt getrennt eine Archivtyp-Nummer
    vorangestellt ist: Archivtyp, zu dem die Meldungen geh�ren; 18.02.2021, WW }
  if Pos (';', Filename) > 0 then begin
    sArchivtyp:=F_Zerlegen (Filename, ';');
    i:=StrToIntDef (sArchivtyp, -1);

    case i of
      integer (at_Parameterarchiv_eichamtlich):
        Archivtyp:=at_Parameterarchiv_eichamtlich;

      integer (at_Parameterarchiv_nichteichamtlich):
        Archivtyp:=at_Parameterarchiv_nichteichamtlich;

      integer (at_Ereignisarchiv):
        Archivtyp:=at_Ereignisarchiv;
    else
      exit;
    end;
  end else
    Archivtyp:=at_Ereignisarchiv;

  if not FileExists (FileName) then exit;
  try
    { Parameternummern-Konfigurationsliste, 09.01.2024, WW }
    if not Assigned (AParamMrgKonfigList) then
      ParamMrgKonfigList:=TParamMrgKonfigList.Create  // Parameternummern-Konfigurationsliste lokal anlegen
    else
      ParamMrgKonfigList:=AParamMrgKonfigList;  // �bergebene Parameternummern-Konfigurationsliste wird verwendet
    try
      { Parameternummern-Konfigurationsliste aus Resourcendatei laden, wenn lokal angelegt: }
      if not Assigned (AParamMrgKonfigList) then
        GetParamMrg_KonfigList_ByParaGruppe (MeldKonv.ParameterGruppe,
                                             MeldKonv.ParameterUnterGruppe,
                                             ParamMrgKonfigList, KonfigPfad);

      { Quellfile �ffnen: }
      FS:=TFileOfRecStream.Create (FileName, fmOpenRead OR fmShareDenyWrite,
                                   SizeOf (TRegisterKonvDataArchivRec));
      try
        { Quelldatei r�ckw�rts lesen (Datens�tze sind chronologisch absteigend
          enthalten, j�ngster zuerst): }
        for iRecPos:=FS.RecCount - 1 downto 0 do begin
          Application.ProcessMessages;
          FS.SeekRec (iRecPos, soFromBeginning);
          FS.ReadRec (RegisterKonvDataArchivRec);

          satz_OK:=true;                    { Vorbelegung: Datensatz ist ok }
          dtDatumZeit:=0;                   { Vorbelegung: Zeitstempel fehlt }
          GeraeteStatus_fehlt:=true;        { Vorbelegung: Ger�testatus fehlt }
          iGeraeteStatus:=0;
          IdentNr_fehlt:=true;              { Vorbelegung: IdentNr fehlt }
          iIdentNr:=0;
          TypNr_fehlt:=true;                { Vorbelegung: TypNr fehlt }
          iTypNr:=0;
          iOrdNr:=-1;  { Default-Ordnungsnummer; 18.02.2021, WW }
          sOldValue:='';
          OldValue_fehlt:=true;                { Vorbelegung: Alter Wert fehlt }
          sNewValue:='';
          NewValue_fehlt:=true;                { Vorbelegung: Neuer Wert fehlt }

          for i:=Low (RegisterKonvDataArchivRec) to High (RegisterKonvDataArchivRec) do begin
            RegisterKonvData:=RegisterKonvDataArchivRec [i];

            if RegisterKonvData.AnzahlBytes > -1 then begin  // nur belegte Datens�tze; 18.02.2021, WW
              // Modbus-Kanaldefinition zu der Startadresse des Werts des
              // Meldungsarchiv-Datensatzes ermitteln:
              sMB_KanalDef:=GetKanalDef_MBRegister_Archiv (MeldKonv.MrgTyp, Archivtyp,
                                                           RegisterKonvData.StartAdresse,
                                                           MeldKonv.MBAbrufData);  // 11.02.2022, WW
              if sMB_KanalDef = C_MBKanalDef_DZ then begin
                { Zeitstempel }
                try
                  dtDatumZeit:=StrToDateTime (RegisterKonvData.Wert);
                  { Anm.: Die Kodierung in der Modbus-Konvertierung erfolgt mittels DateTimeToStr }
                except
                  satz_OK:=false;
                  Break;
                end;
              end

              else if sMB_KanalDef = C_MBKanalDef_SatzStatus then begin
                if length (RegisterKonvData.Wert) > 0 then begin
                  { Ger�testatus }
                  iGeraeteStatus:=StrToInt64 (RegisterKonvData.Wert);  { Status des Ger�ts (Int64) wandeln }
                  GeraeteStatus_fehlt:=false;
                end
                else begin
                  satz_OK:=false;
                  Break;
                end;
              end

              else if sMB_KanalDef = C_MBKanalDef_ONr then begin  // 18.02.2021, WW
                { Ordnungsnummer }
                iOrdNr:=StrToIntDef (RegisterKonvData.Wert, -1);
              end

              else if sMB_KanalDef = C_MBKanalDef_IdentNr then begin  // 18.02.2021, WW
                if length (RegisterKonvData.Wert) > 0 then begin
                  { IdentNr }
                  iIdentNr:=StrToInt (RegisterKonvData.Wert);  { Meldungsnummer }
                  IdentNr_fehlt:=false;
                end
                else begin
                  satz_OK:=false;
                  Break;
                end;
              end

              else if sMB_KanalDef = C_MBKanalDef_TypNr then begin  // 18.02.2021, WW
                if length (RegisterKonvData.Wert) > 0 then begin
                  { TypNr }
                  iTypNr:=StrToInt (RegisterKonvData.Wert);  { Meldungstyp-Nummer }
                  TypNr_fehlt:=false;
                end
                else begin
                  satz_OK:=false;
                  Break;
                end;
              end

              else if sMB_KanalDef = C_MBKanalDef_WertAlt then begin  // 18.02.2021, WW
                sOldValue:=RegisterKonvData.Wert;  { Alter Parameterwert }
                OldValue_fehlt:=false;

                case MeldKonv.MrgTyp of
                  mrgtyp_RSM200_VCF,
                  mrgtyp_RSM200_VMF:  // 09.01.2024, WW
                    begin
                      sOldValue:=KonvParamChangeValueBinToAscii (sOldValue,
                        IdentNr_fehlt, iIdentNr, ParamMrgKonfigList);
                    end;
                end;
              end

              else if sMB_KanalDef = C_MBKanalDef_WertNeu then begin  // 18.02.2021, WW
                sNewValue:=RegisterKonvData.Wert;  { Neuer Parameterwert }
                NewValue_fehlt:=false;

                case MeldKonv.MrgTyp of
                  mrgtyp_RSM200_VCF,
                  mrgtyp_RSM200_VMF:  // 09.01.2024, WW
                    begin
                      sNewValue:=KonvParamChangeValueBinToAscii (sNewValue,
                        IdentNr_fehlt, iIdentNr, ParamMrgKonfigList);
                    end;
                end;
              end;
            end;
          end;  { for i }

          if satz_OK AND (dtDatumZeit > 0) then begin
            DecodeDateTime (dtDatumZeit, year, month, day, hour, min, sec, msec);

            case MeldKonv.MrgTyp of
              mrgtyp_Primus,
              mrgtyp_Prilog:
                begin
                  if (not GeraeteStatus_fehlt) then begin
                    Maske:=1;
                    for i:=0 to 63 do begin
                      Application.ProcessMessages;

                      bGeht:=(iGeraeteStatus AND Maske) = 0;   { Bit nicht gesetzt = Meldung geht }
                      { k�nstliche ger�tespez. Meldungsnummer: }
                      sMNr:=BuildMrgMeldNr_MBStatus_PrimusPrilog (i, bGeht);
                      Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '', year, month, day, hour, min, sec, nil);
                      Add (Meldung);

                      Maske:=Maske SHL 1;
                    end;  { for i }
                  end else
                    DataValid:=false;
                end;

              mrgtyp_TME400_VCF,
              mrgtyp_TME400_VMF,  // 18.02.2021, WW
              mrgtyp_RSM200_VCF,
              mrgtyp_RSM200_VMF:  // 09.01.2024, WW
                begin
                  case Archivtyp of
                    at_Parameterarchiv_eichamtlich,
                    at_Parameterarchiv_nichteichamtlich:
                      begin
                        if (not IdentNr_fehlt) AND
                           (not OldValue_fehlt) AND (not NewValue_fehlt) then begin
                          { iIdentNr enth�lt die Koordinate des ge�nderten Parameters: }
                          sParaNrMrg:=IdentNrToParamMrgNr_TME400 (iIdentNr);  // Koordinate, z.B. A-01
                          ParaChange:=TParaChange.Create (sParaNrMrg, '', sOldValue, sNewValue);

                          // K�nstliche Meldungsnummer:
                          if Archivtyp = at_Parameterarchiv_eichamtlich then
                            sMNr:=CMeldNr_EichParameterVeraendert
                          else
                            sMNr:=CMeldNr_ParameterVeraendert;

                          Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '', year, month, day, hour, min, sec, ParaChange);
                          if iOrdNr <> -1 then
                            Meldung.OrdNr:=iOrdNr;

                          Add (Meldung);
                        end else
                          DataValid:=false;
                      end;

                    at_Ereignisarchiv:
                      begin
                        if (not IdentNr_fehlt) AND (not TypNr_fehlt) then begin
                          { iIdentNr enth�lt die Ereignisnummer, iTypNr den Ereignistyp: }
                          sMNr:=IntToStr (iIdentNr);  // Meldungsnummer
                          if (iTypNr AND $FF) = 0 then  // Low Byte: 0 = Geht, 1 = Kommt
                            sMNr:=sMNr + '-';  // Meldungsnummer f�r Geht-Meldung

                          Meldung:=TMeldung.CreateMrgNr4stellig (sMNr, '', year, month, day, hour, min, sec, nil);
                          if iOrdNr <> -1 then
                            Meldung.OrdNr:=iOrdNr;

                          Add (Meldung);
                        end else
                          DataValid:=false;
                      end;
                  end;
                end;
            end;  // case MrgTyp
          end else
            DataValid:=false;
        end;
      finally
        FS.Free;
      end;
    finally
      // Parameternummern-Konfigurationsliste nur freigeben, wenn lokal angelegt
      if not Assigned (AParamMrgKonfigList) then
        ParamMrgKonfigList.Free;
    end;
  except
  end;
end;

{ Konvertieren von Rohmeldungen der Gruppe P (SICK FLOWSIC500)
  -> Ereignislogbuch, Parameterlogbuch, eichamtliches Logbuch, Gasparameterlogbuch
  -> von der Konvertierung erwarteter Aufbau der Rohdaten:
     alle abgerufenen Downloadpuffer-Daten eines Logbuchs aneinandergereiht (ohne
     1. Byte (Anzahl der Datens�tze) und ohne F�llbytes), Reihenfolge der Datens�tze
     chronologisch absteigend
  -> Filename mit Format: <Datensatz-Typnummer>;<Rohdateiname>
  �bergabe: Record mit Angaben f�r MRG-Meldungskonvertierung
            Liste mit Parameternummern-Konfiguration (Wenn nil, wird die
              Konfiguration aus der Ressourcendatei gelesen)
  R�ckgabe: DataValid -> false, wenn Rohdatenstruktur ung�ltig }
{-----------------------------------------------------------------------------}
procedure TMeldungsListe.KonvMeldungenP (FileName: string; MeldKonv: TMeldKonv;
  AParamMrgKonfigList: TParamMrgKonfigList; var DataValid: boolean);
{-----------------------------------------------------------------------------}
const
  C_ByteOrder_Swapped = bo_LittleEndian;  // FLOWSIC500-Byteorder Big-Endian "geswappt"

type
  { Datensatz-Struktur der Logbucharchive, Typ 4 }
  TLogbuchRec_Typ4 = packed record
    RecordAddr: word;
    RecordID: longword;
    Timestamp: longword;
    EventType: byte;
    RecordStatus: byte;
    RecordData: array [0..13] of char;
    CounterValue: longword;
    UnitCounterID: byte;
    CounterResolution: shortint;
    ChecksumStatus: word;
  end;

var
  FS: TFileOfRecStream;
  iRecPos: integer;
  dtDatumZeit: TDateTime;
  dtDatumZeitUTC: TDateTime;
  satz_OK: boolean;
  Meldung: TMeldung;
  year, month, day: word;
  hour, min, sec, msec: word;
  sParaNrMrg: string;
  sOldValue: string;
  sNewValue: string;
  ParaChange: TParaChange;
  LogbuchRec_Typ4: TLogbuchRec_Typ4;
  iRecTyp: word;
  sRecTyp: string;
  iEventID: byte;
  sEventID: string;
  sVz: string;
  sBinData: string;
  ParamMrgKonfigList: TParamMrgKonfigList;
  ParamMrgData: TParamMrgData;
  iCounterID: byte;
  iCntResolution: shortint;
  dValue: double;
  sParaNrAllg: string;
  sRecordData: string;
  iZeitzone_Minuten: integer;
  iBuf: integer;

begin
  DataValid:=true;   { Vorbelegung f�r R�ckgabe }

  sRecTyp:=F_Zerlegen (FileName, ';');  { Datensatz-Typnummer bis zum Strichpunkt }
  iRecTyp:=StrToInt (sRecTyp);
  if iRecTyp <> 4 then begin
    DataValid:=false;
    exit;
  end;

  if not FileExists (FileName) then exit;
  try
    // Aktuelle Zeitzone des Ger�ts aus Parameterliste lesen:
    // -> Zur Umrechnung der UTC-Zeitstempel der Datens�tze in lokale Zeit
    iZeitzone_Minuten:=0;  // Default
    if MeldKonv.ParameterListe <> nil then                                              
      if MeldKonv.ParameterListe.GetValueInt (CP_SICK_FLOWSIC500_Zeitzone_Minuten, iBuf) then
        iZeitzone_Minuten:=iBuf;

    { Parameternummern-Konfigurationsliste: }
    if not Assigned (AParamMrgKonfigList) then
      ParamMrgKonfigList:=TParamMrgKonfigList.Create  // Parameternummern-Konfigurationsliste lokal anlegen
    else
      ParamMrgKonfigList:=AParamMrgKonfigList;  // �bergebene Parameternummern-Konfigurationsliste wird verwendet
    try
      { Parameternummern-Konfigurationsliste aus Resourcendatei laden, wenn lokal angelegt: }
      if not Assigned (AParamMrgKonfigList) then
        GetParamMrg_KonfigList_ByParaGruppe (MeldKonv.ParameterGruppe,
                                             MeldKonv.ParameterUnterGruppe,  // 11.02.2022, WW
                                             ParamMrgKonfigList, KonfigPfad);

      { Quellfile �ffnen: }
      FS:=TFileOfRecStream.Create (FileName, fmOpenRead OR fmShareDenyWrite,
                                   SizeOf (TLogbuchRec_Typ4));
      try
        { Quelldatei r�ckw�rts lesen (Datens�tze sind chronologisch absteigend
          enthalten, j�ngster zuerst): }
        for iRecPos:=FS.RecCount - 1 downto 0 do begin
          Application.ProcessMessages;
          FS.SeekRec (iRecPos, soFromBeginning);
          FS.ReadRec (LogbuchRec_Typ4);

          { ChecksumStatus (0 = CRC error, 1 = CRC valid) }
          satz_OK:=LogbuchRec_Typ4.ChecksumStatus = 1;

          if satz_OK then begin
            { Zeitstempel }
            UnixSekundenToDateTime (LogbuchRec_Typ4.Timestamp, dtDatumZeitUTC);  // liegt in UTC vor
            // Zeitstempel der Meldung: In lokale Zeit umrechnen (Zeitzone, DST)
            dtDatumZeit:=dtDatumZeitUTC;  // UTC
            if (LogbuchRec_Typ4.RecordStatus AND $80) <> 0 then  { Bit 7: DST active at the record generation time }
              dtDatumZeit:=IncHour (dtDatumZeit); // DST
            { Im Gegensatz zu Datens�tzen des Messperiodenarchivs gibt es hier
              kein "Local time"-Bit zur Auswertung, ob die Zeitzone einzurechnen
              ist oder nicht -> Daher immer einrechnen }
            dtDatumZeit:=IncMinute (dtDatumZeit, iZeitzone_Minuten);  // Zeitzonen-Minuten

            DecodeDateTime (dtDatumZeit, year, month, day, hour, min, sec, msec);

            { Event type }
            iEventID:=LogbuchRec_Typ4.EventType AND $7F;  // Bits 0..6: Event-ID
            sEventID:=IntToStr (iEventID);
            if (LogbuchRec_Typ4.EventType AND $80) = 0 then  // Bit 7: 0 = Geht, 1 = Kommt
              sVz:='-'  // K�nstliche negative Event-ID f�r Geht-Meldung
            else
              sVz:='';

            { Record data, abh�ngig von Event-ID }
            sRecordData:=BufToString (LogbuchRec_Typ4.RecordData,
                                      SizeOf (LogbuchRec_Typ4.RecordData));

            { Event-ID's, welche mit einer Parameter�nderung verbunden sind }
            case iEventID of
              19: begin  // Datum/Uhrzeit gesetzt
                    // UTC-Datum/Uhrzeit vor der �nderung:
                    sBinData:=Copy (sRecordData, 1, 4);
                    sOldValue:=Modbus_BinData2Str (sBinData, C_ByteOrder_Swapped, C_MBWertTyp_U);
                    // Als neue Zeit wird der UTC-Zeitstempel der Meldung genommen
                    sNewValue:=DateTimeToStr (dtDatumZeitUTC);

                    // allgemeine Parameternummer 'Zeitstempel UTC':
                    sParaNrAllg:=CP_SICK_FLOWSIC500_Zeitstempel_UTC;

                    // MRG-spezifische Parameternummer des Parameters aus
                    // Parameternummern-Konfigurationsliste lesen:
                    sParaNrMrg:='';
                    if ParamMrgKonfigList <> nil then begin
                      if ParamMrgKonfigList.FindParamMrgData_ByAllgNr (MeldKonv.Parametergruppe,
                                                                       sParaNrAllg,
                                                                       ParamMrgData) then
                        sParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;
                    end;

                    ParaChange:=TParaChange.Create (sParaNrMrg, '', sOldValue, sNewValue);
                  end;

              23,        // Parameter ge�ndert
              24,        // Metrologischen Parameter ge�ndert
              58,        // Z�hler getauscht
              61: begin  // Gasparameter ge�ndert                               
                    // Parameternummer (Modbus-Registeradresse):
                    sBinData:=Copy (sRecordData, 1, 2);
                    sParaNrMrg:=IntToStr (Bin2Word (sBinData, C_ByteOrder_Swapped));

                    sOldValue:='';
                    sNewValue:='';
                    if ParamMrgKonfigList <> nil then begin
                      // Modbus-Werttyp des Parameters aus Parameternummern-
                      // Konfigurationsliste lesen:
                      if ParamMrgKonfigList.FindParamMrgData (MeldKonv.Parametergruppe,
                                                              sParaNrMrg,
                                                              ParamMrgData) then begin
                        // Alter Wert:
                        sBinData:=Copy (sRecordData, 3, 4);
                        if ParamMrgData.ParaDatentyp = C_MBWertTyp_Nle then begin  // "Little-Endian-String, 0-terminiert"
                          sOldValue:=Modbus_BinData2Str (sBinData, C_ByteOrder_Swapped,
                                                         C_MBWertTyp_N);
                          if ParamMrgData.ParaByteLen > 4 then
                            sOldValue:=sOldValue + '[...]';  // wie in SICK FLOWgate-Software...
                        end else
                          sOldValue:=Modbus_BinData2Str (sBinData, C_ByteOrder_Swapped,
                                                         ParamMrgData.ParaDatentyp);
                        // Neuer Wert:
                        sBinData:=Copy (sRecordData, 7, 4);
                        if ParamMrgData.ParaDatentyp = C_MBWertTyp_Nle then begin  // "Little-Endian-String, 0-terminiert"
                          sNewValue:=Modbus_BinData2Str (sBinData, C_ByteOrder_Swapped,
                                                         C_MBWertTyp_N);
                          if ParamMrgData.ParaByteLen > 4 then
                            sNewValue:=sNewValue + '[...]';  // wie in SICK FLOWgate-Software...
                        end else
                          sNewValue:=Modbus_BinData2Str (sBinData, C_ByteOrder_Swapped,
                                                         ParamMrgData.ParaDatentyp);
                        // Parameterwert-Strings formatieren:
                        if ParamMrgData.AusgabeFormat <> '' then begin
                          WFormatString (sOldValue, ParamMrgData.AusgabeFormat);
                          WFormatString (sNewValue, ParamMrgData.AusgabeFormat);
                        end;
                      end;
                    end;

                    ParaChange:=TParaChange.Create (sParaNrMrg, '', sOldValue, sNewValue);
                  end;

              25: begin  // Firmware ge�ndert
                    // Firmware-Version vorher:
                    sBinData:=Copy (sRecordData, 1, 2);
                    sOldValue:=IntToStr (Bin2Word (sBinData));
                    // Version mit Punkt versehen (z.B. 21601 -> 2.16.01)
                    System.Insert('.', sOldValue, length(sOldValue) - 1);
                    System.Insert('.', sOldValue, length(sOldValue) - 4);
                    // Firmware-Build vorher:
                    sBinData:=Copy (sRecordData, 3, 2);
                    sOldValue:=sOldValue + '.' + IntToStr (Bin2Word (sBinData));
                    // Firmware-CRC vorher:
                    sBinData:=Copy (sRecordData, 5, 2);
                    sOldValue:=sOldValue + ' CRC=' + IntToHex (Bin2Word (sBinData), 4);

                    // Firmware-Version neu:
                    sBinData:=Copy (sRecordData, 7, 2);
                    sNewValue:=IntToStr (Bin2Word (sBinData));
                    // Version mit Punkt versehen (z.B. 21601 -> 2.16.01)
                    System.Insert('.', sNewValue, length(sNewValue) - 1);
                    System.Insert('.', sNewValue, length(sNewValue) - 4);
                    // Firmware-Build neu:
                    sBinData:=Copy (sRecordData, 9, 2);
                    sNewValue:=sNewValue + '.' + IntToStr (Bin2Word (sBinData));
                    // Firmware-CRC neu:
                    sBinData:=Copy (sRecordData, 11, 2);
                    sNewValue:=sNewValue + ' CRC=' + IntToHex (Bin2Word (sBinData), 4);

                    // allgemeine Parameternummer 'Firmware-Version':
                    sParaNrAllg:=CP_SICK_FLOWSIC500_VersionFW;

                    // MRG-spezifische Parameternummer und Ausgabeformat des
                    // Parameters aus Parameternummern-Konfigurationsliste lesen:
                    sParaNrMrg:='';
                    if ParamMrgKonfigList <> nil then begin
                      if ParamMrgKonfigList.FindParamMrgData_ByAllgNr (MeldKonv.Parametergruppe,
                                                                       sParaNrAllg,
                                                                       ParamMrgData) then begin
                        sParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;
                      end;
                    end;

                    ParaChange:=TParaChange.Create (sParaNrMrg, '', sOldValue, sNewValue);
                  end;

              57: begin  // Z�hlwerk gesetzt
                    // ID des Z�hlers:
                    sBinData:=Copy (sRecordData, 1, 1);
                    iCounterID:=Bin2Byte (sBinData);
                    // Aufl�sung des Z�hlers (Faktor):
                    sBinData:=Copy (sRecordData, 2, 1);
                    iCntResolution:=Bin2Shortint (sBinData);
                    // Alter Z�hlerwert:
                    sBinData:=Copy (sRecordData, 3, 4);
                    dValue:=Bin2LongWord (sBinData, C_ByteOrder_Swapped);
                    dValue:=dValue * exp(iCntResolution * ln(10));  // mit Faktor verrechnen, 10 hoch Exponent
                    sOldValue:=FloatToStr (dValue);
                    // Neuer Z�hlerwert:
                    sBinData:=Copy (sRecordData, 7, 4);
                    dValue:=Bin2LongWord (sBinData, C_ByteOrder_Swapped);
                    dValue:=dValue * exp(iCntResolution * ln(10));  // mit Faktor verrechnen, 10 hoch Exponent
                    sNewValue:=FloatToStr (dValue);

                    // Allgemeine Parameternummer des Z�hlers:
                    case iCounterID of
                      0: sParaNrAllg:=CP_SICK_FLOWSIC500_Vm;
                      1: sParaNrAllg:=CP_SICK_FLOWSIC500_VmErr;
                      2: sParaNrAllg:=CP_SICK_FLOWSIC500_Vb;
                      3: sParaNrAllg:=CP_SICK_FLOWSIC500_VbErr;
                      4: sParaNrAllg:=CP_SICK_FLOWSIC500_VbTot;
                    else
                      sParaNrAllg:='';
                    end;

                    // MRG-spezifische Parameternummer und Ausgabeformat des
                    // Parameters aus Parameternummern-Konfigurationsliste lesen:
                    sParaNrMrg:='';
                    if ParamMrgKonfigList <> nil then begin
                      if ParamMrgKonfigList.FindParamMrgData_ByAllgNr (MeldKonv.Parametergruppe,
                                                                       sParaNrAllg,
                                                                       ParamMrgData) then begin
                        sParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;
                        // Z�hlerwert-Strings formatieren:
                        if ParamMrgData.AusgabeFormat <> '' then begin
                          WFormatString (sOldValue, ParamMrgData.AusgabeFormat);
                          WFormatString (sNewValue, ParamMrgData.AusgabeFormat);
                        end;
                      end;
                    end;

                    ParaChange:=TParaChange.Create (sParaNrMrg, '', sOldValue, sNewValue);
                  end;

              64: begin  // Verifikationsstatus ge�ndert
                    // Alter Statuswert:
                    sBinData:=Copy (sRecordData, 1, 2);
                    sOldValue:=IntToStr (Bin2Word (sBinData, C_ByteOrder_Swapped));
                    // Neuer Statuswert:
                    sBinData:=Copy (sRecordData, 3, 2);
                    sNewValue:=IntToStr (Bin2Word (sBinData, C_ByteOrder_Swapped));

                    // allgemeine Parameternummer 'Eichrelevanter Status':
                    sParaNrAllg:=CP_SICK_FLOWSIC500_EichStatus;

                    // MRG-spezifische Parameternummer und Ausgabeformat des
                    // Parameters aus Parameternummern-Konfigurationsliste lesen:
                    sParaNrMrg:='';
                    if ParamMrgKonfigList <> nil then begin
                      if ParamMrgKonfigList.FindParamMrgData_ByAllgNr (MeldKonv.Parametergruppe,
                                                                       sParaNrAllg,
                                                                       ParamMrgData) then begin
                        sParaNrMrg:=ParamMrgData.Parameternummer_im_MRG;
                        // Statuswert-Strings formatieren:
                        if ParamMrgData.AusgabeFormat <> '' then begin
                          WFormatString (sOldValue, ParamMrgData.AusgabeFormat);
                          WFormatString (sNewValue, ParamMrgData.AusgabeFormat);
                        end;
                      end;
                    end;

                    ParaChange:=TParaChange.Create (sParaNrMrg, '', sOldValue, sNewValue);
                  end;
            else
              ParaChange:=nil;
            end;

            Meldung:=TMeldung.CreateMrgNr4stellig (sEventID, sVz, year, month, day,
                                                   hour, min, sec, ParaChange);
            Meldung.OrdNr:=LogbuchRec_Typ4.RecordID;  // Ordnungsnummer
            Meldung.IsAdrMrg:=true;  // Flag setzen: Meldung mit MRG-Meldungsadresse (Event-ID)

            Add (Meldung);
          end else
            DataValid:=false;
        end;
      finally
        FS.Free;
      end;
    finally
      // Parameternummern-Konfigurationsliste nur freigeben, wenn lokal angelegt
      if not Assigned (AParamMrgKonfigList) then
        ParamMrgKonfigList.Free;
    end;
  except
  end;
end;

{ GetMeldungA : erzeugt Meldungsobjekt f�r einen Rohtext der Gruppe A
  Parameter :
    szRoh : Rohtext;
    PSNr : Meldenummer, welche eine Parameter�nderung kennzeichnet
  R�ckgabe :
    nil, wenn Rohtext fehlerhaft, sonst Zeiger auf Meldungsobjekt }
{------------------------------------------------------------------}
Function TMeldungsListe.GetMeldungA (szRoh, PSNr: string): TMeldung;
{------------------------------------------------------------------}
Var
  szMNr : string [3];
  szTemp : string [2];
  Jahr, Monat, Tag, Stunde, Minute, Sekunde: Word;
  Code : Integer;
  ParaChange : TParaChange;
  Valid: boolean;
  S: string;
Begin
  Result := Nil;
  szMNr:=Copy(szRoh, 1, 3);          { MRG-Meldenummer }
  szTemp:=Copy(szRoh, 4, 2);
  Val (szTemp, Jahr, Code);          { Jahr }
  Valid := Code = 0;
  if Valid then begin
    if Jahr < 80 then
      Jahr := Jahr + 2000
    else
      Jahr := Jahr + 1900;
  end;
  szTemp:=Copy(szRoh, 6, 2);
  Val (szTemp, Monat, Code);         { Monat }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 8, 2);
  Val (szTemp, Tag, Code);           { Tag }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 10, 2);
  Val (szTemp, Stunde, Code);        { Stunde }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 12, 2);
  Val (szTemp, Minute, Code);        { Minute }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 14, 2);
  Val (szTemp, Sekunde, Code);       { Sekunde }
  Valid := Valid and (Code = 0);

  if Valid then begin                { �berpr�fung auf plausibles Datum, Zeit }
    try
      EncodeDate (Jahr, Monat, Tag);
      EncodeTime (Stunde, Minute, Sekunde, 0);
    except
      Valid:=false;
    end;
  end;

  if Valid then begin
    ParaChange := nil;
    if szMNr = PSNr then begin
      S:=Copy(szRoh, 16, length(szRoh));
      ParaChange := TParaChange.CreateFromRohString (S);
    end;
    Result := TMeldung.Create (szMNr, Jahr, Monat, Tag, Stunde, Minute, Sekunde, ParaChange);
  end;
end;

{ GetMeldungB : erzeugt Meldungsobjekt f�r einen Rohtext der Gruppe B
  Parameter :
    szRoh : Rohtext;
  R�ckgabe :
    nil, wenn Rohtext fehlerhaft, sonst Zeiger auf Meldungsobjekt }
{------------------------------------------------------------}
function TMeldungsListe.GetMeldungB (szRoh: string): TMeldung;
{------------------------------------------------------------}
var
  szMNr : string [3];
  szTemp : string [2];
  Jahr, Monat, Tag, Stunde, Minute, Sekunde: Word;
  Code : Integer;
  Valid : Boolean;
begin
  Result := Nil;
  szMNr:=Copy(szRoh, 1, 3);        { MRG-Meldenummer }
  szTemp:=Copy(szRoh, 4, 2);
  Val (szTemp, Jahr, Code);          { Jahr }
  Valid := Code = 0;
  if Valid then begin
    if Jahr < 80 then
      Jahr := Jahr + 2000
    else
      Jahr := Jahr + 1900;
  end;
  szTemp := Copy(szRoh, 6, 2);
  Val (szTemp, Monat, Code);         { Monat }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 8, 2);
  Val (szTemp, Tag, Code);           { Tag }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 10, 2);
  Val (szTemp, Stunde, Code);        { Stunde }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 12, 2);
  Val (szTemp, Minute, Code);        { Minute }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 14, 2);
  Val (szTemp, Sekunde, Code);       { Sekunde }
  Valid := Valid and (Code = 0);

  if Valid then begin                { �berpr�fung auf plausibles Datum, Zeit }
    try
      EncodeDate (Jahr, Monat, Tag);
      EncodeTime (Stunde, Minute, Sekunde, 0);
    except
      Valid:=false;
    end;
  end;

  if Valid then
    Result := TMeldung.Create (szMNr, Jahr, Monat, Tag, Stunde, Minute, Sekunde, nil);
end;

{ GetMeldungC : erzeugt Meldungsobjekt f�r einen Rohtext der Gruppe C
  Parameter :
    szRoh : Rohtext;
  R�ckgabe :
    nil, wenn Rohtext fehlerhaft, sonst Zeiger auf Meldungsobjekt }
{------------------------------------------------------------}
function TMeldungsListe.GetMeldungC (szRoh: string): TMeldung;
{------------------------------------------------------------}
var
  szMNr : string [3];
  szTemp : string [2];
  Jahr, Monat, Tag, Stunde, Minute: Word;
  Code : Integer;
  Valid : Boolean;
begin
  Result := Nil;
  case szRoh [11] of
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9': szMNr:='00' + szRoh [11];
    'A' : szMNr:='010';
    'B' : szMNr:='011';
    'C' : szMNr:='012';
    'D' : szMNr:='013';
    'E' : szMNr:='014';
    'F' : szMNr:='015';
  else
    exit;
  end;
  szTemp:=Copy(szRoh, 5, 2);
  Val (szTemp, Jahr, Code);          { Jahr }
  Valid := Code = 0;
  if Valid then begin
    if Jahr < 80 then
      Jahr := Jahr + 2000
    else
      Jahr := Jahr + 1900;
  end;
  szTemp:=Copy(szRoh, 3, 2);
  Val (szTemp, Monat, Code);         { Monat }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 1, 2);
  Val (szTemp, Tag, Code);           { Tag }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 7, 2);
  Val (szTemp, Stunde, Code);        { Stunde }
  Valid := Valid and (Code = 0);
  szTemp:=Copy(szRoh, 9, 2);
  Val (szTemp, Minute, Code);        { Minute }
  Valid := Valid and (Code = 0);

  if Valid then begin                { �berpr�fung auf plausibles Datum, Zeit }
    try
      EncodeDate (Jahr, Monat, Tag);
      EncodeTime (Stunde, Minute, 0, 0);
    except
      Valid:=false;
    end;
  end;

  if Valid then
    Result := TMeldung.Create (szMNr, Jahr, Monat, Tag, Stunde, Minute, 0, nil);
end;

{ GetMeldungD : erzeugt Meldungsobjekt f�r einen Rohtext der Gruppe D
  Parameter :
    szRoh : Rohtext;
  R�ckgabe :
    nil, wenn Rohtext fehlerhaft, sonst Zeiger auf Meldungsobjekt }
{------------------------------------------------------------}
function TMeldungsListe.GetMeldungD (szRoh: string): TMeldung;
{------------------------------------------------------------}
var
  szMNr : string [3];
  szTemp : string [5];
  Jahr, Monat, Tag, Stunde, Minute: Word;
  Code : Integer;
  Valid : Boolean;
  LValue: Word;
  MNr: Integer;
begin
  Result := Nil;
  szTemp := '$' + copy (szRoh, 1, 4);
  val (szTemp, LValue, Code);
  Valid := Code = 0;
  if Valid then begin
    Tag := (LValue and $F800) SHR 11;
    Monat := (LValue and $0780) SHR 7;
    Jahr := (LValue and $7F);
    if Jahr > 80 then
      Jahr := Jahr + 1900
    else
      Jahr := Jahr + 2000;

    szTemp := '$' + copy (szRoh, 5, 4);
    val (szTemp, LValue, Code);
    Valid := Valid and (Code = 0);
    if Valid then begin
      Stunde := (LValue and $07C0) SHR 6;
      Minute := (LValue and $003F);
      MNr := (LValue and $F800) SHR 11;
      szMNr:=Format('%.3d', [MNr]);

      try                            { �berpr�fung auf plausibles Datum, Zeit }
        EncodeDate (Jahr, Monat, Tag);
        EncodeTime (Stunde, Minute, 0, 0);
      except
        Valid:=false;
      end;

      if Valid then
        Result := TMeldung.Create (szMNr, Jahr, Monat, Tag, Stunde, Minute, 0, nil);
    end;
  end;
end;

{ GetMeldungE : erzeugt Meldunggsobjekt f�r einen Rohtext der Gruppe E
  Parameter :
    szRoh : Rohtext;
  R�ckgabe :
    nil, wenn Rohtext fehlerhaft, sonst Zeiger auf Meldungsobjekt }
{------------------------------------------------------------}
function TMeldungsListe.GetMeldungE (szRoh: string): TMeldung;
{------------------------------------------------------------}
var
  szMNr : string [5];
  szTemp : string [2];
  Jahr, Monat, Tag, Stunde, Minute, Sekunde: Word;
  Code : Integer;
  Valid : Boolean;
  VZ: string [1];
begin
  Result := Nil;
  szTemp:=Copy (szRoh, 1, 2);
  Val (szTemp, Jahr, Code);          { Jahr }
  Valid := Code = 0;
  if Valid then begin
    if Jahr < 80 then
      Jahr := Jahr + 2000
    else
      Jahr := Jahr + 1900;
  end;
  szTemp:=Copy (szRoh, 3, 2);
  Val (szTemp, Monat, Code);         { Monat }
  Valid := Valid and (Code = 0);
  szTemp:=Copy (szRoh, 5, 2);
  Val (szTemp, Tag, Code);           { Tag }
  Valid := Valid and (Code = 0);
  szTemp:=Copy (szRoh, 7, 2);
  Val (szTemp, Stunde, Code);        { Stunde }
  Valid := Valid and (Code = 0);
  szTemp:=Copy (szRoh, 9, 2);
  Val (szTemp, Minute, Code);        { Minute }
  Valid := Valid and (Code = 0);
  szTemp:=Copy (szRoh, 11, 2);
  Val (szTemp, Sekunde, Code);       { Sekunde }
  Valid := Valid and (Code = 0);

  szMNr:=Copy (szRoh, 13, 5);        { MRG-Meldenummer: kann 4 oder 5 Stellen haben (mit Blank, + o. -) }
  szMNr:=F_LeftTrunc (szMNr, ' ');
  Vz:='';
  if Length (szMNr) > 0 then
    if (szMNr [1] = '+') OR (szMNr [1] = '-') then begin    { mit Vorzeichen }
      Vz:=szMNr [1];
      szMNr:=Copy (szMNr, 2, 4);
    end else
      szMNr:=Copy (szMNr, 1, 4);

  if Valid then begin                { �berpr�fung auf plausibles Datum, Zeit }
    try
      EncodeDate (Jahr, Monat, Tag);
      EncodeTime (Stunde, Minute, Sekunde, 0);
    except
      Valid:=false;
    end;
  end;

  if Valid then
    Result := TMeldung.CreateMrgNr4stellig (szMNr, Vz, Jahr, Monat, Tag, Stunde, Minute, Sekunde, nil);
end;

{----------------------------------------------------------}
Procedure TMeldungsListe.SaveToExcel (ExcelCaption: string);
{----------------------------------------------------------}
{ Meldungen aus MeldungsListe in Excel konvertieren;
  -> ohne Zugriff auf WICOM-Tabellen
  �bergabe: Titel f�r Excel-Tabelle }
var
  MeldTextKonfigList: TMeldTextKonfigList;

begin
  MeldTextKonfigList:=TMeldTextKonfigList.Create;
  try
    { Meldungstext-Konfigurationsliste aus Resourcendatei laden: }
    GetMeldText_KonfigList (MeldTextKonfigList, KonfigPfad);
    { Meldungslisten-Eintr�ge mit Meldungstexten in Excel-Blatt schreiben: }
    WriteExcelSheet (ExcelCaption, MeldTextKonfigList);
  finally
    MeldTextKonfigList.Free;
  end;
end;

{---------------------------------------------------------------------------------}
Procedure TMeldungsListe.WriteExcelSheet (ExcelCaption: string;
                                          MeldTextKonfigList: TMeldTextKonfigList);
{---------------------------------------------------------------------------------}
{ Inhalt der MeldungsListe mit Meldungstexten in Excel-Blatt schreiben;
  �bergabe: Titel f�r Excel-Tabelle
            Liste mit Meldungstexten }
const
  C_Separator = #9;
var
  SL: TStringList;
  i: integer;
  S: string;
  Meldung: TMeldung;
  DatumZeit: TDateTime;
  MNrMrg: string;
  MNrAllg: string;
  MText: string;
  MTyp: string;
  dummy: string;

begin
  SL:=TStringList.Create;
  try
    SL.Duplicates:=dupAccept;
    SL.Sorted:=false;
    SL.Add (ExcelCaption);
    { Spalten�berschriften: }
    SL.Add (S_DatumZeit + C_Separator + S_Nummer + C_Separator + S_Meldung);
    SL.Add ('');

    { alle Listeneintr�ge in Stringliste schreiben: }
    for i:=0 to Count-1 do begin
      Application.ProcessMessages;
      Meldung:=TMeldung (Items [i]);
      DatumZeit:=EncodeDate (Meldung.Jahr, Meldung.Monat, Meldung.Tag) +
                 EncodeTime (Meldung.Stunde, Meldung.Minute, Meldung.Sekunde, 0);
      MNrMrg:=Meldung.VZ + Meldung.NrMrg;
      MNrAllg:=Meldung.NrAllg;

      MText:='';
      { in Meldungstext-Liste den Meldungstext zur allgemeinen Meldungsnummer suchen: }
      if MeldTextKonfigList <> nil then begin
        if MeldTextKonfigList.FindMeldText (MNrAllg, S, dummy, MTyp) then begin
          MText:=S;
          if MTyp = mtyp_kommt then
            MText:=MText + ' ' + S_kommt
          else if MTyp = mtyp_geht then
            MText:=MText + ' ' +  S_geht;
        end;
      end;
      S:=FormatDateTime(C_FormatDateTime, DatumZeit) + C_Separator +
         MNrMrg + C_Separator + MText;
      SL.Add (S);
    end;
    { Stringliste in Excel ausgeben: }
    InsertToExcel (SL, C_Separator, 1, 1, false, true, true);
  finally
    SL.Free;
  end;
end;

{--------------------------------------------------------------------}
function TMeldungsListe.SaveTo_FTL_SR_NeuAlarm (AlarmFilename: string;
  Kennung: string): integer;
{--------------------------------------------------------------------}
{ Meldungen aus MeldungsListe in Alarmdatei schreiben (wie Tritschler-Programm SrPc);
  �bergabe: Name der Alarmdatei
            Kennung
  R�ckgabe: 0, wenn Schreiben der Alarmdatei OK, sonst Fehlercode FILEERR_FTL_SR_ALARM_...
            lt. Errconst.pas }
var
  MeldTextKonfigList: TMeldTextKonfigList;

begin
  MeldTextKonfigList:=TMeldTextKonfigList.Create;
  try
    { Meldungstext-Konfigurationsliste aus Resourcendatei laden: }
    GetMeldText_KonfigList (MeldTextKonfigList, KonfigPfad);
    { Meldungslisten-Eintr�ge mit Meldungstexten in Alarmdatei schreiben: }
    Result:=WriteFile_FTL_SR_NeuAlarm (AlarmFilename, Kennung, MeldTextKonfigList);
  finally
    MeldTextKonfigList.Free;
  end;
end;

{-----------------------------------------------------------------------}
function TMeldungsListe.WriteFile_FTL_SR_NeuAlarm (AlarmFilename: string;
  Kennung: string; MeldTextKonfigList: TMeldTextKonfigList): integer;
{-----------------------------------------------------------------------}
{ Inhalt der MeldungsListe mit Meldungstexten in Alarmdatei schreiben (wie
  Tritschler-Programm SrPc);
  �bergabe: Name der Alarmdatei mit Pfad
            Kennung
            Liste mit Meldungstexten
  R�ckgabe: 0, wenn Schreiben der Alarmdatei OK, sonst Fehlercode FILEERR_FTL_SR_ALARM_...
            lt. Errconst.pas }
const
  CTrenner = ';';

var
  TFS: TTextFileStreamExt;
  isOK: boolean;
  DirName: string;
  S: string;
  sBuf: string;
  i: integer;
  Meldung: TMeldung;
  MText: string;
  MTyp: string;
  dummy: string;
  SL: TStringList;
  dtDatumZeit: TDateTime;
  MNrMrg_orig: string;

begin
  Result:=0;  // Vorbelegung: OK

  { Meldungseintr�ge f�r Alarmdatei in Stringliste laden: }
  SL:=TStringList.Create;
  try
    for i:=0 to Count-1 do begin
      Application.ProcessMessages;

      Meldung:=TMeldung (Items [i]);
      // originale MRG-Meldungsnummer ohne von Konvertierung angeh�ngtes
      // Kommt/Geht-Zeichen:
      MNrMrg_orig:=Meldung.VZ + Meldung.NrMrg;
      MNrMrg_orig:=Copy (MNrMrg_orig, 1, length (MNrMrg_orig) - 1);

      if MNrMrg_orig <> 'I7' then begin  { Meldung I7 'Probealarm' nicht in Alarmdatei schreiben }
        try
          dtDatumZeit:=EncodeDateTime (Meldung.Jahr, Meldung.Monat, Meldung.Tag,
                                       Meldung.Stunde, Meldung.Minute, Meldung.Sekunde, 0);
          S:=Kennung + CTrenner +  // Kennung
             Copy (Meldung.NrMrg, length (Meldung.NrMrg), 1) + CTrenner +  // Kommt/Geht-Zeichen
             MNrMrg_orig + CTrenner +  // Meldungstyp und -nummer
             FormatDateTime (SFormatDate, dtDatumZeit) + CTrenner +  // Datum
             FormatDateTime (S_FormatTime, dtDatumZeit) + CTrenner;  // Zeit

          MText:='';
          { in Meldungstext-Liste den Meldungstext zur allgemeinen Meldungsnummer suchen: }
          if MeldTextKonfigList <> nil then begin
            if MeldTextKonfigList.FindMeldText (Meldung.NrAllg, sBuf, dummy, MTyp) then begin
              MText:=sBuf;
              if MTyp = mtyp_kommt then
                MText:=MText + ' ' + S_kommt
              else if MTyp = mtyp_geht then
                MText:=MText + ' ' + S_geht;
            end;
          end;
          S:=S + MText;
          SL.Add (S);
        except
        end;
      end;  { if MNrMrg_orig <> 'I7' }
    end;  { for i }

    if SL.Count > 0 then begin
      { Verzeichnis f�r Alarmdatei anlegen, wenn nicht vorhanden: }
      DirName:=ExtractFilePath (AlarmFilename);
      if not DirectoryExists (DirName) then begin
        if not ForceDirectories (DirName) then begin
          Result:=FILEERR_FTL_SR_ALARM_COULDNOTCREATEDIR;  // Fehler Verzeichnis neu anlegen
          exit;
        end;
      end;

      TFS:=nil;
      try
        if FileExists (AlarmFilename) then begin
          TFS:=TTextFileStreamExt.Create (AlarmFilename,
            fmOpenReadWrite OR fmShareDenyWrite, isOK);  // Alarmdatei �ffnen
          if not isOK then begin
            Result:=FILEERR_FTL_SR_ALARM_COULDNOTOPENFILE;  // Fehler Alarmdatei �ffnen
            exit;
          end;
        end
        else begin
          TFS:=TTextFileStreamExt.Create (AlarmFilename, fmCreate, isOK);  // Alarmdatei neu anlegen
          if not isOK then begin
            Result:=FILEERR_FTL_SR_ALARM_COULDNOTCREATEFILE;  // Fehler Alarmdatei neu anlegen
            exit;
          end;
        end;

        TFS.Seek (0, soFromEnd);  // ans Dateiende gehen
        for i:=0 to SL.Count-1 do begin
          Application.ProcessMessages;
          TFS.WriteLn (SL [i]);
        end;  { for i }
      finally
        TFS.Free;
      end;
    end;  { if SL.Count > 0 }
  finally
    SL.Free;
  end;
end;

{---------------------------------------------------------}
function TMeldungsListe.LoadFromXmlFile (sFileName: string;
  bDelete: boolean): boolean;
{---------------------------------------------------------}

  function ThisStringToDateTime(sString: string): TDateTime;
  var
    dtd, dtt : TDateTime;
  begin
    Result := 0;
    if (EncodeDateStr(Copy(sString, 1, 8), 'YYYYMMDD', dtd)) and
       (EncodeTimeStr(Copy(sString, 9, 6), 'HHMMSS', dtt))
    then Result := Trunc(dtd) + Frac(dtt);
  end;

  function DecodeXmlLine(sLine: string; var sMMeldNr, sAMeldNr: string;
    var dtDatumZeit: TDateTime;
    var sMrgParaNr, sAParaNr, sParaOldValue, sParaNewValue: string): boolean;
  var
    s, sVal : string;
    iIndex  : integer;
    pTool   : TDecodeXMLResponse;
  begin
    sMMeldNr := '';
    sAMeldNr := '';
    dtDatumZeit := 0;
    sMrgParaNr:='';
    sAParaNr:='';
    sParaOldValue:='';
    sParaNewValue:='';

    if (Pos(C_lt_Subst + C_MRGMeldungen, sLine) > 0) and
       (Pos('/' + C_gt_Subst, sLine) > 0) then
    begin
      pTool := TDecodeXMLResponse.Create;
      try
        sLine := pTool.CutDataString(
          sLine, C_lt_Subst + C_MRGMeldungen, '/' + C_gt_Subst);

        iIndex := 1;
        s := pTool.GetDataPart(sLine, iIndex, C_quot_Subst);
        while (s <> '') do begin
          // Zugeh�rigen Wert abfragen
          Inc(iIndex);
          sVal := pTool.GetDataPart(sLine, iIndex, C_quot_Subst);

          // Um welchen Kennwert handelt es sich ?
          if (s = C_MrgKennungAllgMeldNr) then sAMeldNr := Trim(sVal)
          else if (s = C_MrgKennungMrgMeldNr) then sMMeldNr := Trim(sVal)
          else if (s = C_MrgKennungDatumZeit) then
            dtDatumZeit := ThisStringToDateTime(sVal)
          else if (s = C_MrgKennungMeldParaAendAllgParaNr) then
            sAParaNr := Trim(sVal)
          else if (s = C_MrgKennungMeldParaAendMrgParaNr) then
            sMrgParaNr := Trim(sVal)
          else if (s = C_MrgKennungMeldParaAendWertAlt) then
            sParaOldValue := sVal
          else if (s = C_MrgKennungMeldParaAendWertNeu) then
            sParaNewValue := sVal;

          // N�chste Kennung einlesen
          Inc(iIndex);
          s := pTool.GetDataPart(sLine, iIndex, C_quot_Subst);
        end;
        Result := (dtDatumZeit > 0);
      finally
        pTool.Free;
      end;
    end
    else Result := False;
  end;

var
  i            : integer;
  sANr, sMrgNr : string;
  dt           : TDateTime;
  sMrgParaNr   : string;
  sAParaNr     : string;
  sParaOldValue: string;
  sParaNewValue: string;
  ParaChange   : TParaChange;
begin
  Result := False;
  Clear;  { Meldungsliste leeren }

  if (FileExists(sFileName)) then
    with TStringList.Create do
    try
      LoadFromFile(sFileName);
      if (Pos(C_WieserStart, Text) > 0) and
         (Pos(C_MRGBlockStart, Text) > 0) and
         (Pos(C_MRGMeldungenStart, Text) > 0) and
         (Pos(C_MRGMeldungenEnd, Text) > 0)
      then begin

        i := 0;
        while (i < Count) do begin
          if (Strings[i] = C_MRGMeldungenStart) then begin
            while (i < Count-1) do begin
              Inc(i);
              if (Strings[i] = C_MRGMeldungenEnd) then begin
                i := Count-1;
                Break;
              end;

              // Decodieren, bei Fehler: raus
              if (not DecodeXmlLine(Strings[i], sMrgNr, sANr, dt,
                                    sMrgParaNr, sAParaNr, sParaOldValue, sParaNewValue))
              then Exit
              else begin
                if length (sMrgParaNr) > 0 then   // Meldung "Parameter�nderung" besitzt MRG-spezifische Parameternummer
                  ParaChange:=TParaChange.Create (sMrgParaNr, sAParaNr, sParaOldValue, sParaNewValue)
                else
                  ParaChange:=nil;
                Self.Add(TMeldung.Create(sMrgNr, sANr, dt, ParaChange));
              end;
            end;
          end;
          Inc(i);
        end;

        Result := True;
      end;
    finally
      Free;
    end;

  if bDelete then
    DeleteFile (sFileName);
end;

{--------------------------------------------------------}
function TMeldungsListe.LoadFromAscFile(sFileName: string;
  MeldKonv: TMeldKonv; ConfigFromDB: boolean): boolean;
{--------------------------------------------------------}
var
  i            : integer;
  sANr, sMrgNr : string;
  dt           : TDateTime;
begin
  Result := False;
  Clear;  { Meldungsliste leeren }

  if (FileExists(sFileName)) then
    with TStringList.Create do
    try
      LoadFromFile(sFileName);
      i := 2;  // 1. Zeile: Kennung, 2. Zeile: Mrg-Typ
      while (i < Count) do begin
        if (Length(Strings[i]) < 17) then begin
          Clear;
          Exit;
        end;

        sANr := '';
        sMrgNr := Copy(Strings[i], 1, 3);
        dt :=
          EncodeDate(StrToInt(Copy(Strings[i], 4, 4)),
            StrToInt(Copy(Strings[i], 8, 2)),
            StrToInt(Copy(Strings[i], 10, 2))) +
          EncodeTime(StrToInt(Copy(Strings[i], 12, 2)),
            StrToInt(Copy(Strings[i], 14, 2)),
            StrToInt(Copy(Strings[i], 16, 2)), 0);

        Self.Add(TMeldung.Create(sMrgNr, sANr, dt, nil));

        Inc(i);
      end;

      Result := True;
    finally
      Free;
    end;

  if (MeldKonv.RohLoeschen) then DeleteFile (sFileName);
end;


{---------------------------------------}
procedure TMeldungsListe.SortByDatumZeit;
{---------------------------------------}
begin
  Sort (Meldungsliste_DatumZeitIndexCompare);  { Liste nach Zeitstempel, Listenindex sortieren }
end;

End.

