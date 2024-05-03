{********************************************************************************}
{* Unit: Zugriff auf LAKS-Archiv-Tabellen (Index-, Satz-, Werte-, Meldungs- und *}
{*       Energieband-Tabellen)                                                  *}
{* 30.06.1999 WW                                                                *}
{********************************************************************************}
unit TbLaksAr;

interface

uses
  Forms, Classes, Db, DbTables, SysUtils, WStrUtils, T_Zeit, LAKSUtil, novell, My_Utils;

const
  { Index-Tabelle für LAKS-Archiv }

  C_TbLIndex            = 'Index.db';

  C_TfLIndex_Index      = 'Index';           { smallint }
  C_TfLIndex_Kennung    = 'Kennung';         { str14 }
  C_TfLIndex_StatNr     = 'StatNr';          { smallint }
  C_TfLIndex_ArchGrpNr  = 'ArchivGruppenNr'; { smallint }
  C_TfLIndex_Instanz    = 'Instanz';         { smallint }
  C_TfLIndex_Geraetetyp = 'Geraetetyp';      { str3 }

  { Tabelle für LAKS-Meldungen }
  C_TbLMeldung          = 'A';

  C_TfLMeld_MeldNr      = 'Abwurf';          { int }
  C_TfLMeld_K_Datum     = 'K_Datum';         { date }
  C_TfLMeld_K_Zeit      = 'K_Zeit';          { time }
  C_TfLMeld_G_Datum     = 'G_Datum';         { date }
  C_TfLMeld_G_Zeit      = 'G_Zeit';          { time }
  C_TfLMeld_MeldStatus  = 'MeldungsStatus';  { str5 }
  C_TfLMeld_ZeitDiff    = 'Zeitdifferenz';   { int }
  C_TfLMeld_LAKSNr      = 'Laksnummer';      { smallint }
  C_TfLMeld_Plus1_K     = 'Plus1_K';         { str 24 }
  C_TfLMeld_Plus1_G     = 'Plus1_G';         { str 24 }
  C_TfLMeld_Plus2_K     = 'Plus2_K';         { str 24 }
  C_TfLMeld_Plus2_G     = 'Plus2_G';         { str 24 }
  C_TfLMeld_Plus3_K     = 'Plus3_K';         { str 24 }
  C_TfLMeld_Plus3_G     = 'Plus3_G';         { str 24 }

  { LAKS-Satz-Tabelle }
  C_TbLSatz             = 'Satz';

  C_TfLSatz_ReferenzNr  = 'ReferenzNr';      { int }
  C_TfLSatz_Index       = 'Index';           { smallint }
  C_TfLSatz_OrdNr       = 'Ordnungsnummer';  { int }
  C_TfLSatz_Datum       = 'Datum';           { date }
  C_TfLSatz_Zeit        = 'Zeit';            { time }
  C_TfLSatz_Status      = 'Status';          { str10 }

  { LAKS-Wert-Tabelle }
  C_TbLWert             = 'W';

  C_TfLWert_ReferenzNr  = 'ReferenzNr';      { int }
  C_TfLWert_Kanaltyp    = 'Kanaltyp';        { str2 }
  C_TfLWert_Werteart    = 'Werteart';        { str2 }
  C_TfLWert_Wert        = 'Wert';            { num }
  C_TfLWert_Status      = 'Status';          { str5 }

  { LAKS-Tabelle für Energiebandänderungen }
  C_TbLEBand             = 'Band';

  C_TfLEBand_BandNr      = 'Bandnummer';      { smallint }
  C_TfLEBand_WertNr      = 'Wertnummer';      { smallint }
  C_TfLEBand_V_Datum     = 'V_Datum';         { date }
  C_TfLEBand_V_Zeit      = 'V_Zeit';          { time }
  C_TfLEBand_W_Datum     = 'W_Datum';         { date }
  C_TfLEBand_W_Zeit      = 'W_Zeit';          { time }
  C_TfLEBand_U_Datum     = 'U_Datum';         { date }
  C_TfLEBand_U_Zeit      = 'U_Zeit';          { time }
  C_TfLEBand_Leistung    = 'Leistung';        { num }


Type

  { Energiebandobjekt zur Aufnahme von Energiebanddaten (in TEBandListe) }

  TEBandItem = class (TObject)
  private
    BandNummer: byte;
    WertNummer: byte;
    V_DT: TDateTime;
    W_DT: TDateTime;
    U_DT: TDateTime;
    Wert: double;
  public
    Constructor Create (ABandnummer: byte; AWertNummer: byte;
                        AV_DT: TDateTime; AW_DT: TDateTime; AU_DT: TDateTime;
                        AWert: double);
  End;

  TEBandListe = class (TList)
  public
    Destructor Destroy; override;
  End;

  { Objekt für Zugriff auf Satz- und Wertetabellen }

  TTbLSatzWert = class(TObject)
  private
    Path: TFileName;
    Kennung: string;
    tbLSatz: TTable;
    tbLWert: TTable;
    procedure CreateTbLSatz;
    procedure CreateTbLWert;
    function GetLetztSatzDaten (var RefNr: integer; var DatumZeit: TDateTime): boolean;
  public
    constructor Create (APath: TFileName; AKennung: string);
    destructor Destroy; override;
    function GetLetztReferenzNr: integer;
    function GetLetztDatumZeit(var DatumZeit: TDateTime): boolean;
    function KonvMesswerte (Rohdateiname: TFileName; Loeschen: boolean): integer;
  end;

  { Objekt für Zugriff auf Meldungstabellen }

  TTbLMeldung = class(TObject)
  private
    Path: TFileName;
    Kennung: string;
    tbLMeldung: TTable;
    procedure CreateTbLMeldung;
    procedure KonvPlusmeldungen(MeldNr: integer; Zustand: string; PlusMeld: string;
                                var Plus1_K: string; var Plus1_G: string;
                                var Plus2_K: string; var Plus2_G: string;
                                var Plus3_K: string; var Plus3_G: string);
    procedure WriteMeldungToTable (AMeldNr: integer; ADatumZeit: TDateTime;
                                   AStatus: string; ALaksNr: integer;
                                   APlus1_K: string; APlus1_G: string;
                                   APlus2_K: string; APlus2_G: string;
                                   APlus3_K: string; APlus3_G: string);
  public
    constructor Create (APath: TFileName; AKennung: string);
    destructor Destroy; override;
    function GetLetztDatumZeit(var DatumZeit: TDateTime): boolean;
    function KonvMeldungen (Rohdateiname: TFileName; Loeschen: boolean): integer;
  end;

  { Objekt für Zugriff auf Energiebandtabellen }

  TTbLEBand = class(TObject)
  private
    Path: TFileName;
    Kennung: string;
    tbLEBand: TTable;
    procedure CreateTbLEBand;
  public
    constructor Create (APath: TFileName; AKennung: string);
    destructor Destroy; override;
    function GetLetztDatumZeit(var DatumZeit: TDateTime): boolean;
    function KonvEnergiebandaenderungen (Rohdateiname: TFileName; Loeschen: boolean): integer;
  end;

function Get_LAKSDatenIndex(Pfad: TFileName; Kennung: string; Eintragen: boolean): integer;

implementation

Const
  { Meldungszustände }

  C_kommt = 'kommt';  { offene Kommt-Meldung }
  C_geht  = 'geht';   { offene Geht-Meldung }
  C_ok    = 'ok';     { abgeschlossene Meldung }

  { Energiebandzustände (Feld "WertNr" in Energiebandtabelle) }
  C_EBandVorgabe    = 0;
  C_EBandUebernahme = 1;


{----------------------------------------------------------}
function W_DateTimeCompare (Item1, Item2: Pointer): Integer;
{----------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TEBandItem-Objekten nach W_DT }
begin
  Result := CmpDateTime ((TEBandItem (Item1).W_DT), (TEBandItem (Item2).W_DT));
end;

{ TEBandItem }

{----------------------------------------------------------------------------------}
Constructor TEBandItem.Create (ABandnummer: byte; AWertNummer: byte;
                               AV_DT: TDateTime; AW_DT: TDateTime; AU_DT: TDateTime;
                               AWert: double);
{----------------------------------------------------------------------------------}
Begin
  Inherited Create;
  Bandnummer:=ABandnummer;
  WertNummer:=AWertNummer;
  V_DT:=AV_DT;
  W_DT:=AW_DT;
  U_DT:=AU_DT;
  Wert:=AWert;
End;


{ TEBandListe }

{-----------------------------}
Destructor TEBandListe.Destroy;
{-----------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    TEBandItem (Items [i]).Free;
  inherited Destroy;
end;


{ Funktionen für Zugriff auf LAKS-Index-Tabelle }

{---------------------------------------}
procedure CreateIndexDB(Pfad: TFileName);
{---------------------------------------}
{ Index-Tabelle anlegen }
var
  IndexTable: TTable;

begin
  Pfad:=ExpandFilename(Pfad);
  IndexTable:=TTable.Create(nil);
  try
    IndexTable.DatabaseName:=Pfad;
    IndexTable.TableName:=C_TbLIndex;
    with IndexTable.FieldDefs do begin
      Clear;
      Add(C_TfLIndex_Index, ftSmallInt, 0, false);
      Add(C_TfLIndex_Kennung, ftString, 14, false);
      Add(C_TfLIndex_StatNr, ftSmallInt, 0, false);
      Add(C_TfLIndex_ArchGrpNr, ftSmallInt, 0, false);
      Add(C_TfLIndex_Instanz, ftSmallInt, 0, false);
      Add(C_TfLIndex_Geraetetyp, ftString, 3, false);
    end;
    with IndexTable.IndexDefs do begin
      Clear;
      Add('Primaerindex', 'Index', [ixPrimary, ixUnique]);
    end;
    IndexTable.CreateTable;
  finally
    IndexTable.Free;
  end;
end;

{-----------------------------------------------------------------------------------------}
function Get_LAKSDatenIndex(Pfad: TFileName; Kennung: string; Eintragen: boolean): integer;
{-----------------------------------------------------------------------------------------}
{ Übergabe: Pfad der Index-Tabelle;
            Kennung;
            Eintragen: true = neuer Eintrag für Kennung
                       false = Rückgabe -1, wenn fehlend
  Rückgabe im Erfolgsfall: Indexnummer, sonst -1 }
type
  PMerkfeld = ^TMerkfeld;
  TMerkfeld = array[1..9999] of boolean;

var
  Idx: integer;
  gefunden: boolean;
  Kenn: string;
  Merkfeld: PMerkfeld;
  IndexTable: TTable;

begin
  Result:=-1;
  Kennung:=F_RightTrunc(Kennung,' ');
  Merkfeld:=New(PMerkfeld);
  try
    for Idx:=Low(Merkfeld^) to High(Merkfeld^) do Merkfeld^[Idx]:=false;
    Idx:=-1;
    Pfad:=ExpandFilename(Pfad);
    IndexTable:=TTable.Create(nil);
    try
      IndexTable.DatabaseName:=Pfad;
      IndexTable.TableName:=C_TblIndex;
      { Tabelle anlegen, wenn nicht vorhanden: }
      if not IndexTable.Exists then CreateIndexDB(Pfad);
      IndexTable.Open;
      try
        IndexTable.First;
        gefunden:=false;
        while not IndexTable.EOF do begin
          Idx:=IndexTable.FieldByName(C_TfLIndex_Index).AsInteger;
          Kenn:=IndexTable.FieldByName(C_TfLIndex_Kennung).AsString;
          Merkfeld^[Idx]:=true;                               { Nummer belegt }
          if Kennung = Kenn then begin                     { Eintrag gefunden }
            gefunden:=true;
            Break;
          end;
          IndexTable.Next;
        end;

        if (not gefunden) AND Eintragen then begin         { Eintrag einfügen }
          Idx:=1;
          while Merkfeld^[Idx] do
            inc(Idx);                           { nächste freie Nummer suchen }
          IndexTable.AppendRecord([Idx,Kennung,0,0,0,'S']);
          gefunden:=true;
        end;
      finally
        IndexTable.Close;
      end;
    finally
      IndexTable.Free;
    end;
  finally
    Dispose(Merkfeld);
  end;
  if (Idx > 0) AND gefunden then Result:=Idx;
end;


{ TTbLSatzWert }

{-------------------------------------------------------------------}
constructor TTbLSatzWert.Create (APath: TFileName; AKennung: string);
{-------------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  Kennung:=AKennung;
  tbLSatz:=TTable.create(nil);
  tbLSatz.DataBaseName:=Path;
  tbLSatz.TableName:='';
  tbLWert:=TTable.create(nil);
  tbLWert.DataBaseName:=Path;
  tbLWert.TableName:='';
end;

{------------------------------}
destructor TTbLSatzWert.Destroy;
{------------------------------}
begin
  tbLWert.Free;
  tbLSatz.Free;
  inherited Destroy;
end;

{-----------------------------------}
procedure TTbLSatzWert.CreateTbLSatz;
{-----------------------------------}
{ Satz-Tabelle anlegen }
begin
  with tbLSatz.FieldDefs do begin
    Clear;
    Add(C_TfLSatz_ReferenzNr, ftInteger, 0, false);
    Add(C_TfLSatz_Index, ftSmallInt, 0, false);
    Add(C_TfLSatz_OrdNr, ftInteger, 0, false);
    Add(C_TfLSatz_Datum, ftDate, 0, false);
    Add(C_TfLSatz_Zeit, ftTime, 0, false);
    Add(C_TfLSatz_Status, ftString, 10, false);
  end;
  tbLSatz.IndexDefs.Clear;                                                     { kein Index }
  tbLSatz.CreateTable;
end;

{-----------------------------------}
procedure TTbLSatzWert.CreateTbLWert;
{-----------------------------------}
{ Wert-Tabelle anlegen }
begin
  with tbLWert.FieldDefs do begin
    Clear;
    Add(C_TfLWert_ReferenzNr, ftInteger, 0, false);
    Add(C_TfLWert_Kanaltyp, ftString, 2, false);
    Add(C_TfLWert_Werteart, ftString, 2, false);
    Add(C_TfLWert_Wert, ftFloat, 0, false);
    Add(C_TfLWert_Status, ftString, 5, false);
  end;
  tbLWert.IndexDefs.Clear;                                                     { kein Index }
  tbLWert.CreateTable;
end;

{----------------------------------------------------------------------------------------------}
function TTbLSatzWert.GetLetztSatzdaten (var RefNr: integer; var DatumZeit: TDateTime): boolean;
{----------------------------------------------------------------------------------------------}
{ ermittelt Referenznummer, Datum und Zeit des letzten Eintrags der Satztabelle;
  Rückgabe: ReferenzNr
            Datum/Zeit
  Ergebnis: true, wenn Daten des letzten Eintrags ermittelt werden konnten }
var
 Idx: integer;
begin
  Result:=false;
  RefNr:=0;
  DatumZeit:=0;
  Idx:=Get_LAKSDatenIndex(Path, Kennung, false);         { Index für Satz-Tabelle ermitteln }
  if Idx <= 0 then exit;                                         { noch kein Index vergeben }

  tbLSatz.TableName:= C_TbLSatz + Format ('%.4d.Db', [Idx]);
  if not tbLSatz.Exists then exit;                                { Tabelle nicht vorhanden }
  tbLSatz.Open;
  try
    tbLSatz.Last;
    if not tbLSatz.Bof then begin                        { wenn mind. 1 Datensatz enthalten }
      RefNr:=tbLSatz.FieldByName (C_TfLSatz_ReferenzNr).AsInteger;
      DatumZeit:=tbLSatz.FieldByName (C_TfLSatz_Datum).AsDateTime +
                 tbLSatz.FieldByName (C_TfLSatz_Zeit).AsDateTime;
      Result:=true;
    end;
  finally
    tbLSatz.Close;
  end;
end;

{------------------------------------------------}
function TTbLSatzWert.GetLetztReferenzNr: integer;
{------------------------------------------------}
{ ermittelt Referenznummer des letzten Eintrags der Satztabelle;
  Ergebnis: ReferenzNr, wenn des letzten Eintrags
            0, wenn noch keine Daten vorhanden sind }
var
  Dummy: TDateTime;
begin
  if not GetLetztSatzdaten (Result, Dummy) then
    Result:=0;
end;

{-------------------------------------------------------------------------}
function TTbLSatzWert.GetLetztDatumZeit(var DatumZeit: TDateTime): boolean;
{-------------------------------------------------------------------------}
{ ermittelt Datum und Zeit des letzten Eintrags der Satztabelle;
  Rückgabe: Datum/Zeit des letzten Eintrags
  Ergebnis: false, wenn noch keine Daten vorhanden sind }
var
  Dummy: integer;
begin
  Result:=GetLetztSatzdaten (Dummy, DatumZeit);
end;

{----------------------------------------------------------------------------------------}
Function TTbLSatzWert.KonvMesswerte (Rohdateiname: TFileName; Loeschen: boolean): integer;
{----------------------------------------------------------------------------------------}
{ Meßwertrohdaten in Satz- und Wertetabellen konvertieren;
  Übergabe: Datei mit Meßwert-Rohdaten
            Loeschen (wenn true, werden Roh- und Zwischendateien gelöscht)
  Ergbenis:  0 = ok
            -1 = Kein Rohfile vorhanden
            -2 = Kann Index nicht eintragen
            -3 = Zwischendatei konnte nicht angelegt werden
            -4 = Rohfile enthält falsche Daten }
Const
  TerminatorSet = [US,RS,ETX];
  C_MaxImpulskanaele = 64;
  C_MaxAnalogkanaele =  8;
  C_MaxSummenkanaele =  8;
  C_AnaOffset = 200;
  C_SuOffset  = 500;

  { Zwischendateien }
  DSatz     = 'SATZ';
  DWerteImp = 'I';
  DWerteAna = 'A';
  DWerteSu  = 'S';

Type

  { Struktur der Satz-Zwischendatei bei Meßwertkonvertierung }
  R_Satz = packed record
             DatumZeit: TDateTime;
       {$H-} Status: string[5]; {$H+}
           end;

  { Kanal-Struktur für Wert-Zwischendatei bei Meßwertkonvertierung }
  R_Wert = packed record
             Wert: double;
             Status: char;
           end;

  { Struktur der Zwischendatei für Impulskanäle }
  R_WerteImp = array[1..C_MaxImpulskanaele] of R_Wert;

  { Struktur der Zwischendatei für Analogkanäle }
  R_WerteAna = array[1..C_MaxAnalogkanaele] of R_Wert;

  { Struktur der Zwischendatei für Summenkanäle }
  R_WerteSu  = array[1..C_MaxSummenkanaele] of R_Wert;

var
  TabIndex : integer;
  Kanal: integer;
  Status: char;
  SWert: single;
  Wert, Faktor: double;
  D_Rohfile: file of char;
  Rohsatz, S: string;
  MPKopf_gelesen: boolean;
  zeich: char;
  DSatzName: TFileName;
  DWerteImpName: TFileName;
  DWerteAnaName: TFileName;
  DWerteSuName: TFileName;
  D_Satz: file of R_Satz;
  D_WerteImp: file of R_WerteImp;
  D_WerteAna: file of R_WerteAna;
  D_WerteSu: file of R_WerteSu;
  Satz: R_Satz;
  WerteImp: R_WerteImp;
  WerteAna: R_WerteAna;
  WerteSu: R_WerteSu;
  ImpDaten: array[1..C_MaxImpulskanaele] of boolean;
  AnaDaten: array[1..C_MaxAnalogkanaele] of boolean;
  SuDaten: array[1..C_MaxSummenkanaele] of boolean;
  zeiger: integer;
  FSize: integer;
  RefNr, RefNr_alt: integer;
  ZwischenDateiDir: TFileName;
  Jahr, Monat, Tag: word;
  Stunde, Minute, Sekunde: word;

begin
  Result:= 0;
  AssignFile(D_Rohfile, RohDateiname);                                       { Rohdatenfile }
  if NetReset(D_Rohfile, SizeOf(char),ForRead + DenyWrite) < 0 then begin
    Result:= -1;
    exit;
  end;

  TabIndex:=Get_LAKSDatenIndex(Path, Kennung, true);       { (neuer) Index für Satz-Tabelle }
  if TabIndex <= 0 then begin                                         { kein Index vergeben }
    CloseFile(D_Rohfile);
    Result:= -2;
    exit;
  end;

  RefNr_alt:=GetLetztReferenzNr;                    { letzte Referenznummer aus Satztabelle }

  { Zwischendateien im Verzeichnis der Rohdatendatei anlegen: }
  ZwischenDateiDir:=ExtractFileDir (Rohdateiname);
  if ZwischenDateiDir [length(ZwischenDateiDir)] <> '\' then
    ZwischenDateiDir:=ZwischenDateiDir + '\';

  { Zwischendatei für Daten der Satztabelle: }
  DSatzName:=ZwischenDateiDir + DSatz + Format ('%.4d.dat', [TabIndex]);
  AssignFile(D_Satz, DSatzName);
  {$I-} Rewrite(D_Satz); {$I+}
  if ioresult <> 0 then begin
    CloseFile(D_Rohfile);
    Result:= -3;
    exit;
  end;

  { Zwischendatei für Impulsdaten der Werttabelle: }
  DWerteImpName:=ZwischenDateiDir + DWerteImp + Format ('%.4d.dat', [TabIndex]);
  AssignFile(D_WerteImp, DWerteImpName);
  {$I-} Rewrite(D_WerteImp); {$I+}
  if ioresult <> 0 then begin
    CloseFile(D_Satz);
    CloseFile(D_Rohfile);
    Result:= -3;
    exit;
  end;

  { Zwischendatei für Analogdaten der Werttabelle: }
  DWerteAnaName:=ZwischenDateiDir + DWerteAna + Format ('%.4d.dat', [TabIndex]);
  AssignFile(D_WerteAna, DWerteAnaName);
  {$I-} Rewrite(D_WerteAna); {$I+}
  if ioresult <> 0 then begin
    CloseFile(D_WerteImp);
    CloseFile(D_Satz);
    CloseFile(D_Rohfile);
    Result:= -3;
    exit;
  end;

  { Zwischendatei für Summendaten der Werttabelle: }
  DWerteSuName:=ZwischenDateiDir + DWerteSu + Format ('%.4d.dat', [TabIndex]);
  AssignFile(D_WerteSu, DWerteSuName);
  {$I-} Rewrite(D_WerteSu); {$I+}
  if ioresult <> 0 then begin
    CloseFile(D_WerteAna);
    CloseFile(D_WerteImp);
    CloseFile(D_Satz);
    CloseFile(D_Rohfile);
    Result:= -3;
    exit;
  end;

  { Vorbelegung der Merkfelder: Für welche Kanäle gibt es Daten ? }
  for Kanal:=1 to C_MaxImpulskanaele do ImpDaten[Kanal]:=false;
  for Kanal:=1 to C_MaxAnalogkanaele do AnaDaten[Kanal]:=false;
  for Kanal:=1 to C_MaxSummenkanaele do SuDaten[Kanal]:=false;


{------------------ Konvertierung: Rohdaten -> Zwischendateien -----------------------------}

  zeich := NUL;
  while zeich <> STX do
    Read(D_Rohfile,zeich);        { lesen bis STX ( es kommt manchmal ein Zeichen davor !) }
  Read(D_Rohfile,zeich);                                                               { e }
  if zeich <> 'e' then begin
    CloseFile(D_WerteSu);
    CloseFile(D_WerteAna);
    CloseFile(D_WerteImp);
    CloseFile(D_Satz);
    CloseFile(D_Rohfile);
    Result:= -4;
    exit;
  end;

  Rohsatz:='';
  MPKopf_gelesen:=false;
  zeich:=NUL;

  while not Eof(D_Rohfile) AND (zeich <> ETX) do begin
    Read(D_Rohfile,zeich);
    if zeich in TerminatorSet then begin
      if Rohsatz <> '' then begin
        if (zeich = US) AND not MPKopf_gelesen then begin           { MP-Kopf konvertieren }
          { Vorbelegung der Werte-Records: fehlend }
          for Kanal:=1 to C_MaxImpulskanaele do WerteImp[Kanal].Status:=NUL;
          for Kanal:=1 to C_MaxAnalogkanaele do WerteAna[Kanal].Status:=NUL;
          for Kanal:=1 to C_MaxSummenkanaele do WerteSu[Kanal].Status:=NUL;

          { Datum und Zeit: }
          Jahr:=Jahr2To4stellig (StrToInt (Copy(Rohsatz,1,2)));
          Monat:=StrToInt (Copy(Rohsatz,3,2));
          Tag:=StrToInt (Copy(Rohsatz,5,2));
          Stunde:=StrToInt (Copy(Rohsatz,7,2));
          Minute:=StrToInt (Copy(Rohsatz,9,2));
          Sekunde:=StrToInt (Copy(Rohsatz,11,2));
          Satz.DatumZeit:=EncodeDate (Jahr, Monat, Tag) +
                          EncodeTime (Stunde, Minute, Sekunde, 0);

          { Satzstatus enthält: }
          S:=chr(ord(Rohsatz[15]) + 1); { Tarifnr. für Auswertung: 1, 2 (LAKS sendet 0, 1) }
          S:=S + Rohsatz[13];                                                 { Satzstatus }
          S:=S + Rohsatz[14];                                                 { Laksnummer }
          S:=S + Copy(Rohsatz,16,2);                                       { Maximumnummer }
          Satz.Status:=S;

          write(D_Satz, Satz);                           { in Satz-Zwischendatei schreiben }
          MPKopf_gelesen:=true;
          Rohsatz:='';
          Continue;
        end;
                                                                      { Kanal konvertieren }
        if (zeich in TerminatorSet) AND MPKopf_gelesen then begin
          S:=Copy(Rohsatz,1,3);                                              { Kanalnummer }
          Kanal:=StrToInt (S);
          Status:=Rohsatz[10];                                               { Kanalstatus }

          S:=UpperCase(StrFilter(Copy(Rohsatz,4,6),' '));                        { Einheit }
          Faktor:=1;
          if      (S = 'WH')  OR (S = 'VARH')  then Faktor:=0.001
          else if (S = 'MWH') OR (S = 'MVARH') then Faktor:=1000
          else if (S = 'GWH') OR (S = 'GVARH') then Faktor:=1000000;
          Decode_LAKSRealStr(Copy(Rohsatz,11,8), SWert);
          Wert:=SWert * Faktor;                                     { Wert kommt als Arbeit }

          { Impulskanal-Nummern: 1..99 }
          if (Kanal >= 1) AND (Kanal <= C_MaxImpulskanaele) then begin
            WerteImp[Kanal].Wert:=Wert;
            WerteImp[Kanal].Status:=Status;
            ImpDaten[Kanal]:=true;
          end;
          { Analogkanal-Nummern: 201..299 }
          if (Kanal >= C_AnaOffset+1) AND (Kanal <= C_AnaOffset+C_MaxAnalogkanaele) then begin
            Kanal:=Kanal-C_AnaOffset;
            WerteAna[Kanal].Wert:=Wert;
            WerteAna[Kanal].Status:=Status;
            AnaDaten[Kanal]:=true;
          end;
          { Summenkanal-Nummern: 501..599 }
          if (Kanal >= C_SuOffset+1) AND (Kanal <= C_SuOffset+C_MaxSummenkanaele) then begin
            Kanal:=Kanal-C_SuOffset;
            WerteSu[Kanal].Wert:=Wert;
            WerteSu[Kanal].Status:=Status;
            SuDaten[Kanal]:=true;
          end;

          if (zeich = RS) OR (zeich = ETX) then begin
            write(D_WerteImp, WerteImp);         { in Impuls-Werte-Zwischendatei schreiben }
            write(D_WerteAna, WerteAna);         { in Analog-Werte-Zwischendatei schreiben }
            write(D_WerteSu, WerteSu);           { in Summen-Werte-Zwischendatei schreiben }
            MPKopf_gelesen:=false;
          end;
        end;
      end;
      Rohsatz:='';
      Application.ProcessMessages;
    end else
      Rohsatz:=Rohsatz+zeich;
  end; { while }
  CloseFile(D_Rohfile);
  if Loeschen then
    DeleteFile (Rohdateiname);                                          { Rohdatei löschen }

{------------------ Konvertierung: Zwischendateien -> Tabellen -----------------------------}

  if FileSize(D_Satz) > 0 then begin
    tbLSatz.TableName:= C_TbLSatz + Format ('%.4d.Db', [TabIndex]);
    if not tbLSatz.Exists then
      CreateTbLSatz;                                             { Satztabelle neu anlegen }
    tbLSatz.Open;
    try
      { Satz-Zwischendatei rückwärts lesen, Daten in Satz-Tabelle schreiben: }
      FSize:=FileSize(D_Satz);
      for zeiger:=FSize-1 downto 0 do begin
        seek(D_Satz, zeiger);
        read(D_Satz, Satz);
        RefNr:=RefNr_alt + FSize - zeiger;
        tbLSatz.AppendRecord ([RefNr,TabIndex,0,
                               Int(Satz.DatumZeit),Frac(Satz.DatumZeit),Satz.Status]);
        Application.ProcessMessages;
      end;
    finally
      tbLSatz.Close;
    end;

    { Impulskanäle: }
    for Kanal:=1 to C_MaxImpulskanaele do begin
      if not ImpDaten[Kanal] then Continue;
      tbLWert.TableName:= C_TbLWert +
                          Format ('%.3d', [Kanal]) + Format ('%.4d.Db', [TabIndex]);
      if not tbLWert.Exists then
        CreateTbLWert;                                           { Werttabelle neu anlegen }
      tbLWert.Open;
      try
        { Werte-Zwischendatei rückwärts lesen, Daten in Werttabelle schreiben: }
        FSize:=FileSize(D_WerteImp);
        for zeiger:=FSize-1 downto 0 do begin
          seek(D_WerteImp, zeiger);
          read(D_WerteImp, WerteImp);
          if WerteImp[Kanal].Status <> NUL then begin
            RefNr:=RefNr_alt + FSize - zeiger;
            tbLWert.AppendRecord ([RefNr,'','',WerteImp[Kanal].Wert,WerteImp[Kanal].Status]);
          end;
          Application.ProcessMessages;
        end;
      finally
        tbLWert.Close;
      end;
    end; { for Kanal }

    { Analogkanäle: }
    for Kanal:=1 to C_MaxAnalogkanaele do begin
      if not AnaDaten[Kanal] then Continue;
      tbLWert.TableName:= C_TbLWert +
                          Format ('%.3d', [Kanal+C_AnaOffset]) + Format ('%.4d.Db', [TabIndex]);
      if not tbLWert.Exists then
        CreateTbLWert;                                           { Werttabelle neu anlegen }
      tbLWert.Open;
      try
        { Werte-Zwischendatei rückwärts lesen, Daten in Werttabelle schreiben: }
        FSize:=FileSize(D_WerteAna);
        for zeiger:=FSize-1 downto 0 do begin
          seek(D_WerteAna, zeiger);
          read(D_WerteAna, WerteAna);
          if WerteAna[Kanal].Status <> NUL then begin
            RefNr:=RefNr_alt + FSize - zeiger;
            tbLWert.AppendRecord ([RefNr,'','',WerteAna[Kanal].Wert,WerteAna[Kanal].Status]);
          end;
          Application.ProcessMessages;
        end;
      finally
        tbLWert.Close;
      end;
    end; { for Kanal }

    { Summenkanäle: }
    for Kanal:=1 to C_MaxSummenkanaele do begin
      if not SuDaten[Kanal] then Continue;
      tbLWert.TableName:= C_TbLWert +
                          Format ('%.3d', [Kanal+C_SuOffset]) + Format ('%.4d.Db', [TabIndex]);
      if not tbLWert.Exists then
        CreateTbLWert;                                           { Werttabelle neu anlegen }
      tbLWert.Open;
      try
        { Werte-Zwischendatei rückwärts lesen, Daten in Werttabelle schreiben: }
        FSize:=FileSize(D_WerteSu);
        for zeiger:=FSize-1 downto 0 do begin
          seek(D_WerteSu, zeiger);
          read(D_WerteSu, WerteSu);
          if WerteSu[Kanal].Status <> NUL then begin
            RefNr:=RefNr_alt + FSize - zeiger;
            tbLWert.AppendRecord ([RefNr,'','',WerteSu[Kanal].Wert,WerteSu[Kanal].Status]);
          end;
          Application.ProcessMessages;
        end;
      finally
        tbLWert.Close;
      end;
    end; { for Kanal }
  end; { if FileSize (D_Satz) }

  CloseFile(D_WerteSu);
  CloseFile(D_WerteAna);
  CloseFile(D_WerteImp);
  CloseFile(D_Satz);

  { Zwischendateien löschen: }
  if Loeschen then begin
    DeleteFile (DSatzName);
    DeleteFile (DWerteImpName);
    DeleteFile (DWerteAnaName);
    DeleteFile (DWerteSuName);
  end;  
end;


{ TTbLMeldung }

{------------------------------------------------------------------}
constructor TTbLMeldung.Create (APath: TFileName; AKennung: string);
{------------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  Kennung:=AKennung;
  tbLMeldung:=TTable.create(nil);
  tbLMeldung.DataBaseName:=Path;
  tbLMeldung.TableName:='';
end;

{------------------------------}
destructor TTbLMeldung.Destroy;
{------------------------------}
begin
  tbLMeldung.Free;
  inherited Destroy;
end;

{-------------------------------------}
procedure TTbLMeldung.CreateTbLMeldung;
{-------------------------------------}
{ Meldungstabelle anlegen }
begin
  with tbLMeldung.FieldDefs do begin
    Clear;
    Add(C_TfLMeld_MeldNr, ftInteger, 0, false);
    Add(C_TfLMeld_K_Datum, ftDate, 0, false);
    Add(C_TfLMeld_K_Zeit, ftTime, 0, false);
    Add(C_TfLMeld_G_Datum, ftDate, 0, false);
    Add(C_TfLMeld_G_Zeit, ftTime, 0, false);
    Add(C_TfLMeld_MeldStatus, ftString, 5, false);
    Add(C_TfLMeld_ZeitDiff, ftInteger, 0, false);
    Add(C_TfLMeld_LAKSNr, ftSmallInt, 0, false);
    Add(C_TfLMeld_Plus1_K, ftString, 24, false);
    Add(C_TfLMeld_Plus1_G, ftString, 24, false);
    Add(C_TfLMeld_Plus2_K, ftString, 24, false);
    Add(C_TfLMeld_Plus2_G, ftString, 24, false);
    Add(C_TfLMeld_Plus3_K, ftString, 24, false);
    Add(C_TfLMeld_Plus3_G, ftString, 24, false);
  end;
  tbLMeldung.IndexDefs.Clear;                                                  { kein Index }
  tbLMeldung.CreateTable;
end;

{------------------------------------------------------------------------}
function TTbLMeldung.GetLetztDatumZeit(var DatumZeit: TDateTime): boolean;
{------------------------------------------------------------------------}
{ ermittelt Datum und Zeit des letzten Eintrags der Meldungstabelle;
  Rückgabe: Datum/Zeit des letzten Eintrags
  Ergebnis: false, wenn noch keine Daten vorhanden sind }
var
 Idx: integer;
begin
  Result:=false;
  DatumZeit:=0;
  Idx:=Get_LAKSDatenIndex(Path, Kennung, false);         { Index für Satz-Tabelle ermitteln }
  if Idx <= 0 then exit;                                         { noch kein Index vergeben }

  tbLMeldung.TableName:= C_TbLMeldung + Format ('%.4d.Db', [Idx]);
  if not tbLMeldung.Exists then exit;                             { Tabelle nicht vorhanden }
  tbLMeldung.Open;
  try
    tbLMeldung.Last;
    if not tbLMeldung.Bof then begin                     { wenn mind. 1 Datensatz enthalten }
      if not tbLMeldung.FieldByName (C_TfLMeld_G_Datum).IsNull then
        DatumZeit:=tbLMeldung.FieldByName (C_TfLMeld_G_Datum).AsDateTime +
                   tbLMeldung.FieldByName (C_TfLMeld_G_Zeit).AsDateTime
      else
        DatumZeit:=tbLMeldung.FieldByName (C_TfLMeld_K_Datum).AsDateTime +
                   tbLMeldung.FieldByName (C_TfLMeld_K_Zeit).AsDateTime;
      Result:=true;
    end;
  finally
    tbLMeldung.Close;
  end;
end;

{-----------------------------------------------------------------------------------------}
Procedure TTbLMeldung.KonvPlusmeldungen(MeldNr: integer; Zustand: string; PlusMeld: string;
                                        var Plus1_K: string; var Plus1_G: string;
                                        var Plus2_K: string; var Plus2_G: string;
                                        var Plus3_K: string; var Plus3_G: string);
{-----------------------------------------------------------------------------------------}
{ Plus-Meldungen konvertieren (zusätzliche meldungsspezifische Informationen) }
var
  PlusZaehler: integer;
  S1: string;
  P1,P2,P3: string;

begin
  Plus1_K:='';
  Plus1_G:='';
  Plus2_K:='';
  Plus2_G:='';
  Plus3_K:='';
  Plus3_G:='';

  PlusZaehler:=0;
  while PlusMeld <> '' do begin
    inc(Pluszaehler);
    S1:=F_Zerlegen(Plusmeld,US);
    case MeldNr of
      10,24:                                              { Variablen-, Parameter-Änderung }
             case Pluszaehler of
               1: begin
                    P1:=Copy(S1,1,6);
                    P2:=Copy(S1,7,length(S1));
                    if Zustand = C_kommt then begin
                      Plus1_K:=P1;
                      Plus2_K:=P2;
                    end
                    else begin
                      Plus1_G:=P1;
                      Plus2_G:=P2;
                    end;
                  end;
               2: if Zustand = C_kommt then
                    Plus3_K:=S1
                  else
                    Plus3_G:=S1;
             end;

       1001:                                                          { Maximumumschaltung }
             case Pluszaehler of
               1: begin
                    P1:=Copy(S1,1,2);
                    P2:=Copy(S1,3,1);
                    P3:=Copy(S1,4,length(S1));
                    if Zustand = C_kommt then begin
                      Plus1_K:=P1;
                      Plus2_K:=P2;
                      Plus3_K:=P3;
                    end
                    else begin
                      Plus1_G:=P1;
                      Plus2_G:=P2;
                      Plus3_G:=P3;
                    end;
                  end;
             end;

       1013:                                                                { ZSP-Funktion }
             case Pluszaehler of
               1: if Zustand = C_kommt then
                    Plus1_K:=S1                                       { Funktion: A, B ... }
                  else
                    Plus1_G:=S1;
               2: if Zustand = C_kommt then begin
                    if Plus1_K = 'B' then
                      Plus2_K:=BinStrToHexStr(S1)            { Abschaltsp.: 0101..0 -> Hex }
                    else
                      Plus2_K:=S1;
                  end
                  else begin
                    if Plus1_G = 'B' then
                      Plus2_G:=BinStrToHexStr(S1)            { Abschaltsp.: 0101..0 -> Hex }
                    else
                      Plus2_G:=S1;
                  end;
             end;

       1022:                                      { frei zuweisbarer Binäreingang schaltet }
       { Ausgangsnummer, Zuweisung, Konditionszeichen zusammen in Plus-Meldung 1 !}
             case Pluszaehler of
               1: if Zustand = C_kommt then
                    Plus1_K:=S1
                  else
                    Plus1_G:=S1;
             2,3: begin
                    if Zustand = C_kommt then
                      Plus1_K:=Plus1_K + S1
                    else
                      Plus1_G:=Plus1_G + S1;
                  end;
               4: if Zustand = C_kommt then
                    Plus2_K:=S1
                  else
                    Plus2_G:=S1;
               5: if Zustand = C_kommt then
                    Plus3_K:=S1
                  else
                    Plus3_G:=S1;
             end;

         29,                                                               { Fehler am Bus }
       1002,                                                          { Abwurfsperrkontakt }
       1003,                                                                 { Laufkontakt }
       1004,                                                               { Impulsausfall }
       1005,                                                         { Überlauf Meßperiode }
       1006,                                                         { Überlauf Arbeit-Tag }
       1007,                                                       { Überlauf Arbeit-Monat }
       1008,                                                        { Überlauf Arbeit-Jahr }
       1014,                                                             { Energieband neu }
       1015,                                                            { Abweichung S2/S3 }
       1016,                                                                    { Überlast }
       1018,                                                        { Prioritätenliste neu }
       1020,                                                 { ZSP Maximumnummer-Vergleich }
       1021,                                              { Signal, wenn Last abgeschaltet }
       1023,                                                           { Generatorregelung }
       2000,                                                  { externe Meldung (Nr. > 16) }
       7001..7999:                                                            { Lastabwurf }
             case Pluszaehler of
               1: if Zustand = C_kommt then
                    Plus1_K:=S1
                  else
                    Plus1_G:=S1;
               2: if Zustand = C_kommt then
                    Plus2_K:=S1
                  else
                    Plus2_G:=S1;
               3: if Zustand = C_kommt then
                    Plus3_K:=S1
                  else
                    Plus3_G:=S1;
             end;
    end;  { case }
  end;  { while }
end;

{---------------------------------------------------------------------------------}
procedure TTbLMeldung.WriteMeldungToTable (AMeldNr: integer; ADatumZeit: TDateTime;
                                           AStatus: string; ALaksNr: integer;
                                           APlus1_K: string; APlus1_G: string;
                                           APlus2_K: string; APlus2_G: string;
                                           APlus3_K: string; APlus3_G: string);
{---------------------------------------------------------------------------------}
{ Meldung in Tabelle eintragen }
var
  Zeitdifferenz: integer;
  TempMeldNr: integer;
  TempK_DatumZeit: TDateTime;
  TempStatus: string;
  TempPlus1_K: string;

begin
  if AStatus = C_geht then begin              { Geht-Meldung sucht zugehörige Kommt-Meldung }
    tbLMeldung.Last;                                                      { letzter Eintrag }
    while not tbLMeldung.Bof do begin                           { Tabelle vom Ende ab lesen }
      TempMeldNr:=tbLMeldung.FieldByName(C_TfLMeld_MeldNr).AsInteger;
      TempK_DatumZeit:=tbLMeldung.FieldByName(C_TfLMeld_K_Datum).AsDateTime +
                       tbLMeldung.FieldByName(C_TfLMeld_K_Zeit).AsDateTime;
      TempStatus:=tbLMeldung.FieldByName(C_TfLMeld_MeldStatus).AsString;
      TempPlus1_K:=tbLMeldung.FieldByName(C_TfLMeld_Plus1_K).AsString;
      if (AMeldNr = TempMeldNr) AND (TempStatus = C_Kommt) then begin
        if ((AMeldNr = 1001) AND (TempPlus1_K = APlus1_G)) OR (AMeldNr <> 1001) then begin
          Zeitdifferenz:=GetTimeDiffInSec (TempK_DatumZeit, ADatumZeit);
          tbLMeldung.Edit;
          tbLMeldung.FieldByName(C_TfLMeld_G_Datum).AsDateTime:=Int(ADatumZeit);
          tbLMeldung.FieldByName(C_TfLMeld_G_Zeit).AsDateTime:=Frac(ADatumZeit);
          tbLMeldung.FieldByName(C_TfLMeld_MeldStatus).AsString:=C_Ok;
          tbLMeldung.FieldByName(C_TfLMeld_ZeitDiff).AsInteger:=ZeitDifferenz;
          tbLMeldung.FieldByName(C_TfLMeld_Plus1_G).AsString:=APlus1_G;
          tbLMeldung.FieldByName(C_TfLMeld_Plus2_G).AsString:=APlus2_G;
          tbLMeldung.FieldByName(C_TfLMeld_Plus3_G).AsString:=APlus3_G;
          tbLMeldung.Post;
          Break;
        end;
      end;
      tbLMeldung.Prior;
    end;  { while }
  end else                                                                  { Kommt-Meldung }
    tbLMeldung.AppendRecord ([AMeldNr,Int(ADatumZeit),Frac(ADatumZeit),nil,nil,AStatus,0,
                              ALaksNr,APlus1_K,nil,APlus2_K,nil,APlus3_K]);
end;

{---------------------------------------------------------------------------------------}
Function TTbLMeldung.KonvMeldungen (Rohdateiname: TFileName; Loeschen: boolean): integer;
{---------------------------------------------------------------------------------------}
{ Meldungsrohdaten in Meldungstabelle konvertieren;
  Übergabe: Datei mit Meldungs-Rohdaten
            Loeschen (wenn true, wird Rohdatei gelöscht)
  Ergbenis:  0 = ok
            -1 = Kein Rohfile vorhanden
            -2 = Kann Index nicht eintragen
            -4 = Rohfile enthält falsche Daten }
Const
  TerminatorSet = [RS,ETX];

var
  TabIndex : integer;
  MeldNr : integer;
  LaksNr: integer;
  Zustand: string;
  D_file: file of char;
  zeich: char;
  S1, Satz: string;
  ErstKonvSatz_suchen: boolean;
  DatumZeit: TDateTime;
  LetztDatumZeit: TDateTime;
  Plus1_K, Plus1_G,
  Plus2_K, Plus2_G,
  Plus3_K, Plus3_G: string;
  Jahr, Monat, Tag: word;
  Stunde, Minute, Sekunde: word;

begin
  Result:= 0;
  AssignFile(D_File, RohDateiname);                                          { Rohdatenfile }
  if NetReset(D_File, SizeOf(char),ForRead + DenyWrite) < 0 then begin
    Result:= -1;
    exit;
  end;

  TabIndex:=Get_LAKSDatenIndex(Path, Kennung, true);    { (neuer) Index für Meldungstabelle }
  if TabIndex <= 0 then begin                                         { kein Index vergeben }
    CloseFile(D_File);
    Result:= -2;
    exit;
  end;

  tbLMeldung.TableName:= C_TbLMeldung + Format ('%.4d.Db', [TabIndex]);
  if not tbLMeldung.Exists then
    CreateTbLMeldung;                                         { Meldungstabelle neu anlegen }
  tbLMeldung.Open;
  try
    { bereits in Tabelle vorhandene Datensätze dürfen nicht nochmal eingetragen werden: }
    tbLMeldung.Last;                                                      { letzter Eintrag }
    if tbLMeldung.Bof then begin                                             { Tabelle leer }
      ErstKonvSatz_suchen:=false;
      LetztDatumZeit:=0;                                                      { Vorbelegung }
    end
    else begin
      if not tbLMeldung.FieldByName (C_TfLMeld_G_Datum).IsNull then begin  { "Geht"-Meldung }
        ErstKonvSatz_suchen:=false;
        LetztDatumZeit:=tbLMeldung.FieldByName (C_TfLMeld_G_Datum).AsDateTime +
                        tbLMeldung.FieldByName (C_TfLMeld_G_Zeit).AsDateTime
      end
      else begin
        ErstKonvSatz_suchen:=true;
        LetztDatumZeit:=tbLMeldung.FieldByName (C_TfLMeld_K_Datum).AsDateTime +
                   tbLMeldung.FieldByName (C_TfLMeld_K_Zeit).AsDateTime;
      end;
    end;

    zeich := NUL;
    while zeich <> STX do
      Read(D_file,zeich);         { lesen bis STX ( es kommt manchmal ein Zeichen davor !) }
    Read(D_file,zeich);                                                                { M }

    if zeich <> 'M' then begin
      CloseFile(D_File);
      Result:= -4;
      exit;
    end;

    Satz:='';
    zeich:=NUL;
    while not Eof(D_file) AND (zeich <> ETX) do begin
      Read(D_file,zeich);
      if zeich in TerminatorSet then begin
        if Satz <> '' then begin                               { Meldungssatz konvertieren }
          S1:=F_Zerlegen(Satz,US);
          { Datum und Zeit: }
          Jahr:=Jahr2To4stellig (StrToInt (Copy(S1,1,2)));
          Monat:=StrToInt (Copy(S1,3,2));
          Tag:=StrToInt (Copy(S1,5,2));
          Stunde:=StrToInt (Copy(S1,7,2));
          Minute:=StrToInt (Copy(S1,9,2));
          Sekunde:=StrToInt (Copy(S1,11,2));
          DatumZeit:=EncodeDate (Jahr, Monat, Tag) + EncodeTime (Stunde, Minute, Sekunde, 0);

          LaksNr:=StrToInt (S1[13]);                                          { Laksnummer }
          MeldNr:=StrToInt (Copy(S1,14,4));                               { Meldungsnummer }
          if UpCase(S1[18]) = 'G' then                                   { Meldungszustand }
            Zustand:=C_geht
          else
            Zustand:=C_kommt;

          { Plusmeldungen konvertieren: }
          KonvPlusmeldungen (MeldNr, Zustand, Satz,
                             Plus1_K, Plus1_G, Plus2_K, Plus2_G, Plus3_K, Plus3_G);

          if ErstKonvSatz_suchen AND (CmpDateTime(DatumZeit, LetztDatumZeit) <= 0) then begin
           { AND (LetztMeldDaten.Nummer = MeldNr) then begin  }
           { keine Abfrage mehr nach der letzten Meldungsnummer, sondern Meldungszeit }
            if CmpDateTime(DatumZeit, LetztDatumZeit) = 0 then begin
              Satz:='';
              Continue; { Alle Meldungen mit gleichem Datum, Zeit müssen abgearbeitet werden }
            end;
            Satz:='';
            ErstKonvSatz_suchen:=false;
            Continue;
          end;
          WriteMeldungToTable (MeldNr, DatumZeit, Zustand, LaksNr,
                               Plus1_K, Plus1_G, Plus2_K, Plus2_G, Plus3_K, Plus3_G);
        end;
        Satz:='';
        Application.ProcessMessages;
      end else
        Satz:=Satz+zeich;
    end;  { while }
  finally
    tbLMeldung.Close;
  end;

  CloseFile(D_file);
  if Loeschen then
    DeleteFile (Rohdateiname);                                          { Rohdatei löschen }
end;


{ TTbLEBand }

{----------------------------------------------------------------}
constructor TTbLEBand.Create (APath: TFileName; AKennung: string);
{----------------------------------------------------------------}
begin
  inherited Create;
  Path:=APath;
  Kennung:=AKennung;
  tbLEBand:=TTable.create(nil);
  tbLEBand.DataBaseName:=Path;
  tbLEBand.TableName:='';
end;

{---------------------------}
destructor TTbLEBand.Destroy;
{---------------------------}
begin
  tbLEBand.Free;
  inherited Destroy;
end;

{---------------------------------}
procedure TTbLEBand.CreateTbLEBand;
{---------------------------------}
{ Energiebandtabelle anlegen }
begin
  with tbLEBand.FieldDefs do begin
    Clear;
    Add(C_TfLEBand_BandNr, ftSmallint, 0, false);
    Add(C_TfLEBand_WertNr, ftSmallint, 0, false);
    Add(C_TfLEBand_V_Datum, ftDate, 0, false);
    Add(C_TfLEBand_V_Zeit, ftTime, 0, false);
    Add(C_TfLEBand_W_Datum, ftDate, 0, false);
    Add(C_TfLEBand_W_Zeit, ftTime, 0, false);
    Add(C_TfLEBand_U_Datum, ftDate, 0, false);
    Add(C_TfLEBand_U_Zeit, ftTime, 0, false);
    Add(C_TfLEBand_Leistung, ftFloat, 0, false);
  end;
  tbLEBand.IndexDefs.Clear;                                                    { kein Index }
  tbLEBand.CreateTable;
end;

{----------------------------------------------------------------------}
function TTbLEBand.GetLetztDatumZeit(var DatumZeit: TDateTime): boolean;
{----------------------------------------------------------------------}
{ ermittelt Datum und Zeit des letzten Eintrags der Energiebandtabelle;
  Rückgabe: Datum/Zeit des letzten Eintrags
  Ergebnis: false, wenn noch keine Daten vorhanden sind }
var
  Idx: integer;
begin
  Result:=false;
  DatumZeit:=0;
  Idx:=Get_LAKSDatenIndex(Path, Kennung, false);   { Index für Energiebandtabelle ermitteln }
  if Idx <= 0 then exit;                                         { noch kein Index vergeben }

  tbLEBand.TableName:= C_TbLEBand + Format ('%.4d.Db', [Idx]);
  if not tbLEBand.Exists then exit;                               { Tabelle nicht vorhanden }
  tbLEBand.Open;
  try
    { Filter auf übernommene Energiebänder: }
    tbLEBand.Filter:=C_TfLEBand_WertNr + ' = ' + IntToStr(C_EBandUebernahme);
    tbLEBand.Filtered:=true;
    tbLEBand.Last;
    if not tbLEBand.Bof then begin                       { wenn mind. 1 Datensatz enthalten }
      DatumZeit:=tbLEBand.FieldByName (C_TfLEBand_V_Datum).AsDateTime +
                 tbLEBand.FieldByName (C_TfLEBand_V_Zeit).AsDateTime;
      Result:=true;
    end;
  finally
    tbLEBand.Close;
  end;
end;


{-------------------------------------------------------------------------}
Function TTbLEBand.KonvEnergiebandaenderungen (Rohdateiname: TFileName;
                                               Loeschen: boolean): integer;
{-------------------------------------------------------------------------}
{ Energiebandrohdaten in Energiebandtabelle konvertieren;
  Übergabe: Datei mit Energiebandrohdaten
            Loeschen (wenn true, wird Rohdatei gelöscht)
  Ergbenis:  0 = ok
            -1 = Kein Rohfile vorhanden
            -2 = Kann Index nicht eintragen
            -4 = Rohfile enthält falsche Daten }
Const
  CMaxEnergiebaender = 8;

type
  TEBand = record
             WertNummer: integer;
             V_DateTime: TDateTime;
             W_DateTime: TDateTime;
             U_DateTime: TDateTime;
             Wert: double;
           end;

var
  TabIndex : integer;
  W_DatumZeit: TDateTime;
  U_DatumZeit: TDateTime;
  V_DatumZeit: TDateTime;
  W_DatumZeit_Vorgabe: TDateTime;
  BandNr: integer;
  WertNr: integer;            { 0: Energieband-Vorgabe; 1: vorgenommene Energiebandänderung }
  Leistung: double;
  D_file: file of char;
  zeich: char;
  S, Satz: string;
  Vorgaben: boolean;
  Faktor: integer;
  Kommastellen: byte;
  i: integer;
  EBand: TEBand;
  EBandFeld: array[0..CMaxEnergiebaender] of TEBand;
  RestSec: integer;
  Len: integer;
  EBandItem: TEBandItem;
  EBandListe: TEBandListe;              { zum zeitlichen Sortieren der Energieband-Vorgaben }
  Jahr, Monat, Tag: word;
  Stunde, Minute, Sekunde: word;

begin
  AssignFile(D_File, RohDateiname);                                          { Rohdatenfile }
  if NetReset(D_File, SizeOf(char),ForRead + DenyWrite) < 0 then begin
    Result:=-1;
    exit;
  end;

  zeich := NUL;
  while zeich <> STX do
    Read(D_file,zeich);           { lesen bis STX ( es kommt manchmal ein Zeichen davor !) }
  Read(D_file,zeich);                                                                  { J }

  if zeich <> 'J' then begin
    CloseFile(D_File);
    Result:= -4;
    exit;
  end;

  TabIndex:=Get_LAKSDatenIndex(Path, Kennung, true); { (neuer) Index für Energiebandtabelle }
  if TabIndex <= 0 then begin                                         { kein Index vergeben }
    CloseFile(D_File);
    Result:= -2;
    exit;
  end;

  tbLEBand.TableName:= C_TbLEBand + Format ('%.4d.Db', [TabIndex]);
  if not tbLEBand.Exists then
    CreateTbLEBand;                                        { Energiebandtabelle neu anlegen }
  tbLEBand.Open;
  try
    { Vorgaben aus Tabelle löschen: Filter auf Energiebandvorgaben }
    tbLEBand.Filter:=C_TfLEBand_WertNr + ' = ' + IntToStr(C_EBandVorgabe);
    tbLEBand.Filtered:=true;
    while not tbLEBand.Eof do
      tbLEBand.Delete;
    tbLEBand.Filtered:=false;

    Satz:='';
    zeich:=NUL;
    Vorgaben:=false;
    EBandFeld[1].WertNummer:=-1;                           { Initialisierungs-Kennzeichnung }

    Faktor:=0;
    W_DatumZeit:=0;
    Kommastellen:=0;

    EBandListe:=TEBandListe.Create;
    try
      while not Eof(D_file) AND (zeich <> ETX) do begin
        Read(D_file,zeich);
        if (zeich = US) OR (zeich = RS) OR (zeich = ETX) then begin
          if Satz <> '' then begin                           { Energiebandsatz konvertieren }
            case Length(Satz) of
              12, 19: begin      { Kopf für übernommene Energiebänder (mit/ohne Einheit, KS }
                    { gemerkte Daten in Tabelle schreiben: }
                    if EBandFeld[1].WertNummer <> -1 then begin
                      for i:=1 to CMaxEnergiebaender do begin
                        with EBandFeld[i] do begin
                          if U_DateTime > 0 then { U_DateTime = 0: Energieband wird nie wirksam ! }
                            tbLEBand.AppendRecord([i, WertNummer,
                                                   Int(V_DateTime), Frac(V_DateTime),
                                                   Int(W_DateTime), Frac(W_DateTime),
                                                   Int(U_DateTime), Frac(U_DateTime),
                                                   Wert]);
                        end;
                      end;
                    end;

                    { neue Daten : }
                    if Length (Satz) = 19 then begin                   { vollständiger Kopf }
                      with EBand do begin       { Vorbelegung für nicht aktives Energieband }
                        WertNummer:=C_EBandUebernahme;

                        { W_Datum und W_Zeit: }
                        Jahr:=Jahr2To4stellig (StrToInt (Copy(Satz,1,2)));
                        Monat:=StrToInt (Copy(Satz,3,2));
                        Tag:=StrToInt (Copy(Satz,5,2));
                        Stunde:=StrToInt (Copy(Satz,7,2));
                        Minute:=StrToInt (Copy(Satz,9,2));
                        Sekunde:=StrToInt (Copy(Satz,11,2));
                        W_DateTime:=EncodeDate (Jahr, Monat, Tag) + EncodeTime (Stunde, Minute, Sekunde, 0);

                        { V_Datum, V_Zeit: der Zeitpunkt der Eingabe interessiert nicht ! }
                        V_DateTime:=W_DateTime;

                        { U_Datum, U_Zeit: Vorbelegung für "keine EBand-Änderung" }
                        U_DateTime:=0;

                        Wert:=0;
                      end;
                      { alle Energiebänder als inaktiv vorbelegen: }
                      for i:=1 to CMaxEnergiebaender do
                        EBandFeld[i]:=EBand;

                      S:=UpperCase(StrFilter(Copy(Satz,13,6),' '));               { Einheit }
                      Faktor:=1;
                      if      (S = 'KW') OR (S = 'KVAR') then Faktor:=1000
                      else if (S = 'MW') OR (S = 'MVAR') then Faktor:=1000000
                      else if (S = 'GW') OR (S = 'GVAR') then Faktor:=1000000000;

                      Kommastellen:=StrToInt (Satz[19]);                     { Kommastellen }
                    end else                            { unvollständiger Kopf -> verwerfen }
                      EBandFeld[1].WertNummer:=-1;         { Initialisierungs-Kennzeichnung }
                  end;

              24, 25: { Kopf für zukünftige Energiebänder, Länge=25 bei neg. Restzeit (Minuszeichen) }
                  begin
                    { gemerkte Daten in Tabelle schreiben: }
                    if EBandFeld[1].WertNummer <> -1 then begin
                      for i:=1 to CMaxEnergiebaender do begin
                        with EBandFeld[i] do begin
                          if U_DateTime > 0 then  { U_Date = 0: Energieband wird nie wirksam ! }
                            tbLEBand.AppendRecord([i, WertNummer,
                                                   Int(V_DateTime), Frac(V_DateTime),
                                                   Int(W_DateTime), Frac(W_DateTime),
                                                   Int(U_DateTime), Frac(U_DateTime),
                                                   Wert]);
                        end;
                      end;
                    end;

                    Vorgaben:=true;

                    { W_Datum und W_Zeit: }
                    Jahr:=Jahr2To4stellig (StrToInt (Copy(Satz,2,2)));
                    Monat:=StrToInt (Copy(Satz,4,2));
                    Tag:=StrToInt (Copy(Satz,6,2));
                    Stunde:=StrToInt (Copy(Satz,8,2));
                    Minute:=StrToInt (Copy(Satz,10,2));
                    Sekunde:=StrToInt (Copy(Satz,12,2));
                    W_DatumZeit:=EncodeDate (Jahr, Monat, Tag) + EncodeTime (Stunde, Minute, Sekunde, 0);

                    Len:=length(Satz);
                    S:=Copy(Satz,14,Len-20);    { Restzeit bis zum nächsten Meßperiodenende }
                    RestSec:=StrToInt(S);

                    { W_Datum, W_Zeit: RestSec aufaddieren }
                    { -> W_Datum, W_Zeit bestimmt den frühestmöglichen Zeitpunkt zu dem die
                         Vorgaben wirksam werden können }
                    W_DatumZeit:=W_DatumZeit + (RestSec/SecsPerDay);

                    S:=UpperCase(StrFilter(Copy(Satz,Len-6,6),' '));              { Einheit }
                    Faktor:=1;
                    if      (S = 'KW') OR (S = 'KVAR') then Faktor:=1000
                    else if (S = 'MW') OR (S = 'MVAR') then Faktor:=1000000
                    else if (S = 'GW') OR (S = 'GVAR') then Faktor:=1000000000;

                    Kommastellen:=StrToInt(Satz[Len]);                       { Kommastellen }
                  end;
            else                                            { Daten der Energiebandänderung }
              BandNr:=StrToInt(Satz[2]);                                       { Bandnummer }

              { U_Datum und U_Zeit: }
              Jahr:=Jahr2To4stellig (StrToInt (Copy(Satz,15,2)));
              Monat:=StrToInt (Copy(Satz,17,2));
              Tag:=StrToInt (Copy(Satz,19,2));
              U_DatumZeit:=EncodeDate (Jahr, Monat, Tag);
              Stunde:=StrToInt (Copy(Satz,21,2));
              if Stunde = 24 then begin                      { "24-Uhr" -> "0-Uhr", Nachtag }
                Stunde:=0;
                U_DatumZeit:=U_DatumZeit + 1;
              end;
              Minute:=StrToInt (Copy(Satz,23,2));
              Sekunde:=StrToInt (Copy(Satz,25,2));
              U_DatumZeit:=U_DatumZeit + EncodeTime (Stunde, Minute, Sekunde, 0);

              S:=Copy(Satz,27,7);                                                { Leistung }
              Leistung:=StrToInt(S);
              for i:=1 to Kommastellen do
                Leistung:=Leistung / 10;
              Leistung:=Leistung * Faktor;                            { Tabellenwert in W ! }

              if Vorgaben then begin
                WertNr:=C_EBandVorgabe;
                { W_Datum_Vorgabe und W_Zeit_Vorgabe: }
                Jahr:=Jahr2To4stellig (StrToInt (Copy(Satz,3,2)));
                Monat:=StrToInt (Copy(Satz,5,2));
                Tag:=StrToInt (Copy(Satz,7,2));
                Stunde:=StrToInt (Copy(Satz,9,2));
                Minute:=StrToInt (Copy(Satz,11,2));
                Sekunde:=StrToInt (Copy(Satz,13,2));
                W_DatumZeit_Vorgabe:=EncodeDate (Jahr, Monat, Tag) + EncodeTime (Stunde, Minute, Sekunde, 0);

                if CmpDateTime(W_DatumZeit_Vorgabe,W_DatumZeit) < 0 then
                  W_DatumZeit_Vorgabe:=W_DatumZeit;

                { V_Datum, V_Zeit: der Zeitpunkt der Eingabe interessiert nicht ! }
                V_DatumZeit:=W_DatumZeit_Vorgabe;

                { Vorgaben zeitlich sortieren: }
                if U_DatumZeit > 0 then begin { U_Datum = 0: Energieband wird nie wirksam ! }
                  EBandItem:=TEBandItem.Create (BandNr, WertNr,
                                                V_DatumZeit, W_DatumZeit_Vorgabe, U_DatumZeit,
                                                Leistung);
                  EBandListe.Add (EBandItem);
                  EBandListe.Sort (W_DateTimeCompare);                { nach W_DT sortieren }
                end;
              end
              else begin
                { in Puffer merken: }
                with EBandFeld[BandNr] do begin
                  U_DateTime:=U_DatumZeit;
                  Wert:=Leistung;
                end;
              end;
            end;  { case }
          end;  { if Satz }
          Satz:='';
          Application.ProcessMessages;
        end else
          Satz:=Satz+zeich;
      end;

      { zeitlich sortierte Vorgaben in Tabelle schreiben: }
      for i:=0 to EBandListe.Count-1 do begin
        EBandItem:=TEBandItem(EBandListe.Items [i]);
        with EBandItem do
          tbLEBand.AppendRecord([BandNummer, WertNummer,
                                 Int(V_DT), Frac(V_DT),
                                 Int(W_DT), Frac(W_DT),
                                 Int(U_DT), Frac(U_DT),
                                 Wert]);
      end;
    finally
      EBandListe.Free;
    end;
  finally
    tbLEBand.Close;
  end;

  Result := 0;
  CloseFile(D_File);
  if Loeschen then
    DeleteFile (Rohdateiname);                                          { Rohdatei löschen }
end;

end.


