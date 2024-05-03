{******************************************************************************}
{* Unit: MRG-Konfigurationsdaten aus ASCII-Dateien lesen                      *}
{* 13.01.2003 WW  Neu                                                         *}
{* 06.08.2021 WW  Erweiterungen zum Laden und Suchen in Listen                *}
{* 11.02.2022 WW  Modbus-Register von MRG-Archiven (Neue Datei MBAbruf.dat)   *}
{******************************************************************************}
Unit MResMrg;

INTERFACE

Uses
  Forms, SysUtils, Contnrs, WStrUtils, WChars, WResConst, WStream, ModbusUtil;

const
  { Dateinamen }

  CResMrgDef      = 'MrgDef.dat';
  CResMrgAbruf    = 'MrgAbruf.dat';
  CResMrgKonv     = 'MrgKonv.dat';
  CResMrgInfo     = 'MrgInfo.dat';
  CResMrgTypGasX  = 'MrgTypGasX.dat';
  CResMrgVersion  = 'MrgVersion.dat';
  CResMrgKanalBit = 'MrgKanalBit.dat';
  CResMBAbruf     = 'MBAbruf.dat';  // 11.02.2022, WW

type

  { allgemeine MRG-spezifische Daten }

  TMrgDefData = record
    MrgTyp: integer;                { Gerätetyp }
    MrgName: string;                { Bezeichnung }
    Kommandogruppe: Integer;        { Index für Abrufkommando }
    MesswertKanaele: Integer;       { Länge der Kanalmaske für Messwertabruf }
    Zaehlerkanaele: Integer;        { Länge der Kanalmaske für Tagessatzabruf }
    AbrufTagessatz: Boolean;        { spezieller Tagesabruf notwendig }
    AbrufPruefSatz: Boolean;        { Prüfungssätze vorhanden }
    AbrufParameter: string;         { Parameterabruf notwendig;
                                      ab 08.08.2014 String, bisher Boolean }
    Jahreswechsel: Boolean;         { MRG kann Jahreswechsel beim Abruf verarbeiten }
    Datumformat: string;            { Formatstring für Parameter "Datum" }
    Zeitformat: string;             { Formatstring für Parameter "Zeit" }
    RpReset: Boolean;               { Rundpuffer-Reset möglich }
    Infobefehl: string;             { Infobefehl-String - liefert Kennung, Fabriknummer etc. }
    ModemAbrufgruppe: integer;      { FUP/Modem-Abrufprotokolle }
    AnzahlKanaele: integer;         { Anzahl der Kanäle }
    AnalogKanal_von: integer;       { Nummer des niedrigsten Analogkanal }
    AnalogKanal_bis: integer;       { Nummer des höchsten Analogkanal }
    Eingangszaehler: boolean;       { Zählerstandskanäle enthalten Eingangszähler }
    Kontrollzaehler: boolean;       { Zählerstandskanäle enthalten Kontrollzähler }
    AnzahlZaehlerkanaele: integer;  { Anzahl der Zählerstandskanäle }
    AnalogKanal_AufzMax: integer;   { Aufzeichnungsmaximum der Analogkanäle }
    AbrufMeldung: Boolean;          { Meldungen können abgerufen werden }
    Analogkanal_StromberOffset: boolean;  { Strombereich bei Berechnung physikalischer Analogwerte berücksichtigen }
    Schnittstellenparameter_seriell: string;  { Schnittstellenparameter für serielle Auslesung }
  end;

  { Objekt für allgemeine MRG-spezifische Daten }

  TMrgDefDataObj = class (TObject)
    Data: TMrgDefData;
  public
    procedure SetData (AData: TMrgDefData);
  end;

  { Liste für allgemeine MRG-spezifische Daten }

  TMrgDefKonfigList = class(TObjectList)
  public
    function LoadFromKonfigFile (Pfad: string): boolean;
    function FindMrgDefData (AMrgTyp: integer; var MrgDefData: TMrgDefData): boolean;
  end;

  { MRG-spezifische Daten für Datenabruf-Befehle }

  TMrgAbrufData = record
    Kommandotyp: string;
    Gruppe: integer;
    AbrufKommando: string;
    AbrufKommando_alle_Daten: string;
  end;

  { Objekt für MRG-spezifische Daten für Datenabruf-Befehle }

  TMrgAbrufDataObj = class (TObject)
    Data: TMrgAbrufData;
  public
    procedure SetData (AData: TMrgAbrufData);
  end;

  { Liste für MRG-spezifische Daten für Datenabruf-Befehle }

  TMrgAbrufKonfigList = class(TObjectList)
  public
    function LoadFromKonfigFile (Pfad: string): boolean;
    function LoadFromList_ByKommandoTypGruppe (AKommandotyp: char;
      AKommandogruppe: integer; ListSrc: TMrgAbrufKonfigList): boolean;
  end;

  { MRG-spezifische Daten für Datenkonvertierung }

  TMrgKonvData = record
    MrgTyp: integer;
    KonvGruppe: Integer;
    MeldKonvGruppe: Integer;
    MNrParaStart: string;
    MNrParameter: string;
    MeldungsGruppe: Integer;
    ParameterGruppe: Integer;
    ParaKonvGruppe: Integer;
    MeldGeraeteart: string;  // 04.06.2009, WW
  end;

  { Objekt für MRG-spezifische Daten für Datenkonvertierung }

  TMrgKonvDataObj = class (TObject)
    Data: TMrgKonvData;
  public
    procedure SetData (AData: TMrgKonvData);
  end;

  { Liste für MRG-spezifische Daten für Datenkonvertierung }

  TMrgKonvKonfigList = class(TObjectList)
  public
    function LoadFromKonfigFile (Pfad: string): boolean;
    function FindMrgKonvData (AMrgTyp: integer; var MrgKonvData: TMrgKonvData): boolean;
  end;

  { Dateninhalte der Antwort auf Infobefehl }

  TMrgInfoData = record
    MrgTyp: integer;
    Kennung: string;
    FabrikNr: string;
    TypNr: string;
    PNr_Kennung: string;
    PNr_FabrikNr: string;
    PNr_TypNr: string;
    PNr_Datum: string;
    PNr_Zeit: string;
  end;

  { Objekt für Dateninhalte der Antwort auf Infobefehl }

  TMrgInfoDataObj = class (TObject)
    Data: TMrgInfoData;
  public
    procedure SetData (AData: TMrgInfoData);
  end;

  { Liste für Dateninhalte der Antwort auf Infobefehl }

  TMrgInfoKonfigList = class(TObjectList)
  public
    function LoadFromKonfigFile (Pfad: string): boolean;
    function FindMrgInfoData (AMrgTyp: integer; var MrgInfoData: TMrgInfoData): boolean;
  end;

  { MRG-spezifische Kanalbit-Daten }

  TMrgKanalBitData = record
    MrgTyp: integer;
    Kommandotyp: string;
    Kanalbitmaske: string;
  end;

  { Objekt für MRG-spezifische Kanalbit-Daten }

  TMrgKanalBitDataObj = class (TObject)
    Data: TMrgKanalBitData;
  public
    procedure SetData (AData: TMrgKanalBitData);
  end;

  { Liste für MRG-spezifische Kanalbit-Daten }

  TMrgKanalBitKonfigList = class(TObjectList)
  public
    function LoadFromKonfigFile (Pfad: string): boolean;
  end;

  { Objekt für Kanal-Bitmaskenzuordnungsliste }

  TMrgKanalBitMaskDataObj = class (TObject)
    Kanal: integer;
  public
    procedure SetData (AKanal: integer);
  end;

  { Liste für Kanal-Bitmaskenzuordnung in MRG-Datenabruf-Befehle/Antworten }

  TMrgKanalBitMaskList = class(TObjectList)
  public
    function LoadFromKonfigList_ByMrgKommandoTyp (AMrgTyp: integer;
      AKommandotyp: string; ListSrc: TMrgKanalBitKonfigList): boolean;
  end;

  { Record für Gas-X-Gerätetyp-Konfigurationsdaten }

  TMrgTypGasXData = record
    MrgTyp_GasX: integer;
    MrgTyp_Wieser: integer;
  end;

  { Objekt für Gas-X-Gerätetyp-Konfigurationsdaten }

  TMrgTypGasXDataObj = class (TObject)
    Data: TMrgTypGasXData;
  public
    procedure SetData (AData: TMrgTypGasXData);
  end;

  { Liste für Gas-X-Gerätetyp-Konfigurationsdaten }

  TMrgTypGasXKonfigList = class(TObjectList)
  public
    function LoadFromKonfigFile (Pfad: string): boolean;
    function FindMrgTypGasXData (AMrgTyp_GasX: integer;
      var MrgTyp_Wieser: integer): boolean;
  end;

  { Record mit Informationen für Modbusregister-Abruf und -Konvertierung }

  TMBRegisterDef = record
    StartAdresse: word;
    Typ: string;
    AnzahlBytes: integer;
    FktCode: byte;
    KanalDef: string;  // Wico-Kanal-Definition: 'Zahl' = Kanal-Nr.,
                       //   Konstanten C_MBKanalDef_... (Datum/Zeit, Status-Information etc.)
  end;

  { Objekt mit Informationen für Modbusregister-Abruf und -Konvertierung }

  TMBRegisterDefObj = class (TObject)
    Data: TMBRegisterDef;
  public
    procedure SetData (AData: TMBRegisterDef);
  end;

  { Liste mit Informationen für Modbusregister-Abruf und -Konvertierung }

  TMBRegisterDefList = class(TObjectList);

  { Modbus-Register von MRG-Archiven }

  TMBAbrufData = record
    MrgTyp: integer;
    Kommandotyp: string;
    MB_ID: integer;
    RecCount: integer;
    RecSize: integer;
    MBRegisterDefList: TMBRegisterDefList;
  end;

  { Objekt für Modbus-Register von MRG-Archiven }

  TMBAbrufDataObj = class (TObject)
    Data: TMBAbrufData;
  public
    procedure SetData (AData: TMBAbrufData);
  end;

  { Liste für Modbus-Register von MRG-Archiven }

  TMBAbrufKonfigList = class(TObjectList)
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    function LoadFromKonfigFile (Pfad: string): boolean;
    function FindMBAbrufData (AMrgTyp: integer; AKommandotyp: char; AMB_ID: integer;
      var MBAbrufData: TMBAbrufData): boolean;
  end;


function GetMrgDefData (AMrgTyp: integer; var MrgDefData: TMrgDefData;
  Pfad: string): boolean;
function GetMrgKonvData (AMrgTyp: integer; var MrgKonvData: TMrgKonvData;
  Pfad: string): boolean;
function GetMrgVersionData (AMrgVersion: string; var MrgTyp: integer;
  Pfad: string): boolean;
procedure SplitMrgInfo (MrgInfo: pchar; var Left_Count: word; Left: pchar; Right: pchar);

IMPLEMENTATION

{-----------------------------------------------------------}
function MBAbrufDataCompare (Item1, Item2: Pointer): Integer;
{-----------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TMBAbrufData-Objekten nach:
  1. MRG-Typ
  2. Kommandotyp
  3. Modbuslisten-ID
  -> in AUFSTEIGENDER Reihenfolge }
begin
  Result:=TMBAbrufDataObj (Item1).Data.MrgTyp -
          TMBAbrufDataObj (Item2).Data.MrgTyp;  { 1. Sortierkriterium: MRG-Typ }
  if Result = 0 then begin
    Result:=CompareStr (TMBAbrufDataObj (Item1).Data.Kommandotyp,
                        TMBAbrufDataObj (Item2).Data.Kommandotyp);  { 2. Sortierkriterium: Kommandotyp }
    if Result = 0 then
      Result:=TMBAbrufDataObj (Item1).Data.MB_ID -
              TMBAbrufDataObj (Item2).Data.MB_ID;  { 3. Sortierkriterium: Modbuslisten-ID }
  end;
end;


{ TMrgDefDataObj }

{----------------------------------------------------}
procedure TMrgDefDataObj.SetData (AData: TMrgDefData);
{----------------------------------------------------}
begin
  Data:=AData;
end;

{ TMrgAbrufDataObj }

{--------------------------------------------------------}
procedure TMrgAbrufDataObj.SetData (AData: TMrgAbrufData);
{--------------------------------------------------------}
begin
  Data:=AData;
end;

{ TMrgKonvDataObj }

{------------------------------------------------------}
procedure TMrgKonvDataObj.SetData (AData: TMrgKonvData);
{------------------------------------------------------}
begin
  Data:=AData;
end;

{ TMrgInfoDataObj }

{------------------------------------------------------}
procedure TMrgInfoDataObj.SetData (AData: TMrgInfoData);
{------------------------------------------------------}
begin
  Data:=AData;
end;

{ TMrgKanalBitDataObj }

{--------------------------------------------------------------}
procedure TMrgKanalBitDataObj.SetData (AData: TMrgKanalBitData);
{--------------------------------------------------------------}
begin
  Data:=AData;
end;

{ TMrgKanalBitMaskDataObj }

{----------------------------------------------------------}
procedure TMrgKanalBitMaskDataObj.SetData (AKanal: integer);
{----------------------------------------------------------}
begin
  Kanal:=AKanal;
end;

{ TMrgTypGasXDataObj }

{------------------------------------------------------------}
procedure TMrgTypGasXDataObj.SetData (AData: TMrgTypGasXData);
{------------------------------------------------------------}
begin
  Data:=AData;
end;

{ TMBRegisterDefObj }

{----------------------------------------------------------}
procedure TMBRegisterDefObj.SetData (AData: TMBRegisterDef);
{----------------------------------------------------------}
begin
  Data:=AData;
end;

{ TMBAbrufDataObj }

{------------------------------------------------------}
procedure TMBAbrufDataObj.SetData (AData: TMBAbrufData);
{------------------------------------------------------}
begin
  Data:=AData;
end;


{ TMrgDefKonfigList }

{--------------------------------------------------------------------}
function TMrgDefKonfigList.LoadFromKonfigFile (Pfad: string): boolean;
{--------------------------------------------------------------------}
{ Lädt Datei MrgDef.Dat in die Liste;
  Übergabe: Pfad zur Datei
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  MrgDefData: TMrgDefData;
  MrgDefDataObj: TMrgDefDataObj;

begin
  Result:=false;
  Clear;

  FName:=Pfad+CResMrgDef;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);        { Feld 1: MRG-Typ }
          MrgDefData.MrgTyp:=StrToIntDef (FieldStr, -1);

          if MrgDefData.MrgTyp > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            try
              FieldCount:=1;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CResTrenner);
                inc (FieldCount);
                with MrgDefData do begin
                  case FieldCount of
                     2: MrgName:=FieldStr;
                     3: Kommandogruppe:=StrToInt (FieldStr);
                     4: MesswertKanaele:=StrToInt (FieldStr);
                     5: Zaehlerkanaele:=StrToInt (FieldStr);
                     6: AbrufTagessatz:=UpperCase (FieldStr) = CResTrue;
                     7: AbrufPruefSatz:=UpperCase (FieldStr) = CResTrue;
                     8: AbrufParameter:=FieldStr;
                     9: Jahreswechsel:=UpperCase (FieldStr) = CResTrue;
                    10: Datumformat:=FieldStr;
                    11: Zeitformat:=FieldStr;
                    12: RpReset:=UpperCase (FieldStr) = CResTrue;
                    13: Infobefehl:=FieldStr;
                    14: ModemAbrufgruppe:=StrToInt (FieldStr);
                    15: AnzahlKanaele:=StrToInt (FieldStr);
                    16: AnalogKanal_von:=StrToInt (FieldStr);
                    17: AnalogKanal_bis:=StrToInt (FieldStr);
                    18: Eingangszaehler:=UpperCase (FieldStr) = CResTrue;
                    19: Kontrollzaehler:=UpperCase (FieldStr) = CResTrue;
                    20: AnzahlZaehlerkanaele:=StrToInt (FieldStr);
                    21: AnalogKanal_AufzMax:=StrToInt (FieldStr);  // neu ab 15.01.2004, WW
                    22: AbrufMeldung:=UpperCase (FieldStr) = CResTrue;  // neu ab 11.02.2004, WW
                    23: Analogkanal_StromberOffset:=UpperCase (FieldStr) = CResTrue;  // neu ab 28.09.2004, WW
                    24: Schnittstellenparameter_seriell:=FieldStr;  // neu ab 10.08.2011, WW
                  end;  { case }
                end;  { with }
              end;  { while length (S) }

              { Listenobjekt createn und in Liste einfügen: }
              MrgDefDataObj:=TMrgDefDataObj.Create;
              MrgDefDataObj.SetData (MrgDefData);
              Add (MrgDefDataObj);
            except
              Result:=false;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{----------------------------------------------------------}
function TMrgDefKonfigList.FindMrgDefData (AMrgTyp: integer;
  var MrgDefData: TMrgDefData): boolean;
{----------------------------------------------------------}
{ Sucht nach MrgDefData-Listeneintrag zu MRG-Typ;
  Übergabe: MRG-Typ
  Rückgabe: MrgDefData-Record
  Ergebnis: true, wenn Eintrag gefunden }
var
  MrgDefDataObj: TMrgDefDataObj;
  i: integer;

begin
  Result:=false;
  // Vorbelegung Rückgabe:
  with MrgDefData do begin
    MrgTyp:=0;
    MrgName:='';
    Kommandogruppe:=0;
    MesswertKanaele:=0;
    Zaehlerkanaele:=0;
    AbrufTagessatz:=false;
    AbrufPruefSatz:=false;
    AbrufParameter:='';
    Jahreswechsel:=false;
    Datumformat:='';
    Zeitformat:='';
    RpReset:=false;
    Infobefehl:='';
    ModemAbrufgruppe:=0;
    AnzahlKanaele:=0;
    AnalogKanal_von:=0;
    AnalogKanal_bis:=0;
    Eingangszaehler:=false;
    Kontrollzaehler:=false;
    AnzahlZaehlerkanaele:=0;
    AnalogKanal_AufzMax:=0;
    AbrufMeldung:=false;
    Analogkanal_StromberOffset:=false;
    Schnittstellenparameter_seriell:='';
  end;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    MrgDefDataObj:=TMrgDefDataObj (Items [i]);
    if (MrgDefDataObj.Data.MrgTyp = AMrgTyp) then begin
      MrgDefData:=MrgDefDataObj.Data;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{ TMrgAbrufKonfigList }

{----------------------------------------------------------------------}
function TMrgAbrufKonfigList.LoadFromKonfigFile (Pfad: string): boolean;
{----------------------------------------------------------------------}
{ Lädt Datei MrgAbruf.Dat in die Liste;
  Übergabe: Pfad zur Datei
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  MrgAbrufData: TMrgAbrufData;
  MrgAbrufDataObj: TMrgAbrufDataObj;

begin
  Result:=false;
  Clear;  // 06.08.2021, WW

  FName:=Pfad+CResMrgAbruf;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;  // 06.08.2021, WW
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);          { Feld 1: Kommandotyp }
          if length (FieldStr) = 1 then begin  // Kommandotyp hat Länge 1; 06.08.2021, WW
            try
              MrgAbrufData.Kommandotyp:=FieldStr;
              MrgAbrufData.Gruppe:=0;
              MrgAbrufData.AbrufKommando:='';
              MrgAbrufData.AbrufKommando_alle_Daten:='';

              FieldCount:=1;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CResTrenner);
                inc (FieldCount);
                with MrgAbrufData do begin
                  case FieldCount of
                    2: Gruppe:=StrToInt (FieldStr);
                    3: Abrufkommando:=FieldStr;
                    4: Abrufkommando_alle_Daten:=FieldStr;
                  end;  { case }
                end;  { with }
              end;  { while length (S) }

              { Listenobjekt createn und in Liste einfügen: }
              MrgAbrufDataObj:=TMrgAbrufDataObj.Create;
              MrgAbrufDataObj.SetData (MrgAbrufData);
              Add (MrgAbrufDataObj);
            except
              Result:=false;
            end;
          end;  // if length (FieldStr)
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{--------------------------------------------------------------------------------}
function TMrgAbrufKonfigList.LoadFromList_ByKommandoTypGruppe (AKommandotyp: char;
  AKommandogruppe: integer; ListSrc: TMrgAbrufKonfigList): boolean;
{--------------------------------------------------------------------------------}
{ Lädt MrgAbrufData-Einträge zu Kommandotyp und Kommandogruppe aus Quell-Liste in
  die Liste;
  Übergaben: Kommandotyp
             Kommandogruppe
             Quell-Liste
  Ergebnis: true, wenn Eintrag zu Kommandotyp/gruppe in Quell-Liste gefunden wurde }
var
  i: integer;
  MrgAbrufDataObj: TMrgAbrufDataObj;
  MrgAbrufDataSrc: TMrgAbrufData;

begin
  Result:=false;
  Clear;  // Liste leeren

  if Assigned (ListSrc) then begin
    for i:=0 to ListSrc.Count - 1 do begin
      Application.ProcessMessages;
      MrgAbrufDataSrc:=TMrgAbrufDataObj (ListSrc.Items [i]).Data;
      if (MrgAbrufDataSrc.Kommandotyp = AKommandotyp) AND
         (MrgAbrufDataSrc.Gruppe = AKommandogruppe) then begin
        Result:=true;
        { Listenobjekt createn und in Liste einfügen: }
        MrgAbrufDataObj:=TMrgAbrufDataObj.Create;
        MrgAbrufDataObj.SetData (MrgAbrufDataSrc);
        Add (MrgAbrufDataObj);
      end;
    end;
  end;
end;


{ TMrgKonvKonfigList }

{---------------------------------------------------------------------}
function TMrgKonvKonfigList.LoadFromKonfigFile (Pfad: string): boolean;
{---------------------------------------------------------------------}
{ Lädt Datei MrgKonv.Dat in die Liste;
  Übergabe: Pfad zur Datei
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  MrgKonvData: TMrgKonvData;
  MrgKonvDataObj: TMrgKonvDataObj;

begin
  Result:=false;
  Clear;

  FName:=Pfad+CResMrgKonv;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);        { Feld 1: MRG-Typ }
          MrgKonvData.MrgTyp:=StrToIntDef (FieldStr, -1);

          if MrgKonvData.MrgTyp > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            try
              FieldCount:=1;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CResTrenner);
                inc (FieldCount);
                with MrgKonvData do begin
                  case FieldCount of
                     2: Konvgruppe:=StrToIntDef (FieldStr, -1);
                     3: MeldKonvGruppe:=StrToIntDef (FieldStr, -1);
                     4: MNrParastart:=FieldStr;
                     5: MNrParameter:=FieldStr;
                     6: Meldungsgruppe:=StrToIntDef (FieldStr, -1);
                     7: Parametergruppe:=StrToIntDef (FieldStr, -1);
                     8: ParaKonvGruppe:=StrToIntDef (FieldStr, -1);
                     9: MeldGeraeteart:=FieldStr;
                  end;  { case }
                end;  { with }
              end;  { while length (S) }

              { Listenobjekt createn und in Liste einfügen: }
              MrgKonvDataObj:=TMrgKonvDataObj.Create;
              MrgKonvDataObj.SetData (MrgKonvData);
              Add (MrgKonvDataObj);
            except
              Result:=false;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{------------------------------------------------------------}
function TMrgKonvKonfigList.FindMrgKonvData (AMrgTyp: integer;
  var MrgKonvData: TMrgKonvData): boolean;
{------------------------------------------------------------}
{ Sucht nach MrgKonvData-Listeneintrag zu MRG-Typ;
  Übergabe: MRG-Typ
  Rückgabe: MrgKonvData-Record
  Ergebnis: true, wenn Eintrag gefunden }
var
  MrgKonvDataObj: TMrgKonvDataObj;
  i: integer;

begin
  Result:=false;
  // Vorbelegung Rückgabe:
  with MrgKonvData do begin
    MrgTyp:=-1;                          { Vorbelegung: keine Daten }
    KonvGruppe:=0;
    MeldKonvGruppe:=0;
    MNrParaStart:='';
    MNrParameter:='';
    MeldungsGruppe:=0;
    ParameterGruppe:=0;
    ParaKonvGruppe:=0;
    MeldGeraeteart:='';
  end;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    MrgKonvDataObj:=TMrgKonvDataObj (Items [i]);
    if (MrgKonvDataObj.Data.MrgTyp = AMrgTyp) then begin
      MrgKonvData:=MrgKonvDataObj.Data;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{ TMrgInfoKonfigList }

{---------------------------------------------------------------------}
function TMrgInfoKonfigList.LoadFromKonfigFile (Pfad: string): boolean;
{---------------------------------------------------------------------}
{ Lädt Datei MrgInfo.Dat in die Liste;
  Übergabe: Pfad zur Datei
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  MrgInfoData: TMrgInfoData;
  MrgInfoDataObj: TMrgInfoDataObj;

begin
  Result:=false;
  Clear;

  FName:=Pfad+CResMrgInfo;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);        { Feld 1: MRG-Typ }
          MrgInfoData.MrgTyp:=StrToIntDef (FieldStr, -1);

          if MrgInfoData.MrgTyp > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            try
              FieldCount:=1;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CResTrenner);
                inc (FieldCount);
                with MrgInfoData do begin
                  case FieldCount of
                    2: Kennung:=FieldStr;
                    3: FabrikNr:=FieldStr;
                    4: TypNr:=FieldStr;
                    5: PNr_Kennung:=FieldStr;
                    6: PNr_FabrikNr:=FieldStr;
                    7: PNr_TypNr:=FieldStr;
                    8: PNr_Datum:=FieldStr;
                    9: PNr_Zeit:=FieldStr;
                  end;  { case }
                end;  { with }
              end;  { while length (S) }

              { Listenobjekt createn und in Liste einfügen: }
              MrgInfoDataObj:=TMrgInfoDataObj.Create;
              MrgInfoDataObj.SetData (MrgInfoData);
              Add (MrgInfoDataObj);
            except
              Result:=false;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{------------------------------------------------------------}
function TMrgInfoKonfigList.FindMrgInfoData (AMrgTyp: integer;
  var MrgInfoData: TMrgInfoData): boolean;
{------------------------------------------------------------}
{ Sucht nach MrgInfoData-Listeneintrag zu MRG-Typ;
  Übergabe: MRG-Typ
  Rückgabe: MrgInfoData-Record
  Ergebnis: true, wenn Eintrag gefunden }
var
  MrgInfoDataObj: TMrgInfoDataObj;
  i: integer;

begin
  Result:=false;
  // Vorbelegung Rückgabe:
  with MrgInfoData do begin
    MrgTyp:=0;
    Kennung:='';
    FabrikNr:='';
    TypNr:='';
    PNr_Kennung:='';
    PNr_FabrikNr:='';
    PNr_TypNr:='';
    PNr_Datum:='';
    PNr_Zeit:='';
  end;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    MrgInfoDataObj:=TMrgInfoDataObj (Items [i]);
    if (MrgInfoDataObj.Data.MrgTyp = AMrgTyp) then begin
      MrgInfoData:=MrgInfoDataObj.Data;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{ TMrgKanalBitKonfigList }

{-------------------------------------------------------------------------}
function TMrgKanalBitKonfigList.LoadFromKonfigFile (Pfad: string): boolean;
{-------------------------------------------------------------------------}
{ Lädt Datei MrgKanalBit.Dat in die Liste;
  Übergabe: Pfad zur Datei
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  MrgKanalBitData: TMrgKanalBitData;
  MrgKanalBitDataObj: TMrgKanalBitDataObj;

begin
  Result:=false;
  Clear;

  FName:=Pfad+CResMrgKanalBit;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);        { Feld 1: MRG-Typ }
          MrgKanalBitData.MrgTyp:=StrToIntDef (FieldStr, -1);

          if MrgKanalBitData.MrgTyp > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            try
              FieldCount:=1;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CResTrenner);
                inc (FieldCount);
                with MrgKanalBitData do begin
                  case FieldCount of
                    2: Kommandotyp:=FieldStr;
                    3: Kanalbitmaske:=FieldStr;
                  end;  { case }
                end;  { with }
              end;  { while length (S) }

              { Listenobjekt createn und in Liste einfügen: }
              MrgKanalBitDataObj:=TMrgKanalBitDataObj.Create;
              MrgKanalBitDataObj.SetData (MrgKanalBitData);
              Add (MrgKanalBitDataObj);
            except
              Result:=false;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;


{ TMrgKanalBitMaskList }

{----------------------------------------------------------------------------------}
function TMrgKanalBitMaskList.LoadFromKonfigList_ByMrgKommandoTyp (
  AMrgTyp: integer; AKommandotyp: string; ListSrc: TMrgKanalBitKonfigList): boolean;
{----------------------------------------------------------------------------------}
{ Lädt MrgKanalBitMaskData-Einträge zu MRG-Typ und Kommandotyp aus Quell-Liste
  in die Liste;
  Übergaben: MRG-Typ
             Kommandotyp
             Quell-Liste
  Ergebnis: true, wenn Eintrag zu MRG-/Kommandotyp in Quell-Liste gefunden wurde }
var
  i: integer;
  MrgKanalBitDataSrc: TMrgKanalBitData;
  sKanalNr: string;
  iKanalNr: integer;
  MrgKanalBitMaskDataObj: TMrgKanalBitMaskDataObj;

begin
  Result:=false;
  Clear;  // Liste leeren

  if Assigned (ListSrc) then begin
    for i:=0 to ListSrc.Count - 1 do begin
      Application.ProcessMessages;
      MrgKanalBitDataSrc:=TMrgKanalBitDataObj (ListSrc.Items [i]).Data;
      if (MrgKanalBitDataSrc.MrgTyp = AMrgTyp) AND
         (MrgKanalBitDataSrc.Kommandotyp = AKommandotyp) then begin
        { Kanalbitmaske in Einzel-Kanäle zerlegen und in
          Kanalbitliste konvertieren:
          -> nur die für den Abruf interessierenden Kanäle (ab 1)
          -> Listen-Index = Bitposition }
        sKanalNr:=F_Zerlegen (MrgKanalBitDataSrc.Kanalbitmaske, CResKanBitTrenner);
        while length (sKanalNr) > 0 do begin
          iKanalNr:=StrToIntDef (sKanalNr, -1);
          if iKanalNr > 0 then begin  // nur interessierende Kanäle
            MrgKanalBitMaskDataObj:=TMrgKanalBitMaskDataObj.Create;
            MrgKanalBitMaskDataObj.SetData (iKanalNr);
            Add (MrgKanalBitMaskDataObj);
          end;
          sKanalNr:=F_Zerlegen (MrgKanalBitDataSrc.Kanalbitmaske, CResKanBitTrenner);
        end; { while }

        Result:=true;
        Break;
      end;
    end;
  end;
end;


{ TMrgTypGasXKonfigList }

{------------------------------------------------------------------------}
function TMrgTypGasXKonfigList.LoadFromKonfigFile (Pfad: string): boolean;
{------------------------------------------------------------------------}
{ Lädt Datei MrgTypGasX.Dat in die Liste;
  Übergabe: Pfad zur Datei
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  MrgTypGasXData: TMrgTypGasXData;
  MrgTypGasXDataObj: TMrgTypGasXDataObj;

begin
  Result:=false;
  Clear;

  FName:=Pfad+CResMrgTypGasX;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);     { Feld 1: MRG-Typ Gas-X }
          MrgTypGasXData.MrgTyp_GasX:=StrToIntDef (FieldStr, -1);

          if MrgTypGasXData.MrgTyp_GasX > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            try
              MrgTypGasXData.MrgTyp_Wieser:=StrToInt (S);  { Feld 2: MRG-Typ Wieser }

              { Listenobjekt createn und in Liste einfügen: }
              MrgTypGasXDataObj:=TMrgTypGasXDataObj.Create;
              MrgTypGasXDataObj.SetData (MrgTypGasXData);
              Add (MrgTypGasXDataObj);
            except
              Result:=false;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{-----------------------------------------------------------------------}
function TMrgTypGasXKonfigList.FindMrgTypGasXData (AMrgTyp_GasX: integer;
  var MrgTyp_Wieser: integer): boolean;
{-----------------------------------------------------------------------}
{ Sucht nach Listeneintrag mit übergebener MRG-Typnummer des Gas-X-Systems und
  liefert die entsprechende MRG-Typnummer des Wieser-Systems;
  Übergabe: MRG-Typnummer Gas-X
  Rückgabe: MRG-Typnummer Wieser
  Ergebnis: true, wenn Eintrag gefunden }
var
  MrgTypGasXDataObj: TMrgTypGasXDataObj;
  i: integer;

begin
  Result:=false;
  // Vorbelegung Rückgabe:
  MrgTyp_Wieser:=-1;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    MrgTypGasXDataObj:=TMrgTypGasXDataObj (Items [i]);
    if (MrgTypGasXDataObj.Data.MrgTyp_GasX = AMrgTyp_GasX) then begin
      MrgTyp_Wieser:=MrgTypGasXDataObj.Data.MrgTyp_Wieser;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{ TMBAbrufKonfigList }

// 11.02.2022, WW
{------------------------------------}
destructor TMBAbrufKonfigList.Destroy;
{------------------------------------}
begin
  Clear;

  inherited Destroy;
end;

{---------------------------------}
procedure TMBAbrufKonfigList.Clear;
{---------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do
    if Assigned (TMBAbrufDataObj (Items [i]).Data.MBRegisterDefList) then
      TMBAbrufDataObj (Items [i]).Data.MBRegisterDefList.Free;  { Modbusregister-Definitionsliste freigeben }

  inherited Clear;
end;

{---------------------------------------------------}
procedure TMBAbrufKonfigList.Delete(iIndex: integer);
{---------------------------------------------------}
begin
  if Assigned (TMBAbrufDataObj (Items [iIndex]).Data.MBRegisterDefList) then
    TMBAbrufDataObj (Items [iIndex]).Data.MBRegisterDefList.Free;  { Modbusregister-Definitionsliste freigeben }

  inherited Delete(iIndex);
end;

{---------------------------------------------------------------------}
function TMBAbrufKonfigList.LoadFromKonfigFile (Pfad: string): boolean;
{---------------------------------------------------------------------}
{ Lädt Datei MBAbruf.Dat in die Liste;
  Übergabe: Pfad zur Datei
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  MBAbrufData: TMBAbrufData;
  MBAbrufDataObj: TMBAbrufDataObj;
  iFktCode: byte;
  iStartAdresse: word;
  MBRegisterDef: TMBRegisterDef;
  MBRegisterDefObj: TMBRegisterDefObj;
  FieldStr_MBRegDef: string;
  FieldCount_MBRegDef: integer;
  iAnzahlRegister: word;

begin
  Result:=false;
  Clear;

  FName:=Pfad+CResMBAbruf;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);        { Feld 1: MRG-Typ }
          MBAbrufData.MrgTyp:=StrToIntDef (FieldStr, -1);

          if MBAbrufData.MrgTyp > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            try
              with MBAbrufData do begin
                MBRegisterDefList:=TMBRegisterDefList.Create;  { Modbusregister-Definitionsliste createn }
                RecSize:=0;

                iStartAdresse:=0;
                iFktCode:=0;
                FieldCount:=1;
                while length (S) > 0 do begin
                  FieldStr:=F_Zerlegen (S, CResTrenner);
                  inc (FieldCount);
                  case FieldCount of
                     2: Kommandotyp:=FieldStr;
                     3: MB_ID:=StrToInt (FieldStr);
                     4: RecCount:=StrToInt (FieldStr);
                     5: iFktCode:=StrToInt (FieldStr);
                     6: iStartAdresse:=StrToInt (FieldStr);
                  else
                    // ab Feld 7 folgen die Registerdefinitionen der Archivdatensatz-Felder
                    FieldCount_MBRegDef:=0;
                    with MBRegisterDef do begin
                      while length (FieldStr) > 0 do begin
                        FieldStr_MBRegDef:=F_Zerlegen (FieldStr, CResTrennerPipe);
                        inc (FieldCount_MBRegDef);
                        case FieldCount_MBRegDef of
                          1: Typ:=FieldStr_MBRegDef;
                          2: AnzahlBytes:=StrToInt (FieldStr_MBRegDef);
                          3: KanalDef:=FieldStr_MBRegDef;
                          // Feld 4 'Bezeichnung' wird nicht verwendet
                        end;  { case }
                      end;  { while length (FieldStr) }

                      StartAdresse:=iStartAdresse;
                      FktCode:=iFktCode;

                      // Register-Startadresse für nächstes Archivdatensatz-Feld berechnen:
                      iAnzahlRegister:=Calc_Modbus_AnzahlRegister (AnzahlBytes);
                      inc (iStartAdresse, iAnzahlRegister);

                      // Register-Größe des Datensatzes berechnen:
                      inc (RecSize, iAnzahlRegister);
                    end;  { with }

                    { Listenobjekt createn und in Modbusregister-Definitionsliste einfügen: }
                    MBRegisterDefObj:=TMBRegisterDefObj.Create;
                    MBRegisterDefObj.SetData (MBRegisterDef);
                    MBRegisterDefList.Add (MBRegisterDefObj);
                  end;  { case }
                end;  { while length (S) }
              end;  { with }

              { Listenobjekt createn und in Liste einfügen: }
              MBAbrufDataObj:=TMBAbrufDataObj.Create;
              MBAbrufDataObj.SetData (MBAbrufData);
              Add (MBAbrufDataObj);
            except
              Result:=false;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;

      // Liste nach der MRG-Typ, Kommandotyp und Modbuslisten-ID sortieren für
      // Such-Algorithmus in FindMBAbrufData:
      Sort(MBAbrufDataCompare);
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{--------------------------------------------------------------------------------}
function TMBAbrufKonfigList.FindMBAbrufData (AMrgTyp: integer; AKommandotyp: char;
  AMB_ID: integer; var MBAbrufData: TMBAbrufData): boolean;
{--------------------------------------------------------------------------------}
{ Sucht nach MBAbrufData-Listeneintrag zu MRG-Typ, Kommandotyp und Modbuslisten-ID;
  Übergabe: MRG-Typ
            Kommandotyp
            Modbuslisten-ID
  Rückgabe: MBAbrufData-Record
  Ergebnis: true, wenn Eintrag gefunden }
var
  MBAbrufDataObj: TMBAbrufDataObj;
  i: integer;

begin
  Result:=false;
  // Vorbelegung Rückgabe:
  with MBAbrufData do begin
    MrgTyp:=-1;                          { Vorbelegung: keine Daten }
    Kommandotyp:='';
    MB_ID:=0;
    RecCount:=0;
    RecSize:=0;
    MBRegisterDefList:=nil;
  end;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    MBAbrufDataObj:=TMBAbrufDataObj (Items [i]);

    // Suche nach Eintrag mit Modbuslisten-ID größer oder gleich der übergebenen:
    // -> Sortierung der Listeneinträge nach Modbuslisten-ID aufsteigend notwendig !
    if (MBAbrufDataObj.Data.MrgTyp = AMrgTyp) AND
       (MBAbrufDataObj.Data.Kommandotyp = AKommandotyp) AND
       (MBAbrufDataObj.Data.MB_ID >= AMB_ID) then begin
      MBAbrufData:=MBAbrufDataObj.Data;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{------------------------------------------------------------------------------}

{--------------------------------------------------------------------}
function GetMrgDefData (AMrgTyp: integer; var MrgDefData: TMrgDefData;
                        Pfad: string): boolean;
{--------------------------------------------------------------------}
{ Liefert typspezifische MRG-Informationen;
  Übergabe: Mrg-Typ
            Pfad zur Ressourcendatei
  Rückgabe: MRG-Informationen in MrgDefData
  Ergebnis: true, wenn Informationen in Datei gefunden und plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;

begin
  Result:=false;
  // Vorbelegung Rückgabe:
  with MrgDefData do begin
    MrgTyp:=0;
    MrgName:='';
    Kommandogruppe:=0;
    MesswertKanaele:=0;
    Zaehlerkanaele:=0;
    AbrufTagessatz:=false;
    AbrufPruefSatz:=false;
    AbrufParameter:='';
    Jahreswechsel:=false;
    Datumformat:='';
    Zeitformat:='';
    RpReset:=false;
    Infobefehl:='';
    ModemAbrufgruppe:=0;
    AnzahlKanaele:=0;
    AnalogKanal_von:=0;
    AnalogKanal_bis:=0;
    Eingangszaehler:=false;
    Kontrollzaehler:=false;
    AnzahlZaehlerkanaele:=0;
    AnalogKanal_AufzMax:=0;
    AbrufMeldung:=false;
    Analogkanal_StromberOffset:=false;
    Schnittstellenparameter_seriell:='';
  end;

  FName:=Pfad+CResMrgDef;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);        { Feld 1: MRG-Typ }
          if StrToIntDef (FieldStr, -1) = AMrgTyp then begin
            try
              MrgDefData.MrgTyp:=AMrgTyp;
              FieldCount:=1;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CResTrenner);
                inc (FieldCount);
                with MrgDefData do begin
                  case FieldCount of
                     2: MrgName:=FieldStr;
                     3: Kommandogruppe:=StrToInt (FieldStr);
                     4: MesswertKanaele:=StrToInt (FieldStr);
                     5: Zaehlerkanaele:=StrToInt (FieldStr);
                     6: AbrufTagessatz:=UpperCase (FieldStr) = CResTrue;
                     7: AbrufPruefSatz:=UpperCase (FieldStr) = CResTrue;
                     8: AbrufParameter:=FieldStr;
                     9: Jahreswechsel:=UpperCase (FieldStr) = CResTrue;
                    10: Datumformat:=FieldStr;
                    11: Zeitformat:=FieldStr;
                    12: RpReset:=UpperCase (FieldStr) = CResTrue;
                    13: Infobefehl:=FieldStr;
                    14: ModemAbrufgruppe:=StrToInt (FieldStr);
                    15: AnzahlKanaele:=StrToInt (FieldStr);
                    16: AnalogKanal_von:=StrToInt (FieldStr);
                    17: AnalogKanal_bis:=StrToInt (FieldStr);
                    18: Eingangszaehler:=UpperCase (FieldStr) = CResTrue;
                    19: Kontrollzaehler:=UpperCase (FieldStr) = CResTrue;
                    20: AnzahlZaehlerkanaele:=StrToInt (FieldStr);
                    21: AnalogKanal_AufzMax:=StrToInt (FieldStr);  // neu ab 15.01.2004, WW
                    22: AbrufMeldung:=UpperCase (FieldStr) = CResTrue;  // neu ab 11.02.2004, WW
                    23: Analogkanal_StromberOffset:=UpperCase (FieldStr) = CResTrue;  // neu ab 28.09.2004, WW
                    24: Schnittstellenparameter_seriell:=FieldStr;  // neu ab 10.08.2011, WW
                  end;  { case }
                end;  { with }
              end;  { while length (S) }
              Result:=true;
              Break;
            except
              Result:=false;
              Break;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{-----------------------------------------------------------------------}
function GetMrgKonvData (AMrgTyp: integer; var MrgKonvData: TMrgKonvData;
                         Pfad: string): boolean;
{-----------------------------------------------------------------------}
{ Liefert gerätespezifische Daten für die Datenkonvertierung;
  Übergabe: MrgTyp
            Pfad zur Ressourcendatei
  Rückgabe: MrgKonvData-Record
  Ergebnis: true, wenn Informationen in Datei gefunden und plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;

begin
  Result:=false;
  // Vorbelegung Rückgabe:
  with MrgKonvData do begin
    MrgTyp:=-1;                          { Vorbelegung: keine Daten }
    KonvGruppe:=0;
    MeldKonvGruppe:=0;
    MNrParaStart:='';
    MNrParameter:='';
    MeldungsGruppe:=0;
    ParameterGruppe:=0;
    ParaKonvGruppe:=0;
    MeldGeraeteart:='';
  end;

  FName:=Pfad+CResMrgKonv;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);        { Feld 1: MRG-Typ }
          if StrToIntDef (FieldStr, -1) = AMrgTyp then begin
            try
              MrgKonvData.MrgTyp:=AMrgTyp;
              FieldCount:=1;
              while length (S) > 0 do begin
                FieldStr:=F_Zerlegen (S, CResTrenner);
                inc (FieldCount);
                with MrgKonvData do begin
                  case FieldCount of
                     2: Konvgruppe:=StrToIntDef (FieldStr, -1);
                     3: MeldKonvGruppe:=StrToIntDef (FieldStr, -1);
                     4: MNrParastart:=FieldStr;
                     5: MNrParameter:=FieldStr;
                     6: Meldungsgruppe:=StrToIntDef (FieldStr, -1);
                     7: Parametergruppe:=StrToIntDef (FieldStr, -1);
                     8: ParaKonvGruppe:=StrToIntDef (FieldStr, -1);
                     9: MeldGeraeteart:=FieldStr;
                  end;  { case }
                end;  { with }
              end;  { while length (S) }
              Result:=true;
              Break;
            except
              Result:=false;
              Break;
            end;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{-------------------------------------------------------------------}
function GetMrgVersionData (AMrgVersion: string; var MrgTyp: integer;
                            Pfad: string): boolean;
{-------------------------------------------------------------------}
{ Liefert zur übergebenen MRG-Version die entsprechende MRG-Typnummer;
  Übergabe: MRG-Version (wie vom MRG im Parameter "Version" geliefert, z.B. MRG 2113BR)
            Pfad zur Ressourcendatei
  Rückgabe: MRG-Typnummer
  Ergebnis: true, wenn Eintrag für MRG-Version gefunden und Daten-Inhalt der
            Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;

begin
  Result:=false;
  // Vorbelegung für den Fall, daß kein Zuordnungseintrag in Datei vorhanden ist:
  MrgTyp:=-1;

  FName:=Pfad+CResMrgVersion;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);     { Feld 1: MRG-Version }
          if UpperCase (StrFilter (FieldStr, ' ')) =
             UpperCase (StrFilter (AMrgVersion, ' ')) then begin
            try
              MrgTyp:=StrToInt (S);                  { Feld 2: MRG-Typnummer }
              Result:=true;
            except
              Result:=false;
            end;
            Break;
          end;
        end;  { while FS.Position < FSize }
      finally
        TFS.Free;
      end;
    except
      Result:=false;
    end;
  end;  { if FileExists }
end;

{---------------------------------------------------------------------------------------}
procedure SplitMrgInfo (MrgInfo: pchar; var Left_Count: word; Left: pchar; Right: pchar);
{---------------------------------------------------------------------------------------}
{ MrgInfo-String aus MrgInfo-Datei in seine einzelnen Bestandteile zerlegen }
var
  Dest : PChar;
  DestSize: word;
  code: integer;
  c: char;
  gefunden: boolean;

begin
  Left_Count:=0;
  StrCopy (Left, '');
  StrCopy (Right, '');

  DestSize := 100;
  GetMem (Dest, DestSize);
  try
    FilterString (Dest, MrgInfo, nil, ',', Nil, 0);                     { Left_Count }
    Val (Dest, Left_Count, code);

    FilterString (Dest, MrgInfo, ',', ',', Nil, 0);                     { Left }
    if StrLen (Dest) > 0 then begin
      if StrLen (Dest) > 1 then begin              { Sonderzeichen }
        gefunden:=false;
        for c:=Low (CSonderzeichen) to High (CSonderzeichen) do begin
          if StrIComp (Dest, CSonderzeichen [c]) = 0 then begin
            gefunden:=true;
            Break;
          end;
        end;
        if gefunden then
          StrPCopy (Left, c)
        else
          StrCopy (Left, '');
      end else                                     { darstellbares Zeichen }
        StrCopy (Left, Dest);
    end;

    FilterString (Dest, MrgInfo, ',', ',', Nil, 1);                     { Right }
    if StrLen (Dest) > 0 then begin
      if StrLen (Dest) > 1 then begin              { Sonderzeichen }
        gefunden:=false;
        for c:=Low (CSonderzeichen) to High (CSonderzeichen) do begin
          if StrIComp (Dest, CSonderzeichen [c]) = 0 then begin
            gefunden:=true;
            Break;
          end;
        end;
        if gefunden then
          StrPCopy (Right, c)
        else
          StrCopy (Right, '');
      end else                                     { darstellbares Zeichen }
        StrCopy (Right, Dest);
    end;
  finally
    FreeMem (Dest, DestSize);
  end;
end;

end.

