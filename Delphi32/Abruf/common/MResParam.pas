{******************************************************************************}
{* Unit: Zugriff auf MRG-Parameter-Konfigurationsdateien                      *}
{* 13.12.2002 WW                                                              *}
{* 28.08.2008 WW  mit Felder 'ParaDatentyp', 'ParaAnzahl' in ParamMrg.dat     *}
{* 26.09.2012 WW  mit Feld 'ParaByteLen' in ParamMrg.dat                      *}
{* 28.07.2014 WW  neue Tabelle mit Elster EK-Parametern (Gasqualitätsdaten)   *}
{* 08.08.2014 WW  mit Feld 'Filtertyp' in ParamMrg.dat                        *}
{* 30.04.2019 WW  mit Feld 'Parameternummer_im_MRG_Schreiben' in ParamMrg.dat *}
{* 10.10.2019 WW  mit Feld 'AusgabeFormat' in ParamMrg.dat                    *}
{* 18.02.2021 WW  mit Feld 'Parameteradresse_im_MRG' in ParamMrg.dat          *}
{* 09.07.2021 WW  Konfigurationslisten leeren vor dem Neuladen                *}
{* 06.08.2021 WW  Erweiterungen zum Laden und Suchen in Listen                *}
{* 29.09.2021 WW  Filtertyp "PasswortNr 1"                                    *}
{* 11.02.2022 WW  mit Feld 'ParameterUntergruppe' in ParamMrg.dat             *}
{* 14.04.2023 WW  neue Datei ParamMomVerb.dat mit Parametern zum Halten der   *}
{*                Verbindung                                                  *}
{* 04.04.2024 WW  Neudefinition der Filtertyp-Konstanten für Gerätezustände   *}
{******************************************************************************}
unit MResParam;

interface

uses
  Forms, Contnrs, SysUtils, WStrUtils, WResConst, MResMrg, WStream;

const
  { Datentyp-Konstanten für MRG-Parameter: }
  { Standard }
  C_PDT_BYTE    =  1;   // 8 Bit ohne Vorzeichen
  C_PDT_SHORT   =  2;   // 8 Bit mit Vorzeichen
  C_PDT_WORD    =  3;   // 16 Bit ohne Vorzeichen (INTEL, Little Endian)
  C_PDT_INT     =  4;   // 16 Bit mit Vorzeichen (INTEL, Little Endian)
  C_PDT_ULONG   =  5;   // 32 Bit ohne Vorzeichen (INTEL, Little Endian)
  C_PDT_LONG    =  6;   // 32 Bit mit Vorzeichen (INTEL, Little Endian)
  C_PDT_STRING  =  7;   // null-terminierter String
//  C_PDT_E_WORD  =  8;   // 24 Bit ohne Vorzeichen (INTEL, Little Endian)
//  C_PDT_E_ULONG =  9;   // 40 Bit ohne Vorzeichen (INTEL, Little Endian)

  C_PDT_SINGLE  =  11;  // 32 Bit-Float
  C_PDT_DOUBLE  =  12;  // 64 Bit-Float

  { Actaris Corus }
  C_PDT_FLAG_CORUS    = 21;
  C_PDT_DATE_CORUS    = 22;
  C_PDT_INDEX_CORUS   = 23;
  C_PDT_ALARM_CORUS   = 24;
  C_PDT_AL_MASK_CORUS = 25;
  C_PDT_DB_MASK_CORUS = 26;
//  C_PDT_DB_STATUS_CORUS = 27;
//  C_PDT_FLOAT16_1_CORUS = 28;
//  C_PDT_FLOAT16_2_CORUS = 29;
//  C_PDT_FLOAT16_3_CORUS = 30;

  { Tritschler, Parametrierung VCn (FTL-internes Protokoll) }
  C_PDT_FLOAT_FTL        = 'F';
  C_PDT_ZAEHLERSTAND_FTL = 'ZS';

  { Filtertyp-Konstanten für MRG-Parameter: }
  C_PFT_GERZUSTAND_AKTUELL     = 'A';  // Parameter, der einen Teil des gesamten           
                                       // aktuellen Gerätezustands abbildet;
                                       // -> neu definiert (bisher Datentyp 100); 04.04.2024, WW
  C_PFT_GERZUSTAND_GESPEICHERT = 'S';  // Parameter, der einen Teil des gesamten
                                       // gespeicherten Gerätezustands abbildet
                                       // -> neu definiert (bisher Datentyp 200); 04.04.2024, WW
  C_PFT_LGZNORMKONV  = 'N';  // Parameter für Messwerte-Konvertierung "LGZ-normiert"
  C_PFT_PASSWORTNR_1 = '1';  // Parameter, welche nur mit Passwort-Nr. 1 gelesen
                             // werden können; 29.09.2021, WW

  { Parameter-Untergruppen }
  C_PUG_FIXED = 0;  // Reservierte Untergruppe für fixe (variantenunabhängige) MRG-Parameternummern

  { Elster EK-Versionen }
  C_EK_VERS_a = 'a';  // wenn Parameter 10:314 lesbar
  C_EK_VERS_b = 'b';  // wenn Parameter 10:314 nicht lesbar, 10:36C lesbar
  C_EK_VERS_c = 'c';  // wenn Parameter 10:314 nicht lesbar, 10:36C nicht lesbar

  { Dateinamen }
  CResParamMrg  = 'ParamMrg.dat';
  CResParaText  = 'ParaText.dat';
  CResParamMeld = 'ParamMeld.dat';
  CResParamEK   = 'ParamEK.dat';
  CResParamMomVerb = 'ParamMomVerb.dat';

type
  { Record für Parameternummern-Konfigurationsdaten }

  TParamMrgData = record
    Parametergruppe: integer;
    Parameternummer: string;
    Parameternummer_im_MRG: string;
    ParaDatentyp: string;  // ab 08.03.2019 allgemeiner als string (zuvor integer)
    ParaAnzahl: integer;  // Anzahl der im Parameterwert enthaltenen Einzelparameter
    ParaByteLen: integer;  // Bytelänge des Parameterwerts; 26.09.2012 WW
    Filtertyp: string;  // zum Filtern auf Teilmenge der Parameter einer Parametegruppe; 08.08.2014, WW
    Parameternummer_im_MRG_Schreiben: string;  // für Tritschler Parametrierung; ab 30.04.2019, WW
    AusgabeFormat: string;  // zum Formatieren des Parameter-Rohwerts für Ausgabe
                            // (Delphi Format-String); ab 10.10.2019, WW
    Parameteradresse_im_MRG: string;  // für Parameter, welche nicht über die MRG-Parameternummer
                                      // im Gerät adressiert werden (z.B. TME400, RSM200 Modbus-Registeradressen); ab 18.02.2021, WW
    ParameterUntergruppe: integer;  // für Geräte mit variablen Parameterlisten (Modbus); 11.02.2022, WW
  end;

  { Objekt für Parameternummern-Konfigurationsdaten }

  TParamMrgDataObj = class (TObject)
    Data: TParamMrgData;
  public
    procedure SetData (AData: TParamMrgData);
  end;

  { Liste für Parameternummern-Konfigurationsdaten }

  TParamMrgKonfigList = class(TObjectList)
  public
    function LoadFromList_ByParaGruppe (
      AParametergruppe, AParameterUntergruppe: integer;
      ListSrc: TParamMrgKonfigList): boolean;
    function FindParamMrgData (AParametergruppe: integer; AParaNr_MRG: string;
      var ParamMrgData: TParamMrgData; bSearchParaAdr_MRG: boolean = false): boolean;
    function FindParamMrgData_ByAllgNr (AParametergruppe: integer;
      AParaNr_Allg: string; var ParamMrgData: TParamMrgData): boolean;
  end;

  { Record für Parametertext-Konfigurationsdaten }

  TParaTextData = record
    Parameternummer: string;
    Parametertext: string;
  end;

  { Objekt für Parametertext-Konfigurationsdaten }

  TParaTextDataObj = class (TObject)
    Data: TParaTextData;
  public
    procedure SetData (AData: TParaTextData);
  end;

  { Liste für Parametertext-Konfigurationsdaten }

  TParaTextKonfigList = class(TObjectList)
  public
    function FindParaText (AParaNr_Allg: string; var ParaText: string): boolean;
  end;

  { Record für Parameternummern-Konfigurationsdaten für Meldungen }

  TParamMeldData = record
    Parametergruppe: integer;
    Parameternummer_Meldung: string;
    Parameternummer_im_MRG: string; 
  end;

  { Objekt für Parameternummern-Konfigurationsdaten für Meldungen }

  TParamMeldDataObj = class (TObject)
    Data: TParamMeldData;
  public
    procedure SetData (AData: TParamMeldData);
  end;

  { Liste für Parameternummern-Konfigurationsdaten für Meldungen }

  TParamMeldKonfigList = class(TObjectList)
  public
    function LoadFromList_ByParaGruppe (AParametergruppe: integer;
      ListSrc: TParamMeldKonfigList): boolean;
    function FindParamMeldData (AParametergruppe: integer; AParaNr_Meld: string;
      var ParamMeldData: TParamMeldData): boolean;
  end;

  { Record für Elster EK-Parameternummern-Konfigurationsdaten }

  TParamEKData = record
    Parametergruppe: integer;
    Parameternummer: string;
    Version: string;  // Geräteversion (a, b, c)
    K_Zahl_Modus: string;  // Wert des Parameters "K-Zahl Modus"
    Parameternummer_im_MRG: string;
  end;

  { Objekt für Elster EK-Parameternummern-Konfigurationsdaten }

  TParamEKDataObj = class (TObject)
    Data: TParamEKData;
  public
    procedure SetData (AData: TParamEKData);
  end;

  { Liste für Elster EK-Parameternummern-Konfigurationsdaten }

  TParamEKKonfigList = class(TObjectList)
  public
    function LoadFromList_ByParaGruppe (AParametergruppe: integer;
      ListSrc: TParamEKKonfigList): boolean;
    function FindParaNrMrg (AParametergruppe: integer; AParaNr_Allg: string;
      AVersion: string; AK_Zahl_Modus: string; var ParaNr_Mrg: string): boolean;
    function FindParaNrAllg (AParametergruppe: integer; AParaNr_MRG: string;
      var ParaNr_Allg: string): boolean;
  end;

  { Struktur für Parameter zum Halten der Verbindung }

  TParamMomVerbData = record
    Parametergruppe: integer;
    Parameternummer: string;
  end;

  { Objekt für Parameter zum Halten der Verbindung }

  TParamMomVerbDataObj = class (TObject)
    Data: TParamMomVerbData;
  public
    procedure SetData (AData: TParamMomVerbData);  
  end;

  { Liste für Parameter zum Halten der Verbindung }

  TParamMomVerbKonfigList = class(TObjectList)
  public
    function LoadFromKonfigFile (Pfad: string): boolean;
    function FindParaNr (AParametergruppe: integer; var ParaNr: string): boolean;
  end;


function PDT_ByteLength (iDatentyp: integer): integer;

function GetParamMrg_KonfigList_ByParaGruppe (
  AParametergruppe, AParameterUntergruppe: integer;
  ParamMrgKonfigList: TParamMrgKonfigList; Pfad: string): boolean;
function GetParaText_KonfigList (ParaTextKonfigList: TParaTextKonfigList;
  Pfad: string): boolean;
function GetParamMeld_KonfigList_ByParaGruppe (AParametergruppe: integer;
  ParamMeldKonfigList: TParamMeldKonfigList; Pfad: string): boolean;
function GetParamEK_KonfigList_ByParaGruppe (AParametergruppe: integer;
  ParamEKKonfigList: TParamEKKonfigList; Pfad: string): boolean;
  
implementation

{----------------------------------------------------}
function PDT_ByteLength (iDatentyp: integer): integer;
{----------------------------------------------------}
{ Liefert die Byte-Länge zu Parameter-Datentypen;
  -> Achtung: Parameter vom Datentyp "PDT-STRING" haben je nach Parameter eine
              dynamische Länge. Für "PDT-String" wird daher keine Byte-Länge
              zurückgeliefert.
  Übergabe: Parameter-Datentyp (PDT-Konstante)
  Ergebnis: Byte-Länge (-1 = unbekannt) }
begin
  case iDatentyp of
    C_PDT_WORD:          Result:=2;
    C_PDT_BYTE:          Result:=1;
    C_PDT_ULONG:         Result:=4;
    C_PDT_SINGLE:        Result:=4;
    C_PDT_FLAG_CORUS:    Result:=1;
    C_PDT_DATE_CORUS:    Result:=4;
    C_PDT_INDEX_CORUS:   Result:=8;
    C_PDT_ALARM_CORUS:   Result:=22;
    C_PDT_AL_MASK_CORUS: Result:=4;
    C_PDT_DB_MASK_CORUS: Result:=2;
  else
    Result:=-1;  // Byte-Länge unbekannt
  end;
end;


{ TParamMrgDataObj }

{--------------------------------------------------------}
procedure TParamMrgDataObj.SetData (AData: TParamMrgData);
{--------------------------------------------------------}
begin
  Data:=AData;
end;

{ TParaTextDataObj }

{--------------------------------------------------------}
procedure TParaTextDataObj.SetData (AData: TParaTextData);
{--------------------------------------------------------}
begin
  Data:=AData;
end;

{ TParamMeldDataObj }

{----------------------------------------------------------}
procedure TParamMeldDataObj.SetData (AData: TParamMeldData);
{----------------------------------------------------------}
begin
  Data:=AData;
end;

{ TParamEKDataObj }

{------------------------------------------------------}
procedure TParamEKDataObj.SetData (AData: TParamEKData);
{------------------------------------------------------}
begin
  Data:=AData;
end;

{ TParamMomVerbDataObj }

{----------------------------------------------------------------}
procedure TParamMomVerbDataObj.SetData (AData: TParamMomVerbData);
{----------------------------------------------------------------}
begin
  Data:=AData;
end;


{ TParamMrgKonfigList }

{------------------------------------------------------}
function TParamMrgKonfigList.LoadFromList_ByParaGruppe (
  AParametergruppe, AParameterUntergruppe: integer;
  ListSrc: TParamMrgKonfigList): boolean;
{------------------------------------------------------}
{ Lädt ParamMrgData-Einträge zu Parametergruppe/-untergruppe aus Quell-Liste in
  die Liste;
  Übergaben: Parametergruppe
             Parameteruntergruppe (wenn < 0: Ohne Filter auf Untergruppe)
             Quell-Liste
  Ergebnis: true, wenn Quell-Liste nicht nil }
var
  i: integer;
  ParamMrgDataObj: TParamMrgDataObj;
  ParamMrgDataSrc: TParamMrgData;
  bAdd: boolean;
  iLastParaUntergruppe: integer;

begin
  Result:=false;
  Clear;  // Liste leeren

  if Assigned (ListSrc) then begin
    Result:=true;
    iLastParaUntergruppe:=-1;

    for i:=0 to ListSrc.Count - 1 do begin
      Application.ProcessMessages;
      ParamMrgDataSrc:=TParamMrgDataObj (ListSrc.Items [i]).Data;
      if ParamMrgDataSrc.Parametergruppe = AParametergruppe then begin
        // Parameter-Untergruppe auswerten; 11.02.2022, WW
        if (AParameterUntergruppe < 0) then  // ohne Filter auf Parameteruntergruppe
          bAdd:=true
        else begin  // Filter auf Parameteruntergruppe
          if (AParameterUntergruppe = C_PUG_FIXED) then  // nur Modbuslisten-ID-unabhängige Einträge ("Fixed address area")
            bAdd:=(ParamMrgDataSrc.ParameterUntergruppe = AParameterUntergruppe)
          else begin
            // Einträge mit erster gefundenen Modbuslisten-ID größer/gleich
            // der übergebenen oder Modbuslisten-ID-unabhängige Einträge ("Fixed address area"):
            // -> Sortierung der Listeneinträge nach Modbuslisten-ID aufsteigend notwendig !
            if (ParamMrgDataSrc.ParameterUntergruppe >= AParameterUntergruppe) then begin
              bAdd:=true;
              if (iLastParaUntergruppe > -1) AND
                 (ParamMrgDataSrc.ParameterUntergruppe > iLastParaUntergruppe) then
                 Break;  // bei gefundener nächst-höherer Parameteruntergruppe, raus

              // Parameteruntergruppe des gefundenen Eintrags merken
              iLastParaUntergruppe:=ParamMrgDataSrc.ParameterUntergruppe;
            end
            else if (ParamMrgDataSrc.ParameterUntergruppe = C_PUG_FIXED) then
              bAdd:=true
            else
              bAdd:=false;
          end;
        end;

        if bAdd then begin
          { Listenobjekt createn und in Liste einfügen: }
          ParamMrgDataObj:=TParamMrgDataObj.Create;
          ParamMrgDataObj.SetData (ParamMrgDataSrc);
          Add (ParamMrgDataObj);
        end;
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------}
function TParamMrgKonfigList.FindParamMrgData (AParametergruppe: integer;
  AParaNr_MRG: string; var ParamMrgData: TParamMrgData;
  bSearchParaAdr_MRG: boolean = false): boolean;
{-----------------------------------------------------------------------}
{ ParamMrgData zu der übergebenen Parametergruppe und gerätespezifischen
  Parameternummer in der Liste suchen;
  Übergaben: Parametergruppe
             gerätespezifische Parameternummer
             Optional:
               Flag: Suchen nach MRG-Parameternummer in Feld 'Parameteradresse_im_MRG'
                     ja/nein
  Rückgabe: ParamMrgData-Struktur
  Ergebnis: true, wenn Eintrag gefunden wurde }
var
  ParamMrgDataObj: TParamMrgDataObj;
  i: integer;
  bFound: boolean;

begin
  Result:=false;
  { Vorbelegung: Rückgabe }
  ParamMrgData.Parametergruppe:=-1;
  ParamMrgData.Parameternummer:='';
  ParamMrgData.Parameternummer_im_MRG:='';
  ParamMrgData.ParaDatentyp:='';
  ParamMrgData.ParaAnzahl:=-1;
  ParamMrgData.ParaByteLen:=-1;
  ParamMrgData.Filtertyp:='';
  ParamMrgData.Parameternummer_im_MRG_Schreiben:='';
  ParamMrgData.AusgabeFormat:='';
  ParamMrgData.Parameteradresse_im_MRG:='';
  ParamMrgData.ParameterUntergruppe:=-1;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParamMrgDataObj:=TParamMrgDataObj (Items [i]);
    if (ParamMrgDataObj.Data.Parametergruppe = AParametergruppe) then begin
      if bSearchParaAdr_MRG AND
        (ParamMrgDataObj.Data.Parameteradresse_im_MRG <> '') then  // 18.02.2021, WW
        // Suchen nach MRG-Parameternummer in Feld 'Parameteradresse_im_MRG',
        // wenn eine MRG-Parameteradresse definiert ist:
        bFound:=ParamMrgDataObj.Data.Parameteradresse_im_MRG = AParaNr_MRG
      else
        // Suchen nach MRG-Parameternummer in Feld 'Parameternummer_im_MRG':
        bFound:=ParamMrgDataObj.Data.Parameternummer_im_MRG = AParaNr_MRG;

      if bFound then begin
        ParamMrgData:=ParamMrgDataObj.Data;  // 06.08.2021, WW
        Result:=true;
        Break;
      end;
    end;
  end;  { for }
end;

{--------------------------------------------------------------------------------}
function TParamMrgKonfigList.FindParamMrgData_ByAllgNr (AParametergruppe: integer;
  AParaNr_Allg: string; var ParamMrgData: TParamMrgData): boolean;
{--------------------------------------------------------------------------------}
{ ParamMrgData zu der übergebenen Parametergruppe und allgemeinen Parameternummer
  in der Liste suchen;
  Übergaben: Parametergruppe
             allgemeine Parameternummer
  Rückgabe: ParamMrgData-Struktur
  Ergebnis: true, wenn Eintrag gefunden wurde }
var
  ParamMrgDataObj: TParamMrgDataObj;
  i: integer;
  bFound: boolean;

begin
  Result:=false;
  { Vorbelegung: Rückgabe }
  ParamMrgData.Parametergruppe:=-1;
  ParamMrgData.Parameternummer:='';
  ParamMrgData.Parameternummer_im_MRG:='';
  ParamMrgData.ParaDatentyp:='';
  ParamMrgData.ParaAnzahl:=-1;
  ParamMrgData.ParaByteLen:=-1;
  ParamMrgData.Filtertyp:='';
  ParamMrgData.Parameternummer_im_MRG_Schreiben:='';
  ParamMrgData.AusgabeFormat:='';
  ParamMrgData.Parameteradresse_im_MRG:='';
  ParamMrgData.ParameterUntergruppe:=-1;

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParamMrgDataObj:=TParamMrgDataObj (Items [i]);
    if (ParamMrgDataObj.Data.Parametergruppe = AParametergruppe) then begin
      // Suchen nach allgemeiner Parameternummer in Feld 'Parameternummer':
      bFound:=ParamMrgDataObj.Data.Parameternummer = AParaNr_Allg;

      if bFound then begin
        ParamMrgData:=ParamMrgDataObj.Data;  // 06.08.2021, WW
        Result:=true;
        Break;
      end;
    end;
  end;  { for }
end;


{ TParaTextKonfigList }

{--------------------------------------------------------------}
function TParaTextKonfigList.FindParaText (AParaNr_Allg: string;
  var ParaText: string): boolean;
{--------------------------------------------------------------}
{ Parametertext zu der übergebenen allgemeinen Parameternummer in der Liste suchen;
  Übergaben: allgemeine Parameternummer
  Rückgabe: Parametertext
  Ergebnis: true, wenn Parametertext gefunden wurde }
var
  ParaTextDataObj: TParaTextDataObj;
  i: integer;

begin
  Result:=false;
  ParaText:='';
  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParaTextDataObj:=TParaTextDataObj (Items [i]);
    if ParaTextDataObj.Data.Parameternummer = AParaNr_Allg then begin
      ParaText:=ParaTextDataObj.Data.Parametertext;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{ TParamMeldKonfigList }

{---------------------------------------------------------------------------------}
function TParamMeldKonfigList.LoadFromList_ByParaGruppe (AParametergruppe: integer;
  ListSrc: TParamMeldKonfigList): boolean;
{---------------------------------------------------------------------------------}
{ Lädt ParamMeldData-Einträge zu Parametergruppe aus Quell-Liste in die Liste;
  Übergaben: Parametergruppe
             Quell-Liste
  Ergebnis: true, wenn Quell-Liste nicht nil }
var
  i: integer;
  ParamMeldDataObj: TParamMeldDataObj;
  ParamMeldDataSrc: TParamMeldData;

begin
  Result:=false;
  Clear;  // Liste leeren

  if Assigned (ListSrc) then begin
    Result:=true;
    for i:=0 to ListSrc.Count - 1 do begin
      Application.ProcessMessages;
      ParamMeldDataSrc:=TParamMeldDataObj (ListSrc.Items [i]).Data;
      if ParamMeldDataSrc.Parametergruppe = AParametergruppe then begin
        { Listenobjekt createn und in Liste einfügen: }
        ParamMeldDataObj:=TParamMeldDataObj.Create;
        ParamMeldDataObj.SetData (ParamMeldDataSrc);
        Add (ParamMeldDataObj);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------}
function TParamMeldKonfigList.FindParamMeldData (AParametergruppe: integer;
  AParaNr_Meld: string; var ParamMeldData: TParamMeldData): boolean;
{-------------------------------------------------------------------------}
{ ParamMeldData zu der übergebenen Parametergruppe und meldungsspezifischen
  Parameternummer in der Liste suchen;
  Übergaben: Parametergruppe
             meldungsspezifische Parameternummer
  Rückgabe: ParamMeldData-Struktur
  Ergebnis: true, wenn Eintrag gefunden wurde }
var
  ParamMeldDataObj: TParamMeldDataObj;
  i: integer;

begin
  Result:=false;
  { Vorbelegung: Rückgabe }
  ParamMeldData.Parametergruppe:=-1;
  ParamMeldData.Parameternummer_Meldung:='';
  ParamMeldData.Parameternummer_im_MRG:='';

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParamMeldDataObj:=TParamMeldDataObj (Items [i]);
    if (ParamMeldDataObj.Data.Parametergruppe = AParametergruppe) AND
       (ParamMeldDataObj.Data.Parameternummer_Meldung = AParaNr_Meld) then begin
      ParamMeldData:=ParamMeldDataObj.Data;  // 06.08.2021, WW
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{ TParamEKKonfigList }

{-------------------------------------------------------------------------------}
function TParamEKKonfigList.LoadFromList_ByParaGruppe (AParametergruppe: integer;
  ListSrc: TParamEKKonfigList): boolean;
{-------------------------------------------------------------------------------}
{ Lädt ParamEKData-Einträge zu Parametergruppe aus Quell-Liste in die Liste;
  Übergaben: Parametergruppe
             Quell-Liste
  Ergebnis: true, wenn Quell-Liste nicht nil }
var
  i: integer;
  ParamEKDataObj: TParamEKDataObj;
  ParamEKDataSrc: TParamEKData;

begin
  Result:=false;
  Clear;  // Liste leeren

  if Assigned (ListSrc) then begin
    Result:=true;  
    for i:=0 to ListSrc.Count - 1 do begin
      Application.ProcessMessages;
      ParamEKDataSrc:=TParamEKDataObj (ListSrc.Items [i]).Data;
      if ParamEKDataSrc.Parametergruppe = AParametergruppe then begin
        { Listenobjekt createn und in Liste einfügen: }
        ParamEKDataObj:=TParamEKDataObj.Create;
        ParamEKDataObj.SetData (ParamEKDataSrc);
        Add (ParamEKDataObj);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------}
function TParamEKKonfigList.FindParaNrMrg (AParametergruppe: integer;
  AParaNr_Allg: string; AVersion: string; AK_Zahl_Modus: string;
  var ParaNr_Mrg: string): boolean;
{-------------------------------------------------------------------}
{ MRG-Parameternummer zu übergebener Parametergruppe, allgemeiner Parameternummer,
  Version und K-Zahl-Modus in der Liste suchen;
  Übergaben: Parametergruppe
             Allgemeine Parameternummer
             Version (a, b, c)
             K-Zahl-Modus
  Rückgabe: MRG-Parameternummer
  Ergebnis: true, wenn MRG-Parameternummer gefunden wurde }
var
  ParamEKDataObj: TParamEKDataObj;
  i: integer;

begin
  Result:=false;
  ParaNr_Mrg:='';
  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParamEKDataObj:=TParamEKDataObj (Items [i]);
    if (ParamEKDataObj.Data.Parametergruppe = AParametergruppe) AND
       (ParamEKDataObj.Data.Parameternummer = AParaNr_Allg) AND
       (ParamEKDataObj.Data.Version = AVersion) AND
       (ParamEKDataObj.Data.K_Zahl_Modus = AK_Zahl_Modus) then begin
      ParaNr_Mrg:=ParamEKDataObj.Data.Parameternummer_im_MRG;
      Result:=true;
      Break;
    end;
  end;  { for }
end;

{----------------------------------------------------------------------------}
function TParamEKKonfigList.FindParaNrAllg (AParametergruppe: integer;
  AParaNr_MRG: string; var ParaNr_Allg: string): boolean;
{----------------------------------------------------------------------------}
{ Allgemeine Parameternummer zu übergebener Parametergruppe und MRG-Parameternummer
  in der Liste suchen;
  Übergaben: Parametergruppe
             MRG-Parameternummer
  Rückgabe: Allgemeine Parameternummer
  Ergebnis: true, wenn allgemeine Parameternummer gefunden wurde }
var
  ParamEKDataObj: TParamEKDataObj;
  i: integer;

begin
  Result:=false;
  ParaNr_Allg:='';
  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParamEKDataObj:=TParamEKDataObj (Items [i]);
    // Suche nach Parameternummer ohne Messgrößenerweiterung '_...':
    if (ParamEKDataObj.Data.Parametergruppe = AParametergruppe) AND
       (ExtractString (ParamEKDataObj.Data.Parameternummer_im_MRG, #0, '_', 0) =
        ExtractString (AParaNr_MRG, #0, '_', 0)) then begin
      ParaNr_Allg:=ParamEKDataObj.Data.Parameternummer;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{ TParamMomVerbKonfigList }

{--------------------------------------------------------------------------}
function TParamMomVerbKonfigList.LoadFromKonfigFile (Pfad: string): boolean;
{--------------------------------------------------------------------------}
{ Lädt Datei ParamMomVerb.Dat in die Liste;
  Übergabe: Pfad zur Datei
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  ParamMomVerbData: TParamMomVerbData;
  ParamMomVerbDataObj: TParamMomVerbDataObj;

begin
  Result:=false;
  Clear;

  FName:=Pfad+CResParamMomVerb;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);     { Feld 1: Parametergruppe }
          ParamMomVerbData.Parametergruppe:=StrToIntDef (FieldStr, -1);

          if ParamMomVerbData.Parametergruppe > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            try
              ParamMomVerbData.Parameternummer:=S;  { Feld 2: Allg. Parameternummer }

              { Listenobjekt createn und in Liste einfügen: }
              ParamMomVerbDataObj:=TParamMomVerbDataObj.Create;
              ParamMomVerbDataObj.SetData (ParamMomVerbData);
              Add (ParamMomVerbDataObj);
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

{---------------------------------------------------------------------}
function TParamMomVerbKonfigList.FindParaNr (AParametergruppe: integer;
  var ParaNr: string): boolean;
{---------------------------------------------------------------------}
{ Sucht nach Listeneintrag mit übergebener Parametergruppe und liefert die
  zugeordnete Parameternummer;
  Übergabe: Parametergruppe
  Rückgabe: Allgemeine Parameternummer
  Ergebnis: true, wenn Eintrag gefunden }
var
  ParamMomVerbDataObj: TParamMomVerbDataObj;
  i: integer;

begin
  Result:=false;
  // Vorbelegung Rückgabe:
  ParaNr:='';

  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParamMomVerbDataObj:=TParamMomVerbDataObj (Items [i]);
    if (ParamMomVerbDataObj.Data.Parametergruppe = AParametergruppe) then begin
      ParaNr:=ParamMomVerbDataObj.Data.Parameternummer;
      Result:=true;
      Break;
    end;
  end;  { for }
end;


{------------------------------------------------------------------------------}

{----------------------------------------------------------------}
function GetParamMrg_KonfigList_ByParaGruppe (
  AParametergruppe, AParameterUntergruppe: integer;
  ParamMrgKonfigList: TParamMrgKonfigList; Pfad: string): boolean;
{----------------------------------------------------------------}
{ Lädt auf übergebene Parametergruppe/-untergruppe gefilterten oder gesamten Inhalt
  von ParamMrg.Dat in Liste;
  Übergabe: Parametergruppe (wenn < 0: Gesamt-Liste wird geladen)
            Parameteruntergruppe (wenn < 0: Ohne Filter auf Untergruppe)
            Pfad zur Ressourcendatei
  Rückgabe: Liste mit Parameter-Konfigurationsdaten
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  ParamMrgData: TParamMrgData;
  ParamMrgDataObj: TParamMrgDataObj;
  bAdd: boolean;
  iLastParaUntergruppe: integer;

begin
  Result:=false;
  if ParamMrgKonfigList = nil then exit;
  ParamMrgKonfigList.Clear;  // 09.07.2021, WW

  FName:=Pfad+CResParamMrg;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        iLastParaUntergruppe:=-1;

        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);      { Feld 1: Parametergruppe }
          ParamMrgData.Parametergruppe:=StrToIntDef (FieldStr, -1);

          if ParamMrgData.Parametergruppe > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            if (AParametergruppe < 0) OR  // alle Parametergruppen; 06.08.2021, WW
               (ParamMrgData.Parametergruppe = AParametergruppe) then begin  // nur gesuchte Parametergruppe
              try
                // Record vorbelegen
                ParamMrgData.Parameternummer:='';
                ParamMrgData.Parameternummer_im_MRG:='';
                ParamMrgData.ParaDatentyp:='';
                ParamMrgData.ParaAnzahl:=1;  // Default: Rohwert enthält 1 Parameter
                ParamMrgData.ParaByteLen:=-1;
                ParamMrgData.Filtertyp:='';
                ParamMrgData.Parameternummer_im_MRG_Schreiben:='';
                ParamMrgData.AusgabeFormat:='';
                ParamMrgData.Parameteradresse_im_MRG:='';
                ParamMrgData.ParameterUntergruppe:=-1;

                FieldCount:=1;
                while length (S) > 0 do begin
                  FieldStr:=F_Zerlegen (S, CResTrenner);
                  inc (FieldCount);
                  with ParamMrgData do begin
                    case FieldCount of
                      2: Parameternummer:=FieldStr;
                      3: Parameternummer_im_MRG:=FieldStr;
                      4: ParaDatentyp:=FieldStr;
                      5: ParaAnzahl:=StrToIntDef (FieldStr, 1);  // Default: Rohwert enthält 1 Parameter
                      6: ParaByteLen:=StrToIntDef (FieldStr, -1);  // 26.09.2012, WW
                      7: Filtertyp:=FieldStr;  // 08.08.2014, WW
                      8: Parameternummer_im_MRG_Schreiben:=FieldStr;  // 30.04.2019, WW
                      9: AusgabeFormat:=FieldStr;  // 10.10.2019, WW
                     10: Parameteradresse_im_MRG:=FieldStr;  // 18.02.2021, WW
                     11: ParameterUntergruppe:=StrToIntDef (FieldStr, -1);  // 11.02.2022, WW
                    end;  { case }
                  end;  { with }
                end;  { while length (S) }

                // Parameter-Untergruppe auswerten; 11.02.2022, WW
                if (AParameterUntergruppe < 0) then  // ohne Filter auf Parameteruntergruppe
                  bAdd:=true
                else begin  // Filter auf Parameteruntergruppe
                  if (AParameterUntergruppe = C_PUG_FIXED) then  // nur Modbuslisten-ID-unabhängige Einträge ("Fixed address area")
                    bAdd:=(ParamMrgData.ParameterUntergruppe = AParameterUntergruppe)
                  else begin
                    // Einträge mit erster gefundenen Modbuslisten-ID größer/gleich
                    // der übergebenen oder Modbuslisten-ID-unabhängige Einträge ("Fixed address area"):
                    // -> Sortierung der Listeneinträge nach Modbuslisten-ID aufsteigend notwendig !
                    if (ParamMrgData.ParameterUntergruppe >= AParameterUntergruppe) then begin
                      bAdd:=true;
                      if (iLastParaUntergruppe > -1) AND
                         (ParamMrgData.ParameterUntergruppe > iLastParaUntergruppe) then
                         Break;  // bei gefundener nächst-höherer Parameteruntergruppe, raus

                      // Parameteruntergruppe des gefundenen Eintrags merken
                      iLastParaUntergruppe:=ParamMrgData.ParameterUntergruppe;
                    end
                    else if (ParamMrgData.ParameterUntergruppe = C_PUG_FIXED) then
                      bAdd:=true
                    else
                      bAdd:=false;
                  end;
                end;

                if bAdd then begin
                  { Listenobjekt createn und in Liste einfügen: }
                  ParamMrgDataObj:=TParamMrgDataObj.Create;
                  ParamMrgDataObj.SetData (ParamMrgData);
                  ParamMrgKonfigList.Add (ParamMrgDataObj);
                end;
              except
                Result:=false;
              end;
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
function GetParaText_KonfigList (ParaTextKonfigList: TParaTextKonfigList;
                                 Pfad: string): boolean;
{-----------------------------------------------------------------------}
{ Lädt Inhalt von ParaText.Dat in Liste;
  Rückgabe: Liste mit Parametertexten
            Pfad zur Ressourcendatei
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  ParaTextData: TParaTextData;
  ParaTextDataObj: TParaTextDataObj;

begin
  Result:=false;
  if ParaTextKonfigList = nil then exit;
  ParaTextKonfigList.Clear;  // 09.07.2021, WW

  FName:=Pfad+CResParaText;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);      { Feld 1: allgemeine Parameternummer }
          if StrToIntDef (FieldStr, -1) > -1 then begin    { Feld 1 enthält gültigen Zahlenwert }
            try
              ParaTextData.Parameternummer:=FieldStr;
              ParaTextData.Parametertext:=S;                { Feld 2: Parametertext }

              { Listenobjekt createn und in Liste einfügen: }
              ParaTextDataObj:=TParaTextDataObj.Create;
              ParaTextDataObj.SetData (ParaTextData);
              ParaTextKonfigList.Add (ParaTextDataObj);
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

{---------------------------------------------------------------------------------------}
function GetParamMeld_KonfigList_ByParaGruppe (AParametergruppe: integer;
                                               ParamMeldKonfigList: TParamMeldKonfigList;
                                               Pfad: string): boolean;
{---------------------------------------------------------------------------------------}
{ liefert auf übergebene Parametergruppe gefilterten oder gesamten Inhalt von
  ParamMeld.Dat als Liste;
  Übergabe: Parametergruppe (wenn < 0: Gesamt-Liste wird geladen)
            Pfad zur Ressourcendatei
  Rückgabe: Liste mit Parameter-Konfigurationsdaten für Meldungen
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  ParamMeldData: TParamMeldData;
  ParamMeldDataObj: TParamMeldDataObj;

begin
  Result:=false;
  if ParamMeldKonfigList = nil then exit;
  ParamMeldKonfigList.Clear;  // 09.07.2021, WW

  FName:=Pfad+CResParamMeld;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);      { Feld 1: Parametergruppe }
          ParamMeldData.Parametergruppe:=StrToIntDef (FieldStr, -1);

          if ParamMeldData.Parametergruppe > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            if (AParametergruppe < 0) OR  // alle Parametergruppen; 06.08.2021, WW
               (ParamMeldData.Parametergruppe = AParametergruppe) then begin  // nur gesuchte Parametergruppe
              try
                // Record vorbelegen
                ParamMeldData.Parameternummer_Meldung:='';
                ParamMeldData.Parameternummer_im_MRG:='';

                FieldCount:=1;
                while length (S) > 0 do begin
                  FieldStr:=F_Zerlegen (S, CResTrenner);
                  inc (FieldCount);
                  with ParamMeldData do begin
                    case FieldCount of
                      2: Parameternummer_Meldung:=FieldStr;
                      3: Parameternummer_im_MRG:=FieldStr;
                    end;  { case }
                  end;  { with }
                end;  { while length (S) }

                { Listenobjekt createn und in Liste einfügen: }
                ParamMeldDataObj:=TParamMeldDataObj.Create;
                ParamMeldDataObj.SetData (ParamMeldData);
                ParamMeldKonfigList.Add (ParamMeldDataObj);
              except
                Result:=false;
              end;
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

{---------------------------------------------------------------------------------}
function GetParamEK_KonfigList_ByParaGruppe (AParametergruppe: integer;
                                             ParamEKKonfigList: TParamEKKonfigList;
                                             Pfad: string): boolean;
{---------------------------------------------------------------------------------}
{ liefert auf übergebene Parametergruppe gefilterten oder gesamten Inhalt von
  ParamEK.Dat als Liste;
  Übergabe: Parametergruppe (wenn < 0: Gesamt-Liste wird geladen)
            Pfad zur Ressourcendatei
  Rückgabe: Liste mit EK-Parameter-Konfigurationsdaten
  Ergebnis: true, wenn Datei gefunden und Daten-Inhalt der Datei plausibel }
var
  TFS: TTextFileStream;
  FName: string;
  FSize: integer;
  S: string;
  FieldStr: string;
  FieldCount: integer;
  ParamEKData: TParamEKData;
  ParamEKDataObj: TParamEKDataObj;

begin
  Result:=false;
  if ParamEKKonfigList = nil then exit;
  ParamEKKonfigList.Clear;  // 09.07.2021, WW

  FName:=Pfad+CResParamEK;
  if FileExists (FName) then begin
    try
      TFS:=TTextFileStream.Create (FName, fmOpenRead OR fmShareDenyWrite);
      try
        Result:=true;
        FSize:=TFS.Size;
        while TFS.Position < FSize do begin
          Application.ProcessMessages;
          TFS.ReadLn (S);
          FieldStr:=F_Zerlegen (S, CResTrenner);      { Feld 1: Parametergruppe }
          ParamEKData.Parametergruppe:=StrToIntDef (FieldStr, -1);

          if ParamEKData.Parametergruppe > -1 then begin  // Feld 1 enthält gültigen Zahlenwert
            if (AParametergruppe < 0) OR  // alle Parametergruppen; 06.08.2021, WW
               (ParamEKData.Parametergruppe = AParametergruppe) then begin  // nur gesuchte Parametergruppe
              try
                // Record vorbelegen
                ParamEKData.Parameternummer:='';
                ParamEKData.Version:='';
                ParamEKData.K_Zahl_Modus:='';
                ParamEKData.Parameternummer_im_MRG:='';

                FieldCount:=1;
                while length (S) > 0 do begin
                  FieldStr:=F_Zerlegen (S, CResTrenner);
                  inc (FieldCount);
                  with ParamEKData do begin
                    case FieldCount of
                      2: Parameternummer:=FieldStr;
                      3: Version:=FieldStr;
                      4: K_Zahl_Modus:=FieldStr;
                      5: Parameternummer_im_MRG:=FieldStr;
                    end;  { case }
                  end;  { with }
                end;  { while length (S) }

                { Listenobjekt createn und in Liste einfügen: }
                ParamEKDataObj:=TParamEKDataObj.Create;
                ParamEKDataObj.SetData (ParamEKData);
                ParamEKKonfigList.Add (ParamEKDataObj);
              except
                Result:=false;
              end;
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

end.

