{******************************************************************************}
{* Unit: Objekte zum Handling von MRG-Parametern                              *}
{*       Konvertieren: Rohfile -> Parameterliste -> Tabelle                   *}
{* 02.12.98 WW                                                                *}
{* 23.05.2002 WW  erweitert um Excel-Konvertierung                            *}
{* 19.02.2003 WW  Objekt-Funktionalitäten von TParameterListe aufgeteilt auf  *}
{*                TParameterListe (gerätespezifische Konvertierungen, Zugriff *}
{*                auf Konfigurationsdateien, Datenbank-unabhängig !) und davon*}
{*                abgeleitet TParameterListeDB (Zugriff auch auf Konfigura-   *}
{*                tionen in Tabellen, Abspeichern in Tabellen, ASCII-Export)  *}
{* 30.07.2003 WW  Excel-Konvertierung auf alle Gerätetypen erweitert          *}
{* 13.01.2004 GD  Erweitert um XML-Datenaustausch                             *}
{* 29.10.2007 WW  resourcestrings                                             *}
{* 26.09.2012 WW  Konvertierung Corus auf Parametergruppen-Antwort angepaßt   *}
{* 27.03.2013 WW  Konvertierung IEC 1107 an Kamstrup UNIGAS 300 angepaßt      *}
{* 24.01.2014 WW  Gerätezustand für EK 280                                    *}
{* 12.02.2014 WW  Gerätezustand für DL 240, 220, 210 und MRG 905, 910         *}
{* 23.05.2014 WW  Gerätezustand für DL 230                                    *}
{* 06.10.2015 WW  Beschleunigte Rohdateizugriffe mit TFileOfCharStream        *}
{* 05.12.2017 WW  Konvertierung Tritschler IEC: Werte des letzten Monatsab-   *}
{*                schluß konvertieren                                         *}
{* 08.03.2019 WW  Konvertierung Modbus                                        *}
{* 30.04.2019 WW  Anpassung Konvertierung Tritschler IEC/Datacon FWU          *}
{* 18.06.2020 WW  XML-Ersatzzeichen decodieren                                *}
{* 18.02.2021 WW  Anpassung Konvertierung Modbus für TME400                   *}
{* 09.07.2021 WW  TParameterListe-Konvertierungsmethoden mit optionaler Über- *}
{*                gabe der allgemeinen und Elster EK-spezifischen Parameter-  *}
{*                Konfigurationsliste                                         *}
{* 29.09.2021 WW  Anpassung Konvertierung Modbus für FLOWSIC500               *}
{* 11.02.2022 WW  Übergaberecord TParaKonv erweitert um Parameter-Untergruppe *}
{*                (für Modbuslisten-Variante Prilog 400)                      *}
{* 17.10.2023 WW  Parameterliste erweitert um Property 'Ersatzzeichen' (für   *}
{*                Elster-Geräte)                                              *}
{* 09.01.2024 WW  RSM200-Rohwerte zu Anzeigewerte verrechnen                  *}
{* 04.04.2024 WW  Gerätezustand für TME400 und RSM200                         *}
{******************************************************************************}
Unit MObjPara;

INTERFACE

Uses
  SysUtils, Forms, Classes, WStrUtils, WSysCon, MP_Boritec, GD_Utils, WChars,
  MP_Tritschler, MResParam, MP_Datacon, COM_Utils, WXmlConst, DecodeResp, MObjList,
  T_BinMask, MP_Elster, MP_Allg, T_GerZustand, WStream, ModbusMasterRes,
  ModbusUtil, MP_RMG, MP_SICK;

const
  { Länge der gerätespezifischen Parameternummer }
  CLen_MrgParaNr_Wieser = 3;  // Wieser-MRGs
  CLen_MrgParaNr_RMG_EC = 4;  // RMG EC 694, EC 900

Type

  { Übergaberecord für MRG-Parameterkonvertierung }

  TParaKonv = record
    ParaKonvGruppe: Integer;  { geräteabh. Konvertierungsgruppe }
    ParaGruppe: Integer;      { geräteabh. Parametergruppe }
    ParaUnterGruppe: Integer; { Variante der Parametergruppe; 11.02.2022, WW }
    RohLoeschen: boolean;     { Rohdaten nach Konvertierung löschen ja/nein }
  end;

  { Parameterobjekt zur Aufnahme in TParameterListe }

  TParameterItem = class (TObject)
  public
    AllgNum: string [szLen_ParaNrAllg];   { allg. Parameternummer (9-stellig) }
    Wert   : string [140];  { Parameterwert;
                              ab 01.09.2008 erweitert von 40 auf 140 Stellen
                              für Actaris Corus-Parameter (Mehrfachwerte) }
    MrgNum : string [szLen_ParaNrMrg];    { MRG-spezifische Parameternummer }
    constructor Create (AAllgNum, AWert, AMRGNum: string);
  End;

  { Liste von Parametern }

  TParameterListe = class (TList)
  private
    FErsatzzeichen: boolean;  // Flag: Rohdaten enthalten Ersatzzeichen ja/nein; 17.10.2023, WW
    Function SplitRohString_Wieser (RohString: string; Parametergruppe: Integer;
                                    ParamMrgKonfigList: TParamMrgKonfigList;
                                    MrgNumLen: integer;
                                    var AllgNum: string; var Wert: string;
                                    var MRGNum: string): boolean;
    function Update (AllgNum: string; Wert: string; MrgNum: string): boolean;
    Function Search (AAllgNum: string; var Index: integer): boolean;
    Function SearchByMrgNum (AMrgNum: string; var Index: integer): boolean;
    function BuildGeraeteZustand_EK260 (AAllgNum: string;
                                        var iGerZustand: Int64): boolean;
    function BuildGeraeteZustand_EK280 (AAllgNum: string;
                                        var iGerZustand: Int64): boolean;
    function BuildGeraeteZustand_DL240 (AAllgNum: string;
                                        var iGerZustand: Int64): boolean;
    function BuildGeraeteZustand_DL230 (AAllgNum: string;
                                        var iGerZustand: Int64): boolean;
    function BuildGeraeteZustand_DL220 (AAllgNum: string;
                                        var iGerZustand: Int64): boolean;
    function BuildGeraeteZustand_DL210 (AAllgNum: string;
                                        var iGerZustand: Int64): boolean;
    function BuildGeraeteZustand_MRG900 (AAllgNum: string;
                                         var iGerZustand: Int64): boolean;
    function BuildGeraeteZustand_TME400 (AAllgNum: string;
                                         var iGerZustand: Int64): boolean;
    function BuildGeraeteZustand_RSM200 (AAllgNum: string;
                                         var iGerZustand: Int64): boolean;
    function BuildGeraeteZustand (AMrgTyp: integer; AAllgNum: string;
      var sGerZustandHex: string): boolean;  // 16.08.2012, WW

    function KonvPara_Wieser (Rohfilename: string; ParameterGruppe: integer;
                              ParamMrgKonfigList: TParamMrgKonfigList;
                              MrgNumLen: integer): boolean;
    function KonvPara_MemoDat (Rohfilename: string): boolean;
    function KonvPara_IEC1107 (Rohfilename: string; ParameterGruppe: integer;
                               ParamMrgKonfigList: TParamMrgKonfigList;
                               AParamEKKonfigList: TParamEKKonfigList): boolean;
    function KonvPara_Tritschler_IEC (Rohfilename: string; ParameterGruppe: integer;
                                      ParamMrgKonfigList: TParamMrgKonfigList): boolean;
    function KonvPara_DS100 (Rohfilename: string; ParameterGruppe: integer;
                             ParamMrgKonfigList: TParamMrgKonfigList): boolean;
    function KonvPara_Tritschler_FTL (Rohfilename: string; ParameterGruppe: integer;
                                      ParamMrgKonfigList: TParamMrgKonfigList): boolean;
    function KonvPara_Corus (Rohfilename: string; ParameterGruppe: integer;
                             ParamMrgKonfigList: TParamMrgKonfigList): boolean;
    function KonvPara_Modbus (RegisterKonvListe: TRegisterKonvList;
                              ParameterGruppe: integer;
                              ParamMrgKonfigList: TParamMrgKonfigList): boolean;
(*    procedure GetZaehlerFaktoren_SICK (var dFaktor_Vb: double; var dFaktor_Vn: double);
    procedure CalcParaAnzeige_SICK; *)
    procedure GetZaehlerFaktor_RSM200 (var dFaktor: double; var sFormat: string);
    procedure CalcParaAnzeige_RSM200;
  protected
    KonfigPfad: string;
    Function KonvRohdatenFromFileList (FileNameList: TTextListe; ParaKonv: TParaKonv;
                                       ParamMrgKonfigList: TParamMrgKonfigList;
                                       ParamEKKonfigList: TParamEKKonfigList): Boolean;
    Function KonvEinzelparameter_Wieser (HeapStr: string; ParameterGruppe: integer;
                                         ParamMrgKonfigList: TParamMrgKonfigList;
                                         MrgNumLen: integer): Boolean;
    Procedure WriteExcelSheet (ExcelCaption: string;
                               ParaTextKonfigList: TParaTextKonfigList);
  public
    constructor Create (AKonfigPfad: string);
    procedure Clear; override;
    procedure Delete(iIndex: integer); virtual;
    Function LoadFromFile (FileName: TFileName; ParaKonv: TParaKonv;
      AParamMrgKonfigList: TParamMrgKonfigList = nil;
      AParamEKKonfigList: TParamEKKonfigList = nil): Boolean;
    Function LoadFromFileList (FileNameList: TTextListe; ParaKonv: TParaKonv;
      AParamMrgKonfigList: TParamMrgKonfigList = nil;
      AParamEKKonfigList: TParamEKKonfigList = nil): Boolean;
    function LoadFromXmlString (sXmlString: string): boolean;
    function LoadFromXmlFile (sFileName: TFileName; bDelete: boolean): boolean;

    Function LoadFromHeapA (HeapStr: string; ParaKonv: TParaKonv;
      AParamMrgKonfigList: TParamMrgKonfigList = nil): Boolean;
    Procedure LoadFromParameterlist (PL_Src: TParameterListe; ClearOldList: boolean = true);
    function LoadFromModbusRegisterRequestList (
      RegisterRequestListe: TRegisterRequestList; ParaKonv: TParaKonv;
      AParamMrgKonfigList: TParamMrgKonfigList = nil): boolean;

    function LoadGeraeteZustand (AMrgTyp: integer; AAllgNum: string): boolean;  // 16.08.2012, WW

    Function GetValue (AllgNum: string; var Wert: string): boolean;
    Function GetValueByMrgNum (MrgNum: string; var Wert: string): boolean;
    Function GetValueDouble (AllgNum: string; var Wert: Double): Boolean;
    Function GetValueInt (AllgNum: string; var Wert: integer): Boolean;

    Procedure SaveToExcel (ExcelCaption: string);

    property Ersatzzeichen: boolean read FErsatzzeichen write FErsatzzeichen;
  End;

function ParaNrAllgCompare (Item1, Item2: Pointer): Integer;
function ParaNrMRGCompare (Item1, Item2: Pointer): Integer;
function FormatPara_Corus_Anzeige (sRohwert: string; iDatentyp: integer;
  iParaAnzahl: integer): string;
function FormatPara_Corus_Roh (sAnzeige: string; iDatentyp: integer;
  iParaAnzahl: integer; iParaByteLen: integer; var sRohwert: string): boolean;
function FormatPara_FTLintern_Roh (sAnzeige: string; sDatentyp: string;
  var sRohwert: string): boolean;

function KonvDS100Param (RohAntwort: string; cBefehl: char;
  var ParamWert: string): boolean;
                            
IMPLEMENTATION

Const
  { Zustände für Rohdatenkonvertierung }

  m_Beginning = 1;
  m_SameParam = 2;
  m_NewParam  = 3;

  C_TrennerParaAnzeige = ' ';  // Leerzeichen zwischen den Parameter-Einzelwerten im Anzeige-String

resourcestring
  S_Nummer = 'Nummer';
  S_Wert   = 'Wert';
  S_Name   = 'Name';


{----------------------------------------------------------}
function ParaNrAllgCompare (Item1, Item2: Pointer): Integer;
{----------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TParameterItem-Objekten nach der allg.
  Parameternummer }
var
  AllgNum1: integer;
  AllgNum2: integer;
  Code: integer;
begin
  Val (TParameterItem (Item1).AllgNum, AllgNum1, Code);
  Val (TParameterItem (Item2).AllgNum, AllgNum2, Code);
  Result:=AllgNum1 - AllgNum2;
end;

{---------------------------------------------------------}
function ParaNrMRGCompare (Item1, Item2: Pointer): Integer;
{---------------------------------------------------------}
{ Vergleichsfunktion zum Sortieren von TParameterItem-Objekten nach der MRG-
  Parameternummer }
begin
  { String-Vergleich, da Geräte-Parameternummern nicht immer Zahlenwerte
    enthalten (Fremdgeräte !): }
  Result:=CompareStr (TParameterItem (Item1).MRGNum,
                      TParameterItem (Item2).MRGNum);
end;

{----------------------------------------------------------------------}
function FormatPara_Corus_Anzeige (sRohwert: string; iDatentyp: integer;
  iParaAnzahl: integer): string;
{----------------------------------------------------------------------}
{ Formatiert Corus-Parameter-Rohwert zu einem Anzeige-String;
  Übergabe: Rohwert-String
            Datentyp des Parameters (C_PDT_xxx-Konstanten aus MResParam.pas)
            Anzahl der im Rohwert enthaltenen Parameter-Einzelwerte
  Ergebnis: Parameter-Anzeige-String }
var
  S: string;
  iLenEinzelwert: integer;
  iPos: integer;
  i: integer;
  sBuf: string;
  dtBuf: TDateTime;

begin
  Result:='';

  if iDatentyp = C_PDT_STRING then
    Result:=F_RightTrunc (sRohwert, NUL)  { abschließende NULL-Zeichen wegschneiden }
  else begin  // Datentypen, bei denen mehrere Einzelwerte im Rohwert enthalten sein können
    iLenEinzelwert:=PDT_ByteLength (iDatentyp);
    if iLenEinzelwert <= -1 then
      iLenEinzelwert:=length (sRohwert);  // Default

    iPos:=1;
    for i:=1 to iParaAnzahl do begin
      Application.ProcessMessages;
      if iParaAnzahl > 1 then
        S:=Copy (sRohwert, iPos, iLenEinzelwert)  // Einzelwert rauskopieren
      else
        S:=sRohwert;

      case iDatentyp of
        C_PDT_WORD: Result:=Result + IntToStr (Bin2Word (S));
        C_PDT_BYTE: Result:=Result + IntToStr (Bin2Byte (S));
        C_PDT_ULONG: Result:=Result + IntToStr (Bin2Longword (S));
        C_PDT_SINGLE: Result:=Result + Format ('%.4f', [Bin2Single (S)]);
        C_PDT_FLAG_CORUS: Result:=Result + IntToStr (Bin2Byte (S));
        C_PDT_DATE_CORUS:
          begin
            dtBuf:=Bin2Date_Corus (S);
            if dtBuf > 0 then  // wenn Datum vorhanden und gültig
              Result:=Result + FormatDateTime (C_FormatDateTime, dtBuf);
          end;
        C_PDT_INDEX_CORUS: Result:=Result + Format ('%.4f', [Bin2Index_Corus (S)]);
        C_PDT_ALARM_CORUS:
          begin
            with Bin2Alarm_Corus (S) do begin
              sBuf:=IntToStr (Active) + '  ' +
                    IntToStr (Memo) + '  ' +
                    Format ('%.4f', [Value]) + '  ';
              sBuf:=sBuf + 'S: ';
              if Start > 0 then  // wenn Datum vorhanden und gültig
                sBuf:=sBuf + FormatDateTime (C_FormatDateTime, Start)
              else
                sBuf:=sBuf + 'k.A.';
              sBuf:=sBuf + '  ';

              sBuf:=sBuf + 'E: ';
              if Ende > 0 then  // wenn Datum vorhanden und gültig
                sBuf:=sBuf + FormatDateTime (C_FormatDateTime, Ende)
               else
                sBuf:=sBuf + 'k.A.';
              sBuf:=sBuf + '  ';

              sBuf:=sBuf + 'LS: ';
              if MStart > 0 then  // wenn Datum vorhanden und gültig
                sBuf:=sBuf + FormatDateTime (C_FormatDateTime, MStart)
              else
                sBuf:=sBuf + 'k.A.';
              sBuf:=sBuf + '  ';

              sBuf:=sBuf + 'LE: ';
              if MEnde > 0 then  // wenn Datum vorhanden und gültig
                sBuf:=sBuf + FormatDateTime (C_FormatDateTime, MEnde)
              else
                sBuf:=sBuf + 'k.A.';
            end;
            Result:=Result + sBuf;
          end;
        C_PDT_AL_MASK_CORUS: Result:=Result + IntToHex (Bin2Longword (S), 8);  { Hex-Darstellung }
                             { ab 26.09.2012 ohne vorangestelltes $-Zeichen (analog zu Geräten) }
        C_PDT_DB_MASK_CORUS: Result:=Result + IntToHex (Bin2Word (S), 4);  { Hex-Darstellung }
                             { ab 26.09.2012 ohne vorangestelltes $-Zeichen (analog zu Geräten) }
      else  // unbekannter Parameter-Datentyp
        Result:=S;  // Rohformat
      end;  { case iDatentyp }

      if i < iParaAnzahl then
        Result:=Result + C_TrennerParaAnzeige;  // Leerzeichen zwischen den Einzelwerten
      inc (iPos, iLenEinzelwert);
    end;  { for i }
  end;
end;

{----------------------------------------------------------------------------}
function FormatPara_Corus_Roh (sAnzeige: string; iDatentyp: integer;
  iParaAnzahl: integer; iParaByteLen: integer; var sRohwert: string): boolean;
{----------------------------------------------------------------------------}
{ Formatiert Corus-Parameter-Anzeige-String zu einem Rohwert;
  Übergabe: Anzeige-String
            Datentyp des Parameters (C_PDT_xxx-Konstanten aus MResParam.pas)
            Anzahl der im Rohwert enthaltenen Parameter-Einzelwerte
            Bytelänge des Parameters
  Rückgabe: Parameter-Rohwert
  Ergebnis: true, wenn Parameter-Rohwert aus Anzeige-String gebildet werden konnte }
var
  S: string;
  i: integer;
  c: cardinal;
  w: word;
  dtBuf: TDateTime;
  sAnzeigeBuf: string;

begin
  Result:=false;
  sRohwert:='';  // Vorbelegung Rückgabe

  try
    if iDatentyp = C_PDT_STRING then begin
      S:=Copy (sAnzeige, 1, iParaByteLen);  { auf max. Länge beschränken }
      sRohwert:=F_RightPad (S, NUL, iParaByteLen);  { mit NULL-Zeichen am Ende auf max. Länge auffüllen }
    end
    else begin  // Datentypen, bei denen mehrere Einzelwerte im Anzeige-String enthalten sein können
      sAnzeigeBuf:=sAnzeige;

      for i:=1 to iParaAnzahl do begin
        Application.ProcessMessages;
        if iParaAnzahl > 1 then
          S:=F_Zerlegen (sAnzeigeBuf, C_TrennerParaAnzeige)  // Einzelwert aus Anzeige-String rauskopieren
        else
          S:=sAnzeigeBuf;

        case iDatentyp of
          C_PDT_WORD: sRohwert:=sRohwert + Word2Bin (StrToInt (S));
          C_PDT_BYTE: sRohwert:=sRohwert + Byte2Bin (StrToInt (S));
          C_PDT_ULONG: sRohwert:=sRohwert + Longword2Bin (StrToInt (S));
          C_PDT_SINGLE: sRohwert:=sRohwert + Single2Bin (StrToFloat (S));
          C_PDT_FLAG_CORUS: sRohwert:=sRohwert + Byte2Bin (StrToInt (S));
          C_PDT_DATE_CORUS:
            begin
              dtBuf:=StrToDateTime (S);
              sRohwert:=sRohwert + Date2Bin_Corus (dtBuf);
            end;
          C_PDT_INDEX_CORUS: sRohwert:=sRohwert + Index2Bin_Corus (StrToFloat (S));
          C_PDT_ALARM_CORUS:
            begin
              sRohwert:='';  // Parameter-Datentyp ALARM wird für Parametrierung nicht benötigt
              exit;
            end;
          C_PDT_AL_MASK_CORUS:
            begin
              c:=cardinal (StrToInt ('$' + S));  { Hex-String }
              sRohwert:=sRohwert + Longword2Bin (c);
            end;
          C_PDT_DB_MASK_CORUS:
            begin
              w:=word (StrToInt ('$' + S));  { Hex-String }
              sRohwert:=sRohwert + Word2Bin (w);
            end;
        else  // unbekannter Parameter-Datentyp
          sRohwert:='';
          exit;
        end;  { case iDatentyp }
      end;  { for i }
    end;
    Result:=true;  // OK, Parameter-Rohwert konnte gebildet werden
  except
    sRohwert:='';
    Result:=false;
  end;
end;

{---------------------------------------------------------------------}
function FormatPara_FTLintern_Roh (sAnzeige: string; sDatentyp: string;
  var sRohwert: string): boolean;
{---------------------------------------------------------------------}
{ Formatiert Parameter-Anzeige-String zu einem Rohwert für internes FTL-Protokoll
  (Tritschler);
  Übergabe: Anzeige-String
            Datentyp des Parameters (C_PDT_xxx_FTL-Konstanten aus MResParam.pas)
  Rückgabe: Parameter-Rohwert
  Ergebnis: true, wenn Parameter-Rohwert aus Anzeige-String gebildet werden konnte }
var
  S: string;
  sAnzeigeBuf: string;
  sVK: string;
  fValue: single;
  iCode: integer;
  sMantisse: string;
  sExponent: string;

begin
  Result:=false;
  sRohwert:='';  // Vorbelegung Rückgabe

  try
    sAnzeigeBuf:=sAnzeige;
    if sDatentyp = C_PDT_ZAEHLERSTAND_FTL then begin  // Parameter-Datentyp 'Zählerstand'
      StrSubst (sAnzeigeBuf, ',', '.');  // Dezimal-Komma -> Dezimal-Punkt
      sVK:=F_Zerlegen (sAnzeigeBuf, '.');

      // Format: 9 Vorkommastellen, 3 Nachkommastellen, kein Dezimalzeichen
      sRohwert:=F_LeftPad (sVK, '0', 9) + F_RightPad (sAnzeigeBuf, '0', 3);
    end
    else if sDatentyp = C_PDT_FLOAT_FTL then begin  // Parameter-Datentyp 'Float'
      StrSubst (sAnzeigeBuf, ',', '.');  // Dezimal-Komma -> Dezimal-Punkt
      Val (sAnzeigeBuf, fValue, iCode);
      if iCode = 0 then begin
        // Format: Float -d.ddddddE-dd
        S:=Format ('%.7e', [fValue]);
        if length (S) > 0 then begin
          StrSubst (S, ',', '.');  // Dezimal-Komma -> Dezimal-Punkt
          // Fehlendes + voranstellen
          if (S [1] <> '+') AND (S [1] <> '-') then
            S:='+' + S;
          // Exponent fest 2-stellig
          sExponent:=S;
          sMantisse:=F_Zerlegen (sExponent, 'E');
          sRohwert:=sMantisse + 'E' +
            Copy (sExponent, 1, 1) +  // Exponent-Vorzeichen
            Copy (sExponent, length (sExponent) - 1, 2);  // Exponent letzte 2 Stellen
        end else
          exit;
      end else
        exit;
    end else  // unbekannter Parameter-Datentyp
      exit;

    Result:=true;  // OK, Parameter-Rohwert konnte gebildet werden
  except
    sRohwert:='';
    Result:=false;
  end;
end;

{-------------------------------------------------------------------}
procedure GetStatusMeldungNrListe_Elster_IEC (sStatusRohWert: string;
  slMeldungNr: TStrings);  // 16.08.2012, WW
{-------------------------------------------------------------------}
{ Konvertiert Meldungsnummern aus Elster IEC Statusparameter-Rohwert in Liste;
  Übergabe: Parameter-Rohwert
  Übergabe/Rückgabe: Liste mit Status-Meldungsnummern }
var
  sMeldNr: string;
  S: string;

begin
  if not Assigned (slMeldungNr) then exit;

  slMeldungNr.Clear;
  // Meldungsnummer(n) sind im Parameterwert durch Space getrennt, z.B. 1 5 16
  S:=sStatusRohWert;
  sMeldNr:=F_Zerlegen (S, ' ');
  while length (sMeldNr) > 0 do begin
    Application.ProcessMessages;
    slMeldungNr.Add (sMeldNr);
    sMeldNr:=F_Zerlegen (S, ' ');
  end;
end;


{ TParameterItem }

{-------------------------------------------------------------------}
Constructor TParameterItem.Create (AAllgNum, AWert, AMRGNum: string);
{-------------------------------------------------------------------}
Begin
  Inherited Create;
  AllgNum := AAllgNum;
  Wert := AWert;
  MRGNum := AMRGNum;
End;


{ TParameterListe }

{-------------------------------------------------------}
Constructor TParameterListe.Create (AKonfigPfad: string);
{-------------------------------------------------------}
Begin
  Inherited Create;
  KonfigPfad:=AKonfigPfad;
  FErsatzzeichen:=false;
End;

{---------------------------------}
procedure TParameterListe.Clear;
{---------------------------------}
var
  i: integer;
begin
  for i:=0 to Count-1 do TParameterItem (Items [i]).Free;

  inherited Clear;
end;

{---------------------------------}
procedure TParameterListe.Delete(iIndex: integer);
{---------------------------------}
begin
  if (Assigned(Items [iIndex])) then TObject(Items [iIndex]).Free;

  inherited Delete(iIndex);
end;

{---------------------------------------------------------------------------------------}
function TParameterListe.Update (AllgNum: string; Wert: string; MrgNum: string): boolean;
{---------------------------------------------------------------------------------------}
{ einzelnen Parameter (Wert, MRG-Parameternummer) in Liste updaten bzw. neuen
  Parameter-Eintrag einfügen, wenn für allg. Parameternummer noch nicht enthalten;
  Übergabe: allg. Parameternummer, Parameterwert, gerätespezifische Parameternummer
  Ergebnis: true, wenn Parameter-Eintrag geupdatet wurde (also bereits vorhanden
            war, nicht neu eingefügt wurde) }
var
  Index: integer;
  ParameterItem: TParameterItem;

begin
  if Search (AllgNum, Index) then begin
    ParameterItem:=TParameterItem (Items [Index]);
    ParameterItem.Wert:=Wert;
    ParameterItem.MrgNum:=MrgNum;
    Result:=true;
  end
  else begin
    ParameterItem:= TParameterItem.Create (AllgNum, Wert, MrgNum);
    Add (ParameterItem);
    Result:=false;
  end;
end;

{------------------------------------------------------------------------------}
Function TParameterListe.Search (AAllgNum: string; var Index: integer): boolean;
{------------------------------------------------------------------------------}
{ sucht in ParameterListe nach Eintrag mit allg. Parameternummer;
  Übergabe: allg. Parameternummer
  Rückgabe: Listenindex
  Ergebnis: true, wenn Eintrag gefunden }
var
  i: integer;

begin
  Result:=false;
  Index:=0;
  for i:=0 to Count-1 do begin
    Application.ProcessMessages;
    if TParameterItem(Items [i]).AllgNum = AAllgNum then begin
      Index:=i;
      Result:=true;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------------}
Function TParameterListe.SearchByMrgNum (AMrgNum: string; var Index: integer): boolean;
{-------------------------------------------------------------------------------------}
{ sucht in ParameterListe nach Eintrag mit MRG-Parameternummer;
  Übergabe: MRG-Parameternummer
  Rückgabe: Listenindex
  Ergebnis: true, wenn Eintrag gefunden }
var
  i: integer;

begin
  Result:=false;
  Index:=0;
  for i:=0 to Count-1 do begin
    Application.ProcessMessages;
    if TParameterItem(Items [i]).MrgNum = AMrgNum then begin
      Index:=i;
      Result:=true;
      Break;
    end;
  end;
end;

{-----------------------------------------------------------------------------}
Function TParameterListe.GetValue (AllgNum: string; var Wert: string): boolean;
{-----------------------------------------------------------------------------}
{ liefert Parameterwert als String über allg. Parameternummer;
  Übergabe: allg. Parameternummer
  Rückgabe: Parameterwert
  Ergebnis: true, wenn Eintrag mit allg. Nummer in Liste gefunden }
Var
  Index : Integer;
Begin
  Result := false;
  Wert := '';
  If Search (AllgNum, Index) Then Begin
    Wert := TParameterItem (Items [Index]).Wert;
    Result := true;
  End;
End;

{------------------------------------------------------------------------------------}
Function TParameterListe.GetValueByMrgNum (MrgNum: string; var Wert: string): boolean;
{------------------------------------------------------------------------------------}
{ liefert Parameterwert als String über MRG-Parameternummer;
  Übergabe: MRG-Parameternummer
  Rückgabe: Parameterwert
  Ergebnis: true, wenn Eintrag mit MRG-Nummer in Liste gefunden }
Var
  Index : Integer;
Begin
  Result := false;
  Wert := '';
  If SearchByMrgNum (MrgNum, Index) Then Begin
    Wert := TParameterItem (Items [Index]).Wert;
    Result := true;
  End;
End;

{-----------------------------------------------------------------------------------}
Function TParameterListe.GetValueDouble (AllgNum: string; var Wert: Double): Boolean;
{-----------------------------------------------------------------------------------}
{ liefert Parameterwert als Double über allgemeine Parameternummer;
  Übergabe: allg. Parameternummer
  Rückgabe: Parameterwert
  Ergebnis: true, wenn Eintrag mit allg. Nummer in Liste gefunden }
Var
  sParam : string;
  Code : Integer;

begin
  Result := False;
  Wert := 0;
  If GetValue (AllgNum, sParam) Then Begin
    Val (sParam, Wert, Code);
    Result := Code = 0;
  End;
End;

{---------------------------------------------------------------------------------}
Function TParameterListe.GetValueInt (AllgNum: string; var Wert: integer): Boolean;
{---------------------------------------------------------------------------------}
{ liefert Parameterwert als Integer über allgemeine Parameternummer;
  Übergabe: allg. Parameternummer
  Rückgabe: Parameterwert
  Ergebnis: true, wenn Eintrag mit allg. Nummer in Liste gefunden }
Var
  sParam : string;
  Code : Integer;

begin
  Result := False;
  Wert := 0;
  If GetValue (AllgNum, sParam) Then Begin
    Val (sParam, Wert, Code);
    Result := Code = 0;
  End;
End;

{----------------------------------------------------------------------------------------}
Function TParameterListe.LoadFromFile (FileName: TFileName; ParaKonv: TParaKonv;
  AParamMrgKonfigList: TParamMrgKonfigList = nil;
  AParamEKKonfigList: TParamEKKonfigList = nil): Boolean;
{----------------------------------------------------------------------------------------}
{ Parameterliste mit Inhalt von FileName (Rohdaten) neu füllen und sortieren;
  Übergabe: Parameter-Rohfilename
            Record mit Angaben für MRG-Parameterkonvertierung
            Liste mit Parameternummern-Konfiguration (optional: Wenn nil, wird die
              Konfiguration aus der Ressourcendatei gelesen)
            Liste mit Elster EK-Parameternummern-Konfiguration (optional: Wenn nil,
              wird die Konfiguration aus der Ressourcendatei gelesen, sofern erforderlich)
   Ergebnis: true, wenn Rohfile erfolgreich in Liste konvertiert werden konnte }
var
  ParamMrgKonfigList: TParamMrgKonfigList;
  FL: TTextListe;

Begin
  if not Assigned (AParamMrgKonfigList) then
    ParamMrgKonfigList:=TParamMrgKonfigList.Create  // Parameternummern-Konfigurationsliste lokal anlegen
  else
    ParamMrgKonfigList:=AParamMrgKonfigList;  // übergebene Parameternummern-Konfigurationsliste wird verwendet; 09.07.2021, WW
  try
    { Parameternummern-Konfigurationsliste aus Resourcendatei laden, wenn lokal angelegt: }
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      GetParamMrg_KonfigList_ByParaGruppe (ParaKonv.ParaGruppe,
                                           ParaKonv.ParaUnterGruppe,  // 11.02.2022, WW
                                           ParamMrgKonfigList, KonfigPfad);

    { Rohdatenfile konvertieren: }
    FL:=TTextListe.Create;
    try
      FL.Add (FileName);
      Result:=KonvRohdatenFromFileList (FL, ParaKonv, ParamMrgKonfigList,
                                        AParamEKKonfigList);
    finally
      FL.Free;
    end;
  finally
    // Parameternummern-Konfigurationsliste nur freigeben, wenn lokal angelegt
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      ParamMrgKonfigList.Free;
  end;
End;

{---------------------------------------------------------------------------------------}
Function TParameterListe.LoadFromFileList (FileNameList: TTextListe; ParaKonv: TParaKonv;
  AParamMrgKonfigList: TParamMrgKonfigList = nil;
  AParamEKKonfigList: TParamEKKonfigList = nil): Boolean;
{---------------------------------------------------------------------------------------}
{ Parameterliste mit Inhalt von FileNameList (Rohdaten-Files) neu füllen und sortieren;
  Übergabe: Liste mit Parameter-Rohfilenamen
            Record mit Angaben für MRG-Parameterkonvertierung
            Liste mit Parameternummern-Konfiguration (optional: Wenn nil, wird die
              Konfiguration aus der Ressourcendatei gelesen)
            Liste mit Elster EK-Parameternummern-Konfiguration (optional: Wenn nil,
              wird die Konfiguration aus der Ressourcendatei gelesen, sofern erforderlich)
   Ergebnis: true, wenn Rohfiles erfolgreich in Liste konvertiert werden konnten }
var
  ParamMrgKonfigList: TParamMrgKonfigList;

Begin
  if not Assigned (AParamMrgKonfigList) then
    ParamMrgKonfigList:=TParamMrgKonfigList.Create  // Parameternummern-Konfigurationsliste lokal anlegen
  else
    ParamMrgKonfigList:=AParamMrgKonfigList;  // übergebene Parameternummern-Konfigurationsliste wird verwendet; 09.07.2021, WW
  try
    { Parameternummern-Konfigurationsliste aus Resourcendatei laden, wenn lokal angelegt: }
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      GetParamMrg_KonfigList_ByParaGruppe (ParaKonv.ParaGruppe,
                                           ParaKonv.ParaUnterGruppe,  // 11.02.2022, WW
                                           ParamMrgKonfigList, KonfigPfad);

    { Rohdatenfiles konvertieren: }
    Result:=KonvRohdatenFromFileList (FileNameList, ParaKonv, ParamMrgKonfigList,
                                      AParamEKKonfigList);
  finally
    // Parameternummern-Konfigurationsliste nur freigeben, wenn lokal angelegt
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      ParamMrgKonfigList.Free;
  end;
End;


{ Lädt Liste aus XML-Datenstring                                 }
{ Parameter: Dateiname                                           }
{ Rückgabe: Erfolg ja/nein                                       }
{----------------------------------------------------------------}
function TParameterListe.LoadFromXmlString (sXmlString: string): boolean;
{----------------------------------------------------------------}

  function DecodeXmlLine(
    sLine: string; var sMParamNr, sAParamNr, sValue: string): boolean;
  var
    s, sVal : string;
    iIndex  : integer;
    pTool   : TDecodeXMLResponse;
  begin
    sMParamNr := '';
    sAParamNr := '';
    sValue := '';

    pTool := TDecodeXMLResponse.Create;
    try
      // Zunächst Wert ermitteln
      sValue := pTool.GetDataPart(sLine, 2, C_gt_Subst, false);  // wegen MRG-Parameterwert kein Trim ! 24.10.2007; WW
      sValue := DecodeXml(sValue);  // XML-Ersatzzeichen decodieren; 18.06.2020, WW
      sLine := pTool.GetDataPart(sLine, 1, C_gt_Subst);

      iIndex := 1;
      s := pTool.GetDataPart(sLine, iIndex, C_quot_Subst);
      while (s <> '') do begin
        Application.ProcessMessages;
        // Zugehörigen Wert abfragen
        Inc(iIndex);
        sVal := pTool.GetDataPart(sLine, iIndex, C_quot_Subst);

        // Um welchen Kennwert handelt es sich ?
        if (s = C_MrgKennungAllgParamNr) then sAParamNr := Trim(sVal)
        else if (s = C_MrgKennungMrgParamNr) then sMParamNr := Trim(sVal);

        // Nächste Kennung einlesen
        Inc(iIndex);
        s := pTool.GetDataPart(sLine, iIndex, C_quot_Subst);
      end;
      Result := (sMParamNr <> '') or (sAParamNr <> '');
    finally
      pTool.Free;
    end;
  end;

var
  sXmlData, sLine     : string;
  sANr, sMrgNr, sWert : string;
begin
  Result := False;
  if (Pos(C_WieserStart, sXmlString) > 0) and
     (Pos(C_MRGBlockStart, sXmlString) > 0) then
  begin

    with TDecodeXMLResponse.Create do
    try
      sXmlData :=
        CutDataString(sXmlString, C_MRGParamlistStart, C_MRGParamlistEnd);
      sLine := GetNextDataBlock(sXmlData, C_lt_Subst + C_MRGParameter + ' ',
        C_lt_Subst + '/' + C_MRGParameter + C_gt_Subst, '', false);  // wegen MRG-Parameterwert kein Trim ! 24.10.2007; WW
      while (sLine <> '') do begin
        Application.ProcessMessages;
        // Decodieren, bei Fehler: raus
        if (not DecodeXmlLine(sLine, sMrgNr, sANr, sWert))
        then Exit
        else Self.Add(TParameterItem.Create (sANr, sWert, sMrgNr));
        sLine := GetNextDataBlock(sXmlData, C_lt_Subst + C_MRGParameter + ' ',
          C_lt_Subst + '/' + C_MRGParameter + C_gt_Subst, '', false);  // wegen MRG-Parameterwert kein Trim ! 24.10.2007; WW
      end;
      Result := True;
    finally
      Free;
    end;

  end;
end;

{ Lädt Liste aus XML-Datei                                       }
{ Parameter: Dateiname                                           }
{            Flag Datei nach dem Laden löschen ja/nein           }
{ Rückgabe: Erfolg ja/nein                                       }
{----------------------------------------------------------------}
function TParameterListe.LoadFromXmlFile (sFileName: TFileName;
  bDelete: boolean): Boolean;
{----------------------------------------------------------------}
var
  S: string;
begin
  Result := False;
  Clear;         { Liste leeren }
  if (FileExists(sFileName)) then begin
    // wegen möglicher binärer MRG-Parameterwerte kein TStringList ! 28.08.2008; WW
    S:=StringFromFile (sFileName);
    Result := Self.LoadFromXmlString(S);
    Sort (ParaNrAllgCompare);    { Liste nach allgemeiner Parameternummer sortieren }
  end;
  if bDelete then
    DeleteFile (sFileName);
end;

{---------------------------------------------------------------------------------------------------}
Function TParameterListe.KonvRohdatenFromFileList (FileNameList: TTextListe; ParaKonv: TParaKonv;
                                                   ParamMrgKonfigList: TParamMrgKonfigList;
                                                   ParamEKKonfigList: TParamEKKonfigList): Boolean;
{---------------------------------------------------------------------------------------------------}
{ Parameterliste mit Inhalt von FileNameList (Rohdaten) neu füllen und sortieren;
  Übergabe: Liste mit Parameter-Rohfilenamen
            Record mit Angaben für MRG-Parameterkonvertierung
            Liste mit Parameternummern-Konfiguration
            Liste mit Elster EK-Parameternummern-Konfiguration
   Ergebnis: true, wenn Rohfile erfolgreich in Liste konvertiert werden konnte }
var
  i: integer;
  FileName: string;

Begin
  Result := True;
  Clear;         { Liste leeren }

  for i:=0 to FileNameList.Count - 1 do begin
    FileName:=FileNameList [i];
    case ParaKonv.ParaKonvGruppe of
      1: if not KonvPara_Wieser (FileName, ParaKonv.ParaGruppe, ParamMrgKonfigList,
           CLen_MrgParaNr_Wieser) then Result:=false; { Wieser-Geräte }
      2: if not KonvPara_MemoDat (FileName) then Result:=false;  { Boritec MemoDat 65 }
      3: if not KonvPara_IEC1107 (FileName, ParaKonv.ParaGruppe,
           ParamMrgKonfigList, ParamEKKonfigList) then Result:=false; { Elster DL 240, EK 260 }
      4: if not KonvPara_Tritschler_IEC (FileName, ParaKonv.ParaGruppe,
           ParamMrgKonfigList) then Result:=false; { Tritschler VC2/TTG/TDS/VC3/VCC, Datacon FWU (IEC-Protokoll) }
      5: if not KonvPara_DS100 (FileName, ParaKonv.ParaGruppe,
           ParamMrgKonfigList) then Result:=false; { Elster DS-100 }
      6: if not KonvPara_Wieser (FileName, ParaKonv.ParaGruppe, ParamMrgKonfigList,
           CLen_MrgParaNr_RMG_EC) then Result:=false; { RMG EC 694, EC 900 }
      7: if not KonvPara_Tritschler_FTL (FileName, ParaKonv.ParaGruppe,
           ParamMrgKonfigList) then Result:=false; { Tritschler TTG (FTL-Protokoll) }
      8: if not KonvPara_Corus (FileName, ParaKonv.ParaGruppe,
           ParamMrgKonfigList) then Result:=false; { Actaris Corus }
    end;  { case }
    Sort (ParaNrAllgCompare);    { Liste nach allgemeiner Parameternummer sortieren }

    if ParaKonv.RohLoeschen then begin
      { Aus dem Rohfilenamen evtl. vorangestellte Kanalnummer incl. Strichpunkt-Trenner
        löschen: 08.09.2011, WW }
      if Pos (';', Filename) > 0 then
        F_Zerlegen (Filename, ';');
      DeleteFile (FileName);
    end;
  end;  { for i }
End;

{---------------------------------------------------------------------------}
Function TParameterListe.LoadFromHeapA (HeapStr: string; ParaKonv: TParaKonv;
  AParamMrgKonfigList: TParamMrgKonfigList = nil): Boolean;
{---------------------------------------------------------------------------}
{ Parameterliste mit Inhalt von HeapStr (Rohdaten eines einzelnen Parameters
  der Gruppe A (ParaKonvGruppe 1 = Wieser)) füllen/updaten und bei Bedarf sortieren;
  Übergabe: Parameter-Rohstring
            Record mit Angaben für MRG-Parameterkonvertierung
            Liste mit Parameternummern-Konfiguration (optional: Wenn nil, wird die
              Konfiguration aus der Ressourcendatei gelesen)
  Ergebnis: true, wenn Rohstring erfolgreich in Liste konvertiert werden konnte }
var
  ParamMrgKonfigList: TParamMrgKonfigList;

Begin
  if not Assigned (AParamMrgKonfigList) then
    ParamMrgKonfigList:=TParamMrgKonfigList.Create  // Parameternummern-Konfigurationsliste lokal anlegen
  else
    ParamMrgKonfigList:=AParamMrgKonfigList;  // übergebene Parameternummern-Konfigurationsliste wird verwendet; 09.07.2021, WW
  try
    { Parameternummern-Konfigurationsliste aus Resourcendatei laden, wenn lokal angelegt: }
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      GetParamMrg_KonfigList_ByParaGruppe (ParaKonv.ParaGruppe,
                                           ParaKonv.ParaUnterGruppe,  // 11.02.2022, WW
                                           ParamMrgKonfigList, KonfigPfad);

    { Rohdatenstring konvertieren: }
    Result:=KonvEinzelparameter_Wieser (HeapStr, ParaKonv.ParaGruppe,
                                        ParamMrgKonfigList, CLen_MrgParaNr_Wieser);
  finally
    // Parameternummern-Konfigurationsliste nur freigeben, wenn lokal angelegt
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      ParamMrgKonfigList.Free;
  end;
End;

{-----------------------------------------------------------------------------}
Procedure TParameterListe.LoadFromParameterlist (PL_Src: TParameterListe;
                                                 ClearOldList: boolean = true);
{-----------------------------------------------------------------------------}
{ Parameterliste mit Inhalt von PL neu füllen (Kopie); sortieren nicht nötig, da PL
  schon sortiert ist;
  Übergabe: zu kopierende Quell-Parameterliste
            Flag 'ClearOldList' (true = Liste vor dem Kopieren leeren) }
var
  ParameterItem: TParameterItem;
  i: integer;
  Neu_Sortieren: boolean;

Begin
  if ClearOldList then begin
    Clear;         { Liste leeren }
    for i:=0 to PL_Src.Count - 1 do begin
      Application.ProcessMessages;
      ParameterItem:=TParameterItem.Create (TParameterItem (PL_Src [i]).AllgNum,
                                            TParameterItem (PL_Src [i]).Wert,
                                            TParameterItem (PL_Src [i]).MRGNum);
      Add (ParameterItem);
    end;
  end
  else begin
    Neu_Sortieren:=false;
    for i:=0 to PL_Src.Count - 1 do begin
      Application.ProcessMessages;
      if not Update (TParameterItem (PL_Src [i]).AllgNum,
                     TParameterItem (PL_Src [i]).Wert,
                     TParameterItem (PL_Src [i]).MrgNum) then
        Neu_Sortieren:=true;
    end;
    if Neu_Sortieren then
      Sort (ParaNrAllgCompare); { Liste nach allg. Parameternummer sortieren, wenn
                                  Eintrag neu eingefügt wurde }
  end;
End;

{----------------------------------------------------------------}
function TParameterListe.LoadFromModbusRegisterRequestList (
  RegisterRequestListe: TRegisterRequestList; ParaKonv: TParaKonv;
  AParamMrgKonfigList: TParamMrgKonfigList = nil): boolean;
{----------------------------------------------------------------}
{ Parameterliste mit den in der Modbus-Register-Requestliste enthaltenen
  Konvertierungslisten-Werten neu füllen und sortieren;
  Übergabe: Register-Requestliste
            Record mit Angaben für MRG-Parameterkonvertierung
            Liste mit Parameternummern-Konfiguration (optional: Wenn nil, wird die
              Konfiguration aus der Ressourcendatei gelesen)
   Ergebnis: true, wenn Werte erfolgreich in Liste konvertiert werden konnten }
var
  ParamMrgKonfigList: TParamMrgKonfigList;
  RegisterKonvListe: TRegisterKonvList;
  i: integer;

Begin
  if not Assigned (AParamMrgKonfigList) then
    ParamMrgKonfigList:=TParamMrgKonfigList.Create  // Parameternummern-Konfigurationsliste lokal anlegen
  else
    ParamMrgKonfigList:=AParamMrgKonfigList;  // übergebene Parameternummern-Konfigurationsliste wird verwendet; 09.07.2021, WW
  try
    { Parameternummern-Konfigurationsliste aus Resourcendatei laden, wenn lokal angelegt: }
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      GetParamMrg_KonfigList_ByParaGruppe (ParaKonv.ParaGruppe,
                                           ParaKonv.ParaUnterGruppe,  // 11.02.2022, WW
                                           ParamMrgKonfigList, KonfigPfad);

    { Modbus-Register-Requestliste konvertieren: }
    Result:=true;
    Clear;         { Liste leeren }

    if Assigned (RegisterRequestListe) then begin
      for i:=0 to RegisterRequestListe.Count - 1 do begin
        { Register-Konvliste enthält die Parameterdaten: }
        RegisterKonvListe:=
          TRegisterRequestDataObj (RegisterRequestListe [i]).Data.RegisterKonvListe;

        case ParaKonv.ParaKonvGruppe of
          9: if not KonvPara_Modbus (RegisterKonvListe, ParaKonv.ParaGruppe,
               ParamMrgKonfigList) then Result:=false; { Modbus-Parameterdaten }
        end;  { case }
        Sort (ParaNrAllgCompare);    { Liste nach allgemeiner Parameternummer sortieren }
      end;  { for i }

      { Rohwerte zu Anzeigewerte verrechnen: }
      case ParaKonv.ParaGruppe of
//!!        135:   war nur ein Versuch...
//          CalcParaAnzeige_SICK;  // SICK FLOWSIC500; 29.09.2021, WW

        136, 137:
          CalcParaAnzeige_RSM200;  // RSM200; 09.01.2024, WW
      end;
    end;
  finally
    // Parameternummern-Konfigurationsliste nur freigeben, wenn lokal angelegt
    if not Assigned (AParamMrgKonfigList) then  // 09.07.2021, WW
      ParamMrgKonfigList.Free;
  end;
end;


{--------------------------- Gerätezustand ------------------------------------}

{-------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand_EK260 (AAllgNum: string;
  var iGerZustand: Int64): boolean;
{-------------------------------------------------------------------}
{ Virtuellen Gerätezustand für EK 260 bilden;
  Übergabe: Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;
  slMeldungNr: TStrings;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  slMeldungNr:=TStringList.Create;
  try
    // Aktueller Gerätezustand:
    if (AAllgNum = CP_ELS_EK260_GerZustand_aktuell) then begin
      // Geräteparameter: Aktueller Systemstatus
      if not GetValue (CP_ELS_AktSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 1
      if not GetValue (CP_ELS_AktStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 2
      if not GetValue (CP_ELS_AktStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 3
      if not GetValue (CP_ELS_AktStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 4
      if not GetValue (CP_ELS_AktStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktStatusK4,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 5
      if not GetValue (CP_ELS_AktStatusK5, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktStatusK5,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 6
      if not GetValue (CP_ELS_AktStatusK6, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktStatusK6,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 7
      if not GetValue (CP_ELS_AktStatusK7, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktStatusK7,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 8
      if not GetValue (CP_ELS_AktStatusK8, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktStatusK8,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 9
      if not GetValue (CP_ELS_AktStatusK9, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_AktStatusK9,
                                      iGerZustand) then exit;
      Result:=true;
    end

    // Gespeicherter Gerätezustand:
    else if (AAllgNum = CP_ELS_EK260_GerZustand_gespeichert) then begin
      // Geräteparameter: System-Statusregister
      if not GetValue (CP_ELS_RegSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 1
      if not GetValue (CP_ELS_RegStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 2
      if not GetValue (CP_ELS_RegStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 3
      if not GetValue (CP_ELS_RegStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 4
      if not GetValue (CP_ELS_RegStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegStatusK4,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 5
      if not GetValue (CP_ELS_RegStatusK5, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegStatusK5,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 6
      if not GetValue (CP_ELS_RegStatusK6, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegStatusK6,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 7
      if not GetValue (CP_ELS_RegStatusK7, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegStatusK7,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 8
      if not GetValue (CP_ELS_RegStatusK8, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegStatusK8,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 9
      if not GetValue (CP_ELS_RegStatusK9, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK260 (slMeldungNr, CP_ELS_RegStatusK9,
                                      iGerZustand) then exit;
      Result:=true;
    end;
  finally
    slMeldungNr.Free;
  end;
end;

{-------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand_EK280 (AAllgNum: string;
  var iGerZustand: Int64): boolean;
{-------------------------------------------------------------------}
{ Virtuellen Gerätezustand für EK 280 bilden;
  Übergabe: Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;
  slMeldungNr: TStrings;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  slMeldungNr:=TStringList.Create;
  try
    // Aktueller Gerätezustand:
    if (AAllgNum = CP_ELS_EK280_GerZustand_aktuell) then begin
      // Geräteparameter: Aktueller Systemstatus
      if not GetValue (CP_ELS_AktSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Systemstatus 2
      if not GetValue (CP_ELS_AktSystemStatus2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktSystemStatus2,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 1
      if not GetValue (CP_ELS_AktStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 2
      if not GetValue (CP_ELS_AktStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 3
      if not GetValue (CP_ELS_AktStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 4
      if not GetValue (CP_ELS_AktStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK4,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 5
      if not GetValue (CP_ELS_AktStatusK5, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK5,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 6
      if not GetValue (CP_ELS_AktStatusK6, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK6,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 7
      if not GetValue (CP_ELS_AktStatusK7, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK7,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 8
      if not GetValue (CP_ELS_AktStatusK8, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK8,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 9
      if not GetValue (CP_ELS_AktStatusK9, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK9,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 10
      if not GetValue (CP_ELS_AktStatusK10, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_AktStatusK10,
                                      iGerZustand) then exit;
      Result:=true;
    end

    // Gespeicherter Gerätezustand:
    else if (AAllgNum = CP_ELS_EK280_GerZustand_gespeichert) then begin
      // Geräteparameter: System-Statusregister
      if not GetValue (CP_ELS_RegSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: System-Statusregister 2
      if not GetValue (CP_ELS_RegSystemStatus2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegSystemStatus2,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 1
      if not GetValue (CP_ELS_RegStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 2
      if not GetValue (CP_ELS_RegStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 3
      if not GetValue (CP_ELS_RegStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 4
      if not GetValue (CP_ELS_RegStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK4,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 5
      if not GetValue (CP_ELS_RegStatusK5, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK5,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 6
      if not GetValue (CP_ELS_RegStatusK6, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK6,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 7
      if not GetValue (CP_ELS_RegStatusK7, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK7,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 8
      if not GetValue (CP_ELS_RegStatusK8, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK8,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 9
      if not GetValue (CP_ELS_RegStatusK9, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK9,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 10
      if not GetValue (CP_ELS_RegStatusK10, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_EK280 (slMeldungNr, CP_ELS_RegStatusK10,
                                      iGerZustand) then exit;
      Result:=true;
    end;
  finally
    slMeldungNr.Free;
  end;
end;

{-------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand_DL240 (AAllgNum: string;
  var iGerZustand: Int64): boolean;
{-------------------------------------------------------------------}
{ Virtuellen Gerätezustand für DL 240 bilden;
  Übergabe: Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;
  slMeldungNr: TStrings;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  slMeldungNr:=TStringList.Create;
  try
    // Aktueller Gerätezustand:
    if (AAllgNum = CP_ELS_DL240_GerZustand_aktuell) then begin
      // Geräteparameter: Aktueller Systemstatus
      if not GetValue (CP_ELS_AktSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_AktSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 1
      if not GetValue (CP_ELS_AktStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_AktStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 2
      if not GetValue (CP_ELS_AktStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_AktStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 3
      if not GetValue (CP_ELS_AktStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_AktStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 4
      if not GetValue (CP_ELS_AktStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_AktStatusK4,
                                      iGerZustand) then exit;
      Result:=true;
    end

    // Gespeicherter Gerätezustand:
    else if (AAllgNum = CP_ELS_DL240_GerZustand_gespeichert) then begin
      // Geräteparameter: System-Statusregister
      if not GetValue (CP_ELS_RegSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_RegSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 1
      if not GetValue (CP_ELS_RegStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_RegStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 2
      if not GetValue (CP_ELS_RegStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_RegStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 3
      if not GetValue (CP_ELS_RegStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_RegStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 4
      if not GetValue (CP_ELS_RegStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL240 (slMeldungNr, CP_ELS_RegStatusK4,
                                      iGerZustand) then exit;
      Result:=true;
    end;
  finally
    slMeldungNr.Free;
  end;
end;

{-------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand_DL230 (AAllgNum: string;
  var iGerZustand: Int64): boolean;
{-------------------------------------------------------------------}
{ Virtuellen Gerätezustand für DL 230 bilden;
  Übergabe: Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;
  slMeldungNr: TStrings;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  slMeldungNr:=TStringList.Create;
  try
    // Aktueller Gerätezustand:
    if (AAllgNum = CP_ELS_DL230_GerZustand_aktuell) then begin
      // Geräteparameter: Aktueller Systemstatus
      if not GetValue (CP_ELS_AktSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Systemstatus 2
      if not GetValue (CP_ELS_AktSystemStatus2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktSystemStatus2,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 1
      if not GetValue (CP_ELS_AktStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 2
      if not GetValue (CP_ELS_AktStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 3
      if not GetValue (CP_ELS_AktStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 4
      if not GetValue (CP_ELS_AktStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktStatusK4,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 5
      if not GetValue (CP_ELS_AktStatusK5, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktStatusK5,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 6
      if not GetValue (CP_ELS_AktStatusK6, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktStatusK6,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 7
      if not GetValue (CP_ELS_AktStatusK7, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktStatusK7,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 8
      if not GetValue (CP_ELS_AktStatusK8, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_AktStatusK8,
                                      iGerZustand) then exit;
      Result:=true;
    end

    // Gespeicherter Gerätezustand:
    else if (AAllgNum = CP_ELS_DL230_GerZustand_gespeichert) then begin
      // Geräteparameter: System-Statusregister
      if not GetValue (CP_ELS_RegSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: System-Statusregister 2
      if not GetValue (CP_ELS_RegSystemStatus2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegSystemStatus2,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 1
      if not GetValue (CP_ELS_RegStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 2
      if not GetValue (CP_ELS_RegStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 3
      if not GetValue (CP_ELS_RegStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 4
      if not GetValue (CP_ELS_RegStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegStatusK4,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 5
      if not GetValue (CP_ELS_RegStatusK5, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegStatusK5,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 6
      if not GetValue (CP_ELS_RegStatusK6, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegStatusK6,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 7
      if not GetValue (CP_ELS_RegStatusK7, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegStatusK7,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 8
      if not GetValue (CP_ELS_RegStatusK8, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL230 (slMeldungNr, CP_ELS_RegStatusK8,
                                      iGerZustand) then exit;
      Result:=true;
    end;
  finally
    slMeldungNr.Free;
  end;
end;

{-------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand_DL220 (AAllgNum: string;
  var iGerZustand: Int64): boolean;
{-------------------------------------------------------------------}
{ Virtuellen Gerätezustand für DL 220 bilden;
  Übergabe: Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;
  slMeldungNr: TStrings;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  slMeldungNr:=TStringList.Create;
  try
    // Aktueller Gerätezustand:
    if (AAllgNum = CP_ELS_DL220_GerZustand_aktuell) then begin
      // Geräteparameter: Aktueller Systemstatus
      if not GetValue (CP_ELS_AktSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_AktSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 1
      if not GetValue (CP_ELS_AktStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_AktStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 2
      if not GetValue (CP_ELS_AktStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_AktStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 3
      if not GetValue (CP_ELS_AktStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_AktStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 4
      if not GetValue (CP_ELS_AktStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_AktStatusK4,
                                      iGerZustand) then exit;
      Result:=true;
    end

    // Gespeicherter Gerätezustand:
    else if (AAllgNum = CP_ELS_DL220_GerZustand_gespeichert) then begin
      // Geräteparameter: System-Statusregister
      if not GetValue (CP_ELS_RegSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_RegSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 1
      if not GetValue (CP_ELS_RegStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_RegStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 2
      if not GetValue (CP_ELS_RegStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_RegStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 3
      if not GetValue (CP_ELS_RegStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_RegStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 4
      if not GetValue (CP_ELS_RegStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL220 (slMeldungNr, CP_ELS_RegStatusK4,
                                      iGerZustand) then exit;
      Result:=true;
    end;
  finally
    slMeldungNr.Free;
  end;
end;

{-------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand_DL210 (AAllgNum: string;
  var iGerZustand: Int64): boolean;
{-------------------------------------------------------------------}
{ Virtuellen Gerätezustand für DL 210 bilden;
  Übergabe: Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;
  slMeldungNr: TStrings;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  slMeldungNr:=TStringList.Create;
  try
    // Aktueller Gerätezustand:
    if (AAllgNum = CP_ELS_DL210_GerZustand_aktuell) then begin
      // Geräteparameter: Aktueller Systemstatus
      if not GetValue (CP_ELS_AktSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_AktSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 1
      if not GetValue (CP_ELS_AktStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_AktStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 2
      if not GetValue (CP_ELS_AktStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_AktStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 3
      if not GetValue (CP_ELS_AktStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_AktStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Aktueller Status 4
      if not GetValue (CP_ELS_AktStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_AktStatusK4,
                                      iGerZustand) then exit;
      Result:=true;
    end

    // Gespeicherter Gerätezustand:
    else if (AAllgNum = CP_ELS_DL210_GerZustand_gespeichert) then begin
      // Geräteparameter: System-Statusregister
      if not GetValue (CP_ELS_RegSystemStatus, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_RegSystemStatus,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 1
      if not GetValue (CP_ELS_RegStatusK1, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_RegStatusK1,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 2
      if not GetValue (CP_ELS_RegStatusK2, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_RegStatusK2,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 3
      if not GetValue (CP_ELS_RegStatusK3, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_RegStatusK3,
                                      iGerZustand) then exit;

      // Geräteparameter: Statusregister 4
      if not GetValue (CP_ELS_RegStatusK4, Wert) then exit;
      GetStatusMeldungNrListe_Elster_IEC (Wert, slMeldungNr);
      if not SetGeraeteZustand_DL210 (slMeldungNr, CP_ELS_RegStatusK4,
                                      iGerZustand) then exit;
      Result:=true;
    end;
  finally
    slMeldungNr.Free;
  end;
end;

{--------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand_MRG900 (AAllgNum: string;
  var iGerZustand: Int64): boolean;
{--------------------------------------------------------------------}
{ Virtuellen Gerätezustand für MRG 905/910 bilden;
  Übergabe: Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  // Aktueller Gerätezustand:
  if (AAllgNum = CP_ALLG_MRG900_GerZustand_aktuell) then begin
    // Geräteparameter: Eichschalter
    if not GetValue (CP_ALLG_Eichschalter, Wert) then exit;
    if not SetGeraeteZustand_MRG900 (Wert, CP_ALLG_Eichschalter,
                                     iGerZustand) then exit;

    // Geräteparameter: Signalausgang 1 Zustand
    if not GetValue (CP_ALLG_Ausg1_Zustand, Wert) then exit;
    if not SetGeraeteZustand_MRG900 (Wert, CP_ALLG_Ausg1_Zustand,
                                     iGerZustand) then exit;

    // Geräteparameter: Signalausgang 2 Zustand
    if not GetValue (CP_ALLG_Ausg2_Zustand, Wert) then exit;
    if not SetGeraeteZustand_MRG900 (Wert, CP_ALLG_Ausg2_Zustand,
                                     iGerZustand) then exit;
    Result:=true;
  end;
end;

{--------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand_TME400 (AAllgNum: string;
  var iGerZustand: Int64): boolean;
{--------------------------------------------------------------------}
{ Virtuellen Gerätezustand für TME400 bilden;
  Übergabe: Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  // Aktueller Gerätezustand:
  if (AAllgNum = CP_RMG_TME400_GerZustand_aktuell) then begin
    // Geräteparameter: Fehlerregister
    if not GetValue (CP_RMG_TME400_Fehlerregister, Wert) then exit;
    if not SetGeraeteZustand_TME400 (Wert, CP_RMG_TME400_Fehlerregister,
                                     iGerZustand) then exit;

    // Geräteparameter: Warnungregister
    if not GetValue (CP_RMG_TME400_Warnungregister, Wert) then exit;
    if not SetGeraeteZustand_TME400 (Wert, CP_RMG_TME400_Warnungregister,
                                     iGerZustand) then exit;

    // Geräteparameter: Hinweisregister
    if not GetValue (CP_RMG_TME400_Hinweisregister, Wert) then exit;
    if not SetGeraeteZustand_TME400 (Wert, CP_RMG_TME400_Hinweisregister,
                                     iGerZustand) then exit;
    Result:=true;
  end;
end;

{--------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand_RSM200 (AAllgNum: string;
  var iGerZustand: Int64): boolean;
{--------------------------------------------------------------------}
{ Virtuellen Gerätezustand für RSM200 bilden;
  Übergabe: Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Gerätezustand als Integer
  Ergebnis: true, wenn bilden des Gerätezustands ohne Fehler }
var
  Wert: string;

begin
  Result:=false;
  iGerZustand:=0;  // Vorbelegung Gerätezustand: Alle Bits gelöscht

  // Aktueller Gerätezustand:
  if (AAllgNum = CP_RMG_RSM200_GerZustand_aktuell) then begin
    // Geräteparameter: Fehlerregister
    if not GetValue (CP_RMG_RSM200_Fehlerregister, Wert) then exit;
    if not SetGeraeteZustand_RSM200 (Wert, CP_RMG_RSM200_Fehlerregister,
                                     iGerZustand) then exit;

    // Geräteparameter: Warnungregister
    if not GetValue (CP_RMG_RSM200_Warnungregister, Wert) then exit;
    if not SetGeraeteZustand_RSM200 (Wert, CP_RMG_RSM200_Warnungregister,
                                     iGerZustand) then exit;

    // Geräteparameter: Hinweisregister
    if not GetValue (CP_RMG_RSM200_Hinweisregister, Wert) then exit;
    if not SetGeraeteZustand_RSM200 (Wert, CP_RMG_RSM200_Hinweisregister,
                                     iGerZustand) then exit;
    Result:=true;
  end;
end;

{-------------------------------------------------------------------------------}
function TParameterListe.BuildGeraeteZustand (AMrgTyp: integer; AAllgNum: string;
  var sGerZustandHex: string): boolean;
{-------------------------------------------------------------------------------}
{ Wert für virtuellen Gerätezustand-Parameter bilden;
  Übergabe: Gerätetyp
            Allgemeine Parameternummer für Gerätezustand
  Rückgabe: Hex-codierter Gerätezustand
  Ergebnis: true, wenn bilden des Werts ohne Fehler }
var
  iGerZustand: Int64;

begin
  Result:=false;
  sGerZustandHex:='';  // Vorbelegung Rückgabe

  case AMrgTyp of
    mrgtyp_EK260: if not BuildGeraeteZustand_EK260 (AAllgNum, iGerZustand) then exit;
    mrgtyp_EK280: if not BuildGeraeteZustand_EK280 (AAllgNum, iGerZustand) then exit;
    mrgtyp_DL240: if not BuildGeraeteZustand_DL240 (AAllgNum, iGerZustand) then exit;
    mrgtyp_DL230: if not BuildGeraeteZustand_DL230 (AAllgNum, iGerZustand) then exit;
    mrgtyp_DL220: if not BuildGeraeteZustand_DL220 (AAllgNum, iGerZustand) then exit;
    mrgtyp_DL210: if not BuildGeraeteZustand_DL210 (AAllgNum, iGerZustand) then exit;
    mrgtyp_MRG905,
    mrgtyp_MRG910: if not BuildGeraeteZustand_MRG900 (AAllgNum, iGerZustand) then exit;
    mrgtyp_TME400_VCF,
    mrgtyp_TME400_VMF: if not BuildGeraeteZustand_TME400 (AAllgNum, iGerZustand) then exit;  // 04.04.2024, WW
    mrgtyp_RSM200_VCF,
    mrgtyp_RSM200_VMF: if not BuildGeraeteZustand_RSM200 (AAllgNum, iGerZustand) then exit;  // 04.04.2024, WW
  else
    exit;  // Fehler: Gerätezustand wurde nicht gebildet
  end;  { case AMrgTyp }

  sGerZustandHex:=IntToHex (iGerZustand, 1);  // Hex, nur soviele Stellen wie nötig
  Result:=true;
end;

{----------------------------------------------------------------------------------------}
function TParameterListe.LoadGeraeteZustand (AMrgTyp: integer; AAllgNum: string): boolean;
{----------------------------------------------------------------------------------------}
{ Wert für virtuellen Gerätezustand-Parameter bilden und in Parameterliste eintragen;
  Übergabe: Gerätetyp
            Allgemeine Parameternummer für Gerätezustand }
var
  sGerZustandHex: string;
  ParameterItem: TParameterItem;

begin
  Result:=BuildGeraeteZustand (AMrgTyp, AAllgNum, sGerZustandHex);

  Clear;  { Liste leeren }
  if Result then begin
    { Gerätezustand in Parameterliste eintragen: }
    ParameterItem:=TParameterItem.Create (AAllgNum, sGerZustandHex, '');
    Add (ParameterItem);
  end;
end;

{------------ Parametergruppen-spezifische Konvertierungsmethoden -------------}

{--------------------------------------------------------------------------------}
function TParameterListe.KonvPara_Wieser (Rohfilename: string;
                                          ParameterGruppe: integer;
                                          ParamMrgKonfigList: TParamMrgKonfigList;
                                          MrgNumLen: integer): boolean;
{--------------------------------------------------------------------------------}
{ Konvertieren von Parametern vom Typ "Wieser" (ParaKonvGruppe 1);
  Ergebnis: true, wenn Konvertieren erfolgreich }
var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  Modus: byte;
  i: integer;
  AnzGelesen: integer;
  Zeichen : char;
  ParameterString: string;
  AllgNum: string;
  Wert: string;
  MRGNum: string;
  ParameterItem: TParameterItem;

begin
  if not FileExists (Rohfilename) then begin
    Result:=false;
    exit;
  end;

  { Parameter aus Rohfile einlesen: }
  FS := TFileOfCharStream.Create (RohfileName, fmOpenRead OR fmShareDenyWrite);
  try
    Result := True;
    Modus := m_Beginning;
    For i := 1 to FS.Size Do Begin
      AnzGelesen:=FS.Read (Zeichen);
      Application.ProcessMessages;
      If AnzGelesen < 1 Then Begin
        Result:=false;
        Break;
      end;

      Case Modus of
        m_Beginning :
          Begin
            If (Zeichen = 'B') OR (Zeichen = 'b') Then
              Modus := m_NewParam;
          End;

        m_SameParam :
          Begin
            If (Zeichen > #31) Then
              ParameterString:=ParameterString + Zeichen
            Else Begin
              Modus := m_NewParam;
              if SplitRohString_Wieser (ParameterString, ParameterGruppe,
                                        ParamMrgKonfigList, MrgNumLen,
                                        AllgNum, Wert, MrgNum) then begin
                ParameterItem:= TParameterItem.Create (AllgNum, Wert, MrgNum);
                Add (ParameterItem);
              end;
            End;
          End;

        m_NewParam :
          Begin
            If (Zeichen > #31) Then Begin
              ParameterString := Zeichen;
              Modus := m_SameParam;
            End;
          End;
      End; { case }
    End; { for }
  finally
    FS.Free;
  end;
end;

{-----------------------------------------------------------------------}
function TParameterListe.KonvPara_MemoDat (Rohfilename: string): boolean;
{-----------------------------------------------------------------------}
{ Konvertieren von Parametern vom Typ "Boritec MemoDat" (ParaKonvGruppe 2)
  -> Da das Gerät keine eindeutige Zuordnung "Parameter <-> Parameternummer" besitzt,
     werden die allgemeinen Parameternummern nicht (wie üblich) über die gerätespezifische
     Parameternummer aus der Parametertabelle geholt, sondern im Quellcode vergeben;
  Ergebnis: true, wenn Konvertieren erfolgreich }
type
  { Zustände für Rohdatenkonvertierung }
  TModus = (m_Start, m_DataLength, m_Data);

var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  zeichen: char;
  rohsatz: string;
  Modus: TModus;
  Befehl: char;
  Datenlaenge: byte;
  S: string;
  ParameterItem: TParameterItem;
  i: SmallInt;

begin
  Result:=true;
  if not FileExists (Rohfilename) then begin
    Result:=false;
    exit;
  end;
  try
    FS:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);   { Rohfile }
    try
      rohsatz := '';
      Befehl:=NUL;
      Datenlaenge:=0;
      Modus:=m_Start;   { als erstes wird das Startzeichen des 1. Telegramms gelesen }

      FSize:=FS.Size;
      while FS.Position < FSize do begin
        Application.ProcessMessages;
        FS.Read (zeichen);
        rohsatz := rohsatz + zeichen;

        case Modus of
          m_Start:
            begin
              if length (rohsatz) = 1 then begin
                if rohsatz [1] = '#' then     { Startzeichen gelesen }
                  Modus:=m_DataLength;    { als Nächstes bis zum Längenbyte lesen }
              end;
              rohsatz:='';
            end;

          m_DataLength:
            begin
              if length (rohsatz) = 2 then begin  { Befehlszeichen und Längenbyte gelesen }
                Befehl:=rohsatz [1];             { Befehlszeichen prüfen }
                Datenlaenge:=Ord (rohsatz [2]);
                Modus:=m_Data;                { als Nächstes das ganze Telegramm  }
                rohsatz:='';
              end;
            end;

          m_Data:
            begin
              if length (rohsatz) = (Datenlaenge + 2) then begin  { Telegramm-Daten plus Checksumme und Endezeichen gelesen }
                { Telegramm-Rohsatz konvertieren:
                  -> die allg. Parameternummer wird nicht über die Zuordnungstabelle
                     PARAMMRG.DB ermittelt, da in dieser Gerätegruppe keine gerätespezifischen
                     Parameternummern existieren
                  -> unter TParameterItem.MrgNum wird statt der gerätespezifischen
                     Parameternummer das Befehlszeichen plus fortlaufende Nr. eingetragen }
                case Befehl of
                  'a': begin
                         S:='$' + IntToHex (Ord (rohsatz [1]), 2);    { Betriebsart, Hex-Darstellung }
                         ParameterItem:= TParameterItem.Create (CP_BOR_Betriebsart, S, Befehl + '1');
                         Add (ParameterItem);
                         S:='$' + IntToHex (Ord (rohsatz [3]), 2) +   { Versionskennung, Hex-Darstellung }
                                  IntToHex (Ord (rohsatz [2]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Versionskennung, S, Befehl + '2');
                         Add (ParameterItem);
                       end;
                  'b': begin
                         i:=Bin2Smallint (Copy (rohsatz, 1, 2));   { Iw Zähler 1, Integer (LowByte HighByte) }
                         S:=IntToStr (i);
                         ParameterItem:= TParameterItem.Create (CP_BOR_IwZaehler1, S, Befehl + '1');
                         Add (ParameterItem);
                         i:=Bin2Smallint (Copy (rohsatz, 3, 2));   { Iw Nenner 1, Integer (LowByte HighByte) }
                         S:=IntToStr (i);
                         ParameterItem:= TParameterItem.Create (CP_BOR_IwNenner1, S, Befehl + '2');
                         Add (ParameterItem);
                         i:=Bin2Smallint (Copy (rohsatz, 5, 2));   { Iw Zähler 2, Integer (LowByte HighByte) }
                         S:=IntToStr (i);
                         ParameterItem:= TParameterItem.Create (CP_BOR_IwZaehler2, S, Befehl + '3');
                         Add (ParameterItem);
                         i:=Bin2Smallint (Copy (rohsatz, 7, 2));   { Iw Nenner 2, Integer (LowByte HighByte) }
                         S:=IntToStr (i);
                         ParameterItem:= TParameterItem.Create (CP_BOR_IwNenner2, S, Befehl + '4');
                         Add (ParameterItem);
                         S:=IntToStr(Ord (rohsatz [9]));    { Meßintervall }
                         ParameterItem:= TParameterItem.Create (CP_BOR_Messintervall, S, Befehl + '5');
                         Add (ParameterItem);
                       end;
                  'c': begin
                         i:=Bin2Smallint (Copy (rohsatz, 1, 2));   { Iw Zähler A, Integer (LowByte HighByte) }
                         S:=IntToStr (i);
                         ParameterItem:= TParameterItem.Create (CP_BOR_IwZaehlerA, S, Befehl + '1');
                         Add (ParameterItem);
                         i:=Bin2Smallint (Copy (rohsatz, 3, 2));   { Iw Nenner A, Integer (LowByte HighByte) }
                         S:=IntToStr (i);
                         ParameterItem:= TParameterItem.Create (CP_BOR_IwNennerA, S, Befehl + '2');
                         Add (ParameterItem);
                         S:=IntToStr (Ord (rohsatz [5]));    { Impulsdauer A }
                         ParameterItem:= TParameterItem.Create (CP_BOR_ImpulsdauerA, S, Befehl + '3');
                         Add (ParameterItem);
                         S:='$' + IntToHex (Ord (rohsatz [6]), 2);    { Zuordnung A, Hex-Darstellung }
                         ParameterItem:= TParameterItem.Create (CP_BOR_ZuordnungA, S, Befehl + '4');
                         Add (ParameterItem);
                       end;
                  'd': begin
                         S:=IntToHex (Ord (rohsatz [2]), 2) +   { Datum letzte Speicherung, BCD-Darstellung }
                            IntToHex (Ord (rohsatz [1]), 2) +
                            IntToHex (Ord (rohsatz [3]), 2) +
                            IntToHex (Ord (rohsatz [4]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Datum_letzteSp, S, Befehl + '1');
                         Add (ParameterItem);
                         S:=IntToHex (Ord (rohsatz [5]), 2) +   { Uhrzeit letzte Speicherung, BCD-Darstellung }
                            IntToHex (Ord (rohsatz [6]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Uhrzeit_letzteSp, S, Befehl + '2');
                         Add (ParameterItem);
                         { 10 Byte Reserve }
                         S:=IntToStr(Ord (rohsatz [17]));                 { Tageswechselzeit }
                         ParameterItem:= TParameterItem.Create (CP_BOR_Tageswechsel, S, Befehl + '3');
                         Add (ParameterItem);
                       end;
                  'e': begin
                         S:='$' + IntToHex (Ord (rohsatz [2]), 2) +   { LCD-Flags, Hex-Darstellung }
                                  IntToHex (Ord (rohsatz [1]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_LCD_Flags, S, Befehl);
                         Add (ParameterItem);
                       end;
                  'f': begin
                        S:=Copy (rohsatz, 1, 12);                 { Meßortkennung }
                        ParameterItem:= TParameterItem.Create (CP_BOR_Messortkennung, S, Befehl);
                        Add (ParameterItem);
                       end;
                  'g': begin
                         S:=IntToHex (Ord (rohsatz [1]), 2) +   { Totalzähler 1 (Summe), BCD-Darstellung }
                            IntToHex (Ord (rohsatz [2]), 2) +
                            IntToHex (Ord (rohsatz [3]), 2) +
                            IntToHex (Ord (rohsatz [4]), 2) +
                            IntToHex (Ord (rohsatz [5]), 2) +
                            IntToHex (Ord (rohsatz [6]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Totalzaehler1, S, Befehl + '1');
                         Add (ParameterItem);
                         S:=IntToHex (Ord (rohsatz [7]), 2) +   { Totalzähler 2, BCD-Darstellung }
                            IntToHex (Ord (rohsatz [8]), 2) +
                            IntToHex (Ord (rohsatz [9]), 2) +
                            IntToHex (Ord (rohsatz [10]), 2) +
                            IntToHex (Ord (rohsatz [11]), 2) +
                            IntToHex (Ord (rohsatz [12]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Totalzaehler2, S, Befehl + '2');
                         Add (ParameterItem);
                       end;
                  'h': begin
                         S:=IntToHex (Ord (rohsatz [2]), 2) +   { Baujahr, BCD-Darstellung }
                            IntToHex (Ord (rohsatz [1]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Baujahr, S, Befehl + '1');
                         Add (ParameterItem);
                         S:=IntToHex (Ord (rohsatz [3]), 2) +   { Version, BCD-Darstellung }
                            IntToHex (Ord (rohsatz [4]), 2) +
                            IntToHex (Ord (rohsatz [5]), 2) +
                            IntToHex (Ord (rohsatz [6]), 2) +
                            IntToHex (Ord (rohsatz [7]), 2) +
                            IntToHex (Ord (rohsatz [8]), 2) +
                            IntToHex (Ord (rohsatz [9]), 2) +
                            IntToHex (Ord (rohsatz [10]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Version, S, Befehl + '2');
                         Add (ParameterItem);
                         S:=IntToHex (Ord (rohsatz [11]), 2) +   { Seriennummer, BCD-Darstellung }
                            IntToHex (Ord (rohsatz [12]), 2) +
                            IntToHex (Ord (rohsatz [13]), 2) +
                            IntToHex (Ord (rohsatz [14]), 2) +
                            IntToHex (Ord (rohsatz [15]), 2) +
                            IntToHex (Ord (rohsatz [16]), 2) +
                            IntToHex (Ord (rohsatz [17]), 2) +
                            IntToHex (Ord (rohsatz [18]), 2) +
                            IntToHex (Ord (rohsatz [19]), 2) +
                            IntToHex (Ord (rohsatz [20]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Seriennummer, S, Befehl + '3');
                         Add (ParameterItem);
                       end;
                  'i': begin
                         i:=Bin2Smallint (Copy (rohsatz, 1, 2));  { Batteriezähler, Integer (LowByte HighByte) }
                         S:=IntToStr (i);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Batteriezaehler, S, Befehl + '1');
                         Add (ParameterItem);
                         S:='$' + IntToHex (Ord (rohsatz [4]), 2) +   { Fehlermerker, Hex-Darstellung }
                                  IntToHex (Ord (rohsatz [3]), 2);
                         ParameterItem:= TParameterItem.Create (CP_BOR_Fehlermerker, S, Befehl + '2');
                         Add (ParameterItem);
                       end;
                  'k': begin
                         S:=IntToStr (Ord (rohsatz [1]));    { Zuordnung Zähler }
                         ParameterItem:= TParameterItem.Create (CP_BOR_Zuord_Zaehler, S, Befehl);
                         Add (ParameterItem);
                       end;
                end;

                Modus:=m_Start;    { als Nächstes: Startzeichen von nächstem Telegramm lesen }
                rohsatz:='';
              end;
            end;
        end;  { case }
      end;  { while FS.Position < FSize }

      if length (rohsatz) > 0 then begin
        Result:=false;        { Fehler Rohdatenlänge }
        Clear;             { Liste löschen, Rohdaten enthalten evtl. Müll }
      end;
    finally
      FS.Free;
    end;
  except
    Result:=false;
  end;
end;

{-----------------------------------------------------------------------------------------}
function TParameterListe.KonvPara_IEC1107 (Rohfilename: string;
                                           ParameterGruppe: integer;
                                           ParamMrgKonfigList: TParamMrgKonfigList;
                                           AParamEKKonfigList: TParamEKKonfigList): boolean;
{-----------------------------------------------------------------------------------------}
{ Konvertieren von Parametern vom Typ IEC1107 (ParaKonvGruppe 3), z.B. Elster DL210,
  DL220, DL240, EK260, Actaris Sparklog;
  -> Rohfileformat: aneinandergehängte Antworten auf Parameter-Lesebefehle
  Ergebnis: true, wenn Konvertieren erfolgreich }
var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  dummy: char;
  sbuf: string;
  Adresse: string;
  Wert: string;
  AllgNum: string;
  VglAdr: string;
  ParameterItem: TParameterItem;
  ParamMrgData: TParamMrgData;
  isFehlertelegramm: boolean;
  sMedium: string;
  gefunden: boolean;
  S: string;
  i: integer;
  ParamEKKonfigList: TParamEKKonfigList;

begin
  Result:=false;
  if not FileExists (Rohfilename) then exit;
  try
    { Elster EK260, EK280: EK-spezifische Parameterliste laden oder übergebene verwenden }
    if (ParameterGruppe = 105) OR (ParameterGruppe = 127) then begin
      if not Assigned (AParamEKKonfigList) then
        ParamEKKonfigList:=TParamEKKonfigList.Create  // EK-Liste lokal anlegen
      else
        ParamEKKonfigList:=AParamEKKonfigList;  // übergebene EK-Liste wird verwendet; 09.07.2021, WW
    end else
      ParamEKKonfigList:=nil;  // EK-Liste nicht erforderlich
    try
      { EK-spezifische Parameternummern-Konfigurationsliste aus Resourcendatei
        laden, wenn lokal angelegt: }
      if not Assigned (AParamEKKonfigList) then  // 09.07.2021, WW
        if ParamEKKonfigList <> nil then
          GetParamEK_KonfigList_ByParaGruppe (ParameterGruppe, ParamEKKonfigList, KonfigPfad);

      FS:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);   { Rohfile }
      try
        { Konvertierung }
        FSize:=FS.Size;
        while FS.Position < FSize do begin
          Application.ProcessMessages;
          rohsatz:='';
          zeichen:=NUL;
          { Datensätze bilden: bis ETX oder LF lesen }
          while (zeichen <> ETX) AND (zeichen <> LF) AND (FS.Position < FSize) do begin
            FS.Read (zeichen);
            if (zeichen <> CR) AND (zeichen <> LF) then
              rohsatz:=rohsatz + zeichen;   { Datensatz bilden (ohne CR, LF) }

            { das auf ETX folgende BCC überlesen }
            if (zeichen = ETX) AND (FS.Position < FSize) then begin
              FS.Read (dummy);
              { vom Rohsatz interessiert nur der Teil zwischen STX und ETX:
                -> geändert wegen EK260 mit NUL-Zeichen bei Datenempfang mit Break, 11.04.2003, WW }
              rohsatz:=ExtractString (rohsatz, STX, zeichen, 0);
            end;
          end;

          { aus rohsatz Daten-Adresse und Daten-Wert(e) extrahieren: }
          sbuf:=F_Zerlegen (rohsatz, '(');                    { lesen bis '(' }
          if length (sbuf) > 0 then begin
            if length (rohsatz) > 0 then
              rohsatz:='(' + rohsatz;

            // Daten-Wert(e) stehen in Klammern, z.B. (1)(5)(16); 16.08.2012, WW
            Wert:='';
            i:=0;
            S:=ExtractString (rohsatz, '(', ')', i);  { Daten-Wert bzw. Fehlernummer }
            while length (S) > 0 do begin
              Application.ProcessMessages;
              if length (Wert) > 0 then
                Wert:=Wert + ' ';  { Trennzeichen Space bei mehreren Datenwerten }
              Wert:=Wert + S;
              inc (i);
              S:=ExtractString (rohsatz, '(', ')', i); { weiterer Datenwert }
            end;

            if (ParameterGruppe = 120) OR                   { Actaris Sparklog }
               (ParameterGruppe = 126) then begin        { Kamstrup UNIGAS 300 }
              Adresse:=sbuf;  { komplette Daten-Adresse }
              if Pos ('-', Adresse) > 0 then
                sMedium:=F_Zerlegen (Adresse, '-')  { mit Medium }
              else
                sMedium:='';

              { Wert-String auf Fehlertelegramm prüfen: gekennzeinet durch ERROR }
              isFehlertelegramm:=Pos ('ERROR', Wert) > 0;
            end
            else begin  { Elster DL210, DL220, 240, EK260 }
              { Daten-Adresse:
                -> alles ab einschließlich Meßgröße wegschneiden, da nicht relevant
                -> Medium wegschneiden, da nicht relevant }
              Adresse:=F_Zerlegen (sbuf, '.');
              if Pos ('-', Adresse) > 0 then
                F_Zerlegen (Adresse, '-');
              sMedium:='';

              { Wert-String auf Fehlertelegramm prüfen: gekennzeichnet durch # an erster Stelle }
              isFehlertelegramm:=Pos ('#', Wert) = 1;

              { Hex-kodierte Elster-Ersatzzeichen im Wert-String wandeln; 17.10.2023, WW }
              if not isFehlertelegramm then
                if WElsterHexToAscii (Wert) then
                  FErsatzzeichen:=true;  // Flag setzen: mit Elster Hex-Ersatzzeichen
            end;

            if not isFehlertelegramm then begin          { Fehlertelegramm verwerfen }
              { Allg. Parameternummer aus der Daten-Adresse ermitteln. Dazu
                Vergleichs-Adresse ohne führende Nullen bilden für Suche in Tabelle
                (Tabellen-Adressen sind ohne führende Nullen, z.B. 01:0170 -> 1:170)

                -> EDIS-Format der Datenadresse: M-KK:GG.AA.T*VV
                   M: Medium (optional, wird bei manchen Daten mitgeschickt)
                   KK: Kanal (max. 2-stellig, optional)
                   GG: Meßgröße (max. 2-stellig; Elster LIS-200: max. 4-stellig)
                   AA: Meßart (max. 2-stellig; Elster LIS-200: max. 3-stellig)
                   T: Tarifstufe (optional)
                   VV: Vorwert (max. 2-stellig) }

              rohsatz:=Adresse;
              if Pos (':', rohsatz) > 0 then begin  { Kanal ist optional }
                sbuf:=F_Zerlegen (rohsatz, ':');  { Kanal }
                if length (sbuf) > 1 then
                  sbuf:=F_TruncNumStr (sbuf);  { führende Nullen eliminieren }
                VglAdr:=sbuf + ':';
              end else  { Kanal nicht vorhanden }
                VglAdr:='';

              if length (rohsatz) > 0 then begin
                sbuf:=F_Zerlegen (rohsatz, '.');  { Meßgröße }
                if length (sbuf) > 1 then
                  sbuf:=F_TruncNumStr (sbuf);  { führende Nullen eliminieren }
                VglAdr:=VglAdr + sbuf;
              end;

              if length (rohsatz) > 0 then begin
                sbuf:=F_Zerlegen (rohsatz, '.');  { Meßart }
                if length (sbuf) > 1 then
                  sbuf:=F_TruncNumStr (sbuf);  { führende Nullen eliminieren }
                VglAdr:=VglAdr + '.' + sbuf;
              end;

              if length (rohsatz) > 0 then begin
                sbuf:=F_Zerlegen (rohsatz, '*');  { Tarifstufe }
                if length (sbuf) > 1 then
                  sbuf:=F_TruncNumStr (sbuf);  { führende Nullen eliminieren }
                VglAdr:=VglAdr + '.' + sbuf;
              end;

              if length (rohsatz) > 0 then begin
                sbuf:=rohsatz;  { Vorwert }
                if length (sbuf) > 1 then
                  sbuf:=F_TruncNumStr (sbuf);  { führende Nullen eliminieren }
                VglAdr:=VglAdr + '*' + sbuf;
              end;

              if length (sMedium) > 0 then
                Adresse:=sMedium + '-' + Adresse;  // 27.03.2013, WW
              AllgNum:=Adresse;

              { in Parameter-Konfigurationsliste allgemeine Parameternummer zur
                gerätespezifischen suchen: }
              if ParamMrgKonfigList <> nil then begin
                { 1. Suche nach Vergleichsadresse ohne Medium: }
                if ParamMrgKonfigList.FindParamMrgData (ParameterGruppe, VglAdr,
                                                        ParamMrgData) then
                  AllgNum:=ParamMrgData.Parameternummer
                else begin
                  { 2. Suche nach Vergleichsadresse mit Medium, falls relevant: }
                  gefunden:=false;
                  if length (sMedium) > 0 then begin
                    if ParamMrgKonfigList.FindParamMrgData (ParameterGruppe, sMedium + '-' + VglAdr,
                                                            ParamMrgData) then begin
                      AllgNum:=ParamMrgData.Parameternummer;
                      gefunden:=true;
                    end;
                  end;

                  if not gefunden then begin
                    { 3. Besonderheit Elster: manche Adressen werden mit Meßgrößen-Erweiterung '_1'
                      gelesen, Antwort kommt aber ohne Erweiterung. Daher nochmalige Suche nach
                      Vergleichsadresse plus Erweiterung }
                    if (ParameterGruppe <> 120) AND         { nicht bei Actaris Sparklog }
                       (ParameterGruppe <> 126) then begin  { nicht bei Kamstrup UNIGAS 300 }
                      if ParamMrgKonfigList.FindParamMrgData (ParameterGruppe, VglAdr + '_1',
                                                              ParamMrgData) then begin
                        AllgNum:=ParamMrgData.Parameternummer;
                        gefunden:=true;
                      end;
                    end;

                    { 4. Besonderheit Actaris Sparklog: manche Adressen werden mit Kanal 0:
                      gelesen, Antwort kommt aber mit Kanal größer 0. Daher nochmalige Suche nach
                      Vergleichsadresse mit Kanal 0 }
                    if (ParameterGruppe = 120) then begin  { nur bei Actaris Sparklog }
                      F_Zerlegen (VglAdr, ':');
                      VglAdr:='0:' + VglAdr;
                      if ParamMrgKonfigList.FindParamMrgData (ParameterGruppe, VglAdr,
                                                              ParamMrgData) then begin
                        AllgNum:=ParamMrgData.Parameternummer;
                        gefunden:=true;
                      end;
                    end;

                    if not gefunden AND (ParamEKKonfigList <> nil) then begin
                      { 5. Besonderheit Elster EK-Serie: Suche nach Vergleichsadresse
                        in EK-spezifischer Parameterliste: }
                      if ParamEKKonfigList.FindParaNrAllg (ParameterGruppe, VglAdr,
                                                           sbuf) then
                        AllgNum:=sbuf;
                    end;
                  end;
                end;
              end;  { if ParamMrgKonfigList <> nil }

              ParameterItem:=TParameterItem.Create (AllgNum, Wert, Adresse);
              Add (ParameterItem);
            end;  { if not isFehlertelegramm }
          end;  { if length (sbuf) > 0 }
        end;  { while FS.Position < FSize }
        Result:=true;
      finally
        FS.Free;
      end;
    finally
      // EK-Liste nur freigeben, wenn lokal angelegt
      if not Assigned (AParamEKKonfigList) then  // 09.07.2021, WW
        ParamEKKonfigList.Free;
    end;
  except
    Result:=false;
  end;
end;

{--------------------------------------------------------------------------------------------------}
function TParameterListe.KonvPara_Tritschler_IEC (Rohfilename: string;
                                                  ParameterGruppe: integer;
                                                  ParamMrgKonfigList: TParamMrgKonfigList): boolean;
{--------------------------------------------------------------------------------------------------}
{ Konvertieren von Parametern vom Typ "Tritschler, IEC-Protokoll" (ParaKonvGruppe 4,
  Tritschler VC2/TTG/TDS/MCO/VCC, Datacon FWU)
  -> Rohfileformat: IEC
  Ergebnis: true, wenn Konvertieren erfolgreich }
var
  ParameterItem: TParameterItem;
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  S: string;
  Kennziffer: string;
  KennzifferExt: string;
  Wert: string;
  AllgNum: string;
  STX_gelesen: boolean;
  i: integer;
  bKonvertieren: boolean;
  ParamMrgData: TParamMrgData;
  sWert_Monatsabschluss: string;
  iPos_Stern: integer;

begin
  Result:=false;
  if not FileExists (Rohfilename) then exit;
  try
    FS:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);   { Rohfile }
    try
      FSize:=FS.Size;
      STX_gelesen:=false;
      sWert_Monatsabschluss:='';
      zeichen:=NUL;
      { Konvertierung }
      while (zeichen <> ETX) AND (FS.Position < FSize) do begin
        Application.ProcessMessages;
        rohsatz:='';
        zeichen:=NUL;
        while (zeichen <> LF) AND (zeichen <> STX) AND (zeichen <> ETX) AND
              (FS.Position < FSize) do begin
          FS.Read (zeichen);
          rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
          if zeichen = STX then
            STX_gelesen:=true;
        end;
        if Pos (STX, rohsatz) > 0 then
          Continue;
        if Pos ('!', rohsatz) = 1 then
          Break;    { nach dem '!' folgen keine weiteren auszuwertenden Daten mehr }

        if not STX_gelesen then begin   { alles bis zum STX: Informationsantwort }
          { Die in der Informationsantwort enthaltenen Infos besitzen keine Kennziffer.
            Die zugeordneten Kennziffern werden daher im Quellcode vergeben: }
          if Copy (rohsatz, 1, 1) = '/' then begin
            S:=Copy (rohsatz, 2, 3);                        { Herstellerkennung }
            Kennziffer:='/1';
            { in Parameter-Konfigurationsliste allgemeine Parameternummer zur
              gerätespezifischen suchen: 30.04.2019, WW }
            AllgNum:=Kennziffer;
            if ParamMrgKonfigList <> nil then begin
              if ParamMrgKonfigList.FindParamMrgData (ParameterGruppe, Kennziffer,
                                                      ParamMrgData) then
                AllgNum:=ParamMrgData.Parameternummer;
            end;

            ParameterItem:= TParameterItem.Create (AllgNum, S, Kennziffer);
            Add (ParameterItem);

            S:=Copy (rohsatz, 5, 1);                        { Baudrate }
            Kennziffer:='/2';
            { in Parameter-Konfigurationsliste allgemeine Parameternummer zur
              gerätespezifischen suchen: 30.04.2019, WW }
            AllgNum:=Kennziffer;
            if ParamMrgKonfigList <> nil then begin
              if ParamMrgKonfigList.FindParamMrgData (ParameterGruppe, Kennziffer,
                                                      ParamMrgData) then
                AllgNum:=ParamMrgData.Parameternummer;
            end;

            ParameterItem:= TParameterItem.Create (AllgNum, S, Kennziffer);
            Add (ParameterItem);

            S:=Copy (rohsatz, 6, 3);                        { Gerätetyp }
            Kennziffer:='/3';
            { in Parameter-Konfigurationsliste allgemeine Parameternummer zur
              gerätespezifischen suchen: 30.04.2019, WW }
            AllgNum:=Kennziffer;
            if ParamMrgKonfigList <> nil then begin
              if ParamMrgKonfigList.FindParamMrgData (ParameterGruppe, Kennziffer,
                                                      ParamMrgData) then
                AllgNum:=ParamMrgData.Parameternummer;
            end;

            ParameterItem:= TParameterItem.Create (AllgNum, S, Kennziffer);
            Add (ParameterItem);

            { nicht für Tritschler VC2, TDS, MCO, VC3, VCC (haben Geräteversion
              schon unter Kennziffer abgelegt, ein weiterer Parameter "Geräteversion"
              daher nicht nötig): }
            if (Parametergruppe = 109) OR (Parametergruppe = 112) then begin
              S:=Copy (rohsatz, 9, length (rohsatz));         { Geräteversion }
              S:=ExtractString (S, NUL, CR, 0);
              if Parametergruppe = 109 then begin  { Datacon FWU }
                if length (S) > 0 then       { Roh-Versionsstring "formatieren" }
                  S:=S [1] + '.' + Copy (S, 2, length (S));
              end;
              Kennziffer:='/4';
              { in Parameter-Konfigurationsliste allgemeine Parameternummer zur
                gerätespezifischen suchen: 30.04.2019, WW }
              AllgNum:=Kennziffer;
              if ParamMrgKonfigList <> nil then begin
                if ParamMrgKonfigList.FindParamMrgData (ParameterGruppe, Kennziffer,
                                                        ParamMrgData) then
                  AllgNum:=ParamMrgData.Parameternummer;
              end;

              ParameterItem:= TParameterItem.Create (AllgNum, S, Kennziffer);
              Add (ParameterItem);
            end;
          end;
        end
        else begin                      { Datenphase: nach dem STX }
          { Tabellenzuordnung: gerätespezifische Parameternummer <-> allg. Parameternummer
            -> als "gerätespezifische Parameternummer" wird die Kennziffer verwendet (z.B. 7.10.1) }
          Kennziffer:=ExtractString (rohsatz, NUL, '(', 0);
          if Parametergruppe = 112 then begin
            { Besonderheit bei TTG: Kennziffer mit mehr als 3 Stellen (Stellen 4 und 5 enthalten
              Nr. des aktuell letzten Monatsabschlusses: }
            if length (Kennziffer) > 3 then    { Monatsabschluss-Stellen durch 'n' ersetzen }
              Kennziffer:=F_RightPad (Copy (Kennziffer, 1, 3), 'n', length (Kennziffer));
          end;

          { nicht bei Datacon FWU und Tritschler TTG (d.h. VC2, TDS, MCO, VC3, VCC); 30.04.2019, WW }
          if (Parametergruppe <> 109) AND (Parametergruppe <> 112) then begin
            { wegen TDS: 29.02.2008, WW
              -> Monatsabschlußwerte nicht konvertieren (enthalten dynamische
                 Monatsabschlußnummer, die nicht in gerätespez. Meldungsnummer
                 hinterlegt werden kann, z.B. *02)
                 05.12.2017, WW: Werte des LETZTEN Monatsabschluß werden konvertiert
              -> Kennziffer 99. (Telegramm-Ausgleichszeichen) nicht konvertieren,
                 ist kein Geräteparameter }
            iPos_Stern:=Pos ('*', Kennziffer);
            bKonvertieren:=(iPos_Stern = 0) AND
                           (Pos ('99.', Kennziffer) <> 1);

            // Prüfen, ob Monatsabschlußnummer in der Kennziffer enthalten ist (nach dem *):
            if (iPos_Stern > 0) then begin
              if (sWert_Monatsabschluss <> '') then begin
                S:=ExtractString(Kennziffer, '*', #0, 0);
                // Wenn ja, Werte des letzten Monatsabschluß konvertieren; 05.12.2017, WW
                bKonvertieren:=bKonvertieren OR (S = sWert_Monatsabschluss);
              end;

              // Für gerätespezifische Meldungsnummer die (dynamische) Monats-
              // abschlußnummer aus der Kennziffer entfernen
              System.Delete (Kennziffer, iPos_Stern + 1, length (Kennziffer));
            end;
          end else
            bKonvertieren:=true;

          if bKonvertieren then begin
            S:=ExtractString (rohsatz, '(', CR, 0);  // geändert 09.03.2004, WW:
            S:=ExtractString (S, NUL, ')', 0);       // für VC2 ab Vs. 6.x mit Archivdaten in den Rohdaten
            i:=0;
            while length (S) > 0 do begin
              Wert:=F_Zerlegen (S, ';');
              { wenn unter einer Kennziffer mehrere Werte zusammengefaßt sind: }
              if not ((length (S) = 0) AND (i = 0)) then begin
                inc (i);
                { "künstliche Kennziffer": Erweiterungsbuchstabe anhängen für Eindeutigkeit (a, b, c etc.) }
                KennzifferExt:=Kennziffer + Chr (i + 96);
              end else
                KennzifferExt:=Kennziffer;

              { nicht bei Datacon FWU und Tritschler TTG (d.h. VC2, TDS, MCO, VC3, VCC); 30.04.2019, WW }
              if (Parametergruppe <> 109) AND (Parametergruppe <> 112) then begin
                // Wert für Monatsabschluß merken; 05.12.2017, WW
                if (Pos ('7.1.', Kennziffer) = 1) AND (sWert_Monatsabschluss = '') then begin
                  sWert_Monatsabschluss:=Wert;
                end;
              end;

              { in Parameter-Konfigurationsliste allgemeine Parameternummer zur gerätespezifischen suchen: }
              AllgNum:=KennzifferExt;
              if ParamMrgKonfigList <> nil then begin
                if ParamMrgKonfigList.FindParamMrgData (ParameterGruppe, KennzifferExt,
                                                        ParamMrgData) then
                  AllgNum:=ParamMrgData.Parameternummer;
              end;

              ParameterItem:=TParameterItem.Create (AllgNum, Wert, KennzifferExt);
              Add (ParameterItem);
            end;
          end;  {if bKonvertieren }
        end;  { if not STX_gelesen }
      end;  { while FS.Position < FSize }
      Result:=true;
    finally
      FS.Free;
    end;
  except
    Result:=false;
  end;
end;

{-----------------------------------------------------------------------------------------}
function TParameterListe.KonvPara_DS100 (Rohfilename: string; ParameterGruppe: integer;
                                         ParamMrgKonfigList: TParamMrgKonfigList): boolean;
{-----------------------------------------------------------------------------------------}
{ Konvertieren von Parametern vom Typ "Elster DS-100" (ParaKonvGruppe 5)
  -> Rohfileformat: aneinander gereihte Rohantworten auf Parameter-Lesebefehle ?a, ?b, ?e etc.;
                    Bei 4-Kanal-Gerät sind die Rohantworten der einzelnen Kanäle
                    durch NUL-Zeichen voneinander getrennt.
  Beispiel: <Befehlszeichen><Parameter-Rohwert>%<Checksumme, 2 Zeichen><CR><LF>
            <Befehlszeichen><Parameter-Rohwert>%<Checksumme, 2 Zeichen><CR><LF>
            .
            .
  Ergebnis: true, wenn Konvertieren erfolgreich }
var
  ParameterItem: TParameterItem;
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  S: string;
  Wert: string;
  AllgNum: string;
  MrgNum: string;
  KanalNr: integer;
  Code: integer;
  Exponent: integer;
  cp: single;
  ParamMrgData: TParamMrgData;
  sKanalNr: string;

begin
  Result:=false;
  KanalNr:=1;  { Vorbelegung: die ersten Rohdaten stammen von Kanal 1 }

  { Wenn dem Rohfilenamen durch Strichpunkt getrennt eine Kanalnummer vorangestellt
    ist: Kanalnummer, zu der die Parameter gehören; 24.08.2011, WW } 
  if Pos (';', Rohfilename) > 0 then begin
    sKanalNr:=F_Zerlegen (Rohfilename, ';');
    KanalNr:=StrToInt (sKanalNr);
  end;

  if not FileExists (Rohfilename) then exit;
  try
    FS:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);   { Rohfile }
    try
      FSize:=FS.Size;
      { Konvertierung }
      while FS.Position < FSize do begin
        Application.ProcessMessages;
        rohsatz:='';
        zeichen:=NUL;
        while (zeichen <> LF) AND (FS.Position < FSize) do begin
          FS.Read (zeichen);
          if zeichen = NUL then begin   { Trennzeichen NUL: Beginn der Rohdaten des nächsten Kanals }
            inc (KanalNr);
            rohsatz:='';
          end else
            rohsatz:=rohsatz + zeichen;                         { Zeile bilden }
        end;

        S:=ExtractString (rohsatz, NUL, '%', 0);
        if length (S) > 0 then begin  { es muß mindestens das Befehlszeichen enthalten sein }
          MrgNum:=S [1] + IntToStr (KanalNr);  { "künstliche" MRG-Parameternummer: Befehlszeichen plus Kanalnummer }
          Wert:=Copy (S, 2, length (S));

          if MrgNum [1] = 'n' then begin  { Rohwert für Parameter "cp-Wert" (Befehl n) in Anzeigewert umrechnen }
            Val (Wert, Exponent, Code);
            if Exponent <> 99 then
              cp:=exp(Exponent*ln(10))/100            { cp =  (10 hoch x) / 100 }
            else
              cp:=1;
            Wert:=FloatToStr (cp);
            StrSubst(Wert, '.', ',');  { Ausgabe fest mit Dezimal-Komma }
          end;

          { in Parameter-Konfigurationsliste allgemeine Parameternummer zur gerätespezifischen suchen: }
          AllgNum:=MrgNum;
          if ParamMrgKonfigList <> nil then begin
            if ParamMrgKonfigList.FindParamMrgData (Parametergruppe, MrgNum,
                                                    ParamMrgData) then
              AllgNum:=ParamMrgData.Parameternummer;
          end;

          ParameterItem:=TParameterItem.Create (AllgNum, Wert, MrgNum);
          Add (ParameterItem);
        end;  { if length (S) }
      end;  { while FS.Position < FSize }
      Result:=true;
    finally
      FS.Free;
    end;
  except
    Result:=false;
  end;
end;

{--------------------------------------------------------------------------------------------------}
function TParameterListe.KonvPara_Tritschler_FTL (Rohfilename: string;
                                                  ParameterGruppe: integer;
                                                  ParamMrgKonfigList: TParamMrgKonfigList): boolean;
{--------------------------------------------------------------------------------------------------}
{ Konvertieren von Parametern vom Typ "Tritschler, FTL-Protokoll" (ParaKonvGruppe 7,
  Tritschler TTG)
  -> Rohfileformat: FTL
  -> konvertiert Antworten auf Befehle: F0000 (Geräteinformation, Kanal 1;
                                               enthält aber kanalunabhängige Parameter)
                                        F0001 (Datentelegramm 1/Kanal 1)
                                        F0011 (Datentelegramm 1/Kanal 2)
  -> von der Konvertierung erwarteter Aufbau der Rohdaten:
     zusammengefasste Teil-Antworten ohne K23-Blöcke (Blocksumme) und CR
  Ergebnis: true, wenn Konvertieren erfolgreich }
var
  ParameterItem: TParameterItem;
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  rohsatz: string;
  zeichen: char;
  Wert: string;
  AllgNum: string;
  sKennziffer: string;
  KanalNr: integer;
  KanalNrStr: string;
  Code: integer;
  bKonv: boolean;
  ParamMrgData: TParamMrgData;

begin
  Result:=false;
  if not FileExists (Rohfilename) then exit;
  try
    FS:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);   { Rohfile }
    try
      KanalNr:=-1;      { Vorbelegung: Kanalnummer nicht gelesen }
      FSize:=FS.Size;
      while FS.Position < FSize do begin
        rohsatz:='K';
        zeichen:=NUL;
        { Datensätze bilden: bis K lesen }
        while (zeichen <> 'K') AND (FS.Position < FSize) do begin
          FS.Read (zeichen);
          if zeichen <> 'K' then
            rohsatz:=rohsatz + zeichen;   { Datensatz bilden }
        end;

        Application.ProcessMessages;

        sKennziffer:=Copy (rohsatz, 1, 3);  { Knn }
        if sKennziffer = 'K85' then begin  { Funktionsnummer }
          KanalNrStr:=Copy (rohsatz, 7, 1);  { Kanal steht an 3. Stelle der Funktionsnummer }
          Val (KanalNrStr, KanalNr, Code);
          if Code <> 0 then exit;
          KanalNr:=KanalNr + 1;  { 0 = Kanal 1; 1 = Kanal 2 }
        end
        else begin
          bKonv:=false;
          { Kanalunabhängige Parameter: }
          if (sKennziffer = 'K80') OR    { Gerätekennung und Softwareversion }
             (sKennziffer = 'K81') OR    { Übertragungsprotokoll }
             (sKennziffer = 'K01') OR    { Kundennummer high }
             (sKennziffer = 'K41') OR    { Kundennummer low }
             (sKennziffer = 'K18') OR    { Hintergrundspeicher - Organisation }
             (sKennziffer = 'K19') OR    { Funktions-Schlüssel-Code-Wort }
             (sKennziffer = 'K02') OR    { aktuelles Datum }
             (sKennziffer = 'K42') OR    { aktuelle Zeit }
             (sKennziffer = 'K11') OR    { Messperiode für Maximummessung }
             (sKennziffer = 'K09') OR    { Batteriezustand }
             (sKennziffer = 'K12') OR    { Datum letzter Servicevorgang }
             (sKennziffer = 'K40') then  { Datum/Zeit letzter Monatsabschluß }
            bKonv:=true

          { Kanalabhängige Parameter: }
          else if (sKennziffer = 'K10') OR          { Skalierung }
                  (sKennziffer = 'K03') OR          { Gesamtzähler }
                  (sKennziffer = 'K48') then begin  { aktuelle Monatsmenge }
            if KanalNr < 0 then exit;  { Kanalnummer wurde nicht gelesen }
            { "künstliche " kanalspezifische gerätespezifische Parameternummer: }
            sKennziffer:=sKennziffer + '_' + IntToStr (KanalNr);  { Knn_k }
            bKonv:=true;
          end;

          if bKonv then begin
            { in Parameter-Konfigurationsliste allgemeine Parameternummer zur gerätespezifischen suchen: }
            AllgNum:=sKennziffer;
            if ParamMrgKonfigList <> nil then begin
              if ParamMrgKonfigList.FindParamMrgData (Parametergruppe, sKennziffer,
                                                      ParamMrgData) then
                AllgNum:=ParamMrgData.Parameternummer;
            end;

            Wert:=Copy (rohsatz, 4, length (rohsatz));  { Parameterwert }
            Wert:=F_LeftTrunc (Wert, ' ');  { führende Spaces wegschneiden }
            ParameterItem:= TParameterItem.Create (AllgNum, Wert, sKennziffer);
            Add (ParameterItem);
          end;
        end;
      end;   { while FS.Position < FSize }
      Result:=true;
    finally
      FS.Free;
    end;
  except
    Result:=false;
  end;
end;

{-----------------------------------------------------------------------------------------}
function TParameterListe.KonvPara_Corus (Rohfilename: string;
                                         ParameterGruppe: integer;
                                         ParamMrgKonfigList: TParamMrgKonfigList): boolean;
{-----------------------------------------------------------------------------------------}
{ Konvertieren von Parametern vom Typ "Actaris Corus" (ParaKonvGruppe 8);
  -> Rohfileformat: aneinandergehängte Antworten auf Parameter-Lesebefehle mit
                    jeweils vorangestellten zugehörigen Parameternummern durch
                    Semikolon getrennt (Parameternummern untereinander durch
                    Komma getrennt)
  Ergebnis: true, wenn Konvertieren erfolgreich }
type
  { Zustände für Rohdatenkonvertierung }
  TModus = (m_Info, m_Start, m_Size, m_Data);

var
  FS: TFileOfCharStream;  // 06.10.2015, WW
  FSize: integer;
  zeichen: char;
  rohsatz: string;
  Modus: TModus;
  sParaNr: string;
  iSize: byte;
  S: string;
  ParameterItem: TParameterItem;
  AllgNum: string;
  Wert: string;
  iParaDatentyp: integer;
  iParaAnzahl: integer;
  iParaByteLen: integer;
  ParamMrgData: TParamMrgData;
  slParaNr: TStringList;
  i, j: integer;
  sData: string;
  iPos: integer;
  bDataLenFehler: boolean;

begin
  Result:=true;
  if not FileExists (Rohfilename) then begin
    Result:=false;
    exit;
  end;
  try
    FS:=TFileOfCharStream.Create (Rohfilename, fmOpenRead OR fmShareDenyWrite);   { Rohfile }
    try
      slParaNr:=TStringList.Create;
      try
        rohsatz := '';
        iSize:=0;
        Modus:=m_Info;  { als erstes wird die vorangestellte Zusatzinfo des 1. Telegramms gelesen }

        FSize:=FS.Size;
        while FS.Position < FSize do begin
          Application.ProcessMessages;
          FS.Read (zeichen);
          rohsatz := rohsatz + zeichen;

          case Modus of
            m_Info:
              begin
                { Trennzeichen ';' zwischen vorangestellter Zusatzinfo (Parameternummern)
                  und den eigentlichen Rohdaten (Rohdaten enthalten NICHT die
                  zugehörigen Parameternummern !); Trennzeichen ',' zwischen den
                  Parameternummern: }
                if zeichen in ['0'..'9', ';', ','] then begin
                  if zeichen = ';' then begin
                    S:=Copy (rohsatz, 1, length (rohsatz)-1);  { ohne Trennzeichen }
                    slParaNr.CommaText:=S;   // Parameternummern in Stringliste laden
                    Modus:=m_Start;
                    rohsatz:='';
                  end;
                end
                else begin
                  Result:=false;  { Fehler: Zusatzinfo fehlt }
                  exit;
                end;
              end;

            m_Start:
              begin
                if length (rohsatz) = 1 then begin
                  if rohsatz [1] = SOH then  { Startzeichen SOH gelesen }
                    Modus:=m_Size  { als Nächstes bis zum Längenbyte lesen }
                  else  { NAK gelesen, es folgen keine Rohdaten }
                    Modus:=m_Info;
                end;
                rohsatz:='';
              end;

            m_Size:
              begin
                if length (rohsatz) = 1 then begin  { Längenbyte gelesen }
                  iSize:=Ord (rohsatz [1]);
                  Modus:=m_Data;  { als Nächstes der Datenteil des Telegramms }
                  rohsatz:='';
                end;
              end;

            m_Data:
              begin
                if length (rohsatz) = (iSize + 3) then begin  { Telegramm-Daten, Endezeichen und 2 CRC-Zeichen gelesen }
                  sData:=Copy (rohsatz, 1, length (rohsatz)-3);  { Parameter-Rohdaten: ohne Endezeichen und 2 CRC-Zeichen }

                  bDataLenFehler:=false;
                  // 2 Durchläufe der Parameternummern-Liste:
                  //  -> 1. Durchlauf: Prüfung der Datenteillänge (Vergleich mit Längenbyte)
                  //  -> 2. Durchlauf: Konvertierung des Datenteils
                  for j:=1 to 2 do begin
                    iPos:=1;  // Vorbelegung: 1. Zeichen in Parameter-Rohdaten
                    for i:=0 to slParaNr.Count - 1 do begin
                      Application.ProcessMessages;
                      sParaNr:=slParaNr [i];
                      { in Parameter-Konfigurationsliste allgemeine Parameternummer und
                        Parameter-Datentyp zur gerätespezifischen Parameternummer suchen: }
                      AllgNum:=sParaNr;
                      iParaDatentyp:=-1;  // kein Parameter-Datentyp definiert
                      iParaAnzahl:=-1;
                      iParaByteLen:=-1;
                      if ParamMrgKonfigList <> nil then begin
                        if ParamMrgKonfigList.FindParamMrgData (Parametergruppe, sParaNr,
                                                                ParamMrgData) then begin
                          AllgNum:=ParamMrgData.Parameternummer;
                          iParaDatentyp:=StrToIntDef (ParamMrgData.ParaDatentyp, -1);  // 08.03.2019, WW
                          iParaAnzahl:=ParamMrgData.ParaAnzahl;
                          iParaByteLen:=ParamMrgData.ParaByteLen;
                        end;
                      end;
                      // Wenn für den Parameter keine spezifische Bytelänge konfiguriert
                      // ist: Standard-Bytelänge des Parameter-Datentyps verwenden
                      if iParaByteLen <= -1 then
                        iParaByteLen:=PDT_ByteLength (iParaDatentyp);  // 26.09.2012, WW

                      if (iParaByteLen > -1) AND (iParaAnzahl > -1) then begin
                        // Parameter-Bytelänge und -Anzahl bekannt
                        if j = 1 then begin  // Datenprüfung
                          inc (iPos, iParaByteLen * iParaAnzahl);
                          // Letzter Parameter: Vergleich der Datenteillänge mit Längenbyte
                          if i = (slParaNr.Count - 1) then begin
                            if iPos <> (length (sData) + 1) then begin
                              bDataLenFehler:=true;
                              Break;
                            end;
                          end;
                        end
                        else if j = 2 then begin  // Datenkonvertierung
                          S:=Copy (sData, iPos, iParaByteLen * iParaAnzahl);  { einzelnen Parameter-Rohwert ausschneiden; 26.09.2012, WW }
                          { Binären Parameter-Rohwert zu Anzeige-String formatieren: }
                          Wert:=FormatPara_Corus_Anzeige (S, iParaDatentyp, iParaAnzahl);
                          ParameterItem:= TParameterItem.Create (AllgNum, Wert, sParaNr);
                          Add (ParameterItem);

                          inc (iPos, iParaByteLen * iParaAnzahl);
                        end;
                      end
                      else begin
                        // Parameter-Bytelänge und -Anzahl unbekannt: Datenprüfung
                        // bzw. -Konvertierung der Parameter-Rohdaten abbrechen,
                        // weiter mit nächstem Telegramm
                        if j = 1 then  // Datenprüfung
                          bDataLenFehler:=true;  // Datenlänge-Fehler-Flag setzen
                        Break;
                      end;
                    end;  { for i }

                    // wenn Datenlänge-Fehler-Flag gesetzt ist, keine Konvertierung:
                    if bDataLenFehler then
                      Break;
                  end;  { for j }

                  Modus:=m_Info;    { als Nächstes: ZusatzInfo von nächstem Telegramm lesen }
                  rohsatz:='';
                end;
              end;
          end;  { case }
        end;  { while FS.Position < FSize }

        if length (rohsatz) > 0 then begin
          Result:=false;        { Fehler Rohdatenlänge }
          Clear;             { Liste löschen, Rohdaten enthalten evtl. Müll }
        end;
      finally
        slParaNr.Free;
      end;
    finally
      FS.Free;
    end;
  except
    Result:=false;
  end;
end;

{------------------------------------------------------------------------------------------}
function TParameterListe.KonvPara_Modbus (RegisterKonvListe: TRegisterKonvList;
                                          ParameterGruppe: integer;
                                          ParamMrgKonfigList: TParamMrgKonfigList): boolean;
{------------------------------------------------------------------------------------------}
{ Konvertieren von per Modbus abgefragten Parametern (ParaKonvGruppe 9);
  -> Die Werte der abgefragten Modbus-Register sind in der Register-KonvListe
     enthalten.
  Ergebnis: true, wenn Konvertieren erfolgreich }
var
  sParaNr: string;
  ParameterItem: TParameterItem;
  AllgNum: string;
  Wert: string;
  ParamMrgData: TParamMrgData;
  i: integer;
  RegisterKonvData: TRegisterKonvData;

begin
  Result:=true;
  if not Assigned (RegisterKonvListe) then begin
    Result:=false;
    exit;
  end;

  try
    for i:=0 to RegisterKonvListe.Count - 1 do begin
      Application.ProcessMessages;
      RegisterKonvData:=TRegisterKonvDataObj (RegisterKonvListe [i]).Data;

      sParaNr:=IntToStr (RegisterKonvData.StartAdresse);  // Default: MRG-Parameternummer ist Registeradresse
      Wert:=RegisterKonvData.Wert;
      { in Parameter-Konfigurationsliste allgemeine und gerätespezifische
        Parameternummer zur Parameter-Registeradresse suchen: }
      AllgNum:=sParaNr;  // Default: Allg. Parameternummer ist Registeradresse
      if ParamMrgKonfigList <> nil then begin
        // Suchen nach MRG-Parameternummer auch in Feld 'Parameteradresse_im_MRG'; 18.02.2021, WW
        if ParamMrgKonfigList.FindParamMrgData (Parametergruppe, sParaNr,
                                                ParamMrgData, true) then begin
          sParaNr:=ParamMrgData.Parameternummer_im_MRG;  // 18.02.2021, WW
          AllgNum:=ParamMrgData.Parameternummer;

          if ParamMrgData.AusgabeFormat <> '' then
            // Wert-String formatieren; 10.10.2019, WW
            WFormatString (Wert, ParamMrgData.AusgabeFormat)
          else if (AllgNum = CP_RMG_PrimusPrilog_FWVersion_App) OR
                  (AllgNum = CP_RMG_PrimusPrilog_FWVersion_Eich) then begin
            // Primus/Prilog 400: FW-Versionen mit Punkt versehen (z.B. 112 -> 1.12)
            System.Insert('.', Wert, length(Wert) - 1);
          end
          else if (AllgNum = CP_SICK_FLOWSIC500_VersionKommTreiber) OR
                  (AllgNum = CP_SICK_FLOWSIC500_VersionFW) OR
                  (AllgNum = CP_SICK_FLOWSIC500_VersionDSP) then begin
            // FLOWSIC500: Versionen mit Punkt versehen (z.B. 21601 -> 2.16.01); 29.09.2021, WW
            System.Insert('.', Wert, length(Wert) - 1);
            System.Insert('.', Wert, length(Wert) - 4);
          end;
        end;
      end;

      ParameterItem:= TParameterItem.Create (AllgNum, Wert, sParaNr);
      Add (ParameterItem);
    end;  // for i
  except
    Result:=false;
  end;
end;


{----------------------------- Wieser -----------------------------------------}

{---------------------------------------------------------------------------------------------}
Function TParameterListe.KonvEinzelparameter_Wieser (HeapStr: string; ParameterGruppe: integer;
                                                     ParamMrgKonfigList: TParamMrgKonfigList;
                                                     MrgNumLen: integer): Boolean;
{---------------------------------------------------------------------------------------------}
{ Parameterliste mit Inhalt von HeapStr (Rohdaten eines einzelnen Parameters
  vom Typ "Wieser" (ParaKonvGruppe 1)) füllen/updaten und bei Bedarf sortieren;
  Übergabe: Einzelparameter-Rohstring
            Parameter-Gruppe
            Liste mit Parameternummern-Konfiguration
  Ergebnis: true, wenn Rohstring erfolgreich in Liste konvertiert werden konnte }
var
  S: string;
  AllgNum: string;
  Wert: string;
  MRGNum: string;

Begin
  Result:=false;
  S:=ExtractString (HeapStr, STX, ETX, 0);  // nur der Teil zwischen STX und ETX interessiert
  if (length (S) > 0) then begin
    if S [1] = 'B' then begin
      S:=Copy (S, 2, length (S));  // B wegschneiden
      if length (S) > 0 then
        if S [length (S)] = US then
          S:=Copy (S, 1, length (S)-1);  // abschließenden US wegschneiden (für MRG 900er)

      if SplitRohString_Wieser (S, ParameterGruppe, ParamMrgKonfigList, MrgNumLen,
                                AllgNum, Wert, MrgNum) then begin
        if not Update (AllgNum, Wert, MrgNum) then
          Sort (ParaNrAllgCompare); { Liste nach allg. Parameternummer sortieren,
                                      wenn Eintrag neu eingefügt wurde }
        Result:=true;
      end;
    end;
  end;
End;

{------------------------------------------------------------------------------------------}
Function TParameterListe.SplitRohString_Wieser (RohString: string; Parametergruppe: Integer;
                                                ParamMrgKonfigList: TParamMrgKonfigList;
                                                MrgNumLen: integer;
                                                var AllgNum: string;
                                                var Wert: string;
                                                var MRGNum: string): boolean;
{------------------------------------------------------------------------------------------}
{ Rohstring eines Parameters vom Typ "Wieser" (ParaKonvGruppe 1) aufsplitten und
  für TParameterItem benötigte Daten liefern;
  Übergabe: Parameter-Rohstring
            Parametergruppe
            Liste mit Parameternummern-Konfiguration
            Länge der gerätespezifischen Parameternummer
  Rückgabe: allg. Parameternummer, Parameterwert, gerätespezifische Parameternummer }
var
  ParamMrgData: TParamMrgData;

Begin
  Result := false;
  AllgNum := '';
  Wert := '';
  MRGNum := '';
  // Länge der MRG-Parameternummer wird übergeben (für EC 694), 27.10.2004 WW
  If Length (RohString) >= MrgNumLen Then Begin  // 02.07.2002 WW: Bedingung >= statt >, da leerer Wert möglich
    MRGNum:=Copy(RohString, 1, MrgNumLen);
    Wert:=Copy(RohString, MrgNumLen + 1, length(RohString));
    AllgNum:=MRGNum;
    { in Parameter-Konfigurationsliste allgemeine Parameternummer zur gerätespezifischen suchen: }
    if ParamMrgKonfigList <> nil then begin
      if ParamMrgKonfigList.FindParamMrgData (Parametergruppe, MRGNum, ParamMrgData) then
        AllgNum:=ParamMrgData.Parameternummer;
    end;
    Result:=true;
  End;
End;


{------------------------------ SICK ------------------------------------------}
(*!! war nur ein Versuch...
     Anm. 09.01.2024, WW: Warum es beim Versuch blieb, liegt evtl. am Aufwand
     aufgrund der Vielzahl an betroffenen Parametern und des zusätzlich
     erforderlichen Rückrechnens beim Parametrieren des Rohwerts. Das für das
     Verrechnen erforderliche Vorhandensein der Faktoren in der Parameterliste
     würde durch den übergeordneten Abruf aller Parameter wohl ausreichend
     gewährleistet.

{------------------------------------------------------------------------}
procedure TParameterListe.GetZaehlerFaktoren_SICK (var dFaktor_Vb: double;
  var dFaktor_Vn: double);
{------------------------------------------------------------------------}
{ FLOWSIC500-Verrechnungsfaktoren für Vb- und Vn-Zähler aus Parameterliste lesen;
  Rückgaben: Verrechnungsfaktoren Vb, Vn }
var
  ParameterItem: TParameterItem;
  i: integer;
  iWert: integer;

begin
  // Vorbelegungen:
  dFaktor_Vb:=1;
  dFaktor_Vn:=1;

  // Parameter für Verrechnungsfaktoren Vb und Vn in Liste suchen:
  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParameterItem:=TParameterItem (Items [i]);

    if (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_FaktorVb) then begin
      // Auflösung Vb-Zählwerke (Faktor); 29.09.2021, WW
      iWert:=StrToIntDef (ParameterItem.Wert, 0);
      dFaktor_Vb:=exp(iWert * ln(10));  // 10 hoch Exponent
    end
    else if (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_FaktorVn) then begin
      // Auflösung Vn-Zählwerke (Faktor); 29.09.2021, WW
      iWert:=StrToIntDef (ParameterItem.Wert, 0);
      dFaktor_Vn:=exp(iWert * ln(10));  // 10 hoch Exponent
    end;
  end;  // for i
end;

{---------------------------------------------}
procedure TParameterListe.CalcParaAnzeige_SICK;
{---------------------------------------------}
{ FLOWSIC500-Rohwerte zu Anzeigewerte verrechnen }
var
  ParameterItem: TParameterItem;
  i: integer;
  dFaktor_Vb: double;
  dFaktor_Vn: double;
  dValue: double;

begin
  // Verrechnungsfaktoren für Vb- und Vn-Zähler aus Parameterliste lesen:
  GetZaehlerFaktoren_SICK (dFaktor_Vb, dFaktor_Vn);

  // Vb-, Vn-Parameter mit Faktoren verrechnen und in Liste aktualisieren:
  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParameterItem:=TParameterItem (Items [i]);

    if (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_CustomVm) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_CustomVmErr) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_Vm) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_VmErr) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBMPA_DELTA) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBTGA_DELTA) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBMOA_DELTA) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBMPA_MAX) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBTGA_MAX) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBMP_DELTA) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBTG_DELTA) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBMO_DELTA) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBMP_MAX) OR
       (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VBTG_MAX) then begin
      dValue:=StrToFloatDef (ParameterItem.Wert, 0) * dFaktor_Vb;
      ParameterItem.Wert:=FloatToStr(dValue);
    end
    else if (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_CustomVb) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_CustomVbErr) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_CustomVbTot) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_Vb) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_VbErr) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_VbTot) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNMPA_DELTA) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNTGA_DELTA) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNMOA_DELTA) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNMPA_MAX) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNTGA_MAX) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNMP_DELTA) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNTG_DELTA) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNMO_DELTA) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNMP_MAX) OR
            (ParameterItem.AllgNum = CP_SICK_FLOWSIC500_MAXLOAD_VNTG_MAX) then begin
      dValue:=StrToFloatDef (ParameterItem.Wert, 0) * dFaktor_Vn;
      ParameterItem.Wert:=FloatToStr(dValue);
    end;
  end;  // for i
end;
*)


{------------------------------ RMG RSM200 ------------------------------------}

{---------------------------------------------------------------------}
procedure TParameterListe.GetZaehlerFaktor_RSM200 (var dFaktor: double;
  var sFormat: string);
{---------------------------------------------------------------------}
{ RSM200-Verrechnungsfaktor und Formatangabe für die Vb/Vn-Zähler aus Parameter-
  liste lesen;
  Rückgaben: Verrechnungsfaktor
             Format-String (Delphi) }
var
  ParameterItem: TParameterItem;
  i: integer;
  iExponent: integer;

begin
  // Vorbelegungen:
  dFaktor:=1;
  sFormat:='';

  // Parameter für Verrechnungsfaktor in Liste suchen:
  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParameterItem:=TParameterItem (Items [i]);

    if (ParameterItem.AllgNum = CP_RMG_RSM200_ZaehlerAufloesExp) then begin
      // Auflösung Exponent (Faktor)
      iExponent:=StrToIntDef (ParameterItem.Wert, 0);
      dFaktor:=exp(iExponent * ln(10));  // 10 hoch Exponent
      // Formatangabe für die zu verrechnenden Werte (werden ggf. zu Floats):
      // -> Im Gerät minimaler Exponent = -3 (wir lassen Formatierung bis -10 zu)
      if (iExponent < 0) AND (iExponent > -10) then
        sFormat:='%.' + IntToStr (Abs(iExponent)) + 'f';
      Break;
    end;
  end;  // for i
end;

{-----------------------------------------------}
procedure TParameterListe.CalcParaAnzeige_RSM200;
{-----------------------------------------------}
{ RSM200-Rohwerte zu Anzeigewerte verrechnen }
var
  ParameterItem: TParameterItem;
  i: integer;
  dFaktor: double;
  dValue: double;
  sWert: string;
  sFormat: string;

begin
  // Verrechnungsfaktor und Formatangabe für die Vb/Vn-Zähler aus Parameterliste
  // lesen:
  GetZaehlerFaktor_RSM200 (dFaktor, sFormat);

  // Vb/Vn-Parameter mit Faktoren verrechnen und in Liste aktualisieren:
  for i:=0 to Count - 1 do begin
    Application.ProcessMessages;
    ParameterItem:=TParameterItem (Items [i]);

    if IsRSM200_VolumeParaNrAllg (ParameterItem.AllgNum) then begin
      dValue:=StrToFloatDef (ParameterItem.Wert, 0) * dFaktor;
      sWert:=FloatToStr(dValue);
      if sFormat <> '' then
        WFormatString (sWert, sFormat);  // Verrechneten Wert-String formatieren
      ParameterItem.Wert:=sWert;
    end;
  end;  // for i
end;

// Anm. WW, 09.01.2024: Der Verrechnungsfaktor muß grundsätzlich auch beim
// Parametrieren berücksichtigt werden (Rückrechnen des Rohwerts). Dies kann
// beim RSM200 allerdings entfallen, da die betroffenen Parameter Nur-Lesend sind.


{------------------------------ Elster DS-100 ---------------------------------}

{---------------------------------------------------------}
function KonvDS100Param (RohAntwort: string; cBefehl: char;
  var ParamWert: string): boolean;
{---------------------------------------------------------}
{ Konvertiert Rohantwort auf Elster DS-100-Parameter-Abfrage in Rohwert des Parameters;
  Übergaben:	Rohantwort
              Befehlszeichen, über das die Rohantwort erhalten wurde
  Rückgabe:	Parameterwert
  Ergebnis: true, wenn Konvertierung erfolgreich }
var
  S: string;

begin
  Result:=false;
	ParamWert:='';  // Vorbelegung Rückgabe
	if length (RohAntwort) > 0 then begin // prüfen auf ungültige Antwort
		if RohAntwort [1] = cBefehl then begin
      S:=ExtractString (RohAntwort, NUL, '%', 0);  // alles ab einschließlich % wegschneiden
      Paramwert:=Copy (S, 2, length (S));  // erstes Zeichen wegschneiden (Befehlszeichen)
			Result:=true;
		end;
	end;
end;


{----------------------------- Excel ------------------------------------------}

{-----------------------------------------------------------}
Procedure TParameterListe.SaveToExcel (ExcelCaption: string);
{-----------------------------------------------------------}
{ Parameter aus ParameterListe in Excel konvertieren;
  -> ohne Zugriff auf WICOM-Tabellen
  Übergabe: Titel für Excel-Tabelle }
var
  ParaTextKonfigList: TParaTextKonfigList;

begin
  ParaTextKonfigList:=TParaTextKonfigList.Create;
  try
    { Parametertext-Konfigurationsliste aus Resourcendatei laden: }
    GetParaText_KonfigList (ParaTextKonfigList, KonfigPfad);
    { Parameterlisten-Einträge mit Parametertexten in Excel-Blatt schreiben: }
    WriteExcelSheet (ExcelCaption, ParaTextKonfigList);
  finally
    ParaTextKonfigList.Free;
  end;
end;

{----------------------------------------------------------------------------------}
Procedure TParameterListe.WriteExcelSheet (ExcelCaption: string;
                                           ParaTextKonfigList: TParaTextKonfigList);
{----------------------------------------------------------------------------------}
{ Inhalt der ParameterListe mit Parametertexten in Excel-Blatt schreiben;
  Übergabe: Titel für Excel-Tabelle
            Liste mit Parametertexten }
const
  C_Separator = #9;
var
  SL: TStringList;
  i: integer;
  S: string;
  MrgNum: string;
  ParaNr_Allg: string;
  PText: string;

begin
  SL:=TStringList.Create;
  try
    SL.Duplicates:=dupAccept;
    SL.Sorted:=false;
    SL.Add (ExcelCaption);
    { Spaltenüberschriften: }
    SL.Add (S_Nummer + C_Separator + S_Wert + C_Separator + S_Name);
    SL.Add ('');

    { alle Listeneinträge in Stringliste schreiben: }
    Sort (ParaNrMRGCompare);       { Liste nach MRG-Parameternummer sortieren }
    for i:=0 to Count-1 do begin
      Application.ProcessMessages;
      MrgNum:=TParameterItem (Items [i]).MrgNum;
      ParaNr_Allg:=TParameterItem (Items [i]).AllgNum;
      PText:='';
      { in Parametertext-Liste den Parametertext zur allgemeinen Parameternummer suchen: }
      if ParaTextKonfigList <> nil then begin
        if ParaTextKonfigList.FindParaText (ParaNr_Allg, S) then
          PText:=S;
      end;
      S:=MrgNum + C_Separator + TParameterItem (Items [i]).Wert + C_Separator + PText;
      SL.Add (S);
    end;
    { Stringliste in Excel ausgeben: }
    InsertToExcel (SL, C_Separator, 1, 1, false, true, true);
  finally
    SL.Free;
  end;
end;

End.

