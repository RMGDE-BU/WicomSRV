{------------------------------------------------------------------------------}
{ Object zum Decodieren von DSfG-Daten in XML-Strings                          }
{                                                                              }
{ 03.11.2003  GD  Neu                                                          }
{ 20.12.2004  GD  Parameteränderung aufgenommen                                }
{ 05.02.2005  GD  Parameteränderung für DSfG-DFÜ-Instanzen                     }
{ 14.02.2005  GD  Erweiterungen für Rufentgegennahme                           }
{ 01.08.2006  GD  Erweiterungen für SMS-XML-Strukturen                         }
{ 24.10.2006  GD  Handling der Zeitumstellung                                  }
{ 03.04.2007  GD  Übergabe von Rohdatendateinamen                              }
{ 05.10.2009  WW  DSfG-DFÜ-Konfigurationsdaten mit Fabriknummer und Baujahr    }
{ 24.02.2011  GD  XML-Ersatzzeichen in Parametertexten					               }
{ 07.03.2012  WW  TDSfGXmlDecodeObject.ConvertXmlToFwUpdateInfoData            }
{ 23.09.2015  WW  Beschleunigte Dateizugriffe mit TFileOfCharStream            }
{ 28.12.2015  WW  Erweiterte TKonvListObj-Felder (Journal-Zeitbereich) zuweisen}
{                 in ConvertXMLFilesToTelegramFiles                            }
{ 12.07.2016  WW  Erweiterte TKonvListObj-Felder (KanalKonvList und KanalJrnZB-}
{                 List) zuweisen in ConvertXMLFilesToTelegramFiles; Anpassung  }
{                 für zeilenweise gelesene Archivgruppen                       }
{ 31.01.2017  WW  ExtractTDSfGDfueKonfigData um Extensionmode erweitert        }
{ 18.06.2020  WW  XML-Ersatzzeichen decodieren in Parametrier-Response         }
{                                                                              }
{ Copyright (C) Karl Wieser GmbH 2003, RMG Messtechnik GmbH 2020               }
{------------------------------------------------------------------------------}
unit DSfGXmlDecodeObj;

interface

uses
  Windows, SysUtils, Classes, StrUtils,
  DecodeResp, GD_Utils, T_Zeit, T_Tools, DListen, WXmlConst, WSysCon, WStream,
  UnixDT, DSG_Utils, AbrufCmd;

const
  C_DfuGroup_SingleMember = 'singlemember';  // Definierter Einzelwert, z.B. PW
  C_DfuGroup_Parameter    = 'parameter';     // Parameter mit Parameternummer
  C_DfuGroup_NtyMask      = 'ntymask';       // NTY-Einstellung für eine DNO

type
  TDfuParamList = class(TStringList)
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FMemberList   : TStrings;
    FParaListNr   : TStrings;
    FParaListWert : TStrings;
    FNtyListDno   : TStrings;
    FNtyListWert  : TStrings;
    FParamCount   : integer;
    FNtyCount     : integer;
  protected
  public
    procedure Clear; override;
    procedure Delete(iIndex: integer); override;
    function Add(const sMemberIdent: string): integer; override;
    procedure AddMember(sIdent, sValue: string);
    procedure AddParam(sParaNr, sParaWert: string);
    procedure AddNty(sNtyDno, sNtyVal: string);
    function GetMemberVal(iIndex: integer): string; overload;
    function GetMemberVal(const sIdent: string): string; overload;
    function GetParamNr(iIndex: integer): string;
    function GetParamVal(iIndex: integer): string; overload;
    function GetParamVal(const sParamNr: string): string; overload;
    function GetNtyDno(iIndex: integer): string;
    function GetNtyVal(iIndex: integer): string; overload;
    function GetNtyVal(const sNtyDno: string): string; overload;
    function ExtractTDSfGDfueKonfigData: TDSfGDfueKonfigData;
    property ParamCount: integer read FParamCount;
    property NtyCount: integer read FNtyCount;
  end;

  TDSfGXmlDecodeObject = class(TDecodeXMLResponse)
  private
    function DecodeInstanzData(const sInstanzData: string; var sResult: string;
      var cInstanzAdresse: char; var sTyp: string): boolean;
    function DSfGDeaArchLogbDataLoop(const sInstanzData: string;
      var sResult: string; cInstanzAdresse: char;
      sTyp, sEndDataTag: string): boolean;
    function DSfGDfuDataLoop(const sInstanzData: string;
      var sResult: string; cInstanzAdresse: char;
      sTyp, sEndDataTag: string): boolean;
    function DecodeDataLine(const sDataLine: string; var sDEA: string;
      var sWert: string; var sUxDT: string; var sONr: string;
      var sStatus: string; var sCRC: string; var sSigVerifyStatus: string): boolean;
    function DecodeDfuDataLine(const sDataLine: string; var sGroup: string;
      var sIdent: string; var sWert: string): boolean;
    function DecodeAttnTelegrDataLine(const sDataLine: string;
      var sNty, sTimeZone: string; var dtDateTime: TDateTime): boolean;
    function CreateKonvListObj(sDEA: string): TKonvListObj;
  protected
  public
    function ConvertDataToList(const sXmlInput: string; var sResult: string;
      var sTyp: string; bThisTyp: boolean = False): boolean;
    function ConvertXmlToDfuParamList(const sXmlInput: string): TDfuParamList;
    function ConvertXmlToDSfGDfueKonfigData(
      const sXmlInput: string): TDSfGDfueKonfigData;
    function ConvertXmlToDSfGAufmTelegrData(
      const sXmlInput: string): TAufmTelegrammList;
    function ConvertXmlToLogRohfileList(const sXmlInput: string): string;
    function ConvertXmlToZeitSyncInfoData(
      sXmlData: string; var ZS_Fehlergruppe: integer; var ZS_Fehlercode: integer;
      var pData: TZeitSyncInfoData): boolean;
    function ConvertXmlToFwUpdateInfoData(sXmlData: string;
      var pData: TFwUpdateInfoData): boolean;
    function ConvertDataToTelegramFile(pSlList: TStrings;
      sPath: TFileName; cDEB: char = 'M'; cAdr: char = '_'): TFileName;
    function ConvertArchivDataListToTelegramFiles(
      pSlList: TStrings; sPath: TFileName; cDEB: char = 'O'): TStrings;
    function ConvertDeaDataListToTelegramFiles(
      pSlList: TStrings; sPath: TFileName; cDEB: char = 'M'): TStrings;
    function ConvertDataListToTelegramFiles(
      pSlList: TStrings; sPath: TFileName; cDEB: char = 'M'): TStrings;
    function ConvertXMLToTelegramFiles(
      sXMLInput: string; sPath: TFileName): TStrings;
    function ConvertXMLFilesToTelegramFiles(pSlXml: TStrings): TStrings;
    function GetFirstRecordIdentValues(sFileName: TFileName;
      var iOrdNr: integer; var dtDatumZeit: TDateTime): boolean;
    function GetXmlDSfGLogData_AR (const sFileName: TFileName;
      sEAdr: string; iGruppe, iKanal: integer; var iLogErg: integer;
      var sRohFileNames: string): boolean;
    function GetXmlDSfGLogData_LB (const sFileName: TFileName;
      sEAdr, sQuellAdr: string; var iLogErg: integer;
      var sRohFileNames: string): boolean;
    function GetXmlDSfGLogData_DE (const sFileName: TFileName;
      sEAdr: string; var iLogErg: integer; var sRohFileNames: string): boolean;
    function ConvertXMLFileToTelegramFile(
      sXMLFile: TFileName): TFileName;
    function ConvertXmlToParamChangeResultRec(
      sXmlData: string; sTyp: string): TParaEinstellResultData;
    function ConvertXmlToRufDeaktAntwortData(sXmlData: string;  // 14.02.2005
      var iErrorGroup, iErrorCode: integer; var sRufNummer: string): boolean;
    function SplitXMLParts(const sXML, sPartSeparator: string;
      var sHeaderEnvelope, sFooterEnvelope, sDataPart: string): boolean;
    function SplitIntoEqualDataLines(
      const sXML, sEqualDataName: string; pSlDataLines: TStrings): boolean;
  end;

  TXMLSplitDataList = class(TStringList)  // 01.08.2006
    constructor Create(const sXmlFile: TFileName;
      const sPartSeparator, sSplitName: string); virtual;
    destructor Destroy; override;
  private
    FDecodeObject  : TDSfGXmlDecodeObject;  // Decoder
    FResult        : boolean;  // Ergebnis-Evaluierung
    FTempPath      : TFileName; // Pfad für Dateiausgabe
    FInstanzAddr   : char;
    FHeaderPart    : string;   // Globaler Datenheader
    FDataPart      : string;   // Globaler Datenteil
    FFooterPart    : string;   // Globaler Datenfooter
    FPartSeparator : string;   // String für Trennung in Header, Footer, ...
    FSplitName     : string;   // Attribut, nach dem die Daten zerlegt werden
    FZeitangaben   : TZeitangaben;  // Gebildete Zeitangaben-Information
    FFileNameList  : TStrings;
    function GetDataValue(iIndex: integer): string;
    function GetDataString(iIndex: integer): string;
    function GetFileName(iIndex: integer): TFileName;
    function CalcZeitangaben: boolean;
  protected
  public
    function SaveToFiles: boolean;

    property MyResult: boolean read FResult;
    property InstanzAddr: char read FInstanzAddr;
    property HeaderPart: string read FHeaderPart;
    property DataPart: string read FDataPart;
    property FooterPart: string read FFooterPart;
    property Zeitangaben: TZeitangaben read FZeitangaben;
    property DataValue [iIndex: integer]: string read GetDataValue;
    property DataString [iIndex: integer]: string read GetDataString;
    property FileName [iIndex: integer]: TFileName read GetFileName;
  end;

implementation

const
  C_File_ViPrefix = '~Vi';

{----------------------------- TDfuParamList ---------------------------}

{-------------------------------------------------}
constructor TDfuParamList.Create;
{-------------------------------------------------}
begin
  inherited;

  FMemberList := TStringList.Create;
  FParaListNr := TStringList.Create;
  FParaListWert := TStringList.Create;
  FNtyListDno := TStringList.Create;
  FNtyListWert := TStringList.Create;

  FParamCount := 0;
  FNtyCount := 0;
end;

{-------------------------------------------------}
destructor TDfuParamList.Destroy;
{-------------------------------------------------}
begin
  FMemberList.Free;
  FParaListNr.Free;
  FParaListWert.Free;
  FNtyListDno.Free;
  FNtyListWert.Free;

  inherited;
end;

{-------------------------------------------------}
procedure TDfuParamList.Clear;
{-------------------------------------------------}
begin
  FMemberList.Clear;
  FParaListNr.Clear;
  FParaListWert.Clear;
  FNtyListDno.Clear;
  FNtyListWert.Clear;

  FParamCount := 0;
  FNtyCount := 0;

  inherited;
end;

{-------------------------------------------------}
procedure TDfuParamList.Delete(iIndex: integer);
{-------------------------------------------------}
begin
  FMemberList.Delete(iIndex);

  inherited;
end;

{-------------------------------------------------}
function TDfuParamList.Add(const sMemberIdent: string): integer;
{-------------------------------------------------}
begin
  FMemberList.Add('');

  Result := inherited Add(sMemberIdent);
end;

{-------------------------------------------------}
procedure TDfuParamList.AddMember(sIdent, sValue: string);
{-------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := IndexOf(sIdent);

  if (iIndex < 0) then iIndex := Add(sIdent);
  FMemberList[iIndex] := DecodeXml(sValue);
end;

{-------------------------------------------------}
procedure TDfuParamList.AddParam(sParaNr, sParaWert: string);
{-------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := FParaListNr.IndexOf(sParaNr);

  if (iIndex < 0) then begin
    FParaListNr.Add(sParaNr);
    FParaListWert.Add(DecodeXml(sParaWert));
    Inc(FParamCount);
  end
  else FParaListWert[iIndex] := DecodeXml(sParaWert);
end;

{-------------------------------------------------}
procedure TDfuParamList.AddNty(sNtyDno, sNtyVal: string);
{-------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := FNtyListDno.IndexOf(sNtyDno);

  if (iIndex < 0) then begin
    FNtyListDno.Add(sNtyDno);
    FNtyListWert.Add(sNtyVal);
    Inc(FNtyCount);
  end
  else FNtyListWert[iIndex] := sNtyVal;
end;

{-------------------------------------------------}
function TDfuParamList.GetMemberVal(iIndex: integer): string;
{-------------------------------------------------}
begin
  Result := FMemberList[iIndex];
end;

{-------------------------------------------------}
function TDfuParamList.GetMemberVal(const sIdent: string): string;
{-------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := IndexOf(sIdent);
  if (iIndex >= 0) then Result := GetMemberVal(iIndex) else Result := '';
end;

{-------------------------------------------------}
function TDfuParamList.GetParamNr(iIndex: integer): string;
{-------------------------------------------------}
begin
  Result := FParaListNr[iIndex];
end;

{-------------------------------------------------}
function TDfuParamList.GetParamVal(iIndex: integer): string;
{-------------------------------------------------}
begin
  Result := FParaListWert[iIndex];
end;

{-------------------------------------------------}
function TDfuParamList.GetParamVal(const sParamNr: string): string;
{-------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := FParaListNr.IndexOf(sParamNr);
  if (iIndex >= 0) then Result := GetParamVal(iIndex) else Result := '';
end;

{-------------------------------------------------}
function TDfuParamList.GetNtyDno(iIndex: integer): string;
{-------------------------------------------------}
begin
  Result := FNtyListDno[iIndex];
end;

{-------------------------------------------------}
function TDfuParamList.GetNtyVal(iIndex: integer): string;
{-------------------------------------------------}
begin
  Result := FNtyListWert[iIndex];
end;

{-------------------------------------------------}
function TDfuParamList.GetNtyVal(const sNtyDno: string): string;
{-------------------------------------------------}
var
  iIndex : integer;
begin
  iIndex := FNtyListDno.IndexOf(sNtyDno);
  if (iIndex >= 0) then Result := GetNtyVal(iIndex) else Result := '';
end;

{ Bildet TDSfGDfueKonfigData-Record aus Inhalt    }
{ Rückgabe: TDSfGDfueKonfigData                   }
{-------------------------------------------------}
function TDfuParamList.ExtractTDSfGDfueKonfigData: TDSfGDfueKonfigData;
{-------------------------------------------------}
var
  s : string;
  i : byte;
begin
  s := GetMemberVal(C_DSfGKennungIAdr);
  if (s <> '') then Result.EAdr_Dfue := s[1]
  else Result.EAdr_Dfue := char (NUL);
  Result.Hersteller := GetMemberVal(C_DSfGHerstellerStart);
  Result.ProgName := GetMemberVal(C_DSfGProgNameStart);
  Result.Version := GetMemberVal(C_DSfGProgVersionStart);
  Result.Extensionmode := StrToIntDef (GetMemberVal(C_DSfGExtensionmodeStart), 0);  // 31.01.2017, WW
  Result.Wieser_Teilnehmer := GetMemberVal(C_DSfGTeilnehmerStart);
  for i := 1 to 5 do begin
    s := GetMemberVal(C_lt_Subst + Format(C_DSfGIAdr_nStart, [i]) + C_gt_Subst);
    if (s <> '') then Result.Wieser_Adresse[i] := s[1]
    else Result.Wieser_Adresse[i] := s;
  end;
  Result.Wieser_Fabriknummer := GetMemberVal(C_DSfGFabrikNrStart);  // 05.10.2009, WW
  Result.Wieser_Baujahr := GetMemberVal(C_DSfGBaujahrStart);  // 05.10.2009, WW

  Result.Wieser_NG_Typ:=GetMemberVal(C_DSfGTypStart);  // 07.03.2012, WW
  Result.Wieser_NG_Version:=GetMemberVal(C_DSfGVersionStart);  // 07.03.2012, WW
  Result.Wieser_NG_Build:=GetMemberVal(C_DSfGBuildStart);  // 07.03.2012, WW
  Result.Wieser_NG_Flashgroesse:='';  // wird nicht per XML geliefert; 07.03.2012, WW
  Result.Wieser_NG_SysConfig:='';  // wird nicht per XML geliefert; 07.03.2012, WW
end;

{------------------------------ TXMLSplitDataList -----------------------------}

{-------------------------------------------------}
constructor TXMLSplitDataList.Create(const sXmlFile: TFileName;
      const sPartSeparator, sSplitName: string);
{-------------------------------------------------}
var
  s, sXml, sR, sT : string;
  c         : char;
  i         : integer;
begin
  inherited Create;

  if (FileExists(sXmlFile)) then begin
    // Dateiinhalt laden
    with TStringList.Create do
    try
      LoadFromFile(sXmlFile);
      sXml := Text;
    finally
      Free;
    end;

    FDecodeObject := TDSfGXmlDecodeObject.Create;  // Decoder
    FTempPath := ExtractFilePath(sXmlFile);
    FFileNameList := TStringList.Create;  // Namen für interne Dateien
    FPartSeparator := sPartSeparator;   // String für Trennung in Header, Footer, ...
    FSplitName := sSplitName;   // Attribut, nach dem die Daten zerlegt werden
    FillChar(FZeitangaben, SizeOf(TZeitangaben), 0);  // Vorbelegung

    // Instanzadresse ermitteln
    c := ' ';
    s := FDecodeObject.CutDataString(sXML, C_DSfGBlockStart, C_DSfGBlockEnd);
    s := FDecodeObject.GetNextDataBlock(s, C_DSfGInstanzStart, C_DSfGInstanzEnd);
    FDecodeObject.DecodeInstanzData(s, sR, c, sT);

    // Falls gültige Instanzadresse, geht es hier weiter ...
    if (c in ['A'..'_']) then begin
      FInstanzAddr := c;
      FResult := FDecodeObject.SplitXMLParts(
        sXML, FPartSeparator, FHeaderPart, FFooterPart, FDataPart);
      if (FResult) then FResult :=
        FDecodeObject.SplitIntoEqualDataLines(FDataPart, FSplitName, Self);
      for i := 0 to Count do FFileNameList.Add('');  // Dateinamen vorbelegen
      if (FResult) then FResult := CalcZeitangaben;
    end
    else FResult := False;
  end
  else FResult := False;
end;

{-------------------------------------------------}
destructor TXMLSplitDataList.Destroy;
{-------------------------------------------------}
begin
  FreeAndNil(FDecodeObject);

  inherited Destroy;
end;

{ Gibt den Datenwert, der für den betreffenden    }
{ Block gültig ist, zurück                        }
{ Parameter: Index des Blocks                     }
{ Rückgabe: Wert oder ''                          }
{-------------------------------------------------}
function TXMLSplitDataList.GetDataValue(iIndex: integer): string;
{-------------------------------------------------}
begin
  Result := GetStringPart(Strings[iIndex], 1, #9);
end;

{ Gibt die Daten, die für den betreffenden Block  }
{ gültig sind, zurück                             }
{ Parameter: Index des Blocks                     }
{ Rückgabe: Daten oder ''                         }
{-------------------------------------------------}
function TXMLSplitDataList.GetDataString(iIndex: integer): string;
{-------------------------------------------------}
var
  sValue, s : string;
begin
  s := Strings[iIndex];
  sValue := DataValue[iIndex];
  Result := Copy(s, Length(sValue)+2, Length(s)-(Length(sValue)+1));
end;

{ Gibt den Dateinamen, in der der betreffende     }
{ Block gespeichert ist, zurück                   }
{ Parameter: Index des Blocks                     }
{ Rückgabe: Dateiname oder ''                     }
{-------------------------------------------------}
function TXMLSplitDataList.GetFileName(iIndex: integer): TFileName;
{-------------------------------------------------}
begin
  Result := FFileNameList[iIndex];
  if (not FileExists(Result)) then Result := '';
end;

{ Speichert die Datenblöcke in einzelnen Dateien  }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TXMLSplitDataList.SaveToFiles: boolean;
{-------------------------------------------------}
var
  sFileName : TFileName;
  i         : integer;
begin
  try
    for i := 0 to Count-1 do begin
      sFileName := FileName[i];
      if (not FileExists(FileName[i])) then
        sFileName := CreateTempRohFile(FTempPath, C_File_ViPrefix);
      with TStringList.Create do
      try
        Add(FHeaderPart);
        Add(DataString[i]);
        Add(FFooterPart);
        SaveToFile(sFileName);
        FFileNameList[i] := sFileName;
      finally
        Free;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

{ Ermittelt aus den Unixzeiten die Zeitangaben    }
{-------------------------------------------------}
function TXMLSplitDataList.CalcZeitangaben: boolean;
{-------------------------------------------------}
var
  sUT, sZZ         : string;
  i, j, iListIndex : integer;
  pSl, pSlTemp     : TStringList;
  h,m,s,ms         : word;
begin
  if (Count > 0) then begin
    Result := True;
    pSl := TStringList.Create;
    pSlTemp := TStringList.Create;
    try
      // Unixzeiten aufschlüsseln
      FDecodeObject.SplitIntoEqualDataLines(FDataPart, 'ts', pSlTemp);
      for i := 0 to pSlTemp.Count-1 do begin
        sUT := GetStringPart(pSlTemp[i], 1, #9);
        with TStringList.Create do
        try
          Text := Copy(pSlTemp.Strings[i], Length(sUT)+2,
            Length(pSlTemp.Strings[i]) - (Length(sUT)+2));
          for j := 0 to Count-1 do  // Einmal pro Eintrag speichern
            pSl.AddObject('', TObject(StrToIntDef('$' + sUT, 0)));
        finally
          Free;
        end;
      end;
      // Zeitzonen aufschlüsseln
      pSlTemp.Clear;
      iListIndex := 0;
      FDecodeObject.SplitIntoEqualDataLines(FDataPart, 'tz', pSlTemp);
      for i := 0 to pSlTemp.Count-1 do begin
        sZZ := GetStringPart(pSlTemp[i], 1, #9);
        with TStringList.Create do
        try
          Text := Copy(pSlTemp.Strings[i], Length(sZZ)+2,
            Length(pSlTemp.Strings[i]) - (Length(sZZ)+2));
          for j := 0 to Count-1 do  // Einmal pro Eintrag speichern
            if (iListIndex < pSl.Count) then begin
              pSl[iListIndex] := sZZ;
              Inc(iListIndex);
            end
            else begin
              Result := False;
              Break;
            end;
        finally
          Free;
        end;
        if (not Result) then Break;
      end;

      // Zeiten auswerten, um Zeitzoneninfo zu erhalten
      if (pSl.Count > 0) then begin
        pSl.CustomSort(StringListObjectSort);  // Nach Zeiten sortieren
        if (pSl[pSl.Count-1] = CMEZ) or (pSl[pSl.Count-1] = CMESZ) then begin
          FZeitangaben.EAdr := FInstanzAddr;
          FZeitangaben.DatumZeit :=
            UnixToDateTime(Integer(pSl.Objects[pSl.Count-1]));
          FZeitangaben.Zeitzone := UpperCase(pSl[pSl.Count-1]);
          FZeitangaben.LetztVerstZZ := 0;
          // Schleife abwärts, um Wechsel zu prüfen
          if (FZeitangaben.Zeitzone = CMEZ) or (FZeitangaben.Zeitzone = CMESZ) 
          then begin
            for i := pSl.Count-2 downto 0 do begin
              if (UpperCase(pSl[i]) <> FZeitangaben.Zeitzone) then begin
                if (UpperCase(pSl[i]) = CMEZ) or (UpperCase(pSl[i]) = CMESZ) 
                then begin
                  DecodeTime(UnixToDateTime(Integer(pSl.Objects[i])), h, m, s, ms);
                  FZeitangaben.LetztVerstZZ :=
                    Trunc(UnixToDateTime(Integer(pSl.Objects[i]))) +
                    EncodeTime(h, 0, 0, 0) + EncodeTime(1, 0, 0, 0);
(*
                  j := 1;  // 24.10.2006

                  while ((i+j) < pSl.Count) and (UnixToDateTime(Integer(pSl.Objects[i+j])) <=
                    UnixToDateTime(Integer(pSl.Objects[i])) +
                      EncodeTime(0, 59, 59, 0))
                  do Inc(j);

                  if ((i+j) < pSl.Count) then FZeitangaben.LetztVerstZZ :=  // vorhergehender Wert ist Zeitpunkt !
                    UnixToDateTime(Integer(pSl.Objects[i+j]));
*)
                end;
                Break;
              end;
            end;
          end;
          FZeitangaben.vom_PC := False;
        end;
      end;
    finally
      pSl.Free;
      pSlTemp.Free;
    end;
  end
  else Result := True; // Kein Eintrag => kein Fehler (und kein Ergebnis ...)
end;

{----------------------------- TDSfGXmlDecodeObject ---------------------------}

{ Extrahiert Kenndaten aus XML-Datenstring        }
{ Parameter: Datenelementadresse                  }
{ Rückgabe: TKonvListObj                          }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.CreateKonvListObj(sDEA: string): TKonvListObj;
{-------------------------------------------------}
var
  iAgNr, iAkNr : integer;
  pZeit        : TZeitangaben;
begin
  Result := TKonvListObj.Create;
  iAgNr := -1;
  iAkNr := -1;
  with pZeit do begin  // Dummy-Belegung
    EAdr:=char (NUL);
    DatumZeit:=0;
    Zeitzone:='';
    LetztVerstZZ:=0;
    vom_PC:=false;
  end;
  // Archivgruppe/-kanal aus DEA bestimmen - wird unten wieder überschrieben
  if (Length(sDea) > 3) and (sDea[1] = 'c') then begin
    if (sDea[2] in ['a'{, 'b'}]) then begin
      if (sDEA[2] = 'a') then iAkNr := Ord(sDea[4]) - Ord('f') + 1
      else if (sDEA[2] = 'b') then iAkNr := 0;
      iAgNr := Ord(sDea[3]) - Ord('a') + 1;
    end;
  end;

  Result.SetData(-1, iAgNr, iAkNr, '', '', pZeit, -1, 0, '');
end;

{ Extrahiert Kenndaten aus XML-Datenstring        }
{ Parameter: XML-String, Rückgabestrings für DEA, }
{            Datum/Zeit, Status, CRC              }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.DecodeDataLine(const sDataLine:
  string; var sDEA: string; var sWert: string; var sUxDT: string;
  var sONr: string; var sStatus: string; var sCRC: string;
  var sSigVerifyStatus: string): boolean;
{-------------------------------------------------}
var
  iIndex      : integer;
  sKennwerte, sPart, sVal : string;
begin
  Result := False;  // Default

  sWert := '';    // Vorbelegung Wert;
  sDEA := '';     // Vorbelegung DEA
  sUxDT := '';    // Vorbelegung Unixzeit
  sStatus := '';  // Vorbelegung Status
  sCRC := '';     // Vorbelegung CRC
  sSigVerifyStatus := '';  // Vorbelegung Signatur-Verifizierungsergebnis

  // Zunächst Wert extrahieren
  sWert := CutDataString(sDataLine, C_DSfGRecEnd, C_DSfGRecStart, false);

  // Anschliessend zugehörige Kennwerte ermitteln
  sKennwerte := GetDataPart(sDataLine, 1, C_DSfGRecEnd, False);
  iIndex := 1;
  sPart := GetDataPart(sKennwerte, iIndex, C_DSfGDataSep);
  while (sPart <> '') do begin
    // Zugehörigen Wert abfragen
    Inc(iIndex);
    sVal := DecodeXml(GetDataPart(sKennwerte, iIndex, C_DSfGDataSep, False));

    // Um welchen Kennwert handelt es sich ?
    if (sPart = C_DSfGKennungDEA) then sDEA := Trim(sVal)
    else if (sPart = C_DSfGKennungOrdNr) then sONr := Trim(sVal)
    else if (sPart = C_DSfGKennungUnixDT) then sUxDT := Trim(sVal)
    else if (sPart = C_DSfGKennungStatus) then sStatus := Trim(sVal)
    else if (sPart = C_DSfGKennungCRC) then sCRC := Trim(sVal)
    else if (sPart = C_DSfGKennungMCode) then sWert := sVal
    else if (sPart = C_DSfGKennungSigVerifyState) then sSigVerifyStatus:=Trim (sVal)  // 15.03.2012, WW
    // Kein bekannter Kennwert => raus !
    else if (sPart <> C_DSfGDfuKennungTZone) then Exit;  

    Inc(iIndex);
    sPart := GetDataPart(sKennwerte, iIndex, C_DSfGDataSep);
  end;

  Result := True;  // Wenn wir am Ende ankommen ist es OK
end;

{ Extrahiert DFÜ-Kenndaten aus XML-Datenstring    }
{ Parameter: XML-String, Rückgabestrings für      }
{            Gruppe, Kennung und Wert             }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.DecodeDfuDataLine(const sDataLine: string; var sGroup: string;
      var sIdent: string; var sWert: string): boolean;
{-------------------------------------------------}
begin
  Result := False;  // Default
  sGroup := '';     // Vorbelegeung Gruppe
  sIdent := '';     // Vorbelegeung Kennung des Wertes
  sWert := '';      // Vorbelegeung Wert

  // Überschriften werden ignoriert
  if (Pos(sDataLine, C_WieserStart) > 0) or
     (Pos(sDataLine, C_WieserEnd) > 0) or
     (Pos(sDataLine, C_DSfGBasicStart) > 0) or
     (Pos(sDataLine, C_DSfGBasicEnd) > 0) or
     (Pos(sDataLine, C_DSfGParaList_Start) > 0) or
     (Pos(sDataLine, C_DSfGParaList_End) > 0) or
     (Pos(sDataLine, (C_DSfGDfuNtyMask + '_' + C_DSfG_List)) > 0) then Exit;

  // Um welche Gruppe handelt es sich ?
  if (Pos(C_DSfGDfuKennungParId, sDataLine) > 0) then
    sGroup := C_DfuGroup_Parameter
  else if (Pos(C_DSfGDfuKennungNtyDno, sDataLine) > 0) then
    sGroup := C_DfuGroup_NtyMask
  else sGroup := C_DfuGroup_SingleMember;

  // Zunächst Wert extrahieren
  sWert := CutDataString(sDataLine, C_DSfGRecEnd, C_DSfGRecStart, false);  // kein Trim bei DFÜ-Parameterwert ! 24.10.2007; WW

  // Anschliessend zugehörige Kennung ermitteln
  if (sGroup = C_DfuGroup_SingleMember) then begin
    sIdent := CutDataString(sDataLine, C_DSfGRecStart, C_DSfGRecEnd);
    if (sIdent <> '') then sIdent := C_lt_Subst + sIdent + C_gt_Subst;
  end
  else sIdent := CutDataString(sDataLine, C_quot_Subst, C_quot_Subst);

  Result := (sIdent <> '');
end;

{ Extrahiert Attn-Kenndaten aus XML-Datenstring   }
{ Parameter: XML-String, Rückgabestrings für      }
{            NTY, Zeitzone und datum/Zeit         }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.DecodeAttnTelegrDataLine(const sDataLine: string;
      var sNty, sTimeZone: string; var dtDateTime: TDateTime): boolean;
{-------------------------------------------------}
var
  sData, sPart, sVal : string;
  iIndex   : integer;
begin
  Result := False;
  sNty := '';
  sTimeZone := '';
  dtDateTime := -1;
  iIndex := 1;
  sData := sDataLine;
  sPart := GetDataPart(sData, iIndex, C_DSfGDataSep);
  while (sPart <> '') do begin
    // Zugehörigen Wert abfragen
    Inc(iIndex);
    sVal := GetDataPart(sData, iIndex, C_DSfGDataSep);

    // Um welchen Kennwert handelt es sich ?
    if (sPart = C_DSfGKennungUnixDT) then
      dtDateTime := XMLStringToDateTime(sVal)
    else if (sPart = C_DSfGDfuKennungNty) then sNty := sVal
    else if (sPart = C_DSfGDfuKennungTZone) then sTimeZone := sVal
    else Exit;  // Kein bekannter Kennwert => raus !

    Inc(iIndex);
    sPart := GetDataPart(sData, iIndex, C_DSfGDataSep);
  end;

  Result := True;  // Wenn wir am Ende ankommen ist es OK
end;

{ Konvertiert XML-String einer Instanz            }
{ Parameter: XML-String, Rückgabestring, InstAdr, }
{            Typ der Daten                        }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.DecodeInstanzData(const sInstanzData: string;
  var sResult: string; var cInstanzAdresse: char; var sTyp: string ): boolean;
{-------------------------------------------------}
var
  sData, sBlock, sPart, sEndDataTag : string;
begin
  Result := False;

  sData := '&lt;' + sInstanzData;

  // 1. Eintrag sollte Instanz-Adresse sein
  sBlock := StringReplace(GetNextDataBlock(
    sData, C_DSfGRecStart, C_DSfGRecEnd), #13#10, '', [rfReplaceAll]);
  sPart := GetDataPart(sBlock, 1, C_DSfGDataSep);

  if (sPart = C_DSfGKennungIAdr) then begin
    sPart := GetDataPart(sBlock, 2, C_DSfGDataSep);
    if (Length(sPart) = 1) and (sPart[1] in ['A'..'_'])
    then cInstanzAdresse := sPart[1]
    else Exit;  // Adresse nicht in ['A'..'_']
  end
  else Exit;    // Block enthält keine Adresse

  // 2. Eintrag enthält den Typ der übergebenen Daten
  sBlock := StringReplace(GetNextDataBlock(
    sData, C_DSfGRecStart, C_DSfGRecEnd), #13#10, '', [rfReplaceAll]);
  sEndDataTag := C_DSfGRecStart + '/' + sBlock + C_DSfGRecEnd;  // => Ende-Tag
  if (sBlock = C_DSfGArchVal + '_' + C_DSfG_List) then
    sTyp := C_DSfGArchVal  // Archivdaten
  else if (sBlock = C_DSfGCurrVal + '_' + C_DSfG_List) then
    sTyp := C_DSfGCurrVal  // Datenelemente
  else if (sBlock = C_DSfGLogbook + C_DSfG_List) then
    sTyp := C_DSfGLogbook  // Logbuchdaten
  else if (sBlock = C_DSfGDfuParams + '_' + C_DSfG_List) then
    sTyp := C_DSfGDfuParams  // DFÜ-Parameter
  else Exit;               // Kein bekannter Typ => raus !

  if (sTyp = C_DSfGArchVal) or (sTyp = C_DSfGCurrVal) or (sTyp = C_DSfGLogbook)
  then Result := DSfGDeaArchLogbDataLoop(
    sData, sResult, cInstanzAdresse, sTyp, sEndDataTag)
  else if (sTyp = C_DSfGDfuParams)
  then Result := DSfGDfuDataLoop(
    sData, sResult, cInstanzAdresse, sTyp, sEndDataTag);
end;

{ Konvertiert XML-String in "lesbares" Format     }
{ Parameter: XML-String, Rückgabestring,          }
{            Instanzadresse, Typ der Daten        }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.DSfGDeaArchLogbDataLoop(
  const sInstanzData: string; var sResult: string; cInstanzAdresse: char;
  sTyp, sEndDataTag: string): boolean;
{-------------------------------------------------}
const
  C_DelayCount = 10;
var
  sData, sBlock                 : string;
  sWert, sDEA, sUxDT, sOrdNr, sStatus, sCRC : string;
  iDelayCount                               : longword;
  sDummy: string;

begin
  Result := False;

  sData := sInstanzData;
  with TStringList.Create do
  try
    // Einlesen der Daten
    iDelayCount := 0;   // Counter für die Entlastung der CPU
    sBlock := StringReplace(GetNextDataBlock(
      sData, Format(C_DSfGRecordStart, [sTyp]), Format(C_DSfGRecordEnd, [sTyp]),
      Format(C_DSfGRecordEnd, [''])), #13#10, '', [rfReplaceAll]);
    while (sBlock <> '') or
          (Pos(sEndDataTag, sData) <> 1) do begin     // 14.04.2014, WW
      if (sBlock = '') then Exit;  // unerwartetes Ende => raus !

      DelayLoop(iDelayCount, C_DelayCount);

      if (DecodeDataLine(sBlock, sDEA, sWert, sUxDT, sOrdNr, sStatus, sCRC, sDummy))
      // Ausgabe: IAdr<us>DEA<us>Wert<us>UnixDT<us>ONr<us>Status<us>CRC
      then Add(cInstanzAdresse + Chr(us) + sDEA + Chr(us) + sWert + Chr(us) +
        sUxDT + Chr(us) + sOrdNr + Chr(us) + sStatus + Chr(us) + sCRC)
      else Exit;  // Fehler bei der Decodierung => raus

      sBlock := StringReplace(GetNextDataBlock(
        sData, Format(C_DSfGRecordStart, [sTyp]), Format(C_DSfGRecordEnd, [sTyp]),
        Format(C_DSfGRecordEnd, [''])), #13#10, '', [rfReplaceAll]);
    end;

    sResult := Text;
    Result := True;
  finally
    Free;
  end;
end;

{ Konvertiert XML-String in "lesbares" Format     }
{ Parameter: XML-String, Rückgabestring,          }
{            Instanzadresse, Typ der Daten        }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.DSfGDfuDataLoop(const sInstanzData: string;
      var sResult: string; cInstanzAdresse: char;
      sTyp, sEndDataTag: string): boolean;
{-------------------------------------------------}
const
  C_DelayCount = 10;
var
  sGroup, sIdent, sWert : string;
  i                     : integer;
  iDelayCount           : longword;
  pSlData               : TStrings;
begin
  try
    pSlData := TStringList.Create;
    with TStringList.Create do
    try
      // Eintrag der Busadresse
      Add(C_DfuGroup_SingleMember + Chr(us) +
        C_DSfGKennungIAdr + Chr(us) + cInstanzAdresse);

      // Übergabe des Textes an Liste
      pSlData.Text := sInstanzData;

      iDelayCount := 0;   // Counter für die Entlastung der CPU
      for i := 0 to pSlData.Count-1 do begin
        if (pSlData[i] = sEndDataTag) then Break;
        DelayLoop(iDelayCount, C_DelayCount);

        // Einlesen der Daten
        if (DecodeDfuDataLine(pSlData[i], sGroup, sIdent, sWert))
        // Ausgabe: Parameter-Gruppe<us>Kennung des Parameters<us>Wert
        then Add(sGroup + Chr(us) + sIdent + Chr(us) + sWert);
      end;

      sResult := Text;
      Result := True;
    finally
      Free;
      pSlData.Free;
    end;
  except
    Result := False;
  end;
end;

{ Konvertiert XML-String in "lesbares" Format     }
{ Parameter: XML-String, Rückgabestring,          }
{            Typ der Daten                        }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertDataToList(const sXmlInput: string;
  var sResult: string; var sTyp: string; bThisTyp: boolean = False): boolean;
{-------------------------------------------------}
var
  sData, sInstanzBlock, sInstResult : string;
  cInstAdr                          : char;
  sThisTyp                          : string;
begin
  Result := False;

  sResult := '';
  sThisTyp := sTyp;

  with TStringList.Create do
  try
    if (IsCorrectXML(sXmlInput)) and (IsCompleteXML(sXmlInput)) then begin
      // Datenteil extrahieren
      sData :=
        CutDataString(sXmlInput, C_DSfGBlockStart, C_DSfGBlockEnd){};

      // Instanzblock extrahieren
      sInstanzBlock :=
        GetNextDataBlock(sData, C_DSfGInstanzStart, C_DSfGInstanzEnd);

      // Daten aus Instanzblock verarbeiten
      while (sInstanzBlock <> '') do begin
        if (DecodeInstanzData(sInstanzBlock, sInstResult, cInstAdr, sTyp)) then
        begin
          if (not bThisTyp) or (sThisTyp = sTyp) then Add(sInstResult);
        end;

        sInstanzBlock :=
          GetNextDataBlock(sData, C_DSfGInstanzStart, C_DSfGInstanzEnd);
      end;

      sResult := Text;
      Result := True;
    end;
  finally
    Free;
  end;
end;

{ Konvertiert DFÜ-XML-String in DFÜ-Datenobject   }
{ Parameter: XML-String                           }
{ Rückgabe: Object mit DFÜ-Parametern             }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXmlToDfuParamList(
  const sXmlInput: string): TDfuParamList;
{-------------------------------------------------}
var
  sData, sTyp           : string;
  sGroup, sIdent, sWert : string;
  i                     : integer;
begin
  Result := TDfuParamList.Create;

  sTyp := C_DSfGDfuParams;
  if (ConvertDataToList(sXmlInput, sData, sTyp, True)) and
     (sTyp = C_DSfGDfuParams)
  then
    with TStringList.Create do
    try
      Text := sData;
      for i := 0 to Count-1 do begin
        sGroup := GetStringPart(Strings[i], 1);
        sIdent := GetStringPart(Strings[i], 2);
        sWert := GetStringPart(Strings[i], 3);

        if (sGroup = C_DfuGroup_SingleMember) then
          Result.AddMember(sIdent, sWert)
        else if (sGroup = C_DfuGroup_Parameter) then
          Result.AddParam(sIdent, sWert)
        else if (sGroup = C_DfuGroup_NtyMask) then
          Result.AddNty(sIdent, sWert);
      end;
    finally
      Free;
    end;
end;

{ Konvertiert XML-String in TDSfGDfueKonfigData   }
{ Parameter: XML-String                           }
{ Rückgabe: TDSfGDfueKonfigData-Object            }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXmlToDSfGDfueKonfigData(
      const sXmlInput: string): TDSfGDfueKonfigData;
{-------------------------------------------------}
var
  p : TDfuParamList;
begin
  p := ConvertXmlToDfuParamList(sXmlInput);
  with p do
  try
    Result := ExtractTDSfGDfueKonfigData;
  finally
    Free;
  end;
end;

{ Konvertiert XML-String in TAufmTelegrammList    }
{ Parameter: XML-String                           }
{ Rückgabe: TAufmTelegrammList-Object             }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXmlToDSfGAufmTelegrData(
      const sXmlInput: string): TAufmTelegrammList;
{-------------------------------------------------}
var
  cAdr         : char;
  s, sNty, sTZ : string;
  dt           : TDateTime;
  sData, sInstance : string;
begin
  Result := TAufmTelegrammList.Create;

  sData := CutDataString(sXmlInput, C_DSfGBlockStart, C_DSfGBlockEnd);
  sInstance :=
    Trim(GetNextDataBlock(sData, C_DSfGInstanzStart, C_DSfGInstanzEnd));
  while (sInstance <> '') do begin
    s :=
      CutDataString(sInstance, C_DSfGKennungIAdr + C_quot_Subst, C_quot_Subst);

    if (Length(s) = 1) and (s[1] in ['0', 'A'..'_']) then begin
      cAdr := s[1];
      sInstance := CutDataString(
        sInstance, C_DSfGDfueAufmTelegrStart, C_DSfGDfueAufmTelegrEnd);
      if (sInstance <> '') then begin
        s := Trim(GetNextDataBlock(
          sInstance, C_lt_Subst + C_DSfGAttnTelegrams, '/' + C_gt_Subst));
        while (s <> '') do begin
          if (DecodeAttnTelegrDataLine(s, sNty, sTZ, dt))
          then Result.Add(TAufmTelegrammListObj.Create(cAdr, sNty, dt, sTZ));
          s := Trim(GetNextDataBlock(
            sInstance, C_lt_Subst + C_DSfGAttnTelegrams, '/' + C_gt_Subst));
        end;
      end;
    end;

    sInstance :=
      Trim(GetNextDataBlock(sData, C_DSfGInstanzStart, C_DSfGInstanzEnd));
  end;
end;


{ Konvertiert XML-String in LogRohfileList        }
{ Parameter: XML-String                           }
{ Rückgabe: LogRohfileList-Object                 }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXmlToLogRohfileList(
      const sXmlInput: string): string;
{-------------------------------------------------}
var
  s                : string;
  sData, sInstance : string;
begin
  Result := '';

  sData := CutDataString(sXmlInput, C_WieserLogStart, C_WieserLogEnd);
  sInstance :=
    Trim(GetNextDataBlock(sData, C_DSfGInstanzStart, C_DSfGInstanzEnd));
  with TStringList.Create do
  try
    while (sInstance <> '') do begin
      s := CutDataString(
        sInstance, C_DSfGKennungIAdr + C_quot_Subst, C_quot_Subst);

      if (Length(s) = 1) and (s[1] in ['0', 'A'..'_']) then begin
        sInstance :=
          CutDataString(sInstance, C_DSfGDfueRFilesStart, C_DSfGDfueRFilesEnd);
        if (sInstance <> '') then begin
          s := Trim(GetNextDataBlock(
            sInstance, C_DSfGKennungName + C_quot_Subst, C_quot_Subst));
          while (s <> '') do begin
            Add(s);
            s := Trim(GetNextDataBlock(
              sInstance, C_DSfGKennungName + C_quot_Subst, C_quot_Subst));
          end;
        end;
      end;

      sInstance :=
        Trim(GetNextDataBlock(sData, C_DSfGInstanzStart, C_DSfGInstanzEnd));
    end;

    if (Count > 0) then Result := CommaText;
  finally
    Free;
  end;
end;


{ Konvertiert XML-Daten in Zeitsync-Information   }
{ Parameter: XML-Datenstring                      }
{ Rückgabe: Fehlergruppe/-code der Zeitsynchronisation }
{           Record mit Zeitinformationen          }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXmlToZeitSyncInfoData(
  sXmlData: string; var ZS_Fehlergruppe: integer; var ZS_Fehlercode: integer;
  var pData: TZeitSyncInfoData): boolean;
{-------------------------------------------------}
var
  s, sText : string;
begin
  with pData do begin  // Vorbelegung
    DZ_Server:=0;
    DZ_Geraet:=0;
  end;
  ZS_Fehlergruppe:=-1;
  ZS_Fehlercode:=-1;

  s := CutDataString(sXmlData, C_DSfGBlockStart, C_DSfGBlockEnd);
  s := CutDataString(s, C_DSfGInstanzStart, C_DSfGInstanzEnd);
  s := CutDataString(s, C_ZeitSynclisteStart, C_ZeitSynclisteEnd);
  if (s <> '') then begin
    sText := CutDataString(s, C_ErrorgroupStart, C_ErrorgroupEnd);
    ZS_Fehlergruppe:=StrToIntDef(sText, -1);
    sText := CutDataString(s, C_ErrorcodeStart, C_ErrorcodeEnd);
    ZS_Fehlercode:=StrToIntDef(sText, -1);
    sText := CutDataString(s, C_DZ_ServerStart, C_DZ_ServerEnd);
    pData.DZ_Server := XMLStringToDateTime(sText);
    sText := CutDataString(s, C_DSfGDZ_InstanceStart, C_DSfGDZ_InstanceEnd);
    pData.DZ_Geraet := XMLStringToDateTime(sText);
    Result := True;
  end
  else Result := False;
end;

{ Konvertiert XML-Daten in Firmware-Update-Information }
{ Parameter: XML-Datenstring                           }
{ Rückgabe: Record mit Firmware-Update-Informationen   }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXmlToFwUpdateInfoData(
  sXmlData: string; var pData: TFwUpdateInfoData): boolean;
{-------------------------------------------------}
var
  s : string;
begin
  with pData do begin  // Vorbelegung
    Version_neu:='';
    Build_neu:='';
  end;

  s := CutDataString(sXmlData, C_DSfGBlockStart, C_DSfGBlockEnd);
  s := CutDataString(s, C_DSfGInstanzStart, C_DSfGInstanzEnd);
  s := CutDataString(s, C_FwUpdListeStart, C_FwUpdListeEnd);
  if (s <> '') then begin
    pData.Version_neu := CutDataString(s, C_DSfGVersionStart, C_DSfGVersionEnd);
    pData.Build_neu := CutDataString(s, C_DSfGBuildStart, C_DSfGBuildEnd);
    Result := True;
  end
  else Result := False;      
end;


{ Konvertiert Daten in Listenform in eine Datei   }
{ Parameter: Stringliste mit DSfG-Daten, Pfad,    }
{            Art der Daten (DEB), Absende-Adr.    }
{ Rückgabe: Name der Datei                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertDataToTelegramFile(pSlList: TStrings;
  sPath: TFileName; cDEB: char = 'M'; cAdr: char = '_'): TFileName;
{-------------------------------------------------}
var
  sH, sD, sT : string;
  cIAdr      : char;
  i          : integer;

begin
  if (pSlList.Count > 0) then cIAdr := pSlList.Text[1] else cIAdr := '0';
  sH := Chr(stx) + cAdr + Chr(us)  // Header-Teil
        + '255' + Chr(us) + '1' + Chr(us) + '1' + Chr(us) + '1' + Chr(us)
        + cIAdr + Chr(us)                    { DNO }
        + 'R' + Chr(us)                      { NTY }
        + 'N' + Chr(us)                      { DFO }
        + cDEB + Chr(us)                     { DEB }
        + IntToStr(pSlList.Count) + Chr(us); { ZAE }

  sD := '';  // Datenteil
  for i := 0 to pSlList.Count-1 do
    sD := sD + Copy(pSlList[i], 3, Length(pSlList[i])-2) + Chr(gs);

  if (pSlList.Count = 0)
  then sT := Copy(sH, 1, Length(sH)-1) + Chr(fs) + Chr(etx)
  else sT := sH + Copy(sD, 1, Length(sD)-1) + Chr(fs) + Chr(etx);

  Result := CreateTempRohFile(sPath, 'TMP');
  try
    WriteRohFile(Result, sT);
  except
    Result := '';
  end
end;

{ Konvertiert Archivdatenliste in Dateien         }
{ Parameter: Stringliste mit Archivdaten, Pfad,   }
{            Art der Daten (DEB)                  }
{ Rückgabe: Stringliste mit abgelegten Dateien    }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertArchivDataListToTelegramFiles(
      pSlList: TStrings; sPath: TFileName; cDEB: char = 'O'): TStrings;
{-------------------------------------------------}
var
  s, sDea : string;
  c, cAdr : char;
  i       : integer;
  pSl     : TStrings;
  pKObj   : TKonvListObj;
begin
  pKObj := nil;               // Objekt mit akt. Kanalinformationen
  Result := TKonvList.Create;
  pSl := TStringList.Create;  // Liste für aktuell zu konvertierenden Daten
  try
    sDea := '';   // Aktueller Archivkanal
    cAdr := ' ';  // Aktuelle Instanz
    c := ' ';

    // Struktur: IAdr<us>DEA<us>Wert<us>UnixDT<us>ONr<us>Status<us>CRC
    for i := 0 to pSlList.Count-1 do begin
      s := GetStringPart(pSlList[i], 1);  // Instanz-Adresse
      if (s <> '') then c := s[1] else Continue;
      s := GetStringPart(pSlList[i], 2);  // DEA

      if (s <> '') then begin             // DEA vorhanden
        if (c <> cAdr) or (s <> sDea) then begin // Wechsel der DEA oder Instanz
          if (pSl.Count > 0) then begin
            Result.AddObject(ConvertDataToTelegramFile(pSl, sPath, cDEB), pKObj);
            pSl.Clear;
          end
          else pKObj.Free;
          cAdr := c;
          sDea := s;
          pKObj := CreateKonvListObj(sDea);
        end;
        pSl.Add(pSlList[i]);
      end;
    end;
    if (pSl.Count > 0) then  // Letztes Telegramm speichern
      Result.AddObject(ConvertDataToTelegramFile(pSl, sPath, cDEB), pKObj)
    else pKObj.Free;
  finally
    pSl.Free;
  end;
end;

{ Konvertiert Archivdatenliste in Dateien         }
{ Parameter: Stringliste mit Datenelementen,      }
{            Pfad, Art der Daten (DEB)            }
{ Rückgabe: Stringliste mit abgelegten Dateien    }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertDeaDataListToTelegramFiles(
  pSlList: TStrings; sPath: TFileName; cDEB: char = 'M'): TStrings;
{-------------------------------------------------}
var
  s       : string;
  c, cAdr : char;
  i       : integer;
  pSl     : TStrings;
begin
  Result := TStringList.Create;
  pSl := TStringList.Create;  // Liste für aktuell zu konvertierenden Daten
  try
    cAdr := ' ';  // Aktuelle Instanz
    c := ' ';

    // Struktur: IAdr<us>DEA<us>Wert<us>UnixDT<us>ONr<us>Status<us>CRC
    for i := 0 to pSlList.Count-1 do begin
      s := GetStringPart(pSlList[i], 1);  // Instanz-Adresse
      if (s <> '') then c := s[1] else Continue;

      if (s <> '') then begin             // Adresse vorhanden
        if (c <> cAdr) then begin         // Wechsel der DEA oder Instanz
          if (pSl.Count > 0) then begin
            Result.Add(ConvertDataToTelegramFile(pSl, sPath, cDEB));
            pSl.Clear;
          end;
          cAdr := c;
        end;
        pSl.Add(pSlList[i]);
      end;
    end;
    if (pSl.Count > 0) then  // Letztes Telegramm speichern
      Result.Add(ConvertDataToTelegramFile(pSl, sPath, cDEB));
  finally
    pSl.Free;
  end;
end;

{ Konvertiert DSfG-Datenliste in Dateien          }
{ Parameter: Stringliste mit Archivdaten, Pfad,   }
{            Art der Daten (DEB)                  }
{ Rückgabe: Stringliste mit abgelegten Dateien    }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertDataListToTelegramFiles(
  pSlList: TStrings; sPath: TFileName; cDEB: char = 'M'): TStrings;
{-------------------------------------------------}
begin
  if (cDEB in ['M', 'V']) then
    Result := ConvertDeaDataListToTelegramFiles(pSlList, sPath, cDEB)
  else if (cDEB in ['O', 'Z']) then begin  // Archiv-/Logbuchdaten
    Result := ConvertArchivDataListToTelegramFiles(pSlList, sPath, cDEB);
  end
  else Result := TStringList.Create;
end;

{ Konvertiert XML-String in Telegramm-Dateien     }
{ Parameter: XML-String                           }
{ Rückgabe: Stringliste mit abgelegten Dateien    }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXMLToTelegramFiles(
  sXMLInput: string; sPath: TFileName): TStrings;
{-------------------------------------------------}
var
  sData, sTyp : string;
  pSlData     : TStrings;
  cTyp        : char;
begin

  if (ConvertDataToList(sXmlInput, sData, sTyp)) then begin
    if (sTyp = C_DSfGArchVal) then cTyp := 'O'
    else if (sTyp = C_DSfGLogbook) then cTyp := 'O'
    else if (sTyp = C_DSfGCurrVal) then cTyp := 'M'
    else cTyp := ' ';

    if (cTyp <> ' ') then begin
      pSlData := TStringList.Create;
      try
        pSlData.Text := sData;
        Result := ConvertDataListToTelegramFiles(pSlData, sPath, cTyp);
      finally
        pSlData.Free;
      end;
    end
    else Result := TStringList.Create;
  end
  else Result := TStringList.Create;
end;

{ Konvertiert XML-Dateien in Telegramm-Dateien    }
{ Parameter: Liste mit XML-Dateien u. ggf. Obj.   }
{ Rückgabe: Stringliste mit abgelegten Dateien    }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXMLFilesToTelegramFiles(
  pSlXml: TStrings): TStrings;
{-------------------------------------------------}
var
  i, j      : integer;

begin
  if (pSlXml is TKonvList)
  then Result := TKonvList.Create
  else Result := TStringList.Create;

  for i := 0 to pSlXml.Count-1 do begin
    if (FileExists(pSlXml[i])) then begin
      j := Result.Add(ConvertXMLFileToTelegramFile(pSlXml[i]));

      // Ggf. Objektinhalte übernehmen
      if (pSlXml is TKonvList) and (Assigned(pSlXml.Objects[i])) then
      begin
        Result.Objects[j] := TKonvListObj.Create;
        TKonvListObj(Result.Objects[j]).InstanzId :=
          TKonvListObj(pSlXml.Objects[i]).InstanzId;
        TKonvListObj(Result.Objects[j]).Gruppe :=
          TKonvListObj(pSlXml.Objects[i]).Gruppe;
        TKonvListObj(Result.Objects[j]).Kanal :=
          TKonvListObj(pSlXml.Objects[i]).Kanal;
        TKonvListObj(Result.Objects[j]).Kanaltyp :=
          TKonvListObj(pSlXml.Objects[i]).Kanaltyp;
        TKonvListObj(Result.Objects[j]).Werteart :=
          TKonvListObj(pSlXml.Objects[i]).Werteart;
        TKonvListObj(Result.Objects[j]).Zeitangaben :=
          TKonvListObj(pSlXml.Objects[i]).Zeitangaben;
        TKonvListObj(Result.Objects[j]).InstanzId_Quelle :=
          TKonvListObj(pSlXml.Objects[i]).InstanzId_Quelle;
        TKonvListObj(Result.Objects[j]).GerTypNr_Quelle :=
          TKonvListObj(pSlXml.Objects[i]).GerTypNr_Quelle;
        // 28.12.2015, WW
        TKonvListObj(Result.Objects[j]).JrnZB_Soll.OrdNr_Von :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB_Soll.OrdNr_Von;
        TKonvListObj(Result.Objects[j]).JrnZB_Soll.OrdNr_Bis :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB_Soll.OrdNr_Bis;
        TKonvListObj(Result.Objects[j]).JrnZB_Soll.DZ_Von :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB_Soll.DZ_Von;
        TKonvListObj(Result.Objects[j]).JrnZB_Soll.DZ_Bis :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB_Soll.DZ_Bis;
        TKonvListObj(Result.Objects[j]).JrnZB_Ist.OrdNr_Von :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB_Ist.OrdNr_Von;
        TKonvListObj(Result.Objects[j]).JrnZB_Ist.OrdNr_Bis :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB_Ist.OrdNr_Bis;
        TKonvListObj(Result.Objects[j]).JrnZB_Ist.DZ_Von :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB_Ist.DZ_Von;
        TKonvListObj(Result.Objects[j]).JrnZB_Ist.DZ_Bis :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB_Ist.DZ_Bis;
        TKonvListObj(Result.Objects[j]).JrnZB_Fehlercode :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB_Fehlercode;
        TKonvListObj(Result.Objects[j]).JrnZB :=
          TKonvListObj(pSlXml.Objects[i]).JrnZB;
        // 12.07.2016, WW
        TKonvListObj(Result.Objects[j]).EAdr :=
          TKonvListObj(pSlXml.Objects[i]).EAdr;
        TKonvListObj(Result.Objects[j]).SetKanalKonvList (
          TKonvListObj(pSlXml.Objects[i]).KanalKonvList);
        TKonvListObj(Result.Objects[j]).SetKanalJrnZBList (
          TKonvListObj(pSlXml.Objects[i]).KanalJrnZBList);
      end
      else if (Result[Result.Count-1] = '') then  // 21.01.2016, WW
        Result.Delete(Result.Count-1)
    end;
  end;
end;

{-------------------------------------------------}
function TDSfGXmlDecodeObject.GetFirstRecordIdentValues(sFileName: TFileName;
  var iOrdNr: integer; var dtDatumZeit: TDateTime): boolean;
{-------------------------------------------------}
var
  pRohStream  : TFileOfCharStream;  // 23.09.2015, WW
  s, s1, s2   : string;
  iUnix       : cardinal;
  iErrorCode  : integer;
begin
  Result := False;
  if (FileExists(sFileName)) then
  try
    pRohStream := TFileOfCharStream.Create(sFileName, fmOpenRead);
    try
      s1 := C_lt_Subst + C_DSfGArchVal + ' ';   // Anfang: Archivdaten
      s2 := C_lt_Subst + C_DSfGLogbook + ' ';   // Anfang: Logbuchdaten
      s := SetStreamPos(pRohStream, [s1, s2]);  // Stream auf Daten position.
      if (s <> '') then begin
        s := ReadStreamUntil(pRohStream, C_gt_Subst);  // 1. Datenzeile lesen
        if (s <> '') then begin
          // Ordnungsnummer
          s1 := CutDataString(s, C_DSfGKennungOrdNr, C_gt_Subst);  // Kennung
          s2 := CutDataString(s1, C_quot_Subst, C_quot_Subst);     // Wert
          iOrdNr := StrToIntDef(s2, 0);                            // Umwandeln
          if (iOrdNr = 0) then Exit;                               // Prüfen

          // Datum-Zeit
          s1 := CutDataString(s, C_DSfGKennungUnixDT, C_gt_Subst); // Kennung
          s2 := CutDataString(s1, C_quot_Subst, C_quot_Subst);     // Wert
          Val('$' + s2, iUnix, iErrorCode);                        // Prüfen
          if (iErrorCode = 0) AND (iUnix > 0)
          then UnixTimeStrToDateTime(s2, dtDatumZeit)              // Umwandeln
          else Exit;

          Result := True;  // Puh, geschafft ...
        end;
      end;
    finally
      pRohStream.Free;
    end;
  except
  // Result ist bereits FALSE
  end;
end;

{-------------------------------------------------}
function TDSfGXmlDecodeObject.GetXmlDSfGLogData_AR (const sFileName: TFileName;
  sEAdr: string; iGruppe, iKanal: integer; var iLogErg: integer;
  var sRohFileNames: string): boolean;
{-------------------------------------------------}
var
  pRohStream            : TFileOfCharStream;  // 23.09.2015, WW
  s, sDea, sL, sL1, sL2 : string;

  procedure PosOnInstance;
  begin
    // Datentyp prüfen
    pRohStream.Seek(0, soFromBeginning);
    s := C_DSfGBlockStart;   // Prüfung: DSfG-Daten
    s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
    if (s = '') then Exit;
    s := C_DSfGInstanzStart + C_DSfGKennungIAdr + C_quot_Subst +
      sEAdr + C_quot_Subst;  // Prüfung: Busadresse
    s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
  end;

  procedure FillLogList;
  begin
    if iKanal > 0 then  // LogData für Archivkanal; 12.07.2016, WW
      sDea := GetDeaArchivkanal(iGruppe, iKanal) + 'd'
    else  // LogData für Archivgruppe
      sDea := GetDeaArchivgruppe(iGruppe);
    // Logergebnis: Auf Eintrag positionieren
    s := C_lt_Subst + C_DSfGArchLog + ' ' + C_DSfGKennungDEA + C_quot_Subst +
      sDea + C_quot_Subst;  // Positionieren auf DEA des Kanals
    s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
    if (s <> '') then begin
      s := ReadStreamUntil(pRohStream, C_gt_Subst);  // Datenzeile lesen
      if (s <> '') then begin
        // Logergebnis extrahieren
        s := CutDataString(s, C_DSfGKennungConvState, C_gt_Subst); // Kennung
        s := CutDataString(s, C_quot_Subst, C_quot_Subst);     // Wert
        iLogErg := StrToIntDef(s, -1);                          // Umwandeln
        Result := (iLogErg >= 0);
      end;
    end;
  end;

  procedure FillRawFileList;
  begin
    sDea := C_lt_Subst + C_RawfileLog + ' ' + C_DSfGKennungName;
    // Rohdatenliste: Auf Eintrag positionieren
    with TStringList.Create do
    try
      while (s <> '') do begin
        s := SetStreamPos(pRohStream, [sDea]);  // Stream auf Daten position.
        if (s <> '') then begin
          s := ReadStreamUntil(pRohStream, C_gt_Subst);  // Datenzeile lesen
          if (s <> '') then begin
            // Dateiergebnis extrahieren
            s := Trim(CutDataString(s, C_quot_Subst, C_quot_Subst)); // Datei
            if (s <> '') then Add(s);
          end;
        end;
      end;
      if (Count > 0) then sRohFileNames := CommaText;
    finally
      Free;
    end;
  end;

begin
  Result := False;
  iLogErg := -1;
  sRohFileNames := '';

  if (FileExists(sFileName)) then
  try
    pRohStream := TFileOfCharStream.Create(sFileName, fmOpenRead);
    try
      // Datentyp prüfen
      PosOnInstance;
      if (s = '') then Exit;
      // Suchstrings setzen
      sL1 := C_lt_Subst + C_DSfGArchLog + '_' + C_DSfG_List + C_gt_Subst;  // Log-Liste
      sL2 := C_lt_Subst + C_RawfileLog + '_' + C_DSfG_List + C_gt_Subst;    // Dateiliste
      // Ersten Suchstring finden
      sL := SetStreamPos(pRohStream, [sL1, sL2]);  // Stream auf Daten position.
      if (sL = '') then Exit;

      // Falls sL = Log-Liste
      if (sL = sL1) then begin
        FillLogList;
        sL := SetStreamPos(pRohStream, [sL2]);  // Stream auf Daten position.
        if (sL = sL2) then FillRawFileList;     // Ggf. Dateiliste behandelm
      end
      // Falls sL = Datei-Liste
      else if (sL = sL2) then begin
        FillRawFileList;
        sL := SetStreamPos(pRohStream, [sL1]);  // Stream auf Daten position.
        if (sL = sL1) then FillLogList;         // Ggf. Logliste behandeln
      end
    finally
      pRohStream.Free;
    end;
  except
  // Result ist bereits FALSE
  end;
end;

{-------------------------------------------------}
function TDSfGXmlDecodeObject.GetXmlDSfGLogData_LB (const sFileName: TFileName;
  sEAdr, sQuellAdr: string; var iLogErg: integer;
  var sRohFileNames: string): boolean;
{-------------------------------------------------}
var
  pRohStream  : TFileOfCharStream;  // 23.09.2015, WW
  s, sDea, sL, sL1, sL2 : string;

  procedure PosOnInstance;
  begin
    // Datentyp prüfen
    pRohStream.Seek(0, soFromBeginning);
    s := C_DSfGBlockStart;   // Prüfung: DSfG-Daten
    s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
    if (s = '') then Exit;
    s := C_DSfGInstanzStart + C_DSfGKennungIAdr + C_quot_Subst +
      sEAdr + C_quot_Subst;  // Prüfung: Busadresse
    s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
  end;

  procedure FillLogList;
  begin
    sDea := GetDeaLogbuch(sQuellAdr[1]) + 'd';
    // Logergebnis: Auf Eintrag positionieren
    s := C_lt_Subst + C_DSfGLogbookLog + ' ' + C_DSfGKennungDEA + C_quot_Subst +
      sDea + C_quot_Subst;  // Positionieren auf DEA des Kanals
    s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
    if (s <> '') then begin
      s := ReadStreamUntil(pRohStream, C_gt_Subst);  // Datenzeile lesen
      if (s <> '') then begin
        // Logergebnis extrahieren
        s := CutDataString(s, C_DSfGKennungConvState, C_gt_Subst); // Kennung
        s := CutDataString(s, C_quot_Subst, C_quot_Subst);         // Wert
        iLogErg := StrToIntDef(s, -1);                          // Umwandeln
        Result := (iLogErg >= 0);
      end;
    end;
  end;

  procedure FillRawFileList;
  begin
    sDea := C_lt_Subst + C_RawfileLog + ' ' + C_DSfGKennungName;
    // Rohdatenliste: Auf Eintrag positionieren
    with TStringList.Create do
    try
      while (s <> '') do begin
        s := SetStreamPos(pRohStream, [sDea]);  // Stream auf Daten position.
        if (s <> '') then begin
          s := ReadStreamUntil(pRohStream, C_gt_Subst);  // Datenzeile lesen
          if (s <> '') then begin
            // Dateiergebnis extrahieren
            s := Trim(CutDataString(s, C_quot_Subst, C_quot_Subst)); // Datei
            if (s <> '') then Add(s);
          end;
        end;
      end;
      if (Count > 0) then sRohFileNames := CommaText;
    finally
      Free;
    end;
  end;

begin
  Result := False;
  iLogErg := -1;
  sRohFileNames := '';

  if (FileExists(sFileName)) then
  try
    pRohStream := TFileOfCharStream.Create(sFileName, fmOpenRead);
    try
      // Datentyp prüfen
      PosOnInstance;  // Stream auf Daten position.
      if (s = '') then Exit;
      // Suchstrings setzen
      sL1 := C_lt_Subst + C_DSfGLogbookLog + '_' + C_DSfG_List + C_gt_Subst;  // Log-Liste
      sL2 := C_lt_Subst + C_RawfileLog + '_' + C_DSfG_List + C_gt_Subst;    // Dateiliste
      // Ersten Suchstring finden
      sL := SetStreamPos(pRohStream, [sL1, sL2]);  // Stream auf Daten position.
      if (sL = '') then Exit;

      // Falls sL = Log-Liste
      if (sL = sL1) then begin
        FillLogList;
        sL := SetStreamPos(pRohStream, [sL2]);  // Stream auf Daten position.
        if (sL = sL2) then FillRawFileList;     // Ggf. Dateiliste behandelm
      end
      // Falls sL = Datei-Liste
      else if (sL = sL2) then begin
        FillRawFileList;
        sL := SetStreamPos(pRohStream, [sL1]);  // Stream auf Daten position.
        if (sL = sL1) then FillLogList;         // Ggf. Logliste behandeln
      end
    finally
      pRohStream.Free;
    end;
  except
  // Result ist bereits FALSE
  end;
end;

{-------------------------------------------------}
function TDSfGXmlDecodeObject.GetXmlDSfGLogData_DE (const sFileName: TFileName;
  sEAdr: string; var iLogErg: integer; var sRohFileNames: string): boolean;
{-------------------------------------------------}
var
  pRohStream  : TFileOfCharStream;  // 23.09.2015, WW
  s, sDea, sL, sL1, sL2 : string;

  procedure PosOnInstance;
  begin
    // Datentyp prüfen
    pRohStream.Seek(0, soFromBeginning);
    s := C_DSfGBlockStart;   // Prüfung: DSfG-Daten
    s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
    if (s = '') then Exit;
    s := C_DSfGInstanzStart + C_DSfGKennungIAdr + C_quot_Subst +
      sEAdr + C_quot_Subst;  // Prüfung: Busadresse
    s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
  end;

  procedure FillLogList;
  begin
    s := ReadStreamUntil(pRohStream, C_gt_Subst);  // Datenzeile lesen
    if (s <> '') then begin
      // Logergebnis extrahieren
      s := CutDataString(s, C_DSfGKennungConvState, C_gt_Subst); // Kennung
      s := CutDataString(s, C_quot_Subst, C_quot_Subst);         // Wert
      iLogErg := StrToIntDef(s, -1);                          // Umwandeln
      Result := (iLogErg >= 0);
    end;
  end;

  procedure FillRawFileList;
  begin
    sDea := C_lt_Subst + C_RawfileLog + ' ' + C_DSfGKennungName;
    // Rohdatenliste: Auf Eintrag positionieren
    with TStringList.Create do
    try
      while (s <> '') do begin
        s := SetStreamPos(pRohStream, [sDea]);  // Stream auf Daten position.
        if (s <> '') then begin
          s := ReadStreamUntil(pRohStream, C_gt_Subst);  // Datenzeile lesen
          if (s <> '') then begin
            // Dateiergebnis extrahieren
            s := Trim(CutDataString(s, C_quot_Subst, C_quot_Subst)); // Datei
            if (s <> '') then Add(s);
          end;
        end;
      end;
      if (Count > 0) then sRohFileNames := CommaText;
    finally
      Free;
    end;
  end;

begin
  Result := False;
  if (FileExists(sFileName)) then
  try
    pRohStream := TFileOfCharStream.Create(sFileName, fmOpenRead);
    try
      // Datentyp prüfen
      PosOnInstance;
      if (s = '') then Exit;

      // Suchstrings setzen
      sL1 := C_lt_Subst + C_DSfGCurrValLog;  // Log-Liste
      sL2 := C_lt_Subst + C_RawfileLog + '_' + C_DSfG_List + C_gt_Subst;    // Dateiliste
      // Ersten Suchstring finden
      sL := SetStreamPos(pRohStream, [sL1, sL2]);  // Stream auf Daten position.
      if (sL = '') then Exit;

      // Falls sL = Log-Liste
      if (sL = sL1) then begin
        FillLogList;
        sL := SetStreamPos(pRohStream, [sL2]);  // Stream auf Daten position.
        if (sL = sL2) then FillRawFileList;     // Ggf. Dateiliste behandelm
      end
      // Falls sL = Datei-Liste
      else if (sL = sL2) then begin
        FillRawFileList;
        sL := SetStreamPos(pRohStream, [sL1]);  // Stream auf Daten position.
        if (sL = sL1) then FillLogList;         // Ggf. Logliste behandeln
      end
    finally
      pRohStream.Free;
    end;
  except
  // Result ist bereits FALSE
  end;
end;

{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXMLFileToTelegramFile(
  sXMLFile: TFileName): TFileName;
{-------------------------------------------------}
var
  pRohStream  : TFileOfCharStream;  // 23.09.2015, WW
  s, s1, s2, s3 : string;
  sAdr, sTyp  : string;
  sResultLine : string;
  iDECount    : integer;
  sLStart, sLEnd : string;
  sDEA,sWert,sUxDT,sONr,sStatus,sCRC : string;
  sSigVerifyStatus: string;

begin
  Result := '';
  sResultLine := '';  // Resultierendes Telegramm
  iDECount := 0;  // Anzahl der Datenelemente

  if (FileExists(sXMLFile)) then
  try
    pRohStream := TFileOfCharStream.Create(sXMLFile, fmOpenRead);
    try
      // Datentyp prüfen
      s := C_DSfGBlockStart;   // Prüfung: DSfG-Daten
      s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
      if (s = '') then Exit;

      // Busadresse holen
      s := C_DSfGInstanzStart + C_DSfGKennungIAdr;  // Prüfung: Busadresse
      s := SetStreamPos(pRohStream, [s]);  // Stream auf Daten position.
      if (s = '') then Exit;
      s := ReadStreamUntil(pRohStream, C_gt_Subst);  // Datenzeile lesen
      sAdr := Trim(CutDataString(s, C_quot_Subst, C_quot_Subst));  // Wert
      if (Length(sAdr) <> 1) then Exit;

      // Datentyp ermitteln
      s1 := C_lt_Subst + C_DSfGArchVal + '_' + C_DSfG_List + C_gt_Subst; // Archivdaten
      s2 := C_lt_Subst + C_DSfGLogbook + C_DSfG_List + C_gt_Subst; // Logbuchdaten
      s3 := C_lt_Subst + C_DSfGCurrVal + '_' + C_DSfG_List + C_gt_Subst; // Datenelemente
      s := SetStreamPos(pRohStream, [s1, s2, s3]);  // Stream auf Daten position.
      if (s = s1) then begin
        sTyp := 'O';
        sLStart := C_lt_Subst + C_DSfGArchVal + ' ';
        sLEnd := '/' + C_DSfGArchVal + C_gt_Subst;
      end
      else if (s = s2) then begin
        sTyp := 'O';
        sLStart := C_lt_Subst + C_DSfGLogbook + ' ';
        sLEnd := '/' + C_gt_Subst;
      end
      else if (s = s3) then begin
        sTyp := 'M';
        sLStart := C_lt_Subst + C_DSfGCurrVal + ' ';
        sLEnd := '/' + C_DSfGCurrVal + C_gt_Subst;
      end;

      // Schleife durch die ermittelten Daten
      s := SetStreamPos(pRohStream, sLStart);
      if (s = '') then Exit;
      s := ReadStreamUntil(pRohStream, sLEnd);  // Datenzeile lesen
      while (s <> '') do begin
        if (DecodeDataLine(CutDataString(s, ' ', sLEnd),
          sDEA, sWert, sUxDT, sONr, sStatus, sCRC, sSigVerifyStatus)) then
        begin
          if (iDECount > 0) then sResultLine := sResultLine + Chr(gs);
          if (sTyp = 'M') then
            sResultLine := sResultLine + sDea + Chr(us) + sWert
          else begin
            // vorhandenen Signatur-Verifizierungsstatus durch Unterstrich getrennt
            // an DSfG-Status anhängen: 15.03.2012, WW
            if length (sSigVerifyStatus) > 0 then
              sSigVerifyStatus:='_' + sSigVerifyStatus;

            sResultLine := sResultLine + sDEA + Chr(us) + sWert + Chr(us) +
              sUxDT + Chr(us) + sONr + Chr(us) +
              sStatus + sSigVerifyStatus + Chr(us) + sCRC;
          end;
          Inc(iDECount);
        end
        else Exit;

        s := SetStreamPos(pRohStream, sLStart);
        if (s = '') then Break;
        s := ReadStreamUntil(pRohStream, sLEnd);  // Datenzeile lesen
      end;

      // Telegramm vervollständigen
      sResultLine := Chr(stx) + '_' + Chr(us)  // Header-Teil
        + '255' + Chr(us) + '1' + Chr(us) + '1' + Chr(us) + '1' + Chr(us)
        + sAdr + Chr(us)                    { DNO }
        + 'R' + Chr(us)                     { NTY }
        + 'N' + Chr(us)                     { DFO }
        + sTyp + Chr(us)                    { DEB }
        + IntToStr(iDECount) + Chr(us)      { ZAE }
        + sResultLine + Chr(fs) + Chr(etx); { Daten und Abschluss }

    finally
      pRohStream.Free;
    end;

    // Ergebnis in Datei schreiben
    Result := CreateTempRohFile(ExtractFilePath(sXMLFile), 'TMP');
    try
      WriteRohFile(Result, sResultLine);
    except
      Result := '';
    end
  except
  // Result ist bereits ''
  end;
end;

{ Konvertiert XML-Daten in Parametrierungsinfos   }
{ Parameter: XML-Datenstring, Datentyp (MRG, ...) }
{ Rückgabe: Record mit Parametrierungsinfos       }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXmlToParamChangeResultRec(
  sXmlData: string; sTyp: string): TParaEinstellResultData;
{-------------------------------------------------}

  function GetVal(const sKennung, sData: string): string;
  var
    iPos     : integer;
    sValData : string;
  begin
    iPos := Pos(sKennung, sData);
    if (iPos > 0) then begin
      sValData := Copy(sData, iPos, Length(sData)-iPos+1);
      Result := CutDataString(sValData, C_quot_Subst, C_quot_Subst);
    end
    else Result := '';
  end;

var
  sData : string;
begin
  with Result do begin  // Vorbelegung
    ParaTyp:='';
    BAdr:='';
    ParaAdr:='';
    WertAlt:='';
    WertNeu:='';
  end;

  // Daten in Abrufrubrik (WIESER, DSFG) ermitteln
  if (sTyp = C_CmdParatyp_DSfG) or (sTyp = C_CmdParatyp_DSfGDfue) then begin
    sData := CutDataString(sXmlData, C_DSfGBlockStart, C_DSfGBlockEnd);
    sData := Trim(CutDataString(sData, C_DSfGInstanzStart, C_DSfGInstanzEnd));

    if (sData <> '') then begin
      if (sTyp = C_CmdParatyp_DSfG) then begin
        // Busadresse steht hinter "instance"
        Result.BAdr := GetVal(C_DSfGKennungIAdr, sData);

        sData :=
          CutDataString(sXmlData, C_ParaAendListeStart, C_ParaAendListeEnd);
        if (sData <> '') then begin  // Es sind tatsächlich Daten da ...
          Result.ParaTyp := sTyp;
          Result.ParaAdr := GetVal(C_DSfGKennungDEA, sData);
          Result.WertAlt := DecodeXml(GetVal(C_ParaAendWertAlt, sData));  // XML-Ersatzzeichen decodieren; 18.06.2020, WW
          Result.WertNeu := DecodeXml(GetVal(C_ParaAendWertNeu, sData));  // XML-Ersatzzeichen decodieren; 18.06.2020, WW
        end;
      end
      else begin    // 05.01.2005
        sData := CutDataString(
          sXmlData, C_DSfGDfueParaAendListeStart, C_DSfGDfueParaAendListeEnd);
        if (sData <> '') then begin  // Es sind tatsächlich Daten da ...
          Result.ParaTyp := sTyp;
          Result.BAdr := GetVal(C_DSfGDfueParaAend_BAdr, sData);
          Result.ParaAdr := GetVal(C_DSfGDfueParaAend_ParaAdr, sData);
          Result.WertAlt := DecodeXml(GetVal(C_ParaAendWertAlt, sData));  // XML-Ersatzzeichen decodieren; 18.06.2020, WW
          Result.WertNeu := DecodeXml(GetVal(C_ParaAendWertNeu, sData));  // XML-Ersatzzeichen decodieren; 18.06.2020, WW
        end;
      end;
    end;
  end;
end;

{ Konvertiert XML-Daten in Rufdeakt-Infos         }
{ Parameter: XML-Datenstring, Rückgaben: bisherige}
{   Rufnummer, Fhlergruppe, -code                 }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.ConvertXmlToRufDeaktAntwortData(sXmlData: string;
  var iErrorGroup, iErrorCode: integer; var sRufNummer: string): boolean;
{-------------------------------------------------}
var
  s, sData, sInstance : string;
begin
  Result := False;
  sData := CutDataString(sXmlData, C_DSfGBlockStart, C_DSfGBlockEnd);
  sInstance :=
    Trim(GetNextDataBlock(sData, C_DSfGInstanzStart, C_DSfGInstanzEnd));
  while (sInstance <> '') do begin
    s :=
      CutDataString(sInstance, C_DSfGKennungIAdr + C_quot_Subst, C_quot_Subst);

    if (Length(s) = 1) and (s[1] in ['0', 'A'..'_']) then begin
      s :=
        CutDataString(sInstance, C_RufDeaktListeStart, C_RufDeaktListeEnd);
      if (s <> '') then begin
        sRufNummer :=
          CutDataString(s, C_DSfGTelNrZentraleStart, C_DSfGTelNrZentraleEnd);
        iErrorGroup :=
          StrToIntDef(CutDataString(s, C_ErrorgroupStart, C_ErrorgroupEnd), -1);
        iErrorcode :=
          StrToIntDef(CutDataString(s, C_ErrorcodeStart, C_ErrorcodeEnd), -1);
        Result := True;
        Break;
      end;
    end;

    sInstance :=
      Trim(GetNextDataBlock(sData, C_DSfGInstanzStart, C_DSfGInstanzEnd));
  end;
end;

{ Zerlegt XML-Daten in Datenheader, Nutzdaten und }
{ Datenfooter                                     }
{ Parameter: XML-Datenstring, Trennstring,        }
{   Übergabevariablen für Datenheader, Daten-     }
{   footer und Nutzdaten                          }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.SplitXMLParts(const sXML, sPartSeparator: string;
  var sHeaderEnvelope, sFooterEnvelope, sDataPart: string): boolean;
{-------------------------------------------------}
var
  iPos : integer;
  s    : string;
begin
  try
    s := sXML;  // Kopie der XML-Daten in Arbeitsdaten

    // Position des Trennstring-Starts wird detektiert
    iPos := Pos(C_lt_Subst + sPartSeparator + C_gt_Subst, s);
    if (iPos > 0) then begin
      // Entsprechend den Header herauskopieren
      sHeaderEnvelope :=
        Copy(s, 1, iPos-1) + C_lt_Subst + sPartSeparator + C_gt_Subst;
      // Daten um Header kürzen
      s := Copy(
        s, Length(sHeaderEnvelope) + 1, Length(s) - Length(sHeaderEnvelope));

      // Position des Trennstring-Stops wird detektiert
      iPos := Pos(C_lt_Subst + '/' + sPartSeparator + C_gt_Subst, s);
      if (iPos > 0) then begin
        // Entsprechend den Datenteil herauskopieren
        sDataPart := Copy(s, 1, iPos-1);
        // Der Rest der Daten ist entsprechend der Footer
        sFooterEnvelope :=
          Copy(s, Length(sDataPart) + 1, Length(s) - Length(sDataPart));
        // Begrenzende Leer- und Steuerzeichen des Datenteils löschen
        sDataPart := Trim(sDataPart);
        Result := True;
      end
      else Result := False;
    end
    else Result := False;
  except
    Result := False;
  end;
end;

{ Zerlegt XML-(Nutz-)Daten in Blöcke mit gleichem }
{ Wert für ein Attribut                           }
{ Parameter: XML-Datenstring, Attributname,       }
{   Übergabeliste für Blöcke (Wert wird jeweils   }
{   mit #9 getrennt vorangestellt)                }
{ Rückgabe: Erfolg ja/nein                        }
{-------------------------------------------------}
function TDSfGXmlDecodeObject.SplitIntoEqualDataLines(
  const sXML, sEqualDataName: string; pSlDataLines: TStrings): boolean;
{-------------------------------------------------}
var
  iPos     : integer;
  s, sLine, sDataLine, sVal, sActVal : string;
begin
  try
    s := Trim(sXML);  // Kopie der XML-Daten in Arbeitsdaten
    sDataLine := '';  // Aktuellen Block vorbelegen
    sActVal := '';    // Aktuellen Wert vorbelegen

    // Das Ende einer Datenzeile wird detektiert
    iPos := Pos('/' + C_gt_Subst, s);  // Ende mit "/>" ? ...
    if (iPos = 0) then begin           // ... sonst Prüfung auf 2x ">"
      iPos := Pos(C_gt_Subst, s);
      if (iPos > 0) then iPos := PosEx(C_gt_Subst, s, iPos+1);
    end
    else Inc(iPos);
    while (iPos > 0) do begin
      // Aktuelle Zeile mit einem enthaltenen Wert ermitteln
      sLine := Copy(s, 1, iPos-1 + Length(C_gt_Subst));

      sVal := CutDataString(sLine, sEqualDataName + '=' + C_quot_Subst,
        C_quot_Subst, True);
      // Wert ist unverändert => zu aktuellem Block hinzufügen
      if (sVal = sActVal) then begin
        sDataLine := sDataLine + Trim(sLine) + #13#10;
      end
      // Wert ist Verändert => letzten Block ablegen, neuen Block starten
      else begin
        if (sDataLine <> '') then
          pSlDataLines.Add(sActVal + #9 + Trim(sDataLine));
        sDataLine := Trim(sLine) + #13#10;
        sActVal := sVal;
      end;

      // Die Arbeitsdaten werden um den bearbeiteten Teil gekürzt
      if (iPos < Length(s))
      then s := Trim(Copy(s, Length(sLine)+1, Length(s)-Length(sLine)))
      else s := '';

      // Das nächste Ende einer Datenzeile wird detektiert (s.o.)
      iPos := Pos('/' + C_gt_Subst, s);
      if (iPos = 0) then begin
        iPos := Pos(C_gt_Subst, s);
        if (iPos > 0) then iPos := PosEx(C_gt_Subst, s, iPos+1);
      end
      else Inc(iPos);
    end;
    // Letzten Block ablegen
    if (sDataLine <> '') then pSlDataLines.Add(sActVal + #9 + Trim(sDataLine));

    Result := True;
  except
    Result := False;
  end;
end;

end.

