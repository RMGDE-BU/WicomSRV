{******************************************************************************}
{* Unit: Routinen für Antwort auf Abruf-Kommandos an Client                   *}
{* 16.12.2002  WW                                                             *}
{* 05.10.2022  WW  mit Rückgabe der Stations-Kennung statt Kennung aus        *}
{*                 Kommando für Gas-X 2.0 (DCWebS-Webservice)                 *}
{******************************************************************************}
unit AbrufAnsw;

interface

uses
  Classes, SysUtils, WStrUtils, AbrufCmd, DecodeREQ, ErrPrc32, WChars, RespConst,
  MObjMeld, MObjPara, DDELList, DListen, WSysCon, DDfueParaList, ErrConstBasic;


function GetClnt_XMLAntwort (Fehlergruppe: integer; Fehlercode: integer; Kommando: string;
                             StationsKennung: string;
                             sRetFreierText: string;
                             DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_VerbAufbauMRG (Fehlergruppe: integer; Fehlercode: integer;
                                           Kommando: string;
                                           StationsKennung: string; GerTyp: integer;
                                           ResponseTraceLogList: TDSfGDataList;
                                           VerbAutoDetectData: TVerbAutoDetectData;
                                           VerbInfoData: TVerbInfoData;
                                           sRetFreierText: string;
                                           DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_VerbAufbauDSfG (Fehlergruppe: integer; Fehlercode: integer;
                                            Kommando: string;
                                            StationsKennung: string;
                                            AufmTelegrammList: TAufmTelegrammList;
                                            DSfGDfueKonfigData: TDSfGDfueKonfigData;
                                            ZeitSyncInfoData: TZeitSyncInfoData;
                                            ZS_Fehlergruppe, ZS_Fehlercode: integer;
                                            ResponseLogList: TResponseLogList;
                                            ResponseTraceLogList: TDSfGDataList;
                                            FwUpdateInfoData: TFwUpdateInfoData;
                                            VerbInfoData: TVerbInfoData;
                                            sRetFreierText: string;
                                            DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_VerbAbbau (Fehlergruppe: integer; Fehlercode: integer;
                                       Kommando: string;
                                       StationsKennung: string;
                                       ResponseTraceLogList: TDSfGDataList;
                                       sRetFreierText: string;
                                       DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_MWTA (Fehlergruppe: integer; Fehlercode: integer;
                                  Kommando: string;
                                  StationsKennung: string; GerTyp: integer;
                                  MessFilenameList: TStringList;
                                  TagFilenameList: TStringList;
                                  MaxMessKanal: integer; MaxTagKanal: integer;
                                  MinMessKanalExt, MaxMessKanalExt: integer;
                                  Tagesende: integer;
                                  ResponseTraceLogList: TDSfGDataList;
                                  von, bis: TDateTime;
                                  LGZnorm_Daten: boolean;
                                  sRetFreierText: string;
                                  DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_ME (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string; GerTyp: integer;
                                MeldungsListe: TMeldungsListe;
                                ResponseTraceLogList: TDSfGDataList;
                                von, bis: TDateTime;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_PA (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string; GerTyp: integer;
                                ParameterListe: TParameterListe;
                                ResponseTraceLogList: TDSfGDataList;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_PR (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string; GerTyp: integer;
                                PruefFilename: string;
                                von, bis: TDateTime;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_DSfGUmschalt (Fehlergruppe: integer; Fehlercode: integer;
                                          Kommando: string;
                                          ResponseTraceLogList: TDSfGDataList;
                                          StationsKennung_vor_Umschaltung: string;
                                          StationsKennung_nach_Umschaltung: string;
                                          sRetFreierText: string;
                                          DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_AR (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string;
                                DSfGArchivdatenListe: TDSfGDataList;
                                ResponseLogList: TResponseLogList;
                                ResponseRohdatenList: TDSfGDataList;
                                ResponseTraceLogList: TDSfGDataList;
                                von, bis: TDateTime;
                                mit_Zeitzone: boolean;
                                Rufcode: integer;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string;
                                ISO646: boolean): string;

function GetClnt_XMLAntwort_LB (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string;
                                DSfGlogbuchdatenListe: TDSfGDataList;
                                ResponseLogList: TResponseLogList;
                                ResponseRohdatenList: TDSfGDataList;
                                ResponseTraceLogList: TDSfGDataList;
                                von, bis: TDateTime;
                                mit_Zeitzone: boolean;
                                Rufcode: integer;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_DE (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string;
                                DELList: TDELList;
                                ResponseLogList: TResponseLogList;
                                ResponseRohdatenList: TDSfGDataList;
                                ResponseTraceLogList: TDSfGDataList;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string;
                                ISO646: boolean): string;

function GetClnt_XMLAntwort_DSfG_Busanalyse (Fehlergruppe: integer; Fehlercode: integer;
                                             Kommando: string;
                                             StationsKennung: string;
                                             DELList: TDELList;
                                             sRetFreierText: string;
                                             DebugResp: boolean; DebugRespPfad: string;
                                             ISO646: boolean): string;

function GetClnt_XMLAntwort_DSfGDfue_Mom (Fehlergruppe: integer; Fehlercode: integer;
                                          Kommando: string;
                                          StationsKennung: string;
                                          DSfGDfue_EAdr: string;
                                          DSfGDfueParaList: TDfueParaList;
                                          ResponseTraceLogList: TDSfGDataList;
                                          sRetFreierText: string;
                                          DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_ZeitSync (Fehlergruppe: integer; Fehlercode: integer;
                                      Kommando: string;
                                      StationsKennung: string; GerTyp: integer;
                                      ResponseTraceLogList: TDSfGDataList;
                                      ZeitSyncInfoData: TZeitSyncInfoData;
                                      sRetFreierText: string;
                                      DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_Parametrieren (Fehlergruppe: integer; Fehlercode: integer;
                                           Kommando: string;
                                           StationsKennung: string; GerTyp: integer;
                                           DSfGDfue_EAdr: string;
                                           ResponseTraceLogList: TDSfGDataList;
                                           ParaEinstellResultData: TParaEinstellResultData;
                                           sRetFreierText: string;
                                           DebugResp: boolean; DebugRespPfad: string;
                                           ISO646: boolean): string;

function GetClnt_XMLAntwort_Transparent (Fehlergruppe: integer; Fehlercode: integer;
                                         Kommando: string;
                                         StationsKennung: string; TransparentAntw: string;
                                         sRetFreierText: string;
                                         DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_Ruf (Fehlergruppe: integer; Fehlercode: integer;
                                 Kommando: string;
                                 StationsKennung: string; Rufcode: byte;
                                 sRetFreierText: string;
                                 DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_Ruf_VI (Fehlergruppe: integer; Fehlercode: integer;
                                    Kommando: string;
                                    StationsKennung: string; Rufcode: byte;
                                    VerbInfoData: TVerbInfoData;
                                    sRetFreierText: string;
                                    DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_Ruf_ME_VI (Fehlergruppe: integer; Fehlercode: integer;
                                       Kommando: string;
                                       StationsKennung: string;
                                       Rufcode: byte; GerTyp: integer;
                                       Meldungsliste: TMeldungsliste;
                                       VerbInfoData: TVerbInfoData;
                                       sRetFreierText: string;
                                       DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_SMS_MWTA (Fehlergruppe: integer; Fehlercode: integer;
                                      Kommando: string;
                                      StationsKennung: string;
                                      Rufcode: byte; GerTyp: integer;
                                      MessFilename: string;
                                      TagFilename: string;
                                      MaxMessKanal: integer; MaxTagKanal: integer;
                                      Tagesende: integer;
                                      sRetFreierText: string;
                                      DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_RufannahmeMRG (Fehlergruppe: integer; Fehlercode: integer;
                                           Kommando: string;
                                           StationsKennung: string;
                                           VerbInfoData: TVerbInfoData;
                                           sRetFreierText: string;
                                           DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_RufannahmeDSfG (Fehlergruppe: integer; Fehlercode: integer;
                                            Kommando: string;
                                            StationsKennung: string;
                                            DSfGDfue_EAdr: string;
                                            AufmTelegrammList: TAufmTelegrammList;
                                            RDeakt_RufNrZentrale_Alt: string;
                                            RDeakt_Fehlergruppe: integer;
                                            RDeakt_Fehlercode: integer;
                                            VerbInfoData: TVerbInfoData;
                                            sRetFreierText: string;
                                            DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_Rufliste (Fehlergruppe: integer; Fehlercode: integer;
                                      Kommando: string;
                                      StationsKennung: string; GerTyp: integer;
                                      Rufliste: string;
                                      sRetFreierText: string;
                                      DebugResp: boolean; DebugRespPfad: string): string;

function GetClnt_XMLAntwort_ZeitAbruf(iFehlergruppe, iFehlercode: integer;
  const sKommando, sStationsKennung, sKennung, sUnixTime, sTimeBias, sTimeInfo: string;
  ResponseTraceLogList: TDSfGDataList;
  sRetFreierText, sDebugRespPfad: string; bDebugResp: boolean): string;  // 17.08.2010, 13.01.2011

implementation

{---------------------------------------------------------------------------------}
function GetClnt_AntwortHeader (Kommando: string; StationsKennung: string): string;
{---------------------------------------------------------------------------------}
{ Antwort-Header bestehend aus Kommandotyp, ProzessID, COM-Nr. und Kennung aus
  Abruf-Kommando zusammensetzen;
  Übergabe: Abruf-Kommando
            Kennung der Station
  Ergebnis: Antwort-Header }
var
  KTyp: string;
  ProzessID: string;
  SNr: string;
  Kennung: string;
  
begin
  GetAbrufKommandoTyp (Kommando, KTyp);              // Kommandotyp
  ProzessID:=GetAbrufKommandoProzessID (Kommando);   // Prozess-ID
  SNr:=GetAbrufKommandoSchnittstellenNr (Kommando);  // Schnittstellen-Nr.
// {$IFDEF GAS-X}  ab 05.10.2022, WW: mit Rückgabe der Stations-Kennung für Gas-X 2.0 (DCWebS-Webservice)
//  Kennung:=GetAbrufKommandoKennung (Kommando);  // für Gas-X: Kennung aus Kommando
// {$ELSE}
  Kennung:=StationsKennung;                     // sonst: tatsächliche Stations-Kennung
// {$ENDIF}

  Result:=KTyp + C_RespSeparator + ProzessID + C_RespSeparator + SNr + C_RespSeparator +
          Kennung;
end;

{-------------------------------------------------------------------}
function GetClnt_Antwort (Kommando: string;
                          Fehlergruppe: integer; Fehlercode: integer;
                          StationsKennung: string;
                          DatenCode: integer = -1;
                          RufCode: integer = -1): string;
{-------------------------------------------------------------------}
{ Antwort für Client zusammenstellen;
  Übergabe: Abrufkommando
            Fehlergruppe/-code
            Kennung der Station
            DatenCode (wenn < 0: Client-Antwort ohne Datencode)
            RufCode (für Rufentgegennahme-Antwort; wenn < 0: Client-Antwort ohne Rufcode)
  Ergebnis: Client-Antwort }
var
{$IFDEF GAS-X}
  KTypStr: string;
  cKTyp: char;
  GasXStatus: integer;
{$ELSE}
  Fehlercode_basic: integer;
{$ENDIF}

begin
{$IFDEF GAS-X}
  // Kommandozeichen aus Kommando lesen:
  GetAbrufKommandoTyp (Kommando, KTypStr);
  if length (KTypStr) > 0 then
    cKTyp:=KTypStr [1]
  else
    cKTyp:=NUL;

  // für GAS-X-Client: Fehlergruppe/-code in Gas-X-Status wandeln
  if Fehlergruppe > 0 then                      // Fehler, Warnung aufgetreten
    GasXStatus:=GetGasXStatus (Fehlergruppe, Fehlercode, cKTyp)
  else                                          // OK
    GasXStatus:=0;
  // Gas-X-Status in Antwort zurückgeben:
  Result:=GetClnt_AntwortHeader (Kommando, StationsKennung) + C_RespSeparator +
          IntToStr (GasXStatus) + C_RespSeparator;
{$ELSE}
  // Standard: Fehlergruppe, Fehlercode in Antwort zurückgeben
  Result:=GetClnt_AntwortHeader (Kommando, StationsKennung) + C_RespSeparator +
          IntToStr (Fehlergruppe) + C_RespSeparator +
          IntToStr (Fehlercode) + C_RespSeparator;
  // ...und Basis-Fehlercode; 16.08.2011, WW
  if Fehlergruppe > 0 then                      // Fehler, Warnung aufgetreten
    Fehlercode_basic:=GetErrorcode_basic (Fehlergruppe, Fehlercode)
  else                                          // OK
    Fehlercode_basic:=BASIC_ERR_OK;
  Result:=Result + IntToStr (Fehlercode_basic) + C_RespSeparator;  // für MDE; 16.08.2011, WW
{$ENDIF}

  if DatenCode >= 0 then
    Result:=Result + IntToStr (DatenCode) + C_RespSeparator;  { mit Datencode }
  if RufCode >= 0 then
    Result:=Result + IntToStr (RufCode) + C_RespSeparator;    { mit Rufcode }
end;

{-------------------------------------------------------------------------------------------------}
function GetClnt_DSfGUmschaltAntwort (Kommando: string; Fehlergruppe: integer; Fehlercode: integer;
                                      StationsKennung_vor_Umschaltung: string;
                                      StationsKennung_nach_Umschaltung: string): string;
{-------------------------------------------------------------------------------------------------}
{ Antwort auf DSfG-Umschaltung für Client zusammenstellen;
  Übergabe: Umschaltkommando
            Fehlergruppe/-code
            Kennungen der Stationen vor und nach der Umschaltung
  Ergebnis: Client-Antwort }
var
{$IFDEF GAS-X}
  KTypStr: string;
  cKTyp: char;
  GasXStatus: integer;
//  SKennung_neu: string;
{$ELSE}
  Fehlercode_basic: integer;
{$ENDIF}

begin
{$IFDEF GAS-X}
  // Kommandozeichen aus Kommando lesen:
  GetAbrufKommandoTyp (Kommando, KTypStr);
  if length (KTypStr) > 0 then
    cKTyp:=KTypStr [1]
  else
    cKTyp:=NUL;

  // für GAS-X-Client: Fehlergruppe/-code in Gas-X-Status wandeln
  if Fehlergruppe > 0 then                      // Fehler, Warnung aufgetreten
    GasXStatus:=GetGasXStatus (Fehlergruppe, Fehlercode, cKTyp)
  else                                          // OK
    GasXStatus:=0;

  // ab 05.10.2022, WW: mit Rückgabe der Stations-Kennung nach Umschaltung für Gas-X 2.0 (DCWebS-Webservice)
//  if GasXStatus = 0 then    // für Gas-X: Kennung_neu aus Kommando nach erfolgreicher Umschaltung
//    SKennung_neu:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3)
//  else
//    SKennung_neu:='';

  // Gas-X-Status in Antwort zurückgeben:
  Result:=GetClnt_AntwortHeader (Kommando, StationsKennung_vor_Umschaltung) + C_RespSeparator +
          StationsKennung_nach_Umschaltung + C_RespSeparator +  // 05.10.2022, WW
          IntToStr (GasXStatus) + C_RespSeparator +
          IntToStr (dc_KeineDaten) + C_RespSeparator;  // Bugfix: mit Datencode; 24.02.2020, WW
{$ELSE}
  // Standard: Fehlergruppe, Fehlercode, Basis-Fehlercode, Datencode und
  // tatsächliche Stations-Kennungen vor und nach Umschaltung in Antwort zurückgeben
  Result:=GetClnt_AntwortHeader (Kommando, StationsKennung_vor_Umschaltung) + C_RespSeparator +
          StationsKennung_nach_Umschaltung + C_RespSeparator +
          IntToStr (Fehlergruppe) + C_RespSeparator +
          IntToStr (Fehlercode) + C_RespSeparator;

  // ...und Basis-Fehlercode; 18.10.2011, WW
  if Fehlergruppe > 0 then                      // Fehler, Warnung aufgetreten
    Fehlercode_basic:=GetErrorcode_basic (Fehlergruppe, Fehlercode)
  else                                          // OK
    Fehlercode_basic:=BASIC_ERR_OK;
  Result:=Result + IntToStr (Fehlercode_basic) + C_RespSeparator;  // für MDE; 16.08.2011, WW

  // ...und Datencode; 18.10.2011, WW
  Result:=Result + IntToStr (dc_KeineDaten) + C_RespSeparator;
{$ENDIF}
end;

{------------------------------------------------------------------------------}
function GetClnt_XMLAntwort (Fehlergruppe: integer; Fehlercode: integer;
                             Kommando: string;
                             StationsKennung: string;
                             sRetFreierText: string;
                             DebugResp: boolean; DebugRespPfad: string): string;
{------------------------------------------------------------------------------}
{ XML-Antwort ohne Daten für Client zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Client-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring zusammenstellen:
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung, dc_KeineDaten);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_Response (C_XMLResult_OK, AntwStr, nil,
                                            Fehlergruppe, Fehlercode,
                                            sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{--------------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_VerbAufbauMRG (Fehlergruppe: integer; Fehlercode: integer;
                                           Kommando: string;
                                           StationsKennung: string; GerTyp: integer;
                                           ResponseTraceLogList: TDSfGDataList;
                                           VerbAutoDetectData: TVerbAutoDetectData;
                                           VerbInfoData: TVerbInfoData;
                                           sRetFreierText: string;
                                           DebugResp: boolean; DebugRespPfad: string): string;
{--------------------------------------------------------------------------------------------}
{ MRG-Verbindungsaufbau-Antwort im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Geräte-Typ
            Zeiger auf Response-Tracelog-Liste
            Record mit automatisch ermittelten Verbindungsparametern
            Verbindungsinformationen-Record
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_VerbAufbauMRG_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_VerbAufbauMRG_Response (C_XMLResult_OK, AntwStr,
                                                          VerbAutoDetectData,
                                                          VerbInfoData,
                                                          StationsKennung, GerTyp,
                                                          ResponseTraceLogList,  // 03.01.2022, WW
                                                          Fehlergruppe, Fehlercode,
                                                          sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{---------------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_VerbAufbauDSfG (Fehlergruppe: integer; Fehlercode: integer;
                                            Kommando: string;
                                            StationsKennung: string;
                                            AufmTelegrammList: TAufmTelegrammList;
                                            DSfGDfueKonfigData: TDSfGDfueKonfigData;
                                            ZeitSyncInfoData: TZeitSyncInfoData;
                                            ZS_Fehlergruppe, ZS_Fehlercode: integer;
                                            ResponseLogList: TResponseLogList;
                                            ResponseTraceLogList: TDSfGDataList;
                                            FwUpdateInfoData: TFwUpdateInfoData;
                                            VerbInfoData: TVerbInfoData;
                                            sRetFreierText: string;
                                            DebugResp: boolean; DebugRespPfad: string): string;
{---------------------------------------------------------------------------------------------}
{ DSfG-Verbindungsaufbau-Antwort im XML-Format zusammenstellen (mit Konfigurationsdaten
  der DSfG-DFÜ-Instanz);
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der DSfG-Station
            Liste mit empfangenen Aufmerksamkeits-Telegrammen
            DSfG-DFÜ-Konfigurationsdaten-Record
            ZeitSync-Info-Daten
            Fehlergruppe/-code der Zeitsynchronisation
            Zeiger auf Responselog-Liste für abgerufenen DSfG-DFÜ-Daten
            Zeiger auf Response-Tracelog-Liste
            Firmwareupdate-Info-Daten
            Verbindungsinformationen-Record
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_VerbAufbauDSfG_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_VerbAufbauDSfG_Response (C_XMLResult_OK, AntwStr,
                                                           AufmTelegrammList,
                                                           DSfGDfueKonfigData,
                                                           ZeitSyncInfoData,
                                                           ZS_Fehlergruppe,
                                                           ZS_Fehlercode,
                                                           ResponseLogList,
                                                           ResponseTraceLogList,  // 03.01.2022, WW
                                                           FwUpdateInfoData,
                                                           VerbInfoData,
                                                           Fehlergruppe, Fehlercode,
                                                           sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

// 03.01.2022, WW
{----------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_VerbAbbau (Fehlergruppe: integer; Fehlercode: integer;
                                       Kommando: string;
                                       StationsKennung: string;
                                       ResponseTraceLogList: TDSfGDataList;
                                       sRetFreierText: string;
                                       DebugResp: boolean; DebugRespPfad: string): string;
{----------------------------------------------------------------------------------------}
{ Verbindungsabbau-Antwort im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Zeiger auf Response-Tracelog-Liste
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Client-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring zusammenstellen:
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung, dc_KeineDaten);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_Response (C_XMLResult_OK, AntwStr,
                                            ResponseTraceLogList,  // 03.01.2022, WW
                                            Fehlergruppe, Fehlercode,
                                            sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{-----------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_MWTA (Fehlergruppe: integer; Fehlercode: integer;
                                  Kommando: string;
                                  StationsKennung: string; GerTyp: integer;
                                  MessFilenameList: TStringList;
                                  TagFilenameList: TStringList;
                                  MaxMessKanal: integer; MaxTagKanal: integer;
                                  MinMessKanalExt, MaxMessKanalExt: integer;
                                  Tagesende: integer;
                                  ResponseTraceLogList: TDSfGDataList;
                                  von, bis: TDateTime;
                                  LGZnorm_Daten: boolean;
                                  sRetFreierText: string;
                                  DebugResp: boolean; DebugRespPfad: string): string;
{-----------------------------------------------------------------------------------}
{ Antwort mit MRG-Messwert und/oder Tagessatzdaten im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Geräte-Typ
            Liste mit Namen der Messwertdateien
            Liste mit Namen der Tagessatzdateien
            Anzahl der Messwert-Kanäle
            Anzahl der Tagessatz-Kanäle
            Niedrigste und höchste Kanalnummer des erweiterten Messwert-Archivs
            Tagesende im Gerät
            Zeiger auf Response-Tracelog-Liste
            Zeitbereich der zurückzugebenden Messwerte/Tagessätze
            Schalter für Datenrückgabeformat (true: normierte LGZ-Daten;
                                              false: Originalwerte)
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_MWTA_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_MWTA_Response (C_XMLResult_OK, AntwStr,
                                                 MessFilenameList, TagFilenameList,
                                                 MaxMessKanal, MaxTagKanal,
                                                 MinMessKanalExt, MaxMessKanalExt,
                                                 Tagesende,
                                                 StationsKennung, GerTyp,
                                                 ResponseTraceLogList,  // 03.01.2022, WW
                                                 von, bis,
                                                 LGZnorm_Daten,
                                                 Fehlergruppe, Fehlercode,
                                                 sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{---------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_ME (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string; GerTyp: integer;
                                MeldungsListe: TMeldungsListe;
                                ResponseTraceLogList: TDSfGDataList;
                                von, bis: TDateTime;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string): string;
{---------------------------------------------------------------------------------}
{ Antwort mit MRG-Meldungen im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Geräte-Typ
            Zeiger auf Meldungsliste
            Zeiger auf Response-Tracelog-Liste
            Zeitbereich der zurückzugebenden Meldungen
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_PR_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_ME_Response (C_XMLResult_OK, AntwStr,
                                               MeldungsListe,
                                               StationsKennung, GerTyp,
                                               ResponseTraceLogList,  // 03.01.2022, WW
                                               von, bis,
                                               Fehlergruppe, Fehlercode,
                                               sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{---------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_PA (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string; GerTyp: integer;
                                ParameterListe: TParameterListe;
                                ResponseTraceLogList: TDSfGDataList;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string): string;
{---------------------------------------------------------------------------------}
{ Antwort mit MRG-Parameter im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Geräte-Typ
            Zeiger auf Parameterliste
            Zeiger auf Response-Tracelog-Liste
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Parameter-Antwortstring (Datencode wird von EnCodeXMLRESP.GET_XML_PR_Response
  // ermittelt und an den Antwortstring angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_PA_Response (C_XMLResult_OK, AntwStr,
                                               ParameterListe,
                                               StationsKennung, GerTyp,
                                               ResponseTraceLogList,  // 03.01.2022, WW
                                               Fehlergruppe, Fehlercode,
                                               sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{---------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_PR (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string; GerTyp: integer;
                                PruefFilename: string;
                                von, bis: TDateTime;
                                sRetFreierText: string; 
                                DebugResp: boolean; DebugRespPfad: string): string;
{---------------------------------------------------------------------------------}
{ Antwort mit MRG-Prüfsatzdaten im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Geräte-Typ
            Name der Prüfsatzdatei
            Zeitbereich der zurückzugebenden Prüfungssätze
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_PR_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_PR_Response (C_XMLResult_OK, AntwStr,
                                               PruefFilename,
                                               StationsKennung, GerTyp, von, bis,
                                               Fehlergruppe, Fehlercode,
                                               sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{-------------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_DSfGUmschalt (Fehlergruppe: integer; Fehlercode: integer;
                                          Kommando: string;
                                          ResponseTraceLogList: TDSfGDataList;
                                          StationsKennung_vor_Umschaltung: string;
                                          StationsKennung_nach_Umschaltung: string;
                                          sRetFreierText: string;
                                          DebugResp: boolean; DebugRespPfad: string): string;
{-------------------------------------------------------------------------------------------}
{ Antwort für DSfG-Slave-Umschaltung im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Umschaltkommando
            Zeiger auf Response-Tracelog-Liste
            Stationskennungen vor und nach Umschaltung
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Umschalt-Antwortstring:
  AntwStr:=GetClnt_DSfGUmschaltAntwort (Kommando, Fehlergruppe, Fehlercode,
                                        StationsKennung_vor_Umschaltung,
                                        StationsKennung_nach_Umschaltung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_Response (C_XMLResult_OK, AntwStr,
                                            ResponseTraceLogList,  // 03.01.2022, WW
                                            Fehlergruppe, Fehlercode, sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{-------------------------------------------------------------------------}
function GetClnt_XMLAntwort_AR (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string;
                                DSfGArchivdatenListe: TDSfGDataList;
                                ResponseLogList: TResponseLogList;
                                ResponseRohdatenList: TDSfGDataList;
                                ResponseTraceLogList: TDSfGDataList;
                                von, bis: TDateTime;
                                mit_Zeitzone: boolean;
                                Rufcode: integer;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string;
                                ISO646: boolean): string;
{------------------------------------------------------------------------}
{ Antwort mit DSfG-Archivdaten im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der DSfG-DFÜ
            Zeiger auf DSfG-Archivdatenliste
            Zeiger auf Responselog-Liste für abgerufene Archivdaten
            Zeiger auf Response-Rohdaten-Liste
            Zeiger auf Response-Tracelog-Liste
            Zeitbereich der zurückzugebenden Archivdaten
            Flag 'mit_Zeitzone': wenn true, wird Zeitzone in XML-Ergebnisdaten eingetragen
            Rufcode (für Anrufentgegennahme-Antwort mit SMS-Archivdaten)
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
            Flag 'Zeichen-Konvertierung ASCII -> ISO 646 ein/aus'
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_PR_Response ermittelt und an den Antwortstring
  // angehängt; optionaler Rufcode wird an EnCodeXMLRESP.GET_XML_AR_Response
  // übergeben und dort ebenfalls an den Antwortstring angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, ISO646);
  try
    Result:=EnCodeXMLRESP.GET_XML_AR_Response (C_XMLResult_OK, AntwStr,
                                               DSfGArchivdatenListe, ResponseLogList,
                                               ResponseRohdatenList,
                                               ResponseTraceLogList,  // 03.01.2022, WW
                                               von, bis, mit_Zeitzone, Rufcode,
                                               Fehlergruppe, Fehlercode,
                                               sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{---------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_LB (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string;
                                DSfGLogbuchdatenListe: TDSfGDataList;
                                ResponseLogList: TResponseLogList;
                                ResponseRohdatenList: TDSfGDataList;
                                ResponseTraceLogList: TDSfGDataList;
                                von, bis: TDateTime;
                                mit_Zeitzone: boolean;
                                Rufcode: integer;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string): string;
{---------------------------------------------------------------------------------}
{ Antwort mit DSfG-Logbuchdaten im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der DSfG-DFÜ
            Zeiger auf DSfG-Logbuchdatenliste
            Zeiger auf Responselog-Liste für abgerufene Logbücher
            Zeiger auf Response-Rohdaten-Liste
            Zeiger auf Response-Tracelog-Liste
            Zeitbereich der zurückzugebenden Logbucheinträge
            Flag 'mit_Zeitzone': wenn true, wird Zeitzone in XML-Ergebnisdaten eingetragen
            Rufcode (für Anrufentgegennahme-Antwort mit SMS-Logbuchdaten)
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_PR_Response ermittelt und an den Antwortstring
  // angehängt; optionaler Rufcode wird an EnCodeXMLRESP.GET_XML_LB_Response
  // übergeben und dort ebenfalls an den Antwortstring angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_LB_Response (C_XMLResult_OK, AntwStr,
                                               DSfGLogbuchdatenListe, ResponseLogList,
                                               ResponseRohdatenList,
                                               ResponseTraceLogList,  // 03.01.2022, WW
                                               von, bis, mit_Zeitzone, Rufcode,
                                               Fehlergruppe, Fehlercode,
                                               sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{-------------------------------------------------------------------------}
function GetClnt_XMLAntwort_DE (Fehlergruppe: integer; Fehlercode: integer;
                                Kommando: string;
                                StationsKennung: string;
                                DELList: TDELList;
                                ResponseLogList: TResponseLogList;
                                ResponseRohdatenList: TDSfGDataList;
                                ResponseTraceLogList: TDSfGDataList;
                                sRetFreierText: string;
                                DebugResp: boolean; DebugRespPfad: string;
                                ISO646: boolean): string;
{------------------------------------------------------------------------}
{ Antwort mit DSfG-Datenelementen im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der DSfG-DFÜ
            Zeiger auf Datenelemente-Liste
            Zeiger auf Responselog-Liste für abgerufende Datenelemente
            Zeiger auf Response-Rohdaten-Liste
            Zeiger auf Response-Tracelog-Liste
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
            Flag 'Zeichen-Konvertierung ASCII -> ISO 646 ein/aus'
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_DE_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, ISO646);
  try
    Result:=EnCodeXMLRESP.GET_XML_DE_Response (C_XMLResult_OK, AntwStr,
                                               DELList, ResponseLogList,
                                               ResponseRohdatenList,
                                               ResponseTraceLogList,  // 03.01.2022, WW
                                               Fehlergruppe, Fehlercode,
                                               sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{--------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_DSfG_Busanalyse (Fehlergruppe: integer; Fehlercode: integer;
                                             Kommando: string;
                                             StationsKennung: string;
                                             DELList: TDELList;
                                             sRetFreierText: string;
                                             DebugResp: boolean; DebugRespPfad: string;
                                             ISO646: boolean): string;
{--------------------------------------------------------------------------------------}
{ Antwort mit DSfG-Busanalysedaten (ausgewählte Datenelemente) im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der DSfG-DFÜ
            Zeiger auf Datenelemente-Liste
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
            Flag 'Zeichen-Konvertierung ASCII -> ISO 646 ein/aus'
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_DE_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, ISO646);
  try
    Result:=EnCodeXMLRESP.GET_XML_DSfG_Busanalyse_Response (C_XMLResult_OK, AntwStr,
                                                            DELList,
                                                            Fehlergruppe, Fehlercode,
                                                            sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{-------------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_DSfGDfue_Mom (Fehlergruppe: integer; Fehlercode: integer;
                                          Kommando: string;
                                          StationsKennung: string;
                                          DSfGDfue_EAdr: string;
                                          DSfGDfueParaList: TDfueParaList;
                                          ResponseTraceLogList: TDSfGDataList;
                                          sRetFreierText: string;
                                          DebugResp: boolean; DebugRespPfad: string): string;
{-------------------------------------------------------------------------------------------}
{ Antwort mit DSfG-DFÜ-Momentanwerten im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der DSfG-DFÜ
            Busadresse der DSfG-DFÜ
            Zeiger auf DSfG-DFÜ-Parameter-Liste
            Zeiger auf Response-Tracelog-Liste
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_DE_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_DSfGDfue_Mom_Response (C_XMLResult_OK, AntwStr,
                                                         DSfGDfueParaList, DSfGDfue_EAdr,
                                                         ResponseTraceLogList,  // 03.01.2022, WW
                                                         Fehlergruppe, Fehlercode,
                                                         sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{---------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_ZeitSync (Fehlergruppe: integer; Fehlercode: integer;
                                      Kommando: string;
                                      StationsKennung: string; GerTyp: integer;
                                      ResponseTraceLogList: TDSfGDataList;
                                      ZeitSyncInfoData: TZeitSyncInfoData;
                                      sRetFreierText: string;
                                      DebugResp: boolean; DebugRespPfad: string): string;
{---------------------------------------------------------------------------------------}
{ Zeitsynchronisations-Antwort im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Geräte-Typ
            Zeiger auf Response-Tracelog-Liste
            Zeitsynchronisations-Informationen
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_ZeitSync_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_ZeitSync_Response (C_XMLResult_OK, AntwStr,
                                                     ZeitSyncInfoData,
                                                     StationsKennung, GerTyp,
                                                     ResponseTraceLogList,  // 03.01.2022, WW
                                                     Fehlergruppe, Fehlercode,
                                                     sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{-----------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_Parametrieren (Fehlergruppe: integer; Fehlercode: integer;
                                           Kommando: string;
                                           StationsKennung: string; GerTyp: integer;
                                           DSfGDfue_EAdr: string;
                                           ResponseTraceLogList: TDSfGDataList;
                                           ParaEinstellResultData: TParaEinstellResultData;
                                           sRetFreierText: string;
                                           DebugResp: boolean; DebugRespPfad: string;
                                           ISO646: boolean): string;
{-----------------------------------------------------------------------------------------}
{ Antwort auf MRG-Parametrierung im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Geräte-Typ
            Busadresse der DSfG-DFÜ (bei DSfG-DFÜ-Parametrierung, sonst leer)
            Zeiger auf Response-Tracelog-Liste
            Parametrier-Ergebnisdaten
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
            Flag 'Zeichen-Konvertierung ASCII -> ISO 646 ein/aus'
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_Parametrieren_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, ISO646);  
  try
    Result:=EnCodeXMLRESP.GET_XML_Parametrieren_Response (C_XMLResult_OK, AntwStr,
                                                          ParaEinstellResultData,
                                                          StationsKennung, GerTyp,
                                                          DSfGDfue_EAdr,
                                                          ResponseTraceLogList,  // 03.01.2022, WW
                                                          Fehlergruppe, Fehlercode,
                                                          sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{------------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_Transparent (Fehlergruppe: integer; Fehlercode: integer;
                                         Kommando: string;
                                         StationsKennung: string; TransparentAntw: string;
                                         sRetFreierText: string;
                                         DebugResp: boolean; DebugRespPfad: string): string;
{------------------------------------------------------------------------------------------}
{ Antwort auf Transparent-Kommando im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Antwort auf Transparentbefehl
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_Parametrieren_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_Transparent_Response (C_XMLResult_OK, AntwStr,
                                                        TransparentAntw,
                                                        Fehlergruppe, Fehlercode,
                                                        sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{----------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_Ruf (Fehlergruppe: integer; Fehlercode: integer;
                                 Kommando: string;
                                 StationsKennung: string; Rufcode: byte;
                                 sRetFreierText: string;
                                 DebugResp: boolean; DebugRespPfad: string): string;
{----------------------------------------------------------------------------------}
{ Rufentgegennahme-Antwort im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der rufenden Station
            Rufcode
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring MIT Rufcode zusammenstellen:
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung,
                            dc_KeineDaten, Rufcode);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_Response (C_XMLResult_OK, AntwStr, nil,
                                            Fehlergruppe, Fehlercode, sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{-------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_Ruf_VI (Fehlergruppe: integer; Fehlercode: integer;
                                    Kommando: string;
                                    StationsKennung: string; Rufcode: byte;
                                    VerbInfoData: TVerbInfoData;
                                    sRetFreierText: string;
                                    DebugResp: boolean; DebugRespPfad: string): string;
{-------------------------------------------------------------------------------------}
{ Rufentgegennahme-Antwort mit Verbindungsinformationen im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der rufenden Station
            Rufcode
            Verbindungsinformationen-Record
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring MIT Rufcode zusammenstellen:
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung,
                            dc_KeineDaten, Rufcode);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_VI_Response (C_XMLResult_OK, AntwStr, VerbInfoData,
                                               Fehlergruppe, Fehlercode, sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{----------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_Ruf_ME_VI (Fehlergruppe: integer; Fehlercode: integer;
                                       Kommando: string;
                                       StationsKennung: string;
                                       Rufcode: byte; GerTyp: integer;
                                       Meldungsliste: TMeldungsliste;
                                       VerbInfoData: TVerbInfoData;
                                       sRetFreierText: string;
                                       DebugResp: boolean; DebugRespPfad: string): string;
{----------------------------------------------------------------------------------------}
{ Rufentgegennahme-Antwort mit MRG-Meldungen und Verbindungsinformationen im
  XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der rufenden Station
            Rufcode
            Geräte-Typ
            Zeiger auf Meldungsliste
            Verbindungsinformationen-Record
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode und OHNE Rufcode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_Ruf_ME_Response ermittelt und an den Antwortstring
  // angehängt; Rufcode wird an EnCodeXMLRESP.GET_XML_Ruf_ME_Response übergeben
  // und dort ebenfalls an den Antwortstring angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_Ruf_ME_VI_Response (C_XMLResult_OK, AntwStr,
                                                      Rufcode, Meldungsliste,
                                                      VerbInfoData,
                                                      StationsKennung, GerTyp,
                                                      Fehlergruppe, Fehlercode,
                                                      sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{---------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_SMS_MWTA (Fehlergruppe: integer; Fehlercode: integer;
                                      Kommando: string;
                                      StationsKennung: string;
                                      Rufcode: byte; GerTyp: integer;
                                      MessFilename: string;
                                      TagFilename: string;
                                      MaxMessKanal: integer; MaxTagKanal: integer;
                                      Tagesende: integer;
                                      sRetFreierText: string;
                                      DebugResp: boolean; DebugRespPfad: string): string;
{---------------------------------------------------------------------------------------}
{ Rufentgegennahme-Antwort mit MRG-Messwert/Tagessatzdaten aus SMS-Empfang im
  XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der rufenden Station
            Rufcode
            Geräte-Typ
            Name der Messwertdatei
            Name der Tagessatzdatei
            Anzahl der Messwert-Kanäle
            Anzahl der Tagessatz-Kanäle
            Tagesende
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode und OHNE Rufcode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_SMS_MWTA_Response ermittelt und an den Antwortstring
  // angehängt; Rufcode wird an EnCodeXMLRESP.GET_XML_SMS_MWTA_Response übergeben
  // und dort ebenfalls an den Antwortstring angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_SMS_MWTA_Response (C_XMLResult_OK, AntwStr,
                                                     Rufcode,
                                                     MessFilename, TagFilename,
                                                     MaxMessKanal, MaxTagKanal,
                                                     Tagesende,
                                                     StationsKennung, GerTyp,
                                                     Fehlergruppe, Fehlercode,
                                                     sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{--------------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_RufannahmeMRG (Fehlergruppe: integer; Fehlercode: integer;
                                           Kommando: string;
                                           StationsKennung: string;
                                           VerbInfoData: TVerbInfoData;
                                           sRetFreierText: string;
                                           DebugResp: boolean; DebugRespPfad: string): string;
{--------------------------------------------------------------------------------------------}
{ Rufannahme-Antwort im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Verbindungsinformationen-Record
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring zusammenstellen:
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung, dc_KeineDaten);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_VI_Response (C_XMLResult_OK, AntwStr,
                                               VerbInfoData,
                                               Fehlergruppe, Fehlercode,
                                               sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{---------------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_RufannahmeDSfG (Fehlergruppe: integer; Fehlercode: integer;
                                            Kommando: string;
                                            StationsKennung: string;
                                            DSfGDfue_EAdr: string;
                                            AufmTelegrammList: TAufmTelegrammList;
                                            RDeakt_RufNrZentrale_Alt: string;
                                            RDeakt_Fehlergruppe: integer;
                                            RDeakt_Fehlercode: integer;
                                            VerbInfoData: TVerbInfoData;
                                            sRetFreierText: string;
                                            DebugResp: boolean; DebugRespPfad: string): string;
{---------------------------------------------------------------------------------------------}
{ Rufannahme-Antwort im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            Busadresse der DSfG-DFÜ
            Liste mit empfangenen Aufmerksamkeits-Telegrammen
            Rufnummer der Zentrale vor der Rufdeaktivierung (Rufdeaktivierungs-Info)
            Fehlergruppe/-code der Rufdeaktivierung
            Verbindungsinformationen-Record
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_RufannahmeDSfG_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_RufannahmeDSfG_Response (C_XMLResult_OK, AntwStr,
                                                           AufmTelegrammList,
                                                           RDeakt_RufNrZentrale_Alt,
                                                           RDeakt_Fehlergruppe,
                                                           RDeakt_Fehlercode,
                                                           DSfGDfue_EAdr,
                                                           VerbInfoData,
                                                           Fehlergruppe, Fehlercode,
                                                           sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

{---------------------------------------------------------------------------------------}
function GetClnt_XMLAntwort_Rufliste (Fehlergruppe: integer; Fehlercode: integer;
                                      Kommando: string;
                                      StationsKennung: string; GerTyp: integer;
                                      Rufliste: string;
                                      sRetFreierText: string;
                                      DebugResp: boolean; DebugRespPfad: string): string;
{---------------------------------------------------------------------------------------}
{ Antwort auf MRG-Ruflistenabfrage-Kommando im XML-Format zusammenstellen;
  Übergabe: Fehlergruppe/-code
            Abrufkommando
            Kennung der Station
            MRG-Rufliste
            Freier Text für erweiterte Ergebnis-Rückgabe
            DebugResp-Flag: wenn true, wird XML-Response in File mitprotokolliert
            Pfad für XML-Response-Debugfile
  Ergebnis: XML-Antwort }
var
  AntwStr: string;
  EnCodeXMLRESP: TEnCodeXMLRESP;

begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_Parametrieren_Response ermittelt und an den Antwortstring
  // angehängt):
  AntwStr:=GetClnt_Antwort (Kommando, Fehlergruppe, Fehlercode, StationsKennung);

  // Antwort in XML-Format einpacken:
  EnCodeXMLRESP:=TEnCodeXMLRESP.Create (DebugResp, DebugRespPfad, false);
  try
    Result:=EnCodeXMLRESP.GET_XML_Rufliste_Response (C_XMLResult_OK, AntwStr,
                                                     Rufliste,
                                                     StationsKennung, GerTyp,
                                                     Fehlergruppe, Fehlercode,
                                                     sRetFreierText);
  finally
    EnCodeXMLRESP.Free;
  end;
end;

// Antwort auf ZeitAbruf-Kommando ('t')  // 17.08.2010
// Parameter: Fehlergruppe, -code, Abrufkommando, EADR, Unix-Zeit, MEZ/MESZ
//            Zeiger auf Response-Tracelog-Liste
// Antwort: XML-Antwort
//----------------------------------------------
function GetClnt_XMLAntwort_ZeitAbruf(iFehlergruppe, iFehlercode: integer;
  const sKommando, sStationsKennung, sKennung, sUnixTime, sTimeBias, sTimeInfo: string;
  ResponseTraceLogList: TDSfGDataList;
  sRetFreierText, sDebugRespPfad: string; bDebugResp: boolean): string;
//----------------------------------------------
var
  sReturn : string;
begin
  // Antwortstring OHNE Datencode zusammenstellen (Datencode wird von
  // EnCodeXMLRESP.GET_XML_ZeitAbruf_Response ermittelt und an den Antwortstring
  // angehängt):
  sReturn :=
    GetClnt_Antwort(sKommando, iFehlergruppe, iFehlercode, sStationsKennung);

  // Antwort in XML-Format einpacken:
  with TEnCodeXMLRESP.Create(bDebugResp, sDebugRespPfad, False) do
  try
    Result := GET_XML_ZeitAbruf_Response(C_XMLResult_OK,
      sReturn, sKennung, sUnixTime, sTimeBias, sTimeInfo,
      ResponseTraceLogList,  // 03.01.2022, WW
      iFehlergruppe, iFehlercode, sRetFreierText);
  finally
    Free;
  end;
end;

end.
