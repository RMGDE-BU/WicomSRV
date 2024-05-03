{******************************************************************************}
{* Unit: Routinen für Abruf-Kommandos                                         *}
{* 13.12.2002  WW  Neu                                                        *}
{* 30.04.2019  WW  Erweiterte Übergabe des Parametrier-Passworts/Passwort-    *}
{*                 nummer im XML-Server-Kommando "Parametrierung MRG" (für    *}
{*                 Tritschler VCn)                                            *}
{* 11.07.2019  WW  Verbindungsaufbau-Kommando: DSfG-Transparentmodus "I"      *}
{******************************************************************************}
unit AbrufCmd;

interface

uses
  Classes, SysUtils, WStrUtils, WChars, T_Zeit;

Const
  { Gerätetyp }
  C_GeraeteTypDSfG = 820;    { Gerätetyp-Nummer für DSfG-Stationsabruf }

  { Kommando-Aufbau }
  C_CmdSeparator       = ';';   { Trennzeichen im Kommando }
  C_CmdSeparatorDyn    = '|';   { Trennzeichen im Kommando für Blöcke mit variabler Länge }
  C_CmdSeparatorAdr    = ',';   { Trennzeichen in der Kommando-Adressliste zwischen DSfG-Busadresse und DEL-Adressen }
  C_CmdSeparatorVonBis = '-';   { Trennzeichen für Datenelement-Bereich von-bis in der Kommando-Adressliste }

  { Kommando-Inhalt }
  C_CmdZSyncBasis_W = 'W';         { MEZ fest }
  C_CmdZSyncBasis_S = 'S';         { Wechsel MEZ/MESZ }
  C_CmdZSyncAbweich_Standard = 0;  { bedeutet: Wert für min. bzw. max. Abweichung
                                               bei Zeitsynchronisation soll aus
                                               INI genommen werden }
  C_CmdReqTypeFull    = 'F';       { _data: Requesttype 'Full' }
  C_CmdReqTypePartial = 'P';       { _data: Requesttype 'Partial' }

  C_CmdRpTyp_MW = 'T';   { Rundpuffer Messwerte }
  C_CmdRpTyp_ME = 'W';   { Rundpuffer Meldungen }
  C_CmdRpTyp_PR = 'P';   { Rundpuffer Prüfungssätze }

  C_CmdRufEnde    = 0;    { Rufentgegennahme: Modus 'Ende' }
  C_CmdRufStart   = 1;    { Rufentgegennahme: Modus 'Start' }
  C_CmdRufSMS_OK  = 2;    { Rufentgegennahme: Client-Bestätigung 'SMS erfolgreich empfangen/verarbeitet' }
  C_CmdRufSMS_Wdh = 3;    { Rufentgegennahme: Client-Bestätigung 'SMS nochmal senden'; 25.11.2008 WW }

  C_CmdParatyp_MRG      = 'M';  { Typ 'MRG-Parameter' }
  C_CmdParatyp_DSfG     = 'D';  { Typ 'DSfG-Parameter' (Datenelement) }
  C_CmdParatyp_DSfGDfue = 'F';  { Typ 'DSfG-DFÜ-Parameter' (Norm-Parameter, Wieser DFÜ-Parameter, NTY-Masken) }

  C_CmdArchDatenTyp_LGZnorm = 0;  { LGZ-Daten, normiert auf Impulse }
  C_CmdArchDatenTyp_LGZorig = 1;  { LGZ-Daten, Originalwerte (physikalisch, Zählerstände...) }
  C_CmdArchDatenTyp_RohFile = 2;  { MRG-Rohdaten in Datei ausgeben (MW, TA, ME, PA) }

  C_CmdTimeout_Default = -1;  { Timeout aus WicomSrv-Einstellung nehmen }

  C_CmdEmpfModus_Telegr  = 0;  { Datenempfang beenden, wenn Telegramm vollständig eingetroffen }
  C_CmdEmpfModus_Timeout = 1;  { Datenempfang bis Timeoutzeit erreicht }

  C_CmdFktCode_Keine             = 0;  { Keine erweiterte Funktion }
  C_CmdFktCode_FwUpdGeraet       = 1;  { Erweiterte Funktion: Geräte-Firmware updaten }
  C_CmdFktCode_FwUpdGeraet_Param = 2;  { Erweiterte Funktion: Geräteparametrierung nach Firmware-Uupdate }

  C_CmdTraceLog_Deaktiviert = 0;  { Tracelog-Ausgabe deaktiviert }
  C_CmdTraceLog_Aktiviert   = 1;  { Tracelog-Ausgabe aktiviert }


  { String-Längen der Kommando-Daten }
  C_MaxMRGKennungLen        = 14;
  C_MaxDSfGKennungLen       = 12;
  C_CmdKennungLen           = 14;     { für MRG- und DSfG-Kennungen }

  C_MaxMRGPasswortLen       = 10;
  C_MaxDSfGPasswortLen      = 16;
  C_CmdPasswortLen          = 16;     { für MRG- und DSfG-Passworte }

  C_CmdRufnummerLen         = 61;     { vergrößert für Wieser-System (alter Wert: 20); 11.07.2005 WW
                                        Wahlverfahren: 1; Vorwahl: 10; Rufnummer: 20
                                        nochmal vergrößert für Elster CL-Adresse (alter Wert 31): 21.03.2007, WW
                                        Wahlverfahren: 1; Vorwahl: 10; Rufnummer: 50 }
  C_CmdGeraeteTypLen        =  5;
  C_CmdPasswortNrLen        =  1;
  C_CmdKennPruefLen         =  1;
  C_CmdModemTypLen          =  2;     { vergrößert für kombinierte Modemtyp-Zeichen; 10.04.2008, WW }
  C_CmdTransparentModusLen  =  1;
  C_CmdDSfGUmleitAdrLen     =  1;
  C_CmdFktCodeLen           =  5;
  C_CmdTraceLogLen          =  1;

  C_CmdDatumLen             =  8;
  C_CmdZeitLen              =  6;

  C_CmdParaNrLen            = 12;    { MRG: 9-stellige allg. Parameternummer;
                                       DSfG: bis zu 12-stellige Abfrage-ID möglich }

  C_CmdZSyncBasisLen        =  6;    { vergrößert für UTC-Offset; 24.02.2020, WW }
  C_CmdZSyncAbweichLen      =  6;

  C_CmdZentraleNrLen        =  1;    { Nummer der Zentrale (1 oder 2) }
  C_CmdDSfGRufDeaktLen      =  1;

type
  TKommandoTyp = (kt_unbekannt, kt_VerbAufbau, kt_VerbAbbau,
                  kt_MessAbruf, kt_MeldAbruf, kt_ParaAbruf, kt_PruefAbruf, kt_ZeitSync,
                  kt_DSfGUmschalt, kt_DSfGBusanalyse, kt_RpReset, kt_Parametrieren,
                  kt_Ruf, kt_Rufannahme, kt_Transparent,
                  kt_Rufliste, kt_SlaveRufQuitt, kt_Rueckruf, kt_ZeitAbruf);

  TDSfG_TransparentModus = (dtm_Ja, dtm_Immer, dtm_Nein);

  { Record mit Daten aus Zeitsynchronisations-Kommando }
  TZeitSyncCmdData = record
    Zeitbasis_Geraet: string [C_CmdZSyncBasisLen];
    Abweichung_min: integer;
    Abweichung_max: integer;
    BAdr_DSfG: string;  // nur für DSfG-ZeitSynch; 17.05.2013 WW
  end;

  { Record mit DSfG-Zeitsynchronisations-Kommandodaten (aus Verbindungsaufbau-Kommando) }
  TDSfG_ZeitSyncCmdData = record
    ZeitSync: boolean;
    Zeitbasis_Geraet: string [C_CmdZSyncBasisLen];
    Abweichung_min: integer;
    Abweichung_max: integer;
  end;

  { DSfG-Adresslisten-Struktur für Archiv/Logbuchabruf und DSfG-DFÜ-Parameterabruf }
   TDSfG_AdresslistData = record
     ReqType: string;   { Request-Typ: F = full, P = partial }
     AdrList: string;   { Busadresse und Datenelemente durch Komma getrennt }
   end;

  { Record mit Daten aus Verbindungsaufbau-Kommando }
  TVerbAufbauCmdData = record
    Kennung: string [C_CmdKennungLen];
    Rufnummer: string [C_CmdRufNummerLen];
    GeraeteTyp: integer;
    Passwort: string [C_CmdPasswortLen];
    PasswortNr: integer;
    KennPruef: boolean;
    ModemTyp: string [C_CmdModemTypLen];
    DSfG_TransparentModus: TDSfG_TransparentModus;
    DSfG_ZeitSyncCmdData: TDSfG_ZeitSyncCmdData;
    FktCode: integer;
    TraceLog: integer;
  end;

  { Record mit Daten aus Messwert-, Meldungs- oder Prüfsatz-Abruf-Kommando }
  TMessMeldPruefAbrufCmdData = record
    cCmd: char;  // Befehlszeichen: E = MRG-Messwerte/DSfG-Archivdaten,
                 //                 M = MRG-Meldungen/DSfG-Logbucheinträge,
                 //                 X = MRG-Prüfungssätze
    vonDZ: TDateTime;
    bisDZ: TDateTime;
    vonOrdNr: integer;  // nur für DSfG-Abruf
    bisOrdNr: integer;  // nur für DSfG-Abruf
    DSfG_AdresslistData: TDSfG_AdresslistData;  // nur für DSfG-Archiv/Logbuchabruf relevant
    KanalaktivMaske: string;  // nur für MRG-Messwertabruf relevant (optional)
    ArchivdatenTyp: integer;  // nur für MRG-Messwertabruf relevant (optional); 19.07.2005 WW
    Dateiname: string;  // nur für MRG-Abruf (optional); 10.08.2011, WW
    Dateiname_TA: string;  // nur für MRG-Abruf (optional); 10.08.2011, WW
  end;

  { Record mit Daten aus Parameter-Abruf-Kommando }
  TParaAbrufCmdData = record
    PNrAllg: string [C_CmdParaNrLen];            { nur für MRG-Parameterabruf relevant }
    ArchivdatenTyp: integer;  // nur für MRG-Parameterabruf (optional); 10.08.2011, WW
    Dateiname: string;  // nur für MRG-Parameterabruf (optional); 10.08.2011, WW
    DSfG_AdresslistData: TDSfG_AdresslistData;   { nur für DSfG-Datenelemente/DSfG-DFÜ-Parameterabruf relevant }
    Timeout: integer;  // Timeout in ms (optional); 16.08.2012, WW
  end;

  { Record mit Daten aus DSfG-Slave-Umschaltung-Kommando }
  TDSfGUmschaltCmdData = record
    Kennung_alt: string [C_CmdKennungLen];
    Kennung_neu: string [C_CmdKennungLen];
    Adresse: string [C_CmdDSfGUmleitAdrLen];
    GeraeteTyp: integer;
    Passwort: string [C_CmdPasswortLen];
    PasswortNr: integer;
    KennPruef: boolean;
  end;

  { Record mit Daten aus Parametrier-Kommando }
  TParaEinstellCmdData = record
    ParaTyp: string;
    BAdr: string;
    ParaAdr: string;
    ZCode1: string;
    ZCode2: string;
    ParaWertNeu: string;
  end;

  { Record mit Daten aus Rufannahme-Kommando }
  TRufannahmeCmdData = record
    Kennung: string [C_CmdKennungLen];
    GeraeteTyp: integer;
    Passwort: string [C_CmdPasswortLen];
    PasswortNr: integer;
    DSfG_RufDeakt: boolean;
  end;

  TTransparentCmdData = record
    Befehl: string;
    Timeout: integer;
    EmpfModus: integer;  // nur für DSfG-Abruf
  end;


function GetAbrufKommandoTyp (Kommando: string; var KTypStr: string): TKommandoTyp;
function GetAbrufKommandoProzessID (Kommando: string): string;
function GetAbrufKommandoSchnittstellenNr (Kommando: string): string;
function GetAbrufKommandoKennung (Kommando: string): string;
function GetVerbAufbauCmdData (Kommando: string): TVerbAufbauCmdData;
function GetMessMeldPruefAbrufCmdData (Kommando: string): TMessMeldPruefAbrufCmdData;
function GetParaAbrufCmdData (Kommando: string): TParaAbrufCmdData;
function GetZeitSyncCmdData (Kommando: string): TZeitSyncCmdData;
function GetDSfGUmschaltCmdData (Kommando: string): TDSfGUmschaltCmdData;
function GetRpResetCmdData (Kommando: string): string;
function GetParaEinstellCmdData (Kommando: string): TParaEinstellCmdData;
function GetTransparentCmdData (Kommando: string): TTransparentCmdData;
function GetRufCmdData (Kommando: string): integer;
function GetRufannahmeCmdData (Kommando: string): TRufannahmeCmdData;
function GetSlaveRufQuittCmdData (Kommando: string): string;
function GetRueckrufCmdData (Kommando: string): integer;

function BuildVerbAufbauSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                CmdData: TVerbAufbauCmdData): string;
function BuildVerbAbbauSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                               Kennung: string): string;
function BuildMessMeldPruefAbrufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                        Kennung: string;
                                        CmdData: TMessMeldPruefAbrufCmdData): string;
function BuildParaAbrufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                               Kennung: string; PNrAllg: string;
                               Timeout: integer): string;
function BuildDSfGUmschaltSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                  CmdData: TDSfGUmschaltCmdData): string;
function BuildRpResetSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                             Kennung: string; RpTyp: char): string;
function BuildZeitSyncSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                              Kennung: string;
                              CmdData: TZeitSyncCmdData): string;
function BuildMRG_ParaEinstellSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                     Kennung: string;
                                     PNrAllg: string; WertNeu: string;
                                     Passwort: string; PasswortNr: integer): string;
function BuildDSfG_DelEinstellSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                      Kennung: string;
                                      EAdr: string; DelAdr: string;
                                      ZCode1: string; ZCode2: string;
                                      WertNeu: string): string;
function BuildDSfGDfue_ParaEinstellSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                           Kennung: string;
                                           DfueCmd: string; ParaAdr: string;
                                           WertNeu: string): string;
function BuildTransparentSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                 Kennung: string; Timeout: integer;
                                 EmpfModus: integer): string;
function BuildRufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                         Modus: byte): string;
function BuildRufannahmeSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                CmdData: TRufannahmeCmdData): string;
function BuildRuflisteSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                              Kennung: string): string;
function BuildSlaveRufQuittSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                   Kennung: string; Adresse: char): string;
function BuildRueckrufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                              Kennung: string; Zentrale: byte): string;

function BuildZeitAbrufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                              Kennung: string): string;  // 17.08.2010

implementation

{----------------------------- Kommando lesen ---------------------------------}

{---------------------------------------------------------------------------------}
function GetAbrufKommandoTyp (Kommando: string; var KTypStr: string): TKommandoTyp;
{---------------------------------------------------------------------------------}
{ Kommandotyp aus Abruf-Kommando ermitteln;
  Übergabe: Abruf-Kommando
  Rückgabe: Komandotyp-String
  Ergebnis: Kommandotyp }
begin
  { Kommandotyp steht bis zum 1. Trennzeichen: }
  KTypStr:=ExtractString (Kommando, NUL, C_CmdSeparator, 0);
  if KTypStr = 'v' then
    Result:=kt_VerbAufbau      { Verbindung aufbauen }
  else if KTypStr = 'e' then
    Result:=kt_VerbAbbau       { Verbindung abbauen }
  else if KTypStr = 'E' then
    Result:=kt_MessAbruf       { Messwerte abrufen }
  else if KTypStr = 'M' then
    Result:=kt_MeldAbruf       { Meldungen abrufen }
  else if KTypStr = 'B' then
    Result:=kt_ParaAbruf       { Parameter abrufen }
  else if KTypStr = 'X' then
    Result:=kt_PruefAbruf      { Prüfungssätze abrufen }
  else if KTypStr = 'Z' then
    Result:=kt_ZeitSync        { Zeitsynchronisation durchführen }
  else if KTypStr = '}' then
    Result:=kt_DSfGUmschalt    { DSfG-Slave-Umschaltung }
  else if KTypStr = 'I' then
    Result:=kt_DSfGBusanalyse  { DSfG-Busanalyse }
  else if KTypStr = 'U' then
    Result:=kt_RpReset         { Rundpufferreset }
  else if KTypStr = 'C' then
    Result:=kt_Parametrieren   { Parametrieren }
  else if KTypStr = 'T' then
    Result:=kt_Transparent     { Transparentbefehl ausführen }
  else if KTypStr = 'R' then
    Result:=kt_Ruf             { Rufentgegennahme starten/beenden }
  else if KTypStr = 'a' then   
    Result:=kt_Rufannahme      { eingegangenen Ruf annehmen }
  else if KTypStr = '#' then
    Result:=kt_Rufliste        { MRG-Rufliste abrufen (Wieser DSfG-Umleitung) }
  else if KTypStr = 'Q' then
    Result:=kt_SlaveRufQuitt   { Slave-Adresse in Rufliste quittieren (Wieser DSfG-Umleitung) }
  else if KTypStr = 'A' then
    Result:=kt_Rueckruf        { Rückruf im MRG auslösen }
  else if KTypStr = 't' then   // 17.08.2010
    Result:=kt_ZeitAbruf       { Zeitinformationen abrufen }
  else
    Result:=kt_unbekannt;
end;

{------------------------------------------------------------}
function GetAbrufKommandoProzessID (Kommando: string): string;
{------------------------------------------------------------}
{ Prozess-ID aus Abruf-Kommando ermitteln;
  Übergabe: Abruf-Kommando
  Ergebnis: Prozess-ID }
begin
  { Prozess-ID: zwischen 1. und 2. Trennzeichen }
  Result:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 0);
end;

{-------------------------------------------------------------------}
function GetAbrufKommandoSchnittstellenNr (Kommando: string): string;
{-------------------------------------------------------------------}
{ Schnittstellen-Nummer aus Abruf-Kommando ermitteln;
  Übergabe: Abruf-Kommando
  Ergebnis: Schnittstellen-Nr. }
begin
  { Schnittstellen-Nr.: zwischen 2. und 3. Trennzeichen }
  Result:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 1);
end;

{----------------------------------------------------------}
function GetAbrufKommandoKennung (Kommando: string): string;
{----------------------------------------------------------}
{ Kennung aus Abruf-Kommando ermitteln;
  Übergabe: Abruf-Kommando
  Ergebnis: Kennung }
begin
  { Kennung: zwischen 3. und 4. Trennzeichen }
  Result:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);
end;

{-------------------------------------------------------------------}
function GetVerbAufbauCmdData (Kommando: string): TVerbAufbauCmdData;
{-------------------------------------------------------------------}
{ wandelt Daten aus Verbindungsaufbau-Kommando in Record;
  Ergebnis: Verbindungsaufbau-Kommandorecord }
var
  CmdData: TVerbAufbauCmdData;
  S: string;
  KommandoBuf: string;

begin
  { optional im Kommando enthaltene DSfG-ZeitSync-Kommandodaten aus Kommando-String extrahieren
    -> geblockt durch Pipe-Zeichen: |ZeitSync-Kommandodaten| }
  KommandoBuf:=ExtractString (Kommando, C_CmdSeparatorDyn, C_CmdSeparatorDyn, 0);   { DSfG-ZeitSync-Kommandodaten }
  if length (KommandoBuf) > 0 then begin
    S:=ExtractString (KommandoBuf, NUL, C_CmdSeparator, 0);             { ZeitSync aktiv }
    CmdData.DSfG_ZeitSyncCmdData.ZeitSync:=StrToIntDef (S, -1) > 0;
    CmdData.DSfG_ZeitSyncCmdData.Zeitbasis_Geraet:=
      ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 0);   { Zeitbasis im Gerät }
    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 1);  { min. Abweichung in s }
    CmdData.DSfG_ZeitSyncCmdData.Abweichung_min:=StrToIntDef (S, -1);
    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 2);  { max. Abweichung in s }
    CmdData.DSfG_ZeitSyncCmdData.Abweichung_max:=StrToIntDef (S, -1);
  end
  else begin
    CmdData.DSfG_ZeitSyncCmdData.ZeitSync:=false;
    CmdData.DSfG_ZeitSyncCmdData.Zeitbasis_Geraet:='';
    CmdData.DSfG_ZeitSyncCmdData.Abweichung_min:=-1;
    CmdData.DSfG_ZeitSyncCmdData.Abweichung_max:=-1;
  end;

  { Kommandoteil mit DSfG-ZeitSync-Kommandodaten kann jetzt abgeschnitten werden: }
  S:=Kommando;
  KommandoBuf:=F_Zerlegen (S, C_CmdSeparatorDyn);

  CmdData.Kennung:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 2);   { Kennung }
  CmdData.Rufnummer:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 3); { Rufnummer }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 4);                 { Geräte-Typ }
  CmdData.GeraeteTyp:=StrToIntDef (S, -1);
  CmdData.Passwort:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 5);  { Passwort }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 6);                 { Passwort-Nummer }
  CmdData.PasswortNr:=StrToIntDef (S, -1);
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 7);                 { Kennung prüfen J/N }
  CmdData.KennPruef:=UpperCase (S) = 'J';
  CmdData.ModemTyp:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 8);  { Modemtyp }
  { optionales Feld "DSfG-DFÜ transparent":
    -> wenn Feld nicht vorhanden: Ja (Kompatibilität zu früheren Versionen, bei
       denen grundsätzlich immer transparent geschaltet wurde) }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 9);                 { DSfG-DFÜ transparent schalten J/I/N }
  if (UpperCase (S) = 'J') OR (length (S) = 0) then
    CmdData.DSfG_TransparentModus:=dtm_Ja
  else if (UpperCase (S) = 'I') then  // 11.07.2019, WW
    CmdData.DSfG_TransparentModus:=dtm_Immer
  else
    CmdData.DSfG_TransparentModus:=dtm_Nein;
  { optionales Feld "Funktionscode":
    -> wenn Feld nicht vorhanden: 0 (Keine Funktion definiert) }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 10);                { Funktionscode }
  CmdData.FktCode:=StrToIntDef (S, C_CmdFktCode_Keine);
  { optionales Feld "TraceLog":
    -> wenn Feld nicht vorhanden: deaktiviert }
  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 11);                { TraceLog }
  CmdData.TraceLog:=StrToIntDef (S, C_CmdTraceLog_Deaktiviert);

  Result:=CmdData;
end;

{-----------------------------------------------------------------------------------}
function GetMessMeldPruefAbrufCmdData (Kommando: string): TMessMeldPruefAbrufCmdData;
{-----------------------------------------------------------------------------------}
{ wandelt Daten aus Kommando für Messwert-, Meldungs- oder Prüfsatz-Abruf in Record;
  Ergebnis: Kommandorecord }
var
  CmdData: TMessMeldPruefAbrufCmdData;
  S: string;
  TimeBuf: TDateTime;
  DayStr: string;
  vonOrdNrStr: string;
  bisOrdNrStr: string;
  isOrdNrCmd: boolean;
  KommandoBuf: string;
  sCmd: string;

begin
  { optional im Kommando enthaltene DSfG-Adressliste aus Kommando-String extrahieren (dynamische Länge !)
    -> geblockt durch Pipe-Zeichen: |Adressliste| }
  S:=ExtractString (Kommando, C_CmdSeparatorDyn, C_CmdSeparatorDyn, 0);   { DSfG-Adressliste }
  if length (S) > 0 then begin
    CmdData.DSfG_AdresslistData.ReqType:=F_Zerlegen (S, C_CmdSeparator);    { Request-Typ }
    CmdData.DSfG_AdresslistData.AdrList:=S;  { Adressdaten (Busadresse, öffentl. Schlüssel X Y und DEL-Adressen) }
  end
  else begin
    CmdData.DSfG_AdresslistData.ReqType:='';
    CmdData.DSfG_AdresslistData.AdrList:='';
  end;

  { Kommandoteil mit DSfG-Adressliste kann jetzt abgeschnitten werden: }
  S:=Kommando;
  KommandoBuf:=F_Zerlegen (S, C_CmdSeparatorDyn);

  sCmd:=ExtractString (KommandoBuf, NUL, C_CmdSeparator, 0);  { Kommando-Zeichen; 08.03.2019, WW }
  if length (sCmd) > 0 then
    CmdData.cCmd:=sCmd [1]
  else
    CmdData.cCmd:=NUL;
  vonOrdNrStr:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 7);
  bisOrdNrStr:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 8);
  { optionaler Ordnungsnummern-Eintrag geht vor Datum/Zeit-Eintrag: }
  isOrdNrCmd:=(length (vonOrdNrStr) > 0) AND (length (bisOrdNrStr) > 0);
  if isOrdNrCmd then begin
    CmdData.vonOrdNr:=StrToIntDef (vonOrdNrStr, -1);    { von-Ordnungsnummer }
    CmdData.bisOrdNr:=StrToIntDef (bisOrdNrStr, -1);    { bis-Ordnungsnummer }

    { von-bis-Datum/Zeit unbenutzt: }
    CmdData.vonDZ:=-1;
    CmdData.bisDZ:=-1;
  end
  else begin
    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 3);                 { von-Datum }
    DayStr:=Copy (S, 7, 2);
    if DayStr = '00' then                   { von-Tag 0 bedeutet alles abrufen }
      CmdData.vonDZ:=0
    else begin
      EncodeDateStr (S, 'YYYYMMDD', CmdData.vonDZ);
      S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 4);               { von-Zeit }
      EncodeTimeStr (S, 'HHMMSS', TimeBuf);
      CmdData.vonDZ:=CmdData.vonDZ + TimeBuf;
    end;
    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 5);                 { bis-Datum }
    EncodeDateStr (S, 'YYYYMMDD', CmdData.bisDZ);
    S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 6);                 { bis-Zeit }
    EncodeTimeStr (S, 'HHMMSS', TimeBuf);
    CmdData.bisDZ:=CmdData.bisDZ + TimeBuf;

    { von-bis-Ordnungsnummer unbenutzt: }
    CmdData.vonOrdNr:=-1;
    CmdData.bisOrdNr:=-1;
  end;
  CmdData.KanalaktivMaske:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 9);  { Kanalaktiv-Maske }

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 10);    { Archivdaten-Typ }
  if length (S) > 0 then  { Archivdaten-Typ ist optional }
    CmdData.ArchivdatenTyp:=StrToIntDef (S, -1)
  else  { Kompatibilität zu früheren WicomSrv-Versionen: normierte LGZ-Daten liefern }
    CmdData.ArchivdatenTyp:=C_CmdArchDatenTyp_LGZnorm;

  CmdData.Dateiname:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 11);  { Dateiname MW/ME, optional }
  CmdData.Dateiname_TA:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 12);  { Dateiname TA, optional }

  Result:=CmdData;
end;

{-----------------------------------------------------------------}
function GetParaAbrufCmdData (Kommando: string): TParaAbrufCmdData;
{-----------------------------------------------------------------}
{ wandelt Daten aus Parameterabruf-Kommando in Record;
  Ergebnis: Parameterabruf-Kommandorecord }
var
  CmdData: TParaAbrufCmdData;
  S: string;
  iSepNr_Timeout: integer;

begin
  CmdData.PNrAllg:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);   { allg. Parameternummer }

{$IFDEF GAS-X}
  { Gas-X-Version: }
  iSepNr_Timeout:=4;  // 09.03.2020, WW
{$ELSE}
  { RMG-Version: }
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);    { Archivdaten-Typ }
  CmdData.ArchivdatenTyp:=StrToIntDef (S, -1);  { Archivdaten-Typ ist optional }
  CmdData.Dateiname:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);  { Dateiname, optional }

  iSepNr_Timeout:=6;
{$ENDIF}

  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, iSepNr_Timeout);    { Timeout }
  CmdData.Timeout:=StrToIntDef (S, -1);  { Timeout ist optional }

  { optional im Kommando enthaltene DSfG-Adressliste aus Kommando-String extrahieren (dynamisch Länge !)
    -> geblockt durch Pipe-Zeichen: |Adressliste| }
  S:=ExtractString (Kommando, C_CmdSeparatorDyn, C_CmdSeparatorDyn, 0);   { DSfG-Adressliste }
  if length (S) > 0 then begin
    CmdData.DSfG_AdresslistData.ReqType:=F_Zerlegen (S, C_CmdSeparator);    { Request-Typ }
    CmdData.DSfG_AdresslistData.AdrList:=S;  { Adressdaten (Busadresse, öffentl. Schlüssel X Y und DEL-Adressen) }
  end
  else begin
    CmdData.DSfG_AdresslistData.ReqType:='';
    CmdData.DSfG_AdresslistData.AdrList:='';
  end;

  Result:=CmdData;
end;

{---------------------------------------------------------------}
function GetZeitSyncCmdData (Kommando: string): TZeitSyncCmdData;
{---------------------------------------------------------------}
{ wandelt Daten aus Zeitsynchronisations-Kommando in Record;
  Ergebnis: Zeitsynchronisations-Kommandorecord }
var
  CmdData: TZeitSyncCmdData;
  S: string;

begin
  CmdData.Zeitbasis_Geraet:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3); { Zeitbasis im Gerät }
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);   { min. Abweichung in  s }
  CmdData.Abweichung_min:=StrToIntDef (S, C_CmdZSyncAbweich_Standard);
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);   { max. Abweichung in  s }
  CmdData.Abweichung_max:=StrToIntDef (S, C_CmdZSyncAbweich_Standard);
  CmdData.BAdr_DSfG:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 6);   { für DSfG: Busadresse }

  Result:=CmdData;
end;

{-----------------------------------------------------------------------}
function GetDSfGUmschaltCmdData (Kommando: string): TDSfGUmschaltCmdData;
{-----------------------------------------------------------------------}
{ wandelt Daten aus DSfG-Slave-Umschaltung-Kommando in Record;
  Ergebnis: DSfG-Slave-Umschaltung-Kommandorecord }
var
  CmdData: TDSfGUmschaltCmdData;
  S: string;

begin
  CmdData.Kennung_alt:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2); { Kennung alt }
  CmdData.Kennung_neu:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3); { Kennung neu }
  CmdData.Adresse:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);   { Adresse }
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);                 { Geräte-Typ }
  CmdData.GeraeteTyp:=StrToIntDef (S, -1);
  CmdData.Passwort:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 6);  { Passwort }
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 7);                 { Passwort-Nummer }
  CmdData.PasswortNr:=StrToIntDef (S, -1);
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 8);                 { Kennung prüfen J/N }
  CmdData.KennPruef:=UpperCase (S) = 'J';

  Result:=CmdData;
end;

{----------------------------------------------------}
function GetRpResetCmdData (Kommando: string): string;
{----------------------------------------------------}
{ Ergebnis: Kürzel für Rundpufferreset-Typ }
begin
  Result:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);  { Rundpuffer-Typ }
end;

{-----------------------------------------------------------------------}
function GetParaEinstellCmdData (Kommando: string): TParaEinstellCmdData;
{-----------------------------------------------------------------------}
{ wandelt Daten aus Parametrier-Kommando in Record;
  Ergebnis: Parametrier-Kommandorecord }
var
  CmdData: TParaEinstellCmdData;
  S: string;

begin
  CmdData.ParaTyp:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);     { Parameter-Typ }
  CmdData.BAdr:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);        { BAdr }
  CmdData.ParaAdr:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);     { ParaAdr }
  CmdData.ZCode1:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 6);      { Zugangscode 1 }
  CmdData.ZCode2:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 7);      { Zugangscode 2 }
  { beim Extrahieren des neuen Parameterwerts berücksichtigen, daß das Separatorzeichen
    enthalten sein kann: }
  S:=ExtractString (Kommando, C_CmdSeparator, NUL, 8);
  CmdData.ParaWertNeu:=Copy (S, 1, length (S)-1);  { abschließendes "echtes" Separatorzeichen wegschneiden }

  Result:=CmdData;
end;

{---------------------------------------------------------------------}
function GetTransparentCmdData (Kommando: string): TTransparentCmdData;
{---------------------------------------------------------------------}
{ Ergebnis: Transparent-Kommandorecord }
var
  CmdData: TTransparentCmdData;
  S: string;
  KommandoBuf: string;

begin
  { im Kommando enthaltenen Tranparentbefehl aus Kommando-String extrahieren (dynamische Länge !)
    -> geblockt durch Pipe-Zeichen: |Transparentbefehl| }
  CmdData.Befehl:=ExtractString (Kommando, C_CmdSeparatorDyn, C_CmdSeparatorDyn, 0);   { Transparentbefehl }

  { Kommandoteil mit Transparentbefehl kann jetzt abgeschnitten werden: }
  S:=Kommando;
  KommandoBuf:=F_Zerlegen (S, C_CmdSeparatorDyn);

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 3);  { Timeout }
  if length (S) > 0 then  { Timeout ist optional }
    CmdData.Timeout:=StrToIntDef (S, -1)
  else  { Kompatibilität zu früheren WicomSrv-Versionen: Timeout aus WicomSrv-Einstellung }
    CmdData.Timeout:=C_CmdTimeout_Default;

  S:=ExtractString (KommandoBuf, C_CmdSeparator, C_CmdSeparator, 4);  { Empfangsmodus }
  if length (S) > 0 then  { Empfangsmodus ist optional }
    CmdData.EmpfModus:=StrToIntDef (S, -1)
  else  { Kompatibilität zu früheren WicomSrv-Versionen: Daten empfangen bis Antworttelegramm vollständig }
    CmdData.EmpfModus:=C_CmdEmpfModus_Telegr;

  Result:=CmdData;
end;

{-------------------------------------------------}
function GetRufCmdData (Kommando: string): integer;
{-------------------------------------------------}
{ gibt Modus aus Rufentgegennahme-Kommando zurück }
var
  S: string;

begin
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);     { Modus }
  Result:=StrToIntDef (S, -1);
end;

{-------------------------------------------------------------------}
function GetRufannahmeCmdData (Kommando: string): TRufannahmeCmdData;
{-------------------------------------------------------------------}
{ wandelt Daten aus Rufannahme-Kommando in Record;
  Ergebnis: Rufannahme-Kommandorecord }
var
  CmdData: TRufannahmeCmdData;
  S: string;

begin
  CmdData.Kennung:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 2);   { Kennung }
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3);                 { Geräte-Typ }
  CmdData.GeraeteTyp:=StrToIntDef (S, -1);
  CmdData.Passwort:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 4);  { Passwort }
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 5);                 { Passwort-Nummer }
  CmdData.PasswortNr:=StrToIntDef (S, -1);
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 6);                 { DSfG-Rufdeaktivierung prüfen J/N }
  CmdData.DSfG_RufDeakt:=UpperCase (S) = 'J';

  Result:=CmdData;
end;

{----------------------------------------------------------}
function GetSlaveRufQuittCmdData (Kommando: string): string;
{----------------------------------------------------------}
{ Ergebnis: Slave-Adresse für Rufquittierung }
begin
  Result:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3); { Slave-Adresse }
end;

{------------------------------------------------------}
function GetRueckrufCmdData (Kommando: string): integer;
{------------------------------------------------------}
{ Ergebnis: Zentrale, an die der Rückruf erfolgen soll }
var
  S: string;

begin
  S:=ExtractString (Kommando, C_CmdSeparator, C_CmdSeparator, 3); { Zentrale }
  Result:=StrToIntDef (S, -1);
end;


{-------------------------- Kommando zusammensetzen ---------------------------}

{----------------------------------------------------------------------------}
function BuildVerbAufbauSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                CmdData: TVerbAufbauCmdData): string;
{----------------------------------------------------------------------------}
{ Server-Kommandostring für Verbindungsaufbau zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Verbindungsaufbau-Kommandorecord
  Ergebnis: Kommando-String }
var
  cKennPruef: char;
  cDSfG_TransparentModus: char;

begin
  with CmdData do begin
    if KennPruef then
      cKennPruef:='J'   // Ja
    else
      cKennPruef:='N';  // Nein
    if DSfG_TransparentModus = dtm_Ja then
      cDSfG_TransparentModus:='J'   // Ja
    else if DSfG_TransparentModus = dtm_Immer then  // 10.07.2019, WW
      cDSfG_TransparentModus:='I'   // Immer
    else
      cDSfG_TransparentModus:='N';  // Nein

    Result:='v' + C_CmdSeparator +
            IntToStr (ProzessID) + C_CmdSeparator +
            IntToStr (SchnittstellenNr) + C_CmdSeparator +
            Kennung + C_CmdSeparator +
            Rufnummer + C_CmdSeparator +
            Format ('%.5d', [GeraeteTyp]) + C_CmdSeparator +  // Gerätetyp-Nummer 5-stellig
            Passwort + C_CmdSeparator +
            IntToStr (PasswortNr) + C_CmdSeparator +
            cKennPruef + C_CmdSeparator +
            ModemTyp + C_CmdSeparator +
            cDSfG_TransparentModus + C_CmdSeparator +
            IntToStr (FktCode) + C_CmdSeparator;
  end;
end;

{---------------------------------------------------------------------------}
function BuildVerbAbbauSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                               Kennung: string): string;
{---------------------------------------------------------------------------}
{ Server-Kommandostring für Verbindungsabbau zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
  Ergebnis: Kommando-String }
begin
  Result:='e' + C_CmdSeparator +
          IntToStr (ProzessID) + C_CmdSeparator +
          IntToStr (SchnittstellenNr) + C_CmdSeparator +
          Kennung + C_CmdSeparator;
end;

{------------------------------------------------------------------------------------}
function BuildMessMeldPruefAbrufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                        Kennung: string;
                                        CmdData: TMessMeldPruefAbrufCmdData): string;
{------------------------------------------------------------------------------------}
{ Server-Kommandostring zum Abruf von MRG-Messwerten, -Meldungen, -Parametern,
  -Prüfungssätzen bzw. DSfG-Archivdaten, -Logbucheinträgen und -Datenelementen zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            Kommandorecord
  Ergebnis: Kommando-String }
var
  S: string;

begin
  with CmdData do begin
    S:=cCmd + C_CmdSeparator +
       IntToStr (ProzessID) + C_CmdSeparator +
       IntToStr (SchnittstellenNr) + C_CmdSeparator +
       Kennung + C_CmdSeparator;
      { von-Datum: }
      if vonDZ < 0 then
        S:=S + C_CmdSeparator   // Datum-Feld und Zeit-Feld leer, da nicht verwendet
      else if vonDZ > 0 then
        S:=S + FormatDateTime ('yyyymmdd', vonDZ) + C_CmdSeparator +
               FormatDateTime ('hhnnss', vonDZ)
      else
        S:=S + FormatDateTime ('yyyymm"00"', 0) + C_CmdSeparator +
               FormatDateTime ('hhnnss', 0);     // von-Tag = 0: Alles
      S:=S + C_CmdSeparator;
      { bis-Datum: }
      if bisDZ < 0 then
        S:=S + C_CmdSeparator   // Datum-Feld und Zeit-Feld leer, da nicht verwendet
      else
        S:=S + FormatDateTime ('yyyymmdd', bisDZ) + C_CmdSeparator +
               FormatDateTime ('hhnnss', bisDZ);
      S:=S + C_CmdSeparator;
      { von-Ordnungsnummer: }
      if vonOrdNr < 0 then
        S:=S + C_CmdSeparator   // von-Ordnungsnummer-Feld leer, da nicht verwendet
      else
        S:=S + IntToStr (vonOrdNr) + C_CmdSeparator;
      { bis-Ordnungsnummer: }
      if bisOrdNr < 0 then
        S:=S + C_CmdSeparator   // bis-Ordnungsnummer-Feld leer, da nicht verwendet
      else
        S:=S + IntToStr (bisOrdNr) + C_CmdSeparator;
      { Kanalaktiv-Maske: }
      S:=S + KanalaktivMaske + C_CmdSeparator;
      { Archivdaten-Typ: }
      S:=S + IntToStr (ArchivdatenTyp) + C_CmdSeparator;
  end;
  Result:=S;
end;

{---------------------------------------------------------------------------}
function BuildParaAbrufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                               Kennung: string; PNrAllg: string;
                               Timeout: integer): string;
{---------------------------------------------------------------------------}
{ Server-Kommandostring zum Abruf von MRG-Parametern/DSfG-Datenelementen zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            allgemeine Parameternummer (nur für MRG)
            Timeout
  Ergebnis: Kommando-String }
begin
  Result:='B' + C_CmdSeparator +
          IntToStr (ProzessID) + C_CmdSeparator +
          IntToStr (SchnittstellenNr) + C_CmdSeparator +
          Kennung + C_CmdSeparator +
          PNrAllg + C_CmdSeparator;
  if Timeout > -1 then
    Result:=Result +
            '' + C_CmdSeparator +          // Feld bleibt leer, nicht verwendet
            '' + C_CmdSeparator +          // Feld bleibt leer, nicht verwendet
            IntToStr (Timeout) + C_CmdSeparator;
end;

{------------------------------------------------------------------------------}
function BuildDSfGUmschaltSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                  CmdData: TDSfGUmschaltCmdData): string;
{------------------------------------------------------------------------------}
{ Server-Kommandostring für DSfG-Umschaltung-zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            DSfG-Umschaltung-Kommandorecord
  Ergebnis: Kommando-String }
var
  cKennPruef: char;
begin
  with CmdData do begin
    if KennPruef then
      cKennPruef:='J'   // Ja
    else
      cKennPruef:='N';  // Nein
    Result:='}' + C_CmdSeparator +
            IntToStr (ProzessID) + C_CmdSeparator +
            IntToStr (SchnittstellenNr) + C_CmdSeparator +
            Kennung_alt + C_CmdSeparator +
            Kennung_neu + C_CmdSeparator +
            Adresse + C_CmdSeparator +
            Format ('%.5d', [GeraeteTyp]) + C_CmdSeparator +  // Gerätetyp-Nummer 5-stellig
            Passwort + C_CmdSeparator +
            IntToStr (PasswortNr) + C_CmdSeparator +
            cKennPruef + C_CmdSeparator;
  end;
end;

{-------------------------------------------------------------------------}
function BuildRpResetSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                             Kennung: string; RpTyp: char): string;
{-------------------------------------------------------------------------}
{ Server-Kommandostring für Rundpufferreset zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            Rundpuffer-Typ (T = Rundpufferreset für Messwerte
                            W = Rundpufferreset für Meldungen
                            P = Rundpufferreset für Prüfungssätze )
  Ergebnis: Kommando-String }
begin
  Result:='U' + C_CmdSeparator +
          IntToStr (ProzessID) + C_CmdSeparator +
          IntToStr (SchnittstellenNr) + C_CmdSeparator +
          Kennung + C_CmdSeparator +
          RpTyp + C_CmdSeparator;
end;

{--------------------------------------------------------------------------}
function BuildZeitSyncSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                              Kennung: string;
                              CmdData: TZeitSyncCmdData): string;
{--------------------------------------------------------------------------}
{ Server-Kommandostring für Zeitsynchronisation zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            Zeitsynchronisation-Kommandorecord
  Ergebnis: Kommando-String }
begin
  with CmdData do begin
    Result:='Z' + C_CmdSeparator +
            IntToStr (ProzessID) + C_CmdSeparator +
            IntToStr (SchnittstellenNr) + C_CmdSeparator +
            Kennung + C_CmdSeparator +
            Zeitbasis_Geraet + C_CmdSeparator +
            IntToStr (Abweichung_min) + C_CmdSeparator +
            IntTostr (Abweichung_max) + C_CmdSeparator +
            BAdr_DSfG + C_CmdSeparator;  // 17.05.2013, WW
  end;
end;

{-----------------------------------------------------------------------------------}
function BuildMRG_ParaEinstellSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                      Kennung: string;
                                      PNrAllg: string; WertNeu: string;
                                      Passwort: string; PasswortNr: integer): string;
{-----------------------------------------------------------------------------------}
{ Server-Kommandostring zum Einstellen eines MRG-Parameters zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            allgemeine MRG-Parameternummer
            neuer Parameterwert
            Parametrier-Passwort
            Parametrier-Passwortnummer (1..4)
  Ergebnis: Kommando-String }
begin
  Result:='C' + C_CmdSeparator +
          IntToStr (ProzessID) + C_CmdSeparator +
          IntToStr (SchnittstellenNr) + C_CmdSeparator +
          Kennung + C_CmdSeparator +
          C_CmdParatyp_MRG + C_CmdSeparator +
          '' + C_CmdSeparator +          // Feld bleibt leer, nicht verwendet
          PNrAllg + C_CmdSeparator +
          Passwort + C_CmdSeparator +               // ab 30.04.2019 WW; zuvor leer, nicht verwendet
          IntToStr (PasswortNr) + C_CmdSeparator +  // ab 30.04.2019 WW; zuvor leer, nicht verwendet
          WertNeu + C_CmdSeparator;
end;

{----------------------------------------------------------------------------------}
function BuildDSfG_DelEinstellSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                      Kennung: string;
                                      EAdr: string; DelAdr: string;
                                      ZCode1: string; ZCode2: string;
                                      WertNeu: string): string;
{----------------------------------------------------------------------------------}
{ Server-Kommandostring zum Einstellen eines DSfG-Datenelements zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            Busadresse
            Datenelement-Adresse
            Zugangscode 1 und 2
            neuer Parameterwert
  Ergebnis: Kommando-String }
begin
  Result:='C' + C_CmdSeparator +
          IntToStr (ProzessID) + C_CmdSeparator +
          IntToStr (SchnittstellenNr) + C_CmdSeparator +
          Kennung + C_CmdSeparator +
          C_CmdParatyp_DSfG + C_CmdSeparator +
          EAdr + C_CmdSeparator +
          DelAdr + C_CmdSeparator +
          ZCode1 + C_CmdSeparator +
          ZCode2 + C_CmdSeparator +
          WertNeu + C_CmdSeparator;
end;

{---------------------------------------------------------------------------------------}
function BuildDSfGDfue_ParaEinstellSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                           Kennung: string;
                                           DfueCmd: string; ParaAdr: string;
                                           WertNeu: string): string;
{---------------------------------------------------------------------------------------}
{ Server-Kommandostring zum Einstellen eines DSfG-DFÜ-Parameters (Norm-Parameter,
  Wieser DFÜ-Parameter, NTY-Masken) zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            DSfG-DFÜ-Kommando
            Parameteradresse
            neuer Parameterwert
  Ergebnis: Kommando-String }
begin
  Result:='C' + C_CmdSeparator +
          IntToStr (ProzessID) + C_CmdSeparator +
          IntToStr (SchnittstellenNr) + C_CmdSeparator +
          Kennung + C_CmdSeparator +
          C_CmdParatyp_DSfGDfue + C_CmdSeparator +
          DfueCmd + C_CmdSeparator +
          ParaAdr + C_CmdSeparator +
          '' + C_CmdSeparator +         // Feld bleibt leer, nicht verwendet
          '' + C_CmdSeparator +         // Feld bleibt leer, nicht verwendet
          WertNeu + C_CmdSeparator;
end;

{-----------------------------------------------------------------------------}
function BuildTransparentSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                 Kennung: string; Timeout: integer;
                                 EmpfModus: integer): string;
{-----------------------------------------------------------------------------}
{ Server-Kommandostring für Ausführung eines Transparentbefehls zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            Timeout (in ms; -1 = Timeout aus WicomSrv-Einstellung nehmen)
            Empfangsmodus (0 = Datenempfang beenden, wenn vollständiges Antwort-
                               telegramm eingetroffen ist
                           1 = Datenempfang über volle Timeout-Zeit)
  Ergebnis: Kommando-String }
begin
  Result:='T' + C_CmdSeparator +
          IntToStr (ProzessID) + C_CmdSeparator +
          IntToStr (SchnittstellenNr) + C_CmdSeparator +
          Kennung + C_CmdSeparator +
          IntToStr (Timeout) + C_CmdSeparator +
          IntToStr (EmpfModus) + C_CmdSeparator;
end;

{---------------------------------------------------------------------}
function BuildRufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                         Modus: byte): string;
{---------------------------------------------------------------------}
{ Server-Kommandostring für Rufentgegennahme zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Modus
  Ergebnis: Kommando-String }
begin
  Result:='R' + C_CmdSeparator +
           IntToStr (ProzessID) + C_CmdSeparator +
           IntToStr (SchnittstellenNr) + C_CmdSeparator +
           '' + C_CmdSeparator +               // Kennung fest leer, wird nicht verwendet
           IntToStr (Modus) + C_CmdSeparator;
end;

{----------------------------------------------------------------------------}
function BuildRufannahmeSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                CmdData: TRufannahmeCmdData): string;
{----------------------------------------------------------------------------}
{ Server-Kommandostring für Rufannahme zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Rufannahme-Kommandorecord
  Ergebnis: Kommando-String }
var
  cDSfG_RufDeakt: char;

begin
  with CmdData do begin
    if DSfG_RufDeakt then
      cDSfG_RufDeakt:='J'   // Ja
    else
      cDSfG_RufDeakt:='N';  // Nein

    Result:='a' + C_CmdSeparator +
            IntToStr (ProzessID) + C_CmdSeparator +
            IntToStr (SchnittstellenNr) + C_CmdSeparator +
            Kennung + C_CmdSeparator +
            Format ('%.5d', [GeraeteTyp]) + C_CmdSeparator +  // Gerätetyp-Nummer 5-stellig
            Passwort + C_CmdSeparator +
            IntToStr (PasswortNr) + C_CmdSeparator +
            cDSfG_RufDeakt + C_CmdSeparator;
  end;
end;

{--------------------------------------------------------------------------}
function BuildRuflisteSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                              Kennung: string): string;
{--------------------------------------------------------------------------}
{ Server-Kommandostring für MRG-Ruflisten-Abfrage zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
  Ergebnis: Kommando-String }
begin
  Result:='#' + C_CmdSeparator +
          IntToStr (ProzessID) + C_CmdSeparator +
          IntToStr (SchnittstellenNr) + C_CmdSeparator +
          Kennung + C_CmdSeparator;
end;

{-------------------------------------------------------------------------------}
function BuildSlaveRufQuittSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                                   Kennung: string; Adresse: char): string;
{-------------------------------------------------------------------------------}
{ Server-Kommandostring für Quittierung der MRG-Rufliste zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            Slave-Adresse
  Ergebnis: Kommando-String }
begin
  Result:='Q' + C_CmdSeparator +
          IntToStr (ProzessID) + C_CmdSeparator +
          IntToStr (SchnittstellenNr) + C_CmdSeparator +
          Kennung + C_CmdSeparator +
          Adresse + C_CmdSeparator;
end;

{--------------------------------------------------------------------------}
function BuildRueckrufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                              Kennung: string; Zentrale: byte): string;
{--------------------------------------------------------------------------}
{ Server-Kommandostring für Rückruf-Auslösung zusammensetzen;
  Übergabe: Prozess-ID
            Schnittstellen-Nummer
            Kennung
            Nummer der Zentrale (1 oder 2)
  Ergebnis: Kommando-String }
begin
  Result:='A' + C_CmdSeparator +
           IntToStr (ProzessID) + C_CmdSeparator +
           IntToStr (SchnittstellenNr) + C_CmdSeparator +
           Kennung + C_CmdSeparator +
           IntToStr (Zentrale) + C_CmdSeparator;
end;

{--------------------------------------------------------------------------}
function BuildZeitabrufSrvCmd (ProzessID: integer; SchnittstellenNr: integer;
                              Kennung: string): string;  // 17.08.2010
{--------------------------------------------------------------------------}
{ Server-Kommandostring für Zeitabruf
  Ergebnis: Kommando-String }
begin
  Result:='t' + C_CmdSeparator +
           IntToStr (ProzessID) + C_CmdSeparator +
           IntToStr (SchnittstellenNr) + C_CmdSeparator +
           Kennung + C_CmdSeparator;
end;

end.
